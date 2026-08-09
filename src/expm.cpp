//#undef NDEBUG
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADER
#define ARMA_DONT_USE_OPENMP // Known to cause speed problems
// Must precede RcppArmadillo.h: defined after the include it has no effect, and
// an Armadillo error message printed from a worker thread is a confirmed crash.
#define ARMA_DONT_PRINT_ERRORS
#include <iostream>
#include <RcppArmadillo.h>
#include <algorithm>
#include <cstring>
#include <cstdlib>
#include <vector>
#include "../inst/include/rxode2.h"
#include "../inst/include/rxode2dataErr.h"
#include "rxProtect.h"
#include "rxomp.h"

#define _(String) (String)

// `indLin()` runs inside `par_indLin`'s `omp parallel for`, so nothing in this
// file may use the R API or throw: `RSprintf` is the thread-safe printer (it
// prints from the master thread only and honors the silent-error flag).
extern "C" void RSprintf(const char *format, ...);

using namespace Rcpp;

std::string symengineRes(std::string val){
  if (val == "e" ||
      val == "E" ||
      val == "EulerGamma" ||
      val == "Catalan" ||
      val == "GoldenRatio" ||
      val == "I"){
    return "rx_SymPy_Res_" + val;
  }
  return val;
}

// Create R source for creating a Inductive linearization matrix
//
// Assume .states=the states in the model
// Assume .env= symengine environment
// @return A character string of R code for inductive linearization
//[[Rcpp::export]]
std::string rxIndLin_(CharacterVector states){
  std::string ret = "matrix(c(";
  std::string n = "c(";
  for (int i = 0; i < states.size(); i++){
    ret += ".rxIndLinLine(.env$rx__d_dt_"+as<std::string>(states[i])+
      "__" + ",.states, \""+ as<std::string>(states[i]) + "\"),";
    n += "\"" + states[i] +"\",";
  }
  ret += "NULL)," + std::to_string(states.size()) + "," + std::to_string(states.size()+2) +
    ",TRUE,list(" + n +"NULL)," + n + "\"_rxF\",\"indLin\")))";
  return ret;
}

extern "C" void F77_NAME(matexprbs)(int *ideg, int *m, double *t, double *H, int *iflag);

// Diagnostics; defined with the other step counters below.
extern "C" void rxIndLinCountIter(int n);

extern "C" void matexp_MH09(double *x, int n, const int p, double *ret);

// Taylor scaling-and-squaring, after Ruiz, Sastre, Ibanez & Defez (2015),
// J. Comput. Appl. Math. 291:370-379.
//
// The point at compartmental sizes is what it does NOT do: all three Pade
// backends need a linear solve, and at n = 2-4 the LAPACK call's fixed overhead
// dominates the arithmetic it performs.  This is matrix multiplies only.
//
// The degree is chosen per call rather than fixed, which is what makes this
// worth having: inductive linearization takes SHORT substeps, so ||A*h|| is
// usually far below 1 and a degree-4 polynomial already reaches unit roundoff.
// A fixed degree spends the same ~14 multiplies whatever the norm and loses to
// Pade on exactly the problems that take the most steps.
//
// theta[i] is the largest ||A|| for which degree `deg[i]` truncates below
// 2^-53: the remainder is bounded by ||A||^(m+1)/(m+1)!, so theta solves
// x^(m+1)/(m+1)! = 2^-53.  Cost is (degree + squarings) matrix multiplies, and
// the pair minimising that is chosen.
static const int    RX_TAYLOR_DEG[]   = {4, 6, 8, 10, 12, 14, 16, 18};
static const double RX_TAYLOR_THETA[] = {1.07e-3, 1.36e-2, 7.36e-2, 1.71e-1,
                                         3.34e-1, 5.56e-1, 8.24e-1, 1.157};
#define RX_TAYLOR_NDEG 8

static inline void matrixExpTaylor(arma::mat& H, arma::mat& out, double t) {
  const arma::uword n = H.n_rows;
  H *= t;
  double nrm = arma::norm(H, "inf");
  if (!R_FINITE(nrm)) nrm = 0.0;
  int deg = RX_TAYLOR_DEG[RX_TAYLOR_NDEG - 1];
  int s = 0;
  int best = -1;
  for (int si = 0; si <= 1023; ++si) {
    double x = std::ldexp(nrm, -si);
    for (int di = 0; di < RX_TAYLOR_NDEG; ++di) {
      if (x <= RX_TAYLOR_THETA[di]) {
        int cost = RX_TAYLOR_DEG[di] + si;
        if (best < 0 || cost < best) { best = cost; deg = RX_TAYLOR_DEG[di]; s = si; }
        break;                    // degrees are ascending: the first fit is cheapest here
      }
    }
    // Once the smallest degree fits, more scaling can only cost more.
    if (best >= 0 && deg == RX_TAYLOR_DEG[0]) break;
    if (std::ldexp(nrm, -si) <= RX_TAYLOR_THETA[0]) break;
  }
  if (s > 0) H *= std::ldexp(1.0, -s);
  out.eye(n, n);
  arma::mat tmp(n, n);
  for (int k = deg; k >= 1; --k) {
    tmp = H * out;
    out = tmp / (double) k;
    out.diag() += 1.0;
  }
  for (int i = 0; i < s; ++i) {
    tmp = out * out;
    out = tmp;
  }
}

// Writes exp(H*t) into `out`, which must already be H's size.
//
// `H` IS DESTROYED.  Type 2 has always overwritten its operand in place, and
// type 3 now scales in place rather than allocating a scaled copy, so the
// caller must treat H as consumed.  Returning the result instead cost an n^2
// copy that no caller could elide.
static inline void matrixExp(arma::mat& H, arma::mat& out, double t, int type,
                             int order){
  switch(type){
  case 4: {
    matrixExpTaylor(H, out, t);
    break;
  }
  case 3: {
    int p = order;
    if (p > 13) p = 13;
    int n = H.n_rows;
    H *= t;
    matexp_MH09(H.memptr(), n, p, out.memptr());
    break;
  }
  case 2: {
    int iflag=0;
    int m = H.n_rows;
    // FIXME C++ implementation for threading.
    out = H;
    F77_CALL(matexprbs)(&order, &m, &t, out.memptr(), &iflag);
    // matexpRBS used to warn through the R API from inside the parallel
    // region and carry on; it now returns iflag < 0 and leaves `out`
    // unfinished, which was previously used as if it were an answer.
    if (iflag < 0) {
      RSprintf(_("matrix exponential failed (singular Pade denominator)\n"));
      out.zeros();
    }
    break;
  }
  default:
    H *= t;
    out = arma::expmat(H);
  }
}

// -- Per-thread content-addressed matrix-exponential cache --------------------
//
// `matrixExp` is a pure function of (operand, t, type, order), and type/order
// cannot change mid-solve.  A bitwise match on (n, t, operand) is therefore a
// PROOF that a stored result is the right answer, rather than an assumption
// about what has been invalidated.  That is why this is content-addressed and
// not a validity flag: the old `ind->cacheME` flag was cleared on some
// covariate paths and not others, and reviving it would be a staleness bug.
// Content addressing needs no such bookkeeping -- a state-dependent rate matrix
// (rxSensMatExp models) simply never hits, and an infusion starting or stopping
// changes the augmented dimension, which is a key mismatch.
//
// `memcmp`, not elementwise ==: conservative on signed zero, correct on NaN bit
// patterns, and it does not invite anyone to relax it into a tolerance later.
//
// Slots have to cover every distinct operand in flight at once, or the round
// robin thrashes and the hit rate collapses.  In flight are: one operand per
// distinct h -- the fixed grid uses one h for every substep but the last, which
// is snapped to the interval end and differs in the last ulp, and the Romberg
// column adds h/2, h/4 and h/8 -- times one operand per *kind*, since the
// Newton path also exponentiates the [[A,I],[0,0]] block that yields P(h)
// alongside meOnly's own augmentation.  Sixteen covers 4 levels x 2 kinds with
// room to spare; the footprint is still trivial at compartmental sizes and is
// bounded by RX_INDLIN_EXPCACHE_MAXN2 regardless.
#define RX_INDLIN_EXPCACHE_N 16
// Skip caching above this n*n -- large sensitivity models would never hit and
// the footprint is per thread.
#define RX_INDLIN_EXPCACHE_MAXN2 16384

typedef struct {
  int n;                     // 0 == empty
  double t;
  std::vector<double> key;   // the operand, before it was consumed
  std::vector<double> val;   // exp(key * t)
} expCacheSlot_t;

typedef struct {
  expCacheSlot_t slot[RX_INDLIN_EXPCACHE_N];
  int next;                  // round-robin victim
} expCache_t;

static std::vector<expCache_t> __indLinExpCache;
static int __indLinExpCacheOff = 0;
// Iteration scheme selector (temporary; becomes the `indLinIteration` control).
static int __indLinUseNewton = 0;

// Sized before the parallel region, from rxData.cpp, alongside the other pools.
extern "C" void ensureIndLinExpCache(int nCores) {
  // Force-miss switch: every lookup fails, so "is this a cache bug?" is one run
  // rather than a bisect.  Read here so it is live per solve.
  __indLinExpCacheOff = (getenv("RXODE2_INDLIN_NO_EXP_CACHE") != NULL);
  __indLinUseNewton = (getenv("RXODE2_INDLIN_NEWTON") != NULL);
  if ((int)__indLinExpCache.size() < nCores) {
    __indLinExpCache.resize(nCores);
  }
  for (int i = 0; i < (int)__indLinExpCache.size(); i++) {
    // Nothing may survive into a different solve: the operands are the same
    // size and could collide across models that share a thread.
    for (int j = 0; j < RX_INDLIN_EXPCACHE_N; j++) __indLinExpCache[i].slot[j].n = 0;
    __indLinExpCache[i].next = 0;
  }
}

extern "C" void freeIndLinExpCache(void) {
  __indLinExpCache.clear();
}

// exp(H*t) into `out`, reusing an identical earlier exponential when there is
// one.  Consumes `H` exactly as `matrixExp` does.  `ind` is only for counting.
static inline void matrixExpCached(arma::mat& H, arma::mat& out, double t,
                                   int type, int order,
                                   rx_solving_options_ind *ind) {
  const int n = (int) H.n_rows;
  const size_t n2 = (size_t) n * (size_t) n;
  expCache_t *c = NULL;
  if (!__indLinExpCacheOff && !__indLinExpCache.empty() &&
      n2 <= RX_INDLIN_EXPCACHE_MAXN2) {
    c = &__indLinExpCache[rx_get_thread((int)__indLinExpCache.size())];
    for (int j = 0; j < RX_INDLIN_EXPCACHE_N; j++) {
      expCacheSlot_t &s = c->slot[j];
      if (s.n == n && memcmp(&s.t, &t, sizeof(double)) == 0 &&
          memcmp(s.key.data(), H.memptr(), n2*sizeof(double)) == 0) {
        memcpy(out.memptr(), s.val.data(), n2*sizeof(double));
        // Diagnostics: exponentials REUSED.  `jac_counter` is free on this path
        // -- a matExp() model's calc_jac is a stub -- so it carries the count
        // out through `$counts$jac` with no extra plumbing.
        if (ind != NULL && ind->jac_counter != NULL) ind->jac_counter[0]++;
        return;
      }
    }
  }
  // Diagnostics: exponentials COMPUTED, reported as `$counts$dadt` (dydt() is
  // likewise a no-op stub for a matExp() model).
  if (ind != NULL && ind->dadt_counter != NULL) ind->dadt_counter[0]++;
  if (c == NULL) {
    matrixExp(H, out, t, type, order);
    return;
  }
  // The operand must be snapshotted BEFORE exponentiating, because matrixExp
  // consumes it.
  expCacheSlot_t &s = c->slot[c->next];
  c->next = (c->next + 1) % RX_INDLIN_EXPCACHE_N;
  s.n = 0;                            // invalid until both halves are written
  if (s.key.size() != n2) s.key.resize(n2);
  if (s.val.size() != n2) s.val.resize(n2);
  memcpy(s.key.data(), H.memptr(), n2*sizeof(double));
  matrixExp(H, out, t, type, order);
  memcpy(s.val.data(), out.memptr(), n2*sizeof(double));
  s.t = t;
  s.n = n;
}
// extern "C" typedef void (*matvec_t) (double *, double *, double *, int *);

// extern "C" typedef void (*DGPADM_t)(int *ideg, int *mx, double *t,
// 				    double *, int *mh, double *,
// 				    int *lfree, int *iwsp, int *iexph,
// 				    int *ns, int *iflag, int *type);

// extern "C" void F77_NAME(DSPHIV)(int *n, int *m, double *t,
// 				 double *u, double *v, double *w,
// 				 double *tol, double *anorm,
// 				 double *wsp, int *lwsp,
// 				 int *iwsp, int *liwsp, matvec_t,
// 				 int *iflag, double *A, DGPADM_t,
// 				 int *type, int *ideg, int *mxstep);

arma::vec phiv(double t, arma::mat& A, arma::vec& u,
	       arma::vec& v, rx_solving_options *op){
  int n = A.n_rows;
  int order = op->indLinMatExpOrder;
  int type = op->indLinMatExpType;
  switch(n){
  case 1: {
    // m = 0
    // I don't think we *should* run into this case, but...
    arma::vec w(1);
    double eAt = exp(t*A(0,0));
    w(0) = eAt*v(0) + (eAt-1)/A(0,0)*u(0);
    return w;
  }
  case 2: {
    // m=1
    double d= (A(0,0)*A(1,1)-A(0,1)*A(1,0));
    d = 1.0/d;
    arma::mat22 Ainv;
    Ainv(0,0) = A(1,1)*d;
    Ainv(1,1) = A(0,0)*d;
    Ainv(0,1) = -A(0,1)*d;
    Ainv(1,0) = -A(0,1)*d;
    // matrixExp consumes its operand, and A belongs to the caller.  The type-2
    // backend already overwrote A here in place, so this also removes a
    // destruction that only happened for some backends.
    arma::mat Aop = A;
    arma::mat expAt(2, 2);
    matrixExp(Aop, expAt, t, type, order);
    arma::vec w = expAt*v + (expAt-arma::eye(2,2))*Ainv*u;
    return w;
  }
  default: {
    double tol = op->indLinPhiTol;
    int m = op->indLinPhiM;
    if (m <= 0) m = std::min(n, 30);
    double anorm = arma::norm(A, "inf");
    int mxrej = 10;  double btol  = 1.0e-7;
    double gamma = 0.9; double delta = 1.2;
    int mb    = m; double t_out   = fabs(t);
    int istep = 0; double t_new   = 0;
    double t_now = 0; double s_error = 0;
    double rndoff= anorm*DBL_EPSILON;
    double sgn = (0.0 < t) - (t > 0.0);
    int k1 = 3, ireject = 0, mx=0;
    double xm = 1.0/m;
    arma::vec w = v;
    arma::mat V, H, F, tmp, Fop;
    arma::vec p;
    double beta=0, fact=0, s=0, t_step=0, h=0, avnorm=0, err_loc=0, p1, p2;
    while (t_now < t_out){
      V = arma::mat(n, m+1, arma::fill::zeros);
      H = arma::mat(m+3, m+3, arma::fill::zeros);
      V.col(0) = A*w+u;
      beta = norm(V.col(0));
      V.col(0) /= beta;
      if (istep == 0){
	fact = R_pow_di((m+1)/M_E,m+1)*sqrt(M_2PI*(m+1));
	t_new = (1/anorm)*pow((fact*tol)/(4*beta*anorm),xm);
	s = R_pow_di(10,(std::floor(log10(t_new))-1));
	t_new = std::ceil(t_new/s)*s;
      }
      istep++;
      t_step = std::min( t_out-t_now,t_new );
      for (int j = 0; j < m; ++j){
	p = A*V.col(j);
	for (int i = 0; i < j; ++i){
	  tmp = V.col(i).t()*p;
	  H(i,j) = tmp(0,0);
	  p = p-H(i,j)*V.col(i);
	}
	s = norm(p);
	if (s < btol){
	  k1 = 0;
	  mb = j;
	  t_step = t_out-t_now;
	  break;
	}
	H(j+1,j) = s;
	V.col(j+1) = (1/s)*p;
      }
      H(0,mb) = 1;
      if (k1 != 0){
	H(m,m+1) = 1;
	H(m+1,m+2) = 1;
	h = H(m,m-1);
	H(m,m-1) = 0;
	avnorm = norm(A*V.col(m-1));
      }
      ireject = 0;
      while(ireject <= mxrej){
	mx = mb + std::max(1,k1);
	// The operand must be separate from the result: matrixExp consumes it.
	Fop = H(arma::span(0,mx-1),arma::span(0,mx-1));
	F.set_size(mx, mx);
	matrixExp(Fop, F, sgn*t_step, type, order);
	if (k1 == 0){
	  err_loc = btol;
	  break;
	} else {
	  F(m,m) = h*F(m-1,m+1);
	  F(m+1,m) = h*F(m-1,m+2);
	  p1 = fabs( beta*F(m,m) );
	  p2 = fabs( beta*F(m+1,m) * avnorm );
	  if (p1 > 10*p2){
	    err_loc = p2;
	    xm = 1.0/m;
	  } else if (p1 > p2){
	    err_loc = (p1*p2)/(p1-p2);
	    xm = 1.0/m;
	  } else{
	    err_loc = p1;
	    xm = 1.0/(m-1.0);
	  }
	}
	if (err_loc <= delta * t_step*tol){
	  break;
	} else {
	  t_step = gamma * t_step * pow(t_step*tol/err_loc, xm);
	  s = R_pow_di(10,std::floor(log10(t_step))-1);
	  t_step = std::ceil(t_step/s) * s;
	  if (ireject == mxrej){
	    // Never throw from here: `indLin()` -- and therefore anything it
	    // calls -- runs inside `par_indLin`'s `omp parallel for`, where a
	    // longjmp out of a worker thread crashes the session.  Accept the
	    // step that missed the tolerance and say so instead.
	    RSprintf(_("requested tolerance is too high\n"));
	    break;
	  }
	  ireject = ireject + 1;
	}
      }
      if (k1-2 > 0){
	mx = mb + k1-2;
      } else {
	mx = mb;
      }
      w = V.cols(0,mx-1)*(beta*F(arma::span(0,mx-1),arma::span(mb,mb))) + w;

      t_now = t_now + t_step;
      t_new = gamma * t_step * pow(t_step*tol/err_loc, xm);
      t_new = std::max(std::min(t_new, 1e300), 1.0-200);
      s = R_pow_di(10.0, std::floor(log10(t_new))-1);
      t_new = std::ceil(t_new/s) * s;
      err_loc = std::max(err_loc,rndoff);
      s_error = s_error + err_loc;
    }
    // err = s_error
    return w;
  }
  }
}

bool expm_assign=false;
SEXP expm_s;

// P(h) = A^-1(exp(Ah) - I) = h*phi1(Ah), the operator the substep map applies
// to the forcing.  Needed as a MATRIX only by the Newton iteration, which forms
// `I - P(h) f'`.
//
// Evaluated as a series rather than an exponential.  Taking it as the top-right
// block of exp([[A,I],[0,0]]h) is exact and needs no A^-1, but it is a 2n x 2n
// exponential against meOnly's n x n (or n+nInf), i.e. ~8x the work on a model
// with no infusion -- measured at 5.8 exponentials per step against Picard's
// 2.9, which was the entire reason Newton lost on wall clock.
//
// The series is not an approximation in the regime that matters.  A matExp()
// model puts its LINEAR dynamics in A, where the exponential already handles
// them exactly, and only the nonlinear remainder in the forcing -- so wherever
// the iteration is the binding constraint, ||A*h|| is small.  The degree is
// chosen from the norm so the truncation sits below unit roundoff, exactly as
// matrixExpTaylor does, and anything with a norm too large to serve that way
// falls back to the exponential.
//
// phi1(z) = sum_{k>=0} z^k/(k+1)!, by Horner:
//   P = I/(m+1)!;  for k = m..1:  P = I/k! + z*P;  then P(h) = h*P.
#define RX_INDLIN_PHI1_MAXNRM 0.5
static bool indLinPhi1(const arma::mat &A, double h, arma::mat &Ph,
                       arma::mat &z, arma::mat &tmp) {
  const arma::uword n = A.n_rows;
  z = A*h;
  double nrm = arma::norm(z, "inf");
  if (!R_FINITE(nrm) || nrm > RX_INDLIN_PHI1_MAXNRM) return false;
  // Smallest m with nrm^(m+1)/(m+2)! <= 2^-53; at nrm <= 1/2, m = 16 is ample
  // for any of them, and the terms are n x n multiplies at compartmental size.
  int m = 16;
  double fk = 1.0;                       // (m+1)!
  for (int k = 2; k <= m + 1; ++k) fk *= (double) k;
  Ph.eye(n, n);
  Ph /= fk;
  for (int k = m; k >= 1; --k) {
    double kf = 1.0;
    for (int q = 2; q <= k; ++q) kf *= (double) q;
    tmp = z*Ph;
    Ph = tmp;
    Ph.diag() += 1.0/kf;
  }
  Ph *= h;
  return true;
}

// -- Forcing Jacobian ---------------------------------------------------------
//
// `Jf(i,j) = d(forcing_i)/d(state_j)`, by central differences of `IndF`.
//
// Differencing IndF is the right FD here and the generic helper is not:
// `OdeBase::ode_jac` (src/ode_impl.cpp) differences `dydt`, which for a
// matExp() model is a no-op stub and would hand back a zero Jacobian without
// complaint.  IndF is a real compiled function, and since it returns
// `InfusionRate + forcing` with the infusion rate independent of the states,
// differencing it gives exactly the forcing Jacobian.
//
// The step follows `_esJacColF` (inst/include/rxode2parseHandleEvid.h): a
// relative epsilon floored at the absolute scale, which is what keeps it
// meaningful for a compartment sitting near zero.
//
// Every state is swept as a column, NOT just the ones in `op->indLin[]`.  That
// list is the states whose forcing is state-dependent -- the nonzero ROWS.  A
// forcing may perfectly well depend on a state that carries no forcing of its
// own, and that entry is a column, not a row.  van der Pol is exactly this
// case: the forcing `mu*(1-y^2)*dy` sits on `dy`, so only `dy` is flagged, yet
// `d/dy = -2*mu*y*dy` is the dominant entry at large mu.  Restricting the sweep
// to the flagged set drops it silently, and Newton then fails to converge at
// all on the stiff problems it exists for.
static void indLinForcingJacFd(int cSub, rx_solving_options *op,
                               int neq, double tcov, double tEval,
                               const double *y, t_IndF IndF,
                               arma::mat &Jf, arma::vec &yPert,
                               arma::vec &fPlus, arma::vec &fMinus) {
  Jf.zeros(neq, neq);
  if (IndF == NULL) return;
  std::copy(y, y + neq, yPert.memptr());
  for (int j = 0; j < neq; ++j) {
    const double yj = y[j];
    const double eps = 6e-6 * std::max(fabs(yj), 1.0);
    yPert[j] = yj + eps;
    IndF(cSub, tcov, tEval, fPlus.memptr(), yPert.memptr());
    yPert[j] = yj - eps;
    IndF(cSub, tcov, tEval, fMinus.memptr(), yPert.memptr());
    yPert[j] = yj;
    const double d = 0.5/eps;
    for (int i = 0; i < neq; ++i) {
      Jf(i, j) = (fPlus[i] - fMinus[i])*d;
    }
  }
}

int meOnly(int cSub, double *yc_, double *yp_, double tp, double tf, double tcov,
	   double tme, double *InfusionRate_, int *on_, t_ME ME, rx_solving_options *op,
	   rx_solving_options_ind *ind){
  // Honor per-individual neqOverride when ind is available; otherwise fall
  // back to op->neq.  Keeps allocations / loops consistent with what the
  // outer indLin solver wrote.
  int neq = (ind != NULL) ? rxEffNeq(ind, op) : op->neq;
  int type = op->indLinMatExpType;
  int order = op->indLinMatExpOrder;
  arma::mat m0(neq, neq);
  ME(cSub, tcov, tme, m0.memptr(), yc_);
  const arma::vec InfusionRate(InfusionRate_, neq, false, false);
  arma::vec yp(yp_, neq, false, true);
  arma::vec yc(yc_, neq, false, true);
  // arma::mat inMat;
  // arma::mat mexp;
  // arma::mat ypout;
  unsigned int i, nInf=0;
  for (i = 0; i < (unsigned int)neq; i++){
    if (InfusionRate[i] != 0.0) nInf++;
  }
  if (nInf == 0){
    arma::mat expAT(neq, neq);
    matrixExpCached(m0, expAT, tf-tp, type, order, ind);
    arma::vec yc_temp = expAT*yp;
    std::copy(yc_temp.begin(), yc_temp.end(), yc_);
    return 1;
  } else {
    arma::mat mout(neq+nInf, neq+nInf, arma::fill::zeros);
    arma::vec ypout(neq+nInf);
    for (int j = neq; j--;){
      std::copy(m0.colptr(j), m0.colptr(j)+neq, mout.colptr(j));
    }
    std::copy(yp.begin(),yp.end(),ypout.begin());
    // Each infused compartment gets a unit column in the augmented block and
    // its rate in the augmented state, which turns the constant forcing into
    // part of the exponential and avoids needing A^-1.  This used to be staged
    // through a zero-filled neq x neq scratch matrix and copied in.
    int cur_nInf = 0;
    for (i = 0; i < (unsigned int)neq; i++){
      if (InfusionRate[i] != 0.0){
        mout(i, neq + cur_nInf) = 1.0;
        ypout[neq + cur_nInf] = InfusionRate[i];
        cur_nInf++;
      }
    }
    arma::vec meSol(neq+nInf);
    arma::mat expAT(neq+nInf, neq+nInf);
    // Unfortunately the tf-tp may change so we can not cache this.
    matrixExpCached(mout, expAT, (tf-tp), type, order, ind);
    meSol = expAT*ypout;
    std::copy(meSol.begin(), meSol.begin()+neq, yc_);
    return 1;
  }
}

// One inductive-linearization pass over `[subTp, subTf]`: build the forcing
// (codes 2/4) and the matrix at `w`, propagate from `y0`, and leave the result
// in `w`.  `u` is NULL for the codes that carry no `IndF` forcing.
static inline int indLinPass(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                             double *w, double *y0, double subTp, double subTf, double tcov,
                             double tEval, double *InfusionRate_, int *on_,
                             t_ME ME, t_IndF IndF, arma::vec *u) {
  double *force = InfusionRate_;
  if (u != NULL) {
    IndF(cSub, tcov, tEval, u->memptr(), w);
    force = u->memptr();
  }
  return meOnly(cSub, w, y0, subTp, subTf, tcov, tEval, force, on_, ME, op, ind);
}

// Cap on the inductive iteration, deliberately small and deliberately NOT
// `op->mxstep` (70000).  The Picard map contracts at a ratio proportional to
// the substep, so an iteration that has not converged in this many passes is
// telling us the substep is too long -- and the driver's answer to that is to
// cut the step, which is cheap.  Grinding tens of thousands of matrix
// exponentials before reaching that conclusion is not.
#define RX_INDLIN_MAXITER 20
// The iterate has to land a factor tighter than the step-size tolerance, or
// the local error estimate (which differences two iterates) ends up measuring
// iteration noise instead of discretization error and the controller chases
// its own tail.  Same convention as lsoda's corrector.
#define RX_INDLIN_PICARD_TOL_FAC 0.1
// Bounds on the relaxation factor.  Anything outside these is a secant fit that
// should not be trusted; the driver cuts the step instead.
#define RX_INDLIN_THETA_MIN 0.1
#define RX_INDLIN_THETA_MAX 10.0
// `indLinStepSearch` codes, matching the R-side character values.
#define RX_INDLIN_SEARCH_NONE   0
#define RX_INDLIN_SEARCH_SECANT 1
#define RX_INDLIN_SEARCH_EXACT  2
// `indLinRichardson` codes.
#define RX_INDLIN_RICH_NEVER  0
#define RX_INDLIN_RICH_ALWAYS 1
#define RX_INDLIN_RICH_AUTO   2
#define RX_INDLIN_RICH_ALWAYS4 3
#define RX_INDLIN_RICH_ALWAYS5 4
// Auto switch-over.  If the second-order step needs N substeps to cross an
// interval, the third-order one needs about N^(2/3) of them -- error falls as
// h^2 against h^3, so the step counts go as E^(-1/2) against E^(-1/3) -- at
// three times the cost per step.  Richardson is therefore cheaper once
// 3*N^(2/3) < N, i.e. once N > 3^3 = 27.  Measured crossover on a
// Michaelis-Menten model: 33 substeps per interval, at atol=rtol=1e-5.
#define RX_INDLIN_AUTO_RICH_N 27
// And once more for the fourth-order column: 7*N^(1/2) < 3*N^(2/3) once
// N^(1/6) > 7/3, i.e. N > (7/3)^6 ~ 161.
#define RX_INDLIN_AUTO_RICH4_N 161
// And the fifth-order column (15 solves) beats the fourth (7) once
// 15*N^(2/5) < 7*N^(1/2), i.e. once N^(1/10) > 15/7, N > (15/7)^10 ~ 1750.
#define RX_INDLIN_AUTO_RICH5_N 1750

// Secant estimate of the iteration map's contraction ratio from two
// consecutive Picard residuals, measured only over the states flagged in
// `op->indLin[]` and scaled by the same `rtol`/`atol` the convergence test
// uses, so states of different magnitude contribute comparably.  `thetaPrev`
// undoes the relaxation that produced `d`: relaxing by theta turns a map
// derivative g' into a measured ratio of 1 + theta*(g'-1).
//
// Returns the relaxation factor 1/(1-g') to use next, clamped to a range that
// keeps a nonlinear map from being over-relaxed off a cliff, or 1.0 (plain
// Picard) when there is nothing usable to estimate from.
static inline double indLinTheta(rx_solving_options *op, const double *rtol, const double *atol,
                                 const arma::vec &w, const arma::vec &d, const arma::vec &dPrev,
                                 double thetaPrev) {
  double num = 0.0, den = 0.0;
  for (int j = op->indLinN; j--;) {
    int k = op->indLin[j];
    double sc = rtol[k]*fabs(w[k]) + atol[k];
    if (sc <= 0.0) continue;
    double dn = d[k]/sc, dp = dPrev[k]/sc;
    num += dn*dp;
    den += dp*dp;
  }
  if (den <= 0.0 || !R_FINITE(num) || thetaPrev <= 0.0) return 1.0;
  double g = 1.0 + (num/den - 1.0)/thetaPrev;
  if (!R_FINITE(g)) return 1.0;
  double theta = 1.0/(1.0 - g);
  // Clamp the relaxation itself rather than the estimated ratio: the point of
  // the bound is that a secant fit of a nonlinear map should not be trusted far
  // from theta = 1.  A measured ratio well below -1 means the map is EXPANDING,
  // and the right answer to that is a shorter step (which the driver will take
  // once this returns -2), not crawling along at theta = 1e-3.
  if (!R_FINITE(theta) || theta < RX_INDLIN_THETA_MIN) return RX_INDLIN_THETA_MIN;
  if (theta > RX_INDLIN_THETA_MAX) return RX_INDLIN_THETA_MAX;
  return theta;
}

// Tolerance-scaled RMS of a Picard residual over the flagged states.  Used both
// as the convergence measure and to notice an iteration that is going backwards.
static inline double indLinResNorm(rx_solving_options *op, const double *rtol, const double *atol,
                                   const arma::vec &w, const arma::vec &d) {
  double s = 0.0;
  int n = 0;
  for (int j = op->indLinN; j--;) {
    int k = op->indLin[j];
    double sc = rtol[k]*fabs(w[k]) + atol[k];
    if (sc <= 0.0) continue;
    double e = d[k]/sc;
    s += e*e;
    n++;
  }
  return (n > 0) ? sqrt(s/(double)n) : 0.0;
}

// The inductive-linearization fixed point over one relinearization substep
// (rxode2#1185).  `y0` is the substep-start state and never moves; `w` is the
// iterate and doubles as the point `ME`/`IndF` are built at, so the
// linearization chases the solution while propagation always restarts from
// `y0`.  The first pass linearizes at `y0` -- that alone is the non-iterating
// codes 1/2 answer.  Converged when the Picard residual `g(w)-w` is within
// `rtol`/`atol` for every state flagged in `op->indLin[]`.
//
// Plain Picard is only marginally contractive once the substep is comparable
// to the forcing's own time scale (a Michaelis-Menten forcing with no linear
// elimination sits right at ratio -1 at the default `hmax`, where it oscillates
// for ~1e5 passes), so each step is relaxed by `indLinTheta()`.  Relaxation
// does not move the fixed point, so the converged answer -- and its order in
// `hmax` -- is the undamped one.
//
// Returns 1 on convergence, -2 when `maxIter` passes are exhausted or the
// iterate leaves the reals -- that is a "cut the step" signal to the driver,
// not a failure -- or whatever `meOnly()` failed with.
// The same substep fixed point as `indLinIterate`, reached by Newton instead of
// relaxed Picard.  Same equation, same answer -- only the path differs.
//
// The map is `w = g(w) = exp(Ah)y0 + P(h) f(t,w)` with `P(h) = A^-1(exp(Ah)-I)`,
// so the residual is `G(w) = w - g(w)` and
//
//     G'(w) = I - P(h) f'(w)
//
// P(h) is approximated by `h*I`.  That is the classical simplified-Newton
// choice (lsoda's corrector uses `I - h*gamma*J` for the same reason), and it
// is a good one here rather than merely convenient: `P(h) = h*phi1(Ah)` and
// phi1(0) = I, so the approximation is exact as `||Ah|| -> 0`.  A matExp()
// model puts its LINEAR dynamics in `A` -- where the exponential handles them
// exactly and no iteration is needed -- and only the nonlinear remainder in the
// forcing, which is what this iterates on.  So the regime where the iteration
// is the binding constraint is precisely the regime where `||Ah||` is small.
// Materialising the true P(h) would cost a second, wider exponential per step;
// if convergence measures poorly this is the first thing to revisit.
//
// An inexact Jacobian costs convergence RATE, never correctness: the fixed
// point of `G(w) = 0` does not depend on `G'`.  That is also why the Jacobian
// is formed once per substep and reused across iterations (a chord iteration)
// rather than refreshed each time.
//
// The contract is `indLinIterate`'s, exactly, because that is what buys the
// Romberg extrapolation, the exponential cache and the event handling:
//   * `*w1Out` is the pass-0 `tEval = subTp` left-endpoint answer -- so this
//     still evaluates one Picard pass to produce it.  Newton's own first
//     iterate is not that value, and returning it would turn the caller's error
//     estimate into a measure of this iteration's transient instead of
//     truncation, silently corrupting every extrapolation level built on it.
//   * `*ratioOut` is a `>= 1` "how much too long is this step" number.
//   * `-2` means "cut the step", never a hard error.
static int indLinNewton(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                        int neq, double *rtol, double *atol, int maxIter,
                        double *yp_, double subTp, double subTf, double tcov,
                        double *InfusionRate_, int *on_, t_ME ME, t_IndF IndF,
                        arma::vec *u, arma::vec *w1Out, double *ratioOut) {
  arma::vec y0(yp_, neq);
  arma::vec w(y0), gw(neq), r(neq), dw(neq);
  arma::vec yPert(neq), fPlus(neq), fMinus(neq);
  arma::mat Jf(neq, neq), G(neq, neq);
  if (ratioOut != NULL) *ratioOut = 1.0;

  // Pass 0: the left-endpoint answer, evaluated at subTp in time as well as in
  // state.  This is the caller's `w1`.
  rxIndLinCountIter(1);
  int ret = indLinPass(cSub, op, ind, w.memptr(), y0.memptr(), subTp, subTf, tcov,
                       subTp, InfusionRate_, on_, ME, IndF, u);
  if (ret <= 0) return ret;
  if (w1Out != NULL) *w1Out = w;

  // Chord Jacobian, formed once at the pass-0 iterate.
  indLinForcingJacFd(cSub, op, neq, tcov, subTf, w.memptr(), IndF,
                     Jf, yPert, fPlus, fMinus);
  const double h = subTf - subTp;
  // P(h) = A^-1(exp(Ah) - I), the operator the map actually applies to the
  // forcing.  It is the top-right block of exp([[A, I],[0, 0]]h) -- the same
  // augmentation `meOnly` uses, widened to the full identity instead of just
  // the infusion unit columns -- so it needs no A^-1 and rides the exponential
  // cache.  `h*I` is its small-||Ah|| limit and was tried first; see the note
  // in the header for when that is and is not adequate.
  arma::mat Aloc(neq, neq);
  ME(cSub, tcov, subTf, Aloc.memptr(), w.memptr());
  arma::mat Ph(neq, neq), phiZ(neq, neq), phiTmp(neq, neq);
  if (!indLinPhi1(Aloc, h, Ph, phiZ, phiTmp)) {
    // ||A*h|| too large for the series: fall back to the exact block.
    arma::mat aug(2*neq, 2*neq, arma::fill::zeros);
    aug.submat(0, 0, neq-1, neq-1) = Aloc;
    aug.submat(0, neq, neq-1, 2*neq-1).eye();
    arma::mat augE(2*neq, 2*neq);
    matrixExpCached(aug, augE, h, op->indLinMatExpType, op->indLinMatExpOrder, ind);
    Ph = augE.submat(0, neq, neq-1, 2*neq-1);
  }
  G = -Ph*Jf;
  G.diag() += 1.0;
  // Invert once and reuse: at compartmental sizes this is a handful of flops,
  // and it keeps the per-iteration cost to a matrix-vector product.  If it is
  // singular, fall through to plain Picard steps rather than failing the step.
  arma::mat Ginv;
  bool haveFact = arma::inv(Ginv, G);

  double resPrev = -1.0;
  int nGrow = 0;
  bool refreshed = false;
  for (int i = 0; i < maxIter; ++i) {
    rxIndLinCountIter(1);
    gw = w;
    ret = indLinPass(cSub, op, ind, gw.memptr(), y0.memptr(), subTp, subTf, tcov,
                     subTf, InfusionRate_, on_, ME, IndF, u);
    if (ret <= 0) return ret;
    r = gw - w;                       // -G(w); the Picard step is exactly this
    for (int j = op->indLinN; j--;) {
      if (!R_FINITE(gw[op->indLin[j]])) {
        std::copy(gw.begin(), gw.end(), yp_);
        return -2;
      }
    }
    double res = indLinResNorm(op, rtol, atol, gw, r);
    // Converged: same test and same 0.1x factor as the Picard path, but on the
    // raw residual -- Newton has no relaxation to undo.
    bool ok = true;
    for (int j = op->indLinN; j--;) {
      int k = op->indLin[j];
      if (fabs(r[k]) >= RX_INDLIN_PICARD_TOL_FAC*(rtol[k]*fabs(gw[k]) + atol[k])) {
        ok = false;
        break;
      }
    }
    if (ok) {
      std::copy(gw.begin(), gw.end(), yp_);
      return 1;
    }
    // A residual that keeps growing means the step is too long; report how badly
    // so the driver can size its cut, exactly as the Picard ratio does.
    //
    // Two consecutive increases, not one -- the same rule the Picard path uses
    // (`nGrow >= 2`).  Newton's residual is legitimately non-monotone early,
    // especially with a chord Jacobian, and bailing on the first bump makes the
    // driver cut steps it did not need to: on van der Pol at mu = 1000 that
    // turned a win into a 2.3x loss, with cut steps going UP.
    //
    // On the first bump, refresh the chord Jacobian at the current iterate
    // before giving up on it (a Shamanskii step).  A stale Jacobian is the
    // likely cause of the stall on a stiff problem, and refreshing costs
    // 2*indLinN cheap IndF evaluations against a whole rejected step.
    if (resPrev > 0.0 && res > resPrev) {
      nGrow++;
      if (nGrow >= 2) {
        if (ratioOut != NULL) {
          double q = res/resPrev;
          *ratioOut = (q > 1.0) ? q : 1.0;
        }
        std::copy(gw.begin(), gw.end(), yp_);
        return -2;
      }
      if (!refreshed) {
        refreshed = true;
        indLinForcingJacFd(cSub, op, neq, tcov, subTf, gw.memptr(), IndF,
                           Jf, yPert, fPlus, fMinus);
        G = -Ph*Jf;
        G.diag() += 1.0;
        haveFact = arma::inv(Ginv, G);
      }
    } else {
      nGrow = 0;
    }
    resPrev = res;
    if (haveFact) {
      dw = Ginv*r;
      w += dw;
    } else {
      w = gw;                          // degrade to a Picard step rather than stall
    }
  }
  std::copy(w.begin(), w.end(), yp_);
  return -2;
}

static int indLinIterate(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                         int neq, double *rtol, double *atol, int maxIter, int stepSearch,
                         double *yp_, double subTp, double subTf, double tcov,
                         double *InfusionRate_, int *on_, t_ME ME, t_IndF IndF,
                         arma::vec *u, arma::vec *w1Out, double *ratioOut) {
  arma::vec y0(yp_, neq);
  arma::vec w(y0);
  arma::vec wPrev(neq), d(neq), dPrev(neq), wTry(neq);
  double theta = 1.0, thetaPrev = 0.0;
  double resPrev = -1.0;
  int nGrow = 0;
  if (ratioOut != NULL) *ratioOut = 1.0;
  for (int i = 0; i < maxIter; ++i) {
    rxIndLinCountIter(1);
    wPrev = w;
    // Pass 0 evaluates at the step START, in time as well as in state; every
    // later pass evaluates at the end.  Both halves of the quadrature have to
    // move together or the average cancels the state error and leaves the
    // explicit-time error behind, which drops the step back to first order for
    // any forcing that reads `t`.
    int ret = indLinPass(cSub, op, ind, w.memptr(), y0.memptr(), subTp, subTf, tcov,
                         (i == 0) ? subTp : subTf,
                         InfusionRate_, on_, ME, IndF, u);
    if (ret <= 0) return ret;
    // Pass 0 linearizes at the substep-start state: that is the forward
    // (explicit) answer, and the caller differences it against the converged
    // backward one to get a local error estimate for free.
    if (i == 0 && w1Out != NULL) *w1Out = w;
    d = w - wPrev;
    for (int j = op->indLinN; j--;) {
      if (!R_FINITE(w[op->indLin[j]])) {
        std::copy(w.begin(), w.end(), yp_);
        return -2;
      }
    }
    // Pick the relaxation BEFORE testing, because the test needs it.  For a
    // map contracting at ratio g', the distance from the current iterate to
    // the fixed point is about |d|/(1-g') = theta*|d|, not |d| -- at g'=0.9
    // those differ by 10x, so testing the bare residual accepts an iterate
    // ten times further out than the tolerance asked for.
    // A residual that is growing means the secant fit is misleading -- `g` is
    // not normal, so a single rise is normal and only a run of them is real.
    // Fall back to plain Picard once, then hand off to a shorter step.  This is
    // the safeguard role Armijo backtracking plays in Schmidt et al. (2024),
    // but read off the residual we already have rather than paid for with extra
    // matrix exponentials.
    double res = indLinResNorm(op, rtol, atol, w, d);
    bool backOff = false;
    if (resPrev >= 0.0 && res > 2.0*resPrev) {
      if (++nGrow >= 2) {
        std::copy(w.begin(), w.end(), yp_);
        return -2;
      }
      backOff = true;
    } else {
      nGrow = 0;
    }
    resPrev = res;

    if (backOff || stepSearch == RX_INDLIN_SEARCH_NONE) {
      theta = 1.0;
    } else if (stepSearch == RX_INDLIN_SEARCH_EXACT) {
      // One extra propagation, at the plain-Picard point, gives the residual
      // there.  The residual is affine in the step for an affine map, so two of
      // them locate the minimizer of ||R(a)|| in closed form -- the same thing
      // Schmidt et al.'s bisection converges to, without spending a matrix
      // exponential per bisection.
      wTry = w;
      int r2 = indLinPass(cSub, op, ind, wTry.memptr(), y0.memptr(), subTp, subTf, tcov,
                          subTf, InfusionRate_, on_, ME, IndF, u);
      if (r2 <= 0) return r2;
      double num = 0.0, den = 0.0;
      for (int j = op->indLinN; j--;) {
        int k = op->indLin[j];
        double sc = rtol[k]*fabs(w[k]) + atol[k];
        if (sc <= 0.0) continue;
        double a0 = d[k]/sc;
        double a1 = (wTry[k] - w[k] - d[k])/sc;
        num += a0*a1;
        den += a1*a1;
      }
      theta = (den > 0.0 && R_FINITE(num)) ? -num/den : 1.0;
      if (!R_FINITE(theta) || theta < RX_INDLIN_THETA_MIN) theta = RX_INDLIN_THETA_MIN;
      if (theta > RX_INDLIN_THETA_MAX) theta = RX_INDLIN_THETA_MAX;
    } else {
      theta = (thetaPrev > 0.0) ? indLinTheta(op, rtol, atol, w, d, dPrev, thetaPrev) : 1.0;
    }
    // Report the measured contraction ratio so a caller that has to cut the
    // step can size the cut instead of guessing.
    if (ratioOut != NULL) {
      double r = fabs(1.0 - 1.0/theta);
      *ratioOut = (R_FINITE(r) && r > 1.0) ? r : 1.0;
    }
    bool converge = true;
    for (int j = op->indLinN; j--;) {
      int k = op->indLin[j];
      if (fabs(theta*d[k]) >=
          RX_INDLIN_PICARD_TOL_FAC*(rtol[k]*fabs(w[k]) + atol[k])) {
        converge = false;
      }
    }
    if (converge) {
      std::copy(w.begin(), w.end(), yp_);
      return 1;
    }
    if (theta != 1.0) w = wPrev + theta*d;
    dPrev = d;
    thetaPrev = theta;
  }
  std::copy(w.begin(), w.end(), yp_);
  return -2;
}

// One adaptive attempt over `[t, t+h]` starting from `y0`.  Leaves the
// propagated state in `yOut` and the scaled local error estimate in `*errOut`.
//
// The estimate is free, and it is a quadrature error in disguise.  Over the
// step the exact solution is
//
//   y(h) = exp(A h) y0 + int_0^h exp(A (h-s)) g(s) ds ,   g(s) = f(y(s))
//
// and freezing the forcing replaces that integral by `phi(h) g(.)` with
// `phi(h) = int_0^h exp(A (h-s)) ds`.  Freezing it at the start (pass 0 of the
// iteration, and exactly what the non-iterating codes 1/2 compute) is the
// left-endpoint rule; freezing it at the converged iterate is the right-endpoint
// rule.  Their errors are the two ends of the same trapezoid:
//
//   w1 - exact = -(h^2/2) g'(0) + O(h^3) ,  w* - exact = +(h^2/2) g'(0) + O(h^3)
//
// so `w* - w1` is TWICE the local error of either -- hence the 0.5 below,
// without which every tolerance would silently be twice as tight as asked --
// and their average is the trapezoidal rule, which is one order better.  The
// same cancellation holds when it is the matrix rather than the forcing that is
// frozen (a sensitivity model built by rxSensMatExp(), where `A` still reads the
// states); only the derivative being expanded changes.
static int indLinTrySubstep(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                            int neq, double *rtol, double *atol,
                            const arma::vec &y0, double t, double h, double tcov,
                            double *InfusionRate_, int *on_, t_ME ME, t_IndF IndF,
                            arma::vec *u, arma::vec &yOut, arma::vec &w1,
                            double *errOut, double *ratioOut) {
  int maxIter = (op->indLinMaxIter > 0) ? op->indLinMaxIter : RX_INDLIN_MAXITER;
  yOut = y0;
  // Selector is an environment variable for now; Phase 4 turns it into the
  // `indLinIteration` control.  Read per call so a test can toggle it.
  int ret;
  if (__indLinUseNewton) {
    ret = indLinNewton(cSub, op, ind, neq, rtol, atol, maxIter,
                       yOut.memptr(), t, t + h, tcov,
                       InfusionRate_, on_, ME, IndF, u, &w1, ratioOut);
  } else {
    ret = indLinIterate(cSub, op, ind, neq, rtol, atol, maxIter, op->indLinStepSearch,
                        yOut.memptr(), t, t + h, tcov,
                        InfusionRate_, on_, ME, IndF, u, &w1, ratioOut);
  }
  if (ret != 1) return ret;
  double err = 0.0;
  for (int k = 0; k < neq; ++k) {
    // Every state, not just the flagged ones: the linearization error
    // propagates into compartments that carry no forcing of their own.
    double sc = atol[k] + rtol[k]*std::max(fabs(y0[k]), fabs(yOut[k]));
    if (sc <= 0.0) continue;
    double e = 0.5*(yOut[k] - w1[k])/sc;
    err += e*e;
  }
  *errOut = sqrt(err / (double) neq);
  // Advance on the average of the two.  Their leading errors are equal and
  // opposite, so the average cancels them and is second order where either
  // alone is first -- for free, since both are already in hand.  This is local
  // extrapolation, the same trick dop853 uses: the step is still sized from the
  // first-order estimate above, but what gets propagated is the better answer.
  // The point is not just accuracy: it changes how accuracy scales with the
  // tolerance, from sqrt(tol) to tol, which is what takes the step count down.
  // Both terms are exp(A*h)*y0 with a Metzler A, so a nonnegative state stays
  // nonnegative under the average.
  for (int k = 0; k < neq; ++k) yOut[k] = 0.5*(yOut[k] + w1[k]);
  return 1;
}

// One adaptive attempt, at whatever order was asked for.
//
// The default form advances on the average (second order) and sizes the step
// from the first-order estimate.  The Richardson form runs that same thing once
// at `h` and twice at `h/2`: for a second-order method the two-half-step answer
// has a quarter the local error of the one-step answer, so their difference
// over three estimates it, and `(4*two - one)/3` cancels it -- third order.
// Both are local extrapolation; what differs is the order of the estimate the
// step is sized from, hence the controller exponent in the caller.
//
// It costs three fixed-point solves per step instead of one, which pays for
// itself only when the tolerance is tight -- measured at roughly 3x fewer
// matrix exponentials at 1e-3 and 12x at 1e-6 -- so it is off by default.
// `nSub` equal substeps of `h/nSub` across `[t, t+h]`, leaving the result in
// `out`.  `cur` is scratch for the running state.  The last substep ends
// exactly at `t+h` rather than accumulating `nSub` additions of `h/nSub`.
static int indLinChain(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                       int neq, double *rtol, double *atol,
                       const arma::vec &y0, double t, double h, int nSub, int locf,
                       double *InfusionRate_, int *on_, t_ME ME, t_IndF IndF,
                       arma::vec *u, arma::vec &out, arma::vec &w1, arma::vec &cur,
                       double *ratioOut) {
  const double hs = h / (double) nSub;
  double rMax = 1.0;
  cur = y0;
  for (int i = 0; i < nSub; ++i) {
    double ts = t + ((double) i)*hs;
    double te = (i == nSub - 1) ? (t + h) : (ts + hs);
    double e = 0.0, r = 1.0;
    int ret = indLinTrySubstep(cSub, op, ind, neq, rtol, atol, cur, ts, te - ts,
                               locf ? ts : te,
                               InfusionRate_, on_, ME, IndF, u, out, w1, &e, &r);
    if (r > rMax) rMax = r;
    if (ret != 1) { *ratioOut = rMax; return ret; }
    cur = out;
  }
  *ratioOut = rMax;
  return 1;
}

// One step, extrapolated to the requested level.
//
//   level 0  the plain averaged substep                    2nd order,  1 solve
//   level 1  Richardson on h, h/2                          3rd order,  3 solves
//   level 2  Romberg column on h, h/2, h/4                 4th order,  7 solves
//   level 3  Romberg column on h, h/2, h/4, h/8            5th order, 15 solves
//
// The base step is second order, so entry j of the tableau is built from
// T(h/2^j) and the leading error term after k eliminations goes as h^(2+k):
// R[k][j] = (f*R[k-1][j+1] - R[k-1][j])/(f-1) with f = 2^(k+1).  The error
// estimate is the difference between the two highest entries, which is the
// standard Romberg estimate.
#define RX_INDLIN_RICH_MAXLVL 3

static int indLinTryStep(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                         int neq, double *rtol, double *atol,
                         const arma::vec &y0, double t, double h, int locf, int useRich,
                         double *InfusionRate_, int *on_, t_ME ME, t_IndF IndF,
                         arma::vec *u, arma::vec &yOut, arma::vec &w1,
                         arma::vec &yScratch, arma::mat &tab,
                         double *errOut, double *ratioOut) {
  double tcov = locf ? t : (t + h);
  if (useRich <= 0) {
    return indLinTrySubstep(cSub, op, ind, neq, rtol, atol, y0, t, h, tcov,
                            InfusionRate_, on_, ME, IndF, u, yOut, w1,
                            errOut, ratioOut);
  }
  if (useRich > RX_INDLIN_RICH_MAXLVL) useRich = RX_INDLIN_RICH_MAXLVL;
  const int nEntry = useRich + 1;          // T(h) ... T(h/2^useRich)
  double rMax = 1.0;
  // Column of base-method results, one per subdivision.
  for (int j = 0; j < nEntry; ++j) {
    double r = 1.0;
    int nSub = 1 << j;
    int ret = indLinChain(cSub, op, ind, neq, rtol, atol, y0, t, h, nSub, locf,
                          InfusionRate_, on_, ME, IndF, u, yOut, w1, yScratch, &r);
    if (r > rMax) rMax = r;
    if (ret != 1) { *ratioOut = rMax; return ret; }
    for (int k = 0; k < neq; ++k) tab(k, j) = yOut[k];
  }
  *ratioOut = rMax;
  // Neville-Aitken sweep in place; column j holds R[k][j] after pass k.
  for (int k = 1; k <= useRich; ++k) {
    double f = (double)(1 << (k + 1));     // 4, 8, 16 ... for a 2nd-order base
    for (int j = 0; j + k < nEntry; ++j) {
      for (int q = 0; q < neq; ++q) {
        tab(q, j) = (f*tab(q, j + 1) - tab(q, j))/(f - 1.0);
      }
    }
  }
  // After the sweep tab(.,0) is the highest entry.  The last pass only touched
  // j = 0, so tab(.,1) still holds the previous column's entry, one order
  // lower -- their difference is the estimate.  At level 1 this reduces to
  // (T(h/2) - T(h))/3, which is what the two-entry Richardson used.
  double err = 0.0;
  for (int q = 0; q < neq; ++q) {
    double best = tab(q, 0);
    double sc = atol[q] + rtol[q]*std::max(fabs(y0[q]), fabs(best));
    double e = best - tab(q, (nEntry > 1) ? 1 : 0);
    yOut[q] = best;
    if (sc <= 0.0) continue;
    e /= sc;
    err += e*e;
  }
  *errOut = sqrt(err / (double) neq);
  return 1;
}

// Adaptive relinearization over `[tp,tf]` for the iterating codes 3/4.
//
// `hCap` is the equal-subdivision substep the non-adaptive path would have
// used, so the controller can only ever step SHORTER than before -- anyone who
// tuned `hmax` keeps at least the accuracy they had, and every old substep
// boundary is still a boundary (which is what keeps time-varying covariate
// sampling a refinement rather than a change).
// -- Step-disposition diagnostics ---------------------------------------------
//
// `$counts` is full: slvr counts accepted steps and dadt/jac already carry the
// exponentials computed and reused.  These answer a different question -- WHY a
// step was retried.  A step cut because the fixed-point iteration would not
// contract is one the error controller would have allowed, so this count is the
// ceiling on what replacing that iteration can win; a step rejected on error is
// not.  Read with `rxIndLinSteps()`, which also resets.
//
// Plain `long` rather than atomics: `par_indLin` is forced single-threaded
// (`solveMethodThreadSafe`), and these are diagnostics, so a torn count under a
// future threaded build would mislead but not corrupt.  Revisit if indLin ever
// becomes reentrant.
static long __indLinNAttempt = 0;
static long __indLinNAccept  = 0;
static long __indLinNRejErr  = 0;   // rejected: local error estimate too large
static long __indLinNCutConv = 0;   // cut: iteration did not converge (ret == -2)
static long __indLinNIter    = 0;   // total iteration passes over all substeps

extern "C" void rxIndLinCountIter(int n) { __indLinNIter += n; }

static int indLinDriveAdaptive(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                               int neq, double *rtol, double *atol,
                               double *yp_, double tp, double tf, double hCap, int locf,
                               double *InfusionRate_, int *on_, t_ME ME, t_IndF IndF,
                               arma::vec *u) {
  const double SAFE = 0.9, FACMIN = 0.1, FACMAX = 5.0;
  double span = tf - tp;
  double t = tp;
  double h = (hCap > 0.0 && std::isfinite(hCap) && hCap < span) ? hCap : span;
  bool lastRejected = false;
  // Steady state re-solves the same tau-sized interval until it stops moving.
  // If the substep schedule drifted between passes the ssRtol/ssAtol test
  // would be reading schedule jitter rather than convergence, so allow the
  // step to shrink there but never to grow.
  bool inSS = (ind != NULL && !ISNA(ind->ssTime));
  arma::vec y0(yp_, neq), yTry(neq), w1(neq), yScratch(neq);
  arma::mat richTab(neq, RX_INDLIN_RICH_MAXLVL + 1);
  // "auto" starts second order and turns Richardson on once this interval has
  // needed enough steps to pay for it, so a loose tolerance never carries the
  // extra cost and a tight one is not left crawling.  The switch is one way
  // within an interval, so it cannot chatter.
  int useRich = (op->indLinRichardson == RX_INDLIN_RICH_ALWAYS) ? 1 : 0;
  if (op->indLinRichardson == RX_INDLIN_RICH_ALWAYS4) useRich = 2;
  if (op->indLinRichardson == RX_INDLIN_RICH_ALWAYS5) useRich = 3;
  int autoRich = (op->indLinRichardson == RX_INDLIN_RICH_AUTO);
  int nAttempt = 0, nAccept = 0;
  while (t < tf) {
    // Snap to `tf` rather than leaving a sliver behind.
    if (t + 1.01*h >= tf) h = tf - t;
    if (h <= 0.0) break;
    __indLinNAttempt++;
    if (++nAttempt > op->mxstep) {
      std::copy(y0.begin(), y0.end(), yp_);
      if (ind != NULL) ind->err |= rxErrIndLinConverge;
      return 1;
    }
    // Decide from the step the controller has settled on rather than by
    // burning the switch-over count first: if crossing what is left of the
    // interval at this step would take more than the break-even number of
    // steps, Richardson is already the cheaper way to finish.
    if (autoRich && nAccept > 0 && h > 0.0) {
      double nLeft = (tf - t)/h;
      // Same break-even argument one level up.  A p-th order step needs about
      // N^(2/p) of the second-order step's N, so the fourth-order column
      // (7 solves) beats the third (3 solves) once 7*N^(1/2) < 3*N^(2/3),
      // i.e. once N > (7/3)^6.
      if (useRich < 3 && nLeft > (double) RX_INDLIN_AUTO_RICH5_N) {
        useRich = 3;
      } else if (useRich < 2 && nLeft > (double) RX_INDLIN_AUTO_RICH4_N) {
        useRich = 2;
      } else if (useRich < 1 && nLeft > (double) RX_INDLIN_AUTO_RICH_N) {
        useRich = 1;
      }
    }
    // The step is sized from an estimate of the order the extrapolation is
    // built on: first order for the plain step, second under Richardson.  The
    // exponent is -1/(p_est + 1) either way.
    double expo = -1.0/((double) (useRich + 2));
    double err = 0.0, ratio = 1.0;
    int ret = indLinTryStep(cSub, op, ind, neq, rtol, atol, y0, t, h, locf, useRich,
                            InfusionRate_, on_, ME, IndF, u, yTry, w1,
                            yScratch, richTab, &err, &ratio);
    if (ret == -2) {
      __indLinNCutConv++;
      // Not a dead end: the contraction ratio is proportional to `h`, so a
      // failure to converge proves the step is too long and a bounded number
      // of cuts must fix it.  Size the cut from the measured ratio instead of
      // halving blindly -- lsoda's corrector-failure convention.
      double cut = 1.0/(2.0*ratio);
      if (cut > 0.25) cut = 0.25;
      if (cut < FACMIN) cut = FACMIN;
      h *= cut;
      lastRejected = true;
      if (h < 1e-10*span) {
        std::copy(y0.begin(), y0.end(), yp_);
        if (ind != NULL) ind->err |= rxErrIndLinConverge;
        return 1;
      }
      continue;
    }
    if (ret <= 0) {
      std::copy(y0.begin(), y0.end(), yp_);
      return ret;
    }
    double fac = (err > 0.0) ? SAFE*pow(err, expo) : FACMAX;
    if (fac < FACMIN) fac = FACMIN;
    if (fac > FACMAX) fac = FACMAX;
    if (err > 1.0) {
      __indLinNRejErr++;
      h *= fac;
      lastRejected = true;
      if (h < 1e-10*span) {
        std::copy(y0.begin(), y0.end(), yp_);
        if (ind != NULL) ind->err |= rxErrIndLinConverge;
        return 1;
      }
      continue;
    }
    y0 = yTry;
    t += h;
    nAccept++;
    __indLinNAccept++;
    // A step that follows a rejection may not grow, and a steady-state step
    // may never grow at all.
    if ((lastRejected || inSS) && fac > 1.0) fac = 1.0;
    lastRejected = false;
    h *= fac;
    if (hCap > 0.0 && std::isfinite(hCap) && h > hCap) h = hCap;
  }
  std::copy(y0.begin(), y0.end(), yp_);
  // `postSolve()` already counts one per output interval; add the substeps
  // this interval actually needed so `$counts$slvr` reports real work.
  if (ind != NULL && ind->slvr_counter != NULL && nAccept > 1) {
    ind->slvr_counter[0] += nAccept - 1;
  }
  return 1;
}

//' Inductive linearization solver
//'
//' @param cSub = Current subject number
//' @param op - rxode2 solving options
//' @param tp - Prior time point/time zero
//' @param yp - Prior state;  vector size = neq; Final state is updated here
//' @param tf - Final Time
//' @param InfusionRate = Rates of each compartment;  vector size = neq
//' @param on Indicator for if the compartment is "on"
//' @param cache
//'    0 = no Cache
//'    When doIndLin == 0, cache > 0 = nInf-1
//' @param ME the rxode2 matrix exponential function
//' @param IndF The rxode2 Inductive Linearization function F
//'
//' @return Returns a status for solving
//'
//'   1 = Successful solve
//'
//'   -1 = Maximum number of iterations reached when doing
//'        inductive linearization
//' @name rxIndLin_
//' @noRd
extern "C" int indLin(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                      double tp, double *yp_, double tf,
		      double *InfusionRate_, int *on_,
		      t_ME ME, t_IndF  IndF){
  int neq = (ind != NULL) ? rxEffNeq(ind, op) : op->neq;
  // Use per-individual tolerance arrays when available (set by
  // _setIndPointersByThread + iniSubject), falling back to op->rtol2/atol2.
  double *rtol = (ind != NULL && ind->rtol2 != NULL) ? ind->rtol2 : op->rtol2;
  double *atol = (ind != NULL && ind->atol2 != NULL) ? ind->atol2 : op->atol2;
  // `op->mxstep` is the attempted-substep budget for the whole interval; the
  // per-substep iteration cap is RX_INDLIN_MAXITER, which is much smaller.
  int doIndLin=op->doIndLin;
  // int indLinPerterb=10;
  // double indLinAmt=1.0;
  // int phiM=op->indLinPhiM;
  // double phiTol=op->indLinPhiTol;
  // double phiAnorm = op->indLinPhiAnorm;

  int locf=(op->is_locf!=2);
  // Relinearization step cap: indLin's premise is a CONSTANT Jacobian/ME
  // over each `meOnly()` call's interval, evaluated ONCE at the interval's
  // start (`ME(cSub, tcov, tf, ..., yc_)` uses the CURRENT state) -- exact
  // for a true (state-independent) matExp() model, but only a first-order
  // approximation for a state-dependent (indLin-forcing, e.g. Michaelis-
  // Menten) one.  Previously this whole `[tp,tf]` interval -- exactly the
  // gap between the caller's requested output times -- was treated as ONE
  // relinearization step with NO internal refinement at all, so `hmax`
  // (and `ind->HMAX`'s auto-computed default) were silently ignored:
  // solving with a coarser output/sampling grid gave a coarser, silently
  // WRONG answer for nonlinear indLin models, with no way for a user to
  // ask for more accuracy short of resampling their own output times.
  // Found while investigating task #8 (matExp+indLin primal-trajectory
  // accuracy for MM elimination): error scaled linearly with output grid
  // spacing (a classic "zero internal step refinement" signature), and
  // `hmax` had ZERO effect on the result at any value. Fixed by honoring
  // `HMAX` (per-subject, `ind->HMAX`, falling back to `op->hmax2` when
  // `ind` is unavailable -- same fallback pattern as `rtol`/`atol` above)
  // as a genuine relinearization step cap: subdivide `[tp,tf]` into equal
  // substeps no longer than `HMAX`, re-evaluating `ME`/`IndF` (via a fresh
  // `meOnly()` call using the just-updated state) at each substep boundary.
  // For a true matExp() model (state-independent ME) this changes nothing
  // but the number of (mathematically equivalent) matrix exponentials
  // computed; for indLin-forcing models it is the actual fix.
  // A zero-length or backward interval has nothing to subdivide; bail before
  // the arithmetic below can divide by it.
  if (!(tf > tp)) return 1;
  double _hmax = (ind != NULL) ? ind->HMAX : op->hmax2;
  int _nSub = 1;
  if (_hmax > 0.0 && std::isfinite(_hmax) && (tf - tp) > _hmax) {
    _nSub = (int) std::ceil((tf - tp) / _hmax);
    if (_nSub < 1) _nSub = 1;
  }
  double _dt = (tf - tp) / (double) _nSub;
  double _subTp = tp;
  int _ret = 1;
  arma::vec u;
  if (doIndLin == 2 || doIndLin == 4) {
    u.zeros(neq);
  }
  if (doIndLin == 3 || doIndLin == 4) {
    // The iterating codes pick their own substep from a local error estimate,
    // capped by the equal subdivision the fixed grid would have used.  Codes
    // 1/2 fall through to that fixed grid: after rxode2#1186 their `A` is
    // constant in the states, so there is no truncation error to control.
    return indLinDriveAdaptive(cSub, op, ind, neq, rtol, atol, yp_, tp, tf, _dt, locf,
                               InfusionRate_, on_, ME, IndF,
                               (doIndLin == 4) ? &u : NULL);
  }
  for (int _sub = 0; _sub < _nSub; _sub++) {
    // Avoid floating-point drift on the final substep by snapping to `tf`.
    double _subTf = (_sub == _nSub - 1) ? tf : _subTp + _dt;
    double tcov = locf ? _subTp : _subTf;
    switch(doIndLin){
    case 1: {
      _ret = meOnly(cSub, yp_, yp_, _subTp, _subTf, tcov, _subTf, InfusionRate_, on_, ME, op, ind);
      break;
    }
    case 2: {
      // Evaluate the forcing at the interval-start state, the same vector
      // `meOnly()` hands to `ME` below (it is `meOnly()` that advances `yp_`).
      IndF(cSub, tcov, _subTf, u.memptr(), yp_);
      _ret = meOnly(cSub, yp_, yp_, _subTp, _subTf, tcov, _subTf, u.memptr(), on_, ME, op, ind);
      break;
    }
    default:
      // Never throw: this runs inside `par_indLin`'s `omp parallel for`, and an
      // Rcpp exception escaping a worker thread is a confirmed session crash
      // (see the `.indLinInfo` leak note in R/rxode2.R) rather than an error.
      // Report through `err` and let the caller NA-fill.
      if (ind != NULL) ind->err |= rxErrIndLinCode;
      return -1;
    }
    if (_ret <= 0) return _ret;
    _subTp = _subTf;
  }
  return _ret;
  // if (doIndLin == 0){
  //   // Total possible enhanced matrix is (neq+neq)x(neq+neq)
  //   // Total possible initial value is (neq+neq)
  //   // expAt is (neq+neq)x(neq+neq)
  //   // Total possible output is (neq+neq)
  //   // =4*neq + 8*neq^2
  //   // These are simple linear with no f
  //   // Hence there is no need for matrix inversion
  // }
  // else {
  //   // In this case the inital matrix should not be expanded. The
  //   // infusions are put into the F function
  //   const arma::vec InfusionRate(InfusionRate_, neq, false, false);
  //   arma::vec yp(yp_, neq, false, false);
  //   arma::vec u(neq);
  //   arma::vec extra(neq,arma::fill::zeros);
  //   arma::vec w(neq);
  //   arma::vec wLast(neq);
  //   double *fptr = u.memptr();
  //   if (doIndLin==1){
  //     // For LOCF tp for NOCB tf
  //     // IndF(cSub, tcov, tf, fptr, wLast.memptr(), InfusionRate_);
  //     IndF(cSub, tcov, tf, fptr, yp_, InfusionRate_);
  //     wLast = phiv((tf-tp), m0, u, yp, op);
  //     // For inhomogenous systems we can return here.
  //     std::copy(wLast.begin(), wLast.end(), &yp_[0]);
  //     return 1;
  //   }
  //   IndF(cSub, tcov, tf, fptr, wLast.memptr(), InfusionRate_,extra.memptr());
  //   w=phiv((tf-tp), m0, u, yp, op);
  //   bool converge = false;
  //   Rprintf("tf: %f:\n",tf);
  //   for (int i = 0; i < maxsteps; ++i){
  //     converge=true;
  //     for (int j=neq;j--;){
  //   	if (fabs(w[j]-wLast[j]) >= rtol[j]*fabs(w[j])+atol[j]){
  //   	  converge = false;
  //   	  break;
  //   	}
  //     }
  //     if (converge){
  //   	break;
  //     }
  //     wLast = w+DOUBLE_EPS; // Try to break out of infinite loop.
  //     IndF(cSub, tcov, tf, fptr, wLast.memptr(), InfusionRate_,extra.memptr());
  //     w=phiv((tf-tp), m0, u, yp, op);
  //     print(wrap(w.t()));
  //   }
  //   if (!converge){
  //     Rprintf("Did not converge!");
  //     std::copy(w.begin(), w.end(), &yp_[0]);
  //     // std::fill_n(&yp_[0], neq, NA_REAL);
  //     return 1;
  //   } else {
  //     std::copy(w.begin(), w.end(), &yp_[0]);
  //     return 1;
  //   }
  // }
  return 1;
}

// Step dispositions for the last solve (or since the last read), as a named
// integer vector; reading resets, so a measurement is one call before and the
// numbers after.  `cutConv` is the count that matters: those are steps the
// error controller would have accepted but the fixed-point iteration could not
// converge on, so it bounds what a better iteration can recover.
extern "C" SEXP _rxode2_rxIndLinSteps(void) {
  rxProtect rx_protect;
  SEXP ret = rx_protect.protect(Rf_allocVector(REALSXP, 5));
  SEXP nm  = rx_protect.protect(Rf_allocVector(STRSXP, 5));
  REAL(ret)[0] = (double) __indLinNAttempt;
  REAL(ret)[1] = (double) __indLinNAccept;
  REAL(ret)[2] = (double) __indLinNRejErr;
  REAL(ret)[3] = (double) __indLinNCutConv;
  REAL(ret)[4] = (double) __indLinNIter;
  SET_STRING_ELT(nm, 0, Rf_mkChar("attempt"));
  SET_STRING_ELT(nm, 1, Rf_mkChar("accept"));
  SET_STRING_ELT(nm, 2, Rf_mkChar("rejErr"));
  SET_STRING_ELT(nm, 3, Rf_mkChar("cutConv"));
  SET_STRING_ELT(nm, 4, Rf_mkChar("iter"));
  Rf_setAttrib(ret, R_NamesSymbol, nm);
  __indLinNAttempt = __indLinNAccept = __indLinNRejErr = 0;
  __indLinNCutConv = __indLinNIter = 0;
  return ret;
}
