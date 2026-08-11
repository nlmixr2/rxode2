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
// Content addressing needs no such bookkeeping -- a rate matrix that moves
// between substeps (one reading `t` or a time-varying covariate) simply never
// hits, and an infusion starting or stopping changes the augmented dimension,
// which is a key mismatch.
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
// `indLinIteration` codes.  Picard is the historical scheme and the cheapest
// per step; the other two exist for stiff forcings, where Picard's contraction
// condition -- not the error controller -- is what limits the step.
#define RX_INDLIN_ITER_PICARD 0
#define RX_INDLIN_ITER_NEWTON 1
#define RX_INDLIN_ITER_EXPRB  2
#define RX_INDLIN_ITER_AUTO   3
// exprb32 carries its own embedded pair, so unlike exprb2 it does not need the
// extrapolation column to produce an error estimate and is not held at a
// minimum Romberg level.
#define RX_INDLIN_ITER_EXPRB32 4
// How many steps `auto` must see cut for non-convergence before it switches.
// One cut can be a transient at a dose event; a short run of them is the
// signal that the iteration, not the tolerance, is setting the step.  Measured
// crossover on van der Pol is between mu = 10 (2.6% of attempts cut) and
// mu = 100 (32.5%), so this only has to be above the noise.
#define RX_INDLIN_AUTO_ITER_CUTS 8
// Lowest Romberg level exprb may run at.  exprb2 has no embedded pair, so its
// error estimate comes from the extrapolation column; at level 1 that estimate
// is unreliable -- on an ordinary Michaelis-Menten subject the delivered error
// plateaus at 4e-6 across three decades of requested tolerance and the step
// count is non-monotone in it.  Level 2 and above track the tolerance.
#define RX_INDLIN_EXPRB_MINRICH 2

// `auto`'s decision has to outlive the interval that made it.
// `indLinDriveAdaptive` runs once per OUTPUT INTERVAL, so a switch held in a
// local resets at every observation: on van der Pol over one period that meant
// re-learning stiffness 200 times and paying the first 8 cuts each time (1592
// cuts against pure exprb's 1).  Keep it per thread, keyed by subject, and
// reset it when the subject changes.
//
// Per thread rather than on `ind`, because a field there is an ABI change to a
// struct downstream packages compile against; this is solver-internal state
// with no reason to be visible.
typedef struct {
  int cSub;      // subject this decision belongs to; -1 = unset
  int scheme;
  int nCut;
  int rich;      // Romberg level earned so far, for the same reason as `scheme`
} indLinAutoState_t;
static std::vector<indLinAutoState_t> __indLinAutoState;

static inline indLinAutoState_t *indLinAutoFor(int cSub) {
  if (__indLinAutoState.empty()) return NULL;
  indLinAutoState_t *a =
    &__indLinAutoState[rx_get_thread((int)__indLinAutoState.size())];
  if (a->cSub != cSub) {
    a->cSub = cSub;
    a->scheme = RX_INDLIN_ITER_PICARD;
    a->nCut = 0;
    a->rich = 0;
  }
  return a;
}

// Sized before the parallel region, from rxData.cpp, alongside the other pools.
extern "C" void ensureIndLinExpCache(int nCores) {
  // Force-miss switch: every lookup fails, so "is this a cache bug?" is one run
  // rather than a bisect.  Read here so it is live per solve.
  __indLinExpCacheOff = (getenv("RXODE2_INDLIN_NO_EXP_CACHE") != NULL);
  if ((int)__indLinExpCache.size() < nCores) {
    __indLinExpCache.resize(nCores);
  }
  if ((int)__indLinAutoState.size() < nCores) {
    __indLinAutoState.resize(nCores);
  }
  for (int i = 0; i < (int)__indLinAutoState.size(); i++) {
    __indLinAutoState[i].cSub = -1;      // nothing survives into another solve
    __indLinAutoState[i].scheme = RX_INDLIN_ITER_PICARD;
    __indLinAutoState[i].nCut = 0;
    __indLinAutoState[i].rich = 0;
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
  __indLinAutoState.clear();
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
// phi_p(z) = sum_{k>=0} z^k/(k+p)!, by Horner:
//   P = I/(m+p)!;  for k = m..1:  P = I/(k+p-1)! + z*P;  then h*phi_p = h*P.
// `p = 1` is P(h) itself; `p = 2` is what the linear-ramp forcing (rxode2#1191)
// applies to the forcing INCREMENT, and is the same series one term along.
#define RX_INDLIN_PHI1_MAXNRM 0.5
static bool indLinPhiSeries(const arma::mat &A, double h, int p, arma::mat &Ph,
                            arma::mat &z, arma::mat &tmp) {
  const arma::uword n = A.n_rows;
  z = A*h;
  double nrm = arma::norm(z, "inf");
  if (!R_FINITE(nrm) || nrm > RX_INDLIN_PHI1_MAXNRM) return false;
  // Smallest m with nrm^(m+1)/(m+p+1)! <= 2^-53; at nrm <= 1/2, m = 16 is ample
  // for any of them, and the terms are n x n multiplies at compartmental size.
  int m = 16;
  double fk = 1.0;                       // (m+p)!
  for (int k = 2; k <= m + p; ++k) fk *= (double) k;
  Ph.eye(n, n);
  Ph /= fk;
  for (int k = m; k >= 1; --k) {
    double kf = 1.0;                     // (k+p-1)!
    for (int q = 2; q <= k + p - 1; ++q) kf *= (double) q;
    tmp = z*Ph;
    Ph = tmp;
    Ph.diag() += 1.0/kf;
  }
  Ph *= h;
  return true;
}

// h*phi_p(Ah): for p = 1 that is P(h) = A^-1(exp(Ah) - I), the operator the
// constant-forcing map applies to the forcing; for p = 2 it is what the
// linear-ramp map applies to the forcing increment.  The Horner series when
// ||A*h|| is small enough to trust it, and otherwise the exact value read out
// of the augmented exponential -- which costs a second, wider exponential, so
// it is the fallback rather than the rule.
//
// The fallback uses the same identity as everything else here: the top-right
// block of exp([[X, W],[0, K]]) with `K` the unit superdiagonal is
// sum_k phi_k(X) w_k, so `p` blocks with only the FIRST one set to the identity
// leaves phi_p(X) alone.  As in indLinExprb32() the block must be X = A*h
// exponentiated at t = 1, or the nilpotent chain is scaled along with it; at
// p = 1 the chain is empty and `h` may be passed directly.
static inline void indLinPmat(rx_solving_options *op, rx_solving_options_ind *ind,
                              int neq, const arma::mat &Aloc, double h, int p,
                              arma::mat &Ph, arma::mat &phiZ, arma::mat &phiTmp) {
  if (indLinPhiSeries(Aloc, h, p, Ph, phiZ, phiTmp)) return;
  const int m = neq*(p + 1);
  arma::mat aug(m, m, arma::fill::zeros);
  if (p == 1) {
    aug.submat(0, 0, neq-1, neq-1) = Aloc;
  } else {
    aug.submat(0, 0, neq-1, neq-1) = Aloc*h;
  }
  aug.submat(0, neq, neq-1, 2*neq-1).eye();
  for (int i = 1; i < p; ++i) {
    aug.submat(i*neq, (i+1)*neq, (i+1)*neq-1, (i+2)*neq-1).eye();
  }
  arma::mat augE(m, m);
  matrixExpCached(aug, augE, (p == 1) ? h : 1.0,
                  op->indLinMatExpType, op->indLinMatExpOrder, ind);
  Ph = augE.submat(0, m-neq, neq-1, m-1);
  if (p != 1) Ph *= h;
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
// `indLinJac` codes, matching the R-side character values.
#define RX_INDLIN_JAC_AUTO     0
#define RX_INDLIN_JAC_SYMBOLIC 1
#define RX_INDLIN_JAC_FD       2

// `global_jt == 1` is set by rxUpdateFuns() when the model's trans reports a
// "fulluser" Jacobian, i.e. when df()/dy() lines were parsed and calc_jac has a
// real body.  Without this check the symbolic path would silently return `-A`:
// an unemitted calc_jac is a stub that writes nothing, so `calc_jac - A` is
// exactly the negative rate matrix rather than the forcing Jacobian.
extern "C" int global_jt;

static inline int indLinUseSymJac(rx_solving_options *op) {
  const int have = (global_jt == 1 && calc_jac != NULL);
  switch (op->indLinJac) {
  case RX_INDLIN_JAC_FD:       return 0;
  case RX_INDLIN_JAC_SYMBOLIC: return have;   // cannot force what is not there
  default:                     return have;   // auto
  }
}

// The forcing Jacobian from the model's own analytic Jacobian.
//
// `calc_jac` is d(RHS)/dy for the WHOLE right-hand side, which in this
// splitting is `A + f'`, and `A` is state-independent by construction (a rate
// constant that reads a compartment is a parse error).  So `f' = calc_jac - A`,
// with both already compiled -- no differencing and no extra forcing
// evaluations.
//
// calc_jac writes ROW-major: parseDfdy.h emits
// `__PDStateVar__[i*(__NROWPD__) + j]` for `df(state_i)/dy(state_j)`.
// Armadillo is column-major, so reading that buffer as a matrix gives the
// TRANSPOSE and it has to be transposed back.  Getting this wrong is not
// visibly wrong: the diagonal survives, so a model whose forcing Jacobian is
// diagonal still solves, while the off-diagonal rate terms come back with the
// wrong sign and position.  On a one-compartment oral model that put a
// spurious +/- ka into the forcing Jacobian, which Newton absorbed (it
// converges to the same fixed point under any J) while exprb -- whose order
// conditions assume J is exact -- lost four orders of accuracy and took sixty
// times the steps.
static void indLinForcingJacSym(int cSub, int neq, double tcov, double tEval,
                                const double *y, t_ME ME, arma::mat &Jf,
                                arma::mat &Jfull, arma::mat &Amat) {
  Jfull.zeros(neq, neq);
  Amat.zeros(neq, neq);
  int nj[2]; nj[0] = neq; nj[1] = cSub;
  calc_jac(nj, tEval, const_cast<double*>(y), Jfull.memptr(), (unsigned int) neq);
  ME(cSub, tcov, tEval, Amat.memptr(), const_cast<double*>(y));
  Jf = Jfull.t() - Amat;
}

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

// One entry point for the forcing Jacobian, so the source is chosen in exactly
// one place.  Callers pass their own scratch; the symbolic path needs two extra
// n-by-n matrices and the finite-difference path three n-vectors, and neither
// allocates when it is not the one taken.
static void indLinForcingJac(int cSub, rx_solving_options *op,
                             int neq, double tcov, double tEval,
                             const double *y, t_IndF IndF, t_ME ME,
                             arma::mat &Jf, arma::vec &yPert,
                             arma::vec &fPlus, arma::vec &fMinus,
                             arma::mat &Jfull, arma::mat &Amat) {
  if (IndF == NULL) { Jf.zeros(neq, neq); return; }
  if (indLinUseSymJac(op)) {
    indLinForcingJacSym(cSub, neq, tcov, tEval, y, ME, Jf, Jfull, Amat);
    return;
  }
  indLinForcingJacFd(cSub, op, neq, tcov, tEval, y, IndF, Jf, yPert, fPlus, fMinus);
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

// `indLinForcing` codes, matching the R-side character values.
#define RX_INDLIN_FORCING_CONST 0
#define RX_INDLIN_FORCING_RAMP  1

// The substep with the forcing taken as the LINE through its two endpoint
// values instead of as a constant (rxode2#1191).  Over `[tp, tf]`, writing
// `h = tf - tp` and `f(s) = f0 + (s/h)(f1 - f0)`,
//
//   y(h) = exp(Ah) y0 + int_0^h exp(A(h-s)) f(s) ds
//        = exp(Ah) y0 + h*phi1(Ah) f0 + h*phi2(Ah) (f1 - f0)
//          `------------ base ------------'  `-- P2 --'
//
// which is EXACT for a forcing that really is linear over the substep, where
// `meOnly()`'s constant column is only first order.
//
// Only `f1` moves with the iterate, so `base` and `P2` are built ONCE per
// substep and a pass costs a forcing evaluation and a matrix-vector product --
// no exponential at all, against one per pass on the constant path.  Keeping
// the forcing OUT of the exponent is the point of splitting it this way:
// folding `f1` into an augmented column (the obvious single-exponential form)
// gives an operand that moves every pass, so it misses the exponential cache
// every time and costs more than it saves.
//
// `base` is `meOnly()` itself, with the substep-start forcing in place of the
// infusion rate -- the same augmented-column identity, and the same cached
// exponential the constant path takes.  `P2` = h*phi2(Ah) comes from the Horner
// series whenever ||Ah|| is small enough to trust it, so the usual case adds no
// exponential of its own.
//
// `A` is evaluated at the substep MIDPOINT.  The constant-forcing path gets its
// second order in a time-varying `A` from the caller averaging a start-linearized
// and an end-linearized answer; this path does not average, so it takes the
// midpoint rule instead.  For the ordinary case of an `A` that is constant in
// time all three agree, and the operand is then the one pass 0 already
// exponentiated.
//
// A nonnegative state stays nonnegative, as under the average: the weights
// regroup as h*(phi1 - phi2) f0 + h*phi2 f1, and for a Metzler `A` both
// phi1 - phi2 = int_0^1 exp(A h (1-s))(1-s) ds and phi2 are entrywise
// nonnegative.
//
// The converged map is symmetric -- its adjoint is itself, because
// exp(z)*phi2(-z) = phi1(z) - phi2(z) turns the two forcing weights into each
// other -- so its error expands in EVEN powers of `h` alone.  That is what
// `indLinRichardson` reads off it; see indLinSymmetric() and the tableau in
// indLinTryStep().
typedef struct {
  arma::vec base;   // exp(Ah) y0 + h*phi1(Ah) f0
  arma::mat P2;     // h*phi2(Ah), the weight on the forcing increment
  arma::vec f0;     // the substep-start forcing, fixed for the whole substep
} indLinRamp_t;

// Is the linear-ramp forcing in play?  It needs a forcing to ramp: with no
// `IndF` the forcing is the infusion rate, which is constant over the substep
// by construction, and the ramp would be the same answer for more work.
static inline bool indLinRampOn(rx_solving_options *op, const arma::vec *u) {
  return (u != NULL) && (op->indLinForcing == RX_INDLIN_FORCING_RAMP);
}

static int indLinRampBuild(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                           int neq, arma::vec &y0, const arma::vec &f0,
                           double subTp, double subTf, double tcov, int *on_,
                           t_ME ME, indLinRamp_t *rmp) {
  const double h = subTf - subTp;
  const double tMid = subTp + 0.5*h;
  rmp->f0 = f0;
  rmp->base = y0;
  const int ret = meOnly(cSub, rmp->base.memptr(), y0.memptr(), subTp, subTf, tcov,
                         tMid, rmp->f0.memptr(), on_, ME, op, ind);
  if (ret <= 0) return ret;
  arma::mat Aloc(neq, neq), phiZ(neq, neq), phiTmp(neq, neq);
  ME(cSub, tcov, tMid, Aloc.memptr(), y0.memptr());
  indLinPmat(op, ind, neq, Aloc, h, 2, rmp->P2, phiZ, phiTmp);
  return 1;
}

// One inductive-linearization pass over `[subTp, subTf]`: build the forcing
// (codes 2/4) and the matrix at `w`, propagate from `y0`, and leave the result
// in `w`.  `u` is NULL for the codes that carry no `IndF` forcing.
//
// `rmp` is the prebuilt ramp under `indLinForcing="ramp"`, and NULL for the
// constant column -- including on the pass that PRODUCES the ramp's left end,
// which is evaluated at the substep start in time as well as in state and so is
// the left-endpoint answer whatever the forcing setting.
static inline int indLinPass(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                             double *w, double *y0, double subTp, double subTf, double tcov,
                             double tEval, double *InfusionRate_, int *on_,
                             t_ME ME, t_IndF IndF, arma::vec *u,
                             const indLinRamp_t *rmp) {
  double *force = InfusionRate_;
  if (u != NULL) {
    IndF(cSub, tcov, tEval, u->memptr(), w);
    if (rmp != NULL) {
      const arma::vec out = rmp->base + rmp->P2*(*u - rmp->f0);
      std::copy(out.begin(), out.end(), w);
      return 1;
    }
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

static inline int indLinMaxIterOf(rx_solving_options *op) {
  return (op->indLinMaxIter > 0) ? op->indLinMaxIter : RX_INDLIN_MAXITER;
}

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
// 3*N^(2/3) < N, i.e. once N > 3^3 = 27.  And once more for the fourth-order
// column: 7*N^(1/2) < 3*N^(2/3) once N > (7/3)^6 ~ 161; and the fifth (15
// solves) beats the fourth (7) once 15*N^(2/5) < 7*N^(1/2), N > (15/7)^10 ~
// 1750.
//
// Those derived values are kept for exprb and REPLACED for Picard.  The
// derivation assumes every level runs the same base step at three times the
// cost of the one below, which is true of Picard and not of exprb -- exprb does
// not iterate, forms a Jacobian per step, and carries a floor of level 2.
// Measured on 200-subject work-precision curves (the thresholds were swept as a
// scale factor over 1, 3, 10, 30, 100): Michaelis-Menten under Picard improves
// monotonically down to ~1/30 of the derived values, 0.313 s -> 0.093 s at a
// delivered error of 1e-4, and turns back up by 1/100; van der Pol under exprb
// degrades 1.8x past ~1/3 and is flat between 1 and 1/3.  A two-compartment
// linear model is insensitive across the whole range.
//
// These are only reachable through the derived-then-measured route above; do
// not "restore" them to the analytic values without re-running that sweep.
//
// The Picard thresholds are shared with the symmetric linear-ramp step, which
// gains two orders per level rather than one, because the derivation lands in
// the same place: 3*N^(1/2) < N gives N > 9, 7*N^(1/3) < 3*N^(1/2) gives
// N > (7/3)^6 ~ 161 and 15*N^(1/4) < 7*N^(1/3) gives N > (15/7)^12 ~ 2600,
// which at the measured 1/30 scale are 0.3, 5.4 and 87 against the 1.0, 5.0 and
// 58 below.
#define RX_INDLIN_AUTO_RICH_N    1.0
#define RX_INDLIN_AUTO_RICH4_N   5.0
#define RX_INDLIN_AUTO_RICH5_N  58.0
#define RX_INDLIN_EXPRB_RICH_N    9.0
#define RX_INDLIN_EXPRB_RICH4_N  54.0
#define RX_INDLIN_EXPRB_RICH5_N 583.0

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
// One exponential Rosenbrock-Euler (exprb2) step.  No iteration at all -- this
// is what distinguishes it from both Picard and Newton, and the reason it is
// worth trying after Newton's ceiling turned out to be below break-even: there
// is no convergence loop to fail, so no step is ever cut for non-convergence,
// and the Jacobian is formed once per STEP rather than once per iteration.
//
// Split at the current state rather than at the state-free rate matrix:
//
//     J = A + f'(y_n),   g(y) = f(y) - f'(y_n) y,   y' = J y + g(y)
//     y_{n+1} = exp(Jh) y_n + P_J(h) g(y_n)
//
// Both terms come from ONE exponential of size n+1, not 2n: augmenting with the
// forcing as a single column,
//
//     exp([[J, g],[0, 0]] h) [y_n; 1] = [exp(Jh)y_n + P_J(h)g ; 1]
//
// which is the same identity `meOnly` uses for infusion rates, with the whole
// forcing vector in one column instead of a unit column per infused
// compartment.
//
// `f(y_n)` is IndF's output, which already folds in the infusion rates, so the
// infusion needs no separate augmentation here.  With no IndF (code 3) the
// forcing is the infusion rate alone and f' is zero, which degenerates to the
// plain matrix-exponential step -- correct, and what that code path already
// does.
//
// Returns 1, or whatever the exponential path failed with.  It cannot return
// -2: there is nothing to converge.
static int indLinExprb2(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                        int neq, const arma::vec &y0, double t, double h, double tcov,
                        double tEval, double *InfusionRate_, t_ME ME, t_IndF IndF,
                        arma::vec *u, arma::vec &yOut,
                        arma::mat &Jf, arma::vec &yPert, arma::vec &fPlus,
                        arma::vec &fMinus, arma::mat &aug, arma::vec &augY,
                        arma::mat &augE, arma::mat &Aloc,
                        arma::mat &Jfull, arma::mat &Amat) {
  // A at the step start.
  ME(cSub, tcov, tEval, Aloc.memptr(), const_cast<double*>(y0.memptr()));
  // f(y_n): IndF when there is one, otherwise the bare infusion rate.
  arma::vec f0(neq);
  if (u != NULL) {
    IndF(cSub, tcov, tEval, f0.memptr(), const_cast<double*>(y0.memptr()));
    indLinForcingJac(cSub, op, neq, tcov, tEval, y0.memptr(), IndF, ME,
                     Jf, yPert, fPlus, fMinus, Jfull, Amat);
  } else {
    std::copy(InfusionRate_, InfusionRate_ + neq, f0.memptr());
    Jf.zeros(neq, neq);
  }
  // J = A + f', g = f(y_n) - f' y_n
  arma::mat J = Aloc + Jf;
  arma::vec g = f0 - Jf*y0;
  // exp([[J, g],[0,0]] h) [y0; 1]
  aug.zeros(neq + 1, neq + 1);
  aug.submat(0, 0, neq - 1, neq - 1) = J;
  aug.submat(0, neq, neq - 1, neq) = g;
  augE.set_size(neq + 1, neq + 1);
  matrixExpCached(aug, augE, h, op->indLinMatExpType, op->indLinMatExpOrder, ind);
  augY.set_size(neq + 1);
  std::copy(y0.begin(), y0.end(), augY.begin());
  augY[neq] = 1.0;
  arma::vec sol = augE*augY;
  yOut.set_size(neq);
  std::copy(sol.begin(), sol.begin() + neq, yOut.begin());
  // Unlike the iterating schemes this cannot fail to converge, but it CAN
  // overflow: `J` is the full linearisation, so at a relaxation layer it
  // carries a large POSITIVE eigenvalue and exp(Jh) blows up unless h is
  // short.  Without this the step returns NaN, the error estimate is NaN, and
  // the controller can no longer reject anything -- van der Pol at mu = 100
  // produced NaN for a whole period before this guard.  Report it the same way
  // a non-converging iteration does, so the driver cuts the step.
  for (int k = 0; k < neq; ++k) {
    if (!R_FINITE(yOut[k])) return -2;
  }
  return 1;
}

// RMS of `fac*(a - b)` against the atol/rtol scale.  Both the fixed-point
// schemes and exprb32 form their error estimate this way -- they differ only in
// which two approximations are differenced and by what factor.
//
// Every state is included, not just the ones carrying a forcing: the
// linearization error propagates into compartments that have none of their own.
static inline double indLinScaledErr(int neq, const double *rtol, const double *atol,
                                     const arma::vec &y0, const arma::vec &a,
                                     const arma::vec &b, double fac) {
  double err = 0.0;
  for (int k = 0; k < neq; ++k) {
    const double sc = atol[k] + rtol[k]*std::max(fabs(y0[k]), fabs(a[k]));
    if (sc <= 0.0) continue;
    const double e = fac*(a[k] - b[k])/sc;
    err += e*e;
  }
  return sqrt(err / (double) neq);
}

// Luan-Ostermann exprb32: third order, with the second-order exprb2 solution
// as an embedded pair.  Writing F for the full right-hand side and
// J = F'(y_n),
//
//     U_2     = y_n + h*phi1(hJ)*F(y_n)                     (this is exprb2)
//     y_{n+1} = U_2 + 2h*phi3(hJ)*D_2,  D_2 = F(U_2) - F(y_n) - J(U_2 - y_n)
//
// so `y_{n+1} - U_2` is an error estimate for the lower-order member and costs
// nothing beyond what the step already computes.  That is the reason to have
// this at all: exprb2 has no pair of its own and has to take its estimate from
// the extrapolation column, which is unreliable at low levels (see
// RX_INDLIN_EXPRB_MINRICH), so it is pinned to level 2 and pays for a tableau
// it only wants an error estimate from.
//
// Two properties of this splitting make the extra stage cheap.  `A` is
// state-independent, so D_2 collapses to `f(U_2) - f(y_n) - f'(U_2 - y_n)` --
// one more forcing evaluation and NO second Jacobian.  And phi3 comes from the
// same augmented-matrix identity already used for phi1, one size wider.
//
// Measured on a scalar Michaelis-Menten step: local order 4.015 against
// exprb2's 3.005, with the estimate tracking exprb2's actual error to three
// digits and their ratio approaching 1 as h falls.
static int indLinExprb32(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                         int neq, double *rtol, double *atol,
                         const arma::vec &y0, double t, double h, double tcov,
                         double tEval, double *InfusionRate_, t_ME ME, t_IndF IndF,
                         arma::vec *u, arma::vec &yOut, double *errOut,
                         arma::mat &Jf, arma::vec &yPert, arma::vec &fPlus,
                         arma::vec &fMinus, arma::mat &aug, arma::vec &augY,
                         arma::mat &augE, arma::mat &Aloc,
                         arma::mat &Jfull, arma::mat &Amat) {
  ME(cSub, tcov, tEval, Aloc.memptr(), const_cast<double*>(y0.memptr()));
  arma::vec f0(neq);
  if (u != NULL) {
    IndF(cSub, tcov, tEval, f0.memptr(), const_cast<double*>(y0.memptr()));
    indLinForcingJac(cSub, op, neq, tcov, tEval, y0.memptr(), IndF, ME,
                     Jf, yPert, fPlus, fMinus, Jfull, Amat);
  } else {
    std::copy(InfusionRate_, InfusionRate_ + neq, f0.memptr());
    Jf.zeros(neq, neq);
  }
  arma::mat J = Aloc + Jf;
  arma::vec g = f0 - Jf*y0;
  // Stage one is exactly exprb2: exp([[J, g],[0,0]] h) [y0; 1].
  aug.zeros(neq + 1, neq + 1);
  aug.submat(0, 0, neq - 1, neq - 1) = J;
  aug.submat(0, neq, neq - 1, neq) = g;
  augE.set_size(neq + 1, neq + 1);
  matrixExpCached(aug, augE, h, op->indLinMatExpType, op->indLinMatExpOrder, ind);
  augY.set_size(neq + 1);
  std::copy(y0.begin(), y0.end(), augY.begin());
  augY[neq] = 1.0;
  arma::vec sol = augE*augY;
  arma::vec U2(neq);
  std::copy(sol.begin(), sol.begin() + neq, U2.begin());
  // Same overflow guard as exprb2: J is the full linearisation and carries a
  // large positive eigenvalue at a relaxation layer.
  for (int k = 0; k < neq; ++k) {
    if (!R_FINITE(U2[k])) return -2;
  }
  if (u == NULL) {
    // No state-dependent forcing, so f' = 0, D_2 = 0 and the correction
    // vanishes identically -- this IS exprb2, and exactly, not to rounding.
    yOut = U2;
    if (errOut != NULL) *errOut = 0.0;
    return 1;
  }
  arma::vec fU2(neq);
  IndF(cSub, tcov, tEval, fU2.memptr(), U2.memptr());
  arma::vec D2 = fU2 - f0 - Jf*(U2 - y0);
  // phi3(hJ) D_2 from one exponential of size neq+3, using
  //   exp([[X, W],[0, K]])  with W = [w_p ... w_1] and K the UNIT superdiagonal
  // whose last column's top block is sum_k phi_k(X) w_k.  Setting w_3 = D_2 and
  // w_2 = w_1 = 0 leaves phi3(X) D_2 alone.
  //
  // X must be J*h with the exponential then taken at t = 1, NOT J with the
  // exponential at t = h: the latter scales the nilpotent block too, and the
  // identity needs its superdiagonal to be exactly one.  That distinction is
  // invisible at p = 1 -- the block is the 1x1 zero and h*0 = 0 -- which is why
  // stage one above can pass `h` directly.
  const int p = 3;
  aug.zeros(neq + p, neq + p);
  aug.submat(0, 0, neq - 1, neq - 1) = J*h;
  aug.submat(0, neq, neq - 1, neq) = D2;
  for (int i = 0; i < p - 1; ++i) aug(neq + i, neq + i + 1) = 1.0;
  augE.set_size(neq + p, neq + p);
  matrixExpCached(aug, augE, 1.0, op->indLinMatExpType, op->indLinMatExpOrder, ind);
  arma::vec phi3D = augE.col(neq + p - 1).head(neq);
  yOut = U2 + (2.0*h)*phi3D;
  for (int k = 0; k < neq; ++k) {
    if (!R_FINITE(yOut[k])) return -2;
  }
  if (errOut != NULL) {
    *errOut = indLinScaledErr(neq, rtol, atol, y0, yOut, U2, 1.0);
  }
  return 1;
}

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
// Has any state carrying a forcing gone non-finite?  Only those: a state the
// iteration never touches cannot have been driven there by it.
static inline bool indLinAnyNonFinite(rx_solving_options *op, const arma::vec &w) {
  for (int j = op->indLinN; j--;) {
    if (!R_FINITE(w[op->indLin[j]])) return true;
  }
  return false;
}

// Exact line search for the relaxation factor: the theta minimising the scaled
// norm of the next residual, given the extra pass in `wTry`.  Clamped, because
// anything outside the bounds is a secant fit that should not be trusted.
static inline double indLinThetaExact(rx_solving_options *op, const double *rtol,
                                      const double *atol, const arma::vec &w,
                                      const arma::vec &d, const arma::vec &wTry) {
  double num = 0.0, den = 0.0;
  for (int j = op->indLinN; j--;) {
    const int k = op->indLin[j];
    const double sc = rtol[k]*fabs(w[k]) + atol[k];
    if (sc <= 0.0) continue;
    const double a0 = d[k]/sc;
    const double a1 = (wTry[k] - w[k] - d[k])/sc;
    num += a0*a1;
    den += a1*a1;
  }
  double theta = (den > 0.0 && R_FINITE(num)) ? -num/den : 1.0;
  if (!R_FINITE(theta) || theta < RX_INDLIN_THETA_MIN) theta = RX_INDLIN_THETA_MIN;
  if (theta > RX_INDLIN_THETA_MAX) theta = RX_INDLIN_THETA_MAX;
  return theta;
}

// Converged?  The distance left to the fixed point is |theta*d| rather than the
// bare residual |d| -- at a contraction ratio of 0.9 those differ by ten times,
// which is how much slack testing |d| alone would have allowed.
static inline bool indLinConverged(rx_solving_options *op, const double *rtol,
                                   const double *atol, const arma::vec &w,
                                   const arma::vec &d, double theta) {
  for (int j = op->indLinN; j--;) {
    const int k = op->indLin[j];
    if (fabs(theta*d[k]) >=
        RX_INDLIN_PICARD_TOL_FAC*(rtol[k]*fabs(w[k]) + atol[k])) {
      return false;
    }
  }
  return true;
}


// Newton's divergence response.  A growing residual gets one chance: refresh
// the chord Jacobian at the current iterate and refactorise, once.  A second
// growth means the step is too long, and the ratio of successive residuals
// sizes the caller's cut.  Returns true to keep iterating.
static inline bool indLinNewtonDiverging(double res, double *resPrev, int *nGrow,
                                         double *ratioOut) {
  const bool growing = (*resPrev > 0.0 && res > *resPrev);
  if (growing && ++(*nGrow) >= 2) {
    if (ratioOut != NULL) {
      // Against the PREVIOUS residual, so this is read before resPrev moves.
      const double q = res/(*resPrev);
      *ratioOut = (q > 1.0) ? q : 1.0;
    }
    return false;
  }
  if (!growing) *nGrow = 0;
  // Unconditional, including the tolerated single growth: leaving resPrev at
  // the older, smaller value would make the next comparison read against a
  // stale baseline and trigger sooner than it should.
  *resPrev = res;
  return true;
}

// What pass 0 leaves behind, for both fixed-point schemes.  It is the caller's
// forward answer `w1`, and under the ramp its forcing is the fixed left end the
// rest of the substep is built from.
static inline int indLinAfterFirstPass(int cSub, rx_solving_options *op,
                                       rx_solving_options_ind *ind, int neq, bool ramp,
                                       arma::vec &y0, const arma::vec &w,
                                       const arma::vec *u, double subTp, double subTf,
                                       double tcov, int *on_, t_ME ME,
                                       arma::vec *w1Out, indLinRamp_t *rmp) {
  if (w1Out != NULL) *w1Out = w;
  if (!ramp) return 1;
  return indLinRampBuild(cSub, op, ind, neq, y0, *u, subTp, subTf, tcov, on_, ME, rmp);
}

// Leave the iteration with `w` as the answer.  Every exit writes the current
// iterate back, including the ones that give up -- the driver reads it to size
// its cut.
static inline int indLinLeave(const arma::vec &w, double *yp_, int code) {
  std::copy(w.begin(), w.end(), yp_);
  return code;
}

// The operator the substep map applies to the endpoint forcing, and so what the
// Newton residual's derivative carries: h*phi2(Ah) under the ramp, which came
// with the rest of it, and P(h) = h*phi1(Ah) for the constant column.
//
// P(h) is the top-right block of exp([[A, I],[0, 0]]h) -- the same augmentation
// `meOnly` uses, widened to the full identity instead of just the infusion unit
// columns -- so it needs no A^-1 and rides the exponential cache.  `h*I` is its
// small-||Ah|| limit and was tried first; see the note in indLinNewton's header
// for when that is and is not adequate.
static inline void indLinNewtonPmat(int cSub, rx_solving_options *op,
                                    rx_solving_options_ind *ind, int neq,
                                    const indLinRamp_t *rmp, double tcov,
                                    double subTf, double h, arma::vec &w,
                                    t_ME ME, arma::mat &Ph) {
  if (rmp != NULL) {
    Ph = rmp->P2;
    return;
  }
  arma::mat Aloc(neq, neq), phiZ(neq, neq), phiTmp(neq, neq);
  ME(cSub, tcov, subTf, Aloc.memptr(), w.memptr());
  indLinPmat(op, ind, neq, Aloc, h, 1, Ph, phiZ, phiTmp);
}

// Newton's answer to a residual that grew: refresh the chord Jacobian at the
// current iterate and refactorise, once.  A stale Jacobian is the likely cause
// of a stall on a stiff problem, and refreshing costs 2*indLinN cheap IndF
// evaluations against a whole rejected step.  Returns whether the factorisation
// is usable.
static inline bool indLinNewtonRefresh(int cSub, rx_solving_options *op,
                                       rx_solving_options_ind *ind, int neq,
                                       bool wasGrowing, bool *refreshed, int *nGrow,
                                       double tcov, double subTf, const arma::vec &gw,
                                       t_IndF IndF, t_ME ME, const arma::mat &Ph,
                                       arma::mat &Jf, arma::vec &yPert,
                                       arma::vec &fPlus, arma::vec &fMinus,
                                       arma::mat &JfullS, arma::mat &AmatS,
                                       arma::mat &G, arma::mat &Ginv, bool haveFact) {
  if (!wasGrowing) {
    *nGrow = 0;
    return haveFact;
  }
  if (*refreshed) return haveFact;
  *refreshed = true;
  indLinForcingJac(cSub, op, neq, tcov, subTf, gw.memptr(), IndF, ME,
                   Jf, yPert, fPlus, fMinus, JfullS, AmatS);
  G = -Ph*Jf;
  G.diag() += 1.0;
  return arma::inv(Ginv, G);
}

static int indLinNewton(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                        int neq, double *rtol, double *atol, int maxIter,
                        double *yp_, double subTp, double subTf, double tcov,
                        double *InfusionRate_, int *on_, t_ME ME, t_IndF IndF,
                        arma::vec *u, arma::vec *w1Out, double *ratioOut) {
  arma::vec y0(yp_, neq);
  arma::vec w(y0), gw(neq), r(neq), dw(neq);
  arma::vec yPert(neq), fPlus(neq), fMinus(neq);
  arma::mat Jf(neq, neq), G(neq, neq), JfullS, AmatS;
  const bool ramp = indLinRampOn(op, u);
  indLinRamp_t rmp;
  if (ratioOut != NULL) *ratioOut = 1.0;

  // Pass 0: the left-endpoint answer, evaluated at subTp in time as well as in
  // state.  This is the caller's `w1`, and the forcing it evaluates is the
  // ramp's fixed left end.
  rxIndLinCountIter(1);
  int ret = indLinPass(cSub, op, ind, w.memptr(), y0.memptr(), subTp, subTf, tcov,
                       subTp, InfusionRate_, on_, ME, IndF, u, NULL);
  if (ret <= 0) return ret;
  ret = indLinAfterFirstPass(cSub, op, ind, neq, ramp, y0, w, u, subTp, subTf,
                             tcov, on_, ME, w1Out, &rmp);
  if (ret <= 0) return ret;
  const indLinRamp_t *rmpP = ramp ? &rmp : NULL;

  // Chord Jacobian, formed once at the pass-0 iterate.
  indLinForcingJac(cSub, op, neq, tcov, subTf, w.memptr(), IndF, ME,
                   Jf, yPert, fPlus, fMinus, JfullS, AmatS);
  const double h = subTf - subTp;
  arma::mat Ph(neq, neq);
  indLinNewtonPmat(cSub, op, ind, neq, rmpP, tcov, subTf, h, w, ME, Ph);
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
                     subTf, InfusionRate_, on_, ME, IndF, u, rmpP);
    if (ret <= 0) return ret;
    r = gw - w;                       // -G(w); the Picard step is exactly this
    if (indLinAnyNonFinite(op, gw)) return indLinLeave(gw, yp_, -2);
    double res = indLinResNorm(op, rtol, atol, gw, r);
    // Converged: same test and same 0.1x factor as the Picard path, but on the
    // raw residual -- Newton has no relaxation to undo.
    // theta = 1: Newton takes the full step, so the residual IS the distance
    // to the fixed point and needs no contraction correction.
    if (indLinConverged(op, rtol, atol, gw, r, 1.0)) return indLinLeave(gw, yp_, 1);
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
    const bool wasGrowing = (resPrev > 0.0 && res > resPrev);
    if (!indLinNewtonDiverging(res, &resPrev, &nGrow, ratioOut)) {
      return indLinLeave(gw, yp_, -2);
    }
    haveFact = indLinNewtonRefresh(cSub, op, ind, neq, wasGrowing, &refreshed, &nGrow,
                                   tcov, subTf, gw, IndF, ME, Ph, Jf, yPert, fPlus,
                                   fMinus, JfullS, AmatS, G, Ginv, haveFact);
    resPrev = res;
    if (haveFact) {
      dw = Ginv*r;
      w += dw;
    } else {
      w = gw;                          // degrade to a Picard step rather than stall
    }
  }
  return indLinLeave(w, yp_, -2);
}

// Pick the relaxation factor for this pass.  Returns the propagation status --
// <= 0 is a hard failure from the extra pass the exact search needs and must be
// passed up -- with `*thetaOut` set when it returns 1.
static int indLinPickTheta(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                           const double *rtol, const double *atol, int stepSearch,
                           bool backOff, const arma::vec &w, const arma::vec &d,
                           const arma::vec &dPrev, double thetaPrev,
                           arma::vec &y0, double subTp, double subTf, double tcov,
                           double *InfusionRate_, int *on_, t_ME ME, t_IndF IndF,
                           arma::vec *u, const indLinRamp_t *rmp,
                           arma::vec &wTry, double *thetaOut) {
  if (backOff || stepSearch == RX_INDLIN_SEARCH_NONE) {
    *thetaOut = 1.0;
    return 1;
  }
  if (stepSearch == RX_INDLIN_SEARCH_EXACT) {
    // One extra propagation, at the plain-Picard point, gives the residual
    // there.  The residual is affine in the step for an affine map, so two of
    // them locate the minimizer of ||R(a)|| in closed form -- the same thing
    // Schmidt et al.'s bisection converges to, without spending a matrix
    // exponential per bisection.
    wTry = w;
    const int r2 = indLinPass(cSub, op, ind, wTry.memptr(), y0.memptr(),
                              subTp, subTf, tcov, subTf,
                              InfusionRate_, on_, ME, IndF, u, rmp);
    if (r2 <= 0) return r2;
    *thetaOut = indLinThetaExact(op, rtol, atol, w, d, wTry);
    return 1;
  }
  *thetaOut = (thetaPrev > 0.0) ? indLinTheta(op, rtol, atol, w, d, dPrev, thetaPrev)
                                : 1.0;
  return 1;
}

// Is the residual growing?  One bad pass is tolerated by backing the
// relaxation off to a plain Picard step; two in a row means this step has no
// fixed point to find and the caller should shorten it.  Returns true to keep
// iterating, false to give up, with `*backOff` set for the tolerated case.
static inline bool indLinResidualOk(double res, double *resPrev, int *nGrow,
                                    bool *backOff) {
  *backOff = false;
  if (*resPrev >= 0.0 && res > 2.0*(*resPrev)) {
    if (++(*nGrow) >= 2) return false;
    *backOff = true;
  } else {
    *nGrow = 0;
  }
  *resPrev = res;
  return true;
}

// Report the measured contraction ratio so a caller that has to cut the step
// can size the cut instead of guessing.
static inline void indLinReportRatio(double *ratioOut, double theta) {
  if (ratioOut == NULL) return;
  const double r = fabs(1.0 - 1.0/theta);
  *ratioOut = (R_FINITE(r) && r > 1.0) ? r : 1.0;
}

// One pass's residual, and what to do with it: give up on the step (-2), pass a
// hard failure up (<= 0), or carry on (1) with `*thetaOut` set.  The two belong
// together -- the relaxation is picked from the same residual the growth test
// reads, and a growing one backs it off to a plain Picard step.
static int indLinPassTheta(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                           const double *rtol, const double *atol, int stepSearch,
                           const arma::vec &w, const arma::vec &d,
                           const arma::vec &dPrev, double thetaPrev, arma::vec &y0,
                           double subTp, double subTf, double tcov,
                           double *InfusionRate_, int *on_, t_ME ME, t_IndF IndF,
                           arma::vec *u, const indLinRamp_t *rmp, arma::vec &wTry,
                           double *resPrev, int *nGrow, double *yp_, double *thetaOut) {
  const double res = indLinResNorm(op, rtol, atol, w, d);
  bool backOff = false;
  if (!indLinResidualOk(res, resPrev, nGrow, &backOff)) {
    return indLinLeave(w, yp_, -2);
  }
  return indLinPickTheta(cSub, op, ind, rtol, atol, stepSearch, backOff,
                         w, d, dPrev, thetaPrev, y0, subTp, subTf, tcov,
                         InfusionRate_, on_, ME, IndF, u, rmp, wTry, thetaOut);
}

// May this pass's iterate be accepted?  Pass 0 under the ramp may not: see
// `needRampPass`.
static inline bool indLinAccept(rx_solving_options *op, const double *rtol,
                                const double *atol, const arma::vec &w,
                                const arma::vec &d, double theta, int i,
                                bool needRampPass) {
  if (i == 0 && needRampPass) return false;
  return indLinConverged(op, rtol, atol, w, d, theta);
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
  // The ramp's fixed part.  Pass 0 evaluates the substep-start forcing anyway,
  // so it is read off that pass rather than paid for separately, and everything
  // built from it holds for the whole substep -- only the endpoint value moves
  // with the iterate.
  const bool ramp = indLinRampOn(op, u);
  indLinRamp_t rmp;
  // Pass 0 is the left-endpoint constant-column answer whatever the forcing
  // setting, so accepting it would return a substep the ramp never touched --
  // asymmetric, and first order.  The caller cannot see that and would collapse
  // its extrapolation tableau with the symmetric factors anyway, so require one
  // ramp pass before converging.  It is reachable: the test is on the whole
  // substep's change, which a step that barely moves passes at once.  Only when
  // there is an iteration to spend on it -- `indLinMaxIter = 1` asks for a
  // single pass and has to still be able to return one.
  const bool needRampPass = ramp && maxIter > 1;
  const indLinRamp_t *rmpP = ramp ? &rmp : NULL;
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
                         InfusionRate_, on_, ME, IndF, u,
                         (i == 0) ? NULL : rmpP);
    if (ret <= 0) return ret;
    // Pass 0 linearizes at the substep-start state: that is the forward
    // (explicit) answer, and the caller differences it against the converged
    // backward one to get a local error estimate for free.
    if (i == 0) {
      ret = indLinAfterFirstPass(cSub, op, ind, neq, ramp, y0, w, u, subTp, subTf,
                                 tcov, on_, ME, w1Out, &rmp);
      if (ret <= 0) return ret;
    }
    d = w - wPrev;
    if (indLinAnyNonFinite(op, w)) return indLinLeave(w, yp_, -2);
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
    const int rt = indLinPassTheta(cSub, op, ind, rtol, atol, stepSearch, w, d,
                                   dPrev, thetaPrev, y0, subTp, subTf, tcov,
                                   InfusionRate_, on_, ME, IndF, u, rmpP, wTry,
                                   &resPrev, &nGrow, yp_, &theta);
    if (rt != 1) return rt;
    indLinReportRatio(ratioOut, theta);
    if (indLinAccept(op, rtol, atol, w, d, theta, i, needRampPass)) {
      return indLinLeave(w, yp_, 1);
    }
    if (theta != 1.0) w = wPrev + theta*d;
    dPrev = d;
    thetaPrev = theta;
  }
  return indLinLeave(w, yp_, -2);
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
// frozen (a rate constant reading `t` or a time-varying covariate); only the
// derivative being expanded changes.
// The two exponential Rosenbrock schemes, wrapped so the substep dispatcher
// stays a dispatcher.  Both own the same scratch, neither iterates, and both
// report an overflow the same way -- `-2` with ratio 2, asking the driver for a
// halving, since there is no measured contraction ratio to size a cut from.
//
// They differ in one respect, and it is the reason exprb32 exists: exprb2 has
// no embedded pair, so it leaves `errOut` for indLinTryStep() to fill from the
// extrapolation column and the driver holds it at RX_INDLIN_EXPRB_MINRICH,
// while exprb32 fills `errOut` itself and runs at the base order.
static int indLinExprbSubstep(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                              int neq, double *rtol, double *atol, int scheme,
                              const arma::vec &y0, double t, double h, double tcov,
                              double *InfusionRate_, t_ME ME, t_IndF IndF,
                              arma::vec *u, arma::vec &yOut, arma::vec &w1,
                              double *errOut, double *ratioOut) {
  arma::mat Jf(neq, neq), aug, augE, Aloc(neq, neq), Jfull, Amat;
  arma::vec yPert(neq), fPlus(neq), fMinus(neq), augY;
  int ret;
  if (scheme == RX_INDLIN_ITER_EXPRB32) {
    ret = indLinExprb32(cSub, op, ind, neq, rtol, atol, y0, t, h, tcov,
                        t, InfusionRate_, ME, IndF, u, yOut, errOut,
                        Jf, yPert, fPlus, fMinus, aug, augY, augE, Aloc,
                        Jfull, Amat);
    if (ret == -2 && errOut != NULL) *errOut = 0.0;
  } else {
    ret = indLinExprb2(cSub, op, ind, neq, y0, t, h, tcov,
                       t, InfusionRate_, ME, IndF, u, yOut,
                       Jf, yPert, fPlus, fMinus, aug, augY, augE, Aloc,
                       Jfull, Amat);
    if (errOut != NULL) *errOut = 0.0;
  }
  if (ratioOut != NULL) *ratioOut = (ret == -2) ? 2.0 : 1.0;
  w1 = yOut;
  return ret;
}

static int indLinTrySubstep(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                            int neq, double *rtol, double *atol, int scheme,
                            const arma::vec &y0, double t, double h, double tcov,
                            double *InfusionRate_, int *on_, t_ME ME, t_IndF IndF,
                            arma::vec *u, arma::vec &yOut, arma::vec &w1,
                            double *errOut, double *ratioOut) {
  int maxIter = indLinMaxIterOf(op);
  yOut = y0;
  int ret;
  if (scheme == RX_INDLIN_ITER_EXPRB32 || scheme == RX_INDLIN_ITER_EXPRB) {
    return indLinExprbSubstep(cSub, op, ind, neq, rtol, atol, scheme, y0, t, h, tcov,
                              InfusionRate_, ME, IndF, u, yOut, w1, errOut, ratioOut);
  }
  if (scheme == RX_INDLIN_ITER_NEWTON) {
    ret = indLinNewton(cSub, op, ind, neq, rtol, atol, maxIter,
                       yOut.memptr(), t, t + h, tcov,
                       InfusionRate_, on_, ME, IndF, u, &w1, ratioOut);
  } else {
    ret = indLinIterate(cSub, op, ind, neq, rtol, atol, maxIter, op->indLinStepSearch,
                        yOut.memptr(), t, t + h, tcov,
                        InfusionRate_, on_, ME, IndF, u, &w1, ratioOut);
  }
  if (ret != 1) return ret;
  if (indLinRampOn(op, u)) {
    // The converged answer already integrates the forcing over the substep as
    // a line, so it is second order on its own and averaging a first-order
    // answer back into it would only undo that.  `w1` is still the low-order
    // member of the pair, and their difference is still the estimate -- at
    // face value now rather than halved, because the ramp sits where the
    // average did, one whole first-order error away from `w1` instead of two.
    *errOut = indLinScaledErr(neq, rtol, atol, y0, yOut, w1, 1.0);
    return 1;
  }
  *errOut = indLinScaledErr(neq, rtol, atol, y0, yOut, w1, 0.5);
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
                       int neq, double *rtol, double *atol, int scheme,
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
    int ret = indLinTrySubstep(cSub, op, ind, neq, rtol, atol, scheme, cur, ts, te - ts,
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
//                                     2nd order   symmetric   exprb32   solves
//   level 0  the plain substep              2nd         2nd       3rd        1
//   level 1  Richardson on h, h/2           3rd         4th       4th        3
//   level 2  Romberg column to h/4          4th         6th       5th        7
//   level 3  Romberg column to h/8          5th         8th       6th       15
//
// Entry j of the tableau is built from T(h/2^j), and
// R[k][j] = (f*R[k-1][j+1] - R[k-1][j])/(f-1) eliminates the leading term of
// R[k-1].  Which term that is depends on the base step, in two ways: what order
// it starts at, and how far its expansion advances per level.  A second-order
// step whose expansion leaves every power behind (h^2, h^3, h^4 ...) has pass
// `k` kill h^(k+1) with f = 2^(k+1).  The symmetric one -- the converged
// linear-ramp substep, see indLinRamp_t -- leaves only the EVEN powers, so pass
// `k` kills h^(2k) with f = 4^k and a level is worth two orders instead of one.
// exprb32 starts at h^3 instead, so its factors run 8, 16, 32.
//
// Getting either wrong is not a crash, only a weak or absent cancellation: the
// asymmetric factors on a symmetric base take h^4 down by 14x rather than
// removing it, and the second-order factors on exprb32 take its h^3 down by 6x,
// each showing up as a column no better -- or worse -- than the one below it.
// The error estimate is the difference between the two highest entries in every
// case.
#define RX_INDLIN_RICH_MAXLVL 3

// Is the base substep its own adjoint?  Only the converged linear-ramp fixed
// point is: the exponential Rosenbrock steps are not symmetric, and neither is
// the constant-column map, whose two members are linearized at opposite ends of
// the step and averaged.
//
// Picard needs a second pass to reach the ramp at all -- its first is the
// left-endpoint constant-column answer -- so at `indLinMaxIter = 1` it can only
// ever return that, and this has to say so rather than let the tableau collapse
// an asymmetric entry with the symmetric factors.  Newton's first loop pass is
// already a ramp pass, so one iteration is enough for it.
static inline bool indLinSymmetric(rx_solving_options *op, int scheme,
                                   const arma::vec *u) {
  if (!indLinRampOn(op, u)) return false;
  if (scheme == RX_INDLIN_ITER_NEWTON) return true;
  return (scheme == RX_INDLIN_ITER_PICARD) && (indLinMaxIterOf(op) > 1);
}

// The base substep's own order.  Everything here is second order except the
// Luan-Ostermann pair, which is third (rxode2#1222).
static inline int indLinBaseOrder(int scheme) {
  return (scheme == RX_INDLIN_ITER_EXPRB32) ? 3 : 2;
}

// How many orders one extrapolation level is worth: two when the base step is
// symmetric, because its expansion skips every odd power, and one otherwise.
static inline int indLinRichGain(rx_solving_options *op, int scheme,
                                 const arma::vec *u) {
  return indLinSymmetric(op, scheme, u) ? 2 : 1;
}

// The order of the estimate a step at extrapolation level `useRich` is sized
// from.
static inline int indLinEstOrder(rx_solving_options *op, int scheme,
                                 const arma::vec *u, int useRich) {
  if (useRich <= 0) {
    // With no tableau the estimate is whatever the substep itself produced: the
    // fixed-point schemes difference their converged answer against the forward
    // one, which measures the forward answer's first-order error, while exprb32
    // differences its embedded pair, whose lower member is second order.
    return (scheme == RX_INDLIN_ITER_EXPRB32) ? 2 : 1;
  }
  // Otherwise it is the entry one level below the top, which the top is
  // differenced against.
  return indLinBaseOrder(scheme) + (useRich - 1)*indLinRichGain(op, scheme, u);
}

static int indLinTryStep(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                         int neq, double *rtol, double *atol, int scheme,
                         const arma::vec &y0, double t, double h, int locf, int useRich,
                         double *InfusionRate_, int *on_, t_ME ME, t_IndF IndF,
                         arma::vec *u, arma::vec &yOut, arma::vec &w1,
                         arma::vec &yScratch, arma::mat &tab,
                         double *errOut, double *ratioOut) {
  double tcov = locf ? t : (t + h);
  if (useRich <= 0) {
    return indLinTrySubstep(cSub, op, ind, neq, rtol, atol, scheme, y0, t, h, tcov,
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
    int ret = indLinChain(cSub, op, ind, neq, rtol, atol, scheme, y0, t, h, nSub, locf,
                          InfusionRate_, on_, ME, IndF, u, yOut, w1, yScratch, &r);
    if (r > rMax) rMax = r;
    if (ret != 1) { *ratioOut = rMax; return ret; }
    for (int k = 0; k < neq; ++k) tab(k, j) = yOut[k];
  }
  *ratioOut = rMax;
  // Neville-Aitken sweep in place; column j holds R[k][j] after pass k.
  //
  // Pass `k` removes the leading term of R[k-1], which is h^(p + (k-1)*s) for a
  // base step of order `p` whose expansion advances `s` orders per level.  So
  // 4, 8, 16 for the second-order asymmetric step, 4, 16, 64 for the symmetric
  // one, and 8, 16, 32 for third-order exprb32.
  const int p = indLinBaseOrder(scheme);
  const int s = indLinRichGain(op, scheme, u);
  for (int k = 1; k <= useRich; ++k) {
    double f = (double)(1 << (p + (k - 1)*s));
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

// The extrapolation level an explicit `indLinRichardson` asks for.  `auto`
// starts at the base order and earns its way up through indLinRaiseRich().
static inline int indLinInitialRich(rx_solving_options *op) {
  switch (op->indLinRichardson) {
  case RX_INDLIN_RICH_ALWAYS:  return 1;
  case RX_INDLIN_RICH_ALWAYS4: return 2;
  case RX_INDLIN_RICH_ALWAYS5: return 3;
  default:                     return 0;
  }
}

// The level to run at given how much of the interval is left, as a pure
// function of (scheme, current level, steps remaining) so the break-even rule
// can be read -- and tested -- without the driver around it.
//
// Break-even: a p-th order step needs about N^(2/p) of the second-order step's
// N, so the fourth-order column (7 solves) beats the third (3 solves) once
// 7*N^(1/2) < 3*N^(2/3), i.e. once N > (7/3)^6.  One level per call, and only
// upward -- the ratchet is what keeps it from chattering.
static inline int indLinRaiseRich(int scheme, int useRich, double nLeft) {
  if (scheme == RX_INDLIN_ITER_EXPRB && useRich < RX_INDLIN_EXPRB_MINRICH) {
    useRich = RX_INDLIN_EXPRB_MINRICH;
  }
  const int isExprb = (scheme == RX_INDLIN_ITER_EXPRB);
  const double r5 = isExprb ? RX_INDLIN_EXPRB_RICH5_N : RX_INDLIN_AUTO_RICH5_N;
  const double r4 = isExprb ? RX_INDLIN_EXPRB_RICH4_N : RX_INDLIN_AUTO_RICH4_N;
  const double r1 = isExprb ? RX_INDLIN_EXPRB_RICH_N  : RX_INDLIN_AUTO_RICH_N;
  if (useRich < 3 && nLeft > r5) return 3;
  if (useRich < 2 && nLeft > r4) return 2;
  if (useRich < 1 && nLeft > r1) return 1;
  return useRich;
}

// Give the interval up: restore the state it was entered with and report a
// convergence failure.  Three places in the driver bail out this way -- the
// mxstep budget and two step-underflow guards -- and having one exit makes it
// checkable that they all leave the entry state behind rather than a partial
// step.
static inline int indLinAbandon(const arma::vec &y0, double *yp_,
                                rx_solving_options_ind *ind) {
  std::copy(y0.begin(), y0.end(), yp_);
  if (ind != NULL) ind->err |= rxErrIndLinConverge;
  return 1;
}

// The stiffness gate.  A step cut for non-convergence is one the error
// controller would have accepted, so a run of them says the iteration -- not
// the tolerance -- is what limits the step, which is the regime the
// exponential Rosenbrock scheme exists for.  One way only, so it cannot
// chatter, and the counter advances only while Picard is still in use.
//
// Switching re-earns the extrapolation level under the new scheme's cost model
// instead of inheriting Picard's: the two use different thresholds, and since
// the level ratchet is one way a level climbed under Picard's much lower ones
// would otherwise stick at 3 for the rest of the subject (measured at 4.25 s
// against 2.18 s on 200-subject van der Pol).  Only when the level is ours to
// choose -- an explicit always4/always5 must not be lowered.
static inline void indLinStiffGate(int autoScheme, int autoRich,
                                   indLinAutoState_t *autoSt,
                                   int *scheme, int *useRich) {
  if (!autoScheme || autoSt == NULL || *scheme != RX_INDLIN_ITER_PICARD) return;
  if (++(autoSt->nCut) < RX_INDLIN_AUTO_ITER_CUTS) return;
  *scheme = autoSt->scheme = RX_INDLIN_ITER_EXPRB;
  if (autoRich) {
    *useRich = RX_INDLIN_EXPRB_MINRICH;
    autoSt->rich = RX_INDLIN_EXPRB_MINRICH;
  } else if (*useRich < RX_INDLIN_EXPRB_MINRICH) {
    *useRich = RX_INDLIN_EXPRB_MINRICH;
  }
}

// Step-size factor from the error estimate, clamped to the growth bounds.
static inline double indLinStepFactor(double err, double expo, double safe,
                                      double facMin, double facMax) {
  double fac = (err > 0.0) ? safe*pow(err, expo) : facMax;
  if (fac < facMin) fac = facMin;
  if (fac > facMax) fac = facMax;
  return fac;
}

// Cut factor after a convergence failure.  Not a dead end: the contraction
// ratio is proportional to h, so a failure proves the step is too long and a
// bounded number of cuts must fix it.  Sized from the measured ratio rather
// than halved blindly -- lsoda's corrector-failure convention.
static inline double indLinCutFactor(double ratio, double facMin) {
  double cut = 1.0/(2.0*ratio);
  if (cut > 0.25) cut = 0.25;
  if (cut < facMin) cut = facMin;
  return cut;
}

// What one attempt concluded.  ACCEPT and RETRY both mean "go round again" --
// they are distinguished because only ACCEPT advances `t` -- while the two
// exits leave the interval.
typedef enum {
  RX_INDLIN_ACT_ACCEPT = 0,  // step taken
  RX_INDLIN_ACT_RETRY,       // rejected or cut; `h` has already been reduced
  RX_INDLIN_ACT_ABANDON,     // out of budget or out of step; restore and report
  RX_INDLIN_ACT_ERROR        // hard failure from below; pass its code up
} indLinAction_t;

// The state one attempt advances.  Bundled rather than passed as six in/out
// pointers: an attempt moves `t`, `h`, `scheme`, `useRich` and `lastRejected`
// together, and spelling that out as a long argument list would hide the very
// thing splitting the loop is meant to show.
typedef struct {
  double t;
  double h;
  int scheme;
  int useRich;
  int lastRejected;
  int nAttempt;
  int nAccept;
} indLinProgress_t;

// Raise the extrapolation level if what is left of the interval now justifies
// it.  exprb32 is excluded: it has its own embedded pair, so a tableau would
// only re-estimate an error it already has.
static inline void indLinMaybeRaise(int autoRich, int richSticky,
                                    indLinAutoState_t *autoSt, double tf,
                                    indLinProgress_t *pr) {
  if (!autoRich || pr->nAccept <= 0 || pr->h <= 0.0 ||
      pr->scheme == RX_INDLIN_ITER_EXPRB32) {
    return;
  }
  pr->useRich = indLinRaiseRich(pr->scheme, pr->useRich, (tf - pr->t)/pr->h);
  // Carry it to the next interval of this subject; one way, so it cannot
  // chatter, and it is dropped when the subject changes.
  if (richSticky && autoSt != NULL && pr->useRich > autoSt->rich) {
    autoSt->rich = pr->useRich;
  }
}

// Commit an accepted step and size the next one.  A step following a rejection
// may not grow, and a steady-state step may never grow at all: steady state
// re-solves the same interval until it stops moving, so a drifting substep
// schedule would make the ssRtol/ssAtol test read schedule jitter rather than
// convergence.
static inline void indLinAcceptStep(indLinProgress_t *pr, bool inSS, double hCap,
                                    double fac, arma::vec &y0,
                                    const arma::vec &yTry) {
  y0 = yTry;
  pr->t += pr->h;
  pr->nAccept++;
  __indLinNAccept++;
  if ((pr->lastRejected || inSS) && fac > 1.0) fac = 1.0;
  pr->lastRejected = 0;
  pr->h *= fac;
  if (hCap > 0.0 && std::isfinite(hCap) && pr->h > hCap) pr->h = hCap;
}

// One adaptive attempt: raise the extrapolation level if the remaining work
// now justifies it, take the step, and decide what the outcome means.  Pulled
// out of indLinDriveAdaptive() so the driver reads as "snap to the endpoint,
// attempt, handle the two ways out" and the policy lives here.
static indLinAction_t indLinAttempt(int cSub, rx_solving_options *op,
                                    rx_solving_options_ind *ind, int neq,
                                    double *rtol, double *atol,
                                    double tf, double span, double hCap, int locf,
                                    double *InfusionRate_, int *on_,
                                    t_ME ME, t_IndF IndF, arma::vec *u,
                                    int autoScheme, int autoRich, int richSticky,
                                    bool inSS, indLinAutoState_t *autoSt,
                                    indLinProgress_t *pr,
                                    arma::vec &y0, arma::vec &yTry, arma::vec &w1,
                                    arma::vec &yScratch, arma::mat &richTab,
                                    int *retOut) {
  const double SAFE = 0.9, FACMIN = 0.1, FACMAX = 5.0;
  __indLinNAttempt++;
  if (++(pr->nAttempt) > op->mxstep) return RX_INDLIN_ACT_ABANDON;
  // Decide from the step the controller has settled on rather than by burning
  // the switch-over count first.
  indLinMaybeRaise(autoRich, richSticky, autoSt, tf, pr);
  // The step is sized from an estimate of the order the extrapolation is built
  // on -- what the substep itself reported for the plain step, and one level
  // down from the top otherwise.  The exponent is -1/(p_est + 1) in every case.
  const double expo =
    -1.0/((double) (indLinEstOrder(op, pr->scheme, u, pr->useRich) + 1));
  double err = 0.0, ratio = 1.0;
  const int ret = indLinTryStep(cSub, op, ind, neq, rtol, atol, pr->scheme, y0,
                                pr->t, pr->h, locf, pr->useRich,
                                InfusionRate_, on_, ME, IndF, u, yTry, w1,
                                yScratch, richTab, &err, &ratio);
  if (ret == -2) {
    __indLinNCutConv++;
    indLinStiffGate(autoScheme, autoRich, autoSt, &(pr->scheme), &(pr->useRich));
    pr->h *= indLinCutFactor(ratio, FACMIN);
    pr->lastRejected = 1;
    return (pr->h < 1e-10*span) ? RX_INDLIN_ACT_ABANDON : RX_INDLIN_ACT_RETRY;
  }
  if (ret <= 0) {
    *retOut = ret;
    return RX_INDLIN_ACT_ERROR;
  }
  double fac = indLinStepFactor(err, expo, SAFE, FACMIN, FACMAX);
  if (err > 1.0) {
    __indLinNRejErr++;
    pr->h *= fac;
    pr->lastRejected = 1;
    return (pr->h < 1e-10*span) ? RX_INDLIN_ACT_ABANDON : RX_INDLIN_ACT_RETRY;
  }
  indLinAcceptStep(pr, inSS, hCap, fac, y0, yTry);
  return RX_INDLIN_ACT_ACCEPT;
}

// Where this interval starts: which scheme, which extrapolation level, and
// whether either is ours to change.  Split out because the driver's remaining
// job is the loop, and because the four flags interact in ways worth reading in
// one place rather than spread over the top of a long function.
static void indLinBeginInterval(rx_solving_options *op, int cSub, int *scheme,
                                int *useRich, int *autoScheme, int *autoRich,
                                int *richSticky, indLinAutoState_t **autoSt) {
  *useRich = indLinInitialRich(op);
  // `auto` starts on Picard and is raised by the stiffness gate, so a model
  // that never needs a Jacobian never forms one -- zero convergence cuts were
  // measured on Michaelis-Menten at every tolerance, where Picard is also the
  // cheapest per step.
  *scheme = op->indLinIteration;
  *autoScheme = (*scheme == RX_INDLIN_ITER_AUTO);
  *autoRich = (op->indLinRichardson == RX_INDLIN_RICH_AUTO);
  // Only a model with a state-dependent forcing has anything for the
  // extrapolation to work on: with the forcing state-free the exponential is
  // already exact, the controller takes one step per interval, and carrying a
  // level earned during a dose interval into every later one is pure cost --
  // measured at 48% on a two-compartment linear model.
  *richSticky = (*autoRich && op->indLinN > 0);
  *autoSt = (*autoScheme || *richSticky) ? indLinAutoFor(cSub) : NULL;
  if (*autoScheme) {
    *scheme = (*autoSt != NULL) ? (*autoSt)->scheme : RX_INDLIN_ITER_PICARD;
  }
  // The earned level has to outlive the interval that earned it, for the same
  // reason the scheme does: this runs once per OUTPUT INTERVAL, so a level held
  // only locally resets at every observation and an interval spanning a few
  // substeps never re-earns it.  A 13-observation profile then ran most of its
  // trajectory at second order whatever the tolerance -- 0.626 s against
  // 0.098 s for a forced level 4 on 200 subjects -- and scaling the thresholds
  // down by 100x recovered none of it, because the reset was the binding
  // constraint rather than the threshold.
  if (*richSticky && *autoSt != NULL && (*autoSt)->rich > *useRich) {
    *useRich = (*autoSt)->rich;
  }
  // exprb2 has no embedded pair, so its error estimate comes from the
  // extrapolation column and it needs at least two entries to have one at all.
  if (*scheme == RX_INDLIN_ITER_EXPRB && *useRich < RX_INDLIN_EXPRB_MINRICH) {
    *useRich = RX_INDLIN_EXPRB_MINRICH;
  }
}

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
  int useRich, scheme, autoScheme, autoRich, richSticky;
  indLinAutoState_t *autoSt;
  indLinBeginInterval(op, cSub, &scheme, &useRich, &autoScheme, &autoRich,
                      &richSticky, &autoSt);
  indLinProgress_t pr;
  pr.t = t; pr.h = h; pr.scheme = scheme; pr.useRich = useRich;
  pr.lastRejected = 0; pr.nAttempt = 0; pr.nAccept = 0;
  while (pr.t < tf) {
    // Snap to `tf` rather than leaving a sliver behind.
    if (pr.t + 1.01*pr.h >= tf) pr.h = tf - pr.t;
    if (pr.h <= 0.0) break;
    int ret = 1;
    const indLinAction_t act =
      indLinAttempt(cSub, op, ind, neq, rtol, atol, tf, span, hCap, locf,
                    InfusionRate_, on_, ME, IndF, u,
                    autoScheme, autoRich, richSticky, inSS, autoSt, &pr,
                    y0, yTry, w1, yScratch, richTab, &ret);
    if (act == RX_INDLIN_ACT_ABANDON) return indLinAbandon(y0, yp_, ind);
    if (act == RX_INDLIN_ACT_ERROR) {
      std::copy(y0.begin(), y0.end(), yp_);
      return ret;
    }
  }
  std::copy(y0.begin(), y0.end(), yp_);
  // `postSolve()` already counts one per output interval; add the substeps
  // this interval actually needed so `$counts$slvr` reports real work.
  if (ind != NULL && ind->slvr_counter != NULL && pr.nAccept > 1) {
    ind->slvr_counter[0] += pr.nAccept - 1;
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
