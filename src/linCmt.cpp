#ifndef NDEBUG
#define NDEBUG // just in case
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <cstdlib>
#include <cstring>
#include <algorithm>
#include "rxomp.h"
#include "../inst/include/rxode2.h"
#include "timsort.h"
#define SORT gfx::timsort
#include "linCmt.h"
#include "linCmtSensType.h"
#include "../inst/include/rxode2EventTranslate.h"

#ifdef RXODE2_NO_STAN_TBB_OBSERVER
// stan-math's init_chainablestack.hpp is kept out of the build (no linkable
// TBB, see rxode2_sundials_stan_compat.h).  Its global ad_tape_observer
// was also what created the main thread's AD tape; without STAN_THREADS the
// tape is a plain (shared) global, so constructing one ChainableStack here
// initializes it for the whole DLL.
namespace {
stan::math::ChainableStack rxode2MainThreadAdTape;
}
#endif

extern rx_solving_options op_global;
extern t_update_inis update_inis;
extern "C" rx_solve *getRxSolve_(void);
extern "C" double linCmtB(rx_solve *rx, int id,
                          double _t, int linCmt,
                          int ncmt, int oral0,
                          int which1, int which2,
                          int trans,
                          double p1, double v1,
                          double p2, double p3,
                          double p4, double p5,
                          double ka);


#define getLinRate ind->InfusionRate + op->linOffset
#define isSameTime(xout, xp) (fabs((xout)-(xp)) <= 2.0*DBL_EPSILON*max2(fabs(xout),fabs(xp)))

// Does this individual have a steady-state infusion (rate/dur) dose?
//
// The dose-time sensitivity (linCmtB which1 = -3) needs dA/dt, which includes
// the infusion rate.  A regular (non-SS) infusion's rate is recovered at
// output time via the linCmtRateHist cache (see linCmtBRateSlot() /
// linCmtBdoseTime() below, nlmixr2/rxode2#1236).  A steady-state infusion
// establishes its amounts analytically (handleSSinf8()/solveSSinf(), setting
// ind->linSS only transiently around that single call) and, for a one-shot
// SS dose with no following schedule, deliberately leaves ind->InfusionRate
// at 0 afterward -- the closed-form amount is exact but is not the limit of
// an ongoing rate, so -dA/dt is not well defined by the cache at the SS
// dose's own index; report NA there instead of a value that depends on
// solve-order happenstance.  A steady-state BOLUS (linCmtSsBolus) never
// touches InfusionRate and is unaffected -- it stays exact (see
// test-lincmt-dose-time-sens.R).
static inline int linCmtHasSsInfusion(rx_solving_options_ind *ind) {
  for (int i = 0; i < ind->ndoses; ++i) {
    int wh, cmt, wh100, whI, wh0;
    getWh(getEvid(ind, ind->idose[i]), &wh, &cmt, &wh100, &whI, &wh0);
    if ((wh0 == EVID0_SS0 || wh0 == EVID0_SS || wh0 == EVID0_SS20 ||
         wh0 == EVID0_SS2 || wh0 == EVID0_SSINF) &&
        whI != EVIDF_NORMAL && whI != EVIDF_REPLACE && whI != EVIDF_MULT) {
      return 1;
    }
  }
  return 0;
}

// Per-idx cache of the infusion rate feeding a linCmt() model, keyed the same
// way as the amounts cached via getAdvan()/Alast: written once, while the
// index is genuinely being solved (ind->InfusionRate live and correct), and
// read back on any later re-query of that same index (the output pass in
// rxode2_df.cpp, which clears ind->InfusionRate before recomputing lhs, or a
// backward re-query within the same solve).  width is op->numLin, the size of
// the rate vector linCmtBdoseTime()'s dAdt() expects.  linCmtB() writes this
// for every genuinely-solved idx (whether or not the model actually has an
// infusion -- it is simply 0 for a bolus-only regimen), so a re-query (grow =
// 0) should always find it; NULL is a defensive fallback for an idx that
// somehow was not, rather than a case expected to occur.
static inline double *linCmtBRateSlot(rx_solving_options_ind *ind, int idx, int width, int grow) {
  if (idx < 0 || width <= 0) return NULL;
  if (ind->linCmtRateHistW != width) {
    free(ind->linCmtRateHist);
    ind->linCmtRateHist = NULL;
    ind->linCmtRateHistCap = 0;
    ind->linCmtRateHistW = width;
  }
  if (idx >= ind->linCmtRateHistCap) {
    if (!grow) return NULL;
    int newCap = ind->linCmtRateHistCap > 0 ? ind->linCmtRateHistCap : 64;
    while (idx >= newCap) newCap *= 2;
    double *np = (double*) realloc(ind->linCmtRateHist, (size_t)newCap * width * sizeof(double));
    if (np == NULL) (Rf_error)("cannot allocate linCmt rate history");
    memset(np + (size_t)ind->linCmtRateHistCap * width, 0,
           (size_t)(newCap - ind->linCmtRateHistCap) * width * sizeof(double));
    ind->linCmtRateHist = np;
    ind->linCmtRateHistCap = newCap;
  }
  return ind->linCmtRateHist + (size_t)idx * width;
}

// Create linear compartment models for testing
using namespace Rcpp;

// Global linear compartment A model object Since this CAN be
// threaded, this needs to be a std::vector.  This is created once to
// reduce memory allocation and deallocation time.
typedef struct {
  stan::math::linCmtStan lc;
  Eigen::Matrix<double, -1, 1> theta;
  Eigen::Matrix<double, Eigen::Dynamic, 1> fx;
  Eigen::Matrix<double, Eigen::Dynamic, 1> yp;
  Eigen::Matrix<double, Eigen::Dynamic, 2> gg;
} linA_t;

std::vector<linA_t> __linCmtA;

extern "C" void ensureLinCmtA(int nCores) {
  if (__linCmtA.size() < nCores) {
    __linCmtA.resize(nCores);
  }
}

// Global linear compartment B model object
// Refactored to per-thread vector for thread safety, matching linCmtA pattern.
typedef struct {
  stan::math::linCmtStan lc;
  double data[14];
  int numSens;
  Eigen::Matrix<double, Eigen::Dynamic, 1> fx;
  Eigen::Matrix<double, Eigen::Dynamic, 1> yp;
  Eigen::Matrix<double, Eigen::Dynamic, 2> g;
  Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> J;
  Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> Js;
  Eigen::Matrix<double, Eigen::Dynamic, 1> Jg;
  // d(Alast_i)/d(Alast_{i-1}) -- the state-transition Jacobian, only ever
  // populated on demand (which1 == -4, see linCmtB() below), not on every
  // ordinary call -- this is exclusively for the time-varying-covariate AD
  // fix (see project_lincmt_timevarying_covariate_bug); an ordinary,
  // constant-theta solve never requests it and pays nothing extra.
  Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> JAlast;
} linB_t;

std::vector<linB_t> __linCmtB;

extern "C" void ensureLinCmtB(int nCores) {
  if ((int)__linCmtB.size() < nCores) {
    __linCmtB.resize(nCores);
  }
}

#define linCmtBaddrTheta 0
#define linCmtBaddrThetaSens 1
static inline double * getLinCmtDoubleAddr(linB_t &lcb, int type) {
  switch (type) {
  case linCmtBaddrTheta: // max 7
    return lcb.data;
  case linCmtBaddrThetaSens:  // max 7
    return &lcb.data[7];
  // note fx needs cannot be a Map for use in stan :(
  }
  return NULL;
}

// Fill a macro-parameter vector in the order macros2micros() expects, which
// depends on the compartment count and whether the model is oral.  Templated
// on the target because the callers hold theta either as its own Matrix or as
// an Eigen::Map over per-thread scratch.  Returns 0 for a shape that is not a
// linCmt() model, leaving the vector untouched.
template <typename T>
static inline int linCmtFillTheta(T &th, int ncmt, int oral0,
                                  double p1, double v1,
                                  double p2, double p3,
                                  double p4, double p5,
                                  double ka) {
  switch (ncmt + 10*oral0) {
  case 1:  th << p1, v1; return 1;
  case 11: th << p1, v1, ka; return 1;
  case 2:  th << p1, v1, p2, p3; return 1;
  case 12: th << p1, v1, p2, p3, ka; return 1;
  case 3:  th << p1, v1, p2, p3, p4, p5; return 1;
  case 13: th << p1, v1, p2, p3, p4, p5, ka; return 1;
  }
  return 0;
}

// Reverse-mode AD under -DSTAN_THREADS keeps one tape per thread
// (AutodiffStackSingleton::instance_ is thread_local) but creates it lazily;
// an OpenMP worker that never built a ChainableStack has a null tape and
// crashes on its first var.  One static thread_local per thread creates it
// exactly once (a no-op on a thread that already has one).
static inline void linCmtRevTapeInit() {
  static thread_local stan::math::ChainableStack _rxLinCmtTape;
  (void)_rxLinCmtTape;
}

// [[Rcpp::export]]
RObject linCmtModelDouble(double dt,
                          double p1, double v1, double p2,
                          double p3, double p4, double p5,
                          double ka,
                          NumericVector alastNV, NumericVector rateNV,
                          const int ncmt, const int oral0, const int trans,
                          bool deriv,
                          int type,
                          double tau, double tinf, double amt,
                          int bolusCmt,
                          int ndiff,
                          int sensType=3,
                          double sensH=0.001) {
  stan::math::linCmtStan lc(ncmt, oral0, trans, deriv, type, ndiff);
  if (type == linCmtSsInf) {
    lc.setSsInf(tinf, tau);
  } else if (type == linCmtSsBolus) {
    lc.setSsBolus(amt, tau, bolusCmt);
  }
  Eigen::Matrix<double, -1, 1> theta0;
  Eigen::Matrix<double, -1, 1> alast0 = as<Eigen::Matrix<double, -1, 1> >(alastNV);
  Eigen::Matrix<double, -1, 1> rate = as<Eigen::Matrix<double, -1, 1> >(rateNV);
  int nAlast = lc.getNalast();

  if (alast0.size() != nAlast) {
    Rcpp::stop("Alast0 size needs to be %d", nAlast);
  }
  theta0.resize(lc.getNpars());
  Eigen::Map<Eigen::Matrix<double, -1, 1>> theta(theta0.data(), theta0.size());

  linCmtFillTheta(theta, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka);

  int numSens = lc.numSens();
  Eigen::Matrix<double, Eigen::Dynamic, 1> thetaSens0(numSens);
  Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1>> thetaSens(thetaSens0.data(), thetaSens0.size());

  Eigen::Matrix<double, 7, 1> scale;
  scale.setZero();

  // The AD methods (3 and 31 reverse, 30 forward fvar, 100 auto -> reverse)
  // use the unscaled (isAD = true) path so the Jacobian comes out in
  // true-theta units.
  if (sensType == 100) sensType = 31;
  lc.sensTheta(theta, thetaSens, linCmtSensIsAD(sensType), scale.data());

  double *a = new double[nAlast];
  double *asave = new double[nAlast];
  double *r = new double[lc.getNrate()];
  lc.setPtr(a, r, asave);
  lc.setAlast(alast0, nAlast);
  lc.setRate(rate.data());
  lc.setDt(dt);
  List retList;
  if (deriv) {
    Eigen::Matrix<double, Eigen::Dynamic, 1> fx;
    Eigen::Matrix<double, -1, -1> Js(ncmt+ oral0, numSens);//(ncmt + oral0, 2*ncmt + oral0);
    Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> J =
      Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>::Constant(ncmt + oral0, 2*ncmt+ oral0, NA_REAL);
    lc.resizeModel();

    // Getting the sensitivity with numerical differencs
    Eigen::Matrix<double, Eigen::Dynamic, 1> yp(ncmt+oral0, 1);
    Eigen::Matrix<double, Eigen::Dynamic, 2> g(ncmt, 2);
    lc.linAcalcAlast(yp, g, theta);

    // double d = lc.fdoubleh(thetaSens);

    Eigen::Matrix<double, Eigen::Dynamic, 1> h = Eigen::Matrix<double, Eigen::Dynamic, 1>::Constant(thetaSens.size(), 1, 0.001);
    h.setZero();

    switch (sensType) {
    case 1: // forward
      lc.fForwardJac(thetaSens, h.data(), fx, Js);
      break;
    case 2:  // central
      lc.fCentralJac(thetaSens, h.data(), fx, Js);
      break;
    case 3:
    case 31: // reverse-mode AD (the production default via auto)
      linCmtRevTapeInit();
      stan::math::jacobian(lc, thetaSens, fx, Js);
      break;
    case 30:  // forward-mode AD (fvar); should match case 3 to round-off
      lc.linCmtFwdJac(thetaSens, fx, Js);
      break;
    case 10:
      h = Eigen::Matrix<double, Eigen::Dynamic, 1>::Constant(thetaSens.size(), sensH);
      lc.fForwardJac(thetaSens, h.data(), fx, Js);
      break;
    case 20:
      h = Eigen::Matrix<double, Eigen::Dynamic, 1>::Constant(thetaSens.size(), sensH);
      lc.fCentralJac(thetaSens, h.data(), fx, Js);
      break;
    default:
      delete[] a;
      delete[] r;
      delete[] asave;
      Rcpp::stop("linCmtModelDouble: unsupported sensType %d", sensType);
    }
    lc.updateJfromJs(J, Js);
    lc.saveJac(J);
    Eigen::Matrix<double, -1, 1> Jg(ncmt+oral0);
    lc.getJacCp(J, fx, theta, Jg);
    double val = lc.adjustF(fx, theta);
    NumericVector Alast(nAlast);
    for (int i = 0; i < nAlast; i++) {
      Alast[i] = asave[i];
    }
    retList = List::create(_["val"] = wrap(val),
                           _["J"] = wrap(J),
                           _["Jg"] = wrap(Jg),
                           _["Alast"] = Alast);
  } else {
    Eigen::Matrix<double, Eigen::Dynamic, 1> fx;
    Eigen::Matrix<double, Eigen::Dynamic, 1> yp(oral0+ncmt, 1);
    Eigen::Matrix<double, Eigen::Dynamic, 2> g(ncmt, 2);
    lc.linAcalcAlast(yp, g, theta);
    fx = lc(theta);
    double val = lc.adjustF(fx, theta);
    NumericVector Alast(nAlast);
    for (int i = 0; i < nAlast; i++) {
      Alast[i] = asave[i];
    }
    retList = List::create(_["val"] = wrap(val),
                           _["Alast"] = Alast);
  }
  delete[] a;
  delete[] r;
  delete[] asave;
  return retList;
}

// Phase 3b.4 runtime fast-path state for the which1=-5 carry advance (see
// the -5 branch in linCmtB below).  The enable flag is set from the R main
// thread only (test/benchmark toggle; default on); the counters are
// incremented under omp atomic inside threaded inner solves and prove from
// R that the mechanism engaged (feedback_tests_assert_mechanism_used):
// AdvCalls counts every -5 invocation, AdvFast the subset that skipped the
// transition-matrix work.
static int linCmtCarryFastEnabled = 1;
static uint64_t linCmtCarryAdvCallsN = 0;
static uint64_t linCmtCarryAdvFastN = 0;

//' Toggle the linCmt() carry-advance runtime fast path (test/benchmark hook)
//'
//' @param enable logical; new state
//' @return the previous state, invisibly
//' @keywords internal
//' @export
// [[Rcpp::export]]
LogicalVector linCmtCarrySetFast(bool enable) {
  bool prev = linCmtCarryFastEnabled != 0;
  linCmtCarryFastEnabled = enable ? 1 : 0;
  return LogicalVector::create(prev);
}

//' Read (and optionally reset) the linCmt() carry-advance fast-path counters
//'
//' @param reset logical; when TRUE zero the counters after reading
//' @return named numeric vector: advCalls (every which1=-5 invocation),
//'   advFast (subset that took the constant-theta skip)
//' @keywords internal
//' @export
// [[Rcpp::export]]
NumericVector linCmtCarryFastStats(bool reset = false) {
  NumericVector out = NumericVector::create(
    _["advCalls"] = (double)linCmtCarryAdvCallsN,
    _["advFast"] = (double)linCmtCarryAdvFastN);
  if (reset) {
    linCmtCarryAdvCallsN = 0;
    linCmtCarryAdvFastN = 0;
  }
  return out;
}

// Test-only entry point for Phases 2/3b.2 of the sensitivity-carry subsystem
// (project_lincmt_timevarying_covariate_bug / the linCmt-subject-ad plan):
// exercises linCmtB()'s which1=-5/-6/-7 (cumulative carry) sentinels through
// a REAL, already-solved subject context, not a fabricated ind/rx_solve.
// Must be called from R right after an rxSolve() of a real linCmt() model in
// the SAME session (getRxSolve_() returns whatever the most recent solve
// left behind, exactly like every other post-solve accessor in this package
// -- see rxSerialize.cpp/rxData.cpp). `id` is the 0-based subject index.
// `t`/`tPrior` are that subject's real per-row output time and the real
// time of the PRECEDING row (0 for the first row), read back from the
// solved data.frame -- ind->idx/ind->tprior are set here to mirror exactly
// what rxode2_df.cpp's own per-row output-pass loop already does before
// calling calc_lhs for row i, so linCmtB's dt computation
// (ind->doSS ? tout-tprior : _t-tprior) sees the same values a real
// generated model's calc_lhs would produce.
// `theta` is n x 7 (p1, v1, p2, p3, p4, p5, ka) -- per ROW, so a
// time-varying covariate on ANY linCmt() parameter (not just p1) is
// representable, which the 3b.2 multi-pair test needs (e.g. eta.cl on cl
// AND eta.v on a wt-driven v). For which1=-7 the value to add rides in the
// p2 argument slot of linCmtB (per that sentinel's contract), so addVal[i]
// is passed THERE and theta(i,2) is ignored for those rows (no theta is
// read by -7 anyway).
// [[Rcpp::export]]
NumericVector linCmtCarryLiveTest(int id, NumericVector t, NumericVector tPrior,
                                   NumericMatrix theta,
                                   int ncmt, int oral0, int trans,
                                   IntegerVector which1, IntegerVector which2,
                                   Nullable<NumericVector> addVal = R_NilValue) {
  rx_solve *rx = getRxSolve_();
  rx_solving_options_ind *ind = &(rx->subjects[id]);
  int n = t.size();
  if (theta.nrow() != n || theta.ncol() != 7) {
    Rcpp::stop("theta must be length(t) x 7 (p1, v1, p2, p3, p4, p5, ka)");
  }
  int m = ncmt + oral0;
  NumericVector out(n);
  NumericVector av = addVal.isNull() ? NumericVector(n, 0.0) : NumericVector(addVal);
  for (int i = 0; i < n; i++) {
    if (which1[i] == -5 || which1[i] == -6 || which1[i] == -7) {
      int pair = which2[i] / m;
      if (pair < 0 || pair >= RX_LINCMT_CARRY_MAXPAIRS) {
        Rcpp::stop("carry pair index %d out of range (which2=%d, m=%d): at most RX_LINCMT_CARRY_MAXPAIRS=%d simultaneous carry-eligible (linCmt parameter, eta) pairs are supported",
                   pair, which2[i], m, RX_LINCMT_CARRY_MAXPAIRS);
      }
    }
    ind->idx = i;
    ind->tprior = tPrior[i];
    // The -5 advance derives its interval from its own previous invocation
    // time (ind->linCmtCarryTlast) rather than ind->tprior (stale in the
    // post-solve lhs pass) -- seed it per call so this harness keeps its
    // documented caller-supplied t/tPrior interval semantics.  Also pin the
    // slow path (mode 2): this harness's manual drive predates the 3b.4
    // constant-theta skip and its callers (the phase 2/3b.2 benches) assert
    // the full M-advance semantics, constant theta included.
    if (which1[i] == -5) {
      ind->linCmtCarryTlast = tPrior[i];
      ind->linCmtCarryVarying = 2;
    }
    double p2i = which1[i] == -7 ? av[i] : theta(i, 2);
    out[i] = linCmtB(rx, id, t[i], 0, ncmt, oral0, which1[i], which2[i], trans,
                      theta(i, 0), theta(i, 1), p2i, theta(i, 3),
                      theta(i, 4), theta(i, 5), theta(i, 6));
  }
  return out;
}

// Compartment count m = ncmt + oral0 for the carry sentinels, or 0 when the
// shape is not one linCmt() can produce.  A hand-written linCmtB() call can
// pass any ncmt/oral0, and ind->linCmtCarryT only has 4 rows, so every
// sentinel that indexes by row must refuse anything else.
static inline int linCmtCarryM(int ncmt, int oral0) {
  if (ncmt < 1 || ncmt > 3 || oral0 < 0 || oral0 > 1) return 0;
  return ncmt + oral0;
}

// Which thread slots linCmtB() has run on since the last reset -- the
// observable that a solve really was multi-threaded (tests assert on it).
#define RX_LINCMTB_THREAD_SEEN 256
static int linCmtBThreadSeen[RX_LINCMTB_THREAD_SEEN];

// Which sensType codes linCmtB() has computed a Jacobian with since the last
// reset -- the observable that a solve (or a fit) really used a given method.
#define RX_LINCMTB_SENS_SEEN 128
static int linCmtBSensSeen[RX_LINCMTB_SENS_SEEN];

//[[Rcpp::export]]
IntegerVector linCmtBSensTypesSeen(bool reset) {
  std::vector<int> v;
  for (int i = 0; i < RX_LINCMTB_SENS_SEEN; i++) {
    if (linCmtBSensSeen[i]) v.push_back(i);
    if (reset) linCmtBSensSeen[i] = 0;
  }
  return wrap(v);
}

//[[Rcpp::export]]
int linCmtBThreadsSeen(bool reset) {
  int n = 0;
  for (int i = 0; i < RX_LINCMTB_THREAD_SEEN; i++) {
    if (linCmtBThreadSeen[i]) n++;
    if (reset) linCmtBThreadSeen[i] = 0;
  }
  return n;
}

/*
 *  linCmtA
 *
 *  This function is called from rxode2 to compute the linear function
 *  values as well as the compartment amounts.
 *
 *  @param rx The rxSolve object
 *
 *  @param id The subject id
 *
 *  @param linCmt the compartment number of the linear compartment model
 *
 *  @param trans The transformation id
 *
 *  @param ncmt The number of compartments
 *
 *  @param oral0 A indicator of 0 or 1 saying if this was an oral dose
 *
 *  @param which1 The index of the amount to be returned. When less
 *  than zero, this returns the linear compartment model value for the
 *  time.  When greater than zero it returns the amount in the linear
 *  compartment models which can be:
 *
 *   depot, central, peripheral, second peripheral
 *
 *  @param _t The time where the function/jacobian is evaluated
 *
 *  @param p1 The first parameter, can be clearance
 *
 *  @param v1 The central volume
 *
 *  @param p2 The second parameter, can be inter-comparmental clearance
 *
 *  @param p3 The third parameter, can be second peripheral volume
 *
 *  @param p4 The fourth parameter, can be second inter-compartmental
 *            clearance
 *
 *  @param p5 The fifth parameter, can be second peripheral volume
 *
 *  @param ka The first order oral absorption rate constant
 *
 *  @return The function value or the jacobian value
 *
 * This function can bebe called multiple times in the same function.
 *
 * The first time linCmtA is called time _t and a specific id
 * called the function and gradients are calculated.
 *
 * @author Matthew Fidler
 *
 */
extern "C" double linCmtA(rx_solve *rx, int id,
                          double _t,
                          int linCmt, int ncmt,
                          int oral0, int which,
                          int trans,
                          double p1, double v1,
                          double p2, double p3,
                          double p4, double p5,
                          // Oral parameters
                          double ka) {
#define fx    lca.fx
#define J     lca.J
#define Jg    lca.Jg
#define lc    lca.lc
#define theta lca.theta
#define yp    lca.yp
  rx_solving_options_ind *ind = &(rx->subjects[id]);
  rx_solving_options *op = rx->op;
  // get the linear solved system object.
  // rx_get_thread() honors the cross-DLL thread-id override (see rxData.cpp /
  // setRxThreadId): under an external OpenMP team rxode2's omp_get_thread_num()
  // would return 0 for every worker, collapsing this per-thread linCmt scratch
  // onto slot 0.  __linCmtB (line ~489) already uses rx_get_thread().
  linA_t &lca = __linCmtA[rx_get_thread((int)__linCmtA.size())];
  int idx = ind->idx;
  // Create the solved system object
  if (!lc.isSame(ncmt, oral0, trans, rx->ndiff)) {
    lc.setModelType(ncmt, oral0, trans, ind->linSS, rx->ndiff);
    // only resize when needed
    theta = Eigen::Matrix<double, Eigen::Dynamic, 1>(lc.getNpars());
    fx = Eigen::Matrix<double, Eigen::Dynamic, 1>(ncmt + oral0);
    yp = Eigen::Matrix<double, Eigen::Dynamic, 1>(ncmt + oral0, 1);
    lca.gg = Eigen::Matrix<double, Eigen::Dynamic, 2>(ncmt, 2);
  } else {
    lc.setSsType(ind->linSS);
  }
  if (ind->linSS == linCmtSsInf) {
    lc.setSsInf(ind->linSSvar, ind->linSStau);
  } else if (ind->linSS == linCmtSsBolus) {
    lc.setSsBolus(ind->linSSvar, ind->linSStau, ind->linSSbolusCmt);
  }

  // Get number of items in Alast
  int nAlast = lc.getNalast();

  // Get/Set the pointers
  double *asave = ind->linCmtSave;
  double *r = getLinRate;
  double *a;
  if (ind->linCmtAlast == NULL) {
    a = getAdvan(ind->solvedIdx);
  } else {
    a = ind->linCmtAlast;
  }
  lc.setPtr(a, r, asave);
  // Setup parameter matrix
  linCmtFillTheta(theta, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka);

  // Here we restore the last solved value
  if (!ind->doSS && ind->solvedIdx >= idx) {
    double *acur = getAdvan(idx);
    if (which < 0) {
      fx = lc.restoreFx(acur);
      return lc.adjustF(fx, theta);
    } else {
      return acur[which];
    }
  }
  // Currently this may not have been calculated, calculate now
  if (which < 0) {
    if (ind->_rxFlag == 11) {
      // If we are calculating the LHS values or other values, these are
      // stored in the corresponding compartments.
      //
      // This also handles the case where _t = ind->tcur, where the
      // solution is already known
      // ind->linCmtSave = getAdvan(idx);
      fx = lc.restoreFx(getAdvan(idx));
    } else {
      // Here we are doing ODE solving OR only linear solving
      // so we calculate these values here.
      //
      // For these cases:

      // ind->tprior gives the prior known time or current time solved to
      //
      // ind->tout gives the time solved
      //
      // _t gives the time requested to solve for (which with ODE
      // solving may not be tout); note that if _t = ind->tprior the
      // solution is the last solution solved or initial conditions
      //

      // Get/Set the dt; This is only applicable in the ODE/linCmt() case

      double dt;
      if (ind->doSS) {
        dt = ind->tout - ind->tprior;
      } else {
        dt =  _t - ind->tprior;
      }
      lc.setDt(dt);

      lc.linAcalcAlast(yp, lca.gg, theta);

      fx = lc(theta);
    }
    return lc.adjustF(fx, theta, ind->linCmtHV);
  } else if (which >= 0 && which < nAlast) {
    // Return the amount in the linear compartment model
    // which can be depot, central, peripheral, second peripheral
    // This assumes that the function value is the first
    if (ind->_rxFlag != 11) {
      return ind->linCmtSave[which];
    } else {
      double *acur = getAdvan(idx);
      return acur[which];
    }
  }
  // Invalid index
  return NA_REAL;
#undef fx
#undef J
#undef Jg
#undef lc
#undef theta
#undef yp
}

// These scaling/step-size helpers are read from the finite-difference H setup
// (shi21ForwardH/gillForwardH) after ind_linCmtFH has populated THIS thread's
// linCmtB slot, so they must read the same per-thread slot rx_get_thread()
// selects -- not the hardcoded [0] slot, which another thread mutates (and may
// resize) during a parallel linCmt solve.  rx_get_thread() takes an int, so the
// size_t pool size is cast once to avoid a narrowing conversion.
extern "C" double linCmtScaleInitPar(int which) {
  int tid = rx_get_thread((int)__linCmtB.size());
  return __linCmtB[tid].lc.initPar(which);
}

extern "C" double linCmtScaleInitN() {
  int tid = rx_get_thread((int)__linCmtB.size());
  Eigen::Matrix<double, Eigen::Dynamic, 1> theta = __linCmtB[tid].lc.initPar();
  return theta.size();
}

extern "C" int linCmtZeroJac(int i) {
  int tid = rx_get_thread((int)__linCmtB.size());
  return __linCmtB[tid].lc.parDepV1(i);
}



// linCmtB's which1 = -3 case: the dose-time (moving boundary) sensitivity.
//
// `amt` holds the amounts at the requested time from the which1=-1/which2=-1
// call the caller is required to have made, and the linear system's own
// right-hand side gives d/dL exactly (see linCmtStan::dAdt()).  `rate` is
// either the live ind->InfusionRate slice (while genuinely solving) or the
// linCmtBRateSlot() cache for that idx (a re-query, e.g. the output pass);
// see the caller in linCmtB().  Returns NA_REAL for a call that does not
// describe the model `lc` is set up for, when `rate` could not be recovered
// (NULL -- see linCmtBRateSlot()), for a steady-state infusion
// (linCmtHasSsInfusion()), or for an out of range `which2`.
static inline double linCmtBdoseTime(stan::math::linCmtStan &lc,
                                     const Eigen::Matrix<double, Eigen::Dynamic, 1> &amt,
                                     rx_solving_options_ind *ind,
                                     const double *rate,
                                     int ncmt, int oral0, int which2, int trans,
                                     double p1, double v1,
                                     double p2, double p3,
                                     double p4, double p5,
                                     double ka) {
  if (lc.ncmt_ != ncmt || lc.oral0_ != oral0 || lc.trans_ != trans ||
      (int)amt.size() != ncmt + oral0) {
    return NA_REAL;
  }
  if (rate == NULL) return NA_REAL;
  if (linCmtHasSsInfusion(ind)) return NA_REAL;
  Eigen::Matrix<double, Eigen::Dynamic, 1> th(lc.getNpars());
  if (!linCmtFillTheta(th, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka)) {
    return NA_REAL;
  }
  Eigen::Matrix<double, Eigen::Dynamic, 2> gm =
    stan::math::macros2micros(th, ncmt, trans);
  Eigen::Matrix<double, Eigen::Dynamic, 1> dot(ncmt + oral0);
  lc.dAdt(amt, gm, ka, rate, dot);
  if (which2 == -3) return -dot(oral0, 0) / lc.getVc(th);
  if (which2 >= 0 && which2 < ncmt + oral0) return -dot(which2, 0);
  return NA_REAL;
}

/*
 *  linCmtB
 *
 *  This function is called from rxode2 to compute both the jacobian of
 *  the linear model and the function value.
 *
 *  @param rx The rxSolve object
 *
 *  @param id The subject id
 *
 *  @param linCmt the compartment number of the linear compartment model
 *
 *  @param trans The transformation id
 *
 *  @param ncmt The number of compartments
 *
 *  @param oral0 A indicator of 0 or 1 saying if this was an oral dose
 *
 *  @param which1 The first index of the Jacobian (0 indexed; compartment number)

 *  @param which2 The second index of the Jacobian (0 indexed; parameter number)

 *
 *  When which1 and which2 are both -1, the solved linear compartment
 *  model value returned
 *
 *  When which2 is -2, the amounts in the saved function are returned
 *  with which1 (zero indexed)
 *
 *  The order of the amounts is as follows:
 *
 *   (depot if present), central, peripheral, second peripheral
 *
 *  When which1 is -2, the gradient of the linear compartment model
 *  with respect to the parameter is returned.
 *
 *  When which1 is -3, the DOSE-TIME (moving boundary) sensitivity is
 *  returned -- the derivative with respect to a delay applied to every dose
 *  feeding the linear system, i.e. what a modeled `alag()` on its dosed
 *  compartment produces (nlmixr2/rxode2#1119).  which2 = -3 gives it for the
 *  reported concentration, which2 >= 0 for the amount in that compartment.
 *  The system is linear and its whole input is delayed together, so
 *  A(t; L) = A(t - L; 0) and the derivative is exactly -dA/dt; chain-rule it
 *  with d(alag)/dp to get the sensitivity wrt a model parameter.  This
 *  requires that EVERY dose reaching the linear system carries the same
 *  `alag()`; it is not the per-compartment derivative of a model that lags
 *  its compartments differently (nlmixr2/rxode2#1237).  A model that would
 *  violate this -- calling `linCmtB(which1 = -3)` while its linCmt()
 *  compartments carry more than one distinct `alag()` expression -- is
 *  refused at build time by `.rxLinCmtDoseTimeSensCheck()` (R/eventSens.R)
 *  rather than silently given the single-delay answer.  A regular infusion's
 *  rate is recovered at output time via the linCmtBRateSlot() per-idx cache
 *  (nlmixr2/rxode2#1236); an individual with a steady-state infusion still
 *  gets `NA_REAL` -- see linCmtHasSsInfusion().
 *
 *  The parameter order is as follows:
 *
 *   p1, v1, p2, p3, p4, p5, ka; for 3 compartment models
 *
 *   p1, v1, p2, p3, ka; for 2 compartment models
 *
 *   p1, v1, ka; for 1 compartment models
 *
 *  The ka is only appended for oral model
 *
 *  @param _t The time where the function/jacobian is evaluated
 *
 *  @param p1 The first parameter, can be clearance
 *
 *  @param v1 The central volume
 *
 *  @param p2 The second parameter, can be inter-comparmental clearance
 *
 *  @param p3 The third parameter, can be second peripheral volume
 *
 *  @param p4 The fourth parameter, can be second inter-compartmental
 *            clearance
 *
 *  @param p5 The fifth parameter, can be second peripheral volume
 *
 *  @param ka The first order oral absorption rate constant
 *
 *  @return The function value or the jacobian value
 *
 * This function will likely be called multiple times in the same ODE
 * system when running focei.
 *
 * The first time linCmtB is called time _t and a specific id
 * called the function and gradients are calculated.
 *
 * @author Matthew Fidler
 *
*/
// linCmtSensIsAD() (the AD jacobian classifier; forward-mode fvar 3/30,
// reverse-mode 31, auto 100 -> unscaled thetaSens + passthrough trueTheta; the
// finite-difference methods keep the scaled path) lives in linCmtSensType.h so
// linCmt.cpp, par_solve.cpp and rxData.cpp share one definition.
extern "C" double linCmtB(rx_solve *rx, int id,
                          double _t, int linCmt,
                          int ncmt, int oral0,
                          int which1, int which2,
                          int trans,
                          double p1, double v1,
                          double p2, double p3,
                          double p4, double p5,
                          // Oral parameters
                          double ka) {
  // Per-thread linCmtB state (matching linCmtA pattern for thread safety)
  int _tid = rx_get_thread(__linCmtB.size());
  linB_t &lcb = __linCmtB[_tid];
  if (_tid < RX_LINCMTB_THREAD_SEEN) {
#pragma omp atomic write
    linCmtBThreadSeen[_tid] = 1;
  }
#define fx        lcb.fx
#define Jg        lcb.Jg
#define lc        lcb.lc
#define J         lcb.J
#define Js        lcb.Js
#define yp        lcb.yp
#define g         lcb.g
  rx_solving_options_ind *ind = &(rx->subjects[id]);
  rx_solving_options *op = rx->op;
  int idx = ind->idx;
  bool resized = false;
  // Create the solved system object
  if (which1 != -1 || which2 != -1) {
    // If we are calculating the LHS values or other values, these are
    // stored in the corresponding compartments.
    //
    // This assumes that the linear compartment solution of which=-1,
    // -1 has already been called
    //
    // This also handles the case where _t = ind->tcur, where the
    // solution is already known
    // double *acur = getAdvan(idx);
    // J  = lc.restoreJac(acur);
    // fx = lc.restoreFx(acur);
    if (which1 >= 0 && which2 >= 0) {
      // w1, w2 are > 0
      return J(which1, which2);
    } else if (which1 >= 0 && which2 == -2) {
      // w2 < 0
      return fx(which1);
    } else if (which1 == -2 && which2 >= 0) {
      return Jg(which2);
    } else if (which1 == -3) {
      // Mirrors the fx/J restore-vs-solve condition above (idx already
      // solved -> a re-query, e.g. the output pass, where ind->InfusionRate
      // has since been cleared/moved on; use the cached rate instead).
      const double *rate = (!ind->doSS && ind->solvedIdx >= idx) ?
        linCmtBRateSlot(ind, idx, op->numLin, 0) : getLinRate;
      return linCmtBdoseTime(lc, fx, ind, rate, ncmt, oral0, which2,
                             trans, p1, v1, p2, p3, p4, p5, ka);
    } else if (which1 == -4) {
      // d(Alast_i)/d(Alast_{i-1}) -- the state-transition Jacobian, the one
      // missing ingredient for a general (arbitrary covariate formula)
      // time-varying-covariate fix (project_lincmt_timevarying_covariate_bug).
      // Self-contained (recomputes theta/g fresh from the passed-in p1/v1/...
      // rather than relying on any state cached by a prior -1,-1 call) so it
      // does not depend on call-ordering assumptions the other sentinels
      // above rely on. which2 packs (row, col) as row + m*col, row = output
      // compartment, col = which PREVIOUS-timepoint compartment is being
      // differentiated against; m = ncmt+oral0 is derivable by the caller
      // from its own model knowledge. A constant matrix (the closed form is
      // linear in Alast) recomputed via one forward-mode (fvar) pass per
      // call -- opt-in only: an ordinary constant-theta solve never requests
      // which1=-4, so this adds no cost to the common case. Validated (as
      // linCmtAlastTransitionMatrixProto) against rxode2's own
      // linToOde()-generated equivalent ODE model to machine epsilon across
      // all 1/2/3-cmt IV/oral configs -- see
      // feedback_lincmt_verify_against_linToOde_not_invented_odes.
      int m = linCmtCarryM(ncmt, oral0);
      if (m == 0) return NA_REAL;
      int row = which2 % m;
      int col = which2 / m;
      if (which2 < 0 || col >= m) return NA_REAL;
      int npars = lc.getNpars();
      typedef stan::math::fvar<double> fv;
      Eigen::Matrix<double, Eigen::Dynamic, 1> thetaD(npars);
      linCmtFillTheta(thetaD, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka);
      Eigen::Matrix<fv, Eigen::Dynamic, 1> thetaF(npars);
      for (int k = 0; k < npars; k++) thetaF(k, 0) = fv(thetaD(k, 0), 0.0);
      Eigen::Matrix<fv, Eigen::Dynamic, 2> gF = stan::math::macros2micros(thetaF, ncmt, trans);
      fv kaV(0.0, 0.0);
      if (oral0) kaV = thetaF(ncmt*2, 0);

      double dt = ind->doSS ? (ind->tout - ind->tprior) : (_t - ind->tprior);
      lc.setDt(dt);
      const double *rate = (!ind->doSS && ind->solvedIdx >= idx) ?
        linCmtBRateSlot(ind, idx, op->numLin, 0) : getLinRate;
      lc.setRate(const_cast<double*>(rate));

      Eigen::Matrix<fv, Eigen::Dynamic, 1> yp0 =
        Eigen::Matrix<fv, Eigen::Dynamic, 1>::Zero(m);
      yp0(col, 0) = fv(0.0, 1.0);
      Eigen::Matrix<fv, Eigen::Dynamic, 1> ret(m);
      if (ncmt == 1) lc.linCmtStan1<fv>(gF, yp0, kaV, ret);
      else if (ncmt == 2) lc.linCmtStan2<fv>(gF, yp0, kaV, ret);
      else if (ncmt == 3) lc.linCmtStan3<fv>(gF, yp0, kaV, ret);
      return ret(row, 0).d_;
    } else if (which1 == -7) {
      // Add a caller-supplied local contribution (dPredDTheta_i *
      // dThetaDEta_i, computed by the caller -- R for now, generated model
      // code once Phase 3b.3 wires this in) into the stored cumulative carry
      // at (row, pair), ON TOP OF whatever which1=-5 already accumulated for
      // this row via the state-transition multiply. which2 packs (row, pair)
      // as row + m*pair like -5/-6; the value to add rides in p2 (unused
      // by this sentinel otherwise -- no theta is read here).
      int m = linCmtCarryM(ncmt, oral0);
      if (m == 0) return NA_REAL;
      int row = which2 % m;
      int pair = which2 / m;
      if (which2 < 0 || pair >= RX_LINCMT_CARRY_MAXPAIRS) return NA_REAL;
      ind->linCmtCarryT[row*RX_LINCMT_CARRY_MAXPAIRS + pair] += p2;
      return ind->linCmtCarryT[row*RX_LINCMT_CARRY_MAXPAIRS + pair];
    } else if (which1 == -5 || which1 == -6) {
      // Cumulative-carry sentinels (per-subject storage: ind->linCmtCarryT,
      // see rxode2parseStruct.h; reset at iniSubject() in par_solve.h).
      // The stored buffer is 4 rows (compartments; top m = ncmt+oral0 used)
      // x RX_LINCMT_CARRY_MAXPAIRS columns, row-major, stride
      // RX_LINCMT_CARRY_MAXPAIRS. Each COLUMN is one carry-eligible
      // (linCmt-parameter, eta) pair's own carried d(Alast)/d(eta) m-vector,
      // evolving independently as s_i = M_i*s_{i-1} + (which1=-7
      // contributions). which2 packs (row, pair) as row + m*pair -- THIS is
      // the encoding 3b.3's codegen must emit. A pair index >=
      // RX_LINCMT_CARRY_MAXPAIRS returns NA (visible poison, no OOB write);
      // the model-build layer must enforce the cap before emitting code.
      //
      // which1=-6 is a pure read: return the current cumulative
      // ind->linCmtCarryT[(row,pair)] with no recomputation -- lets a caller
      // inspect s_i without re-triggering an advance.
      //
      // which1=-5 is the mutating advance: s_i = M_i * s_{i-1} for EVERY
      // pair column at once, where M_i is THIS interval's local transition
      // matrix (the same quantity which1=-4 returns one column of,
      // recomputed here for every column since the full m x m matrix is
      // needed for the multiply). M_i depends only on this row's OWN theta
      // values -- identical no matter which eta a pair differentiates
      // against -- so ONE advance serves every pair. Must be called EXACTLY
      // ONCE PER ROW per subject (documented contract -- NOT once per row
      // per pair; a second call for the same row would apply the same
      // transition twice to every column). Composing each pair's OWN local
      // contribution (dPredDTheta_i * dThetaDEta_i) is left to the caller
      // via which1=-7 -- this sentinel only carries the state-transition
      // part forward.
      int m = linCmtCarryM(ncmt, oral0);
      if (m == 0) return NA_REAL;
      int row = which2 % m;
      int pair = which2 / m;
      if (which2 < 0 || pair >= RX_LINCMT_CARRY_MAXPAIRS) return NA_REAL;
      if (which1 == -6) {
        return ind->linCmtCarryT[row*RX_LINCMT_CARRY_MAXPAIRS + pair];
      }
#pragma omp atomic
      linCmtCarryAdvCallsN++;
      // Runtime per-subject fast path (phase 3b.4): while this subject's
      // theta has been identical on every row of the CURRENT pass, the
      // carried recurrence telescopes to G*J_n with J the production
      // constant-theta Jacobian (exact there), so skipping the M advance --
      // leaving the carry AND tracker columns un-multiplied -- makes the
      // emitted -7 adds accumulate G*(J_i - J_{i-1}) = G*J_n.  First row is
      // always skippable (carry state is 0, M*0 = 0 bit-identically).  Mode
      // lives in ind and resets at iniSubject(), so the comparison is
      // within-pass only -- etas changing between inner iterations never
      // cross it.  Tlast must still advance so a later theta change resumes
      // the slow path with the correct interval.
      // PRECONDITION: this skip is exact only for the tracker-column
      // calling convention nlmixr2est's carry codegen emits (each pair's
      // -7 add is G*(J_i - P) with P the pair's tracker column, itself
      // updated by a -7 add of J_i - P), because that is what telescopes.
      // A caller feeding -7 an arbitrary local contribution (the generic
      // s_i = M_i*s_{i-1} + c_i reading of this sentinel) must pin the slow
      // path with linCmtCarrySetFast(FALSE) or set ind->linCmtCarryVarying
      // = 2 (as linCmtCarryLiveTest does).
      if (linCmtCarryFastEnabled) {
        double thNow[7] = {p1, v1, p2, p3, p4, p5, ka};
        if (ind->linCmtCarryVarying == 0) {
          memcpy(ind->linCmtCarryPrevTheta, thNow, sizeof(thNow));
          ind->linCmtCarryVarying = 1;
          ind->linCmtCarryTlast = _t;
#pragma omp atomic
          linCmtCarryAdvFastN++;
          return ind->linCmtCarryT[row*RX_LINCMT_CARRY_MAXPAIRS + pair];
        }
        if (ind->linCmtCarryVarying == 1) {
          // Exact bit compare; any difference (including NaN anywhere)
          // flips permanently to the slow path for this pass.
          if (memcmp(ind->linCmtCarryPrevTheta, thNow, sizeof(thNow)) == 0) {
            ind->linCmtCarryTlast = _t;
#pragma omp atomic
            linCmtCarryAdvFastN++;
            return ind->linCmtCarryT[row*RX_LINCMT_CARRY_MAXPAIRS + pair];
          }
          ind->linCmtCarryVarying = 2;
        }
      }
      // Unlike which1=-4 (which always follows a which1=-1,-1 call earlier
      // in the same calc_lhs invocation, guaranteeing lc is already sized
      // for this ncmt/oral0/trans), which1=-5 may be the first touch of lc
      // on this thread for a standalone re-query -- size it defensively.
      if (!lc.isSame(ncmt, oral0, trans, rx->ndiff)) {
        lc.setModelType(ncmt, oral0, trans, ind->linSS, rx->ndiff);
      }
      // The advance interval comes from this sentinel's OWN previous
      // invocation time, not ind->tprior: calc_lhs fires exactly once per
      // event row in solve order, but in the post-solve lhs pass ind->tprior
      // is stale (frozen at the solve's final interval), so _t - tprior is
      // wrong for every row but the last.  linCmtCarryTlast is reset to NAN
      // at iniSubject(); the first row advances over dt = 0 (M = I,
      // harmless: the carry state is still 0).
      double carryDt = ISNAN(ind->linCmtCarryTlast) ? 0.0 :
        (_t - ind->linCmtCarryTlast);
      ind->linCmtCarryTlast = _t;
      int npars = lc.getNpars();
      typedef stan::math::fvar<double> fv;
      Eigen::Matrix<double, Eigen::Dynamic, 1> thetaD(npars);
      linCmtFillTheta(thetaD, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka);
      Eigen::Matrix<fv, Eigen::Dynamic, 1> thetaF(npars);
      for (int k = 0; k < npars; k++) thetaF(k, 0) = fv(thetaD(k, 0), 0.0);
      Eigen::Matrix<fv, Eigen::Dynamic, 2> gF = stan::math::macros2micros(thetaF, ncmt, trans);
      fv kaV(0.0, 0.0);
      if (oral0) kaV = thetaF(ncmt*2, 0);

      lc.setDt(carryDt);
      const double *rate = (!ind->doSS && ind->solvedIdx >= idx) ?
        linCmtBRateSlot(ind, idx, op->numLin, 0) : getLinRate;
      // linCmtBRateSlot() returns NULL when this idx's rate was never
      // cached live -- expected for -5 called as a standalone re-query well
      // after the whole subject finished solving (the cache is only ever
      // written while an idx is genuinely being solved for the first time,
      // src/linCmt.cpp ~line 1698), which never happens again once
      // ind->solvedIdx has reached its final value. In that situation
      // ind->InfusionRate (getLinRate) is still a safe, defined per-thread
      // buffer to fall back to -- same defensive intent already documented
      // on linCmtBRateSlot() itself ("NULL is a defensive fallback ... not
      // a case expected to occur"), just now actually handled by a caller.
      if (rate == NULL) rate = getLinRate;
      lc.setRate(const_cast<double*>(rate));

      // Full local m x m transition matrix: one forward-mode pass per column.
      double localM[16];
      for (int c = 0; c < m; c++) {
        Eigen::Matrix<fv, Eigen::Dynamic, 1> yp0c =
          Eigen::Matrix<fv, Eigen::Dynamic, 1>::Zero(m);
        yp0c(c, 0) = fv(0.0, 1.0);
        Eigen::Matrix<fv, Eigen::Dynamic, 1> retc(m);
        if (ncmt == 1) lc.linCmtStan1<fv>(gF, yp0c, kaV, retc);
        else if (ncmt == 2) lc.linCmtStan2<fv>(gF, yp0c, kaV, retc);
        else if (ncmt == 3) lc.linCmtStan3<fv>(gF, yp0c, kaV, retc);
        for (int r = 0; r < m; r++) localM[r*4 + c] = retc(r, 0).d_;
      }
      // Advance EVERY pair column at once: s_new = M * s_old per column
      // (only the top m rows participate; untouched rows of the buffer stay
      // 0 from iniSubject()'s reset).
      double tNew[4*RX_LINCMT_CARRY_MAXPAIRS];
      for (int r = 0; r < m; r++) {
        for (int c = 0; c < RX_LINCMT_CARRY_MAXPAIRS; c++) {
          double s = 0.0;
          for (int k2 = 0; k2 < m; k2++) {
            s += localM[r*4 + k2] *
              ind->linCmtCarryT[k2*RX_LINCMT_CARRY_MAXPAIRS + c];
          }
          tNew[r*RX_LINCMT_CARRY_MAXPAIRS + c] = s;
        }
      }
      for (int r = 0; r < m; r++) {
        for (int c = 0; c < RX_LINCMT_CARRY_MAXPAIRS; c++) {
          ind->linCmtCarryT[r*RX_LINCMT_CARRY_MAXPAIRS + c] =
            tNew[r*RX_LINCMT_CARRY_MAXPAIRS + c];
        }
      }
      return ind->linCmtCarryT[row*RX_LINCMT_CARRY_MAXPAIRS + pair];
    }
  } else if (!lc.isSame(ncmt, oral0, trans, rx->ndiff)) {
    lc.setModelType(ncmt, oral0, trans, ind->linSS, rx->ndiff);
    // only resize when needed
    fx = Eigen::Matrix<double, Eigen::Dynamic, 1>(ncmt + oral0);
    int npars = lc.getNpars();
    // NA fill and resize
    J = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>::Constant(ncmt + oral0, npars, NA_REAL);

    lcb.numSens = lc.numSens();
    Js = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(ncmt+oral0, lcb.numSens);//(ncmt + oral0, 2*ncmt + oral0);
    // thetaSens.resize(numSens);

    // AlastA.resize(ncmt + oral0);
    Jg = Eigen::Matrix<double, Eigen::Dynamic, 1>(lc.getNpars());

    yp = Eigen::Matrix<double, Eigen::Dynamic, 1>(ncmt + oral0);
    g = Eigen::Matrix<double, Eigen::Dynamic, 2>(ncmt, 2);
    lc.setForwardOpts(rx->linCmtSuspect, rx->linCmtForwardMax);
  } else {
    lc.setSsType(ind->linSS);
  }
  if (id == 0 && ind->linH[0] == 0) {
    lc.resetFlags();
  }
  lc.setId(id);

  Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1> >
    theta(getLinCmtDoubleAddr(lcb, linCmtBaddrTheta), lc.getNpars());

  linCmtFillTheta(theta, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka);

  Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1> >
    thetaSens(getLinCmtDoubleAddr(lcb, linCmtBaddrThetaSens), lcb.numSens);

  // isAD (unscaled thetaSens + passthrough trueTheta) is used by every AD
  // jacobian path: forward-mode (3/30/auto), reverse-mode (31). The finite
  // difference methods keep the scaled path.
  lc.sensTheta(theta, thetaSens, linCmtSensIsAD(rx->sensType), rx->linCmtScale);
  if (ind->linSS == linCmtSsInf) {
    lc.setSsInf(ind->linSSvar, ind->linSStau);
  } else if (ind->linSS == linCmtSsBolus) {
    lc.setSsBolus(ind->linSSvar, ind->linSStau, ind->linSSbolusCmt);
  }

  // Get number of items in Alast
  int nAlast = lc.getNalast();

  // Get/Set the pointers
  double *asave = ind->linCmtSave;
  double *r = getLinRate;
  double *a;

  // idx is genuinely being solved right now (not a re-query of an
  // already-solved index, e.g. the output pass) -- ind->InfusionRate is live
  // and correct, so cache it for linCmtBdoseTime() (which1 = -3) to read back
  // on a later re-query, once ind->InfusionRate has been cleared/moved on.
  if (op->numLin > 0 && (ind->doSS || ind->solvedIdx < idx)) {
    double *rslot = linCmtBRateSlot(ind, idx, op->numLin, 1);
    if (rslot != NULL) std::copy(r, r + op->numLin, rslot);
  }

  if (ind->linCmtAlast == NULL) {
    a = getAdvan(ind->solvedIdx);
  } else {
    a = ind->linCmtAlast;
  }
  lc.setPtr(a, r, asave);

  // Setup parameter matrix


  // Here we restore the last solved value
  if (!ind->doSS && ind->solvedIdx >= idx) {
    double *acur = getAdvan(idx);
    J = lc.restoreJac(acur);
    fx = lc.restoreFx(acur);
  } else {
    // Calculate everything while solving using linCmt()
    if (ind->_rxFlag == 11) {
      // If we are calculating the LHS values or other values, these are
      // stored in the corresponding compartments.
      //
      // This also handles the case where _t = ind->tcur, where the
      // solution is already known
      // ind->linCmtSave = getAdvan(idx);
      double *acur = getAdvan(idx);
      J = lc.restoreJac(acur);
      fx = lc.restoreFx(acur);
    } else {
      // Here we are doing ODE solving OR only linear solving
      // so we calculate these values here.
      //
      // For these cases:

      // ind->tprior gives the prior known time or current time solved to
      //
      // ind->tout gives the time solved
      //
      // _t gives the time requested to solve for (which with ODE
      // solving may not be tout); note that if _t = ind->tprior the
      // solution is the last solution solved or initial conditions
      //

      // Get/Set the dt; This is only applicable in the ODE/linCmt() case
      double dt;
      if (ind->doSS) {
        dt = ind->tout - ind->tprior;
      } else {
        dt =  _t - ind->tprior;
      }
      lc.setDt(dt);
      if (rx->ndiff == 0) {
        lc.linAcalcAlast(yp, g, theta);
        lc.calcFx(thetaSens);
        lc.fHCalcJac(thetaSens,ind->linH, fx, Js);
      } else if (ind->linCmtHparIndex >= -1) {
        if (ind->linCmtHparIndex >= 0) {
          thetaSens(ind->linCmtHparIndex, 0) += ind->linCmtH;
        }
        lc.linAcalcAlast(yp, g, theta);
        lc.calcFx(thetaSens);
        lc.fHCalcJac(thetaSens,ind->linH, fx, Js);
      } else {
        if (rx->sensType >= 0 && rx->sensType < RX_LINCMTB_SENS_SEEN) {
#pragma omp atomic write
          linCmtBSensSeen[rx->sensType] = 1;
        }
        switch (rx->sensType) {
        case 1: // forward
        case 10:
        case 6: // forward difference with gill H est
          lc.linAcalcAlast(yp, g, theta);
          lc.calcFx(thetaSens);
          lc.fForwardJac(thetaSens, ind->linH, fx, Js);
          break;

        case 20:
        case 2:  // central
          lc.linAcalcAlast(yp, g, theta);
          lc.calcFx(thetaSens);
          lc.fCentralJac(thetaSens, ind->linH, fx, Js);
          break;

        case 40: // 3-point forward difference
        case 4:  // 3-point forward difference
        case 7:  // 3-point forward difference with gill H est
          lc.linAcalcAlast(yp, g, theta);
          lc.calcFx(thetaSens);
          lc.fF3Jac(thetaSens, ind->linH, fx, Js);
          break;

        case 50: // 5-point endpoint difference
        case 5: // 5-point endpoint difference
          lc.linAcalcAlast(yp, g, theta);
          lc.calcFx(thetaSens);
          lc.fEndpoint5Jac(thetaSens, ind->linH, fx, Js);
          break;

        case 31: // reverse-mode AD (escape hatch / validation)
          linCmtRevTapeInit();
          stan::math::jacobian(lc, thetaSens, fx, Js);
          break;

        case 3:  // "AD": now forward-mode (fvar); matches reverse to round-off
        case 30: // explicit forward-mode AD
        default: // auto and anything unspecified -> forward-mode AD
          lc.linCmtFwdJac(thetaSens, fx, Js);
          break;
        }
        lc.updateJfromJs(J, Js);
        lc.saveJac(J);
      }
    }
  }
  lc.getJacCp(J, fx, theta, Jg);
  return lc.adjustF(fx, theta, ind->linCmtHV);
#undef fx
#undef J
#undef Jg
#undef lc
#undef Js
#undef yp
#undef g
}
