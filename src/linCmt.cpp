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

#define RX_LINHYB_MAXM 4
#define RX_LINHYB_MAXP 7

// One superposition term of the hybrid strategy's observation phase: an
// amount deposited at time t whose own theta-dependence rides in J
// (row-major m x RX_LINHYB_MAXP; all zero for a plain dose), or a step
// change in the infusion-rate vector at time t.  Under constant theta the
// terms evolve independently and the state at a later time is their sum.
typedef struct {
  double t;
  int isRate;
  double amt[RX_LINHYB_MAXM];
  double J[RX_LINHYB_MAXM*RX_LINHYB_MAXP];
  double rate[RX_LINHYB_MAXM];
} linHybEntry;

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
  // Hybrid-strategy state (rxControl(linCmtSensStrategy), see
  // linCmtHybRow()).  A subject is solved start to finish on one thread, so
  // per-thread storage keyed on (hybId, hybLastIdx, hybLastT) suffices.
  std::vector<linHybEntry> hybList;
  int hybId = -1;
  int hybLastIdx = -1;
  double hybLastT = 0.0;
  double hybLastFx[RX_LINHYB_MAXM];
  double hybLastJ[RX_LINHYB_MAXM*RX_LINHYB_MAXP];
  double hybPrevRate[RX_LINHYB_MAXM];
  double hybTheta[RX_LINHYB_MAXP];
  int hybFull = 0;
} linB_t;

std::vector<linB_t> __linCmtB;

extern "C" void ensureLinCmtB(int nCores) {
  if ((int)__linCmtB.size() < nCores) {
    __linCmtB.resize(nCores);
  }
}

// (Re)size a thread's linCmtB slot for a model shape: the kernel AND the
// per-slot scratch (fx/J/Js/Jg/yp/g, numSens).  Every path that can be the
// first touch of a slot for a shape must go through here -- sizing only the
// kernel leaves isSame() true for the next ordinary call, which then skips
// this block and runs on zero-length scratch.
static inline void linCmtBsetModel(linB_t &lcb, int ncmt, int oral0, int trans,
                                   int linSS, rx_solve *rx) {
  lcb.lc.setModelType(ncmt, oral0, trans, linSS, rx->ndiff);
  int npars = lcb.lc.getNpars();
  lcb.fx = Eigen::Matrix<double, Eigen::Dynamic, 1>(ncmt + oral0);
  lcb.J = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>::Constant(ncmt + oral0, npars, NA_REAL);
  lcb.numSens = lcb.lc.numSens();
  lcb.Js = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(ncmt + oral0, lcb.numSens);
  lcb.Jg = Eigen::Matrix<double, Eigen::Dynamic, 1>(npars);
  lcb.yp = Eigen::Matrix<double, Eigen::Dynamic, 1>(ncmt + oral0);
  lcb.g = Eigen::Matrix<double, Eigen::Dynamic, 2>(ncmt, 2);
  lcb.lc.setForwardOpts(rx->linCmtSuspect, rx->linCmtForwardMax);
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

// ---- hybrid sensitivity strategy ------------------------------------------
//
// rxControl(linCmtSensStrategy="auto"/"hybrid"): a subject's rows up to the
// start of its trailing observation-only run are solved by the sequential
// kernel (phase 1, whatever linCmtSensType picked); from that row on the
// per-row amount+Jacobian buffer is filled by superposition over the phase-1
// end state (phase 2), one reverse-mode nest per row that sweeps only the
// pred (central) output.  A FOCEi inner model reads only d(cp)/d(theta), so
// that single sweep replaces one forward pass per requested direction or one
// adjoint sweep per compartment.
//
// Buffer contract for a phase-2 row: amounts for every compartment are exact
// (plain doubles from the term sum); the Jacobian block holds the central row
// and, for a model that reads raw rows (rx->linCmtBraw), every row; otherwise
// the other rows are written as zero.  Nothing downstream reads those rows:
// d(cp)/d(theta) (getJacCp) uses only the central row, the carry sentinels
// compute their transition matrix from theta, and any row the SEQUENTIAL
// kernel takes while the phase-2 state is live (steady state, a modeled-lag
// or extra dose, a theta change between rows) is preceded by
// linCmtHybFlush(), which rewrites the previous row's block with the full
// Jacobian from the term list so the carried state is complete again.

static int linCmtHybSubjN = 0, linCmtHybRowN = 0, linCmtHybDoseN = 0,
  linCmtHybRateN = 0, linCmtHybConsN = 0, linCmtHybFlushN = 0,
  linCmtHybFullN = 0;

//' Read (and optionally reset) the linCmt() hybrid-strategy counters
//'
//' @param reset logical; when TRUE zero the counters after reading
//' @return named integer vector: subjects (phase-2 primes), rows (phase-2
//'   rows filled), doses, rateSteps, consolidations, flushes (hand-backs to
//'   the sequential kernel), fullRows (rows swept for every Jacobian row)
//' @keywords internal
//' @export
//[[Rcpp::export]]
IntegerVector linCmtHybStats(bool reset = false) {
  IntegerVector r = IntegerVector::create(_["subjects"] = linCmtHybSubjN,
                                          _["rows"] = linCmtHybRowN,
                                          _["doses"] = linCmtHybDoseN,
                                          _["rateSteps"] = linCmtHybRateN,
                                          _["consolidations"] = linCmtHybConsN,
                                          _["flushes"] = linCmtHybFlushN,
                                          _["fullRows"] = linCmtHybFullN);
  if (reset) {
    linCmtHybSubjN = linCmtHybRowN = linCmtHybDoseN = linCmtHybRateN =
      linCmtHybConsN = linCmtHybFlushN = linCmtHybFullN = 0;
  }
  return r;
}

// Value and Jacobian of the superposition of terms e[0..n) at tEval, by one
// reverse-mode nest (every var lives inside it, so the tape is empty between
// calls).  An amount term enters as A0 + J0*(theta - theta0): exact in value
// and, because the kernel is linear in its initial state, in gradient.  With
// fullJ == 0 only the central row (oral0) is swept; the other rows of Jout
// are zero.
static void linCmtHybEval(stan::math::linCmtStan &lc, const double *thetaD,
                          int ncmt, int oral0, int trans,
                          const linHybEntry *e, int n, double tEval,
                          double *fxOut, double *Jout, int fullJ) {
  typedef stan::math::var var;
  int npars = lc.getNpars();
  int m = ncmt + oral0;
  double zeroRate[RX_LINHYB_MAXM] = {0.0, 0.0, 0.0, 0.0};
  linCmtRevTapeInit();
  stan::math::nested_rev_autodiff nested;
  Eigen::Matrix<var, Eigen::Dynamic, 1> theta(npars);
  for (int j = 0; j < npars; j++) theta(j, 0) = thetaD[j];
  Eigen::Matrix<var, Eigen::Dynamic, 2> g =
    stan::math::macros2micros(theta, ncmt, trans);
  var kaV = 0.0;
  if (oral0) kaV = theta(ncmt*2, 0);
  Eigen::Matrix<var, Eigen::Dynamic, 1> total =
    Eigen::Matrix<var, Eigen::Dynamic, 1>::Zero(m);
  Eigen::Matrix<var, Eigen::Dynamic, 1> yp0(m), ret(m);
  for (int i = 0; i < n; i++) {
    double dt = tEval - e[i].t;
    if (dt < 0.0) dt = 0.0;
    lc.setDt(dt);
    if (e[i].isRate) {
      lc.setRate(const_cast<double*>(e[i].rate));
      for (int c = 0; c < m; c++) yp0(c, 0) = 0.0;
    } else {
      lc.setRate(zeroRate);
      for (int c = 0; c < m; c++) {
        var v = e[i].amt[c];
        const double *Jc = e[i].J + c*RX_LINHYB_MAXP;
        for (int j = 0; j < npars; j++) {
          if (Jc[j] != 0.0) v += Jc[j] * (theta(j, 0) - thetaD[j]);
        }
        yp0(c, 0) = v;
      }
    }
    if (ncmt == 1) lc.linCmtStan1<var>(g, yp0, kaV, ret);
    else if (ncmt == 2) lc.linCmtStan2<var>(g, yp0, kaV, ret);
    else lc.linCmtStan3<var>(g, yp0, kaV, ret);
    for (int c = 0; c < m; c++) total(c, 0) += ret(c, 0);
  }
  for (int k = 0; k < m; k++) fxOut[k] = total(k, 0).val();
  for (int k = 0; k < m; k++) {
    double *Jk = Jout + k*RX_LINHYB_MAXP;
    if (!fullJ && k != oral0) {
      for (int j = 0; j < npars; j++) Jk[j] = 0.0;
      continue;
    }
    nested.set_zero_all_adjoints();
    total(k, 0).grad();
    for (int j = 0; j < npars; j++) Jk[j] = theta(j, 0).adj();
  }
}

// Per-subject pre-pass, run once per pass on the subject's first
// linCmtB() row (ind->ix is sorted by then): the trailing run of observation
// rows in solve order is the superposition phase.  Engaging is a performance
// choice only (the filler is exact wherever it runs), so a row the scan
// cannot see -- a pushed dose, a modeled time -- is simply handled as an
// ordinary row later.
static inline void linCmtHybPrepass(rx_solve *rx, rx_solving_options_ind *ind,
                                    int ncmt, int oral0) {
  ind->linCmtHybStart = -1;
  if (rx->linCmtSensStrategy == 1 || rx->ndiff == 0) return;
  int n = ind->n_all_times;
  int j = n;
  while (j > 0 && isObs(getEvid(ind, ind->ix[j-1]))) j--;
  int nObs = n - j;
  if (nObs < 1 || j < 1) return;
  if (rx->linCmtSensStrategy == 2) {
    ind->linCmtHybStart = j;
    return;
  }
  int m = ncmt + oral0;
  int nreq = linCmtSensNreq(rx->ndiff, ncmt, oral0);
  if (m >= 2 && nObs >= rx->linCmtHybridMinObs && nreq >= rx->linCmtHybridMinDirs) {
    ind->linCmtHybStart = j;
  }
}

// Does the hybrid filler take this row?  Only from the subject's pre-pass
// boundary on, where the sequential AD Jacobian would have been computed,
// only for a pure linCmt() solve (no ODE integration), and never for the
// steady-state kernel or a modeled-lag/extra dose (those rows go to the
// sequential kernel after linCmtHybFlush()).
static inline bool linCmtHybEngage(rx_solve *rx, rx_solving_options_ind *ind,
                                   int idx, int ncmt, int oral0) {
  if (rx->linCmtSensStrategy == 1) return false;
  if (ind->linCmtHybStart < 0 || idx < ind->linCmtHybStart || ind->linCmtHybOff) return false;
  if (rx->ndiff == 0 || ind->linCmtHparIndex >= -1) return false;
  if (!linCmtSensIsAD(rx->sensType)) return false;
  if (ind->doSS || ind->linSS != 0 || idx < 0) return false;
  if (rxEffNeq(ind, rx->op) - rx->op->numLin - rx->op->numLinSens != 0) return false;
  if (ind->extraDoseN[0] > ind->idxExtra || ind->pendingDosesN[0] > 0) return false;
  return true;
}

// The phase-2 state on this thread continues the row before idx.
static inline bool linCmtHybLive(linB_t &lcb, int id, int idx, double tprior) {
  return lcb.hybId == id && lcb.hybLastIdx == idx - 1 && lcb.hybLastT == tprior;
}

static inline void linCmtHybJtoM(const double *J, int m, int npars,
                                 Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> &Jm) {
  Jm.resize(m, npars);
  for (int k = 0; k < m; k++) {
    for (int j = 0; j < npars; j++) Jm(k, j) = J[k*RX_LINHYB_MAXP + j];
  }
}

// The previous row was filled by the hybrid with only the central Jacobian
// row; rewrite that row's saved block (a, the state the sequential kernel is
// about to carry forward) with the full Jacobian from the term list, then
// drop the phase-2 state.
static void linCmtHybFlush(linB_t &lcb, rx_solving_options_ind *ind, double *a,
                           const double *r, int ncmt, int oral0, int trans) {
  int m = ncmt + oral0;
  int npars = lcb.lc.getNpars();
  double fx[RX_LINHYB_MAXM], J[RX_LINHYB_MAXM*RX_LINHYB_MAXP];
  linCmtHybEval(lcb.lc, lcb.hybTheta, ncmt, oral0, trans, lcb.hybList.data(),
                (int)lcb.hybList.size(), lcb.hybLastT, fx, J, 1);
  Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> Jm;
  linCmtHybJtoM(J, m, npars, Jm);
  lcb.lc.setPtr(a, const_cast<double*>(r), a);
  lcb.lc.saveJac(Jm);
  lcb.lc.setPtr(a, const_cast<double*>(r), ind->linCmtSave);
  lcb.hybLastIdx = -1;
#pragma omp atomic
  linCmtHybFlushN++;
}

// Row-at-a-time hybrid filler: produces the row's fx/J and saved block from
// the per-thread term list instead of the carried Alast.  The list is primed
// from the buffer's own state (amounts + the sequential kernel's Jacobian at
// tprior) at the pre-pass boundary and whenever anything other than this
// filler last wrote the row before -- so the phase-1 end state is the first
// term, carrying its own sensitivity.  Later rows need no event-table
// parsing: an amount added since the last fill (a dose the pre-pass did not
// see, e.g. a pushed one) is a new term, a change in the live rate vector a
// rate-step term; a rate decrease or the active-term ceiling collapses the
// list into one primed term (exact, the term carries its Jacobian).
static void linCmtHybRow(linB_t &lcb, rx_solve *rx, rx_solving_options_ind *ind,
                         rx_solving_options *op, int id, int idx, double _t,
                         double *a, const double *r,
                         int ncmt, int oral0, int trans, const double *thetaD) {
  stan::math::linCmtStan &lc = lcb.lc;
  int m = ncmt + oral0;
  int npars = lc.getNpars();
  int nRate = op->numLin < m ? op->numLin : m;
  int ceiling = rx->linCmtHybridMaxActive;
  if (ceiling < 1) ceiling = 1;
  double tprior = ind->tprior;
  std::vector<linHybEntry> &L = lcb.hybList;
  // Generated code may call linCmtB(-1, -1) several times for one row before
  // par_solve() marks it solved; the list is already this row's, so only
  // re-evaluate it (re-priming from the buffer would read this row's own
  // pred-only block as the carried state).
  bool again = lcb.hybId == id && lcb.hybLastIdx == idx && lcb.hybLastT == _t;
  Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> Jb = lc.restoreJac(a);
  bool prime = !again && !linCmtHybLive(lcb, id, idx, tprior);
  if (!prime) {
    int k0 = lcb.hybFull ? 0 : oral0;
    int k1 = lcb.hybFull ? m : oral0 + 1;
    for (int k = k0; k < k1 && !prime; k++) {
      for (int j = 0; j < npars; j++) {
        if (Jb(k, j) != lcb.hybLastJ[k*RX_LINHYB_MAXP + j]) { prime = true; break; }
      }
    }
  }
  linHybEntry e;
  if (again) {
    // nothing to add
  } else if (prime) {
    L.clear();
    memset(&e, 0, sizeof(linHybEntry));
    e.t = tprior;
    for (int k = 0; k < m; k++) {
      e.amt[k] = a[k];
      for (int j = 0; j < npars; j++) e.J[k*RX_LINHYB_MAXP + j] = Jb(k, j);
    }
    L.push_back(e);
    for (int c = 0; c < RX_LINHYB_MAXM; c++) lcb.hybPrevRate[c] = 0.0;
    for (int j = 0; j < npars; j++) lcb.hybTheta[j] = thetaD[j];
    lcb.hybFull = rx->linCmtBraw ? 1 : 0;
#pragma omp atomic
    linCmtHybSubjN++;
  } else {
    memset(&e, 0, sizeof(linHybEntry));
    e.t = tprior;
    bool any = false;
    for (int k = 0; k < m; k++) {
      double d = a[k] - lcb.hybLastFx[k];
      if (d != 0.0) { e.amt[k] = d; any = true; }
    }
    if (any) {
      L.push_back(e);
#pragma omp atomic
      linCmtHybDoseN++;
    }
  }
  // The 2/3-cmt kernels only integrate a POSITIVE rate, so a rate step can
  // only be added; a decrease (an infusion ending) collapses the list into
  // one primed term at tprior and restarts from the absolute rate.
  bool rateUp = false, rateDown = false;
  for (int c = 0; c < nRate && !again; c++) {
    double d = r[c] - lcb.hybPrevRate[c];
    if (d > 0.0) rateUp = true;
    if (d < 0.0) rateDown = true;
  }
  bool consolidate = !again && (rateDown || ((int)L.size() + (rateUp ? 1 : 0) > ceiling));
  if (consolidate) {
    double fxc[RX_LINHYB_MAXM], Jc[RX_LINHYB_MAXM*RX_LINHYB_MAXP];
    linCmtHybEval(lc, thetaD, ncmt, oral0, trans, L.data(), (int)L.size(),
                  tprior, fxc, Jc, 1);
    memset(&e, 0, sizeof(linHybEntry));
    e.t = tprior;
    for (int k = 0; k < m; k++) {
      e.amt[k] = fxc[k];
      for (int j = 0; j < npars; j++) e.J[k*RX_LINHYB_MAXP + j] = Jc[k*RX_LINHYB_MAXP + j];
    }
    L.clear();
    L.push_back(e);
    for (int c = 0; c < RX_LINHYB_MAXM; c++) lcb.hybPrevRate[c] = 0.0;
#pragma omp atomic
    linCmtHybConsN++;
  }
  memset(&e, 0, sizeof(linHybEntry));
  e.t = tprior;
  e.isRate = 1;
  bool anyRate = false;
  for (int c = 0; c < nRate && !again; c++) {
    double d = r[c] - lcb.hybPrevRate[c];
    if (d != 0.0) { e.rate[c] = d; anyRate = true; }
    lcb.hybPrevRate[c] = r[c];
  }
  if (anyRate) {
    L.push_back(e);
#pragma omp atomic
    linCmtHybRateN++;
  }
  double fxo[RX_LINHYB_MAXM], Jo[RX_LINHYB_MAXM*RX_LINHYB_MAXP];
  linCmtHybEval(lc, thetaD, ncmt, oral0, trans, L.data(), (int)L.size(), _t,
                fxo, Jo, lcb.hybFull);
  for (int k = 0; k < m; k++) {
    lcb.fx(k, 0) = fxo[k];
    for (int j = 0; j < npars; j++) lcb.J(k, j) = Jo[k*RX_LINHYB_MAXP + j];
  }
  lc.saveJac(lcb.J);
  for (int k = 0; k < m; k++) ind->linCmtSave[k] = fxo[k];
  // remember exactly what the buffer will hand back next row
  Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> Js2 =
    lc.restoreJac(ind->linCmtSave);
  for (int k = 0; k < m; k++) {
    lcb.hybLastFx[k] = fxo[k];
    for (int j = 0; j < npars; j++) lcb.hybLastJ[k*RX_LINHYB_MAXP + j] = Js2(k, j);
  }
  lcb.hybId = id;
  lcb.hybLastIdx = idx;
  lcb.hybLastT = _t;
  if (again) return;
#pragma omp atomic
  linCmtHybRowN++;
  if (lcb.hybFull) {
#pragma omp atomic
    linCmtHybFullN++;
  }
}

// Alast snapshot returned by linCmtModelDouble().
static inline NumericVector linCmtModelDoubleAlast(const double *asave, int nAlast) {
  NumericVector Alast(nAlast);
  for (int i = 0; i < nAlast; i++) {
    Alast[i] = asave[i];
  }
  return Alast;
}

// Jacobian for linCmtModelDouble(); false for a sensType it does not
// support.  The AD methods (3 and 31 reverse, 30 forward fvar) need no
// step; 10/20 use sensH, 1/2 the kernel's own step choice.
static inline bool linCmtModelDoubleJac(stan::math::linCmtStan &lc, int sensType,
                                        double sensH,
                                        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1>> &thetaSens,
                                        Eigen::Matrix<double, Eigen::Dynamic, 1> &fx,
                                        Eigen::Matrix<double, -1, -1> &Js) {
  Eigen::Matrix<double, Eigen::Dynamic, 1> h =
    Eigen::Matrix<double, Eigen::Dynamic, 1>::Zero(thetaSens.size());
  switch (sensType) {
  case 1: // forward
    lc.fForwardJac(thetaSens, h.data(), fx, Js);
    return true;
  case 2:  // central
    lc.fCentralJac(thetaSens, h.data(), fx, Js);
    return true;
  case 31: // reverse-mode AD
    linCmtRevTapeInit();
    stan::math::jacobian(lc, thetaSens, fx, Js);
    return true;
  case 3:   // "AD": forward-mode fvar, the same as linCmtB()'s own dispatch
  case 30:  // explicit forward-mode AD; matches 31 to round-off
    lc.linCmtFwdJac(thetaSens, fx, Js);
    return true;
  case 10:
    h.setConstant(sensH);
    lc.fForwardJac(thetaSens, h.data(), fx, Js);
    return true;
  case 20:
    h.setConstant(sensH);
    lc.fCentralJac(thetaSens, h.data(), fx, Js);
    return true;
  default:
    return false;
  }
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

  // The AD methods (3/30 forward fvar, 31 reverse, 100 auto resolved by the
  // requested-direction count) use the unscaled (isAD = true) path so the
  // Jacobian comes out in true-theta units.
  sensType = linCmtSensResolveAuto(sensType, ndiff, ncmt, oral0);
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
    Eigen::Matrix<double, -1, -1> Js(ncmt+ oral0, numSens);
    Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> J =
      Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>::Constant(ncmt + oral0, 2*ncmt+ oral0, NA_REAL);
    lc.resizeModel();

    Eigen::Matrix<double, Eigen::Dynamic, 1> yp(ncmt+oral0, 1);
    Eigen::Matrix<double, Eigen::Dynamic, 2> g(ncmt, 2);
    lc.linAcalcAlast(yp, g, theta);

    if (!linCmtModelDoubleJac(lc, sensType, sensH, thetaSens, fx, Js)) {
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
    retList = List::create(_["val"] = wrap(val),
                           _["J"] = wrap(J),
                           _["Jg"] = wrap(Jg),
                           _["Alast"] = linCmtModelDoubleAlast(asave, nAlast));
  } else {
    Eigen::Matrix<double, Eigen::Dynamic, 1> fx;
    Eigen::Matrix<double, Eigen::Dynamic, 1> yp(oral0+ncmt, 1);
    Eigen::Matrix<double, Eigen::Dynamic, 2> g(ncmt, 2);
    lc.linAcalcAlast(yp, g, theta);
    fx = lc(theta);
    double val = lc.adjustF(fx, theta);
    retList = List::create(_["val"] = wrap(val),
                           _["Alast"] = linCmtModelDoubleAlast(asave, nAlast));
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
typedef stan::math::fvar<double> linCmtFv;

// Live rate for a carry sentinel: the cached per-idx slot on a re-query
// (the output pass, where ind->InfusionRate has moved on), otherwise
// ind->InfusionRate -- also the fallback when the slot was never cached,
// i.e. a standalone re-query after the subject finished solving.
static inline const double *linCmtBcarryRate(rx_solving_options_ind *ind,
                                             rx_solving_options *op, int idx) {
  const double *rate = (!ind->doSS && ind->solvedIdx >= idx) ?
    linCmtBRateSlot(ind, idx, op->numLin, 0) : getLinRate;
  if (rate == NULL) rate = getLinRate;
  return rate;
}

// Micro constants and ka as fvar seeds (derivative part 0) from the row's
// own macro parameters, for the state-transition passes below.
static inline void linCmtBcarryMicros(stan::math::linCmtStan &lc, int ncmt, int oral0,
                                      int trans,
                                      double p1, double v1, double p2, double p3,
                                      double p4, double p5, double ka,
                                      Eigen::Matrix<linCmtFv, Eigen::Dynamic, 2> &gF,
                                      linCmtFv &kaV) {
  int npars = lc.getNpars();
  Eigen::Matrix<double, Eigen::Dynamic, 1> thetaD(npars);
  linCmtFillTheta(thetaD, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka);
  Eigen::Matrix<linCmtFv, Eigen::Dynamic, 1> thetaF(npars);
  for (int k = 0; k < npars; k++) thetaF(k, 0) = linCmtFv(thetaD(k, 0), 0.0);
  gF = stan::math::macros2micros(thetaF, ncmt, trans);
  kaV = linCmtFv(0.0, 0.0);
  if (oral0) kaV = thetaF(ncmt*2, 0);
}

// One column of the local state-transition matrix d(Alast_i)/d(Alast_{i-1})
// (constant: the closed form is linear in Alast) -- a forward-mode pass
// seeded on previous-timepoint compartment col.
static inline void linCmtBtransitionColumn(stan::math::linCmtStan &lc,
                                           const Eigen::Matrix<linCmtFv, Eigen::Dynamic, 2> &gF,
                                           linCmtFv kaV, int ncmt, int m, int col,
                                           double *out) {
  Eigen::Matrix<linCmtFv, Eigen::Dynamic, 1> yp0 =
    Eigen::Matrix<linCmtFv, Eigen::Dynamic, 1>::Zero(m);
  yp0(col, 0) = linCmtFv(0.0, 1.0);
  Eigen::Matrix<linCmtFv, Eigen::Dynamic, 1> ret(m);
  if (ncmt == 1) lc.linCmtStan1<linCmtFv>(gF, yp0, kaV, ret);
  else if (ncmt == 2) lc.linCmtStan2<linCmtFv>(gF, yp0, kaV, ret);
  else lc.linCmtStan3<linCmtFv>(gF, yp0, kaV, ret);
  for (int r = 0; r < m; r++) out[r] = ret(r, 0).d_;
}

// which1 == -4: entry (row, col) of d(Alast_i)/d(Alast_{i-1}), which2 =
// row + m*col, m = ncmt+oral0.  Recomputes theta/g from the passed-in
// parameters so it does not depend on state cached by a prior -1,-1 call;
// opt-in only, so an ordinary solve pays nothing for it.  Validated against
// rxode2's own linToOde() ODE (useLinCmt=FALSE) across all 1/2/3-cmt
// IV/oral configs.
static inline double linCmtBtransition(linB_t &lcb, rx_solve *rx,
                                       rx_solving_options_ind *ind,
                                       rx_solving_options *op, int idx, double _t,
                                       int ncmt, int oral0, int which2, int trans,
                                       double p1, double v1, double p2, double p3,
                                       double p4, double p5, double ka) {
  int m = linCmtCarryM(ncmt, oral0);
  if (m == 0) return NA_REAL;
  int row = which2 % m;
  int col = which2 / m;
  if (which2 < 0 || col >= m) return NA_REAL;
  // may be the first touch of this slot (a model with no -1 call)
  if (!lcb.lc.isSame(ncmt, oral0, trans, rx->ndiff)) {
    linCmtBsetModel(lcb, ncmt, oral0, trans, ind->linSS, rx);
  }
  Eigen::Matrix<linCmtFv, Eigen::Dynamic, 2> gF;
  linCmtFv kaV;
  linCmtBcarryMicros(lcb.lc, ncmt, oral0, trans, p1, v1, p2, p3, p4, p5, ka, gF, kaV);
  // ind->tprior is stale in the post-solve lhs pass (frozen at the solve's
  // final interval, see linCmtBcarryAdvance), so the interval is the time
  // since the previous event row in solve order.
  double dt;
  if (ind->doSS) {
    dt = ind->tout - ind->tprior;
  } else {
    dt = idx > 0 ? _t - getTime(ind->ix[idx - 1], ind) : 0.0;
  }
  lcb.lc.setDt(dt);
  lcb.lc.setRate(const_cast<double*>(linCmtBcarryRate(ind, op, idx)));
  double colOut[4];
  linCmtBtransitionColumn(lcb.lc, gF, kaV, ncmt, m, col, colOut);
  return colOut[row];
}

// which1 == -7: add a caller-supplied local contribution (dPredDTheta_i *
// dThetaDEta_i, riding in the otherwise unused p2) to the stored cumulative
// carry at (row, pair), which2 = row + m*pair, on top of what -5 carried.
static inline double linCmtBcarryAdd(rx_solving_options_ind *ind, int ncmt, int oral0,
                                     int which2, double p2) {
  int m = linCmtCarryM(ncmt, oral0);
  if (m == 0) return NA_REAL;
  int row = which2 % m;
  int pair = which2 / m;
  if (which2 < 0 || pair >= RX_LINCMT_CARRY_MAXPAIRS) return NA_REAL;
  ind->linCmtCarryT[row*RX_LINCMT_CARRY_MAXPAIRS + pair] += p2;
  return ind->linCmtCarryT[row*RX_LINCMT_CARRY_MAXPAIRS + pair];
}

// Runtime per-subject fast path for the -5 advance (phase 3b.4): while this
// subject's theta has been bit-identical on every row of the CURRENT pass,
// the carried recurrence telescopes to G*J_n with J the production
// constant-theta Jacobian (exact there), so skipping the M advance --
// leaving the carry AND tracker columns un-multiplied -- makes the emitted
// -7 adds accumulate G*(J_i - J_{i-1}) = G*J_n.  The first row is always
// skippable (carry state is 0).  Mode lives in ind and resets at
// iniSubject(), so the comparison is within-pass only -- etas changing
// between inner iterations never cross it.  Tlast still advances so a
// later theta change resumes the slow path with the correct interval.
// PRECONDITION: exact only for the tracker-column calling convention
// nlmixr2est's carry codegen emits (each pair's -7 add is G*(J_i - P) with
// P the pair's tracker column, itself updated by a -7 add of J_i - P).  A
// caller feeding -7 an arbitrary local contribution must pin the slow path
// with linCmtCarrySetFast(FALSE) or set ind->linCmtCarryVarying = 2 (as
// linCmtCarryLiveTest does).  Returns true when the advance was skipped.
static inline bool linCmtBcarryFast(rx_solving_options_ind *ind, const double *thNow,
                                    double _t) {
  if (!linCmtCarryFastEnabled) return false;
  if (ind->linCmtCarryVarying == 0) {
    memcpy(ind->linCmtCarryPrevTheta, thNow, 7*sizeof(double));
    ind->linCmtCarryVarying = 1;
  } else if (ind->linCmtCarryVarying == 1 &&
             memcmp(ind->linCmtCarryPrevTheta, thNow, 7*sizeof(double)) != 0) {
    // Exact bit compare; any difference (including NaN anywhere) flips
    // permanently to the slow path for this pass.
    ind->linCmtCarryVarying = 2;
  }
  if (ind->linCmtCarryVarying == 2) return false;
  ind->linCmtCarryTlast = _t;
#pragma omp atomic
  linCmtCarryAdvFastN++;
  return true;
}

// s_new = M * s_old for every pair column (top m rows; the untouched rows
// stay 0 from iniSubject()'s reset).  localM is 4x4 row-major.
static inline void linCmtBcarryApplyM(rx_solving_options_ind *ind, const double *localM,
                                      int m) {
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
}

// which1 == -6 (read) / -5 (advance) of the cumulative carry
// ind->linCmtCarryT (4 rows x RX_LINCMT_CARRY_MAXPAIRS columns, row-major;
// each column is one carry-eligible (linCmt-parameter, eta) pair's own
// d(Alast)/d(eta) m-vector evolving as s_i = M_i*s_{i-1} + (-7 adds)).
// which2 = row + m*pair is the encoding the carry codegen emits; a pair
// index past the cap returns NA (no OOB write) -- the model-build layer
// enforces the cap.  -6 returns the stored value with no recomputation.
// -5 applies THIS interval's local transition matrix M_i (which depends
// only on the row's own theta, so one advance serves every pair) to every
// column at once; it must be called EXACTLY ONCE PER ROW per subject.  Each
// pair's own local contribution is composed by the caller via -7.
static inline double linCmtBcarryAdvance(linB_t &lcb, rx_solve *rx,
                                         rx_solving_options_ind *ind,
                                         rx_solving_options *op, int idx, double _t,
                                         int ncmt, int oral0, int which1, int which2,
                                         int trans,
                                         double p1, double v1, double p2, double p3,
                                         double p4, double p5, double ka) {
  int m = linCmtCarryM(ncmt, oral0);
  if (m == 0) return NA_REAL;
  int row = which2 % m;
  int pair = which2 / m;
  if (which2 < 0 || pair >= RX_LINCMT_CARRY_MAXPAIRS) return NA_REAL;
  int slot = row*RX_LINCMT_CARRY_MAXPAIRS + pair;
  if (which1 == -6) return ind->linCmtCarryT[slot];
#pragma omp atomic
  linCmtCarryAdvCallsN++;
  double thNow[7] = {p1, v1, p2, p3, p4, p5, ka};
  if (linCmtBcarryFast(ind, thNow, _t)) return ind->linCmtCarryT[slot];
  // may be the first touch of lc on this thread (a standalone re-query)
  if (!lcb.lc.isSame(ncmt, oral0, trans, rx->ndiff)) {
    linCmtBsetModel(lcb, ncmt, oral0, trans, ind->linSS, rx);
  }
  // The advance interval is the time since this sentinel's OWN previous
  // invocation: calc_lhs fires once per event row in solve order, but in
  // the post-solve lhs pass ind->tprior is frozen at the solve's final
  // interval.  Tlast is NAN after iniSubject(), so the first row advances
  // over dt = 0 (M = I on a zero carry state).
  double carryDt = ISNAN(ind->linCmtCarryTlast) ? 0.0 :
    (_t - ind->linCmtCarryTlast);
  ind->linCmtCarryTlast = _t;
  Eigen::Matrix<linCmtFv, Eigen::Dynamic, 2> gF;
  linCmtFv kaV;
  linCmtBcarryMicros(lcb.lc, ncmt, oral0, trans, p1, v1, p2, p3, p4, p5, ka, gF, kaV);
  lcb.lc.setDt(carryDt);
  lcb.lc.setRate(const_cast<double*>(linCmtBcarryRate(ind, op, idx)));
  double localM[16];
  for (int c = 0; c < m; c++) {
    double colOut[4];
    linCmtBtransitionColumn(lcb.lc, gF, kaV, ncmt, m, c, colOut);
    for (int r = 0; r < m; r++) localM[r*4 + c] = colOut[r];
  }
  linCmtBcarryApplyM(ind, localM, m);
  return ind->linCmtCarryT[slot];
}

// Reads of the row's stored Jacobian/amounts/concentration gradient.
static inline bool linCmtBread(linB_t &lcb, int which1, int which2, double *out) {
  if (which1 >= 0 && which2 >= 0) {
    *out = lcb.J(which1, which2);
    return true;
  }
  if (which1 >= 0 && which2 == -2) {
    *out = lcb.fx(which1);
    return true;
  }
  if (which1 == -2 && which2 >= 0) {
    *out = lcb.Jg(which2);
    return true;
  }
  return false;
}

// Sentinel reads and carry calls (which1/which2 not both -1).  These assume
// the -1,-1 solve for this row has already run.  Returns false for a
// combination no sentinel handles, in which case linCmtB() falls through
// to the ordinary solve path.
static inline bool linCmtBquery(linB_t &lcb, rx_solve *rx, rx_solving_options_ind *ind,
                                rx_solving_options *op, int idx, double _t,
                                int ncmt, int oral0, int which1, int which2, int trans,
                                double p1, double v1, double p2, double p3,
                                double p4, double p5, double ka, double *out) {
  if (linCmtBread(lcb, which1, which2, out)) return true;
  if (which1 == -3) {
    // idx already solved -> a re-query (e.g. the output pass) where
    // ind->InfusionRate has since been cleared/moved on; use the cached rate.
    const double *rate = (!ind->doSS && ind->solvedIdx >= idx) ?
      linCmtBRateSlot(ind, idx, op->numLin, 0) : getLinRate;
    *out = linCmtBdoseTime(lcb.lc, lcb.fx, ind, rate, ncmt, oral0, which2,
                           trans, p1, v1, p2, p3, p4, p5, ka);
  } else if (which1 == -4) {
    *out = linCmtBtransition(lcb, rx, ind, op, idx, _t, ncmt, oral0, which2, trans,
                             p1, v1, p2, p3, p4, p5, ka);
  } else if (which1 == -7) {
    *out = linCmtBcarryAdd(ind, ncmt, oral0, which2, p2);
  } else if (which1 == -5 || which1 == -6) {
    *out = linCmtBcarryAdvance(lcb, rx, ind, op, idx, _t, ncmt, oral0, which1, which2,
                               trans, p1, v1, p2, p3, p4, p5, ka);
  } else {
    return false;
  }
  return true;
}

static inline void linCmtBsetupSs(stan::math::linCmtStan &lc, rx_solving_options_ind *ind) {
  if (ind->linSS == linCmtSsInf) {
    lc.setSsInf(ind->linSSvar, ind->linSStau);
  } else if (ind->linSS == linCmtSsBolus) {
    lc.setSsBolus(ind->linSSvar, ind->linSStau, ind->linSSbolusCmt);
  }
}

// idx is genuinely being solved right now (not a re-query of an already
// solved index, e.g. the output pass) -- ind->InfusionRate is live, so cache
// it for linCmtBdoseTime() (which1 = -3) to read back on a later re-query.
static inline void linCmtBcacheRate(rx_solving_options_ind *ind, rx_solving_options *op,
                                    int idx, const double *r) {
  if (op->numLin > 0 && (ind->doSS || ind->solvedIdx < idx)) {
    double *rslot = linCmtBRateSlot(ind, idx, op->numLin, 1);
    if (rslot != NULL) std::copy(r, r + op->numLin, rslot);
  }
}

// Finite-difference family for a sensType: 1 forward (1/10/6, 6 with gill H
// estimate), 2 central (2/20), 3 three-point forward (4/40/7), 4 five-point
// endpoint (5/50); 0 for the AD methods.
static inline int linCmtBfdKind(int sensType) {
  static const int kind[51] = {
    0, 1, 2, 0, 3, 4, 1, 3, 0, 0,  // 0-9
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  // 10-19
    2, 0, 0, 0, 0, 0, 0, 0, 0, 0,  // 20-29
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  // 30-39
    3, 0, 0, 0, 0, 0, 0, 0, 0, 0,  // 40-49
    4                              // 50
  };
  return (sensType >= 0 && sensType <= 50) ? kind[sensType] : 0;
}

static inline void linCmtBfdJac(linB_t &lcb, int kind, double *linH,
                                Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1> > &theta,
                                Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1> > &thetaSens) {
  lcb.lc.linAcalcAlast(lcb.yp, lcb.g, theta);
  lcb.lc.calcFx(thetaSens);
  if (kind == 1) {
    lcb.lc.fForwardJac(thetaSens, linH, lcb.fx, lcb.Js);
  } else if (kind == 2) {
    lcb.lc.fCentralJac(thetaSens, linH, lcb.fx, lcb.Js);
  } else if (kind == 3) {
    lcb.lc.fF3Jac(thetaSens, linH, lcb.fx, lcb.Js);
  } else {
    lcb.lc.fEndpoint5Jac(thetaSens, linH, lcb.fx, lcb.Js);
  }
}

// The row's Jacobian by rx->sensType: a finite-difference family, reverse-
// mode AD (31), or forward-mode AD (3/30 and anything unspecified).
static inline void linCmtBjac(linB_t &lcb, rx_solve *rx, rx_solving_options_ind *ind,
                              Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1> > &theta,
                              Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1> > &thetaSens) {
  if (rx->sensType >= 0 && rx->sensType < RX_LINCMTB_SENS_SEEN) {
#pragma omp atomic write
    linCmtBSensSeen[rx->sensType] = 1;
  }
  int kind = linCmtBfdKind(rx->sensType);
  if (kind != 0) {
    linCmtBfdJac(lcb, kind, ind->linH, theta, thetaSens);
  } else if (rx->sensType == 31) {
    linCmtRevTapeInit();
    stan::math::jacobian(lcb.lc, thetaSens, lcb.fx, lcb.Js);
  } else {
    lcb.lc.linCmtFwdJac(thetaSens, lcb.fx, lcb.Js);
  }
  lcb.lc.updateJfromJs(lcb.J, lcb.Js);
  lcb.lc.saveJac(lcb.J);
}

// Fill the row's fx/J: restore an already-solved idx (or the lhs pass),
// otherwise advance the kernel from the previous state over dt and take
// the Jacobian.  ind->tprior is the prior solved time, ind->tout the time
// solved, _t the requested time (with ODE solving not necessarily tout).
static inline void linCmtBsolveRow(linB_t &lcb, rx_solve *rx, rx_solving_options_ind *ind,
                                   rx_solving_options *op, int id, int idx, double _t,
                                   double *a, const double *r, int ncmt, int oral0, int trans,
                                   Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1> > &theta,
                                   Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1> > &thetaSens) {
  if ((!ind->doSS && ind->solvedIdx >= idx) || ind->_rxFlag == 11) {
    double *acur = getAdvan(idx);
    lcb.J = lcb.lc.restoreJac(acur);
    lcb.fx = lcb.lc.restoreFx(acur);
    return;
  }
  lcb.lc.setDt(ind->doSS ? (ind->tout - ind->tprior) : (_t - ind->tprior));
  if (rx->ndiff != 0 && ind->linCmtHparIndex < -1) {
    const double *thD = getLinCmtDoubleAddr(lcb, linCmtBaddrTheta);
    if (ind->linCmtHybStart == -2) {
      // first computed row of this subject's pass: whatever phase-2 state
      // this thread still holds belongs to an earlier subject or solve (its
      // id/idx/tprior can coincide), so drop it before testing liveness
      lcb.hybId = -1;
      lcb.hybLastIdx = -1;
      linCmtHybPrepass(rx, ind, ncmt, oral0);
    }
    bool live = linCmtHybLive(lcb, id, idx, ind->tprior);
    if (linCmtHybEngage(rx, ind, idx, ncmt, oral0)) {
      if (live && memcmp(thD, lcb.hybTheta, lcb.lc.getNpars()*sizeof(double)) != 0) {
        // theta changed between rows: the superposition terms would mix
        // two thetas, so hand the rest of the pass to the sequential kernel
        if (!lcb.hybFull) linCmtHybFlush(lcb, ind, a, r, ncmt, oral0, trans);
        lcb.hybLastIdx = -1;
        ind->linCmtHybOff = 1;
      } else {
        linCmtHybRow(lcb, rx, ind, op, id, idx, _t, a, r, ncmt, oral0, trans, thD);
        return;
      }
    } else if (live && !lcb.hybFull) {
      linCmtHybFlush(lcb, ind, a, r, ncmt, oral0, trans);
    }
    linCmtBjac(lcb, rx, ind, theta, thetaSens);
    return;
  }
  if (rx->ndiff != 0 && ind->linCmtHparIndex >= 0) {
    thetaSens(ind->linCmtHparIndex, 0) += ind->linCmtH;
  }
  lcb.lc.linAcalcAlast(lcb.yp, lcb.g, theta);
  lcb.lc.calcFx(thetaSens);
  lcb.lc.fHCalcJac(thetaSens, ind->linH, lcb.fx, lcb.Js);
}

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
  rx_solving_options_ind *ind = &(rx->subjects[id]);
  rx_solving_options *op = rx->op;
  int idx = ind->idx;
  if (which1 != -1 || which2 != -1) {
    double out;
    if (linCmtBquery(lcb, rx, ind, op, idx, _t, ncmt, oral0, which1, which2, trans,
                     p1, v1, p2, p3, p4, p5, ka, &out)) {
      return out;
    }
  } else if (!lcb.lc.isSame(ncmt, oral0, trans, rx->ndiff)) {
    linCmtBsetModel(lcb, ncmt, oral0, trans, ind->linSS, rx);
  } else {
    lcb.lc.setSsType(ind->linSS);
  }
  if (id == 0 && ind->linH[0] == 0) {
    lcb.lc.resetFlags();
  }
  lcb.lc.setId(id);

  Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1> >
    theta(getLinCmtDoubleAddr(lcb, linCmtBaddrTheta), lcb.lc.getNpars());
  linCmtFillTheta(theta, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka);

  Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1> >
    thetaSens(getLinCmtDoubleAddr(lcb, linCmtBaddrThetaSens), lcb.numSens);

  // isAD (unscaled thetaSens + passthrough trueTheta) is used by every AD
  // jacobian path: forward-mode (3/30/auto), reverse-mode (31). The finite
  // difference methods keep the scaled path.
  lcb.lc.sensTheta(theta, thetaSens, linCmtSensIsAD(rx->sensType), rx->linCmtScale);
  linCmtBsetupSs(lcb.lc, ind);

  double *r = getLinRate;
  linCmtBcacheRate(ind, op, idx, r);
  double *a = (ind->linCmtAlast == NULL) ? getAdvan(ind->solvedIdx) : ind->linCmtAlast;
  lcb.lc.setPtr(a, r, ind->linCmtSave);

  linCmtBsolveRow(lcb, rx, ind, op, id, idx, _t, a, r, ncmt, oral0, trans, theta, thetaSens);
  lcb.lc.getJacCp(lcb.J, lcb.fx, theta, lcb.Jg);
  return lcb.lc.adjustF(lcb.fx, theta, ind->linCmtHV);
}
