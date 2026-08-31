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

// What does this individual's REGIMEN look like, as far as the dose-time
// sensitivity (linCmtB which1 = -3) is concerned?
//
// Steady-state infusion:
//
// The dose-time sensitivity (linCmtB which1 = -3) needs dA/dt, which includes
// the infusion rate feeding the LINEAR system (ind->InfusionRate +
// op->linOffset), so only a steady-state infusion into a linCmt() compartment
// matters -- one into a mixed model's ODE compartment never touches that
// slice and must not refuse the answer.  A regular (non-SS) infusion's rate is recovered at
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
//
// Dosed compartments:
//
// The -dA/dt identity assumes every dose feeding the linear system carries
// the same delay, so which linCmt() compartments this individual actually
// doses has to be compared against op->linCmtLagMask (the ones the model
// lags).  `*dosedMask` is a bitmask over the linCmt() block of the physical
// compartments actually dosed (bit c = block index c, 0 = depot when oral),
// matching op->linCmtLagMask: `cmt` out of getWh() is the 0-based index into
// the full state vector and the linCmt() block starts at op->linOffset, so
// block index c = cmt - op->linOffset.  Doses into a mixed model's ODE
// compartments fall outside the block and are ignored -- they do not feed the
// linear system, and neither does a plain zero bolus: it puts nothing into
// the compartment, so it cannot break the shared-delay assumption and must
// not be allowed to refuse an otherwise answerable regimen.  ONLY that shape
// is skipped -- a zero `amt` on a rate/duration record is an infinite
// infusion, and on a replace or multiply record it sets the compartment to
// zero; both genuinely dose.
//
// The scan covers the individual's WHOLE regimen: an EVID 3 reset between a
// lagged and an unlagged dose means the two never coexist in the system, but
// resets are not in ind->idose and this does not window on them, so such a
// regimen is refused rather than answered.  That is the conservative
// direction (a loud NA, not a wrong number).
//
// Both answers come out of ONE pass over ind->idose.
static inline void linCmtDoseScan(rx_solving_options_ind *ind,
                                  rx_solving_options *op,
                                  int *dosedMask, int *ssInf) {
  int mask = 0, ss = 0;
  int nLin = op->numLin < 31 ? op->numLin : 31;
  for (int i = 0; i < ind->ndoses; ++i) {
    int wh, cmt, wh100, whI, wh0;
    getWh(getEvid(ind, ind->idose[i]), &wh, &cmt, &wh100, &whI, &wh0);
    int c = cmt - op->linOffset;
    if (c < 0 || c >= nLin) continue;   // not part of the linear system
    if ((wh0 == EVID0_SS0 || wh0 == EVID0_SS || wh0 == EVID0_SS20 ||
         wh0 == EVID0_SS2 || wh0 == EVID0_SSINF) &&
        whI != EVIDF_NORMAL && whI != EVIDF_REPLACE && whI != EVIDF_MULT) {
      ss = 1;
    }
    if (!(whI == EVIDF_NORMAL && wh0 == EVID0_REGULAR &&
          getDoseNumber(ind, i) == 0.0)) {
      mask |= (1 << c);
    }
  }
  *dosedMask = mask;
  *ssInf = ss;
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

#define RX_LINWIN_MAXM 4
#define RX_LINWIN_MAXP 7
#define RX_LINWIN_DELTAS 4
#define RX_LINWIN_MISSRUN 8
// One slot outside the round-robin, used only while the give-up guard has
// disarmed the speculative memo: it holds the CURRENT row's gap so that
// row's own repeat executions are served, without the guard ever reading it
// as evidence that the design reuses an interval.
#define RX_LINWIN_SOLO RX_LINWIN_DELTAS
#define RX_LINWIN_SLOTS (RX_LINWIN_DELTAS + 1)

typedef stan::math::fvar<double> linCmtFv;

// The theta-only constants of the closed form (the elimination constant, or
// the eigen-decomposition L/C of the 2/3-cmt system, plus ka) and their
// derivatives in every parameter direction, computed once per theta-keyed
// window (theta is constant across it) so that each row needs only the
// dt-dependent tail of the kernel.
typedef struct {
  int valid;
  int ncmt, oral0, trans, npars;
  double theta[RX_LINWIN_MAXP];
  double k10, dk10[RX_LINWIN_MAXP];
  double ka, dka[RX_LINWIN_MAXP];
  double L[3], dL[RX_LINWIN_MAXP][3];
  double C[3][3][3], dC[RX_LINWIN_MAXP][3][3][3];
  // Delta-keyed memo of the tail's dt-dependent exponentials: for a row
  // gap delta, E_i = exp(-L_i*delta) (k10 plays L for 1-cmt) plus
  // exp(-ka*delta) when oral, with the tangent in every direction built
  // with the exact fvar operation order -- a hit is bitwise identical to
  // recomputation.  Sized from measured gap reuse (uniform designs: 1
  // distinct delta; interleaved q24h dosing: <= 4).  Round-robin
  // replacement; reset whenever the window refills.
  int deltaMemoOn;
  // Give-up guard: a design with no gap reuse pays the build with no hit
  // ever -- after RX_LINWIN_MISSRUN consecutive misses the window stops
  // building INTO THE ROUND-ROBIN (any hit resets the run; a window refill
  // re-arms).  While disarmed the cache still holds only the gaps of the
  // stretch that tripped it, so a later regular stretch's gap could never
  // enter it -- lastDelta re-arms on a row whose gap repeats the previous
  // row's, which is what a regular stretch produces on its second row and
  // what a genuinely irregular one never does.
  //
  // What the guard must NOT give up is the reuse that is there whatever
  // the schedule: one row is looked up several times under one theta, and
  // disarming used to make each of those executions recompute.  The solo
  // slot (RX_LINWIN_SOLO, outside the round-robin and never reported as
  // crossRow) holds the current row's gap while disarmed, so an irregular
  // design builds per ROW rather than per EXECUTION.
  //
  // All of that evidence is about the DESIGN, so it has to be gathered
  // per ROW rather than per call.  One row reaches this code several
  // times -- the generated model runs the value line from dydt and from
  // calc_lhs, and a fit's inner problem re-walks a subject many times --
  // and each of those executions looks the same gap up again.  Counting
  // a re-execution as a repeat made every design look regular inside a
  // fit: the guard never disarmed, and a matrix was built for gaps that
  // never actually recur.  lastIdx gates the bookkeeping to genuinely
  // new rows, and deltaIdx records which row put a gap into the cache so
  // a hit from a different row can be told apart from that same row
  // asking a second time.
  int missRun;
  double lastDelta;
  int lastIdx;
  int nDelta, deltaNext;
  double delta[RX_LINWIN_SLOTS];
  int deltaIdx[RX_LINWIN_SLOTS];
  double expL[RX_LINWIN_SLOTS][3];
  double dExpL[RX_LINWIN_SLOTS][RX_LINWIN_MAXP][3];
  double expKa[RX_LINWIN_SLOTS];
  double dExpKa[RX_LINWIN_SLOTS][RX_LINWIN_MAXP];
  // EXPLORATION ONLY (RX_LINCMT_PHI): the interval's state-transition
  // matrix Phi(delta) and its per-direction tangents, assembled by
  // probing the tail kernel with unit-basis prior states (so the entries
  // are exact by construction, no new closed-form algebra) and cached
  // alongside the exponentials.  A row then propagates with 2*m*m plain
  // double multiply-adds per direction instead of an fvar tail pass.
  // Phi is reused only within one subject: the window itself is per
  // THREAD and outlives a subject, so a matrix carried across subjects
  // would make "which rows propagate through Phi" depend on how subjects
  // happened to be handed to threads, and with it the last digits of the
  // result.  Keying the cache to the current subject keeps a solve
  // identical whatever the thread count, for one build per subject.
  int phiId, phiLastIdx;
  int phiBuilt[RX_LINWIN_SLOTS];
  // The closed-form assembly (linCmtPhiAnalyticRow) caches into the same
  // phi/dPhi storage but under its own built flag: a solve never mixes the
  // two routes, and keeping the flags apart means a later solve at the same
  // theta cannot pick up the other route's rounding.
  //
  // phiANd is the direction mask those cached matrices were built with.  A
  // built matrix only carries the columns the mask asked for, and the window
  // key does NOT include the mask -- a fit alternates models (inner, pred)
  // that share a shape and a theta but request different directions, so
  // without this a matrix built for the narrower mask would be reused for
  // the wider one and the extra directions would read whatever was there.
  // The probe-built route is not exposed to this because it discards its
  // matrices at the start of every subject of every solve.
  int phiABuilt[RX_LINWIN_SLOTS];
  int phiANd;
  double phi[RX_LINWIN_SLOTS][RX_LINWIN_MAXM][RX_LINWIN_MAXM];
  double dPhi[RX_LINWIN_SLOTS][RX_LINWIN_MAXP][RX_LINWIN_MAXM][RX_LINWIN_MAXM];
} linCmtWin;

// Global linear compartment B model object
// Refactored to per-thread vector for thread safety, matching linCmtA pattern.
// Tagged rather than anonymous: the members are C++ (Eigen matrices, a Stan
// object), so a typedef naming an anonymous struct for linkage is what clang
// warns about under -Wnon-c-typedef-for-linkage.
typedef struct linB_s {
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
  // Theta-keyed window of hoisted constants for the amortized sequential
  // row Jacobian (linCmtSeqTailJac); per-thread, refilled on a theta or
  // shape change.
  linCmtWin win{};
  // Last-row value memo: a repeated (-1,-1) call for the same row (the
  // generated model executes the value line from dydt, calc_lhs and the
  // output pass) returns the cached adjustF() result and leaves J/Jg/fx
  // standing for the reads.  Keyed on everything the value depends on;
  // invalidated by a model reshape and by any carry/dose-time sentinel.
  // Row last served by the thin value path (fx + Vc only): J/Jg are stale
  // for that row until a call-form query lazily restores them.
  int liteId = -1, liteIdx = -1;
  int memoId = -1, memoIdx = -1, memoFlag = -1, memoDoSS = -1, memoHpar = -9;
  double memoT = 0.0, memoH = 0.0, memoHV = 0.0, memoVal = 0.0;
  double memoArgs[7] = {0, 0, 0, 0, 0, 0, 0};
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
  lcb.memoIdx = -1; // reshape invalidates the last-row value memo
  lcb.liteId = lcb.liteIdx = -1;
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

// ---- amortized sequential row Jacobian ------------------------------------
//
// The theta-only constants of the closed form (k10, or the 2/3-cmt
// eigen-decomposition, and ka) and their derivatives are taken once per
// theta-keyed window by forward-mode passes (linCmtWinFill) and each
// ordinary row evaluates only the dt-dependent tail of the kernel
// (linCmtStanNTail) -- no tape, no allocation -- one forward pass per
// requested direction (linCmtSeqTailJac).  Steady-state rows and the FD
// families keep the full evaluator.

static int linCmtWinN = 0, linCmtSeqTailN = 0, linCmtSeqFullN = 0;
// Rows whose tail took the single multi-direction (dualN) pass.
static int linCmtSeqDualN = 0;
static int linCmtValCompN = 0, linCmtValRestN = 0, linCmtMemoHitN = 0;
static int linCmtValLiteN = 0;
static int linCmtExpBuildN = 0, linCmtExpHitN = 0, linCmtExpSoloN = 0;
static int linCmtPhiBuildN = 0;
static int linCmtPhiRowN = 0;
// Rows propagated through the closed-form (analytic) transition matrix.
static int linCmtPhiARowN = 0;
// -1: follow the per-window RX_LINCMT_DELTA_MEMO latch; 0/1: force.
static int linCmtDeltaMemoForce = -1;

//' Force the delta-keyed exponential memo on or off (tests/benchmarks)
//'
//' @param on integer: 1 forces the memo on, 0 forces it off, -1 (the
//'   default) follows the RX_LINCMT_DELTA_MEMO environment latch read at
//'   window-fill time
//' @return the previous setting, invisibly usable to restore it
//' @keywords internal
//' @export
//[[Rcpp::export]]
int linCmtDeltaMemo(int on = -1) {
  int prev = linCmtDeltaMemoForce;
  linCmtDeltaMemoForce = on;
  return prev;
}

//' Read (and optionally reset) the amortized linCmt() sequential counters
//'
//' @param reset logical; when TRUE zero the counters after reading
//' @return named integer vector: windows (window-constant recomputations),
//'   seqTailRows (rows evaluated from the window's dt-dependent tail),
//'   seqFullRows (rows that fell back to the full forward evaluator),
//'   valueCompute (value executions that solved the row),
//'   valueRestore (value executions that restored an already-solved row),
//'   memoHit (value executions short-circuited by the last-row memo),
//'   valueLite (already-solved value re-executions served by the thin
//'   fx-plus-scaling path with the Jacobian restore skipped),
//'   expBuild (delta-keyed exponential-memo builds: one per distinct row
//'   gap per theta window), expHit (rows whose exponentials came from the
//'   delta memo; disable with RX_LINCMT_DELTA_MEMO=off), expSolo (of
//'   those builds, the ones that went to the within-row slot the guard
//'   keeps serving after it stops speculating), dualRows (rows
//'   whose tail took one multi-direction pass, linCmtSensType="ADm"),
//'   phiAnalyticRows (rows propagated through the closed-form transition
//'   matrix; RX_LINCMT_PHI=2)
//' @keywords internal
//' @export
//[[Rcpp::export]]
IntegerVector linCmtSeqStats(bool reset = false) {
  IntegerVector r = IntegerVector::create(_["windows"] = linCmtWinN,
                                          _["seqTailRows"] = linCmtSeqTailN,
                                          _["seqFullRows"] = linCmtSeqFullN,
                                          _["valueCompute"] = linCmtValCompN,
                                          _["valueRestore"] = linCmtValRestN,
                                          _["memoHit"] = linCmtMemoHitN,
                                          _["valueLite"] = linCmtValLiteN,
                                          _["expBuild"] = linCmtExpBuildN,
                                          _["expHit"] = linCmtExpHitN,
                                          _["phiBuild"] = linCmtPhiBuildN,
                                          _["phiRows"] = linCmtPhiRowN,
                                          _["dualRows"] = linCmtSeqDualN,
                                          _["phiAnalyticRows"] = linCmtPhiARowN,
                                          _["expSolo"] = linCmtExpSoloN);
  if (reset) {
    linCmtWinN = linCmtSeqTailN = linCmtSeqFullN = 0;
    linCmtValCompN = linCmtValRestN = linCmtMemoHitN = 0;
    linCmtValLiteN = 0;
    linCmtExpBuildN = linCmtExpHitN = linCmtExpSoloN = 0;
    linCmtPhiBuildN = linCmtPhiRowN = 0;
    linCmtSeqDualN = linCmtPhiARowN = 0;
  }
  return r;
}

// Window constants and their derivative in each parameter direction, by
// npars forward-mode passes through macros2micros and the
// eigen-decomposition (once per window; the per-row cost is in the tail).
static void linCmtWinFill(stan::math::linCmtStan &lc, linCmtWin &w,
                             const double *thetaD, int ncmt, int oral0, int trans) {
  int npars = lc.getNpars();
  w.ncmt = ncmt; w.oral0 = oral0; w.trans = trans; w.npars = npars;
  for (int j = 0; j < npars; j++) w.theta[j] = thetaD[j];
  Eigen::Matrix<linCmtFv, Eigen::Dynamic, 1> thetaF(npars);
  for (int j = 0; j < npars; j++) {
    for (int i = 0; i < npars; i++) thetaF(i, 0) = linCmtFv(thetaD[i], i == j ? 1.0 : 0.0);
    Eigen::Matrix<linCmtFv, Eigen::Dynamic, 2> gF =
      stan::math::macros2micros(thetaF, ncmt, trans);
    linCmtFv kaV = oral0 ? thetaF(ncmt*2, 0) : linCmtFv(0.0, 0.0);
    w.dka[j] = kaV.d_;
    if (j == 0) w.ka = kaV.val_;
    if (ncmt == 1) {
      w.dk10[j] = gF(0, 1).d_;
      if (j == 0) w.k10 = gF(0, 1).val_;
    } else if (ncmt == 2) {
      stan::math::solComp2struct<linCmtFv> s =
        stan::math::computeSolComp2(gF(0, 1), gF(1, 0), gF(1, 1));
      for (int i = 0; i < 2; i++) {
        w.dL[j][i] = s.L(i, 0).d_;
        if (j == 0) w.L[i] = s.L(i, 0).val_;
        for (int r = 0; r < 2; r++) {
          w.dC[j][0][r][i] = s.C1(r, i).d_;
          w.dC[j][1][r][i] = s.C2(r, i).d_;
          if (j == 0) {
            w.C[0][r][i] = s.C1(r, i).val_;
            w.C[1][r][i] = s.C2(r, i).val_;
          }
        }
      }
    } else {
      stan::math::solComp3struct<linCmtFv> s =
        stan::math::computeSolComp3(gF(0, 1), gF(1, 0), gF(1, 1), gF(2, 0), gF(2, 1));
      for (int i = 0; i < 3; i++) {
        w.dL[j][i] = s.L(i, 0).d_;
        if (j == 0) w.L[i] = s.L(i, 0).val_;
        for (int r = 0; r < 3; r++) {
          w.dC[j][0][r][i] = s.C1(r, i).d_;
          w.dC[j][1][r][i] = s.C2(r, i).d_;
          w.dC[j][2][r][i] = s.C3(r, i).d_;
          if (j == 0) {
            w.C[0][r][i] = s.C1(r, i).val_;
            w.C[1][r][i] = s.C2(r, i).val_;
            w.C[2][r][i] = s.C3(r, i).val_;
          }
        }
      }
    }
  }
  w.valid = 1;
  // A new window invalidates the delta memo (its tangents embed dL/dka).
  w.nDelta = 0;
  w.deltaNext = 0;
  w.missRun = 0;
  w.lastDelta = NA_REAL;
  w.lastIdx = -1;
  w.delta[RX_LINWIN_SOLO] = NA_REAL;
  w.phiId = -1;
  w.phiLastIdx = -1;
  for (int s = 0; s < RX_LINWIN_SLOTS; s++) w.phiBuilt[s] = w.phiABuilt[s] = 0;
  w.phiANd = -1;
  {
    const char *e = getenv("RX_LINCMT_DELTA_MEMO");
    w.deltaMemoOn = !(e != NULL && e[0] == 'o' && e[1] == 'f' && e[2] == 'f');
  }
#pragma omp atomic
  linCmtWinN++;
}

// Find (or build) the delta memo slot for this row's gap.  The build uses
// the exact operation order of the fvar tail evaluation (u = (-L)*delta;
// E = exp(u); dE_j = ((-dL_j)*delta)*E), so a memo hit is bitwise
// identical to recomputing the exponentials inside the tail.
// crossRow reports a hit from a DIFFERENT row than the one that cached
// the gap -- the only evidence that the interval actually recurs in the
// design, and so the only thing allowed to re-arm the guard or engage a
// transition matrix.  A row asking again (several executions per row in a
// solve, many more across a fit's inner re-walks) still reuses the cached
// exponentials, which is free and bitwise identical; it just is not
// evidence.
static int linCmtWinDeltaSlot(linCmtWin &w, double delta, int idx, int *hit,
                              int *crossRow) {
  *hit = 0;
  *crossRow = 0;
  int newRow = (idx != w.lastIdx);
  for (int s = 0; s < w.nDelta; s++) {
    if (memcmp(&w.delta[s], &delta, sizeof(double)) == 0) {
      *hit = 1;
      if (newRow) {
        *crossRow = (w.deltaIdx[s] != idx);
        w.missRun = 0;
        w.lastDelta = delta; // keep the re-arm detector's "previous gap" exact
        w.lastIdx = idx;
      }
#pragma omp atomic
      linCmtExpHitN++;
      return s;
    }
  }
  // Miss.  A row whose gap repeats the previous row's is the signature of
  // a regular stretch; an irregular one never produces it, so re-arming on
  // it costs a genuinely irregular design nothing (it keeps building only
  // where reuse is actually returning).  A stretch that resumes with a
  // gap PAIR rather than a single repeated gap stays disarmed -- the
  // cached gaps are stale and no consecutive repeat appears; that residual
  // case only forgoes hits, it is never wrong.
  int deltaRepeat = newRow && (memcmp(&w.lastDelta, &delta, sizeof(double)) == 0);
  int solo = 0;
  if (newRow) {
    w.lastDelta = delta;
    w.lastIdx = idx;
    if (w.missRun >= RX_LINWIN_MISSRUN) {
      if (deltaRepeat) w.missRun = 0; // reuse is back: re-arm
      else solo = 1;                  // no cross-row reuse: stop speculating
    } else {
      w.missRun++;
    }
  } else if (w.missRun >= RX_LINWIN_MISSRUN) {
    solo = 1; // disarmed: a re-execution is not evidence to re-arm on
  }
  int s;
  if (solo) {
    // Disarmed, so nothing here may claim the interval recurs -- but a row
    // is looked up several times under one theta (once per linCmtB() call
    // the model generates, and again on a fit's inner re-walks), and those
    // repeats are reuse that is present whatever the schedule.  The solo
    // slot serves them, so an irregular design builds its exponentials --
    // and, under linCmtSensPhi = 2, assembles its transition matrix -- once
    // per ROW instead of once per EXECUTION.  It stays outside the
    // round-robin the scan above walks and never reports crossRow, so the
    // guard's reading of the design, and with it the probe-built route's
    // engage rule, are exactly as before.
    if (memcmp(&w.delta[RX_LINWIN_SOLO], &delta, sizeof(double)) == 0) {
      *hit = 1;
#pragma omp atomic
      linCmtExpHitN++;
      return RX_LINWIN_SOLO;
    }
    s = RX_LINWIN_SOLO;
#pragma omp atomic
    linCmtExpSoloN++;
  } else {
    s = w.deltaNext;
    w.deltaNext = (w.deltaNext + 1) % RX_LINWIN_DELTAS;
    if (w.nDelta < RX_LINWIN_DELTAS) w.nDelta++;
  }
  w.delta[s] = delta;
  w.deltaIdx[s] = idx;
  w.phiBuilt[s] = w.phiABuilt[s] = 0;
  int nL = (w.ncmt == 1) ? 1 : w.ncmt;
  for (int i = 0; i < nL; i++) {
    double Lv = (w.ncmt == 1) ? w.k10 : w.L[i];
    double E = exp((-Lv)*delta);
    w.expL[s][i] = E;
    for (int j = 0; j < w.npars; j++) {
      double dLv = (w.ncmt == 1) ? w.dk10[j] : w.dL[j][i];
      w.dExpL[s][j][i] = ((-dLv)*delta)*E;
    }
  }
  if (w.oral0) {
    double E = exp((-w.ka)*delta);
    w.expKa[s] = E;
    for (int j = 0; j < w.npars; j++) {
      w.dExpKa[s][j] = ((-w.dka[j])*delta)*E;
    }
  }
#pragma omp atomic
  linCmtExpBuildN++;
  return s;
}

// Assemble the interval's state-transition matrix Phi(delta) and its
// per-direction tangents by probing the tail kernel with unit-basis prior
// states and a zero rate.  Column c is exactly the tail's response to
// yp = e_c, so the entries need no new closed-form algebra and inherit the
// kernel's own arithmetic; a row of the same interval then propagates with
// plain double multiply-adds instead of an fvar tail pass per direction.
// Both forms evaluate the same exact closed-form solution -- only the
// order in which the products are accumulated differs (Phi is summed
// first, then applied), so the two can disagree in the last few digits
// with neither being the more correct.  Infusion rows are affine rather
// than linear in the prior state and keep the tail path.
// RX_LINCMT_PHI is a benchmarking force only (unset = follow the
// rxSolve(linCmtSensPhi=) control): -1 unset, 0 off, 1 probe-built matrix,
// 2 closed-form (analytic) matrix.
static int linCmtPhiForce() {
  static int mode = -2;
  if (mode == -2) {
    const char *e = getenv("RX_LINCMT_PHI");
    mode = (e == NULL) ? -1 : atoi(e);
  }
  return mode;
}

static void linCmtPhiBuild(stan::math::linCmtStan &lc, linCmtWin &w, int dSlot,
                           int ncmt, int oral0, int npars, int nd) {
  int m = ncmt + oral0;
  double zeroRate[RX_LINWIN_MAXM] = {0.0, 0.0, 0.0, 0.0};
  double *origRate = lc.rate_;
  lc.rate_ = zeroRate;
  bool haveVal = false;
  int nL = (ncmt == 1) ? 1 : ncmt;
  for (int j = 0; j < npars; j++) {
    int bit = (oral0 && j == 2*ncmt) ? diffKa : (diffP1 << j);
    if ((nd & bit) == 0) continue;
    linCmtFv kaV(w.ka, w.dka[j]);
    linCmtFv k10(w.k10, w.dk10[j]);
    stan::math::solComp2struct<linCmtFv> s2;
    stan::math::solComp3struct<linCmtFv> s3;
    if (ncmt == 2) {
      for (int i = 0; i < 2; i++) {
        s2.L(i, 0) = linCmtFv(w.L[i], w.dL[j][i]);
        for (int r = 0; r < 2; r++) {
          s2.C1(r, i) = linCmtFv(w.C[0][r][i], w.dC[j][0][r][i]);
          s2.C2(r, i) = linCmtFv(w.C[1][r][i], w.dC[j][1][r][i]);
        }
      }
    } else if (ncmt == 3) {
      for (int i = 0; i < 3; i++) {
        s3.L(i, 0) = linCmtFv(w.L[i], w.dL[j][i]);
        for (int r = 0; r < 3; r++) {
          s3.C1(r, i) = linCmtFv(w.C[0][r][i], w.dC[j][0][r][i]);
          s3.C2(r, i) = linCmtFv(w.C[1][r][i], w.dC[j][1][r][i]);
          s3.C3(r, i) = linCmtFv(w.C[2][r][i], w.dC[j][2][r][i]);
        }
      }
    }
    linCmtFv preEv[RX_LINWIN_MAXM];
    for (int i = 0; i < nL; i++) {
      preEv[i] = linCmtFv(w.expL[dSlot][i], w.dExpL[dSlot][j][i]);
    }
    preEv[nL] = oral0 ? linCmtFv(w.expKa[dSlot], w.dExpKa[dSlot][j]) :
      linCmtFv(0.0, 0.0);
    for (int c = 0; c < m; c++) {
      linCmtFv yp[RX_LINWIN_MAXM], ret[RX_LINWIN_MAXM];
      for (int r = 0; r < m; r++) yp[r] = linCmtFv(r == c ? 1.0 : 0.0, 0.0);
      for (int r = 0; r < RX_LINWIN_MAXM; r++) ret[r] = linCmtFv(0.0, 0.0);
      if (ncmt == 1) lc.linCmtStan1Tail<linCmtFv>(k10, yp, kaV, ret, preEv);
      else if (ncmt == 2) lc.linCmtStan2Tail<linCmtFv>(s2, yp, kaV, ret, preEv);
      else lc.linCmtStan3Tail<linCmtFv>(s3, yp, kaV, ret, preEv);
      for (int r = 0; r < m; r++) {
        if (!haveVal) w.phi[dSlot][r][c] = ret[r].val_;
        w.dPhi[dSlot][j][r][c] = ret[r].d_;
      }
    }
    haveVal = true;
  }
  lc.rate_ = origRate;
  w.phiBuilt[dSlot] = 1;
#pragma omp atomic
  linCmtPhiBuildN++;
}

// Closed-form row propagation: the analytic gradient of the tail.
//
// The tail is affine in the prior state -- read straight off
// linCmtStanNTail(): with n = ncmt disposition compartments,
//
//   ret = Phi(dt) yp + b(dt)
//   Phi[r][oral0+c] = sum_i C[c][r][i] E_i          (disposition source c)
//   Phi[r][0]       = ka * sum_i C[0][r][i] Ea_i    (depot source, oral)
//   Phi[0][0]       = expa,  Phi[0][c != 0] = 0     (oral)
//   b[r]            = -rDepot*(C1 Ea)_r + R*(C1 Rm)_r
//   b[0]            = rDepot*(1 - expa)/ka
//
// with Ea_i = (E_i - expa)/(ka - L_i) and Rm_i = (1 - E_i)/L_i.  Every
// tangent is then one line in quantities the theta-keyed window already
// holds (dL, dC, dka, dk10) plus the interval exponentials' own tangents:
//
//   dEa_i = ((dE_i - dExpa) - Ea_i*(dka - dL_i)) / (ka - L_i)
//   dRm_i = ((-dE_i)*L_i - (1 - E_i)*dL_i) / (L_i*L_i)
//
// linCmtPhiBuild() below gets the same matrices by PROBING the kernel with
// unit-basis prior states, which costs m kernel evaluations per direction --
// more than a whole row's tail -- so it may only be paid on an interval
// that demonstrably recurs, and it has to exclude rate-bearing rows.  The
// closed form costs about one kernel evaluation, so it can be assembled for
// an interval that never recurs; measured, the two are complementary and
// this path uses both facts:
//
//   * Phi is rate-free, so it is CACHED in the delta-memo slot exactly as
//     the probe-built one is.  Where intervals repeat, a row still costs
//     only the multiply-adds -- that reuse is worth more than any build.
//   * Where the interval does NOT repeat (or the memo has stopped
//     building), the matrix is assembled into locals for this row alone,
//     which the probe could never afford.  That is where the win is.
//   * b carries the depot and infusion terms.  It depends on the row's
//     rates rather than on theta, so it is built per row -- only on rows
//     that actually carry a rate -- and rate-bearing rows need not be
//     excluded.
//
// Whether the matrix came from the cache or was just built, it is the same
// deterministic function of the window constants and the interval, computed
// by the same code, so a row's result does not depend on the cache state.
// That is why this path needs none of the per-subject restart the probe
// path needs to stay independent of how subjects were handed to threads.
//
// This is the same closed form summed in a different order (Phi first, then
// applied), exactly as the probe-built matrix already shipped is: floating-
// point addition is not associative, so it can differ from the row-by-row
// tail in the last few digits, with neither more correct.  The kernels'
// branch structure is reproduced verbatim -- the one compartment degenerate
// ka == k10 limit, its sqrt(DBL_EPSILON) infusion test, and the R > 0.0
// test of the two and three compartment kernels -- because those are
// behavior, not tidiness.
// The interval's exponentials and their tangents, indexed by PARAMETER (the
// window's own layout).  A delta-memo hit supplies them; a miss builds them
// with the memo's own operation order, so a row's result does not depend on
// whether the memo happened to hold the gap.
//
// Filled LAZILY.  A cached matrix on a row with no rate needs none of this,
// and that is the common case on a regular design -- computing it eagerly
// would put three exponentials back on every row the cache exists to spare.
typedef struct {
  double E[3], dE[RX_LINWIN_MAXP][3], expa, dexpa[RX_LINWIN_MAXP];
  int have;
} linCmtRowExp;

static void linCmtRowExpFill(linCmtRowExp &e, const linCmtWin &w, int ncmt,
                             int oral0, int nL, int dSlot, const int *jIdx,
                             int nreq, double dt) {
  e.have = 1;
  const int cached = (dSlot >= 0);
  for (int i = 0; i < nL; i++) {
    if (cached) {
      e.E[i] = w.expL[dSlot][i];
      for (int s = 0; s < nreq; s++) e.dE[jIdx[s]][i] = w.dExpL[dSlot][jIdx[s]][i];
    } else {
      double Lv = (ncmt == 1) ? w.k10 : w.L[i];
      e.E[i] = exp((-Lv)*dt);
      for (int s = 0; s < nreq; s++) {
        int j_ = jIdx[s];
        double dLv = (ncmt == 1) ? w.dk10[j_] : w.dL[j_][i];
        e.dE[j_][i] = ((-dLv)*dt)*e.E[i];
      }
    }
  }
  if (oral0) {
    if (cached) {
      e.expa = w.expKa[dSlot];
      for (int s = 0; s < nreq; s++) e.dexpa[jIdx[s]] = w.dExpKa[dSlot][jIdx[s]];
    } else {
      e.expa = exp((-w.ka)*dt);
      for (int s = 0; s < nreq; s++) e.dexpa[jIdx[s]] = ((-w.dka[jIdx[s]])*dt)*e.expa;
    }
  }
}

#define RX_LINCMT_GET_E()                                               \
  if (!e.have)                                                          \
    linCmtRowExpFill(e, w, ncmt, oral0, nL, dSlot, jIdx, nreq, dt)

// One compartment: k10 is the only eigenvalue and C is 1, so the disposition
// block is a scalar.  Oral adds the depot column, which is where the kernel's
// degenerate ka == k10 limit lives -- reproduced, not tidied, because it is
// behavior.
static void linCmtPhiAssemble1(const linCmtWin &w, const linCmtRowExp &e,
                               int oral0, const int *jIdx, int nreq, double dt,
                               double (*phi)[RX_LINWIN_MAXM],
                               double (*dphi)[RX_LINWIN_MAXM][RX_LINWIN_MAXM]) {
  const double *E = e.E;
  const double (*dE)[3] = e.dE;
  const double expa = e.expa;
  const double *dexpa = e.dexpa;
  phi[oral0][oral0] = E[0];
  for (int s = 0; s < nreq; s++) dphi[jIdx[s]][oral0][oral0] = dE[jIdx[s]][0];
  if (oral0) {
    phi[0][0] = expa;
    for (int s = 0; s < nreq; s++) dphi[jIdx[s]][0][0] = dexpa[jIdx[s]];
    const double ka10 = w.ka - w.k10;
    if (fabs(ka10) <= sqrt(DBL_EPSILON)) {
      // the ka == k10 limit: ret[1] += (yp[0]*k10 - rate[0]) * dt * E
      phi[1][0] = w.k10*dt*E[0];
      for (int s = 0; s < nreq; s++) {
        const int j = jIdx[s];
        dphi[j][1][0] = (w.dk10[j]*dt)*E[0] + (w.k10*dt)*dE[j][0];
      }
    } else {
      const double T = (E[0] - expa)/ka10;
      phi[1][0] = w.ka*T;
      for (int s = 0; s < nreq; s++) {
        const int j = jIdx[s];
        const double dT = ((dE[j][0] - dexpa[j]) - T*(w.dka[j] - w.dk10[j]))/ka10;
        dphi[j][1][0] = w.dka[j]*T + w.ka*dT;
      }
    }
  }
}

// Two and three compartments: Phi's disposition block is the spectral sum
// sum_i C_c[r][i] E_i, and the depot column ka * sum_i C_1[r][i] Ea_i with
// Ea_i = (E_i - expa)/(ka - L_i).
static void linCmtPhiAssembleN(const linCmtWin &w, const linCmtRowExp &e,
                               int ncmt, int oral0, const int *jIdx, int nreq,
                               double dt, double (*phi)[RX_LINWIN_MAXM],
                               double (*dphi)[RX_LINWIN_MAXM][RX_LINWIN_MAXM]) {
  const int n = ncmt;
  const double *E = e.E;
  const double (*dE)[3] = e.dE;
  const double expa = e.expa;
  const double *dexpa = e.dexpa;
  for (int r = 0; r < n; r++) {
    for (int c = 0; c < n; c++) {
      double v = 0.0;
      for (int i = 0; i < n; i++) v += w.C[c][r][i]*E[i];
      phi[oral0 + r][oral0 + c] = v;
      for (int s = 0; s < nreq; s++) {
        const int j = jIdx[s];
        double dv = 0.0;
        for (int i = 0; i < n; i++) {
          dv += w.dC[j][c][r][i]*E[i] + w.C[c][r][i]*dE[j][i];
        }
        dphi[j][oral0 + r][oral0 + c] = dv;
      }
    }
  }
  if (oral0) {
    double Ea[3], dEa[RX_LINWIN_MAXP][3];
    for (int i = 0; i < n; i++) {
      const double den = w.ka - w.L[i];
      Ea[i] = (E[i] - expa)/den;
      for (int s = 0; s < nreq; s++) {
        const int j = jIdx[s];
        dEa[j][i] = ((dE[j][i] - dexpa[j]) - Ea[i]*(w.dka[j] - w.dL[j][i]))/den;
      }
    }
    for (int r = 0; r < n; r++) {
      double P = 0.0;
      for (int i = 0; i < n; i++) P += w.C[0][r][i]*Ea[i];
      phi[oral0 + r][0] = w.ka*P;
      for (int s = 0; s < nreq; s++) {
        const int j = jIdx[s];
        double dP = 0.0;
        for (int i = 0; i < n; i++) {
          dP += w.dC[j][0][r][i]*Ea[i] + w.C[0][r][i]*dEa[j][i];
        }
        dphi[j][oral0 + r][0] = w.dka[j]*P + w.ka*dP;
      }
    }
    phi[0][0] = expa;
    for (int s = 0; s < nreq; s++) dphi[jIdx[s]][0][0] = dexpa[jIdx[s]];
  }
}

// Phi(delta) and its tangent in each requested direction, from the window's
// eigenvalues and spectral matrices.  Column oral0+c is the response to a
// unit of disposition compartment c, column 0 (oral only) the response to a
// unit in the depot.
static void linCmtPhiAssemble(const linCmtWin &w, linCmtRowExp &e, int ncmt,
                              int oral0, int m, int nL, int dSlot,
                              const int *jIdx, int nreq, double dt,
                              double (*phi)[RX_LINWIN_MAXM],
                              double (*dphi)[RX_LINWIN_MAXM][RX_LINWIN_MAXM]) {
  RX_LINCMT_GET_E();
  for (int r = 0; r < m; r++) {
    for (int c = 0; c < m; c++) {
      phi[r][c] = 0.0;
      for (int s = 0; s < nreq; s++) dphi[jIdx[s]][r][c] = 0.0;
    }
  }
  if (ncmt == 1) linCmtPhiAssemble1(w, e, oral0, jIdx, nreq, dt, phi, dphi);
  else linCmtPhiAssembleN(w, e, ncmt, oral0, jIdx, nreq, dt, phi, dphi);
}

// One compartment: the infusion's approach to steady state is
// R*(1 - E)/k10, and the depot transfer carries the same degenerate
// ka == k10 limit the transition matrix does.
static void linCmtPhiAffine1(const linCmtWin &w, const linCmtRowExp &e,
                             int oral0, const int *jIdx, int nreq, double dt,
                             double rDepot, double R, double *bv,
                             double (*dbv)[RX_LINWIN_MAXM]) {
  const double *E = e.E;
  const double (*dE)[3] = e.dE;
  const double expa = e.expa;
  const double *dexpa = e.dexpa;
  if (rDepot != 0.0) {
    const double ka10 = w.ka - w.k10;
    if (fabs(ka10) <= sqrt(DBL_EPSILON)) {
      bv[1] += -rDepot*dt*E[0];
      for (int s = 0; s < nreq; s++) {
        const int j = jIdx[s];
        dbv[j][1] += -rDepot*dt*dE[j][0];
      }
    } else {
      const double T = (E[0] - expa)/ka10;
      bv[1] += -rDepot*T;
      for (int s = 0; s < nreq; s++) {
        const int j = jIdx[s];
        const double dT = ((dE[j][0] - dexpa[j]) - T*(w.dka[j] - w.dk10[j]))/ka10;
        dbv[j][1] += -rDepot*dT;
      }
    }
  }
  if (fabs(R) > sqrt(DBL_EPSILON)) {
    const double k2 = w.k10*w.k10;
    bv[oral0] += R*((1.0 - E[0])/w.k10);
    for (int s = 0; s < nreq; s++) {
      const int j = jIdx[s];
      dbv[j][oral0] += R*(((-dE[j][0])*w.k10 - (1.0 - E[0])*w.dk10[j])/k2);
    }
  }
}

// Two and three compartments: the same two terms summed over the spectral
// decomposition, with Rm_i = (1 - E_i)/L_i in place of the scalar form.
static void linCmtPhiAffineN(const linCmtWin &w, const linCmtRowExp &e,
                             int ncmt, int oral0, const int *jIdx, int nreq,
                             double dt, double rDepot, double R, double *bv,
                             double (*dbv)[RX_LINWIN_MAXM]) {
  const int n = ncmt;
  const double *E = e.E;
  const double (*dE)[3] = e.dE;
  const double expa = e.expa;
  const double *dexpa = e.dexpa;
  if (rDepot != 0.0) {
    double Ea[3], dEa[RX_LINWIN_MAXP][3];
    for (int i = 0; i < n; i++) {
      const double den = w.ka - w.L[i];
      Ea[i] = (E[i] - expa)/den;
      for (int s = 0; s < nreq; s++) {
        const int j = jIdx[s];
        dEa[j][i] = ((dE[j][i] - dexpa[j]) - Ea[i]*(w.dka[j] - w.dL[j][i]))/den;
      }
    }
    for (int r = 0; r < n; r++) {
      double P = 0.0;
      for (int i = 0; i < n; i++) P += w.C[0][r][i]*Ea[i];
      bv[oral0 + r] += -rDepot*P;
      for (int s = 0; s < nreq; s++) {
        const int j = jIdx[s];
        double dP = 0.0;
        for (int i = 0; i < n; i++) {
          dP += w.dC[j][0][r][i]*Ea[i] + w.C[0][r][i]*dEa[j][i];
        }
        dbv[j][oral0 + r] += -rDepot*dP;
      }
    }
  }
  if (R > 0.0) {
    double Rm[3], dRm[RX_LINWIN_MAXP][3];
    for (int i = 0; i < n; i++) {
      const double L2 = w.L[i]*w.L[i];
      Rm[i] = (1.0 - E[i])/w.L[i];
      for (int s = 0; s < nreq; s++) {
        const int j = jIdx[s];
        dRm[j][i] = ((-dE[j][i])*w.L[i] - (1.0 - E[i])*w.dL[j][i])/L2;
      }
    }
    for (int r = 0; r < n; r++) {
      double v = 0.0;
      for (int i = 0; i < n; i++) v += w.C[0][r][i]*Rm[i];
      bv[oral0 + r] += R*v;
      for (int s = 0; s < nreq; s++) {
        const int j = jIdx[s];
        double dv = 0.0;
        for (int i = 0; i < n; i++) {
          dv += w.dC[j][0][r][i]*Rm[i] + w.C[0][r][i]*dRm[j][i];
        }
        dbv[j][oral0 + r] += R*dv;
      }
    }
  }
}

static void linCmtPhiAffine(const linCmtWin &w, linCmtRowExp &e, int ncmt,
                            int oral0, int m, int nL, int dSlot,
                            const int *jIdx, int nreq, double dt,
                            const double *rate, double rDepot, double R,
                            double *bv, double (*dbv)[RX_LINWIN_MAXM]) {
  RX_LINCMT_GET_E();
  const double expa = e.expa;
  const double *dexpa = e.dexpa;
  for (int r = 0; r < m; r++) {
    bv[r] = 0.0;
    for (int s = 0; s < nreq; s++) dbv[jIdx[s]][r] = 0.0;
  }
  if (rDepot != 0.0) {
    const double ka2 = w.ka*w.ka;
    bv[0] += rDepot*(1.0 - expa)/w.ka;
    for (int s = 0; s < nreq; s++) {
      const int j = jIdx[s];
      dbv[j][0] += rDepot*((-dexpa[j])*w.ka - (1.0 - expa)*w.dka[j])/ka2;
    }
  }
  if (ncmt == 1) linCmtPhiAffine1(w, e, oral0, jIdx, nreq, dt, rDepot, R, bv, dbv);
  else linCmtPhiAffineN(w, e, ncmt, oral0, jIdx, nreq, dt, rDepot, R, bv, dbv);
}

#undef RX_LINCMT_GET_E

static bool linCmtPhiAnalyticRow(linB_t &lcb, linCmtWin &w, int ncmt, int oral0,
                                 int npars, int nd, int dSlot, int m,
                                 const Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> &J,
                                 const double *ypv, double dt, const double *rate) {
  const int nL = (ncmt == 1) ? 1 : ncmt;
  int jIdx[RX_LINWIN_MAXP];
  int nreq = 0;
  for (int j = 0; j < npars; j++) {
    int bit = (oral0 && j == 2*ncmt) ? diffKa : (diffP1 << j);
    if ((nd & bit) == 0) continue;
    jIdx[nreq++] = j;
  }
  if (nreq == 0) return false; // nothing requested: let the caller decide

  // A cached matrix carries only the columns its mask asked for; a wider
  // mask has to rebuild.
  if (w.phiANd != nd) {
    for (int i = 0; i < RX_LINWIN_SLOTS; i++) w.phiABuilt[i] = 0;
    w.phiANd = nd;
  }
  double phiLoc[RX_LINWIN_MAXM][RX_LINWIN_MAXM];
  double dphiLoc[RX_LINWIN_MAXP][RX_LINWIN_MAXM][RX_LINWIN_MAXM];
  const bool cached = (dSlot >= 0);
  double (*phi)[RX_LINWIN_MAXM] = cached ? w.phi[dSlot] : phiLoc;
  double (*dphi)[RX_LINWIN_MAXM][RX_LINWIN_MAXM] = cached ? w.dPhi[dSlot] : dphiLoc;
  linCmtRowExp e;
  e.have = 0;
  e.expa = 0.0;

  if (!cached || !w.phiABuilt[dSlot]) {
    linCmtPhiAssemble(w, e, ncmt, oral0, m, nL, dSlot, jIdx, nreq, dt,
                      phi, dphi);
    if (cached) w.phiABuilt[dSlot] = 1;
  }

  double bv[RX_LINWIN_MAXM], dbv[RX_LINWIN_MAXP][RX_LINWIN_MAXM];
  const double rDepot = oral0 ? rate[0] : 0.0;
  const double R = rate[oral0] + rDepot;
  const bool affine = (rDepot != 0.0) ||
    (ncmt == 1 ? (fabs(R) > sqrt(DBL_EPSILON)) : (R > 0.0));
  if (affine)
    linCmtPhiAffine(w, e, ncmt, oral0, m, nL, dSlot, jIdx, nreq, dt, rate,
                    rDepot, R, bv, dbv);

  for (int r = 0; r < m; r++) {
    double v = affine ? bv[r] : 0.0;
    for (int c = 0; c < m; c++) v += phi[r][c]*ypv[c];
    lcb.fx(r, 0) = v;
    lcb.lc.Asave_[r] = v;
  }
  for (int s = 0; s < nreq; s++) {
    const int j = jIdx[s];
    for (int r = 0; r < m; r++) {
      double d = affine ? dbv[j][r] : 0.0;
      for (int c = 0; c < m; c++) d += phi[r][c]*J(c, j) + dphi[j][r][c]*ypv[c];
      lcb.Js(r, s) = d;
    }
  }
  return true;
}

// Multi-directional (dualN) row tail: the same window+tail evaluation the
// per-direction fvar loop below performs, but with every requested direction
// carried as a separate tangent through ONE pass.  The closed form -- the
// exponentials on a delta-memo miss, the divisions of the depot transfer,
// the whole kernel arithmetic -- is therefore evaluated once per row instead
// of once per direction.  dualN mirrors each stan/math/fwd rule's operation
// order, and the kernel is the identical template, so slot si here computes
// exactly what the fvar pass for that direction computes: results are
// bitwise identical, not merely equal to round-off.
// The window's eigen-decomposition, seeded into the dual spectral structures:
// the VALUES once for the row, then one TANGENT per requested direction into
// that direction's slot.  Only the eigen-decomposition is laid out per
// compartment count, so keeping the ncmt branch here leaves the caller
// reading as the sequence it is -- seed, evaluate, unpack.
template <int N>
static void linCmtDualSpectralValue(const linCmtWin &w, int ncmt,
                                    stan::math::solComp2struct<stan::math::dualN<N> > &s2,
                                    stan::math::solComp3struct<stan::math::dualN<N> > &s3) {
  typedef stan::math::dualN<N> dv;
  if (ncmt == 2) {
    for (int i = 0; i < 2; i++) {
      s2.L(i, 0) = dv(w.L[i]);
      for (int r = 0; r < 2; r++) {
        s2.C1(r, i) = dv(w.C[0][r][i]);
        s2.C2(r, i) = dv(w.C[1][r][i]);
      }
    }
  } else if (ncmt == 3) {
    for (int i = 0; i < 3; i++) {
      s3.L(i, 0) = dv(w.L[i]);
      for (int r = 0; r < 3; r++) {
        s3.C1(r, i) = dv(w.C[0][r][i]);
        s3.C2(r, i) = dv(w.C[1][r][i]);
        s3.C3(r, i) = dv(w.C[2][r][i]);
      }
    }
  }
}

template <int N>
static void linCmtDualSpectralTangent(const linCmtWin &w, int ncmt, int j, int si,
                                      stan::math::solComp2struct<stan::math::dualN<N> > &s2,
                                      stan::math::solComp3struct<stan::math::dualN<N> > &s3) {
  if (ncmt == 2) {
    for (int i = 0; i < 2; i++) {
      s2.L(i, 0).d_[si] = w.dL[j][i];
      for (int r = 0; r < 2; r++) {
        s2.C1(r, i).d_[si] = w.dC[j][0][r][i];
        s2.C2(r, i).d_[si] = w.dC[j][1][r][i];
      }
    }
  } else if (ncmt == 3) {
    for (int i = 0; i < 3; i++) {
      s3.L(i, 0).d_[si] = w.dL[j][i];
      for (int r = 0; r < 3; r++) {
        s3.C1(r, i).d_[si] = w.dC[j][0][r][i];
        s3.C2(r, i).d_[si] = w.dC[j][1][r][i];
        s3.C3(r, i).d_[si] = w.dC[j][2][r][i];
      }
    }
  }
}

template <int N>
static bool linCmtSeqTailDualN(linB_t &lcb, linCmtWin &w, int ncmt, int oral0,
                               int npars, int nd, int dSlot, int m,
                               const Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> &J,
                               const double *ypv) {
  typedef stan::math::dualN<N> dv;
  stan::math::linCmtStan &lc = lcb.lc;
  int nL = (ncmt == 1) ? 1 : ncmt;
  dv kaV(w.ka), k10(w.k10);
  stan::math::solComp2struct<dv> s2;
  stan::math::solComp3struct<dv> s3;
  // Window constants: values once, tangents per requested slot below.
  linCmtDualSpectralValue<N>(w, ncmt, s2, s3);
  dv preEv[RX_LINWIN_MAXM];
  const dv *preE = NULL;
  if (dSlot >= 0) {
    for (int i = 0; i < nL; i++) preEv[i] = dv(w.expL[dSlot][i]);
    preEv[nL] = oral0 ? dv(w.expKa[dSlot]) : dv(0.0);
    preE = preEv;
  }
  dv yp[RX_LINWIN_MAXM], ret[RX_LINWIN_MAXM];
  for (int c = 0; c < m; c++) yp[c] = dv(ypv[c]);
  for (int c = 0; c < RX_LINWIN_MAXM; c++) ret[c] = dv(0.0);
  // Tangent slots, in the canonical requested order updateJfromJs expects.
  int si = 0;
  for (int j = 0; j < npars; j++) {
    int bit = (oral0 && j == 2*ncmt) ? diffKa : (diffP1 << j);
    if ((nd & bit) == 0) continue;
    if (si >= N) return false;
    kaV.d_[si] = w.dka[j];
    k10.d_[si] = w.dk10[j];
    linCmtDualSpectralTangent<N>(w, ncmt, j, si, s2, s3);
    if (dSlot >= 0) {
      for (int i = 0; i < nL; i++) preEv[i].d_[si] = w.dExpL[dSlot][j][i];
      if (oral0) preEv[nL].d_[si] = w.dExpKa[dSlot][j];
    }
    for (int c = 0; c < m; c++) yp[c].d_[si] = J(c, j);
    si++;
  }
  if (si != N) return false;
  if (ncmt == 1) lc.linCmtStan1Tail<dv>(k10, yp, kaV, ret, preE);
  else if (ncmt == 2) lc.linCmtStan2Tail<dv>(s2, yp, kaV, ret, preE);
  else lc.linCmtStan3Tail<dv>(s3, yp, kaV, ret, preE);
  for (int c = 0; c < m; c++) {
    lcb.fx(c, 0) = ret[c].v_;
    lc.Asave_[c] = ret[c].v_;
    for (int i = 0; i < N; i++) lcb.Js(c, i) = ret[c].d_[i];
  }
  return true;
}

// EXPLORATION ONLY (RX_LINCMT_ABLATE, default 0 = off): ablation levels
// used to measure what share of a sensitivity row is the per-direction
// fvar work, i.e. the ceiling of a transition-matrix propagation that
// would replace it with plain-double multiply-adds.  1 skips the Tail
// kernel, 2 skips the whole per-direction body (constant fill, yp/preE
// construction and the kernel).  Both produce WRONG (but finite and
// bounded) derivatives -- timing counterfactuals, never a solve mode.
static int linCmtAblateMode() {
  static int mode = -1;
  if (mode < 0) {
    const char *e = getenv("RX_LINCMT_ABLATE");
    mode = (e == NULL) ? 0 : atoi(e);
  }
  return mode;
}

// Sequential window+tail row Jacobian (the amortization the hybrid's
// observation phase already uses, applied to EVERY ordinary row): the
// theta-only constants (k10 or the eigen-decomposition L/C, and ka) and
// their tangents come from the theta-keyed window, so each requested
// direction evaluates only the dt-dependent tail -- no macros2micros and
// no eigen-decomposition per row.  Mathematically identical to
// linCmtFwdJac under the AD path (isAD passthrough); steady-state rows
// never come here (the SS kernels are not factored into constants+tail),
// and any other shape falls back to the full evaluator.  Outputs match
// linCmtFwdJac exactly: fx, the masked Js (columns in canonical requested
// order, as updateJfromJs expects) and the Asave_ amounts for the next
// row's carry.
static bool linCmtSeqTailJac(linB_t &lcb, int phiCtl, int subjId, int idx,
                             int dual) {
  stan::math::linCmtStan &lc = lcb.lc;
  if (lc.type_ != linCmtNormal) return false;
  int ncmt = lc.ncmt_, oral0 = lc.oral0_, trans = lc.trans_;
  int npars = lc.getNpars();
  if (npars > RX_LINWIN_MAXP || ncmt + oral0 > RX_LINWIN_MAXM) return false;
  int m = ncmt + oral0;
  const double *thetaD = getLinCmtDoubleAddr(lcb, linCmtBaddrTheta);
  linCmtWin &w = lcb.win;
  if (!w.valid || w.ncmt != ncmt || w.oral0 != oral0 || w.trans != trans ||
      w.npars != npars || memcmp(w.theta, thetaD, npars*sizeof(double)) != 0) {
    linCmtWinFill(lc, w, thetaD, ncmt, oral0, trans);
  }
  // One delta-memo lookup per row; every requested direction reuses the
  // slot (the exponentials are shared, only the tangent differs by j).
  int memoOn = (linCmtDeltaMemoForce >= 0) ? linCmtDeltaMemoForce : w.deltaMemoOn;
  int dHit = 0, dCross = 0;
  int dSlot = memoOn ? linCmtWinDeltaSlot(w, lc.dt_, idx, &dHit, &dCross) : -1;
  // double Alast reconstruction once per row (shared by every direction);
  // J's columns align with linCmtFillTheta's order (ka last when oral).
  lc.restoreJacTo(lc.A_, lc.fwdJ_);
  const Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> &J = lc.fwdJ_;
  double AlastA[RX_LINWIN_MAXM], ypv[RX_LINWIN_MAXM];
  for (int c = 0; c < m; c++) {
    double s = lc.A_[c];
    for (int k = 0; k < npars; k++) s -= J(c, k)*thetaD[k];
    AlastA[c] = s;
    double v = AlastA[c];
    for (int k = 0; k < npars; k++) v += J(c, k)*thetaD[k];
    ypv[c] = v;
  }
  int nd = lc.numDiff_;
  if (nd == 0) nd = 127;
  int si = 0;
  bool first = true;
  const int ablate = linCmtAblateMode();
  lcb.fx.resize(m);
  // Engage rule: a transition matrix only pays for itself when it is
  // REUSED, and the evidence for that is a delta-memo hit from a
  // DIFFERENT row (dCross) -- that, and only that, says this interval
  // recurs in the design under this theta.  A hit from the same row
  // asking again is not evidence: one row reaches here several times in a
  // solve and far more across a fit's inner re-walks, and treating those
  // as reuse engaged a matrix on gaps that never recur.  So Phi is
  // assembled on the first cross-row hit for a slot and reused
  // thereafter, and never on a miss -- a design whose intervals never
  // repeat builds no Phi at all, in a fit as in a single solve, so it
  // cannot be slowed down by this path.  Infusion rows are affine rather
  // than linear in the prior state and keep the tail below.
  int phiForce = linCmtPhiForce();
  int phiOn = (phiForce >= 0) ? phiForce : phiCtl;
  // ... and a row index that has not advanced means a new solve reached
  // this window, even when the subject identifier happens to repeat.
  // Closed-form assembly: cheap enough for every ordinary row, so it needs
  // neither a delta-memo hit nor evidence that the interval recurs, and it
  // carries the depot and infusion terms rather than excluding them.
  if (phiOn == 2) {
    if (linCmtPhiAnalyticRow(lcb, w, ncmt, oral0, npars, nd, dSlot, m, J, ypv,
                             lc.dt_, lc.rate_)) {
#pragma omp atomic
      linCmtPhiARowN++;
      return true;
    }
    return false; // nothing requested
  }
  if (phiOn == 1 && (w.phiId != subjId || idx < w.phiLastIdx)) {
    // Start every subject from the same blank interval state.  The window
    // is per THREAD and outlives a subject, so without this the answer to
    // "has this interval been seen before" -- and with it which rows
    // propagate through a matrix and which evaluate the tail -- would
    // depend on how subjects happened to be handed to threads, and the
    // last digits of a solve would move with the thread count.  Restarting
    // per subject costs a few exponentials and one matrix per subject and
    // keeps a solve identical however many threads run it.
    w.phiId = subjId;
    w.nDelta = 0;
    w.deltaNext = 0;
    w.missRun = 0;
    w.lastDelta = NA_REAL;
    w.lastIdx = -1;
    w.delta[RX_LINWIN_SOLO] = NA_REAL;
    for (int i = 0; i < RX_LINWIN_SLOTS; i++) w.phiBuilt[i] = w.phiABuilt[i] = 0;
    dHit = dCross = 0;
    dSlot = memoOn ? linCmtWinDeltaSlot(w, lc.dt_, idx, &dHit, &dCross) : -1;
  }
  if (phiOn == 1) w.phiLastIdx = idx;
  if (phiOn == 1 && dSlot >= 0 && (dCross || w.phiBuilt[dSlot])) {
    bool rateFree = true;
    for (int c = 0; c < m; c++) {
      if (lc.rate_[c] != 0.0) { rateFree = false; break; }
    }
    if (rateFree) {
      if (!w.phiBuilt[dSlot]) {
        linCmtPhiBuild(lc, w, dSlot, ncmt, oral0, npars, nd);
      }
      for (int j = 0; j < npars; j++) {
        int bit = (oral0 && j == 2*ncmt) ? diffKa : (diffP1 << j);
        if ((nd & bit) == 0) continue;
        for (int r = 0; r < m; r++) {
          double d = 0.0;
          for (int c = 0; c < m; c++) {
            d += w.phi[dSlot][r][c]*J(c, j) + w.dPhi[dSlot][j][r][c]*ypv[c];
          }
          if (first) {
            double v = 0.0;
            for (int c = 0; c < m; c++) v += w.phi[dSlot][r][c]*ypv[c];
            lcb.fx(r, 0) = v;
            lc.Asave_[r] = v;
          }
          lcb.Js(r, si) = d;
        }
        first = false;
        si++;
      }
      if (first) return false;
#pragma omp atomic
      linCmtPhiRowN++;
      return true;
    }
  }
  // One pass carrying every requested direction, instead of the loop below.
  // Skipped under the ablation knob, which is defined only for the
  // per-direction path it is measuring.
  if (dual && ablate == 0) {
    int nreq = 0;
    for (int j = 0; j < npars; j++) {
      int bit = (oral0 && j == 2*ncmt) ? diffKa : (diffP1 << j);
      if ((nd & bit) != 0) nreq++;
    }
    bool done = false;
    switch (nreq) {
    case 1: done = linCmtSeqTailDualN<1>(lcb, w, ncmt, oral0, npars, nd, dSlot, m, J, ypv); break;
    case 2: done = linCmtSeqTailDualN<2>(lcb, w, ncmt, oral0, npars, nd, dSlot, m, J, ypv); break;
    case 3: done = linCmtSeqTailDualN<3>(lcb, w, ncmt, oral0, npars, nd, dSlot, m, J, ypv); break;
    case 4: done = linCmtSeqTailDualN<4>(lcb, w, ncmt, oral0, npars, nd, dSlot, m, J, ypv); break;
    case 5: done = linCmtSeqTailDualN<5>(lcb, w, ncmt, oral0, npars, nd, dSlot, m, J, ypv); break;
    case 6: done = linCmtSeqTailDualN<6>(lcb, w, ncmt, oral0, npars, nd, dSlot, m, J, ypv); break;
    case 7: done = linCmtSeqTailDualN<7>(lcb, w, ncmt, oral0, npars, nd, dSlot, m, J, ypv); break;
    default: break;
    }
    if (done) {
#pragma omp atomic
      linCmtSeqDualN++;
      return true;
    }
  }
  for (int j = 0; j < npars; j++) {
    int bit = (oral0 && j == 2*ncmt) ? diffKa : (diffP1 << j);
    if ((nd & bit) == 0) continue;
    if (ablate == 2) {
      // Counterfactual cost of a transition-matrix step: 2*m*m double
      // multiply-adds per direction, no fvar, no transcendentals.
      double e0 = (dSlot >= 0) ? w.expL[dSlot][0] : 0.5;
      for (int c = 0; c < m; c++) {
        double v = 0.0, d = 0.0;
        for (int r = 0; r < m; r++) {
          v += e0*ypv[r];
          d += e0*J(r, j);
        }
        if (first) { lcb.fx(c, 0) = v; lc.Asave_[c] = v; }
        lcb.Js(c, si) = d;
      }
      first = false;
      si++;
      continue;
    }
    linCmtFv kaV(w.ka, w.dka[j]);
    linCmtFv k10(w.k10, w.dk10[j]);
    stan::math::solComp2struct<linCmtFv> s2;
    stan::math::solComp3struct<linCmtFv> s3;
    if (ncmt == 2) {
      for (int i = 0; i < 2; i++) {
        s2.L(i, 0) = linCmtFv(w.L[i], w.dL[j][i]);
        for (int r = 0; r < 2; r++) {
          s2.C1(r, i) = linCmtFv(w.C[0][r][i], w.dC[j][0][r][i]);
          s2.C2(r, i) = linCmtFv(w.C[1][r][i], w.dC[j][1][r][i]);
        }
      }
    } else if (ncmt == 3) {
      for (int i = 0; i < 3; i++) {
        s3.L(i, 0) = linCmtFv(w.L[i], w.dL[j][i]);
        for (int r = 0; r < 3; r++) {
          s3.C1(r, i) = linCmtFv(w.C[0][r][i], w.dC[j][0][r][i]);
          s3.C2(r, i) = linCmtFv(w.C[1][r][i], w.dC[j][1][r][i]);
          s3.C3(r, i) = linCmtFv(w.C[2][r][i], w.dC[j][2][r][i]);
        }
      }
    }
    linCmtFv yp[RX_LINWIN_MAXM], ret[RX_LINWIN_MAXM];
    for (int c = 0; c < m; c++) yp[c] = linCmtFv(ypv[c], J(c, j));
    for (int c = 0; c < RX_LINWIN_MAXM; c++) ret[c] = linCmtFv(0.0, 0.0);
    linCmtFv preEv[RX_LINWIN_MAXM];
    const linCmtFv *preE = NULL;
    if (dSlot >= 0) {
      int nL = (ncmt == 1) ? 1 : ncmt;
      for (int i = 0; i < nL; i++) {
        preEv[i] = linCmtFv(w.expL[dSlot][i], w.dExpL[dSlot][j][i]);
      }
      preEv[nL] = oral0 ? linCmtFv(w.expKa[dSlot], w.dExpKa[dSlot][j]) :
        linCmtFv(0.0, 0.0);
      preE = preEv;
    }
    if (ablate == 1) {
      // Counterfactual: the per-direction constants are still built, only
      // the tail kernel itself is replaced by a bounded stand-in.
      double e0 = (dSlot >= 0) ? w.expL[dSlot][0] : 0.5;
      for (int c = 0; c < m; c++) {
        ret[c] = linCmtFv(e0*yp[c].val_, e0*yp[c].d_);
      }
    } else if (ncmt == 1) lc.linCmtStan1Tail<linCmtFv>(k10, yp, kaV, ret, preE);
    else if (ncmt == 2) lc.linCmtStan2Tail<linCmtFv>(s2, yp, kaV, ret, preE);
    else lc.linCmtStan3Tail<linCmtFv>(s3, yp, kaV, ret, preE);
    if (first) {
      for (int c = 0; c < m; c++) {
        lcb.fx(c, 0) = ret[c].val_;
        lc.Asave_[c] = ret[c].val_;
      }
      first = false;
    }
    for (int c = 0; c < m; c++) lcb.Js(c, si) = ret[c].d_;
    si++;
  }
  if (first) return false; // nothing requested: let the full path decide
  return true;
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
// support.  The AD methods (3/30 forward fvar, 32 multi-direction fvar,
// 31 reverse) need no
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
  case 32:  // "ADm": all directions in one forward-mode pass; bitwise as 3
    lc.linCmtDualJac(thetaSens, fx, Js);
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

  // The AD methods (3/30 forward fvar, 31 reverse, 100 auto -> forward) use
  // the unscaled (isAD = true) path so the Jacobian comes out in true-theta
  // units.
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

//' Highest carry sentinel `linCmtB(which1 = -k)` this build understands
//'
//' nlmixr2est gates its carry codegen on this: `-8` (the fast-path pin an
//' event-modifier jump needs) is only emitted when the loaded rxode2 has it.
//' @return integer, the magnitude of the most negative carry sentinel
//' @keywords internal
//' @export
// [[Rcpp::export]]
IntegerVector linCmtCarrySentinelMax() {
  return IntegerVector::create(8L);
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
      lc.restoreFxTo(acur, fx);
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
      lc.restoreFxTo(getAdvan(idx), fx);
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
// see the caller in linCmtB().  The identity holds only while every dose
// feeding the linear system carries the same delay, which is a property of
// this individual's REGIMEN, so op->linCmtLagMask (which compartments the
// model lags) is compared against the compartments actually dosed: an
// individual that doses no lagged compartment (including one that doses
// none at all and runs off an initial condition) gets an exact 0, and one
// mixing lagged and unlagged doses gets NA_REAL rather than the biased
// single-delay answer (nlmixr2/rxode2#1237).  Also NA_REAL for a call that
// does not describe the model `lc` is set up for, when `rate` could not be
// recovered (NULL -- see linCmtBRateSlot()), for a steady-state infusion
// (linCmtDoseScan()), or for an out of range `which2`.
static inline double linCmtBdoseTime(stan::math::linCmtStan &lc,
                                     const Eigen::Matrix<double, Eigen::Dynamic, 1> &amt,
                                     rx_solving_options_ind *ind,
                                     rx_solving_options *op,
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
  if (which2 != -3 && (which2 < 0 || which2 >= ncmt + oral0)) return NA_REAL;
  int dosed, ssInf;
  linCmtDoseScan(ind, op, &dosed, &ssInf);
  // Does this individual's regimen actually satisfy the one-shared-delay
  // assumption the -dA/dt identity rests on (nlmixr2/rxode2#1237)?  Which
  // compartments carry a modeled alag() is model information
  // (op->linCmtLagMask); which ones this individual doses is DATA, so it can
  // only be decided here.  With no modeled alag() declared on any linCmt()
  // compartment there is nothing to compare against -- the caller is asking
  // for a delay it applies to every dose itself, so answer as before.
  if (op->linCmtLagMask != 0) {
    if ((dosed & op->linCmtLagMask) == 0) {
      // No dose reaches a lagged compartment, so the amounts do not depend on
      // the delay at all: the derivative is exactly 0, whatever the rest of
      // the regimen looks like.  (The IV arm of a paired IV/oral design lands
      // here, and it is exact even for the steady-state infusion refused
      // below.)  This covers an individual with no linCmt() dose at all,
      // whose amounts are whatever the initial conditions put there -- those
      // are not delayed either, so -dA/dt would be a nonzero answer to a
      // question whose answer is 0.
      return 0.0;
    }
    if ((dosed & ~op->linCmtLagMask) != 0) {
      // Some doses are delayed and some are not, so there is no single delay
      // to differentiate wrt.  Refuse rather than return the single-delay
      // answer, which is biased by the undelayed doses' contribution -- see
      // nlmixr2/rxode2#1237.
      return NA_REAL;
    }
  }
  if (rate == NULL) return NA_REAL;
  if (ssInf) return NA_REAL;
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
 *  its compartments differently (nlmixr2/rxode2#1237).  Two complementary
 *  checks keep it from silently answering anyway: a model whose linCmt()
 *  compartments carry more than one distinct `alag()` expression is refused
 *  at build time by `.rxLinCmtDoseTimeSensCheck()` (R/eventSens.R), and an
 *  individual whose REGIMEN doses both a lagged and an unlagged linCmt()
 *  compartment -- data this package only sees while solving -- gets
 *  `NA_REAL` here.  An individual dosing no lagged compartment at all gets
 *  an exact 0: its amounts do not depend on the delay, whether they came
 *  from undelayed doses or from an initial condition.  A regular
 *  infusion's rate is recovered at output time via the linCmtBRateSlot()
 *  per-idx cache (nlmixr2/rxode2#1236); an individual with a steady-state
  *  infusion still gets `NA_REAL` -- see linCmtDoseScan().
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
  // A dose-time/carry sentinel may touch lc state between value calls;
  // pure reads (above) do not.  Drop the last-row value memo either way --
  // correctness over a lost short-circuit.
  lcb.memoIdx = -1;
  if (which1 == -3) {
    // idx already solved -> a re-query (e.g. the output pass) where
    // ind->InfusionRate has since been cleared/moved on; use the cached rate.
    const double *rate = (!ind->doSS && ind->solvedIdx >= idx) ?
      linCmtBRateSlot(ind, idx, op->numLin, 0) : getLinRate;
    *out = linCmtBdoseTime(lcb.lc, lcb.fx, ind, op, rate, ncmt, oral0, which2,
                           trans, p1, v1, p2, p3, p4, p5, ka);
  } else if (which1 == -4) {
    *out = linCmtBtransition(lcb, rx, ind, op, idx, _t, ncmt, oral0, which2, trans,
                             p1, v1, p2, p3, p4, p5, ka);
  } else if (which1 == -7) {
    *out = linCmtBcarryAdd(ind, ncmt, oral0, which2, p2);
  } else if (which1 == -8) {
    // Pin this subject's pass to the full -5 advance: a caller feeding -7
    // a contribution that does not telescope (an event jump) must not have
    // the constant-theta fast path skip the M products that propagate it.
    ind->linCmtCarryVarying = 2;
    *out = 0.0;
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
                              int sensType, int id, int idx,
                              Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1> > &theta,
                              Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1> > &thetaSens) {
  if (sensType >= 0 && sensType < RX_LINCMTB_SENS_SEEN) {
#pragma omp atomic write
    linCmtBSensSeen[sensType] = 1;
  }
  int kind = linCmtBfdKind(sensType);
  if (kind != 0) {
    linCmtBfdJac(lcb, kind, ind->linH, theta, thetaSens);
  } else if (sensType == 31) {
    linCmtRevTapeInit();
    stan::math::jacobian(lcb.lc, thetaSens, lcb.fx, lcb.Js);
  } else if (linCmtSeqTailJac(lcb, rx->linCmtSensPhi, id, idx,
                              sensType == 32)) {
#pragma omp atomic
    linCmtSeqTailN++;
  } else if (sensType == 32) {
    // Steady-state rows (and any other shape the window cannot factor into
    // constants + tail) still get the single multi-direction pass.
#pragma omp atomic
    linCmtSeqFullN++;
    lcb.lc.linCmtDualJac(thetaSens, lcb.fx, lcb.Js);
  } else {
#pragma omp atomic
    linCmtSeqFullN++;
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
#pragma omp atomic
    linCmtValRestN++;
    double *acur = getAdvan(idx);
    lcb.lc.restoreJacTo(acur, lcb.J);
    lcb.lc.restoreFxTo(acur, lcb.fx);
    return;
  }
#pragma omp atomic
  linCmtValCompN++;
  lcb.lc.setDt(ind->doSS ? (ind->tout - ind->tprior) : (_t - ind->tprior));
  if (rx->ndiff != 0 && ind->linCmtHparIndex < -1) {
    linCmtBjac(lcb, rx, ind, rx->sensType, id, idx, theta, thetaSens);
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
    if (lcb.liteIdx == idx && lcb.liteId == id) {
      // The thin value path left J/Jg stale for this row; a call-form
      // query consumes them -- restore once, lazily.
      Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1> >
        theta(getLinCmtDoubleAddr(lcb, linCmtBaddrTheta), lcb.lc.getNpars());
      linCmtFillTheta(theta, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka);
      double *acur = getAdvan(idx);
      lcb.lc.restoreJacTo(acur, lcb.J);
      lcb.lc.restoreFxTo(acur, lcb.fx);
      lcb.lc.getJacCp(lcb.J, lcb.fx, theta, lcb.Jg);
      lcb.liteId = lcb.liteIdx = -1;
    }
    double out;
    if (linCmtBquery(lcb, rx, ind, op, idx, _t, ncmt, oral0, which1, which2, trans,
                     p1, v1, p2, p3, p4, p5, ka, &out)) {
      return out;
    }
  } else if (!lcb.lc.isSame(ncmt, oral0, trans, rx->ndiff)) {
    linCmtBsetModel(lcb, ncmt, oral0, trans, ind->linSS, rx);
  } else {
    // Last-row value memo: the generated model executes this value call
    // many times per row (compute phase and restore path alike); a repeat
    // with an identical key returns the cached result with J/Jg/fx left
    // standing for the reads.  The key covers every input the value
    // depends on; sentinels and reshapes invalidate (see linCmtBquery /
    // linCmtBsetModel).
    const double args[7] = {p1, v1, p2, p3, p4, p5, ka};
    if (lcb.memoIdx == idx && lcb.memoId == id && lcb.memoT == _t &&
        lcb.memoFlag == ind->_rxFlag && lcb.memoDoSS == (int)ind->doSS &&
        lcb.memoHpar == ind->linCmtHparIndex &&
        lcb.memoH == ind->linCmtH && lcb.memoHV == ind->linCmtHV &&
        memcmp(lcb.memoArgs, args, sizeof(args)) == 0) {
#pragma omp atomic
      linCmtMemoHitN++;
      return lcb.memoVal;
    }
    lcb.lc.setSsType(ind->linSS);
  }
  // Thin value path (the dydt/calc_lhs consolidation, linCmtB only): an
  // already-solved row's value re-execution (the calc_lhs walk and the
  // output pass) needs only fx and the concentration scaling -- skip
  // sensTheta, the SS setup, the rate cache, the m x npars Jacobian
  // restore and getJacCp.  J/Jg are left stale for the row and restored
  // lazily if a call-form query (carry sentinel/read) follows.
  // FD-perturbed evaluations keep the full path.
  if (which1 == -1 && which2 == -1 &&
      ind->linCmtAlast == NULL && ind->linCmtHparIndex < -1 &&
      ((!ind->doSS && ind->solvedIdx >= idx) || ind->_rxFlag == 11)) {
    Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1> >
      thetaL(getLinCmtDoubleAddr(lcb, linCmtBaddrTheta), lcb.lc.getNpars());
    linCmtFillTheta(thetaL, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka);
    lcb.lc.restoreFxTo(getAdvan(idx), lcb.fx);
    double val = lcb.lc.adjustF(lcb.fx, thetaL, ind->linCmtHV);
#pragma omp atomic
    linCmtValLiteN++;
    lcb.liteId = id; lcb.liteIdx = idx;
    lcb.memoId = id; lcb.memoIdx = idx; lcb.memoT = _t;
    lcb.memoFlag = ind->_rxFlag; lcb.memoDoSS = (int)ind->doSS;
    lcb.memoHpar = ind->linCmtHparIndex;
    lcb.memoH = ind->linCmtH; lcb.memoHV = ind->linCmtHV;
    lcb.memoArgs[0] = p1; lcb.memoArgs[1] = v1; lcb.memoArgs[2] = p2;
    lcb.memoArgs[3] = p3; lcb.memoArgs[4] = p4; lcb.memoArgs[5] = p5;
    lcb.memoArgs[6] = ka;
    lcb.memoVal = val;
    return val;
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
  double val = lcb.lc.adjustF(lcb.fx, theta, ind->linCmtHV);
  lcb.memoId = id; lcb.memoIdx = idx; lcb.memoT = _t;
  lcb.memoFlag = ind->_rxFlag; lcb.memoDoSS = (int)ind->doSS;
  lcb.memoHpar = ind->linCmtHparIndex;
  lcb.memoH = ind->linCmtH; lcb.memoHV = ind->linCmtHV;
  lcb.memoArgs[0] = p1; lcb.memoArgs[1] = v1; lcb.memoArgs[2] = p2;
  lcb.memoArgs[3] = p3; lcb.memoArgs[4] = p4; lcb.memoArgs[5] = p5;
  lcb.memoArgs[6] = ka;
  lcb.memoVal = val;
  return val;
}
