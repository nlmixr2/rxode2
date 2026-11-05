#define R_NO_REMAP
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "../inst/include/rxode2.h"
#include "tte.h"
#include "tteState.h"
#include <algorithm>
#include <vector>

// 1 when the subject's current record is an event row of endpoint k
static double rxTteRowSim(int cSub, int k) {
  rx_solve *rx = getRxSolve2_();
  rx_solving_options_ind *ind = &(rx->subjects[cSub]);
  if (ind->idx < 0 || ind->idx >= ind->n_all_times) return 0.0;
  int kk;
  int role = rxTteRole(ind->dv[ind->ix[ind->idx]], &kk);
  return (kk == k && role == RXTTE_EVENT) ? 1.0 : 0.0;
}

// Number of recorded events at or before t
static int rxTteEventsBy(const rxTteState *st, double t) {
  int n = 0, m = (int)st->evT.size();
  while (n < m && st->evT[n] <= t) n++;
  return n;
}

// Decode the tteTime()/tteCount() code +/-(k + 100*cmt) (negative: origin=TRUE)
static inline void rxTteDecode(double code, int *k, int *cmt, int *origin) {
  int c = (int)(fabs(code) + 0.5);
  *k = c % 100;
  *cmt = c / 100;
  *origin = code < 0;
}

// Estimation solves have no simulated event store; the events are the
// endpoint's DV=1 records in the data.  One cache per thread (a thread solves
// one subject at a time), invalidated by every new solve.
static int _rxTteDataGen = 0;
extern "C" void rxTteDataNewSolve(void) {
  _rxTteDataGen++;
}

struct rxTteDataCache {
  int gen = -1;
  const rx_solving_options_ind *ind = NULL;
  double code = 0.0;
  double start = 0.0;
  std::vector<double> evT;
};

// Is data record r an observation of endpoint cmt?  Its CMT is remapped like
// the model's _CMT; without a CMT covariate every observation is.
static bool rxTteDataIsEndpoint(rx_solving_options *op, rx_solving_options_ind *ind, int r,
                                int cmtBase, int cmtSens, int cmt) {
  if (ind->evid[r] != 0) return false;
  if (op->cmtCov < 0 || ind->cov_ptr == NULL) return true;
  double v = ind->cov_ptr[(size_t)ind->n_all_times_orig * (size_t)op->cmtCov + (size_t)r];
  if (ISNA(v)) return false;
  if (cmtSens > 0 && fabs(v) > cmtBase) v = v < 0 ? v + cmtSens : v - cmtSens;
  return (int)v == cmt;
}

// The event times (DV=1 records) and observation start of an endpoint: its
// first record, or the subject's first record for origin=TRUE
static const rxTteDataCache &rxTteDataEvents(rx_solving_options_ind *ind, int cmtBase,
                                             int cmtSens, double code) {
  static thread_local rxTteDataCache c;
  if (c.gen == _rxTteDataGen && c.ind == ind && c.code == code) return c;
  int k, cmt, origin;
  rxTteDecode(code, &k, &cmt, &origin);
  rx_solving_options *op = getRxSolve2_()->op;
  c.gen = _rxTteDataGen;
  c.ind = ind;
  c.code = code;
  c.evT.clear();
  c.start = R_PosInf;
  for (int r = 0; r < ind->n_all_times_orig; r++) {
    double t = ind->all_times[r];
    bool ep = rxTteDataIsEndpoint(op, ind, r, cmtBase, cmtSens, cmt);
    if ((origin || ep) && t < c.start) c.start = t;
    if (ep && ind->dv[r] == 1.0) c.evT.push_back(t);
  }
  std::sort(c.evT.begin(), c.evT.end());
  if (!R_FINITE(c.start)) c.start = 0.0;
  return c;
}

// tteTime()/tteCount() of an estimation solve: the events before t, or at t
// while integrating (the hazard right after an event), but not at t for the
// record's own likelihood (the hazard just before its event)
static double rxTteData(int cSub, double t, int what, int cmtBase, int cmtSens,
                        double code) {
  if (what == 0) return 0.0;
  if (what == 1) return NA_REAL;
  rx_solving_options_ind *ind = &(getRxSolve2_()->subjects[cSub]);
  const rxTteDataCache &c = rxTteDataEvents(ind, cmtBase, cmtSens, code);
  int n = ind->inLhs ?
    (int)(std::lower_bound(c.evT.begin(), c.evT.end(), t) - c.evT.begin()) :
    (int)(std::upper_bound(c.evT.begin(), c.evT.end(), t) - c.evT.begin());
  if (what == 3) return (double)n;
  return n > 0 ? c.evT[n - 1] : c.start;
}

// Model-side helpers: what 0 = sim (1 on an event row), 1 = H at the last
// event at or before t, 2 = time of that event (window start before the
// first), 3 = number of events at or before t
extern "C" double _rxTte(int cSub, double t, int what, int cmtBase, int cmtSens,
                         double code) {
  int k, cmt, origin;
  rxTteDecode(code, &k, &cmt, &origin);
  rxTteState *st = rxTteGet(cSub, k);
  if (st == NULL) return rxTteData(cSub, t, what, cmtBase, cmtSens, code);
  if (what == 0) return rxTteRowSim(cSub, k);
  int n = rxTteEventsBy(st, t);
  if (what == 3) return (double)n;
  if (n > 0) return what == 1 ? st->evH[n - 1] : st->evT[n - 1];
  if (what == 1) return st->hStart;
  return ISNA(st->start) ? 0.0 : st->start;
}
