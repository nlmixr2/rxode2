#define R_NO_REMAP
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "../inst/include/rxode2.h"
#include "tte.h"
#include "tteState.h"

// 1 when the subject's current record is an event row of endpoint k
static double rxTteRowSim(int cSub, int k) {
  rx_solve *rx = getRxSolve_();
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

// Model-side helpers: what 0 = sim (1 on an event row), 1 = H at the last
// event at or before t, 2 = time of that event (window start before the
// first), 3 = number of events at or before t
extern "C" double _rxTte(int cSub, double t, int what, double kd) {
  int k = (int)kd;
  rxTteState *st = rxTteGet(cSub, k);
  if (st == NULL) return (what == 0 || what == 3) ? 0.0 : NA_REAL;
  if (what == 0) return rxTteRowSim(cSub, k);
  int n = rxTteEventsBy(st, t);
  if (what == 3) return (double)n;
  if (n > 0) return what == 1 ? st->evH[n - 1] : st->evT[n - 1];
  if (what == 1) return st->hStart;
  return ISNA(st->start) ? 0.0 : st->start;
}
