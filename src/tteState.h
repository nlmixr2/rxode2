// tte() per-solve configuration and per-subject event state, shared by
// tte.cpp (store), tteLocate.cpp (root finding) and tteModel.cpp (model
// helpers).  Internal to rxode2.
#ifndef __RXODE2_TTE_STATE_H__
#define __RXODE2_TTE_STATE_H__

#include <vector>

struct rxTteEndpoint {
  int hIdx;          // 0-based state index of the cumulative hazard
  double maxEvent;   // maximum number of events (Inf = unlimited)
  int interval;      // 1 = intervalCensored
  int dropout;       // 1 = the event ends the subject's observations
};

struct rxTteState {
  double start;      // window start (NA = no window)
  double end;        // right censoring time
  double hStart;     // cumulative hazard at the window start
  double h0;         // cumulative hazard at the last event (or start)
  double e;          // current exp(1) threshold
  int started;
  int done;
  std::vector<double> evT, evH;
};

const rxTteEndpoint &rxTteEp(int k);
rxTteState *rxTteGet(int subj, int k);

#endif
