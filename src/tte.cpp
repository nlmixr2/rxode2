#define R_NO_REMAP
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "rxomp.h"
#include <cmath>
#include <cstdint>
#include <vector>
#include "../inst/include/rxode2.h"
#include "tte.h"
#include "tteState.h"

// Per-solve tte() configuration, set by _rxode2_rxTteSetup() from R before a
// ui simulation and cleared after it, and the per-subject event state.
static std::vector<rxTteEndpoint> _rxTteEps;
static uint64_t _rxTteSeed = 0;
static std::vector<rxTteState> _rxTteSt;
static uint32_t _rxTteNsub = 0;

extern "C" int rxTteN(void) {
  return (int)_rxTteEps.size();
}

const rxTteEndpoint &rxTteEp(int k) { return _rxTteEps[k - 1]; }
int rxTteHIdx(int k) { return rxTteEp(k).hIdx; }
int rxTteIsInterval(int k) { return rxTteEp(k).interval; }
int rxTteDropout(int k) { return rxTteEp(k).dropout; }

rxTteState *rxTteGet(int subj, int k) {
  size_t i = (size_t)subj * _rxTteEps.size() + (size_t)(k - 1);
  if (k < 1 || k > rxTteN() || i >= _rxTteSt.size()) return NULL;
  return &_rxTteSt[i];
}

// splitmix64: a counter-based draw, so the k-th threshold of a subject is the
// same for any thread count and independent of the other random streams
static inline uint64_t rxTteMix(uint64_t z) {
  z += 0x9e3779b97f4a7c15ULL;
  z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;
  z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;
  return z ^ (z >> 31);
}

static inline double rxTteThreshold(int subj, int k, int count) {
  uint64_t z = rxTteMix(_rxTteSeed);
  z = rxTteMix(z ^ (uint64_t)(uint32_t)subj);
  z = rxTteMix(z ^ ((uint64_t)(uint32_t)k << 32));
  z = rxTteMix(z ^ (uint64_t)(uint32_t)count);
  // uniform on (0, 1)
  double u = ((double)(z >> 11) + 0.5) * (1.0 / 9007199254740992.0);
  return -std::log(u);
}

// The subject count is fixed for the whole solve, so this resizes once
static void rxTteAlloc(rx_solve *rx) {
  uint32_t n = rx->nsub * rx->nsim;
  if (n == 0) n = rx->nsub;
  if (_rxTteNsub == n && _rxTteSt.size() == (size_t)n * _rxTteEps.size()) return;
  _rxTteSt.assign((size_t)n * _rxTteEps.size(), rxTteState());
  _rxTteNsub = n;
}

static void rxTteReset(rxTteState *st, int subj, int k) {
  st->start = st->end = NA_REAL;
  st->hStart = st->h0 = 0.0;
  st->e = rxTteThreshold(subj, k, 0);
  st->started = st->done = 0;
  st->evT.clear();
  st->evH.clear();
}

// Widen an endpoint's observation window to cover record r
static void rxTteWindowRecord(rx_solving_options_ind *ind, int subj, int r) {
  int k;
  int role = rxTteRole(ind->dv[r], &k);
  rxTteState *st = role == 0 ? NULL : rxTteGet(subj, k);
  if (st == NULL || role == RXTTE_EVENT || role == RXTTE_USED) return;
  double t = ind->all_times[r];
  if (role == RXTTE_START) {
    if (ISNA(st->start) || t < st->start) st->start = t;
  } else if (ISNA(st->end) || t > st->end) {
    st->end = t;
  }
}

void rxTteSubjectInit(rx_solving_options_ind *ind, int subj) {
  if (_rxTteEps.empty()) return;
  rx_solve *rx = getRxSolve_();
#pragma omp critical(rxTteAllocCrit)
  rxTteAlloc(rx);
  for (int k = 1; k <= rxTteN(); k++) {
    rxTteState *st = rxTteGet(subj, k);
    if (st != NULL) rxTteReset(st, subj, k);
  }
  for (int r = 0; r < ind->n_all_times; r++) rxTteWindowRecord(ind, subj, r);
}

// Record an event; returns 1 when the endpoint has reached maxEventNumber
int rxTteRecord(int subj, int k, double tStar, double hStar) {
  rxTteState *st = rxTteGet(subj, k);
  if (st == NULL) return 1;
  st->evT.push_back(tStar);
  st->evH.push_back(hStar);
  st->h0 = hStar;
  int count = (int)st->evT.size();
  st->e = rxTteThreshold(subj, k, count);
  st->done = (double)count >= rxTteEp(k).maxEvent;
  return st->done;
}

void rxTteSetDone(int subj, int k) {
  rxTteState *st = rxTteGet(subj, k);
  if (st != NULL) st->done = 1;
}

// Unused event slots and slots holding interval-censored events are not
// output rows
void rxTteSubjectFinish(rx_solving_options_ind *ind) {
  for (int r = 0; r < ind->n_all_times; r++) {
    int k;
    int role = rxTteRole(ind->dv[r], &k);
    if (role == RXTTE_FREE || role == RXTTE_USED) ind->evid[r] = 9;
  }
}

// Set (or with NULL clear) the tte() configuration for the next solve
extern "C" SEXP _rxode2_rxTteSetup(SEXP cfg) {
  _rxTteEps.clear();
  _rxTteSt.clear();
  _rxTteNsub = 0;
  if (Rf_isNull(cfg)) return R_NilValue;
  int n = Rf_length(VECTOR_ELT(cfg, 0));
  _rxTteEps.resize(n);
  for (int i = 0; i < n; i++) {
    _rxTteEps[i].hIdx = INTEGER(VECTOR_ELT(cfg, 0))[i];
    _rxTteEps[i].maxEvent = REAL(VECTOR_ELT(cfg, 1))[i];
    _rxTteEps[i].interval = INTEGER(VECTOR_ELT(cfg, 2))[i];
    _rxTteEps[i].dropout = INTEGER(VECTOR_ELT(cfg, 3))[i];
  }
  _rxTteSeed = (uint64_t)(uint32_t)INTEGER(VECTOR_ELT(cfg, 4))[0];
  return R_NilValue;
}
