#define R_NO_REMAP
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "rxomp.h"
#include <cmath>
#include <cstdint>
#include <vector>
#include <boost/math/tools/roots.hpp>
#include "../inst/include/rxode2.h"
#include "tte.h"

// Per-solve tte() configuration, set by rxTteSetup() from R before a ui
// simulation and cleared after it.
struct rxTteEndpoint {
  int hIdx;          // 0-based state index of the cumulative hazard
  double maxEvent;   // maximum number of events (Inf = unlimited)
  int interval;      // 1 = intervalCensored
  int dropout;       // 1 = the event ends the subject's observations
};

static std::vector<rxTteEndpoint> _rxTteEp;
static uint64_t _rxTteSeed = 0;

// Per-subject, per-endpoint event state
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

static std::vector<rxTteState> _rxTteSt;
static uint32_t _rxTteNsub = 0;

extern "C" int rxTteN(void) {
  return (int)_rxTteEp.size();
}

int rxTteHIdx(int k) { return _rxTteEp[k - 1].hIdx; }
int rxTteIsInterval(int k) { return _rxTteEp[k - 1].interval; }
int rxTteDropout(int k) { return _rxTteEp[k - 1].dropout; }

static inline rxTteState *rxTteGet(int subj, int k) {
  size_t i = (size_t)subj * _rxTteEp.size() + (size_t)(k - 1);
  if (k < 1 || k > (int)_rxTteEp.size() || i >= _rxTteSt.size()) return NULL;
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

static void rxTteAlloc(rx_solve *rx) {
  uint32_t n = rx->nsub * rx->nsim;
  if (n == 0) n = rx->nsub;
  if (_rxTteNsub == n && _rxTteSt.size() == (size_t)n * _rxTteEp.size()) return;
  _rxTteSt.assign((size_t)n * _rxTteEp.size(), rxTteState());
  _rxTteNsub = n;
}

void rxTteSubjectInit(rx_solving_options_ind *ind, int subj) {
  if (_rxTteEp.empty()) return;
  rx_solve *rx = getRxSolve_();
  // every thread checks before touching its own subject, and the size is
  // fixed for the whole solve, so the vector is resized at most once
#pragma omp critical(rxTteAllocCrit)
  rxTteAlloc(rx);
  int nk = (int)_rxTteEp.size();
  for (int k = 1; k <= nk; k++) {
    rxTteState *st = rxTteGet(subj, k);
    if (st == NULL) continue;
    st->start = NA_REAL;
    st->end = NA_REAL;
    st->hStart = 0.0;
    st->h0 = 0.0;
    st->e = rxTteThreshold(subj, k, 0);
    st->started = 0;
    st->done = 0;
    st->evT.clear();
    st->evH.clear();
  }
  for (int r = 0; r < ind->n_all_times; r++) {
    int k;
    int role = rxTteRole(ind->dv[r], &k);
    if (role == 0) continue;
    rxTteState *st = rxTteGet(subj, k);
    if (st == NULL) continue;
    double t = ind->all_times[r];
    if (role == RXTTE_START) {
      if (ISNA(st->start) || t < st->start) st->start = t;
    } else if (role == RXTTE_CENSOR || role == RXTTE_INTERVAL || role == RXTTE_FREE) {
      if (ISNA(st->end) || t > st->end) st->end = t;
    }
  }
}

// Earliest tte event in (xs, xout], located by toms748 on H(t) - H0 - E with
// H(t) from re-integrating the interval's starting state ys.
int rxTteLocate(rx_solving_options_ind *ind, int subj, int neq, double xs,
                const double *ys, double xout, const double *yout,
                const rxTteInteg &integ, double *tStar, int *kOut,
                double *yStar) {
  int nk = (int)_rxTteEp.size();
  if (nk == 0) return 0;
  // the window starts at its own record, so no interval straddles it
  for (int k = 1; k <= nk; k++) {
    rxTteState *st = rxTteGet(subj, k);
    if (st == NULL || st->started || ISNA(st->start)) continue;
    if (xout >= st->start) {
      st->started = 1;
      st->hStart = st->h0 = yout[_rxTteEp[k - 1].hIdx];
    }
  }
  if (!(xout > xs)) return 0;
  int found = 0;
  std::vector<double> tmp(neq);
  for (int k = 1; k <= nk; k++) {
    rxTteState *st = rxTteGet(subj, k);
    if (st == NULL || st->done || !st->started || xs < st->start) continue;
    int h = _rxTteEp[k - 1].hIdx;
    if (!ISNA(st->end) && xs >= st->end) continue;
    double target = st->h0 + st->e;
    if (!(yout[h] >= target)) continue;
    double lo = xs, hi = xout;
    if (!ISNA(st->end) && hi > st->end) continue; // the censoring row is a record
    auto g = [&](double t) {
      std::copy(ys, ys + neq, tmp.begin());
      integ(tmp.data(), xs, t);
      return tmp[h] - target;
    };
    double flo = ys[h] - target;
    double fhi = yout[h] - target;
    double t;
    if (flo >= 0.0) {
      t = lo;
    } else if (fhi == 0.0) {
      t = hi;
    } else {
      boost::uintmax_t maxIter = 100;
      boost::math::tools::eps_tolerance<double> tol(40);
      std::pair<double, double> r =
        boost::math::tools::toms748_solve(g, lo, hi, flo, fhi, tol, maxIter);
      t = r.second;
    }
    if (!found || t < *tStar) {
      found = 1;
      *tStar = t;
      *kOut = k;
    }
  }
  if (found) {
    std::copy(ys, ys + neq, yStar);
    integ(yStar, xs, *tStar);
  }
  return found;
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
  if ((double)count >= _rxTteEp[k - 1].maxEvent) {
    st->done = 1;
    return 1;
  }
  return 0;
}

// Unused event slots and slots holding interval-censored events are not
// output rows
void rxTteSubjectFinish(rx_solving_options_ind *ind) {
  for (int r = 0; r < ind->n_all_times; r++) {
    int k;
    int role = rxTteRole(ind->dv[r], &k);
    if (role == RXTTE_FREE || role == RXTTE_USED) ind->evid[r] = 2;
  }
}

void rxTteSetDone(int subj, int k) {
  rxTteState *st = rxTteGet(subj, k);
  if (st != NULL) st->done = 1;
}

// Model-side helpers: what 0 = sim (1 on an event row), 1 = H at the last
// event at or before t, 2 = time of that event (window start before the
// first), 3 = number of events at or before t
extern "C" double _rxTte(int cSub, double t, int what, double kd) {
  int k = (int)kd;
  rxTteState *st = rxTteGet(cSub, k);
  if (st == NULL) {
    return (what == 3 || what == 0) ? 0.0 : NA_REAL;
  }
  if (what == 0) {
    rx_solve *rx = getRxSolve_();
    rx_solving_options_ind *ind = &(rx->subjects[cSub]);
    if (ind->idx < 0 || ind->idx >= ind->n_all_times) return 0.0;
    int kk;
    int role = rxTteRole(ind->dv[ind->ix[ind->idx]], &kk);
    return (kk == k && role == RXTTE_EVENT) ? 1.0 : 0.0;
  }
  int n = 0;
  int m = (int)st->evT.size();
  while (n < m && st->evT[n] <= t) n++;
  switch (what) {
  case 1:
    return n == 0 ? st->hStart : st->evH[n - 1];
  case 2:
    return n == 0 ? (ISNA(st->start) ? 0.0 : st->start) : st->evT[n - 1];
  case 3:
    return (double)n;
  }
  return NA_REAL;
}

// Set (or with NULL clear) the tte() configuration for the next solve
extern "C" SEXP _rxode2_rxTteSetup(SEXP cfg) {
  _rxTteEp.clear();
  _rxTteSt.clear();
  _rxTteNsub = 0;
  if (Rf_isNull(cfg)) return R_NilValue;
  SEXP hIdx = VECTOR_ELT(cfg, 0);
  SEXP maxEvent = VECTOR_ELT(cfg, 1);
  SEXP interval = VECTOR_ELT(cfg, 2);
  SEXP dropout = VECTOR_ELT(cfg, 3);
  SEXP seed = VECTOR_ELT(cfg, 4);
  int n = Rf_length(hIdx);
  _rxTteEp.resize(n);
  for (int i = 0; i < n; i++) {
    _rxTteEp[i].hIdx = INTEGER(hIdx)[i];
    _rxTteEp[i].maxEvent = REAL(maxEvent)[i];
    _rxTteEp[i].interval = INTEGER(interval)[i];
    _rxTteEp[i].dropout = INTEGER(dropout)[i];
  }
  _rxTteSeed = (uint64_t)(uint32_t)INTEGER(seed)[0];
  return R_NilValue;
}
