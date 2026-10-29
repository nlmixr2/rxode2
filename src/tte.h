// Time-to-event (tte()) simulation: per-subject event store and root finding.
// The solver-loop side (re-timing records, rewinding the integrator) lives in
// par_solve.cpp; this header is internal to rxode2.
#ifndef __RXODE2_TTE_H__
#define __RXODE2_TTE_H__

#include <functional>

// Record roles, encoded in the DV of the records rxSolve() adds for a tte
// endpoint: DV = -(RXTTE_MARK + 10*k + role), k the 1-based tte index.
#define RXTTE_MARK 1000000.0
#define RXTTE_START 1     // window start (DV=0)
#define RXTTE_CENSOR 2    // right censoring (DV=0)
#define RXTTE_INTERVAL 3  // interval end, no event (DV=0)
#define RXTTE_FREE 4      // unused event slot (evid 9, not output, once solved)
#define RXTTE_EVENT 5     // event row / interval end holding an event (DV=1)
#define RXTTE_USED 6      // event slot used by an interval-censored event

static inline int rxTteRole(double dv, int *k) {
  if (dv <= -RXTTE_MARK && dv > -2.0 * RXTTE_MARK) {
    int c = (int)(-dv - RXTTE_MARK + 0.5);
    *k = c / 10;
    return c % 10;
  }
  *k = 0;
  return 0;
}

static inline double rxTteDv(int k, int role) {
  return -(RXTTE_MARK + 10.0 * k + role);
}

typedef std::function<void(double *y, double t0, double t1)> rxTteInteg;

extern "C" int rxTteN(void);
int rxTteHIdx(int k);
int rxTteIsInterval(int k);
int rxTteDropout(int k);
void rxTteSubjectInit(rx_solving_options_ind *ind, int subj);
int rxTteLocate(rx_solving_options_ind *ind, int subj, int neq, double xs,
                const double *ys, double xout, const double *yout,
                const rxTteInteg &integ, double *tStar, int *kOut,
                double *yStar);
int rxTteRecord(int subj, int k, double tStar, double hStar);
void rxTteSetDone(int subj, int k);
void rxTteSubjectFinish(rx_solving_options_ind *ind);

#endif
