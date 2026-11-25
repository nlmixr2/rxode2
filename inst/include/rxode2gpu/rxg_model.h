#ifndef RXG_MODEL_H
#define RXG_MODEL_H
// Included by the generated device model code (<model>.cl) after its
// RXG_NEQ/RXG_NLHS/RXG_NPAR and __DDT/_LHS_ defines.
#include "rxode2gpu/rxg_prelude.h"

#ifndef RXG_HAS_DELAY
#define RXG_HAS_DELAY 0
#endif
#ifndef RXG_TIME
#define RXG_TIME 1 // the model may read time
#endif

// Integrator codes (rxg_stepper.h); defined here so RXG_METHOD may name
// one before the history degree below is chosen
#define RXG_RKT54 1
#define RXG_VERN76 2
#define RXG_ROS54P 3
#define RXG_RKT54_ROS54P 4  // AutoSwitch composites (rxg_solve())
#define RXG_VERN76_ROS54P 5

#ifndef RXG_NDLY
#define RXG_NDLY 0
#endif
#define RXG_NDLY1 (RXG_NDLY > 0 ? RXG_NDLY : 1)
// Degree of the history interpolant: 7 for vern76, 4 otherwise
#ifndef RXG_HDEG
#if defined(RXG_METHOD) && RXG_METHOD != RXG_VERN76 && RXG_METHOD != RXG_VERN76_ROS54P
#define RXG_HDEG 4
#else
#define RXG_HDEG 7
#endif
#endif

// Dense history read by delay(): one record per accepted step holding, for
// each state delay() reads (column RXG_DCOL(state)), the step's interpolant
// y(t_old + s*h) = sum_j a_j s^j (j = 0..RXG_HDEG), then t_old and h.
#define RXG_HREC ((RXG_HDEG + 1)*RXG_NDLY1 + 2)
#define RXG_NLAG 4  // distinct delays tracked for breakpoints
#define RXG_NBP 64  // breakpoints kept
typedef struct rxg_hist {
  RXG_GLOBAL double *rec; // cap records of RXG_HREC values
  int n, cap;             // records used and available
  double t0;              // start of the history; pre-history before it
  double minT;            // smallest positive delay seen (caps the step)
  double ini[RXG_NEQ1];   // pre-history: the state at t0
  // the constant pre-history leaves derivative discontinuities at t0 plus
  // sums of the delays; steps land on them (as DelayDiffEq's constant_lags)
  double lag[RXG_NLAG];   // distinct delays seen
  int nlag;
  double bp[RXG_NBP];     // breakpoints after t0, increasing
  int nbp;
  int bpDirty;            // a new delay was seen: rebuild bp
} rxg_hist;

// Everything rxg_dydt()/rxg_jac() read besides time and the states.  The
// host fills one per integration interval [t0, t1].
typedef struct rxg_ctx {
  double p0[RXG_NPAR1]; // parameters (incl. covariates) at t0
  double p1[RXG_NPAR1]; // parameters at t1 (read when covType != 0)
  double pl[RXG_NLHS1]; // last lhs values
  double ir[RXG_NEQ1];  // infusion rates
  int on[RXG_NEQ1];     // compartment on/off
  double t0, t1;        // interval ends, used to interpolate covariates
  double curShift;      // time shift (steady state / reset)
  double tpk;           // record time PK statements read when nonmem != 0
  int covType;          // 0 p0; 1 linear; 2 p0 before the midpoint, p1 after
  int nonmem;           // rxControl(nonmem=TRUE)
  double lambda, yj;    // transform-both-sides values (rx_lambda_, rx_yj_)
  double logitHi, logitLow; // (rx_hi_, rx_low_)
  rxg_hist *hist;       // delay() history (models with delay() only)
} rxg_ctx;

#define _IR (_rxg->ir)
#define _ON (_rxg->on)
#define _PL (_rxg->pl)
// per-call copies the generated code declares, seeded from the context
#define rx_lambda_ _rxgLambda
#define rx_yj_ _rxgYj
#define rx_hi_ _rxgHi
#define rx_low_ _rxgLow
// delay(state, T) in the generated code
#define _rxDelay(ind, i, t, T) rxg_delay(_rxg, i, t, T)

RXG_FN const double *rxg_par(const rxg_ctx *c, double t, double *pp) {
  if (c->covType == 0) return c->p0;
  if (c->covType == 2) return (t < 0.5*(c->t0 + c->t1)) ? c->p0 : c->p1;
  double dt = c->t1 - c->t0;
  double f = (dt > 0) ? (t - c->t0)/dt : 0.0;
  for (int i = 0; i < RXG_NPAR; i++) {
    pp[i] = (c->p0[i] == c->p1[i]) ? c->p0[i] : c->p0[i] + (c->p1[i] - c->p0[i])*f;
  }
  return pp;
}

#if RXG_HAS_DELAY
// Start a subject's history at t0 (solver time) with the state y
RXG_FN void rxg_histInit(rxg_hist *h, RXG_GLOBAL double *rec, int cap,
                         double t0, const double *y) {
  h->rec = rec;
  h->n = 0;
  h->cap = cap;
  h->t0 = t0;
  h->minT = INFINITY;
  h->nlag = 0;
  h->nbp = 0;
  h->bpDirty = 0;
  for (int i = 0; i < RXG_NEQ; i++) h->ini[i] = y[i];
}

// Remember a delay for the breakpoints
RXG_FN void _rxg_noteLag(rxg_hist *h, double T) {
  if (!(T > 0.0) || !isfinite(T)) return;
  if (T < h->minT) h->minT = T;
  for (int k = 0; k < h->nlag; k++) {
    if (fabs(h->lag[k] - T) <= 1e-12*T) return;
  }
  if (h->nlag < RXG_NLAG) {
    h->lag[h->nlag++] = T;
    h->bpDirty = 1;
  }
}

// State i at t - T (model time t): the constant pre-history before the
// history starts, otherwise the interpolant of the step holding t - T (the
// last step's, extrapolated, past its end)
RXG_FN double rxg_delay(const rxg_ctx *c, int i, double t, double T) {
  rxg_hist *h = c->hist;
  _rxg_noteLag(h, T);
  double td = t - c->curShift - T;
  int col = RXG_DCOL(i);
  if (h->n == 0 || td <= h->t0 || col < 0) return h->ini[i];
  const int it = (RXG_HDEG + 1)*RXG_NDLY1;
  int lo = 0, hi = h->n - 1;
  while (lo < hi) {
    int mid = (lo + hi + 1)/2;
    if (h->rec[mid*RXG_HREC + it] <= td) lo = mid; else hi = mid - 1;
  }
  RXG_GLOBAL const double *r = h->rec + lo*RXG_HREC;
  double s = (td - r[it])/r[it + 1];
  double v = r[RXG_HDEG*RXG_NDLY1 + col];
  for (int j = RXG_HDEG - 1; j >= 0; j--) v = r[j*RXG_NDLY1 + col] + s*v;
  return v;
}
#endif

#endif // RXG_MODEL_H
