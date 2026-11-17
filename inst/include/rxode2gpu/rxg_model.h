#ifndef RXG_MODEL_H
#define RXG_MODEL_H
// Included by the generated device model code (<model>.cl) after its
// RXG_NEQ/RXG_NLHS/RXG_NPAR and __DDT/_LHS_ defines.
#include "rxode2gpu/rxg_prelude.h"

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
} rxg_ctx;

#define _IR (_rxg->ir)
#define _ON (_rxg->on)
#define _PL (_rxg->pl)
// per-call copies the generated code declares, seeded from the context
#define rx_lambda_ _rxgLambda
#define rx_yj_ _rxgYj
#define rx_hi_ _rxgHi
#define rx_low_ _rxgLow

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

#endif // RXG_MODEL_H
