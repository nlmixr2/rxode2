#ifndef RXG_HOST_H
#define RXG_HOST_H
// .C() entry points for a model's device code compiled as host C (included
// after the model's .cl file).
#include <string.h>
#include "rxode2gpu/rxg_stepper.h"

static void rxg_host_ctx(rxg_ctx *c, double *p0, double *p1, double *pl,
                         double *ir, int *on, double *tt, int *flags) {
  memcpy(c->p0, p0, sizeof(double)*RXG_NPAR1);
  memcpy(c->p1, p1, sizeof(double)*RXG_NPAR1);
  memcpy(c->pl, pl, sizeof(double)*RXG_NLHS1);
  memcpy(c->ir, ir, sizeof(double)*RXG_NEQ1);
  memcpy(c->on, on, sizeof(int)*RXG_NEQ1);
  c->t0 = tt[0];
  c->t1 = tt[1];
  c->curShift = tt[2];
  c->tpk = tt[3];
  c->lambda = tt[4];
  c->yj = tt[5];
  c->logitHi = tt[6];
  c->logitLow = tt[7];
  c->covType = flags[0];
  c->nonmem = flags[1];
}

void rxg_host_dims(int *d) {
  d[0] = RXG_NEQ;
  d[1] = RXG_NLHS;
  d[2] = RXG_NPAR;
  d[3] = RXG_HAS_JAC;
}

void rxg_host_dydt(double *t, double *y, double *dy, double *p0, double *p1,
                   double *pl, double *ir, int *on, double *tt, int *flags) {
  rxg_ctx c;
  rxg_host_ctx(&c, p0, p1, pl, ir, on, tt, flags);
  rxg_dydt(t[0], y, dy, &c);
}

void rxg_host_jac(double *t, double *y, double *jac, double *p0, double *p1,
                  double *pl, double *ir, int *on, double *tt, int *flags) {
  rxg_ctx c;
  rxg_host_ctx(&c, p0, p1, pl, ir, on, tt, flags);
  for (int i = 0; i < RXG_NEQ*RXG_NEQ; i++) jac[i] = 0.0;
  rxg_jac(t[0], y, jac, RXG_NEQ, &c);
}

// Solve over the times tm[0..nt-1] from y (nt x neq, row t), keeping the
// step sizes and the AutoSwitch state between intervals.
// dw = {hmin, hmax, nonstifftol, stifftol, h (out: the last step size)}
// iw = {method, mxstep, maxStiff, maxNonstiff, switchMax, stiffFirst,
//       istate (out), intervals solved with ros54p (out), intervals ending
//       on the stiff secondary (out)}
// cnt has RXG_NCNT counters.
void rxg_host_solve(double *y, double *tm, int *nt, double *dw, int *iw,
                    double *atol, double *rtol, int *cnt, double *p0,
                    double *p1, double *pl, double *ir, int *on, double *tt,
                    int *flags) {
  rxg_ctx c;
  rxg_opt o;
  rxg_auto a;
  rxg_host_ctx(&c, p0, p1, pl, ir, on, tt, flags);
  memcpy(o.atol, atol, sizeof(double)*RXG_NEQ1);
  memcpy(o.rtol, rtol, sizeof(double)*RXG_NEQ1);
  o.hmin = dw[0];
  o.hmax = dw[1];
  o.nonstifftol = dw[2];
  o.stifftol = dw[3];
  o.mxstep = iw[1];
  o.maxStiff = iw[2];
  o.maxNonstiff = iw[3];
  o.switchMax = iw[4];
  rxg_autoInit(&a, &o, iw[5]);
  double cur[RXG_NEQ1];
  double h = dw[4];
  int r = RXG_SUCCESS, sw = 0, nst = 0;
  for (int j = 0; j < RXG_NEQ; j++) cur[j] = y[j];
  for (int i = 1; i < *nt; i++) {
    int lu0 = cnt[RXG_CNT_LU];
    int r1 = rxg_solve(iw[0], tm[i - 1], tm[i], cur, &h, &c, &o, &a, cnt);
    if (r1 != RXG_SUCCESS) r = r1;
    if (cnt[RXG_CNT_LU] > lu0) sw++;
    nst += a.mode;
    for (int j = 0; j < RXG_NEQ; j++) y[i*RXG_NEQ + j] = cur[j];
    if (r1 != RXG_SUCCESS) break;
  }
  dw[4] = h;
  iw[6] = r;
  iw[7] = sw;
  iw[8] = nst;
}

#endif // RXG_HOST_H
