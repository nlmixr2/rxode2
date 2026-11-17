#ifndef RXG_HOST_H
#define RXG_HOST_H
// .C() entry points for a model's device code compiled as host C (included
// after the model's .cl file).
#include <string.h>

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

#endif // RXG_HOST_H
