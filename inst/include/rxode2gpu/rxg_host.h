#ifndef RXG_HOST_H
#define RXG_HOST_H
// .C() entry points for a model's device code compiled as host C (included
// after the model's .cl file).
#include <string.h>
#include <stdlib.h>
#include "rxode2gpu/rxg_round.h"
#ifdef _OPENMP
#include <omp.h>
#endif

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
  c->hist = 0;
  c->preOk = 0;
}

#if RXG_HAS_DELAY
// Standalone evaluation has no history: delay() returns the state passed
#define RXG_HOST_NOHIST(c, t, y) \
  rxg_hist _nh;                  \
  rxg_histInit(&_nh, 0, 0, t, y); \
  (c).hist = &_nh
#else
#define RXG_HOST_NOHIST(c, t, y) (void)0
#endif

// Model sizes and the round layout (rxg_round.h).  RXG_HREC here uses the
// largest history degree, so it bounds every method's record size.
void rxg_host_dims(int *d) {
  d[0] = RXG_NEQ;
  d[1] = RXG_NLHS;
  d[2] = RXG_NPAR;
  d[3] = RXG_HAS_JAC;
  d[4] = RXG_HAS_DELAY;
  d[5] = RXG_HREC;
  d[6] = RXG_HHDR;
  d[7] = RXG_RD;
  d[8] = RXG_RI;
  d[9] = RXG_AD;
  d[10] = RXG_AI;
  d[11] = RXG_OD;
  d[12] = RXG_OI;
  d[13] = RXG_NCNT;
}

// One round on the host (the CPU backend of rxg_kernel()); requests run in
// parallel on nthreads OpenMP threads
void rxg_host_round(int *n, int *nthreads, double *y, double *rd, int *ri,
                    double *p0, double *p1, double *pl, double *ir, int *on,
                    double *ad, int *ai, double *hs, double *hrec, double *od,
                    int *oi) {
  int nn = *n;
#ifdef _OPENMP
  int nt = (*nthreads > 0) ? *nthreads : 1;
#pragma omp parallel for num_threads(nt) schedule(dynamic)
#endif
  for (int i = 0; i < nn; i++) {
    rxg_round1(i, y, rd, ri, p0, p1, pl, ir, on, ad, ai, hs, hrec, od, oi);
  }
}

// n subjects' timelines on the host (rxg_timeline1()), in parallel on
// nthreads OpenMP threads
void rxg_host_timeline(int *n, int *nthreads, int *off, double *y, double *rd,
                       int *ri, double *p0, double *p1, double *pl, double *ir,
                       int *on, double *ad, int *ai, double *hs, double *hrec,
                       double *od, int *oi) {
  int nn = *n;
#ifdef _OPENMP
  int nt = (*nthreads > 0) ? *nthreads : 1;
#pragma omp parallel for num_threads(nt) schedule(dynamic)
#endif
  for (int i = 0; i < nn; i++) {
    rxg_timeline1(i, off, y, rd, ri, p0, p1, pl, ir, on, ad, ai, hs, hrec, od, oi);
  }
}

void rxg_host_dydt(double *t, double *y, double *dy, double *p0, double *p1,
                   double *pl, double *ir, int *on, double *tt, int *flags) {
  rxg_ctx c;
  rxg_host_ctx(&c, p0, p1, pl, ir, on, tt, flags);
  RXG_HOST_NOHIST(c, t[0], y);
  rxg_dydt(t[0], y, dy, &c);
}

void rxg_host_jac(double *t, double *y, double *jac, double *p0, double *p1,
                  double *pl, double *ir, int *on, double *tt, int *flags) {
  rxg_ctx c;
  rxg_host_ctx(&c, p0, p1, pl, ir, on, tt, flags);
  RXG_HOST_NOHIST(c, t[0], y);
  for (int i = 0; i < RXG_NEQ*RXG_NEQ; i++) jac[i] = 0.0;
  rxg_jac(t[0], y, jac, RXG_NEQ, &c);
}

// Solve (I/hg - J) x = b with the integrators' LU (J row major); ok is 0
// when the matrix is singular
void rxg_host_lu(double *J, double *hg, double *b, int *ok) {
#if RXG_SOLVE_ROS
  double W[RXG_NEQ1*RXG_NEQ1], Jp[RXG_NEQ1*RXG_NEQ1];
  int piv[RXG_NEQ1];
  for (int i = 0; i < RXG_NEQ; i++) {
    for (int j = 0; j < RXG_NEQ; j++) Jp[i*RXG_NEQ1 + j] = J[i*RXG_NEQ + j];
  }
  ok[0] = _rxg_luW(Jp, hg[0], W, piv);
  if (ok[0]) _rxg_luSolve(W, piv, b);
#else
  ok[0] = 0;
#endif
}

// Solve over the times tm[0..nt-1] from y (nt x neq, row t), keeping the
// step sizes and the AutoSwitch state between intervals.
// dw = {hmin, hmax, nonstifftol, stifftol, h (out: the last step size)}
// iw = {method, mxstep, maxStiff, maxNonstiff, switchMax, stiffFirst,
//       istate (out), intervals solved with ros54p (out), intervals a
//       composite started on the stiff secondary (out), delay() history
//       records available, records used (out)}
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
#if RXG_HAS_DELAY
  rxg_hist hist;
  double *rec = (double*)malloc(sizeof(double)*RXG_HREC*(size_t)(iw[9] > 0 ? iw[9] : 1));
  if (rec == NULL) {
    iw[6] = RXG_ERR_HIST;
    return;
  }
  rxg_histInit(&hist, rec, iw[9], tm[0], y);
  c.hist = &hist;
#endif
  double cur[RXG_NEQ1];
  double h = dw[4];
  int r = RXG_SUCCESS, sw = 0, nst = 0;
  for (int j = 0; j < RXG_NEQ; j++) cur[j] = y[j];
  for (int i = 1; i < *nt; i++) {
    int lu0 = cnt[RXG_CNT_LU];
    nst += a.mode;
    int r1 = rxg_solve(iw[0], tm[i - 1], tm[i], cur, &h, &c, &o, &a, cnt);
    if (r1 != RXG_SUCCESS) r = r1;
    if (cnt[RXG_CNT_LU] > lu0) sw++;
    for (int j = 0; j < RXG_NEQ; j++) y[i*RXG_NEQ + j] = cur[j];
    if (r1 != RXG_SUCCESS) break;
  }
#if RXG_HAS_DELAY
  iw[10] = hist.n;
  free(rec);
#endif
  dw[4] = h;
  iw[6] = r;
  iw[7] = sw;
  iw[8] = nst;
}

#endif // RXG_HOST_H
