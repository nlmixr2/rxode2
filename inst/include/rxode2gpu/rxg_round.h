#ifndef RXG_ROUND_H
#define RXG_ROUND_H
// One round of batched integration: request i integrates subject
// ri[i*RXG_RI + RXG_RI_SUB] over [t0, t1].  Each subject's AutoSwitch state
// and delay() history stay in ad/ai/hs/hrec between rounds, so a round
// transfers only the requests.  At most one request per subject per round.
// Built into the OpenCL kernel rxg_kernel() and, as host C, into
// rxg_host_round() (rxg_host.h).
#include "rxode2gpu/rxg_stepper.h"

// rd: doubles per request
#define RXG_RD_T0 0
#define RXG_RD_T1 1
#define RXG_RD_H 2      // starting step size in, next step size out
#define RXG_RD_SHIFT 3
#define RXG_RD_TPK 4
#define RXG_RD_LAMBDA 5
#define RXG_RD_YJ 6
#define RXG_RD_HI 7
#define RXG_RD_LOW 8
#define RXG_RD_TOLF 9  // the subject's tolerance factor (ind->tolFactor)
#define RXG_RD 10
// ri: ints per request
#define RXG_RI_SUB 0
#define RXG_RI_COV 1
#define RXG_RI_NONMEM 2
#define RXG_RI_INIT 3   // RXG_INIT_* bits
#define RXG_RI_ISTATE 4 // out
#define RXG_RI_CNT 5    // out: RXG_NCNT counters of this request
#define RXG_RI (5 + RXG_NCNT)
#define RXG_INIT_AUTO 1 // reset the AutoSwitch state
#define RXG_INIT_HIST 2 // restart the delay() history at t0 from y
// ad/ai: the AutoSwitch state per subject; ai[RXG_AI_SEEN] is 1 once the
// subject has run a round (its states are valid), so a subject's first
// round starts fresh even without RXG_INIT_* bits
#define RXG_AD 4
#define RXG_AI_SEEN 7
#define RXG_AI 8
// hs: the delay() history header per subject (records in hrec)
#define RXG_HHDR (7 + RXG_NEQ1 + RXG_NLAG + RXG_NBP)
// od/oi: options shared by every request; a request's tolerances are
// min(tol*RXG_RD_TOLF, the cap), as iniSubject() sets them on the host
#define RXG_OD (2*RXG_NEQ1 + 5) // atol, rtol, hmin, hmax, nonstifftol, stifftol, cap
#define RXG_OI_METHOD 0
#define RXG_OI_MXSTEP 1
#define RXG_OI_MAXSTIFF 2
#define RXG_OI_MAXNONSTIFF 3
#define RXG_OI_SWITCHMAX 4
#define RXG_OI_STIFFFIRST 5
#define RXG_OI_HCAP 6   // history records per subject
#define RXG_OI 7

RXG_FN void _rxg_autoLoad(rxg_auto *a, RXG_GLOBAL const double *d,
                          RXG_GLOBAL const int *k) {
  a->h0 = d[0];
  a->h1 = d[1];
  a->st.tol = d[2];
  a->st.tStop = d[3];
  a->mode = k[0];
  a->count = k[1];
  a->since = k[2];
  a->backoff = k[3];
  a->switched = k[4];
  a->st.iasti = k[5];
  a->st.nonsti = k[6];
}

RXG_FN void _rxg_autoSave(const rxg_auto *a, RXG_GLOBAL double *d,
                          RXG_GLOBAL int *k) {
  d[0] = a->h0;
  d[1] = a->h1;
  d[2] = a->st.tol;
  d[3] = a->st.tStop;
  k[0] = a->mode;
  k[1] = a->count;
  k[2] = a->since;
  k[3] = a->backoff;
  k[4] = a->switched;
  k[5] = a->st.iasti;
  k[6] = a->st.nonsti;
  k[RXG_AI_SEEN] = 1;
}

#if RXG_HAS_DELAY
RXG_FN void _rxg_histLoad(rxg_hist *h, RXG_GLOBAL const double *d,
                          RXG_GLOBAL double *rec) {
  h->rec = rec;
  h->n = (int)d[0];
  h->cap = (int)d[1];
  h->t0 = d[2];
  h->minT = d[3];
  h->nlag = (int)d[4];
  h->nbp = (int)d[5];
  h->bpDirty = (int)d[6];
  d += 7;
  for (int i = 0; i < RXG_NEQ1; i++) h->ini[i] = d[i];
  d += RXG_NEQ1;
  for (int i = 0; i < RXG_NLAG; i++) h->lag[i] = d[i];
  d += RXG_NLAG;
  for (int i = 0; i < RXG_NBP; i++) h->bp[i] = d[i];
}

RXG_FN void _rxg_histSave(const rxg_hist *h, RXG_GLOBAL double *d) {
  d[0] = (double)h->n;
  d[1] = (double)h->cap;
  d[2] = h->t0;
  d[3] = h->minT;
  d[4] = (double)h->nlag;
  d[5] = (double)h->nbp;
  d[6] = (double)h->bpDirty;
  d += 7;
  for (int i = 0; i < RXG_NEQ1; i++) d[i] = h->ini[i];
  d += RXG_NEQ1;
  for (int i = 0; i < RXG_NLAG; i++) d[i] = h->lag[i];
  d += RXG_NLAG;
  for (int i = 0; i < RXG_NBP; i++) d[i] = h->bp[i];
}
#endif

RXG_FN void _rxg_roundCtx(rxg_ctx *c, int i, RXG_GLOBAL const double *rd,
                          RXG_GLOBAL const int *ri,
                          RXG_GLOBAL const double *p0,
                          RXG_GLOBAL const double *p1,
                          RXG_GLOBAL const double *pl,
                          RXG_GLOBAL const double *ir,
                          RXG_GLOBAL const int *on) {
  size_t np = (size_t)i*RXG_NPAR1, nl = (size_t)i*RXG_NLHS1,
    ne = (size_t)i*RXG_NEQ1;
  for (int j = 0; j < RXG_NPAR1; j++) {
    c->p0[j] = p0[np + j];
    c->p1[j] = p1[np + j];
  }
  for (int j = 0; j < RXG_NLHS1; j++) c->pl[j] = pl[nl + j];
  for (int j = 0; j < RXG_NEQ1; j++) {
    c->ir[j] = ir[ne + j];
    c->on[j] = on[ne + j];
  }
  c->t0 = rd[RXG_RD_T0];
  c->t1 = rd[RXG_RD_T1];
  c->curShift = rd[RXG_RD_SHIFT];
  c->tpk = rd[RXG_RD_TPK];
  c->lambda = rd[RXG_RD_LAMBDA];
  c->yj = rd[RXG_RD_YJ];
  c->logitHi = rd[RXG_RD_HI];
  c->logitLow = rd[RXG_RD_LOW];
  c->covType = ri[RXG_RI_COV];
  c->nonmem = ri[RXG_RI_NONMEM];
  c->hist = 0;
  c->preOk = 0;
  // parameters equal at both ends are constant over the interval
  if (c->covType == 1) {
    int same = 1;
    for (int j = 0; j < RXG_NPAR; j++) {
      if (c->p0[j] != c->p1[j]) same = 0;
    }
    if (same) c->covType = 0;
  }
#if RXG_NPRE > 0
  if (c->covType == 0) {
    double y0[RXG_NEQ1];
    for (int j = 0; j < RXG_NEQ1; j++) y0[j] = 0.0;
    rxg_pre(c->t0, y0, y0, c, c->pre);
    c->preOk = 1;
  }
#endif
}

RXG_FN void _rxg_roundOpt(rxg_opt *o, RXG_GLOBAL const double *od,
                          RXG_GLOBAL const int *oi, double tolf) {
  double cap = od[2*RXG_NEQ1 + 4];
  for (int j = 0; j < RXG_NEQ1; j++) {
    double at = od[j]*tolf, rt = od[RXG_NEQ1 + j]*tolf;
    o->atol[j] = at < cap ? at : cap;
    o->rtol[j] = rt < cap ? rt : cap;
  }
  o->hmin = od[2*RXG_NEQ1];
  o->hmax = od[2*RXG_NEQ1 + 1];
  o->nonstifftol = od[2*RXG_NEQ1 + 2];
  o->stifftol = od[2*RXG_NEQ1 + 3];
  o->mxstep = oi[RXG_OI_MXSTEP];
  o->maxStiff = oi[RXG_OI_MAXSTIFF];
  o->maxNonstiff = oi[RXG_OI_MAXNONSTIFF];
  o->switchMax = oi[RXG_OI_SWITCHMAX];
}

// Request i of a round
RXG_FN void rxg_round1(int i, RXG_GLOBAL double *y, RXG_GLOBAL double *rd,
                       RXG_GLOBAL int *ri, RXG_GLOBAL const double *p0,
                       RXG_GLOBAL const double *p1,
                       RXG_GLOBAL const double *pl,
                       RXG_GLOBAL const double *ir, RXG_GLOBAL const int *on,
                       RXG_GLOBAL double *ad, RXG_GLOBAL int *ai,
                       RXG_GLOBAL double *hs, RXG_GLOBAL double *hrec,
                       RXG_GLOBAL const double *od,
                       RXG_GLOBAL const int *oi) {
  RXG_GLOBAL double *rdi = rd + (size_t)i*RXG_RD;
  RXG_GLOBAL int *rii = ri + (size_t)i*RXG_RI;
  RXG_GLOBAL double *yi = y + (size_t)i*RXG_NEQ1;
  int s = rii[RXG_RI_SUB], init = rii[RXG_RI_INIT];
  RXG_GLOBAL double *ads = ad + (size_t)s*RXG_AD;
  RXG_GLOBAL int *ais = ai + (size_t)s*RXG_AI;
  if (ais[RXG_AI_SEEN] != 1) init |= RXG_INIT_AUTO | RXG_INIT_HIST;
  rxg_ctx c;
  rxg_opt o;
  rxg_auto a;
  double yy[RXG_NEQ1];
  int cnt[RXG_NCNT];
  for (int j = 0; j < RXG_NCNT; j++) cnt[j] = 0;
  for (int j = 0; j < RXG_NEQ1; j++) yy[j] = yi[j];
  _rxg_roundOpt(&o, od, oi, rdi[RXG_RD_TOLF]);
  _rxg_roundCtx(&c, i, rdi, rii, p0, p1, pl, ir, on);
  if (init & RXG_INIT_AUTO) {
    rxg_autoInit(&a, &o, oi[RXG_OI_STIFFFIRST]);
  } else {
    _rxg_autoLoad(&a, ads, ais);
  }
#if RXG_HAS_DELAY
  rxg_hist hist;
  int cap = oi[RXG_OI_HCAP];
  RXG_GLOBAL double *hss = hs + (size_t)s*RXG_HHDR;
  RXG_GLOBAL double *rec = hrec + (size_t)s*(size_t)cap*RXG_HREC;
  if (init & RXG_INIT_HIST) {
    rxg_histInit(&hist, rec, cap, c.t0, yy);
  } else {
    _rxg_histLoad(&hist, hss, rec);
  }
  c.hist = &hist;
#endif
#ifdef RXG_METHOD
  int method = RXG_METHOD;
#else
  int method = oi[RXG_OI_METHOD];
#endif
  double h = rdi[RXG_RD_H];
  int st = rxg_solve(method, c.t0, c.t1, yy, &h, &c, &o, &a, cnt);
  for (int j = 0; j < RXG_NEQ1; j++) yi[j] = yy[j];
  rdi[RXG_RD_H] = h;
  rii[RXG_RI_ISTATE] = st;
  for (int j = 0; j < RXG_NCNT; j++) rii[RXG_RI_CNT + j] = cnt[j];
  _rxg_autoSave(&a, ads, ais);
#if RXG_HAS_DELAY
  _rxg_histSave(&hist, hss);
#endif
}

// A subject's whole timeline: rows off[i] to off[i+1] - 1, in order, each a
// request of rxg_round1() (row k's subject state is ri[RXG_RI_SUB], i here).
// The host recorded them with the subject's events and no integration, so the
// y of a row after the first is the jump its events add (a dose) to the state
// the previous row ended with (several doses at one time arrive summed), and
// an rd[RXG_RD_H] < 0 continues with the
// step size the previous row ended with.  A row with ri[RXG_RI_SUB] < 0 ends
// the timeline early (rows reserved but not used), as does a failed row.
RXG_FN void rxg_timeline1(int i, RXG_GLOBAL const int *off,
                          RXG_GLOBAL double *y, RXG_GLOBAL double *rd,
                          RXG_GLOBAL int *ri, RXG_GLOBAL const double *p0,
                          RXG_GLOBAL const double *p1,
                          RXG_GLOBAL const double *pl,
                          RXG_GLOBAL const double *ir, RXG_GLOBAL const int *on,
                          RXG_GLOBAL double *ad, RXG_GLOBAL int *ai,
                          RXG_GLOBAL double *hs, RXG_GLOBAL double *hrec,
                          RXG_GLOBAL const double *od,
                          RXG_GLOBAL const int *oi) {
  int k0 = off[i], k1 = off[i + 1];
  for (int k = k0; k < k1; k++) {
    RXG_GLOBAL int *rik = ri + (size_t)k*RXG_RI;
    if (rik[RXG_RI_SUB] < 0) break;
    if (k > k0) {
      RXG_GLOBAL int *rip = ri + (size_t)(k - 1)*RXG_RI;
      if (rip[RXG_RI_ISTATE] != RXG_SUCCESS) break;
      RXG_GLOBAL double *yk = y + (size_t)k*RXG_NEQ1;
      RXG_GLOBAL const double *yp = y + (size_t)(k - 1)*RXG_NEQ1;
      for (int j = 0; j < RXG_NEQ1; j++) yk[j] += yp[j];
      RXG_GLOBAL double *rdk = rd + (size_t)k*RXG_RD;
      if (rdk[RXG_RD_H] < 0.0) rdk[RXG_RD_H] = rd[(size_t)(k - 1)*RXG_RD + RXG_RD_H];
    }
    rxg_round1(k, y, rd, ri, p0, p1, pl, ir, on, ad, ai, hs, hrec, od, oi);
  }
}

#ifdef RXG_OPENCL
// Argument order is fixed: src/gpu_opencl.c sets them by position.  tl 0: n
// requests of one round; tl 1: n subjects' timelines (rows from off).
__kernel void rxg_kernel(__global double *y, const int n, __global double *rd,
                         __global int *ri, __global const double *p0,
                         __global const double *p1,
                         __global const double *pl,
                         __global const double *ir, __global const int *on,
                         __global double *ad, __global int *ai,
                         __global double *hs, __global double *hrec,
                         __global const double *od, __global const int *oi,
                         __global const int *off, const int tl) {
  int i = (int)get_global_id(0);
  if (i >= n) return;
  if (tl) {
    rxg_timeline1(i, off, y, rd, ri, p0, p1, pl, ir, on, ad, ai, hs, hrec, od, oi);
  } else {
    rxg_round1(i, y, rd, ri, p0, p1, pl, ir, on, ad, ai, hs, hrec, od, oi);
  }
}
#endif

#endif // RXG_ROUND_H
