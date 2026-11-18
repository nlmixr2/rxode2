#ifdef IN_PAR_SOLVE
// Batched solving with the device integrators (methods gpuRkt54, gpuVern76,
// gpuRos54p and the device AutoSwitch composites, codes 110-114).
//
// Events stay on the host.  Each subject runs rxode2's per-subject loop
// (ind_euler_0()'s, with the same helpers) as a resumable state machine that
// stops whenever it needs y integrated over [xp, xout].  A round integrates
// every waiting subject at once: an OpenCL kernel launch (src/gpu_opencl.c)
// or, for the CPU backend, the same round built as host C (rxg_host_round()
// of the model's host library).  Then the subjects resume.
//
// Up to op->cores subjects are in flight, one per "slot".  A slot owns the
// per-thread scratch of _setIndPointersByThread() (rx_get_thread() returns
// the slot through setRxThreadId()), so a subject keeps its infusion rates,
// pending doses and random-number stream while it waits; op->cores is the
// batch size during this solve (R sets it) and the OpenMP team is
// rxGpuSolveCfg.threads.  Steady state integrates synchronously on the host
// with the same stepper (gpu_solveWith1Pt()).
#include "gpu_opencl.h"

extern "C" void setRxThreadId(int id);
extern "C" void _update_par_ptr(double t, unsigned int id, rx_solve *rx, int idx);
extern "C" double _rxPkTime(double t, unsigned int id, rx_solve *rx);

// rxg_host_round() of the model's host library (rxode2gpu/rxg_host.h)
typedef void (*rxGpuHostRound_t)(int *n, int *nthreads, double *y, double *rd,
                                 int *ri, double *p0, double *p1, double *pl,
                                 double *ir, int *on, double *ad, int *ai,
                                 double *hs, double *hrec, double *od, int *oi);

// The request layout of rxode2gpu/rxg_round.h
#define RXGS_RD_T0 0
#define RXGS_RD_T1 1
#define RXGS_RD_H 2
#define RXGS_RD_SHIFT 3
#define RXGS_RD_TPK 4
#define RXGS_RD_LAMBDA 5
#define RXGS_RD_YJ 6
#define RXGS_RD_HI 7
#define RXGS_RD_LOW 8
#define RXGS_RI_SUB 0
#define RXGS_RI_COV 1
#define RXGS_RI_NONMEM 2
#define RXGS_RI_INIT 3
#define RXGS_RI_ISTATE 4
#define RXGS_INIT_ALL 3

typedef struct rxGpuSolveCfg_t {
  int on;                  // a GPU solve is set up
  int threads;             // OpenMP team of the host work
  rxGpuHostRound_t round;  // host build of the round
  rxGpuCl *cl;             // OpenCL run (NULL: CPU backend)
  int dims[RXG_D_N];       // layout (dims[RXG_D_NSUB] = slots)
  std::vector<double> od;
  std::vector<int> oi;
} rxGpuSolveCfg_t;

static rxGpuSolveCfg_t rxGpuSolveCfg = {0, 1, NULL, NULL, {0}, {}, {}};

// error texts of the device return codes (postSolve() indexes -istate-1)
static const char *rxGpuErr[] = {
  "the device integrator reached mxstep",
  "the device integrator step size fell below its minimum",
  "the device integrator is not available for this model",
  "stiffness detected",
  "the delay() history of the device integrator is full"
};

// Per-subject (or per-slot) integrator state for the host build
struct rxGpuHostState {
  std::vector<double> ad, hs, hrec;
  std::vector<int> ai;
  void alloc(int n) {
    const int *d = rxGpuSolveCfg.dims;
    ad.assign((size_t)n*d[RXG_D_AD], 0.0);
    ai.assign((size_t)n*d[RXG_D_AI], 0);
    hs.assign((size_t)n*d[RXG_D_HHDR], 0.0);
    hrec.assign(d[RXG_D_DELAY] ? (size_t)n*d[RXG_D_HCAP]*d[RXG_D_HREC] : 1, 0.0);
  }
};

// Request buffers of n rows
struct rxGpuReq {
  std::vector<double> y, rd, p0, p1, pl, ir;
  std::vector<int> ri, on;
  void alloc(int n) {
    const int *d = rxGpuSolveCfg.dims;
    y.assign((size_t)n*d[RXG_D_NEQ1], 0.0);
    rd.assign((size_t)n*d[RXG_D_RD], 0.0);
    ri.assign((size_t)n*d[RXG_D_RI], 0);
    p0.assign((size_t)n*d[RXG_D_NPAR1], 0.0);
    p1.assign((size_t)n*d[RXG_D_NPAR1], 0.0);
    pl.assign((size_t)n*d[RXG_D_NLHS1], 0.0);
    ir.assign((size_t)n*d[RXG_D_NEQ1], 0.0);
    on.assign((size_t)n*d[RXG_D_NEQ1], 1);
  }
};

// Fill request row r: subject state sub integrates yp from xp to xout
static void rxGpuFillReq(rxGpuReq &q, int r, int sub, int init, double h,
                         rx_solve *rx, rx_solving_options *op,
                         rx_solving_options_ind *ind, const double *yp,
                         double xp, double xout) {
  const int *d = rxGpuSolveCfg.dims;
  // the device layout bounds every row
  int neq = op->neq, npar = rx->npars, nlhs = op->nlhs;
  if (neq > d[RXG_D_NEQ1]) neq = d[RXG_D_NEQ1];
  if (npar > d[RXG_D_NPAR1]) npar = d[RXG_D_NPAR1];
  if (nlhs > d[RXG_D_NLHS1]) nlhs = d[RXG_D_NLHS1];
  double *y = &q.y[(size_t)r*d[RXG_D_NEQ1]];
  for (int j = 0; j < neq; j++) y[j] = yp[j];
  double *ir = &q.ir[(size_t)r*d[RXG_D_NEQ1]];
  int *on = &q.on[(size_t)r*d[RXG_D_NEQ1]];
  for (int j = 0; j < neq; j++) {
    ir[j] = ind->InfusionRate ? ind->InfusionRate[j] : 0.0;
    on[j] = ind->on ? ind->on[j] : 1;
  }
  double *pl = &q.pl[(size_t)r*d[RXG_D_NLHS1]];
  for (int j = 0; j < nlhs; j++) pl[j] = ind->lhs ? ind->lhs[j] : NA_REAL;
  // parameters and covariates a quarter and three quarters of the way in
  // (inside the interval, away from the records at its ends, which may be
  // doses carrying other covariates): different values mean linear
  // interpolation, extended to both ends (covType 1); equal values are
  // constant (locf, nocb, midpoint, or no change)
  double *p0 = &q.p0[(size_t)r*d[RXG_D_NPAR1]];
  double *p1 = &q.p1[(size_t)r*d[RXG_D_NPAR1]];
  double dt = xout - xp;
  _update_par_ptr(xp + 0.25*dt, ind->id, rx, ind->idx);
  for (int j = 0; j < npar; j++) p0[j] = ind->par_ptr[j];
  _update_par_ptr(xp + 0.75*dt, ind->id, rx, ind->idx);
  for (int j = 0; j < npar; j++) {
    double a = p0[j], b = ind->par_ptr[j];
    if (a != b) {
      // the line through (1/4, a) and (3/4, b) at 0 and 1
      p0[j] = 1.5*a - 0.5*b;
      p1[j] = 1.5*b - 0.5*a;
    } else {
      p1[j] = a;
    }
  }
  int covType = 1;
  double *rd = &q.rd[(size_t)r*d[RXG_D_RD]];
  rd[RXGS_RD_T0] = xp;
  rd[RXGS_RD_T1] = xout;
  rd[RXGS_RD_H] = h;
  rd[RXGS_RD_SHIFT] = ind->curShift;
  rd[RXGS_RD_TPK] = _rxPkTime(0.5*(xp + xout) + ind->curShift, ind->id, rx);
  rd[RXGS_RD_LAMBDA] = ind->lambda;
  rd[RXGS_RD_YJ] = ind->yj;
  rd[RXGS_RD_HI] = ind->logitHi;
  rd[RXGS_RD_LOW] = ind->logitLow;
  int *ri = &q.ri[(size_t)r*d[RXG_D_RI]];
  ri[RXGS_RI_SUB] = sub;
  ri[RXGS_RI_COV] = covType;
  ri[RXGS_RI_NONMEM] = rx->nonmem;
  ri[RXGS_RI_INIT] = init;
}

// One request integrated now on the host (steady state, ind_solve())
static int rxGpuHostOne(rxGpuReq &q, rxGpuHostState &st, double *yp,
                        int neq, double *h) {
  const int *d = rxGpuSolveCfg.dims;
  int n = 1, nt = 1;
  rxGpuSolveCfg.round(&n, &nt, q.y.data(), q.rd.data(), q.ri.data(),
                      q.p0.data(), q.p1.data(), q.pl.data(), q.ir.data(),
                      q.on.data(), st.ad.data(), st.ai.data(), st.hs.data(),
                      st.hrec.data(), rxGpuSolveCfg.od.data(),
                      rxGpuSolveCfg.oi.data());
  if (neq > d[RXG_D_NEQ1]) neq = d[RXG_D_NEQ1];
  for (int j = 0; j < neq; j++) yp[j] = q.y[j];
  if (h != NULL) *h = q.rd[RXGS_RD_H];
  return q.ri[RXGS_RI_ISTATE];
}

// _rxSolveOneInterval() of a GPU method: steady state and other synchronous
// integrations, with a fresh integrator state
extern "C" void gpu_solveWith1Pt(int *neq, double *yp, double *xp, double xout,
                                 int *istate, rx_solving_options *op,
                                 rx_solving_options_ind *ind) {
  if (!rxGpuSolveCfg.on || rxGpuSolveCfg.round == NULL) {
    *istate = -3;
    if (ind->rc[0] == 0) ind->rc[0] = -2019;
    return;
  }
  rx_solve *rx = getRxSolve_();
  if (op->neq > 0) {
    rxGpuReq q;
    q.alloc(1);
    rxGpuHostState st;
    st.alloc(1);
    rxGpuFillReq(q, 0, 0, RXGS_INIT_ALL, 0.0, rx, op, ind, yp, *xp, xout);
    int r = rxGpuHostOne(q, st, yp, op->neq, NULL);
    if (r != 2) {
      *istate = r;
      if (ind->rc[0] == 0) ind->rc[0] = -2019;
      return;
    }
  }
  (void)neq;
  *xp = xout;
  *istate = 1;
}

// A subject's place in rxode2's per-subject loop
enum {
  RXGS_INIT,     // start the subject
  RXGS_TOP,      // record i
  RXGS_EXTRA,    // the next extra (pushed or lagged) dose before xout
  RXGS_EXTRA_RES,// the integration up to that dose finished
  RXGS_EXTRA_EV, // apply that dose
  RXGS_MAIN,     // integrate up to record i
  RXGS_MAIN_RES, // that integration finished
  RXGS_POST,     // the events of record i
  RXGS_DONE
};

typedef struct rxGpuSub {
  int id;        // subject id
  int st;        // RXGS_*
  int i;         // record
  int istate;    // 1 restart, 2 continuing (device RXG_SUCCESS), < 0 failed
  int bad;
  int init;      // the next request starts the integrator state
  int req;       // posted a request (y from xp to t1)
  double xp, xout, t1, h;
  double *yp;
  int neq[2];
} rxGpuSub;

// Advance subject s until it posts a request (returns 1) or finishes
// (returns 0).  Runs on the subject's slot; q row r receives the request.
static int rxGpuAdvance(rx_solve *rx, rx_solving_options *op, rxGpuSub *s,
                        rxGpuReq &q, int r, int slot) {
  rx_solving_options_ind *ind = &(rx->subjects[s->id]);
  int *neq = s->neq;
  int neqOde = op->neq - op->numLin - op->numLinSens;
  for (;;) {
    switch (s->st) {
    case RXGS_INIT:
      neq[0] = op->neq;
      neq[1] = s->id;
      neq[0] = rxEffNeq(ind, op);
      s->bad = 0;
      s->istate = 1;
      s->init = RXGS_INIT_ALL;
      s->h = 0.0;
      s->i = 0;
      if (!iniSubject(neq[1], 0, ind, op, rx, update_inis)) {
        s->st = RXGS_DONE;
        return 0;
      }
      s->xp = getAllTimes(ind, 0);
      ind->solvedIdx = 0;
      s->st = RXGS_TOP;
      break;
    case RXGS_TOP: {
      if (s->i >= ind->n_all_times) {
        s->st = RXGS_DONE;
        return 0;
      }
      int i = s->i;
      ind->idx = i;
      ind->linSS = 0;
      if (ind->mainSorted == 0) {
        double *_rtime = ind->timeThread;
        for (int _j = i; _j < ind->n_all_times; _j++) {
          int _raw = ind->ix[_j];
          int _evid = getEvid(ind, _raw);
          if (_evid >= 10 && _evid <= 99) {
            _rtime[_raw] = ind->mtime[_evid - 10];
          } else if (!isObs(_evid)) {
            int _wh, _cmt, _wh100, _whI, _wh0;
            getWh(_evid, &_wh, &_cmt, &_wh100, &_whI, &_wh0);
            if (_whI == EVIDF_MODEL_RATE_OFF || _whI == EVIDF_MODEL_DUR_OFF) {
              _rtime[_raw] = getAllTimes(ind, _raw);
            }
          }
        }
        reSortMainTimeline(ind, i);
        ind->mainSorted = 1;
      }
      _growSolveIfNeeded(ind, op, i, 1);
      s->yp = getSolve(i);
      s->xout = ind->timeThread[ind->ix[i]];
      if (getEvid(ind, ind->ix[i]) != 3 && !isSameTime(s->xout, s->xp)) {
        if (ind->err) {
          ind->rc[0] = -1000;
          badSolveExit(s->i);
          s->bad = 1;
          s->st = RXGS_POST;
        } else {
          s->st = RXGS_EXTRA;
        }
      } else {
        s->st = RXGS_POST;
      }
      break;
    }
    case RXGS_EXTRA:
      if (!s->bad &&
          handleExtraDose(neq, ind->BadDose, ind->InfusionRate, ind->dose,
                          s->yp, s->xout, s->xp, ind->id, &(s->i),
                          ind->n_all_times, &(s->istate), op, ind,
                          update_inis, NULL)) {
        if (!isSameTime(ind->extraDoseNewXout, s->xp)) {
          double xo = ind->extraDoseNewXout;
          preSolve(op, ind, s->xp, xo, s->yp);
          s->st = RXGS_EXTRA_RES;
          if (neqOde > 0) {
            rxGpuFillReq(q, r, slot, s->init, (s->istate == 1) ? 0.0 : s->h,
                         rx, op, ind, s->yp, s->xp, xo);
            s->init = 0;
            s->t1 = xo;
            return 1;
          }
        } else {
          s->st = RXGS_EXTRA_EV;
        }
      } else {
        s->st = RXGS_MAIN;
      }
      break;
    case RXGS_EXTRA_RES: {
      copyLinCmt(neq, ind, op, s->yp);
      postSolve(neq, &(s->istate), ind->rc, &(s->i), s->yp, rxGpuErr, 5, true,
                ind, op, rx);
      if (*(ind->rc) < 0) s->bad = 1;
      s->xp = ind->extraDoseNewXout;
      s->st = RXGS_EXTRA_EV;
      break;
    }
    case RXGS_EXTRA_EV:
      if (!s->bad) {
        int idx = ind->idx;
        int ixds = ind->ixds;
        int trueIdx = ind->extraDoseTimeIdx[ind->idxExtra];
        ind->idx = -1 - trueIdx;
        handle_evid(ind->extraDoseEvid[trueIdx], neq[0], ind->BadDose,
                    ind->InfusionRate, ind->dose, s->yp, s->xout, neq[1], ind);
        s->istate = 1;
        ind->ixds = ixds;
        ind->idx = idx;
        ind->idxExtra++;
        s->xp = ind->extraDoseNewXout;
      }
      s->st = RXGS_EXTRA;
      break;
    case RXGS_MAIN:
      if (!s->bad && !isSameTime(s->xout, s->xp)) {
        preSolve(op, ind, s->xp, s->xout, s->yp);
        s->st = RXGS_MAIN_RES;
        if (neqOde > 0) {
          rxGpuFillReq(q, r, slot, s->init, (s->istate == 1) ? 0.0 : s->h,
                       rx, op, ind, s->yp, s->xp, s->xout);
          s->init = 0;
          s->t1 = s->xout;
          return 1;
        }
      } else {
        s->xp = s->xout;
        s->st = RXGS_POST;
      }
      break;
    case RXGS_MAIN_RES:
      copyLinCmt(neq, ind, op, s->yp);
      postSolve(neq, &(s->istate), ind->rc, &(s->i), s->yp, rxGpuErr, 5, true,
                ind, op, rx);
      if (*(ind->rc) < 0) s->bad = 1;
      s->xp = s->xout;
      s->st = RXGS_POST;
      break;
    case RXGS_POST: {
      if (!s->bad) {
        ind->idx = s->i;
        if (getEvid(ind, ind->ix[s->i]) == 3) {
          handleEvid3(ind, op, rx, neq, &(s->xp), &(s->xout), s->yp,
                      &(s->istate), update_inis);
        } else if (handleEvid1(&(s->i), rx, neq, s->yp, &(s->xout))) {
          handleSS(neq, ind->BadDose, ind->InfusionRate, ind->dose, s->yp,
                   s->xout, s->xp, ind->id, &(s->i), ind->n_all_times,
                   &(s->istate), op, ind, update_inis, NULL);
          if (ind->wh0 == EVID0_OFF) {
            ind->solve[ind->cmt] = op->inits[ind->cmt];
          }
          if (rx->istateReset) s->istate = 1;
          s->xp = s->xout;
        }
        int _mtime_requeued = 0;
        if (rx->nMtime > 0) {
          if (recomputeMtimeIfNeeded(rx, ind, s->yp, s->i, s->xout)) {
            ind->mainSorted = 0;
            _mtime_requeued = 1;
          }
        }
        if (rx->needSort & needSortAlag) {
          if (refreshLagTimesIfNeeded(rx, ind, s->yp, s->i + 1, s->xout)) {
            ind->mainSorted = 0;
          }
        }
        updateSolve(ind, op, neq, s->xout, s->i, ind->n_all_times);
        if (_mtime_requeued) s->i--;
      }
      rxNewindAfterRecord(ind, s->i);
      ind->solvedIdx = s->i;
      s->i++;
      s->st = RXGS_TOP;
      break;
    }
    default:
      return 0;
    }
  }
}

// Apply a request's result to its subject
static void rxGpuResult(rxGpuSub *s, rxGpuReq &q, int r, int neq) {
  const int *d = rxGpuSolveCfg.dims;
  if (neq > d[RXG_D_NEQ1]) neq = d[RXG_D_NEQ1];
  const double *y = &q.y[(size_t)r*d[RXG_D_NEQ1]];
  for (int j = 0; j < neq; j++) s->yp[j] = y[j];
  s->h = q.rd[(size_t)r*d[RXG_D_RD] + RXGS_RD_H];
  int st = q.ri[(size_t)r*d[RXG_D_RI] + RXGS_RI_ISTATE];
  s->istate = (st == 2) ? 2 : st;
}

// One subject solved alone on the host (ind_solve(), eg nlmixr2's
// per-subject solves)
extern "C" void ind_gpu(rx_solve *rx, int solveid, t_dydt c_dydt,
                        t_update_inis u_inis) {
  (void)c_dydt; (void)u_inis;
  rx_solving_options *op = rx->op;
  if (!rxGpuSolveCfg.on || rxGpuSolveCfg.round == NULL) {
    // may run on a team thread: report through the subject, never R
    rx_solving_options_ind *ind = &(rx->subjects[solveid]);
    ind->rc[0] = -2019;
    return;
  }
  rxGpuReq q;
  q.alloc(1);
  rxGpuHostState st;
  st.alloc(1);
  rxGpuSub s;
  s.id = solveid;
  s.st = RXGS_INIT;
  while (rxGpuAdvance(rx, op, &s, q, 0, 0)) {
    rxGpuHostOne(q, st, q.y.data(), op->neq, NULL);
    rxGpuResult(&s, q, 0, op->neq);
  }
}

// All subjects in batched rounds
extern "C" void par_gpu(rx_solve *rx) {
  rx_solving_options *op = rx->op;
  if (!rxGpuSolveCfg.on || rxGpuSolveCfg.round == NULL) {
    (Rf_errorcall)(R_NilValue, _("the GPU solver is not set up"));
  }
  int nslot = op->cores;
  if (rxGpuSolveCfg.cl != NULL && rxGpuSolveCfg.cl->dims[RXG_D_NSUB] < nslot) {
    nslot = rxGpuSolveCfg.cl->dims[RXG_D_NSUB];
  }
  if (rxGpuSolveCfg.dims[RXG_D_NSUB] < nslot) nslot = rxGpuSolveCfg.dims[RXG_D_NSUB];
  if (nslot < 1) nslot = 1;
  uint32_t nsub = rx->nsub, nsim = rx->nsim;
  int nsolve = (int)(nsim * nsub);
  if (nslot > nsolve) nslot = nsolve;
  int threads = rxGpuSolveCfg.threads > 0 ? rxGpuSolveCfg.threads : 1;
  uint32_t seed0 = getRxSeed1(op->cores);
  std::vector<rxGpuSub> sub((size_t)(nslot > 0 ? nslot : 1));
  rxGpuReq slotReq, roundReq;
  slotReq.alloc(nslot > 0 ? nslot : 1);
  roundReq.alloc(nslot > 0 ? nslot : 1);
  rxGpuHostState hst;
  if (rxGpuSolveCfg.cl == NULL) hst.alloc(nslot > 0 ? nslot : 1);
  std::vector<int> active((size_t)(nslot > 0 ? nslot : 1), 0);
  std::vector<int> map((size_t)(nslot > 0 ? nslot : 1), 0);
  int next = 0;
  for (int k = 0; k < nslot; k++) {
    sub[k].id = rx->ordId[next++] - 1;
    sub[k].st = RXGS_INIT;
    sub[k].req = 0;
    active[k] = 1;
  }
  const int *d = rxGpuSolveCfg.dims;
  int nact = nslot;
  int fail = 0;
  while (nact > 0 && !fail) {
    // advance every subject (applying its last result) until it waits
#ifdef _OPENMP
#pragma omp parallel for num_threads(threads) schedule(dynamic)
#endif
    for (int k = 0; k < nslot; k++) {
      if (!active[k]) continue;
      setRxThreadId(k);
      rxGpuSub *s = &sub[k];
      for (;;) {
        if (s->st == RXGS_INIT) setSeedEng1(seed0 + s->id);
        if (rxGpuAdvance(rx, op, s, slotReq, k, k)) {
          s->req = 1;
          break;
        }
        // finished: the slot takes the next subject
        s->req = 0;
        int nx;
#ifdef _OPENMP
#pragma omp atomic capture
#endif
        nx = next++;
        if (nx >= nsolve) {
          active[k] = 0;
          break;
        }
        s->id = rx->ordId[nx] - 1;
        s->st = RXGS_INIT;
      }
      setRxThreadId(-1);
    }
    // gather the requests into one round
    int n = 0;
    nact = 0;
    for (int k = 0; k < nslot; k++) {
      if (!active[k]) continue;
      nact++;
      if (!sub[k].req) continue;
      map[n] = k;
      memcpy(&roundReq.y[(size_t)n*d[RXG_D_NEQ1]], &slotReq.y[(size_t)k*d[RXG_D_NEQ1]], sizeof(double)*d[RXG_D_NEQ1]);
      memcpy(&roundReq.rd[(size_t)n*d[RXG_D_RD]], &slotReq.rd[(size_t)k*d[RXG_D_RD]], sizeof(double)*d[RXG_D_RD]);
      memcpy(&roundReq.ri[(size_t)n*d[RXG_D_RI]], &slotReq.ri[(size_t)k*d[RXG_D_RI]], sizeof(int)*d[RXG_D_RI]);
      memcpy(&roundReq.p0[(size_t)n*d[RXG_D_NPAR1]], &slotReq.p0[(size_t)k*d[RXG_D_NPAR1]], sizeof(double)*d[RXG_D_NPAR1]);
      memcpy(&roundReq.p1[(size_t)n*d[RXG_D_NPAR1]], &slotReq.p1[(size_t)k*d[RXG_D_NPAR1]], sizeof(double)*d[RXG_D_NPAR1]);
      memcpy(&roundReq.pl[(size_t)n*d[RXG_D_NLHS1]], &slotReq.pl[(size_t)k*d[RXG_D_NLHS1]], sizeof(double)*d[RXG_D_NLHS1]);
      memcpy(&roundReq.ir[(size_t)n*d[RXG_D_NEQ1]], &slotReq.ir[(size_t)k*d[RXG_D_NEQ1]], sizeof(double)*d[RXG_D_NEQ1]);
      memcpy(&roundReq.on[(size_t)n*d[RXG_D_NEQ1]], &slotReq.on[(size_t)k*d[RXG_D_NEQ1]], sizeof(int)*d[RXG_D_NEQ1]);
      n++;
    }
    if (n == 0) continue;
    if (rxGpuSolveCfg.cl != NULL) {
      if (rxGpuClRound(rxGpuSolveCfg.cl, n, roundReq.y.data(), roundReq.rd.data(),
                       roundReq.ri.data(), roundReq.p0.data(), roundReq.p1.data(),
                       roundReq.pl.data(), roundReq.ir.data(),
                       roundReq.on.data()) != 0) {
        fail = 1;
        break;
      }
    } else {
      rxGpuSolveCfg.round(&n, &threads, roundReq.y.data(), roundReq.rd.data(),
                          roundReq.ri.data(), roundReq.p0.data(), roundReq.p1.data(),
                          roundReq.pl.data(), roundReq.ir.data(), roundReq.on.data(),
                          hst.ad.data(), hst.ai.data(), hst.hs.data(),
                          hst.hrec.data(), rxGpuSolveCfg.od.data(),
                          rxGpuSolveCfg.oi.data());
    }
    for (int r = 0; r < n; r++) {
      int k = map[r];
      rxGpuResult(&sub[k], roundReq, r, op->neq);
      sub[k].req = 0;
    }
  }
  setRxSeedFinal(seed0 + (uint32_t)nsolve);
  // the rest of the solve (output) runs on the real team, never wider than
  // the pools op->cores sized
  if (threads < op->cores) op->cores = threads;
  if (fail) {
    (Rf_errorcall)(R_NilValue, _("an OpenCL round failed"));
  }
}

// R: install (or clear, with round NULL) the GPU solve of the next rxSolve()
//   round   external pointer to rxg_host_round() of the model's host library
//   cl      OpenCL run of .rxGpuRun() or NULL (CPU backend)
//   dims    the layout (RXG_D_*; dims[RXG_D_NSUB] slots)
//   od, oi  integrator options
//   threads OpenMP team
extern "C" SEXP _rxode2_rxGpuSolveSet(SEXP round, SEXP cl, SEXP dims, SEXP od,
                                      SEXP oi, SEXP threads) {
  if (Rf_isNull(round)) {
    rxGpuSolveCfg.on = 0;
    rxGpuSolveCfg.round = NULL;
    rxGpuSolveCfg.cl = NULL;
    return R_NilValue;
  }
  if (TYPEOF(round) != EXTPTRSXP || R_ExternalPtrAddrFn(round) == NULL) {
    (Rf_errorcall)(R_NilValue, _("invalid host round"));
  }
  if (Rf_length(dims) != RXG_D_N) (Rf_errorcall)(R_NilValue, _("invalid GPU layout"));
  rxGpuCl *g = NULL;
  if (!Rf_isNull(cl)) {
    if (TYPEOF(cl) != EXTPTRSXP || R_ExternalPtrTag(cl) != Rf_install("rxGpuCl") ||
        R_ExternalPtrAddr(cl) == NULL) {
      (Rf_errorcall)(R_NilValue, _("invalid OpenCL run"));
    }
    g = (rxGpuCl *)R_ExternalPtrAddr(cl);
  }
  rxGpuSolveCfg.round = (rxGpuHostRound_t)R_ExternalPtrAddrFn(round);
  rxGpuSolveCfg.cl = g;
  memcpy(rxGpuSolveCfg.dims, INTEGER(dims), sizeof(int)*RXG_D_N);
  rxGpuSolveCfg.od.assign(REAL(od), REAL(od) + Rf_length(od));
  rxGpuSolveCfg.oi.assign(INTEGER(oi), INTEGER(oi) + Rf_length(oi));
  rxGpuSolveCfg.threads = Rf_asInteger(threads);
  rxGpuSolveCfg.on = 1;
  return R_NilValue;
}

#endif // IN_PAR_SOLVE
