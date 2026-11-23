#ifdef IN_PAR_SOLVE
#include <chrono>
// Batched solving with the device integrators (methods gpuT54, gpuVern76,
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
#include "../inst/include/rxMemAlloc.h"

extern "C" void setRxThreadId(int id);
extern "C" void _update_par_ptr(double t, unsigned int id, rx_solve *rx, int idx);
extern "C" double _rxPkTime(double t, unsigned int id, rx_solve *rx);

// rxg_host_round() of the model's host library (rxode2gpu/rxg_host.h)
typedef void (*rxGpuHostRound_t)(int *n, int *nthreads, double *y, double *rd,
                                 int *ri, double *p0, double *p1, double *pl,
                                 double *ir, int *on, double *ad, int *ai,
                                 double *hs, double *hrec, double *od, int *oi);

// rxg_host_timeline() of the model's host library: n subjects' timelines
typedef void (*rxGpuHostTimeline_t)(int *n, int *nthreads, int *off, double *y,
                                    double *rd, int *ri, double *p0, double *p1,
                                    double *pl, double *ir, int *on, double *ad,
                                    int *ai, double *hs, double *hrec, double *od,
                                    int *oi);

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
#define RXGS_RD_TOLF 9
#define RXGS_RI_SUB 0
#define RXGS_RI_COV 1
#define RXGS_RI_NONMEM 2
#define RXGS_RI_INIT 3
#define RXGS_RI_ISTATE 4
#define RXGS_INIT_ALL 3

typedef struct rxGpuSolveCfg_t {
  int on;                  // a GPU solve is set up
  int armed;               // installed by R for the next solve setup
  std::string key;         // the model it is for (model_vars name)
  int gen;                 // bumped by every install: workspaces re-allocate
  int threads;             // OpenMP team of the host work
  int slots;               // thread slots the solve setup sized its pools for
  rxGpuHostRound_t round;  // host build of the round
  rxGpuHostTimeline_t timeline; // host build of the timelines (NULL: rounds only)
  rxGpuCl *cl;             // OpenCL run (NULL: CPU backend)
  int dims[RXG_D_N];       // layout (dims[RXG_D_NSUB] = slots)
  std::vector<double> od;
  std::vector<int> oi;
} rxGpuSolveCfg_t;

static rxGpuSolveCfg_t rxGpuSolveCfg = {0, 0, std::string(), 0, 1, 0, NULL, NULL, NULL, {0}, {}, {}};

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
    rxMemAssign(gpuHostState, ad, (size_t)n*d[RXG_D_AD], 0.0);
    rxMemAssign(gpuHostState, ai, (size_t)n*d[RXG_D_AI], 0);
    rxMemAssign(gpuHostState, hs, (size_t)n*d[RXG_D_HHDR], 0.0);
    rxMemAssign(gpuHostState, hrec, d[RXG_D_DELAY] ? (size_t)n*d[RXG_D_HCAP]*d[RXG_D_HREC] : 1, 0.0);
  }
};

// Request buffers of n rows
struct rxGpuReq {
  std::vector<double> y, rd, p0, p1, pl, ir;
  std::vector<int> ri, on;
  void alloc(int n) {
    const int *d = rxGpuSolveCfg.dims;
    rxMemAssign(gpuHostWork, y, (size_t)n*d[RXG_D_NEQ1], 0.0);
    rxMemAssign(gpuHostWork, rd, (size_t)n*d[RXG_D_RD], 0.0);
    rxMemAssign(gpuHostWork, ri, (size_t)n*d[RXG_D_RI], 0);
    rxMemAssign(gpuHostWork, p0, (size_t)n*d[RXG_D_NPAR1], 0.0);
    rxMemAssign(gpuHostWork, p1, (size_t)n*d[RXG_D_NPAR1], 0.0);
    rxMemAssign(gpuHostWork, pl, (size_t)n*d[RXG_D_NLHS1], 0.0);
    rxMemAssign(gpuHostWork, ir, (size_t)n*d[RXG_D_NEQ1], 0.0);
    rxMemAssign(gpuHostWork, on, (size_t)n*d[RXG_D_NEQ1], 1);
  }
  // the timeline rows of a launch (counted as gpuTimeline)
  void allocTimeline(int n) {
    const int *d = rxGpuSolveCfg.dims;
    rxMemAssign(gpuTimeline, y, (size_t)n*d[RXG_D_NEQ1], 0.0);
    rxMemAssign(gpuTimeline, rd, (size_t)n*d[RXG_D_RD], 0.0);
    rxMemAssign(gpuTimeline, ri, (size_t)n*d[RXG_D_RI], 0);
    rxMemAssign(gpuTimeline, p0, (size_t)n*d[RXG_D_NPAR1], 0.0);
    rxMemAssign(gpuTimeline, p1, (size_t)n*d[RXG_D_NPAR1], 0.0);
    rxMemAssign(gpuTimeline, pl, (size_t)n*d[RXG_D_NLHS1], 0.0);
    rxMemAssign(gpuTimeline, ir, (size_t)n*d[RXG_D_NEQ1], 0.0);
    rxMemAssign(gpuTimeline, on, (size_t)n*d[RXG_D_NEQ1], 1);
  }
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

// Buffers reused across solves: allocated for the installed layout
// (rxGpuSolveCfg.gen) and grown, never shrunk, with the number of rows.  A
// subject's first request carries the init bits, so state a reused slot holds
// from an earlier subject is never read.
struct rxGpuWork {
  int gen = -1;
  int n = 0;
  rxGpuReq q, q2;       // one request per slot, and the round's packed rows
  rxGpuReq qt;          // timeline rows (RXG_D_NROW of them)
  rxGpuHostState st;    // AutoSwitch and delay() state per slot (host round)
  std::vector<rxGpuSub> sub;
  std::vector<int> active, map, off;
  int nt = 0;
  void ensure(int rows, bool host, bool batch) {
    if (rows < 1) rows = 1;
    int trow = batch ? rxGpuSolveCfg.dims[RXG_D_NROW] : 0;
    if (gen == rxGpuSolveCfg.gen && rows <= n && trow <= nt &&
        (!host || !st.ad.empty())) return;
    q.alloc(rows);
    if (batch) {
      q2.alloc(rows);
      rxMemAssign(gpuHostWork, sub, (size_t)rows, rxGpuSub());
      rxMemAssign(gpuHostWork, active, (size_t)rows, 0);
      rxMemAssign(gpuHostWork, map, (size_t)rows, 0);
    }
    if (trow > 0) {
      qt.allocTimeline(trow);
      rxMemAssign(gpuTimeline, off, (size_t)rows + 1, 0);
    }
    if (host) st.alloc(rows);
    gen = rxGpuSolveCfg.gen;
    n = rows;
    nt = trow;
  }
  void release() {
    q = rxGpuReq();
    q2 = rxGpuReq();
    qt = rxGpuReq();
    std::vector<int>().swap(off);
    nt = 0;
    st = rxGpuHostState();
    std::vector<rxGpuSub>().swap(sub);
    std::vector<int>().swap(active);
    std::vector<int>().swap(map);
    gen = -1;
    n = 0;
  }
};

// par_gpu() runs on the main thread.  ind_solve() and steady state run one
// subject at a time on any thread: each thread slot has a workspace for each,
// sized at solve setup (rxGpuSolveWorkers()), since a steady-state leg runs
// inside an ind_solve() subject.  Everything is freed with the GPU setup and
// when the package unloads (rxGpuSolveFreeAll()).
static rxGpuWork rxGpuParWork;
static std::vector<rxGpuWork> rxGpuOneWork, rxGpuSsWork;

// One subject's workspace from pool: its thread slot's, or the caller's own
static rxGpuWork *rxGpuOneWorkFor(rx_solving_options *op, rxGpuWork &local,
                                  std::vector<rxGpuWork> &pool) {
  int t = rx_get_thread(op->cores);
  rxGpuWork *w = (t >= 0 && (size_t)t < pool.size()) ? &pool[(size_t)t] : &local;
  w->ensure(1, true, false);
  return w;
}

// Solve setup (single threaded): a workspace for each of n thread slots
extern "C" void rxGpuSolveWorkers(int n) {
  rxGpuSolveCfg.slots = n;
  if (n > 0 && (size_t)n > rxGpuOneWork.size()) rxMemResize(gpuSlotWork, rxGpuOneWork, (size_t)n);
  if (n > 0 && (size_t)n > rxGpuSsWork.size()) rxMemResize(gpuSlotWork, rxGpuSsWork, (size_t)n);
}

// Free every GPU solve buffer (clearing the setup, unloading the package)
extern "C" void rxGpuSolveFreeAll(void) {
  rxGpuParWork.release();
  std::vector<rxGpuWork>().swap(rxGpuOneWork);
  std::vector<rxGpuWork>().swap(rxGpuSsWork);
  std::vector<double>().swap(rxGpuSolveCfg.od);
  std::vector<int>().swap(rxGpuSolveCfg.oi);
  std::string().swap(rxGpuSolveCfg.key);
  rxGpuSolveCfg.on = 0;
  rxGpuSolveCfg.armed = 0;
  rxGpuSolveCfg.slots = 0;
  rxGpuSolveCfg.round = NULL;
  rxGpuSolveCfg.timeline = NULL;
  rxGpuSolveCfg.cl = NULL;
}

// Host bytes of a GPU solve with layout d (dims[RXG_D_NSUB] slots), for
// rxMemoryGpuComponents_(): out[0] the round's request buffers and subject
// table (gpuHostWork), out[1] the host build's integrator state per slot
// (gpuHostState; CPU backend only), out[2] the per-thread one-subject
// workspaces of ind_solve() and steady state (gpuSlotWork), out[3] the
// timeline rows and offsets (gpuTimeline; 0 without timelines)
extern "C" void rxGpuHostBytes(const int *d, int opencl, int threads, double *out) {
  const double rows = (double)d[RXG_D_NSUB];
  // y and ir (doubles) and on (ints) are state wide
  const double req = (2.0*d[RXG_D_NEQ1] + d[RXG_D_RD] + 2.0*d[RXG_D_NPAR1] +
                      d[RXG_D_NLHS1])*sizeof(double) +
    ((double)d[RXG_D_RI] + d[RXG_D_NEQ1])*sizeof(int);
  const double st = ((double)d[RXG_D_AD] + d[RXG_D_HHDR])*sizeof(double) +
    (double)d[RXG_D_AI]*sizeof(int) +
    (d[RXG_D_DELAY] ? (double)d[RXG_D_HCAP]*d[RXG_D_HREC]*sizeof(double) : 0.0);
  // q and q2, and the subject, active and map tables
  out[0] = rows*(2.0*req + sizeof(rxGpuSub) + 2.0*sizeof(int));
  out[1] = opencl ? 0.0 : rows*st;
  // rxGpuSolveWorkers(): two pools with a workspace per slot, each thread's
  // holding one row of requests and integrator state; and a timeline's
  // fallback (a subject solved alone) holds one more per thread while it runs
  out[2] = 2.0*(rows > threads ? rows : (double)threads)*sizeof(rxGpuWork) +
    (d[RXG_D_NROW] > 0 ? 3.0 : 2.0)*threads*(2.0*req + st);
  out[3] = d[RXG_D_NROW] > 0 ?
    (double)d[RXG_D_NROW]*req + (rows + 1.0)*sizeof(int) : 0.0;
}

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
  // yp holds the subject's effective states (setIndNeqOverride(), as
  // nlmixr2est's prediction solves use); the states past them are not fed
  // back into the leading ones, so they start at zero and are discarded
  int eff = rxEffNeq(ind, op);
  if (eff > neq) eff = neq;
  double *y = &q.y[(size_t)r*d[RXG_D_NEQ1]];
  for (int j = 0; j < eff; j++) y[j] = yp[j];
  for (int j = eff; j < neq; j++) y[j] = 0.0;
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
  // the subject's sticky tolerance factor (atolRtolFactorC_()), read every
  // request so a change between rounds applies to the next one
  rd[RXGS_RD_TOLF] = ind->tolFactor;
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
  if (rxGpuSolveCfg.dims[RXG_D_DELAY]) {
    // the delay() history would restart every interval
    rxSolveWarnPush(ind->id, "steady state is not supported with delay() by the GPU methods");
    *istate = -3;
    if (ind->rc[0] == 0) ind->rc[0] = -2019;
    return;
  }
  if (op->neq > 0) {
    rxGpuWork local;
    rxGpuWork *w = rxGpuOneWorkFor(op, local, rxGpuSsWork);
    rxGpuReq &q = w->q;
    rxGpuHostState &st = w->st;
    rxGpuFillReq(q, 0, 0, RXGS_INIT_ALL, 0.0, rx, op, ind, yp, *xp, xout);
    int r = rxGpuHostOne(q, st, yp, rxEffNeq(ind, op), NULL);
    if (r != 2) {
      *istate = r;
      if (ind->rc[0] == 0) ind->rc[0] = -2019;
      return;
    }
  }
  (void)neq; // neq[0] is rxEffNeq(ind, op)
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


// what an rxGpuAdvance() state step did
enum {
  RXGA_DONE = 0, // the subject finished
  RXGA_POSTED,   // it posted a request
  RXGA_NEXT      // it moved to another state
};

// post the request to integrate s from t0 to t1 in q row r, then wait in
// state next
static int rxGpuPost(rxGpuSub *s, int next, double t0, double t1, rxGpuReq &q,
                     int r, int slot, rx_solve *rx, rx_solving_options *op,
                     rx_solving_options_ind *ind) {
  rxGpuFillReq(q, r, slot, s->init, (s->istate == 1) ? 0.0 : s->h, rx, op, ind,
               s->yp, t0, t1);
  s->init = 0;
  s->t1 = t1;
  s->st = next;
  return RXGA_POSTED;
}

static int rxGpuStInit(rx_solve *rx, rx_solving_options *op, rxGpuSub *s,
                       rx_solving_options_ind *ind) {
  int *neq = s->neq;
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
    return RXGA_DONE;
  }
  s->xp = getAllTimes(ind, 0);
  ind->solvedIdx = 0;
  s->st = RXGS_TOP;
  return RXGA_NEXT;
}

// re-sort the rest of the timeline from record i after modeled times moved
static void rxGpuResort(rx_solving_options_ind *ind, int i) {
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

static int rxGpuStTop(rx_solving_options *op, rxGpuSub *s,
                      rx_solving_options_ind *ind) {
  if (s->i >= ind->n_all_times) {
    s->st = RXGS_DONE;
    return RXGA_DONE;
  }
  int i = s->i;
  ind->idx = i;
  ind->linSS = 0;
  if (ind->mainSorted == 0) rxGpuResort(ind, i);
  _growSolveIfNeeded(ind, op, i, 1);
  s->yp = getSolve(i);
  s->xout = ind->timeThread[ind->ix[i]];
  if (getEvid(ind, ind->ix[i]) == 3 || isSameTime(s->xout, s->xp)) {
    s->st = RXGS_POST;
  } else if (ind->err) {
    ind->rc[0] = -1000;
    badSolveExit(s->i);
    s->bad = 1;
    s->st = RXGS_POST;
  } else {
    s->st = RXGS_EXTRA;
  }
  return RXGA_NEXT;
}

static int rxGpuStExtra(rx_solve *rx, rx_solving_options *op, rxGpuSub *s,
                        rx_solving_options_ind *ind, int neqOde, rxGpuReq &q,
                        int r, int slot) {
  if (s->bad ||
      !handleExtraDose(s->neq, ind->BadDose, ind->InfusionRate, ind->dose,
                       s->yp, s->xout, s->xp, ind->id, &(s->i),
                       ind->n_all_times, &(s->istate), op, ind,
                       update_inis, NULL)) {
    s->st = RXGS_MAIN;
    return RXGA_NEXT;
  }
  if (isSameTime(ind->extraDoseNewXout, s->xp)) {
    s->st = RXGS_EXTRA_EV;
    return RXGA_NEXT;
  }
  double xo = ind->extraDoseNewXout;
  preSolve(op, ind, s->xp, xo, s->yp);
  s->st = RXGS_EXTRA_RES;
  if (neqOde > 0) return rxGpuPost(s, RXGS_EXTRA_RES, s->xp, xo, q, r, slot, rx, op, ind);
  return RXGA_NEXT;
}

// the integration s posted finished: rxode2's post-solve checks
static void rxGpuStResult(rx_solve *rx, rx_solving_options *op, rxGpuSub *s,
                          rx_solving_options_ind *ind) {
  copyLinCmt(s->neq, ind, op, s->yp);
  postSolve(s->neq, &(s->istate), ind->rc, &(s->i), s->yp, rxGpuErr, 5, true,
            ind, op, rx);
  if (*(ind->rc) < 0) s->bad = 1;
}

static void rxGpuStExtraEv(rxGpuSub *s, rx_solving_options_ind *ind) {
  if (!s->bad) {
    int idx = ind->idx;
    int ixds = ind->ixds;
    int trueIdx = ind->extraDoseTimeIdx[ind->idxExtra];
    ind->idx = -1 - trueIdx;
    handle_evid(ind->extraDoseEvid[trueIdx], s->neq[0], ind->BadDose,
                ind->InfusionRate, ind->dose, s->yp, s->xout, s->neq[1], ind);
    s->istate = 1;
    ind->ixds = ixds;
    ind->idx = idx;
    ind->idxExtra++;
    s->xp = ind->extraDoseNewXout;
  }
  s->st = RXGS_EXTRA;
}

static int rxGpuStMain(rx_solve *rx, rx_solving_options *op, rxGpuSub *s,
                       rx_solving_options_ind *ind, int neqOde, rxGpuReq &q,
                       int r, int slot) {
  if (s->bad || isSameTime(s->xout, s->xp)) {
    s->xp = s->xout;
    s->st = RXGS_POST;
    return RXGA_NEXT;
  }
  preSolve(op, ind, s->xp, s->xout, s->yp);
  s->st = RXGS_MAIN_RES;
  if (neqOde > 0) return rxGpuPost(s, RXGS_MAIN_RES, s->xp, s->xout, q, r, slot, rx, op, ind);
  return RXGA_NEXT;
}

// the dose or reset events of record s->i
static void rxGpuPostEvents(rx_solve *rx, rx_solving_options *op, rxGpuSub *s,
                            rx_solving_options_ind *ind) {
  int *neq = s->neq;
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
}

// modeled times and lags that moved with record s->i; returns 1 when the
// record is requeued (its mtime changed)
static int rxGpuPostRetime(rx_solve *rx, rxGpuSub *s,
                           rx_solving_options_ind *ind) {
  int requeued = 0;
  if (rx->nMtime > 0 && recomputeMtimeIfNeeded(rx, ind, s->yp, s->i, s->xout)) {
    ind->mainSorted = 0;
    requeued = 1;
  }
  if ((rx->needSort & needSortAlag) &&
      refreshLagTimesIfNeeded(rx, ind, s->yp, s->i + 1, s->xout)) {
    ind->mainSorted = 0;
  }
  return requeued;
}

static void rxGpuStPost(rx_solve *rx, rx_solving_options *op, rxGpuSub *s,
                        rx_solving_options_ind *ind) {
  if (!s->bad) {
    ind->idx = s->i;
    rxGpuPostEvents(rx, op, s, ind);
    int requeued = rxGpuPostRetime(rx, s, ind);
    updateSolve(ind, op, s->neq, s->xout, s->i, ind->n_all_times);
    if (requeued) s->i--;
  }
  rxNewindAfterRecord(ind, s->i);
  ind->solvedIdx = s->i;
  s->i++;
  s->st = RXGS_TOP;
}

// one state of subject s; an RXGA_* code
static int rxGpuStep(rx_solve *rx, rx_solving_options *op, rxGpuSub *s,
                     rx_solving_options_ind *ind, int neqOde, rxGpuReq &q,
                     int r, int slot) {
  switch (s->st) {
  case RXGS_INIT:
    return rxGpuStInit(rx, op, s, ind);
  case RXGS_TOP:
    return rxGpuStTop(op, s, ind);
  case RXGS_EXTRA:
    return rxGpuStExtra(rx, op, s, ind, neqOde, q, r, slot);
  case RXGS_EXTRA_RES:
    rxGpuStResult(rx, op, s, ind);
    s->xp = ind->extraDoseNewXout;
    s->st = RXGS_EXTRA_EV;
    return RXGA_NEXT;
  case RXGS_EXTRA_EV:
    rxGpuStExtraEv(s, ind);
    return RXGA_NEXT;
  case RXGS_MAIN:
    return rxGpuStMain(rx, op, s, ind, neqOde, q, r, slot);
  case RXGS_MAIN_RES:
    rxGpuStResult(rx, op, s, ind);
    s->xp = s->xout;
    s->st = RXGS_POST;
    return RXGA_NEXT;
  case RXGS_POST:
    rxGpuStPost(rx, op, s, ind);
    return RXGA_NEXT;
  default:
    return RXGA_DONE;
  }
}

// Advance subject s until it posts a request (returns 1) or finishes
// (returns 0).  Runs on the subject's slot; q row r receives the request.
static int rxGpuAdvance(rx_solve *rx, rx_solving_options *op, rxGpuSub *s,
                        rxGpuReq &q, int r, int slot) {
  rx_solving_options_ind *ind = &(rx->subjects[s->id]);
  int neqOde = op->neq - op->numLin - op->numLinSens;
  for (;;) {
    int st = rxGpuStep(rx, op, s, ind, neqOde, q, r, slot);
    if (st != RXGA_NEXT) return st == RXGA_POSTED;
  }
}

// Apply a request's result to its subject (its effective states, s->neq[0])
static void rxGpuResult(rxGpuSub *s, rxGpuReq &q, int r) {
  const int *d = rxGpuSolveCfg.dims;
  int neq = s->neq[0];
  if (neq > d[RXG_D_NEQ1]) neq = d[RXG_D_NEQ1];
  const double *y = &q.y[(size_t)r*d[RXG_D_NEQ1]];
  for (int j = 0; j < neq; j++) s->yp[j] = y[j];
  s->h = q.rd[(size_t)r*d[RXG_D_RD] + RXGS_RD_H];
  int st = q.ri[(size_t)r*d[RXG_D_RI] + RXGS_RI_ISTATE];
  s->istate = (st == 2) ? 2 : st;
}

// One subject solved alone on the host with workspace w (one row)
static void rxGpuSolveAlone(rx_solve *rx, int solveid, rxGpuWork *w) {
  rx_solving_options *op = rx->op;
  rxGpuReq &q = w->q;
  rxGpuHostState &st = w->st;
  rxGpuSub s;
  s.id = solveid;
  s.st = RXGS_INIT;
  while (rxGpuAdvance(rx, op, &s, q, 0, 0)) {
    rxGpuHostOne(q, st, q.y.data(), op->neq, NULL);
    rxGpuResult(&s, q, 0);
  }
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
  rxGpuWork local;
  rxGpuSolveAlone(rx, solveid, rxGpuOneWorkFor(op, local, rxGpuOneWork));
}

// Batched-solve counters for the benchmarks (.rxGpuSolveStats()): solves,
// rounds, requests, and seconds advancing subjects on the host and in rounds
static struct {
  double solves, rounds, requests, advance, round;
} rxGpuStats = {0, 0, 0, 0, 0};

extern "C" SEXP _rxode2_rxGpuSolveStats(SEXP reset) {
  // coerce before allocating: the result is not protected
  int doReset = Rf_asLogical(reset) == 1;
  SEXP ret = Rf_allocVector(REALSXP, 5);
  double *r = REAL(ret);
  r[0] = rxGpuStats.solves;
  r[1] = rxGpuStats.rounds;
  r[2] = rxGpuStats.requests;
  r[3] = rxGpuStats.advance;
  r[4] = rxGpuStats.round;
  if (doReset) rxGpuStats = {0, 0, 0, 0, 0};
  return ret;
}

// Copy request row sr of src into row dr of dst
static void rxGpuCopyRow(rxGpuReq &dst, int dr, const rxGpuReq &src, int sr) {
  const int *d = rxGpuSolveCfg.dims;
  size_t a = (size_t)dr, b = (size_t)sr;
  memcpy(&dst.y[a*d[RXG_D_NEQ1]], &src.y[b*d[RXG_D_NEQ1]], sizeof(double)*d[RXG_D_NEQ1]);
  memcpy(&dst.rd[a*d[RXG_D_RD]], &src.rd[b*d[RXG_D_RD]], sizeof(double)*d[RXG_D_RD]);
  memcpy(&dst.ri[a*d[RXG_D_RI]], &src.ri[b*d[RXG_D_RI]], sizeof(int)*d[RXG_D_RI]);
  memcpy(&dst.p0[a*d[RXG_D_NPAR1]], &src.p0[b*d[RXG_D_NPAR1]], sizeof(double)*d[RXG_D_NPAR1]);
  memcpy(&dst.p1[a*d[RXG_D_NPAR1]], &src.p1[b*d[RXG_D_NPAR1]], sizeof(double)*d[RXG_D_NPAR1]);
  memcpy(&dst.pl[a*d[RXG_D_NLHS1]], &src.pl[b*d[RXG_D_NLHS1]], sizeof(double)*d[RXG_D_NLHS1]);
  memcpy(&dst.ir[a*d[RXG_D_NEQ1]], &src.ir[b*d[RXG_D_NEQ1]], sizeof(double)*d[RXG_D_NEQ1]);
  memcpy(&dst.on[a*d[RXG_D_NEQ1]], &src.on[b*d[RXG_D_NEQ1]], sizeof(int)*d[RXG_D_NEQ1]);
}

// Can subject ind be solved as a timeline: do its events never depend on its
// states?  Observations, boluses and infusions with a fixed rate or duration
// (and their ends) only: no steady state, reset, replace, multiply, turning a
// compartment off, phantom doses or mtime().  The model was checked in R
// (.rxGpuTimelineModelOk()).
static int rxGpuTlSubjectOk(rx_solving_options_ind *ind) {
  for (int j = 0; j < ind->n_all_times; j++) {
    int evid = getEvid(ind, j);
    if (evid == 3) return 0;
    if (isObs(evid)) continue;
    if (evid >= 10 && evid <= 99) return 0;
    int wh, cmt, wh100, whI, wh0;
    getWh(evid, &wh, &cmt, &wh100, &whI, &wh0);
    if (wh0 != EVID0_REGULAR && wh0 != EVID0_RATEADJ) return 0;
    if (whI != EVIDF_NORMAL && whI != EVIDF_INF_RATE && whI != EVIDF_INF_DUR) return 0;
  }
  return 1;
}

// Every subject of the solve as a timeline, and a launch holding a full batch
// of nslot subjects: their records average at most the rows a slot has (with
// fewer subjects a launch would fill the device less than a round does)
static int rxGpuTlSolveOk(rx_solve *rx, int nsolve, int nslot, int nrow) {
  if (rxGpuSolveCfg.timeline == NULL || nrow < 1 || rx->nMtime > 0 ||
      (rx->needSort & needSortAlag) != 0) {
    return 0;
  }
  double tot = 0.0;
  for (int k = 0; k < nsolve; k++) {
    rx_solving_options_ind *ind = &(rx->subjects[rx->ordId[k] - 1]);
    if (ind->n_all_times > nrow || !rxGpuTlSubjectOk(ind)) return 0;
    tot += ind->n_all_times;
  }
  return tot <= (double)nsolve*((double)nrow/(double)nslot);
}

// All subjects as timelines: a launch integrates each subject of a chunk over
// its whole timeline (rxg_timeline1()).  A chunk's subjects are first run
// through their events without integrating (the states come back zero), which
// records each request as a row: its first row starts from the subject's
// initial states and every later row's y is what the events added since the
// previous request.  After the launch the subjects run again, taking the rows'
// results in order.  Returns 0, or 1 when an OpenCL launch failed.
static int rxGpuTimelines(rx_solve *rx, rx_solving_options *op, rxGpuWork &w,
                          int nslot, int nsolve, int threads, uint32_t seed0) {
  const int *d = rxGpuSolveCfg.dims;
  const int nrow = d[RXG_D_NROW];
  std::vector<rxGpuSub> &sub = w.sub;
  std::vector<int> &off = w.off, &used = w.map;
  rxGpuReq &scratch = w.q, &tl = w.qt;
  rxGpuHostState &hst = w.st;
  int next = 0;
  while (next < nsolve) {
    // the chunk: subjects while their records (a bound on their requests)
    // fit the rows
    auto _t0 = std::chrono::steady_clock::now();
    int m = 0, rows = 0;
    while (next < nsolve && m < nslot) {
      int id = rx->ordId[next] - 1;
      int b = rx->subjects[id].n_all_times;
      if (m > 0 && rows + b > nrow) break;
      sub[m].id = id;
      off[m] = rows;
      rows += b;
      m++;
      next++;
    }
    off[m] = rows;
    // record
#ifdef _OPENMP
#pragma omp parallel for num_threads(threads) schedule(dynamic)
#endif
    for (int k = 0; k < m; k++) {
      setRxThreadId(k);
      rxGpuSub *s = &sub[k];
      s->st = RXGS_INIT;
      setSeedEng1(seed0 + s->id);
      int row = off[k];
      used[k] = 0;
      while (rxGpuAdvance(rx, op, s, scratch, k, k)) {
        int restart = s->istate == 1;
        // the integration's result: zero states, so the next request's y is
        // what the events add
        int ne = s->neq[0] < d[RXG_D_NEQ1] ? s->neq[0] : d[RXG_D_NEQ1];
        for (int j = 0; j < ne; j++) s->yp[j] = 0.0;
        s->istate = 2;
        if (row >= off[k + 1]) {
          used[k] = -1; // more requests than records: the replay solves it alone
          continue;
        }
        rxGpuCopyRow(tl, row, scratch, k);
        tl.rd[(size_t)row*d[RXG_D_RD] + RXGS_RD_H] = restart ? 0.0 : -1.0;
        row++;
      }
      if (used[k] == 0) used[k] = row - off[k];
      for (int r = row; r < off[k + 1]; r++) tl.ri[(size_t)r*d[RXG_D_RI] + RXGS_RI_SUB] = -1;
      setRxThreadId(-1);
    }
    auto _t1 = std::chrono::steady_clock::now();
    rxGpuStats.advance += std::chrono::duration<double>(_t1 - _t0).count();
    // one launch for the chunk
    rxGpuStats.rounds++;
    for (int k = 0; k < m; k++) if (used[k] > 0) rxGpuStats.requests += used[k];
    if (rxGpuSolveCfg.cl != NULL) {
      if (rxGpuClTimeline(rxGpuSolveCfg.cl, m, rows, off.data(), tl.y.data(),
                          tl.rd.data(), tl.ri.data(), tl.p0.data(), tl.p1.data(),
                          tl.pl.data(), tl.ir.data(), tl.on.data()) != 0) {
        return 1;
      }
    } else {
      rxGpuSolveCfg.timeline(&m, &threads, off.data(), tl.y.data(), tl.rd.data(),
                             tl.ri.data(), tl.p0.data(), tl.p1.data(), tl.pl.data(),
                             tl.ir.data(), tl.on.data(), hst.ad.data(), hst.ai.data(),
                             hst.hs.data(), hst.hrec.data(), rxGpuSolveCfg.od.data(),
                             rxGpuSolveCfg.oi.data());
    }
    auto _t2 = std::chrono::steady_clock::now();
    rxGpuStats.round += std::chrono::duration<double>(_t2 - _t1).count();
    // replay: the same requests, answered by the rows
#ifdef _OPENMP
#pragma omp parallel for num_threads(threads) schedule(dynamic)
#endif
    for (int k = 0; k < m; k++) {
      setRxThreadId(k);
      rxGpuSub *s = &sub[k];
      int ok = used[k] >= 0;
      if (ok) {
        s->st = RXGS_INIT;
        setSeedEng1(seed0 + s->id);
        int row = off[k];
        const size_t rd = (size_t)d[RXG_D_RD];
        while (rxGpuAdvance(rx, op, s, scratch, k, k)) {
          if (row >= off[k] + used[k] ||
              scratch.rd[(size_t)k*rd + RXGS_RD_T0] != tl.rd[(size_t)row*rd + RXGS_RD_T0] ||
              scratch.rd[(size_t)k*rd + RXGS_RD_T1] != tl.rd[(size_t)row*rd + RXGS_RD_T1]) {
            ok = 0;
            break;
          }
          rxGpuResult(s, tl, row);
          row++;
        }
      }
      if (!ok) {
        // its requests differ from the recording: solve it alone on the host,
        // in a workspace of its own (the shared ones are per thread, and slot
        // k may share one with another slot)
        setSeedEng1(seed0 + s->id);
        rxGpuWork local;
        local.ensure(1, true, false);
        rxGpuSolveAlone(rx, s->id, &local);
      }
      setRxThreadId(-1);
    }
    rxGpuStats.advance += std::chrono::duration<double>(std::chrono::steady_clock::now() - _t2).count();
  }
  return 0;
}

// All subjects in batched rounds
// The rounds; returns 0, or 1 when an OpenCL round failed, which par_gpu()
// raises once the rounds have returned.  Its buffers are the reused
// rxGpuParWork, freed when the GPU setup is cleared.
static int par_gpu0(rx_solve *rx);

extern "C" void par_gpu(rx_solve *rx) {
  if (!rxGpuSolveCfg.on || rxGpuSolveCfg.round == NULL) {
    (Rf_errorcall)(R_NilValue, _("the GPU solver is not set up"));
  }
  if (par_gpu0(rx)) {
    (Rf_errorcall)(R_NilValue, _("an OpenCL round failed"));
  }
}

// the slots of a solve: those the setup sized the per-thread pools for (or
// op->cores), within the buffers and the solve count.  op->cores drops to the
// host team after each solve, and a caller re-solving without a new setup
// (nlmixr2est's SAEM, every iteration) would otherwise round only that many
// subjects at a time
static int rxGpuSlotCount(rx_solve *rx, int nsolve) {
  rx_solving_options *op = rx->op;
  int nslot = rxGpuSolveCfg.slots > op->cores ? rxGpuSolveCfg.slots : op->cores;
  if (rxGpuSolveCfg.cl != NULL && rxGpuSolveCfg.cl->dims[RXG_D_NSUB] < nslot) {
    nslot = rxGpuSolveCfg.cl->dims[RXG_D_NSUB];
  }
  if (rxGpuSolveCfg.dims[RXG_D_NSUB] < nslot) nslot = rxGpuSolveCfg.dims[RXG_D_NSUB];
  if (nslot < 1) nslot = 1;
  if (nslot > nsolve) nslot = nsolve;
  return nslot;
}

// advance the subject on slot k until it waits on a request, the slot taking
// the next subject (from *next) each time one finishes
static void rxGpuRunSlot(rx_solve *rx, rx_solving_options *op, int k,
                         rxGpuWork &w, uint32_t seed0, int *next, int nsolve) {
  rxGpuSub *s = &w.sub[k];
  for (;;) {
    if (s->st == RXGS_INIT) setSeedEng1(seed0 + s->id);
    if (rxGpuAdvance(rx, op, s, w.q, k, k)) {
      s->req = 1;
      return;
    }
    // finished: the slot takes the next subject
    s->req = 0;
    int nx;
#ifdef _OPENMP
#pragma omp atomic capture
#endif
    nx = (*next)++;
    if (nx >= nsolve) {
      w.active[k] = 0;
      return;
    }
    s->id = rx->ordId[nx] - 1;
    s->st = RXGS_INIT;
  }
}

static void rxGpuCopyRow(rxGpuReq &dst, int dr, const rxGpuReq &src, int sr) {
  const int *d = rxGpuSolveCfg.dims;
  size_t a = (size_t)dr, b = (size_t)sr;
  memcpy(&dst.y[a*d[RXG_D_NEQ1]], &src.y[b*d[RXG_D_NEQ1]], sizeof(double)*d[RXG_D_NEQ1]);
  memcpy(&dst.rd[a*d[RXG_D_RD]], &src.rd[b*d[RXG_D_RD]], sizeof(double)*d[RXG_D_RD]);
  memcpy(&dst.ri[a*d[RXG_D_RI]], &src.ri[b*d[RXG_D_RI]], sizeof(int)*d[RXG_D_RI]);
  memcpy(&dst.p0[a*d[RXG_D_NPAR1]], &src.p0[b*d[RXG_D_NPAR1]], sizeof(double)*d[RXG_D_NPAR1]);
  memcpy(&dst.p1[a*d[RXG_D_NPAR1]], &src.p1[b*d[RXG_D_NPAR1]], sizeof(double)*d[RXG_D_NPAR1]);
  memcpy(&dst.pl[a*d[RXG_D_NLHS1]], &src.pl[b*d[RXG_D_NLHS1]], sizeof(double)*d[RXG_D_NLHS1]);
  memcpy(&dst.ir[a*d[RXG_D_NEQ1]], &src.ir[b*d[RXG_D_NEQ1]], sizeof(double)*d[RXG_D_NEQ1]);
  memcpy(&dst.on[a*d[RXG_D_NEQ1]], &src.on[b*d[RXG_D_NEQ1]], sizeof(int)*d[RXG_D_NEQ1]);
}

// gather the slots' requests into the round's rows (w.map: row -> slot);
// *nact gets the active slots.  Returns the request count.
static int rxGpuGather(rxGpuWork &w, int nslot, int *nact) {
  int n = 0;
  *nact = 0;
  for (int k = 0; k < nslot; k++) {
    if (!w.active[k]) continue;
    (*nact)++;
    if (!w.sub[k].req) continue;
    w.map[n] = k;
    rxGpuCopyRow(w.q2, n, w.q, k);
    n++;
  }
  return n;
}

// integrate the round's n requests (OpenCL or the host build); 0, or 1 when
// an OpenCL round failed
static int rxGpuRunRound(rxGpuWork &w, int n, int threads) {
  rxGpuReq &q = w.q2;
  if (rxGpuSolveCfg.cl != NULL) {
    return rxGpuClRound(rxGpuSolveCfg.cl, n, q.y.data(), q.rd.data(),
                        q.ri.data(), q.p0.data(), q.p1.data(), q.pl.data(),
                        q.ir.data(), q.on.data()) != 0;
  }
  rxGpuHostState &hst = w.st;
  rxGpuSolveCfg.round(&n, &threads, q.y.data(), q.rd.data(), q.ri.data(),
                      q.p0.data(), q.p1.data(), q.pl.data(), q.ir.data(),
                      q.on.data(), hst.ad.data(), hst.ai.data(), hst.hs.data(),
                      hst.hrec.data(), rxGpuSolveCfg.od.data(),
                      rxGpuSolveCfg.oi.data());
  return 0;
}

static int par_gpu0(rx_solve *rx) {
  rx_solving_options *op = rx->op;
  uint32_t nsub = rx->nsub, nsim = rx->nsim;
  int nsolve = (int)(nsim * nsub);
  int nslot = rxGpuSlotCount(rx, nsolve);
  int threads = rxGpuSolveCfg.threads > 0 ? rxGpuSolveCfg.threads : 1;
  uint32_t seed0 = getRxSeed1(op->cores);
  rxGpuWork &w = rxGpuParWork;
  w.ensure(nslot, rxGpuSolveCfg.cl == NULL, true);
  std::vector<rxGpuSub> &sub = w.sub;
  int next = 0;
  for (int k = 0; k < nslot; k++) {
    sub[k].id = rx->ordId[next++] - 1;
    sub[k].st = RXGS_INIT;
    sub[k].req = 0;
    w.active[k] = 1;
  }
  int nact = nslot;
  int fail = 0;
  rxGpuStats.solves++;
  if (rxGpuTlSolveOk(rx, nsolve, nslot, d[RXG_D_NROW])) {
    fail = rxGpuTimelines(rx, op, w, nslot, nsolve, threads, seed0);
    setRxSeedFinal(seed0 + (uint32_t)nsolve);
    if (threads < op->cores) op->cores = threads;
    return fail;
  }
  while (nact > 0 && !fail) {
    auto _t0 = std::chrono::steady_clock::now();
    // advance every subject (applying its last result) until it waits
#ifdef _OPENMP
#pragma omp parallel for num_threads(threads) schedule(dynamic)
#endif
    for (int k = 0; k < nslot; k++) {
      if (!w.active[k]) continue;
      setRxThreadId(k);
      rxGpuRunSlot(rx, op, k, w, seed0, &next, nsolve);
      setRxThreadId(-1);
    }
    int n = rxGpuGather(w, nslot, &nact);
    auto _t1 = std::chrono::steady_clock::now();
    rxGpuStats.advance += std::chrono::duration<double>(_t1 - _t0).count();
    if (n == 0) continue;
    rxGpuStats.rounds++;
    rxGpuStats.requests += n;
    fail = rxGpuRunRound(w, n, threads);
    if (fail) break;
    rxGpuStats.round += std::chrono::duration<double>(std::chrono::steady_clock::now() - _t1).count();
    for (int r = 0; r < n; r++) {
      int k = w.map[r];
      rxGpuResult(&sub[k], w.q2, r);
      sub[k].req = 0;
    }
  }
  setRxSeedFinal(seed0 + (uint32_t)nsolve);
  // the rest of the solve (output) runs on the real team, never wider than
  // the pools op->cores sized
  if (threads < op->cores) op->cores = threads;
  return fail;
}

// R: install (or clear, with round NULL) the GPU solve of the next rxSolve()
//   round   external pointer to rxg_host_round() of the model's host library
//   timeline external pointer to its rxg_host_timeline(), or NULL: rounds only
//           (dims[RXG_D_NROW] sizes the timeline rows)
//   cl      OpenCL run of .rxGpuRun() or NULL (CPU backend)
//   dims    the layout (RXG_D_*; dims[RXG_D_NSUB] slots)
//   od, oi  integrator options
//   threads OpenMP team
// Solve setup (rxSolve_()) of a GPU method: 1 when R installed the GPU setup
// for this model just before (it then serves this solve and its ind_solve()
// re-solves); 0 otherwise -- a solve set up from C, as nlmixr2est's FOCEi
// does, or of another model -- and the method runs as its host counterpart.
extern "C" int rxGpuSolveClaim(const char *key) {
  int ok = rxGpuSolveCfg.on && rxGpuSolveCfg.armed && rxGpuSolveCfg.round != NULL &&
    key != NULL && rxGpuSolveCfg.key == key;
  rxGpuSolveCfg.armed = 0;
  if (!ok) rxGpuSolveCfg.on = 0;
  return ok;
}

// slots the installed GPU setup needs (op->cores covers them)
extern "C" int rxGpuSolveSlots(void) {
  return rxGpuSolveCfg.on ? rxGpuSolveCfg.dims[RXG_D_NSUB] : 0;
}

// a solve that must run on one thread (NA times, ...) advances its subjects
// on one thread too
extern "C" void rxGpuSolveOneThread(void) {
  rxGpuSolveCfg.threads = 1;
}

extern "C" SEXP _rxode2_rxGpuSolveSet(SEXP round, SEXP cl, SEXP dims, SEXP od,
                                      SEXP oi, SEXP threads, SEXP key,
                                      SEXP timeline) {
  if (Rf_isNull(round)) {
    rxGpuSolveFreeAll();
    rxGpuSolveCfg.round = NULL;
    rxGpuSolveCfg.cl = NULL;
    return R_NilValue;
  }
  if (TYPEOF(round) != EXTPTRSXP || R_ExternalPtrAddrFn(round) == NULL) {
    (Rf_errorcall)(R_NilValue, _("invalid host round"));
  }
  if (Rf_length(dims) != RXG_D_N) (Rf_errorcall)(R_NilValue, _("invalid GPU layout"));
  if (TYPEOF(key) != STRSXP || Rf_length(key) != 1) (Rf_errorcall)(R_NilValue, _("invalid model key"));
  if (!Rf_isNull(timeline) &&
      (TYPEOF(timeline) != EXTPTRSXP || R_ExternalPtrAddrFn(timeline) == NULL ||
       INTEGER(dims)[RXG_D_NROW] < INTEGER(dims)[RXG_D_NSUB])) {
    (Rf_errorcall)(R_NilValue, _("invalid host timeline"));
  }
  // nothing is installed until every argument checks out
  rxGpuSolveCfg.on = 0;
  rxGpuSolveCfg.armed = 0;
  rxGpuCl *g = NULL;
  if (!Rf_isNull(cl)) {
    if (TYPEOF(cl) != EXTPTRSXP || R_ExternalPtrTag(cl) != Rf_install("rxGpuCl") ||
        R_ExternalPtrAddr(cl) == NULL) {
      (Rf_errorcall)(R_NilValue, _("invalid OpenCL run"));
    }
    g = (rxGpuCl *)R_ExternalPtrAddr(cl);
  }
  rxGpuSolveCfg.round = (rxGpuHostRound_t)R_ExternalPtrAddrFn(round);
  rxGpuSolveCfg.timeline = Rf_isNull(timeline) ? NULL :
    (rxGpuHostTimeline_t)R_ExternalPtrAddrFn(timeline);
  rxGpuSolveCfg.cl = g;
  memcpy(rxGpuSolveCfg.dims, INTEGER(dims), sizeof(int)*RXG_D_N);
  rxMemAssign(fixed, rxGpuSolveCfg.od, REAL(od), REAL(od) + Rf_length(od));
  rxMemAssign(fixed, rxGpuSolveCfg.oi, INTEGER(oi), INTEGER(oi) + Rf_length(oi));
  rxGpuSolveCfg.threads = Rf_asInteger(threads);
  rxGpuSolveCfg.gen++;
  rxGpuSolveCfg.key = CHAR(STRING_ELT(key, 0));
  rxGpuSolveCfg.on = 1;
  rxGpuSolveCfg.armed = 1;
  return R_NilValue;
}

#endif // IN_PAR_SOLVE
