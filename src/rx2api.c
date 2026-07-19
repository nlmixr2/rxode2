#include "../inst/include/rxode2.h"
#include "rx2api.h"

rx_solve *getRxSolve_(void);

// ABI-boundary guard: downstream packages resolve these accessors through the
// function-pointer table and may hand us a NULL `rx` if their cached solve
// pointer has not been populated yet (e.g. an accessor is called before
// `rx = getRxSolve_()` on a cold first fit).  Rather than dereference NULL and
// segfault the host process, fall back to the global solve structure so the
// returned `rx` (and its `rx->op`) is always safe to read.
//
// The global solve struct (`&rx_global`) and its options (`&op_global`) are
// static storage, so reading a scalar counter/flag off `rx` or `rx->op` (nsub,
// nall, nobs, npars, neq, nlhs, stiff, ...) is always memory-safe; before a
// solve those scalars are simply zero.  Downstream code relies on this: e.g.
// babelmixr2's PopED integration probes these counts at load time (in `.onLoad`
// via `popedGetLoadedInfo()`), long before any `rxSolve()`, and expects zeros
// rather than an error.  So this helper does NOT treat an un-populated solve as
// fatal -- the "not set up" signal (`rx->subjects == NULL`, backed by
// `inds_global`, which stays NULL until a solve allocates the subject array) is
// checked at the one place that actually dereferences a subject
// (`getSolvingOptionsInd`), where dereferencing NULL would crash.
//
// Call sites pass `__func__` for `what` so the reported accessor name always
// matches the calling function without manual upkeep across renames.
// Shared "solve not set up" message body reused by the accessors below so the
// wording cannot drift between call sites.  %s is the accessor name; the trailing
// %s is a branch-specific clause describing exactly which piece is missing.
#define RX_SOLVE_NOT_SETUP_FMT                                          \
  "rxode2: cannot access the solve (%s): the solving environment is "   \
  "not set up. This usually means a solve accessor was called before "  \
  "rxSolve() populated the solving environment (%s)."

static inline rx_solve *rxSolveOrError(rx_solve *rx, const char *what) {
  if (rx == NULL) {
    rx = getRxSolve_();
  }
  if (rx == NULL || rx->op == NULL) {
    Rf_error(RX_SOLVE_NOT_SETUP_FMT, what, "rx_solve is NULL/uninitialized");
  }
  return rx;
}

rx_solving_options* getSolvingOptions(rx_solve* rx) {
  rx = rxSolveOrError(rx, __func__);
  return rx->op;
}

rx_solving_options_ind *getSolvingOptionsInd(rx_solve *rx, int id) {
  rx = rxSolveOrError(rx, __func__);
  // Unlike the scalar accessors, this dereferences the subject array, so an
  // un-populated solve (subjects still NULL, before any rxSolve()) is fatal
  // here -- raise the clean R error instead of crashing on NULL.  The `rx`
  // itself is the (non-NULL) global here, so the message names the actually
  // missing piece: no solve has populated the subject array yet.
  if (rx->subjects == NULL) {
    Rf_error(RX_SOLVE_NOT_SETUP_FMT, __func__, "no solve has populated the subject array yet");
  }
  // nsub/nsim are uint32_t; multiply in 64-bit so the product cannot wrap and
  // corrupt the bounds check for large subject/simulation counts.
  uint64_t nall = (uint64_t)rx->nsub*(uint64_t)rx->nsim;
  if (id < 0 || (uint64_t)id >= nall) {
    Rf_error("[getSolvingOptionsInd]: id (%d) should be between [0, %llu); nsub: %u nsim: %u", id, (unsigned long long)nall, (unsigned int)rx->nsub, (unsigned int)rx->nsim);
  }
  return &(rx->subjects[id]);
}

////////////////////////////////////////////////////////////////////////
// Individual solving options
////////////////////////////////////////////////////////////////////////

double getIndLambda(rx_solving_options_ind* ind) {
  return ind->lambda;
}

int getIndLambdaYj(rx_solving_options_ind* ind) {
  return (int)(ind->yj);
}

double getIndLogitLow(rx_solving_options_ind* ind) {
  return ind->logitLow;
}

double getIndLogitHi(rx_solving_options_ind* ind) {
  return ind->logitHi;
}


void setIndParPtr(rx_solving_options_ind* ind, int i, double val) {
  rx_solve* rx = getRxSolve_();
  if (i < 0 || i >= rx->npars) {
    Rf_error("[setIndParPtr]: i (%d) should be between [0, %d) when assigning  %f", i, rx->npars, val);
  }
  ind->par_ptr[i] = val;
}

double getIndParPtr(rx_solving_options_ind* ind, int i) {
  rx_solve* rx = getRxSolve_();
  if (i < 0 || i >= rx->npars) {
    Rf_error("[getIndParPtr]: i (%d) should be between [0, %d)", i, rx->npars);
  }
  return ind->par_ptr[i];
}

int getIndNallTimes(rx_solving_options_ind* ind) {
  return ind->n_all_times;
}

void setIndIdx(rx_solving_options_ind* ind, int j) {
  ind->idx = j;
}

int getIndIx(rx_solving_options_ind* ind, int j) {
  if (j < 0 || j >= ind->n_all_times) {
    Rf_error("[getIndIx]: j (%d) should be between [0, %d)", j, ind->n_all_times);
  }
  return ind->ix[j];
}

int getIndMixest(rx_solving_options_ind* ind) {
  return ind->mixest;
}

void setIndMixest(rx_solving_options_ind* ind, int mixest) {
  if (mixest < 0) {
    Rf_error("[setIndMixest]: mixest (%d) should be >= 0", mixest);
  }
  ind->mixest = mixest;
}

int getRxMixnum(rx_solve *rx) {
  // This is the number of mixtures
  rx = rxSolveOrError(rx, __func__);
  return rx->mixnum;
}

void setRxMixnum(rx_solve *rx, int mixnum) {
  if (mixnum < 0) {
    Rf_error("[setRxMixnum]: mixnum (%d) should be >= 0", mixnum);
  }
  rx->mixnum = mixnum;
}

double getIndTolFactor(rx_solving_options_ind *ind) {
  return ind->tolFactor;
}

void setIndTolFactor(rx_solving_options_ind *ind, double tolFactor) {
  ind->tolFactor = tolFactor;
}

int getIndNeqOverride(rx_solving_options_ind *ind) {
  return ind->neqOverride;
}

void setIndNeqOverride(rx_solving_options_ind *ind, int neq) {
  ind->neqOverride = neq;
}

int getIndEvid(rx_solving_options_ind* ind, int kk) {
  if (kk < 0 || kk >= ind->n_all_times) {
    Rf_error("[getIndEvid]: kk (%d) should be between [0, %d)", kk, ind->n_all_times);
  }
  return ind->evid[kk];
}

double *getIndLhs(rx_solving_options_ind* ind) {
  return ind->lhs;
}

int getIndNdoses(rx_solving_options_ind* ind) {
  return ind->ndoses;
}

int getIndNevid2(rx_solving_options_ind* ind) {
  return ind->nevid2;
}

void setIndSolve(rx_solving_options_ind* ind, int solve) {
  ind->solved = solve;
}

double *getIndSolve(rx_solving_options_ind* ind) {
  return ind->solve;
}

// Buffer-pointer accessors: get/set the per-individual ODE solve buffers so a
// caller can temporarily swap in privately-owned, larger buffers (e.g. to solve
// a higher-state sensitivity model against the same event table) and restore the
// originals afterward.  Pair with setOpNeq() to keep the effective state count
// consistent with the swapped buffers.
void setIndSolvePtr(rx_solving_options_ind* ind, double* solve) {
  ind->solve = solve;
}

double *getIndSolveSave(rx_solving_options_ind* ind) {
  return ind->solveSave;
}

void setIndSolveSave(rx_solving_options_ind* ind, double* solveSave) {
  ind->solveSave = solveSave;
}

double *getIndSolveLast(rx_solving_options_ind* ind) {
  return ind->solveLast;
}

void setIndSolveLast(rx_solving_options_ind* ind, double* solveLast) {
  ind->solveLast = solveLast;
}

double *getIndSolveLast2(rx_solving_options_ind* ind) {
  return ind->solveLast2;
}

void setIndSolveLast2(rx_solving_options_ind* ind, double* solveLast2) {
  ind->solveLast2 = solveLast2;
}

double getIndDv(rx_solving_options_ind* ind, int j) {
  if (j < 0 || j >= ind->n_all_times) {
    Rf_error("[getIndDv]: j (%d) should be between [0, %d)", j, ind->n_all_times);
  }
  if (j >= ind->n_all_times_orig) {
    // dv is NA for events added after the original event table (e.g. evid_() pushes)
    return NA_REAL;
  }
  return ind->dv[j];
}

int getIndYj(rx_solving_options_ind* ind) {
  return (int)(ind->yj);
}

double getIndLimit(rx_solving_options_ind* ind, int kk) {
  if (kk < 0 || kk >= ind->n_all_times) {
    Rf_error("[getIndLimit]: kk (%d) should be between [0, %d)", kk, ind->n_all_times);
  }
  if (kk >= ind->n_all_times_orig) {
    // limit is -Inf for events added after the original event table (e.g. evid_() pushes)
    return R_NegInf;
  }
  return ind->limit[kk];
}

int getIndCens(rx_solving_options_ind* ind, int kk) {
  if (kk < 0 || kk >= ind->n_all_times) {
    Rf_error("[getIndCens]: kk (%d) should be between [0, %d)", kk, ind->n_all_times);
  }
  if (kk >= ind->n_all_times_orig) {
    // cens is 0 for events added after the original event table (e.g. evid_() pushes)
    return 0;
  }
  return ind->cens[kk];
}

int getIndIdx(rx_solving_options_ind* ind) {
  return ind->idx;
}

// Per-observation endpoint (compartment) from the CMT time-varying covariate.  The
// covariate index is cached in op->cmtCov at setup (getIndCmt does no name lookup).
// Returns the raw CMT value at observation row kk, or 1 when the model has no CMT
// covariate (a single-endpoint model) or the value is missing.  CMT values are the
// data's compartment numbers (distinct, not necessarily sequential).
int getIndCmt(rx_solving_options* op, rx_solving_options_ind* ind, int kk) {
  if (op == NULL || op->cmtCov < 0) return 1;
  if (kk < 0 || kk >= ind->n_all_times) {
    Rf_error("[getIndCmt]: kk (%d) should be between [0, %d)", kk, ind->n_all_times);
  }
  double v = ind->cov_ptr[(size_t)ind->n_all_times * (size_t)op->cmtCov + (size_t)kk];
  if (ISNA(v)) return 1;
  return (int) v;
}

////////////////////////////////////////////////////////////////////////
// Solving options (rx->op)
////////////////////////////////////////////////////////////////////////

int getOpNeq(rx_solving_options* op) {
  return op->neq;
}

void setOpNeq(rx_solving_options* op, int neq) {
  op->neq = neq;
}

int hasOpBadSolve(rx_solving_options* op) {
  return op->badSolve;
}

int getOpNlin(rx_solving_options* op) {
  // always return 0 since op->nlin has been removed from rxode2
  return 0;
}

int getOpCores(rx_solving_options* op) {
  return op->cores;
}

int getOpNlhs(rx_solving_options* op) {
  return op->nlhs;
}

int getOpStiff(rx_solving_options* op) {
  return op->stiff;
}

void resetOpBadSolve(rx_solving_options* op) {
  op->badSolve = 0;
}
////////////////////////////////////////////////////////////////////////
// Solving options in rx
////////////////////////////////////////////////////////////////////////

int getRxNsub(rx_solve *rx) {
  rx = rxSolveOrError(rx, __func__);
  return (int)rx->nsub;
}

int hasRxLimit(rx_solve *rx) {
  rx = rxSolveOrError(rx, __func__);
  return rx->limit;
}

int hasRxCens(rx_solve *rx) {
  rx = rxSolveOrError(rx, __func__);
  return rx->cens;
}

int getRxNall(rx_solve *rx) {
  rx = rxSolveOrError(rx, __func__);
  return rx->nall;
}

int getRxNobs(rx_solve *rx) {
  rx = rxSolveOrError(rx, __func__);
  return rx->nobs;
}

int getRxNobs2(rx_solve *rx) {
  rx = rxSolveOrError(rx, __func__);
  return rx->nobs2;
}

int getRxNsim(rx_solve *rx) {
  rx = rxSolveOrError(rx, __func__);
  return (int)rx->nsim;
}

int getRxNpars(rx_solve *rx) {
  rx = rxSolveOrError(rx, __func__);
  return rx->npars;
}

int getOrdId(rx_solve *rx, int solveid) {
  rx = rxSolveOrError(rx, __func__);
  return rx->ordId[solveid];
}
////////////////////////////////////////////////////////////////////////
// Get solve vector for ith solve
////////////////////////////////////////////////////////////////////////
double * getOpIndSolve(rx_solving_options* op, rx_solving_options_ind* ind, int idx) {
  if (idx  < 0 || idx >= ind->n_all_times) {
    Rf_error("[getOpIndSolve]: the individual should be between [0, %d); neq: %d", ind->n_all_times, op->neq);
  }
  return ind->solve + (rxEffNeq(ind, op))*(idx);
}
