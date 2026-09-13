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

// ---- reporting a bad index from inside a parallel region ------------------
//
// Rf_error() longjmps to R's top level.  On an OpenMP worker that unwinds past
// the runtime's own state, so an out-of-range index -- a caller bug, but one
// that should still report itself -- takes the session down instead of
// printing.  These accessors ARE called from inside downstream per-subject
// parallel regions (nlmixr2est's inner/saem/imp/nlm loops all read events and
// solve slices through them), so that is a live path, not a theoretical one.
//
// Same philosophy as the NULL handling above: do not crash the host.  Inside a
// region, record the first message and hand back something in range so the
// region can finish; the result is wrong, but wrong in a way the caller
// survives, and the message is raised at the next serial boundary (rxSolve_(),
// beside the existing op->abort check).  Outside a region, raise immediately,
// which is what every caller had before.
//
// One buffer, first writer wins, taken under a critical section: this runs only
// on the error path, so the cost is irrelevant and an interleaved message would
// not be.

// This file is C, and Makevars.in puts $(SHLIB_OPENMP_CXXFLAGS) on
// PKG_CXXFLAGS only -- so _OPENMP is NOT defined here and omp_in_parallel()
// would compile out to the shim's 0, answering "serial" on every worker and
// defeating the whole point.  Ask a C++ translation unit, which does have the
// flag.  (libgomp is linked either way: PKG_LIBS carries it too.)
extern int rxInParallel(void);

// Recording is done under a critical section in the C++ helper for the same
// reason -- no OpenMP pragmas are available here.
extern void rxApiErrRecord(const char *msg);

static void rxApiError(const char *fmt, ...) {
  char buf[512];
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(buf, sizeof(buf), fmt, ap);
  va_end(ap);
  if (rxInParallel()) {
    rxApiErrRecord(buf);
    return;
  }
  Rf_error("%s", buf);
}

// Is an index error waiting to be reported?  For a caller that wants to check
// without raising (a driver deciding whether to abort a fit early).
extern int rxApiErrPendingImpl(void);
int rxApiErrPending(void) {
  return rxApiErrPendingImpl();
}

// Raise a recorded index error, if any.  Safe to call when none is pending.
// Must be called from the main thread, outside any parallel region.
extern const char *rxApiErrTakeImpl(void);
void rxApiErrRaise(void) {
  const char *msg = rxApiErrTakeImpl();   // clears; Rf_error() does not return
  if (msg == NULL) return;
  Rf_error("%s", msg);
}

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
    rxApiError("[getSolvingOptionsInd]: id (%d) should be between [0, %llu); nsub: %u nsim: %u", id, (unsigned long long)nall, (unsigned int)rx->nsub, (unsigned int)rx->nsim);
    id = 0;   // in range whenever the array exists; the message is raised later
  }
  // Walk the array at the stride it was ALLOCATED with, not at this
  // translation unit's `sizeof` -- see rxIndSize in rx2api.h.  It is
  // statically initialized, so it is never zero here.
  return (rx_solving_options_ind*)((char*)rx->subjects + (size_t)id*rxIndSize);
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
    rxApiError("[setIndParPtr]: i (%d) should be between [0, %d) when assigning  %f", i, rx->npars, val);
    return;
  }
  ind->par_ptr[i] = val;
}

double getIndParPtr(rx_solving_options_ind* ind, int i) {
  rx_solve* rx = getRxSolve_();
  if (i < 0 || i >= rx->npars) {
    rxApiError("[getIndParPtr]: i (%d) should be between [0, %d)", i, rx->npars);
    return NA_REAL;
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
    rxApiError("[getIndIx]: j (%d) should be between [0, %d)", j, ind->n_all_times);
    return 0;
  }
  return ind->ix[j];
}

int getIndMixest(rx_solving_options_ind* ind) {
  return ind->mixest;
}

void setIndMixest(rx_solving_options_ind* ind, int mixest) {
  if (mixest < 0) {
    rxApiError("[setIndMixest]: mixest (%d) should be >= 0", mixest);
    return;
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
    rxApiError("[setRxMixnum]: mixnum (%d) should be >= 0", mixnum);
    return;
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
    rxApiError("[getIndEvid]: kk (%d) should be between [0, %d)", kk, ind->n_all_times);
    return 0;
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
    rxApiError("[getIndDv]: j (%d) should be between [0, %d)", j, ind->n_all_times);
    return NA_REAL;
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
    rxApiError("[getIndLimit]: kk (%d) should be between [0, %d)", kk, ind->n_all_times);
    return NA_REAL;
  }
  if (kk >= ind->n_all_times_orig) {
    // limit is -Inf for events added after the original event table (e.g. evid_() pushes)
    return R_NegInf;
  }
  return ind->limit[kk];
}

int getIndCens(rx_solving_options_ind* ind, int kk) {
  if (kk < 0 || kk >= ind->n_all_times) {
    rxApiError("[getIndCens]: kk (%d) should be between [0, %d)", kk, ind->n_all_times);
    return 0;
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
// CMT values are the data's compartment numbers (distinct, not necessarily
// sequential).  Two distinct answers, deliberately NOT the same value:
//
//   * the model has NO CMT covariate (a single-endpoint model), or the individual
//     has no covariate array -> 1, because every observation IS compartment 1;
//   * the covariate is present but this row's value is MISSING -> NA_INTEGER, so a
//     caller can tell "no compartment recorded here" from "compartment 1".
//
// Returning 1 for a missing value made the two indistinguishable, which silently
// mis-classifies an NA row as a real compartment.  A caller that wants the old
// lenient behaviour writes `cmt = getIndCmt(...); if (cmt == NA_INTEGER) cmt = 1;`.
int getIndCmt(rx_solving_options* op, rx_solving_options_ind* ind, int kk) {
  if (op == NULL || op->cmtCov < 0 || ind == NULL || ind->cov_ptr == NULL) return 1;
  if (kk < 0 || kk >= ind->n_all_times) {
    rxApiError("[getIndCmt]: kk (%d) should be between [0, %d)", kk, ind->n_all_times);
    return 0;
  }
  double v = ind->cov_ptr[(size_t)ind->n_all_times * (size_t)op->cmtCov + (size_t)kk];
  if (ISNA(v) || ISNAN(v)) return NA_INTEGER;
  return (int) v;
}

// Inverse of getIndCmt(): write the CMT covariate at row kk.  Lets a downstream
// package re-base the CMT column (e.g. nlmixr2est's shared FOCEi solve pool,
// whose peer models carry different sensitivity-compartment counts and so
// different _CMT offsets) without reaching into op->cmtCov / ind->cov_ptr.  A
// no-op for a model with no CMT covariate (single endpoint) or an individual
// with no covariate array.
void setIndCmt(rx_solving_options* op, rx_solving_options_ind* ind, int kk, int cmt) {
  if (op == NULL || op->cmtCov < 0 || ind == NULL || ind->cov_ptr == NULL) return;
  if (kk < 0 || kk >= ind->n_all_times) {
    rxApiError("[setIndCmt]: kk (%d) should be between [0, %d)", kk, ind->n_all_times);
    return;
  }
  // NA_INTEGER is INT_MIN, so a plain (double) cast would store -2147483648.0 -- a
  // finite value that ISNA() does not recognize and that covariate interpolation would
  // treat as a real compartment number.  Round-trip it as the double NA instead, which
  // is what getIndCmt() reads back as NA_INTEGER.
  ind->cov_ptr[(size_t)ind->n_all_times * (size_t)op->cmtCov + (size_t)kk] =
    (cmt == NA_INTEGER) ? NA_REAL : (double) cmt;
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
  // NULL is the identity, the same reading par_solve.c's own rxDriveTeamId()
  // gives it: rx->ordId holds the solve order only once something has built
  // one, and "no order yet" means the data order.  Callers run inside OpenMP
  // regions, where an Rf_error() would longjmp across threads, so this cannot
  // raise -- returning the identity keeps each subject mapped to itself
  // rather than dereferencing NULL.
  if (rx->ordId == NULL) return solveid + 1;
  return rx->ordId[solveid];
}
////////////////////////////////////////////////////////////////////////
// Get solve vector for ith solve
////////////////////////////////////////////////////////////////////////
double * getOpIndSolve(rx_solving_options* op, rx_solving_options_ind* ind, int idx) {
  if (idx  < 0 || idx >= ind->n_all_times) {
    rxApiError("[getOpIndSolve]: the individual should be between [0, %d); neq: %d", ind->n_all_times, op->neq);
    idx = 0;   // the slice start is always mapped; the message is raised later
  }
  return ind->solve + (rxEffNeq(ind, op))*(idx);
}

