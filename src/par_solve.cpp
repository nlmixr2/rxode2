#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "rxomp.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string>
#include <vector>
#include "strncmp.h"
#include "timsort.h"
#include "../inst/include/rxode2.h"
#include "../inst/include/rxode2dataErr.h"
#include "../inst/include/rxode2parseHandleEvid.h"
#include "../inst/include/rxode2parseGetTime.h"
#include "../inst/include/rxode2EventTranslate.h"
#include "linCmtDiffConstant.h"

#define SORT gfx::timsort

#define isSameTimeOp(xout, xp) (op->stiff == 0 ? isSameTimeDop(xout, xp) : isSameTime(xout, xp))

// dop853 is same time
#include "rxProtect.h"

extern "C" uint32_t getRxSeed1(int ncores);
extern "C" void setSeedEng1(uint32_t seed);
extern "C" void setRxSeedFinal(uint32_t seed);

extern "C" {
#include "dop853.h"
#include "common.h"
#include "solveWarn.h"
#include "lsoda.h"
#include "rxode2_df.h"
}
#include "par_solve.h"
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )
#define badSolveExit(i) for (int j = rxEffNeq(ind, op)*(ind->n_all_times); j--;){ \
    ind->solve[j] = NA_REAL;                                           \
  }                                                                     \
  _Pragma("omp atomic write")                                           \
  op->badSolve = 1;                                                     \
  i = ind->n_all_times-1; // Get out of here!
// Yay easy parallel support
// For Mac, see: http://thecoatlessprofessor.com/programming/openmp-in-r-on-os-x/ (as far as I can tell)
// and https://github.com/Rdatatable/data.table/wiki/Installation#openmp-enabled-compiler-for-mac
// It may have arrived, though I'm not sure...
// According to http://dirk.eddelbuettel.com/papers/rcpp_parallel_talk_jan2015.pdf
// OpenMP is excellent for parallelizing existing loops where the iterations are independent;
// OpenMP is used by part of the R core, therefore support will come for all platforms at some time in the future.
// Since these are independent, we will just use Open MP.

#define _(String) (String)
int _isRstudio = 0;

#include "rxData.h"

extern "C" void setRstudioPrint(int rstudio);
extern "C" void RSprintf(const char *format, ...);

// Pre-generated eta batch draw (defined in rxthreefry.cpp)
extern "C" void rxPreGenEta(rx_solve *rx, int ncores);
extern "C" void rxEtaPreDeactivate(void);

extern "C" SEXP _rxHasOpenMp(){
  rxProtect rx_protect;
  SEXP ret = rx_protect.protect(Rf_allocVector(LGLSXP,1));
#ifdef _OPENMP
  INTEGER(ret)[0] = 1;
#else
  INTEGER(ret)[0] = 0;
#endif
  // UNPROTECT
  return ret;
}

rx_solve rx_global;

// -- Per-thread LSODA context pool --------------------------------------------
// Eliminates one malloc/free pair of the large alloc_mem block per subject.
// Pattern mirrors the __linCmtA / __linCmtB pool in linCmt.cpp.
struct lsoda_pool_t {
  struct lsoda_context_t ctx;          // context (holds common ptr, function, etc.)
  struct lsoda_opt_t     opt;          // persistent opt storage ctx->opt points here
  int                    allocated_neq; // neq used for alloc_mem; 0 = not prepared yet
};

static std::vector<lsoda_pool_t> __lsodaCtxPool;

extern "C" void ensureLsodaCtxPool(int nCores) {
  if ((int)__lsodaCtxPool.size() < nCores) {
    __lsodaCtxPool.resize(nCores); // value-initializes new slots (all zero)
  }
}

extern "C" void freeLsodaCtxPool() {
  for (int i = 0; i < (int)__lsodaCtxPool.size(); i++) {
    lsoda_pool_t &p = __lsodaCtxPool[i];
    if (p.allocated_neq > 0 && p.ctx.common != NULL) {
      if (p.ctx.error) {
        free(p.ctx.error);
        p.ctx.error = NULL;
      }
      lsoda_free(&p.ctx);
      p.ctx.common = NULL;
    }
    p.allocated_neq = 0;
  }
  __lsodaCtxPool.clear();
}
// -----------------------------------------------------------------------------

// -- Per-thread Fortran LSODA rwork/iwork pool --------------------------------
// Eliminates false-sharing on the global rwork/iwork arrays when par_lsoda runs
// in parallel.  Each thread gets its own pre-allocated work arrays sized once
// at setup time (22 + neq*max(16,neq+9) + 1 doubles; 20 + neq + 1 ints).
struct rwork_pool_t {
  double      *rworkp = NULL;  // real work array
  int         *iworkp = NULL;  // integer work array
  unsigned int rworki = 0;  // current capacity (element count)
  unsigned int iworki = 0;
};

static std::vector<rwork_pool_t> __rworkPool;

extern "C" void ensureRworkPool(int nCores, int lrw, int liw) {
  int need = lrw + 1;
  int needI = liw + 1;
  if ((int)__rworkPool.size() < nCores)
    __rworkPool.resize(nCores); // zero-initialises new slots
  for (int i = 0; i < nCores; i++) {
    rwork_pool_t &p = __rworkPool[i];
    if ((int)p.rworki < need) {
      double *np;
      if (p.rworki == 0 || p.rworkp == NULL) {
        if (p.rworkp != NULL) free(p.rworkp);
        np = (double *)calloc((size_t)need, sizeof(double));
      } else {
        np = (double *)realloc(p.rworkp, (size_t)need * sizeof(double));
        if (np) memset(np + p.rworki, 0, ((size_t)need - p.rworki) * sizeof(double));
      }
      if (!np) (Rf_error)("ensureRworkPool: out of memory allocating rwork (%d doubles)", need);
      p.rworkp = np;
      p.rworki = (unsigned int)need;
    }
    if ((int)p.iworki < needI) {
      int *np;
      if (p.iworki == 0 || p.iworkp == NULL) {
        if (p.iworkp != NULL) free(p.iworkp);
        np = (int *)calloc((size_t)needI, sizeof(int));
      } else {
        np = (int *)realloc(p.iworkp, (size_t)needI * sizeof(int));
        if (np) memset(np + p.iworki, 0, ((size_t)needI - p.iworki) * sizeof(int));
      }
      if (!np) (Rf_error)("ensureRworkPool: out of memory allocating iwork (%d ints)", needI);
      p.iworkp = np;
      p.iworki = (unsigned int)needI;
    }
  }
}

extern "C" void freeRworkPool() {
  for (int i = 0; i < (int)__rworkPool.size(); i++) {
    rwork_pool_t &p = __rworkPool[i];
    if (p.rworki > 0 && p.rworkp != NULL) {
      free(p.rworkp);
      p.rworkp = NULL;
      p.rworki = 0;
    }
    if (p.iworki > 0 && p.iworkp != NULL) {
      free(p.iworkp);
      p.iworkp = NULL;
      p.iworki = 0;
    }
  }
  __rworkPool.clear();
}
// -----------------------------------------------------------------------------

extern "C" void nullGlobals() {
  lineNull(&(rx_global.factors));
  lineNull(&(rx_global.factorNames));
}

static inline const char *getId(int id) {
  rx_solve *rx = &rx_global;
  int curLen=  rx->factorNs[0];
  const char *unknownId = "Unknown";
  if (id < 0) {
    return unknownId; // Bad value
  }
  if (id < curLen){
    if (id >= rx->factors.n) {
      return unknownId;
    }
    return rx->factors.line[id];
  } else {
    return unknownId;
  }
}

extern "C" const char *rxGetId(int id) {
  return getId(id);
}

extern "C" void printErr(int err, int id){
  RSprintf("Recovered solving errors for internal ID %s (%d):\n", getId(id), err);
  if (err & rxErrCorruptETSort){
    RSprintf("  Corrupted event table during sort (1)\n");
  }
  if (err & rxErrRate0){
    RSprintf("  Rate is zero/negative\n");
  }
  if (err & rxErrModelRateAbsent){
    RSprintf("  Modeled rate requested in event table, but not in model; use 'rate(cmt) ='\n");
  }
  if (err & rxErrCorruptETSort2){
    RSprintf("  Corrupted event table during sort (2)\n");
  }
  if (err & rxErrDurNeg0){
    RSprintf("  Duration is zero/negative\n");
  }
  if (err & rxErrModelDurAbsent){
    RSprintf("  Modeled duration requested in event table, but not in model; use 'dur(cmt) ='\n");
  }
  if (err & rxErrModelData686){
    RSprintf("  Data error 686\n");
  }
  if (err & rxErrModelDataNeg6){
    RSprintf("  Data Error -6\n");
  }
  if (err & rxErrModelDataErr8){
    RSprintf("  Data Error 8\n");
  }
  if (err & rxErrModelDataErr886){
    RSprintf("  Data error 886\n");
  }
  if (err & rxErrModelDataErr797){
    RSprintf("  Data error 797\n");
  }
  if (err & rxErrModelDataNeg7){
    RSprintf("  Data Error -7\n");
  }
  if (err & rxErrModelDataErr9){
    RSprintf("  Data Error 9\n");
  }
  if (err & rxErrModelDataErr997){
    RSprintf("  Data error 997\n");
  }
  if (err & rxErrCorruptETSort3){
    RSprintf("  Corrupted event table during sort (3)\n");
  }
  if (err & rxErrCorruptET) {
    RSprintf("  Corrupted event table\n");
  }
  if (err & rxErrCorruptET2){
    RSprintf("  Corrupted events\n");
  }
  if (err & rxErrNegCmt){
    RSprintf("  Supplied an invalid EVID\n");
  }
  if (err & rxErrSync){
    RSprintf("  Corrupted event table (during sync)\n");
  }
  if (err & rxErrSync2){
    RSprintf("  Corrupted event table (end of sync)\n");
  }
  if (err & rxErrModeledFss2){
    RSprintf("  SS=2 & Modeled F does not work\n");
  }

  if (err & rxErrModeledFss2n2){
    RSprintf("  SS=2 & Modeled F does not work\n");
  }
  if (err & rxErrModeledFss2n3){
    RSprintf("  SS=2 & Modeled F does not work\n");
  }
  if (err & rxErrRate02){
    RSprintf(" Rate is zero/negative\n");
  }
}

rx_solving_options op_global;
extern int nPastEvid_global;

rx_solving_options_ind *inds_global = NULL;

rx_solving_options_ind *inds_thread = NULL;


void par_flush_console() {
#if !defined(WIN32) && !defined(__WIN32) && !defined(__WIN32__)
  R_FlushConsole();
#endif
}

extern "C" int isRstudio();
extern "C" int isProgSupported();
int par_progress_0=0;
int par_progress_1=0;
double par_progress__=1.0;

extern "C" SEXP _rxParProgress(SEXP num){
  par_progress__=REAL(num)[0];
  return R_NilValue;
}

clock_t _lastT0;

extern "C" int par_progress(int c, int n, int d, int cores, clock_t t0, int stop){
  if (par_progress__ > 0.0){
    float progress =0.0;
    progress = (float)(c);
    progress /=((float)(n));
    if (progress < 0.) progress = 0.;
    if (progress > 1.) progress = 1.;
    if (progress == 0.) {
      par_progress_0=0;
      par_progress_1=0;
    }
    if (c <= n && ((!par_progress_1 && progress == 1.0) ||
                   ((double)(clock() - _lastT0))/CLOCKS_PER_SEC > par_progress__)){
      if (progress == 1.0){
        par_progress_1=1;
      }
      if (std::isnan(progress)) {
        progress=0.0;
      }
      int nticks= (int)(progress * 50);
      int curTicks = d;
      if (nticks < 0) nticks=0;
      if (nticks > 50) nticks=50;
      if (curTicks < 0) curTicks=0;
      if (curTicks > 50) curTicks=50;
      int isSupported = isProgSupported();
      if (_isRstudio) isSupported = 0;

      if (isSupported == -1){
      } else if (isSupported == 0){
        int i;
        for (i = curTicks; i < nticks; i++){
          if (i == 0) {
            RSprintf("[");
          } else if (i % 5 == 0) {
            RSprintf("|");
          } else {
            RSprintf("=");
          }
        }
        if (nticks == 50){
          if (!par_progress_0){
            par_progress_0 = 1;
            RSprintf("] ");
            _lastT0 = clock();
            clock_t t = _lastT0 - t0;
            double ts = ((double)t)/CLOCKS_PER_SEC;
            if (ts < 60){
              RSprintf("0:00:%02.f ", std::floor(ts));
            } else {
              double f = std::floor(ts/60);
              double s = ts-f*60;
              if (f >= 60){
                double h = std::floor(f/60);
                f = f-h*60;
                RSprintf("%.0f:%02.f:%02.f ", h, f, std::floor(s));
              } else {
                RSprintf("0:%02.f:%02.f ", f, std::floor(s));
              }
            }
            RSprintf("\n");
          }
        }
      } else {
        if (!par_progress_0){
          RSprintf("\r");
          int i;
          for (i = 0; i < nticks; i++){
            if (i == 0) {
              RSprintf("[");
            } else if (i % 5 == 0) {
              RSprintf("|");
            } else {
              RSprintf("=");
            }
          }
          if (nticks < 50) {
            RSprintf(">");
          }
          else {
            par_progress_0 = 1;
          }
          for (i = nticks+1; i < 50; i++){
            RSprintf("-");
          }
          RSprintf("] ");
          if (nticks < 50) RSprintf(" ");
          RSprintf("%02.f%%; ",100*progress,cores);
          _lastT0 = clock();
          clock_t t = _lastT0 - t0;
          double ts = ((double)t)/CLOCKS_PER_SEC;
          if (ts < 60){
            RSprintf("0:00:%02.f ", std::floor(ts));
          } else {
            double f = std::floor((double)(ts/60.0));
            double s = ts-f*60;
            if (f >= 60){
              double h = std::floor(f/60);
              f = f-h*60;
              RSprintf("%.0f:%02.f:%02.f ", h, f, std::floor(s));
            } else {
              RSprintf("0:%02.f:%02.f ", f, std::floor(s));
            }
          }
          if (stop){
            RSprintf("Stopped Calculation!\n");
          }
          par_flush_console();
        }
      }
      return nticks;
    }
  }
  return d;
}

typedef struct {
  int cur;
  int n;
  int d;
  int cores;
  clock_t t0;
} rx_tick;

rx_tick rxt;

extern "C" SEXP _rxTick(){
  rxProtect rx_protect;
  rxt.cur++;
  SEXP ret = rx_protect.protect(Rf_allocVector(INTSXP, 1));
  rxt.d =par_progress(rxt.cur, rxt.n, rxt.d, rxt.cores, rxt.t0, 0);
  INTEGER(ret)[0]=rxt.d;
  // UNPROTECT
  return ret;
}

extern "C" SEXP _rxProgress(SEXP num, SEXP core){
  par_progress_1=0;
  rxt.t0 = clock();
  rxt.cores = INTEGER(core)[0];
  rxt.n = INTEGER(num)[0];
  rxt.d=0;
  rxt.cur = 0;
  return R_NilValue;
}

extern "C" SEXP _rxProgressStop(SEXP clear){
  int clearB = INTEGER(clear)[0];
  par_progress(rxt.n, rxt.n, rxt.d, rxt.cores, rxt.t0, 0);
  par_progress_0=0;
  if (clearB){
    int doIt=isProgSupported();
    if (doIt == -1){
    } else if (isRstudio() || doIt==0){
      RSprintf("\n");
    } else {
      RSprintf("\r                                                                                 \r");
    }
  } else {
    int doIt=isProgSupported();
    if (isRstudio() || doIt == 0){
      RSprintf("\n");
    }
  }
  rxt.d = rxt.n;
  rxt.cur = rxt.n;
  return R_NilValue;
}

extern "C" SEXP _rxProgressAbort(SEXP str){
  par_progress(rxt.n, rxt.n, rxt.d, rxt.cores, rxt.t0, 0);
  par_progress_0=0;
  if (rxt.d != rxt.n || rxt.cur != rxt.n){
    rxSolveFreeC();
    (Rf_errorcall)(R_NilValue, "%s", CHAR(STRING_ELT(str,0)));
  }
  return R_NilValue;
}

t_set_solve set_solve = NULL;

extern "C" void rxOptionsIniEnsure(int mx, int cores) {
  R_Free(inds_global);
  R_Free(inds_thread);
  inds_global = R_Calloc(mx, rx_solving_options_ind);
  inds_thread = R_Calloc(max2(1, cores), rx_solving_options_ind);
  rx_solve *rx=(&rx_global);
  rx->subjects = inds_global;
  rx->ordId = NULL;
}

extern "C" int rxstrcmpi(const char * str1, const char * str2);

extern "C" int compareFactorInt(int val,
                                const char *valStr,
                                int value) {
  if (val <= 0) {
    return 0; // Bad value
  }
  return (val == value);
}

extern "C" int compareFactorVal(int val,
                                const char *valStr,
                                const char *cmpValue){
  rx_solve *rx=(&rx_global);
  int base = 0, curLen=  rx->factorNs[0], curG=0;
  if (val <= 0) {
    return 0; // Bad value
  }
  if (!strcmp(valStr, "ID")) {
    // For ID these are zero
    if (val-1 < curLen){
      if (val-1 >= rx->factors.n) {
        return 0;
      }
      return (!strcmp(rx->factors.line[val-1], cmpValue));
    } else {
      return 0;
    }
  }
  base += curLen;
  curLen = rx->factorNs[++curG];
  if (!rxstrcmpi(valStr, "cmt")) {
    if (val-1 < curLen){
      if (base+val-1 >= rx->factors.n) {
        return 0;
      }
      return (!strcmp(rx->factors.line[base+val-1],
                      cmpValue));
    } else {
      return 0;
    }
  }
  int totN = rx->factorNames.n-2;
  base += curLen;
  for (int i = 0; i < totN; ++i) {
    const char *curFactor = rx->factorNames.line[++curG];
    curLen = rx->factorNs[curG];
    if (!strncmpci(valStr, curFactor, strlen(valStr))) {
      if (val-1 < curLen){
        if (base+val-1 >= rx->factors.n) {
          return 0;
        }
        return (!strcmp(rx->factors.line[base+val-1], cmpValue));
      } else {
        return 0;
      }
    }
    base += curLen;
  }
  // Other factors
  return 0;
}

t_dydt dydt = NULL;

t_calc_jac calc_jac = NULL;

t_calc_lhs calc_lhs = NULL;

// Event ("jump") sensitivities: model-generated total-derivative functions for
// the modeled alag / F fractions (resolved by name from the model lib, like
// calc_jac/Lag/F -- model-internal ABI, never crosses a package boundary).
t_dLag dLag = NULL;
t_dF dF = NULL;

// Event ("jump") sensitivity runtime dims for the current model (one model
// solves at a time, so module globals are sufficient -- same pattern as me_code
// / _es_*Code).  Set from R before a solve via _rxode2_setEventSensDims().
// _rxEsActive: 1 when jump injection is on; _rxEsNState/_rxEsNParam: the physical
// state count and first-order sensitivity-parameter count (sens compartment for
// state k / param p is nState + p*nState + k).
int _rxEsActive = 0;
int _rxEsNState = 0;
int _rxEsNParam = 0;
int _rxEsNParam2 = 0;
// Third-order (Phase H1) calcSens3 parameter count.  Set via its own setter
// (`_rxode2_setEventSensNParam3`) rather than widening `_rxEsNParam`
// dims -- same rationale as `_rxEsUseCalcJac`'s dedicated setter (added
// after the original dims setter shipped; a new arg would break the
// existing 4-arg call sites).
int _rxEsNParam3 = 0;
// When 1, handle_evid's dtau/lag jump row sources its Jacobian column from
// `calc_jac` instead of a central difference of `dydt` -- needed for
// matExp()/indLin() models, whose compiled `dydt()` is a no-op stub (the
// primal system is solved by matrix-exponential propagation, not RHS
// evaluation), which otherwise makes that row silently always zero. Set
// alongside the dims from R (`.rxSetEventSensDims()`/`rxEventSensLoadModel()`)
// based on the model's `mv$indLin`.
int _rxEsUseCalcJac = 0;
// Aliases exposed to handle_evid (extern in rxode2.h).  Distinct names from the
// local `dydt`/`dLag` globals so the widely-used `dydt` identifier is not shadowed
// in the many TUs that include handle_evid.  Point at the model functions in
// rxUpdateFuns().
t_dLag dLagEs = NULL;
t_dRate dRateEs = NULL;
t_dDur dDurEs = NULL;
t_dF d2FEs = NULL;
t_dLag d2LagEs = NULL;
t_dRate d2RateEs = NULL;
t_dDur d2DurEs = NULL;
t_dF d3FEs = NULL;
// Phase H1's dtau/lag row: d(F)/dq (q in calcSens2's index space) and the
// total derivative of the physical Jacobian column d(J[k][c])/dq.
t_dF dFQEs = NULL;
t_dLag dLagJacEs = NULL;
// Safety guard for the dtau/lag row's 2nd-order piece: nonzero at (cmt,q)
// means q ALSO drives this alag, the case not yet handled (see
// `.rxEventSensDerivs()`'s "lagQ" table).
t_dLag dLagQEs = NULL;
// Modeled-DUR continuous-forcing 2nd-order piece: d(dur)/dq (q in calcSens2's
// index space), avoiding a calcSens2-position -> calcSens-position
// cross-index map (see `.rxEventSensDerivs()`'s "durQ" table).
t_dDur dDurQEs = NULL;
t_DUR durEsFn = NULL;
t_dydt dydtEs = NULL;

extern "C" SEXP _rxode2_setEventSensDims(SEXP active, SEXP nState, SEXP nParam, SEXP nParam2) {
  _rxEsActive = INTEGER(active)[0];
  _rxEsNState = INTEGER(nState)[0];
  _rxEsNParam = INTEGER(nParam)[0];
  _rxEsNParam2 = INTEGER(nParam2)[0];
  return R_NilValue;
}

extern "C" SEXP _rxode2_setEventSensUseCalcJac(SEXP useCalcJac) {
  _rxEsUseCalcJac = INTEGER(useCalcJac)[0];
  return R_NilValue;
}

extern "C" SEXP _rxode2_setEventSensNParam3(SEXP nParam3) {
  _rxEsNParam3 = INTEGER(nParam3)[0];
  return R_NilValue;
}

// C-callable (R_RegisterCCallable) for downstream packages (e.g. nlmixr2est's
// FOCEi) that solve a sensitivity model directly through ind_solve() without
// going through R's rxSolve()/.rxSetEventSensDims().  Points rxode2's event
// ("jump") sensitivity globals -- the dosing-derivative function pointers and
// the runtime dims -- at the supplied model (its `trans` vector) and activates
// the jumps.  The handle_evid jump blocks are additionally guarded by a
// compartment-count bound (<= neq), so leaving this active while solving a
// smaller model (no sensitivity compartments, e.g. the FOCEi pred model) is
// safe: those solves skip the injection automatically.
extern "C" void rxode2EventSensLoad(SEXP trans, int active, int nState, int nParam, int nParam2) {
  const char *lib = CHAR(STRING_ELT(trans, 0));
  const char *prefix = CHAR(STRING_ELT(trans, 2));
  const char *s_dydt = CHAR(STRING_ELT(trans, 3));
  const char *s_DUR = CHAR(STRING_ELT(trans, 17));
  char nm[300];
  dydtEs = (t_dydt) R_GetCCallable(lib, s_dydt);
  durEsFn = (t_DUR) R_GetCCallable(lib, s_DUR);
  snprintf(nm, 300, "%sdLag", prefix);  dLagEs  = (t_dLag)  R_GetCCallable(lib, nm);
  snprintf(nm, 300, "%sdF", prefix);    dF      = (t_dF)    R_GetCCallable(lib, nm);
  snprintf(nm, 300, "%sdRate", prefix); dRateEs = (t_dRate) R_GetCCallable(lib, nm);
  snprintf(nm, 300, "%sdDur", prefix);  dDurEs  = (t_dDur)  R_GetCCallable(lib, nm);
  snprintf(nm, 300, "%sd2F", prefix);   d2FEs   = (t_dF)    R_GetCCallable(lib, nm);
  snprintf(nm, 300, "%sd2Lag", prefix); d2LagEs = (t_dLag)  R_GetCCallable(lib, nm);
  snprintf(nm, 300, "%sd2Rate", prefix);d2RateEs= (t_dRate) R_GetCCallable(lib, nm);
  snprintf(nm, 300, "%sd2Dur", prefix); d2DurEs = (t_dDur)  R_GetCCallable(lib, nm);
  snprintf(nm, 300, "%sd3F", prefix);   d3FEs   = (t_dF)    R_GetCCallable(lib, nm);
  snprintf(nm, 300, "%sdFQ", prefix);   dFQEs   = (t_dF)    R_GetCCallable(lib, nm);
  snprintf(nm, 300, "%sdLagJac", prefix); dLagJacEs = (t_dLag) R_GetCCallable(lib, nm);
  snprintf(nm, 300, "%sdLagQ", prefix); dLagQEs = (t_dLag) R_GetCCallable(lib, nm);
  snprintf(nm, 300, "%sdDurQ", prefix); dDurQEs = (t_dDur) R_GetCCallable(lib, nm);
  _rxEsActive = active;
  _rxEsNState = nState;
  _rxEsNParam = nParam;
  _rxEsNParam2 = nParam2;
}

// Toggle only the active flag (cheap; for bracketing or for turning the jumps
// off after a run).  Pointers/dims are preserved.
extern "C" void rxode2EventSensSetActive(int active) {
  _rxEsActive = active;
}

// R .Call wrapper around rxode2EventSensLoad, so a downstream package's R code
// (e.g. nlmixr2est's FOCEi) can point the event-sensitivity globals at a
// sensitivity model just before handing off to a direct C++ solve loop.
extern "C" SEXP _rxode2_eventSensLoad(SEXP trans, SEXP active, SEXP nState,
                                      SEXP nParam, SEXP nParam2) {
  rxode2EventSensLoad(trans, INTEGER(active)[0], INTEGER(nState)[0],
                      INTEGER(nParam)[0], INTEGER(nParam2)[0]);
  return R_NilValue;
}

t_update_inis update_inis = NULL;

t_dydt_lsoda_dum dydt_lsoda_dum = NULL;

t_dydt_liblsoda dydt_liblsoda = NULL;

t_jdum_lsoda jdum_lsoda = NULL;

t_get_solve get_solve = NULL;

t_assignFuns assignFuns=NULL;

static inline void copyLinCmt(int *neq,
                              rx_solving_options_ind *ind, rx_solving_options *op,
                              double *yp) {
  if (op->numLin > 0) {
    // Here we are doing ODE solving OR only linear solving
    // save the values here
    std::copy(yp,
              yp + op->neq,
              ind->linCmtDummy);
    dydt(neq, ind->tout, ind->linCmtDummy, ind->linCmtDummy);
    std::copy(ind->linCmtSave,
              ind->linCmtSave + op->numLin + op->numLinSens,
              yp + op->linOffset);
  }
}

static inline void postSolve(int *neq, int *idid, int *rc, int *i, double *yp, const char** err_msg, int nerr, bool doPrint,
                             rx_solving_options_ind *ind, rx_solving_options *op, rx_solve *rx) {
  if (*idid <= 0) {
    if (err_msg != NULL) {
      int cid = -*idid-1;
      if (cid > 0 && cid < nerr){
        RSprintf("IDID=%d, %s\n", *idid, err_msg[-*idid-1]);
      }
      else {
        RSprintf("IDID=%d, unhandled exception\n", *idid);
      }
    }
    *rc = *idid;
    badSolveExit(*i);
  } else if (ind->err) {
    if (doPrint) printErr(ind->err, ind->id);
    /* RSprintf("IDID=%d, %s\n", istate, err_msg_ls[-*istate-1]); */
    *rc = -2019;
    // Bad Solve => NA
    badSolveExit(*i);
  } else {
    if (R_FINITE(rx->stateTrimU)){
      double top=fabs(rx->stateTrimU);
      for (int j = rxEffNeq(ind, op); j--;) {
        yp[j]= min(top,yp[j]);
      }
    }
    if (R_FINITE(rx->stateTrimL)){
      double bottom=rx->stateTrimL;
      for (int j = rxEffNeq(ind, op); j--;) {
        yp[j]= max(bottom,yp[j]);
      }
    }
  }
  ind->slvr_counter[0]++;
}

int global_jt = 2;
int global_mf = 22;
int global_debug = 0;

/* -----------------------------------------------------------------------
 * DLSODE-based solvers (method 106 = lsode/Adams, method 107 = bdf/BDF)
 *
 * DLSODE (Hindmarsh 1983, from ODEPACK) uses a fixed method:
 *   lsode (106): MF=10  -- Adams (nonstiff), variable order 1-12
 *   bdf   (107): MF=22  -- BDF (stiff), internally generated dense Jacobian
 *
 * DLSODE uses non-reentrant COMMON blocks -- NOT thread-safe.
 * Both methods always run single-threaded.
 *
 * DLSODE's F signature: F(NEQ, T, Y, YDOT, RPAR, IPAR) -- deSolve extension.
 * We bridge via rxode2_dlsode_F which drops RPAR/IPAR and passes full NEQ[].
 * JAC is a dummy (MF=10 = no Jacobian; MF=22 = internal finite-difference Jacobian).
 * ----------------------------------------------------------------------- */
extern "C" {
  void F77_NAME(dlsode)(void (*)(int*, double*, double*, double*, double*, int*),
                        int*, double*, double*, double*,
                        int*, double*, double*, int*, int*, int*,
                        double*, int*, int*, int*,
                        void (*)(int*, double*, double*, int*, int*, double*, int*, double*, int*),
                        int*, double*, int*);
}

/* Bridge: DLSODE calls F(NEQ, T, Y, YDOT, RPAR, IPAR).
 * rxode2's derivative uses F(NEQ, T, Y, YDOT) -- same NEQ array (solveid in NEQ[1]). */
static void rxode2_dlsode_F(int *neq, double *t, double *y, double *ydot,
                             double *rpar, int *ipar) {
  (void)rpar; (void)ipar;
  dydt_lsoda_dum(neq, t, y, ydot);
}

/* Dummy JAC -- never called for MF=10 or MF=22 (both use internal Jacobian). */
static void rxode2_dlsode_JAC(int *neq, double *t, double *y,
                               int *ml, int *mu, double *pd, int *nrowpd,
                               double *rpar, int *ipar) {
  (void)neq; (void)t; (void)y; (void)ml; (void)mu;
  (void)pd; (void)nrowpd; (void)rpar; (void)ipar;
}


extern "C" int _locateTimeIndex(double obs_time,  rx_solving_options_ind *ind);

extern "C" int handle_evidL(int evid, double *yp, double xout, int id, rx_solving_options_ind *ind);
extern "C" double _getDur(int l, rx_solving_options_ind *ind, int backward, unsigned int *p);

extern "C" void _rxode2_assignFuns2(rx_solve rx,
                                   rx_solving_options op,
                                   t_F f,
                                   t_LAG lag,
                                   t_RATE rate,
                                   t_DUR dur,
                                   t_calc_mtime mtime,
                                   t_ME me,
                                   t_IndF indf,
                                   t_getTime gettime,
                                   t_locateTimeIndex timeindex,
                                   t_handle_evidL handleEvid,
                                   t_getDur getdur) {
  rx_solve *rxp = getRxSolve_();
  rxp->fns.f = f;
  rxp->fns.lag = lag;
  rxp->fns.rate = rate;
  rxp->fns.dur = dur;
  rxp->fns.mtime = mtime;
  rxp->fns.me = me;
  rxp->fns.indf = indf;
  rxp->fns.gettime = gettime;
  rxp->fns.timeindex = timeindex;
  rxp->fns.handleEvid = handleEvid;
  rxp->fns.getdur = getdur;
}

void rxUpdateFuns(SEXP trans){
  const char *lib, *s_dydt, *s_calc_jac, *s_calc_lhs, *s_inis, *s_dydt_lsoda_dum, *s_dydt_jdum_lsoda,
    *s_ode_solver_solvedata, *s_ode_solver_get_solvedata, *s_dydt_liblsoda, *s_AMT, *s_LAG, *s_RATE,
    *s_DUR, *s_mtime, *s_assignFuns,
    *s_ME, *s_IndF;
  lib = CHAR(STRING_ELT(trans, 0));
  s_dydt = CHAR(STRING_ELT(trans, 3));
  s_calc_jac = CHAR(STRING_ELT(trans, 4));
  s_calc_lhs = CHAR(STRING_ELT(trans, 5));
  s_inis = CHAR(STRING_ELT(trans, 8));
  s_dydt_lsoda_dum = CHAR(STRING_ELT(trans, 9));
  s_dydt_jdum_lsoda = CHAR(STRING_ELT(trans, 10));
  s_ode_solver_solvedata = CHAR(STRING_ELT(trans, 11));
  s_ode_solver_get_solvedata = CHAR(STRING_ELT(trans, 12));
  s_dydt_liblsoda = CHAR(STRING_ELT(trans, 13));
  s_AMT=CHAR(STRING_ELT(trans,14));
  s_LAG=CHAR(STRING_ELT(trans, 15));
  s_RATE=CHAR(STRING_ELT(trans, 16));
  s_DUR=CHAR(STRING_ELT(trans, 17));
  s_mtime=CHAR(STRING_ELT(trans, 18));
  s_assignFuns=CHAR(STRING_ELT(trans, 19));
  s_ME=CHAR(STRING_ELT(trans, 20));
  s_IndF=CHAR(STRING_ELT(trans, 21));
  global_jt = 2;
  global_mf = 22;
  global_debug = 0;
  if (strcmp(CHAR(STRING_ELT(trans, 1)),"fulluser") == 0){
    global_jt = 1;
    global_mf = 21;
  } else {
    global_jt = 2;
    global_mf = 22;
  }
  calc_lhs =(t_calc_lhs) R_GetCCallable(lib, s_calc_lhs);
  dydt =(t_dydt) R_GetCCallable(lib, s_dydt);
  calc_jac =(t_calc_jac) R_GetCCallable(lib, s_calc_jac);
  // Event ("jump") sensitivity helpers: names are <prefix>dLag / <prefix>dF.
  // Derived from the prefix (trans[2]) rather than carried in the trans vector,
  // so the shared model-vars structure is unchanged.  Every model exports these
  // (trivial body when unused), so the lookup never misses.
  {
    const char *s_prefix = CHAR(STRING_ELT(trans, 2));
    char s_dLag[300], s_dF[300], s_dRate[300], s_dDur[300], s_d2F[300];
    char s_d2Lag[300], s_d2Rate[300], s_d2Dur[300], s_d3F[300];
    char s_dFQ[300], s_dLagJac[300], s_dLagQ[300], s_dDurQ[300];
    snprintf(s_dLag, 300, "%sdLag", s_prefix);
    snprintf(s_dF, 300, "%sdF", s_prefix);
    snprintf(s_dRate, 300, "%sdRate", s_prefix);
    snprintf(s_dDur, 300, "%sdDur", s_prefix);
    snprintf(s_d2F, 300, "%sd2F", s_prefix);
    snprintf(s_d2Lag, 300, "%sd2Lag", s_prefix);
    snprintf(s_d2Rate, 300, "%sd2Rate", s_prefix);
    snprintf(s_d2Dur, 300, "%sd2Dur", s_prefix);
    snprintf(s_d3F, 300, "%sd3F", s_prefix);
    snprintf(s_dFQ, 300, "%sdFQ", s_prefix);
    snprintf(s_dLagJac, 300, "%sdLagJac", s_prefix);
    snprintf(s_dLagQ, 300, "%sdLagQ", s_prefix);
    snprintf(s_dDurQ, 300, "%sdDurQ", s_prefix);
    dLag = (t_dLag) R_GetCCallable(lib, s_dLag);
    dF = (t_dF) R_GetCCallable(lib, s_dF);
    dLagEs = dLag;   // expose to handle_evid (jump sensitivities)
    dRateEs = (t_dRate) R_GetCCallable(lib, s_dRate);
    dDurEs = (t_dDur) R_GetCCallable(lib, s_dDur);
    d2FEs = (t_dF) R_GetCCallable(lib, s_d2F);
    d2LagEs = (t_dLag) R_GetCCallable(lib, s_d2Lag);
    d2RateEs = (t_dRate) R_GetCCallable(lib, s_d2Rate);
    d2DurEs = (t_dDur) R_GetCCallable(lib, s_d2Dur);
    d3FEs = (t_dF) R_GetCCallable(lib, s_d3F);
    dFQEs = (t_dF) R_GetCCallable(lib, s_dFQ);
    dLagJacEs = (t_dLag) R_GetCCallable(lib, s_dLagJac);
    dLagQEs = (t_dLag) R_GetCCallable(lib, s_dLagQ);
    dDurQEs = (t_dDur) R_GetCCallable(lib, s_dDurQ);
    dydtEs = dydt;
  }
  update_inis =(t_update_inis) R_GetCCallable(lib, s_inis);
  dydt_lsoda_dum =(t_dydt_lsoda_dum) R_GetCCallable(lib, s_dydt_lsoda_dum);
  jdum_lsoda =(t_jdum_lsoda) R_GetCCallable(lib, s_dydt_jdum_lsoda);
  set_solve = (t_set_solve)R_GetCCallable(lib, s_ode_solver_solvedata);
  get_solve = (t_get_solve)R_GetCCallable(lib, s_ode_solver_get_solvedata);
  dydt_liblsoda = (t_dydt_liblsoda)R_GetCCallable(lib, s_dydt_liblsoda);
  t_F AMT = (t_F)R_GetCCallable(lib, s_AMT);
  t_LAG LAG = (t_LAG) R_GetCCallable(lib, s_LAG);
  t_RATE RATE = (t_RATE) R_GetCCallable(lib, s_RATE);
  t_DUR DUR = (t_DUR) R_GetCCallable(lib, s_DUR);
  durEsFn = DUR;   // expose duration to handle_evid (modeled-dur jump sensitivities)
  t_ME ME  = (t_ME) R_GetCCallable(lib, s_ME);
  t_IndF IndF  = (t_IndF) R_GetCCallable(lib, s_IndF);
  t_calc_mtime calc_mtime = (t_calc_mtime) R_GetCCallable(lib, s_mtime);
  assignFuns = (t_assignFuns)R_GetCCallable(lib, s_assignFuns);
  rx_solve *rx=(&rx_global);
  rx->subjects = inds_global;
  rx_solving_options *op = &op_global;
  rx->op = op;
  char s_assignFuns2[300];
  snprintf(s_assignFuns2, 300, "%s2", s_assignFuns);
  rxode2_assignFuns2_t assignFuns2 = (rxode2_assignFuns2_t)R_GetCCallable(lib, s_assignFuns2);
  if (assignFuns2 != NULL) {
    // The generated model's __assignFuns2 may call back into rxode2 via a
    // cached static function pointer.  That pointer becomes stale when rxode2
    // is reloaded (e.g. by pkgload::load_all inside devtools::test), causing
    // the callback to write to the OLD rx_global instead of the current one.
    // Call it for external-package side-effects only; then unconditionally
    // overwrite rx->fns below so the current rx_global is always correct.
    assignFuns2(rx_global, op_global, AMT, LAG, RATE, DUR, calc_mtime, ME, IndF, getTime, _locateTimeIndex, handle_evidL, _getDur);
  }
  // Always set rx->fns directly so the current rx_global has valid pointers
  // regardless of whether assignFuns2 was called and regardless of whether
  // its internal cached callable was stale.
  rx->fns.f = AMT;
  rx->fns.lag = LAG;
  rx->fns.rate = RATE;
  rx->fns.dur = DUR;
  rx->fns.mtime = calc_mtime;
  rx->fns.me = ME;
  rx->fns.indf = IndF;
  rx->fns.gettime = getTime;
  rx->fns.timeindex = _locateTimeIndex;
  rx->fns.handleEvid = handle_evidL;
  rx->fns.getdur = _getDur;
}

extern "C" void rxClearFuns(){
  calc_lhs		= NULL;
  dydt			= NULL;
  calc_jac		= NULL;
  update_inis		= NULL;
  dydt_lsoda_dum	= NULL;
  jdum_lsoda		= NULL;
  set_solve		= NULL;
  get_solve		= NULL;
  dydt_liblsoda		= NULL;
  rx_global.fns.f = NULL;
  rx_global.fns.lag = NULL;
  rx_global.fns.rate = NULL;
  rx_global.fns.dur = NULL;
  rx_global.fns.mtime = NULL;
  rx_global.fns.me = NULL;
  rx_global.fns.indf = NULL;
  rx_global.fns.gettime = NULL;
  rx_global.fns.timeindex = NULL;
  rx_global.fns.handleEvid = NULL;
  rx_global.fns.getdur = NULL;
}

extern "C" void F77_NAME(dlsoda)(
                                 void (*)(int *, double *, double *, double *),
                                 int *, double *, double *, double *, int *, double *, double *,
                                 int *, int *, int *, double *,int *,int *, int *,
                                 void (*)(int *, double *, double *, int *, int *, double *, int *),
                                 int *);

static inline void linSolve(int *neq, rx_solving_options_ind *ind,
                            double *yp, double *xp, double xout) {
  // there is no ODEs when using linSolve
  rx_solve *rx = &rx_global;
  rx_solving_options *op = &op_global;

  // The assumption here is that the linear solver does not depend on
  // state values.
  //
  // This means that the clearances do not depend on
  // depot values.  To ensure this, the values while solving are set to NA
  //
  dydt(neq, xout, rx->ypNA, ind->linCmtDummy);
  std::copy(ind->linCmtSave,
            ind->linCmtSave + op->numLin + op->numLinSens,
            yp + op->linOffset);
  // The values **could** depend on state, though it would require a
  // recursive solve and currently isn't implemented.  It also would
  // no longer be a linear compartment model :)
}

extern "C" rx_solve *getRxSolve2_(){
  return &rx_global;
}

extern "C" rx_solve *getRxSolve_(){
  rx_solve *rx = &rx_global;
  rx->subjects = inds_global;
  rx->op = &op_global;
  return &rx_global;
}

extern "C" double getTime(int idx, rx_solving_options_ind *ind) {
  return getTime__(idx, ind, 0);
}

// Re-sorts ind->ix[startI..n_all_times-1] using ind->timeThread for times.
// timeThread must be current for all raw indices at positions >= startI.
static inline void reSortMainTimeline(rx_solving_options_ind *ind, int startI) {
  double *time = ind->timeThread;
  int    *evid = ind->evid;
  SORT(ind->ix + startI, ind->ix + ind->n_all_times,
       [time, evid](int a, int b) -> bool {
         double ta = time[a], tb = time[b];
         if (ta != tb) return ta < tb;
         // Match etTrans() ordering: higher evid (doses) sort before lower evid (obs)
         // etTran() negates evid so that larger evid values become more negative;
         // sort first
         int ea = evid[a], eb = evid[b];
         // Reset events (evid==3) must sort BEFORE dose events at the same time
         // to preserve evid=4 (reset+dose) semantics: reset zeroes compartments,
         // then dose is applied -- not the other way around.
         if (ea == 3 && eb != 3) return true;
         if (eb == 3 && ea != 3) return false;
         // Otherwise: higher evid (doses) before lower evid (obs) -- matches etTrans()
         if (ea != eb) return ea > eb;
         return a < b;
       });
}

// Re-sorts ind->idose[startDose..ndoses-1] by time (for pushed future doses).
static inline void _rxSortIdoseSuffix(rx_solving_options_ind *ind, int startDose) {
  if (startDose >= ind->ndoses) return;
  double *time = ind->timeThread;
  int    *idose = ind->idose;
  SORT(idose + startDose, idose + ind->ndoses,
       [time](int a, int b) -> bool {
         return time[a] < time[b];
       });
}

// Push a current/future event into the individual's own event arrays during ODE solving.
// _curTime: current ODE model time (for past-time guard with solver tolerance).
// Returns 1 on success, 0 if ignored (past time or unknown evid), -1 on alloc failure.
extern "C" int _rxPushDose(rx_solving_options_ind *_ind, double _curTime,
                           double _time, int _evid, double _amt, int _cmt,
                           double _rate, double _ii, int _addl, int _ss,
                           int _isDur) {
  rx_solving_options *op = &op_global;
  int _isDurFlag = _isDur & 1;

  if (!_ind->indOwnAlloc) return 0; // safety: only works with owned arrays

  rx_solve *rx = &rx_global;

  // Loop over addl+1 doses: dose 0 uses the given ss, subsequent doses use ss=0
  int nDosesToPush = (_addl > 0 && _ii > 0) ? _addl + 1 : 1;
  int anyPushed = 0;
  for (int _rep = 0; _rep < nDosesToPush; _rep++) {
    double _doseTime = _time + _rep * _ii;
    int    _doseSs   = (_rep == 0) ? _ss : 0;

    if (isSameTimeOp(_doseTime, _curTime)) {
      if (_doseTime < _curTime) {
        // Dose at same time as current time, BUT marginally before curTime;
        // assign to be the same.
        _doseTime = _curTime;
      }
    } else if (_doseTime < _curTime) {
#pragma omp atomic
      nPastEvid_global++;
      continue;
    }

    // Each addl repetition is a standalone event; ii=0 so the solver does not
    // auto-schedule further repeats.  For the first dose (rep==0) with SS, we
    // pass the original _ii so flg is set correctly; for all others ii=0.
    double _doseIi = (_rep == 0 && _doseSs != 0) ? _ii : 0.0;
    rx_translated_event ev = _rxTranslateOneEvent(_doseTime, _evid, _cmt,
                                                  _amt, _doseIi, _doseSs,
                                                  _rate, _isDurFlag);
    if (ev.n == 0) continue;

    int splitDoseEvent = -1;
    if (rx->splitBolus != NULL && rx->splitBolusN >= 2 && _cmt == rx->splitBolus[0]) {
      for (int _k = 0; _k < ev.n; _k++) {
        if (_rxShouldSplitTranslatedBolus(ev.evid[_k], _cmt, _amt, rx->splitBolus[0])) {
          splitDoseEvent = _k;
          break;
        }
      }
    }

    int nDose = 0;
    for (int _k = 0; _k < ev.n; _k++) {
      if (ev.isDose[_k]) {
        nDose += (_k == splitDoseEvent) ? rx->splitBolusN - 1 : 1;
      }
    }

    // Check per-individual push limit (maxExtra > 0 enables the guard).
    _ind->nPushedExtra++;
    if (rx->maxExtra > 0 && _ind->nPushedExtra > rx->maxExtra) {
      int bad = 1;
#pragma omp atomic write
      rx->extraPushAbort = bad;
#pragma omp atomic write
      op->badSolve = bad;
      return -1;
    }

    // Grow main event arrays if needed.
    // NOTE: ind->solve is intentionally NOT reallocated here.  _rxPushDose can
    // be called from within the dydt callback while an ODE integrator (lsoda/dop)
    // is actively using yp = getSolve(i) = ind->solve + neq*i.  Reallocating
    // ind->solve here would free that buffer under the integrator's feet, causing
    // a use-after-free / heap corruption.  The pushed events all have future
    // times; the current solver loop uses a cached nx = n_all_times and never
    // accesses getSolve(j) for j >= nx.  The solve array is grown to the correct
    // size by rxAllocInd() at the start of the next rxSolve() call.
    if (_ind->n_all_times + ev.n > _ind->indOwnAllocN) {
      int newCap = _ind->n_all_times + ev.n + EVID_EXTRA_SIZE;
      // dose, all_times, ii, evid: allocate newCap+1 so that the [idx+1]
      // "plus-one" macros (setDoseP1, getDoseP1, setAllTimesP1, getAllTimesP1,
      // getEvidP1) are always within bounds when idx == n_all_times-1.
      double *a   = (double*)realloc(_ind->all_times,  newCap * sizeof(double));
      double *d   = (double*)realloc(_ind->dose,       newCap * sizeof(double));
      double *i2  = (double*)realloc(_ind->ii,         newCap * sizeof(double));
      int    *ev2 = (int*)   realloc(_ind->evid,       newCap * sizeof(int));
      int    *ix  = (int*)   realloc(_ind->ix,         newCap * sizeof(int));
      double *tt  = (double*)realloc(_ind->timeThread, newCap * sizeof(double));
      if (!a || !d || !i2 || !ev2 || !ix || !tt) {
        int bad = 1;
#pragma omp atomic write
        op->badSolve = bad;
        return -1;
      }
      _ind->all_times  = a;  _ind->dose = d;  _ind->ii = i2;
      _ind->evid       = ev2; _ind->ix  = ix; _ind->timeThread = tt;
      _ind->indOwnAllocN = newCap;
      // Zero guard elements (solve slots are grown on next rxAllocInd call)
      memset(a + _ind->n_all_times, 0, (newCap - _ind->n_all_times) * sizeof(double));
      memset(d + _ind->n_all_times, 0, (newCap - _ind->n_all_times) * sizeof(double));
      memset(i2 + _ind->n_all_times, 0, (newCap - _ind->n_all_times) * sizeof(double));
      memset(ev2 + _ind->n_all_times, 0, (newCap - _ind->n_all_times) * sizeof(int));
    }

    // Grow idose if needed
    if (nDose > 0 && _ind->ndoses + nDose > _ind->idoseOwnAllocN) {
      int newCap = _ind->ndoses + nDose + EVID_EXTRA_SIZE;
      int *id = (int*)realloc(_ind->idose, (newCap + 1) * sizeof(int));
      if (!id) {
        int bad = 1;
#pragma omp atomic write
        op->badSolve = bad;
        return -1;
      }
      memset(id + _ind->ndoses, 0, (newCap - _ind->ndoses) * sizeof(int));
      _ind->idose          = id;
      _ind->idoseOwnAllocN = newCap;
    }

    // Append the translated events
    int doseSuffix = _ind->ndoses; // idose sort start
    int rawStart = _ind->n_all_times;
    for (int _k = 0; _k < ev.n; _k++) {
      int splitStart = (_k == splitDoseEvent) ? 1 : 0;
      int splitEnd = (_k == splitDoseEvent) ? rx->splitBolusN : 1;
      for (int _s = splitStart; _s < splitEnd; _s++) {
        int rawIdx = _ind->n_all_times;
        int curEvid = ev.evid[_k];
        if (_k == splitDoseEvent) {
          curEvid = _rxEncodeEventCmt(ev.evid[_k], rx->splitBolus[_s]);
        }
        _ind->all_times[rawIdx]  = ev.time[_k];
        _ind->dose[rawIdx]       = ev.amt[_k];
        _ind->ii[rawIdx]         = ev.ii[_k];
        _ind->evid[rawIdx]       = curEvid;
        _ind->timeThread[rawIdx] = ev.time[_k];
        _ind->ix[rawIdx]         = rawIdx;
        _ind->n_all_times++;
        if (ev.isDose[_k]) {
          _ind->idose[_ind->ndoses++] = rawIdx;
        }
      }
    }

    int savedIdx = _ind->idx;
    int savedWh = _ind->wh, savedCmt = _ind->cmt, savedWh100 = _ind->wh100,
      savedWhI = _ind->whI, savedWh0 = _ind->wh0;
    for (int rawIdx = rawStart; rawIdx < _ind->n_all_times; rawIdx++) {
      _ind->idx = rawIdx;
      _ind->timeThread[rawIdx] = getTime__(rawIdx, _ind, 1);
    }
    _ind->idx = savedIdx;
    _ind->wh = savedWh; _ind->cmt = savedCmt; _ind->wh100 = savedWh100;
    _ind->whI = savedWhI; _ind->wh0 = savedWh0;

    // Re-sort ix from current event forward so new events land in the right slots
    int sortStart = (_ind->idx >= 0 && _ind->idx < _ind->n_all_times)
                    ? _ind->idx : 0;
    reSortMainTimeline(_ind, sortStart);

    // Re-sort unprocessed idose suffix
    _rxSortIdoseSuffix(_ind, doseSuffix < _ind->ixds ? _ind->ixds : doseSuffix);
    anyPushed = 1;
  } // end addl loop

  return anyPushed ? 1 : 0;
}

// Recomputes ind->mtime[k] with current state yp when the solver is exactly at
// the original sort-time mtime0[k].  Only fires once per mtime slot (mtime0[k]
// is set to R_NegInf after firing to prevent double re-evaluation).
// Updates timeThread for changed mtime events in ix[nextI..n_all_times-1].
// Returns 1 if any mtime value changed (re-sort needed), 0 otherwise.
static inline int recomputeMtimeIfNeeded(rx_solve *rx,
                                         rx_solving_options_ind *ind,
                                         double *yp, int nextI, double xout) {
  if (rx->nMtime == 0) return 0;
  int nm = rx->nMtime;
  // Check whether we are at the original time of any pending mtime slot.
  int needEval = 0;
  for (int k = 0; k < nm; k++) {
    if (ind->mtime0[k] != R_NegInf && isSameTime(xout, ind->mtime0[k])) {
      needEval = 1;
      break;
    }
  }
  if (!needEval) return 0;
  // Evaluate all mtime slots with current state into a temporary buffer so we
  // can selectively apply only the slot(s) whose trigger time has arrived.
  double newMtime[90];
  for (int k = 0; k < nm; k++) newMtime[k] = ind->mtime[k];
  if (ind->fns && ind->fns->mtime) ind->fns->mtime(ind->id, newMtime, yp);
  int changed = 0;
  double *time = ind->timeThread;
  for (int k = 0; k < nm; k++) {
    if (ind->mtime0[k] == R_NegInf) continue;          // already fired
    if (!isSameTime(xout, ind->mtime0[k])) continue;   // not yet at trigger time
    // Lock in the one-time re-evaluated value and update timeThread.
    if (newMtime[k] != ind->mtime[k]) {
      ind->mtime[k] = newMtime[k];
      for (int j = nextI; j < ind->n_all_times; j++) {
        int raw = ind->ix[j];
        int evid = getEvid(ind, raw);
        if (evid == k + 10) {
          time[raw] = ind->mtime[k];
          changed = 1;
        }
      }
    }
    ind->mtime0[k] = R_NegInf; // mark fired; no further re-evaluation
  }
  return changed;
}

// Recomputes timeThread for remaining non-stop dose events using current state yp.
// Guards on needSortAlag; saves/restores ind wh fields to avoid side effects.
// Returns 1 if any lag-adjusted time changed (re-sort needed), 0 otherwise.
//
// NOTE: The generated RxLag function returns (t + _alag[cmt] - ind->curShift).
// sortInd runs with curShift=0, so timeThread[raw] = all_times[raw] + lag.
// At runtime after a reset curShift != 0; we must zero it temporarily so
// getLag gives the same sort-key convention as sortInd.
static inline int refreshLagTimesIfNeeded(rx_solve *rx,
                                          rx_solving_options_ind *ind,
                                          double *yp, int nextI,
                                          double xout) {
  if (!(rx->needSort & needSortAlag)) return 0;
  rx_solving_options *op = &op_global;
  double *time = ind->timeThread;
  int changed = 0;
  int savedIdx = ind->idx;
  int savedWh = ind->wh, savedCmt = ind->cmt, savedWh100 = ind->wh100,
      savedWhI = ind->whI, savedWh0 = ind->wh0;
  // Zero curShift so getLag matches the sort-time convention (sortInd uses curShift=0).
  double savedCurShift = ind->curShift;
  ind->curShift = 0.0;
  for (int j = nextI; j < ind->n_all_times; j++) {
    int raw = ind->ix[j];
    int evid = getEvid(ind, raw);
    if (isObs(evid) || evid == 9) continue;
    if (evid == 3) continue;  // reset event: cmt=-1 would cause _alag[-1] UB in RxLag
    if (evid >= 10 && evid <= 99) continue;  // mtime handled by recomputeMtimeIfNeeded
    int wh, cmt, wh100, whI, wh0;
    getWh(evid, &wh, &cmt, &wh100, &whI, &wh0);
    // Stop events store absolute times; lag must not be re-applied
    if (whI == EVIDF_MODEL_RATE_OFF || whI == EVIDF_MODEL_DUR_OFF) continue;
    // Fixed-rate infusions (EVIDF_INF_RATE): stop event sort key is lagged_start + f*dur,
    // not raw_stop + lag. Skip all EVIDF_INF_RATE events to avoid corrupting stop times.
    if (whI == EVIDF_INF_RATE) continue;
    // Set ind->idx to j (ix[] position) so _update_par_ptr reads covariates at the
    // correct event position (getValue uses y[ind->ix[ind->idx]] = y[raw]).
    ind->idx = j;
    ind->wh = wh; ind->cmt = cmt; ind->wh100 = wh100; ind->whI = whI; ind->wh0 = wh0;
    double rawTime = getAllTimes(ind, raw);
    // Only recompute a dose's lag when the solver is exactly at the dose's original
    // (raw) time. At that moment yp holds the state at rawTime, giving the correct
    // lag = f(state at dosing time). Updating before or after the raw time would use
    // a different state (e.g. pushing a pending dose forward on every observation).
    if (!isSameTime(rawTime, xout)) continue;
    double newTime = getLag(ind, ind->id, cmt, rawTime, yp);
    if (!ISNA(newTime) && newTime != time[raw]) {
      time[raw] = newTime;
      changed = 1;
    }
  }
  ind->curShift = savedCurShift;
  ind->idx = savedIdx;
  ind->wh = savedWh; ind->cmt = savedCmt; ind->wh100 = savedWh100;
  ind->whI = savedWhI; ind->wh0 = savedWh0;
  return changed;
}

// Adapted from
extern "C" void sortInd(rx_solving_options_ind *ind) {
// #ifdef _OPENMP
//   int core = omp_get_thread_num();
// #else
//   int core = 0;
// #endif
  rx_solve *rx = &rx_global;
  rx_solving_options *op = &op_global;
  // Reset times for infusion
  int doSort = 1;
  double *time = ind->timeThread;
  ind->ixds = 0;
  ind->curShift = 0;
  for (int i = 0; i < ind->n_all_times; i++) {
    ind->ix[i] = i;
    ind->idx = i;
    if (!isObs(getEvid(ind, i))) {
      time[i] = getTime__(ind->ix[i], ind, 1);
      ind->ixds++;
    } else {
      if (getEvid(ind, i) == 3) {
        ind->curShift -= rx->maxShift;
      }
      time[i] = getTime__(ind->ix[i], ind, 1);
    }
    if (op->naTime != 0) {
      doSort=0;
      break;
    }
  }
  if (doSort) {
    reSortMainTimeline(ind, 0);
  }
}

extern "C" int iniSubjectE(int solveid, int inLhs, rx_solving_options_ind *ind, rx_solving_options *op, rx_solve *rx,
                           t_update_inis u_inis) {
  return iniSubject(solveid, inLhs, ind, op, rx, u_inis);
}

static void chkIntFn(void *dummy) {
  R_CheckUserInterrupt();
}

int checkInterrupt() {
  return (R_ToplevelExec(chkIntFn, NULL) == FALSE);
}

static const char *err_msg_ls[] =
  {
    "excess work done on this call (perhaps wrong jt).",
    "excess accuracy requested (tolerances too small).",
    "illegal input detected (see printed message).",
    "repeated error test failures (check all inputs).",
    "repeated convergence failures (perhaps bad jacobian supplied or wrong choice of jt or tolerances).",
    "error weight became zero during problem. (solution component i vanished, and atol or atol(i) = 0.)",
    "work space insufficient to finish (see messages)."
  };

//dummy solout fn
extern "C" void solout(long int nr, double t_old, double t, double *y, int *nptr,
                       dop853_ctx_t *ctx, void *userdata, int *irtrn){}

extern "C" int indLin(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                      double tp, double *yp_, double tf,
		      double *InfusionRate_, int *on_,
		      t_ME ME, t_IndF  IndF);

extern "C" void rkf78_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rk4_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void ck54_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void ab_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void abm_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void dop5_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void bs_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void ros4_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void iem_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void sem_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void sb3a_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void sb3am4_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void vv_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void mm_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void em_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void trapz_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void ssp3_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkf32_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rk43_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void dop54_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void vern65_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void vern76_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void dop87_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void vern98_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void grk4a_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void ind_grk4a(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void par_grk4a(rx_solve *rx);
extern "C" void ros6_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void backwardEuler_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void gauss6_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void iiic6_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void radauiia5_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void geng5_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void sdirk43_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void euler_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void midpoint_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void heun_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkssp22_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rk3_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkssp53_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rks4_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkr4_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkls44_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkls54_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkssp54_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rks5_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rk5_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkc5_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkl5_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rklk5a_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rklk5b_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkb6_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rk7_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rk8_10_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkcv8_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rk8_12_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rks10_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkz10_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rko10_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkh10_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkbs32_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkssp43_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkf45_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkt54_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rks54_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkpp54_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkpp54b_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkbs54_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkss54_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkdp65_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkc65_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rktp64_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkv65r_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkv65_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void dverk65_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rktf65_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rktp75_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rktmy7_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rktmy7s_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkv76r_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkss76_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkv78_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void dverk78_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkdp85_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rktp86_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkv87e_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkv87r_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkev87_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkk87_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkf89_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkv89_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkt98a_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkv98r_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rks98_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkf108_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkc108_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkb109_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rks1110a_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkf1210_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rko129_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void rkf1412_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind);
extern "C" void ind_rkbs32(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkssp43(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkf45(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkt54(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rks54(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkpp54(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkpp54b(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkbs54(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkss54(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkdp65(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkc65(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rktp64(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkv65r(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkv65(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_dverk65(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rktf65(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rktp75(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rktmy7(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rktmy7s(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkv76r(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkss76(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkv78(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_dverk78(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkdp85(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rktp86(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkv87e(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkv87r(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkev87(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkk87(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkf89(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkv89(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkt98a(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkv98r(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rks98(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkf108(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkc108(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkb109(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rks1110a(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkf1210(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rko129(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkf1412(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void par_rkbs32(rx_solve *rx);
extern "C" void par_rkssp43(rx_solve *rx);
extern "C" void par_rkf45(rx_solve *rx);
extern "C" void par_rkt54(rx_solve *rx);
extern "C" void par_rks54(rx_solve *rx);
extern "C" void par_rkpp54(rx_solve *rx);
extern "C" void par_rkpp54b(rx_solve *rx);
extern "C" void par_rkbs54(rx_solve *rx);
extern "C" void par_rkss54(rx_solve *rx);
extern "C" void par_rkdp65(rx_solve *rx);
extern "C" void par_rkc65(rx_solve *rx);
extern "C" void par_rktp64(rx_solve *rx);
extern "C" void par_rkv65r(rx_solve *rx);
extern "C" void par_rkv65(rx_solve *rx);
extern "C" void par_dverk65(rx_solve *rx);
extern "C" void par_rktf65(rx_solve *rx);
extern "C" void par_rktp75(rx_solve *rx);
extern "C" void par_rktmy7(rx_solve *rx);
extern "C" void par_rktmy7s(rx_solve *rx);
extern "C" void par_rkv76r(rx_solve *rx);
extern "C" void par_rkss76(rx_solve *rx);
extern "C" void par_rkv78(rx_solve *rx);
extern "C" void par_dverk78(rx_solve *rx);
extern "C" void par_rkdp85(rx_solve *rx);
extern "C" void par_rktp86(rx_solve *rx);
extern "C" void par_rkv87e(rx_solve *rx);
extern "C" void par_rkv87r(rx_solve *rx);
extern "C" void par_rkev87(rx_solve *rx);
extern "C" void par_rkk87(rx_solve *rx);
extern "C" void par_rkf89(rx_solve *rx);
extern "C" void par_rkv89(rx_solve *rx);
extern "C" void par_rkt98a(rx_solve *rx);
extern "C" void par_rkv98r(rx_solve *rx);
extern "C" void par_rks98(rx_solve *rx);
extern "C" void par_rkf108(rx_solve *rx);
extern "C" void par_rkc108(rx_solve *rx);
extern "C" void par_rkb109(rx_solve *rx);
extern "C" void par_rks1110a(rx_solve *rx);
extern "C" void par_rkf1210(rx_solve *rx);
extern "C" void par_rko129(rx_solve *rx);
extern "C" void par_rkf1412(rx_solve *rx);
extern "C" void ind_ros6(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_backwardEuler(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_gauss6(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_iiic6(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_radauiia5(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_geng5(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_sdirk43(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_euler(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_midpoint(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_heun(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkssp22(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rk3(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkssp53(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rks4(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkr4(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkls44(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkls54(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkssp54(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rks5(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rk5(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkc5(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkl5(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rklk5a(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rklk5b(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkb6(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rk7(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rk8_10(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkcv8(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rk8_12(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rks10(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkz10(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rko10(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void ind_rkh10(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis);
extern "C" void par_ros6(rx_solve *rx);
extern "C" void par_backwardEuler(rx_solve *rx);
extern "C" void par_gauss6(rx_solve *rx);
extern "C" void par_iiic6(rx_solve *rx);
extern "C" void par_radauiia5(rx_solve *rx);
extern "C" void par_geng5(rx_solve *rx);
extern "C" void par_sdirk43(rx_solve *rx);
extern "C" void par_euler(rx_solve *rx);
extern "C" void par_midpoint(rx_solve *rx);
extern "C" void par_heun(rx_solve *rx);
extern "C" void par_rkssp22(rx_solve *rx);
extern "C" void par_rk3(rx_solve *rx);
extern "C" void par_rkssp53(rx_solve *rx);
extern "C" void par_rks4(rx_solve *rx);
extern "C" void par_rkr4(rx_solve *rx);
extern "C" void par_rkls44(rx_solve *rx);
extern "C" void par_rkls54(rx_solve *rx);
extern "C" void par_rkssp54(rx_solve *rx);
extern "C" void par_rks5(rx_solve *rx);
extern "C" void par_rk5(rx_solve *rx);
extern "C" void par_rkc5(rx_solve *rx);
extern "C" void par_rkl5(rx_solve *rx);
extern "C" void par_rklk5a(rx_solve *rx);
extern "C" void par_rklk5b(rx_solve *rx);
extern "C" void par_rkb6(rx_solve *rx);
extern "C" void par_rk7(rx_solve *rx);
extern "C" void par_rk8_10(rx_solve *rx);
extern "C" void par_rkcv8(rx_solve *rx);
extern "C" void par_rk8_12(rx_solve *rx);
extern "C" void par_rks10(rx_solve *rx);
extern "C" void par_rkz10(rx_solve *rx);
extern "C" void par_rko10(rx_solve *rx);
extern "C" void par_rkh10(rx_solve *rx);


/* Run one ODE interval with the specified method code.
   On entry:  *xp = interval start time; yp = current state.
   On return: *xp = xout (if successful), yp = updated state.
   *istate <= 0 or ind->rc[0] == -2019 signals failure.
   *idid is set for dop853 (method==0); positive = success, -4 = stiff detected.
   autoSwitchPrimary: when true, dop853 uses nstiff=50 to enable internal stiffness test. */
static inline void _rxSolveOneInterval(int method, bool autoSwitchPrimary,
                                       int *neq, double *yp, double *xp,
                                       double xout, int *istate, int *idid,
                                       rx_solving_options *op,
                                       rx_solving_options_ind *ind,
                                       int *i, void *ctx,
                                       int eff) {
  int itol = 0;
  // Single-interval / steady-state sub-solves advance the FORWARD primal only.
  // The discrete-adjoint method variants use method code = base + 200 (e.g.
  // rk4s=206 -> rk4=6, liblsodaadj=202 -> liblsoda=2, dop853s=200 -> dop853=0);
  // here they reuse their base method's single-point stepper (the backward
  // sweep is driven separately by the adjoint driver).  Without this the
  // steady-state pre-solve had no matching case and left yp un-advanced,
  // giving divergent ss results.  All base codes are < 200, so the test
  // cleanly identifies adjoint codes.
  if (method >= 200) method -= 200;
  switch (method) {
    case 3:
      if (!isSameTime(xout, *xp)) {
        preSolve(op, ind, *xp, xout, yp);
        *idid = indLin(ind->id, op, ind, *xp, yp, xout, ind->InfusionRate, ind->on,
                       (ind->fns ? ind->fns->me : NULL), (ind->fns ? ind->fns->indf : NULL));
      }
      if (*idid <= 0) {
        ind->rc[0] = *idid;
        badSolveExit(*i);
      } else if (ind->err){
        ind->rc[0] = *idid;
        badSolveExit(*i);
      }
      break;
    case 2:
      if (!isSameTime(xout, *xp)) {
        preSolve(op, ind, *xp, xout, yp);
        lsoda((lsoda_context_t*)ctx, yp, xp, xout);
        copyLinCmt(neq, ind, op, yp);
      }
      if (*istate <= 0) {
        RSprintf("IDID=%d, %s\n", *istate, err_msg_ls[-(*istate)-1]);
        ind->rc[0] = -2019;
        break;
      } else if (ind->err){
        printErr(ind->err, ind->id);
        ind->rc[0] = -2019;
        *i = ind->n_all_times-1;
        break;
      }
      break;
    case 5:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkf78_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 6:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rk4_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 7:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); ck54_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 8:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); ab_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 9:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); abm_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 10:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); dop5_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 11:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); bs_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 13:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); ros4_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 14:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); iem_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 15:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); sem_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 16:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); sb3a_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 17:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); sb3am4_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 18:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); vv_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 19:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); mm_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 20:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); em_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 21:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); cvode_solveWith1Pt(neq, yp, xp, xout, istate, op, ind, ctx); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 22:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); trapz_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 23:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); ssp3_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 24:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkf32_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 25:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rk43_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 26:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); dop54_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 27:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); vern65_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 28:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); vern76_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 29:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); dop87_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 30:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); vern98_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 31:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); grk4a_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 32:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); ros6_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 33:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); backwardEuler_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 34:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); gauss6_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 35:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); iiic6_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 36:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); radauiia5_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 37:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); geng5_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 38:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); sdirk43_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 39:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); euler_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 40:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); midpoint_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 41:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); heun_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 42:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkssp22_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 43:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rk3_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 44:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkssp53_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 45:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rks4_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 46:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkr4_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 47:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkls44_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 48:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkls54_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 49:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkssp54_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 50:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rks5_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 51:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rk5_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 52:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkc5_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 53:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkl5_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 54:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rklk5a_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 55:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rklk5b_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 56:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkb6_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 57:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rk7_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 58:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rk8_10_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 59:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkcv8_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 60:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rk8_12_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 61:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rks10_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 62:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkz10_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 63:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rko10_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 64:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkh10_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 65:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkbs32_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 66:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkssp43_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 67:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkf45_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 68:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkt54_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 69:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rks54_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 70:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkpp54_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 71:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkpp54b_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 72:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkbs54_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 73:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkss54_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 74:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkdp65_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 75:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkc65_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 76:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rktp64_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 77:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkv65r_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 78:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkv65_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 79:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); dverk65_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 80:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rktf65_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 81:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rktp75_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 82:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rktmy7_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 83:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rktmy7s_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 84:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkv76r_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 85:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkss76_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 86:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkv78_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 87:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); dverk78_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 88:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkdp85_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 89:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rktp86_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 90:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkv87e_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 91:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkv87r_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 92:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkev87_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 93:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkk87_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 94:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkf89_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 95:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkv89_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 96:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkt98a_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 97:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkv98r_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 98:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rks98_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 99:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkf108_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 100:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkc108_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 101:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkb109_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 102:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rks1110a_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 103:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkf1210_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 104:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rko129_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 105:
      if (!isSameTime(xout, *xp)) { preSolve(op, ind, *xp, xout, yp); rkf1412_solveWith1Pt(neq, yp, xp, xout, istate, op, ind); copyLinCmt(neq, ind, op, yp); }
      if (*istate <= 0) { ind->rc[0] = -2019; break; } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; } break;
    case 106:
      if (!isSameTime(xout, *xp)) {
        preSolve(op, ind, *xp, xout, yp);
        neq[0] = eff - op->numLin - op->numLinSens;
        {
          int _lrw = 22 + op->neq * max(16, op->neq + 9);
          int _liw = 20 + op->neq;
          int _itol = 1, _itask = 1, _iopt = 1, _mf = 10;
          double _rpar = 0.0; int _ipar = 0;
          F77_CALL(dlsode)(rxode2_dlsode_F, neq, yp, xp, &xout,
                           &_itol, &(op->RTOL), &(op->ATOL), &_itask,
                           istate, &_iopt, __rworkPool[0].rworkp,
                           &_lrw, __rworkPool[0].iworkp, &_liw,
                           rxode2_dlsode_JAC, &_mf, &_rpar, &_ipar);
        }
        neq[0] = eff;
        copyLinCmt(neq, ind, op, yp);
      }
      if (*istate <= 0) {
        RSprintf("IDID=%d, %s\n", *istate, err_msg_ls[-(*istate)-1]);
        ind->rc[0] = -2019;
        break;
      } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; }
      break;
    case 107:
      if (!isSameTime(xout, *xp)) {
        preSolve(op, ind, *xp, xout, yp);
        neq[0] = eff - op->numLin - op->numLinSens;
        {
          int _lrw = 22 + 9 * op->neq + 2 * op->neq * op->neq;
          int _liw = 20 + op->neq;
          int _itol = 1, _itask = 1, _iopt = 1, _mf = 22;
          double _rpar = 0.0; int _ipar = 0;
          F77_CALL(dlsode)(rxode2_dlsode_F, neq, yp, xp, &xout,
                           &_itol, &(op->RTOL), &(op->ATOL), &_itask,
                           istate, &_iopt, __rworkPool[0].rworkp,
                           &_lrw, __rworkPool[0].iworkp, &_liw,
                           rxode2_dlsode_JAC, &_mf, &_rpar, &_ipar);
        }
        neq[0] = eff;
        copyLinCmt(neq, ind, op, yp);
      }
      if (*istate <= 0) {
        RSprintf("IDID=%d, %s\n", *istate, err_msg_ls[-(*istate)-1]);
        ind->rc[0] = -2019;
        break;
      } else if (ind->err) { printErr(ind->err, ind->id); ind->rc[0] = -2019; break; }
      break;
    case 1:
      if (!isSameTime(xout, *xp)) {
        preSolve(op, ind, *xp, xout, yp);
        neq[0] = eff - op->numLin - op->numLinSens;
        {
          int _lrw = 22 + op->neq * max(16, op->neq + 9);
          int _liw = 20 + op->neq;
          int _itol = 1, _itask = 1, _iopt = 1;
          F77_CALL(dlsoda)(dydt_lsoda_dum, neq, yp, xp, &xout,
                           &_itol, &(op->RTOL), &(op->ATOL), &_itask,
                           istate, &_iopt, __rworkPool[0].rworkp,
                           &_lrw, __rworkPool[0].iworkp, &_liw, jdum_lsoda, &global_jt);
        }
        neq[0] = eff;
        copyLinCmt(neq, ind, op, yp);
      }
      if (*istate <= 0) {
        RSprintf("IDID=%d, %s\n", *istate, err_msg_ls[-(*istate)-1]);
        ind->rc[0] = -2019;
        break;
      } else if (ind->err){
        printErr(ind->err, ind->id);
        ind->rc[0] = -2019;
        break;
      }
      break;
    case 0:
      if (!isSameTimeDop(xout, *xp)) {
        preSolve(op, ind, *xp, xout, yp);
        neq[0] = eff - op->numLin - op->numLinSens;
        double _h0use = (ind->autoHcur > 0.0) ? ind->autoHcur : op->H0;
        *idid = dop853(neq,
                       dydt,
                       *xp,
                       yp,
                       xout,
                       &(op->RTOL),
                       &(op->ATOL),
                       itol,
                       solout,
                       0,
                       NULL,
                       DBL_EPSILON,
                       0,
                       0,
                       0,
                       0,
                       ind->HMAX,
                       _h0use,
                       op->mxstep,
                       1,
                       autoSwitchPrimary ? 50 : -1,
                       0,
                       NULL,
                       0,
                       NULL,
                       ind->id
                       );
        neq[0] = eff;
        copyLinCmt(neq, ind, op, yp);
      }
      if (*idid == -4) {
        /* stiffness detected: signal for AutoSwitch; not treated as fatal here */
        ind->rc[0] = -2019;
        break;
      }
      if (*idid < 0) {
        ind->rc[0] = -2019;
        break;
      } else if (ind->err){
        printErr(ind->err, ind->id);
        *i = ind->n_all_times-1;
        break;
      }
      break;
  }
}

/* Reactive AutoSwitch hysteresis counter (defined later); used by the
   single-interval composite dispatch below. */
static void rxAutoSwitchCount(rx_solving_options *op, rx_solving_options_ind *ind,
                              bool dop853Tried, bool dop853Failed);

static inline void solveWith1Pt(int *neq,
                                int *BadDose,
                                double *InfusionRate,
                                double *dose,
                                double *yp,
                                double xout, double xp, int id,
                                int *i, int nx,
                                int *istate,
                                rx_solving_options *op,
                                rx_solving_options_ind *ind,
                                t_update_inis u_inis,
                                void *ctx){
  int idid = 1, itol=0;
  // Per-individual effective neq under neqOverride; allocations are still
  // sized for op->neq, only stepping uses the smaller stride.
  int eff = rxEffNeq(ind, op);
  int neqOde = eff - op->numLin - op->numLinSens;
  if (neqOde == 0 && eff > 0) {
    if (!isSameTime(xout, xp)) {
      preSolve(op, ind, xp, xout, yp);
      linSolve(neq, ind, yp, &xp, xout);
    }
  } else {
    bool _autoSwitchActive = (op->stiff2 > 0);
    if (!_autoSwitchActive) {
      /* Single (non-composite) method: run it directly. */
      _rxSolveOneInterval(op->stiff, false,
                          neq, yp, &xp, xout, istate, &idid,
                          op, ind, i, ctx, eff);
    } else {
      /* Reactive AutoSwitch composite -- the same scheme as
         dopSegmentAutoSwitch / denseSegmentSolve: probe with the primary
         (dop853's built-in stiffness estimator is enabled by the
         autoSwitchPrimary flag, nstiff=50) and only fall back to the stiff
         secondary (op->stiff2) when the primary reports stiffness/failure.
         Once persistently stiff (autoMethod==1) the probe is skipped.

         The previous interval-length Gershgorin pre/post check lived here; it
         over-estimated the spectral radius on the long tau-sized intervals
         used during steady-state solving and spuriously toggled autoMethod,
         which then corrupted the main-timeline (dense) solve.  Dropping it
         keeps the composite consistent across the SS and main-solve paths. */
      double _ypStack[64];
      double *_ypDyn = NULL;
      double *_ypSave = (eff <= 64) ? _ypStack
                                    : (_ypDyn = (double*)malloc((size_t)eff * sizeof(double)));
      if (_ypSave == NULL) {
        ind->err = 1;
        if (ind->rc[0] == 0) ind->rc[0] = -2019;
        return;
      }
      memcpy(_ypSave, yp, (size_t)eff * sizeof(double));
      double _xpOrig = xp;
      if (ind->autoMethod == 0) {
        _rxSolveOneInterval(op->stiff, true,
                            neq, yp, &xp, xout, istate, &idid,
                            op, ind, i, ctx, eff);
        bool _failed = (idid <= 0 || ind->rc[0] == -2019 || ind->err != 0);
        if (_failed) {
          ind->rc[0] = 0; ind->err = 0; *istate = 1; idid = 1;
          memcpy(yp, _ypSave, (size_t)eff * sizeof(double));
          xp = _xpOrig;
          _rxSolveOneInterval(op->stiff2, false,
                              neq, yp, &xp, xout, istate, &idid,
                              op, ind, i, ctx, eff);
        }
        rxAutoSwitchCount(op, ind, true, _failed);
      } else {
        _rxSolveOneInterval(op->stiff2, false,
                            neq, yp, &xp, xout, istate, &idid,
                            op, ind, i, ctx, eff);
        rxAutoSwitchCount(op, ind, false, false);
      }
      if (_ypDyn != NULL) { free(_ypDyn); _ypDyn = NULL; }
    }
  }
}



//' This function is used to check if the steady state can be handled
//' by the pre-solved linear solutions
//'
//' @param op The rx solving options
//'
//' @param ind The rx solving individual options
//'
//' @param idx The index of the current dose
//'
//' @return 1 if the steady state can be handled, 0 otherwise
//'
//' Currently this requires:
//'
//' - Only linCmt() is defined (no mixed ODE/solved)
//' - The dose is to the depot or central compartment
static inline int canHandleSSLinear(rx_solving_options *op, rx_solving_options_ind *ind, int idx) {
  if (op->ssSolved == 1 && op->neq == op->numLinSens + op->numLin) {
    int evid = getEvid(ind, idx);
    int wh, cmt, wh100, whI, wh0;
    getWh(evid, &wh, &cmt, &wh100, &whI, &wh0);
    if (op->depotLin) {
      return (cmt == 0 || cmt == 1);
    } else {
      return (cmt == 0);
    }
  }
  return 0;
}

extern "C" void handleSSbolus(int *neq,
                              int *BadDose,
                              double *InfusionRate,
                              double *dose,
                              double *yp,
                              double *xout, double xp, int id,
                              int *i, int nx,
                              int *istate,
                              rx_solving_options *op,
                              rx_solving_options_ind *ind,
                              t_update_inis u_inis,
                              void *ctx,
                              rx_solve *rx,
                              double *xout2,
                              double *xp2,
                              double *curIi,
                              int *canBreak,
                              int adjustEvidBolusLag) {
  ind->ssTime = *xp2;
  if (canHandleSSLinear(op, ind, ind->ix[*i])) {

    // only a linear solved, use calculated steady state instead of
    // solved steady state.
    //
    // First set the steady state options for linCmtA or linCmtB
    ind->linSS = 3;
    ind->linSStau = *curIi;

    // use handle evid to figure out what dose is applied

    double ydum[2];
    ydum[0] = ydum[1] = 0;

    // here dose can be to the depot or central compartment
    ind->idx=*i;
    *xout2 = *xp2 + *curIi;
    int evid = getEvid(ind, ind->ix[*i])-adjustEvidBolusLag;
    int wh, cmt, wh100, whI, wh0;
    getWh(evid, &wh, &cmt, &wh100, &whI, &wh0);
    ind->linSSbolusCmt = cmt;
    // Use "real" xout for handle_evid functions.
    handle_evid(getEvid(ind, ind->ix[*i])-adjustEvidBolusLag, neq[0],
                BadDose, InfusionRate, dose, ydum,
                *xout, neq[1], ind);
    ind->ixds--; // This dose stays in place

    ind->linSSvar = ydum[cmt];
    solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
                 *xout2, *xp2, id, i, nx, istate, op, ind, u_inis, ctx);
    ind->linSS = 0; // reset to normal solve

    if (yp[cmt] > 0) {
      // now advance system by the bolus time to get the right derivatives
      // and use the trough concentrations (as expected in this function)
      *xp2 = *xout2;
      *xout2 = *xp2 + *curIi;
      solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
                   *xout2, *xp2, id, i, nx, istate, op, ind, u_inis, ctx);
      ind->ssTime = NA_REAL;
      return;
    }
  }
  // advance to trough
  ind->idx=*i;
  *xout2 = *xp2 + *curIi;
  // yp is last solve or y0
  solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
               *xout2, *xp2, id, i, nx, istate, op, ind, u_inis, ctx);
  *xp2 = *xout2;
  int evid=0;
  for (int j = 0; j < op->maxSS; j++) {
    ind->idx=*i;
    *xout2 = *xp2 + *curIi;
    // Use "real" xout for handle_evid functions.
    evid = getEvid(ind, ind->ix[*i])-adjustEvidBolusLag;
    handle_evid(evid, neq[0],
                BadDose, InfusionRate, dose, yp,
                *xout, neq[1], ind);
    // yp is last solve or y0
    solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
                 *xout2, *xp2, id, i, nx, istate, op, ind, u_inis, ctx);
    ind->ixds--; // This dose stays in place
    // REprintf("ixds-- #2\n");
    *canBreak=1;
    if (j <= op->minSS -1){
      if (ind->rc[0]== -2019){
        badSolveExit(*i);
        break;
      }
      for (int k = rxEffNeq(ind, op); k--;) {
        ind->solveLast[k] = yp[k];
      }
      *canBreak=0;
    } else if (j >= op->minSS){
      if (ind->rc[0] == -2019){
        for (int k = neq[0]; k--;) {
          yp[k] = ind->solveLast[k];
        }
        ind->rc[0] = 2019;
        break;
      }
      for (int k = neq[0]; k--;){
        if (op->ssRtol[k]*fabs(yp[k]) + op->ssAtol[k] <= fabs(yp[k]-ind->solveLast[k])){
          *canBreak=0;
        }
        ind->solveLast[k] = yp[k];
      }
      if (*canBreak){
        break;
      }
    }
    *istate=1;
    *xp2 = *xout2;
  }
  ind->ssTime = NA_REAL;
}

// Adjoint steady-state INFUSION handoff.  rk4s.cpp is #included into this
// translation unit, so the rk4s discrete-adjoint driver reads these directly.
// When op->adjoint, the steady-state infusion dispatch below publishes the
// converged period's on/off durations, rate, and compartment so ind_rk4s_0 can
// record one steady-state period (infusion ON for _adjSSinfDur, OFF for
// _adjSSinfDur2) for the monodromy IC term.  Fixed-rate infusions with
// dur < ii only (dR/dp == 0, single on/off window per period); other ss cases
// leave _adjSSinfKind at its handleSS-entry value so the driver can guard them.
//   kind: 0 none, 1 fixed-rate periodic infusion (dur<ii), 2 unhandled ss
static thread_local int    _adjSSinfKind = 0;
static thread_local int    _adjSSinfCmt = -1;
// ss==1 BOLUS period, published for the liblsodaadj multistep driver (whose ss
// pre-solve is recording-paused, so it re-records one ss period for the monodromy
// IC using this interval; the rk4s framework uses its own event-derived _ssPend*).
// 0 = no bolus ss this window.  Dose Jacobian is I, so only the period is needed.
static thread_local double _adjSSbolusIi = 0.0;
// Two-phase steady-state infusion period: rate _adjSSinfRate for _adjSSinfDur,
// then rate _adjSSinfRate2 for _adjSSinfDur2.  Fixed-rate periodic (dur<ii) uses
// (R, dur) then (0, ii-dur); large-duration (dur>=ii, overlapping infusions)
// uses ((numDoseInf+1)*R, offTime) then (numDoseInf*R, addTime).
static thread_local double _adjSSinfDur = 0.0, _adjSSinfDur2 = 0.0, _adjSSinfRate = 0.0, _adjSSinfRate2 = 0.0;
// STICKY snapshot of the ss handoff for the liblsodaadj backward fill: the
// forward loop calls handleSS again for every addl-expanded dose (even under
// addlDropSs), and each entry RESETS the live handoff -- so the driver snapshots
// the regimen the moment handleSS actually establishes one (kind != 0) and reads
// the snapshot, not the live (reset) value, in the backward sweep.
static thread_local int    _lsSsKind = 0;      // 0 none, 1 finite-infusion monodromy, 2 continuous
static thread_local double _lsSsBolusIi = 0.0;
static thread_local int    _lsSsCmt = -1;
static thread_local double _lsSsDur = 0.0, _lsSsDur2 = 0.0, _lsSsRate = 0.0, _lsSsRate2 = 0.0;
static inline void lsAdjSnapshotSs() {
  // called right after an ss handleSS while liblsoda recording is active
  if (_adjSSbolusIi > 0.0) { _lsSsKind = 1; _lsSsBolusIi = _adjSSbolusIi; }
  else if (_adjSSinfKind == 1) { _lsSsKind = 1; _lsSsBolusIi = 0.0; _lsSsCmt = _adjSSinfCmt;
    _lsSsDur = _adjSSinfDur; _lsSsDur2 = _adjSSinfDur2; _lsSsRate = _adjSSinfRate; _lsSsRate2 = _adjSSinfRate2; }
  else if (_adjSSinfKind == 2) { _lsSsKind = 2; _lsSsCmt = _adjSSinfCmt; }
}
// ss==2 (superposition): Y_after = Y_before + Y_ss_new(p).  handleSS publishes
// the new-regimen steady state Y_ss_new (yp just BEFORE the += solveSave) so the
// rk4s driver can record that regimen's monodromy and apply lambda^T dY_ss_new/dp
// at the interior ss2 event.  1 = a bolus ss2 was just added (infusion ss2 TBD).
static thread_local int    _adjSS2 = 0;
// _adjSS2peak holds the added regimen's steady state Y_ss_new (yp just BEFORE the
// += solveSave).  For a bolus/finite infusion ss2 it is the monodromy period
// boundary; for a full-interval infusion ss2 it is the constant steady state used
// in the dY_ss_new/dp = -J^{-1} df/dp linear solve (kind 2).
static thread_local std::vector<double> _adjSS2peak;

extern "C" void solveSSinf(int *neq,
                           int *BadDose,
                           double *InfusionRate,
                           double *dose,
                           double *yp,
                           double *xout, double xp, int id,
                           int *i, int nx,
                           int *istate,
                           rx_solving_options *op,
                           rx_solving_options_ind *ind,
                           t_update_inis u_inis,
                           void *ctx,
                           rx_solve *rx,
                           double *xout2,
                           double *xp2,
                           int *infBixds,
                           int *bi,
                           int *infEixds,
                           int *ei,
                           double *curIi,
                           double *dur,
                           double *dur2,
                           int *canBreak) {
  double ssStart = *xp2;
  double ssStop = *xp2 + *dur;
  if (canHandleSSLinear(op, ind, ind->idose[*infBixds])) {
    // only a linear solved, use calculated steady state instead of
    // solved steady state.
    //
    // First set the steady state options for linCmtA or linCmtB
    ind->linSS = 2;
    ind->linSSvar = *dur; // tinf or dur
    ind->linSStau = *curIi;
    ind->ssTime = ssStart;

    // Next, turn on the infusion in the appropriate compartment
    *canBreak=1;
    *xout2 = *xp2+*dur;
    ind->idx=*bi;
    ind->ixds = *infBixds;
    handle_evid(getEvid(ind, ind->idose[*infBixds]), neq[0],
                BadDose, InfusionRate, dose, yp,
                *xout, neq[1], ind);
    // yp is last solve or y0
    *istate=1;
    // yp is last solve or y0
    solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
                 *xout2, *xp2, id, i, nx, istate, op, ind, u_inis, ctx);
    *xp2 = *xout2;
    // Turn off Infusion; no need to solve anything afterward
    *xout2 = *xp2 + *dur2;
    ind->ixds = *infEixds;
    ind->idx=*ei;
    handle_evid(getEvid(ind, ind->idose[*infEixds]), neq[0],
                BadDose, InfusionRate, dose, yp,
                *xout+*dur, neq[1], ind);
    ind->linSS = 0; // switch back to normal solve
    if (yp[ind->cmt] > 0) {
      // solved successfully, return
      ind->ssTime = NA_REAL;
      return;
    }
  }
  for (int j = 0; j < op->maxSS; j++) {
    // Turn on Infusion, solve (0-dur)
    *canBreak=1;
    *xout2 = *xp2+*dur;
    ind->idx=*bi;
    ind->ixds = *infBixds;
    ind->ssTime = ssStart;

    handle_evid(getEvid(ind, ind->idose[*infBixds]), neq[0],
                BadDose, InfusionRate, dose, yp,
                *xout, neq[1], ind);
    // yp is last solve or y0
    *istate=1;
    // yp is last solve or y0
    solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
                 *xout2, *xp2, id, i, nx, istate, op, ind, u_inis, ctx);
    *xp2 = *xout2;
    // Turn off Infusion, solve (*dur-ii)
    *xout2 = *xp2 + *dur2;
    ind->ixds = *infEixds;
    ind->idx=*ei;
    handle_evid(getEvid(ind, ind->idose[*infEixds]), neq[0],
                BadDose, InfusionRate, dose, yp,
                *xout+*dur, neq[1], ind);
    if (j <= op->minSS -1){
      if (ind->rc[0]== -2019){
        badSolveExit(*i);
        break;
      }
      for (int k = neq[0]; k--;) {
        ind->solveLast[k] = yp[k];
      }
      *canBreak=0;
    } else if (j >= op->minSS){
      if (ind->rc[0]== -2019){
        if (op->strictSS){
          badSolveExit(*i);
        } else {
          for (int k = neq[0]; k--;){
            yp[k] = ind->solveLast[k];
          }
          ind->rc[0] = 2019;
        }
      }
      for (int k = neq[0]; k--;) {
        if (op->ssRtol[k]*fabs(yp[k]) + op->ssAtol[k] <= fabs(yp[k]-ind->solveLast[k])){
          *canBreak=0;
        }
        ind->solveLast[k] = yp[k];
      }
    }
    // yp is last solve or y0
    *istate=1;
    ind->ssTime = ssStop;
    solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
                 *xout2, *xp2, id, i, nx, istate, op, ind, u_inis, ctx);
    if (j <= op->minSS -1){
      if (ind->rc[0]== -2019){
        badSolveExit(*i);
        break;
      }
      for (int k = neq[0]; k--;){
        ind->solveLast2[k] = yp[k];
      }
      *canBreak=0;
    } else if (j >= op->minSS){
      if (ind->rc[0]== -2019){
        if (op->strictSS){
          badSolveExit(*i);
        } else {
          for (int k = neq[0]; k--;){
            yp[k] = ind->solveLast2[k];
          }
          ind->rc[0] = 2019;
        }
        break;
      }
      for (int k = neq[0]; k--;){
        if (op->ssRtol[k]*fabs(yp[k]) + op->ssAtol[k] <= fabs(yp[k]-ind->solveLast2[k])){
          *canBreak=0;
        }
        ind->solveLast2[k] = yp[k];
      }
      if (*canBreak){
        break;
      }
    }
    *xp2 = *xout2;
  }
  ind->ssTime = NA_REAL;
}

extern "C" void solveSSinfLargeDur(int *neq,
                                   int *BadDose,
                                   double *InfusionRate,
                                   double *dose,
                                   double *yp,
                                   double *xout, double xp, int id,
                                   int *i, int nx,
                                   int *istate,
                                   rx_solving_options *op,
                                   rx_solving_options_ind *ind,
                                   t_update_inis u_inis,
                                   void *ctx,
                                   rx_solve *rx,
                                   double *xout2,
                                   double *xp2,
                                   int *infBixds,
                                   int *bi,
                                   int *infEixds,
                                   int *ei,
                                   double *curIi,
                                   double *dur,
                                   int *numDoseInf,
                                   double *offTime,
                                   double *addTime,
                                   int *canBreak) {
  *numDoseInf = (int)(*dur / *curIi);
  *offTime = *dur - (*numDoseInf)*(*curIi);
  *addTime = *curIi - *offTime;
  ind->ixds = *infBixds;
  ind->idx = *bi;
  double timeStart = *xp2;
  ind->ssTime = timeStart;
  for (int j = 0; j < *numDoseInf; j++) {
    ind->ixds = *infBixds;
    ind->idx = *bi;
    *xout2 = *xp2 + *curIi;
    // Use "real" xout for handle_evid functions.
    handle_evid(getEvid(ind, ind->ix[*bi]), neq[0],
                BadDose, InfusionRate, dose, yp,
                *xout, neq[1], ind);
    // yp is last solve or y0
    solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
                 *xout2, *xp2, id, i, nx, istate, op, ind, u_inis, ctx);
  }
  int nDup = 0;
  double time2 = *xp2;
  double time3 = *xp2 + *addTime;
  for (int j = 0; j < op->maxSS; j++) {
    // Turn on Infusion, solve (0-dur)
    *canBreak =1;
    *xout2    = *xp2 + *offTime;
    ind->idx  = *bi;
    ind->ixds = *infBixds;
    handle_evid(getEvid(ind, ind->idose[*infBixds]), neq[0],
                BadDose, InfusionRate, dose, yp,
                *xout, neq[1], ind);
    // yp is last solve or y0
    *istate=1;
    // yp is last solve or y0
    ind->ssTime = time2;
    solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
                 *xout2, *xp2, id, i, nx, istate, op, ind, u_inis, ctx);
    *xp2 = *xout2;
    // Turn off Infusion, solve (dur-ii)
    *xout2 = *xp2 + *addTime;
    ind->ixds = *infEixds;
    ind->idx= *ei;
    handle_evid(getEvid(ind, ind->idose[*infEixds]), neq[0],
                BadDose, InfusionRate, dose, yp,
                *xout + *dur, neq[1], ind);
    if (j <= op->minSS -1){
      if (ind->rc[0]== -2019){
        badSolveExit(*i);
        break;
      }
      for (int k = neq[0]; k--;) {
        ind->solveLast[k] = yp[k];
      }
      *canBreak=0;
    } else if (j >= op->minSS){
      if (ind->rc[0]== -2019){
        if (op->strictSS){
          badSolveExit(*i);
        } else {
          for (int k = neq[0]; k--;){
            yp[k] = ind->solveLast[k];
          }
          ind->rc[0] = 2019;
        }
      }
      for (int k = neq[0]; k--;) {
        if (op->ssRtol[k]*fabs(yp[k]) + op->ssAtol[k] <= fabs(yp[k]-ind->solveLast[k])){
          *canBreak=0;
        }
        ind->solveLast[k] = yp[k];
      }
    }
    // yp is last solve or y0
    *istate=1;
    ind->ssTime = time3;
    solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
                 *xout2, *xp2, id, i, nx, istate, op, ind, u_inis, ctx);
    if (j <= op->minSS -1){
      if (ind->rc[0]== -2019){
        badSolveExit(*i);
        break;
      }
      for (int k = neq[0]; k--;){
        ind->solveLast2[k] = yp[k];
      }
      *canBreak=0;
    } else if (j >= op->minSS){
      if (ind->rc[0]== -2019){
        if (op->strictSS){
          badSolveExit(*i);
        } else {
          for (int k = neq[0]; k--;){
            yp[k] = ind->solveLast2[k];
          }
          ind->rc[0] = 2019;
        }
        break;
      }
      for (int k = neq[0]; k--;){
        if (op->ssRtol[k]*fabs(yp[k]) + op->ssAtol[k] <= fabs(yp[k]-ind->solveLast2[k])){
          *canBreak=0;
        }
        ind->solveLast2[k] = yp[k];
      }
      if (*canBreak){
        nDup++;
        if (nDup >= 7) {
          break;
        }
      }
    }
    *xp2 = *xout2;
  }
  ind->ssTime = NA_REAL;
}

extern "C" void handleSSinf8(int *neq,
                             int *BadDose,
                             double *InfusionRate,
                             double *dose,
                             double *yp,
                             double *xout, double xp, int id,
                             int *i, int nx,
                             int *istate,
                             rx_solving_options *op,
                             rx_solving_options_ind *ind,
                             t_update_inis u_inis,
                             void *ctx,
                             rx_solve *rx,
                             int *infBixds,
                             int *bi,
                             double *rateOn,
                             double *xout2,
                             double *xp2,
                             int *canBreak) {
  ind->ssTime = *xp2;
  if (canHandleSSLinear(op, ind, ind->idose[*infBixds])) {
    // only a linear solved, use calculated steady state instead of
    // solved steady state.
    //
    // First set the steady state options for linCmtA or linCmtB
    ind->linSS = 1; // infinite infusin

    // Next, turn on the infusion in the appropriate compartment
    ind->ixds= *infBixds;
    ind->idx= *bi;
    // REprintf("Assign ind->ixds to %d (idx: %d) #0\n", ind->ixds, ind->idx);
    // Rate is fixed, so modifying bio-availability doesn't change duration.
    ind->InfusionRate[ind->cmt] = *rateOn;
    ind->on[ind->cmt] = 1;
    *xp2 = 0.0;

    // next solve
    solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
                 *xout2, *xp2, id, i, nx, istate, op, ind, u_inis, ctx);

    // turn off infusion
    ind->InfusionRate[ind->cmt] = 0.0;
    *xp2=*xout;
    *istate=1;

    ind->linSS = 0; // switch back to normal solve
    if (yp[ind->cmt] > 0.0) {
      // successful solve; for some reason the mac doesn't update here
      // :(
      ind->ssTime = NA_REAL;
      return;
    }
  }
  ind->ixds= *infBixds;
  ind->idx= *bi;
  // REprintf("Assign ind->ixds to %d (idx: %d) #0\n", ind->ixds, ind->idx);
  // Rate is fixed, so modifying bio-availability doesn't change duration.
  ind->InfusionRate[ind->cmt] = *rateOn;
  ind->on[ind->cmt] = 1;
  *xp2 = 0.0;
  double infStep = op->infSSstep, a1=1.0, t1=*xp2+1.0;
  // Based on http://www.rxkinetics.com/theo.html -- Chiou method
  for (int j = 0; j < op->maxSS; j++) {
    if (j == 0) *xout2 = *xp2+1.; // the first level drawn one hour after infusion
    else *xout2 = *xp2+infStep;
    solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
                 *xout2, *xp2, id, i, nx, istate, op, ind, u_inis, ctx);
    *canBreak=1;
    if (j <= op->minSS -1){
      for (int k = neq[0]; k--;) {
        ind->solveLast[k] = yp[k];
      }
      if (j == 0) {
        a1 = yp[ind->cmt];
      }
      *canBreak=0;
    } else {
      for (int k = 0; k < neq[0]; k++){
        // if (k == 0) REprintf("\n");
        // REprintf("j: %d rtol[k:%d]: %f  Atol: %f xout: %f:  y: %f diff: %f\n", j, k,
        //          op->ssRtol[k], op->ssAtol[k], *xout2,
        //          yp[k],
        //          fabs(yp[k]-ind->solveLast[k]));
        if (op->ssRtol[k]*fabs(yp[k]) + op->ssAtol[k] <= fabs(yp[k]-ind->solveLast[k])){
          *canBreak=0;
        }
        ind->solveLast[k] = yp[k];
      }
      if (*canBreak){
        ind->InfusionRate[ind->cmt] = 0.0;
        break;
      } else {
        // Assumes that this is at least one half life.
        double a2 = yp[ind->cmt];
        infStep = max2(infStep,M_LN2/(*rateOn/(a1+a2) + 2*(a1-a2)/((a1+a2)*(*xout-t1))));
      }
    }
    *xp2=*xout;
    *istate=1;
  }
  ind->ssTime = NA_REAL;
}


void handleSS(int *neq,
              int *BadDose,
              double *InfusionRate,
              double *dose,
              double *yp,
              double xout, double xp, int id,
              int *i, int nx,
              int *istate,
              rx_solving_options *op,
              rx_solving_options_ind *ind,
              t_update_inis u_inis,
              void *ctx) {
  rx_solve *rx = &rx_global;
  _adjSSinfKind = 0; _adjSS2 = 0; _adjSSbolusIi = 0.0;   // reset adjoint ss handoffs
  int j;
  int doSS2=0;
  int doSSinf=0;
  int maxSS = op->maxSS;
  int minSS = op->minSS;
  int isSsLag = ind->wh0 == EVID0_SS20 || ind->wh0 == EVID0_SS0;
  bool skipDosingEvent = false, isRateDose = false;
  bool isModeled = ind->whI == EVIDF_MODEL_DUR_ON ||
    ind->whI == EVIDF_MODEL_RATE_ON;
  int adjustEvidBolusLag = 0;
  if (isModeled &&
      ind->whI == EVIDF_MODEL_DUR_ON &&
      ((isSsLag && getDoseP1(ind, ind->ixds) == 0.0) ||
       (!isSsLag && getDose(ind, ind->ixds) == 0.0))
      ) {
    // This is a modeled duration dose with duration = 0.
    // Handle like a bolus dose.
    isModeled = false;
    ind->whI = EVIDF_NORMAL;
    adjustEvidBolusLag = EVIDF_MODEL_DUR_ON*10000;
  }
  double curIi = ind->ixds == 0 ? 0.0 : getIiNumber(ind, ind->ixds-1);
  if (((ind->wh0 == EVID0_SS2  || isSsLag ||
        ind->wh0 == EVID0_SS) &&
       curIi > 0) || ind->wh0 == EVID0_SSINF) {
    int oldIdx = ind->idx, oldIxds = ind->ixds;
    int ignoreDoses[4];
    ignoreDoses[0] = ignoreDoses[1] = ignoreDoses[2] = ignoreDoses[3] = -1;
    int nIgnoredDoses = 0;
    if (isSsLag) {
      maxSS--; minSS--;
    }
    ind->doSS=1;
    ind->ixds--; // This dose stays in place; Reverse dose
    if (ind->wh0 == EVID0_SS2 || ind->wh0 == EVID0_SS20){
      doSS2=1;
    } else if (ind->wh0 == EVID0_SSINF) {
      doSSinf=1;
    }
    double dur = 0, dur2=0, rateOn=0.0, rateOff = 0.0;
    int infBixds =0, infBixds2=0, infEixds = 0, infFixds = 0,
      infSixds = 0,
      ei=0, wh, cmt, wh100, whI, wh0, oldI,
      bi = *i, fi = *i;
    if (doSSinf) {
    } else if (ind->whI == EVIDF_INF_RATE || ind->whI == EVIDF_INF_DUR) {
      if (getDose(ind, ind->idose[ind->ixds]) < 0) return;
      oldI = ind->whI;
      isRateDose=true;
      infBixds = infBixds2 = infFixds = ind->ixds;
      // Find the next fixed length infusion that is turned off.
      if (isSsLag) {
        ignoreDoses[nIgnoredDoses++] = infBixds;
        int bEvid = getEvid(ind, ind->idose[infBixds2]);
        double bIi = getIiNumber(ind, infBixds2);
        double cIi = bIi;
        double bDose = getDoseNumber(ind,ind->ixds);
        double cDose = bDose;
        getWh(bEvid, &wh, &cmt, &wh100, &whI, &wh0);
        // This structure is
        // TIME  EVID AMT II
        //    0 10209  10 24
        //    0 10208 -10 24
        //    0 10201  10  0
        // ...
        //   10 10201 -10  0
        //
        // or with ss=2
        //
        // TIME  EVID AMT II
        //    0 10219  10 24 (note the 19 flag)
        //    0 10208 -10 24
        //    0 10201  10  0
        // ...
        //   10 10201 -10  0
        int evid8 = bEvid - ind->wh0 + EVID0_INFRM;
        bool foundEvid8 = false;
        int evid1 = bEvid - ind->wh0 + EVID0_REGULAR;
        bool foundEvid1 = false;
        while (!foundEvid1 && infBixds2 < ind->ndoses) {
          infBixds2++;
          if (infBixds2 == ind->ndoses) {
            infBixds = infBixds2 = infEixds = -1;
            break;
          } else {
            bEvid = getEvid(ind, ind->idose[infBixds2]);
            cDose = getDoseNumber(ind, infBixds2);
            cIi = getIiNumber(ind, infBixds2);
            if (!foundEvid8 && bEvid == evid8 && cDose == -bDose && cIi == bIi) {
              foundEvid8 = true;
              ignoreDoses[nIgnoredDoses++] = infBixds2;
            } else if (!foundEvid1 && bEvid == evid1 && cDose == bDose && cIi == 0.0) {
              foundEvid1 = true;
              ignoreDoses[nIgnoredDoses++] = infBixds2;
              break;
            }
          }
        }
        if (infEixds != -1) {
          handleInfusionGetEndOfInfusionIndex(infBixds2,
                                              &infEixds, rx, op, ind);
        }
        if (infEixds == -1 && infBixds2 != -1) {
          ind->wrongSSDur=1;
          // // Bad Solve => NA
          badSolveExit(*i);
          return;
        } else {
          ignoreDoses[nIgnoredDoses++] = infEixds;
          double f = 1.0;
          if (ind->whI == EVIDF_INF_RATE) {
            f = getAmt(ind, ind->id, ind->cmt, 1.0, getAllTimes(ind, ind->idose[infBixds2]), yp);
            rateOn = getDose(ind, ind->idose[infBixds2]);
            rateOff = getDose(ind, ind->idose[infEixds]);
          } else {
            f = 1.0;
            rateOn = getAmt(ind, ind->id, ind->cmt, getDose(ind, ind->idose[infBixds2]), getAllTimes(ind, ind->idose[infBixds2]), yp);
            rateOff = getAmt(ind, ind->id, ind->cmt, getDose(ind, ind->idose[infEixds]), getAllTimes(ind, ind->idose[infEixds]), yp);
          }
          // REprintf("get dur isSsLag: %d\n", isSsLag);
          dur = getAllTimes(ind, ind->idose[infEixds]);// -
          // REprintf("\ttime infEixds: %f %d\n", dur, infEixds);
          dur -= getAllTimes(ind, ind->idose[infBixds2]);
          // REprintf("\ttime infBixds: %f %d\n", getAllTimes(ind, ind->idose[infBixds2]), infBixds2);
          dur *= f;
          // REprintf("\tdur: %f\n", dur);
          dur2 = curIi - dur;
          // REprintf("\tdur2: %f\n", dur2);
          infSixds = infBixds;
        }
       } else {
        // This is the infusion structure:
        // TIME  EVID AMT II
        //    0 10201  10  0
        // ...
        //   10 10201 -10  0
        infBixds=infBixds2=infEixds=infFixds=ind->ixds;
        ignoreDoses[nIgnoredDoses++] = infBixds;
        handleInfusionGetEndOfInfusionIndex(infBixds, &infEixds, rx, op, ind);
        if (infEixds == -1) {
          ind->wrongSSDur=1;
          // // Bad Solve => NA
          badSolveExit(*i);
          return;
        } else {
          ignoreDoses[nIgnoredDoses++] = infEixds;
          double f = 1.0;
          if (ind->whI == EVIDF_INF_RATE) {
            f = getAmt(ind, ind->id, ind->cmt, 1.0, getAllTimes(ind, ind->idose[infBixds2]), yp);
            rateOn = getDose(ind, ind->idose[infBixds2]);
            rateOff = getDose(ind, ind->idose[infEixds]);
          } else {
            f = 1.0;
            rateOn = getAmt(ind, ind->id, ind->cmt, getDose(ind, ind->idose[infBixds2]), getAllTimes(ind, ind->idose[infBixds2]), yp);
            rateOff = getAmt(ind, ind->id, ind->cmt, getDose(ind, ind->idose[infEixds]), getAllTimes(ind, ind->idose[infEixds]), yp);
          }
          // REprintf("get dur2\n");
          dur = getAllTimes(ind, ind->idose[infEixds]);// -
          // REprintf("\ttime infEixds: %f\n", dur);
          dur -= getAllTimes(ind, ind->idose[ind->ixds]);
          dur *= f;
          // REprintf("\tdur: %f\n", dur);
          dur2 =  curIi - dur;
          infSixds = infBixds;
          // REprintf("\tdur2: %f\n", dur2);
        }
      }
    } else if (isModeled) {
      isRateDose=true;
      // These are typically right next to another.
      infBixds=infBixds2=infEixds=infFixds=ind->ixds;
      if (isSsLag) {
        // The structure of this modeled rate with a lag item is:
        //
        // SS=1
        // TIME  EVID AMT II
        //    0 90209 100 24
        //    0 90201 100  0
        //    0 70201 100  0
        //
        // OR
        //
        // SS=2
        // TIME  EVID AMT II
        //    0 90219 100 24
        //    0 90201 100  0
        //    0 70201 100  0

        // The structure of this modeled duration item is:
        //
        // SS=1
        // TIME  EVID AMT II
        //    0 80209 100 24
        //    0 80201 100  0
        //    0 60201 100  0
        //
        // OR
        //
        // SS=2
        // TIME  EVID AMT II
        //    0 80219 100 24
        //    0 80201 100  0
        //    0 60201 100  0
        ignoreDoses[nIgnoredDoses++] = infBixds;
        int bEvid = getEvid(ind, ind->idose[infBixds]);
        double cIi = 0.0;
        double bDose = getDoseNumber(ind,ind->ixds);
        double cDose = bDose;
        int evid1 = bEvid - ind->wh0 + EVID0_REGULAR;
        bool foundEvid1 = false;
        int evidOff = bEvid - ind->wh0 + EVID0_REGULAR - 2*10000;
        bool foundEvidOff = false;
        int curRec = infBixds;
        while (!foundEvidOff && curRec < ind->ndoses) {
          curRec++;
          if (curRec == ind->ndoses) {
            infBixds = infBixds2 = infEixds = -1;
            break;
          } else {
            bEvid = getEvid(ind, ind->idose[curRec]);
            cDose = getDoseNumber(ind, curRec);
            cIi = getIiNumber(ind, curRec);
            if (!foundEvid1 && bEvid == evid1 && cDose == bDose && cIi == 0.0) {
              foundEvid1 = true;
              infBixds = infBixds2 = curRec;
              ignoreDoses[nIgnoredDoses++] = infBixds;
            } else if (!foundEvidOff && bEvid == evidOff  && cDose < 0.0 &&cIi == 0.0) {
              // note that this record stores the calculated infusion rate (in amt) and duration (in time)
              foundEvidOff = true;
              infEixds = curRec;
              ignoreDoses[nIgnoredDoses++] = infEixds;
              break;
            }
          }
        }
        if (infEixds == -1) {
          ind->wrongSSDur=1;
          // // Bad Solve => NA
          badSolveExit(*i);
          return;
        } else {
          // These use the getTime_() to grab calculated duration
          // REprintf("getDur3\n");
          dur = getTime_(ind->idose[infEixds], ind);// -
          dur -= getTime_(ind->idose[infBixds],ind);
          // REprintf("\tdur: %f\n", dur);
          dur2 = curIi - dur;
          // REprintf("\tdur2: %f\n", dur2);
          // Search from index 0: companion SS records (e.g. evid 90201) sort
          // BEFORE the SS trigger (e.g. evid 90209) at the same time, so a
          // forward search starting at *i never finds them.
          bi = 0;
          while (bi < ind->n_all_times && ind->ix[bi] != ind->idose[infBixds]) {
            bi++;
          }
          if (bi >= ind->n_all_times) {
            ind->wrongSSDur = 1;
            badSolveExit(*i);
            ind->doSS = 0;
            updateExtraDoseGlobals(ind);
            ind->ixds = oldIxds;
            ind->idx = oldIdx;
            return;
          }
          infSixds = infBixds;
        }
      } else {
        // SS=1
        // TIME  EVID AMT II
        //    0 90210 100 24
        //    0 70201 100  0
        //
        // OR
        //
        // SS=2
        // TIME  EVID AMT II
        //    0 90201 100 24
        //    0 70201 100  0

        // The structure of this modeled duration item is:
        //
        // SS=1
        // TIME  EVID AMT II
        //    0 80201 100 24
        //    0 60201 100  0
        //
        // OR
        //
        // SS=2
        // TIME  EVID AMT II
        //    0 80201 100 24
        //    0 60201 100  0
        infFixds = infBixds = infBixds2 = ind->ixds;
        infEixds = infBixds+1;
        ignoreDoses[nIgnoredDoses++] = infBixds;
        ignoreDoses[nIgnoredDoses++] = infEixds;
        dur = getAllTimes(ind, ind->idose[infBixds]);
        dur2 = getAllTimes(ind, ind->idose[infBixds+1]);
        dur = dur2-dur;
        dur2 = curIi - dur;
        infSixds = infBixds;
      }
      if (dur > curIi) {
        infSixds = infFixds;
      }
      rateOn = -getDose(ind, ind->idose[infBixds2+1]);
      rateOff = -rateOn;
    }
    if (ind->wh0 == EVID0_SSINF) {
      infEixds=infBixds=infBixds2 = ind->ixds;
      if (ind->whI == EVIDF_INF_RATE) {
        rateOn = getDose(ind, ind->idose[infBixds]);
        rateOff = -getDose(ind, ind->idose[infEixds]);
      } else if (isModeled) {
        rateOn =getRate(ind, ind->id, ind->cmt, 0.0,
                        getAllTimes(ind, ind->idose[ind->ixds]), yp);
        rateOff = -rateOn;
      } else {
        // shouldn't ever get here modeled duration would have to be
        // infinite for this to occur... but I don't think that is
        // supported yet
        rateOn = getAmt(ind, ind->id, ind->cmt, getDose(ind, ind->idose[infBixds2]), getAllTimes(ind, ind->idose[infBixds2]), yp);
        rateOff = getAmt(ind, ind->id, ind->cmt, getDose(ind, ind->idose[infEixds]), getAllTimes(ind, ind->idose[infEixds]), yp);
      }
    } else if (ind->whI == EVIDF_INF_RATE ||
               ind->whI == EVIDF_INF_DUR ||
               isModeled) {
      // Search from index 0: the end-of-infusion record (e.g. evid 70201)
      // sorts BEFORE the SS trigger at the same time, so searching from *i
      // forward never finds it.
      ei = 0;
      while(ei < ind->n_all_times && ind->ix[ei] != ind->idose[infEixds]) {
        ei++;
      }
      if (ei >= ind->n_all_times || ind->ix[ei] != ind->idose[infEixds]){
        /* (Rf_errorcall)(R_NilValue, "Cannot figure out infusion end time."); */
        if (!(ind->err & rxErrRate02)){
          ind->err += rxErrRate02;
          /* (Rf_errorcall)(R_NilValue, "Rate is zero/negative"); */
        }
        return;
      }
    }
    // REprintf("rateOn: %f rateOff: %f; dur: %f dur2: %f\n", rateOn, rateOff, dur, dur2);
    double startTimeD = 0.0;
    double curLagExtra = 0.0;
    int overIi = 0, regEvid = 0, extraEvid = 0;
    if (isRateDose) {
      startTimeD = getTime_(ind->idose[infSixds], ind);
      regEvid = getEvidClassic(ind->cmt+1, rateOn, rateOn, 0.0, 0.0, 1, 0);
      extraEvid = regEvid - EVID0_REGULAR + EVID0_RATEADJ;
    } else {
      startTimeD = getTime_(ind->idose[ind->ixds], ind);
    }
    if (isSsLag) {
      int wh0 = ind->wh0; ind->wh0=1;
      curLagExtra = getLag(ind, neq[1], ind->cmt, startTimeD, yp) -
        startTimeD;
      ind->wh0 = wh0;
      overIi = floor(curLagExtra/curIi);
      curLagExtra = curLagExtra - overIi*curIi;
    }
    // First Reset
    for (j = neq[0]; j--;) {
      ind->InfusionRate[j] = 0;
      // ind->on[j] = 1; // nonmem doesn't reset on according to doc
    }
    // REprintf("reset & cancel pending doses\n");
    cancelInfusionsThatHaveStarted(ind, neq[1], startTimeD);
    if (!rx->ss2cancelAllPending && doSS2) {
    } else {
      cancelPendingDoses(ind, neq[1]);
    }
    ind->cacheME=0;
    // Reset LHS to NA
    ind->inLhs = 0;
    for (j = op->nlhs; j--;) ind->lhs[j] = NA_REAL;
    u_inis(neq[1], yp); // Update initial conditions @ current time
    if (rx->istateReset) *istate = 1;
    int k;
    double xp2, xout2;
    int canBreak=0;
    xp2 = xp;
    if (doSSinf || isSameTimeOp(curIi, dur)) {
      handleSSinf8(neq,
                   BadDose,
                   InfusionRate,
                   dose,
                   yp,
                   &xout, xp, id,
                   i, nx,
                   istate,
                   op,
                   ind,
                   u_inis,
                   ctx,
                   rx,
                   &infBixds,
                   &bi,
                   &rateOn,
                   &xout2,
                   &xp2,
                   &canBreak);
      if (isSameTimeOp(curIi, dur) && !isSameTimeOp(dur, 0.0)) {
        ind->InfusionRate[ind->cmt] = rateOn;
        pushDosingEvent(startTimeD+curIi, rateOff, extraEvid, ind);
        for (int ii = 0; ii < nIgnoredDoses; ++ii) {
          pushIgnoredDose(ignoreDoses[ii], ind);
        }
      } else {
        ind->InfusionRate[ind->cmt] = 0.0;
      }
      // REprintf("at ss: %f (inf: %f; rate: %f)\n", yp[ind->cmt],
      //          ind->InfusionRate[ind->cmt], rate);
      if (doSS2){
        // ss==2 superposition: yp (BEFORE the += solveSave) is the added regimen's
        // constant steady state Y_ss_new; publish it in _adjSS2peak so the rk4s
        // driver adds lambda^T dY_ss_new/dp (via -J^{-1} df/dp) at the ss2 event.
        if (op->adjoint) {
          _adjSS2 = 1; _adjSS2peak.assign(yp, yp + neq[0]);
          _adjSSinfKind = 2; _adjSSinfCmt = ind->cmt;
        }
        // Add at the end
        for (j = neq[0];j--;) yp[j]+=ind->solveSave[j];
      }
      // Adjoint handoff: continuous / full-interval infusion reaches a CONSTANT
      // steady state (f(Y_ss)+R.e = 0), so dY_ss/dp = -J^{-1} df/dp (a linear
      // solve, no monodromy).  kind 2 tells the rk4s driver to take that path.
      if (op->adjoint && !doSS2) {
        _adjSSinfKind = 2; _adjSSinfCmt = ind->cmt;
      }
      ind->doSS=0;
      updateExtraDoseGlobals(ind);
      return;
    } else if (dur == 0) {
      // Bolus
      handleSSbolus(neq,
                    BadDose,
                    InfusionRate,
                    dose,
                    yp,
                    &xout, xp, id,
                    i, nx,
                    istate,
                    op,
                    ind,
                    u_inis,
                    ctx,
                    rx,
                    &xout2,
                    &xp2,
                    &curIi,
                    &canBreak,
                    adjustEvidBolusLag);
      // Adjoint handoff (liblsodaadj): a bolus ss window has period curIi and dose
      // Jacobian I; publish the period so the multistep driver re-records one
      // steady-state period's flow from the post-dose peak for the monodromy IC.
      if (op->adjoint && !doSS2) _adjSSbolusIi = curIi;
      if (isSsLag) {
        //advance the lag time
        ind->idx=*i;
        xout2 = xp2 + curIi - curLagExtra;
        // Use "real" xout for handle_evid functions.
        *istate=1;
        handle_evid(getEvid(ind, ind->ix[bi])-adjustEvidBolusLag, neq[0],
                    BadDose, InfusionRate, dose, yp,
                    xout, neq[1], ind);
        // yp is last solve or y0
        solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
                     xout2, xp2, id, i, nx, istate, op, ind, u_inis, ctx);
        for (int cur = 0; cur < overIi; ++cur) {
          pushDosingEvent(startTimeD+curLagExtra+cur*curIi,
                          rateOn, regEvid, ind);
        }
        for (k = neq[0]; k--;){
          ind->solveLast[k] = yp[k];
        }
        if (adjustEvidBolusLag != 0) {
          pushIgnoredDose(ind->ixds+1, ind);
        }
        ind->ixds--; // This dose stays in place
        // REprintf("ixds-- #3\n");
        xp2 = xout2;
      }
    } else {
      if (dur > curIi) {
        // in this case, the duration is greater than the inter-dose interval
        // number of doses before infusions turn off:
        int numDoseInf;
        double offTime, addTime;
        solveSSinfLargeDur(neq,
                           BadDose,
                           InfusionRate,
                           dose,
                           yp,
                           &xout, xp, id,
                           i, nx,
                           istate,
                           op,
                           ind,
                           u_inis,
                           ctx,
                           rx,
                           &xout2,
                           &xp2,
                           &infBixds,
                           &bi,
                           &infEixds,
                           &ei,
                           &curIi,
                           &dur,
                           &numDoseInf,
                           &offTime,
                           &addTime,
                           &canBreak);
        // Adjoint handoff: large-duration fixed-rate infusion (dur>=ii) reaches
        // a periodic steady state with numDoseInf overlapping infusions -- the
        // period effective rate is (numDoseInf+1)*R for offTime then numDoseInf*R
        // for addTime.  Fixed rate -> dR/dp==0, so the two-phase monodromy IC
        // term (kind 1) applies.
        if (op->adjoint && addTime > 0 && numDoseInf >= 1) {
          int _w, _c, _w100, _wI, _w0;
          getWh(getEvid(ind, ind->idose[infBixds]), &_w, &_c, &_w100, &_wI, &_w0);
          if (_wI == EVIDF_INF_RATE) {
            double _R = getDose(ind, ind->idose[infBixds]);
            _adjSSinfKind = 1; _adjSSinfCmt = _c;
            _adjSSinfDur  = offTime; _adjSSinfRate  = (numDoseInf + 1) * _R;
            _adjSSinfDur2 = addTime; _adjSSinfRate2 = numDoseInf * _R;
          }
        }
        skipDosingEvent = true;
        // REprintf("Assign ind->ixds to %d (idx: %d) #1\n", indf->ixds, ind->idx);
        for (int cur = 0; cur < overIi; ++cur) {
          pushDosingEvent(startTimeD + offTime + cur*curIi + curLagExtra,
                          rateOff, extraEvid, ind);
          pushDosingEvent(startTimeD + (cur+1)*curIi + curLagExtra,
                          rateOn, extraEvid, ind);
        }
        for (int cur = 0; cur < numDoseInf+1; ++cur) {
          pushDosingEvent(startTimeD + offTime + (overIi+cur)*curIi + curLagExtra,
                          rateOff, extraEvid, ind);
        }
        if (curLagExtra > 0) {
          double solveTo=curIi - curLagExtra;
          if (solveTo > offTime) {
            // infusion where the lag time does not cause the infusion
            // to occur during the inter-dose interval
            xp2 = startTimeD;
            xout2 = xp2+offTime;
            ind->idx=bi;
            ind->ixds = infBixds;
            // REprintf("Assign ind->ixds to %d (idx: %d) #2\n", ind->ixds, ind->idx);
            handle_evid(getEvid(ind, ind->idose[infBixds]), neq[0],
                        BadDose, InfusionRate, dose, yp,
                        xout, neq[1], ind);
            // yp is last solve or y0
            *istate=1;
            // yp is last solve or y0
            solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
                         xout2, xp2, id, i, nx, istate, op, ind, u_inis, ctx);
            xp2 = xout2;
            // Turn off Infusion, and solve to infusion stop time
            xout2 = xp2 + solveTo - offTime;
            ind->ixds = infEixds;
            ind->idx=ei;
            // REprintf("Assign ind->ixds to %d (idx: %d) #3\n", ind->ixds, ind->idx);
            handle_evid(getEvid(ind, ind->idose[infEixds]), neq[0],
                        BadDose, InfusionRate, dose, yp,
                        xout+dur, neq[1], ind);
            *istate=1;
            solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
                         xout2, xp2, id, i, nx, istate, op, ind, u_inis, ctx);
            pushDosingEvent(startTimeD+curLagExtra,
                            rateOn, extraEvid, ind);
          } else {
            // infusion where the lag time occurs during the inter-dose interval split
            xp2 = startTimeD;
            xout2 = xp2 + solveTo;
            ind->idx=bi;
            ind->ixds = infBixds;
            // REprintf("Assign ind->ixds to %d (idx: %d) #4\n", ind->ixds, ind->idx);

            handle_evid(getEvid(ind, ind->idose[infBixds]), neq[0],
                        BadDose, InfusionRate, dose, yp,
                        xout, neq[1], ind);
            // yp is last solve or y0
            *istate=1;
            // yp is last solve or y0
            solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
                         xout2, xp2, id, i, nx, istate, op, ind, u_inis, ctx);
            pushDosingEvent(startTimeD+offTime-solveTo,
                            rateOff, extraEvid, ind);
            pushDosingEvent(startTimeD+curLagExtra,
                            rateOn, extraEvid, ind);
          }
        } else {
          // infusion without a lag time.
          *istate=1;
          ind->idx = bi;
          ind->ixds = infBixds;
          // REprintf("Assign ind->ixds to %d (idx: %d) #5\n", ind->ixds, ind->idx);
          handle_evid(getEvid(ind, ind->idose[infBixds]), neq[0], BadDose, InfusionRate, dose, yp,
                      xout, neq[1], ind);
        }
        // REprintf("Assign ind->ixds to %d (idx: %d) #5a\n", ind->ixds, ind->idx);
        // yp is last solve or y0
        *istate=1;
        for (int ii = 0; ii < nIgnoredDoses; ++ii) {
          pushIgnoredDose(ignoreDoses[ii], ind);
        }
      } else if (ind->err) {
        printErr(ind->err, ind->id);
        badSolveExit(*i);
      } else {
        // Infusion
        dur2 = curIi-dur;
        if (isModeled && isSsLag) {
          // adjust start time for modeled w/ssLag
          startTimeD = (ind->fns ? ind->fns->gettime(ind->idose[infFixds],ind) : getTime(ind->idose[infFixds],ind));
        }
        solveSSinf(neq,
                   BadDose,
                   InfusionRate,
                   dose,
                   yp,
                   &xout, xp, id,
                   i, nx,
                   istate,
                   op,
                   ind,
                   u_inis,
                   ctx,
                   rx,
                   &xout2,
                   &xp2,
                   &infBixds,
                   &bi,
                   &infEixds,
                   &ei,
                   &curIi,
                   &dur,
                   &dur2,
                   &canBreak);
        // Adjoint handoff: a fixed-rate (whI == EVIDF_INF_RATE) infusion with a
        // single on/off window per period (dur < ii, this solveSSinf branch)
        // has dR/dp == 0, so the rk4s driver can record its steady-state period
        // for the monodromy IC term.  rate = getDose of the ON dose.
        if (op->adjoint && dur > 0 && dur2 >= 0) {
          int _w, _c, _w100, _wI, _w0;
          getWh(getEvid(ind, ind->idose[infBixds]), &_w, &_c, &_w100, &_wI, &_w0);
          if (_wI == EVIDF_INF_RATE) {
            _adjSSinfKind = 1; _adjSSinfCmt = _c;
            _adjSSinfDur = dur; _adjSSinfDur2 = dur2;
            _adjSSinfRate = getDose(ind, ind->idose[infBixds]);
            _adjSSinfRate2 = 0.0;                 // OFF phase (dur<ii)
          }
        }
        *istate=1;
        // REprintf("Assign ind->ixds to %d (idx: %d) #6\n", ind->ixds, ind->idx);
        for (k = neq[0]; k--;){
          ind->solveLast[k] = yp[k];
        }
        xp2 = xout2;
        bool doNoLag = false;
        if (isSsLag) {
          double totTime = xp2 + dur + dur2 - curLagExtra;
          if (curLagExtra > 0) {
            if (isSameTimeOp(curLagExtra+dur, curIi)) {
              // REprintf("isSameTimeOp(curLagExtra:%f+dur:%f, curIi:%f)\n",
              //          curLagExtra, dur, curIi);
              ind->idx  = bi;
              ind->ixds = infBixds;
              handle_evid(getEvid(ind, ind->idose[infBixds]), neq[0],
                          BadDose, InfusionRate, dose, yp,
                          xout, neq[1], ind);
              xp2   = startTimeD;
              xout2 = startTimeD + dur;
              // yp is last solve or y0
              *istate=1;
              // yp is last solve or y0
              solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
                           xout2, xp2, id, i, nx, istate, op, ind, u_inis, ctx);
              ind->idx=ei;
              ind->ixds = infEixds;
              handle_evid(getEvid(ind, ind->idose[infEixds]), neq[0],
                          BadDose, InfusionRate, dose, yp,
                          xout, neq[1], ind);
              ind->idx=ei;
              ind->ixds = infEixds;
              for (int cur = 0; cur < (overIi+1); ++cur) {
                pushDosingEvent(startTimeD+curLagExtra+cur*curIi,
                                rateOn, regEvid, ind);
                pushDosingEvent(startTimeD+curLagExtra+dur+cur*curIi,
                                rateOff, regEvid, ind);
              }
              ind->idx=fi;
              ind->ixds = infFixds;
            } else if (curLagExtra > dur2) {
              // REprintf("(curLagExtra: %f > dur2: %f)\n",
              //          curLagExtra, dur2);
              // dosing time occurs during the infusion
              double solveExtra=dur+dur2-curLagExtra;
              ind->idx=bi;
              ind->ixds = infBixds;
              handle_evid(getEvid(ind, ind->idose[infBixds]), neq[0],
                          BadDose, InfusionRate, dose, yp,
                          xout, neq[1], ind);
              xp2   = startTimeD;
              xout2 = startTimeD + solveExtra;
              // yp is last solve or y0
              *istate=1;
              // yp is last solve or y0
              solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
                           xout2, xp2, id, i, nx, istate, op, ind, u_inis, ctx);

              for (int cur = 0; cur < (overIi+1); ++cur) {
                pushDosingEvent(startTimeD+dur-solveExtra+cur*curIi,
                                rateOff, (cur == 0) ? extraEvid : regEvid, ind);
                pushDosingEvent(startTimeD+dur+dur2-solveExtra+cur*curIi,
                                rateOn, regEvid, ind);
              }
              pushDosingEvent(startTimeD+dur-solveExtra+(overIi+1)*curIi,
                              rateOff, overIi == 0 ? extraEvid : regEvid, ind);
              ind->idx=fi;
              ind->ixds = infFixds;
            } else {
              if (xp2 + dur < totTime) {
                xout2 = xp2 + dur;
              } else {
                xout2 = totTime;
              }
              ind->idx=bi;
              ind->ixds = infBixds;
              handle_evid(getEvid(ind, ind->idose[infBixds]), neq[0],
                          BadDose, InfusionRate, dose, yp,
                          xout, neq[1], ind);
              // yp is last solve or y0
              *istate=1;
              // yp is last solve or y0
              solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
                           xout2, xp2, id, i, nx, istate, op, ind, u_inis, ctx);
              if (!isSameTimeOp(totTime, xout2)) {
                // don't give the infusion off dose
                xp2 = xout2;
                // Turn off Infusion, solve (dur-ii)
                xout2 = totTime;
                ind->ixds = infEixds;
                ind->idx = ei;
                handle_evid(getEvid(ind, ind->idose[infEixds]), neq[0],
                            BadDose, InfusionRate, dose, yp,
                            xout+dur, neq[1], ind);
                // yp is last solve or y0
                *istate=1;
                solveWith1Pt(neq, BadDose, InfusionRate, dose, yp,
                             xout2, xp2, id, i, nx, istate, op, ind, u_inis, ctx);
              }
              for (int cur = 0; cur < (overIi+1); ++cur) {
                pushDosingEvent(startTimeD+curLagExtra+cur*curIi,
                                rateOn, regEvid, ind);
                pushDosingEvent(startTimeD+curLagExtra+dur+cur*curIi,
                                rateOff, regEvid, ind);
              }
            }
          } else {
            doNoLag = true;
          }
        } else {
          doNoLag = true;
        }
        if (doNoLag) {
          ind->idx=bi;
          ind->ixds=infBixds;
          handle_evid(getEvid(ind, ind->idose[infBixds]), neq[0],
                      BadDose, InfusionRate, dose, yp,
                      xout, neq[1], ind);
          pushDosingEvent(startTimeD+dur,
                          rateOff, extraEvid, ind);
        }
        *istate=1;
        for (k = neq[0]; k--;){
          ind->solveLast[k] = yp[k];
        }
        xp2 = xout2;
        ind->idx=fi;
        ind->ixds = infFixds;
        for (int ii = 0; ii < nIgnoredDoses; ++ii) {
          pushIgnoredDose(ignoreDoses[ii], ind);
        }
      }
    }
    if (doSS2){
      // Publish the new-regimen steady state Y_ss_new (yp before adding back the
      // saved pre-ss2 state) for the adjoint superposition term.
      if (op->adjoint) { _adjSS2 = 1; _adjSS2peak.assign(yp, yp + neq[0]); }
      // Add at the end
      for (j = neq[0];j--;) yp[j]+=ind->solveSave[j];
    }
    if (!doSSinf && !isSsLag && !skipDosingEvent){
      // REprintf("handleEvid %d %d %d\n", doSSinf, isSsLag, skipDosingEvent);
      handle_evid(getEvid(ind, ind->ix[*i]), neq[0],
                  BadDose, InfusionRate, dose, yp,
                  xout, neq[1], ind);
    }
    // if (isRateDose) {
    //   REprintf("at %f\n", getAllTimes(ind, ind->idose[infFixds]));
    //   REprintf("\tinfFixds(idx:%d/ixds:%d): %d %f %f (ignored: %d)\n",
    //            ind->idose[infFixds], infFixds,
    //            getEvid(ind, ind->idose[infFixds]),
    //            getAllTimes(ind, ind->idose[infFixds]),
    //            getDose(ind, ind->idose[infFixds]),
    //            isIgnoredDose(ind, infFixds));
    //   REprintf("\tinfSixds(idx:%d/ixds:%d): %d %f %f (ignored: %d)\n",
    //            ind->idose[infSixds], infSixds,
    //            getEvid(ind, ind->idose[infSixds]),
    //            getAllTimes(ind, ind->idose[infSixds]),
    //            getDose(ind, ind->idose[infSixds]),
    //            isIgnoredDose(ind, infSixds));
    //   REprintf("\tinfBixds(idx:%d/ixds:%d): %d %f %f (ignored: %d)\n",
    //            ind->idose[infBixds], infBixds,
    //            getEvid(ind, ind->idose[infBixds]),
    //            getAllTimes(ind, infBixds),
    //            getDose(ind, ind->idose[infBixds]),
    //            isIgnoredDose(ind, infBixds));
    //   REprintf("\tinfBixds2(idx:%d/ixds:%d): %d %f %f (ignored: %d)\n",
    //            ind->idose[infBixds2], infBixds2,
    //            getEvid(ind, ind->idose[infBixds2]),
    //            getAllTimes(ind, ind->idose[infBixds2]),
    //            getDose(ind, ind->idose[infBixds2]),
    //            isIgnoredDose(ind, infBixds2));
    //   REprintf("\tinfEixds(idx:%d/ixds:%d): %d %f %f (ignored: %d)\n",
    //            ind->idose[infEixds], infEixds,
    //            getEvid(ind, ind->idose[infEixds]),
    //            getAllTimes(ind, ind->idose[infEixds]),
    //            getDose(ind, ind->idose[infEixds]),
    //            isIgnoredDose(ind, infEixds));
    // }
    ind->doSS=0;
    ind->ixds=oldIxds; ind->idx=oldIdx;
  }
  updateExtraDoseGlobals(ind);
}

// Grow ind->solve if slot i (and optionally i+1) are beyond the current
// allocated capacity.  Safe to call between ODE steps -- no integrator holds
// a live pointer into ind->solve at those points.
static inline void _growSolveIfNeeded(rx_solving_options_ind *ind,
                                      rx_solving_options *op,
                                      int i, int need_next) {
  if (!ind->indOwnAlloc) return;
  int need = need_next ? i + 2 : i + 1;
  if (need > ind->solveAllocN) {
    int newN = need + EVID_EXTRA_SIZE;
    double *ns = (double*)realloc(ind->solve, (int64_t)op->neq * newN * sizeof(double));
    if (ns) {
      memset(ns + (int64_t)op->neq * ind->solveAllocN, 0,
             (int64_t)op->neq * (newN - ind->solveAllocN) * sizeof(double));
      ind->solve = ns;
      ind->solveAllocN = newN;
    }
    // On realloc failure ind->solve is unchanged and the access will be OOB,
    // but there is no good recovery path here.
  }
}

static inline void
updateSolve(rx_solving_options_ind *ind, rx_solving_options *op, int *neq,
            double &xout,
            int &i, int &nx) {
  // Grow if needed before accessing getSolve(i) and optionally getSolve(i+1).
  _growSolveIfNeeded(ind, op, i, (i + 1 != nx));
  if (i+1 != nx) {
    std::copy(getSolve(i), getSolve(i) + rxEffNeq(ind, op), getSolve(i+1));
  }
  calc_lhs(neq[1], xout, getSolve(i), ind->lhs);
}

//================================================================================
// Inductive linearization routines
extern "C" void ind_indLin0(rx_solve *rx, rx_solving_options *op, int solveid,
                            t_update_inis u_inis) {
  clock_t t0 = clock();
  assignFuns();
  int i;
  int neq[2];
  neq[1] = solveid;
  /* double *yp = &yp0[neq[1]*neq[0]]; */
  int nx;
  rx_solving_options_ind *ind;
  double xout, xoutp;
  int *rc;
  double *yp;
  int idid = 0;
  int localBadSolve = 0;
  ind = &(rx->subjects[neq[1]]);
  // Per-individual effective neq (rxEffNeq) needs ind in scope.
  neq[0] = rxEffNeq(ind, op);
  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) return;
  nx = ind->n_all_times;
  rc= ind->rc;
  double xp = ind->all_times[0];
  xoutp=xp;
  ind->solvedIdx = 0;
  for (i=0; i<ind->n_all_times; i++) {
    ind->idx=i;
    ind->linSS=0;
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
            _rtime[_raw] = getAllTimes(ind, _raw);  // absolute time stored by updateDur/updateRate
          }
        }
      }
      reSortMainTimeline(ind, i);
      ind->mainSorted = 1;
    }
    xout = ind->timeThread[ind->ix[i]];
    _growSolveIfNeeded(ind, op, i, 1);
    yp = getSolve(i);
    if(getEvid(ind, ind->ix[i]) != 3 && !isSameTime(xout, xp)) {
      if (ind->err){
        *rc = -1000;
        // Bad Solve => NA
        badSolveExit(i);
        localBadSolve = 1;
      } else {
        preSolve(op, ind, xoutp, xout, yp);
        idid = indLin(solveid, op, ind, xoutp, yp, xout, ind->InfusionRate, ind->on,
                      (ind->fns ? ind->fns->me : NULL), (ind->fns ? ind->fns->indf : NULL));
        xoutp=xout;
        postSolve(neq, &idid, rc, &i, yp, NULL, 0, true, ind, op, rx);
      }
    }
    ind->_newind = 2;
    if (!localBadSolve){
      ind->idx = i;
      if (getEvid(ind, ind->ix[i]) == 3) {
        handleEvid3(ind, op, rx, neq, &xp, &xout,  yp, &idid, u_inis);
      } else if (handleEvid1(&i, rx, neq, yp, &xout)){
        handleSS(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                 xp, ind->id, &i, ind->n_all_times, &idid, op, ind, u_inis, NULL);
        if (ind->wh0 == 30){
          yp[ind->cmt] = op->inits[ind->cmt];
        }
        if (rx->istateReset) idid = 1;
        xp = xout;
      }
      int _mtime_requeued = 0;
      if (rx->nMtime > 0) {
        if (recomputeMtimeIfNeeded(rx, ind, yp, i, xout)) {
          ind->mainSorted = 0;
          _mtime_requeued = 1;
        }
      }
      if (rx->needSort & needSortAlag) {
        if (refreshLagTimesIfNeeded(rx, ind, yp, i + 1, xout)) {
          ind->mainSorted = 0;
        }
      }
      updateSolve(ind, op, neq, xout, i, ind->n_all_times);
      ind->slvr_counter[0]++; // doesn't need do be critical; one subject at a time.
      if (_mtime_requeued) i--;
    }
    ind->solvedIdx = i;
  }
  ind->solveTime += ((double)(clock() - t0))/CLOCKS_PER_SEC;
}

extern "C" void ind_indLin(rx_solve *rx,
                           int solveid, t_update_inis u_inis){
  assignFuns();
  rx_solving_options *op = &op_global;
  ind_indLin0(rx, op, solveid, u_inis);
}


extern "C" void par_indLin(rx_solve *rx){
  assignFuns();
  rx_solving_options *op = &op_global;
#ifdef _OPENMP
  int cores = op->cores;
#else
  int cores = 1;
#endif
  uint32_t nsub = rx->nsub, nsim = rx->nsim;
  int nsolve = (int)(nsim*nsub); // safe: overflow guard ensures nsim*nsub <= INT_MAX
  int displayProgress = (op->nDisplayProgress <= nsolve);
  clock_t t0 = clock();
  int curTick=0;
  int cur=0;
  // Breaking of of loop ideas came from http://www.thinkingparallel.com/2007/06/29/breaking-out-of-loops-in-openmp/
  // http://permalink.gmane.org/gmane.comp.lang.r.devel/27627
  // Use omp atomic read/write for thread-safe flag access across OpenMP threads
  int abort = 0;
  uint32_t seed0 = getRxSeed1(cores);
#ifdef _OPENMP
#pragma omp parallel for num_threads(cores) schedule(dynamic,1)
#endif
  for (int solveid = 0; solveid < nsolve; solveid++){
    int localAbort;
#pragma omp atomic read
    localAbort = abort;
    if (localAbort == 0){
      setSeedEng1(seed0 + solveid - 1);
      ind_indLin(rx, solveid, update_inis);
      if (displayProgress){
#pragma omp critical
        cur++;
#ifdef _OPENMP
        if (omp_get_thread_num() == 0) // only in master thread!
#endif
          {
            curTick = par_progress(cur, nsolve, curTick, cores, t0, 0);
            int localAbort2;
#pragma omp atomic read
            localAbort2 = abort;
            if (localAbort2 == 0){
              if (checkInterrupt()) {
                int newAbort = 1;
#pragma omp atomic write
                abort = newAbort;
              }
            }
          }
      }
    }
  }
  setRxSeedFinal(seed0 + (uint32_t)nsolve);
  if (abort == 1){
    op->abort = 1;
    par_progress(cur, nsolve, curTick, cores, t0, 1);
  } else {
    if (displayProgress && curTick < 50) par_progress(nsolve, nsolve, curTick, cores, t0, 0);
  }
  if (displayProgress) {
    int doIt = isProgSupported();
    if (doIt == -1){
    } else if (isRstudio() || doIt == 0){
      RSprintf("\n");
    } else {
      RSprintf("\r                                                                                \r");
    }
  }
}




// ================================================================================
// liblsoda
// Discrete-adjoint (method="liblsodaadj") recording hooks, defined in the
// #included lsoda_adjoint.cpp; ind_liblsoda0 installs them on the ctx common
// block when recording is active for this thread.
extern "C" int  lsAdjIsActive(void);
extern "C" void lsAdjSetActive(int a);
extern "C" void lsAdjInitStep(struct lsoda_context_t *ctx);
extern "C" void lsAdjPushStep(struct lsoda_context_t *ctx);

extern "C" void ind_liblsoda0(rx_solve *rx, rx_solving_options *op, struct lsoda_opt_t opt, int solveid,
                              t_dydt_liblsoda dydt_liblsoda, t_update_inis u_inis) {
  clock_t t0 = clock();
  int i;
  int neq[2];
  // Here we pick the sorted solveid
  // rx->ordId[solveid]-1
  // This -1 is because R is 1 indexed and C/C++ is 0 indexed
  // This uses data.table for ordering which will return a 1 as the first item
  // This way we solve based on the item that takes the likely takes most time to solve
  //
  // First this is ordered by the number of times needed to solve
  // If called externally again this is then ordered by the total time that the solver spent in an id.
  //
  neq[1] = rx->ordId[solveid]-1;
  /* double *yp = &yp0[neq[1]*neq[0]]; */
  rx_solving_options_ind *ind = &(rx->subjects[neq[1]]);
  // Per-individual effective neq honors ind->neqOverride (rxEffNeq); under
  // a parallel inner-pred alternation in nlmixr2est this is the predNeq
  // for this thread's subject, while op->neq remains the full neq.
  neq[0] = rxEffNeq(ind, op);
  double xout;
  int *rc;
  double *yp;
  int neqOde = *neq - op->numLin - op->numLinSens;
  int localBadSolve = 0;

  // -- LSODA context: use per-thread pool when available (avoids per-subject
  //    malloc/free of the large alloc_mem working-array block).
  bool _usingPool = (!__lsodaCtxPool.empty());
  lsoda_pool_t *_pool = _usingPool
    ? &__lsodaCtxPool[rx_get_thread((int)__lsodaCtxPool.size())]
    : NULL;
  struct lsoda_context_t *ctx;
  if (_usingPool) {
    ctx = &_pool->ctx;
  } else {
    ctx = lsoda_create_ctx();
    if (ctx == NULL) {
      rxSolveFreeC();
      (Rf_error)(_("not enough memory for lsoda context"));
    }
  }
  ctx->function = (_lsoda_f)dydt_liblsoda;
  ctx->data = neq;
  ctx->neq = neqOde;
  ctx->state = 1;
  ctx->error = NULL;
  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) {
    if (!_usingPool) free(ctx);
    return;
  }
  // x = ind->all_times;
  rc= ind->rc;
  double xp = ind->all_times[0];
  // Use per-individual tolerance arrays if available (assigned by
  // _setIndPointersByThread and scaled by iniSubject).
  if (ind->rtol2 != NULL && ind->atol2 != NULL) {
    opt.rtol = ind->rtol2;
    opt.atol = ind->atol2;
  }
  if (_usingPool) {
    _pool->opt = opt; // copy opt into pool so ctx->opt stays valid across subjects
    if (_pool->allocated_neq != neqOde) {
      // neq changed (or first use): free old memory and re-prepare
      if (_pool->allocated_neq > 0 && ctx->common != NULL) {
        lsoda_free(ctx);
        ctx->common = NULL;
      }
      if (!lsoda_prepare(ctx, &_pool->opt)) {
        freeLsodaCtxPool();
        rxSolveFreeC();
        (Rf_error)(_("not enough memory for lsoda context"));
      }
      _pool->allocated_neq = neqOde;
    } else {
      // neq changed or reusing pool slot: free and re-prepare to guarantee
      // identical state to a first-subject solve (lsoda_reset is insufficient
      // because check_opt normalises mxordn/mxords on state=1 entry).
      lsoda_free(ctx);
      ctx->common = NULL;
      if (!lsoda_prepare(ctx, &_pool->opt)) {
        freeLsodaCtxPool();
        rxSolveFreeC();
        (Rf_error)(_("not enough memory for lsoda context"));
      }
      _pool->allocated_neq = neqOde;
      ctx->function = (_lsoda_f)dydt_liblsoda;
      ctx->data = neq;
      ctx->neq = neqOde;
      ctx->state = 1;
      ctx->error = NULL;
      ctx->opt = &_pool->opt;
    }
  } else {
    lsoda_prepare(ctx, &opt);
  }
  /* Record the rxode2 subject id on the LSODA context so intdy.c can
     attribute its warnings to the correct subject. Placed before `memory`
     in lsoda_common_t so lsoda_reset's memset doesn't clobber it on
     pooled-context reuse. */
  ctx->common->id = ind->id;
  // Discrete-adjoint recording (method="liblsodaadj"): install the per-step /
  // init hooks when the adjoint driver has flagged this thread active; set
  // unconditionally so a pooled ctx never carries a stale hook onto a following
  // non-adjoint subject.
  if (lsAdjIsActive()) {
    ctx->common->adjInit = lsAdjInitStep;
    ctx->common->adjPush = lsAdjPushStep;
  } else {
    ctx->common->adjInit = NULL;
    ctx->common->adjPush = NULL;
  }
  ind->solvedIdx = 0;
  for(i=0; i< ind->n_all_times; i++) {
    ind->idx=i;
    ind->linSS=0;
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
            _rtime[_raw] = getAllTimes(ind, _raw);  // absolute time stored by updateDur/updateRate
          }
        }
      }
      reSortMainTimeline(ind, i);
      ind->mainSorted = 1;
    }
    _growSolveIfNeeded(ind, op, i, 1);
    yp = getSolve(i);
    xout = ind->timeThread[ind->ix[i]];
    if (getEvid(ind, ind->ix[i]) != 3) {
      if (ind->err){
        *rc = -1000;
        // Bad Solve => NA
        badSolveExit(i);
        localBadSolve = 1;
      } else {
        if (handleExtraDose(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                            xp, ind->id, &i, ind->n_all_times,
                            &(ctx->state), op, ind, u_inis, ctx)) {
          if (!localBadSolve && !isSameTime(ind->extraDoseNewXout, xp)) {
            preSolve(op, ind, xp, ind->extraDoseNewXout, yp);
            lsoda(ctx, yp, &xp, ind->extraDoseNewXout);
            copyLinCmt(neq, ind, op, yp);
            postSolve(neq, &(ctx->state), rc, &i, yp, NULL, 0, false, ind, op, rx);
            if (*rc < 0) localBadSolve = 1;
          }
          if (!localBadSolve) {
            int idx = ind->idx;
            int ixds= ind->ixds;
            int trueIdx = ind->extraDoseTimeIdx[ind->idxExtra];
            ind->idx = -1-trueIdx;
            handle_evid(ind->extraDoseEvid[trueIdx], neq[0],
                        ind->BadDose, ind->InfusionRate, ind->dose, yp, xout, neq[1], ind);
            ctx->state = 1;
            ind->idx = idx;
            ind->ixds = ixds;
            ind->idxExtra++;
            if (!isSameTime(xout, ind->extraDoseNewXout)) {
              preSolve(op, ind, ind->extraDoseNewXout, xout, yp);
              lsoda(ctx,yp, &ind->extraDoseNewXout, xout);
              copyLinCmt(neq, ind, op, yp);
              postSolve(neq, &(ctx->state), rc, &i, yp, NULL, 0, false, ind, op, rx);
              if (*rc < 0) localBadSolve = 1;
            }
            xp =  ind->extraDoseNewXout;
          }
        }
        if (!localBadSolve && !isSameTime(xout, xp)) {
          preSolve(op, ind, xp, xout, yp);
          if (ctx->state == 2) {
            // liblsoda continuation mode does not call f at xp (it uses the cached
            // derivative from the previous step).  Explicitly call f at xp so that
            // any model side-effects that depend on _atEventTime (e.g. evid_() push
            // doses) fire at the correct time, matching dop853 behaviour.
            std::vector<double> _evid_tmpydot(neqOde);
            (*ctx->function)(xp, yp, _evid_tmpydot.data(), ctx->data);
          }
          lsoda(ctx, yp, &xp, xout);
          copyLinCmt(neq, ind, op, yp);
          postSolve(neq, &(ctx->state), rc, &i, yp, NULL, 0, false, ind, op, rx);
          if (*rc < 0) localBadSolve = 1;
        }
        xp = xout;
      }
    }
    ind->_newind = 2;
    if (!localBadSolve){
      ind->idx = i;
      if (getEvid(ind, ind->ix[i]) == 3) {
        handleEvid3(ind, op, rx, neq, &xp, &xout,  yp, &(ctx->state), u_inis);
      } else if (handleEvid1(&i, rx, neq, yp, &xout)) {
        // Pause discrete-adjoint recording across the steady-state pre-solve so
        // the recorded window starts cleanly at Y_ss (its repeated per-period
        // doses must not pollute the segment-event reconstruction); the IC
        // sensitivity dY_ss/dp is added analytically in the backward sweep.
        int _lsWasActive = lsAdjIsActive();
        if (_lsWasActive) lsAdjSetActive(0);
        handleSS(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                 xp, ind->id, &i, ind->n_all_times, &(ctx->state), op, ind, u_inis, ctx);
        if (_lsWasActive) { lsAdjSetActive(1); lsAdjSnapshotSs(); }
        if (ind->wh0 == EVID0_OFF){
          yp[ind->cmt] = op->inits[ind->cmt];
        }
        if (rx->istateReset) ctx->state = 1;
        xp = xout;
      }
      int _mtime_requeued = 0;
      if (rx->nMtime > 0) {
        if (recomputeMtimeIfNeeded(rx, ind, yp, i, xout)) {
          ind->mainSorted = 0;
          _mtime_requeued = 1;
        }
      }
      if (rx->needSort & needSortAlag) {
        if (refreshLagTimesIfNeeded(rx, ind, yp, i + 1, xout)) {
          ind->mainSorted = 0;
        }
      }
      updateSolve(ind, op, neq, xout, i, ind->n_all_times);
      ind->slvr_counter[0]++; // doesn't need do be critical; one subject at a time.
      if (_mtime_requeued) i--;
      /* for(j=0; j<neq[0]; j++) ret[neq[0]*i+j] = yp[j]; */
    }
    ind->solvedIdx = i;
  }
  // Reset LHS to NA
  if (_usingPool) {
    // Pool: keep the context alive for the next subject; just clean up any error.
    if (ctx->error) {
      free(ctx->error);
      ctx->error = NULL;
    }
  } else {
    lsoda_free(ctx);
    free(ctx);
  }
  ind->solveTime += ((double)(clock() - t0))/CLOCKS_PER_SEC;
}

extern "C" void ind_liblsoda(rx_solve *rx, int solveid,
                             t_dydt_liblsoda dydt, t_update_inis u_inis){
  rx_solving_options *op = rx->op;
  struct lsoda_opt_t opt = {0};
  opt.ixpr = 0; // No extra printing...
  // Unlike traditional lsoda, these are vectors.
  opt.rtol = op->rtol2;
  opt.atol = op->atol2;
  opt.itask = 1;
  opt.mxstep = op->mxstep;
  opt.mxhnil = op->mxhnil;
  opt.mxordn = op->MXORDN;
  opt.mxords = op->MXORDS;
  opt.h0 = op->H0;
  opt.hmax = op->hmax2;
  opt.hmin = op->HMIN;
  opt.hmxi = op->hmxi;
  ind_liblsoda0(rx, op, opt, solveid, dydt, u_inis);
}


extern "C" int getRxThreads(const int64_t n, const bool throttle);

extern "C" void ind_linCmt0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq,
                            t_dydt c_dydt, t_update_inis u_inis);

extern "C" void par_linCmt(rx_solve *rx) {
  rx_solving_options *op = &op_global;
#ifdef _OPENMP
  int cores = op->cores;
#else
  int cores = 1;
#endif
  uint32_t nsub = rx->nsub, nsim = rx->nsim;
  int nsolve = (int)(nsim*nsub); // safe: overflow guard ensures nsim*nsub <= INT_MAX
  int displayProgress = (op->nDisplayProgress <= nsolve);
  clock_t t0 = clock();
  int neq[2];
  neq[0] = op->neq;
  neq[1] = 0;
  /* double *yp0=(double*) malloc((op->neq)*nsim*nsub*sizeof(double)); */
  int curTick=0;
  int cur=0;
  // Breaking of of loop ideas came from http://www.thinkingparallel.com/2007/06/29/breaking-out-of-loops-in-openmp/
  // http://permalink.gmane.org/gmane.comp.lang.r.devel/27627
  // It was buggy due to Rprint.  Use REprint instead since Rprint calls the interrupt every so often....
  // Use omp atomic read/write for thread-safe flag access across OpenMP threads
  int abort = 0;
  uint32_t seed0 = getRxSeed1(cores);
#ifdef _OPENMP
#pragma omp parallel for num_threads(cores)
#endif
  for (int thread=0; thread < cores; thread++) {
    for (int solveid = thread; solveid < nsolve; solveid+=cores){
      int localAbort;
#pragma omp atomic read
        localAbort = abort;
      if (localAbort == 0){
        setSeedEng1(seed0 + rx->ordId[solveid] - 1);

        ind_linCmt0(rx, op, solveid, neq, dydt, update_inis);

        if (displayProgress && thread == 0) {
#pragma omp critical
          cur++;
#ifdef _OPENMP
          if (omp_get_thread_num() == 0) // only in master thread!
#endif
            {
              curTick = par_progress(cur, nsolve, curTick, cores, t0, 0);
              int localAbort2;
#pragma omp atomic read
              localAbort2 = abort;
              if (localAbort2 == 0){
                if (checkInterrupt()) {
                  int newAbort = 1;
#pragma omp atomic write
                  abort = newAbort;
                }
              }
            }
        }
      }
    }
  }
  setRxSeedFinal(seed0 + (uint32_t)nsolve);
  if (abort == 1){
    op->abort = 1;
    /* yp0 = NULL; */
    par_progress(cur, nsolve, curTick, cores, t0, 1);
  } else {
    if (displayProgress && curTick < 50) par_progress(nsolve, nsolve, curTick, cores, t0, 0);
  }
  if (displayProgress) {
    int doIt = isProgSupported();
    if (doIt == -1){
    } else if (isRstudio() || doIt == 0){
      RSprintf("\n");
    } else {
      RSprintf("\r                                                                                \r");
    }
  }
}

extern "C" void par_liblsodaR(rx_solve *rx) {
  rx_solving_options *op = &op_global;
#ifdef _OPENMP
  int cores = op->cores;
#else
  int cores = 1;
#endif
  uint32_t nsub = rx->nsub, nsim = rx->nsim;
  int nsolve = (int)(nsim*nsub); // safe: overflow guard ensures nsim*nsub <= INT_MAX
  int displayProgress = (op->nDisplayProgress <= nsolve);
  clock_t t0 = clock();
  /* double *yp0=(double*) malloc((op->neq)*nsim*nsub*sizeof(double)); */
  struct lsoda_opt_t opt = {0};
  opt.ixpr = 0; // No extra printing...
  // Unlike traditional lsoda, these are vectors.
  opt.rtol = op->rtol2;
  opt.atol = op->atol2;
  opt.itask = 1;
  opt.mxstep = op->mxstep;
  opt.mxhnil = op->mxhnil;
  opt.mxordn = op->MXORDN;
  opt.mxords = op->MXORDS;
  opt.h0 = op->H0;
  opt.hmax = op->hmax2;
  opt.hmin = op->HMIN;
  opt.hmxi = op->hmxi;
  int curTick=0;
  int cur=0;
  // Breaking of of loop ideas came from http://www.thinkingparallel.com/2007/06/29/breaking-out-of-loops-in-openmp/
  // http://permalink.gmane.org/gmane.comp.lang.r.devel/27627
  // It was buggy due to Rprint.  Use REprint instead since Rprint calls the interrupt every so often....
  // Use omp atomic read/write for thread-safe flag access across OpenMP threads
  int abort = 0;
  uint32_t seed0 = getRxSeed1(cores);
#ifdef _OPENMP
#pragma omp parallel for num_threads(cores)
#endif
  for (int thread=0; thread < cores; thread++) {
    for (int solveid = thread; solveid < nsolve; solveid+=cores){
      int localAbort;
#pragma omp atomic read
        localAbort = abort;
      if (localAbort == 0){
        setSeedEng1(seed0 + rx->ordId[solveid] - 1 );
        ind_liblsoda0(rx, op, opt, solveid, dydt_liblsoda, update_inis);
        if (displayProgress && thread == 0) {
#pragma omp critical
          cur++;
#ifdef _OPENMP
          if (omp_get_thread_num() == 0) // only in master thread!
#endif
            {
              curTick = par_progress(cur, nsolve, curTick, cores, t0, 0);
              int localAbort2;
#pragma omp atomic read
              localAbort2 = abort;
              if (localAbort2 == 0){
                if (checkInterrupt()) {
                  int newAbort = 1;
#pragma omp atomic write
                  abort = newAbort;
                }
              }
            }
        }
      }
    }
  }
  setRxSeedFinal(seed0 + (uint32_t)nsolve);
  if (abort == 1){
    op->abort = 1;
    /* yp0 = NULL; */
    par_progress(cur, nsolve, curTick, cores, t0, 1);
  } else {
    if (displayProgress && curTick < 50) par_progress(nsolve, nsolve, curTick, cores, t0, 0);
  }
  if (displayProgress) {
    int doIt = isProgSupported();
    if (doIt == -1){
    } else if (isRstudio() || doIt == 0){
      RSprintf("\n");
    } else {
      RSprintf("\r                                                                                \r");
    }
  }
}

extern "C" void par_liblsoda(rx_solve *rx){
  rx_solving_options *op = &op_global;
#ifdef _OPENMP
  int cores = op->cores;
#else
  int cores = 1;
#endif
  uint32_t nsub = rx->nsub, nsim = rx->nsim;
  int nsolve = (int)(nsim*nsub); // safe: overflow guard ensures nsim*nsub <= INT_MAX
  int displayProgress = (op->nDisplayProgress <= nsolve);
  clock_t t0 = clock();
  /* double *yp0=(double*) malloc((op->neq)*nsim*nsub*sizeof(double)); */
  struct lsoda_opt_t opt = {0};
  opt.ixpr = 0; // No extra printing...
  // Unlike traditional lsoda, these are vectors.
  opt.rtol = op->rtol2;
  opt.atol = op->atol2;
  opt.itask = 1;
  opt.mxstep = op->mxstep;
  opt.mxhnil = op->mxhnil;
  opt.mxordn = op->MXORDN;
  opt.mxords = op->MXORDS;
  opt.h0 = op->H0;
  opt.hmax = op->hmax2;
  opt.hmin = op->HMIN;
  opt.hmxi = op->hmxi;
  int curTick=0;
  int cur=0;
  // Breaking of of loop ideas came from http://www.thinkingparallel.com/2007/06/29/breaking-out-of-loops-in-openmp/
  // http://permalink.gmane.org/gmane.comp.lang.r.devel/27627
  // It was buggy due to Rprint.  Use REprint instead since Rprint calls the interrupt every so often....
  // Use omp atomic read/write for thread-safe flag access across OpenMP threads
  int abort = 0;
  uint32_t seed0 = getRxSeed1(cores);
#ifdef _OPENMP
#pragma omp parallel for num_threads(op->cores) schedule(dynamic,1)
#endif
  for (int solveid = 0; solveid < nsolve; solveid++){
    int localAbort;
#pragma omp atomic read
        localAbort = abort;
    if (localAbort == 0){
      setSeedEng1(seed0 + rx->ordId[solveid] - 1);
      ind_liblsoda0(rx, op, opt, solveid, dydt_liblsoda, update_inis);
      if (displayProgress){
#pragma omp critical
        cur++;
#ifdef _OPENMP
        if (omp_get_thread_num() == 0) // only in master thread!
#endif
          {
            curTick = par_progress(cur, nsolve, curTick, cores, t0, 0);
            int localAbort2;
#pragma omp atomic read
            localAbort2 = abort;
            if (localAbort2 == 0){
              if (checkInterrupt()) {
                int newAbort = 1;
#pragma omp atomic write
                abort = newAbort;
              }
            }
          }
      }
    }
  }
  setRxSeedFinal(seed0 + (uint32_t)nsolve);
  if (abort == 1){
    op->abort = 1;
    /* yp0 = NULL; */
    par_progress(cur, nsolve, curTick, cores, t0, 1);
  } else {
    if (displayProgress && curTick < 50) par_progress(nsolve, nsolve, curTick, cores, t0, 0);
  }
  if (displayProgress) {
    int doIt = isProgSupported();
    if (doIt == -1){
    } else if (isRstudio() || doIt == 0){
      RSprintf("\n");
    } else {
      RSprintf("\r                                                                                \r");
    }
  }
}


double *global_InfusionRatep;
unsigned int global_InfusionRatei = 0;
double *global_InfusionRate(unsigned int mx){
  if (mx >= global_InfusionRatei){
    bool first = (global_InfusionRatei == 0);
    global_InfusionRatei = mx+1024;
    if (first) {
      global_InfusionRatep = R_Calloc(global_InfusionRatei, double);
    } else {
      global_InfusionRatep = R_Realloc(global_InfusionRatep, global_InfusionRatei, double);
    }
  }
  return global_InfusionRatep;
}

double *global_scalep;
unsigned int global_scalei = 0;
double *global_scale(unsigned int mx){
  if (mx >= global_scalei){
    bool first = (global_scalei==0);
    global_scalei = mx+1024;
    if (first) {
      global_scalep = R_Calloc(global_scalei, double);
    } else {
      global_scalep = R_Realloc(global_scalep, global_scalei, double);
    }
  }
  return global_scalep;
}


int *global_BadDosep;
unsigned int global_BadDosei = 0;
int *global_BadDose(unsigned int mx){
  if (mx >= global_BadDosei){
    bool first = (global_BadDosei==0);
    global_BadDosei = mx+1024;
    if (first) {
      global_BadDosep = R_Calloc(global_BadDosei, int);
    } else {
      global_BadDosep = R_Realloc(global_BadDosep, global_BadDosei, int);
    }
  }
  return global_BadDosep;
}

extern "C" void rxOptionsIni() {
  rx_solve *rx=(&rx_global);

  rx->op = &op_global;
  rx->subjects = inds_global;
}

extern "C" void rxOptionsFree(){
  freeLsodaCtxPool();
  freeRworkPool();
  rxEtaPreFree();

  if (global_InfusionRatei != 0) R_Free(global_InfusionRatep);
  global_InfusionRatei = 0;

  if (global_BadDosei != 0) R_Free(global_BadDosep);
  global_BadDosei = 0;

  if (global_scalei !=  0) R_Free(global_scalep);
  global_scalei = 0;
}

extern "C" void freeExtraDosingC();
extern "C" void rxFreeLast(){
  freeExtraDosingC();
  R_Free(inds_global);
  R_Free(inds_thread);
  inds_thread = NULL;
  inds_global=NULL;
}

extern "C" void ind_lsoda0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq, double *rwork, int lrw, int *iwork, int liw, int jt,
                           t_dydt_lsoda_dum dydt_lsoda,
                           t_update_inis u_inis,
                           t_jdum_lsoda jdum){
  clock_t t0 = clock();
  rx_solving_options_ind *ind;
  double *yp;
  void *ctx = NULL;


  int istate = 1, i = 0;
  // Thread-local copies of solver flags (replaces shared globals gitol/gitask/giopt/gliw/glrw)
  int itol = 1, itask = 1, iopt = 1;

  std::fill(rwork, rwork + lrw + 1, 0.0); // Works because it is a double
  std::fill(iwork, iwork + liw + 1, 0); // Works because it is a integer

  neq[1] = solveid;

  ind = &(rx->subjects[neq[1]]);
  // Per-individual effective neq honors ind->neqOverride; allocations are
  // sized for op->neq, only stepping uses the smaller stride.
  int eff = rxEffNeq(ind, op);

  rwork[4] = op->H0; // H0
  rwork[5] = ind->HMAX; // Hmax
  rwork[6] = op->HMIN; // Hmin

  iwork[4] = 0; // ixpr
  iwork[5] = op->mxstep; // mxstep
  iwork[6] = op->mxhnil; // MXHNIL
  iwork[7] = op->MXORDN; // MXORDN
  iwork[8] = op->MXORDS;  // MXORDS

  double xp = getAllTimes(ind, 0);
  double xout;
  int localBadSolve = 0;

  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) return;
  ind->solvedIdx = 0;
  for(i=0; i < ind->n_all_times; i++) {
    ind->idx=i;
    ind->linSS=0;
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
            _rtime[_raw] = getAllTimes(ind, _raw);  // absolute time stored by updateDur/updateRate
          }
        }
      }
      reSortMainTimeline(ind, i);
      ind->mainSorted = 1;
    }
    _growSolveIfNeeded(ind, op, i, 1);
    yp   = getSolve(i);
    xout = ind->timeThread[ind->ix[i]];
    if (getEvid(ind, ind->ix[i]) != 3 && !isSameTime(xout, xp)) {
      if (ind->err){
        ind->rc[0] = -1000;
        // Bad Solve => NA
        badSolveExit(i);
        localBadSolve = 1;
      } else {
        if (handleExtraDose(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                            xp, ind->id, &i, ind->n_all_times, &istate, op, ind, u_inis, ctx)) {
          if (!localBadSolve && !isSameTime(ind->extraDoseNewXout, xp)) {
            preSolve(op, ind, xp, ind->extraDoseNewXout, yp);
            neq[0] = eff - op->numLin - op->numLinSens;
            F77_CALL(dlsoda)(dydt_lsoda, neq, yp, &xp, &ind->extraDoseNewXout, &itol, &(op->RTOL), &(op->ATOL), &itask,
                             &istate, &iopt, rwork, &lrw, iwork, &liw, jdum, &jt);
            neq[0] = eff;
            copyLinCmt(neq, ind, op, yp);
            postSolve(neq, &istate, ind->rc, &i, yp, err_msg_ls, 7, true, ind, op, rx);
            if (*(ind->rc) < 0) localBadSolve = 1;
          }
          if (!localBadSolve) {
            int idx = ind->idx;
            int ixds = ind->ixds;
            int trueIdx = ind->extraDoseTimeIdx[ind->idxExtra];
            ind->idx = -1-trueIdx;
            handle_evid(ind->extraDoseEvid[trueIdx], neq[0],
                        ind->BadDose, ind->InfusionRate, ind->dose, yp, xout, neq[1], ind);
            istate = 1;
            ind->ixds = ixds; // This is a fake dose, real dose stays in place
            ind->idx = idx;
            ind->idxExtra++;
            if (!isSameTime(xout, ind->extraDoseNewXout)) {
              preSolve(op, ind, ind->extraDoseNewXout, xout, yp);
              neq[0] = eff - op->numLin - op->numLinSens;
              F77_CALL(dlsoda)(dydt_lsoda, neq, yp, &ind->extraDoseNewXout, &xout, &itol, &(op->RTOL), &(op->ATOL), &itask,
                               &istate, &iopt, rwork, &lrw, iwork, &liw, jdum, &jt);
              neq[0] = eff;
              copyLinCmt(neq, ind, op, yp);
              postSolve(neq, &istate, ind->rc, &i, yp, err_msg_ls, 7, true, ind, op, rx);
              if (*(ind->rc) < 0) localBadSolve = 1;
            }
            xp =  ind->extraDoseNewXout;
          }
        }
        if (!localBadSolve && !isSameTime(xout, xp)) {
          preSolve(op, ind, xp, xout, yp);
          neq[0] = eff - op->numLin - op->numLinSens;
          F77_CALL(dlsoda)(dydt_lsoda, neq, yp,
                           &xp, &xout, &itol,
                           &(op->RTOL),
                           &(op->ATOL),
                           &itask,
                           &istate, &iopt, rwork, &lrw, iwork, &liw, jdum, &jt);
          neq[0] = eff;
          copyLinCmt(neq, ind, op, yp);
          postSolve(neq, &istate, ind->rc, &i, yp, err_msg_ls, 7, true, ind, op, rx);
          if (*(ind->rc) < 0) localBadSolve = 1;
        }
        xp = xout;
        //dadt_counter = 0;
      }
    }
    ind->_newind = 2;
    if (!localBadSolve){
      ind->idx = i;
      if (getEvid(ind, ind->ix[i]) == 3) {
        handleEvid3(ind, op, rx, neq, &xp, &xout,  yp, &(istate), u_inis);
      } else if (handleEvid1(&i, rx, neq, yp, &xout)){
        handleSS(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                 xp, ind->id, &i, ind->n_all_times, &istate, op, ind, u_inis, ctx);
        if (ind->wh0 == EVID0_OFF){
          yp[ind->cmt] = op->inits[ind->cmt];
        }
        if (rx->istateReset) istate = 1;
        xp = xout;
      }
      int _mtime_requeued = 0;
      if (rx->nMtime > 0) {
        if (recomputeMtimeIfNeeded(rx, ind, yp, i, xout)) {
          ind->mainSorted = 0;
          _mtime_requeued = 1;
        }
      }
      if (rx->needSort & needSortAlag) {
        if (refreshLagTimesIfNeeded(rx, ind, yp, i + 1, xout)) {
          ind->mainSorted = 0;
        }
      }
      // Copy to next solve so when assigned to
      // yp=ind->solve[neq[0]*i]; it will be the prior values
      updateSolve(ind, op, neq, xout, i, ind->n_all_times);
      if (_mtime_requeued) i--;
    }
    ind->solvedIdx = i;
  }
  ind->solveTime += ((double)(clock() - t0))/CLOCKS_PER_SEC;
}

extern "C" void ind_lsoda(rx_solve *rx, int solveid,
                          t_dydt_lsoda_dum dydt_ls, t_update_inis u_inis, t_jdum_lsoda jdum,
                          int cjt) {
  rx_solving_options *op = rx->op;
  int neq[2];
  neq[0] = op->neq;
  neq[1] = 0;

  int lrw=22+neq[0]*max(16, neq[0]+9), liw=20+neq[0];
  if (global_debug)
    RSprintf("JT: %d\n",cjt);
  double *rwork = __rworkPool[0].rworkp;
  int    *iwork = __rworkPool[0].iworkp;
  ind_lsoda0(rx, op, solveid, neq, rwork, lrw, iwork, liw, cjt,
             dydt_ls, u_inis, jdum);
}

extern "C" void par_lsoda(rx_solve *rx) {
  uint32_t nsub = rx->nsub, nsim = rx->nsim;
  int nsolve = (int)(nsim*nsub); // safe: overflow guard ensures nsim*nsub <= INT_MAX
  rx_solving_options *op = rx->op;
  int displayProgress = (op->nDisplayProgress <= nsolve);
  clock_t t0 = clock();

  int baseNeq = op->neq;
  int lrw = 22 + baseNeq * max(16, baseNeq + 9), liw = 20 + baseNeq;
  int jt = global_jt;
  if (global_debug)
    RSprintf("JT: %d\n", jt);

  // Fortran dlsoda uses non-reentrant COMMON blocks -- must remain single-threaded.
  // Pool slot 0 is always pre-allocated by ensureRworkPool before solving begins.
  double *rwork = __rworkPool[0].rworkp;
  int    *iwork = __rworkPool[0].iworkp;

  int curTick = 0;
  int abort = 0;
  uint32_t seed0 = getRxSeed1(1);
  for (int solveid = 0; solveid < nsolve; solveid++){
    if (abort == 0){
      setSeedEng1(seed0 + solveid - 1);
      int neq[2];
      neq[0] = baseNeq;
      neq[1] = 0;
      ind_lsoda0(rx, op, solveid, neq, rwork, lrw, iwork, liw, jt,
                 dydt_lsoda_dum, update_inis, jdum_lsoda);
      if (displayProgress){
        curTick = par_progress(solveid, nsolve, curTick, 1, t0, 0);
#ifdef _OPENMP
        if (omp_get_thread_num() == 0) {
#endif
        if (checkInterrupt()){
          abort = 1;
          break;
        }
#ifdef _OPENMP
        }
#endif
      }
    }
  }
  setRxSeedFinal(seed0 + (uint32_t)nsolve);
  if (abort == 1){
    op->abort = 1;
  } else {
    if (displayProgress && curTick < 50) par_progress(nsolve, nsolve, curTick, 1, t0, 0);
  }
}


/* -----------------------------------------------------------------------
 * ind_lsode0: per-subject DLSODE solver (MF=10 Adams or MF=22 BDF).
 * Same structure as ind_lsoda0 but calls F77_CALL(dlsode) with MF and
 * extra RPAR/IPAR args via rxode2_dlsode_F bridge.
 * ----------------------------------------------------------------------- */
static void ind_lsode0(rx_solve *rx, rx_solving_options *op, int solveid,
                       int *neq, double *rwork, int lrw, int *iwork, int liw,
                       int mf) {
  clock_t t0 = clock();
  rx_solving_options_ind *ind;
  double *yp;
  void *ctx = NULL;

  int istate = 1;
  int itol = 1, itask = 1, iopt = 1;
  double rpar = 0.0; int ipar = 0;

  std::fill(rwork, rwork + lrw + 1, 0.0);
  std::fill(iwork, iwork + liw + 1, 0);

  neq[1] = solveid;
  ind = &(rx->subjects[neq[1]]);
  int eff = rxEffNeq(ind, op);

  rwork[4] = op->H0;
  rwork[5] = (ind->HMAX > 0.0 && std::isfinite(ind->HMAX)) ? ind->HMAX : 0.0;
  rwork[6] = op->HMIN;
  iwork[5] = op->mxstep;
  iwork[6] = op->mxhnil;

  double xp = getAllTimes(ind, 0);
  double xout;
  int localBadSolve = 0;

  if (!iniSubject(neq[1], 0, ind, op, rx, update_inis)) return;
  ind->solvedIdx = 0;
  for (int i = 0; i < ind->n_all_times; i++) {
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
          if (_whI == EVIDF_MODEL_RATE_OFF || _whI == EVIDF_MODEL_DUR_OFF)
            _rtime[_raw] = getAllTimes(ind, _raw);
        }
      }
      reSortMainTimeline(ind, i);
      ind->mainSorted = 1;
    }
    _growSolveIfNeeded(ind, op, i, 1);
    yp   = getSolve(i);
    xout = ind->timeThread[ind->ix[i]];
    if (getEvid(ind, ind->ix[i]) != 3 && !isSameTime(xout, xp)) {
      if (ind->err) {
        ind->rc[0] = -1000;
        badSolveExit(i);
        localBadSolve = 1;
      } else {
        if (handleExtraDose(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                            xp, ind->id, &i, ind->n_all_times, &istate, op, ind, update_inis, ctx)) {
          if (!localBadSolve && !isSameTime(ind->extraDoseNewXout, xp)) {
            preSolve(op, ind, xp, ind->extraDoseNewXout, yp);
            neq[0] = eff - op->numLin - op->numLinSens;
            if (op->indOwnAlloc && ind->_atEventTime) {
              // Pre-evaluate dydt at xp so in-model evid_() pushes at the correct
              // time before lsode's Adams predictor evaluates at xp + H_prev.
              { std::vector<double> _tmp_f_ls((size_t)neq[0]); dydt(neq, xp, yp, _tmp_f_ls.data()); }
            }
            F77_CALL(dlsode)(rxode2_dlsode_F, neq, yp, &xp, &ind->extraDoseNewXout,
                             &itol, &(op->RTOL), &(op->ATOL), &itask, &istate, &iopt,
                             rwork, &lrw, iwork, &liw, rxode2_dlsode_JAC, &mf, &rpar, &ipar);
            neq[0] = eff;
            copyLinCmt(neq, ind, op, yp);
            postSolve(neq, &istate, ind->rc, &i, yp, err_msg_ls, 7, true, ind, op, rx);
            if (*(ind->rc) < 0) localBadSolve = 1;
          }
          if (!localBadSolve) {
            int idx = ind->idx, ixds = ind->ixds;
            int trueIdx = ind->extraDoseTimeIdx[ind->idxExtra];
            ind->idx = -1 - trueIdx;
            handle_evid(ind->extraDoseEvid[trueIdx], neq[0],
                        ind->BadDose, ind->InfusionRate, ind->dose, yp, xout, neq[1], ind);
            istate = 1;
            ind->ixds = ixds;
            ind->idx  = idx;
            ind->idxExtra++;
            if (!isSameTime(xout, ind->extraDoseNewXout)) {
              double _xp_ls = ind->extraDoseNewXout;
              preSolve(op, ind, _xp_ls, xout, yp);
              neq[0] = eff - op->numLin - op->numLinSens;
              // istate is already 1 from the dose handler above (needed for restart
              // after a dose event); also pre-evaluate to push any in-model doses
              // at the canonical sub-interval start time.
              if (op->indOwnAlloc && ind->_atEventTime) {
                { std::vector<double> _tmp_f_ls((size_t)neq[0]); dydt(neq, _xp_ls, yp, _tmp_f_ls.data()); }
              }
              F77_CALL(dlsode)(rxode2_dlsode_F, neq, yp, &ind->extraDoseNewXout, &xout,
                               &itol, &(op->RTOL), &(op->ATOL), &itask, &istate, &iopt,
                               rwork, &lrw, iwork, &liw, rxode2_dlsode_JAC, &mf, &rpar, &ipar);
              neq[0] = eff;
              copyLinCmt(neq, ind, op, yp);
              postSolve(neq, &istate, ind->rc, &i, yp, err_msg_ls, 7, true, ind, op, rx);
              if (*(ind->rc) < 0) localBadSolve = 1;
            }
            xp = ind->extraDoseNewXout;
          }
        }
        if (!localBadSolve && !isSameTime(xout, xp)) {
          preSolve(op, ind, xp, xout, yp);
          neq[0] = eff - op->numLin - op->numLinSens;
          if (op->indOwnAlloc && ind->_atEventTime) {
            // Pre-evaluate dydt at xp so in-model evid_() pushes doses at the
            // correct canonical start time before lsode's Adams predictor can
            // evaluate at xp + H_prev.
            { std::vector<double> _tmp_f_ls((size_t)neq[0]); dydt(neq, xp, yp, _tmp_f_ls.data()); }
          }
          F77_CALL(dlsode)(rxode2_dlsode_F, neq, yp, &xp, &xout,
                           &itol, &(op->RTOL), &(op->ATOL), &itask, &istate, &iopt,
                           rwork, &lrw, iwork, &liw, rxode2_dlsode_JAC, &mf, &rpar, &ipar);
          neq[0] = eff;
          copyLinCmt(neq, ind, op, yp);
          postSolve(neq, &istate, ind->rc, &i, yp, err_msg_ls, 7, true, ind, op, rx);
          if (*(ind->rc) < 0) localBadSolve = 1;
        }
        xp = xout;
      }
    }
    ind->_newind = 2;
    if (!localBadSolve) {
      ind->idx = i;
      if (getEvid(ind, ind->ix[i]) == 3) {
        handleEvid3(ind, op, rx, neq, &xp, &xout, yp, &istate, update_inis);
      } else if (handleEvid1(&i, rx, neq, yp, &xout)) {
        handleSS(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                 xp, ind->id, &i, ind->n_all_times, &istate, op, ind, update_inis, ctx);
        if (ind->wh0 == EVID0_OFF) ind->solve[ind->cmt] = op->inits[ind->cmt];
        if (rx->istateReset) {
          istate = 1;
        }
        xp = xout;
      }
      int _mtime_requeued = 0;
      if (rx->nMtime > 0) {
        if (recomputeMtimeIfNeeded(rx, ind, yp, i, xout)) {
          ind->mainSorted = 0;
          _mtime_requeued = 1;
        }
      }
      if (rx->needSort & needSortAlag) {
        if (refreshLagTimesIfNeeded(rx, ind, yp, i + 1, xout))
          ind->mainSorted = 0;
      }
      updateSolve(ind, op, neq, xout, i, ind->n_all_times);
      if (_mtime_requeued) i--;
    }
    ind->solvedIdx = i;
  }
  ind->solveTime += ((double)(clock() - t0)) / CLOCKS_PER_SEC;
}

/* par_lsode_bdf: serial loop over subjects using DLSODE (mf=10 or 22). */
static void par_lsode_bdf(rx_solve *rx, int mf) {
  uint32_t nsub = rx->nsub, nsim = rx->nsim;
  int nsolve = (int)(nsim * nsub);
  rx_solving_options *op = rx->op;
  int displayProgress = (op->nDisplayProgress <= nsolve);
  clock_t t0 = clock();

  int baseNeq = op->neq;
  int lrw = (mf == 22) ? (22 + 9 * baseNeq + 2 * baseNeq * baseNeq)
                        : (22 + baseNeq * max(16, baseNeq + 9));
  int liw = 20 + baseNeq;

  double *rwork = __rworkPool[0].rworkp;
  int    *iwork = __rworkPool[0].iworkp;

  int curTick = 0, abort = 0;
  uint32_t seed0 = getRxSeed1(1);
  for (int solveid = 0; solveid < nsolve; solveid++) {
    if (abort == 0) {
      setSeedEng1(seed0 + solveid - 1);
      int neq[2]; neq[0] = baseNeq; neq[1] = 0;
      ind_lsode0(rx, op, solveid, neq, rwork, lrw, iwork, liw, mf);
      if (displayProgress) {
        curTick = par_progress(solveid + 1, nsolve, curTick, 1, t0, 0);
      }
      if (op->abort) abort = 1;
    }
  }
  if (abort == 1) op->abort = 1;
  else if (displayProgress && curTick < 50) par_progress(nsolve, nsolve, curTick, 1, t0, 0);
}

extern "C" void ind_lsode(rx_solve *rx, int solveid) {
  /* lsode: DLSODE Adams MF=10 (non-stiff, variable order 1-12).
   * Non-reentrant COMMON blocks -- always single-threaded. */
  rx_solving_options *op = rx->op;
  int neq[2]; neq[0] = op->neq; neq[1] = 0;
  int lrw = 22 + neq[0] * max(16, neq[0] + 9), liw = 20 + neq[0];
  ind_lsode0(rx, op, solveid, neq, __rworkPool[0].rworkp, lrw,
             __rworkPool[0].iworkp, liw, 10);
}

extern "C" void ind_bdf(rx_solve *rx, int solveid) {
  /* bdf: DLSODE BDF MF=22 (stiff, internally generated dense Jacobian).
   * Non-reentrant COMMON blocks -- always single-threaded. */
  rx_solving_options *op = rx->op;
  int neq[2]; neq[0] = op->neq; neq[1] = 0;
  int lrw = 22 + 9 * neq[0] + 2 * neq[0] * neq[0], liw = 20 + neq[0];
  ind_lsode0(rx, op, solveid, neq, __rworkPool[0].rworkp, lrw,
             __rworkPool[0].iworkp, liw, 22);
}

extern "C" void par_lsode(rx_solve *rx) {
  /* lsode: DLSODE Adams MF=10 -- single-threaded (non-reentrant COMMON blocks). */
  par_lsode_bdf(rx, 10);
}

extern "C" void par_bdf(rx_solve *rx) {
  /* bdf: DLSODE BDF MF=22 -- single-threaded (non-reentrant COMMON blocks). */
  par_lsode_bdf(rx, 22);
}

extern "C" double ind_linCmt0H(rx_solve *rx, rx_solving_options *op, int solveid, int *_neq,
                               t_dydt c_dydt, t_update_inis u_inis) {
  int i;
  double xout;
  double *yp;
  int istate = 0;
  rx_solving_options_ind *ind;
  double *x;
  int *BadDose;
  double *InfusionRate;
  double *inits;
  int *rc;
  void *ctx = NULL;
  int idid=1;
  const char **err_msg = NULL;
  int nx;
  int neq[2];
  neq[1] = rx->ordId[solveid]-1;
  ind = &(rx->subjects[neq[1]]);
  // Per-individual effective neq (rxEffNeq) -- see ind_liblsoda0 for context.
  neq[0] = rxEffNeq(ind, op);

  double ret = 0.0;
  double cur = 0.0;
  double delta = 0.0;
  int n =0, nzero = 0;

  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) return NA_REAL;

  nx = ind->n_all_times;
  inits = op->inits;
  BadDose = ind->BadDose;
  InfusionRate = ind->InfusionRate;
  x = ind->all_times;
  rc= ind->rc;
  double xp = x[0];
  ind->solvedIdx = 0;
  for(i=0; i<nx; i++) {
    ind->idx=i;
    ind->linSS=0;
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
            _rtime[_raw] = getAllTimes(ind, _raw);  // absolute time stored by updateDur/updateRate
          }
        }
      }
      reSortMainTimeline(ind, i);
      ind->mainSorted = 1;
    }
    _growSolveIfNeeded(ind, op, i, 1);
    yp = getSolve(i);
    xout = ind->timeThread[ind->ix[i]];
    if (global_debug) {
      RSprintf("i=%d xp=%f xout=%f\n", i, xp, xout);
    }
    if (getEvid(ind, ind->ix[i]) != 3) {
      if (ind->err) {
        printErr(ind->err, ind->id);
        *rc = idid;
        // Bad Solve => NA
        badSolveExit(i);
      } else {
        if (handleExtraDose(neq, BadDose, InfusionRate, ind->dose, yp, xout,
                            xp, ind->id, &i, nx, &istate, op, ind, u_inis, ctx)) {
          if (!isSameTime(ind->extraDoseNewXout, xp)) {
            preSolve(op, ind, xp, ind->extraDoseNewXout, yp);
            linSolve(neq, ind, yp, &xp, ind->extraDoseNewXout);
            postSolve(neq, &idid, rc, &i, yp, err_msg, 4, true, ind, op, rx);
            xp = ind->extraDoseNewXout;
          }
          int idx = ind->idx;
          int ixds = ind->ixds;
          int trueIdx = ind->extraDoseTimeIdx[ind->idxExtra];
          ind->idx = -1-trueIdx;
          handle_evid(ind->extraDoseEvid[trueIdx], neq[0],
                      BadDose, InfusionRate, ind->dose, yp, xout, neq[1], ind);
          ind->idx = idx;
          ind->ixds = ixds;
          ind->idxExtra++;
          if (!isSameTime(xout, ind->extraDoseNewXout)) {
            preSolve(op, ind, ind->extraDoseNewXout, xout, yp);
            linSolve(neq, ind, yp, &(ind->extraDoseNewXout), xout);
            postSolve(neq, &idid, rc, &i, yp, err_msg, 4, true, ind, op, rx);
            xp = ind->extraDoseNewXout;
          }
        }
        if (!isSameTime(xout, xp)) {
          preSolve(op, ind, xp, xout, yp);
          linSolve(neq, ind, yp, &xp, xout);
          postSolve(neq, &idid, rc, &i, yp, err_msg, 4, true, ind, op, rx);
          xp = xout;
        }
      }
    }
    ind->_newind = 2;
    if (!op->badSolve){
      ind->idx = i;
      if (getEvid(ind, ind->ix[i]) == 3) {
        handleEvid3(ind, op, rx, neq, &xp, &xout,  yp, &(idid), u_inis);
      } else if (handleEvid1(&i, rx, neq, yp, &xout)) {
        handleSS(neq, BadDose, InfusionRate, ind->dose, yp, xout,
                 xp, ind->id, &i, nx, &istate, op, ind, u_inis, ctx);
        if (ind->wh0 == EVID0_OFF){
          yp[ind->cmt] = inits[ind->cmt];
        }
        xp = xout;
      }
      int _mtime_requeued = 0;
      if (rx->nMtime > 0) {
        if (recomputeMtimeIfNeeded(rx, ind, yp, i, xout)) {
          ind->mainSorted = 0;
          _mtime_requeued = 1;
        }
      }
      if (rx->needSort & needSortAlag) {
        if (refreshLagTimesIfNeeded(rx, ind, yp, i + 1, xout)) {
          ind->mainSorted = 0;
        }
      }
      // Geometric mean of all compartments
      double cur0 = 0.0;
      int n0=0;
      int nzero0 = 0;
      int cmtId = 1;
      for (int i = 0; i < rx->linCmtNcmt; ++i) {
        if ((rx->linCmtHcmt & cmtId) != 0) {
          cur = yp[op->numLin + rx->linCmtOral0 + i];
          if (cur == 0) {
            nzero0++;
          } else {
            n0++;
            switch (rx->linCmtHmeanI) {
            case 1:
              delta = cur - cur0; // arithmetic mean
              break;
            case 2:
              delta = log(cur) - cur0; // geometric mean
              break;
            case 3:
              delta = 1.0/cur - cur0; // harmonic mean
              break;
            }
            cur0 += delta/n0;
          }
        }
        cmtId <<= 1;
      }
      if (rx->linCmtOral0 == 1) {
        if ((rx->linCmtHcmt & linCmtDepot) != 0) {
          cur = yp[op->numLin + rx->linCmtOral0 + i];
          if (cur == 0) {
            nzero0++;
          } else {
            n0++;
            switch (rx->linCmtHmeanI) {
            case 1:
              delta = cur - cur0; // arithmetic mean
              break;
            case 2:
              delta = log(cur) - cur0; // geometric mean
              break;
            case 3:
              delta = 1.0/cur - cur0; // harmonic mean
              break;
            }
            cur0 += delta/n0;
          }
        }
      }
      if ((rx->linCmtHcmt & linCmtConc) != 0) {
        cur = yp[op->numLin+rx->linCmtOral0]/ind->linCmtHV;
        if (cur == 0) {
          nzero0++;
        } else {
          n0++;
          switch (rx->linCmtHmeanI) {
          case 1:
            delta = cur - cur0; // arithmetic mean
            break;
          case 2:
            delta = log(cur) - cur0; // geometric mean
            break;
          case 3:
            delta = 1.0/cur - cur0; // harmonic mean
            break;
          }
          cur0 += delta/n0;
        }
      }
      // Finalize the current mean
      double correction0 = 1.0;
      switch (rx->linCmtHmeanI) {
      case 1:
        // arithmetic mean, already finalized
        cur = cur0;
        break;
      case 2:
        // geometric mean
        cur = exp(cur0);
        break;
      case 3:
        // harmonic mean
        correction0 = (double)(n0-nzero0)/((double)n);
        if (correction0 <= 0) correction0=1;
        cur =  (double)(n0)/cur0 * correction0;
        break;
      }
      if (cur == 0) {
        nzero++;
      } else {
        n++;
        switch (rx->linCmtHmeanO) {
        case 1: // arithmetic mean
          delta = cur - ret;
          break;
        case 2: // geometric mean
          delta = log(cur) - ret;
          break;
        case 3: // harmonic mean
          delta = 1.0/cur - ret;
          break;
        }
        // delta = log(cur) - ret; // geometric mean
        ret += delta/n0;
      }

      updateSolve(ind, op, neq, xout, i, nx);

      ind->slvr_counter[0]++; // doesn't need do be critical; one subject at a time.
      if (_mtime_requeued) i--;
      /* for(j=0; j<neq[0]; j++) ret[neq[0]*i+j] = yp[j]; */
    }
    ind->solvedIdx = i;
  }
  double correction;
  switch (rx->linCmtHmeanO) {
  case 1: // arithmetic mean
    return ret;
    break;
  case 2: // geometric mean
    return exp(ret);
    break;
  case 3: // harmonic mean
    correction =  (double)(n-nzero)/((double)n);
    if (correction <= 0) correction=1;
    return (double)(n)/ret * correction;
  }
  return ret;
}

extern "C" void ind_linCmt0(rx_solve *rx, rx_solving_options *op, int solveid, int *_neq,
                            t_dydt c_dydt, t_update_inis u_inis) {
  clock_t t0 = clock();
  int i;
  double xout;
  double *yp;
  int istate = 0;
  rx_solving_options_ind *ind;
  int *rc;
  void *ctx = NULL;
  int idid=1;
  const char **err_msg = NULL;
  int nx;
  int neq[2];
  neq[0] = op->neq;
  neq[1] = rx->ordId[solveid]-1;
  ind = &(rx->subjects[neq[1]]);

  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) return;

  nx = ind->n_all_times;
  rc= ind->rc;
  double xp = ind->all_times[0];
  ind->solvedIdx = 0;
  bool _skipEvid = false;
  for(i=0; i<nx; i++) {
    ind->idx=i;
    ind->linSS=0;
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
            _rtime[_raw] = getAllTimes(ind, _raw);  // absolute time stored by updateDur/updateRate
          }
        }
      }
      reSortMainTimeline(ind, i);
      ind->mainSorted = 1;
    }
    _growSolveIfNeeded(ind, op, i, 1);
    yp = getSolve(i);
    xout = ind->timeThread[ind->ix[i]];
    if (global_debug) {
      RSprintf("i=%d xp=%f xout=%f\n", i, xp, xout);
    }
    bool _linSolveCalled = false;
    if (getEvid(ind, ind->ix[i]) != 3) {
      if (ind->err) {
        printErr(ind->err, ind->id);
        *rc = idid;
        // Bad Solve => NA
        badSolveExit(i);
      } else {
        if (handleExtraDose(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                            xp, ind->id, &i, nx, &istate, op, ind, u_inis, ctx)) {
          if (!isSameTime(ind->extraDoseNewXout, xp)) {
            preSolve(op, ind, xp, ind->extraDoseNewXout, yp);
            linSolve(neq, ind, yp, &xp, ind->extraDoseNewXout);
            postSolve(neq, &idid, rc, &i, yp, err_msg, 4, true, ind, op, rx);
            xp = ind->extraDoseNewXout;
          }
          int idx = ind->idx;
          int ixds = ind->ixds;
          int trueIdx = ind->extraDoseTimeIdx[ind->idxExtra];
          ind->idx = -1-trueIdx;
          handle_evid(ind->extraDoseEvid[trueIdx], neq[0],
                      ind->BadDose, ind->InfusionRate, ind->dose, yp, xout, neq[1], ind);
          ind->idx = idx;
          ind->ixds = ixds;
          ind->idxExtra++;
          if (!isSameTime(xout, ind->extraDoseNewXout)) {
            preSolve(op, ind, ind->extraDoseNewXout, xout, yp);
            linSolve(neq, ind, yp, &(ind->extraDoseNewXout), xout);
            postSolve(neq, &idid, rc, &i, yp, err_msg, 4, true, ind, op, rx);
            xp = ind->extraDoseNewXout;
          }
        }
        if (!isSameTime(xout, xp)) {
          // Keep ind->idx=i so _rxPushDose uses sortStart=i: pushed events at
          // the same time as the current obs sort before it (doses before obs),
          // which preserves the correct reset-before-dose-before-obs ordering.
          // If a push displaced the current obs to ix[i+1], _skipEvid prevents
          // evid_() from double-firing when the displaced obs is re-processed.
          int _nBeforePush = ind->n_all_times;
          preSolve(op, ind, xp, xout, yp);
          linSolve(neq, ind, yp, &xp, xout);
          postSolve(neq, &idid, rc, &i, yp, err_msg, 4, true, ind, op, rx);
          xp = xout;
          _linSolveCalled = true;
          if (ind->n_all_times > _nBeforePush) {
            _skipEvid = true;
            nx = ind->n_all_times;
          }
        }
      }
    }
    ind->_newind = 2;
    if (!op->badSolve){
      ind->idx = i;
      if (getEvid(ind, ind->ix[i]) == 3) {
        handleEvid3(ind, op, rx, neq, &xp, &xout,  yp, &(idid), u_inis);
      } else if (handleEvid1(&i, rx, neq, yp, &xout)) {
        handleSS(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                 xp, ind->id, &i, nx, &istate, op, ind, u_inis, ctx);
        if (ind->wh0 == EVID0_OFF){
          yp[ind->cmt] = op->inits[ind->cmt];
        }
        xp = xout;
      }
      int _mtime_requeued = 0;
      if (rx->nMtime > 0) {
        if (recomputeMtimeIfNeeded(rx, ind, yp, i, xout)) {
          ind->mainSorted = 0;
          _mtime_requeued = 1;
        }
      }
      if (rx->needSort & needSortAlag) {
        if (refreshLagTimesIfNeeded(rx, ind, yp, i + 1, xout)) {
          ind->mainSorted = 0;
        }
      }
      ind->idx = i + 1;
      // Fire evid_() via calc_lhs only for same-time obs (when linSolve was
      // not called).  For non-same-time obs, preSolve already set
      // _atEventTime=1 and linSolve's internal dydt(xout) captured and
      // cleared it, firing evid_() once at the observation time.  Setting
      // _atEventTime=1 again here would cause calc_lhs to fire a second time,
      // double-counting pushed doses.
      // _skipEvid: set when a push displaced the current obs to ix[i+1]; consume
      // the flag here to prevent the displaced obs from re-firing evid_().
      if (op->indOwnAlloc && isObs(getEvid(ind, ind->ix[i])) && !_linSolveCalled) {
        if (_skipEvid) {
          _skipEvid = false;
        } else {
          ind->_atEventTime = 1;
        }
      }
      updateSolve(ind, op, neq, xout, i, nx);
      // Refresh nx after updateSolve: evid_() inside calc_lhs may have pushed
      // new events into the timeline, growing ind->n_all_times.
      nx = ind->n_all_times;
      ind->slvr_counter[0]++; // doesn't need do be critical; one subject at a time.
      if (_mtime_requeued) i--;
      /* for(j=0; j<neq[0]; j++) ret[neq[0]*i+j] = yp[j]; */
    }
    ind->solvedIdx = i;
  }
  ind->solveTime += ((double)(clock() - t0))/CLOCKS_PER_SEC;
}

extern "C" void ind_linCmt(rx_solve *rx, int solveid,
                           t_dydt dydt, t_update_inis u_inis){
  rx_solving_options *op = &op_global;
  int neq[2];
  neq[0] = op->neq;
  neq[1] = solveid;
  ind_linCmt0(rx, op, solveid, neq, dydt, u_inis);
}

// --- AutoSwitch helpers -------------------------------------------------------
// AutoSwitch is reactive: each interval the non-stiff primary (dop853) is tried
// first with its built-in stiffness estimator enabled (nstiff=50 -- the Hairer
// II dominant-eigenvalue estimate hlamb = h*||k4-k3||/||k5-yhat|| using the
// ACTUAL step h, the same quantity Julia's AutoSwitch calls eigen_est).  If
// dop853 reports/encounters stiffness (idid<=0) the interval is re-solved with
// the stiff secondary.  A Gershgorin pre-check on the full interval length is
// deliberately NOT used: it overestimates stiffness (rho*interval vs rho*step)
// and false-triggers switches on non-stiff problems whose Jacobian norm is
// large but whose accuracy-limited step is small (e.g. Lorenz-type systems).
//
// rxAutoSwitchCount keeps a little hysteresis so a persistently stiff problem
// stops paying the wasted dop853 probe every interval:
//   dop853Tried + failed  -> count stiff intervals; after maxStiff, stick on
//                            the stiff secondary (autoMethod=1)
//   dop853Tried + ok      -> reset the stiff counter
//   stiff secondary used  -> after maxNonstiff intervals, optimistically try
//                            the primary again (autoMethod=0)
static void rxAutoSwitchCount(rx_solving_options *op, rx_solving_options_ind *ind,
                              bool dop853Tried, bool dop853Failed) {
  if (dop853Tried) {
    if (dop853Failed) {
      ind->autoCount = (ind->autoCount > 0) ? ind->autoCount + 1 : 1;
      if (ind->autoCount >= op->autoSwitchMaxStiff) {
        ind->autoMethod = 1;
        ind->autoCount = 0;
        ind->autoLastSwitchIntervals = 0;
      }
    } else {
      if (ind->autoCount > 0) ind->autoCount = 0;
    }
  } else {
    ind->autoLastSwitchIntervals++;
    int back = (op->autoSwitchMaxNonstiff > 0) ? op->autoSwitchMaxNonstiff : 3;
    if (ind->autoLastSwitchIntervals >= back) {
      ind->autoMethod = 0;
      ind->autoLastSwitchIntervals = 0;
    }
  }
}

// Solve one non-dense interval [xp, xout] with the dop853+ros4 AutoSwitch
// composite (point-to-point, no dense output): the analogue of
// denseSegmentSolve for the standard non-dense path.  Reactive switching --
// dop853 (with its stiffness estimator on) is tried first, and the interval is
// re-solved with the requested stiff secondary (op->stiff2) if dop853 reports
// stiffness; once a problem is persistently stiff the probe is skipped
// (autoMethod==1).  The caller has already run preSolve() and set neq[0] to the
// ODE dimension.
static int dopSegmentAutoSwitch(rx_solve *rx, rx_solving_options *op,
                                rx_solving_options_ind *ind, int *neq, t_dydt c_dydt,
                                double xp, double xout, double *yp,
                                int *istate, int itol, int eff) {
  (void) rx;
  // Non-composite: plain dop853 (unchanged behavior).
  if (op->stiff2 <= 0) {
    return dop853(neq, c_dydt, xp, yp, xout, op->rtol2, op->atol2, itol,
                  solout, 0, NULL, DBL_EPSILON, 0, 0, 0, 0,
                  ind->HMAX, op->H0, op->mxstep, 1, -1,
                  0, NULL, 0, NULL, ind->id);
  }
  int idid;
  if (ind->autoMethod == 0) {
    // Probe with dop853; nstiff=50 enables its built-in stiffness estimator.
    double _ypStack[64];
    double *_ypDyn = NULL;
    double *ypSave = (eff <= 64) ? _ypStack
                                 : (_ypDyn = (double*)malloc((size_t)eff * sizeof(double)));
    if (ypSave == NULL) {
      ind->err = 1;
      if (ind->rc[0] == 0) ind->rc[0] = -2019;
      return -4;
    }
    memcpy(ypSave, yp, (size_t)eff * sizeof(double));
    idid = dop853(neq, c_dydt, xp, yp, xout, op->rtol2, op->atol2, itol,
                  solout, 0, NULL, DBL_EPSILON, 0, 0, 0, 0,
                  ind->HMAX, op->H0, op->mxstep, 1, 50,
                  0, NULL, 0, NULL, ind->id);
    bool failed = (idid <= 0 || ind->err);
    if (failed) {
      // Stiffness: restore the interval-start state and re-solve with the
      // requested stiff secondary (op->stiff2) via the shared per-method
      // interval solver (its preSolve() is idempotent).
      ind->rc[0] = 0;
      ind->err = 0;
      *istate = 1;
      memcpy(yp, ypSave, (size_t)eff * sizeof(double));
      double xpLoc = xp;
      int _di = 0, _sd = 1;
      _rxSolveOneInterval(op->stiff2, false, neq, yp, &xpLoc, xout, istate,
                          &_sd, op, ind, &_di, NULL, eff);
      idid = (*istate > 0 && !ind->err) ? 1 : -4;
    }
    if (_ypDyn) free(_ypDyn);
    rxAutoSwitchCount(op, ind, true, failed);
  } else {
    // Persistently stiff: go straight to the stiff secondary.
    double xpLoc = xp;
    int _di = 0, _sd = 1;
    _rxSolveOneInterval(op->stiff2, false, neq, yp, &xpLoc, xout, istate,
                        &_sd, op, ind, &_di, NULL, eff);
    idid = (*istate > 0 && !ind->err) ? 1 : -4;
    rxAutoSwitchCount(op, ind, false, false);
  }
  return idid;
}

extern "C" void ind_dop0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq,
                         t_dydt c_dydt,
                         t_update_inis u_inis) {
  clock_t t0 = clock();
  int itol=1;           //1: rtol/atol are vectors (per-compartment), matching liblsoda
  int idid=0;
  int i;
  double xout;
  double *yp;
  void *ctx = NULL;
  int istate = 0;
  static const char *err_msg[]=
    {
      "input is not consistent",
      "larger nmax is needed",
      "step size becomes too small",
      "problem is probably stiff (interrupted)"
    };
  rx_solving_options_ind *ind;
  double *x;
  double *InfusionRate;
  double *inits;
  int *rc;
  int nx;
  neq[1] = solveid;
  ind = &(rx->subjects[neq[1]]);
  // Per-individual effective neq (rxEffNeq) honors ind->neqOverride; under
  // override the dop853 stepping uses the smaller stride.  Caller (ind_dop /
  // par_dop) initialises neq[0] from op->neq before this function runs;
  // update it now that ind is known so handle_evid / postSolve / etc. see
  // the per-individual effective stride for the current subject.
  int eff = rxEffNeq(ind, op);
  neq[0] = eff;
  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) return;
  nx = ind->n_all_times;
  inits = op->inits;
  InfusionRate = ind->InfusionRate;
  x = ind->all_times;
  rc= ind->rc;
  double xp = x[0];
  ind->solvedIdx = 0;
  for(i=0; i<ind->n_all_times; i++) {
    ind->idx=i;
    ind->linSS=0;
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
            _rtime[_raw] = getAllTimes(ind, _raw);  // absolute time stored by updateDur/updateRate
          }
        }
      }
      reSortMainTimeline(ind, i);
      ind->mainSorted = 1;
    }
    yp = getSolve(i);
    xout = ind->timeThread[ind->ix[i]];
    if (global_debug){
      RSprintf("i=%d xp=%f xout=%f\n", i, xp, xout);
    }
    if (getEvid(ind, ind->ix[i]) != 3) {
      if (ind->err){
        printErr(ind->err, ind->id);
        *rc = idid;
        // Bad Solve => NA
        badSolveExit(i);
      } else {
        if (handleExtraDose(neq, ind->BadDose, InfusionRate, ind->dose, yp, xout,
                            xp, ind->id, &i, nx, &istate, op, ind, u_inis, ctx)) {
          if (!isSameTimeDop(ind->extraDoseNewXout, xp)) {
            preSolve(op, ind, xp, ind->extraDoseNewXout, yp);
            neq[0] = eff - op->numLin - op->numLinSens;
            idid = dopSegmentAutoSwitch(rx, op, ind, neq, c_dydt, xp,
                                        ind->extraDoseNewXout, yp, &istate, itol, eff);
            neq[0] = eff;
            copyLinCmt(neq, ind, op, yp);
            postSolve(neq, &idid, rc, &i, yp, err_msg, 4, true, ind, op, rx);
            xp = ind->extraDoseNewXout;
          }
          int idx = ind->idx;
          int ixds = ind->ixds;
          int trueIdx = ind->extraDoseTimeIdx[ind->idxExtra];
          ind->idx = -1-trueIdx;
          handle_evid(ind->extraDoseEvid[trueIdx], neq[0],
                      ind->BadDose, InfusionRate, ind->dose, yp, xout, neq[1], ind);
          idid = 1;
          ind->idx = idx;
          ind->ixds = ixds;
          ind->idxExtra++;
          if (!isSameTimeDop(xout, ind->extraDoseNewXout)) {
            preSolve(op, ind, ind->extraDoseNewXout, xout, yp);
            neq[0] = eff - op->numLin - op->numLinSens;
            idid = dopSegmentAutoSwitch(rx, op, ind, neq, c_dydt,
                                        ind->extraDoseNewXout, xout, yp, &istate, itol, eff);
            neq[0] = eff;
            copyLinCmt(neq, ind, op, yp);
            postSolve(neq, &idid, rc, &i, yp, err_msg, 4, true, ind, op, rx);
            xp = ind->extraDoseNewXout;
          }
        }
        if (!isSameTimeDop(xout, xp)) {
          preSolve(op, ind, xp, xout, yp);
          neq[0] = eff - op->numLin - op->numLinSens;
          idid = dopSegmentAutoSwitch(rx, op, ind, neq, c_dydt, xp, xout, yp,
                                      &istate, itol, eff);
          neq[0] = eff;
          copyLinCmt(neq, ind, op, yp);
          postSolve(neq, &idid, rc, &i, yp, err_msg, 4, true, ind, op, rx);
          xp = xout;
        }
        //dadt_counter = 0;
      }
    }
    if (!op->badSolve) {
      ind->idx = i;
      if (getEvid(ind, ind->ix[i]) == 3) {
        handleEvid3(ind, op, rx, neq, &xp, &xout,  yp, &(idid), u_inis);
      } else if (handleEvid1(&i, rx, neq, yp, &xout)){
        handleSS(neq, ind->BadDose, InfusionRate, ind->dose, yp, xout,
                 xp, ind->id, &i, ind->n_all_times, &istate, op, ind, u_inis, ctx);
        if (ind->wh0 == EVID0_OFF){
          yp[ind->cmt] = inits[ind->cmt];
        }
        xp = xout;
      }
      int _mtime_requeued = 0;
      if (rx->nMtime > 0) {
        if (recomputeMtimeIfNeeded(rx, ind, yp, i, xout)) {
          ind->mainSorted = 0;
          _mtime_requeued = 1;
        }
      }
      if (rx->needSort & needSortAlag) {
        if (refreshLagTimesIfNeeded(rx, ind, yp, i + 1, xout)) {
          ind->mainSorted = 0;
        }
      }
      /* for(j=0; j<neq[0]; j++) ret[neq[0]*i+j] = yp[j]; */
      updateSolve(ind, op, neq, xout, i, ind->n_all_times);
      if (_mtime_requeued) i--;
    }
    ind->solvedIdx = i;
  }
  ind->solveTime += ((double)(clock() - t0))/CLOCKS_PER_SEC;
}

// --- Delay differential equation (DDE) dense history -------------------------
// Records of accepted dense steps so that delay(state, T) lookups can later
// interpolate any earlier state to the accuracy of the integrator (the
// approach of Hairer's RETARD method and the 'dde' package).  A single subject
// solve may mix dop853 and ros4 steps (the dense AutoSwitch composite), so a
// uniform record layout with a per-record solver tag is used:
//
//   slots [0 .. 8n-1] : dop853 rcont1..8, or ros4 4 cubic samples (first 4n)
//   slot   8n         : t_old
//   slot   8n+1       : h
//   slot   8n+2       : type (0 = dop853 8th-order, 1 = ros4 cubic)
//
// t_old/h/type live in the last three slots regardless of solver, so the
// bracketing search and dispatch in _rxDelay are uniform.
#define RX_DELAY_STRIDE(n) (8 * (n) + 3)

static double *rxDelayHistSlot(rx_solving_options_ind *ind, int n) {
  int stride = RX_DELAY_STRIDE(n);
  if (ind->delayHistStride != stride || ind->delayHistNeq != n) {
    ind->delayHistN = 0;                 // neq changed: start a fresh buffer
    ind->delayHistStride = stride;
    ind->delayHistNeq = n;
  }
  if (ind->delayHistN >= ind->delayHistCap) {
    int newCap = ind->delayHistCap > 0 ? ind->delayHistCap * 2 : 256;
    double *np = (double*) realloc(ind->delayHist,
                                   (size_t) newCap * stride * sizeof(double));
    if (np == NULL) return NULL;  // out of memory: stop growing
    ind->delayHist = np;
    ind->delayHistCap = newCap;
  }
  return ind->delayHist + (size_t) ind->delayHistN * stride;
}

// Record one dop853 dense step (8th-order Dormand-Prince interpolant).  Only the
// columns for the states delay() looks back on (op->delayState) are stored, so a
// model that delays one state in a large ODE system keeps a compact history.
static void rxDelayHistPush(rx_solving_options_ind *ind, rx_solving_options *op,
                            double xold, double h, dop853_ctx_t *ctx) {
  int nd = op->nDelayState;
  double *rec = rxDelayHistSlot(ind, nd);
  if (rec == NULL) return;
  const int *ds = op->delayState;
  const double *rc[8] = {ctx->rcont1, ctx->rcont2, ctx->rcont3, ctx->rcont4,
                         ctx->rcont5, ctx->rcont6, ctx->rcont7, ctx->rcont8};
  for (int k = 0; k < 8; k++) {
    double *col = rec + (size_t) k * nd;
    const double *src = rc[k];
    for (int c = 0; c < nd; c++) col[c] = src[ds[c]];
  }
  rec[8 * nd]     = xold;
  rec[8 * nd + 1] = h;
  rec[8 * nd + 2] = 0.0;  // dop853 record
  ind->delayHistN++;
}

// Record one ros4 (Rosenbrock) dense step.  ros4's dense output is a cubic in
// the normalized step variable, so four samples at s = 0, 1/3, 2/3, 1 (via the
// stepper's public calc_state) reconstruct it exactly.
static void rxDelayHistPushSamples(rx_solving_options_ind *ind, rx_solving_options *op,
                                   double xold, double h,
                                   const double *s0, const double *s1,
                                   const double *s2, const double *s3) {
  int nd = op->nDelayState;
  double *rec = rxDelayHistSlot(ind, nd);
  if (rec == NULL) return;
  const int *ds = op->delayState;
  const double *src[4] = {s0, s1, s2, s3};
  for (int k = 0; k < 4; k++) {
    double *col = rec + (size_t) k * nd;
    const double *s = src[k];
    for (int c = 0; c < nd; c++) col[c] = s[ds[c]];
  }
  rec[8 * nd]     = xold;
  rec[8 * nd + 1] = h;
  rec[8 * nd + 2] = 1.0;  // ros4 cubic-sample record
  ind->delayHistN++;
}

// Cap the maximum step size so the integrator never steps over the smallest
// delay; this keeps every delay() lookup inside already-recorded history
// instead of extrapolating off the current (not yet recorded) step.
static inline double rxDelayCapHmax(rx_solving_options_ind *ind) {
  double h = ind->HMAX;
  if (ind->delayHistOn && R_FINITE(ind->delayMinT)) {
    if (h <= 0.0 || ind->delayMinT < h) h = ind->delayMinT;
  }
  return h;
}

// Dense-output context passed as userdata to dopDenseSolout
struct DopDenseCtx {
  rx_solving_options_ind *ind;
  rx_solving_options     *op;
  int                    *neq;
  int                     obs_next;    // next ix-array slot to fill in solout
  int                     segment_end; // last ix-array slot for this segment
};

static void dopDenseSolout(long int nr, double xold, double x, double *y,
                           int *nptr, dop853_ctx_t *ctx,
                           void *userdata, int *irtrn) {
  DopDenseCtx *dc = (DopDenseCtx *)userdata;
  rx_solving_options_ind *ind = dc->ind;
  rx_solving_options     *op  = dc->op;
  int solveid = dc->neq[1];
  int n = nptr[0];
  double eps = 1e-8;

  // For delay differential equations, record this accepted step's dense
  // coefficients so delay() lookups can interpolate it later.  The very first
  // solout call (xold == x) carries no dense coefficients yet, so skip it.
  if (ind->delayHistOn && x != xold) {
    rxDelayHistPush(ind, op, ctx->xold, ctx->hout, ctx);
  }

  while (dc->obs_next <= dc->segment_end) {
    int    idx   = dc->obs_next;
    int    raw   = ind->ix[idx];
    double t_obs = ind->timeThread[raw];

    if (t_obs > x + eps) break;              // beyond this DOP853 step

    if (!isObs(getEvid(ind, raw))) {         // key events shouldn't appear here
      dc->obs_next++;
      continue;
    }
    if (t_obs >= xold - eps) {              // in [xold, x]: interpolate
      _growSolveIfNeeded(ind, op, idx, (idx + 1 != ind->n_all_times));
      double *slot = getSolve(idx);
      if (xold == x) {
        // Initial solout call before the first step: rcont1 not yet initialized.
        // y holds the initial state; copy it directly for any obs at the start time.
        for (int j = 0; j < n; j++) slot[j] = y[j];
      } else {
        for (int j = 0; j < n; j++)
          slot[j] = contd8(ctx, j, t_obs);   // 0-based component index
      }
      calc_lhs(solveid, t_obs, slot, ind->lhs);
    }
    dc->obs_next++;
  }
}

// Defined in ros4.cpp (included later in this translation unit).
int rxRos4DenseSegment(rx_solve *rx, rx_solving_options *op, rx_solving_options_ind *ind,
                       int *neq, t_dydt c_dydt, double xp, double xout, double *yp,
                       int obs_first, int obs_last, int solveid);

// Solve one dense segment [xp, xout], filling the segment's observations and
// recording delay history.  When the AutoSwitch composite is active (stiff2>0)
// this picks dop853 or ros4 per segment using the same Gershgorin pre/post
// stiffness checks and autoMethod hysteresis as the non-dense path, so the
// dop853+ros4 composite switches to ros4 mid-solve in dense mode.  The caller
// must set neq[0] = eff - numLin - numLinSens and dc->obs_next/segment_end.
// Leaves yp = state at xout and returns the dop853/ros4 idid.
static int denseSegmentSolve(rx_solve *rx, rx_solving_options *op,
                             rx_solving_options_ind *ind, int *neq, t_dydt c_dydt,
                             double xp, double xout, double *yp,
                             DopDenseCtx *dc, int itol, int eff) {
  int neqOde  = neq[0];
  int solveid = neq[1];
  // Non-composite: plain dop853 dense output (unchanged behavior).
  if (op->stiff2 <= 0) {
    return dop853(neq, c_dydt, xp, yp, xout, op->rtol2, op->atol2, itol,
                  dopDenseSolout, 2, NULL, DBL_EPSILON, 0, 0, 0, 0,
                  rxDelayCapHmax(ind), op->H0, op->mxstep, 1, -1,
                  neqOde, NULL, 0, dc, ind->id);
  }
  // Reactive switching, mirroring dopSegmentAutoSwitch (see its comment): probe
  // with dop853 (nstiff=50 enables its built-in stiffness estimator) and re-solve
  // the segment with ros4 if it reports stiffness; skip the probe once
  // persistently stiff (autoMethod==1).  No interval-length Gershgorin pre-check.
  int idid;
  if (ind->autoMethod == 0) {
    int hist0 = ind->delayHistN;
    double _ypStack[64];
    double *_ypDyn = NULL;
    double *ypSave = (eff <= 64) ? _ypStack
                                 : (_ypDyn = (double*)malloc((size_t)eff * sizeof(double)));
    if (ypSave == NULL) {
      ind->err = 1;
      if (ind->rc[0] == 0) ind->rc[0] = -2019;
      return -4;
    }
    memcpy(ypSave, yp, (size_t)eff * sizeof(double));
    idid = dop853(neq, c_dydt, xp, yp, xout, op->rtol2, op->atol2, itol,
                  dopDenseSolout, 2, NULL, DBL_EPSILON, 0, 0, 0, 0,
                  rxDelayCapHmax(ind), op->H0, op->mxstep, 1, 50,
                  neqOde, NULL, 0, dc, ind->id);
    bool failed = (idid <= 0 || ind->err);
    if (failed) {
      // Stiffness: discard dop853's partial delay history, restore the
      // segment-start state, and re-solve the whole segment with ros4 (which
      // re-fills all observations in the segment).
      ind->delayHistN = hist0;
      ind->rc[0] = 0;
      ind->err = 0;
      memcpy(yp, ypSave, (size_t)eff * sizeof(double));
      idid = rxRos4DenseSegment(rx, op, ind, neq, c_dydt, xp, xout, yp,
                                dc->obs_next, dc->segment_end, solveid);
    }
    if (_ypDyn) free(_ypDyn);
    rxAutoSwitchCount(op, ind, true, failed);
  } else {
    idid = rxRos4DenseSegment(rx, op, ind, neq, c_dydt, xp, xout, yp,
                              dc->obs_next, dc->segment_end, solveid);
    rxAutoSwitchCount(op, ind, false, false);
  }
  return idid;
}

extern "C" void ind_dop0_dense(rx_solve *rx, rx_solving_options *op, int solveid,
                               int *neq, t_dydt c_dydt, t_update_inis u_inis) {
  clock_t t0 = clock();
  int itol=1;
  int idid=0;
  int i;
  double xout;
  double *yp;
  void *ctx = NULL;
  int istate = 0;
  static const char *err_msg[]=
    {
      "input is not consistent",
      "larger nmax is needed",
      "step size becomes too small",
      "problem is probably stiff (interrupted)"
    };
  rx_solving_options_ind *ind;
  double *x;
  double *InfusionRate;
  double *inits;
  int *rc;
  int nx;
  neq[1] = rx->ordId[solveid]-1;
  ind = &(rx->subjects[neq[1]]);
  int eff = rxEffNeq(ind, op);
  neq[0] = eff;
  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) return;
  nx = ind->n_all_times;
  inits = op->inits;
  InfusionRate = ind->InfusionRate;
  x = ind->all_times;
  rc = ind->rc;
  double xp = x[0];
  ind->solvedIdx = 0;

  // Delay differential equation history: start recording dense steps for this
  // subject when the model uses delay().  The history before x[0] is the
  // constant initial condition (handled in _rxDelay).
  ind->delayHistOn = op->hasDelay;
  ind->delayHistN  = 0;
  ind->delayT0     = x[0];
  ind->delayMinT   = R_PosInf;
  ind->delayWarmed = 0;

  // Track the last key event index so we know where each segment starts.
  // -1 means we haven't seen any key event yet; segment scans start at 0.
  int last_key_i = -1;

  DopDenseCtx dc;
  dc.ind = ind;
  dc.op  = op;
  dc.neq = neq;

  for (i = 0; i < nx; i++) {
    ind->idx  = i;
    ind->linSS = 0;

    // mainSorted re-sort -- identical to ind_dop0
    if (ind->mainSorted == 0) {
      double *_rtime = ind->timeThread;
      for (int _j = i; _j < ind->n_all_times; _j++) {
        int _raw  = ind->ix[_j];
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

    yp   = getSolve(i);
    xout = ind->timeThread[ind->ix[i]];
    if (global_debug) {
      RSprintf("dense i=%d xp=%f xout=%f\n", i, xp, xout);
    }

    int this_evid = getEvid(ind, ind->ix[i]);
    bool is_obs   = isObs(this_evid);
    bool is_last  = (i == nx - 1);
    bool need_seg = is_obs && is_last; // last obs: close the final segment

    if (is_obs && !is_last) {
      // Pure observation interior to a segment.
      // Same-time obs (at xp): fill directly from the carry-forward state.
      if (isSameTimeDop(xout, xp)) {
        _growSolveIfNeeded(ind, op, i, 1);
        int src = (last_key_i >= 0) ? last_key_i : 0;
        std::copy(getSolve(src), getSolve(src) + eff, getSolve(i));
        calc_lhs(solveid, xout, getSolve(i), ind->lhs);
        // also prime the next slot so getSolve(i+1) stays consistent
        std::copy(getSolve(i), getSolve(i) + eff, getSolve(i+1));
      }
      // else: will be filled by dopDenseSolout when the segment closes
      ind->solvedIdx = i;
      continue;
    }

    // KEY EVENT or last obs: close the pending segment, then process the event.
    // Obs events use 'continue' above and skip updateSolve's carry-forward,
    // so getSolve(i) may be uninitialized. Grow the buffer and copy current
    // state from the last key event's slot before any integration call.
    _growSolveIfNeeded(ind, op, i, !is_last);
    yp = getSolve(i);
    {
      int _src = (last_key_i >= 0) ? last_key_i : 0;
      if (i != _src) std::copy(getSolve(_src), getSolve(_src) + eff, yp);
    }
    if (this_evid == 3) {
      // evid3 (reset): no extraDose, but close any pending obs segment first.
      if (!isSameTimeDop(xout, xp)) {
        dc.obs_next    = last_key_i + 1;
        dc.segment_end = i - 1;
        if (dc.obs_next <= dc.segment_end) {
          preSolve(op, ind, xp, xout, yp);
          neq[0] = eff - op->numLin - op->numLinSens;
          idid = denseSegmentSolve(rx, op, ind, neq, c_dydt, xp, xout, yp,
                                   &dc, itol, eff);
          neq[0] = eff;
          copyLinCmt(neq, ind, op, yp);
          postSolve(neq, &idid, rc, &i, yp, err_msg, 4, true, ind, op, rx);
          xp = xout;
        }
      }
    } else {
      if (ind->err) {
        printErr(ind->err, ind->id);
        *rc = idid;
        badSolveExit(i);
      } else {
        // extraDose loop: use dense output for each pre-dose sub-segment so
        // obs before every extra-dose time are filled via dopDenseSolout.
        // The final post-dose sub-segment is handled by the main dense call below.
        while (handleExtraDose(neq, ind->BadDose, InfusionRate, ind->dose, yp, xout,
                               xp, ind->id, &i, nx, &istate, op, ind, u_inis, ctx)) {
          if (!isSameTimeDop(ind->extraDoseNewXout, xp)) {
            // Dense dop853 from xp to extraDoseNewXout, filling obs in this range.
            dc.obs_next    = last_key_i + 1;
            dc.segment_end = need_seg ? i : i - 1;
            preSolve(op, ind, xp, ind->extraDoseNewXout, yp);
            neq[0] = eff - op->numLin - op->numLinSens;
            idid = denseSegmentSolve(rx, op, ind, neq, c_dydt, xp,
                                     ind->extraDoseNewXout, yp, &dc, itol, eff);
            neq[0] = eff;
            copyLinCmt(neq, ind, op, yp);
            postSolve(neq, &idid, rc, &i, yp, err_msg, 4, true, ind, op, rx);
            xp = ind->extraDoseNewXout;
          }
          int idx  = ind->idx;
          int ixds = ind->ixds;
          int trueIdx = ind->extraDoseTimeIdx[ind->idxExtra];
          ind->idx = -1-trueIdx;
          handle_evid(ind->extraDoseEvid[trueIdx], neq[0],
                      ind->BadDose, InfusionRate, ind->dose, yp, xout, neq[1], ind);
          idid = 1;
          ind->idx = idx;
          ind->ixds = ixds;
          ind->idxExtra++;
        }

        // Dense segment solve from xp to xout, filling all pending obs via callback.
        // For a key (non-obs) event: obs in segment are slots [last_key_i+1 .. i-1].
        // For last-obs case (need_seg): obs in segment are slots [last_key_i+1 .. i].
        if (!isSameTimeDop(xout, xp)) {
          dc.obs_next    = last_key_i + 1;
          dc.segment_end = need_seg ? i : i - 1;

          preSolve(op, ind, xp, xout, yp);
          neq[0] = eff - op->numLin - op->numLinSens;
          // Before the first delay step, evaluate the RHS once so delay()
          // learns the minimum delay used to cap the step size.
          if (ind->delayHistOn && !ind->delayWarmed) {
            std::vector<double> _ddt((size_t)neq[0]);
            c_dydt(neq, xp, yp, _ddt.data());
            ind->delayWarmed = 1;
          }
          idid = denseSegmentSolve(rx, op, ind, neq, c_dydt, xp, xout, yp,
                                   &dc, itol, eff);
          neq[0] = eff;
          copyLinCmt(neq, ind, op, yp);
          postSolve(neq, &idid, rc, &i, yp, err_msg, 4, true, ind, op, rx);
          xp = xout;
        }
      }
    }

    // Handle the key event and updateSolve -- identical to ind_dop0
    if (!op->badSolve) {
      ind->idx = i;
      if (this_evid == 3) {
        handleEvid3(ind, op, rx, neq, &xp, &xout, yp, &(idid), u_inis);
      } else if (handleEvid1(&i, rx, neq, yp, &xout)) {
        handleSS(neq, ind->BadDose, InfusionRate, ind->dose, yp, xout,
                 xp, ind->id, &i, ind->n_all_times, &istate, op, ind, u_inis, ctx);
        if (ind->wh0 == EVID0_OFF) {
          yp[ind->cmt] = inits[ind->cmt];
        }
        xp = xout;
      }
      int _mtime_requeued = 0;
      if (rx->nMtime > 0) {
        if (recomputeMtimeIfNeeded(rx, ind, yp, i, xout)) {
          ind->mainSorted = 0;
          _mtime_requeued = 1;
        }
      }
      if (rx->needSort & needSortAlag) {
        if (refreshLagTimesIfNeeded(rx, ind, yp, i + 1, xout)) {
          ind->mainSorted = 0;
        }
      }
      updateSolve(ind, op, neq, xout, i, ind->n_all_times);
      if (_mtime_requeued) i--;
    }
    last_key_i = i;
    ind->solvedIdx = i;
  }
  // Release this subject's delay history (only needed during its own solve).
  if (ind->delayHist != NULL) {
    free(ind->delayHist);
    ind->delayHist = NULL;
    ind->delayHistCap = 0;
    ind->delayHistStride = 0;
    ind->delayHistNeq = 0;
  }
  ind->delayHistN = 0;
  ind->delayHistOn = 0;
  ind->solveTime += ((double)(clock() - t0))/CLOCKS_PER_SEC;
}

extern "C" void ind_dop(rx_solve *rx, int solveid,
                        t_dydt c_dydt, t_update_inis u_inis){
  rx_solving_options *op = rx->op;
  int neq[2];
  neq[0] = op->neq;
  neq[1] = 0;
  if (op->useDense)
    ind_dop0_dense(rx, op, solveid, neq, c_dydt, u_inis);
  else
    ind_dop0(rx, op, solveid, neq, c_dydt, u_inis);
}

void par_dop(rx_solve *rx){
  rx_solving_options *op = rx->op;
#ifdef _OPENMP
  int cores = op->cores;
#else
  int cores = 1;
#endif
  uint32_t nsub = rx->nsub, nsim = rx->nsim;
  int nsolve = (int)(nsim*nsub); // safe: overflow guard ensures nsim*nsub <= INT_MAX
  int displayProgress = (op->nDisplayProgress <= nsolve);
  clock_t t0 = clock();
  int curTick = 0;
  int cur = 0;
  // dop853 is thread-safe: dop853_ctx_t is stack-allocated per call (no static state)
  int abort = 0;
  uint32_t seed0 = getRxSeed1(cores);
#ifdef _OPENMP
#pragma omp parallel for num_threads(op->cores)
#endif
  for (int solveid = 0; solveid < nsolve; solveid++){
    int neq[2];        // per-thread: ind_dop0 writes neq[0] and neq[1]
    neq[0] = op->neq;
    neq[1] = 0;
    int localAbort;
#pragma omp atomic read
    localAbort = abort;
    if (localAbort == 0){
      setSeedEng1(seed0 + rx->ordId[solveid] - 1);
      if (op->useDense)
        ind_dop0_dense(rx, op, solveid, neq, dydt, update_inis);
      else
        ind_dop0(rx, op, solveid, neq, dydt, update_inis);
      if (displayProgress){
#pragma omp critical
        cur++;
#ifdef _OPENMP
        if (omp_get_thread_num() == 0) // only in master thread!
#endif
          {
            curTick = par_progress(cur, nsolve, curTick, cores, t0, 0);
            int localAbort2;
#pragma omp atomic read
            localAbort2 = abort;
            if (localAbort2 == 0){
              if (checkInterrupt()) {
                int newAbort = 1;
#pragma omp atomic write
                abort = newAbort;
              }
            }
          }
      }
    }
  }
  setRxSeedFinal(seed0 + (uint32_t)nsolve);
  if (abort == 1){
    op->abort = 1;
    par_progress(cur, nsolve, curTick, cores, t0, 1);

  } else {
    if (displayProgress && curTick < 50) par_progress(nsolve, nsolve, curTick, cores, t0, 0);
  }
  if (displayProgress){
    int doIt = isProgSupported();
    if (doIt == -1){
    } else if (isRstudio() || doIt == 0){
      RSprintf("\n");
    } else {
      RSprintf("\r                                                                                \r");
    }
  }
}

double ind_linCmtFH(double h, int i,
                    rx_solve *rx, rx_solving_options *op, int solveid, int *_neq,
                    t_dydt c_dydt, t_update_inis u_inis) {
  rx_solving_options_ind *ind = &(rx->subjects[_neq[1]]);
  ind->linCmtH = h;
  ind->linCmtHparIndex = i;
  return ind_linCmt0H(rx, op, solveid, _neq, c_dydt, u_inis);
}

// Gill 1983 Chat
static inline double Chat(double phi, double h, double epsA){
  if (phi == 0) return 2*epsA/(h*h);
  return 2*epsA/(h*fabs(phi));
}

static inline double ChatP(double phi, double h, double epsA){
  if (phi == 0) return 4*epsA/(h*h*h);
  return 4*epsA/(h*h*fabs(phi));
}

static inline double Phi(double fp, double f, double fn, double h){
  return (fp-2*f+fn)/(h*h);
}
static inline double phiC(double fp, double fn, double h){
  return (fp-fn)/(2*h);
}
static inline double phiF(double f, double fp, double h){
  return (fp-f)/h;
}
static inline double phiB(double f, double fn, double h){
  return (f-fn)/h;
}

extern "C" double linCmtScaleInitPar(int which);

//' @param *hf is the forward difference final estimate
//' @param *hphif is central difference final estimate (when switching from forward to central differences)
//' @param *df is the derivative estimate
//' @param *df2 is the 2nd derivative estimate, useful for pre-conditioning.
//' @param *ef is the err of the final estimate.
//' @param thetaSens is the sensitivity vector
//' @param cpar (integer) is the parameter we are considering
//'
//' @param epsR (err) is the relative error for the problem
//'
//' @param K is the maximum number of iterations before giving up on searching for the best interval.

//' @param fTol gradient error tolerance that
//'     is acceptable before issuing a warning/error about the gradient estimates.
//'
//' @param gillStep When looking for the optimal forward difference
//'     step size, this is This is the step size to increase the
//'     initial estimate by.  So each iteration the new step size =
//'     (prior step size)*gillStep
//'
//' @param gillF This is the f value at the current estimate
//'
//' Returns 1 -- Success
//'         2 -- Large error; Derivative estimate error 50% or more of the derivative
//'         3 -- Function constant or nearly constant for this parameter
//'         4 -- Function odd or nearly linear, df = K, df2 ~ 0
//'         5 -- df2 increases rapidly as h decreases
int gill83linCmt(double *hf, double *hphif, double *df, double *df2, double *ef,
                 int cpar, double epsR, int K, double gillStep,
                 double fTol, double gillF,
                 rx_solve *rx, rx_solving_options *op, int solveid, int *_neq,
                 t_dydt c_dydt, t_update_inis u_inis) {
  double f , hbar, h0, fp, fn=NA_REAL,
    phif, phib, phic, phicc = 0, phi, Chf, Chb,
    Ch, hs, hphi, hk, tmp, ehat, lasth,
    lastht=NA_REAL, lastfpt=NA_REAL, phict=NA_REAL;
  f = gillF;
  int k = 0;
  double x = linCmtScaleInitPar(cpar);
  // Relative error should be given by the tolerances, I believe.
  double epsA=std::fabs(f)*epsR;
  // FD1: // Initialization
  hbar = 2*(1+std::fabs(x))*sqrt(epsA/(1+std::fabs(f)));
  h0 = gillStep*hbar;
  lasth=h0;
  // theta(cpar, 0) = x + h0;
  fp = ind_linCmtFH(h0, cpar, rx, op, solveid, _neq, c_dydt, u_inis);
  // theta(cpar, 0) = x - h0;
  fn = ind_linCmtFH(-h0, cpar, rx, op, solveid, _neq, c_dydt, u_inis);
  phif = phiF(f, fp, h0);
  phib = phiB(f, fn, h0);
  phic = phiC(fp, fn, h0);
  phi = Phi(fp, f, fn, h0);

  Chf = Chat(phif, h0, epsA);
  Chb = Chat(phib, h0, epsA);
  Ch  = ChatP(phi, h0, epsA);
  hs  = -1;
  hphi=hbar; // Not defined in Gill, but used for central difference switch if there are problems
  // FD2:  // Decide if to accept the interval
  hk = h0;
  if (max2(Chf, Chb) <= 0.1){
    hs=h0;
  }
  if (0.001 <= Ch && Ch <= 0.1){
    phicc=phic;
    hphi=h0;
    if (fTol != 0 && fabs(phif) < fTol){
      lastfpt = fp;
      phict=phic;
      lastht  = lasth;
    }
    goto FD5;
  }
  if (fTol != 0 && fabs(phif) < fTol){
    lastfpt = fp;
    lastht  = lasth;
    phict=phic;
  }
  if (Ch < 0.001){
    goto FD4;
  }
 FD3: // Increase h
  k++;
  hk=hk*gillStep;
  lasth=hk;
  // Compute the associated finite difference estimates and their
  // relative condition errors.
  // theta(cpar, 0) = x + hk;
  fp = ind_linCmtFH(hk, cpar, rx, op, solveid, _neq, c_dydt, u_inis);
  // theta(cpar, 0) = x-hk;
  fn = ind_linCmtFH(-hk, cpar, rx, op, solveid, _neq, c_dydt, u_inis);
  phif = phiF(f, fp, hk);
  phib = phiB(f, fn, hk);
  phic = phiC(fp, fn, hk);
  phi = Phi(fp, f, fn, hk);
  Chf = Chat(phif, hk, epsA);
  Chb = Chat(phib, hk, epsA);
  Ch = ChatP(phi, hk, epsA);
  if (hs < 0 && max2(Chf, Chb) <= 0.1){
    hs = hk;
  }
  if (Ch <= 0.1){
    phicc=phic;
    hphi = hk;
    if (fTol != 0 && fabs(phif) < fTol){
      lastfpt = fp;
      lastht  = lasth;
      phict=phic;
    }
    goto FD5;
  }
  if (fTol != 0 && fabs(phif) < fTol){
    lastfpt = fp;
    lastht  = lasth;
    phict=phic;
  }
  if (k == K) goto FD6;
  goto FD3;
 FD4: // Decrease h
  k++;
  hk=hk/gillStep;
  lasth=hk;
  // Compute the associated finite difference estimates and their
  // relative condition errors.
  // theta(cpar, 0) = x + hk;
  fp = ind_linCmtFH(hk, cpar, rx, op, solveid, _neq, c_dydt, u_inis);
  // theta(cpar, 0) = x-hk;
  fn = ind_linCmtFH(-hk, cpar, rx, op, solveid, _neq, c_dydt, u_inis);
  phif = phiF(f, fp, hk);
  phib = phiB(f, fn, hk);
  tmp=phic;
  phic = phiC(fp, fn, hk);
  phi = Phi(fp, f, fn, hk);
  Chf = Chat(phif, hk, epsA);
  Chb = Chat(phib, hk, epsA);
  Ch = ChatP(phi, hk, epsA);
  if (Ch > .1){
    phicc=tmp;
    hphi=hk*gillStep; // hphi = h_k-1
    if (fTol != 0 && fabs(phif) < fTol){
      lastfpt = fp;
      lastht  = lasth;
      phict=phic;
    }
    goto FD5;
  }
  if (max2(Chf, Chb) <= 0.1){
    hs = hk;
  }
  if (0.001 <= Ch && Ch <= 1){
    hphi = hk;
    if (fTol != 0 && fabs(phif) < fTol){
      lastfpt = fp;
      lastht  = lasth;
      phict=phic;
    }
    goto FD5;
  }
  if (fTol != 0 && fabs(phif) < fTol){
    lastfpt = fp;
    lastht  = lasth;
    phict=phic;
  }
  if (k == K) goto FD6;
  goto FD4;
 FD5: // Compute the estimate of the optimal interval
  *df2 = phi;
  *hf = 2*sqrt(epsA/fabs(phi));
  fp = ind_linCmtFH(*hf, cpar, rx, op, solveid, _neq, c_dydt, u_inis);
  // Restore theta
  // theta(cpar, 0) = x;
  *df = phiF(f, fp, *hf);
  *ef = (*hf)*fabs(phi)/2+2*epsA/(*hf);
  *hphif=hphi;
  ehat = fabs(*df-phicc);
  if (max2(*ef, ehat) <= 0.5*(*df)){
    return 1;
  } else {
    // warning("The finite difference derivative err more than 50%% of the slope; Consider a different starting point.");
    if (!ISNA(lastht)){
      // Could be used;  Stick with the last below Ftol
      // *hf = lasth;
      // fp = lastfp;
      // *df = phiF(f, fp, *hf);
      // *df2=0;
      // // *df = 0.0; // Doesn't move.
      // *hphif=2*(*hf);
      // } else {
      *hf = lastht;
      fp = lastfpt;
      *df = phiF(f, fp, *hf);
      *df2=phic;
      // *df = 0.0; // Doesn't move.
      *hphif=phict;
    }
    return 2;
  }
  //
 FD6: // Check unsatisfactory cases
  if (hs < 0){
    // F nearly constant.
    // Use sqrt(h0) as a last ditch effort.
    *hf = pow(DBL_EPSILON, 0.25);//hbar;
    // *df=phic;
    // theta(cpar, 0) = x + *hf;
    fp = ind_linCmtFH(*hf, cpar, rx, op, solveid, _neq, c_dydt, u_inis);
    *df = phiF(f, fp, *hf);
    *df2=0;
    // *df = 0.0; // Doesn't move.
    *hphif= sqrt(h0);
    // warning("The surface around the initial estimate is nearly constant in one parameter grad=0.  Consider a different starting point.");
    return 3;
  }
  if (Ch > 0.1){ // Odd or nearly linear.
    *hf = h0;
    *df = phic;
    *df2 = 0;
    *ef = 2*epsA/(*hf);
    *hphif=hphi;
    // warning("The surface odd or nearly linear for one parameter; Check your function.");
    return 4;
  }
  // f'' is increasing rapidly as h decreases
  *hf = h0;
  *df = phic;
  *df2 = phi;
  *hphif=hphi;
  *ef = (*hf)*fabs(phi)/2+2*epsA/(*hf);
  // warning("The surface around the initial estimate is highly irregular in at least one parameter.  Consider a different starting point.");
  return 5;
}

extern "C" double linCmtScaleInitN();
extern "C" int linCmtZeroJac(int i);

void gillForwardH(rx_solve *rx, rx_solving_options *op, int solveid, int *_neq,
 t_dydt c_dydt, t_update_inis u_inis) {
  double f0 = ind_linCmtFH(0.0, -1, rx, op, solveid, _neq, c_dydt, u_inis);
  double hf=0, hphif=0, df=0, df2=0, ef=0;
  int N = linCmtScaleInitN();
  rx_solving_options_ind *ind = &(rx->subjects[_neq[1]]);
  double *hh = ind->linH;

  for (int i = 0; i < N; i++) {
    if (linCmtZeroJac(i)) {
      hh[i] = 0.0;
      continue;
    }
    gill83linCmt(&hf, &hphif, &df, &df2, &ef,
                 i, rx->linCmtGillRtol, rx->linCmtGillK, rx->linCmtGillStep,
                 rx->linCmtGillFtol, f0, rx, op, solveid, _neq, c_dydt, u_inis);
    hh[i] = hf;
  }
}

double shiRF(double &h,
             double ef,
             int &idx,
             double &f0, double &f1, double &l, double &u,
             bool &finiteF1, bool &finiteF4,
             rx_solve *rx, rx_solving_options *op, int solveid, int *_neq,
             t_dydt c_dydt, t_update_inis u_inis) {
  // Eigen::Matrix<double, Eigen::Dynamic, 1> tp4 = thetaIn;
  // Eigen::Matrix<double, Eigen::Dynamic, 1> tp1 = thetaIn;
  // tp4[idx] += 4*h;
  // tp1[idx] += h;
  // f1 = fdoubleh(tp1);
  f1 = ind_linCmtFH(h, idx, rx, op, solveid, _neq, c_dydt, u_inis);
  finiteF1 = std::isfinite(f1);
  if (!finiteF1) {
    finiteF4 = true;
    return -1.0;
  }
  double f4 = ind_linCmtFH(4*h, idx, rx, op, solveid, _neq, c_dydt, u_inis);
  finiteF4 = std::isfinite(f4);
  if (!finiteF4) {
    return -1.0;
  }
  // REprintf("f0 = %f f1 = %f f4 = %f\n", f0, f1, f4);
  return abs(f4-4*f1+3*f0)/(8.0*ef);
}

double shi21Forward(double &h,
                    double &f0,
                    // arma::vec &gr,
                    int idx,
                    double ef,
                    double rl,
                    double ru,
                    int maxiter,
                    rx_solve *rx, rx_solving_options *op, int solveid, int *_neq,
                    t_dydt c_dydt, t_update_inis u_inis) {
  // Algorithm 2.1 in paper
  // q=2, alpha=4, r=3
  // s = 0, 1
  // w = -1, 1
  if (h == 0) {
    // 2/sqrt(3) = 1.154700538379251684162
    h = 1.154700538379251684162 * sqrt(ef);
  } else {
    h = fabs(h);
  }
  double l = 0, u = R_PosInf, rcur = NA_REAL;
  double f1;
  double lasth = h;
  int iter=0;
  bool finiteF1 = true, finiteF4 = true, calcGrad = false;
  while(true) {
    iter++;
    if (iter > maxiter) {
      h = lasth;
      break;
    }
    rcur = shiRF(h, ef, idx, f0, f1, l, u,
                 finiteF1, finiteF4, rx, op, solveid, _neq,
                 c_dydt, u_inis);
    if (rcur == -1) {
      if (!finiteF1) {
        // hnew = t + 2.5*hold
        h = 0.5*h;
        continue;
      }
      h = 3.5*h;

      if (!calcGrad) {
        lasth = h;
        // gr = (f1-f0)/h;
      }
      continue;
    } else {
      lasth = h;
      // gr = (f1-f0)/h;
    }
    if (rcur < rl) {
      l = h;
    } else if (rcur > ru) {
      u = h;
    } else {
      break;
    }
    if (!R_finite(u)) {
      h = 4.0*h;
    } else if (l == 0) {
      h = h/4.0;
    } else {
      h = (l + u)/2.0;
    }
  }
  return h;
}

void shi21ForwardH(rx_solve *rx, rx_solving_options *op, int solveid, int *_neq,
                   t_dydt c_dydt, t_update_inis u_inis) {
  double h = 0.0;
  double f0 = ind_linCmtFH(0.0, -1, rx, op, solveid, _neq, c_dydt, u_inis);
  int N = linCmtScaleInitN();
  rx_solving_options_ind *ind = &(rx->subjects[_neq[1]]);
  double *hh = ind->linH;
  for (int i = 0; i < N; i++) {
    if (linCmtZeroJac(i)) {
      hh[i] = 0.0;
      continue;
    }
    h = 0.0;
    hh[i] = shi21Forward(h, f0, i,
                         rx->linCmtShiErr,
                         1.5,
                         6.0,
                         rx->linCmtShiMax,
                         rx, op, solveid, _neq,
                         c_dydt, u_inis);
  }
}

double shiRC(double &h, double ef,
             int &idx,
             double &fp1, double &fm1,
             double &l, double &u,
             bool &finiteFp1, bool &finiteFp3,
             bool &finiteFm1, bool &finiteFm3,
             rx_solve *rx, rx_solving_options *op, int solveid, int *_neq,
             t_dydt c_dydt, t_update_inis u_inis) {
  // tp3(idx)  += 3*h;
  // tp1(idx)  += h;
  // tm3(idx)  -= 3*h;
  // tm1(idx)  -= h;
  fp1 = ind_linCmtFH(h, idx, rx, op, solveid, _neq, c_dydt, u_inis);
  finiteFp1 = std::isfinite(fp1);
  if (!finiteFp1) {
    finiteFm1 = true;
    finiteFp3 = true;
    finiteFm3 = true;
    return -1.0;
  }
  fm1 = ind_linCmtFH(-h, idx, rx, op, solveid, _neq, c_dydt, u_inis);
  finiteFm1 = std::isfinite(fp1);
  if (!finiteFm1) {
    finiteFp3 = true;
    finiteFm3 = true;
    return -1.0;
  }
  double fp3 = ind_linCmtFH(3*h, idx, rx, op, solveid, _neq, c_dydt, u_inis);
  finiteFp3 = std::isfinite(fp3);
  if (!finiteFp3) {
    finiteFp3 = true;
    return -1.0;
  }
  double fm3 = ind_linCmtFH(-3*h, idx, rx, op, solveid, _neq, c_dydt, u_inis);
  finiteFm3 = std::isfinite(fp3);
  if (!finiteFm3) {
    return -1.0;
  }
  return abs(fp3-3*fp1+3*fm1-fm3)/(8.0*ef);
}

double shi21Central(double &h,
                    double &f0, int idx,
                    double ef, double rl, double ru, double nu,
                    int maxiter,
                    rx_solve *rx, rx_solving_options *op, int solveid, int *_neq,
                    t_dydt c_dydt, t_update_inis u_inis) {
  // Algorithm 3.1
  // weights = -0.5, 0.5
  // s = -1, 1
  // Equation 3.3
  //
  if (h == 0.0) {
    h = pow(3.0*ef, 0.3333333333333333333333);
  } else {
    h = fabs(h);
  }
  double l = 0, u = R_PosInf, rcur = NA_REAL;
  double hlast = h;

  double fp1;
  double fm1;

  int iter=0;
  bool finiteFp1 = true, finiteFp3 = true,
    finiteFm1=true, finiteFm3=true, calcGrad=false;
  while(true) {
    iter++;
    if (iter > maxiter) {
      h=hlast;
      break;
    }
    rcur = shiRC(h, ef, idx, fp1, fm1, l, u,
                 finiteFp1, finiteFp3, finiteFm1, finiteFm3,
                 rx, op, solveid, _neq, c_dydt, u_inis);
    // Need f1 from shiRF to compute forward difference
    if (rcur == -1.0) {
      if (!finiteFp1) {
        // hnew*3 = hold*0.5
        h = h*0.5/3.0;
        continue;
      } else if (!finiteFm1) {
        if (!calcGrad) {
          // forward difference
          calcGrad = true;
          // gr = (fp1-f0)/h;
        }
        h = h*0.5/3.0;
        continue;
      }
      // hnew*3 = hold*2
      h = h*2.0/3.0;
      if (!calcGrad) {
        // central difference
        calcGrad = true;
        // gr = (fp1-fm1)/(2*h);
        hlast = h;
      }
      continue;
    } else {
      calcGrad = true;
      // gr = (fp1-fm1)/(2*h);
      hlast = h;
    }
    if (rcur < rl) {
      l = h;
    } else if (rcur > ru) {
      u = h;
    } else {
      break;
    }
    if (!R_finite(u)) {
      h = nu*h;
    } else if (l == 0) {
      h = h/nu;
    } else {
      h = (l + u)/2.0;
    }
  }
  return h;
}

void shi21CentralH(rx_solve *rx, rx_solving_options *op, int solveid, int *_neq,
                   t_dydt c_dydt, t_update_inis u_inis) {
  double h = 0.0;
  double f0 = ind_linCmtFH(0.0, -1, rx, op, solveid, _neq, c_dydt, u_inis);
  int N = linCmtScaleInitN();
  rx_solving_options_ind *ind = &(rx->subjects[_neq[1]]);
  double *hh = ind->linH;
  for (int i = 0; i < N; i++) {
    if (linCmtZeroJac(i)) {
      hh[i] = 0.0;
      continue;
    }
    h = 0.0;
    hh[i] = shi21Central(h, f0, i,
                         rx->linCmtShiErr,
                         1.5,
                         4.5,
                         3.0,
                         rx->linCmtShiMax,
                         rx, op, solveid, _neq,
                         c_dydt, u_inis);
  }
}


void setupLinH(rx_solve *rx, int solveid,
               t_dydt dydt, t_update_inis u_inis) {
  if (rx->sensType == 100) {
    rx->sensType = 3; // AD
  }
  rx_solving_options *op = &op_global;
  int neq[2];
  neq[0] = op->neq;
  neq[1] = solveid;
  rx_solving_options_ind *ind = &(rx->subjects[neq[1]]);
  double *hh = ind->linH;
  if (ind->linCmtHparIndex == -3) return; // already setup
  switch (rx->sensType) {
  case 1: // forward; shi difference
    shi21ForwardH(rx, op, solveid, neq, dydt, u_inis);
    break;
  case 2: // central; shi
    shi21CentralH(rx, op, solveid, neq, dydt, u_inis);
    break;
  case 3: // 3pt forward; shi
    shi21ForwardH(rx, op, solveid, neq, dydt, u_inis);
    break;
  case 4: // 5-point endpoint difference; shi
    shi21CentralH(rx, op, solveid, neq, dydt, u_inis);
    for (int i = 0; i < 7; i++) {
      hh[i] /= 4;
    }
    break;
  case 7: // 3 pt forward, gill
  case 6: // forward difference with gill H est
    gillForwardH(rx, op, solveid, neq, dydt, u_inis);
    break;
  case 8: // 5-point endpoint; Gill
    gillForwardH(rx, op, solveid, neq, dydt, u_inis);
    for (int i = 0; i < 7; i++) {
      hh[i] /= 4;
    }
    break;
  default: // all the rest are constant
    std::fill_n(ind->linH, 7, rx->sensH);
    break;
  }
  ind->linCmtH = NA_REAL;
  ind->linCmtHparIndex = -3;
}

// This is used by focei to optimize per individual
extern "C" void ind_solve(rx_solve *rx, unsigned int cid,
                          t_dydt_liblsoda dydt_lls,
                          t_dydt_lsoda_dum dydt_lsoda, t_jdum_lsoda jdum,
                          t_dydt c_dydt, t_update_inis u_inis,
                          int jt) {
  // par_progress_1=0;
  // _isRstudio = isRstudio();
  // setRstudioPrint(_isRstudio);
  // rxt.t0 = clock();
  // rxt.cores = 1;
  // rxt.n = 100;
  // rxt.d = 0;
  // rxt.cur = 0;
  // assignFuns();
  rx_solving_options *op = rx->op;
  if (op->neq !=  0) {
    if (rx->linB == 1) {
      // Setup H
      setupLinH(rx, cid, c_dydt, u_inis);
    }
    // Per-individual neqOverride: when set (e.g. nlmixr2est's predOde from
    // shi21EtaGeneral / likInner0 doFD path), force dispatch through the
    // ODE-solver branch.  The pure-linCmt fast path assumes the full
    // op->neq state layout and shares op->numLin / op->numLinSens with the
    // rxInner sensitivity model; under override the caller is solving the
    // smaller predNoLhs ODE model, which by construction has numLin == 0,
    // so the linCmt fast path is not the right dispatch.
    rx_solving_options_ind *_ind = &(rx->subjects[cid]);
    int _eff = rxEffNeq(_ind, op);
    if (_eff == op->neq && op->neq == op->numLinSens + op->numLin) {
      // This only is linear compartment solving
      ind_linCmt(rx, cid, c_dydt, u_inis);
      return;
    } else {
      switch (op->stiff){
      case 3:
        ind_indLin(rx, cid, u_inis);
        break;
      case 2:
        ind_liblsoda(rx, cid, dydt_lls, u_inis);
        break;
      case 1:
        ind_lsoda(rx,cid, dydt_lsoda, u_inis, jdum, jt);
        break;
      case 5:
        ind_rkf78(rx, cid, c_dydt, u_inis);
        break;
      case 6:
        ind_rk4(rx, cid, c_dydt, u_inis);
        break;
      case 206: // rk4s      -- discrete-adjoint RK4
      case 239: // eulers    -- discrete-adjoint forward Euler
      case 240: // midpoints -- discrete-adjoint explicit midpoint
      case 241: // heuns     -- discrete-adjoint Heun
      case 243: // rk3s      -- discrete-adjoint Kutta RK3
      case 210: // dop5s     -- discrete-adjoint adaptive Dormand-Prince 5(4)
      case 225: // rk43s     -- discrete-adjoint adaptive Runge-Kutta 4(3)
      case 200: // dop853s   -- discrete-adjoint adaptive Dormand-Prince 8(5,3)
      case 207: // ck54s     -- discrete-adjoint adaptive Cash-Karp 5(4)
      case 265: // bs32s     -- discrete-adjoint adaptive Bogacki-Shampine 3(2)
      case 227: // vern65s   -- discrete-adjoint adaptive Verner 6(5)
      case 228: // vern76s   -- discrete-adjoint adaptive Verner 7(6)
      case 229: // dop87s    -- discrete-adjoint adaptive Prince-Dormand 8(7)
      case 226: // dop54s/dp54s -- discrete-adjoint adaptive Dormand-Prince 5(4)
      case 230: // vern98s   -- discrete-adjoint adaptive Verner 9(8)
      case 205: // f78s      -- discrete-adjoint adaptive Fehlberg 7(8)
      case 213: // ros4s     -- discrete-adjoint Rosenbrock (stiff)
      case 236: // radauiia5s -- discrete-adjoint Radau IIA 5th (stiff)
      case 233: // backwardEulers -- discrete-adjoint implicit Euler (stiff)
      case 234: // gauss6s   -- discrete-adjoint Gauss-Legendre 6th (stiff)
      case 238: // sdirk43s  -- discrete-adjoint SDIRK 5-stage order 3 (stiff)
      case 235: // iiic6s    -- discrete-adjoint Lobatto IIIC 6th (stiff)
      case 231: // ros43s    -- discrete-adjoint GRK4A Rosenbrock 4th (stiff)
      case 232: // ros6s     -- discrete-adjoint ROW6A Rosenbrock 6th (stiff)
      case 237: // geng5s    -- discrete-adjoint Geng5 fully-implicit 5th (stiff)
      case 267: // f45s
      case 268: // t54s
      case 270: // pp54s
      case 271: // pp54bs
      case 272: // bs54s
      case 273: // ss54s
      case 274: // dp65s
      case 275: // c65s
      case 276: // tp64s
      case 277: // v65rs
      case 279: // dverk65s
      case 280: // tf65s
      case 281: // tp75s
      case 283: // tmy7ss
      case 284: // v76rs
      case 285: // ss76s
      case 286: // v78s
      case 287: // dverk78s
      case 288: // dp85s
      case 289: // tp86s
      case 290: // v87es
      case 291: // v87rs
      case 292: // ev87s
      case 293: // k87s
      case 295: // v89s
      case 296: // t98as
      case 297: // v98rs
      case 298: // s98s
      case 300: // c108s
      case 301: // b109s
      case 302: // s1110as
      case 304: // o129s
      case 282: // tmy7adj
        ind_rk4s(rx, cid, c_dydt, u_inis);
        break;
      case 221: // cvodesadj -- CVODES adjoint sensitivities (self-managed primal)
        ind_cvodesadj(rx, cid, c_dydt, u_inis);
        break;
      case 202: // liblsodaadj -- exact discrete adjoint of liblsoda's multistep map
        ind_liblsodaadj(rx, cid, c_dydt, u_inis);
        break;
      case 208: // abs -- discrete-adjoint Adams-Bashforth
        ind_ab_adj(rx, cid, c_dydt, u_inis);
        break;
      case 7:
        ind_ck54(rx, cid, c_dydt, u_inis);
        break;
      case 8:
        ind_ab(rx, cid, c_dydt, u_inis);
        break;
      case 9:
        ind_abm(rx, cid, c_dydt, u_inis);
        break;
      case 10:
        ind_dop5(rx, cid, c_dydt, u_inis);
        break;
      case 11:
        ind_bs(rx, cid, c_dydt, u_inis);
        break;
      case 13:
        ind_ros4(rx, cid, c_dydt, u_inis);
        break;
      case 14:
        ind_iem(rx, cid, c_dydt, u_inis);
        break;
      case 15:
        ind_sem(rx, cid, c_dydt, u_inis);
        break;
      case 16:
        ind_sb3a(rx, cid, c_dydt, u_inis);
        break;
      case 17:
        ind_sb3am4(rx, cid, c_dydt, u_inis);
        break;
      case 18:
        ind_vv(rx, cid, c_dydt, u_inis);
        break;
      case 19:
        ind_mm(rx, cid, c_dydt, u_inis);
        break;
      case 20:
        ind_em(rx, cid, c_dydt, u_inis);
        break;
      case 21:
        ind_cvode(rx, cid, u_inis);
        break;
      case 22:
        ind_trapz(rx, cid, c_dydt, u_inis);
        break;
      case 23:
        ind_ssp3(rx, cid, c_dydt, u_inis);
        break;
      case 24:
        ind_rkf32(rx, cid, c_dydt, u_inis);
        break;
      case 25:
        ind_rk43(rx, cid, c_dydt, u_inis);
        break;
      case 26:
        ind_dop54(rx, cid, c_dydt, u_inis);
        break;
      case 27:
        ind_vern65(rx, cid, c_dydt, u_inis);
        break;
      case 28:
        ind_vern76(rx, cid, c_dydt, u_inis);
        break;
      case 29:
        ind_dop87(rx, cid, c_dydt, u_inis);
        break;
      case 30:
        ind_vern98(rx, cid, c_dydt, u_inis);
        break;
      case 31: ind_grk4a(rx, cid, c_dydt, u_inis); break;
      case 32: ind_ros6(rx, cid, c_dydt, u_inis); break;
      case 33: ind_backwardEuler(rx, cid, c_dydt, u_inis); break;
      case 34: ind_gauss6(rx, cid, c_dydt, u_inis); break;
      case 35: ind_iiic6(rx, cid, c_dydt, u_inis); break;
      case 36: ind_radauiia5(rx, cid, c_dydt, u_inis); break;
      case 37: ind_geng5(rx, cid, c_dydt, u_inis); break;
      case 38: ind_sdirk43(rx, cid, c_dydt, u_inis); break;
      case 39: ind_euler(rx, cid, c_dydt, u_inis); break;
      case 40: ind_midpoint(rx, cid, c_dydt, u_inis); break;
      case 41: ind_heun(rx, cid, c_dydt, u_inis); break;
      case 42: ind_rkssp22(rx, cid, c_dydt, u_inis); break;
      case 43: ind_rk3(rx, cid, c_dydt, u_inis); break;
      case 44: ind_rkssp53(rx, cid, c_dydt, u_inis); break;
      case 45: ind_rks4(rx, cid, c_dydt, u_inis); break;
      case 46: ind_rkr4(rx, cid, c_dydt, u_inis); break;
      case 47: ind_rkls44(rx, cid, c_dydt, u_inis); break;
      case 48: ind_rkls54(rx, cid, c_dydt, u_inis); break;
      case 49: ind_rkssp54(rx, cid, c_dydt, u_inis); break;
      case 50: ind_rks5(rx, cid, c_dydt, u_inis); break;
      case 51: ind_rk5(rx, cid, c_dydt, u_inis); break;
      case 52: ind_rkc5(rx, cid, c_dydt, u_inis); break;
      case 53: ind_rkl5(rx, cid, c_dydt, u_inis); break;
      case 54: ind_rklk5a(rx, cid, c_dydt, u_inis); break;
      case 55: ind_rklk5b(rx, cid, c_dydt, u_inis); break;
      case 56: ind_rkb6(rx, cid, c_dydt, u_inis); break;
      case 57: ind_rk7(rx, cid, c_dydt, u_inis); break;
      case 58: ind_rk8_10(rx, cid, c_dydt, u_inis); break;
      case 59: ind_rkcv8(rx, cid, c_dydt, u_inis); break;
      case 60: ind_rk8_12(rx, cid, c_dydt, u_inis); break;
      case 61: ind_rks10(rx, cid, c_dydt, u_inis); break;
      case 62: ind_rkz10(rx, cid, c_dydt, u_inis); break;
      case 63: ind_rko10(rx, cid, c_dydt, u_inis); break;
      case 64: ind_rkh10(rx, cid, c_dydt, u_inis); break;
      case 65: ind_rkbs32(rx, cid, c_dydt, u_inis); break;
      case 66: ind_rkssp43(rx, cid, c_dydt, u_inis); break;
      case 67: ind_rkf45(rx, cid, c_dydt, u_inis); break;
      case 68: ind_rkt54(rx, cid, c_dydt, u_inis); break;
      case 69: ind_rks54(rx, cid, c_dydt, u_inis); break;
      case 70: ind_rkpp54(rx, cid, c_dydt, u_inis); break;
      case 71: ind_rkpp54b(rx, cid, c_dydt, u_inis); break;
      case 72: ind_rkbs54(rx, cid, c_dydt, u_inis); break;
      case 73: ind_rkss54(rx, cid, c_dydt, u_inis); break;
      case 74: ind_rkdp65(rx, cid, c_dydt, u_inis); break;
      case 75: ind_rkc65(rx, cid, c_dydt, u_inis); break;
      case 76: ind_rktp64(rx, cid, c_dydt, u_inis); break;
      case 77: ind_rkv65r(rx, cid, c_dydt, u_inis); break;
      case 78: ind_rkv65(rx, cid, c_dydt, u_inis); break;
      case 79: ind_dverk65(rx, cid, c_dydt, u_inis); break;
      case 80: ind_rktf65(rx, cid, c_dydt, u_inis); break;
      case 81: ind_rktp75(rx, cid, c_dydt, u_inis); break;
      case 82: ind_rktmy7(rx, cid, c_dydt, u_inis); break;
      case 83: ind_rktmy7s(rx, cid, c_dydt, u_inis); break;
      case 84: ind_rkv76r(rx, cid, c_dydt, u_inis); break;
      case 85: ind_rkss76(rx, cid, c_dydt, u_inis); break;
      case 86: ind_rkv78(rx, cid, c_dydt, u_inis); break;
      case 87: ind_dverk78(rx, cid, c_dydt, u_inis); break;
      case 88: ind_rkdp85(rx, cid, c_dydt, u_inis); break;
      case 89: ind_rktp86(rx, cid, c_dydt, u_inis); break;
      case 90: ind_rkv87e(rx, cid, c_dydt, u_inis); break;
      case 91: ind_rkv87r(rx, cid, c_dydt, u_inis); break;
      case 92: ind_rkev87(rx, cid, c_dydt, u_inis); break;
      case 93: ind_rkk87(rx, cid, c_dydt, u_inis); break;
      case 94: ind_rkf89(rx, cid, c_dydt, u_inis); break;
      case 95: ind_rkv89(rx, cid, c_dydt, u_inis); break;
      case 96: ind_rkt98a(rx, cid, c_dydt, u_inis); break;
      case 97: ind_rkv98r(rx, cid, c_dydt, u_inis); break;
      case 98: ind_rks98(rx, cid, c_dydt, u_inis); break;
      case 99: ind_rkf108(rx, cid, c_dydt, u_inis); break;
      case 100: ind_rkc108(rx, cid, c_dydt, u_inis); break;
      case 101: ind_rkb109(rx, cid, c_dydt, u_inis); break;
      case 102: ind_rks1110a(rx, cid, c_dydt, u_inis); break;
      case 103: ind_rkf1210(rx, cid, c_dydt, u_inis); break;
      case 104: ind_rko129(rx, cid, c_dydt, u_inis); break;
      case 105: ind_rkf1412(rx, cid, c_dydt, u_inis); break;
      case 106: ind_lsode(rx, cid); break;
      case 107: ind_bdf(rx, cid); break;

      case 0:
        ind_dop(rx, cid, c_dydt, u_inis);
        break;
      }
    }
  }
  iniSubject(cid, 1, &(rx->subjects[cid]), op, rx, u_inis);
  par_progress_0=0;
}

extern "C" void par_solve(rx_solve *rx) {
  _isRstudio = isRstudio();
  setRstudioPrint(_isRstudio);
  par_progress_1=0;
  rxt.t0 = clock();
  rxt.cores = 1;
  rxt.n = 100;
  rxt.d = 0;
  rxt.cur = 0;
  assignFuns();
  rx_solving_options *op = rx->op;
  if (op->neq != 0) {
    if (rx->linB == 1) {
      // Setup H
    }
    if (op->neq == op->numLinSens + op->numLin) {
      // This only is linear compartment solving
      par_linCmt(rx);
      return;
    } else {
#ifdef _OPENMP
      int cores = op->cores;
#else
      int cores = 1;
#endif
      int etaCores = cores;
      // DLSODA/DLSODE backends are intentionally single-threaded (COMMON blocks).
      // Keep ETA pre-generation single-threaded too for these methods on Windows.
      if (op->stiff == 1 || op->stiff == 106 || op->stiff == 107) {
        etaCores = 1;
      }
      // Pre-generate all eta draws before the parallel loop.
      // simeta() reads from the buffer instead of calling rxRmvnA() per subject.
      rxPreGenEta(rx, etaCores);
      switch(op->stiff){
      case 3:
        par_indLin(rx);
        break;
      case 2:
        par_liblsoda(rx);
        break;
      case 4:
        par_liblsodaR(rx);
        break;
      case 1:
        // lsoda
        par_lsoda(rx);
        break;
      case 5:
        // rkf78
        par_rkf78(rx);
        break;
      case 6:
        // rk4
        par_rk4(rx);
        break;
      case 206: // rk4s -- discrete-adjoint RK4
      case 239: // eulers
      case 240: // midpoints
      case 241: // heuns
      case 243: // rk3s
      case 210: // dop5s
      case 225: // rk43s
      case 200: // dop853s
      case 207: // ck54s
      case 265: // bs32s
      case 227: // vern65s
      case 228: // vern76s
      case 229: // dop87s
      case 226: // dop54s/dp54s
      case 230: // vern98s
      case 205: // f78s
      case 213: // ros4s
      case 236: // radauiia5s
      case 233: // backwardEulers
      case 234: // gauss6s
      case 238: // sdirk43s
      case 235: // iiic6s
      case 231: // ros43s
      case 232: // ros6s
      case 237: // geng5s
      case 267: // f45s
      case 268: // t54s
      case 270: // pp54s
      case 271: // pp54bs
      case 272: // bs54s
      case 273: // ss54s
      case 274: // dp65s
      case 275: // c65s
      case 276: // tp64s
      case 277: // v65rs
      case 279: // dverk65s
      case 280: // tf65s
      case 281: // tp75s
      case 283: // tmy7ss
      case 284: // v76rs
      case 285: // ss76s
      case 286: // v78s
      case 287: // dverk78s
      case 288: // dp85s
      case 289: // tp86s
      case 290: // v87es
      case 291: // v87rs
      case 292: // ev87s
      case 293: // k87s
      case 295: // v89s
      case 296: // t98as
      case 297: // v98rs
      case 298: // s98s
      case 300: // c108s
      case 301: // b109s
      case 302: // s1110as
      case 304: // o129s
      case 282: // tmy7adj
        par_rk4s(rx);
        break;
      case 221: // cvodesadj
        par_cvodesadj(rx);
        break;
      case 202: // liblsodaadj
        par_liblsodaadj(rx);
        break;
      case 208: // abs
        par_ab_adj(rx);
        break;
      case 7:
        // ck54
        par_ck54(rx);
        break;
      case 8:
        par_ab(rx);
        break;
      case 9:
        par_abm(rx);
        break;
      case 10:
        par_dop5(rx);
        break;
      case 11:
        par_bs(rx);
        break;
      case 13:
        par_ros4(rx);
        break;
      case 14:
        par_iem(rx);
        break;
      case 15:
        par_sem(rx);
        break;
      case 16:
        par_sb3a(rx);
        break;
      case 17:
        par_sb3am4(rx);
        break;
      case 18:
        par_vv(rx);
        break;
      case 19:
        par_mm(rx);
        break;
      case 20:
        par_em(rx);
        break;
      case 21:
        par_cvode(rx);
        break;
      case 22:
        par_trapz(rx);
        break;
      case 23:
        par_ssp3(rx);
        break;
      case 24:
        par_rkf32(rx);
        break;
      case 25:
        par_rk43(rx);
        break;
      case 26:
        par_dop54(rx);
        break;
      case 27:
        par_vern65(rx);
        break;
      case 28:
        par_vern76(rx);
        break;
      case 29:
        par_dop87(rx);
        break;
      case 30:
        par_vern98(rx);
        break;
      case 31: par_grk4a(rx); break;
      case 32: par_ros6(rx); break;
      case 33: par_backwardEuler(rx); break;
      case 34: par_gauss6(rx); break;
      case 35: par_iiic6(rx); break;
      case 36: par_radauiia5(rx); break;
      case 37: par_geng5(rx); break;
      case 38: par_sdirk43(rx); break;
      case 39: par_euler(rx); break;
      case 40: par_midpoint(rx); break;
      case 41: par_heun(rx); break;
      case 42: par_rkssp22(rx); break;
      case 43: par_rk3(rx); break;
      case 44: par_rkssp53(rx); break;
      case 45: par_rks4(rx); break;
      case 46: par_rkr4(rx); break;
      case 47: par_rkls44(rx); break;
      case 48: par_rkls54(rx); break;
      case 49: par_rkssp54(rx); break;
      case 50: par_rks5(rx); break;
      case 51: par_rk5(rx); break;
      case 52: par_rkc5(rx); break;
      case 53: par_rkl5(rx); break;
      case 54: par_rklk5a(rx); break;
      case 55: par_rklk5b(rx); break;
      case 56: par_rkb6(rx); break;
      case 57: par_rk7(rx); break;
      case 58: par_rk8_10(rx); break;
      case 59: par_rkcv8(rx); break;
      case 60: par_rk8_12(rx); break;
      case 61: par_rks10(rx); break;
      case 62: par_rkz10(rx); break;
      case 63: par_rko10(rx); break;
      case 64: par_rkh10(rx); break;
      case 65: par_rkbs32(rx); break;
      case 66: par_rkssp43(rx); break;
      case 67: par_rkf45(rx); break;
      case 68: par_rkt54(rx); break;
      case 69: par_rks54(rx); break;
      case 70: par_rkpp54(rx); break;
      case 71: par_rkpp54b(rx); break;
      case 72: par_rkbs54(rx); break;
      case 73: par_rkss54(rx); break;
      case 74: par_rkdp65(rx); break;
      case 75: par_rkc65(rx); break;
      case 76: par_rktp64(rx); break;
      case 77: par_rkv65r(rx); break;
      case 78: par_rkv65(rx); break;
      case 79: par_dverk65(rx); break;
      case 80: par_rktf65(rx); break;
      case 81: par_rktp75(rx); break;
      case 82: par_rktmy7(rx); break;
      case 83: par_rktmy7s(rx); break;
      case 84: par_rkv76r(rx); break;
      case 85: par_rkss76(rx); break;
      case 86: par_rkv78(rx); break;
      case 87: par_dverk78(rx); break;
      case 88: par_rkdp85(rx); break;
      case 89: par_rktp86(rx); break;
      case 90: par_rkv87e(rx); break;
      case 91: par_rkv87r(rx); break;
      case 92: par_rkev87(rx); break;
      case 93: par_rkk87(rx); break;
      case 94: par_rkf89(rx); break;
      case 95: par_rkv89(rx); break;
      case 96: par_rkt98a(rx); break;
      case 97: par_rkv98r(rx); break;
      case 98: par_rks98(rx); break;
      case 99: par_rkf108(rx); break;
      case 100: par_rkc108(rx); break;
      case 101: par_rkb109(rx); break;
      case 102: par_rks1110a(rx); break;
      case 103: par_rkf1210(rx); break;
      case 104: par_rko129(rx); break;
      case 105: par_rkf1412(rx); break;
      case 106: par_lsode(rx); break;
      case 107: par_bdf(rx); break;
      case 0:
        // dop
        par_dop(rx);
        break;
      }
      rxEtaPreDeactivate();
    }
  } else {
    // Pure-LHS (neq==0) models: the ODE solve block is skipped entirely.
    // Call iniSubject for every subject so that:
    //   (1) _setIndPointersByThread sets ind->timeThread (non-NULL invariant
    //       that rxode2_df relies on), and
    //   (2) sortInd initialises ix[] (required for correct event iteration
    //       in rxode2_df).
    // rxSolveFree() at the start of rxSolve_ clears timeThread for any
    // subject that previously owned its allocation, so this loop is the
    // only place those pointers are restored before output creation.
    uint32_t nsubAll = (uint32_t)rx->nsub * rx->nsim;
    for (uint32_t _sid = 0; _sid < nsubAll; _sid++) {
      iniSubject((int)_sid, 1, &rx->subjects[_sid], op, rx, update_inis);
    }
  }
  /* Standalone rxSolve users see one summary line per call instead of a
     flood. nlmixr2est does not enter through par_solve for inner iterations
     (it calls ind_solve per subject) and flushes from its own iteration
     printout — so this flush will not interfere with that aggregation. */
  rxSolveWarnFlush(5);
  par_progress_0=0;
}

rx_solve *_globalRx = NULL;

extern "C" void rxode2_assign_rx(rx_solve *rx){
  _globalRx=rx;
}

extern "C" double rxLhsP(int i, rx_solve *rx, unsigned int id){
  rx_solving_options_ind *ind = &(rx->subjects[id]);
  rx_solving_options *op = &op_global;
  if (i < op->nlhs){
    return(ind->lhs[i]);
  } else {
    rxSolveFreeC();
    (Rf_errorcall)(R_NilValue, "Trying to access an equation that isn't calculated. lhs(%d/%d); id: %s\n",i, op->nlhs, getId(id));
  }
  return 0;
}

#undef min
#undef max
#include "implicit_euler_rxode2.hpp"
#define IN_PAR_SOLVE
#include "rkf78.cpp"
#include "rk4.cpp"
#include "rk4s.cpp"
#include "cvodes_adjoint.cpp"   // CVODES ASA adjoint-sensitivity driver
#include "lsoda_adjoint.cpp"    // exact discrete-adjoint of liblsoda's multistep map
#include "ck54.cpp"
#include "ab.cpp"
#include "ab_adjoint.cpp"      // discrete-adjoint of Adams-Bashforth (abs, 208)
#include "abm.cpp"
#include "dop5.cpp"
#include "bs.cpp"
#include "ros4.cpp"
#include "iem.cpp"
#include "sem.cpp"
#include "sb3a.cpp"
#include "sb3am4.cpp"
#include "vv.cpp"
#include "mm.cpp"
#include "em.cpp"
#include "cvode.cpp"
#include "cvode_dense.cpp"
#include "trapz.cpp"
#include "ssp3.cpp"
#include "rkf32.cpp"
#include "rk43.cpp"
#include "dop54.cpp"
#include "vern65.cpp"
#include "vern76.cpp"
#include "dop87.cpp"
#include "vern98.cpp"
#include "grk4a.cpp"
#include "implicit_solvers.cpp"
#include "euler.cpp"
#include "midpoint.cpp"
#include "heun.cpp"
#include "rkssp22.cpp"
#include "rk3.cpp"
#include "rkssp53.cpp"
#include "rks4.cpp"
#include "rkr4.cpp"
#include "rkls44.cpp"
#include "rkls54.cpp"
#include "rkssp54.cpp"
#include "rks5.cpp"
#include "rk5.cpp"
#include "rkc5.cpp"
#include "rkl5.cpp"
#include "rklk5a.cpp"
#include "rklk5b.cpp"
#include "rkb6.cpp"
#include "rk7.cpp"
#include "rk8_10.cpp"
#include "rkcv8.cpp"
#include "rk8_12.cpp"
#include "rks10.cpp"
#include "rkz10.cpp"
#include "rko10.cpp"
#include "rkh10.cpp"
#include "rkbs32.cpp"
#include "rkssp43.cpp"
#include "rkf45.cpp"
#include "rkt54.cpp"
#include "rks54.cpp"
#include "rkpp54.cpp"
#include "rkpp54b.cpp"
#include "rkbs54.cpp"
#include "rkss54.cpp"
#include "rkdp65.cpp"
#include "rkc65.cpp"
#include "rktp64.cpp"
#include "rkv65r.cpp"
#include "rkv65.cpp"
#include "dverk65.cpp"
#include "rktf65.cpp"
#include "rktp75.cpp"
#include "rktmy7.cpp"
#include "rktmy7s.cpp"
#include "rkv76r.cpp"
#include "rkss76.cpp"
#include "rkv78.cpp"
#include "dverk78.cpp"
#include "rkdp85.cpp"
#include "rktp86.cpp"
#include "rkv87e.cpp"
#include "rkv87r.cpp"
#include "rkev87.cpp"
#include "rkk87.cpp"
#include "rkf89.cpp"
#include "rkv89.cpp"
#include "rkt98a.cpp"
#include "rkv98r.cpp"
#include "rks98.cpp"
#include "rkf108.cpp"
#include "rkc108.cpp"
#include "rkb109.cpp"
#include "rks1110a.cpp"
#include "rkf1210.cpp"
#include "rko129.cpp"
#include "rkf1412.cpp"
