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
#include "strncmp.h"
#include "timsort.h"
#include "../inst/include/rxode2.h"
#include "../inst/include/rxode2parseHandleEvid.h"
#include "../inst/include/rxode2parseGetTime.h"
#include "linCmtDiffConstant.h"

#define SORT gfx::timsort

#define isSameTimeOp(xout, xp) (op->stiff == 0 ? isSameTimeDop(xout, xp) : isSameTime(xout, xp))

// dop853 is same time

extern "C" uint32_t getRxSeed1(int ncores);
extern "C" void setSeedEng1(uint32_t seed);
extern "C" void setRxSeedFinal(uint32_t seed);

extern "C" {
#include "dop853.h"
#include "common.h"
#include "lsoda.h"
#include "rxode2_df.h"
}
#include "par_solve.h"
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )
#define badSolveExit(i) for (int j = op->neq*(ind->n_all_times); j--;){ \
    ind->solve[j] = NA_REAL;                                           \
  }                                                                     \
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

extern "C" SEXP _rxHasOpenMp(){
  SEXP ret = PROTECT(Rf_allocVector(LGLSXP,1));
#ifdef _OPENMP
  INTEGER(ret)[0] = 1;
#else
  INTEGER(ret)[0] = 0;
#endif
  UNPROTECT(1);
  return ret;
}

rx_solve rx_global;

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

rx_solving_options_ind *inds_global = NULL;
int gitol=0, gitask = 1, giopt = 0, gliw=0, glrw = 0;

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
  rxt.cur++;
  SEXP ret = PROTECT(Rf_allocVector(INTSXP, 1));
  rxt.d =par_progress(rxt.cur, rxt.n, rxt.d, rxt.cores, rxt.t0, 0);
  INTEGER(ret)[0]=rxt.d;
  UNPROTECT(1);
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
    Rf_errorcall(R_NilValue, "%s", CHAR(STRING_ELT(str,0)));
  }
  return R_NilValue;
}

t_set_solve set_solve = NULL;

extern "C" void rxOptionsIniEnsure(int mx){
  R_Free(inds_global);
  inds_global = R_Calloc(mx, rx_solving_options_ind);
  rx_solve *rx=(&rx_global);
  rx->subjects = inds_global;
  rx->ordId = NULL;
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
  if (!strcmp(valStr, "cmt") ||
      !strcmp(valStr, "CMT") ||
      !strcmp(valStr, "Cmt")) {
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

t_update_inis update_inis = NULL;

t_dydt_lsoda_dum dydt_lsoda_dum = NULL;

t_dydt_liblsoda dydt_liblsoda = NULL;

t_jdum_lsoda jdum_lsoda = NULL;

t_get_solve get_solve = NULL;

t_assignFuns assignFuns=NULL;

t_F AMT = NULL;
t_LAG LAG = NULL;
t_RATE RATE = NULL;
t_DUR DUR = NULL;
t_calc_mtime calc_mtime = NULL;

t_ME ME = NULL;
t_IndF IndF = NULL;


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
      for (int j = op->neq; j--;) {
        yp[j]= min(top,yp[j]);
      }
    }
    if (R_FINITE(rx->stateTrimL)){
      double bottom=rx->stateTrimL;
      for (int j = op->neq; j--;) {
        yp[j]= max(bottom,yp[j]);
      }
    }
  }
  ind->slvr_counter[0]++;
}

int global_jt = 2;
int global_mf = 22;
int global_debug = 0;

double *global_rworkp;
int *global_iworkp;

unsigned int global_rworki = 0;
double *global_rwork(unsigned int mx){
  if (mx >= global_rworki){
    bool first = (global_rworki == 0);
    global_rworki = mx+1024;
    if (first) {
      global_rworkp = R_Calloc(global_rworki, double);
    } else {
      global_rworkp = R_Realloc(global_rworkp, global_rworki, double);
    }
  }
  return global_rworkp;
}

extern "C" int _locateTimeIndex(double obs_time,  rx_solving_options_ind *ind);

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
  update_inis =(t_update_inis) R_GetCCallable(lib, s_inis);
  dydt_lsoda_dum =(t_dydt_lsoda_dum) R_GetCCallable(lib, s_dydt_lsoda_dum);
  jdum_lsoda =(t_jdum_lsoda) R_GetCCallable(lib, s_dydt_jdum_lsoda);
  set_solve = (t_set_solve)R_GetCCallable(lib, s_ode_solver_solvedata);
  get_solve = (t_get_solve)R_GetCCallable(lib, s_ode_solver_get_solvedata);
  dydt_liblsoda = (t_dydt_liblsoda)R_GetCCallable(lib, s_dydt_liblsoda);
  AMT = (t_F)R_GetCCallable(lib, s_AMT);
  LAG = (t_LAG) R_GetCCallable(lib, s_LAG);
  RATE = (t_RATE) R_GetCCallable(lib, s_RATE);
  DUR = (t_DUR) R_GetCCallable(lib, s_DUR);
  ME  = (t_ME) R_GetCCallable(lib, s_ME);
  IndF  = (t_IndF) R_GetCCallable(lib, s_IndF);
  calc_mtime = (t_calc_mtime) R_GetCCallable(lib, s_mtime);
  assignFuns = R_GetCCallable(lib, s_assignFuns);
  rx_solve *rx=(&rx_global);
  rx->subjects = inds_global;
  rx_solving_options *op = &op_global;
  rx->op = op;
  char s_assignFuns2[300];
  snprintf(s_assignFuns2, 300, "%s2", s_assignFuns);
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
    SORT(ind->ix, ind->ix + ind->n_all_times,
         [ind, time](int a, int b){
           double timea = time[a],
             timeb = time[b];
           if (timea == timeb) {
             return a < b;
           }
           return timea < timeb;
         });
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
extern "C" void solout(long int nr, double t_old, double t, double *y, int *nptr, int *irtrn){}

extern "C" int indLin(int cSub, rx_solving_options *op, double tp, double *yp_, double tf,
                      double *InfusionRate_, int *on_,
                      t_ME ME, t_IndF  IndF);

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
  int idid, itol=0;
  int neqOde = op->neq - op->numLin - op->numLinSens;
  if (neqOde == 0 && op->neq > 0) {
    if (!isSameTime(xout, xp)) {
      preSolve(op, ind, xp, xout, yp);
      linSolve(neq, ind, yp, &xp, xout);
    }
  } else {
    switch(op->stiff) {
    case 3:
      if (!isSameTime(xout, xp)) {
        preSolve(op, ind, xp, xout, yp);
        idid = indLin(ind->id, op, xp, yp, xout, ind->InfusionRate, ind->on,
                      ME, IndF);
      }
      if (idid <= 0) {
        /* RSprintf("IDID=%d, %s\n", istate, err_msg_ls[-*istate-1]); */
        ind->rc[0] = idid;
        // Bad Solve => NA
        badSolveExit(*i);
      } else if (ind->err){
        /* RSprintf("IDID=%d, %s\n", istate, err_msg_ls[-*istate-1]); */
        ind->rc[0] = idid;
        // Bad Solve => NA
        badSolveExit(*i);
      }
      break;
    case 2:
      if (!isSameTime(xout, xp)) {
        preSolve(op, ind, xp, xout, yp);
        lsoda((lsoda_context_t*)ctx, yp, &xp, xout);
        copyLinCmt(neq, ind, op, yp);
      }
      if (*istate <= 0) {
        RSprintf("IDID=%d, %s\n", *istate, err_msg_ls[-(*istate)-1]);
        ind->rc[0] = -2019;
        break;
      } else if (ind->err){
        printErr(ind->err, ind->id);
        ind->rc[0] = -2019;
        *i = ind->n_all_times-1; // Get out of here!
        break;
      }
      break;
    case 1:
      if (!isSameTime(xout, xp)) {
        preSolve(op, ind, xp, xout, yp);
        neq[0] = op->neq - op->numLin - op->numLinSens;
        F77_CALL(dlsoda)(dydt_lsoda_dum, neq, yp, &xp, &xout,
                         &gitol, &(op->RTOL), &(op->ATOL), &gitask,
                         istate, &giopt, global_rworkp,
                         &glrw, global_iworkp, &gliw, jdum_lsoda, &global_jt);
        neq[0] = op->neq;
        copyLinCmt(neq, ind, op, yp);
      }
      if (*istate <= 0) {
        RSprintf("IDID=%d, %s\n", *istate, err_msg_ls[-(*istate)-1]);
        ind->rc[0] = -2019;/* *istate; */
        break;
      } else if (ind->err){
        printErr(ind->err, ind->id);
        ind->rc[0] = -2019;
        break;
      }
      break;
    case 0:
      if (!isSameTimeDop(xout, xp)) {
        preSolve(op, ind, xp, xout, yp);
        // change to real ODE num
        neq[0] = op->neq - op->numLin - op->numLinSens;
        idid = dop853(neq,       /* dimension of the system <= UINT_MAX-1*/
                      dydt,         /* function computing the value of f(x,y) */
                      xp,           /* initial x-value */
                      yp,           /* initial values for y */
                      xout,         /* final x-value (xend-x may be positive or negative) */
                      &(op->RTOL),          /* relative error tolerance */
                      &(op->ATOL),          /* absolute error tolerance */
                      itol,         /* switch for rtoler and atoler */
                      solout,         /* function providing the numerical solution during integration */
                      0,         /* switch for calling solout */
                      NULL,           /* messages stream */
                      DBL_EPSILON,    /* rounding unit */
                      0,              /* safety factor */
                      0,              /* parameters for step size selection */
                      0,
                      0,              /* for stabilized step size control */
                      ind->HMAX,              /* maximal step size */
                      op->H0,            /* initial step size */
                      op->mxstep,            /* maximal number of allowed steps */
                      1,            /* switch for the choice of the coefficients */
                      -1,                     /* test for stiffness */
                      0,                      /* number of components for which dense outpout is required */
                      NULL,           /* indexes of components for which dense output is required, >= nrdens */
                      0                       /* declared length of icon */
                      );
        // switch to overall states
        neq[0] = op->neq;
        copyLinCmt(neq, ind, op, yp);
      }
      if (idid < 0) {
        ind->rc[0] = -2019;
        break;
      } else if (ind->err){
        printErr(ind->err, ind->id);
        *i = ind->n_all_times-1; // Get out of here!
        break;
      }
      break;
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
                              int *canBreak) {
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
    int evid = getEvid(ind, ind->ix[*i]);
    int wh, cmt, wh100, whI, wh0;
    getWh(evid, &wh, &cmt, &wh100, &whI, &wh0);
    ind->linSSbolusCmt = cmt;
    // Use "real" xout for handle_evid functions.
    handle_evid(getEvid(ind, ind->ix[*i]), neq[0],
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
  for (int j = 0; j < op->maxSS; j++) {
    ind->idx=*i;
    *xout2 = *xp2 + *curIi;
    // Use "real" xout for handle_evid functions.
    handle_evid(getEvid(ind, ind->ix[*i]), neq[0],
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
      for (int k = op->neq; k--;) {
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
        ind->solveLast[k] = yp[k];
        if (op->ssRtol[k]*fabs(yp[k]) + op->ssAtol[k] <= fabs(yp[k]-ind->solveLast[k])){
          *canBreak=0;
        }
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
        ind->solveLast[k] = yp[k];
        if (op->ssRtol[k]*fabs(yp[k]) + op->ssAtol[k] <= fabs(yp[k]-ind->solveLast[k])){
          *canBreak=0;
        }
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
      if (canBreak){
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
  int j;
  int doSS2=0;
  int doSSinf=0;
  int maxSS = op->maxSS;
  int minSS = op->minSS;
  int isSsLag = ind->wh0 == EVID0_SS20 || ind->wh0 == EVID0_SS0;
  bool skipDosingEvent = false, isRateDose = false;
  bool isModeled = ind->whI == EVIDF_MODEL_DUR_ON ||
    ind->whI == EVIDF_MODEL_RATE_ON;
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
    if (doSSinf){
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
          while (ind->ix[bi] != ind->idose[infBixds] && bi < ind->n_all_times) {
            bi++;
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
                        getAllTimes(ind, ind->idose[ind->ixds]));
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
      ei = *i;
      while(ind->ix[ei] != ind->idose[infEixds] && ei < ind->n_all_times) {
        ei++;
      }
      if (ind->ix[ei] != ind->idose[infEixds]){
        /* Rf_errorcall(R_NilValue, "Cannot figure out infusion end time."); */
        if (!(ind->err & 8388608)){
          ind->err += 8388608;
          /* Rf_errorcall(R_NilValue, "Rate is zero/negative"); */
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
      curLagExtra = getLag(ind, neq[1], ind->cmt, startTimeD) -
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
        // Add at the end
        for (j = neq[0];j--;) yp[j]+=ind->solveSave[j];
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
                    &canBreak);
      if (isSsLag) {
        //advance the lag time
        ind->idx=*i;
        xout2 = xp2 + curIi - curLagExtra;
        // Use "real" xout for handle_evid functions.
        *istate=1;
        handle_evid(getEvid(ind, ind->ix[bi]), neq[0],
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
          startTimeD = getTime(ind->idose[infFixds],ind);
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

static inline void
updateSolve(rx_solving_options_ind *ind, rx_solving_options *op, int *neq,
            double &xout,
            int &i, int &nx) {
  if (i+1 != nx) {
    std::copy(getSolve(i), getSolve(i+1), getSolve(i+1));
  }
  calc_lhs(neq[1], xout, getSolve(i), ind->lhs);
}

//================================================================================
// Inductive linearization routines
extern "C" void ind_indLin0(rx_solve *rx, rx_solving_options *op, int solveid,
                            t_update_inis u_inis, t_ME ME, t_IndF IndF) {
  clock_t t0 = clock();
  assignFuns();
  int i;
  int neq[2];
  neq[0] = op->neq;
  neq[1] = solveid;
  /* double *yp = &yp0[neq[1]*neq[0]]; */
  int nx;
  rx_solving_options_ind *ind;
  double *inits;
  double *x;
  int *BadDose;
  double *InfusionRate;
  double xout, xoutp;
  int *rc;
  double *yp;
  inits = op->inits;
  int idid = 0;
  ind = &(rx->subjects[neq[1]]);
  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) return;
  nx = ind->n_all_times;
  BadDose = ind->BadDose;
  InfusionRate = ind->InfusionRate;
  x = ind->all_times;
  rc= ind->rc;
  double xp = x[0];
  xoutp=xp;
  ind->solvedIdx = 0;
  for (i=0; i<nx; i++) {
    ind->idx=i;
    ind->linSS=0;
    xout = getTime_(ind->ix[i], ind);
    yp = getSolve(i);
    if(getEvid(ind, ind->ix[i]) != 3 && !isSameTime(xout, xp)) {
      if (ind->err){
        *rc = -1000;
        // Bad Solve => NA
        badSolveExit(i);
      } else {
        preSolve(op, ind, xoutp, xout, yp);
        idid = indLin(solveid, op, xoutp, yp, xout, ind->InfusionRate, ind->on,
                      ME, IndF);
        xoutp=xout;
        postSolve(neq, &idid, rc, &i, yp, NULL, 0, true, ind, op, rx);
      }
    }
    ind->_newind = 2;
    if (!op->badSolve){
      ind->idx = i;
      if (getEvid(ind, ind->ix[i]) == 3) {
        handleEvid3(ind, op, rx, neq, &xp, &xout,  yp, &idid, u_inis);
      } else if (handleEvid1(&i, rx, neq, yp, &xout)){
        handleSS(neq, BadDose, InfusionRate, ind->dose, yp, xout,
                 xp, ind->id, &i, nx, &idid, op, ind, u_inis, NULL);
        if (ind->wh0 == 30){
          yp[ind->cmt] = inits[ind->cmt];
        }
        if (rx->istateReset) idid = 1;
        xp = xout;
      }

      updateSolve(ind, op, neq, xout, i, nx);
      ind->slvr_counter[0]++; // doesn't need do be critical; one subject at a time.
    }
    ind->solvedIdx = i;
  }
  ind->solveTime += ((double)(clock() - t0))/CLOCKS_PER_SEC;
}

extern "C" void ind_indLin(rx_solve *rx,
                           int solveid, t_update_inis u_inis, t_ME ME, t_IndF IndF){
  assignFuns();
  rx_solving_options *op = &op_global;
  ind_indLin0(rx, op, solveid, u_inis, ME, IndF);
}


extern "C" void par_indLin(rx_solve *rx){
  assignFuns();
  rx_solving_options *op = &op_global;
  int cores = 1;
  int nsub = rx->nsub, nsim = rx->nsim;
  int displayProgress = (op->nDisplayProgress <= nsim*nsub);
  clock_t t0 = clock();
  /* double *yp0=(double*) malloc((op->neq)*nsim*nsub*sizeof(double)); */
  int curTick=0;
  int cur=0;
  // Breaking of of loop ideas came from http://www.thinkingparallel.com/2007/06/29/breaking-out-of-loops-in-openmp/
  // http://permalink.gmane.org/gmane.comp.lang.r.devel/27627
  // It was buggy due to Rprint.  Use REprint instead since Rprint calls the interrupt every so often....
  int abort = 0;
  // FIXME parallel
  uint32_t seed0 = getRxSeed1(1);
  for (int solveid = 0; solveid < nsim*nsub; solveid++){
    if (abort == 0){
      setSeedEng1(seed0 + solveid - 1 );
      ind_indLin(rx, solveid, update_inis, ME, IndF);
      if (displayProgress){ // Can only abort if it is long enough to display progress.
        curTick = par_progress(solveid, nsim*nsub, curTick, 1, t0, 0);
      }
    }
  }
  setRxSeedFinal(seed0 + nsim*nsub);
  if (abort == 1){
    op->abort = 1;
    /* yp0 = NULL; */
    par_progress(cur, nsim*nsub, curTick, cores, t0, 1);
  } else {
    if (displayProgress && curTick < 50) par_progress(nsim*nsub, nsim*nsub, curTick, cores, t0, 0);
  }
}




// ================================================================================
// liblsoda
extern "C" void ind_liblsoda0(rx_solve *rx, rx_solving_options *op, struct lsoda_opt_t opt, int solveid,
                              t_dydt_liblsoda dydt_liblsoda, t_update_inis u_inis) {
  clock_t t0 = clock();
  int i;
  int neq[2];
  neq[0] = op->neq;
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
  int nx;
  rx_solving_options_ind *ind;
  double *inits;
  double *x;
  int *BadDose;
  double *InfusionRate;
  double xout;
  int *rc;
  double *yp;
  int neqOde = *neq - op->numLin - op->numLinSens;
  inits = op->inits;
  struct lsoda_context_t * ctx = lsoda_create_ctx();
  ctx->function = (_lsoda_f)dydt_liblsoda;
  ctx->data = neq;
  ctx->neq = neqOde;
  ctx->state = 1;
  ctx->error=NULL;
  ind = &(rx->subjects[neq[1]]);
  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) {
    free(ctx);
    ctx = NULL;
    return;
  }
  nx = ind->n_all_times;
  BadDose = ind->BadDose;
  InfusionRate = ind->InfusionRate;
  x = ind->all_times;
  rc= ind->rc;
  double xp = x[0];
  lsoda_prepare(ctx, &opt);
  ind->solvedIdx = 0;
  for(i=0; i<nx; i++) {
    ind->idx=i;
    ind->linSS=0;
    yp = getSolve(i);
    xout = getTime_(ind->ix[i], ind);
    if (getEvid(ind, ind->ix[i]) != 3) {
      if (ind->err){
        *rc = -1000;
        // Bad Solve => NA
        badSolveExit(i);
      } else {
        // REprintf("xp: %f xout: %f\n", xp, xout);
        if (handleExtraDose(neq, BadDose, InfusionRate, ind->dose, yp, xout,
                            xp, ind->id, &i, nx, &(ctx->state), op, ind, u_inis, ctx)) {
          if (!isSameTime(ind->extraDoseNewXout, xp)) {
            preSolve(op, ind, xp, ind->extraDoseNewXout, yp);
            lsoda(ctx, yp, &xp, ind->extraDoseNewXout);
            copyLinCmt(neq, ind, op, yp);
            postSolve(neq, &(ctx->state), rc, &i, yp, NULL, 0, false, ind, op, rx);
          }
          int idx = ind->idx;
          int ixds= ind->ixds;
          int trueIdx = ind->extraDoseTimeIdx[ind->idxExtra];
          ind->idx = -1-trueIdx;
          handle_evid(ind->extraDoseEvid[trueIdx], neq[0],
                      BadDose, InfusionRate, ind->dose, yp, xout, neq[1], ind);
          ctx->state = 1;
          ind->idx = idx;
          ind->ixds = ixds;
          ind->idxExtra++;
          if (!isSameTime(xout, ind->extraDoseNewXout)) {
            preSolve(op, ind, ind->extraDoseNewXout, xout, yp);
            lsoda(ctx,yp, &ind->extraDoseNewXout, xout);
            copyLinCmt(neq, ind, op, yp);
            postSolve(neq, &(ctx->state), rc, &i, yp, NULL, 0, false, ind, op, rx);
          }
          xp =  ind->extraDoseNewXout;
        }
        if (!isSameTime(xout, xp)) {
          preSolve(op, ind, xp, xout, yp);
          lsoda(ctx, yp, &xp, xout);
          copyLinCmt(neq, ind, op, yp);
          postSolve(neq, &(ctx->state), rc, &i, yp, NULL, 0, false, ind, op, rx);
        }
        xp = xout;
      }
    }
    ind->_newind = 2;
    if (!op->badSolve){
      ind->idx = i;
      if (getEvid(ind, ind->ix[i]) == 3) {
        handleEvid3(ind, op, rx, neq, &xp, &xout,  yp, &(ctx->state), u_inis);
      } else if (handleEvid1(&i, rx, neq, yp, &xout)) {
        handleSS(neq, BadDose, InfusionRate, ind->dose, yp, xout,
                 xp, ind->id, &i, nx, &(ctx->state), op, ind, u_inis, ctx);
        if (ind->wh0 == EVID0_OFF){
          yp[ind->cmt] = inits[ind->cmt];
        }
        if (rx->istateReset) ctx->state = 1;
        xp = xout;
      }
      updateSolve(ind, op, neq, xout, i, nx);
      ind->slvr_counter[0]++; // doesn't need do be critical; one subject at a time.
      /* for(j=0; j<neq[0]; j++) ret[neq[0]*i+j] = yp[j]; */
    }
    ind->solvedIdx = i;
  }
  // Reset LHS to NA
  lsoda_free(ctx);
  free(ctx);
  ind->solveTime += ((double)(clock() - t0))/CLOCKS_PER_SEC;
}

extern "C" void ind_liblsoda(rx_solve *rx, int solveid,
                             t_dydt_liblsoda dydt, t_update_inis u_inis){
  rx_solving_options *op = &op_global;
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
  int nsub = rx->nsub, nsim = rx->nsim;
  int displayProgress = (op->nDisplayProgress <= nsim*nsub);
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
  int abort = 0;
  uint32_t seed0 = getRxSeed1(cores);
#ifdef _OPENMP
#pragma omp parallel for num_threads(cores)
#endif
  for (int thread=0; thread < cores; thread++) {
    for (int solveid = thread; solveid < nsim*nsub; solveid+=cores){
      if (abort == 0){
        setSeedEng1(seed0 + rx->ordId[solveid] - 1);

        ind_linCmt0(rx, op, solveid, neq, dydt, update_inis);

        if (displayProgress && thread == 0) {
#pragma omp critical
          cur++;
#ifdef _OPENMP
          if (omp_get_thread_num() == 0) // only in master thread!
#endif
            {
              curTick = par_progress(cur, nsim*nsub, curTick, cores, t0, 0);
              if (abort == 0){
                if (checkInterrupt()) abort =1;
              }
            }
        }
      }
    }
  }
  setRxSeedFinal(seed0 + nsim*nsub);
  if (abort == 1){
    op->abort = 1;
    /* yp0 = NULL; */
    par_progress(cur, nsim*nsub, curTick, cores, t0, 1);
  } else {
    if (displayProgress && curTick < 50) par_progress(nsim*nsub, nsim*nsub, curTick, cores, t0, 0);
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
  int nsub = rx->nsub, nsim = rx->nsim;
  int displayProgress = (op->nDisplayProgress <= nsim*nsub);
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
  int abort = 0;
  uint32_t seed0 = getRxSeed1(cores);
#ifdef _OPENMP
#pragma omp parallel for num_threads(cores)
#endif
  for (int thread=0; thread < cores; thread++) {
    for (int solveid = thread; solveid < nsim*nsub; solveid+=cores){
      if (abort == 0){
        setSeedEng1(seed0 + rx->ordId[solveid] - 1 );
        ind_liblsoda0(rx, op, opt, solveid, dydt_liblsoda, update_inis);
        if (displayProgress && thread == 0) {
#pragma omp critical
          cur++;
#ifdef _OPENMP
          if (omp_get_thread_num() == 0) // only in master thread!
#endif
            {
              curTick = par_progress(cur, nsim*nsub, curTick, cores, t0, 0);
              if (abort == 0){
                if (checkInterrupt()) abort =1;
              }
            }
        }
      }
    }
  }
  setRxSeedFinal(seed0 + nsim*nsub);
  if (abort == 1){
    op->abort = 1;
    /* yp0 = NULL; */
    par_progress(cur, nsim*nsub, curTick, cores, t0, 1);
  } else {
    if (displayProgress && curTick < 50) par_progress(nsim*nsub, nsim*nsub, curTick, cores, t0, 0);
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
  int nsub = rx->nsub, nsim = rx->nsim;
  int displayProgress = (op->nDisplayProgress <= nsim*nsub);
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
  int abort = 0;
  uint32_t seed0 = getRxSeed1(cores);
#ifdef _OPENMP
#pragma omp parallel for num_threads(op->cores)
#endif
  for (int solveid = 0; solveid < nsim*nsub; solveid++){
    if (abort == 0){
      setSeedEng1(seed0 + rx->ordId[solveid] - 1);
      ind_liblsoda0(rx, op, opt, solveid, dydt_liblsoda, update_inis);
      if (displayProgress){
#pragma omp critical
        cur++;
#ifdef _OPENMP
        if (omp_get_thread_num() == 0) // only in master thread!
#endif
          {
            curTick = par_progress(cur, nsim*nsub, curTick, cores, t0, 0);
            if (abort == 0){
              if (checkInterrupt()) abort =1;
            }
          }
      }
    }
  }
  setRxSeedFinal(seed0 + nsim*nsub);
  if (abort == 1){
    op->abort = 1;
    /* yp0 = NULL; */
    par_progress(cur, nsim*nsub, curTick, cores, t0, 1);
  } else {
    if (displayProgress && curTick < 50) par_progress(nsim*nsub, nsim*nsub, curTick, cores, t0, 0);
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

unsigned int global_iworki = 0;
int *global_iwork(unsigned int mx){
  if (mx >= global_iworki){
    bool first = (global_iworki == 0);
    global_iworki = mx+1024;
    if (first) {
      global_iworkp = R_Calloc(global_iworki, int);
    } else {
      global_iworkp = R_Realloc(global_iworkp, global_iworki, int);
    }
  }
  return global_iworkp;
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
  if (global_iworki != 0) R_Free(global_iworkp);
  global_iworki = 0;

  if (global_rworki != 0) R_Free(global_rworkp);
  global_rworki = 0;

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
  gitol = 1; gitask = 1; giopt = 1;
  gliw = liw;
  glrw = lrw;

  std::fill(rwork, rwork + lrw + 1, 0.0); // Works because it is a double
  std::fill(iwork, iwork + liw + 1, 0); // Works because it is a integer

  neq[1] = solveid;

  ind = &(rx->subjects[neq[1]]);

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

  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) return;
  ind->solvedIdx = 0;
  for(i=0; i < ind->n_all_times; i++) {
    ind->idx=i;
    ind->linSS=0;
    yp   = getSolve(i);
    xout = getTime_(ind->ix[i], ind);
    if (getEvid(ind, ind->ix[i]) != 3 && !isSameTime(xout, xp)) {
      if (ind->err){
        ind->rc[0] = -1000;
        // Bad Solve => NA
        badSolveExit(i);
      } else {
        if (handleExtraDose(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                            xp, ind->id, &i, ind->n_all_times, &istate, op, ind, u_inis, ctx)) {
          if (!isSameTime(ind->extraDoseNewXout, xp)) {
            preSolve(op, ind, xp, ind->extraDoseNewXout, yp);
            neq[0] = op->neq - op->numLin - op->numLinSens;
            F77_CALL(dlsoda)(dydt_lsoda, neq, yp, &xp, &ind->extraDoseNewXout, &gitol, &(op->RTOL), &(op->ATOL), &gitask,
                             &istate, &giopt, rwork, &lrw, iwork, &liw, jdum, &jt);
            neq[0] = op->neq;
            copyLinCmt(neq, ind, op, yp);
            postSolve(neq, &istate, ind->rc, &i, yp, err_msg_ls, 7, true, ind, op, rx);
          }
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
            neq[0] = op->neq - op->numLin - op->numLinSens;
            F77_CALL(dlsoda)(dydt_lsoda, neq, yp, &ind->extraDoseNewXout, &xout, &gitol, &(op->RTOL), &(op->ATOL), &gitask,
                             &istate, &giopt, rwork, &lrw, iwork, &liw, jdum, &jt);
            neq[0] = op->neq;
            copyLinCmt(neq, ind, op, yp);
            postSolve(neq, &istate, ind->rc, &i, yp, err_msg_ls, 7, true, ind, op, rx);
          }
          xp =  ind->extraDoseNewXout;
        }
        if (!isSameTime(xout, xp)) {
          preSolve(op, ind, xp, xout, yp);
          neq[0] = op->neq - op->numLin - op->numLinSens;
          F77_CALL(dlsoda)(dydt_lsoda, neq, yp,
                           &xp, &xout, &gitol,
                           &(op->RTOL),
                           &(op->ATOL),
                           &gitask,
                           &istate, &giopt, rwork, &lrw, iwork, &liw, jdum, &jt);
          neq[0] = op->neq;
          copyLinCmt(neq, ind, op, yp);
          postSolve(neq, &istate, ind->rc, &i, yp, err_msg_ls, 7, true, ind, op, rx);
        }
        xp = xout;
        //dadt_counter = 0;
      }
    }
    ind->_newind = 2;
    if (!op->badSolve){
      ind->idx = i;
      if (getEvid(ind, ind->ix[i]) == 3) {
        handleEvid3(ind, op, rx, neq, &xp, &xout,  yp, &(istate), u_inis);
      } else if (handleEvid1(&i, rx, neq, yp, &xout)){
        handleSS(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                 xp, ind->id, &i, ind->n_all_times, &istate, op, ind, u_inis, ctx);
        if (ind->wh0 == EVID0_OFF){
          ind->solve[ind->cmt] = op->inits[ind->cmt];
        }
        if (rx->istateReset) istate = 1;
        xp = xout;
      }
      // Copy to next solve so when assigned to
      // yp=ind->solve[neq[0]*i]; it will be the prior values
      updateSolve(ind, op, neq, xout, i, ind->n_all_times);
    }
    ind->solvedIdx = i;
  }
  ind->solveTime += ((double)(clock() - t0))/CLOCKS_PER_SEC;
}

extern "C" void ind_lsoda(rx_solve *rx, int solveid,
                          t_dydt_lsoda_dum dydt_ls, t_update_inis u_inis, t_jdum_lsoda jdum,
                          int cjt) {
  int neq[2];
  neq[0] = op_global.neq;
  neq[1] = 0;

  // Set jt to 1 if full is specified.
  int lrw=22+neq[0]*max(16, neq[0]+9), liw=20+neq[0];
  double *rwork;
  int *iwork;
  if (global_debug)
    RSprintf("JT: %d\n",cjt);
  rwork = global_rwork(lrw+1);
  iwork = global_iwork(liw+1);
  ind_lsoda0(rx, &op_global, solveid, neq, rwork, lrw, iwork, liw, cjt,
             dydt_ls, u_inis, jdum);
}

extern "C" void par_lsoda(rx_solve *rx) {
  int nsub = rx->nsub, nsim = rx->nsim;
  int displayProgress = (op_global.nDisplayProgress <= nsim*nsub);
  clock_t t0 = clock();
  int neq[2];
  neq[0] = op_global.neq;
  neq[1] = 0;
  /* yp = global_yp(neq[0]); */

  // Set jt to 1 if full is specified.
  int lrw=22+neq[0]*max(16, neq[0]+9), liw=20+neq[0], jt = global_jt;
  double *rwork;
  int *iwork;


  if (global_debug)
    RSprintf("JT: %d\n",jt);
  rwork = global_rwork(lrw+1);
  iwork = global_iwork(liw+1);

  int curTick = 0;
  int abort = 0;
  uint32_t seed0 = getRxSeed1(1);
  for (int solveid = 0; solveid < nsim*nsub; solveid++){
    if (abort == 0){
      setSeedEng1(seed0 + solveid - 1 );
      ind_lsoda0(rx, &op_global, solveid, neq, rwork, lrw, iwork, liw, jt,
                 dydt_lsoda_dum, update_inis, jdum_lsoda);
      if (displayProgress){ // Can only abort if it is long enough to display progress.
        curTick = par_progress(solveid, nsim*nsub, curTick, 1, t0, 0);
        if (checkInterrupt()){
          abort =1;
          break;
        }
      }
    }
  }
  setRxSeedFinal(seed0 + nsim*nsub);
  if (abort == 1){
    op_global.abort = 1;
  } else {
    if (displayProgress && curTick < 50) par_progress(nsim*nsub, nsim*nsub, curTick, 1, t0, 0);
  }
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
  neq[0] = op->neq;
  neq[1] = rx->ordId[solveid]-1;
  ind = &(rx->subjects[neq[1]]);

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
    yp = getSolve(i);
    xout = getTime_(ind->ix[i], ind);
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
  neq[0] = op->neq;
  neq[1] = rx->ordId[solveid]-1;
  ind = &(rx->subjects[neq[1]]);

  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) return;

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
    yp = getSolve(i);
    xout = getTime_(ind->ix[i], ind);
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
      updateSolve(ind, op, neq, xout, i, nx);
      ind->slvr_counter[0]++; // doesn't need do be critical; one subject at a time.
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

extern "C" void ind_dop0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq,
                         t_dydt c_dydt,
                         t_update_inis u_inis) {
  clock_t t0 = clock();
  double rtol=op->RTOL, atol=op->ATOL;
  int itol=0;           //0: rtol/atol scalars; 1: rtol/atol vectors
  int iout=0;           //iout=0: solout() NEVER called
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
  int *BadDose;
  double *InfusionRate;
  double *inits;
  int *rc;
  int nx;
  neq[1] = solveid;
  ind = &(rx->subjects[neq[1]]);
  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) return;
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
    yp = getSolve(i);
    xout = getTime_(ind->ix[i], ind);
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
        if (handleExtraDose(neq, BadDose, InfusionRate, ind->dose, yp, xout,
                            xp, ind->id, &i, nx, &istate, op, ind, u_inis, ctx)) {
          if (!isSameTimeDop(ind->extraDoseNewXout, xp)) {
            preSolve(op, ind, xp, ind->extraDoseNewXout, yp);
            neq[0] = op->neq - op->numLin - op->numLinSens;
            idid = dop853(neq,       /* dimension of the system <= UINT_MAX-1*/
                          c_dydt,       /* function computing the value of f(x,y) */
                          xp,           /* initial x-value */
                          yp,           /* initial values for y */
                          ind->extraDoseNewXout, /* final x-value (xend-x may be positive or negative) */
                          &rtol,          /* relative error tolerance */
                          &atol,          /* absolute error tolerance */
                          itol,         /* switch for rtoler and atoler */
                          solout,         /* function providing the numerical solution during integration */
                          iout,         /* switch for calling solout */
                          NULL,           /* messages stream */
                          DBL_EPSILON,    /* rounding unit */
                          0,              /* safety factor */
                          0,              /* parameters for step size selection */
                          0,
                          0,              /* for stabilized step size control */
                          ind->HMAX,              /* maximal step size */
                          op->H0,            /* initial step size */
                          op->mxstep, /* maximal number of allowed steps */
                          1,            /* switch for the choice of the coefficients */
                          -1,                     /* test for stiffness */
                          0,                      /* number of components for which dense outpout is required */
                          NULL,           /* indexes of components for which dense output is required, >= nrdens */
                          0                       /* declared length of icon */
                          );
            neq[0] = op->neq;
            copyLinCmt(neq, ind, op, yp);
            postSolve(neq, &idid, rc, &i, yp, err_msg, 4, true, ind, op, rx);
            xp = ind->extraDoseNewXout;
          }
          int idx = ind->idx;
          int ixds = ind->ixds;
          int trueIdx = ind->extraDoseTimeIdx[ind->idxExtra];
          ind->idx = -1-trueIdx;
          handle_evid(ind->extraDoseEvid[trueIdx], neq[0],
                      BadDose, InfusionRate, ind->dose, yp, xout, neq[1], ind);
          idid = 1;
          ind->idx = idx;
          ind->ixds = ixds;
          ind->idxExtra++;
          if (!isSameTimeDop(xout, ind->extraDoseNewXout)) {
            preSolve(op, ind, ind->extraDoseNewXout, xout, yp);
            neq[0] = op->neq - op->numLin - op->numLinSens;
            idid = dop853(neq,       /* dimension of the system <= UINT_MAX-1*/
                          c_dydt,       /* function computing the value of f(x,y) */
                          ind->extraDoseNewXout,           /* initial x-value */
                          yp,           /* initial values for y */
                          xout, /* final x-value (xend-x may be positive or negative) */
                          &rtol,          /* relative error tolerance */
                          &atol,          /* absolute error tolerance */
                          itol,         /* switch for rtoler and atoler */
                          solout,         /* function providing the numerical solution during integration */
                          iout,         /* switch for calling solout */
                          NULL,           /* messages stream */
                          DBL_EPSILON,    /* rounding unit */
                          0,              /* safety factor */
                          0,              /* parameters for step size selection */
                          0,
                          0,              /* for stabilized step size control */
                          ind->HMAX,              /* maximal step size */
                          op->H0,            /* initial step size */
                          op->mxstep, /* maximal number of allowed steps */
                          1,            /* switch for the choice of the coefficients */
                          -1,                     /* test for stiffness */
                          0,                      /* number of components for which dense outpout is required */
                          NULL,           /* indexes of components for which dense output is required, >= nrdens */
                          0                       /* declared length of icon */
                          );
            neq[0] = op->neq;
            copyLinCmt(neq, ind, op, yp);
            postSolve(neq, &idid, rc, &i, yp, err_msg, 4, true, ind, op, rx);
            xp = ind->extraDoseNewXout;
          }
        }
        if (!isSameTimeDop(xout, xp)) {
          preSolve(op, ind, xp, xout, yp);
          neq[0] = op->neq - op->numLin - op->numLinSens;
          idid = dop853(neq,       /* dimension of the system <= UINT_MAX-1*/
                        c_dydt,       /* function computing the value of f(x,y) */
                        xp,           /* initial x-value */
                        yp,           /* initial values for y */
                        xout,         /* final x-value (xend-x may be positive or negative) */
                        &rtol,          /* relative error tolerance */
                        &atol,          /* absolute error tolerance */
                        itol,         /* switch for rtoler and atoler */
                        solout,         /* function providing the numerical solution during integration */
                        iout,         /* switch for calling solout */
                        NULL,           /* messages stream */
                        DBL_EPSILON,    /* rounding unit */
                        0,              /* safety factor */
                        0,              /* parameters for step size selection */
                        0,
                        0,              /* for stabilized step size control */
                        ind->HMAX,              /* maximal step size */
                        op->H0,            /* initial step size */
                        op->mxstep, /* maximal number of allowed steps */
                        1,            /* switch for the choice of the coefficients */
                        -1,                     /* test for stiffness */
                        0,                      /* number of components for which dense outpout is required */
                        NULL,           /* indexes of components for which dense output is required, >= nrdens */
                        0                       /* declared length of icon */
                        );
          neq[0] = op->neq;
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
        handleSS(neq, BadDose, InfusionRate, ind->dose, yp, xout,
                 xp, ind->id, &i, nx, &istate, op, ind, u_inis, ctx);
        if (ind->wh0 == EVID0_OFF){
          yp[ind->cmt] = inits[ind->cmt];
        }
        xp = xout;
      }
      /* for(j=0; j<neq[0]; j++) ret[neq[0]*i+j] = yp[j]; */
      updateSolve(ind, op, neq, xout, i, nx);
    }
    ind->solvedIdx = i;
  }
  ind->solveTime += ((double)(clock() - t0))/CLOCKS_PER_SEC;
}

extern "C" void ind_dop(rx_solve *rx, int solveid,
                        t_dydt c_dydt, t_update_inis u_inis){
  rx_solving_options *op = &op_global;
  int neq[2];
  neq[0] = op->neq;
  neq[1] = 0;
  ind_dop0(rx, &op_global, solveid, neq, c_dydt, u_inis);
}

void par_dop(rx_solve *rx){
  rx_solving_options *op = &op_global;
  int nsub = rx->nsub, nsim = rx->nsim;
  int displayProgress = (op->nDisplayProgress <= nsim*nsub);
  clock_t t0 = clock();
  int neq[2];
  neq[0] = op->neq;
  neq[1] = 0;

  //DE solver config vars
  // This part CAN be parallelized, if dop is thread safe...
  // Therefore you could use https://github.com/jacobwilliams/dop853, but I haven't yet

  int curTick = 0;
  int abort = 0;
  uint32_t seed0 = getRxSeed1(1);
  for (int solveid = 0; solveid < nsim*nsub; solveid++){
    if (abort == 0){
      setSeedEng1(seed0 + solveid - 1 );
      ind_dop0(rx, &op_global, solveid, neq, dydt, update_inis);
      if (displayProgress && abort == 0){
        if (checkInterrupt()) abort =1;
      }
      if (displayProgress) curTick = par_progress(solveid, nsim*nsub, curTick, 1, t0, 0);
    }
  }
  setRxSeedFinal(seed0 + nsim*nsub);
  if (abort == 1){
    op->abort = 1;
  } else {
    if (displayProgress && curTick < 50) par_progress(nsim*nsub, nsim*nsub, curTick, 1, t0, 0);
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
  par_progress_1=0;
  _isRstudio = isRstudio();
  setRstudioPrint(_isRstudio);
  rxt.t0 = clock();
  rxt.cores = 1;
  rxt.n = 100;
  rxt.d = 0;
  rxt.cur = 0;
  assignFuns();
  rx_solving_options *op = &op_global;
  if (op->neq !=  0) {
    if (rx->linB == 1) {
      // Setup H
      setupLinH(rx, cid, c_dydt, u_inis);
    }
    if (op->neq == op->numLinSens + op->numLin) {
      // This only is linear compartment solving
      ind_linCmt(rx, cid, c_dydt, u_inis);
      return;
    } else {
      switch (op->stiff){
      case 3:
        ind_indLin(rx, cid, u_inis, ME, IndF);
        break;
      case 2:
        ind_liblsoda(rx, cid, dydt_lls, u_inis);
        break;
      case 1:
        ind_lsoda(rx,cid, dydt_lsoda, u_inis, jdum, jt);
        break;
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
  rx_solving_options *op = &op_global;
  if (op->neq != 0) {
    if (rx->linB == 1) {
      // Setup H
    }
    if (op->neq == op->numLinSens + op->numLin) {
      // This only is linear compartment solving
      par_linCmt(rx);
      return;
    } else {
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
      case 0:
        // dop
        par_dop(rx);
        break;
      }
    }
  }
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
    Rf_errorcall(R_NilValue, "Trying to access an equation that isn't calculated. lhs(%d/%d); id: %s\n",i, op->nlhs, getId(id));
  }
  return 0;
}
