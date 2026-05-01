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
#include "../inst/include/rxode2dataErr.h"
#include "../inst/include/rxode2parseHandleEvid.h"
#include "../inst/include/rxode2parseGetTime.h"
#include "../inst/include/rxode2EventTranslate.h"
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
extern int nPastEvid_global;

rx_solving_options_ind *inds_global = NULL;

rx_solving_options_ind *inds_thread = NULL;

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
  rxode2_assignFuns2_t assignFuns2 = (rxode2_assignFuns2_t)R_GetCCallable(lib, s_assignFuns2);
  if (assignFuns2 != NULL) {
    assignFuns2(*rx, *op, AMT, LAG, RATE, DUR, calc_mtime, ME, IndF, getTime, _locateTimeIndex, handle_evidL, _getDur);
  } else {
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
         // then dose is applied — not the other way around.
         if (ea == 3 && eb != 3) return true;
         if (eb == 3 && ea != 3) return false;
         // Otherwise: higher evid (doses) before lower evid (obs) — matches etTrans()
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

// Push a future event into the individual's own event arrays during ODE solving.
// _curTime: current ODE model time (for past-time guard).
// Returns 1 on success, 0 if ignored (past time or unknown evid), -1 on alloc failure.
extern "C" int _rxPushDose(rx_solving_options_ind *_ind, double _curTime,
                            double _time, int _evid, double _amt, int _cmt,
                            double _rate, double _ii, int _addl, int _ss) {
  rx_solving_options *op = &op_global;

  if (!_ind->indOwnAlloc) return 0; // safety: only works with owned arrays

  rx_solve *rx = &rx_global;

  // Loop over addl+1 doses: dose 0 uses the given ss, subsequent doses use ss=0
  int nDosesToPush = (_addl > 0 && _ii > 0) ? _addl + 1 : 1;
  int anyPushed = 0;
  for (int _rep = 0; _rep < nDosesToPush; _rep++) {
    double _doseTime = _time + _rep * _ii;
    int    _doseSs   = (_rep == 0) ? _ss : 0;

    if (_doseTime <= _curTime) {
#pragma omp atomic
      nPastEvid_global++;
      continue;
    }

    // Each addl repetition is a standalone event; ii=0 so the solver does not
    // auto-schedule further repeats.  For the first dose (rep==0) with SS, we
    // pass the original _ii so flg is set correctly; for all others ii=0.
    double _doseIi = (_rep == 0 && _doseSs != 0) ? _ii : 0.0;
    rx_translated_event ev = _rxTranslateOneEvent(_doseTime, _evid, _cmt,
                                                   _amt, _doseIi, _doseSs, _rate);
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
  calc_mtime(ind->id, newMtime, yp);
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
extern "C" void solout(long int nr, double t_old, double t, double *y, int *nptr, int *irtrn){}

extern "C" int indLin(int cSub, rx_solving_options *op, rx_solving_options_ind *ind,
                      double tp, double *yp_, double tf,
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
        idid = indLin(ind->id, op, ind, xp, yp, xout, ind->InfusionRate, ind->on,
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
                    &canBreak,
                    adjustEvidBolusLag);
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

// Grow ind->solve if slot i (and optionally i+1) are beyond the current
// allocated capacity.  Safe to call between ODE steps — no integrator holds
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
    std::copy(getSolve(i), getSolve(i) + op->neq, getSolve(i+1));
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
  double xout, xoutp;
  int *rc;
  double *yp;
  int idid = 0;
  int localBadSolve = 0;
  ind = &(rx->subjects[neq[1]]);
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
                      ME, IndF);
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
                           int solveid, t_update_inis u_inis, t_ME ME, t_IndF IndF){
  assignFuns();
  rx_solving_options *op = &op_global;
  ind_indLin0(rx, op, solveid, u_inis, ME, IndF);
}


extern "C" void par_indLin(rx_solve *rx){
  assignFuns();
  rx_solving_options *op = &op_global;
  int cores = 1;
  uint32_t nsub = rx->nsub, nsim = rx->nsim;
  int nsolve = (int)(nsim*nsub); // safe: overflow guard ensures nsim*nsub <= INT_MAX
  int displayProgress = (op->nDisplayProgress <= nsolve);
  clock_t t0 = clock();
  /* double *yp0=(double*) malloc((op->neq)*nsim*nsub*sizeof(double)); */
  int curTick=0;
  int cur=0;
  // Breaking of of loop ideas came from http://www.thinkingparallel.com/2007/06/29/breaking-out-of-loops-in-openmp/
  // http://permalink.gmane.org/gmane.comp.lang.r.devel/27627
  // It was buggy due to Rprint.  Use REprint instead since Rprint calls the interrupt every so often....
  // volatile ensures reads/writes are not cached in registers across threads
  volatile int abort = 0;
  // FIXME parallel
  uint32_t seed0 = getRxSeed1(1);
  for (int solveid = 0; solveid < nsolve; solveid++){
    if (abort == 0){
      setSeedEng1(seed0 + solveid - 1 );
      ind_indLin(rx, solveid, update_inis, ME, IndF);
      if (displayProgress){ // Can only abort if it is long enough to display progress.
        curTick = par_progress(solveid, nsolve, curTick, 1, t0, 0);
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
  rx_solving_options_ind *ind;
  double xout;
  int *rc;
  double *yp;
  int neqOde = *neq - op->numLin - op->numLinSens;
  int localBadSolve = 0;
  struct lsoda_context_t * ctx = lsoda_create_ctx();
  if (ctx == NULL) {
    rxSolveFreeC();
    (Rf_error)(_("not enough memory for lsoda context"));
  }
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
  // x = ind->all_times;
  rc= ind->rc;
  double xp = ind->all_times[0];
  // Use per-individual tolerance arrays if available (assigned by
  // _setIndPointersByThread and scaled by iniSubject).
  if (ind->rtol2 != NULL && ind->atol2 != NULL) {
    opt.rtol = ind->rtol2;
    opt.atol = ind->atol2;
  }
  lsoda_prepare(ctx, &opt);
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
            double _evid_tmpydot[neqOde];
            (*ctx->function)(xp, yp, _evid_tmpydot, ctx->data);
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
        handleSS(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                 xp, ind->id, &i, ind->n_all_times, &(ctx->state), op, ind, u_inis, ctx);
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
#pragma omp parallel for num_threads(op->cores)
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
            neq[0] = op->neq - op->numLin - op->numLinSens;
            F77_CALL(dlsoda)(dydt_lsoda, neq, yp, &xp, &ind->extraDoseNewXout, &gitol, &(op->RTOL), &(op->ATOL), &gitask,
                             &istate, &giopt, rwork, &lrw, iwork, &liw, jdum, &jt);
            neq[0] = op->neq;
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
              neq[0] = op->neq - op->numLin - op->numLinSens;
              F77_CALL(dlsoda)(dydt_lsoda, neq, yp, &ind->extraDoseNewXout, &xout, &gitol, &(op->RTOL), &(op->ATOL), &gitask,
                               &istate, &giopt, rwork, &lrw, iwork, &liw, jdum, &jt);
              neq[0] = op->neq;
              copyLinCmt(neq, ind, op, yp);
              postSolve(neq, &istate, ind->rc, &i, yp, err_msg_ls, 7, true, ind, op, rx);
              if (*(ind->rc) < 0) localBadSolve = 1;
            }
            xp =  ind->extraDoseNewXout;
          }
        }
        if (!localBadSolve && !isSameTime(xout, xp)) {
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
          ind->solve[ind->cmt] = op->inits[ind->cmt];
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
  uint32_t nsub = rx->nsub, nsim = rx->nsim;
  int nsolve = (int)(nsim*nsub); // safe: overflow guard ensures nsim*nsub <= INT_MAX
  int displayProgress = (op_global.nDisplayProgress <= nsolve);
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
  for (int solveid = 0; solveid < nsolve; solveid++){
    if (abort == 0){
      setSeedEng1(seed0 + solveid - 1 );
      ind_lsoda0(rx, &op_global, solveid, neq, rwork, lrw, iwork, liw, jt,
                 dydt_lsoda_dum, update_inis, jdum_lsoda);
      if (displayProgress){ // Can only abort if it is long enough to display progress.
        curTick = par_progress(solveid, nsolve, curTick, 1, t0, 0);
        if (checkInterrupt()){
          abort =1;
          break;
        }
      }
    }
  }
  setRxSeedFinal(seed0 + (uint32_t)nsolve);
  if (abort == 1){
    op_global.abort = 1;
  } else {
    if (displayProgress && curTick < 50) par_progress(nsolve, nsolve, curTick, 1, t0, 0);
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
      // For linCmt-only models with evid_() (indOwnAlloc), set _atEventTime=1
      // so that calc_lhs (called inside updateSolve) fires any evid_() calls.
      // This replicates the ODE behaviour where preSolve sets _atEventTime=1
      // before dydt, which fires evid_() at the first sub-step.
      if (op->indOwnAlloc) ind->_atEventTime = 1;
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

extern "C" void ind_dop0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq,
                         t_dydt c_dydt,
                         t_update_inis u_inis) {
  clock_t t0 = clock();
  int itol=1;           //1: rtol/atol are vectors (per-compartment), matching liblsoda
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
  double *InfusionRate;
  double *inits;
  int *rc;
  int nx;
  neq[1] = solveid;
  ind = &(rx->subjects[neq[1]]);
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
            neq[0] = op->neq - op->numLin - op->numLinSens;
            idid = dop853(neq,       /* dimension of the system <= UINT_MAX-1*/
                          c_dydt,       /* function computing the value of f(x,y) */
                          xp,           /* initial x-value */
                          yp,           /* initial values for y */
                          ind->extraDoseNewXout, /* final x-value (xend-x may be positive or negative) */
                          op->rtol2,      /* relative error tolerance (per-compartment vector) */
                          op->atol2,      /* absolute error tolerance (per-compartment vector) */
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
                      ind->BadDose, InfusionRate, ind->dose, yp, xout, neq[1], ind);
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
                          op->rtol2,      /* relative error tolerance (per-compartment vector) */
                          op->atol2,      /* absolute error tolerance (per-compartment vector) */
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
                        op->rtol2,      /* relative error tolerance (per-compartment vector) */
                        op->atol2,      /* absolute error tolerance (per-compartment vector) */
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
      ind_dop0(rx, &op_global, solveid, neq, dydt, update_inis);
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
    (Rf_errorcall)(R_NilValue, "Trying to access an equation that isn't calculated. lhs(%d/%d); id: %s\n",i, op->nlhs, getId(id));
  }
  return 0;
}
