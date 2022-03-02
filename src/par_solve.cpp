#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "rxomp.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <R.h>
#include <string>
#include <Rinternals.h>
#include <Rmath.h> //Rmath includes math.
#include <R_ext/Rdynload.h>
#include "../inst/include/rxode2.h"
#include "strncmp.h"
#include "handle_evid.h"
#include "getTime.h"
extern "C" {
#include "dop853.h"
#include "common.h"
#include "lsoda.h"
}
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )
#define badSolveExit(i) for (int j = op->neq*(ind->n_all_times); j--;){ \
    ind->solve[j] = NA_REAL;                                            \
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

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif
int _isRstudio = 0;

#include "rxData.h"

extern "C" void setRstudioPrint(int rstudio);
extern "C" void RSprintf(const char *format, ...);

extern "C" SEXP _rxHasOpenMp(){
  SEXP ret = PROTECT(allocVector(LGLSXP,1));
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

void printErr(int err, int id){
  RSprintf("Recovered solving errors for internal ID %s (%d):\n", getId(id), err);
  if (err & 1){
    RSprintf("  Corrupted event table during sort (1)\n");
  }
  if (err & 2){
    RSprintf("  Rate is zero/negative\n");
  }
  if (err & 4){
    RSprintf("  Modeled rate requested in event table, but not in model; use 'rate(cmt) ='\n");
  }
  if (err & 4){
    RSprintf("  Modeled rate requested in event table, but not in model; use 'rate(cmt) ='\n");
  }
  if (err & 8){
    RSprintf("  Corrupted event table during sort (2)\n");
  }
  if (err & 16){
    RSprintf("  Duration is zero/negative\n");
  }
  if (err & 32){
    RSprintf("  Modeled duration requested in event table, but not in model; use 'dur(cmt) ='\n");
  }
  if (err & 64){
    RSprintf("  Data error 686\n");
  }
  if (err & 128){
    RSprintf("  Data Error -6\n");
  }
  if (err & 256){
    RSprintf("  Data Error 8\n");
  }
  if (err & 512){
    RSprintf("  Data error 886\n");
  }
  if (err & 1024){
    RSprintf("  Data error 797\n");
  }
  if (err & 2048){
    RSprintf("  Data Error -7\n");
  }
  if (err & 4096){
    RSprintf("  Data Error 9\n");
  }
  if (err & 8192){
    RSprintf("  Data error 997\n");
  }
  if (err & 16384){
    RSprintf("  Corrupted event table during sort (1)\n");
  }
  if (err & 32768){
    RSprintf("  Corrupted event table\n");
  }
  if (err & 131072){
    RSprintf("  Corrupted events\n");
  }
  if (err & 65536){
    RSprintf("  Supplied an invalid EVID\n");
  }
  if (err & 262144){
    RSprintf("  Corrupted event table\n");
  }
  if (err & 524288){
    RSprintf("  The event table has been corrupted\n");
  }
  if (err & 1048576){
    RSprintf("  SS=2 & Modeled F does not work\n");
  }
  if (err & 2097152){
    RSprintf("  SS=2 & Modeled F does not work\n");
  }
  if (err & 4194304){
    RSprintf("  SS=2 & Modeled F does not work\n");
  }
  if (err & 8388608){
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
  SEXP ret = PROTECT(allocVector(INTSXP, 1));
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
    Rf_errorcall(R_NilValue, CHAR(STRING_ELT(str,0)));
  }
  return R_NilValue;
}

t_set_solve set_solve = NULL;

extern "C" void rxOptionsIniEnsure(int mx){
  R_Free(inds_global);
  inds_global = R_Calloc(mx, rx_solving_options_ind);
  rx_solve *rx=(&rx_global);
  rx->subjects = inds_global;
  rx->keys = NULL;
  rx->nradix=NULL;
  rx->TMP=NULL;
  rx->UGRP=NULL;
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

extern "C" t_calc_lhs getRxLhs() {
  return calc_lhs;
}

extern "C" t_update_inis getUpdateInis() {
  return update_inis;
}

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


static inline void postSolve(int *idid, int *rc, int *i, double *yp, const char** err_msg, int nerr, bool doPrint,
                             rx_solving_options_ind *ind, rx_solving_options *op, rx_solve *rx) {
  if (*idid <= 0) {
    if (err_msg != NULL) {
      int cid = -*idid-1;
      if (cid > 0 && cid < nerr) RSprintf("IDID=%d, %s\n", *idid, err_msg[-*idid-1]);
      else RSprintf("IDID=%d, unhandled exception\n", *idid);
    }
    *rc = *idid;
    badSolveExit(*i);
  } else if (ind->err){
    if (doPrint) printErr(ind->err, ind->id);
    /* RSprintf("IDID=%d, %s\n", istate, err_msg_ls[-*istate-1]); */
    *rc = -2019;
    // Bad Solve => NA
    badSolveExit(*i);
  } else {
    if (R_FINITE(rx->stateTrimU)){
      double top=fabs(rx->stateTrimU);
      for (int j = op->neq; j--;) yp[j]= min(top,yp[j]);
    }
    if (R_FINITE(rx->stateTrimL)){
      double bottom=rx->stateTrimL;
      for (int j = op->neq; j--;) yp[j]= max(bottom,yp[j]);
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

extern "C" void radix_r(const int from, const int to, const int radix,
                        rx_solving_options_ind *ind, rx_solve *rx);
extern "C" void calcNradix(int *nbyte, int *nradix, int *spare, uint64_t *maxD, uint64_t *minD);

extern "C" uint64_t dtwiddle(const void *p, int i);
// Adapted from
// https://github.com/Rdatatable/data.table/blob/588e0725320eacc5d8fc296ee9da4967cee198af/src/forder.c#L630-L649
extern "C" void sortRadix(rx_solving_options_ind *ind){
#ifdef _OPENMP
  int core = omp_get_thread_num();
#else
  int core = 0;
#endif
  rx_solve *rx = &rx_global;
  rx_solving_options *op = &op_global;
  uint8_t **key = rx->keys[core];
  // Reset times for infusion
  int wh, cmt, wh100, whI, wh0;
  int doSort = 1;
  double *time = new double[ind->n_all_times];
  uint64_t *all = new uint64_t[ind->n_all_times];
  uint64_t minD, maxD;
  ind->ixds = 0;
  ind->curShift = 0;
  for (int i = 0; i < ind->n_all_times; i++) {
    ind->ix[i] = i;
    ind->idx = i;
    if (!isObs(ind->evid[i])) {
      time[i] = getTime__(ind->ix[i], ind, 1);
      ind->ixds++;
    } else {
      if (ind->evid[i] == 3) {
        ind->curShift -= rx->maxShift;
      }
      time[i] = getTime__(ind->ix[i], ind, 1);
    }
    all[i]  = dtwiddle(time, i);
    if (i == 0){
      minD = maxD = all[0];
    } else if (all[i] < minD){
      minD = all[i];
    } else if (all[i] > maxD) {
      maxD = all[i];
    }
    if (op->naTime == 1){
      doSort=0;
      break;
    }
  }
  if (doSort){
    int nradix=0, nbyte=0, spare=0;
    calcNradix(&nbyte, &nradix, &spare, &maxD, &minD);
    rx->nradix[core] = nradix;
    // Allocate more space if needed
    for (int b = 0; b < nbyte; b++){
      if (key[b] == NULL) {
        key[b] = (uint8_t *)calloc(rx->maxAllTimes+1, sizeof(uint8_t));
      }
    }
    for (int i = 0; i < ind->n_all_times; i++) {
      uint64_t elem = all[i] - minD;
      elem <<= spare;
      for (int b= nbyte-1; b>0; b--) {
        key[b][i] = (uint8_t)(elem & 0xff);
        elem >>= 8;
      }
      // rxode2 uses key[0][i] = 0 | (uint8_t)(elem & 0xff) instead of
      //  key[0][i] |= (uint8_t)(elem & 0xff)
      // because unlike data.table, key[0][i] is not necessarily zero.
      key[0][i] = 0 | (uint8_t)(elem & 0xff);
    }
    radix_r(0, ind->n_all_times-1, 0, ind, rx);
  }
  delete[] time;
  delete[] all;
}


static inline int iniSubject(int solveid, int inLhs, rx_solving_options_ind *ind, rx_solving_options *op, rx_solve *rx,
                             t_update_inis u_inis) {
  ind->ixds = ind->idx = ind->_update_par_ptr_in = 0; // reset dosing
  ind->id=solveid;
  ind->cacheME=0;
  ind->curShift=0.0;
  // neq[0] = op->neq
  for (int j = (op->neq + op->extraCmt); j--;) {
    ind->InfusionRate[j] = 0;
    ind->on[j] = 1;
    ind->tlastS[j] = NA_REAL;
    ind->tfirstS[j] = NA_REAL;
    ind->curDoseS[j] = NA_REAL;
  }
  ind->inLhs = inLhs;
  if (rx->nMtime) calc_mtime(solveid, ind->mtime);
  for (int j = op->nlhs; j--;) ind->lhs[j] = NA_REAL;
  if ((inLhs == 0 && op->neq > 0) ||
      (inLhs == 1 && op->neq == 0 && (rx->nIndSim > 0 || (rx->simflg & 1) != 0 ))) {
    ind->isIni = 1;
    // Also can update individual random variables (if needed)
    if (inLhs == 0) memcpy(ind->solve, op->inits, op->neq*sizeof(double));
    u_inis(solveid, ind->solve); // Update initial conditions @ current time
    ind->isIni = 0;
  }
  ind->_newind = 1;
  ind->dosenum = 0;
  ind->tlast = NA_REAL;
  ind->tfirst = NA_REAL;
  ind->curDose = NA_REAL;
  if (inLhs == 0 || (inLhs == 1 && op->neq==0)) {
    ind->solved = -1;
    if (rx->needSort){
      sortRadix(ind);
      if (op->badSolve) return 0;
    }
  }
  ind->ixds=ind->idx=0;
  return 1;
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

void solveSS_1(int *neq,
               int *BadDose,
               double *InfusionRate,
               double *dose,
               double *yp,
               int do_transit_abs,
               double xout, double xp, int id,
               int *i, int nx,
               int *istate,
               rx_solving_options *op,
               rx_solving_options_ind *ind,
               t_update_inis u_inis,
               void *ctx){
  int idid;
  int itol=0;
  switch(op->stiff){
  case 3:
    idid = indLin(ind->id, op, xp, yp, xout, ind->InfusionRate, ind->on,
                  ME, IndF);
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
    lsoda((lsoda_context_t*)ctx, yp, &xp, xout);
    if (*istate <= 0) {
      RSprintf("IDID=%d, %s\n", *istate, err_msg_ls[-(*istate)-1]);
      /* ind->rc[0] = *istate; */
      ind->rc[0] = -2019;
      // Bad Solve => NA
      /* for (j=neq[0]*(ind->n_all_times); j--;) ind->solve[j] = NA_REAL; */
      /* op->badSolve = 1; */
      /* *i = ind->n_all_times-1; */ // Get out of here!
      /* j=op->maxSS; */
      break;
    } else if (ind->err){
      printErr(ind->err, ind->id);
      ind->rc[0] = -2019;
      /* for (j=neq[0]*(ind->n_all_times); j--;) ind->solve[j] = NA_REAL; */
      /* op->badSolve = 1; */
      *i = ind->n_all_times-1; // Get out of here!
      /* j=op->maxSS; */
      break;
    }
    break;
  case 1:
    F77_CALL(dlsoda)(dydt_lsoda_dum, neq, yp, &xp, &xout,
                     &gitol, &(op->RTOL), &(op->ATOL), &gitask,
                     istate, &giopt, global_rworkp,
                     &glrw, global_iworkp, &gliw, jdum_lsoda, &global_jt);
    if (*istate <= 0) {
      RSprintf("IDID=%d, %s\n", *istate, err_msg_ls[-(*istate)-1]);
      ind->rc[0] = -2019;/* *istate; */
      // Bad Solve => NA
      /* for (j=neq[0]*(ind->n_all_times); j--;) ind->solve[j] = NA_REAL; */
      /* op->badSolve = 1; */
      /* *i = ind->n_all_times-1; // Get out of here! */
      /* j=op->maxSS; */
      break;
    } else if (ind->err){
      printErr(ind->err, ind->id);
      ind->rc[0] = -2019;
      /* for (j=neq[0]*(ind->n_all_times); j--;) ind->solve[j] = NA_REAL; */
      /* op->badSolve = 1; */
      /* *i = ind->n_all_times-1; // Get out of here! */
      /* j=op->maxSS; */
      break;
    }
    break;
  case 0:
    idid = dop853(neq,       /* dimension of the system <= UINT_MAX-1*/
                  dydt,       /* function computing the value of f(x,y) */
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
                  0,              /* maximal step size */
                  0,            /* initial step size */
                  op->mxstep,            /* maximal number of allowed steps */
                  1,            /* switch for the choice of the coefficients */
                  -1,                     /* test for stiffness */
                  0,                      /* number of components for which dense outpout is required */
                  NULL,           /* indexes of components for which dense output is required, >= nrdens */
                  0                       /* declared length of icon */
                  );
    if (idid < 0) {
      ind->rc[0] = -2019;
      // Bad Solve => NA
      /* for (j=neq[0]*(ind->n_all_times); j--;) ind->solve[j] = NA_REAL; */
      /* op->badSolve = 1; */
      /* *i = ind->n_all_times-1; // Get out of here! */
      /* j=op->maxSS; */
      break;
    } else if (ind->err){
      printErr(ind->err, ind->id);
      /* ind->rc[0] = -1000; */
      /* for (j=neq[0]*(ind->n_all_times); j--;) ind->solve[j] = NA_REAL; */
      /* op->badSolve = 1; */
      *i = ind->n_all_times-1; // Get out of here!
      /* j=op->maxSS; */
      break;
    }
    break;
  }
}

void handleSS(int *neq,
              int *BadDose,
              double *InfusionRate,
              double *dose,
              double *yp,
              int do_transit_abs,
              double xout, double xp, int id,
              int *i, int nx,
              int *istate,
              rx_solving_options *op,
              rx_solving_options_ind *ind,
              t_update_inis u_inis,
              void *ctx){
  rx_solve *rx = &rx_global;
  int j;
  int doSS2=0;
  int doSSinf=0;
  /* Rprintf("evid: %d\n", ind->evid[ind->ixds-1]); */
  if (((ind->wh0 == EVID0_SS2 || ind->wh0 == EVID0_SS) &&
       getIiNumber(ind, ind->ixds-1) > 0) || ind->wh0 == EVID0_SSINF){
    ind->doSS=1;
    ind->ixds--; // This dose stays in place; Reverse dose
    if (ind->wh0 == EVID0_SS2){
      doSS2=1;
    } else if (ind->wh0 == EVID0_SSINF){
      doSSinf=1;
    }
    double dur = 0, dur2=0;
    int infBixds =0, infEixds = 0, ei=0, wh, cmt, wh100, whI, wh0, oldI;
    if (doSSinf){
    } else if (ind->whI == EVIDF_INF_RATE || ind->whI == EVIDF_INF_DUR) {
      oldI = ind->whI;
      infBixds = ind->ixds;
      // Find the next fixed length infusion that is turned off.
      for (j = ind->ixds+1; j < ind->ndoses; j++){
        if (getDoseNumber(ind, j) == -getDoseNumber(ind, ind->ixds)){
          getWh(ind->evid[ind->idose[j]], &wh, &cmt, &wh100, &whI, &wh0);
          if (whI == oldI && cmt == ind->cmt){
            dur = getTime_(ind->idose[j], ind) -
              getTime_(ind->ix[*i], ind);
            dur2 = getIiNumber(ind, ind->ixds) - dur;
            /* Rprintf("000; dur: %f; dur2: %f; ii: %f;\n", dur, dur2, getIiNumber(ind, ind->ixds)); */
            infEixds = j;
            break;
          }
        }
      }
    } else if (ind->whI == EVIDF_MODEL_DUR_ON || ind->whI == EVIDF_MODEL_RATE_ON) {
      // These are right next to another.
      infBixds = ind->ixds;
      infEixds = ind->ixds+1;
      dur = getTime_(ind->idose[infEixds], ind) -
        getTime_(ind->idose[infBixds],ind);
      dur2 = getIiNumber(ind, ind->ixds) - dur;
    }
    /* bi = *i; */
    if (ind->wh0 == EVID0_SSINF){
    } else if (ind->whI == EVIDF_INF_RATE || ind->whI == EVIDF_INF_DUR || ind->whI == EVIDF_MODEL_DUR_ON || ind->whI == EVIDF_MODEL_RATE_ON) {
      ei = *i;
      while(ind->ix[ei] != ind->idose[infEixds] && ei < ind->n_all_times){
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
    // First Reset
    for (j = neq[0]; j--;) {
      ind->InfusionRate[j] = 0;
      ind->on[j] = 1;
    }
    ind->cacheME=0;
    // Reset LHS to NA
    ind->inLhs = 0;
    for (j = op->nlhs; j--;) ind->lhs[j] = NA_REAL;
    memcpy(yp,op->inits, neq[0]*sizeof(double));
    u_inis(neq[1], yp); // Update initial conditions @ current time
    if (rx->istateReset) *istate = 1;
    int k;
    double xp2, xout2;
    int canBreak=0;
    xp2 = xp;
    if (doSSinf) {
      double rate;
      ind->idx=*i;
      // Rate is fixed, so modifying bio-availability doesn't change duration.
      if (ind->whI == EVIDF_MODEL_RATE_ON){
        rate  = getRate(ind, ind->id, ind->cmt, 0.0,
                        ind->all_times[ind->idose[ind->ixds]]);
      } else {
        rate = getDoseNumber(ind, ind->ixds);
      }
      ind->InfusionRate[ind->cmt] = rate;
      ind->on[ind->cmt] = 1;
      double infStep = op->infSSstep, a1=1.0, t1=xp2+1.0;
      // Based on http://www.rxkinetics.com/theo.html -- Chiou method
      for (j = 0; j < op->maxSS; j++){
        if (j == 0) xout2 = xp2+1.; // the first level drawn one hour after infusion
        else xout2 = xp2+infStep;
        solveSS_1(neq, BadDose, InfusionRate, dose, yp, op->do_transit_abs,
                  xout2, xp2, id, i, nx, istate, op, ind, u_inis, ctx);
        canBreak=1;
        if (j <= op->minSS -1){
          for (k = neq[0]; k--;) {
            ind->solveLast[k] = yp[k];
          }
          if (j == 0) {
            a1 = yp[ind->cmt];
          }
          canBreak=0;
        } else {
          for (k = neq[0]; k--;){
            if (op->ssRtol[k]*fabs(yp[k]) + op->ssAtol[k] <= fabs(yp[k]-ind->solveLast[k])){
              canBreak=0;
            }
            ind->solveLast[k] = yp[k];
          }
          if (canBreak){
            ind->InfusionRate[ind->cmt] = 0.0;
            break;
          } else {
            // Assumes that this is at least one half life.
            double a2 = yp[ind->cmt];
            infStep = max2(infStep,M_LN2/(rate/(a1+a2) + 2*(a1-a2)/((a1+a2)*(xout-t1))));
          }
        }
        xp2=xout;
        *istate=1;
      }
    } else if (dur == 0){
      // Oral or Steady State Infusion
      for (j = 0; j < op->maxSS; j++){
        ind->idx=*i;
        xout2 = xp2+ind->ii[ind->idx];
        // Use "real" xout for handle_evid functions.
        handle_evid(ind->evid[ind->ix[*i]], neq[0],
                    BadDose, InfusionRate, dose, yp,
                    op->do_transit_abs, xout, neq[1], ind);
        // yp is last solve or y0
        solveSS_1(neq, BadDose, InfusionRate, dose, yp, op->do_transit_abs,
                  xout2, xp2, id, i, nx, istate, op, ind, u_inis, ctx);
        ind->ixds--; // This dose stays in place
        canBreak=1;
        if (j <= op->minSS -1){
          if (ind->rc[0]== -2019){
            badSolveExit(*i);
            break;
          }
          for (k = op->neq; k--;) {
            ind->solveLast[k] = yp[k];
          }
          canBreak=0;
        } else if (j >= op->minSS){
          if (ind->rc[0] == -2019){
            for (k = neq[0]; k--;) {
              yp[k] = ind->solveLast[k];
            }
            ind->rc[0] = 2019;
            break;
          }
          for (k = neq[0]; k--;){
            if (op->ssRtol[k]*fabs(yp[k]) + op->ssAtol[k] <= fabs(yp[k]-ind->solveLast[k])){
              canBreak=0;
            }
            ind->solveLast[k] = yp[k];
          }
          if (canBreak){
            break;
          }
        }
        *istate=1;
        xp2 = xout2;
      }
    } else {
      if (dur >= getIiNumber(ind, ind->ixds)){
        ind->wrongSSDur=1;
        // Bad Solve => NA
        badSolveExit(*i);
      } else if (ind->err){
        printErr(ind->err, ind->id);
        badSolveExit(*i);
      } else {
        // Infusion
        for (j = 0; j < op->maxSS; j++){
          // Turn on Infusion, solve (0-dur)
          canBreak=1;
          xout2 = xp2+dur;
          ind->idx=*i;
          ind->ixds = infBixds;
          handle_evid(ind->evid[ind->idose[infBixds]], neq[0],
                      BadDose, InfusionRate, dose, yp,
                      op->do_transit_abs, xout, neq[1], ind);
          // yp is last solve or y0
          *istate=1;
          // yp is last solve or y0
          solveSS_1(neq, BadDose, InfusionRate, dose, yp, op->do_transit_abs,
                    xout2, xp2, id, i, nx, istate, op, ind, u_inis, ctx);
          xp2 = xout2;
          // Turn off Infusion, solve (dur-ii)
          xout2 = xp2+dur2;
          ind->ixds = infEixds;
          ind->idx=ei;
          handle_evid(ind->evid[ind->idose[infEixds]], neq[0],
                      BadDose, InfusionRate, dose, yp,
                      op->do_transit_abs, xout+dur, neq[1], ind);
          if (j <= op->minSS -1){
            if (ind->rc[0]== -2019){
              badSolveExit(*i);
              break;
            }
            for (k = neq[0]; k--;) {
              ind->solveLast[k] = yp[k];
            }
            canBreak=0;
          } else if (j >= op->minSS){
            if (ind->rc[0]== -2019){
              if (op->strictSS){
                badSolveExit(*i);
              } else {
                for (k = neq[0]; k--;){
                  yp[k] = ind->solveLast[k];
                }
                ind->rc[0] = 2019;
              }
            }
            for (k = neq[0]; k--;) {
              ind->solveLast[k] = yp[k];
              if (op->ssRtol[k]*fabs(yp[k]) + op->ssAtol[k] <= fabs(yp[k]-ind->solveLast[k])){
                canBreak=0;
              }
            }
          }
          // yp is last solve or y0
          *istate=1;
          solveSS_1(neq, BadDose, InfusionRate, dose, yp, op->do_transit_abs,
                    xout2, xp2, id, i, nx, istate, op, ind, u_inis, ctx);
          if (j <= op->minSS -1){
            if (ind->rc[0]== -2019){
              badSolveExit(*i);
              break;
            }
            for (k = neq[0]; k--;){
              ind->solveLast2[k] = yp[k];
            }
            canBreak=0;
          } else if (j >= op->minSS){
            if (ind->rc[0]== -2019){
              if (op->strictSS){
                badSolveExit(*i);
              } else {
                for (k = neq[0]; k--;){
                  yp[k] = ind->solveLast2[k];
                }
                ind->rc[0] = 2019;
              }
              break;
            }
            for (k = neq[0]; k--;){
              if (op->ssRtol[k]*fabs(yp[k]) + op->ssAtol[k] <= fabs(yp[k]-ind->solveLast2[k])){
                canBreak=0;
              }
              ind->solveLast2[k] = yp[k];
            }
            if (canBreak){
              break;
            }
          }
          xp2 = xout2;
        }
        *istate=1;
        ind->idx=*i;
        ind->ixds = infBixds;
      }
    }

    if (doSS2){
      // Add at the end
      for (j = neq[0];j--;) yp[j]+=ind->solveSave[j];
    }
    ind->idx=*i;
    if (!doSSinf){
      handle_evid(ind->evid[ind->ix[*i]], neq[0],
                  BadDose, InfusionRate, dose, yp,
                  op->do_transit_abs, xout, neq[1], ind);
    }
    ind->doSS=0;
  }
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
  unsigned int j;
  for(i=0; i<nx; i++) {
    ind->idx=i;
    xout = getTime_(ind->ix[i], ind);
    yp = getSolve(i);
    if(ind->evid[ind->ix[i]] != 3 && !isSameTime(xout, xp)) {
      if (ind->err){
        *rc = -1000;
        // Bad Solve => NA
        badSolveExit(i);
      } else {
        idid = indLin(solveid, op, xoutp, yp, xout, ind->InfusionRate, ind->on,
                      ME, IndF);
        xoutp=xout;
        postSolve(&idid, rc, &i, yp, NULL, 0, true, ind, op, rx);
      }
    }
    ind->_newind = 2;
    if (!op->badSolve){
      ind->idx = i;
      if (ind->evid[ind->ix[i]] == 3){
        ind->curShift -= rx->maxShift;
        for (j = neq[0]; j--;) {
          ind->InfusionRate[j] = 0;
          ind->on[j] = 1;
          ind->cacheME=0;
        }
        memcpy(yp,inits, neq[0]*sizeof(double));
        u_inis(neq[1], yp); // Update initial conditions @ current time
        if (rx->istateReset) idid = 1;
        xp=xout;
        ind->ixds++;
      } else if (handleEvid1(&i, rx, neq, yp, &xout)){
        handleSS(neq, BadDose, InfusionRate, ind->dose, yp, op->do_transit_abs, xout,
                 xp, ind->id, &i, nx, &idid, op, ind, u_inis, NULL);
        if (ind->wh0 == 30){
          yp[ind->cmt] = inits[ind->cmt];
        }
        if (rx->istateReset) idid = 1;
        xp = xout;
      }
      if (i+1 != nx) memcpy(getSolve(i+1), yp, neq[0]*sizeof(double));
      calc_lhs(neq[1], xout, getSolve(i), ind->lhs);
      ind->slvr_counter[0]++; // doesn't need do be critical; one subject at a time.
    }
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
  for (int solveid = 0; solveid < nsim*nsub; solveid++){
    if (abort == 0){
      ind_indLin(rx, solveid, update_inis, ME, IndF);
      if (displayProgress){ // Can only abort if it is long enough to display progress.
        curTick = par_progress(solveid, nsim*nsub, curTick, 1, t0, 0);
      }
    }
  }
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
  int *evid;
  double *x;
  int *BadDose;
  double *InfusionRate;
  double xout;
  int *rc;
  double *yp;
  inits = op->inits;
  struct lsoda_context_t * ctx = lsoda_create_ctx();
  ctx->function = (_lsoda_f)dydt_liblsoda;
  ctx->data = neq;
  ctx->neq = neq[0];
  ctx->state = 1;
  ctx->error=NULL;
  ind = &(rx->subjects[neq[1]]);
  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) {
    free(ctx);
    ctx = NULL;
    return;
  }
  nx = ind->n_all_times;
  evid = ind->evid;
  BadDose = ind->BadDose;
  InfusionRate = ind->InfusionRate;
  x = ind->all_times;
  rc= ind->rc;
  double xp = x[0];
  unsigned int j;
  lsoda_prepare(ctx, &opt);
  for(i=0; i<nx; i++) {
    ind->idx=i;
    yp = getSolve(i);
    xout = getTime_(ind->ix[i], ind);
    if(ind->evid[ind->ix[i]] != 3 && !isSameTime(xout, xp)) {
      if (ind->err){
        *rc = -1000;
        // Bad Solve => NA
        badSolveExit(i);
      } else {
        lsoda(ctx, yp, &xp, xout);
        postSolve(&(ctx->state), rc, &i, yp, NULL, 0, false, ind, op, rx);
      }
    }
    ind->_newind = 2;
    if (!op->badSolve){
      ind->idx = i;
      if (ind->evid[ind->ix[i]] == 3){
        ind->curShift -= rx->maxShift;
        for (j = neq[0]; j--;) {
          ind->InfusionRate[j] = 0;
          ind->on[j] = 1;
          ind->cacheME=0;
        }
        memcpy(yp,inits, neq[0]*sizeof(double));
        u_inis(neq[1], yp); // Update initial conditions @ current time
        if (rx->istateReset) ctx->state = 1;
        xp=xout;
        ind->ixds++;
      } else if (handleEvid1(&i, rx, neq, yp, &xout)){
        handleSS(neq, BadDose, InfusionRate, ind->dose, yp, op->do_transit_abs, xout,
                 xp, ind->id, &i, nx, &(ctx->state), op, ind, u_inis, ctx);
        if (ind->wh0 == 30){
          yp[ind->cmt] = inits[ind->cmt];
        }
        if (rx->istateReset) ctx->state = 1;
        xp = xout;
      }
      if (i+1 != nx) memcpy(getSolve(i+1), yp, neq[0]*sizeof(double));
      calc_lhs(neq[1], xout, getSolve(i), ind->lhs);
      ind->slvr_counter[0]++; // doesn't need do be critical; one subject at a time.
      /* for(j=0; j<neq[0]; j++) ret[neq[0]*i+j] = yp[j]; */
    }
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
#ifdef _OPENMP
#pragma omp parallel for num_threads(cores)
#endif
  for (int thread=0; thread < cores; thread++) {
    for (int solveid = thread; solveid < nsim*nsub; solveid+=cores){
      if (abort == 0){
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
#ifdef _OPENMP
#pragma omp parallel for num_threads(op->cores)
#endif
  for (int solveid = 0; solveid < nsim*nsub; solveid++){
    if (abort == 0){
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

extern "C" void rxFreeLast(){
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

  /* memset(rwork,0.0,lrw+1); */ // Does not work since it is a double
  for (i = lrw+1; i--;) rwork[i]=0;
  memset(iwork,0,liw+1); // Works because it is a integer

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

  double xp = ind->all_times[0];
  double xout;

  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) return;
  unsigned int j;
  for(i=0; i < ind->n_all_times; i++) {
    ind->idx=i;
    yp   = getSolve(i);
    xout = getTime_(ind->ix[i], ind);
    if (ind->evid[ind->ix[i]] != 3 && !isSameTime(xout, xp)) {
      if (ind->err){
        ind->rc[0] = -1000;
        // Bad Solve => NA
        badSolveExit(i);
      } else {
        F77_CALL(dlsoda)(dydt_lsoda, neq, yp, &xp, &xout, &gitol, &(op->RTOL), &(op->ATOL), &gitask,
                         &istate, &giopt, rwork, &lrw, iwork, &liw, jdum, &jt);
        postSolve(&istate, ind->rc, &i, yp, err_msg_ls, 7, true, ind, op, rx);
        //dadt_counter = 0;
      }
    }
    ind->_newind = 2;
    if (!op->badSolve){
      ind->idx = i;
      if (ind->evid[ind->ix[i]] == 3){
        ind->curShift -= rx->maxShift;
        for (j = neq[0]; j--;) {
          ind->InfusionRate[j] = 0;
          ind->on[j] = 1;
        }
        memcpy(yp, op->inits, neq[0]*sizeof(double));
        u_inis(neq[1], yp); // Update initial conditions @ current time
        if (rx->istateReset) istate = 1;
        ind->ixds++;
        xp = xout;
      } else if (handleEvid1(&i, rx, neq, yp, &xout)){
        handleSS(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, op->do_transit_abs, xout,
                 xp, ind->id, &i, ind->n_all_times, &istate, op, ind, u_inis, ctx);
        if (ind->wh0 == 30){
          ind->solve[ind->cmt] = op->inits[ind->cmt];
        }
        if (rx->istateReset) istate = 1;
        xp = xout;
      }
      // Copy to next solve so when assigned to yp=ind->solve[neq[0]*i]; it will be the prior values
      if (i+1 != ind->n_all_times) memcpy(getSolve(i+1), yp, neq[0]*sizeof(double));
      calc_lhs(neq[1], xout, getSolve(i), ind->lhs);
    }
  }
  ind->solveTime += ((double)(clock() - t0))/CLOCKS_PER_SEC;
}

extern "C" void ind_lsoda(rx_solve *rx, int solveid,
                          t_dydt_lsoda_dum dydt_ls, t_update_inis u_inis, t_jdum_lsoda jdum,
                          int cjt){
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

extern "C" void par_lsoda(rx_solve *rx){
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
  for (int solveid = 0; solveid < nsim*nsub; solveid++){
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
  if (abort == 1){
    op_global.abort = 1;
  } else {
    if (displayProgress && curTick < 50) par_progress(nsim*nsub, nsim*nsub, curTick, 1, t0, 0);
  }
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
  int *evid;
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
  evid = ind->evid;
  BadDose = ind->BadDose;
  InfusionRate = ind->InfusionRate;
  x = ind->all_times;
  rc= ind->rc;
  double xp = x[0];
  unsigned int j;
  for(i=0; i<nx; i++) {
    ind->idx=i;
    yp = getSolve(i);
    xout = getTime_(ind->ix[i], ind);
    if (global_debug){
      RSprintf("i=%d xp=%f xout=%f\n", i, xp, xout);
    }
    if (ind->evid[ind->ix[i]] != 3 && !isSameTime(xout, xp)) {
      if (ind->err){
        printErr(ind->err, ind->id);
        *rc = idid;
        // Bad Solve => NA
        badSolveExit(i);
      } else {
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
                      0,              /* maximal step size */
                      0,            /* initial step size */
                      op->mxstep, /* maximal number of allowed steps */
                      1,            /* switch for the choice of the coefficients */
                      -1,                     /* test for stiffness */
                      0,                      /* number of components for which dense outpout is required */
                      NULL,           /* indexes of components for which dense output is required, >= nrdens */
                      0                       /* declared length of icon */
                      );
      }
      postSolve(&idid, rc, &i, yp, err_msg, 4, true, ind, op, rx);
      xp = xRead();
      //dadt_counter = 0;
    }
    if (!op->badSolve){
      ind->idx = i;
      if (ind->evid[ind->ix[i]] == 3){
        ind->curShift -= rx->maxShift;
        for (j = neq[0]; j--;) {
          ind->InfusionRate[j] = 0;
          ind->on[j] = 1;
          ind->cacheME=0;
        }
        memcpy(yp, op->inits, neq[0]*sizeof(double));
        u_inis(neq[1], yp); // Update initial conditions @ current time
        ind->ixds++;
        xp=xout;
      } else if (handleEvid1(&i, rx, neq, yp, &xout)){
        handleSS(neq, BadDose, InfusionRate, ind->dose, yp, op->do_transit_abs, xout,
                 xp, ind->id, &i, nx, &istate, op, ind, u_inis, ctx);
        if (ind->wh0 == 30){
          yp[ind->cmt] = inits[ind->cmt];
        }
        xp = xout;
      }
      /* for(j=0; j<neq[0]; j++) ret[neq[0]*i+j] = yp[j]; */
      if (i+1 != nx) memcpy(getSolve(i+1), getSolve(i), neq[0]*sizeof(double));
      calc_lhs(neq[1], xout, getSolve(i), ind->lhs);
    }
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
  for (int solveid = 0; solveid < nsim*nsub; solveid++){
    if (abort == 0){
      ind_dop0(rx, &op_global, solveid, neq, dydt, update_inis);
      if (displayProgress && abort == 0){
        if (checkInterrupt()) abort =1;
      }
      if (displayProgress) curTick = par_progress(solveid, nsim*nsub, curTick, 1, t0, 0);
    }
  }
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

extern "C" void ind_solve(rx_solve *rx, unsigned int cid,
                          t_dydt_liblsoda dydt_lls,
                          t_dydt_lsoda_dum dydt_lsoda, t_jdum_lsoda jdum,
                          t_dydt c_dydt, t_update_inis u_inis,
                          int jt){
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
  if (op->neq !=  0){
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
  iniSubject(cid, 1, &(rx->subjects[cid]), op, rx, u_inis);
  par_progress_0=0;
}

extern "C" void par_solve(rx_solve *rx){
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
  if (op->neq != 0){
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

extern "C" SEXP getDfLevels(const char *item, rx_solve *rx){
  int totN = rx->factorNames.n;
  int base = 0, curLen= rx->factorNs[0], curG=0;
  curLen= rx->factorNs[0];
  base += curLen;
  curLen = rx->factorNs[++curG];
  base += curLen;
  for (int i = 2; i < totN; ++i) {
    const char *curFactor = rx->factorNames.line[i];
    curLen = rx->factorNs[i];
    if (!strncmpci(item, curFactor, strlen(item))) {
      SEXP lvl = PROTECT(allocVector(STRSXP, curLen));
      for (int j = 0; j < curLen; j++){
        SET_STRING_ELT(lvl, j, mkChar(rx->factors.line[base+j]));
      }
      SEXP val = PROTECT(allocVector(INTSXP, rx->nr));
      setAttrib(val, R_LevelsSymbol, lvl);
      SEXP cls = PROTECT(allocVector(STRSXP, 1));
      SET_STRING_ELT(cls, 0, mkChar("factor"));
      setAttrib(val,R_ClassSymbol, cls);
      UNPROTECT(3);
      return val;
    }
    base += curLen;
  }
  SEXP val = PROTECT(allocVector(REALSXP, rx->nr));
  UNPROTECT(1);
  return val;
}

extern "C" void _update_par_ptr(double t, unsigned int id, rx_solve *rx, int idx);

static inline void dfCountRowsForNmOutput(rx_solve *rx, int nsim, int nsub) {
  rx_solving_options_ind *ind;
  int ntimes, di, wh, cmt, wh100, whI, wh0, evid;
  int neq[2];
  rx->nr=0;
  for (int csim = 0; csim < nsim; csim++){
    for (int csub = 0; csub < nsub; csub++){
      neq[1] = csub+csim*nsub;
      ind = &(rx->subjects[neq[1]]);
      ind->id = neq[1];
      ntimes = ind->n_all_times;
      di = 0;
      for (int i = 0; i < ntimes; i++){
        evid = ind->evid[ind->ix[i]];
        if (evid == 9) continue; // not output in NONMEM
        if (isDose(evid)) {
          getWh(evid, &wh, &cmt, &wh100, &whI, &wh0);
          if (whI == 7  || whI == 6){
            di++;
            continue;
          }
          if (getDoseNumber(ind, di) <= 0) {
            di++;
            continue;
          }
          di++;
          rx->nr++;
        } else if (isObs(evid)) {
          rx->nr++;
        }
      }
    }
  }
  di = 0;
}

extern "C" SEXP rxode2_df(int doDose0, int doTBS) {
  rx_solve *rx;
  rx = &rx_global;
  rx_solving_options *op = &op_global;
  int add_cov = rx->add_cov;
  int ncov = op->ncov;
  int ncov0 = rx->nCov0;
  int nkeep  = rx->nKeepF;
  int nlhs = op->nlhs;
  int nobs = rx->nobs - rx->nevid9;
  int nsim = rx->nsim;
  int nall = rx->nall - rx->nevid9;
  int errNcol = rxGetErrsNcol();
  int errNrow = rxGetErrsNrow();
  if (op->nsvar != errNcol){
    rxSolveFreeC();
    Rf_errorcall(R_NilValue, "The simulated residual errors do not match the model specification (%d=%d)",op->nsvar, errNcol);
  }
  int doDose;
  int evid0 = 0;
  int nmevid=0;
  int subsetEvid = 0;
  if (doDose0 == -1){
    nobs = rx->nobs2;
    doDose=0;
    evid0=1;
  } else if (doDose0 == 2 || doDose0 == 3){
    // rate dur ii ss
    doDose=1;
    nmevid=1;
    if (doDose0 == 3){
      subsetEvid=1;
      doDose0 = 2;
    }
  } else {
    doDose=doDose0;
  }
  int di = 0;
  double *dfp;
  int *dfi;
  int ii=0, jj = 0, ntimes;
  int nBadDose;
  int *BadDose;
  int *svar = rx->svar;
  int kk = 0;
  int wh, cmt, wh100, whI, wh0;
  int //dullEvid = 1,
    dullRate=1, dullDur=1,
    dullSS=1, dullIi=1;
  int csub = 0, evid = 0;
  int nsub = rx->nsub;
  int *rmState = rx->stateIgnore;
  int nPrnState =0;
  int i, j;
  int neq[2];
  double *scale;
  rx_solving_options_ind *ind;
  if (subsetEvid == 1){
    dfCountRowsForNmOutput(rx, nsim, nsub);
  } else {
    rx->nr = (doDose == 1 ? nall : nobs)*nsim;
  }
  scale = op->scale;
  neq[0] = op->neq;
  neq[1] = 0;
  for (i = 0; i < neq[0]; i++){
    nPrnState+= (1-rmState[i]);
  }
  // Mutiple ID data?
  int md = 0;
  if (rx->nsub > 1) md = 1;
  // Multiple simulation data?
  int sm = 0;
  if (rx->nsim > 1) sm = 1;
  int ncols =1+nPrnState+nlhs;
  int ncols2 = add_cov*(ncov+ncov0)+nkeep;
  int doseCols = 0;
  int nevid2col = 0;
  if (doDose){
    doseCols = 2;
  } else if (rx->hasEvid2 && doDose0 != -1) {
    // has evid=2 and not ignoring evid=2 (doDose != -1)
    nevid2col = 1;
  }
  int ms = 0;
  if (rx->maxShift != 0.0) ms = 1;
  int nidCols = md + sm + ms;
  int pro = 0;
  if (op->badSolve){
    if (op->naTime){
      rxSolveFreeC();
      Rf_errorcall(R_NilValue, _("'alag(.)'/'rate(.)'/'dur(.)' cannot depend on the state values"));
    }
    if (nidCols == 0){
      rxSolveFreeC();
      Rf_errorcall(R_NilValue, _("could not solve the system"));
    } else {
      warning(_("some ID(s) could not solve the ODEs correctly; These values are replaced with 'NA'"));
    }
  }
  int ncol = ncols+ncols2+nidCols+doseCols+doTBS*4+5*nmevid*doDose+nevid2col;
  SEXP df = PROTECT(allocVector(VECSXP,ncol)); pro++;
  for (i = nidCols; i--;){
    SET_VECTOR_ELT(df, i, PROTECT(allocVector(INTSXP, rx->nr))); pro++;
  }
  i = nidCols;
  double *par_ptr;
  double *errs = rxGetErrs();
  int updateErr = 0;

  if (errNcol > 0){
    updateErr = 1;
  }
  if (doDose){
    //evid
    SET_VECTOR_ELT(df, i++, PROTECT(allocVector(INTSXP, rx->nr))); pro++;
    if (nmevid){
      // cmt
      SET_VECTOR_ELT(df, i++, PROTECT(allocVector(INTSXP, rx->nr))); pro++;
      // ss
      SET_VECTOR_ELT(df, i++, PROTECT(allocVector(INTSXP, rx->nr))); pro++;
    }
    // amt
    SET_VECTOR_ELT(df, i++, PROTECT(allocVector(REALSXP, rx->nr))); pro++;
  } else if (nevid2col) {
    SET_VECTOR_ELT(df, i++, PROTECT(allocVector(INTSXP, rx->nr))); pro++;
  }
  doseCols += nevid2col;
  SEXP paramNames = PROTECT(rxParamNames(op->modNamePtr)); pro++;
  SEXP fkeepNames = PROTECT(get_fkeepn()); pro++;
  for (i = md + sm + ms + doseCols + 2*nmevid; i < ncols + doseCols + nidCols + 2*nmevid; i++){
    SET_VECTOR_ELT(df, i, PROTECT(allocVector(REALSXP, rx->nr))); pro++;
  }
  // These could be factors
  j = ncols + doseCols + nidCols + 2*nmevid;
  const char *charItem;
  int *par_cov = op->par_cov;
  SEXP tmp;
  for (i = 0; i < ncov*add_cov; i++){
    charItem =CHAR(STRING_ELT(paramNames, par_cov[i]-1));
    SET_VECTOR_ELT(df, j++, PROTECT(getDfLevels(charItem, rx))); pro++;
  }
  par_cov = rx->cov0;
  for (i = 0; i < ncov0*add_cov; i++){
    charItem =CHAR(STRING_ELT(paramNames, par_cov[i]));
    SET_VECTOR_ELT(df, j++, PROTECT(getDfLevels(charItem, rx))); pro++;
  }
  for (i = 0; i < nkeep; i++){
    charItem = CHAR(STRING_ELT(fkeepNames, i));
    SET_VECTOR_ELT(df, j++, PROTECT(getDfLevels(charItem, rx))); pro++;
  }
  ncols+= ncols2;
  for (i = ncols + doseCols + nidCols + 2*nmevid; i < ncols + doseCols + nidCols + doTBS*4 + nmevid*5; i++){
    SET_VECTOR_ELT(df, i, PROTECT(allocVector(REALSXP, rx->nr))); pro++;
  }
  // Now create the data frame
  int resetno = 0;
  for (int csim = 0; csim < nsim; csim++) {
    int curi = 0;
    for (csub = 0; csub < nsub; csub++){
      resetno=0;
      neq[1] = csub+csim*nsub;
      ind = &(rx->subjects[neq[1]]);
      iniSubject(neq[1], 1, ind, op, rx, update_inis);
      ntimes = ind->n_all_times;
      par_ptr = ind->par_ptr;
      di = 0;
      if (ind->allCovWarn && csim == 0){
        warning(_("one or more covariates were all 'NA' for subject 'id=%d'"), csub+1);
      }
      for (i = 0; i < ntimes; i++){
        ind->idx = i;
        if (evid == 3) {
          ind->curShift -= rx->maxShift;
          resetno++;
        }
        double curT = getTime_(ind->ix[ind->idx], ind);
        evid = ind->evid[ind->ix[ind->idx]];
        if (evid == 9) continue;
        if (isDose(evid)){
          getWh(ind->evid[ind->ix[i]], &(ind->wh), &(ind->cmt), &(ind->wh100), &(ind->whI), &(ind->wh0));
          handleTlastInline(&curT, ind);
        }
        if (updateErr){
          for (j=0; j < errNcol; j++){
            // The error pointer is updated if needed
            par_ptr[svar[j]] = errs[errNrow*j+kk];
          }
          if ((doDose && evid!= 9) || (evid0 == 0 && isObs(evid)) || (evid0 == 1 && evid==0)){
            // Only increment if this is an observation or of this a
            // simulation that requests dosing information too.
            kk++;
          }
        }
        if (nlhs){
          calc_lhs(neq[1], curT, getSolve(i), ind->lhs);
        }
        if (subsetEvid == 1){
          if (isObs(evid) && evid >= 10) continue;
          if (isDose(evid)){
            getWh(evid, &wh, &cmt, &wh100, &whI, &wh0);
            if (whI == 7 || whI == 6){
              dullRate=0;
              di++;
              continue;
            }
            if (getDoseNumber(ind, di) <= 0){
              di++;
              continue;
            }
          }
        }
        jj  = 0;
        int solveId=csim*nsub+csub;
        if (doDose || (evid0 == 0 && isObs(evid)) || (evid0 == 1 && evid==0)) {
          // sim.id
          if (sm){
            dfi = INTEGER(VECTOR_ELT(df, jj));
            dfi[ii] = csim+1;
            jj++;
          }
          // id
          if (md){
            dfi = INTEGER(VECTOR_ELT(df, jj));
            dfi[ii] = csub+1;
            jj++;
          }
          if (ms) {
            dfi = INTEGER(VECTOR_ELT(df, jj));
            dfi[ii] = resetno+1;
            jj++;
          }
          // evid, cmt, ss, amt, dur, ii
          if (doDose) {
            if (nmevid){
              if (isObs(evid)) {
                // evid
                dfi = INTEGER(VECTOR_ELT(df, jj++));
                if (evid >= 10){
                  dfi[ii] = evid+91; // mtime 101 102 103...
                  /* dullEvid=0; */
                } else {
                  /* if (evid == 2) dullEvid=0; */
                  dfi[ii] = evid;
                }
                // cmt
                dfi = INTEGER(VECTOR_ELT(df, jj++));
                dfi[ii] = NA_INTEGER; // Has all states, cmt makes no sense.
                // ss
                dfi = INTEGER(VECTOR_ELT(df, jj++));
                dfi[ii] = 0;
                // amt
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = NA_REAL;
                // rate
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = NA_REAL;
                // dur
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = NA_REAL;
                // ii
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = NA_REAL;
              } else {
                getWh(evid, &wh, &cmt, &wh100, &whI, &wh0);
                dfi = INTEGER(VECTOR_ELT(df, jj++));
                double curAmt = getDoseNumber(ind, di);
                if (whI == 7){
                  dullRate=0;
                  dfi[ii] = -1;
                } else if (whI == 6){
                  dullRate=0;
                  dfi[ii] = -2; // evid
                } else {
                  if (curAmt > 0) {
                    if (whI == 4){
                      dfi[ii] = 5;
                    } else if (whI == 5){
                      dfi[ii] = 6;
                    } else {
                      dfi[ii] = 1; // evid
                    }
                  } else {
                    if (whI == 1){
                      dullRate=0;
                      dfi[ii] = -10; // evid
                    } else if (whI == 2) {
                      dullDur=0;
                      dfi[ii] = -20; // evid
                    } else if (whI == 4){
                      dfi[ii] = 5;
                    } else if (whI == 5){
                      dfi[ii] = 6;
                    } else {
                      dfi[ii] = 1;
                    }
                  }
                }
                // cmt
                dfi = INTEGER(VECTOR_ELT(df, jj++));
                if (evid == 2 || evid == 3){
                  dfi[ii] = NA_INTEGER;
                } else if (wh0 == 30){
                  dfi[ii] = -cmt-1;
                } else {
                  dfi[ii] = cmt+1;
                }
                // ss
                dfi = INTEGER(VECTOR_ELT(df, jj++));
                switch (wh0){
                  /* case 30: */
                case 20:
                  dullSS=0;
                  dfi[ii] = 2;
                  break;
                case 10:
                  dullSS=0;
                  dfi[ii] = 1;
                  break;
                default:
                  dfi[ii] = 0;
                  break;
                }
              }
            } else {
              // evid
              dfi = INTEGER(VECTOR_ELT(df, jj++));
              dfi[ii] = evid;
              // amt
              dfp = REAL(VECTOR_ELT(df, jj++));
              dfp[ii] = isObs(evid) ? NA_REAL : getDoseNumber(ind, di++);
            }
            if (nmevid && isDose(evid)){
              double curIi = getIiNumber(ind, di);
              if (curIi != 0) dullIi=0;
              double curAmt = getDoseNumber(ind, di++);
              // rate dur ii ss
              switch(ind->whI){
              case 9: // modeled rate
                // amt
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = curAmt;
                // rate
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = -1.0;
                // dur
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = NA_REAL;
                // ii
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = curIi;
                break;
              case 8: // modeled duration
                // amt
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = curAmt;
                // rate
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = -2.0;
                // dur
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = NA_REAL;
                // ii
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = curIi;
                break;
              case 7: // End modeled rate
                // amt
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = NA_REAL;
                // rate
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = NA_REAL;
                // dur
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = NA_REAL;
                // ii
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = curIi;
                break;
              case 6: // end modeled duration
                // amt
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = NA_REAL;
                // rate
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = NA_REAL;
                // dur
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = NA_REAL;
                // ii
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = curIi;
                break;
              case 2: // Infusion specified by dur
                if (curAmt < 0){
                  // amt
                  dfp = REAL(VECTOR_ELT(df, jj++));
                  dfp[ii] = NA_REAL;
                  // rate
                  dfp = REAL(VECTOR_ELT(df, jj++));
                  dfp[ii] = NA_REAL;
                  // dur
                  dfp = REAL(VECTOR_ELT(df, jj++));
                  dfp[ii] = NA_REAL;
                } else {
                  // Find the next fixed length infusion that is turned off.
                  double curDur=0.0;
                  for (int jjj = di; jjj < ind->ndoses; jjj++){
                    if (getDoseNumber(ind, jjj) == -curAmt){
                      int nWh = 0, nCmt = 0, nWh100 = 0, nWhI = 0, nWh0 = 0;
                      getWh(ind->evid[ind->idose[jjj]], &nWh, &nCmt, &nWh100, &nWhI, &nWh0);
                      if (nWhI == whI && nCmt == cmt){
                        curDur = getTime_(ind->idose[jjj], ind) -
                          getTime_(ind->ix[i], ind);
                        break;
                      }
                    }
                  }
                  // amt
                  dfp = REAL(VECTOR_ELT(df, jj++));
                  dfp[ii] = curAmt*curDur;
                  // rate
                  dfp = REAL(VECTOR_ELT(df, jj++));
                  dfp[ii] = NA_REAL;
                  // dur
                  dfp = REAL(VECTOR_ELT(df, jj++));
                  dfp[ii] = curDur;
                }
                // ii
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = curIi;
                break;
              case 1: // Infusion specified by rate
                if (curAmt < 0){
                  // amt
                  dfp = REAL(VECTOR_ELT(df, jj++));
                  dfp[ii] = NA_REAL;
                  // rate
                  dfp = REAL(VECTOR_ELT(df, jj++));
                  dfp[ii] = NA_REAL;
                  // dur
                  dfp = REAL(VECTOR_ELT(df, jj++));
                  dfp[ii] = NA_REAL;
                } else {
                  double curDur=0.0;
                  for (int jjj = di; jjj < ind->ndoses; jjj++){
                    if (getDoseNumber(ind, jjj) == -curAmt){
                      int nWh = 0, nCmt = 0, nWh100 = 0, nWhI = 0, nWh0 = 0;
                      getWh(ind->evid[ind->idose[jjj]], &nWh, &nCmt, &nWh100, &nWhI, &nWh0);
                      if (nWhI == whI && nCmt == cmt){
                        curDur = getTime_(ind->idose[jjj], ind) -
                          getTime_(ind->ix[i], ind);
                        break;
                      }
                    }
                  }
                  // amt
                  dfp = REAL(VECTOR_ELT(df, jj++));
                  dfp[ii] = curAmt*curDur;
                  // rate
                  dfp = REAL(VECTOR_ELT(df, jj++));
                  dfp[ii] = curAmt;
                  // dur
                  dfp = REAL(VECTOR_ELT(df, jj++));
                  dfp[ii] = NA_REAL;
                }
                // ii
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = curIi;
                break;
              default:
                // Non infusion dose.
                // Could be multiply/replace events
                // amt
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = curAmt;
                // rate
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = NA_REAL;
                // dur
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = NA_REAL;
                // ii
                dfp = REAL(VECTOR_ELT(df, jj++));
                dfp[ii] = curIi;
              }
            }
          } else if (nevid2col)  {
            //evid
            dfi = INTEGER(VECTOR_ELT(df, jj++));
            dfi[ii] = evid;
          }
          // time
          dfp = REAL(VECTOR_ELT(df, jj++));
          dfp[ii] = getTime_(ind->ix[i], ind) + ind->curShift;
          // LHS
          if (nlhs){
            for (j = 0; j < nlhs; j++){
              dfp = REAL(VECTOR_ELT(df, jj));
              dfp[ii] =ind->lhs[j];
              jj++;
            }
          }
          // States
          if (nPrnState){
            for (j = 0; j < neq[0]; j++){
              if (!rmState[j]){
                dfp = REAL(VECTOR_ELT(df, jj));
                dfp[ii] = (getSolve(i))[j] / scale[j];
                jj++;
              }
            }
          }
          // Cov
          int didUpdate = 0;
          if (add_cov*ncov > 0){
            // This takes care of the time varying covariates that may be shuffled.
            _update_par_ptr(curT, solveId, rx, ind->idx);
            didUpdate=1;
            for (j = 0; j < add_cov*ncov; j++){
              tmp = VECTOR_ELT(df, jj);
              double tmpD = par_ptr[op->par_cov[j]-1];
              if (TYPEOF(tmp) == REALSXP) {
                dfp = REAL(tmp);
                // is this ntimes = nAllTimes or nObs time for this subject...?
                dfp[ii] = tmpD;
              } else {
                dfi = INTEGER(tmp);
                // is this ntimes = nAllTimes or nObs time for this subject...?
                dfi[ii] = (int)(tmpD);
              }
              jj++;
            }
          }
          if (add_cov*ncov0 > 0){
            for (j = 0; j < add_cov*ncov0; j++){
              tmp  = VECTOR_ELT(df, jj);
              if (TYPEOF(tmp) == REALSXP){
                dfp = REAL(tmp);
                // is this ntimes = nAllTimes or nObs time for this subject...?
                dfp[ii] = ind->par_ptr[rx->cov0[j]];
              } else {
                dfi = INTEGER(tmp);
                // is this ntimes = nAllTimes or nObs time for this subject...?
                dfi[ii] = (int)(ind->par_ptr[rx->cov0[j]]);
              }
              jj++;
            }
          }
          if (nkeep && didUpdate==0) _update_par_ptr(curT, solveId, rx, ind->idx);
          for (j = 0; j < nkeep; j++){
            tmp = VECTOR_ELT(df, jj);
            if (TYPEOF(tmp) == REALSXP){
              dfp = REAL(tmp);
              // is this ntimes = nAllTimes or nObs time for this subject...?
              dfp[ii] = get_fkeep(j, curi + ind->ix[i], ind);
            } else {
              dfi = INTEGER(tmp);
              /* if (j == 0) RSprintf("j: %d, %d; %f\n", j, i, get_fkeep(j, curi + i)); */
              // is this ntimes = nAllTimes or nObs time for this subject...?
              dfi[ii] = (int) (get_fkeep(j, curi + ind->ix[i], ind));
            }
            jj++;
          }
          //
          if (doTBS){
            dfp = REAL(VECTOR_ELT(df, jj));
            dfp[ii] = ind->lambda;
            jj++;
            dfp = REAL(VECTOR_ELT(df, jj));
            dfp[ii] = ind->yj;
            jj++;
            dfp = REAL(VECTOR_ELT(df, jj));
            dfp[ii] = ind->logitLow;
            jj++;
            dfp = REAL(VECTOR_ELT(df, jj));
            dfp[ii] = ind->logitHi;
            jj++;
          }
          ii++;
        }
        ind->_newind = 2;
      }
      curi += ntimes;
      nBadDose = ind->nBadDose;
      BadDose = ind->BadDose;
      if (nBadDose && csim == 0){
        for (i = 0; i < nBadDose; i++){
          if (BadDose[i] > op->extraCmt){
            warning(_("dose to compartment %d ignored (not in system; 'id=%d')"), BadDose[i],csub+1);
          }
        }
      }
      if (updateErr){
        for (j=0; j < errNcol; j++){
          par_ptr[svar[j]] = NA_REAL;
        }
      }
      ind->inLhs = 0;
    }
  }
  SEXP sexp_rownames = PROTECT(allocVector(INTSXP,2)); pro++;
  INTEGER(sexp_rownames)[0] = NA_INTEGER;
  INTEGER(sexp_rownames)[1] = -rx->nr;
  setAttrib(df, R_RowNamesSymbol, sexp_rownames);
  SEXP sexp_colnames = PROTECT(allocVector(STRSXP, ncol)); pro++;
  jj = 0;
  if (sm){
    SET_STRING_ELT(sexp_colnames, jj, mkChar("sim.id"));
    jj++;
  }
  // id
  if (md){
    SET_STRING_ELT(sexp_colnames, jj, mkChar("id"));
    jj++;
  }
  if (ms) {
    SET_STRING_ELT(sexp_colnames, jj, mkChar("resetno"));
    jj++;
  }

  if (doDose){
    SET_STRING_ELT(sexp_colnames, jj, mkChar("evid"));
    jj++;
    if (nmevid){
      SET_STRING_ELT(sexp_colnames, jj, mkChar("cmt"));
      jj++;
      SET_STRING_ELT(sexp_colnames, jj, mkChar("ss"));
      jj++;
    }
    SET_STRING_ELT(sexp_colnames, jj, mkChar("amt"));
    jj++;
    if (nmevid){
      SET_STRING_ELT(sexp_colnames, jj, mkChar("rate"));
      jj++;
      SET_STRING_ELT(sexp_colnames, jj, mkChar("dur"));
      jj++;
      SET_STRING_ELT(sexp_colnames, jj, mkChar("ii"));
      jj++;
    }
  } else if (nevid2col) {
    SET_STRING_ELT(sexp_colnames, jj, mkChar("evid"));
    jj++;
  }
  SET_STRING_ELT(sexp_colnames, jj, mkChar("time"));
  jj++;

  // Put in LHS names
  SEXP lhsNames = PROTECT(rxLhsNames(op->modNamePtr)); pro++;
  for (i = 0; i < nlhs; i++){
    SET_STRING_ELT(sexp_colnames, jj, STRING_ELT(lhsNames,i));
    jj++;
  }
  // Put in state names
  SEXP stateNames = PROTECT(rxStateNames(op->modNamePtr)); pro++;
  if (nPrnState){
    for (j = 0; j < neq[0]; j++){
      if (!rmState[j]){
        SET_STRING_ELT(sexp_colnames, jj, STRING_ELT(stateNames,j));
        jj++;
      }
    }
  }
  // Put in Cov names
  par_cov = op->par_cov;
  for (i = 0; i < ncov*add_cov; i++){
    SET_STRING_ELT(sexp_colnames,jj, STRING_ELT(paramNames, par_cov[i]-1));
    jj++;
  }
  par_cov = rx->cov0;
  for (i = 0; i < ncov0*add_cov; i++){
    SET_STRING_ELT(sexp_colnames,jj, STRING_ELT(paramNames, par_cov[i]));
    jj++;
  }
  for (i = 0; i < nkeep; i++){
    SET_STRING_ELT(sexp_colnames,jj, STRING_ELT(fkeepNames, i));
    jj++;
  }
  if (doTBS){
    SET_STRING_ELT(sexp_colnames, jj, mkChar("rxLambda"));
    jj++;
    SET_STRING_ELT(sexp_colnames, jj, mkChar("rxYj"));
    jj++;
    SET_STRING_ELT(sexp_colnames, jj, mkChar("rxLow"));
    jj++;
    SET_STRING_ELT(sexp_colnames, jj, mkChar("rxHi"));
    jj++;
  }
  setAttrib(df, R_NamesSymbol, sexp_colnames);
  SEXP df2;
  if (nmevid) {
    int ncol2 = ncol - dullRate - dullDur-dullSS-dullIi;
    df2 = PROTECT(allocVector(VECSXP,ncol2)); pro++;
    SEXP sexp_colnames2 = PROTECT(allocVector(STRSXP,ncol2)); pro++;
    jj = 0;
    kk = 0;
    if (sm){
      SET_STRING_ELT(sexp_colnames2, jj, mkChar("sim.id"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
    }
    // id
    if (md){
      SET_STRING_ELT(sexp_colnames2, jj, mkChar("id"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
    }
    if (ms) {
      SET_STRING_ELT(sexp_colnames, jj, mkChar("resetno"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++; kk++;
    }
    SET_STRING_ELT(sexp_colnames2, jj, mkChar("evid"));
    SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
    jj++;kk++;
    SET_STRING_ELT(sexp_colnames2, jj, mkChar("cmt"));
    SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
    jj++;kk++;
    if (dullSS){
      kk++;
    } else {
      SET_STRING_ELT(sexp_colnames2, jj, mkChar("ss"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
    }
    SET_STRING_ELT(sexp_colnames2, jj, mkChar("amt"));
    SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
    jj++;kk++;
    if (dullRate){
      kk++;
    } else {
      SET_STRING_ELT(sexp_colnames2, jj, mkChar("rate"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
    }
    if (dullDur){
      kk++;
    } else {
      SET_STRING_ELT(sexp_colnames2, jj, mkChar("dur"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
    }
    if (dullIi){
      kk++;
    } else {
      SET_STRING_ELT(sexp_colnames2, jj, mkChar("ii"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
    }
    SET_STRING_ELT(sexp_colnames2, jj, mkChar("time"));
    SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
    jj++;kk++;

    // Put in LHS names
    SEXP lhsNames2 = PROTECT(rxLhsNames(op->modNamePtr)); pro++;
    for (i = 0; i < nlhs; i++){
      SET_STRING_ELT(sexp_colnames2, jj, STRING_ELT(lhsNames2,i));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
    }
    // Put in state names
    SEXP stateNames2 = PROTECT(rxStateNames(op->modNamePtr)); pro++;
    if (nPrnState){
      for (j = 0; j < neq[0]; j++){
        if (!rmState[j]){
          SET_STRING_ELT(sexp_colnames2, jj, STRING_ELT(stateNames2,j));
          SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
          jj++;kk++;
        }
      }
    }
    // Put in Cov names
    SEXP paramNames2 = PROTECT(rxParamNames(op->modNamePtr)); pro++;
    int *par_cov = op->par_cov;
    for (i = 0; i < ncov*add_cov; i++){
      SET_STRING_ELT(sexp_colnames2,jj, STRING_ELT(paramNames2, par_cov[i]-1));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
    }
    par_cov = rx->cov0;
    for (i = 0; i < ncov0*add_cov; i++){
      SET_STRING_ELT(sexp_colnames2,jj, STRING_ELT(paramNames2, par_cov[i]));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
    }
    for (i = 0; i < nkeep; i++){
      SET_STRING_ELT(sexp_colnames2,jj, STRING_ELT(fkeepNames, i));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++; kk++;
    }
    if (doTBS){
      SET_STRING_ELT(sexp_colnames2, jj, mkChar("rxLambda"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
      SET_STRING_ELT(sexp_colnames2, jj, mkChar("rxYj"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
      SET_STRING_ELT(sexp_colnames2, jj, mkChar("rxLow"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
      SET_STRING_ELT(sexp_colnames2, jj, mkChar("rxHi"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
    }
    setAttrib(df2, R_NamesSymbol, sexp_colnames2);
    setAttrib(df2, R_RowNamesSymbol, sexp_rownames);
  } else {
    df2=df;
  }
  UNPROTECT(pro);
  return df2;
}
