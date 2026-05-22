// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: nil; -*-
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
// [[Rcpp::interfaces(r,cpp)]]
// [[Rcpp::depends(RcppArmadillo)]]
//#undef NDEBUG
#define STRICT_R_HEADER
#define NCMT 100
// NONMEM 7.1 has a max of 50 obesrrvations/individual
#define MAXIDS 500
#define NALL 500
#define NDOSES 50
//#define rxSolveT 1
// NONMEM nTHETA=20
// NONMEM nETA=30
// NONMEM nSIGMA=10

#define NPARS 60
#include <RcppArmadillo.h>
#include <Rmath.h>
#include <thread>
#include <string>
#include <vector>
#include <cstring>
#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>
#include <climits>
#include <cmath>
#include "checkmate.h"
#include <stdint.h>    // for uint64_t rather than unsigned long long
#include "../inst/include/rxode2.h"
#include "../inst/include/rxode2parseVer.h"
#include "../inst/include/rxode2parseHandleEvid.h"
#include "../inst/include/rxode2parseGetTime.h"
#include "timsort.h"
#define SORT gfx::timsort
#include "par_solve.h"
#include "rxomp.h"
#include <Rcpp.h>
#include "strncmp.h"
#include "rxode2_altrep.h"
#define rxModelVars(a) rxModelVar_s(a)
#define min2( a , b )  ( (a) < (b) ? (a) : (b) )
void resetSolveLinB();
using namespace Rcpp;
using namespace arma;

#define _(String) (String)

#include "rxData.h"

extern t_update_inis update_inis;
extern t_calc_lhs calc_lhs;

static inline bool rxEqIntBlockVal(int a, int b) {
  if (a == NA_INTEGER && b == NA_INTEGER) return true;
  return a == b;
}

static inline bool rxEqRealBlockVal(double a, double b) {
  if ((ISNA(a) || std::isnan(a)) && (ISNA(b) || std::isnan(b))) return true;
  return a == b;
}

static bool rxCanRepBySim(SEXP col, int rowsPerSim, int nsim) {
  if (rowsPerSim <= 0 || nsim <= 1 || ALTREP(col)) return false;
  if ((R_xlen_t)rowsPerSim * (R_xlen_t)nsim != XLENGTH(col)) return false;
  int type = TYPEOF(col);
  if (type == INTSXP || type == LGLSXP) {
    const int *p = INTEGER(col);
    for (int s = 1; s < nsim; ++s) {
      R_xlen_t off = (R_xlen_t)s * (R_xlen_t)rowsPerSim;
      for (int i = 0; i < rowsPerSim; ++i) {
        if (!rxEqIntBlockVal(p[i], p[off + i])) return false;
      }
    }
    return true;
  } else if (type == REALSXP) {
    const double *p = REAL(col);
    for (int s = 1; s < nsim; ++s) {
      R_xlen_t off = (R_xlen_t)s * (R_xlen_t)rowsPerSim;
      for (int i = 0; i < rowsPerSim; ++i) {
        if (!rxEqRealBlockVal(p[i], p[off + i])) return false;
      }
    }
    return true;
  } else if (type == STRSXP) {
    for (int s = 1; s < nsim; ++s) {
      R_xlen_t off = (R_xlen_t)s * (R_xlen_t)rowsPerSim;
      for (int i = 0; i < rowsPerSim; ++i) {
        if (STRING_ELT(col, i) != STRING_ELT(col, off + i)) return false;
      }
    }
    return true;
  }
  return false;
}

static SEXP rxRepFromFirstSim(SEXP col, int rowsPerSim, int nsim) {
  int type = TYPEOF(col);
  if (type == INTSXP) {
    IntegerVector base(rowsPerSim);
    memcpy(base.begin(), INTEGER(col), (size_t)rowsPerSim * sizeof(int));
    RObject out = rxode2_make_rep_int(base, nsim);
    Rf_copyMostAttrib(col, out);
    return wrap(out);
  } else if (type == LGLSXP) {
    LogicalVector base(rowsPerSim);
    memcpy(base.begin(), LOGICAL(col), (size_t)rowsPerSim * sizeof(int));
    RObject out = rxode2_make_rep_lgl(base, nsim);
    Rf_copyMostAttrib(col, out);
    return wrap(out);
  } else if (type == REALSXP) {
    NumericVector base(rowsPerSim);
    memcpy(base.begin(), REAL(col), (size_t)rowsPerSim * sizeof(double));
    RObject out = rxode2_make_rep_real(base, nsim);
    Rf_copyMostAttrib(col, out);
    return wrap(out);
  } else if (type == STRSXP) {
    CharacterVector base(rowsPerSim);
    for (int i = 0; i < rowsPerSim; ++i) {
      SET_STRING_ELT(base, i, STRING_ELT(col, i));
    }
    RObject out = rxode2_make_rep_str(base, nsim);
    Rf_copyMostAttrib(col, out);
    return wrap(out);
  }
  return R_NilValue;
}

extern "C" SEXP getDfLevels(const char *item, rx_solve *rx, R_xlen_t nrow) {
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
      CharacterVector lvl(curLen);
      for (int j = 0; j < curLen; j++){
        SET_STRING_ELT(lvl, j,Rf_mkChar(rx->factors.line[base+j]));
      }
      IntegerVector val(nrow);
      Rf_setAttrib(val, R_LevelsSymbol, lvl);
      CharacterVector cls(1);
      SET_STRING_ELT(cls, 0, Rf_mkChar("factor"));
      Rf_setAttrib(val,R_ClassSymbol, cls);
      return val;
    }
    base += curLen;
  }
  NumericVector val(nrow);
  return wrap(val);
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
        evid = getEvid(ind, ind->ix[i]);
        if (evid == 9) continue; // not output in NONMEM
        if (isDose(evid)) {
          getWh(evid, &wh, &cmt, &wh100, &whI, &wh0);
          if (whI == EVIDF_MODEL_RATE_OFF  || whI == EVIDF_MODEL_DUR_OFF){
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

extern "C" void printErr(int err, int id);
extern "C" void setupFkeepCache();
SEXP rxode2_df(int doDose0, int doTBS, std::vector<int>& lvlI, bool isIdentity) {
  rx_solve *rx;
  rx = &rx_global;
  rx_solving_options *op = &op_global;

  int add_cov = rx->add_cov;
  int ncov = op->ncov;
  int ncov0 = rx->nCov0;
  int nkeep  = rx->nKeepF;
  int nlhs = op->nlhs;
  int nobs = rx->nobs - rx->nevid9;
  int nsim = (int)rx->nsim;
  int nall = rx->nall - rx->nevid9;
  int errNcol = rxGetErrsNcol();
  int errNrow = rxGetErrsNrow();

  if (op->nsvar != errNcol) {
    rxSolveFreeC();
    (Rf_errorcall)(R_NilValue, _("The simulated residual errors do not match the model specification (%d=%d)"),op->nsvar, errNcol);
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
  int nsub = (int)rx->nsub;
  IntegerVector rmState = rxStateIgnore(op->modNamePtr);
  int nPrnState =0;
  int i, j;
  int neq[2];
  double *scale;
  rx_solving_options_ind *ind;
  if (subsetEvid == 1){
    dfCountRowsForNmOutput(rx, nsim, nsub);
    if (rx->nr > (int64_t)INT_MAX) {
      rxSolveFreeC();
      (Rf_errorcall)(R_NilValue,
                     _("the output size (%lld rows) is too large for rxSolve to handle"),
                     (long long)rx->nr);
    }
  } else if (doDose == 1) {
    int64_t nrLong = (int64_t)nall * (int64_t)nsim;
    if (nrLong > (int64_t)INT_MAX) {
      rxSolveFreeC();
      (Rf_errorcall)(R_NilValue,
                     _("the output size (%lld rows) is too large for rxSolve to handle"),
                     (long long)nrLong);
    }
    rx->nr = nrLong;
  } else {
    // When evid_() pushed extra observation events during the solve, n_all_times grew beyond
    // n_all_times_orig, so we must count per-subject observations rather than using nobs*nsim.
    // For models without evid_() (indOwnAlloc=0), ix may not be fully initialised at this
    // point (sortInd runs during the solve pass, which may be skipped for neq==0 models),
    // so fall back to the nobs * nsim formula which is always correct in that case.
    bool anyPushed = false;
    for (int _cs = 0; _cs < nsim && !anyPushed; _cs++) {
      for (int _cb = 0; _cb < nsub && !anyPushed; _cb++) {
        if (rx->subjects[_cb + _cs * nsub].nPushedExtra > 0) anyPushed = true;
      }
    }
    int64_t nrLong;
    if (anyPushed) {
      // Count output rows from the grown n_all_times using the same predicate as subRowStart:
      // include doses when doDose==1, observations when doDose==0, etc.
      nrLong = 0;
      for (int _cs = 0; _cs < nsim; _cs++) {
        for (int _cb = 0; _cb < nsub; _cb++) {
          rx_solving_options_ind *_ind = &rx->subjects[_cb + _cs * nsub];
          for (int _i = 0; _i < _ind->n_all_times; _i++) {
            int _e = getEvid(_ind, _ind->ix[_i]);
            if (isObs(_e)) nrLong++;
          }
        }
      }
    } else {
      nrLong = (int64_t)nobs * (int64_t)nsim;
    }
    if (nrLong > (int64_t)INT_MAX) {
      rxSolveFreeC();
      (Rf_errorcall)(R_NilValue,
                     _("the output size (%lld rows) is too large for rxSolve to handle"),
                     (long long)nrLong);
    }
    rx->nr = nrLong;
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
  bool simSubjectPath = (rx->nsim > 1 && rx->nsub == 1);
  // Multiple simulation data?
  int sm = 0;
  if (rx->nsim > 1) sm = 1;
  int ncols =1+nPrnState + nlhs;
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
  if (op->badSolve){
    if (op->naTime){
      rxSolveFreeC();
      int cmt = op->naTime/10;
      int errNo = op->naTime - 10*cmt;
      CharacterVector stateNames = rxStateNames(op->modNamePtr);
      if (errNo == rxErrNaTimeLag) {
        (Rf_errorcall)(R_NilValue,
                     _("'alag(%s)' and maybe more items produce NA (could depend on state values)"),
                     CHAR(STRING_ELT(stateNames, cmt)));
      } else if (errNo == rxErrNaTimeRate) {
        (Rf_errorcall)(R_NilValue,
                     _("'rate(%s)' and maybe more items produce NA (could depend on state values)"),
                     CHAR(STRING_ELT(stateNames, cmt)));
      } else if (errNo == rxErrNaTimeDur) {
        (Rf_errorcall)(R_NilValue,
                     _("'dur(%s)' and maybe more items produce NA (could depend on state values)"),
                     CHAR(STRING_ELT(stateNames, cmt)));
      } else if (errNo == rxErrNaTimeAmtI) {
        (Rf_errorcall)(R_NilValue,
                     _("'amt(%s)' calculation during infusion and maybe more items produce NA (could depend on state values)"),
                     CHAR(STRING_ELT(stateNames, cmt)));
      } else if (errNo == rxErrNaTimeAmt) {
        (Rf_errorcall)(R_NilValue,
                     _("'amt(%s)' calculation and maybe more items produce NA (could depend on state values)"),
                     CHAR(STRING_ELT(stateNames, cmt)));
      } else {
        (Rf_errorcall)(R_NilValue, _("NA time error %d"), errNo);
      }
      // (Rf_errorcall)(R_NilValue, "%s", _("'alag(.)'/'rate(.)'/'dur(.)' cannot depend on the state values"));
    }
    if (nidCols == 0){
      if (rx->extraPushAbort) {
        rxSolveFreeC();
        (Rf_errorcall)(R_NilValue,
          _("evid_() pushed more than maxExtra=%d events per individual; increase maxExtra in rxControl()/rxSolve()"),
          rx->maxExtra);
      }
      for (int solveid = 0; solveid < (int)(rx->nsub * rx->nsim); solveid++){
        rx_solving_options_ind *indE = &(rx->subjects[solveid]);
        if (indE->err != 0) {
          printErr(indE->err, indE->id);
        }
      }
      rxSolveFreeC();
      (Rf_errorcall)(R_NilValue, "%s", _("could not solve the system"));
    } else {
      if (rx->extraPushAbort) {
        rxSolveFreeC();
        (Rf_errorcall)(R_NilValue,
          _("evid_() pushed more than maxExtra=%d events per individual; increase maxExtra in rxControl()/rxSolve()"),
          rx->maxExtra);
      }
      warning(_("some ID(s) could not solve the ODEs correctly; These values are replaced with 'NA'"));
    }
  }
  // Pre-compute per-subject row counts before allocating output columns so
  // cov/keep columns can be sized to rowsPerSimUniform rather than rx->nr.
  int updateErr = (errNcol > 0) ? 1 : 0;
  int nsolve_df = nsim * nsub;
  std::vector<int> subRowStart(nsolve_df + 1, 0);
  std::vector<int> subKkStart(nsolve_df + 1, 0);
  std::vector<int> subCuriStart(nsolve_df + 1, 0);
  for (int _csim = 0; _csim < nsim; _csim++) {
    int _curi = 0;
    for (int _csub = 0; _csub < nsub; _csub++) {
      int _sid = _csim * nsub + _csub;
      subCuriStart[_sid] = _curi;
      _curi += rx->subjects[_sid].n_all_times;
    }
  }
  for (int _sid = 0; _sid < nsolve_df; _sid++) {
    rx_solving_options_ind *_sind = &rx->subjects[_sid];
    int _di = 0, _subRows = 0, _subKk = 0;
    for (int _ti = 0; _ti < _sind->n_all_times; _ti++) {
      int _ev = getEvid(_sind, _sind->ix[_ti]);
      if (_ev == 9) continue;
      if (updateErr) {
        if ((doDose && _ev != 9) || (evid0 == 0 && isObs(_ev)) || (evid0 == 1 && _ev == 0)) {
          _subKk++;
        }
      }
      if (subsetEvid == 1) {
        if (isObs(_ev) && _ev >= 10) continue;
        if (isDose(_ev)) {
          int _wh=0, _cmt=0, _wh100=0, _whI=0, _wh0=0;
          getWh(_ev, &_wh, &_cmt, &_wh100, &_whI, &_wh0);
          if (_whI == EVIDF_MODEL_RATE_OFF || _whI == EVIDF_MODEL_DUR_OFF) { _di++; continue; }
          if (getDoseNumber(_sind, _di) <= 0) { _di++; continue; }
          _di++;
        }
      }
      if (doDose || (evid0 == 0 && isObs(_ev)) || (evid0 == 1 && _ev == 0)) {
        _subRows++;
      }
    }
    subRowStart[_sid + 1] = subRowStart[_sid] + _subRows;
    subKkStart[_sid + 1]  = subKkStart[_sid]  + _subKk;
  }
  // subRowStart[nsolve_df] is the authoritative total row count; it uses the same
  // per-event predicate as the fill loop and correctly includes any extra events
  // pushed by evid_() that are not reflected in the nall/nobs estimates above.
  rx->nr = (int64_t)subRowStart[nsolve_df];
  bool uniformSimBlocks = false;
  int rowsPerSimUniform = 0;
  if (nsim > 1 && nsub > 0) {
    rowsPerSimUniform = subRowStart[nsub] - subRowStart[0];
    if (rowsPerSimUniform > 0) {
      uniformSimBlocks = true;
      for (int _sim = 1; _sim < nsim && uniformSimBlocks; _sim++) {
        int _from = _sim * nsub;
        int _rows = subRowStart[_from + nsub] - subRowStart[_from];
        if (_rows != rowsPerSimUniform) uniformSimBlocks = false;
      }
    }
  }
  bool hasRuntimeEventMutation = false;
  for (int _sid = 0; _sid < nsolve_df && !hasRuntimeEventMutation; _sid++) {
    rx_solving_options_ind *_sind = &rx->subjects[_sid];
    if (_sind->nPushedExtra > 0) hasRuntimeEventMutation = true;
  }
  // Dose/time/cov/keep columns are sized to one sim block when the block pattern
  // repeats identically; the fill loop only writes csim==0 rows and ALTREP wraps.
  bool doseTimeShrunk = uniformSimBlocks && !hasRuntimeEventMutation && rowsPerSimUniform > 0 && nmevid == 0;
  bool covKeepShrunk  = doseTimeShrunk && ncols2 > 0;
  R_xlen_t doseTimeNrow = doseTimeShrunk ? (R_xlen_t)rowsPerSimUniform : (R_xlen_t)rx->nr;
  R_xlen_t covKeepNrow  = covKeepShrunk  ? (R_xlen_t)rowsPerSimUniform : (R_xlen_t)rx->nr;

  int ncol = ncols+ncols2+nidCols+doseCols+doTBS*4+5*nmevid*doDose+nevid2col;
  List df = List(ncol);
  for (i = nidCols; i--;){
    df[i] = IntegerVector((R_xlen_t)rx->nr);
  }
  i = nidCols;
  double *par_ptr;
  double *errs = rxGetErrs();
  if (doDose){
    //evid
    df[i++] = IntegerVector(doseTimeNrow);
    if (nmevid){
      // cmt
      df[i++] = IntegerVector(doseTimeNrow);
      // ss
      df[i++] = IntegerVector(doseTimeNrow);
    }
    // amt
    df[i++] = NumericVector(doseTimeNrow);
  } else if (nevid2col) {
    df[i++] = IntegerVector((R_xlen_t)rx->nr);
  }
  doseCols += nevid2col;
  CharacterVector paramNames = rxParamNames(op->modNamePtr);
  CharacterVector fkeepNames = get_fkeepn();
  // time comes in here
  df[md + sm +ms + doseCols + 2*nmevid] = NumericVector(doseTimeNrow);
  CharacterVector lhsNames = rxLhsNames(op->modNamePtr);
  // time
  int i0 = md + sm + ms + doseCols + 2*nmevid;
  df[i0] = NumericVector(doseTimeNrow);
  i0++;

  // nlhs
  for (i = i0; i < i0+nlhs; i++){
    if (op->lhs_str[i-i0] == 1) {
      // factor; from string expression
      SEXP _tmp = getDfLevels(CHAR(STRING_ELT(lhsNames, i-i0)), rx, (R_xlen_t)rx->nr);
      if (TYPEOF(_tmp) != INTSXP) {
        IntegerVector val((R_xlen_t)rx->nr, NA_INTEGER);
        CharacterVector cls(1);
        SET_STRING_ELT(cls, 0, Rf_mkChar("factor"));
        Rf_setAttrib(val, R_ClassSymbol, cls);
        df[i] = val;
      } else {
        df[i] = _tmp;
      }
    } else {
      df[i] = NumericVector((R_xlen_t)rx->nr);
    }
  }
  i0+=nlhs;

  // Rest is numeric
  for (i = i0; i < ncols + doseCols + nidCols + 2*nmevid; i++){
    df[i] = NumericVector((R_xlen_t)rx->nr);
  }
  // These could be factors
  j = ncols + doseCols + nidCols + 2*nmevid;
  const char *charItem;
  int *par_cov = op->par_cov;
  SEXP tmp;
  for (i = 0; i < ncov*add_cov; i++){
    charItem =CHAR(STRING_ELT(paramNames, par_cov[i]-1));
    df[j++] = getDfLevels(charItem, rx, covKeepNrow);
  }
  par_cov = rx->cov0;
  for (i = 0; i < ncov0*add_cov; i++){
    charItem =CHAR(STRING_ELT(paramNames, par_cov[i]));
    df[j++] = getDfLevels(charItem, rx, covKeepNrow);
  }
  for (i = 0; i < nkeep; i++){
    charItem = CHAR(STRING_ELT(fkeepNames, i));
    df[j++] = getDfLevels(charItem, rx, covKeepNrow);
  }
  ncols += ncols2;
  for (i = ncols + doseCols + nidCols + 2*nmevid;
       i < ncols + doseCols + nidCols + nmevid*5 - nkeep;
       i++){
    df[i] = NumericVector(doseTimeNrow);
  }
  // keep items (when nmevid==0 these are the same positions as the main-block
  // keep allocated by getDfLevels above; use covKeepNrow so the size matches).
  j = 0;
  for (i = ncols + doseCols + nidCols + nmevid*5 - nkeep;
       i < ncols + doseCols + nidCols + nmevid*5;
       i++) {
    int curType = get_fkeepType(j);
    if (curType == 4) {
      df[i] = assign_fkeepAttr(j, NumericVector(covKeepNrow));
    } else if (curType == 1) {
      df[i] = StringVector(covKeepNrow);
    } else if (curType == 5) {
      df[i] = assign_fkeepAttr(j, LogicalVector(covKeepNrow));
    } else {
      IntegerVector cur(covKeepNrow);
      if (curType == 2) {
        cur.attr("levels") = get_fkeepLevels(j);
      }
      df[i] = assign_fkeepAttr(j,cur);
    }
    j++;
  }
  // tbs items
  for (i = ncols + doseCols + nidCols + nmevid*5;
       i < ncols + doseCols + nidCols + nmevid*5 + doTBS*4;
       i++) {
    df[i] = NumericVector(doseTimeNrow);
  }
  // Now create the data frame
  // Pre-extract raw column data pointers — safe as long as no R API calls
  // occur inside the parallel fill region below.
  std::vector<double*> colR(ncol, nullptr);
  std::vector<int*>    colI(ncol, nullptr);
  std::vector<int>     colType(ncol, 0);
  bool hasStrCol = false;
  for (int _c = 0; _c < ncol; _c++) {
    SEXP _col = VECTOR_ELT(df, _c);
    colType[_c] = TYPEOF(_col);
    switch (colType[_c]) {
    case REALSXP: colR[_c] = REAL(_col); break;
    case INTSXP:
    case LGLSXP:
      // Leave nullptr for ALTREP columns — values are computed on-the-fly;
      // do not call INTEGER() which would materialise them.
      colI[_c] = ALTREP(_col) ? nullptr : INTEGER(_col);
      break;
    case STRSXP:  hasStrCol = true; break;
    default: break;
    }
  }
  // Pre-compute keep-column base df-index and build CHARSXP level caches for
  // any STRSXP keep columns.  SET_STRING_ELT is called directly inside the
  // OpenMP region (thread-safe when writing to disjoint row indices).
  int _jj_keep_base = ncols + doseCols + nidCols + nmevid*5 - nkeep;
  std::vector<SEXP *> strLevelCache(ncol, nullptr);
  std::vector<int>    strLevelCount(ncol, 0);
  std::vector<SEXP>   colSEXP(ncol, R_NilValue);
  if (nkeep && hasStrCol) {
    for (int _j = 0; _j < nkeep; _j++) {
      int _jj = _jj_keep_base + _j;
      if (colType[_jj] == STRSXP) {
        colSEXP[_jj] = VECTOR_ELT(df, _jj);
        SEXP _lvl = get_fkeepLevels(_j);
        int _nLev = Rf_length(_lvl);
        strLevelCount[_jj] = _nLev;
        // 1-based: slot 0 = NA_STRING, slots 1..nLev = interned level CHARSXPs
        SEXP *_lc = (SEXP *)R_alloc(_nLev + 1, sizeof(SEXP));
        _lc[0] = NA_STRING;
        for (int _k = 0; _k < _nLev; _k++)
          _lc[_k + 1] = STRING_ELT(_lvl, _k);
        strLevelCache[_jj] = _lc;
      }
    }
  }
  // Pre-compute LHS factor level counts for bounds checking in the fill loop.
  int _lhsColStart = nidCols + doseCols + 2*nmevid + 1; // +1 for time column
  std::vector<int> lhsLevelCount(nlhs, 0);
  for (int _lhsJ = 0; _lhsJ < nlhs; _lhsJ++) {
    if (op->lhs_str[_lhsJ] == 1) {
      SEXP _lhsCol = VECTOR_ELT(df, _lhsColStart + _lhsJ);
      SEXP _lvl = Rf_getAttrib(_lhsCol, R_LevelsSymbol);
      lhsLevelCount[_lhsJ] = (_lvl != R_NilValue) ? Rf_length(_lvl) : 0;
    }
  }
  // Use ALTREP compact sequences for id/sim.id when all subjects contribute
  // the same number of rows.  This avoids allocating and filling large integer
  // vectors for bookkeeping columns that are fully predictable.
  // Condition: nsolve_df > 0 and all per-solveid row counts are equal.
  bool useAltrepId = false;
  if (nsolve_df > 1 && (md || sm)) {
    int rows0 = subRowStart[1] - subRowStart[0];
    if (rows0 > 0) {
      useAltrepId = true;
      for (int _sid = 1; _sid < nsolve_df && useAltrepId; _sid++) {
        if (subRowStart[_sid + 1] - subRowStart[_sid] != rows0) {
          useAltrepId = false;
        }
      }
    }
    if (useAltrepId) {
      bool mdFilled = false;
      int jj_alt = 0;
      if (sm) {
        int rows_per_sim = nsub * rows0;
        df[jj_alt] = rxode2_make_seqrep(nsim, rows_per_sim, (R_xlen_t)rx->nr);
        jj_alt++;
      }
      if (md) {
        if (!lvlI.empty() && isIdentity) {
          // Sequential integer IDs (1:nsub): seqrep or repint(seqrep, nsim).
          df[jj_alt] = rxode2_make_seqrep(nsub, rows0, (R_xlen_t)rx->nr);
          jj_alt++;
          mdFilled = true;
        } else if (!lvlI.empty() && !isIdentity && nsim > 1) {
          // Non-sequential integer IDs, multi-sim: build rep_int(base, nsim)
          // directly from lvlI.  Allocates only nsub*rows0 (one sim block) —
          // avoids the O(nr) fill + uniformity scan in the old rxData.cpp re-wrap.
          int baseLen = nsub * rows0;
          IntegerVector _bp(baseLen);
          for (int _s = 0; _s < nsub; _s++)
            for (int _r = 0; _r < rows0; _r++)
              _bp[_s * rows0 + _r] = lvlI[_s];
          RObject cur = rxode2_make_rep_int(_bp, nsim);
          df[jj_alt] = cur;
          jj_alt++;
          mdFilled = true;
        }
        // lvlI.empty() (character IDs): no ALTREP — fill loop writes csub_par+1
        // and factor conversion in rxData.cpp attaches character levels.
        // !isIdentity && nsim==1: fill loop writes lvlI[csub_par] directly.
      }
      int jj_null = 0;
      if (sm) { colI[jj_null] = nullptr; jj_null++; }
      if (mdFilled) { colI[jj_null] = nullptr; }
    }
  }

  // Unified data-frame fill.  SET_STRING_ELT is called directly inside the
  // OpenMP region; it is safe because each thread writes to disjoint row indices.
  if (nkeep) setupFkeepCache();
#ifdef _OPENMP
#pragma omp parallel for num_threads(op->cores) schedule(dynamic,1)
#endif
    for (int solveid = 0; solveid < nsolve_df; solveid++) {
      int csim     = solveid / nsub;
      int csub_par = solveid % nsub;
      int ii       = subRowStart[solveid];
      bool _writeDT = !doseTimeShrunk || csim == 0;
      int kk       = (errNrow > 0) ? min2(subKkStart[solveid], errNrow - 1) : 0;
      int curi     = subCuriStart[solveid];
      int resetno_p = 0;
      int di_p      = 0;
      int jj_p, j;
      int evid_p = 0;
      int wh_p, cmt_p, wh100_p, whI_p, wh0_p;
      // Shadow outer `ind` so that getSolve() macro works correctly.
      rx_solving_options_ind *ind = &(rx->subjects[solveid]);
      iniSubject(solveid, 1, ind, op, rx, update_inis);
      int ntimes_p    = ind->n_all_times;
      double *par_ptr_p = ind->par_ptr;

      for (int i = 0; i < ntimes_p; i++) {
        ind->idx = i;
        if (evid_p == 3) {
          ind->curShift -= rx->maxShift;
          resetno_p++;
        }
        double curT = getTime_(ind->ix[ind->idx], ind);
        evid_p = getEvid(ind, ind->ix[ind->idx]);
        if (evid_p == 9) continue;
        if (isDose(evid_p)) {
          getWh(getEvid(ind, ind->ix[i]), &(ind->wh), &(ind->cmt), &(ind->wh100), &(ind->whI), &(ind->wh0));
          switch (ind->whI) {
          case EVIDF_INF_RATE:
          case EVIDF_MODEL_DUR_ON:
          case EVIDF_MODEL_DUR_OFF:
          case EVIDF_MODEL_RATE_ON:
          case EVIDF_MODEL_RATE_OFF:
#pragma omp atomic write
            dullRate = 0;
            break;
          case EVIDF_INF_DUR:
#pragma omp atomic write
            dullDur = 0;
            break;
          }
          handleTlastInline(&curT, ind);
        }
        if (updateErr) {
          for (j = 0; j < errNcol; j++) {
            par_ptr_p[svar[j]] = errs[errNrow*j + kk];
          }
          if ((doDose && evid_p != 9) || (evid0 == 0 && isObs(evid_p)) || (evid0 == 1 && evid_p == 0)) {
            kk = min2(kk + 1, errNrow - 1);
          }
        }
        if (nlhs) {
          calc_lhs(solveid, curT, getSolve(i), ind->lhs);
        }
        if (subsetEvid == 1) {
          if (isObs(evid_p) && evid_p >= 10) continue;
          if (isDose(evid_p)) {
            getWh(evid_p, &wh_p, &cmt_p, &wh100_p, &whI_p, &wh0_p);
            if (whI_p == EVIDF_MODEL_RATE_OFF || whI_p == EVIDF_MODEL_DUR_OFF) {
#pragma omp atomic write
              dullRate = 0;
              di_p++;
              continue;
            } else if (whI_p == EVIDF_INF_RATE || whI_p == EVIDF_MODEL_RATE_ON || whI_p == EVIDF_MODEL_DUR_ON) {
#pragma omp atomic write
              dullRate = 0;
            } else if (whI_p == EVIDF_INF_DUR) {
#pragma omp atomic write
              dullDur = 0;
            }
            if (getDoseNumber(ind, di_p) <= 0) {
              di_p++;
              continue;
            }
          }
        }
        jj_p = 0;
        if (doDose || (evid0 == 0 && isObs(evid_p)) || (evid0 == 1 && evid_p == 0)) {
          if (sm) { if (colI[jj_p] != nullptr) colI[jj_p][ii] = csim + 1;                                   jj_p++; }
          if (md) {
            if (colI[jj_p] != nullptr) {
              colI[jj_p][ii] = lvlI.empty() ? csub_par + 1 : lvlI[csub_par];
            }
            jj_p++;
          }
          if (ms) { colI[jj_p][ii] = resetno_p + 1; jj_p++; }
          if (doDose) {
            if (nmevid) {
              if (isObs(evid_p)) {
                if (_writeDT) colI[jj_p][ii] = (evid_p >= 10) ? evid_p + 91 : evid_p; jj_p++;
                if (_writeDT) colI[jj_p][ii] = NA_INTEGER; jj_p++;  // cmt
                if (_writeDT) colI[jj_p][ii] = 0;          jj_p++;  // ss
                if (_writeDT) colR[jj_p][ii] = NA_REAL;    jj_p++;  // amt
                if (_writeDT) colR[jj_p][ii] = NA_REAL;    jj_p++;  // rate
                if (_writeDT) colR[jj_p][ii] = NA_REAL;    jj_p++;  // dur
                if (_writeDT) colR[jj_p][ii] = NA_REAL;    jj_p++;  // ii
              } else {
                getWh(evid_p, &wh_p, &cmt_p, &wh100_p, &whI_p, &wh0_p);
                double curAmt = getDoseNumber(ind, di_p);
                if (evid_p == 3) {
                  if (_writeDT) colI[jj_p][ii] = 3;
                } else if (whI_p == EVIDF_MODEL_RATE_OFF) {
#pragma omp atomic write
                  dullRate = 0;
                  if (_writeDT) colI[jj_p][ii] = -1;
                } else if (whI_p == EVIDF_MODEL_DUR_OFF) {
#pragma omp atomic write
                  dullRate = 0;
                  if (_writeDT) colI[jj_p][ii] = -2;
                } else {
                  if (curAmt > 0) {
                    if (whI_p == EVIDF_REPLACE) {
                      if (_writeDT) colI[jj_p][ii] = 5;
                    } else if (whI_p == EVIDF_MULT) {
                      if (_writeDT) colI[jj_p][ii] = 6;
                    } else {
                      if (_writeDT) colI[jj_p][ii] = 1;
                      if (whI_p == EVIDF_INF_RATE || whI_p == EVIDF_MODEL_DUR_ON || whI_p == EVIDF_MODEL_RATE_ON) {
#pragma omp atomic write
                        dullRate = 0;
                      } else if (whI_p == EVIDF_INF_DUR) {
#pragma omp atomic write
                        dullDur = 0;
                      }
                    }
                  } else {
                    if (whI_p == EVIDF_INF_RATE) {
#pragma omp atomic write
                      dullRate = 0;
                      if (_writeDT) colI[jj_p][ii] = -10;
                    } else if (whI_p == EVIDF_INF_DUR) {
#pragma omp atomic write
                      dullDur = 0;
                      if (_writeDT) colI[jj_p][ii] = -20;
                    } else if (whI_p == EVIDF_REPLACE) {
                      if (_writeDT) colI[jj_p][ii] = 5;
                    } else if (whI_p == EVIDF_MULT) {
                      if (_writeDT) colI[jj_p][ii] = 6;
                    } else {
                      if (_writeDT) colI[jj_p][ii] = 1;
                      if (whI_p == EVIDF_INF_DUR) {
#pragma omp atomic write
                        dullDur = 0;
                      }
                    }
                  }
                }
                jj_p++;  // evid column done
                // cmt
                if (evid_p == 2 || evid_p == 3) {
                  if (_writeDT) colI[jj_p][ii] = NA_INTEGER;
                } else if (wh0_p == 30) {
                  if (_writeDT) colI[jj_p][ii] = -cmt_p - 1;
                } else {
                  if (_writeDT) colI[jj_p][ii] = cmt_p + 1;
                }
                jj_p++;
                // ss
                switch (wh0_p) {
                case EVID0_SS2:
#pragma omp atomic write
                  dullSS = 0;
                  if (_writeDT) colI[jj_p][ii] = 2;
                  break;
                case EVID0_SS:
#pragma omp atomic write
                  dullSS = 0;
                  if (_writeDT) colI[jj_p][ii] = 1;
                  break;
                case 40:
#pragma omp atomic write
                  dullRate = 0;
#pragma omp atomic write
                  dullSS = 0;
#pragma omp atomic write
                  dullIi = 0;
                  if (_writeDT) colI[jj_p][ii] = 1;
                  break;
                default:
                  if (_writeDT) colI[jj_p][ii] = 0;
                  break;
                }
                jj_p++;
              }
            } else {
              // !nmevid
              if (_writeDT) colI[jj_p][ii] = evid_p;
              jj_p++;
              if (isObs(evid_p)) {
                if (_writeDT) colR[jj_p][ii] = NA_REAL;
              } else {
                if (_writeDT) colR[jj_p][ii] = getDoseNumber(ind, di_p);
                di_p++;
              }
              jj_p++;
            }
            if (nmevid && isDose(evid_p)) {
              double curIi   = getIiNumber(ind, di_p);
              if (curIi != 0) {
#pragma omp atomic write
                dullIi = 0;
              }
              double curAmt_r = getDoseNumber(ind, di_p++);
              if (!_writeDT) { jj_p += 4; } else switch (ind->whI) {
              case EVIDF_MODEL_RATE_ON:
                colR[jj_p][ii] = curAmt_r; jj_p++;
                colR[jj_p][ii] = -1.0;     jj_p++;
                colR[jj_p][ii] = NA_REAL;  jj_p++;
                colR[jj_p][ii] = curIi;    jj_p++;
                break;
              case EVIDF_MODEL_DUR_ON:
                colR[jj_p][ii] = curAmt_r; jj_p++;
                colR[jj_p][ii] = -2.0;     jj_p++;
                colR[jj_p][ii] = NA_REAL;  jj_p++;
                colR[jj_p][ii] = curIi;    jj_p++;
                break;
              case EVIDF_MODEL_RATE_OFF:
                colR[jj_p][ii] = NA_REAL; jj_p++;
                colR[jj_p][ii] = NA_REAL; jj_p++;
                colR[jj_p][ii] = NA_REAL; jj_p++;
                colR[jj_p][ii] = curIi;   jj_p++;
                break;
              case EVIDF_MODEL_DUR_OFF:
                colR[jj_p][ii] = NA_REAL; jj_p++;
                colR[jj_p][ii] = NA_REAL; jj_p++;
                colR[jj_p][ii] = NA_REAL; jj_p++;
                colR[jj_p][ii] = curIi;   jj_p++;
                break;
              case EVIDF_INF_DUR:
                if (curAmt_r < 0) {
                  colR[jj_p][ii] = NA_REAL; jj_p++;
                  colR[jj_p][ii] = NA_REAL; jj_p++;
                  colR[jj_p][ii] = NA_REAL; jj_p++;
                } else {
                  double curDur = 0.0;
                  for (int jjj = di_p; jjj < ind->ndoses; jjj++) {
                    if (getDoseNumber(ind, jjj) == -curAmt_r) {
                      int nWh=0, nCmt=0, nWh100=0, nWhI=0, nWh0=0;
                      getWh(getEvid(ind, ind->idose[jjj]), &nWh, &nCmt, &nWh100, &nWhI, &nWh0);
#pragma omp atomic write
                      dullRate = 0;
                      if (nWhI == ind->whI && nCmt == ind->cmt) {
                        curDur = getTime_(ind->idose[jjj], ind) - getTime_(ind->ix[i], ind);
                        break;
                      }
                    }
                  }
                  colR[jj_p][ii] = curAmt_r * curDur; jj_p++;
                  colR[jj_p][ii] = NA_REAL;            jj_p++;
                  colR[jj_p][ii] = curDur;             jj_p++;
                }
                colR[jj_p][ii] = curIi; jj_p++;
                break;
              case EVIDF_INF_RATE:
                if (curAmt_r < 0) {
                  colR[jj_p][ii] = NA_REAL; jj_p++;
                  colR[jj_p][ii] = NA_REAL; jj_p++;
                  colR[jj_p][ii] = NA_REAL; jj_p++;
                } else {
                  double curDur = 0.0;
                  for (int jjj = di_p; jjj < ind->ndoses; jjj++) {
                    if (getDoseNumber(ind, jjj) == -curAmt_r) {
                      int nWh=0, nCmt=0, nWh100=0, nWhI=0, nWh0=0;
                      getWh(getEvid(ind, ind->idose[jjj]), &nWh, &nCmt, &nWh100, &nWhI, &nWh0);
#pragma omp atomic write
                      dullRate = 0;
                      if (nWhI == ind->whI && nCmt == ind->cmt) {
                        curDur = getTime_(ind->idose[jjj], ind) - getTime_(ind->ix[i], ind);
                        break;
                      }
                    }
                  }
                  colR[jj_p][ii] = curAmt_r * curDur; jj_p++;
                  colR[jj_p][ii] = curAmt_r;           jj_p++;
                  colR[jj_p][ii] = NA_REAL;            jj_p++;
                }
                colR[jj_p][ii] = curIi; jj_p++;
                break;
              default:
                colR[jj_p][ii] = curAmt_r; jj_p++;
                colR[jj_p][ii] = NA_REAL;  jj_p++;
                colR[jj_p][ii] = NA_REAL;  jj_p++;
                colR[jj_p][ii] = curIi;    jj_p++;
              }
            }
          } else if (nevid2col) {
            colI[jj_p][ii] = evid_p; jj_p++;
          }
          // time
          if (_writeDT) {
            if (evid_p == 3) {
              colR[jj_p][ii] = getTime_(ind->ix[i], ind) + ind->curShift - rx->maxShift;
              if (fabs(colR[jj_p][ii]) < sqrt(DBL_EPSILON)) colR[jj_p][ii] = 0.0;
            } else {
              colR[jj_p][ii] = getTime_(ind->ix[i], ind) + ind->curShift;
            }
          }
          jj_p++;
          // LHS
          if (nlhs) {
            for (j = 0; j < nlhs; j++) {
              if (op->lhs_str[j] == 1) {
                int _lhsVal;
                if (ISNA(ind->lhs[j])) {
                  _lhsVal = NA_INTEGER;
                } else {
                  _lhsVal = (int)(ind->lhs[j]);
                  int _len = lhsLevelCount[j];
                  if (_lhsVal < 1 || _lhsVal > _len) _lhsVal = NA_INTEGER;
                }
                if (colI[jj_p] != nullptr) colI[jj_p][ii] = _lhsVal;
                jj_p++;
              } else {
                if (colR[jj_p] != nullptr) colR[jj_p][ii] = ind->lhs[j];
                jj_p++;
              }
            }
          }
          // States
          if (nPrnState) {
            for (j = 0; j < neq[0]; j++) {
              if (!rmState[j]) {
                colR[jj_p][ii] = (getSolve(i))[j] / scale[j]; jj_p++;
              }
            }
          }
          // Covariates and keep: when covKeepShrunk the columns hold only one
          // sim-block; skip writes for csim > 0 (same values, smaller buffer).
          int didUpdate_p = 0;
          if (add_cov*ncov > 0) {
            if (!covKeepShrunk || csim == 0) {
              _update_par_ptr(curT, solveid, rx, ind->idx);
              didUpdate_p = 1;
              for (j = 0; j < add_cov*ncov; j++) {
                double tmpD = par_ptr_p[op->par_cov[j] - 1];
                if (colType[jj_p] == REALSXP) {
                  colR[jj_p][ii] = tmpD;
                } else {
                  colI[jj_p][ii] = (int)(tmpD);
                }
                jj_p++;
              }
            } else {
              jj_p += add_cov*ncov;
            }
          }
          if (add_cov*ncov0 > 0) {
            if (!covKeepShrunk || csim == 0) {
              for (j = 0; j < add_cov*ncov0; j++) {
                if (colType[jj_p] == REALSXP) {
                  colR[jj_p][ii] = ind->par_ptr[rx->cov0[j]];
                } else {
                  colI[jj_p][ii] = (int)(ind->par_ptr[rx->cov0[j]]);
                }
                jj_p++;
              }
            } else {
              jj_p += add_cov*ncov0;
            }
          }
          if (!covKeepShrunk || csim == 0) {
            if (nkeep && didUpdate_p == 0) _update_par_ptr(curT, solveid, rx, ind->idx);
            for (j = 0; j < nkeep; j++) {
              double _fv = get_fkeep(j, curi + ind->ix[i], ind, curi);
              if (colType[jj_p] == REALSXP) {
                colR[jj_p][ii] = _fv;
              } else if (colType[jj_p] == STRSXP) {
                int _idx = (R_IsNA(_fv) || std::isnan(_fv)) ? 0 : (int)_fv;
                if (_idx < 0 || _idx > strLevelCount[jj_p]) _idx = 0;
                SET_STRING_ELT(colSEXP[jj_p], ii, strLevelCache[jj_p][_idx]);
              } else if (colType[jj_p] == LGLSXP) {
                if (ISNA(_fv) || std::isnan(_fv)) colI[jj_p][ii] = NA_LOGICAL;
                else                              colI[jj_p][ii] = (int)(_fv);
              } else {
                if (ISNA(_fv) || std::isnan(_fv)) colI[jj_p][ii] = NA_INTEGER;
                else                              colI[jj_p][ii] = (int)(_fv);
              }
              jj_p++;
            }
          } else {
            jj_p += nkeep;
          }
          if (doTBS) {
            if (_writeDT) colR[jj_p][ii] = ind->lambda;   jj_p++;
            if (_writeDT) colR[jj_p][ii] = ind->yj;       jj_p++;
            if (_writeDT) colR[jj_p][ii] = ind->logitLow; jj_p++;
            if (_writeDT) colR[jj_p][ii] = ind->logitHi;  jj_p++;
          }
          ii++;
        }
        ind->_newind = 2;
      }
      if (updateErr) {
        for (j = 0; j < errNcol; j++) {
          par_ptr_p[svar[j]] = NA_REAL;
        }
      }
      ind->inLhs = 0;
    } // end for (solveid)
  // Emit bad-dose warnings (R API not allowed inside the fill loop).
  for (int _csub = 0; _csub < nsub; _csub++) {
    rx_solving_options_ind *_ind = &(rx->subjects[_csub]); // csim==0 subjects
    nBadDose = _ind->nBadDose;
    BadDose  = _ind->BadDose;
    if (nBadDose) {
      for (i = 0; i < nBadDose; i++) {
        if (BadDose[i] > op->extraCmt) {
          warning(_("dose to compartment %d ignored (not in system; 'id=%d')"), BadDose[i], _csub+1);
        }
      }
    }
  }
  if (uniformSimBlocks && !hasRuntimeEventMutation) {
    int colAt = nidCols;
    // Helper: directly ALTREP-wrap a pre-sized (one sim block) column.
    auto _wrapDirect = [&](int _c) {
      SEXP cur = VECTOR_ELT(df, _c);
      int _type = TYPEOF(cur);
      RObject out = R_NilValue;
      if (_type == INTSXP) {
        out = rxode2_make_rep_int(cur, nsim);
        Rf_copyMostAttrib(cur, out);
      } else if (_type == LGLSXP) {
        out = rxode2_make_rep_lgl(cur, nsim);
        Rf_copyMostAttrib(cur, out);
      } else if (_type == REALSXP) {
        out = rxode2_make_rep_real(cur, nsim);
        Rf_copyMostAttrib(cur, out);
      } else if (_type == STRSXP) {
        out = rxode2_make_rep_str(cur, nsim);
        Rf_copyMostAttrib(cur, out);
      }
      if (out != R_NilValue) SET_VECTOR_ELT(df, _c, out);
    };
    // Dose/time columns: pre-sized to one block when doseTimeShrunk → direct wrap.
    // Otherwise fall through to rxCanRepBySim for the full-size columns.
    std::vector<int> repCols;
    if (doDose) {
      if (doseTimeShrunk) _wrapDirect(colAt); else repCols.push_back(colAt); colAt++;
      if (nmevid) {
        if (doseTimeShrunk) _wrapDirect(colAt); else repCols.push_back(colAt); colAt++;  // cmt
        if (doseTimeShrunk) _wrapDirect(colAt); else repCols.push_back(colAt); colAt++;  // ss
      }
      if (doseTimeShrunk) _wrapDirect(colAt); else repCols.push_back(colAt); colAt++;    // amt
      if (nmevid) {
        if (doseTimeShrunk) _wrapDirect(colAt); else repCols.push_back(colAt); colAt++;  // rate
        if (doseTimeShrunk) _wrapDirect(colAt); else repCols.push_back(colAt); colAt++;  // dur
        if (doseTimeShrunk) _wrapDirect(colAt); else repCols.push_back(colAt); colAt++;  // ii
      }
    } else if (nevid2col) {
      repCols.push_back(colAt++); // evid — not doseTimeShrunk path
    }
    // time
    if (doseTimeShrunk) _wrapDirect(colAt); else repCols.push_back(colAt);
    // Shared column-start for cov/keep and TBS blocks.
    int _covColStart = colAt + 1 + nlhs + nPrnState;
    int _tbsColStart = _covColStart + ncols2;
    // Cov/keep: pre-sized when covKeepShrunk → direct wrap; otherwise rxCanRepBySim.
    if (!covKeepShrunk && ncols2 > 0) {
      for (int _c = _covColStart; _c < _covColStart + ncols2 && _c < ncol; _c++) {
        repCols.push_back(_c);
      }
    }
    // TBS columns repeat identically across sims (same schedule).
    if (!doseTimeShrunk && doTBS) {
      for (int _c = _tbsColStart; _c < _tbsColStart + 4 && _c < ncol; _c++) {
        repCols.push_back(_c);
      }
    }
    for (unsigned int _k = 0; _k < repCols.size(); ++_k) {
      int _col = repCols[_k];
      SEXP cur = VECTOR_ELT(df, _col);
      if (rxCanRepBySim(cur, rowsPerSimUniform, nsim)) {
        SET_VECTOR_ELT(df, _col, rxRepFromFirstSim(cur, rowsPerSimUniform, nsim));
      }
    }
    if (covKeepShrunk) {
      for (int _c = _covColStart; _c < _covColStart + ncols2 && _c < ncol; _c++) {
        _wrapDirect(_c);
      }
    }
    if (doseTimeShrunk && doTBS) {
      for (int _c = _tbsColStart; _c < _tbsColStart + 4 && _c < ncol; _c++) {
        _wrapDirect(_c);
      }
    }
  }
  IntegerVector sexp_rownames = IntegerVector(2);
  sexp_rownames[0] = NA_INTEGER;
  sexp_rownames[1] = -(int)rx->nr;
  Rf_setAttrib(df, R_RowNamesSymbol, sexp_rownames);
  CharacterVector sexp_colnames = CharacterVector(ncol);
  jj = 0;
  if (sm){
    sexp_colnames[jj] = Rf_mkChar("sim.id");
    jj++;
  }
  // id
  if (md){
    sexp_colnames[jj] = Rf_mkChar("id");
    jj++;
  }
  if (ms) {
    sexp_colnames[jj] = Rf_mkChar("resetno");
    jj++;
  }

  if (doDose){
    sexp_colnames[jj] = Rf_mkChar("evid");
    jj++;
    if (nmevid){
      sexp_colnames[jj] = Rf_mkChar("cmt");
      jj++;
      sexp_colnames[jj] = Rf_mkChar("ss");
      jj++;
    }
    sexp_colnames[jj] = Rf_mkChar("amt");
    jj++;
    if (nmevid){
      sexp_colnames[jj] = Rf_mkChar("rate");
      jj++;
      sexp_colnames[jj] = Rf_mkChar("dur");
      jj++;
      sexp_colnames[jj] = Rf_mkChar("ii");
      jj++;
    }
  } else if (nevid2col) {
    sexp_colnames[jj] = Rf_mkChar("evid");
    jj++;
  }
  sexp_colnames[jj] = Rf_mkChar("time");
  jj++;

  // Put in LHS names
  for (i = 0; i < nlhs; i++){
    sexp_colnames[jj] = STRING_ELT(lhsNames,i);
    jj++;
  }
  // Put in state names
  CharacterVector stateNames = rxStateNames(op->modNamePtr);
  if (nPrnState){
    for (j = 0; j < neq[0]; j++){
      if (!rmState[j]){
        sexp_colnames[jj] = STRING_ELT(stateNames,j);
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
    SET_STRING_ELT(sexp_colnames, jj, Rf_mkChar("rxLambda"));
    jj++;
    SET_STRING_ELT(sexp_colnames, jj, Rf_mkChar("rxYj"));
    jj++;
    SET_STRING_ELT(sexp_colnames, jj, Rf_mkChar("rxLow"));
    jj++;
    SET_STRING_ELT(sexp_colnames, jj, Rf_mkChar("rxHi"));
    jj++;
  }
  Rf_setAttrib(df, R_NamesSymbol, sexp_colnames);
  List df2;
  if (nmevid) {
    int ncol2 = ncol - dullRate - dullDur-dullSS-dullIi;
    df2 = List(ncol2);
    CharacterVector sexp_colnames2 = CharacterVector(ncol2);
    jj = 0;
    kk = 0;
    if (sm){
      SET_STRING_ELT(sexp_colnames2, jj, Rf_mkChar("sim.id"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
    }
    // id
    if (md){
      SET_STRING_ELT(sexp_colnames2, jj, Rf_mkChar("id"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
    }
    if (ms) {
      SET_STRING_ELT(sexp_colnames2, jj, Rf_mkChar("resetno"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++; kk++;
    }
    SET_STRING_ELT(sexp_colnames2, jj, Rf_mkChar("evid"));
    SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
    jj++;kk++;
    SET_STRING_ELT(sexp_colnames2, jj, Rf_mkChar("cmt"));
    SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
    jj++;kk++;
    if (dullSS){
      kk++;
    } else {
      SET_STRING_ELT(sexp_colnames2, jj, Rf_mkChar("ss"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
    }
    SET_STRING_ELT(sexp_colnames2, jj, Rf_mkChar("amt"));
    SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
    jj++;kk++;
    if (dullRate){
      kk++;
    } else {
      SET_STRING_ELT(sexp_colnames2, jj, Rf_mkChar("rate"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
    }
    if (dullDur){
      kk++;
    } else {
      SET_STRING_ELT(sexp_colnames2, jj, Rf_mkChar("dur"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
    }
    if (dullIi){
      kk++;
    } else {
      SET_STRING_ELT(sexp_colnames2, jj, Rf_mkChar("ii"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
    }
    SET_STRING_ELT(sexp_colnames2, jj, Rf_mkChar("time"));
    SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
    jj++;kk++;

    // Put in LHS names
    CharacterVector lhsNames2 = rxLhsNames(op->modNamePtr);
    for (i = 0; i < nlhs; i++){
      SET_STRING_ELT(sexp_colnames2, jj, STRING_ELT(lhsNames2,i));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
    }
    // Put in state names
    CharacterVector stateNames2 = rxStateNames(op->modNamePtr);
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
    CharacterVector paramNames2 = rxParamNames(op->modNamePtr);
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
      SET_STRING_ELT(sexp_colnames2, jj, Rf_mkChar("rxLambda"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
      SET_STRING_ELT(sexp_colnames2, jj, Rf_mkChar("rxYj"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
      SET_STRING_ELT(sexp_colnames2, jj, Rf_mkChar("rxLow"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
      SET_STRING_ELT(sexp_colnames2, jj, Rf_mkChar("rxHi"));
      SET_VECTOR_ELT(df2, jj, VECTOR_ELT(df, kk));
      jj++;kk++;
    }
    Rf_setAttrib(df2, R_NamesSymbol, sexp_colnames2);
    Rf_setAttrib(df2, R_RowNamesSymbol, sexp_rownames);
  } else {
    df2=df;
  }
  return wrap(df2);
}
