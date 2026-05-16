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

extern "C" SEXP getDfLevels(const char *item, rx_solve *rx) {
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
      SEXP lvl = PROTECT(Rf_allocVector(STRSXP, curLen));
      for (int j = 0; j < curLen; j++){
        SET_STRING_ELT(lvl, j,Rf_mkChar(rx->factors.line[base+j]));
      }
      SEXP val = PROTECT(Rf_allocVector(INTSXP, (R_xlen_t)rx->nr));
      Rf_setAttrib(val, R_LevelsSymbol, lvl);
      SEXP cls = PROTECT(Rf_allocVector(STRSXP, 1));
      SET_STRING_ELT(cls, 0, Rf_mkChar("factor"));
      Rf_setAttrib(val,R_ClassSymbol, cls);
      UNPROTECT(3);
      return val;
    }
    base += curLen;
  }
  SEXP val = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t)rx->nr));
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
      // Count observation rows from the grown n_all_times (includes evid_()-pushed observations).
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
  int ncol = ncols+ncols2+nidCols+doseCols+doTBS*4+5*nmevid*doDose+nevid2col;
  List df = List(ncol);//PROTECT(Rf_allocVector(VECSXP,ncol)); pro++;
  for (i = nidCols; i--;){
    df[i] = IntegerVector((R_xlen_t)rx->nr);
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
    df[i++] = IntegerVector((R_xlen_t)rx->nr);
    if (nmevid){
      // cmt
      df[i++] = IntegerVector((R_xlen_t)rx->nr);
      // ss
      df[i++] = IntegerVector((R_xlen_t)rx->nr);
    }
    // amt
    df[i++] = NumericVector((R_xlen_t)rx->nr);
  } else if (nevid2col) {
    df[i++] = IntegerVector((R_xlen_t)rx->nr);
  }
  doseCols += nevid2col;
  CharacterVector paramNames = rxParamNames(op->modNamePtr);
  CharacterVector fkeepNames = get_fkeepn();
  // time comes in here
  df[md + sm +ms + doseCols + 2*nmevid] = NumericVector((R_xlen_t)rx->nr);
  CharacterVector lhsNames = rxLhsNames(op->modNamePtr);
  // time
  int i0 = md + sm + ms + doseCols + 2*nmevid;
  df[i0] = NumericVector((R_xlen_t)rx->nr);
  i0++;

  // nlhs
  for (i = i0; i < i0+nlhs; i++){
    if (op->lhs_str[i-i0] == 1) {
      // factor; from string expression
      df[i] = getDfLevels(CHAR(STRING_ELT(lhsNames, i-i0)), rx);
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
    df[j++] = getDfLevels(charItem, rx);
  }
  par_cov = rx->cov0;
  for (i = 0; i < ncov0*add_cov; i++){
    charItem =CHAR(STRING_ELT(paramNames, par_cov[i]));
    df[j++] = getDfLevels(charItem, rx);
  }
  for (i = 0; i < nkeep; i++){
    charItem = CHAR(STRING_ELT(fkeepNames, i));
    df[j++] = getDfLevels(charItem, rx);
  }
  ncols += ncols2;
  for (i = ncols + doseCols + nidCols + 2*nmevid;
       i < ncols + doseCols + nidCols + nmevid*5 - nkeep;
       i++){
    df[i] = NumericVector((R_xlen_t)rx->nr);
  }
  // keep items
  j = 0;
  for (i = ncols + doseCols + nidCols + nmevid*5 - nkeep;
       i < ncols + doseCols + nidCols + nmevid*5;
       i++) {
    int curType = get_fkeepType(j);
    if (curType == 4) {
      df[i] = assign_fkeepAttr(j, NumericVector((R_xlen_t)rx->nr));
    } else if (curType == 1) {
      df[i] = StringVector((R_xlen_t)rx->nr);
    } else if (curType == 5) {
      df[i] = assign_fkeepAttr(j, LogicalVector((R_xlen_t)rx->nr));
    } else {
      IntegerVector cur((R_xlen_t)rx->nr);
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
    df[i] = NumericVector((R_xlen_t)rx->nr);
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
  // Serial pre-pass: compute per-subject starting row index (ii), error
  // index (kk), and curi offset so the parallel fill knows where each
  // subject writes without a reduction step.
  int nsolve_df = nsim * nsub;
  std::vector<int> subRowStart(nsolve_df + 1, 0);
  std::vector<int> subKkStart(nsolve_df + 1, 0);
  std::vector<int> subCuriStart(nsolve_df + 1, 0);
  // curi resets to 0 at the start of each simulation
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
      // kk is incremented before the subsetEvid continue checks in the fill loop
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
        if (subRowStart[_sid + 1] - subRowStart[_sid] != rows0) useAltrepId = false;
      }
    }
    if (useAltrepId) {
      int jj_alt = 0;
      if (sm) {
        // sim.id: rep(1:nsim, each = nsub * rows0)
        int rows_per_sim = nsub * rows0;
        df[jj_alt] = rxode2_make_seqrep(nsim, rows_per_sim, (R_xlen_t)rx->nr);
        jj_alt++;
      }
      if (md) {
        // id: rep(1:nsub, each = rows0, times = nsim)
        // The formula ((i / rows0) % nsub) + 1 = rx_seqrep with n_vals=nsub,
        // run_len=rows0, total_len=rx->nr covers all nsim repetitions.
        df[jj_alt] = rxode2_make_seqrep(nsub, rows0, (R_xlen_t)rx->nr);
        jj_alt++;
      }
      if (ms) {
        // shift column: resetno changes within a subject — cannot ALTREP, leave as-is
        // (already allocated above as IntegerVector)
      }
    }
    // colI[] was extracted before ALTREP replacement — nullify those entries
    // so the fill loop skips writing into the replaced (now-dead) IntegerVectors.
    int jj_null = 0;
    if (sm) { colI[jj_null] = nullptr; jj_null++; }
    if (md) { colI[jj_null] = nullptr; }
  }

  // Parallel data-frame fill.  Each thread fills a disjoint slice of the
  // output arrays.  No R API calls inside the parallel region.
  bool runSerial = true;
#ifdef _OPENMP
  if (!hasStrCol) {
    runSerial = false;
#pragma omp parallel for num_threads(op->cores) schedule(dynamic,1)
    for (int solveid = 0; solveid < nsolve_df; solveid++) {
      int csim     = solveid / nsub;
      int csub_par = solveid % nsub;
      int ii       = subRowStart[solveid];
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
          if (sm) { if (colI[jj_p]) colI[jj_p][ii] = csim + 1;     jj_p++; }
          if (md) { if (colI[jj_p]) colI[jj_p][ii] = csub_par + 1; jj_p++; }
          if (ms) { colI[jj_p][ii] = resetno_p + 1; jj_p++; }
          if (doDose) {
            if (nmevid) {
              if (isObs(evid_p)) {
                colI[jj_p][ii] = (evid_p >= 10) ? evid_p + 91 : evid_p; jj_p++;
                colI[jj_p][ii] = NA_INTEGER; jj_p++;  // cmt
                colI[jj_p][ii] = 0;          jj_p++;  // ss
                colR[jj_p][ii] = NA_REAL;    jj_p++;  // amt
                colR[jj_p][ii] = NA_REAL;    jj_p++;  // rate
                colR[jj_p][ii] = NA_REAL;    jj_p++;  // dur
                colR[jj_p][ii] = NA_REAL;    jj_p++;  // ii
              } else {
                getWh(evid_p, &wh_p, &cmt_p, &wh100_p, &whI_p, &wh0_p);
                double curAmt = getDoseNumber(ind, di_p);
                if (evid_p == 3) {
                  colI[jj_p][ii] = 3;
                } else if (whI_p == EVIDF_MODEL_RATE_OFF) {
#pragma omp atomic write
                  dullRate = 0;
                  colI[jj_p][ii] = -1;
                } else if (whI_p == EVIDF_MODEL_DUR_OFF) {
#pragma omp atomic write
                  dullRate = 0;
                  colI[jj_p][ii] = -2;
                } else {
                  if (curAmt > 0) {
                    if (whI_p == EVIDF_REPLACE) {
                      colI[jj_p][ii] = 5;
                    } else if (whI_p == EVIDF_MULT) {
                      colI[jj_p][ii] = 6;
                    } else {
                      colI[jj_p][ii] = 1;
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
                      colI[jj_p][ii] = -10;
                    } else if (whI_p == EVIDF_INF_DUR) {
#pragma omp atomic write
                      dullDur = 0;
                      colI[jj_p][ii] = -20;
                    } else if (whI_p == EVIDF_REPLACE) {
                      colI[jj_p][ii] = 5;
                    } else if (whI_p == EVIDF_MULT) {
                      colI[jj_p][ii] = 6;
                    } else {
                      colI[jj_p][ii] = 1;
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
                  colI[jj_p][ii] = NA_INTEGER;
                } else if (wh0_p == 30) {
                  colI[jj_p][ii] = -cmt_p - 1;
                } else {
                  colI[jj_p][ii] = cmt_p + 1;
                }
                jj_p++;
                // ss
                switch (wh0_p) {
                case EVID0_SS2:
#pragma omp atomic write
                  dullSS = 0;
                  colI[jj_p][ii] = 2;
                  break;
                case EVID0_SS:
#pragma omp atomic write
                  dullSS = 0;
                  colI[jj_p][ii] = 1;
                  break;
                case 40:
#pragma omp atomic write
                  dullRate = 0;
#pragma omp atomic write
                  dullSS = 0;
#pragma omp atomic write
                  dullIi = 0;
                  colI[jj_p][ii] = 1;
                  break;
                default:
                  colI[jj_p][ii] = 0;
                  break;
                }
                jj_p++;
              }
            } else {
              // !nmevid
              colI[jj_p][ii] = evid_p; jj_p++;
              colR[jj_p][ii] = isObs(evid_p) ? NA_REAL : getDoseNumber(ind, di_p++); jj_p++;
            }
            if (nmevid && isDose(evid_p)) {
              double curIi   = getIiNumber(ind, di_p);
              if (curIi != 0) {
#pragma omp atomic write
                dullIi = 0;
              }
              double curAmt_r = getDoseNumber(ind, di_p++);
              switch (ind->whI) {
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
          if (evid_p == 3) {
            colR[jj_p][ii] = getTime_(ind->ix[i], ind) + ind->curShift - rx->maxShift;
            if (fabs(colR[jj_p][ii]) < sqrt(DBL_EPSILON)) colR[jj_p][ii] = 0.0;
          } else {
            colR[jj_p][ii] = getTime_(ind->ix[i], ind) + ind->curShift;
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
                colI[jj_p][ii] = _lhsVal; jj_p++;
              } else {
                colR[jj_p][ii] = ind->lhs[j]; jj_p++;
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
          // Covariates
          int didUpdate_p = 0;
          if (add_cov*ncov > 0) {
            _update_par_ptr(curT, solveid, rx, ind->idx);
            didUpdate_p = 1;
            for (j = 0; j < add_cov*ncov; j++) {
              double tmpD = par_ptr_p[op->par_cov[j] - 1];
              if (colType[jj_p] == REALSXP) { colR[jj_p][ii] = tmpD; }
              else                           { colI[jj_p][ii] = (int)(tmpD); }
              jj_p++;
            }
          }
          if (add_cov*ncov0 > 0) {
            for (j = 0; j < add_cov*ncov0; j++) {
              if (colType[jj_p] == REALSXP) { colR[jj_p][ii] = ind->par_ptr[rx->cov0[j]]; }
              else                          { colI[jj_p][ii] = (int)(ind->par_ptr[rx->cov0[j]]); }
              jj_p++;
            }
          }
          if (nkeep && didUpdate_p == 0) _update_par_ptr(curT, solveid, rx, ind->idx);
          for (j = 0; j < nkeep; j++) {
            double _fv = get_fkeep(j, curi + ind->ix[i], ind, curi);
            if (colType[jj_p] == REALSXP) {
              colR[jj_p][ii] = _fv;
            } else if (colType[jj_p] == STRSXP) {
              // Should not reach here: runSerial=false only when !hasStrCol
            } else if (colType[jj_p] == LGLSXP) {
              if (ISNA(_fv) || std::isnan(_fv)) colI[jj_p][ii] = NA_LOGICAL;
              else                              colI[jj_p][ii] = (int)(_fv);
            } else {
              if (ISNA(_fv) || std::isnan(_fv)) colI[jj_p][ii] = NA_INTEGER;
              else                              colI[jj_p][ii] = (int)(_fv);
            }
            jj_p++;
          }
          if (doTBS) {
            colR[jj_p][ii] = ind->lambda;   jj_p++;
            colR[jj_p][ii] = ind->yj;       jj_p++;
            colR[jj_p][ii] = ind->logitLow; jj_p++;
            colR[jj_p][ii] = ind->logitHi;  jj_p++;
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
    } // end parallel for (solveid)
  }
#endif // _OPENMP
  if (runSerial) {
    // Serial fill: uses pre-extracted colR/colI (avoids repeated VECTOR_ELT
    // per cell).  Also handles STRSXP keep columns via SET_STRING_ELT.
    int resetno = 0;
    for (int csim = 0; csim < nsim; csim++) {
      int curi = 0;
      for (csub = 0; csub < nsub; csub++) {
        resetno = 0;
        neq[1] = csub + csim*nsub;
        ind = &(rx->subjects[neq[1]]);
        iniSubject(neq[1], 1, ind, op, rx, update_inis);
        ntimes  = ind->n_all_times;
        par_ptr = ind->par_ptr;
        di = 0;
        for (i = 0; i < ntimes; i++) {
          ind->idx = i;
          if (evid == 3) {
            ind->curShift -= rx->maxShift;
            resetno++;
          }
          double curT = getTime_(ind->ix[ind->idx], ind);
          evid = getEvid(ind, ind->ix[ind->idx]);
          if (evid == 9) continue;
          if (isDose(evid)) {
            getWh(getEvid(ind, ind->ix[i]), &(ind->wh), &(ind->cmt), &(ind->wh100), &(ind->whI), &(ind->wh0));
            switch (ind->whI) {
            case EVIDF_INF_RATE:
            case EVIDF_MODEL_DUR_ON:
            case EVIDF_MODEL_DUR_OFF:
            case EVIDF_MODEL_RATE_ON:
            case EVIDF_MODEL_RATE_OFF:
              dullRate = 0;
              break;
            case EVIDF_INF_DUR:
              dullDur = 0;
              break;
            }
            handleTlastInline(&curT, ind);
          }
          if (updateErr) {
            for (j = 0; j < errNcol; j++) {
              par_ptr[svar[j]] = errs[errNrow*j + kk];
            }
            if ((doDose && evid != 9) || (evid0 == 0 && isObs(evid)) || (evid0 == 1 && evid == 0)) {
              kk = min2(kk + 1, errNrow - 1);
            }
          }
          if (nlhs) {
            calc_lhs(neq[1], curT, getSolve(i), ind->lhs);
          }
          if (subsetEvid == 1) {
            if (isObs(evid) && evid >= 10) continue;
            if (isDose(evid)) {
              getWh(evid, &wh, &cmt, &wh100, &whI, &wh0);
              if (whI == EVIDF_MODEL_RATE_OFF || whI == EVIDF_MODEL_DUR_OFF) {
                dullRate = 0;
                di++;
                continue;
              } else if (whI == EVIDF_INF_RATE || whI == EVIDF_MODEL_RATE_ON || whI == EVIDF_MODEL_DUR_ON) {
                dullRate = 0;
              } else if (whI == EVIDF_INF_DUR) {
                dullDur = 0;
              }
              if (getDoseNumber(ind, di) <= 0) {
                di++;
                continue;
              }
            }
          }
          jj = 0;
          int solveId = csim*nsub + csub;
          if (doDose || (evid0 == 0 && isObs(evid)) || (evid0 == 1 && evid == 0)) {
            if (sm) { if (colI[jj]) colI[jj][ii] = csim + 1;   jj++; }
            if (md) { if (colI[jj]) colI[jj][ii] = csub + 1;   jj++; }
            if (ms) { colI[jj][ii] = resetno + 1; jj++; }
            if (doDose) {
              if (nmevid) {
                if (isObs(evid)) {
                  colI[jj][ii] = (evid >= 10) ? evid + 91 : evid; jj++;
                  colI[jj][ii] = NA_INTEGER; jj++;  // cmt
                  colI[jj][ii] = 0;          jj++;  // ss
                  colR[jj][ii] = NA_REAL;    jj++;  // amt
                  colR[jj][ii] = NA_REAL;    jj++;  // rate
                  colR[jj][ii] = NA_REAL;    jj++;  // dur
                  colR[jj][ii] = NA_REAL;    jj++;  // ii
                } else {
                  getWh(evid, &wh, &cmt, &wh100, &whI, &wh0);
                  double curAmt = getDoseNumber(ind, di);
                  if (evid == 3) {
                    colI[jj][ii] = 3;
                  } else if (whI == EVIDF_MODEL_RATE_OFF) {
                    dullRate = 0;
                    colI[jj][ii] = -1;
                  } else if (whI == EVIDF_MODEL_DUR_OFF) {
                    dullRate = 0;
                    colI[jj][ii] = -2;
                  } else {
                    if (curAmt > 0) {
                      if (whI == EVIDF_REPLACE)     { colI[jj][ii] = 5; }
                      else if (whI == EVIDF_MULT)   { colI[jj][ii] = 6; }
                      else {
                        colI[jj][ii] = 1;
                        if (whI == EVIDF_INF_RATE || whI == EVIDF_MODEL_DUR_ON || whI == EVIDF_MODEL_RATE_ON) {
                          dullRate = 0;
                        } else if (whI == EVIDF_INF_DUR) {
                          dullDur = 0;
                        }
                      }
                    } else {
                      if (whI == EVIDF_INF_RATE) {
                        dullRate = 0; colI[jj][ii] = -10;
                      } else if (whI == EVIDF_INF_DUR) {
                        dullDur = 0;  colI[jj][ii] = -20;
                      } else if (whI == EVIDF_REPLACE) { colI[jj][ii] = 5; }
                      else if (whI == EVIDF_MULT)      { colI[jj][ii] = 6; }
                      else {
                        colI[jj][ii] = 1;
                        if (whI == EVIDF_INF_DUR) dullDur = 0;
                      }
                    }
                  }
                  jj++;  // evid done
                  if (evid == 2 || evid == 3) { colI[jj][ii] = NA_INTEGER; }
                  else if (wh0 == 30)          { colI[jj][ii] = -cmt - 1; }
                  else                         { colI[jj][ii] = cmt + 1; }
                  jj++;  // cmt done
                  switch (wh0) {
                  case EVID0_SS2: dullSS = 0; colI[jj][ii] = 2; break;
                  case EVID0_SS:  dullSS = 0; colI[jj][ii] = 1; break;
                  case 40:
                    dullRate = 0; dullSS = 0; dullIi = 0; colI[jj][ii] = 1; break;
                  default: colI[jj][ii] = 0; break;
                  }
                  jj++;  // ss done
                }
              } else {
                colI[jj][ii] = evid; jj++;
                colR[jj][ii] = isObs(evid) ? NA_REAL : getDoseNumber(ind, di++); jj++;
              }
              if (nmevid && isDose(evid)) {
                double curIi  = getIiNumber(ind, di);
                if (curIi != 0) dullIi = 0;
                double curAmt = getDoseNumber(ind, di++);
                switch (ind->whI) {
                case EVIDF_MODEL_RATE_ON:
                  colR[jj][ii] = curAmt; jj++; colR[jj][ii] = -1.0;   jj++;
                  colR[jj][ii] = NA_REAL; jj++; colR[jj][ii] = curIi; jj++;
                  break;
                case EVIDF_MODEL_DUR_ON:
                  colR[jj][ii] = curAmt; jj++; colR[jj][ii] = -2.0;   jj++;
                  colR[jj][ii] = NA_REAL; jj++; colR[jj][ii] = curIi; jj++;
                  break;
                case EVIDF_MODEL_RATE_OFF:
                  colR[jj][ii] = NA_REAL; jj++; colR[jj][ii] = NA_REAL; jj++;
                  colR[jj][ii] = NA_REAL; jj++; colR[jj][ii] = curIi;  jj++;
                  break;
                case EVIDF_MODEL_DUR_OFF:
                  colR[jj][ii] = NA_REAL; jj++; colR[jj][ii] = NA_REAL; jj++;
                  colR[jj][ii] = NA_REAL; jj++; colR[jj][ii] = curIi;  jj++;
                  break;
                case EVIDF_INF_DUR:
                  if (curAmt < 0) {
                    colR[jj][ii] = NA_REAL; jj++; colR[jj][ii] = NA_REAL; jj++;
                    colR[jj][ii] = NA_REAL; jj++;
                  } else {
                    double curDur = 0.0;
                    for (int jjj = di; jjj < ind->ndoses; jjj++) {
                      if (getDoseNumber(ind, jjj) == -curAmt) {
                        int nWh=0, nCmt=0, nWh100=0, nWhI=0, nWh0=0;
                        getWh(getEvid(ind, ind->idose[jjj]), &nWh, &nCmt, &nWh100, &nWhI, &nWh0);
                        dullRate = 0;
                        if (nWhI == whI && nCmt == cmt) {
                          curDur = getTime_(ind->idose[jjj], ind) - getTime_(ind->ix[i], ind);
                          break;
                        }
                      }
                    }
                    colR[jj][ii] = curAmt*curDur; jj++;
                    colR[jj][ii] = NA_REAL;        jj++;
                    colR[jj][ii] = curDur;         jj++;
                  }
                  colR[jj][ii] = curIi; jj++;
                  break;
                case EVIDF_INF_RATE:
                  if (curAmt < 0) {
                    colR[jj][ii] = NA_REAL; jj++; colR[jj][ii] = NA_REAL; jj++;
                    colR[jj][ii] = NA_REAL; jj++;
                  } else {
                    double curDur = 0.0;
                    for (int jjj = di; jjj < ind->ndoses; jjj++) {
                      if (getDoseNumber(ind, jjj) == -curAmt) {
                        int nWh=0, nCmt=0, nWh100=0, nWhI=0, nWh0=0;
                        getWh(getEvid(ind, ind->idose[jjj]), &nWh, &nCmt, &nWh100, &nWhI, &nWh0);
                        dullRate = 0;
                        if (nWhI == whI && nCmt == cmt) {
                          curDur = getTime_(ind->idose[jjj], ind) - getTime_(ind->ix[i], ind);
                          break;
                        }
                      }
                    }
                    colR[jj][ii] = curAmt*curDur; jj++;
                    colR[jj][ii] = curAmt;         jj++;
                    colR[jj][ii] = NA_REAL;        jj++;
                  }
                  colR[jj][ii] = curIi; jj++;
                  break;
                default:
                  colR[jj][ii] = curAmt;  jj++; colR[jj][ii] = NA_REAL; jj++;
                  colR[jj][ii] = NA_REAL; jj++; colR[jj][ii] = curIi;  jj++;
                }
              }
            } else if (nevid2col) {
              colI[jj][ii] = evid; jj++;
            }
            // time
            if (evid == 3) {
              colR[jj][ii] = getTime_(ind->ix[i], ind) + ind->curShift - rx->maxShift;
              if (fabs(colR[jj][ii]) < sqrt(DBL_EPSILON)) colR[jj][ii] = 0.0;
            } else {
              colR[jj][ii] = getTime_(ind->ix[i], ind) + ind->curShift;
            }
            jj++;
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
                  colI[jj][ii] = _lhsVal; jj++;
                } else {
                  colR[jj][ii] = ind->lhs[j]; jj++;
                }
              }
            }
            // States
            if (nPrnState) {
              for (j = 0; j < neq[0]; j++) {
                if (!rmState[j]) {
                  colR[jj][ii] = (getSolve(i))[j] / scale[j]; jj++;
                }
              }
            }
            // Covariates
            int didUpdate = 0;
            if (add_cov*ncov > 0) {
              _update_par_ptr(curT, solveId, rx, ind->idx);
              didUpdate = 1;
              for (j = 0; j < add_cov*ncov; j++) {
                double tmpD = par_ptr[op->par_cov[j] - 1];
                if (colType[jj] == REALSXP) { colR[jj][ii] = tmpD; }
                else                        { colI[jj][ii] = (int)(tmpD); }
                jj++;
              }
            }
            if (add_cov*ncov0 > 0) {
              for (j = 0; j < add_cov*ncov0; j++) {
                if (colType[jj] == REALSXP) { colR[jj][ii] = ind->par_ptr[rx->cov0[j]]; }
                else                        { colI[jj][ii] = (int)(ind->par_ptr[rx->cov0[j]]); }
                jj++;
              }
            }
            if (nkeep && didUpdate == 0) _update_par_ptr(curT, solveId, rx, ind->idx);
            for (j = 0; j < nkeep; j++) {
              if (colType[jj] == REALSXP) {
                colR[jj][ii] = get_fkeep(j, curi + ind->ix[i], ind, curi);
              } else if (colType[jj] == STRSXP) {
                SET_STRING_ELT(VECTOR_ELT(df, jj), ii,
                               get_fkeepChar(j, get_fkeep(j, curi + ind->ix[i], ind, curi)));
              } else if (colType[jj] == LGLSXP) {
                double curD = get_fkeep(j, curi + ind->ix[i], ind, curi);
                if (ISNA(curD) || std::isnan(curD)) colI[jj][ii] = NA_LOGICAL;
                else                                colI[jj][ii] = (int)(curD);
              } else {
                double curD = get_fkeep(j, curi + ind->ix[i], ind, curi);
                if (ISNA(curD) || std::isnan(curD)) colI[jj][ii] = NA_INTEGER;
                else                                colI[jj][ii] = (int)(curD);
              }
              jj++;
            }
            if (doTBS) {
              colR[jj][ii] = ind->lambda;   jj++;
              colR[jj][ii] = ind->yj;       jj++;
              colR[jj][ii] = ind->logitLow; jj++;
              colR[jj][ii] = ind->logitHi;  jj++;
            }
            ii++;
          }
          ind->_newind = 2;
        }
        curi += ntimes;
        nBadDose = ind->nBadDose;
        BadDose  = ind->BadDose;
        if (nBadDose && csim == 0) {
          for (i = 0; i < nBadDose; i++) {
            if (BadDose[i] > op->extraCmt) {
              warning(_("dose to compartment %d ignored (not in system; 'id=%d')"), BadDose[i], csub+1);
            }
          }
        }
        if (updateErr) {
          for (j = 0; j < errNcol; j++) {
            par_ptr[svar[j]] = NA_REAL;
          }
        }
        ind->inLhs = 0;
      }
    }
  } // end if (runSerial)
  // Emit bad-dose warnings after the parallel region (R API not allowed inside).
  if (!runSerial) {
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
