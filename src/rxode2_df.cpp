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
#include <rxode2parseVer.h>
#include <rxode2parseHandleEvid.h>
#include <rxode2parseGetTime.h>
#include "par_solve.h"
#include <Rcpp.h>
#include "strncmp.h"
#define rxModelVars(a) rxModelVar_s(a)
#define min2( a , b )  ( (a) < (b) ? (a) : (b) )
void resetSolveLinB();
using namespace Rcpp;
using namespace arma;

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

#include "rxData.h"

extern t_update_inis update_inis;
extern t_calc_lhs calc_lhs;

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
      SEXP lvl = PROTECT(Rf_allocVector(STRSXP, curLen));
      for (int j = 0; j < curLen; j++){
        SET_STRING_ELT(lvl, j,Rf_mkChar(rx->factors.line[base+j]));
      }
      SEXP val = PROTECT(Rf_allocVector(INTSXP, rx->nr));
      Rf_setAttrib(val, R_LevelsSymbol, lvl);
      SEXP cls = PROTECT(Rf_allocVector(STRSXP, 1));
      SET_STRING_ELT(cls, 0, Rf_mkChar("factor"));
      Rf_setAttrib(val,R_ClassSymbol, cls);
      UNPROTECT(3);
      return val;
    }
    base += curLen;
  }
  SEXP val = PROTECT(Rf_allocVector(REALSXP, rx->nr));
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
  int nsim = rx->nsim;
  int nall = rx->nall - rx->nevid9;
  int errNcol = rxGetErrsNcol();
  int errNrow = rxGetErrsNrow();
  if (op->nsvar != errNcol){
    rxSolveFreeC();
    Rf_errorcall(R_NilValue, _("The simulated residual errors do not match the model specification (%d=%d)"),op->nsvar, errNcol);
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
  if (op->badSolve){
    if (op->naTime){
      rxSolveFreeC();
      Rf_errorcall(R_NilValue, "%s", _("'alag(.)'/'rate(.)'/'dur(.)' cannot depend on the state values"));
    }
    if (nidCols == 0){
      for (int solveid = 0; solveid < rx->nsub * rx->nsim; solveid++){
        rx_solving_options_ind *indE = &(rx->subjects[solveid]);
        if (indE->err != 0) {
          printErr(indE->err, indE->id);
        }
      }
      rxSolveFreeC();
      Rf_errorcall(R_NilValue, "%s", _("could not solve the system"));
    } else {
      warning(_("some ID(s) could not solve the ODEs correctly; These values are replaced with 'NA'"));
    }
  }
  int ncol = ncols+ncols2+nidCols+doseCols+doTBS*4+5*nmevid*doDose+nevid2col;
  List df = List(ncol);//PROTECT(Rf_allocVector(VECSXP,ncol)); pro++;
  for (i = nidCols; i--;){
    df[i] = IntegerVector(rx->nr);
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
    df[i++] = IntegerVector(rx->nr);
    if (nmevid){
      // cmt
      df[i++] = IntegerVector(rx->nr);
      // ss
      df[i++] = IntegerVector(rx->nr);
    }
    // amt
    df[i++] = NumericVector(rx->nr);
  } else if (nevid2col) {
    df[i++] = IntegerVector(rx->nr);
  }
  doseCols += nevid2col;
  CharacterVector paramNames = rxParamNames(op->modNamePtr);
  CharacterVector fkeepNames = get_fkeepn();
  for (i = md + sm + ms + doseCols + 2*nmevid; i < ncols + doseCols + nidCols + 2*nmevid; i++){
    df[i] = NumericVector(rx->nr);
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
    df[i] = NumericVector(rx->nr);
  }
  // keep items
  j = 0;
  for (i = ncols + doseCols + nidCols + nmevid*5 - nkeep;
       i < ncols + doseCols + nidCols + nmevid*5;
       i++) {
    int curType = get_fkeepType(j);
    if (curType == 4) {
      df[i] = assign_fkeepAttr(j, NumericVector(rx->nr));
    } else if (curType == 1) {
      df[i] = assign_fkeepAttr(j, StringVector(rx->nr));
      df[i] = StringVector(rx->nr);
    } else if (curType == 5) {
      df[i] = assign_fkeepAttr(j, LogicalVector(rx->nr));
    } else {
      IntegerVector cur(rx->nr);
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
    df[i] = NumericVector(rx->nr);
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
      for (i = 0; i < ntimes; i++){
        ind->idx = i;
        if (evid == 3) {
          ind->curShift -= rx->maxShift;
          resetno++;
        }
        double curT = getTime_(ind->ix[ind->idx], ind);
        evid = getEvid(ind, ind->ix[ind->idx]);
        if (evid == 9) continue;
        if (isDose(evid)){
          getWh(getEvid(ind, ind->ix[i]), &(ind->wh), &(ind->cmt), &(ind->wh100), &(ind->whI), &(ind->wh0));
          switch (ind->whI) {
          case EVIDF_INF_RATE:
          case EVIDF_MODEL_DUR_ON:
          case EVIDF_MODEL_DUR_OFF:
          case EVIDF_MODEL_RATE_ON:
          case EVIDF_MODEL_RATE_OFF:
            dullRate=0;
            break;
          case EVIDF_INF_DUR:
            dullDur=0;
            break;
          }
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
            kk=min2(kk+1, errNrow-1);
          }
        }
        if (nlhs){
          calc_lhs(neq[1], curT, getSolve(i), ind->lhs);
        }
        if (subsetEvid == 1){
          if (isObs(evid) && evid >= 10) continue;
          if (isDose(evid)){
            getWh(evid, &wh, &cmt, &wh100, &whI, &wh0);
            if (whI == EVIDF_MODEL_RATE_OFF || whI == EVIDF_MODEL_DUR_OFF){
              // modeled duration is in the RATE column like NONMEM datsets
              dullRate=0;
              di++;
              continue;
            } else if (whI == EVIDF_INF_RATE || whI == EVIDF_MODEL_RATE_ON || whI == EVIDF_MODEL_DUR_ON) {
              dullRate = 0;
            } else if (whI == EVIDF_INF_DUR) {
              dullDur = 0;
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
                if (whI == EVIDF_MODEL_RATE_OFF){
                  dullRate=0;
                  dfi[ii] = -1;
                } else if (whI == EVIDF_MODEL_DUR_OFF){
                  dullRate=0; // rate specifies modeled duration
                  dfi[ii] = -2; // evid
                } else {
                  if (curAmt > 0) {
                    if (whI == EVIDF_REPLACE){
                      dfi[ii] = 5;
                    } else if (whI == EVIDF_MULT){
                      dfi[ii] = 6;
                    } else {
                      dfi[ii] = 1; // evid
                      if (whI == EVIDF_INF_RATE || whI == EVIDF_MODEL_DUR_ON || whI == EVIDF_MODEL_RATE_ON) {
                        dullRate = 0;
                      } else if (whI == EVIDF_INF_DUR) {
                        dullDur = 0;
                      }
                    }
                  } else {
                    if (whI == EVIDF_INF_RATE){
                      dullRate=0;
                      dfi[ii] = -10; // evid
                    } else if (whI == EVIDF_INF_DUR) {
                      dullDur=0;
                      dfi[ii] = -20; // evid
                    } else if (whI == EVIDF_REPLACE){
                      dfi[ii] = 5;
                    } else if (whI == EVIDF_MULT){
                      dfi[ii] = 6;
                    } else {
                      dfi[ii] = 1;
                      if (whI == EVIDF_INF_DUR) {
                        dullDur = 0;
                      }
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
                case EVID0_SS2:
                  dullSS=0;
                  dfi[ii] = 2;
                  break;
                case EVID0_SS:
                  dullSS=0;
                  dfi[ii] = 1;
                  break;
                case 40:
                  dullRate=0;
                  dullSS=0;
                  dullIi=0;
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
              case EVIDF_MODEL_RATE_ON: // modeled rate
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
              case EVIDF_MODEL_DUR_ON: // modeled duration
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
              case EVIDF_MODEL_RATE_OFF: // End modeled rate
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
              case EVIDF_MODEL_DUR_OFF: // end modeled duration
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
              case EVIDF_INF_DUR: // Infusion specified by dur
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
                      getWh(getEvid(ind, ind->idose[jjj]), &nWh, &nCmt, &nWh100, &nWhI, &nWh0);
                      dullRate=0;
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
              case EVIDF_INF_RATE: // Infusion specified by rate
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
                      getWh(getEvid(ind, ind->idose[jjj]), &nWh, &nCmt, &nWh100, &nWhI, &nWh0);
                      dullRate=0;
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
            } else if (TYPEOF(tmp) == STRSXP){
              SET_STRING_ELT(tmp, ii, get_fkeepChar(j, get_fkeep(j, curi + ind->ix[i], ind)));
            } else if (TYPEOF(tmp) == LGLSXP) {
              // Everything here is double
              dfi = LOGICAL(tmp);
              double curD = get_fkeep(j, curi + ind->ix[i], ind);
              if (ISNA(curD) || std::isnan(curD)) {
                dfi[ii] = NA_LOGICAL;
              } else {
                dfi[ii] = (int) (curD);
              }
            } else {
              dfi = INTEGER(tmp);
              /* if (j == 0) RSprintf("j: %d, %d; %f\n", j, i, get_fkeep(j, curi + i)); */
              // is this ntimes = nAllTimes or nObs time for this subject...?
              double curD = get_fkeep(j, curi + ind->ix[i], ind);
              if (ISNA(curD) || std::isnan(curD)) {
                dfi[ii] = NA_INTEGER;
              } else {
                dfi[ii] = (int) (curD);
              }
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
  IntegerVector sexp_rownames = IntegerVector(2);
  sexp_rownames[0] = NA_INTEGER;
  sexp_rownames[1] = -rx->nr;
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
  CharacterVector lhsNames = rxLhsNames(op->modNamePtr);
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
