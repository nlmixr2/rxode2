#ifndef __genModelVars_H__
#define __genModelVars_H__
#pragma once
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>   /* dj: import intptr_t */
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rmath.h>
#include <unistd.h>
#include <errno.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2parse", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif
#include "../inst/include/rxode2parse.h"
#include "../inst/include/rxode2parseSbuf.h"
#include "tran.h"
#include "../inst/include/rxode2parseVer.h"

static inline SEXP calcSLinCmt(void) {
  SEXP sLinCmt = PROTECT(Rf_allocVector(INTSXP,12));
  INTEGER(sLinCmt)[0] = tb.ncmt;
  INTEGER(sLinCmt)[1] = tb.hasKa;
  INTEGER(sLinCmt)[2] = tb.linB;
  INTEGER(sLinCmt)[3] = tb.maxeta;
  INTEGER(sLinCmt)[4] = tb.maxtheta;
  INTEGER(sLinCmt)[6] = tb.linCmtN;
  INTEGER(sLinCmt)[7] = tb.linCmtFlg;
  INTEGER(sLinCmt)[8] = tb.nInd;
  INTEGER(sLinCmt)[9] = tb.simflg;
  INTEGER(sLinCmt)[10]= tb.thread;
  INTEGER(sLinCmt)[11]= tb.nLlik;

  SEXP sLinCmtN = PROTECT(Rf_allocVector(STRSXP, 12));
  SET_STRING_ELT(sLinCmtN, 0, mkChar("ncmt"));
  SET_STRING_ELT(sLinCmtN, 1, mkChar("ka"));
  SET_STRING_ELT(sLinCmtN, 2, mkChar("linB"));
  SET_STRING_ELT(sLinCmtN, 3, mkChar("maxeta"));
  SET_STRING_ELT(sLinCmtN, 4, mkChar("maxtheta"));
  SET_STRING_ELT(sLinCmtN, 5, mkChar("hasCmt"));
  SET_STRING_ELT(sLinCmtN, 6, mkChar("linCmt"));
  SET_STRING_ELT(sLinCmtN, 7, mkChar("linCmtFlg"));
  SET_STRING_ELT(sLinCmtN, 8, mkChar("nIndSim"));
  SET_STRING_ELT(sLinCmtN, 9, mkChar("simflg"));
  SET_STRING_ELT(sLinCmtN, 10, mkChar("thread"));
  SET_STRING_ELT(sLinCmtN, 11, mkChar("nLlik"));
  Rf_setAttrib(sLinCmt,   R_NamesSymbol, sLinCmtN);
  UNPROTECT(2);
  return(sLinCmt);
}

static inline SEXP calcVersionInfo(void) {
  SEXP version  = PROTECT(Rf_allocVector(STRSXP, 3));
  SEXP versionn = PROTECT(Rf_allocVector(STRSXP, 3));

  SET_STRING_ELT(versionn,0,mkChar("version"));
  SET_STRING_ELT(versionn,1,mkChar("repo"));
  SET_STRING_ELT(versionn,2,mkChar("md5"));

  SET_STRING_ELT(version,0,mkChar(__VER_ver__));
  SET_STRING_ELT(version,1,mkChar(__VER_repo__));
  SET_STRING_ELT(version,2,mkChar(__VER_md5__));
  Rf_setAttrib(version,   R_NamesSymbol, versionn);
  UNPROTECT(2);
  return version;
}

static inline void calcNparamsNlhsNslhs(void) {
  int sli=0, li=0, pi=0;
  for (int i=0; i<NV; i++) {
    int islhs = tb.lh[i];
    if (islhs>1 &&
        islhs != isLhsStateExtra &&
        islhs != isLHSparam &&
        islhs != isSuppressedLHS &&
        islhs != isLHSstr &&
        islhs != isSuppressedLHSstr) {
      continue;      /* is a state var */
    }
    if (islhs == isSuppressedLHS ||
        islhs == isSuppressedLHSstr){
      sli++;
    } else if (islhs == isLHS ||
               islhs == isLHSstr ||
               islhs == isLhsStateExtra ||
               islhs == isLHSparam ||
               islhs == isLHSstr){
      li++;
      if (islhs == isLHSparam) pi++;
    } else {
      pi++;
    }
  }
  tb.pi=pi;
  tb.li=li;
  tb.sli=sli;
}

static inline void calcNextra(void) {
  int offCmt=0,nExtra = 0;
  char *buf, buf2[200];
  for (int i = 0; i < tb.statei; i++){
    if (offCmt == 0 && tb.idu[i] == 0){
      offCmt = 1;
      nExtra++;
      buf=tb.ss.line[tb.di[i]];
    } else if (offCmt == 1 && tb.idu[i] == 1){
      // There is an compartment that doesn't have a derivative
      if (tb.linCmt == 0){
        char *v = rc_dup_str(buf, 0);
        snprintf(buf2, 200, "compartment '%s' needs differential equations defined", v);
        updateSyntaxCol();
        trans_syntax_error_report_fn0(buf2);
      } else if (!strcmp("depot", buf) || !strcmp("central", buf)) {
      } else {
        char *v = rc_dup_str(buf, 0);
        snprintf(buf2, 200, _("compartment '%s' needs differential equations defined"), v);
        updateSyntaxCol();
        trans_syntax_error_report_fn0(buf2);
      }
    } else if (offCmt == 1 && tb.idu[i] == 0){
      nExtra++;
    }
  }
  tb.nExtra=nExtra;
}

static inline void calcExtracmt(void) {
  extraCmt = 0;
  if (tb.linCmt){
    if (tb.hasKa){
      extraCmt=2;
    } else {
      extraCmt=1;
    }
    if (tb.hasDepotCmt == -1){
      trans_syntax_error_report_fn0(_("'cmt(depot)' does not work with 'linCmt()'"));
    }
    if (tb.hasCentralCmt == -1) {
      trans_syntax_error_report_fn0("'cmt(central)' does not work with 'linCmt()'");
    }
  }
}

static inline SEXP calcIniVals(void) {
  int pro=0;
  SEXP inin  = PROTECT(Rf_allocVector(STRSXP, tb.isPi + tb.ini_i)); pro++;
  SEXP ini   = PROTECT(Rf_allocVector(REALSXP, tb.isPi + tb.ini_i)); pro++;
  char *buf;
  for (int i=tb.isPi + tb.ini_i;i--;) REAL(ini)[i] = NA_REAL;
  int ini_i=0;
  int redo = 0;
  for (int i = 0; i < NV; i++){
    buf=tb.ss.line[i];
    if (tb.ini[i] == 1 && tb.lh[i] != isLHS){
      if (tb.isPi && !strcmp("pi", buf)) {
        redo=1;
        tb.isPi=0;
        break;
      }
      SET_STRING_ELT(inin,ini_i,mkChar(buf));
      REAL(ini)[ini_i++] = tb.iniv[i];
    }
  }
  if (tb.isPi){
    SET_STRING_ELT(inin,ini_i,mkChar("pi"));
    REAL(ini)[ini_i++] = M_PI;
  } else if (redo){
    inin  = PROTECT(Rf_allocVector(STRSXP, tb.ini_i));pro++;
    ini   = PROTECT(Rf_allocVector(REALSXP, tb.ini_i));pro++;
    for (int i = tb.ini_i; i--;) REAL(ini)[i] = NA_REAL;
    ini_i=0;
    for (int i = 0; i < NV; i++){
      buf=tb.ss.line[i];
      if (tb.ini[i] == 1 && tb.lh[i] != isLHS){
        if (tb.isPi && !strcmp("pi", buf)) {
          redo=1;
          tb.isPi=0;
          break;
        }
        SET_STRING_ELT(inin,ini_i,mkChar(buf));
        REAL(ini)[ini_i++] = tb.iniv[i];
      }
    }
  }
  tb.ini_i = ini_i;

  Rf_setAttrib(ini,   R_NamesSymbol, inin);
  UNPROTECT(pro);
  return ini;
}

SEXP orderForderS1(SEXP ordIn);

static inline int sortStateVectorsErrHandle(int prop, int pass, int i) {
  if (prop == 0 || pass == 1) {
    return 1;
  }
  if ((prop & prop0) != 0) {
    sAppend(&sbt, "'%s(0)', ", tb.ss.line[tb.di[i]]);
  }
  if ((prop & propF) != 0) {
    sAppend(&sbt, "'f(%s)', ", tb.ss.line[tb.di[i]]);
  }
  if ((prop & propAlag) != 0) {
    sAppend(&sbt, "'alag(%s)', ", tb.ss.line[tb.di[i]]);
  }
  if ((prop & propRate) != 0) {
    sAppend(&sbt, "'rate(%s)', ", tb.ss.line[tb.di[i]]);
  }
  if ((prop & propDur) != 0) {
    sAppend(&sbt, "'dur(%s)', ", tb.ss.line[tb.di[i]]);
  }
  // Take off trailing "',
  sbt.o -= 2;
  sbt.s[sbt.o] = 0;
  sAppend(&sbt, " present, but d/dt(%s) not defined\n", tb.ss.line[tb.di[i]]);
  return 0;
}

static inline SEXP sortStateVectors(SEXP ordS) {
  int *ord = INTEGER(ordS);
  for (int i = 0; i < Rf_length(ordS); i++) {
    ord[i] = 0; // explicitly initialize to avoid valgrind warning
  }
  sbt.o = 0; // we can use sbt.o since all the code has already been output
  sbt.s[0] = 0;
  for (int i = 0; i < tb.de.n; i++) {
    int cur = tb.didx[i];
    int prop = tb.dprop[i];
    int pass = 0;
    if (tb.linCmt){
      if (tb.hasDepotCmt == 1 && !strcmp("depot", tb.ss.line[tb.di[i]])){
        pass = 1;
      } else if ((tb.hasCentralCmt == 1 || tb.hasDepotCmt == 1)  &&
                 !strcmp("central", tb.ss.line[tb.di[i]])) {
        pass = 1;
      }
    }
    if (cur == 0) {
      // This has a property without an ODE or cmt() statement; should error here.
      if (sortStateVectorsErrHandle(prop, pass, i)) continue;
    } else if (cur < 0) {
      // This is a compartment only defined by CMT() and is used for
      // dvid ordering, no properties should be defined.
      ord[i] = -cur;
      if (sortStateVectorsErrHandle(prop, pass, i)) continue;
    } else {
      ord[i] = cur;
    }
  }
  if (sbt.o != 0) {
    sbt.o--; // remove last newline
    sbt.s[sbt.o] = 0;
    sPrint(&_gbuf, "%s", sbt.s);
    return R_NilValue;
  }
  return orderForderS1(ordS);
}

static inline void populateStateVectors(SEXP state, SEXP sens, SEXP normState, int *stateRm, SEXP extraState, SEXP stateProp, SEXP sensProp, SEXP normProp, int *ordFp) {
  int k=0, j=0, m=0, p=0;
  char *buf;
  int *statePropI = INTEGER(stateProp);
  int *sensPropI = INTEGER(sensProp);
  int *normPropI = INTEGER(normProp);
  for (int i=0; i<tb.de.n; i++) {                     /* name state vars */
    buf=tb.ss.line[tb.di[ordFp[i]-1]] ;
    if (tb.idu[i] == 1){
      if (strncmp(buf,"rx__sens_", 9) == 0){
        statePropI[k] = tb.dprop[ordFp[i]-1];
        sensPropI[j] = tb.dprop[ordFp[i]-1];
        SET_STRING_ELT(sens,j++,mkChar(buf));
        SET_STRING_ELT(state,k++,mkChar(buf));
        stateRm[k-1]=tb.idi[ordFp[i]-1];
      } else {
        statePropI[k] = tb.dprop[ordFp[i]-1];
        normPropI[m] = tb.dprop[ordFp[i]-1];
        SET_STRING_ELT(normState,m++,mkChar(buf));
        SET_STRING_ELT(state,k++,mkChar(buf));
        stateRm[k-1]=tb.idi[ordFp[i]-1];
      }
    } else {
      SET_STRING_ELT(extraState, p++, mkChar(buf));
    }
  }
}

static inline void populateDfdy(SEXP dfdy) {
  char *df, *dy;
  for (int i=0; i<tb.ndfdy; i++) {                     /* name state vars */
    df=tb.ss.line[tb.df[i]];
    dy=tb.ss.line[tb.dy[i]];
    int foundIt=0;
    for (int j = 1; j <= tb.maxtheta;j++){
      sPrint(&_bufw,"_THETA_%d_",j);
      if (!strcmp(dy,_bufw.s)){
        sPrint(&_bufw,"THETA[%d]",j);
        foundIt=1;
        break;
      }
    }
    if (!foundIt){
      for (int j = 1; j <= tb.maxeta;j++){
        sPrint(&_bufw,"_ETA_%d_",j);
        if (!strcmp(dy,_bufw.s)){
          sPrint(&_bufw,"ETA[%d]",j);
        }
      }
    }
    if (!foundIt){
      sClear(&_bufw);
      sPrint(&_bufw,"%s",dy);
    }
    sPrint(&_bufw2,"df(%s)/dy(%s)",df,_bufw.s);
    SET_STRING_ELT(dfdy,i,mkChar(_bufw2.s));
  }
}

static inline int assertStateCannotHaveDiff(int islhs, int i, char *buf) {
  if (islhs>1 && islhs != isLhsStateExtra && islhs != isLHSparam &&
      islhs != isLHSstr) {
    if (tb.lag[i] != 0){
      buf=tb.ss.line[i];
      if (islhs == isState){
        sPrint(&_bufw, _("state '%s': 'lag', 'lead', 'first', 'last', 'diff' not legal"), buf);
        trans_syntax_error_report_fn0(_bufw.s);
      } else if (islhs == 10 || islhs == 11){
        sPrint(&_bufw, _("suppress '%s': 'lag', 'lead', 'first', 'last', 'diff' not legal"), buf);
        trans_syntax_error_report_fn0(_bufw.s);
      }
    }
    return 1;
  }
  return 0;
}

static inline int setLhsAndDualLhsParam(int islhs, SEXP lhs, SEXP params, char *buf,
                                        int *li, int *pi, SEXP lhsStr) {
  if (islhs == isLHS || islhs == isLHSstr ||
      islhs == isLhsStateExtra || islhs == isLHSparam) {
    SET_STRING_ELT(lhs, li[0], mkChar(buf));
    INTEGER(lhsStr)[li[0]] = islhs == isLHSstr;
    li[0] = li[0]+1;
    if (islhs == isLHSparam) {
      if (!strcmp("CMT", buf)) {
        tb.hasCmt = 1;
      }
      SET_STRING_ELT(params, pi[0], mkChar(buf));
      pi[0] = pi[0]+1;
    }
    return 1;
  }
  return 0;
}

static inline void paramSubThetaEtaToBufw(char *buf) {
  int foundIt=0;
  for (int j = 1; j <= tb.maxtheta;j++){
    sPrint(&_bufw,"_THETA_%d_",j);
    if (!strcmp(buf, _bufw.s)){
      sPrint(&_bufw,"THETA[%d]",j);
      foundIt=1;
      break;
    }
  }
  if (!foundIt){
    for (int j = 1; j <= tb.maxeta;j++){
      sPrint(&_bufw,"_ETA_%d_",j);
      if (!strcmp(buf, _bufw.s)){
        sPrint(&_bufw,"ETA[%d]",j);
        foundIt=1;
        break;
      }
    }
  }
  if (!foundIt){
    sPrint(&_bufw, "%s", buf);
  }
  if (!strcmp("CMT", _bufw.s)) {
    tb.hasCmt = 1;
  }
}

static inline void assertLhsAndDualLhsDiffNotLegal(int islhs, int i, char *buf) {
  if (tb.lag[i] != 0){
    if (islhs == isLHSparam){
      sPrint(&_bufw, _("redefined '%s': 'lag', 'lead', 'first', 'last', 'diff' not legal"), buf);
      trans_syntax_error_report_fn0(_bufw.s);
    } else if (islhs == isLHS && tb.lag[i] != 1){
      sPrint(&_bufw, _("lhs '%s': only 'lag(%s,1)' and 'diff(%s,1)' supported"), buf, buf, buf);
      trans_syntax_error_report_fn0(_bufw.s);
    }
  }
}

static inline void populateParamsLhsSlhs(SEXP params, SEXP lhs, SEXP slhs, int *interp, SEXP lhsStr) {
  int li=0, pi=0, sli = 0;
  char *buf;
  for (int i=0; i<NV; i++) {
    int islhs = tb.lh[i];
    if (islhs == isSuppressedLHS || islhs == isSuppressedLHSstr){
      SET_STRING_ELT(slhs, sli++, mkChar(tb.ss.line[i]));
    }
    buf=tb.ss.line[i];

    if (assertStateCannotHaveDiff(islhs, i, buf)) continue;
    assertLhsAndDualLhsDiffNotLegal(islhs, i, buf);
    /* is a state var */
    if (!setLhsAndDualLhsParam(islhs, lhs, params, buf, &li, &pi, lhsStr)) {
      paramSubThetaEtaToBufw(buf);
      interp[pi] = tb.interp[i] + 1; // Makes into a legible factor
      SET_STRING_ELT(params, pi++, mkChar(_bufw.s));
    }
  }
}

SEXP generateModelVars(void);

#endif  // __genModelVars_H__
