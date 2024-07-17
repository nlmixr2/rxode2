#ifndef __PRINT_NODE_H__
#define __PRINT_NODE_H__
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

int rxstrcmpi(const char * str1, const char * str2);
static inline int nodeTime(char *value) {
  if (!rxstrcmpi("time",value)){
    aAppendN("t", 1);
    sAppendN(&sbt, "t", 1);
    return 1;
  }
  return 0;
}

static inline int nodeCmt(char *value) {
  if (!strcmp("CMT",value)){
    aAppendN("_CMT", 4);
    sAppendN(&sbt, "CMT", 3);
    return 1;
  }
  return 0;
}

static inline int nodeAmt(char *value) {
  if (!rxstrcmpi("AMT",value)){
    aAppendN("amt", 3);
    sAppendN(&sbt, "amt", 3);
    return 1;
  }
  return 0;
}
 
static inline int nodeTlast(char *value) {
  if (!rxstrcmpi("tlast",value)){
    aAppendN("_solveData->subjects[_cSub].tlast", 33);
    sAppendN(&sbt, "tlast", 5);
    return 1;
  }
  return 0;
}

static inline int nodePtr(char *value) {
  if (!strcmp("rx__PTR__",value)){
    aAppendN("_solveData, _cSub", 17);
    sAppendN(&sbt, "rx__PTR__", 9);
    return 1;
  }
  return 0;
}

static inline int nodeNaN(char *value){
  if (!strcmp("NaN",value)){
    aAppendN("NAN", 3);
    sAppendN(&sbt,"NaN", 3);
    return 1;
  }
  return 0;
}

static inline int nodeNA(char *value) {
  if (!strcmp("NA",value)){
    aAppendN("NA_REAL", 7);
    sAppendN(&sbt,"NA", 2);
    return 1;
  }
  return 0;
}

static inline int nodeInf(char *value) {
  if (!strcmp("Inf",value)){
    if (sbt.o > 0 && sbt.s[sbt.o-1] == '-'){
      sb.o--; sbDt.o--;
      aAppendN("R_NegInf", 8);
    } else {
      aAppendN("R_PosInf", 8);
    }
    sAppendN(&sbt,"Inf", 3);
    return 1;
  }
  return 0;
}

static inline int nodeFunGamma(char *value) {
  if (!strcmp("gamma",value)){
    aAppendN("lgammafn", 8);
    sAppendN(&sbt, "lgammafn", 8);
    return 1;
  }
  return 0;
}

static inline int nodeFunLfactorial(char *value) {
  if (!strcmp("lfactorial",value)){
    aAppendN("lgamma1p", 8);
    sAppendN(&sbt, "lgamma1p", 8);
    return 1;
  }
  return 0;
}

static inline int nodeFunLog(char *value) {
  if (!strcmp("log",value)){
    aAppendN("_safe_log", 9);
    sAppendN(&sbt, "log", 3);
    return 1;
  }
  return 0;
}

static inline int nodeFunAbs(char *value) {
  if (!strcmp("abs",value)){
    aAppendN("fabs", 4);
    sAppendN(&sbt,"abs", 3);
    return 1;
  }
  return 0;
}

static inline int nodeFunLinCmt(char *value) {
  if (!strcmp("linCmt",value)) {
    if (tb.linCmt == 0){
      aAppendN("linCmt", 6);
      aProp(-100);
      sAppendN(&sbt,"linCmt", 6);
      tb.linCmt=1;
    } else {
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("only one 'linCmt()' per model"));
    }
    return 1;
  }
  return 0;
}

static inline int nodeFunLinCmtA(char *value) {
  if (!strcmp("linCmtA",value)){
    aAppendN("linCmtA", 7);
    sAppendN(&sbt,"linCmtA", 7);
    tb.linCmt=2;
    return 1;
  }
  return 0;
}

static inline int nodeFunLinCmtB(char *value) {
  if (!strcmp("linCmtB",value)){
    aAppendN("linCmtB", 7);
    sAppendN(&sbt,"linCmtB", 7);
    tb.linCmt=2;
    return 1;
  }
  return 0;
}

static inline int nodeFunLinCmtC(char *value){
  if (!strcmp("linCmtC",value)){
    aAppendN("linCmtC", 7);
    sAppendN(&sbt,"linCmtC", 7);
    tb.linCmt=2;
    return 1;
  }
  return 0;
}

void wprint_node(int depth, char *name, char *value, void *client_data);
#endif //
