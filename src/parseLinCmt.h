#ifndef __PARSELINCMT_H__
#define __PARSELINCMT_H__
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
#include "tran.h"
#include "../inst/include/rxode2parseSbuf.h"

typedef struct linCmtStruct {
  int ka;

  int k12;
  int k21;

  int k13;
  int k31;

  int kel;

  int a;
  int b;
  int c;

  int aob;

  int alpha;
  int beta;
  int gamma;

  int cl;

  int cl1;
  int cl2;
  int cl3;
  int cl4;

  int v;

  int v1;
  int v2;
  int v3;
  int v4;

  int vp;
  int vp1;
  int vp2;
  int vp3;

  int vss;

  int cmtc;

  int clStyle;
  int vStyle;

  int trans;
  int ncmt;

  sbuf ret0;
  sbuf ret;
  const char *mid;
  SEXP vars;
} linCmtStruct;

static inline void linCmtIni(linCmtStruct *lin){
  lin->ka   = -1;

  lin->k12  = -1;
  lin->k21  = -1;

  lin->k13  = -1;
  lin->k31  = -1;

  lin->cmtc = -1;
  lin->kel  = -1;

  lin->cl = -1;

  lin->cl1 = -1;
  lin->cl2 = -1;
  lin->cl3 = -1;
  lin->cl4 = -1;

  lin->v = -1;

  lin->v1 = -1;
  lin->v2 = -1;
  lin->v3 = -1;
  lin->v4 = -1;

  lin->vp = -1;
  lin->vp1 = -1;
  lin->vp2 = -1;
  lin->vp3 = -1;

  lin->vss = -1;

  lin->a = -1;
  lin->b = -1;
  lin->c = -1;

  lin->aob = -1;

  lin->alpha = -1;
  lin->beta  = -1;
  lin->gamma = -1;

  lin->clStyle=-1;
  lin->vStyle = -1;

  sNull(&(lin->ret0));
  sNull(&(lin->ret));
}

static inline void linCmtCmt(linCmtStruct *lin, const int cmt){
  if (lin->cmtc == -1) {
    lin->cmtc = cmt;
  }
  if (lin->cmtc != cmt){
    _rxode2parse_unprotect();
    err_trans("inconsistent central compartment numbers, not sure if central compartment no is '1' or '2'");
  }
}

#define errLinLen 150
extern char errLin[errLinLen];
extern int errOff;

#include "parseLinCmtCl.h"
#include "parseLinCmtV.h"
#include "parseLinCmtAB.h"

static inline int linCmtStartsWithV(linCmtStruct *lin, const char *in, int *index) {
  if (in[0] == 'v' || in[0] == 'V') {
    linCmtV(lin, in, index);
    return 1;
  }
  return 0;
}

static inline int linCmtStartsWithC(linCmtStruct *lin, const char *in, int *index){
  if (in[0] == 'c' || in[0] == 'C') {
    linCmtC(lin, in, index);
    return 1;
  }
  return 0;
}

static inline int linCmtStartsWithK(linCmtStruct *lin, const char *in, int *index) {
  if (in[0] == 'k' || in[0] == 'K') {
    linCmtK(lin, in, index);
    return 1;
  }
  return 0;
}

static inline int linCmtStartsWithQ(linCmtStruct *lin, const char *in, int *index){
  if (in[0] == 'Q' || in[0] == 'q') {
    linCmtQ(lin, in, index);
    return 1;
  }
  return 0;
}

static inline int linCmtStartsWithA(linCmtStruct *lin, const char *in, int *index) {
  if (in[0] == 'A' || in[0] == 'a') {
    linCmtA(lin, in, index);
    return 1;
  }
  return 0;
}

static inline int linCmtStartsWithB(linCmtStruct *lin, const char *in, int *index) {
  if (in[0] == 'B' || in[0] == 'b') {
    linCmtB(lin, in, index);
    return 1;
  }
  return 0;
}

static inline int isLinCmtGamma(linCmtStruct *lin, const char *in, int *index) {
  if ((in[0] == 'G' || in[0] == 'g') &&
      (in[1] == 'A' || in[1] == 'a') &&
      (in[2] == 'M' || in[2] == 'm') &&
      (in[3] == 'M' || in[3] == 'm') &&
      (in[4] == 'A' || in[4] == 'a') &&
      in[5] == '\0') {
    lin->gamma = *index;
    return 1;
  }
  return 0;
}

static inline void linCmtStr(linCmtStruct *lin, const char *in, int *index) {
  int tmp = linCmtStartsWithV(lin, in, index) ||
    linCmtStartsWithC(lin, in, index) ||
    linCmtStartsWithK(lin, in, index) ||
    linCmtStartsWithQ(lin, in, index) ||
    linCmtStartsWithA(lin, in, index) ||
    linCmtStartsWithB(lin, in, index) ||
    isLinCmtGamma(lin, in, index);
  (void)tmp;
}

#include "parseLinCmtAdjustPars.h"

#ifdef _isrxode2parse_
#define _linCmtParse _rxode2parse_linCmtParse
#define _rxode2_linCmtGen _rxode2parse_linCmtGen
#endif


SEXP _linCmtParse(SEXP vars, SEXP inStr, SEXP verboseSXP);
SEXP _rxode2_linCmtGen(SEXP linCmt, SEXP vars, SEXP linCmtSens, SEXP verbose);

typedef struct linCmtGenStruct {
  sbuf last;
  sbuf d_tlag;
  sbuf d_tlag2;
  sbuf d_F;
  sbuf d_F2;
  sbuf d_rate1;
  sbuf d_dur1;
  sbuf d_rate2;
  sbuf d_dur2;
  sbuf last2;
} linCmtGenStruct;

extern linCmtGenStruct _linCmtGenStruct;

static inline void linCmtGenIni(linCmtGenStruct *linG) {
  sNull(&(linG->last));
  sNull(&(linG->d_tlag));
  sNull(&(linG->d_tlag2));
  sNull(&(linG->d_F));
  sNull(&(linG->d_F2));
  sNull(&(linG->d_rate1));
  sNull(&(linG->d_dur1));
  sNull(&(linG->d_rate2));
  sNull(&(linG->d_dur2));
  sNull(&(linG->last2));
  
  sIni(&(linG->last));
  sIni(&(linG->d_tlag));
  sIni(&(linG->d_tlag2));
  sIni(&(linG->d_F));
  sIni(&(linG->d_F2));
  sIni(&(linG->d_rate1));
  sIni(&(linG->d_dur1));
  sIni(&(linG->d_rate2));
  sIni(&(linG->d_dur2));
  sIni(&(linG->last2));

  sAppendN(&(linG->d_tlag),"0.0, ", 5);
  sAppendN(&(linG->d_tlag2), ", 0.0, ", 7);
  sAppendN(&(linG->d_F), "1.0, ", 5);
  sAppendN(&(linG->d_F2), "1.0, ", 5);
  sAppendN(&(linG->d_rate1), "0.0, ", 5);
  sAppendN(&(linG->d_dur1), "0.0, ", 5);
  sAppendN(&(linG->d_rate2), "0.0, ", 5);
  sAppendN(&(linG->d_dur2), "0.0)", 4);
}

static inline void linCmtGenFree(linCmtGenStruct *linG) {
  sFree(&(linG->last));
  sFree(&(linG->d_tlag));
  sFree(&(linG->d_tlag2));
  sFree(&(linG->d_F));
  sFree(&(linG->d_F2));
  sFree(&(linG->d_rate1));
  sFree(&(linG->d_dur1));
  sFree(&(linG->d_rate2));
  sFree(&(linG->d_dur2));
  sFree(&(linG->last2));
}

#include "parseLinCmtTransAlpha.h"
#include "parseLinCmtTransCl.h"
#include "parseLinCmtTransK.h"

#endif // __PARSELINCMT_H__
