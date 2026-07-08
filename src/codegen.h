#ifndef __CODEGEN_H__
#define __CODEGEN_H__
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
#define _(String) (String)
#include "../inst/include/rxode2parse.h"
#include "../inst/include/rxode2_control.h"
#include "tran.h"
#include "../inst/include/rxode2parseSbuf.h"

// show_ode = 1 dydt
#define ode_dydt 1
// show_ode = 2 Jacobian
#define ode_jac  2
// show_ode = 3 Ini statement
#define ode_ini 3
// show_ode = 0 LHS
#define ode_lhs 0
// show_ode = 4 functional bioavailibility
#define ode_printaux 4
// show_ode = 5 functional bioavailibility
#define ode_fbio 5
// show_ode == 6 functional lag
#define ode_lag 6
// show_ode == 7 functional rate
#define ode_rate 7
// show_ode == 8 functional duration
#define ode_dur 8
// show_ode == 9 functional mtimes
#define ode_mtime 9
// show_ode == 10 ME matrix
#define ode_mexp 10
// show_ode == 11 Inductive vector
#define ode_indLinVec 11
// show_ode == 12 initialize lhs to last value
// show_ode == 13 #define lags for lhs values
// show_ode == 14 #define lags for params/covs
// show_ode == 15 #define sync lhs for simeps
#define ode_simeps 15
// show_ode == 16 #define sync lhs for simeps
#define ode_simeta 16
// show_ode == 22 event-sensitivity d(alag)/dp  (jump sensitivities)
#define ode_dLag 22
// show_ode == 23 event-sensitivity d(F)/dp  (jump sensitivities)
#define ode_dF 23
// show_ode == 24 event-sensitivity d(rate)/dp  (jump sensitivities)
#define ode_dRate 24
// show_ode == 25 event-sensitivity d(dur)/dp  (jump sensitivities)
#define ode_dDur 25
// show_ode == 26 event-sensitivity d2(F)/dp/dq  (second-order jump sensitivities)
#define ode_d2F 26
// show_ode == 27/28/29 event-sensitivity d2(alag|rate|dur)/dp/dq (second-order
// jump sensitivities, dtau/infusion rows)
#define ode_d2Lag 27
#define ode_d2Rate 28
#define ode_d2Dur 29
// show_ode == 30 event-sensitivity d3(F)/dp/dq/dr (third-order jump
// sensitivities, additive-bolus F row only -- Phase H1 scope)
#define ode_d3F 30
// show_ode == 31 event-sensitivity d(F)/dq, q in calcSens2's index space
// (Phase H1's dtau/lag row: feeds d(delta)/dq = amt*dFQ[c][q])
#define ode_dFQ 31
// show_ode == 32 event-sensitivity d(J[k][c])/dq -- total derivative of the
// PHYSICAL Jacobian column wrt a calcSens2 parameter (Phase H1's dtau/lag
// row).  Buffer is (nState x nState x np2), NOT the usual (nState x nParam)
// dosing-parameter shape -- see `.rxEventSensCLines()$lagJacQ`.
#define ode_dLagJac 32
// show_ode == 33 event-sensitivity d(alag)/dq, q in calcSens2's index space
// (Phase H1's dtau/lag row SAFETY GUARD: nonzero here means q ALSO drives
// the same event's alag, the case the 2nd-order dtau row does not yet
// handle correctly -- see `.rxEventSensCLines()$lagQ`).
#define ode_dLagQ 33
// show_ode == 34 event-sensitivity d(dur)/dq, q in calcSens2's index space
// (modeled-DUR continuous-forcing 2nd-order piece: the quotient-rule 2nd
// derivative of rate=F*amt/dur needs d(dur)/dq at q's OWN index space,
// avoiding a calcSens2-position -> calcSens-position cross-index map --
// see `.rxEventSensCLines()$durQ`).
#define ode_dDurQ 34
// show_ode == 35 non-constant delay() pre-history: past(state, tau) <- expr,
// emitted as the dedicated _rxPast() history function (its own _past[] array,
// independent of the modeled-lag _alag[] function)
#define ode_past 35
// True for any of the event-sensitivity dosing-derivative functions
// (dLag/dF/dRate/dDur/d2F/d2Lag/d2Rate/d2Dur/d3F/dFQ/dLagJac/dLagQ/dDurQ);
// they share the same codegen preamble (which also populates the second-
// and third-order sensitivity locals) and emit only their R-generated
// body lines.  Kept contiguous so this is a range test.
#define ode_is_es_dcode(x) ((x) >= ode_dLag && (x) <= ode_dDurQ)

// Scenarios
#define print_double 0
#define print_populateParameters 1
#define print_void 2
#define print_lastLhsValue  3
#define print_lhsLags 4
#define print_paramLags 5
#define print_simeps 15
#define print_simeta 16

static inline void printDdtDefine(int show_ode, int scenario) {
  if (show_ode == ode_jac || show_ode == ode_lhs){
    //__DDtStateVar_#__
    // These will be defined and used in Jacobian or LHS functions
    for (int i = 0; i < tb.de.n; i++){
      if (scenario == print_double){
        sAppend(&sbOut,"  double  __DDtStateVar_%d__;\n",i);
      } else {
        sAppend(&sbOut,"  (void)__DDtStateVar_%d__;\n",i);
      }
    }
  }
}

static inline void printPDStateVar(int show_ode, int scenario) {
  // Now get Jacobain information  __PDStateVar_df_dy__ if needed
  char *buf1, *buf2;
  if (show_ode != ode_ini && show_ode != ode_simeps){
    for (int i = 0; i < tb.ndfdy; i++){
      buf1 = tb.ss.line[tb.df[i]];
      buf2 = tb.ss.line[tb.dy[i]];
      // This is for dydt/ LHS/ or jacobian for df(state)/dy(parameter)
      if (show_ode == ode_dydt || show_ode == ode_lhs || tb.sdfdy[i] == 1){
	if (scenario == print_double){
	  sAppend(&sbOut,"  double __PDStateVar_%s_SeP_%s__;\n",buf1,buf2);
	} else {
	  sAppend(&sbOut,"  (void)__PDStateVar_%s_SeP_%s__;\n",buf1,buf2);
	}
      }
    }
  }
}


/*
 * This determines if the variable should skip printing
 *
 * This is used when declaring variables based on different types of functions.
 *
 * @param scenario is an integer representing the types of printing scenarios handled.
 *
 *  - print_paramLags -- used for defining lags using #define lag_var(x)
 *
 *  - print_lhsLags -- also used for using #define lag_var(x) but for lhs
 *        variables instead of params
 *
 *  - print_lastLhsValue -- this is used for setting the last value of the lhs
 *
 *    This is used for all other scenarios
 *
 */
static inline int shouldSkipPrintLhsI(int scenario, int lhs, int i) {
  switch(scenario){
  case print_paramLags:
    // covariate/parameter lags use _getParCov(parNo, ...); visit every
    // parameter (same set/order as printPopulateParameters) so the running
    // ordinal is the parameter's true index, and emit macros only for lagged
    // ones (printParamLags gates on tb.lag[i]).  lhs variables are handled by
    // print_lhsLags and are skipped here so the _getParCov definition does not
    // clobber the correct lhs-lag one.
    return (lhs && tb.lh[i] > 0 && tb.lh[i] != isLHSparam);
  case print_lhsLags:
    // visit every lhs-storage variable so the running ordinal matches the
    // _lhs[]/_PL[] write-back order (printLhsLag emits macros only for the
    // ones that are actually lagged)
    return !(tb.lh[i] == isLHS || tb.lh[i] == isLHSstr ||
             tb.lh[i] == isLhsStateExtra || tb.lh[i] == isLHSparam);
  case print_lastLhsValue:
    return !(tb.lh[i] == isLHS || tb.lh[i] == isLHSstr ||
             tb.lh[i] == isLhsStateExtra || tb.lh[i] == isLHSparam);
  }
  return (lhs && tb.lh[i]>0 && tb.lh[i] != isLHSparam);
}

static inline void printParamLags(char *buf, int *j, int i) {
  // *j is the parameter's true index (this visits every parameter); only emit
  // the history macros for parameters actually used in lag/lead/first/last/diff
  if (tb.lag[i] == 0) {
    j[0] = j[0] + 1;
    return;
  }
  sAppendN(&sbOut, "#undef diff_", 12);
  doDot(&sbOut, buf);
  sAppendN(&sbOut, "1\n", 2);
  sAppendN(&sbOut, "#define diff_", 13);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "1(x) (x - _getParCov(_cSub, _solveData, %d, (&_solveData->subjects[_cSub])->idx - 1))\n", *j);

  sAppendN(&sbOut, "#undef diff_", 12);
  doDot(&sbOut, buf);
  sAppendN(&sbOut, "\n", 1);
  sAppendN(&sbOut, "#define diff_", 13);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "(x,y) (x - _getParCov(_cSub, _solveData, %d, (&_solveData->subjects[_cSub])->idx - (y)))\n", *j);

  sAppendN(&sbOut, "#undef first_", 13);
  doDot(&sbOut, buf);
  sAppendN(&sbOut, "1\n", 2);
  sAppendN(&sbOut, "#define first_", 14);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "1(x) _getParCov(_cSub, _solveData, %d, NA_INTEGER)\n", *j);

  sAppendN(&sbOut, "#undef last_", 12);
  doDot(&sbOut, buf);
  sAppendN(&sbOut, "1\n", 2);
  sAppendN(&sbOut, "#define last_", 13);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "1(x) _getParCov(_cSub, _solveData, %d, (&_solveData->subjects[_cSub])->n_all_times - 1)\n", *j);

  sAppendN(&sbOut, "#undef lead_", 12);
  doDot(&sbOut, buf);
  sAppendN(&sbOut, "1\n", 2);
  sAppendN(&sbOut, "#define lead_", 13);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "1(x) _getParCov(_cSub, _solveData, %d, (&_solveData->subjects[_cSub])->idx + 1)\n", *j);

  sAppendN(&sbOut, "#undef lead_", 12);
  doDot(&sbOut, buf);
  sAppendN(&sbOut, "\n", 1);
  sAppendN(&sbOut, "#define lead_", 13);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "(x, y) _getParCov(_cSub, _solveData, %d, (&_solveData->subjects[_cSub])->idx + (y))\n", *j);

  sAppendN(&sbOut, "#undef lag_", 11);
  doDot(&sbOut, buf);
  sAppendN(&sbOut, "1\n", 2);
  sAppendN(&sbOut, "#define lag_", 12);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "1(x) _getParCov(_cSub, _solveData, %d, (&_solveData->subjects[_cSub])->idx - 1)\n", *j);

  sAppendN(&sbOut, "#undef lag_", 11);
  doDot(&sbOut, buf);
  sAppendN(&sbOut, "\n", 1);
  sAppendN(&sbOut, "#define lag_", 12);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "(x,y) _getParCov(_cSub, _solveData, %d, (&_solveData->subjects[_cSub])->idx - (y))\n", *j);

  // lag0()/diff0()/lead0(): same as above but yield 0 (not NA) with no prior value
  sAppendN(&sbOut, "#undef lag0_", 12);
  doDot(&sbOut, buf);
  sAppendN(&sbOut, "1\n", 2);
  sAppendN(&sbOut, "#define lag0_", 13);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "1(x) (ISNA(_getParCov(_cSub, _solveData, %d, (&_solveData->subjects[_cSub])->idx - 1)) ? 0.0 : _getParCov(_cSub, _solveData, %d, (&_solveData->subjects[_cSub])->idx - 1))\n", *j, *j);
  sAppendN(&sbOut, "#undef lag0_", 12);
  doDot(&sbOut, buf);
  sAppendN(&sbOut, "\n", 1);
  sAppendN(&sbOut, "#define lag0_", 13);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "(x,y) (ISNA(_getParCov(_cSub, _solveData, %d, (&_solveData->subjects[_cSub])->idx - (y))) ? 0.0 : _getParCov(_cSub, _solveData, %d, (&_solveData->subjects[_cSub])->idx - (y)))\n", *j, *j);

  sAppendN(&sbOut, "#undef diff0_", 13);
  doDot(&sbOut, buf);
  sAppendN(&sbOut, "1\n", 2);
  sAppendN(&sbOut, "#define diff0_", 14);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "1(x) (x - (ISNA(_getParCov(_cSub, _solveData, %d, (&_solveData->subjects[_cSub])->idx - 1)) ? 0.0 : _getParCov(_cSub, _solveData, %d, (&_solveData->subjects[_cSub])->idx - 1)))\n", *j, *j);
  sAppendN(&sbOut, "#undef diff0_", 13);
  doDot(&sbOut, buf);
  sAppendN(&sbOut, "\n", 1);
  sAppendN(&sbOut, "#define diff0_", 14);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "(x,y) (x - (ISNA(_getParCov(_cSub, _solveData, %d, (&_solveData->subjects[_cSub])->idx - (y))) ? 0.0 : _getParCov(_cSub, _solveData, %d, (&_solveData->subjects[_cSub])->idx - (y))))\n", *j, *j);

  sAppendN(&sbOut, "#undef lead0_", 13);
  doDot(&sbOut, buf);
  sAppendN(&sbOut, "1\n", 2);
  sAppendN(&sbOut, "#define lead0_", 14);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "1(x) (ISNA(_getParCov(_cSub, _solveData, %d, (&_solveData->subjects[_cSub])->idx + 1)) ? 0.0 : _getParCov(_cSub, _solveData, %d, (&_solveData->subjects[_cSub])->idx + 1))\n", *j, *j);
  sAppendN(&sbOut, "#undef lead0_", 13);
  doDot(&sbOut, buf);
  sAppendN(&sbOut, "\n", 1);
  sAppendN(&sbOut, "#define lead0_", 14);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "(x,y) (ISNA(_getParCov(_cSub, _solveData, %d, (&_solveData->subjects[_cSub])->idx + (y))) ? 0.0 : _getParCov(_cSub, _solveData, %d, (&_solveData->subjects[_cSub])->idx + (y)))\n", *j, *j);
  j[0]=j[0]+1;
}

static inline void printLhsLag(char *buf, int *j, int i) {
  // *j is the lhs ordinal (matches the _lhs[_LHS_*_] write-back and the
  // sticky-variable _PL[_LHS_*_] load).  _PL (= _ind->lhs) holds the PREVIOUS
  // record's lhs values during calc_lhs (write-back is at the end) and is reset
  // to NA at the start of each individual, so lag/diff by 1 read the previous
  // record's value and yield NA on the first record.  Deeper lags on an lhs are
  // rejected at parse time (only lag(x,1)/diff(x,1) are supported).
  if (tb.lag[i] != 0) {
    sAppendN(&sbOut, "#define diff_", 13);
    doDot(&sbOut, buf);
    sAppend(&sbOut, "1(x) ((x) - _PL[_LHS_%d_])\n", *j);
    sAppendN(&sbOut, "#define diff_", 13);
    doDot(&sbOut, buf);
    sAppend(&sbOut, "(x,y) ((x) - _PL[_LHS_%d_])\n", *j);
    sAppendN(&sbOut, "#define lag_", 12);
    doDot(&sbOut, buf);
    sAppend(&sbOut, "1(x) _PL[_LHS_%d_]\n", *j);
    sAppendN(&sbOut, "#define lag_", 12);
    doDot(&sbOut, buf);
    sAppend(&sbOut, "(x, y) _PL[_LHS_%d_]\n", *j);
    // lead/first/last on an lhs have no well-defined value from the backward
    // history; define them to the previous value so the symbol resolves (these
    // are not meaningfully supported on lhs variables)
    sAppendN(&sbOut, "#define lead_", 13);
    doDot(&sbOut, buf);
    sAppend(&sbOut, "1(x) _PL[_LHS_%d_]\n", *j);
    sAppendN(&sbOut, "#define lead_", 13);
    doDot(&sbOut, buf);
    sAppend(&sbOut, "(x,y) _PL[_LHS_%d_]\n", *j);
    sAppendN(&sbOut, "#define first_", 14);
    doDot(&sbOut, buf);
    sAppend(&sbOut, "1(x) _PL[_LHS_%d_]\n", *j);
    sAppendN(&sbOut, "#define last_", 13);
    doDot(&sbOut, buf);
    sAppend(&sbOut, "1(x) _PL[_LHS_%d_]\n", *j);
    // lag0()/diff0()/lead0(): same as above but yield 0 (not NA) on the first
    // record, so downstream arithmetic stays finite
    sAppendN(&sbOut, "#define diff0_", 14);
    doDot(&sbOut, buf);
    sAppend(&sbOut, "1(x) ((x) - (ISNA(_PL[_LHS_%d_]) ? 0.0 : _PL[_LHS_%d_]))\n", *j, *j);
    sAppendN(&sbOut, "#define diff0_", 14);
    doDot(&sbOut, buf);
    sAppend(&sbOut, "(x,y) ((x) - (ISNA(_PL[_LHS_%d_]) ? 0.0 : _PL[_LHS_%d_]))\n", *j, *j);
    sAppendN(&sbOut, "#define lag0_", 13);
    doDot(&sbOut, buf);
    sAppend(&sbOut, "1(x) (ISNA(_PL[_LHS_%d_]) ? 0.0 : _PL[_LHS_%d_])\n", *j, *j);
    sAppendN(&sbOut, "#define lag0_", 13);
    doDot(&sbOut, buf);
    sAppend(&sbOut, "(x, y) (ISNA(_PL[_LHS_%d_]) ? 0.0 : _PL[_LHS_%d_])\n", *j, *j);
    sAppendN(&sbOut, "#define lead0_", 14);
    doDot(&sbOut, buf);
    sAppend(&sbOut, "1(x) (ISNA(_PL[_LHS_%d_]) ? 0.0 : _PL[_LHS_%d_])\n", *j, *j);
    sAppendN(&sbOut, "#define lead0_", 14);
    doDot(&sbOut, buf);
    sAppend(&sbOut, "(x,y) (ISNA(_PL[_LHS_%d_]) ? 0.0 : _PL[_LHS_%d_])\n", *j, *j);
  }
  j[0] = j[0]+1;
}

static inline void printLastLhsValue(char *buf, int *j) {
  sAppendN(&sbOut, "  ", 2);
  doDot(&sbOut, buf);
  sAppend(&sbOut, " = _PL[_LHS_%d_];\n", *j);
  j[0] = j[0]+1;
}

static inline void printDoubleDeclaration(char *buf) {
  sAppendN(&sbOut,"  double ", 9);
  doDot(&sbOut, buf);
  if (!strcmp("rx_lambda_", buf) || !strcmp("rx_yj_", buf) ||
      !strcmp("rx_hi_", buf) || !strcmp("rx_low_", buf)){
    sAppendN(&sbOut, "__", 2);
  }
  sAppendN(&sbOut, " = NA_REAL;\n", 12);
}

static inline void printVoidDeclaration(char *buf) {
  sAppend(&sbOut,"  ");
  sAppend(&sbOut,"(void)");
  doDot(&sbOut, buf);
  if (!strcmp("rx_lambda_", buf) || !strcmp("rx_yj_", buf) ||
      !strcmp("rx_low_", buf) || !strcmp("rx_hi_", buf)){
    sAppendN(&sbOut, "__", 2);
  }
  sAppendN(&sbOut, ";\n", 2);
}

static inline void printPopulateParameters(char *buf, int *j, int *i) {
  if (tb.lh[*i] != isLHS &&
      !(tb.lh[*i] == isLHSparam && tb.ini[*i] == 1)) {
    sAppendN(&sbOut,"  ", 2);
    doDot(&sbOut, buf);
    sAppend(&sbOut, " = _PP[%d];\n", *j);
  }
  j[0] = j[0]+1;
}

static inline void printSimEps(char *buf, int *j) {
  sAppend(&sbOut,"  if (_solveData->svar[_svari] == %d) {", *j);
  doDot(&sbOut, buf);
  sAppend(&sbOut, " = _PP[%d];}; ", *j);
  j[0] = j[0]+1;
}

static inline void printSimEta(char *buf, int *j) {
  sAppend(&sbOut,"  if (_solveData->ovar[_ovari] == %d) {", *j);
  doDot(&sbOut, buf);
  sAppend(&sbOut, " = _PP[%d];}; ", *j);
  j[0] = j[0]+1;
}


void prnt_vars(int scenario, int lhs, const char *pre_str, const char *post_str, int show_ode);

static inline void printCModelVars(const char *prefix) {
  sAppend(&sbOut, "extern SEXP %smodel_vars(void){\n  int pro=0;\n", prefix);
  sAppend(&sbOut, "  SEXP _mv = PROTECT(_rxGetModelLib(\"%smodel_vars\"));pro++;\n", prefix);
  sAppendN(&sbOut, "  if (!_rxIsCurrentC(_mv)){\n", 28);
  sAppend(&sbOut, "%s\n", _mv.s);
  sAppendN(&sbOut, "    SEXP lst      = PROTECT(_rxQr(rw));pro++;\n", 46);
  sAppendN(&sbOut, "    _assign_ptr(lst);\n", 22);
  sAppendN(&sbOut, "    UNPROTECT(pro);\n", 20);

  sAppendN(&sbOut, "    return lst;\n", 16);
  sAppendN(&sbOut, "  } else {\n", 11);
  sAppendN(&sbOut, "    UNPROTECT(pro);\n", 20);
  sAppendN(&sbOut, "    return _mv;\n", 16);
  sAppendN(&sbOut, "  }\n", 4);
  sAppendN(&sbOut, "}\n", 2);
}

static inline void printRInit(const char *libname, const char *libname2, const char *prefix) {
  sAppend(&sbOut,"\n//Create function to call from R's main thread that assigns the required functions. Sometimes they don't get assigned.\nextern void %sassignFuns(void){\n  _assignFuns();\n}\n", prefix);
  sAppend(&sbOut,"\n//Initialize the dll to match rxode2's calls\nvoid R_init0_%s(void){\n  // Get C callables on load; Otherwise it isn't thread safe\n", libname2);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sassignFuns2\", (DL_FUNC) __assignFuns2);\n", libname, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sassignFuns\", (DL_FUNC) %sassignFuns);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sinis\",(DL_FUNC) %sinis);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sdydt\",(DL_FUNC) %sdydt);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%scalc_lhs\",(DL_FUNC) %scalc_lhs);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%scalc_jac\",(DL_FUNC) %scalc_jac);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sdydt_lsoda\", (DL_FUNC) %sdydt_lsoda);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%scalc_jac_lsoda\", (DL_FUNC) %scalc_jac_lsoda);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sode_solver_solvedata\", (DL_FUNC) %sode_solver_solvedata);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sode_solver_get_solvedata\", (DL_FUNC) %sode_solver_get_solvedata);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sF\", (DL_FUNC) %sF);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sLag\", (DL_FUNC) %sLag);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sdLag\", (DL_FUNC) %sdLag);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sdF\", (DL_FUNC) %sdF);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sdRate\", (DL_FUNC) %sdRate);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sdDur\", (DL_FUNC) %sdDur);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sd2F\", (DL_FUNC) %sd2F);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sd2Lag\", (DL_FUNC) %sd2Lag);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sd2Rate\", (DL_FUNC) %sd2Rate);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sd2Dur\", (DL_FUNC) %sd2Dur);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sd3F\", (DL_FUNC) %sd3F);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sdFQ\", (DL_FUNC) %sdFQ);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sdLagJac\", (DL_FUNC) %sdLagJac);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sdLagQ\", (DL_FUNC) %sdLagQ);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sdDurQ\", (DL_FUNC) %sdDurQ);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sRate\", (DL_FUNC) %sRate);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sDur\", (DL_FUNC) %sDur);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%smtime\", (DL_FUNC) %smtime);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sME\", (DL_FUNC) %sME);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sIndF\", (DL_FUNC) %sIndF);\n", libname, prefix, prefix);
  sAppend(&sbOut, "  R_RegisterCCallable(\"%s\",\"%sdydt_liblsoda\", (DL_FUNC) %sdydt_liblsoda);\n", libname, prefix, prefix);
  sAppend(&sbOut,"}\n//Initialize the dll to match rxode2's calls\nvoid R_init_%s(DllInfo *info){\n  // Get C callables on load; Otherwise it isn't thread safe\n  R_init0_%s();", libname2, libname2);
  sAppend(&sbOut, "\n  static const R_CallMethodDef callMethods[]  = {\n    {\"%smodel_vars\", (DL_FUNC) &%smodel_vars, 0},\n    {NULL, NULL, 0}\n  };\n",
  	  prefix, prefix);
  sAppendN(&sbOut, "\n  R_registerRoutines(info, NULL, callMethods, NULL, NULL);\n  R_useDynamicSymbols(info,FALSE);\n", 95);
  sAppendN(&sbOut, "  _assignFuns0();\n", 18);
  sAppendN(&sbOut, "\n}\n", 3);
  sAppend(&sbOut, "\nvoid R_unload_%s (DllInfo *info){\n  // Free resources required for single subject solve.\n  SEXP _mv = PROTECT(_rxGetModelLib(\"%smodel_vars\"));\n",
	  libname2, prefix);
  sAppend(&sbOut, "  if (!Rf_isNull(_mv)){\n    _rxRmModelLib(\"%smodel_vars\");\n  }\n  UNPROTECT(1);\n}\n", prefix);
}

void print_aux_info(char *model, const char *prefix, const char *libname, const char *pMd5, const char *timeId,
		    const char *libname2);

void codegen(char *model, int show_ode, const char *prefix, const char *libname, const char *pMd5, const char *timeId, const char *libname2);
void writeSb(sbuf *sbb, FILE *fp);

#define gCode(i) (&sbOut)->s[0]='\0';		\
  (&sbOut)->o=0;				\
  codegen(gBuf, i, CHAR(STRING_ELT(prefix,0)),	\
	  CHAR(STRING_ELT(libname, 0)),		\
	  CHAR(STRING_ELT(pMd5,0)),		\
	  CHAR(STRING_ELT(timeId, 0)),		\
	  CHAR(STRING_ELT(libname, 1)));					\
  writeSb(&sbOut, fpIO);

SEXP _rxode2_codegen(SEXP c_file, SEXP prefix, SEXP libname,
                          SEXP pMd5, SEXP timeId, SEXP mvLast, SEXP goodFuns,
                          SEXP esDLagCode, SEXP esDFCode,
                          SEXP esDRateCode, SEXP esDDurCode, SEXP esD2FCode,
                          SEXP esD2LagCode, SEXP esD2RateCode, SEXP esD2DurCode,
                          SEXP esD3FCode, SEXP esDFQCode, SEXP esDLagJacCode,
                          SEXP esDLagQCode, SEXP esDDurQCode);

extern int fullPrint;
#endif // __CODEGEN_H__
