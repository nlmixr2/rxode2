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

static inline int isStateLhsI(int i) {
  if (tb.lh[i] == isState){
    int doCont=0;
    for (int j = 0; j < tb.de.n; j++) {
      if (tb.di[j] == i) {
	if (!tb.idu[j]) doCont = 1;
	break;
      }
    }
    if (doCont) return 1;
  }
  return 0;
}

static inline int shouldSkipPrintLhsI(int scenario, int lhs, int i) {
  if (isStateLhsI(i)) return 1;
  switch(scenario){
  case print_paramLags:
    return (tb.lag[i] == notLHS || tb.lh[i] == isState);
  case print_lhsLags:
    return (tb.lag[i] == 0 || tb.lh[i] != isLHS);
  case print_lastLhsValue:
    return !(tb.lh[i] == isLHS || tb.lh[i] == isLhsStateExtra || tb.lh[i] == isLHSparam);
  }
  return (lhs && tb.lh[i]>0 && tb.lh[i] != isLHSparam);
}

static inline void printParamLags(char *buf, int *j) {
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
  j[0]=j[0]+1;
}

static inline void printLhsLag(char *buf, int *j) {
  sAppendN(&sbOut, "#define lead_", 13);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "1(x) _solveData->subjects[_cSub].lhs[%d]\n", *j);
  sAppendN(&sbOut, "#define lead_", 13);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "(x,y) _solveData->subjects[_cSub].lhs[%d]\n", *j);
  sAppendN(&sbOut, "#define diff_", 13);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "1(x) _solveData->subjects[_cSub].lhs[%d]\n", *j);
  sAppendN(&sbOut, "#define diff_", 13);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "(x,y) _solveData->subjects[_cSub].lhs[%d]\n", *j);
  sAppendN(&sbOut, "#define lag_", 12);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "1(x) _solveData->subjects[_cSub].lhs[%d]\n", *j);
  sAppendN(&sbOut, "#define lag_", 12);
  doDot(&sbOut, buf);
  sAppend(&sbOut, "(x, y) _solveData->subjects[_cSub].lhs[%d]\n", *j);
  j[0] = j[0]+1;
}

static inline void printLastLhsValue(char *buf, int *j) {
  sAppendN(&sbOut, "  ", 2);
  doDot(&sbOut, buf);
  sAppend(&sbOut, " = _PL[%d];\n", *j);
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

static inline void printPopulateParameters(char *buf, int *j) {
  sAppendN(&sbOut,"  ", 2);
  doDot(&sbOut, buf);
  sAppend(&sbOut, " = _PP[%d];\n", *j);
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
  sAppendN(&sbOut, "    SEXP hash    = PROTECT(allocVector(STRSXP, 1));pro++;\n", 58);
  sAppend(&sbOut, "#define __doBuf__  snprintf(buf, __doBufN__, \"", _mv.o+1);
  int off=0;
  int off2 = 0;
  for (int i = 0; i < _mv.o; i++){
    if (off != 0 && off % 4095 == 0) {
      sAppend(&sbOut, "\"); \\\n snprintf(buf+%d, __doBufN__-%d, \"", off2, off2);
    }
    off++;
    off2++;
    if (_mv.s[i] == '%'){
      sAppendN(&sbOut, "%%", 2);
      off++;
    } else if (_mv.s[i] == '?') {
      // Avoid digrahps/trigraphs
      sAppendN(&sbOut, "\\?", 2);
    } else if (_mv.s[i] == '"'){
      sAppendN(&sbOut, "\\\"", 2);
    } else if (_mv.s[i] == '\''){
      sAppendN(&sbOut, "'", 1);
    } else if (_mv.s[i] == ' '){
      sAppendN(&sbOut, " ", 1);
    } else if (_mv.s[i] == '\n'){
      sAppendN(&sbOut, "\\n", 2);
    } else if (_mv.s[i] == '\t'){
      sAppendN(&sbOut, "\\t", 2);
    } else if (_mv.s[i] == '\\'){
      sAppendN(&sbOut, "\\\\", 2);
    } else if (_mv.s[i] >= 33  && _mv.s[i] <= 126){ // ASCII only
      sPut(&sbOut, _mv.s[i]);
    }
  }
  sAppendN(&sbOut, "\");\n", 4);
  sAppend(&sbOut,"    char buf[%d];\n#define __doBufN__ %d\n    __doBuf__\n#undef __doBuf__\n#undef __doBufN__\n", off+1, off+1);
  sAppendN(&sbOut,"    SET_STRING_ELT(hash, 0, mkChar(buf));\n", 42);
  sAppendN(&sbOut, "    SEXP lst      = PROTECT(_rxQr(hash));pro++;\n", 48);
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
  sAppend(&sbOut, "  if (!isNull(_mv)){\n    _rxRmModelLib(\"%smodel_vars\");\n  }\n  UNPROTECT(1);\n}\n", prefix);
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

SEXP _rxode2parse_codegen(SEXP c_file, SEXP prefix, SEXP libname,
                          SEXP pMd5, SEXP timeId, SEXP mvLast, SEXP goodFuns);

extern int fullPrint;
#endif // __CODEGEN_H__
