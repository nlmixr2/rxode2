#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "codegen.h"

SEXP _rxode2parse_rxFunctionName;
SEXP _rxode2parse_functionName;
SEXP _rxode2parse_functionType;
SEXP _rxode2parse_functionPackageName;
SEXP _rxode2parse_functionPackageFunction;
SEXP _rxode2parse_functionArgMin;
SEXP _rxode2parse_functionArgMax;
SEXP _rxode2parse_functionThreadSafe;

SEXP _rxode2parse_funName;
SEXP _rxode2parse_funNameInt;

SEXP _rxode2parse_packages;

#undef df

SEXP getRxode2ParseDfBuiltin(void);

int _rxode2parse_protected = 0;
void _rxode2parse_assignTranslationBuiltin(void) {
  SEXP df = getRxode2ParseDfBuiltin();
  _rxode2parse_funName = PROTECT(VECTOR_ELT(df, 0)); _rxode2parse_protected++;
  _rxode2parse_funNameInt = PROTECT(VECTOR_ELT(df, 1)); _rxode2parse_protected++;
}

void _rxode2parse_assignTranslation(SEXP df) {
  _rxode2parse_unprotect();
  _rxode2parse_rxFunctionName = PROTECT(VECTOR_ELT(df, 0)); _rxode2parse_protected++;
  _rxode2parse_functionName = PROTECT(VECTOR_ELT(df, 1)); _rxode2parse_protected++;
  _rxode2parse_functionType = PROTECT(VECTOR_ELT(df, 2)); _rxode2parse_protected++;
  _rxode2parse_functionPackageName = PROTECT(VECTOR_ELT(df, 3)); _rxode2parse_protected++;
  _rxode2parse_functionPackageFunction = PROTECT(VECTOR_ELT(df, 4)); _rxode2parse_protected++;
  _rxode2parse_functionArgMin = PROTECT(VECTOR_ELT(df, 5)); _rxode2parse_protected++;
  _rxode2parse_functionArgMax = PROTECT(VECTOR_ELT(df, 6)); _rxode2parse_protected++;
  _rxode2parse_functionThreadSafe = PROTECT(VECTOR_ELT(df, 7));_rxode2parse_protected++;
  _rxode2parse_assignTranslationBuiltin();
}

void _rxode2parse_unprotect(void) {
  if (_rxode2parse_protected) UNPROTECT(_rxode2parse_protected);
  _rxode2parse_protected = 0;
}


#include "codegen2.h"

SEXP _rxode2parse_rxQs(SEXP);
SEXP _rxode2parse_rxQr(SEXP);
SEXP getRxode2ParseDf(void);
SEXP getRxode2ParseGetPointerAssignment(void);

static FILE *fpIO;

/* when prnt_vars() is called, user defines the behavior in "case" */
void prnt_vars(int scenario, int lhs, const char *pre_str, const char *post_str, int show_ode) {
  int i, j;
  char *buf;
  sAppend(&sbOut, "%s", pre_str);
  if (scenario == print_double || scenario == print_void){
    printDdtDefine(show_ode, scenario);
    printPDStateVar(show_ode, scenario);
  }
  for (i=0, j=0; i<NV; i++) {
    if (shouldSkipPrintLhsI(scenario, lhs, i)) continue;
    buf = tb.ss.line[i];
    switch(scenario) {
    case print_paramLags: // Case 5 is for using #define lag_var(x)
      printParamLags(buf, &j);
      break;
    case print_lhsLags: // Case 4 is for using #define lag_var(x)
      printLhsLag(buf, &j);
      break;
    case print_lastLhsValue: // Case 3 is for using the last lhs value
      printLastLhsValue(buf, &j);
      break;
    case print_double:   // Case 0 is for declaring the variables
      printDoubleDeclaration(buf);
      break;
    case print_void: // Case 2 is for suppressing all the warnings for the variables by using (void)var;
      // See https://stackoverflow.com/questions/1486904/how-do-i-best-silence-a-warning-about-unused-variables
      printVoidDeclaration(buf);
      break;
    case print_populateParameters:
      // Case 1 is for declaring the par_ptr.
      printPopulateParameters(buf, &j);
      break;
    case print_simeps:
      // Case 15 is for declaring eps the sync parameters
      printSimEps(buf, &j);
      break;
    case print_simeta:
      // Case 16 is for declaring eta the sync parameters
      printSimEta(buf, &j);
      break;
    default: break;
    }
  }
  sAppend(&sbOut, "%s", post_str);
}



void print_aux_info(char *model, const char *prefix, const char *libname, const char *pMd5, const char *timeId,
                    const char *libname2){
  sbuf bufw;
  sNull(&bufw);
  sIniTo(&bufw, 1024);
  /* char bufw[1024]; */
  printCModelVars(prefix);

  sAppend(&sbOut,"extern void %sdydt_lsoda(int *neq, double *t, double *A, double *DADT)\n{\n  %sdydt(neq, *t, A, DADT);\n}\n", prefix, prefix);
  sAppend(&sbOut, "extern int %sdydt_liblsoda(double __t, double *y, double *ydot, void *data)\n{\n  int *neq = (int*)(data);\n  %sdydt(neq, __t, y, ydot);\n  return(0);\n}\n",
          prefix,prefix);
  sAppend(&sbOut,"extern void %scalc_jac_lsoda(int *neq, double *t, double *A,int *ml, int *mu, double *JAC, int *nrowpd){\n  // Update all covariate parameters\n  %scalc_jac(neq, *t, A, JAC, *nrowpd);\n}\n",
          prefix, prefix);

  printRInit(libname, libname2, prefix);

  sFree(&bufw);
}

extern SEXP getRxode2ParseDf(void);

void codegen(char *model, int show_ode, const char *prefix, const char *libname, const char *pMd5, const char *timeId, const char *libname2) {
  _rxode2parse_assignTranslation(getRxode2ParseDf());
  _rxode2parse_packages = getRxode2ParseGetPointerAssignment();
  if (show_ode == ode_printaux) {
    print_aux_info(model, prefix, libname, pMd5, timeId, libname2);
  } else {
    int i, j;
    char *buf;
    if (show_ode == ode_dydt){
      const char *extra = "";
      if (strncmp("rx_", libname, 3) != 0) extra = libname;
      writeHeader(md5, extra);
      for (int i = Rf_length(_rxode2parse_functionName); i--;) {
        sAppend(&sbOut, "#define %s _rx%s%s%ld\n",
                R_CHAR(STRING_ELT(_rxode2parse_functionName, i)),
                extra, md5, __timeId++);
      }
      sAppendN(&sbOut,"#include <rxode2_model_shared.h>\n",33);
      int mx = maxSumProdN;
      if (SumProdLD > mx) mx = SumProdLD;
      sAppend(&sbOut,"#define __MAX_PROD__ %d\n", mx);
      int baseSize = tb.statei-tb.nExtra+extraCmt - tb.sensi;
      if (tb.sensi > 0){
        // This converts CMT to user CMT in model
        // Hence CMT = 4 could translate in data to 44 with sensi=10
        // Then cmt=44 translates back to cmt-10 or 4.
        // This makes the sensitivity equations insensitive to CMT changes that occur in FOCEi
        sAppend(&sbOut,"#define _CMT ((fabs(CMT)<=%d) ? CMT : ((CMT<0) ? CMT+%d: CMT-%d))\n",
                baseSize, tb.sensi, tb.sensi);
      } else {
        sAppendN(&sbOut,"#define _CMT CMT\n", 17);
      }
      // Now define lhs lags
      prnt_vars(print_lhsLags, 1, "", "", 13);
      // And covariate/parameter lags
      prnt_vars(print_paramLags, 1, "", "", 15);
      // Add sync PP define
      prnt_vars(print_simeps, 1, "#define _SYNC_simeps_ for (int _svari=_solveData->neps; _svari--;){", "}\n", 15);
      prnt_vars(print_simeta, 1, "#define _SYNC_simeta_ for (int _ovari=_solveData->neta; _ovari--;){", "}\n", 16);
      writeBody0();
      sAppendN(&sbOut,"#include \"extraC.h\"\n", 20);
      writeBody1();
      for (int i = Rf_length(_rxode2parse_functionName); i--;) {
        sAppend(&sbOut,"  %s = (%s) R_GetCCallable(\"%s\", \"%s\");\n",
                R_CHAR(STRING_ELT(_rxode2parse_functionName, i)),
                R_CHAR(STRING_ELT(_rxode2parse_functionType, i)),
                R_CHAR(STRING_ELT(_rxode2parse_functionPackageName, i)),
                R_CHAR(STRING_ELT(_rxode2parse_functionPackageFunction, i)));
      }
      writeBody2();
      for (int i = Rf_length(_rxode2parse_packages); i--;) {
        const char* cur = R_CHAR(STRING_ELT(_rxode2parse_packages, i));
        sAppend(&sbOut,"    static rxode2_assignFuns2 %s_assignFuns2 = NULL;\n", cur);
        sAppend(&sbOut,"    if (%s_assignFuns2 == NULL) %s_assignFuns2 = (rxode2_assignFuns2)(R_GetCCallable(\"%s\", \"_%s_assignFuns2\"));\n",
                cur, cur, cur, cur);
        sAppend(&sbOut,"    %s_assignFuns2(rx, op, f, lag, rate, dur, mtime, me, indf, gettime, timeindex, handleEvid, getdur);\n",
                cur);
      }
      writeBody3();
      sAppend(&sbOut, "extern void  %sode_solver_solvedata (rx_solve *solve){\n  _solveData = solve;\n}\n",prefix);
      sAppend(&sbOut, "extern rx_solve *%sode_solver_get_solvedata(void){\n  return _solveData;\n}\n", prefix);
      sAppend(&sbOut, "SEXP %smodel_vars(void);\n", prefix);
      sAppendN(&sbOut,"\n", 1);
      sAppendN(&sbOut, "\n// prj-specific differential eqns\nvoid ", 40);
      sAppend(&sbOut, "%sdydt(int *_neq, double __t, double *__zzStateVar__, double *__DDtStateVar__)\n{\n  int _itwhile = 0;\n  (void)_itwhile;\n  int _cSub = _neq[1];\n  double t = __t + _solveData->subjects[_neq[1]].curShift;\n  (void)t;\n  (&_solveData->subjects[_cSub])->_rxFlag=1;\n  ", prefix);
    } else if (show_ode == ode_jac){
      sAppend(&sbOut, "// Jacobian derived vars\nvoid %scalc_jac(int *_neq, double __t, double *__zzStateVar__, double *__PDStateVar__, unsigned int __NROWPD__) {\n  int _itwhile = 0;\n  (void)_itwhile;\n    int _cSub=_neq[1];\n  double t = __t + _solveData->subjects[_neq[1]].curShift;\n  (void)t;\n  (&_solveData->subjects[_cSub])->_rxFlag=2;\n  ", prefix);
    } else if (show_ode == ode_ini){
      sAppend(&sbOut,  "// Functional based initial conditions.\nvoid %sinis(int _cSub, double *__zzStateVar__){\n  int _itwhile = 0;\n  (void)_itwhile;\n  \n  (&_solveData->subjects[_cSub])->_rxFlag=3;\n  ", prefix);
      if (foundF0){
        sAppendN(&sbOut, "  double t=0;\n", 14);
      }
    } else if (show_ode == ode_fbio){
      if (foundF){
        int nnn = tb.de.n;
        if (tb.linCmt){
          if (tb.hasKa){
            nnn+=2;
          } else {
            nnn+=1;
          }
        }
        sAppend(&sbOut,  "// Functional based bioavailability (returns amount)\ndouble %sF(int _cSub,  int _cmt, double _amt, double __t, double *__zzStateVar__){\n  int _itwhile = 0;\n  (void)_itwhile;\n  double *_f=_solveData->subjects[_cSub].cF;\n  (void)_f;\n  double t = __t + _solveData->subjects[_cSub].curShift;\n  (void)t;\n  (&_solveData->subjects[_cSub])->_rxFlag=4;\n  ",
                prefix, nnn);
        for (int jjj = nnn; jjj--;){
          sAppend(&sbOut, "  _f[%d]=1.0;\n",jjj);
        }
      } else {
        sAppend(&sbOut,  "// Functional based bioavailability\ndouble %sF(int _cSub,  int _cmt, double _amt, double __t, double *__zzStateVar__){\n return _amt;\n  ",
                prefix);
      }
    } else if (show_ode == ode_lag){
      if (foundLag){
        int nnn = tb.de.n;
        if (tb.linCmt){
          if (tb.hasKa){
            nnn+=2;
          } else {
            nnn+=1;
          }
        }
        sAppend(&sbOut,  "// Functional based absorption lag\ndouble %sLag(int _cSub,  int _cmt, double __t){\n  int _itwhile = 0;\n  (void)_itwhile;\n  double *restrict _alag = _solveData->subjects[_cSub].alag;\n  (void)_alag; \n  double t = __t + _solveData->subjects[_cSub].curShift;\n  (void)t;\n  (&_solveData->subjects[_cSub])->_rxFlag=5;\n  ",
                prefix, nnn);
        for (int jjj = nnn; jjj--;){
          sAppend(&sbOut, "  _alag[%d]=0.0;\n",jjj);
        }
      } else {
        sAppend(&sbOut,  "// Functional based absorption lag\ndouble %sLag(int _cSub,  int _cmt, double __t, double *__zzStateVar__){\n return __t;\n",
                prefix);
      }
    } else if (show_ode == ode_rate){
      if (foundRate){
        int nnn = tb.de.n;
        if (tb.linCmt){
          if (tb.hasKa){
            nnn+=2;
          } else {
            nnn+=1;
          }
        }
        sAppend(&sbOut,  "// Modeled zero-order rate\ndouble %sRate(int _cSub,  int _cmt, double _amt, double __t){\n    int _itwhile = 0;\n  (void)_itwhile;\n  double *restrict _rate= _solveData->subjects[_cSub].cRate;\n  (void)_rate;\n   double t = __t + _solveData->subjects[_cSub].curShift;\n  (void)t;\n  (&_solveData->subjects[_cSub])->_rxFlag=6;\n  ",
                prefix, nnn);
        for (int jjj = nnn; jjj--;){
          sAppend(&sbOut, "  _rate[%d]=0.0;\n",jjj);
        }
      } else {
        sAppend(&sbOut,  "// Modeled zero-order rate\ndouble %sRate(int _cSub,  int _cmt, double _amt, double __t, double *__zzStateVar__){\n return 0.0;\n",
                prefix);
      }
    } else if (show_ode == ode_dur){
      if (foundDur){
        int nnn = tb.de.n;
        if (tb.linCmt){
          if (tb.hasKa){
            nnn+=2;
          } else {
            nnn+=1;
          }
        }
        sAppend(&sbOut,  "// Modeled zero-order duration\ndouble %sDur(int _cSub,  int _cmt, double _amt, double __t){\n  int _itwhile = 0;\n  (void)_itwhile;\n double *restrict _dur = _solveData->subjects[_cSub].cDur;\n  (void)_dur;\n    double t = __t + _solveData->subjects[_cSub].curShift;\n  (void)t;\n  (&_solveData->subjects[_cSub])->_rxFlag=7;\n  ",
                prefix, nnn);
        for (int jjj = nnn; jjj--;){
          sAppend(&sbOut, "  _dur[%d]=0.0;\n",jjj);
        }
      } else {
        sAppend(&sbOut,  "// Modeled zero-order duration\ndouble %sDur(int _cSub,  int _cmt, double _amt, double __t){\n return 0.0;\n",
                prefix);
      }
    } else if (show_ode == ode_mtime){
      if (nmtime){
        sAppend(&sbOut,  "// Model Times\nvoid %smtime(int _cSub, double *_mtime){\n  int _itwhile = 0;\n  (void)_itwhile;\n  double t = 0;\n  (&_solveData->subjects[_cSub])->_rxFlag=8;\n  ",
                prefix);
      } else {
        sAppend(&sbOut,  "// Model Times\nvoid %smtime(int _cSub, double *_mtime){\n",
                prefix);
      }
    } else if (show_ode == ode_mexp){
      sAppend(&sbOut, "// Matrix Exponential (%d)\nvoid %sME(int _cSub, double _t, double __t, double *_mat, const double *__zzStateVar__){\n  int _itwhile = 0;\n  (void)_itwhile;\n  double t = __t + _solveData->subjects[_cSub].curShift;\n  (void)t;\n  (&_solveData->subjects[_cSub])->_rxFlag=9;\n  ",
              tb.matn, prefix);
    } else if (show_ode == ode_indLinVec) {
      sAppend(&sbOut, "// Inductive linearization Matf\nvoid %sIndF(int _cSub, double _t, double __t, double *_matf){\n int _itwhile = 0;\n  (void)_itwhile;\n  double t = __t + _solveData->subjects[_cSub].curShift;\n  (void)t;\n  (&_solveData->subjects[_cSub])->_rxFlag=10;\n  ", prefix);
    } else {
      sAppend(&sbOut,  "// prj-specific derived vars\nvoid %scalc_lhs(int _cSub, double __t, double *__zzStateVar__, double *_lhs) {\n    int _itwhile = 0;\n  (void)_itwhile;\n  double t = __t + _solveData->subjects[_cSub].curShift;\n  (void)t;\n  (&_solveData->subjects[_cSub])->_rxFlag=11;\n  ", prefix);
    }
    if ((show_ode == ode_jac && found_jac == 1 && good_jac == 1) ||
        (show_ode != ode_jac && show_ode != ode_ini && show_ode != ode_fbio &&
         show_ode != ode_dur && show_ode != ode_rate && show_ode != ode_lag &&
         show_ode != ode_lhs && show_ode != ode_mtime && show_ode != ode_mexp &&
         show_ode != ode_indLinVec) ||
        (show_ode == ode_dur && foundDur) ||
        (show_ode == ode_rate && foundRate) ||
        (show_ode == ode_lag && foundLag) ||
        (show_ode == ode_fbio && foundF) ||
        (show_ode == ode_ini && foundF0) ||
        (show_ode == ode_lhs && tb.li) ||
        (show_ode == ode_mtime && nmtime) ||
        (show_ode == ode_mexp && tb.matn) ||
        (show_ode == ode_indLinVec && tb.matnf)){
      prnt_vars(print_double, 0, "", "\n",show_ode);     /* declare all used vars */
      if (maxSumProdN > 0 || SumProdLD > 0){
        int mx = maxSumProdN;
        if (SumProdLD > mx) mx = SumProdLD;
        sAppend(&sbOut,  "  double _p[%d], _input[%d];\n", mx, mx);
        sAppend(&sbOut,  "  double _pld[%d];\n", mx);
        sAppend(&sbOut,  "  for (int ddd=%d; ddd--;){_p[ddd]=_input[ddd]=_pld[ddd]=0.0;}", mx);

      }
      if (maxUdf > 0) {
        sAppend(&sbOut,  "  double __udf[%d];\n", maxUdf);
      }
      else prnt_vars(print_void, 0, "  (void)t;\n", "\n",show_ode);     /* declare all used vars */
      if (maxSumProdN){
        sAppendN(&sbOut,  "  (void)_p;\n  (void)_input;\n", 28);
        if (SumProdLD){
          sAppendN(&sbOut,  "  (void)_pld;\n", 14);
        }
      }
      prnt_vars(print_lastLhsValue, 0,"","\n", 12);
      if (show_ode == ode_ini){
        sAppendN(&sbOut, "  _update_par_ptr(0.0, _cSub, _solveData, _idx);\n", 49);
      } else if (show_ode == ode_lag || show_ode == ode_rate || show_ode == ode_dur ||
                 show_ode == ode_mtime){
        // functional lag, rate, duration, mtime
        sAppendN(&sbOut, "  _update_par_ptr(NA_REAL, _cSub, _solveData, _idx);\n", 53);
      } else if (show_ode == ode_indLinVec || show_ode == ode_mexp){
        sAppendN(&sbOut, "  _update_par_ptr(_t, _cSub, _solveData, _idx);\n", 48);
      } else {
        sAppendN(&sbOut, "  _update_par_ptr(__t, _cSub, _solveData, _idx);\n", 49);
      }
      prnt_vars(print_populateParameters, 1, "", "\n",show_ode);                   /* pass system pars */
      if (show_ode != ode_mtime && show_ode != ode_indLinVec){
        for (i=0; i<tb.de.n; i++) {                   /* name state vars */
          buf = tb.ss.line[tb.di[i]];
          if(tb.idu[i] != 0){
            if (show_ode == ode_lag || show_ode == ode_dur || show_ode == ode_rate){
              sAppendN(&sbOut, "  ", 2);
              doDot(&sbOut, buf);
              sAppend(&sbOut, " = NA_REAL;\n", i, i);
            } else {
              // stateExtra
              sAppendN(&sbOut, "  ", 2);
              doDot(&sbOut, buf);
              sAppend(&sbOut, " = __zzStateVar__[%d]*((double)(_ON[%d]));\n", i, i);
            }
          } else {
            break;
          }
        }
        sAppendN(&sbOut, "\n", 1);
      }
    }
    if ((foundDur && show_ode == ode_dur) ||
        (foundRate && show_ode == ode_rate) ||
        (foundLag && show_ode == ode_lag) ||
        (foundF && show_ode == ode_fbio) ||
        (foundF0 && show_ode == ode_ini) ||
        (show_ode == ode_lhs && tb.li) ||
        (show_ode == ode_mtime && nmtime) ||
        (show_ode == ode_jac && found_jac == 1 && good_jac == 1) ||
        (show_ode != ode_mtime && show_ode != ode_lhs &&
         show_ode != ode_jac && show_ode != ode_ini &&
         show_ode != ode_fbio && show_ode != ode_lag  &&
         show_ode != ode_rate && show_ode != ode_dur)){
      for (i = 0; i < sbPm.n; i++){
        switch(sbPm.lType[i]){
        case TLIN:
          if (show_ode != ode_mexp && show_ode != ode_indLinVec &&
              show_ode != ode_fbio && show_ode != ode_lag &&
              show_ode != ode_rate && show_ode != ode_dur){
            sAppend(&sbOut,"  %s",show_ode == ode_dydt ? sbPm.line[i] : sbPmDt.line[i]);
          }
          break;
        case TMTIME:
        case TASSIGN:
          if (show_ode != ode_mexp && show_ode != ode_indLinVec){
            sAppend(&sbOut,"  %s",show_ode == ode_dydt ? sbPm.line[i] : sbPmDt.line[i]);
          }
          break;
        case TINI:
          // See if this is an ini or a reclaimed expression.
          if (show_ode != ode_mexp && show_ode != ode_indLinVec){
            if (sbPm.lProp[i] >= 0 ){
              tb.ix = sbPm.lProp[i];
              if (tb.lh[tb.ix] == isLHS || tb.lh[tb.ix] == isLHSparam){
                sAppend(&sbOut,"  %s",show_ode == ode_dydt ? sbPm.line[i] : sbPmDt.line[i]);
              }
            }
          }
          break;
        case TF0:
          // functional ini
          if (show_ode == ode_ini) sAppend(&sbOut,"  %s",sbPmDt.line[i]);
          break;
        case FBIO:
          if (show_ode == ode_fbio) sAppend(&sbOut,"  %s", sbPmDt.line[i]);
          break;
        case ALAG:
          if (show_ode == ode_lag) sAppend(&sbOut, "  %s", sbPmDt.line[i]);
          break;
        case RATE:
          if (show_ode == ode_rate) sAppend(&sbOut, "  %s", sbPmDt.line[i]);
          break;
        case DUR:
          if (show_ode == ode_dur) sAppend(&sbOut,"  %s", sbPmDt.line[i]);
          break;
        case TJAC:
          if (show_ode == ode_lhs) sAppend(&sbOut, "  %s", sbPmDt.line[i]);
          else if (show_ode == ode_jac)  sAppend(&sbOut, "  %s", sbPm.line[i]);
          break;
        case TDDT:
          // d/dt()
          if (show_ode != ode_ini && show_ode != ode_fbio && show_ode != ode_lag &&
              show_ode != ode_rate && show_ode != ode_dur && show_ode != ode_mtime &&
              show_ode !=10 && show_ode != ode_indLinVec){
            sAppend(&sbOut, "  %s", show_ode == ode_dydt ? sbPm.line[i] : sbPmDt.line[i]);
          }
          break;
        case PPRN:
          // Rprintf
          if ((fullPrint && show_ode != ode_mexp && show_ode != ode_indLinVec) || (!fullPrint && show_ode == 1)) {
            sAppend(&sbOut, "  %s", show_ode == ode_dydt ? sbPm.line[i] : sbPmDt.line[i]);
          }
          break;
        case TLOGIC:
          if (show_ode != ode_mexp && show_ode != ode_indLinVec){
            sAppend(&sbOut,"  %s",show_ode == ode_dydt ? sbPm.line[i] : sbPmDt.line[i]);
          }
          break;
        case TMAT0:
          if (show_ode == ode_mexp){
            sAppend(&sbOut,"  %s", sbPm.line[i]);
          }
          break;
        case TMATF:
          if (show_ode == ode_indLinVec){
            sAppend(&sbOut,"  %s", sbPm.line[i]);
          }
          break;
        default:
          RSprintf("line Number: %d\n", i);
          RSprintf("type: %d\n", sbPm.lType[i]);
          RSprintf("line: %s\n", sbPm.line[i]);
          RSprintf("PmDt Line: %s\n", sbPmDt.line[i]);
          RSprintf("Prop: %d\n", sbPm.lProp[i]);
        }
      }
      // End statements
      switch (show_ode){
      case ode_dur:
        // RATE
        sAppendN(&sbOut, "\n  return _dur[_cmt];\n", 22);
        break;
      case ode_rate:
        // DUR
        sAppendN(&sbOut, "\n  return _rate[_cmt];\n", 23);
        break;
      case ode_lag:
        // Alag
        sAppendN(&sbOut, "\n  return t + _alag[_cmt] - _solveData->subjects[_cSub].curShift;\n", 66);
        break;
      case ode_fbio:
        sAppendN(&sbOut, "\n  return _f[_cmt]*_amt;\n", 25);
        break;
      }
    }
    if (show_ode == ode_dydt){
      sAppendN(&sbOut,  "  (&_solveData->subjects[_cSub])->dadt_counter[0]++;\n}\n\n", 56);
    } else if (show_ode == ode_jac){
      //sAppendN(&sbOut, "  free(__ld_DDtStateVar__);\n");
      sAppendN(&sbOut,  "  (&_solveData->subjects[_cSub])->jac_counter[0]++;\n", 52);
      sAppendN(&sbOut,  "}\n", 2);
    } else if (show_ode == ode_ini){
      if (foundF0){
        for (i = 0; i < tb.de.n; i++) {
          if (tb.idu[i]) {
            buf=tb.ss.line[tb.di[i]];
            sAppend(&sbOut, "  __zzStateVar__[%d]=((double)(_ON[%d]))*(",i,i);
            doDot(&sbOut, buf);
            sAppendN(&sbOut,  ");\n", 3);
          }
        }
      }
      sAppendN(&sbOut,  "}\n", 2);
    } else if (show_ode == ode_fbio || show_ode == ode_lag || show_ode == ode_rate ||
               show_ode == ode_dur){
      sAppendN(&sbOut,  "}\n", 2);
    } else if (show_ode == ode_lhs && tb.li){
      sAppendN(&sbOut,  "\n", 1);
      for (i=0, j=0; i<NV; i++) {
        if (tb.lh[i] != isLHS && tb.lh[i] != isLhsStateExtra && tb.lh[i] != isLHSparam) continue;
        buf = tb.ss.line[i];
        sAppend(&sbOut,  "  _lhs[%d]=", j);
        doDot(&sbOut, buf);
        sAppendN(&sbOut,  ";\n", 2);
        j++;
      }
      sAppendN(&sbOut,  "}\n", 2);
    } else if (show_ode == ode_mtime && nmtime){
      sAppendN(&sbOut,  "\n", 1);
      for (i=0, j=0; i<NV; i++) {
        if (tb.mtime[i] != 1) continue;
        buf = tb.ss.line[i];
        sAppend(&sbOut,  "  _mtime[%d]=", j);
        doDot(&sbOut, buf);
        sAppendN(&sbOut,  ";\n", 2);
        j++;
      }
      sAppendN(&sbOut,  "}\n", 2);
    } else {
      sAppendN(&sbOut,  "}\n", 2);
    }
  }
  _rxode2parse_unprotect();
}

void writeSb(sbuf *sbb, FILE *fp){
  // Adapted from ideas by Christian H
  // http://forums.codeguru.com/showthread.php?77477-What-is-the-fastest-way-to-write-data-to-a-file
  unsigned totalWritten=0;
  const unsigned OS_PAGESIZE = 4*1024;
  while( totalWritten < sbb->o) {
    register unsigned toWrite = min( OS_PAGESIZE, sbb->o - totalWritten);
    register unsigned written = fwrite(sbb->s + totalWritten, 1, toWrite, fp);
    if( toWrite != written){
      fclose(fp);
      _rxode2parse_unprotect();
      err_trans("IO error writing parsed C file");
    } else{
      totalWritten += written; // add the written bytes
    }
  }
  if (totalWritten != sbb->o) {
    fclose(fp);
    _rxode2parse_unprotect();
    err_trans("IO error writing parsed C file");
  }
}

extern SEXP _goodFuns;

SEXP _rxode2parse_codegen(SEXP c_file, SEXP prefix, SEXP libname,
                          SEXP pMd5, SEXP timeId, SEXP mvLast,
                          SEXP goodFuns){
  _goodFuns = PROTECT(goodFuns); _rxode2parse_protected++;
  if (!sbPm.o || !sbNrm.o){
    _rxode2parse_unprotect();
    err_trans("nothing in output queue to write");
  }
  if (!isString(c_file) || length(c_file) != 1){
    _rxode2parse_unprotect();
    err_trans("c_file should only be 1 file");
  }
  if (length(libname) != 2){
    _rxode2parse_unprotect();
    err_trans("libname needs 2 elements");
  }
  fpIO = fopen(CHAR(STRING_ELT(c_file,0)), "wb");
  err_msg((intptr_t) fpIO, "error opening output c file\n", -2);
  if (badMd5){
    SET_STRING_ELT(VECTOR_ELT(mvLast, RxMv_md5), 0, mkChar(""));
  } else {
    SET_STRING_ELT(VECTOR_ELT(mvLast, RxMv_md5), 0, mkChar(md5));
  }
  SET_STRING_ELT(VECTOR_ELT(mvLast, RxMv_model), 1, mkChar(me_code));
  int pro = 0;
  SEXP trans = PROTECT(VECTOR_ELT(mvLast, RxMv_trans)); pro++;
  sbuf buf; sNull(&buf);
  sIni(&buf);
  if (strcmp(CHAR(STRING_ELT(trans, 0)), CHAR(STRING_ELT(libname, 0)))) {
    SET_STRING_ELT(trans, 0, STRING_ELT(libname, 0)); // libname
    SET_STRING_ELT(trans, 2, STRING_ELT(prefix, 0)); // prefix
    const char *curPrefix = CHAR(STRING_ELT(prefix,0));
    sPrint(&buf, "%sdydt", curPrefix);
    SET_STRING_ELT(trans, 3, mkChar(buf.s)); // dydt
    sPrint(&buf, "%scalc_jac", curPrefix);
    SET_STRING_ELT(trans, 4, mkChar(buf.s)); // calc_jac
    sPrint(&buf, "%scalc_lhs", curPrefix);
    SET_STRING_ELT(trans, 5, mkChar(buf.s)); // calc_lhs
    sPrint(&buf, "%smodel_vars", curPrefix);
    SET_STRING_ELT(trans, 6, mkChar(buf.s)); // model_vars
    sPrint(&buf, "%stheta", curPrefix);
    SET_STRING_ELT(trans, 7, mkChar(buf.s)); // theta
    sPrint(&buf, "%sinis", curPrefix);
    SET_STRING_ELT(trans, 8, mkChar(buf.s)); // inis
    sPrint(&buf, "%sdydt_lsoda", curPrefix);
    SET_STRING_ELT(trans, 9, mkChar(buf.s)); // dydt_lsoda
    sPrint(&buf, "%scalc_jac_lsoda", curPrefix);
    SET_STRING_ELT(trans, 10, mkChar(buf.s)); // calc_jac_lsoda
    sPrint(&buf, "%sode_solver_solvedata", curPrefix);
    SET_STRING_ELT(trans, 11, mkChar(buf.s)); // ode_solver_solvedata
    sPrint(&buf, "%sode_solver_get_solvedata", curPrefix);
    SET_STRING_ELT(trans, 12, mkChar(buf.s)); // ode_solver_get_solvedata
    sPrint(&buf, "%sdydt_liblsoda", curPrefix);
    SET_STRING_ELT(trans, 13, mkChar(buf.s)); // dydt_liblsoda
    sPrint(&buf, "%sF", curPrefix);
    SET_STRING_ELT(trans, 14, mkChar(buf.s)); // F
    sPrint(&buf, "%sLag", curPrefix);
    SET_STRING_ELT(trans, 15, mkChar(buf.s)); // Lag
    sPrint(&buf, "%sRate", curPrefix);
    SET_STRING_ELT(trans, 16, mkChar(buf.s)); // Rate
    sPrint(&buf, "%sDur", curPrefix);
    SET_STRING_ELT(trans, 17, mkChar(buf.s)); // Dur
    sPrint(&buf, "%smtime", curPrefix);
    SET_STRING_ELT(trans, 18, mkChar(buf.s)); // mtime
    sPrint(&buf, "%sassignFuns", curPrefix);
    SET_STRING_ELT(trans, 19, mkChar(buf.s)); // assignFuns
    sPrint(&buf, "%sME", curPrefix);
    SET_STRING_ELT(trans, 20, mkChar(buf.s)); // ME
    sPrint(&buf, "%sIndF", curPrefix);
    SET_STRING_ELT(trans, 21, mkChar(buf.s)); // IndF
  }
  sPrint(&_mv, "%s", CHAR(STRING_ELT(PROTECT(_rxode2parse_rxQs(mvLast)), 0))); pro++;
  UNPROTECT(pro);
  sFree(&buf);
  //SET_STRING_ELT(tran, 0, mkChar());
  sFree(&sbOut);
  sIniTo(&sbOut, (int)((sbPm.sN)*5.3));
  // show_ode = 1 dydt
  // show_ode = 2 Jacobian
  // show_ode = 3 Ini statement
  // show_ode = 0 LHS
  // show_ode = 5 functional bioavailibility
  // show_ode = 6 functional rate
  if (tb.linCmt != 0) {
    char *buf;
    int badCentral=false, badDepot=false;
    for (int i=tb.de.n; i--;) {                     /* name state vars */
      buf=tb.ss.line[tb.di[i]];
      if (tb.hasKa == 1 && !strcmp(buf,"depot")){
        badDepot=true;
      } else if (!strcmp(buf, "central")) {
        badCentral=true;
      }
    }
    if (badCentral && badDepot){
      fclose(fpIO);
      _rxode2parse_unprotect();
      err_trans("linCmt() and ode have 'central' and 'depot' compartments, rename ODE 'central'/'depot'");
    } else if (badCentral) {
      fclose(fpIO);
      _rxode2parse_unprotect();
      err_trans("linCmt() and ode has a 'central' compartment, rename ODE 'central'");
    } else if (badDepot) {
      fclose(fpIO);
      _rxode2parse_unprotect();
      err_trans("linCmt() and ode has a 'depot' compartment, rename ODE 'depot'");
    }
    (&sbOut)->s[0]='\0';
    if (tb.hasKa == 1) {
      sAppend(&sbOut, "#define _DEPOT_ %d\n", tb.statei);
      sAppend(&sbOut, "#define _CENTRAL_ %d\n", tb.statei+1);
    } else if (tb.hasCentral == 1) {
      if (tb.hasDepot){
        fclose(fpIO);
        _rxode2parse_unprotect();
        err_trans("linCmt() does not have 'depot' compartment without a 'ka'");
        return R_NilValue;
      }
      sAppend(&sbOut, "#define _CENTRAL_ %d\n", tb.statei);
    }
    writeSb(&sbOut, fpIO);
  }
  gCode(1); // d/dt()
  gCode(2); // jac
  gCode(3); // ini()
  gCode(0); //
  gCode(5);
  gCode(6);
  gCode(7);
  gCode(8);
  gCode(9); // mtime
  gCode(10); //mat
  gCode(11); //matF
  gCode(4); // Registration
  writeFooter(); // undef
  fclose(fpIO);
  parseFree(0);
  reset();
  return R_NilValue;
}
