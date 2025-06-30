#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "genModelVars.h"

SEXP _rxode2parse_getUdf(void);
extern void calcLinCmt(void);
SEXP generateModelVars(void) {
  calcLinCmt();
  assertNoLinCmtDepotCentral();
  calcNparamsNlhsNslhs();
  calcNextra();


  int pro = 0;
  SEXP lst   = PROTECT(Rf_allocVector(VECSXP, 29));pro++;
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 29));pro++;

  SEXP sNeedSort = PROTECT(Rf_allocVector(INTSXP,1));pro++;
  int *iNeedSort  = INTEGER(sNeedSort);
  iNeedSort[0] = needSort;

  SEXP sLinCmt =PROTECT(calcSLinCmt());pro++;

  SEXP sMtime = PROTECT(Rf_allocVector(INTSXP,1));pro++;
  int *iMtime  = INTEGER(sMtime);
  iMtime[0] = (int)nmtime;

  SEXP tran  = PROTECT(Rf_allocVector(STRSXP, 22));pro++;
  SEXP trann = PROTECT(Rf_allocVector(STRSXP, 22));pro++;

  // These are the normal states
  SEXP state      = PROTECT(Rf_allocVector(STRSXP,tb.statei-tb.nExtra));pro++;
  SEXP stateProp  = PROTECT(Rf_allocVector(INTSXP,tb.statei-tb.nExtra));pro++;

  SEXP stateRmS   = PROTECT(Rf_allocVector(INTSXP,tb.statei-tb.nExtra));pro++;
  int *stateRm    = INTEGER(stateRmS);

  SEXP extraState = PROTECT(Rf_allocVector(STRSXP,tb.nExtra));pro++;

  SEXP sens       = PROTECT(Rf_allocVector(STRSXP,tb.sensi));pro++;
  SEXP sensProp   = PROTECT(Rf_allocVector(INTSXP,tb.sensi));pro++;

  SEXP normState  = PROTECT(Rf_allocVector(STRSXP,tb.statei-tb.sensi-tb.nExtra));pro++;
  SEXP normProp   = PROTECT(Rf_allocVector(INTSXP,tb.statei-tb.sensi-tb.nExtra));pro++;

  SEXP ordS = PROTECT(Rf_allocVector(INTSXP, tb.de.n));pro++;
  SEXP ordF = PROTECT(sortStateVectors(ordS)); pro++;
  if (Rf_isNull(ordF)) {
    UNPROTECT(pro);
    trans_syntax_error_report_fn0(_gbuf.s);
    return R_NilValue;
  }
  populateStateVectors(state, sens, normState, stateRm, extraState, stateProp, sensProp, normProp, INTEGER(ordF));
  SEXP dfdy = PROTECT(Rf_allocVector(STRSXP,tb.ndfdy));pro++;
  populateDfdy(dfdy);

  SEXP params = PROTECT(Rf_allocVector(STRSXP, tb.pi));pro++;
  SEXP lhsIn  = PROTECT(Rf_allocVector(STRSXP, tb.li));pro++;
  SEXP lhs    = PROTECT(Rf_allocVector(STRSXP, tb.li));pro++;
  SEXP lhsStrIn = PROTECT(Rf_allocVector(LGLSXP, tb.li));pro++;
  SEXP lhsStr = PROTECT(Rf_allocVector(LGLSXP, tb.li));pro++;
  SEXP lhsOrd = PROTECT(Rf_allocVector(INTSXP, tb.li)); pro++;
  SEXP slhs   = PROTECT(Rf_allocVector(STRSXP, tb.sli));pro++;
  SEXP interp = PROTECT(Rf_allocVector(INTSXP, tb.pi));pro++;

  SEXP version = PROTECT(calcVersionInfo());pro++;
  SEXP ini = PROTECT(calcIniVals()); pro++;

  SEXP model  = PROTECT(Rf_allocVector(STRSXP,2));pro++;
  SEXP modeln = PROTECT(Rf_allocVector(STRSXP,2));pro++;

  populateParamsLhsSlhs(params, lhsIn, slhs, INTEGER(interp), lhsStrIn,
                        INTEGER(lhsOrd));

  SEXP lhsOrdFS = PROTECT(orderForderS1(lhsOrd)); pro++;
  int *lhsOrdF = INTEGER(lhsOrdFS);
  int *lhsStrInI = INTEGER(lhsStrIn);
  int *lhsStrI = INTEGER(lhsStr);
  for (int i = 0; i < Rf_length(lhsOrdFS); ++i) {
    // change from 1 indexed to 0 indexed (will be saved 0 indexed)
    lhsOrdF[i] = lhsOrdF[i]-1;
    SET_STRING_ELT(lhs, i, STRING_ELT(lhsIn, lhsOrdF[i]));
    lhsStrI[i] = lhsStrInI[lhsOrdF[i]];
  }
  INTEGER(sLinCmt)[5] = tb.hasCmt;
  tb.ini_i = Rf_length(ini);

  SET_STRING_ELT(names,0,Rf_mkChar("params"));
  SET_VECTOR_ELT(lst,  0,params);

  SET_STRING_ELT(names,1,Rf_mkChar("lhs"));
  SET_VECTOR_ELT(lst,  1,lhs);

  SET_STRING_ELT(names,2,Rf_mkChar("state"));
  SET_VECTOR_ELT(lst,  2,state);

  SET_STRING_ELT(names,3,Rf_mkChar("trans"));
  SET_VECTOR_ELT(lst,  3,tran);

  SET_STRING_ELT(names,4,Rf_mkChar("model"));
  SET_VECTOR_ELT(lst,  4,model);

  SET_STRING_ELT(names,5,Rf_mkChar("ini"));
  SET_VECTOR_ELT(lst,  5,ini);

  SET_STRING_ELT(names,6,Rf_mkChar("dfdy"));
  SET_VECTOR_ELT(lst,  6,dfdy);

  SET_STRING_ELT(names,7,Rf_mkChar("sens"));
  SET_VECTOR_ELT(lst,  7,sens);

  SET_STRING_ELT(names,8,Rf_mkChar("state.ignore"));
  SET_VECTOR_ELT(lst,  8,stateRmS);

  SET_STRING_ELT(names,9,Rf_mkChar("version"));
  SET_VECTOR_ELT(lst,  9,version);

  SET_STRING_ELT(names,10,Rf_mkChar("normal.state"));
  SET_VECTOR_ELT(lst,  10,normState);

  SET_STRING_ELT(names,11,Rf_mkChar("needSort"));
  SET_VECTOR_ELT(lst,  11,sNeedSort);

  SET_STRING_ELT(names,12,Rf_mkChar("nMtime"));
  SET_VECTOR_ELT(lst,  12,sMtime);

  SET_STRING_ELT(names, 13, Rf_mkChar("extraCmt"));
  SEXP sExtraCmt = PROTECT(Rf_allocVector(INTSXP,1));pro++;
  INTEGER(sExtraCmt)[0] = 0;
  SET_VECTOR_ELT(lst, 13, sExtraCmt);

  SET_STRING_ELT(names, 14, Rf_mkChar("stateExtra"));
  SET_VECTOR_ELT(lst,   14, extraState);

  SET_STRING_ELT(names, 15, Rf_mkChar("dvid"));
  SEXP sDvid = PROTECT(Rf_allocVector(INTSXP,tb.dvidn));pro++;
  for (int i = 0; i < tb.dvidn; i++) INTEGER(sDvid)[i]=tb.dvid[i];
  SET_VECTOR_ELT(lst,  15, sDvid);

  SET_STRING_ELT(names, 16, Rf_mkChar("indLin"));
  SEXP matLst = PROTECT(Rf_allocVector(VECSXP, 0));pro++;
  SET_VECTOR_ELT(lst,  16, matLst);

  SET_STRING_ELT(names, 17, Rf_mkChar("flags"));
  SET_VECTOR_ELT(lst,   17, sLinCmt);

  SET_STRING_ELT(names, 18, Rf_mkChar("slhs"));
  SET_VECTOR_ELT(lst,   18, slhs);

  SEXP alagVarSexp = PROTECT(Rf_allocVector(INTSXP, tb.alagn));pro++;
  int *alagVar = INTEGER(alagVarSexp);
  int *ordFI = INTEGER(ordF);

  for (int i = 0; i < tb.alagn; ++i) {
    alagVar[i] = ordFI[tb.alag[i]-1];
  }
  SET_STRING_ELT(names, 19, Rf_mkChar("alag"));
  SET_VECTOR_ELT(lst,   19, alagVarSexp);

  sPrint(&_bufw,"%.*s", (int)strlen(model_prefix)-1, model_prefix);

  SET_STRING_ELT(trann,0,Rf_mkChar("lib.name"));
  SET_STRING_ELT(tran,0,Rf_mkChar(_bufw.s));

  SET_STRING_ELT(trann,1,Rf_mkChar("jac"));
  if (found_jac == 1 && good_jac == 1){
    SET_STRING_ELT(tran,1,Rf_mkChar("fulluser")); // Full User Matrix
  } else {
    SET_STRING_ELT(tran,1,Rf_mkChar("fullint")); // Full Internal Matrix
  }

  SET_STRING_ELT(trann,2,Rf_mkChar("prefix"));
  SET_STRING_ELT(tran,2,Rf_mkChar(_bufw.s));

  sPrint(&_bufw,"%sdydt",model_prefix);
  SET_STRING_ELT(trann,3,Rf_mkChar("dydt"));
  SET_STRING_ELT(tran,3,Rf_mkChar(_bufw.s)) ;

  sPrint(&_bufw,"%scalc_jac",model_prefix);
  SET_STRING_ELT(trann,4,Rf_mkChar("calc_jac"));
  SET_STRING_ELT(tran, 4,Rf_mkChar(_bufw.s));

  sPrint(&_bufw,"%scalc_lhs",model_prefix);
  SET_STRING_ELT(trann,5,Rf_mkChar("calc_lhs"));
  SET_STRING_ELT(tran, 5,Rf_mkChar(_bufw.s));

  sPrint(&_bufw,"%smodel_vars",model_prefix);
  SET_STRING_ELT(trann,6,Rf_mkChar("model_vars"));
  SET_STRING_ELT(tran, 6,Rf_mkChar(_bufw.s));

  sPrint(&_bufw,"%stheta",model_prefix);
  SET_STRING_ELT(trann,7,Rf_mkChar("theta"));
  SET_STRING_ELT(tran, 7,Rf_mkChar(_bufw.s));

  sPrint(&_bufw,"%sinis",model_prefix);
  SET_STRING_ELT(trann,8,Rf_mkChar("inis"));
  SET_STRING_ELT(tran, 8,Rf_mkChar(_bufw.s));

  sPrint(&_bufw,"%sdydt_lsoda",model_prefix);
  SET_STRING_ELT(trann,9,Rf_mkChar("dydt_lsoda"));
  SET_STRING_ELT(tran, 9,Rf_mkChar(_bufw.s));

  sPrint(&_bufw,"%scalc_jac_lsoda",model_prefix);
  SET_STRING_ELT(trann,10,Rf_mkChar("calc_jac_lsoda"));
  SET_STRING_ELT(tran, 10,Rf_mkChar(_bufw.s));

  sPrint(&_bufw,"%sode_solver_solvedata",model_prefix);
  SET_STRING_ELT(trann,11,Rf_mkChar("ode_solver_solvedata"));
  SET_STRING_ELT(tran, 11,Rf_mkChar(_bufw.s));

  sPrint(&_bufw,"%sode_solver_get_solvedata",model_prefix);
  SET_STRING_ELT(trann,12,Rf_mkChar("ode_solver_get_solvedata"));
  SET_STRING_ELT(tran, 12,Rf_mkChar(_bufw.s));

  sPrint(&_bufw,"%sdydt_liblsoda",model_prefix);
  SET_STRING_ELT(trann,13,Rf_mkChar("dydt_liblsoda"));
  SET_STRING_ELT(tran, 13,Rf_mkChar(_bufw.s));

  sPrint(&_bufw,"%sF",model_prefix);
  SET_STRING_ELT(trann,14,Rf_mkChar("F"));
  SET_STRING_ELT(tran, 14,Rf_mkChar(_bufw.s));

  sPrint(&_bufw,"%sLag",model_prefix);
  SET_STRING_ELT(trann,15,Rf_mkChar("Lag"));
  SET_STRING_ELT(tran, 15,Rf_mkChar(_bufw.s));

  sPrint(&_bufw,"%sRate",model_prefix);
  SET_STRING_ELT(trann,16,Rf_mkChar("Rate"));
  SET_STRING_ELT(tran, 16,Rf_mkChar(_bufw.s));

  sPrint(&_bufw,"%sDur",model_prefix);
  SET_STRING_ELT(trann,17,Rf_mkChar("Dur"));
  SET_STRING_ELT(tran, 17,Rf_mkChar(_bufw.s));

  sPrint(&_bufw,"%smtime",model_prefix);
  SET_STRING_ELT(trann,18,Rf_mkChar("mtime"));
  SET_STRING_ELT(tran, 18,Rf_mkChar(_bufw.s));

  sPrint(&_bufw,"%sassignFuns",model_prefix);
  SET_STRING_ELT(trann,19,Rf_mkChar("assignFuns"));
  SET_STRING_ELT(tran, 19,Rf_mkChar(_bufw.s));

  sPrint(&_bufw,"%sME",model_prefix);
  SET_STRING_ELT(trann,20,Rf_mkChar("ME"));
  SET_STRING_ELT(tran, 20,Rf_mkChar(_bufw.s));

  sPrint(&_bufw,"%sIndF",model_prefix);
  SET_STRING_ELT(trann,21,Rf_mkChar("IndF"));
  SET_STRING_ELT(tran, 21,Rf_mkChar(_bufw.s));

  SET_STRING_ELT(modeln,0,Rf_mkChar("normModel"));
  SET_STRING_ELT(model,0,Rf_mkChar(sbNrm.s));

  SET_STRING_ELT(modeln,1,Rf_mkChar("indLin"));
  SET_STRING_ELT(model,1,Rf_mkChar(me_code));

  SET_STRING_ELT(names, 20, Rf_mkChar("udf"));
  SEXP udf = PROTECT(_rxode2parse_getUdf());pro++;
  SET_VECTOR_ELT(lst,   20, udf);

  Rf_setAttrib(interp, R_NamesSymbol, params);
  SEXP clsInterp = PROTECT(Rf_allocVector(STRSXP, 1));pro++;
  SET_STRING_ELT(clsInterp, 0, Rf_mkChar("factor"));
  Rf_classgets(interp, clsInterp);

  SEXP lvlInterp = PROTECT(Rf_allocVector(STRSXP, 5));pro++;
  SET_STRING_ELT(lvlInterp, 0, Rf_mkChar("default"));
  SET_STRING_ELT(lvlInterp, 1, Rf_mkChar("linear"));
  SET_STRING_ELT(lvlInterp, 2, Rf_mkChar("locf"));
  SET_STRING_ELT(lvlInterp, 3, Rf_mkChar("nocb"));
  SET_STRING_ELT(lvlInterp, 4, Rf_mkChar("midpoint"));
  Rf_setAttrib(interp, R_LevelsSymbol, lvlInterp);

  SET_VECTOR_ELT(lst, 21, interp);
  SET_STRING_ELT(names, 21, Rf_mkChar("interp"));

  SEXP strAssign = PROTECT(Rf_allocVector(VECSXP, tb.str.n));pro++;
  SEXP strAssignN = PROTECT(Rf_allocVector(STRSXP, tb.str.n));pro++;
  for (int i = 0; i < tb.str.n; i++) {
    SEXP cur = PROTECT(Rf_allocVector(STRSXP, tb.sin[i]));pro++;
    int k = 0;
    for (int j = 0; j < tb.strVal.n; j++) {
      if (tb.strValI[j] == i) {
        SET_STRING_ELT(cur, k, Rf_mkChar(tb.strVal.line[j]));
        k++;
      }
    }
    SET_VECTOR_ELT(strAssign, i, cur);
    SET_STRING_ELT(strAssignN, i, Rf_mkChar(tb.str.line[i]));
  }
  Rf_setAttrib(strAssign, R_NamesSymbol, strAssignN);
  Rf_setAttrib(lhsStr, R_NamesSymbol, lhs);


  SET_VECTOR_ELT(lst, 22, strAssign);
  SET_STRING_ELT(names, 22, Rf_mkChar("strAssign"));

  SET_VECTOR_ELT(lst, 23, lhsStr);
  SET_STRING_ELT(names, 23, Rf_mkChar("lhsStr"));

  Rf_setAttrib(stateProp, R_NamesSymbol, state);
  SET_VECTOR_ELT(lst, 24, stateProp);
  SET_STRING_ELT(names, 24, Rf_mkChar("stateProp"));

  Rf_setAttrib(sensProp, R_NamesSymbol, sens);
  SET_VECTOR_ELT(lst, 25, sensProp);
  SET_STRING_ELT(names, 25, Rf_mkChar("sensProp"));

  Rf_setAttrib(normProp, R_NamesSymbol, normState);
  SET_VECTOR_ELT(lst, 26, normProp);
  SET_STRING_ELT(names, 26, Rf_mkChar("normProp"));

  Rf_setAttrib(ordF, R_NamesSymbol, state);
  SET_VECTOR_ELT(lst, 27, ordF);
  SET_STRING_ELT(names, 27, Rf_mkChar("stateOrd"));

  Rf_setAttrib(lhsOrdFS, R_NamesSymbol, lhs);

  SET_VECTOR_ELT(lst, 28, lhsOrdFS);
  SET_STRING_ELT(names, 28, Rf_mkChar("lhsOrd"));


  Rf_setAttrib(tran,  R_NamesSymbol, trann);
  Rf_setAttrib(lst,   R_NamesSymbol, names);
  Rf_setAttrib(model, R_NamesSymbol, modeln);
  SEXP cls = PROTECT(Rf_allocVector(STRSXP, 1));pro++;
  SET_STRING_ELT(cls, 0, Rf_mkChar("rxModelVars"));
  Rf_classgets(lst, cls);

  UNPROTECT(pro);
  return lst;
}
