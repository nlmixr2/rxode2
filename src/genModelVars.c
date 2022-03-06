#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "genModelVars.h"

SEXP generateModelVars() {
  calcExtracmt();
  calcNparamsNlhsNslhs();
  calcNextra();

  int pro = 0;
  SEXP lst   = PROTECT(allocVector(VECSXP, 19));pro++;
  SEXP names = PROTECT(allocVector(STRSXP, 19));pro++;

  SEXP sNeedSort = PROTECT(allocVector(INTSXP,1));pro++;
  int *iNeedSort  = INTEGER(sNeedSort);
  iNeedSort[0] = needSort;

  SEXP sLinCmt =PROTECT(calcSLinCmt());pro++;

  SEXP sMtime = PROTECT(allocVector(INTSXP,1));pro++;
  int *iMtime  = INTEGER(sMtime);
  iMtime[0] = (int)nmtime;

  SEXP tran  = PROTECT(allocVector(STRSXP, 22));pro++;
  SEXP trann = PROTECT(allocVector(STRSXP, 22));pro++;

  SEXP state      = PROTECT(allocVector(STRSXP,tb.statei-tb.nExtra));pro++;
  SEXP stateRmS   = PROTECT(allocVector(INTSXP,tb.statei-tb.nExtra));pro++;
  int *stateRm    = INTEGER(stateRmS);
  SEXP extraState = PROTECT(allocVector(STRSXP,tb.nExtra));pro++;
  SEXP sens     = PROTECT(allocVector(STRSXP,tb.sensi));pro++;
  SEXP normState= PROTECT(allocVector(STRSXP,tb.statei-tb.sensi-tb.nExtra));pro++;

  populateStateVectors(state, sens, normState, stateRm, extraState);

  SEXP dfdy = PROTECT(allocVector(STRSXP,tb.ndfdy));pro++;
  populateDfdy(dfdy);


  SEXP params = PROTECT(allocVector(STRSXP, tb.pi));pro++;
  SEXP lhs    = PROTECT(allocVector(STRSXP, tb.li));pro++;
  SEXP slhs   = PROTECT(allocVector(STRSXP, tb.sli));pro++;

  SEXP version = PROTECT(calcVersionInfo());pro++;
  SEXP ini = PROTECT(calcIniVals()); pro++;


  SEXP model  = PROTECT(allocVector(STRSXP,2));pro++;
  SEXP modeln = PROTECT(allocVector(STRSXP,2));pro++;

  populateParamsLhsSlhs(params, lhs, slhs);


  INTEGER(sLinCmt)[5] = tb.hasCmt;
  tb.ini_i = length(ini);

  SET_STRING_ELT(names,0,mkChar("params"));
  SET_VECTOR_ELT(lst,  0,params);

  SET_STRING_ELT(names,1,mkChar("lhs"));
  SET_VECTOR_ELT(lst,  1,lhs);

  SET_STRING_ELT(names,2,mkChar("state"));
  SET_VECTOR_ELT(lst,  2,state);

  SET_STRING_ELT(names,3,mkChar("trans"));
  SET_VECTOR_ELT(lst,  3,tran);

  SET_STRING_ELT(names,4,mkChar("model"));
  SET_VECTOR_ELT(lst,  4,model);

  SET_STRING_ELT(names,5,mkChar("ini"));
  SET_VECTOR_ELT(lst,  5,ini);

  SET_STRING_ELT(names,6,mkChar("dfdy"));
  SET_VECTOR_ELT(lst,  6,dfdy);

  SET_STRING_ELT(names,7,mkChar("sens"));
  SET_VECTOR_ELT(lst,  7,sens);

  SET_STRING_ELT(names,8,mkChar("state.ignore"));
  SET_VECTOR_ELT(lst,  8,stateRmS);

  SET_STRING_ELT(names,9,mkChar("version"));
  SET_VECTOR_ELT(lst,  9,version);

  SET_STRING_ELT(names,10,mkChar("normal.state"));
  SET_VECTOR_ELT(lst,  10,normState);

  SET_STRING_ELT(names,11,mkChar("needSort"));
  SET_VECTOR_ELT(lst,  11,sNeedSort);

  SET_STRING_ELT(names,12,mkChar("nMtime"));
  SET_VECTOR_ELT(lst,  12,sMtime);

  SET_STRING_ELT(names, 13, mkChar("extraCmt"));
  SEXP sExtraCmt = PROTECT(allocVector(INTSXP,1));pro++;
  INTEGER(sExtraCmt)[0] = extraCmt;
  SET_VECTOR_ELT(lst, 13, sExtraCmt);

  SET_STRING_ELT(names, 14, mkChar("stateExtra"));
  SET_VECTOR_ELT(lst,   14, extraState);

  SET_STRING_ELT(names, 15, mkChar("dvid"));
  SEXP sDvid = PROTECT(allocVector(INTSXP,tb.dvidn));pro++;
  for (int i = 0; i < tb.dvidn; i++) INTEGER(sDvid)[i]=tb.dvid[i];
  SET_VECTOR_ELT(lst,  15, sDvid);

  SET_STRING_ELT(names, 16, mkChar("indLin"));
  SEXP matLst = PROTECT(allocVector(VECSXP, 0));pro++;
  SET_VECTOR_ELT(lst,  16, matLst);

  SET_STRING_ELT(names, 17, mkChar("flags"));
  SET_VECTOR_ELT(lst,   17, sLinCmt);

  SET_STRING_ELT(names, 18, mkChar("slhs"));
  SET_VECTOR_ELT(lst,   18, slhs);

  sPrint(&_bufw,"%.*s", (int)strlen(model_prefix)-1, model_prefix);

  SET_STRING_ELT(trann,0,mkChar("lib.name"));
  SET_STRING_ELT(tran,0,mkChar(_bufw.s));

  SET_STRING_ELT(trann,1,mkChar("jac"));
  if (found_jac == 1 && good_jac == 1){
    SET_STRING_ELT(tran,1,mkChar("fulluser")); // Full User Matrix
  } else {
    SET_STRING_ELT(tran,1,mkChar("fullint")); // Full Internal Matrix
  }

  SET_STRING_ELT(trann,2,mkChar("prefix"));
  SET_STRING_ELT(tran,2,mkChar(_bufw.s));

  sPrint(&_bufw,"%sdydt",model_prefix);
  SET_STRING_ELT(trann,3,mkChar("dydt"));
  SET_STRING_ELT(tran,3,mkChar(_bufw.s)) ;

  sPrint(&_bufw,"%scalc_jac",model_prefix);
  SET_STRING_ELT(trann,4,mkChar("calc_jac"));
  SET_STRING_ELT(tran, 4,mkChar(_bufw.s));

  sPrint(&_bufw,"%scalc_lhs",model_prefix);
  SET_STRING_ELT(trann,5,mkChar("calc_lhs"));
  SET_STRING_ELT(tran, 5,mkChar(_bufw.s));

  sPrint(&_bufw,"%smodel_vars",model_prefix);
  SET_STRING_ELT(trann,6,mkChar("model_vars"));
  SET_STRING_ELT(tran, 6,mkChar(_bufw.s));

  sPrint(&_bufw,"%stheta",model_prefix);
  SET_STRING_ELT(trann,7,mkChar("theta"));
  SET_STRING_ELT(tran, 7,mkChar(_bufw.s));

  sPrint(&_bufw,"%sinis",model_prefix);
  SET_STRING_ELT(trann,8,mkChar("inis"));
  SET_STRING_ELT(tran, 8,mkChar(_bufw.s));

  sPrint(&_bufw,"%sdydt_lsoda",model_prefix);
  SET_STRING_ELT(trann,9,mkChar("dydt_lsoda"));
  SET_STRING_ELT(tran, 9,mkChar(_bufw.s));

  sPrint(&_bufw,"%scalc_jac_lsoda",model_prefix);
  SET_STRING_ELT(trann,10,mkChar("calc_jac_lsoda"));
  SET_STRING_ELT(tran, 10,mkChar(_bufw.s));

  sPrint(&_bufw,"%sode_solver_solvedata",model_prefix);
  SET_STRING_ELT(trann,11,mkChar("ode_solver_solvedata"));
  SET_STRING_ELT(tran, 11,mkChar(_bufw.s));

  sPrint(&_bufw,"%sode_solver_get_solvedata",model_prefix);
  SET_STRING_ELT(trann,12,mkChar("ode_solver_get_solvedata"));
  SET_STRING_ELT(tran, 12,mkChar(_bufw.s));

  sPrint(&_bufw,"%sdydt_liblsoda",model_prefix);
  SET_STRING_ELT(trann,13,mkChar("dydt_liblsoda"));
  SET_STRING_ELT(tran, 13,mkChar(_bufw.s));

  sPrint(&_bufw,"%sF",model_prefix);
  SET_STRING_ELT(trann,14,mkChar("F"));
  SET_STRING_ELT(tran, 14,mkChar(_bufw.s));

  sPrint(&_bufw,"%sLag",model_prefix);
  SET_STRING_ELT(trann,15,mkChar("Lag"));
  SET_STRING_ELT(tran, 15,mkChar(_bufw.s));

  sPrint(&_bufw,"%sRate",model_prefix);
  SET_STRING_ELT(trann,16,mkChar("Rate"));
  SET_STRING_ELT(tran, 16,mkChar(_bufw.s));

  sPrint(&_bufw,"%sDur",model_prefix);
  SET_STRING_ELT(trann,17,mkChar("Dur"));
  SET_STRING_ELT(tran, 17,mkChar(_bufw.s));

  sPrint(&_bufw,"%smtime",model_prefix);
  SET_STRING_ELT(trann,18,mkChar("mtime"));
  SET_STRING_ELT(tran, 18,mkChar(_bufw.s));

  sPrint(&_bufw,"%sassignFuns",model_prefix);
  SET_STRING_ELT(trann,19,mkChar("assignFuns"));
  SET_STRING_ELT(tran, 19,mkChar(_bufw.s));

  sPrint(&_bufw,"%sME",model_prefix);
  SET_STRING_ELT(trann,20,mkChar("ME"));
  SET_STRING_ELT(tran, 20,mkChar(_bufw.s));

  sPrint(&_bufw,"%sIndF",model_prefix);
  SET_STRING_ELT(trann,21,mkChar("IndF"));
  SET_STRING_ELT(tran, 21,mkChar(_bufw.s));

  SET_STRING_ELT(modeln,0,mkChar("normModel"));
  SET_STRING_ELT(model,0,mkChar(sbNrm.s));

  SET_STRING_ELT(modeln,1,mkChar("indLin"));
  SET_STRING_ELT(model,1,mkChar(me_code));

  setAttrib(tran,  R_NamesSymbol, trann);
  setAttrib(lst,   R_NamesSymbol, names);
  setAttrib(model, R_NamesSymbol, modeln);
  SEXP cls = PROTECT(allocVector(STRSXP, 1));pro++;
  SET_STRING_ELT(cls, 0, mkChar("rxModelVars"));
  classgets(lst, cls);

  UNPROTECT(pro);
  return lst;
}

