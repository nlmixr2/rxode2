#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "parseLinCmt.h"

char errLin[errLinLen];
int errOff = 0;

int _linCmtParsePro=0;



static inline void linCmtParseFinalizeStrings(linCmtStruct *lin, int verbose,
					      const char *first, const char *end1, const char *end2) {
  for (int i = Rf_length(lin->vars); i--;){
    linCmtStr(lin, CHAR(STRING_ELT(lin->vars, i)), &i);
  }
  linCmtAdjustPars(lin);
  lin->trans =-1;
  lin->ncmt = -1;
  sIni(&(lin->ret0));
  sIni(&(lin->ret));
  if (lin->cl != -1) {
    linCmtParseTransCl(lin, verbose);
  } else if (lin->kel != -1) {
    linCmtParseTranKel(lin, verbose);
  } else if (lin->aob != -1) {
    linCmtParseAOB(lin, verbose);
  }  else if (lin->k21 != -1) {
    linCmtParseTransK21(lin, verbose);
  } else if (lin->alpha != -1) {
    linCmtParseTransAlpha(lin, verbose);
  }
  sAppend(&(lin->ret), "%s", first);
  sAppend(&(lin->ret), "%d, %s", lin->ncmt, lin->ret0.s);
  sAppend(&(lin->ret), "%s", end1);
  if (lin->ka == -1) {
    sAppendN(&(lin->ret), "0.0", 3);
    if (verbose) RSprintf("\n");
  } else {
    sAppend(&(lin->ret), "%s", CHAR(STRING_ELT(lin->vars, lin->ka)));
    if (verbose) RSprintf(_(" with first order absorption\n"));
  }
  sAppend(&(lin->ret), "%s", end2);
}

static inline SEXP linCmtParseSEXP(linCmtStruct *lin) {
  int pro = 0;
  SEXP strV = PROTECT(allocVector(STRSXP, 1)); pro++;
  SEXP lst = PROTECT(allocVector(VECSXP, 3)); pro++;
  SEXP lstN = PROTECT(allocVector(STRSXP, 3)); pro++;

  SEXP transSXP = PROTECT(allocVector(INTSXP, 1)); pro++;
  INTEGER(transSXP)[0] = lin->trans;

  SEXP ncmtSXP = PROTECT(allocVector(INTSXP, 1)); pro++;
  INTEGER(ncmtSXP)[0] = lin->ncmt;

  SET_STRING_ELT(strV, 0, mkChar(lin->ret.s));
  SET_VECTOR_ELT(lst,  0, strV);
  SET_STRING_ELT(lstN, 0, mkChar("str"));

  SET_STRING_ELT(lstN, 1, mkChar("ncmt"));
  SET_VECTOR_ELT(lst,  1, ncmtSXP);

  SET_STRING_ELT(lstN, 2, mkChar("trans"));
  SET_VECTOR_ELT(lst,  2, transSXP);

  setAttrib(lst, R_NamesSymbol, lstN);
  sFree(&(lin->ret0));
  sFree(&(lin->ret));
  UNPROTECT(pro);
  if (lin->trans == -1) {
    UNPROTECT(_linCmtParsePro);
    _linCmtParsePro=0;
    err_trans("could not figure out linCmt() model");
  }
  _linCmtParsePro=0;
  return lst;
}


SEXP _linCmtParse(SEXP vars0, SEXP inStr, SEXP verboseSXP) {
  linCmtStruct lin;
  linCmtIni(&lin);
  lin.vars = vars0;
  int verbose = 0;
  if (TYPEOF(verboseSXP) == LGLSXP) {
    verbose = INTEGER(verboseSXP)[0];
  }
  const char *first = "linCmtB(rx__PTR__, t, ";
  const char *mid0 = "0, ";
  const char *end1 = "rx_tlag, rx_F, rx_rate, rx_dur,";
  const char *end2 = ", yrx_tlag2, rx_F2, rx_rate2, rx_dur2)";
  int type = TYPEOF(inStr);
  if (type == STRSXP) {
    int len = Rf_length(inStr);
    if (len > 0) {
      first = CHAR(STRING_ELT(inStr, 0));
    }
    if (len > 1) {
      mid0 = CHAR(STRING_ELT(inStr, 1));
    }
    if (len > 2) {
      end1 = CHAR(STRING_ELT(inStr, 2));
    }
    if (len > 3) {
      end2 = CHAR(STRING_ELT(inStr, 3));
    }
  }
  lin.mid = mid0;
  linCmtParseFinalizeStrings(&lin, verbose, first, end1, end2);
  return linCmtParseSEXP(&lin);
}

static inline void linCmtGenKa(linCmtGenStruct *linG) {
  // depot, central
  int i;
  for (i = 0; i < depotLines.n; i++){
    switch(depotLines.lType[i]){
    case FBIO:
      sClear(&(linG->d_F));
      sAppend(&(linG->d_F), "%s, ", depotLines.line[i]);
      break;
    case ALAG:
      sClear(&(linG->d_tlag));
      sAppend(&(linG->d_tlag), "%s, ", depotLines.line[i]);
      break;
    case RATE:
      sClear(&(linG->d_rate1));
      sAppend(&(linG->d_rate1), "%s, ", depotLines.line[i]);
      break;
    case DUR:
      sClear(&(linG->d_dur1));
      sAppend(&(linG->d_dur1), "%s, ", depotLines.line[i]);
      break;
    default:
      RSprintf("unknown depot line(%d): %s \n", depotLines.lType[i], depotLines.line[i]);
    }
  }
  for (i = 0; i < centralLines.n; i++){
    switch(centralLines.lType[i]){
    case FBIO:
      sClear(&(linG->d_F2));
      sAppend(&(linG->d_F2), "%s, ", centralLines.line[i]);
      break;
    case ALAG:
      sClear(&(linG->d_tlag2));
      sAppend(&(linG->d_tlag2), ", %s, ", centralLines.line[i]);
      break;
    case RATE:
      sClear(&(linG->d_rate2));
      sAppend(&(linG->d_rate2), "%s, ", centralLines.line[i]);
      break;
    case DUR:
      sClear(&(linG->d_dur2));
      sAppend(&(linG->d_dur2), "%s)", centralLines.line[i]);
      break;
    }
  }
}

static inline void linCmtGenBolus(linCmtGenStruct *linG) {
  int i;
  for (i = 0; i < depotLines.n; i++){
    switch(depotLines.lType[i]){
    case FBIO:
      sAppendN(&(linG->last), "'f(depot)' ", 11);
      break;
    case ALAG:
      sAppendN(&(linG->last), "'alag(depot)' ", 14);
      break;
    case RATE:
      sAppend(&(linG->last), "'rate(depot)' ", 14);
      break;
    case DUR:
      sAppend(&(linG->last), "'dur(depot)' ", 13);
      break;
    default:
      RSprintf("unknown depot line(%d): %s \n", depotLines.lType[i], depotLines.line[i]);
    }
  }
  if (linG->last.o) {
    errLin[0] = '\0';
    errOff=0;
    snprintf(errLin, errLinLen, "%s does not exist without a 'depot' compartment, specify a 'ka' parameter", linG->last.s);
    errOff=strlen(errLin);
    err_trans(errLin);
  }
  // central only
  for (i = 0; i < centralLines.n; i++){
    switch(centralLines.lType[i]){
    case FBIO:
      sClear(&(linG->d_F));
      sAppend(&(linG->d_F), "%s, ", centralLines.line[i]);
      break;
    case ALAG:
      sClear(&(linG->d_tlag));
      sAppend(&(linG->d_tlag), "%s, ", centralLines.line[i]);
      break;
    case RATE:
      sClear(&(linG->d_rate1));
      sAppend(&(linG->d_rate1), "%s, ", centralLines.line[i]);
      break;
    case DUR:
      sClear(&(linG->d_dur1));
      sAppend(&(linG->d_dur1), "%s, ", centralLines.line[i]);
      break;
    }
  }
}

static inline int linCmtGenFinalize(linCmtGenStruct *linG, SEXP linCmt, SEXP vars, SEXP linCmtSens, SEXP verbose, SEXP linCmtP) {
  for (int i = 0; i < sbNrmL.n; i++){
    if (sbNrmL.lProp[i]== -100){
      char *line = sbNrmL.line[i];
      if (line[0] != '\0') {
	while (strncmp(line, "linCmt(", 7)){
	  if (line[0] == '\0') {
	    return 1;
	  }
	  else sPut(&(linG->last2), line[0]);
	  line++;
	}
      }
      if (strlen(line) > 7) line +=7;
      else {
	return 1;
      }
      sAppend(&(linG->last2), "%s", CHAR(STRING_ELT(VECTOR_ELT(linCmtP, 0), 0)));
      while (line[0] != ')'){
	if (line[0] == '\0') {
	  return 1;
	}
	if (line[0] == '('){
	  return 2;
	}
	line++;
      }
      if (line[0] != '\0') sAppend(&(linG->last2), "%s", ++line);
    } else {
      sAppend(&(linG->last2), "%s", sbNrmL.line[i]);
    }
  }
  return 0;
}

static inline SEXP linCmtGenSEXP(linCmtGenStruct *linG, SEXP linCmt, SEXP vars, SEXP linCmtSens, SEXP verbose) {
  int pro=0;
  SEXP inStr = PROTECT(Rf_allocVector(STRSXP, 4)); pro++;
  int doSens = 0;
  if (TYPEOF(linCmtSens) == INTSXP){
    doSens = INTEGER(linCmtSens)[0];
  }
  sAppend(&(linG->last), "%s%s%s%s", linG->d_tlag.s, linG->d_F.s, linG->d_rate1.s, linG->d_dur1.s);
  SET_STRING_ELT(inStr, 2, mkChar(linG->last.s));
  sClear(&(linG->last));
  sAppend(&(linG->last), "%s%s%s%s",linG->d_tlag2.s, linG->d_F2.s,  linG->d_rate2.s, linG->d_dur2.s);
  SET_STRING_ELT(inStr, 3, mkChar(linG->last.s));
  sClear(&(linG->last));
  if (doSens == 2){
    sAppend(&(linG->last), "linCmtB(rx__PTR__, t, %d, ", INTEGER(linCmt)[0]);
    SET_STRING_ELT(inStr, 0, mkChar(linG->last.s));
    SET_STRING_ELT(inStr, 1, mkChar("0, "));
  } else {
    if (doSens == 1){
      sAppend(&(linG->last), "linCmtA(rx__PTR__, t, %d, ", INTEGER(linCmt)[0]);
    } else if (doSens == 3) {
      sAppend(&(linG->last), "linCmtC(rx__PTR__, t, %d, ", INTEGER(linCmt)[0]);
    }
    SET_STRING_ELT(inStr, 0, mkChar(linG->last.s));
    SET_STRING_ELT(inStr, 1, mkChar(""));
  }
  _linCmtParsePro=pro;
  SEXP linCmtP = PROTECT(_linCmtParse(vars, inStr, verbose)); pro++;
  switch(linCmtGenFinalize(linG, linCmt, vars, linCmtSens, verbose, linCmtP)) {
  case 1:
    UNPROTECT(pro);
    err_trans("linCmt() bad parse");
    return R_NilValue;
  case 2:
    UNPROTECT(pro);
    err_trans("linCmt() cannot have any extra parentheses in it");
    return R_NilValue;
    break;
  }
  SEXP ret = PROTECT(Rf_allocVector(STRSXP,1)); pro++;
  SET_STRING_ELT(ret, 0, mkChar(linG->last2.s));
  UNPROTECT(pro);
  return ret;
}

linCmtGenStruct _linCmtGenStruct;

SEXP _rxode2_linCmtGen(SEXP linCmt, SEXP vars, SEXP linCmtSens, SEXP verbose) {
  linCmtGenIni(&_linCmtGenStruct);
  /* SEXP ret = PROTECT(allocVector(STRSXP, 1)); */
  if (tb.hasKa){
    linCmtGenKa(&_linCmtGenStruct);
  } else {
    linCmtGenBolus(&_linCmtGenStruct);
  }
  return linCmtGenSEXP(&_linCmtGenStruct, linCmt, vars, linCmtSens, verbose);
}
