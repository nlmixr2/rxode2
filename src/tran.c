#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>   /* dj: import intptr_t */
//#include "ode.h"
#include "../inst/include/rxode2parseSbuf.h"
#include "getOption.h"
#include "parseLinCmt.h"
#include "tran.h"
#include "genModelVars.h"
#include "print_node.h"
#include <errno.h>
#include <dparser.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rmath.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2parse", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif
#include "tran.g.d_parser.h"

#define MXSYM 50000
#define MXDER 5000
#define MXLEN 12000
/* #define SBUF_MXBUF 5 */
#define MXLINE 100
/* #define MXLINE 5 */

#include <string.h>
#include <stdlib.h>
#include <stddef.h>

#if (__STDC_VERSION__ >= 199901L)
#include <stdint.h>
#endif

void RSprintf(const char *format, ...);

// from mkdparse_tree.h
typedef void (print_node_fn_t)(int depth, char *token_name, char *token_value, void *client_data);

int syntaxErrorExtra = 0;
int isEsc=0;
int fullPrint = 0;
const char *lastStr;
int lastStrLoc=0;

SEXP _goodFuns;
vLines _dupStrs;

int rx_syntax_error = 0, rx_suppress_syntax_info=0, rx_syntax_require_ode_first = 1;

extern D_ParserTables parser_tables_rxode2parse;

unsigned int found_jac = 0, nmtime=0;
int rx_syntax_allow_ini = 1,
  maxSumProdN = 0, SumProdLD = 0, good_jac=1, extraCmt=0,
  maxUdf=0;

sbuf s_inits;

symtab tb;

static inline void addSymbolStr(char *value) {
  addLine(&(tb.ss),"%s",value);
  if (tb.depotN == -1 && !strcmp("depot", value)) {
    tb.depotN = NV-1;
  } else if (tb.centralN && !strcmp("central", value)){
    tb.centralN = NV-1;
  }
}


sbuf sb, sbDt; /* buffer w/ current parsed & translated line */
sbuf sbt;

sbuf firstErr;


int firstErrD=0;

vLines sbPm, sbPmDt, sbNrmL;
sbuf sbNrm;
sbuf sbExtra;
vLines depotLines, centralLines;

const char *model_prefix = NULL;
const char *me_code = NULL;
const char *md5 = NULL;
int badMd5 = 0;
int foundF=0,foundLag=0, foundRate=0, foundDur=0, foundF0=0, needSort=0;

sbuf sbOut;

int lastSyntaxErrorLine=0;
void updateSyntaxCol(void);

#include "parseVars.h"

char *gBuf;
int gBufFree=0;
int gBufLast = 0;
D_Parser *curP=NULL;
D_ParseNode *_pn = 0;

void freeP(void){
  if (_pn){
    free_D_ParseTreeBelow(curP,_pn);
    free_D_ParseNode(curP,_pn);
  }
  _pn=0;
  if (curP != NULL){
    free_D_Parser(curP);
  }
  curP = NULL;
}

int skipDouble=0;

int depotAttr=0, centralAttr=0;

sbuf _gbuf, _mv;

static inline int new_de(const char *s);
static inline int handleRemainingAssignmentsCalcProps(nodeInfo ni, char *name, int i, D_ParseNode *pn, D_ParseNode *xpn, char *v);
static inline int finalizeLineDdt(nodeInfo ni, char *name);
static inline int finalizeLineParam(nodeInfo ni, char *name);
static inline int isCmtLhsStatement(nodeInfo ni, char *name, char *v);
//static inline int add_deCmtProp(nodeInfo ni, char *name, char *v, int hasLhs, int fromWhere);
static inline void add_de(nodeInfo ni, char *name, char *v, int hasLhs, int fromWhere);

#include "parseFuns.h"
#include "parseLogical.h"
#include "parseIdentifier.h"
#include "parseIndLin.h"
#include "parseStatements.h"
#include "parseDfdy.h"
#include "parseCmtProperties.h"
#include "parseDdt.h"

static inline int parseNodePossiblySkipRecursion(nodeInfo ni, char *name, D_ParseNode *pn, D_ParseNode *xpn,
						 int *i, int nch, int *depth) {
  if (isSkipChild(ni, name, *i))  return 1;

  // Inductive linearization matrices
  handleIndLinMat0(ni, name);
  handleIndLinMatf(ni, name);
  // Determine if this is a function and change depth flag if needed
  setFunctionFlag(ni, name, *i, depth);
  if (handleIfElse(ni, name, *i) ||
      // simeta()/simeps()
      handleSimFunctions(ni, name, i, nch, pn) ||
      handleStringEqualityStatements(ni, name, *i, xpn) ||
      handleDvidStatement(ni, name, xpn, pn) ||
      handleFunctions(ni, name, i, depth, nch, xpn, pn) ||
      handleTheta(ni, name, xpn) ||
      handleEta(ni, name, xpn)) return 1;
  return 0;
}

static inline int parseNodeAfterRecursion(nodeInfo ni, char *name, D_ParseNode *pn, D_ParseNode *xpn,
					  int *i, int nch, int *depth, int *safe_zero,
					  int *ii, int *found, int *isWhile) {
  handleSafeZero(ni, name, *i, safe_zero, xpn);  // protect against divide by zeros
  if (handlePrintf(ni, name, *i, xpn) ||
      handleJac(ni, name, *i, xpn, ii, found) ||
      handleLogicalExpr(ni, name, *i, pn, xpn, isWhile) ||
      handleCmtProperty(ni, name, *i, xpn) ||
      handleDdtAssign(ni, name, *i, pn, xpn) ||
      handleDdtRhs(ni, name, xpn)) return 1;
  if (*i==0 && nodeHas(power_expression)) {
    aAppendN("),", 2);
    sAppendN(&sbt, "^", 1);
  }
  handleRemainingAssignments(ni, name, *i, pn, xpn);
  return 0;
}

void wprint_parsetree(D_ParserTables pt, D_ParseNode *pn, int depth, print_node_fn_t fn, void *client_data) {
  char *name = (char*)pt.symbols[pn->symbol].name;
  nodeInfo ni;
  niReset(&ni);
  int nch = d_get_number_of_children(pn), i, ii, found, safe_zero = 0;
  char *value = (char*)rc_dup_str(pn->start_loc.s, pn->end);
  // Add symbol, check/flag if recursive
  handleIdentifier(ni, name, value);
  // Add (double) in front of function arguments
  handleFunctionArguments(name, depth);
  // print/change identifier/operator and change operator information (if needed)
  handleOperatorsOrPrintingIdentifiers(depth, fn, client_data, ni, name, value);

  if (nch != 0) {
    int isWhile=0;
    if (nodeHas(power_expression)) {
      aAppendN("Rx_pow(_as_dbleps(", 18);
    }
    for (i = 0; i < nch; i++) {
      D_ParseNode *xpn = d_get_child(pn, i);

      if (parseNodePossiblySkipRecursion(ni, name, pn, xpn, &i, nch, &depth)) continue;

      // Recursively parse tree
      wprint_parsetree(pt, xpn, depth, fn, client_data);

      parseNodeAfterRecursion(ni, name, pn, xpn, &i, nch, &depth, &safe_zero,
			      &ii, &found, &isWhile);
    }
    finalizeLine(ni, name, pn, isWhile, i);
  }
}

void err_msg(int chk, const char *msg, int code)
{
  if(!chk) {
    parseFree(0);
    _rxode2parse_unprotect();
    Rf_errorcall(R_NilValue, "%s",msg);
  }
}

sbuf _bufw, _bufw2;

void parseFreeLast(void) {
  if (gBufFree) R_Free(gBuf);
  sFree(&sbOut);
  freeP();
  sFree(&_bufw);
  sFree(&_bufw2);
}

sbuf sbErr1;
sbuf sbErr2;

void parseFree(int last) {
  sFree(&sb);
  sFree(&sbDt);
  sFree(&sbt);
  sFree(&sbNrm);
  sFree(&sbExtra);
  sFree(&s_inits);
  sFree(&_bufw);
  sFree(&_bufw2);
  sFree(&firstErr);
  sFree(&_gbuf);
  sFree(&_mv);
  sFree(&sbErr1);
  sFree(&sbErr2);
  lineFree(&sbPm);
  lineFree(&sbPmDt);
  lineFree(&sbNrmL);
  lineFree(&(tb.ss));
  lineFree(&(tb.de));
  lineFree(&depotLines);
  lineFree(&centralLines);
  lineFree(&_dupStrs);
  linCmtGenFree(&_linCmtGenStruct);
  R_Free(tb.lh);
  R_Free(tb.lag);
  R_Free(tb.alag);
  R_Free(tb.ini);
  R_Free(tb.mtime);
  R_Free(tb.iniv);
  R_Free(tb.ini0);
  R_Free(tb.di);
  R_Free(tb.idi);
  R_Free(tb.idu);
  R_Free(tb.dvid);
  R_Free(tb.df);
  R_Free(tb.dy);
  R_Free(tb.sdfdy);
  freeP();
  if (last){
    parseFreeLast();
  }
}

SEXP _rxode2parse_parseFreeSexp(SEXP last) {
  parseFree(INTEGER(last)[0]);
  return R_NilValue;
}

char *alagLinCmtLine = NULL;
char *fLinCmtLine = NULL;
char *durLinCmtLine = NULL;
char *rateLinCmtLine = NULL;

char *alag1LinCmtLine = NULL;
char *f1LinCmtLine = NULL;
char *rate1LinCmtLine = NULL;
char *dur1LinCmtLine = NULL;

SEXP _rxode2parse_resetUdf(void);

void reset(void) {
  // Reset sb/sbt string buffers
  parseFree(0);
  sIniTo(&_bufw, 1024);
  sIniTo(&_bufw2, 2100);
  sIniTo(&sb, MXSYM);
  sIniTo(&sbDt, MXDER);
  sIniTo(&sbt, SBUF_MXBUF);
  sIniTo(&sbNrm, SBUF_MXBUF);
  sIniTo(&sbExtra,SBUF_MXBUF);
  sIniTo(&_gbuf, 1024);
  sIniTo(&sbErr1, SBUF_MXBUF);
  sIniTo(&sbErr2, SBUF_MXBUF);
  sIni(&_mv);
  sClear(&_mv);
  sIniTo(&firstErr, SBUF_MXBUF);
  firstErrD=0;

  sIniTo(&s_inits, MXSYM);

  lineIni(&sbPm);
  lineIni(&sbPmDt);
  lineIni(&sbNrmL);
  lineIni(&depotLines);
  lineIni(&centralLines);
  lineIni(&_dupStrs);

  lineIni(&(tb.ss));
  lineIni(&(tb.de));

  tb.lh		= R_Calloc(MXSYM, int);
  tb.ini	= R_Calloc(MXSYM, int);
  tb.mtime	= R_Calloc(MXSYM, int);
  tb.iniv	= R_Calloc(MXSYM, double);
  tb.ini0	= R_Calloc(MXSYM, int);
  tb.di		= R_Calloc(MXDER, int);
  tb.idi	= R_Calloc(MXDER, int);
  tb.idu	= R_Calloc(MXDER, int);
  tb.lag	= R_Calloc(MXSYM, int);
  tb.alag   = R_Calloc(MXSYM, int);
  tb.alagn  = 0;
  tb.dvid	= R_Calloc(MXDER, int);
  tb.thread     = 1; // Thread safe flag
  tb.dvidn      = 0;
  tb.ix		= 0;
  tb.id         = 0;
  tb.fn		= 0;
  tb.ixL        = -1;
  tb.didEq      = 0;
  tb.NEnd       = -1;
  tb.pos_de	= 0;
  tb.ini_i	= 0;
  tb.statei	= 0;
  tb.nExtra     = 0;
  tb.sensi	= 0;
  tb.li		= 0;
  tb.sli	= 0;
  tb.pi		= 0;
  tb.isPi       = 0;
  tb.isNA       = 0;
  tb.linCmt     = 0;
  tb.linCmtN    = -100;
  tb.linCmtFlg  = 0;
  tb.df		= R_Calloc(MXSYM, int);
  tb.dy		= R_Calloc(MXSYM, int);
  tb.sdfdy	= R_Calloc(MXSYM, int);
  tb.cdf	= 0;
  tb.ndfdy	= 0;
  tb.maxtheta   = 0;
  tb.hasCmt     = 0;
  tb.maxeta     = 0;
  tb.hasDepot   = 0;
  tb.hasCentral = 0;
  tb.hasDepotCmt = 0;
  tb.hasCentralCmt = 0;
  tb.hasKa      = 0;
  tb.allocS	= MXSYM;
  tb.allocD	= MXDER;
  tb.matn	= 0;
  tb.matnf	= 0;
  tb.ncmt	= 0;
  tb.linB	= 0;
  tb.curPropN	= 0;
  tb.depotN	= -1;
  tb.centralN	= -1;
  tb.linExtra   = false;
  tb.nwhile     = 0;
  tb.nInd       = 0;
  tb.simflg     = 0;
  tb.nLlik      = 0;
  // Reset Arrays
  // Reset integers
  NV		= 0;

  // reset globals
  good_jac = 1;
  found_jac = 0;
  rx_syntax_error = 0;
  rx_suppress_syntax_info=0;
  rx_syntax_allow_ini = 1;

  maxSumProdN = 0;
  SumProdLD = 0;

  foundDur=0;
  foundF0=0;
  nmtime=0;
  syntaxErrorExtra=0;
  lastSyntaxErrorLine=0;
  foundF=0;
  foundLag=0;
  foundRate=0;
  gBufLast=0;
  lastStrLoc=0;
  lastSyntaxErrorLine=0;
  needSort=0;
  nmtime=0;
  syntaxErrorExtra=0;
  extraCmt=0;

  // reset extra lines from linCmt()
  alagLinCmtLine = NULL;
  fLinCmtLine = NULL;
  durLinCmtLine = NULL;
  rateLinCmtLine = NULL;

  alag1LinCmtLine = NULL;
  f1LinCmtLine = NULL;
  rate1LinCmtLine = NULL;
  dur1LinCmtLine = NULL;
  _rxode2parse_resetUdf();
}

static void rxSyntaxError(struct D_Parser *ap);

static inline void assertCorrectDfDy(void) {
  char *buf1, *buf2, bufe[2048];
  int i, j, found, islhs;
  for (i=0; i<tb.ndfdy; i++) {                     /* name state vars */
    buf1=tb.ss.line[tb.df[i]];
    found=0;
    for (j=0; j<tb.de.n; j++) {                     /* name state vars */
      buf2=tb.ss.line[tb.di[j]];
      if (!strcmp(buf1, buf2)){
	found=1;
	break;
      }
    }
    if (!found){
      buf2=tb.ss.line[tb.dy[i]];
      snprintf(bufe, 2048, NOSTATE,buf1,buf2,buf1);
      trans_syntax_error_report_fn0(bufe);
    }
    // Now the dy()
    buf1=tb.ss.line[tb.dy[i]];
    found=0;
    for (j=0; j<tb.de.n; j++) {                     /* name state vars */
      buf2=tb.ss.line[tb.di[j]];
      if (!strcmp(buf1, buf2)){
	found=1;
	break;
      }
    }
    if (!found){
      for (j=0; j<NV; j++) {
	islhs = tb.lh[j];
	buf2=tb.ss.line[j];
	if (islhs>1 && tb.lh[i] != isLhsStateExtra) continue; /* is a state var */
	buf2=tb.ss.line[j];
	if ((islhs != 1 || tb.ini[j] == 1) &&!strcmp(buf1, buf2)){
	  found=1;
	  // This is a df(State)/dy(Parameter)
	  tb.sdfdy[i] = 1;
	  break;
	}
      }
    }
    if (!found){
      buf2=tb.ss.line[tb.df[i]];
      buf2=tb.ss.line[tb.dy[i]];
      snprintf(bufe,2048,NOSTATEVAR,buf1,buf2,buf2);
      trans_syntax_error_report_fn0(bufe);
    }
  }
}

void trans_internal(const char* parse_file, int isStr){
  freeP();
  curP = new_D_Parser(&parser_tables_rxode2parse, sizeof(D_ParseNode_User));
  curP->save_parse_tree = 1;
  curP->error_recovery = 1;
  curP->initial_scope = NULL;
  curP->syntax_error_fn = rxSyntaxError;
  if (isStr){
    if (gBufFree) R_Free(gBuf);
    // Should be able to use gBuf directly, but I believe it cause
    // problems with R's garbage collection, so duplicate the string.
    gBuf = (char*)(parse_file);
    gBufFree=0;
  } else {
    if (gBufFree) R_Free(gBuf);
    gBuf = rc_sbuf_read(parse_file);
    gBufFree=1;
    err_msg((intptr_t) gBuf, "error: empty buf for FILE_to_parse\n", -2);
  }
  sFree(&sbNrm);
  sFree(&sbExtra);
  sIniTo(&sbNrm, SBUF_MXBUF);
  sIniTo(&sbExtra, SBUF_MXBUF);
  lineIni(&sbPm);
  lineIni(&sbPmDt);
  lineIni(&sbNrmL);
  // do not free these, they remain until next parse for quick parsing of linCmt() models
  lineIni(&depotLines);
  lineIni(&centralLines);

  _pn= dparse(curP, gBuf, (int)strlen(gBuf));
  if (!_pn || curP->syntax_errors) {
    rx_syntax_error = 1;
  } else {
    wprint_parsetree(parser_tables_rxode2parse, _pn, 0, wprint_node, NULL);
    // Determine Jacobian vs df/dvar
    assertCorrectDfDy();
  }
}


extern int _rxode2parse_protected;
static inline int setupTrans(SEXP parse_file, SEXP prefix, SEXP model_md5, SEXP parseStr,
                             SEXP isEscIn, SEXP inME, SEXP goodFuns, SEXP fullPrintIn) {
  _goodFuns = PROTECT(goodFuns); _rxode2parse_protected++;
  // Make sure buffers are initialized.
  isEsc=INTEGER(isEscIn)[0];
  fullPrint=INTEGER(fullPrintIn)[0];

  int isStr =INTEGER(parseStr)[0];
  reset();
  rx_suppress_syntax_info = R_get_option("rxode2.suppress.syntax.info",0);
  rx_syntax_allow_ini  = R_get_option("rxode2.syntax.allow.ini",1);
  rx_syntax_require_ode_first = R_get_option("rxode2.syntax.require.ode.first",1);
  set_d_use_r_headers(0);
  set_d_rdebug_grammar_level(0);
  set_d_verbose_level(0);

  if (isString(prefix) && length(prefix) == 1){
    model_prefix = CHAR(STRING_ELT(prefix,0));
  } else {
    _rxode2parse_unprotect();
    err_trans("model prefix must be specified");
  }

  if (isString(inME) && length(inME) == 1){
    me_code = CHAR(STRING_ELT(inME,0));
  } else {
    freeP();
    _rxode2parse_unprotect();
    err_trans("extra ME code must be specified");
  }

  if (isString(model_md5) && length(model_md5) == 1){
    md5 = CHAR(STRING_ELT(model_md5,0));
    badMd5 = 0;
    if (strlen(md5)!= 32){
      badMd5=1;
    }
  } else {
    badMd5=1;
  }
  return isStr;
}

static inline void finalizeSyntaxError(void) {
  if (rx_syntax_error){
    if(!rx_suppress_syntax_info){
      if (gBuf[gBufLast] != '\0'){
	gBufLast++;
	RSprintf("\n:%03d: ", lastSyntaxErrorLine);
	for (; gBuf[gBufLast] != '\0'; gBufLast++){
	  if (gBuf[gBufLast] == '\n'){
	    RSprintf("\n:%03d: ", ++lastSyntaxErrorLine);
	  } else{
	    RSprintf("%c", gBuf[gBufLast]);
	  }
	}
      }
      if (isEsc){
	RSprintf("\n\033[1m================================================================================\033[0m\n");
      }
      else {
	RSprintf("\n================================================================================\n");
      }
    }
    if (firstErrD == 1) {
      firstErrD=0;
      _rxode2parse_unprotect();
      err_trans(firstErr.s);
    } else {
      _rxode2parse_unprotect();
      err_trans("syntax errors (see above)");
    }
  }
}

void _rxode2parse_assignTranslation(SEXP df);
SEXP getRxode2ParseDf(void);

SEXP _rxode2parse_trans(SEXP parse_file, SEXP prefix, SEXP model_md5, SEXP parseStr,
                   SEXP isEscIn, SEXP inME, SEXP goodFuns, SEXP fullPrintIn){
  const char *in = NULL;
  _rxode2parse_assignTranslation(getRxode2ParseDf());
  int isStr = setupTrans(parse_file, prefix, model_md5, parseStr, isEscIn, inME, goodFuns, fullPrintIn);
  in = CHAR(STRING_ELT(parse_file,0));
  trans_internal(in, isStr);
  SEXP lst = PROTECT(generateModelVars());
  finalizeSyntaxError();
  UNPROTECT(1);
  _rxode2parse_unprotect();
  return lst;
}

SEXP _rxode2parse_parseModel(SEXP type){
  if (!sbPm.o){
    _rxode2parse_unprotect();
    err_trans("model no longer loaded in memory");
  }
  int iT = INTEGER(type)[0];
  SEXP pm;
  switch (iT){
  case 1:
    pm = PROTECT(allocVector(STRSXP, sbPmDt.n));
    for (int i = 0; i < sbPmDt.n; i++){
      SET_STRING_ELT(pm, i, mkChar(sbPmDt.line[i]));
    }
    break;

  default:
    pm = PROTECT(allocVector(STRSXP, sbPm.n));
    for (int i = 0; i < sbPm.n; i++){
      SET_STRING_ELT(pm, i, mkChar(sbPm.line[i]));
    }
    break;
  }
  UNPROTECT(1);
  return pm;
}

SEXP _rxode2parse_codeLoaded(void){
  SEXP pm = PROTECT(allocVector(INTSXP, 1));
  if (!sbPm.o || !sbNrm.o){
    INTEGER(pm)[0]=0;
  } else {
    INTEGER(pm)[0]=1;
  }
  UNPROTECT(1);
  return pm;
}

SEXP _rxode2parse_isLinCmt(void) {
  SEXP ret = PROTECT(allocVector(INTSXP, 1));
  INTEGER(ret)[0]=tb.linCmt;
  UNPROTECT(1);
  return ret;
}

#include "parseSyntaxErrors.h"

////////////////////////////////////////////////////////////////////////////////
// linCmtParse

// Taken from dparser and changed to use R_Calloc
char * rc_dup_str(const char *s, const char *e) {
  lastStr=s;
  int l = e ? e-s : (int)strlen(s);
  syntaxErrorExtra=min(l-1, 40);
  addLine(&_dupStrs, "%.*s", l, s);
  return _dupStrs.line[_dupStrs.n-1];
}

void transIniNull(void) {
  sNull(&(s_inits));
  lineNull(&(tb.ss));
  lineNull(&(tb.de));
  sNull(&(sb));
  sNull(&(sbDt));
  sNull(&(sbt));
  sNull(&(firstErr));
  sNull(&(sbNrm));
  sNull(&(sbExtra));
  sNull(&(sbOut));
  lineNull(&(sbPm));
  lineNull(&(sbPmDt));
  lineNull(&(sbNrmL));
  lineNull(&(depotLines));
  lineNull(&(centralLines));
  sNull(&(_gbuf));
  sNull(&(_mv));
  sNull(&(_bufw));
  sNull(&(_bufw2));
  lineNull(&(_dupStrs));
}
