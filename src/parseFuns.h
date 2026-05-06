// -*- mode: C++; c-indent-level: 2; c-basic-offset: 2; indent-tabs-mode: nil; -*-
////////////////////////////////////////////////////////////////////////////////
// rxode2 parsing function routines

#include <ctype.h>
#include "threadSafeConstants.h"

SEXP rxode2_getUdf2(const char *fun, const int nargs);

static inline int isAtFunctionArg(const char *name) {
  return !strcmp("(", name) ||
    !strcmp(")", name) ||
    !strcmp(",", name);
}

static inline void handleFunctionArguments(char *name, int depth) {
  if (isAtFunctionArg(name)) {
    sPut(&sb, name[0]);
    sPut(&sbDt, name[0]);
    if (!skipDouble && !(strcmp(",", name)) && depth == 1){
      aAppendN("(double)", 8);
      skipDouble=0;
    }
    sPut(&sbt, name[0]);
  }
}


static inline void setFunctionFlag(nodeInfo ni, char *name, int i, int *depth) {
  tb.fn = (i==0 && (nodeHas(function)) ? 1 : 0);
  if (tb.fn == 0) tb.fn = (i==0 && (nodeHas(function_name)) ? 2 : 0);
  if (tb.fn == 1) *depth = 0;
}


static inline int handleSimFunctions(nodeInfo ni, char *name, int *i, int nch,
				     D_ParseNode *pn){
  if (nodeHas(simfun_statement) && *i == 0) {
    *i = nch; // done
    //if (tb.thread != threadSafe) tb.thread = threadSafeRepNumThread;
    sb.o=0;sbDt.o=0; sbt.o=0;
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    aType(TLOGIC);
    if (!strcmp("simeta", v)) {
      foundF0=1;
      if ((tb.simflg & 1) == 0) tb.simflg += 1;
    } else {
      if ((tb.simflg & 2) == 0) tb.simflg += 2;
    }
    sAppend(&sb, "%s(_cSub);\n  _SYNC_%s_;", v, v);
    sAppend(&sbDt, "%s(_cSub);\n  _SYNC_%s_;", v, v);
    sAppend(&sbt, "%s();", v);
    addLine(&sbPm, "%s\n", sb.s);
    addLine(&sbPmDt, "%s\n", sbDt.s);
    sAppend(&sbNrm, "%s\n", sbt.s);
    addLine(&sbNrmL, "%s\n", sbt.s);
    /* Free(v); */
    ENDLINE;
    return 1;
  }
  return 0;
}

static inline int handleSplitBolusStatement(nodeInfo ni, char *name, int *i, int nch,
                                            D_ParseNode *pn) {
  if (nodeHas(splitBolus_statement) && *i == 0) {
    if (tb.splitBolusN != 0) {
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("only one 'splitBolus()' statement is supported per model"));
    }
    *i = nch;
    sb.o = 0; sbDt.o = 0; sbt.o = 0;
    D_ParseNode *cSrc = d_get_child(pn, 2);
    D_ParseNode *c1 = d_get_child(pn, 4);
    D_ParseNode *rest = d_get_child(pn, 5);
    int nTargets = d_get_number_of_children(rest) + 1;
    int nCmts = nTargets + 1;
    char **vals = (char**)R_Calloc(nCmts, char*);
    vals[0] = (char*)rc_dup_str(cSrc->start_loc.s, cSrc->end);
    vals[1] = (char*)rc_dup_str(c1->start_loc.s, c1->end);
    for (int j = 0; j < nTargets - 1; ++j) {
      char *cur = (char*)rc_dup_str(d_get_child(rest, j)->start_loc.s,
                                    d_get_child(rest, j)->end);
      vals[j + 2] = cur + 1;
      while (*vals[j + 2] == ' ' || *vals[j + 2] == '\t') vals[j + 2]++;
    }
    for (int j = 1; j < nCmts; ++j) {
      for (int k = j + 1; k < nCmts; ++k) {
        if (!strcmp(vals[j], vals[k])) {
          updateSyntaxCol();
          trans_syntax_error_report_fn(_("'splitBolus()' target compartments must all be different"));
        }
      }
    }
    for (int j = 0; j < nCmts; ++j) {
      int hasLhs = isCmtLhsStatement(ni, name, vals[j]);
      if (new_de(vals[j], fromCMTprop)) {
        add_de(ni, name, vals[j], hasLhs, fromCMTprop);
      } else {
        new_or_ith(vals[j]);
      }
      tb.splitBolus[j] = tb.id + 1;
    }
    tb.splitBolusN = nCmts;
    sAppend(&sbt, "splitBolus(%s", vals[0]);
    for (int j = 1; j < nCmts; ++j) {
      sAppend(&sbt, ",%s", vals[j]);
    }
    sAppend(&sbt, ");");
    sAppend(&sbNrm, "%s\n", sbt.s);
    addLine(&sbNrmL, "%s\n", sbt.s);
    R_Free(vals);
    return 1;
  }
  return 0;
}

typedef struct transFunctions {
  int isNorm;
  int isExp;
  int isF;
  int isGamma;
  int isBeta;
  int isPois;
  int isT;
  int isUnif;
  int isWeibull;
  int isNormV;
  int isCauchy;
  int isLead;
  int isFirst;
  int isLast;
  int isDiff;
  int isLinB;
  int isPnorm;

  int isTad;
  int isTad0;

  int isTafd;
  int isTafd0;

  int isTlast;
  int isTlast0;

  int isDose;
  int isPodo;

  int isDose0;
  int isPodo0;

  int isTfirst;
  int isTfirst0;

  int isInd;
  nodeInfo ni;
  char *name;
  int *i;
  int *depth;
  int nch;
  D_ParseNode *xpn;
  D_ParseNode *pn;
  char *v;
} transFunctions;

static inline void transFunctionsIni(transFunctions *tf) {
  tf->isNorm=0;
  tf->isExp=0;
  tf->isF=0;
  tf->isGamma=0;
  tf->isBeta=0;
  tf->isPois=0;
  tf->isT=0;
  tf->isUnif=0;
  tf->isWeibull=0;
  tf->isNormV=0;
  tf->isCauchy=0;
  tf->isLead=0;
  tf->isFirst=0;
  tf->isLast=0;
  tf->isDiff=0;
  tf->isLinB=0;
  tf->isPnorm=0;

  tf->isTad=0;
  tf->isTad0=0;

  tf->isTafd=0;
  tf->isTafd0=0;

  tf->isTlast = 0;
  tf->isTlast0 = 0;

  tf->isDose = 0;
  tf->isPodo = 0;

  tf->isDose0 = 0;
  tf->isPodo0 = 0;


  tf->isTfirst = 0;
  tf->isTfirst0 = 0;

  tf->isInd=0;
}

transFunctions _tf;

#include "parseFunsDosing.h"

static inline int handleFunctionLogit(transFunctions *tf) {
  if (!strcmp("logit", tf->v) || !strcmp("expit", tf->v) ||
      !strcmp("invLogit", tf->v) || !strcmp("logitInv", tf->v) ||
      !strcmp("probit", tf->v) || !strcmp("probitInv", tf->v)){
    int ii = d_get_number_of_children(d_get_child(tf->pn,3))+1;
    if (ii == 1){
      D_ParseNode *xpn = d_get_child(tf->pn, 2);
      char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      if (allSpaces(v2)){
        updateSyntaxCol();
        sPrint(&_gbuf, _("'%s' takes 1-3 arguments '%s(x,low,high)'"),
               tf->v, tf->v);
        trans_syntax_error_report_fn(_gbuf.s);
      }
      /* Free(v2); */
      sAppend(&sb, "_%s1(", tf->v);
      sAppend(&sbDt,"_%s1(", tf->v);
      sAppend(&sbt, "%s(", tf->v);
    } else if (ii == 2) {
      sAppend(&sb, "_%s2(", tf->v);
      sAppend(&sbDt,"_%s2(", tf->v);
      sAppend(&sbt, "%s(", tf->v);
    } else if (ii == 3) {
      sAppend(&sb, "%s(", tf->v);
      sAppend(&sbDt,"%s(", tf->v);
      sAppend(&sbt, "%s(", tf->v);
    } else {
      updateSyntaxCol();
      sPrint(&_gbuf, _("'%s' takes 1-3 arguments '%s(x,low,high)'"),
	     tf->v, tf->v);
      trans_syntax_error_report_fn(_gbuf.s);
    }
    tf->i[0] = 1;// Parse next arguments
    tf->depth[0]=1;
    return 1;
  }
  return 0;
}

static inline int handleFunctionSum(transFunctions *tf) {
  if (!strcmp("prod",tf->v)   || !strcmp("sum", tf->v) || !strcmp("sign",  tf->v) ||
      !strcmp("max", tf->v)   || !strcmp("min", tf->v) ||
      !strcmp("rxord", tf->v) ||
      !strcmp("mix", tf->v)) {
    int ii = d_get_number_of_children(d_get_child(tf->pn,3))+1;
    if (!strcmp("prod", tf->v)) {
      sAppend(&sb, "_prod(_p, _input, _solveData->prodType, %d, (double) ", ii);
      sAppend(&sbDt, "_prod(_p, _input, _solveData->prodType, %d, (double) ", ii);
      if (maxSumProdN < ii){
        maxSumProdN = ii;
      }
    } else if (!strcmp("sum", tf->v)) {
      sAppend(&sb, "_sum(_p, _pld, -__MAX_PROD__, _solveData->sumType, %d, (double) ", ii);
      sAppend(&sbDt, "_sum(_p, _pld, -__MAX_PROD__, _solveData->sumType, %d, (double) ", ii);
      if (SumProdLD < ii) {
        SumProdLD = ii;
      }
    } else if (!strcmp("rxord", tf->v)) {
      sAppend(&sb, "_rxord(_cSub, %d, (double) ", ii);
      sAppend(&sbDt, "_rxord(_cSub, %d, (double) ", ii);
    } else if (!strcmp("mix", tf->v)) {
      sAppend(&sb, "_mix(_cSub, %d, (double) ", ii);
      sAppend(&sbDt, "_mix(_cSub, %d, (double) ", ii);
      if (ii % 2 == 0 || ii < 3) {
        sPrint(&_gbuf,
               _("'mix' takes an odd number of arguments (and at least 3) supplied %d"),
               ii);
        /* Free(v2); */
        trans_syntax_error_report_fn(_gbuf.s);
      }
      if (tb.hasMix == 0) {
        tb.hasMix = (ii + 1)/2; // number of mixtures
      } else if (tb.hasMix != (ii + 1)/2) {
        sPrint(&_gbuf,
               _("'mix' cannot change the number of arguments (%d) in a model (ie mixnum from %d to %d)"),
               ii, tb.hasMix, (ii + 1)/2);
        /* Free(v2); */
        trans_syntax_error_report_fn(_gbuf.s);
      }
    } else {
      sAppend(&sb, "_%s(%d, (double) ", tf->v, ii);
      sAppend(&sbDt, "_%s(%d, (double) ", tf->v, ii);
    }
    sAppend(&sbt, "%s(", tf->v);
    /* Free(tf->v); */
    tf->i[0] = 1;// Parse next arguments
    tf->depth[0]=1;
    return 1;
  }
  return 0;
}

#include "parseFunsDiff.h"
#include "parseFunsRandom.h"
#include "parseFunsNa.h"
#include "parseFunsLinCmt.h"

static inline int handleFunctionsExceptLinCmt(transFunctions *tf) {
  return handleFunctionDosenum(tf) ||
    handleFunctionTad(tf) ||
    handleFunctionSum(tf) ||
    handleFunctionLogit(tf) ||
    handleFunctionDiff(tf) ||
    handleFunctionPnorm(tf) ||
    handleFunctionTransit(tf) ||
    handleFunctionRxnorm(tf) ||
    handleFunctionRchisq(tf) ||
    handleFunctionRgeom(tf) ||
    handleFunctionRbinom(tf) ||
    handleFunctionRnbinom(tf) ||
    handleFunctionRnbinomMu(tf) ||
    handleFunctionIsNan(tf) ||
    handleFunctionIsNa(tf) ||
    handleFunctionIsFinite(tf) ||
    handleFunctionIsInfinite(tf);
}

#define max2( a , b )  ( (a) > (b) ? (a) : (b) )

static inline void handleLlFunctions(transFunctions *tf) {
  if (!strncmp("llikX", tf->v, 5)) {
    D_ParseNode *xpn = d_get_child(tf->pn,2);
    char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    tb.nLlik = max2(tb.nLlik, toInt(v2)+1);
  } else if (!strncmp("llik", tf->v, 4)) {
    tb.nLlik = max2(tb.nLlik, 1);
  }
}

#undef max2

extern SEXP _rxode2parse_rxFunctionName;
extern SEXP _rxode2parse_functionArgMin;
extern SEXP _rxode2parse_functionArgMax;
extern SEXP _rxode2parse_funName;
extern SEXP _rxode2parse_funNameInt;
extern SEXP _rxode2parse_functionThreadSafe;


static inline int handleBadFunctions(transFunctions *tf) {
  // Split out to handle anticipated automatic conversion of R
  // functions to C
  int foundFun = 0;
  int curThread = 0;
  for (int j = Rf_length(_goodFuns); j--;){
    if (!strcmp(CHAR(STRING_ELT(_goodFuns, j)),tf->v)){
      int ii = d_get_number_of_children(d_get_child(tf->pn,3))+1;
      int argMin=-1, argMax=-1;
      for (int kk = Rf_length(_rxode2parse_rxFunctionName); kk--;) {
        if (!strcmp(CHAR(STRING_ELT(_rxode2parse_rxFunctionName, kk)),tf->v)) {
          argMin = INTEGER(_rxode2parse_functionArgMin)[kk];
          argMax = INTEGER(_rxode2parse_functionArgMax)[kk];
          curThread = INTEGER(_rxode2parse_functionThreadSafe)[kk];
          if (curThread == 0) tb.thread = notThreadSafe;
          if (argMin == NA_INTEGER || argMax == NA_INTEGER) {
            argMin = argMax = -1;
            break;
          }
          if (argMax < argMin) {
            int tmp = argMax;
            argMax = argMin;
            argMin = tmp;
          }
          break;
        }
      }
      if (argMin == -1) {
        // Check for more known args
        for (int kk = Rf_length(_rxode2parse_funName); kk--;) {
          if (!strcmp(CHAR(STRING_ELT(_rxode2parse_funName, kk)),tf->v)) {
            argMin = argMax = INTEGER(_rxode2parse_funNameInt)[kk];
            if (argMin == NA_INTEGER) {
              argMin=argMax=-1;
              break;
            }
            break;
          }
        }
      }
      if (argMin != -1) {
        if (argMin == argMax && argMin != ii) {
          updateSyntaxCol();
          sPrint(&_gbuf, _("'%s' takes %d arguments, supplied %d"),
                 tf->v, argMin, ii);
          /* Free(v2); */
          trans_syntax_error_report_fn(_gbuf.s);
          return 0;
        } else if (argMin > ii || argMax < ii) {
          sPrint(&_gbuf, _("'%s' takes %d-%d arguments, supplied %d"),
                 tf->v, argMin, argMax, ii);
          /* Free(v2); */
          trans_syntax_error_report_fn(_gbuf.s);
          return 0;
        }
      }
      // Save log-likelihood information
      handleLlFunctions(tf);
      foundFun = 1;
      j=0;
      break;
    }
  }
  if (foundFun == 0){
    int ii = d_get_number_of_children(d_get_child(tf->pn,3))+1;
    if (ii == 1) {
      D_ParseNode *xpn = d_get_child(tf->pn, 2);
      char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      int allSpace=allSpaces(v2);
      if (allSpace) ii = 0;
    }
    SEXP lst = PROTECT(rxode2_getUdf2(tf->v, ii));
    int udf = INTEGER(VECTOR_ELT(lst, 0))[0];
    const char *udfInfo = R_CHAR(STRING_ELT(VECTOR_ELT(lst, 1), 0));
    UNPROTECT(1);
    if (udf == NA_INTEGER) {
      sPrint(&_gbuf, "%s", udfInfo);
      updateSyntaxCol();
      trans_syntax_error_report_fn(_gbuf.s);
    } else {
      if (udf == -42) {
        sAppend(&sb, "_udf(\".rxUiUdfNone\", (double *) NULL, -42, (double) ", ii);
        sAppend(&sbDt, "_udf(\".rxUiUdfNone\",(double *) NULL, -42, (double) ", ii);
        sAppend(&sbt, "%s(", tf->v);
        tb.thread = notThreadSafe;
        tf->i[0] = 1;// Parse next arguments
        tf->depth[0]=1;
        return 1;
      } else if (udf != ii) {
        sPrint(&_gbuf, _("user function '%s' takes %d arguments, supplied %d"),
               tf->v, udf, ii);
        updateSyntaxCol();
        trans_syntax_error_report_fn(_gbuf.s);
      } else {
        if (maxUdf < ii){
          maxUdf = ii;
        }
        sAppend(&sb, "_udf(\"%s\", __udf, %d, (double) ", tf->v, ii);
        sAppend(&sbDt, "_udf(\"%s\", __udf, %d, (double) ", tf->v, ii);
        sAppend(&sbt, "%s(", tf->v);
        tb.thread = notThreadSafe;
        tf->i[0] = 1;// Parse next arguments
        tf->depth[0]=1;
        return 1;
      }
    }
  }
  return 0;
}

static inline int handleFunctions(nodeInfo ni, char *name, int *i, int *depth, int nch, D_ParseNode *xpn, D_ParseNode *pn) {
  if (tb.fn == 1) {
    transFunctions *tf = &_tf;
    transFunctionsIni(tf);
    tf->ni = ni;
    tf->name = name;
    tf->i = i;
    tf->depth = depth;
    tf->nch = nch;
    tf->xpn = xpn;
    tf->pn = pn;
    tf->v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    if (handleFunctionsExceptLinCmt(tf)) {
      return 1;
    } else if (handleFunctionLinCmt(tf)){
      return 0;
    } else if (handleBadFunctions(tf)) {
      return 1;
    }
  }
  return 0;
}

static inline int isStrInteger(const char *s) {
  if (!s || *s == '\0') return 0;
  if (*s == '+' || *s == '-') s++;
  if (!isdigit((unsigned char)*s)) return 0;
  while (*s) {
    if (!isdigit((unsigned char)*s)) return 0;
    s++;
  }
  return 1;
}

static inline const char *rxPushDoseCmtExpr(nodeInfo ni, char *name, char *vCmt) {
  while (*vCmt == ' ' || *vCmt == '\t') vCmt++;
  if (isStrInteger(vCmt)) {
    sPrint(&_gbuf, "(int)(%s)", vCmt);
    return _gbuf.s;
  }
  if (*vCmt == '\'' || *vCmt == '"') {
    char quote = *vCmt;
    vCmt++;
    char *tmp = vCmt;
    while (*tmp && *tmp != '\0') tmp++;
    while (tmp > vCmt && (tmp[-1] == ' ' || tmp[-1] == '\t')) tmp--;
    if (tmp > vCmt && tmp[-1] == quote) tmp--;
    tmp[0] = '\0';
  }
  int hasLhs = isCmtLhsStatement(ni, name, vCmt);
  if (new_de(vCmt, fromCMTprop)) {
    add_de(ni, name, vCmt, hasLhs, fromCMTprop);
    aProp(tb.de.n);
  } else {
    new_or_ith(vCmt);
    aProp(tb.ix);
  }
  // while DDT# is 0-indexed, the _rxPushDose() is 1-indexed, so add 1
  // to the DDT# for the _rxPushDose() call
  sPrint(&_gbuf, "(__DDT%d__ + 1)", tb.id);
  return _gbuf.s;
}

static inline int handleBolusStatement(nodeInfo ni, char *name, int *i, int nch,
                                       D_ParseNode *pn) {
  if (nodeHas(bolus_statement) && *i == 0) {
    tb.evid_ = 1;
    *i = nch; // skip all children; we process the whole statement at once
    sb.o = 0; sbDt.o = 0; sbt.o = 0;
    // Children: 0='bolus', 1='(', 2=amt, 3=',', 4=cmt, 5=',',
    //           6=ii, 7=',', 8=addl, 9=',', 10=ss, 11=')'
    D_ParseNode *cAmt  = d_get_child(pn, 2);
    D_ParseNode *cCmt  = d_get_child(pn, 4);
    D_ParseNode *cIi   = d_get_child(pn, 6);
    D_ParseNode *cAddl = d_get_child(pn, 8);
    D_ParseNode *cSs   = d_get_child(pn, 10);
    char *vAmt  = (char*)rc_dup_str(cAmt->start_loc.s,  cAmt->end);
    char *vCmt  = (char*)rc_dup_str(cCmt->start_loc.s,  cCmt->end);
    char *vIi   = (char*)rc_dup_str(cIi->start_loc.s,   cIi->end);
    char *vAddl = (char*)rc_dup_str(cAddl->start_loc.s, cAddl->end);
    char *vSs   = (char*)rc_dup_str(cSs->start_loc.s,   cSs->end);
    aType(TEVID);

    // Children: 0='evid_', 1='(', 2=time, 3=',', 4=evid, 5=',', 6=amt, 7=',',
    //           8=cmt, 9=',', 10=rate, 11=',', 12=ii, 13=',', 14=addl, 15=',',
    //           16=ss, 17=')'
    const char *cmtExpr = rxPushDoseCmtExpr(ni, name, vCmt);
    sAppend(&sb,  "_rxPushDose(_ind, t, t, 1, %s, %s, %s, 0.0, (int)(%s), (int)(%s), 0);\n",
            vAmt, cmtExpr, vIi, vAddl, vSs);
    sAppend(&sbDt,  "_rxPushDose(_ind, t, t, 1, %s, %s, %s, 0.0, (int)(%s), (int)(%s), 0);\n",
            vAmt, cmtExpr, vIi, vAddl, vSs);
    sAppend(&sbt, "bolus(%s, %s, %s, %s, %s);",
            vAmt, vCmt, vIi, vAddl, vSs);
    addLine(&sbPm,   "%s\n", sb.s);
    addLine(&sbPmDt, "%s\n", sbDt.s);
    sAppend(&sbNrm,  "%s\n", sbt.s);
    addLine(&sbNrmL, "%s\n", sbt.s);
    ENDLINE;
    return 1;
  }
  return 0;
}

static inline int handleInfuseStatement(nodeInfo ni, char *name, int *i, int nch,
                                        D_ParseNode *pn) {
  if (nodeHas(infuse_statement) && *i == 0) {
    tb.evid_ = 1;
    *i = nch; // skip all children; we process the whole statement at once
    sb.o = 0; sbDt.o = 0; sbt.o = 0;

    // Children: 0='infuse', 1='(', 2=amt, 3=',', 4=rate, 5=',',
    //           6=cmt, 7=',', 8=ii, 9=',', 10=addl, 11=','
    //           12=ss, 13=')'
    D_ParseNode *cAmt  = d_get_child(pn, 2);
    D_ParseNode *cRate = d_get_child(pn, 4);
    D_ParseNode *cCmt  = d_get_child(pn, 6);
    D_ParseNode *cIi   = d_get_child(pn, 8);
    D_ParseNode *cAddl = d_get_child(pn, 10);
    D_ParseNode *cSs   = d_get_child(pn, 12);

    char *vAmt  = (char*)rc_dup_str(cAmt->start_loc.s,  cAmt->end);
    char *vRate = (char*)rc_dup_str(cRate->start_loc.s,  cRate->end);
    char *vCmt  = (char*)rc_dup_str(cCmt->start_loc.s,  cCmt->end);
    char *vIi   = (char*)rc_dup_str(cIi->start_loc.s,   cIi->end);
    char *vAddl = (char*)rc_dup_str(cAddl->start_loc.s, cAddl->end);
    char *vSs   = (char*)rc_dup_str(cSs->start_loc.s,   cSs->end);
    aType(TEVID);

    // Children: 0='evid_', 1='(', 2=time, 3=',', 4=evid, 5=',', 6=amt, 7=',',
    //           8=cmt, 9=',', 10=rate, 11=',', 12=ii, 13=',', 14=addl, 15=',',
    //           16=ss, 17=')'
    const char *cmtExpr = rxPushDoseCmtExpr(ni, name, vCmt);
    //                                        amt,cmt,
    sAppend(&sb,  "_rxPushDose(_ind, t, t, 1, %s, %s, %s, %s, (int)(%s), (int)(%s), 0);\n",
            vAmt, cmtExpr, vRate, vIi, vAddl, vSs);
    sAppend(&sbDt,  "_rxPushDose(_ind, t, t, 1, %s, %s, %s, %s, (int)(%s), (int)(%s), 0);\n",
            vAmt, cmtExpr, vRate, vIi, vAddl, vSs);

    // Children: 0='infuse', 1='(', 2=amt, 3=',', 4=rate, 5=',',
    //           6=cmt, 7=',', 8=ii, 9=',', 10=addl, 11=','
    //           12=ss, 13=')'
    sAppend(&sbt, "infuse(%s, %s, %s, %s, %s, %s);",
            vAmt, vRate, vCmt, vIi, vAddl, vSs);
    addLine(&sbPm,   "%s\n", sb.s);
    addLine(&sbPmDt, "%s\n", sbDt.s);
    sAppend(&sbNrm,  "%s\n", sbt.s);
    addLine(&sbNrmL, "%s\n", sbt.s);
    ENDLINE;
    return 1;
  }
  return 0;
}

static inline int handleInfuseDurStatement(nodeInfo ni, char *name, int *i, int nch,
                                           D_ParseNode *pn) {
  if (nodeHas(infuseDur_statement) && *i == 0) {
    tb.evid_ = 1;
    *i = nch; // skip all children; we process the whole statement at once
    sb.o = 0; sbDt.o = 0; sbt.o = 0;

    D_ParseNode *cAmt  = d_get_child(pn, 2);
    D_ParseNode *cDur  = d_get_child(pn, 4);
    D_ParseNode *cCmt  = d_get_child(pn, 6);
    D_ParseNode *cIi   = d_get_child(pn, 8);
    D_ParseNode *cAddl = d_get_child(pn, 10);
    D_ParseNode *cSs   = d_get_child(pn, 12);

    char *vAmt  = (char*)rc_dup_str(cAmt->start_loc.s,  cAmt->end);
    char *vDur  = (char*)rc_dup_str(cDur->start_loc.s,  cDur->end);
    char *vCmt  = (char*)rc_dup_str(cCmt->start_loc.s,  cCmt->end);
    char *vIi   = (char*)rc_dup_str(cIi->start_loc.s,   cIi->end);
    char *vAddl = (char*)rc_dup_str(cAddl->start_loc.s, cAddl->end);
    char *vSs   = (char*)rc_dup_str(cSs->start_loc.s,   cSs->end);
    aType(TEVID);

    const char *cmtExpr = rxPushDoseCmtExpr(ni, name, vCmt);
    sAppend(&sb,  "_rxPushDose(_ind, t, t, 1, %s, %s, %s, %s, (int)(%s), (int)(%s), 1);\n",
            vAmt, cmtExpr, vDur, vIi, vAddl, vSs);
    sAppend(&sbDt,  "_rxPushDose(_ind, t, t, 1, %s, %s, %s, %s, (int)(%s), (int)(%s), 1);\n",
            vAmt, cmtExpr, vDur, vIi, vAddl, vSs);
    sAppend(&sbt, "infuseDur(%s, %s, %s, %s, %s, %s);",
            vAmt, vDur, vCmt, vIi, vAddl, vSs);
    addLine(&sbPm,   "%s\n", sb.s);
    addLine(&sbPmDt, "%s\n", sbDt.s);
    sAppend(&sbNrm,  "%s\n", sbt.s);
    addLine(&sbNrmL, "%s\n", sbt.s);
    ENDLINE;
    return 1;
  }
  return 0;
}

static inline int handleEvidStatement(nodeInfo ni, char *name, int *i, int nch,
                                       D_ParseNode *pn) {
  if (nodeHas(evid_statement) && *i == 0) {
    tb.evid_ = 1;
    *i = nch; // skip all children; we process the whole statement at once
    sb.o = 0; sbDt.o = 0; sbt.o = 0;
    // Grammar: 'evid_' '(' e0 ',' e1 ',' e2 ',' e3 ',' e4 ',' e5 ',' e6 ',' e7 ')'
    // Children: 0='evid_', 1='(', 2=time, 3=',', 4=evid, 5=',', 6=amt, 7=',',
    //           8=cmt, 9=',', 10=rate, 11=',', 12=ii, 13=',', 14=addl, 15=',',
    //           16=ss, 17=')'
    D_ParseNode *cTime = d_get_child(pn, 2);
    D_ParseNode *cEvid = d_get_child(pn, 4);
    D_ParseNode *cAmt  = d_get_child(pn, 6);
    D_ParseNode *cCmt  = d_get_child(pn, 8);
    D_ParseNode *cRate = d_get_child(pn, 10);
    D_ParseNode *cIi   = d_get_child(pn, 12);
    D_ParseNode *cAddl = d_get_child(pn, 14);
    D_ParseNode *cSs   = d_get_child(pn, 16);
    char *vTime = (char*)rc_dup_str(cTime->start_loc.s, cTime->end);
    char *vEvid = (char*)rc_dup_str(cEvid->start_loc.s, cEvid->end);
    char *vAmt  = (char*)rc_dup_str(cAmt->start_loc.s,  cAmt->end);
    char *vCmt  = (char*)rc_dup_str(cCmt->start_loc.s,  cCmt->end);
    char *vRate = (char*)rc_dup_str(cRate->start_loc.s,  cRate->end);
    char *vIi   = (char*)rc_dup_str(cIi->start_loc.s,   cIi->end);
    char *vAddl = (char*)rc_dup_str(cAddl->start_loc.s, cAddl->end);
    char *vSs   = (char*)rc_dup_str(cSs->start_loc.s,   cSs->end);
    aType(TEVID);
    const char *cmtExpr = rxPushDoseCmtExpr(ni, name, vCmt);
    sAppend(&sb,  "_rxPushDose(_ind, t, %s, (int)(%s), %s, %s, %s, %s, (int)(%s), (int)(%s), 0);\n",
            vTime, vEvid, vAmt, cmtExpr, vRate, vIi, vAddl, vSs);
    sAppend(&sbDt, "_rxPushDose(_ind, t, %s, (int)(%s), %s, %s, %s, %s, (int)(%s), (int)(%s), 0);\n",
            vTime, vEvid, vAmt, cmtExpr, vRate, vIi, vAddl, vSs);
    sAppend(&sbt, "evid_(%s, %s, %s, %s, %s, %s, %s, %s);",
            vTime, vEvid, vAmt, vCmt, vRate, vIi, vAddl, vSs);
    addLine(&sbPm,   "%s\n", sb.s);
    addLine(&sbPmDt, "%s\n", sbDt.s);
    sAppend(&sbNrm,  "%s\n", sbt.s);
    addLine(&sbNrmL, "%s\n", sbt.s);
    ENDLINE;
    return 1;
  }
  return 0;
}

static inline int handlePrintf(nodeInfo ni, char *name, int i, D_ParseNode *xpn) {
  if (nodeHas(printf_statement)){
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    if (i == 0){
      sb.o =0; sbDt.o =0;
      sbt.o=0;
      tb.thread = notThreadSafe;
      aType(PPRN);
      aAppendN("Rprintf(", 8);
      sAppendN(&sbt,"printf(", 7);
      sb.o--;sbDt.o--;sbt.o--;
    }
    if (i == 2){
      sAppend(&sb,"%s",v);
      sAppend(&sbDt,"%s",v);
      sAppend(&sbt,"%s",v);
    }
    if (i == 4){
      addLine(&sbPm, "%s;\n", sb.s);
      addLine(&sbPmDt, "%s;\n", sbDt.s);
      sAppend(&sbNrm, "%s;\n", sbt.s);
      addLine(&sbNrmL, "%s;\n", sbt.s);
      ENDLINE
        }
    /* Free(v); */
    return 1;
  }
  return 0;
}
