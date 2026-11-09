/*
 * rxToSEemit.h -- walk an rxode2 expression and emit the text symengine parses.
 *
 * The output must match rxToSE() BYTE FOR BYTE.  Anything this cannot
 * reproduce sets ctx->failed and the R shim falls back to the R walker;
 * falling back is always safe, guessing is not.
 */
#ifndef __RX_TO_SE_EMIT_H__
#define __RX_TO_SE_EMIT_H__

#include "rxToSEnode.h"
#include "seNum.h"

/* .rxSEreserved: these names mean something to symengine, so rxToSE() shadows
   them.  .rxFromSE() maps rx_SymPy_Res_<name> back. */
static const char *rtReserved[] = {
  "e", "E", "EulerGamma", "Catalan", "GoldenRatio", "I"
};
#define rtNreserved ((int)(sizeof(rtReserved)/sizeof(rtReserved[0])))

/* .rxSEcnt: rxode2 writes C's constants, symengine wants the expression they
   stand for.  Missing this made every constant pass through verbatim --
   rxToSE("M_LN2") gave "M_LN2" instead of "log(2)" -- which the fixture did
   not catch because its corpus contains log(2) as an INPUT, never M_LN2. */
typedef struct { const char *name, *expr; } rtCnt;

static const rtCnt rtCnts[] = {
  {"M_E",            "E"},
  {"M_PI",           "pi"},
  {"M_PI_2",         "pi/2"},
  {"M_PI_4",         "pi/4"},
  {"M_1_PI",         "1/pi"},
  {"M_2_PI",         "2/pi"},
  {"M_2PI",          "2*pi"},
  {"M_SQRT_PI",      "sqrt(pi)"},
  {"M_2_SQRTPI",     "2/sqrt(pi)"},
  {"M_1_SQRT_2PI",   "1/sqrt(2*pi)"},
  {"M_SQRT2",        "sqrt(2)"},
  {"M_SQRT_3",       "sqrt(3)"},
  {"M_SQRT_32",      "sqrt(32)"},
  {"M_SQRT_2dPI",    "sqrt(2/pi)"},
  {"M_LN_SQRT_PI",   "log(sqrt(pi))"},
  {"M_LN_SQRT_2PI",  "log(sqrt(2*pi))"},
  {"M_LN_SQRT_PId2", "log(sqrt(pi/2))"},
  {"M_LOG10_2",      "log(2)/log(10)"},
  {"M_LOG2E",        "1/log(2)"},
  {"M_LOG10E",       "1/log(10)"},
  {"M_LN2",          "log(2)"},
  {"M_LN10",         "log(10)"}
};
#define rtNcnts ((int)(sizeof(rtCnts)/sizeof(rtCnts[0])))

static const char *rtFail(seCtx *ctx) {
  return seFailWhy(ctx, SE_WHY_OTHER);
}

static const char *rtFailWhy(seCtx *ctx, int why) {
  return seFailWhy(ctx, why);
}

/* ..curCall while the arguments of a call are converted */
static int rtPush(seCtx *ctx, const char *call) {
  if (!ctx->collect || call == NULL) return 1;
  if (ctx->ncall >= 64) { rtFail(ctx); return 0; }
  ctx->callStack[ctx->ncall++] = call;
  return 1;
}

static void rtPop(seCtx *ctx, const char *call) {
  if (ctx->collect && call != NULL && ctx->ncall > 0) ctx->ncall--;
}

/* paste(..curCall[..curCall != ""], collapse = "."), "" when empty */
static const char *rtCallPath(seCtx *ctx) {
  const char *out = "";
  int i, first = 1;
  for (i = 0; i < ctx->ncall; i++) {
    if (ctx->callStack[i][0] == '\0') continue;
    out = first ? ctx->callStack[i] : seCat(ctx, out, ".", ctx->callStack[i], NULL, NULL, NULL);
    first = 0;
  }
  return out;
}

/* .rxToSEDualVarFunction: names that are both a variable and a function.  Used
   bare they mean the call, and the tad/tafd family expands to a difference
   from the dosing time.  Without this the emitter passed them through as
   plain symbols, which test-dsl.R caught and the fixture did not. */
typedef struct { const char *name, *expr; } rtDual;

static const rtDual rtDuals[] = {
  {"tlast",   "tlast()"},
  {"tlast0",  "tlast0()"},
  {"tfirst",  "tfirst()"},
  {"tfirst0", "tfirst0()"},
  {"dose",    "dose()"},
  {"dose0",   "dose0()"},
  {"podo",    "podo()"},
  {"podo0",   "podo0()"},
  {"dosenum", "dosenum()"},
  {"tad",     "(t-tlast())"},
  {"tad0",    "(t-tlast0())"},
  {"tafd",    "(t-tfirst())"},
  {"tafd0",   "(t-tfirst0())"}
  /* dosenum0 is deliberately absent: rxToSE() raises "function 'dosenum0' or
     its derivatives are not supported", so it goes to the R walker to get the
     message right */
};
#define rtNduals ((int)(sizeof(rtDuals)/sizeof(rtDuals[0])))

/* .rxSEdouble: two-argument rewrites, keyed on the rxode2 operator/function */
typedef struct { const char *name, *open, *mid, *close; } rtOp2;

static const rtOp2 rtOps2[] = {
  {"==",     "rxEq(",  ",",   ")"},
  {"!=",     "rxNeq(", ",",   ")"},
  {">=",     "rxGeq(", ",",   ")"},
  {"<=",     "rxLeq(", ",",   ")"},
  {"<",      "rxLt(",  ",",   ")"},
  {">",      "rxGt(",  ",",   ")"},
  {"&&",     "rxAnd(", ",",   ")"},
  {"&",      "rxAnd(", ",",   ")"},
  {"||",     "rxOr(",  ",",   ")"},
  {"|",      "rxOr(",  ",",   ")"},
  {"%%",     "rxMod(", ",",   ")"},
  {"Rx_pow", "(",      ")^(", ")"},
  {"lbeta",  "log(beta(", ",", "))"}
};
#define rtNops2 ((int)(sizeof(rtOps2)/sizeof(rtOps2[0])))

/* .rxSEsingle: one-argument rewrites */
/* `call` is what .rxToSECall() pushes on ..curCall while it converts the
   argument (.rxSEsingle's third element; NULL for the dedicated handlers of
   pnorm/qnorm/fabs, which push nothing) */
typedef struct { const char *name, *open, *close, *call; } rtOp1;

static const rtOp1 rtOps1[] = {
  {"digamma",    "polygamma(0,",        ")", "psigamma"},
  {"trigamma",   "polygamma(1,",        ")", "psigamma"},
  {"tetragamma", "polygamma(2,",        ")", "psigamma"},
  {"pentagamma", "polygamma(3,",        ")", "psigamma"},
  {"cospi",      "cos(pi*(",            "))", "cos"},
  {"sinpi",      "sin(pi*(",            "))", "sin"},
  {"tanpi",      "tan(pi*(",            "))", "tan"},
  {"log1p",      "log(1+",              ")", "log"},
  {"expm1",      "(exp(",               ")-1)", "exp"},
  {"factorial",  "gamma(",              "+1)", "gamma"},
  {"lfactorial", "lgamma(",             "+1)", "lgamma"},
  {"lgamma1p",   "lgamma(",             "+1)", "lgamma"},
  {"log10",      "log(",                ")/log(10)", "log"},
  {"log2",       "log(",                ")/log(2)", "log"},
  {"log1pexp",   "log(1+exp(",          "))", "log1pexp"},
  {"phi",        "0.5*(1+erf((",        ")/sqrt(2)))", NULL},
  {"pnorm",      "0.5*(1+erf((",        ")/sqrt(2)))", NULL},
  {"normcdf",    "0.5*(1+erf((",        ")/sqrt(2)))", NULL},
  {"qnorm",      "sqrt(2)*erfinv(2*(",  ")-1)", NULL},
  {"fabs",       "abs0(",               ")", NULL}
};
#define rtNops1 ((int)(sizeof(rtOps1)/sizeof(rtOps1[0])))

/* Functions that pass through unchanged.  An ALLOW-list, for the same reason
   as seFromSEcalls.h: a deny-list silently mistranslates the day someone adds
   a handler in R.  Anything not here -- linCmt, the lag/delay family, the llik
   family, transit, user functions -- goes to the R walker. */
static const char *rtPassFns[] = {
  "exp", "log", "sqrt", "erf", "erfc", "gamma", "lgamma",
  "sin", "cos", "tan", "asin", "acos", "atan", "atan2",
  "sinh", "cosh", "tanh", "asinh", "acosh", "atanh",
  "floor", "ceil", "ceiling", "round", "trunc", "ftrunc", "fround", "fprec",
  "sign", "beta"
};
/* Deliberately NOT here, each checked against rxToSE() one at a time rather
   than assumed: abs -> abs0, and choose and lchoose expand into gamma/lgamma
   forms.  Both go to the R walker. */
#define rtNpassFns ((int)(sizeof(rtPassFns)/sizeof(rtPassFns[0])))

/* collect an arg_list's left spine in source order; see seParseNode.h */
#define rtArgs(pn, args, max) seArgsFlattenT(rtPt, (pn), (args), (max))

/* THETA[1] -> THETA_1_ */
static const char *rtIndex(seCtx *ctx, D_ParseNode *pn) {
  const char *nm = rtNodeText(ctx, d_get_child(pn, 0));
  const char *ix = rtNodeText(ctx, d_get_child(pn, 2));
  const char *p;
  int isTheta = strcmp(nm, "THETA") == 0;
  if (!isTheta && strcmp(nm, "ETA") != 0) return rtFail(ctx);
  /* R writes the number back with as.character(): THETA[01] is THETA_1_,
     which this does not reproduce, so only plain integers */
  if (*ix < '1' || *ix > '9') return rtFail(ctx);
  for (p = ix; *p != '\0'; p++) {
    if (*p < '0' || *p > '9') return rtFail(ctx);
  }
  if (strlen(ix) > 9) return rtFail(ctx);
  if (ctx->collect) {
    /* .rxToSESquareBracket() records the largest THETA/ETA */
    const char *path = rtCallPath(ctx);
    if (isTheta) {
      if (!seGrow(ctx, (void**) &ctx->thetas, &ctx->nthetas, &ctx->mthetas, sizeof(int)) ||
          !seGrow(ctx, (void**) &ctx->thetaFuns, &ctx->nthetaFuns, &ctx->mthetaFuns, sizeof(char*))) {
        return rtFail(ctx);
      }
      ctx->thetas[ctx->nthetas++] = atoi(ix);
      ctx->thetaFuns[ctx->nthetaFuns++] = path;
    } else {
      if (!seGrow(ctx, (void**) &ctx->etas, &ctx->netas, &ctx->metas, sizeof(int)) ||
          !seGrow(ctx, (void**) &ctx->etaFuns, &ctx->netaFuns, &ctx->metaFuns, sizeof(char*))) {
        return rtFail(ctx);
      }
      ctx->etas[ctx->netas++] = atoi(ix);
      ctx->etaFuns[ctx->netaFuns++] = path;
    }
  }
  return seCat(ctx, nm, "_", ix, "_", NULL, NULL);
}

static const char *rtSymbol(seCtx *ctx, D_ParseNode *pn) {
  const char *raw = rtNodeText(ctx, pn);
  int i;
  if (ctx->collect) {
    if (!seGrow(ctx, (void**) &ctx->syms, &ctx->nsyms, &ctx->msyms, sizeof(char*))) {
      return rtFail(ctx);
    }
    ctx->syms[ctx->nsyms++] = raw;
  }
  /* NA and dosenum0 raise in rxToSE(); let the R walker produce the message */
  if (strcmp(raw, "NA") == 0 || strcmp(raw, "dosenum0") == 0) return rtFail(ctx);
  /* a bare dual variable/function name means the call */
  for (i = 0; i < rtNduals; i++) {
    if (raw[0] != rtDuals[i].name[0]) continue;
    if (strcmp(raw, rtDuals[i].name) == 0) return rtDuals[i].expr;
  }
  /* a C constant stands for an expression symengine understands */
  if (raw[0] == 'M' && raw[1] == '_') {
    for (i = 0; i < rtNcnts; i++) {
      if (strcmp(raw, rtCnts[i].name) == 0) return rtCnts[i].expr;
    }
  }
  for (i = 0; i < rtNreserved; i++) {
    if (raw[0] == rtReserved[i][0] && strcmp(raw, rtReserved[i]) == 0) {
      return seCat(ctx, "rx_SymPy_Res_", raw, NULL, NULL, NULL, NULL);
    }
  }
  return raw;
}

/* the one-argument rewrites; NULL when `name` is not one */
static const char *rtCall1(seCtx *ctx, const char *name, D_ParseNode **args) {
  int i;
  for (i = 0; i < rtNops1; i++) {
    const char *a;
    if (name[0] != rtOps1[i].name[0]) continue;
    if (strcmp(name, rtOps1[i].name) != 0) continue;
    if (!rtPush(ctx, rtOps1[i].call)) return "";
    a = rtEmit(ctx, args[0]);
    rtPop(ctx, rtOps1[i].call);
    if (ctx->failed) return "";
    return seCat(ctx, rtOps1[i].open, a, rtOps1[i].close, NULL, NULL, NULL);
  }
  return NULL;
}

/* ... and the two-argument ones */
static const char *rtCall2(seCtx *ctx, const char *name, D_ParseNode **args) {
  int i;
  for (i = 0; i < rtNops2; i++) {
    const char *a, *b;
    if (name[0] != rtOps2[i].name[0]) continue;
    if (strcmp(name, rtOps2[i].name) != 0) continue;
    a = rtEmit(ctx, args[0]);
    if (ctx->failed) return "";
    b = rtEmit(ctx, args[1]);
    if (ctx->failed) return "";
    return seCat(ctx, rtOps2[i].open, a, rtOps2[i].mid, b, rtOps2[i].close, NULL);
  }
  return NULL;
}

/* a function symengine knows by the same name */
static const char *rtCallPass(seCtx *ctx, const char *name,
                              D_ParseNode **args, int nargs) {
  int i, j;
  for (i = 0; i < rtNpassFns; i++) {
    const char *body = "";
    if (name[0] != rtPassFns[i][0]) continue;
    if (strcmp(name, rtPassFns[i]) != 0) continue;
    /* exp(1) is Euler's number, which symengine spells E; log(1) and sqrt(1)
       get no such treatment, so this is the one case, not a general fold */
    if (nargs == 1 && strcmp(name, "exp") == 0 &&
        strcmp(rtNodeText(ctx, args[0]), "1") == 0) {
      return "E";
    }
    /* the generic branch of .rxToSECall() pushes the function's name */
    if (!rtPush(ctx, rtPassFns[i])) return "";
    for (j = 0; j < nargs; j++) {
      const char *a = rtEmit(ctx, args[j]);
      if (ctx->failed) { rtPop(ctx, rtPassFns[i]); return ""; }
      body = (j == 0) ? a : seCat(ctx, body, ",", a, NULL, NULL, NULL);
    }
    rtPop(ctx, rtPassFns[i]);
    return seCat(ctx, name, "(", body, ")", NULL, NULL);
  }
  return NULL;
}

static const char *rtCall(seCtx *ctx, D_ParseNode *pn) {
  const char *name = rtNodeText(ctx, d_get_child(pn, 0));
  const char *got;
  D_ParseNode *args[8];
  D_ParseNode *argNode = NULL;
  int nch = d_get_number_of_children(pn), nargs = 0, i;
  for (i = 0; i < nch; i++) {
    if (rtNiIs(d_get_child(pn, i), arg_list)) { argNode = d_get_child(pn, i); break; }
  }
  if (argNode != NULL) {
    nargs = rtArgs(argNode, args, 8);
    if (nargs < 0) return rtFail(ctx);
  }
  if (nargs == 1) {
    got = rtCall1(ctx, name, args);
    if (got != NULL) return got;
  }
  if (nargs == 2) {
    got = rtCall2(ctx, name, args);
    if (got != NULL) return got;
  }
  got = rtCallPass(ctx, name, args, nargs);
  if (got != NULL) return got;
  /* user function, linCmt, llik, ...: the R walker's answer can depend on R
     functions in the caller's environment, so it is not cached */
  return rtFailWhy(ctx, SE_WHY_UNKNOWN_FN);
}

/* a binary operator that rxToSE() rewrites into a call */
static const char *rtBinaryOp(seCtx *ctx, const char *op, const char *a,
                              const char *b) {
  int i;
  for (i = 0; i < rtNops2; i++) {
    if (op[0] != rtOps2[i].name[0]) continue;
    if (strcmp(op, rtOps2[i].name) != 0) continue;
    return seCat(ctx, rtOps2[i].open, a, rtOps2[i].mid, b, rtOps2[i].close, NULL);
  }
  return NULL;
}

static const char *rtEmitUnary(seCtx *ctx, D_ParseNode *pn) {
  D_ParseNode *opNode = d_get_child(pn, 0);
  const char *inner = rtEmit(ctx, d_get_child(pn, 1));
  if (ctx->failed) return "";
  if (rtIsLit(opNode, '!')) return seCat(ctx, "rxNot(", inner, ")", NULL, NULL, NULL);
  /* NB: rxToSE() puts a space after a unary sign -- "-a" becomes "- a" */
  return seCat(ctx, rtNodeName(opNode), " ", inner, NULL, NULL, NULL);
}

static const char *rtEmitBinary(seCtx *ctx, D_ParseNode *pn) {
  D_ParseNode *opNode = d_get_child(pn, 1);
  const char *op, *rw;
  const char *a = rtEmit(ctx, d_get_child(pn, 0));
  const char *b;
  if (ctx->failed) return "";
  b = rtEmit(ctx, d_get_child(pn, 2));
  if (ctx->failed) return "";
  /* rel_op wraps the operator token one level down */
  op = rtNiIs(opNode, rel_op) ? rtNodeName(d_get_child(opNode, 0))
    : rtNodeName(opNode);
  rw = rtBinaryOp(ctx, op, a, b);
  if (rw != NULL) return rw;
  /* symengine reads '^'; rxode2 writes either */
  if (op[0] == '*' && op[1] == '*') op = "^";
  return seCat(ctx, a, op, b, NULL, NULL, NULL);
}

static const char *rtEmit(seCtx *ctx, D_ParseNode *pn) {
  if (ctx->failed) return "";
  int nch = d_get_number_of_children(pn);

  if (rtNodeHas(symbol)) return rtSymbol(ctx, pn);
  if (rtNodeHas(number) || rtNodeHas(integer_num) || rtNodeHas(float_num)) {
    /* R parses the number and writes it back with as.character(): 1.50 is
       1.5 and 1e5 is 1e+05 */
    return seDblToStr(ctx, R_strtod(rtNodeText(ctx, pn), NULL));
  }
  if (rtNodeHas(index_expression)) return rtIndex(ctx, pn);
  if (rtNodeHas(function_call)) return rtCall(ctx, pn);
  if (nch == 1) return rtEmit(ctx, d_get_child(pn, 0));
  if (nch == 2 && rtNodeHas(unary_expression)) return rtEmitUnary(ctx, pn);
  if (nch == 3) {
    if (rtIsLit(d_get_child(pn, 0), '(')) {
      return seCat(ctx, "(", rtEmit(ctx, d_get_child(pn, 1)), ")",
                   NULL, NULL, NULL);
    }
    return rtEmitBinary(ctx, pn);
  }
  return rtFail(ctx);
}

#endif /* __RX_TO_SE_EMIT_H__ */
