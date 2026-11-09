/*
 * seFromSEcalls.h -- which symengine function calls the C emitter can render,
 * and how.
 *
 * The table is an ALLOW-list on purpose.  Most names in .rxFromSE() reach its
 * generic call branch untouched, but a good number do not -- log() and
 * lgamma() take the .SE1p route, sin/cos/tan the .SE1m one, and Derivative,
 * Subs, polygamma, the lag/delay family, linCmt, max/min, the tlast/podo
 * family, the llik family, rxTBS and the rxEq/rxAnd spellings each have their
 * own handler.  A deny-list would silently mistranslate the day someone adds
 * a handler in R; an allow-list merely stops covering it.
 *
 * Included by seFromSE.c after the node classifiers and seEmit()'s forward
 * declaration.
 */
#ifndef __SE_FROM_SE_CALLS_H__
#define __SE_FROM_SE_CALLS_H__

/* Functions that reach .rxFromSE()'s GENERIC call branch unchanged: no
   special-case handler, not in .SE1p/.SE1m/.SEsingle/.SEdouble, and not one of
   the rewrites keyed on the argument's shape.  log has its own emitter,
   seEmitLog().  Everything else -- lgamma/loggamma (lgamma1p), sin/cos/tan
   (sinpi/cospi/tanpi), Derivative, Subs, polygamma, the lag/lead/delay family,
   linCmt, max/min, the tlast/podo family, the llik family, rxTBS and the
   rxEq/rxAnd/... operator spellings -- goes to the R walker.

   This is an ALLOW-list on purpose.  A deny-list silently mistranslates the
   day someone adds a handler in R. */
typedef struct { const char *name; int nargs; } seFn;

static const seFn seFns[] = {
  {"exp", 1}, {"sqrt", 1}, {"erf", 1}, {"erfc", 1},
  {"gamma", 1}, {"factorial", 1}, {"lfactorial", 1},
  {"sinh", 1}, {"cosh", 1}, {"tanh", 1},
  {"asin", 1}, {"acos", 1}, {"atan", 1},
  {"asinh", 1}, {"acosh", 1}, {"atanh", 1},
  {"floor", 1}, {"ceiling", 1}, {"trunc", 1}, {"sign", 1},
  {"beta", 2}, {"atan2", 2}, {"choose", 2}, {"lchoose", 2}
};
#define seNfns ((int)(sizeof(seFns)/sizeof(seFns[0])))

/* .SEdouble (R/symengine.R): two-argument spellings that come back as an
   infix operator.  Checked BEFORE the generic arity branch in .rxFromSE(), and
   the arguments are NOT .stripP()ed there. */
typedef struct { const char *name, *open, *mid, *close; } seOp2;

static const seOp2 seOps2[] = {
  {"lbeta",     "lbeta(", ",",   ")"},
  {"rxMod",     "(",      "%%",  ")"},
  {"rxEq",      "(",      "==",  ")"},
  {"rxNeq",     "(",      "!=",  ")"},
  {"rxGeq",     "(",      ">=",  ")"},
  {"rxLeq",     "(",      "<=",  ")"},
  {"rxGt",      "(",      ">",   ")"},
  {"rxLt",      "(",      "<",   ")"},
  {"rxAnd",     "(",      "&&",  ")"},
  {"rxOr",      "(",      "||",  ")"},
  {"R_pow",     "(",      ")^(", ")"},
  {"R_pow_di",  "(",      ")^(", ")"},
  {"Rx_pow",    "(",      ")^(", ")"},
  {"Rx_pow_di", "(",      ")^(", ")"}
};
#define seNops2 ((int)(sizeof(seOps2)/sizeof(seOps2[0])))

/* .rxToSEDualVarFunction: names that are both a variable and a function.
   .rxFunctionMake() turns a zero-argument rxode2 call into f(NaN) on the way
   into symengine, so f(NaN) has to come back as f() -- NOT as a one-argument
   call.  Only these names are promoted; exp(NaN) really is exp(NaN). */
static const char *seDualVarFns[] = {
  "tlast", "tlast0", "tad", "tad0", "tafd", "tafd0", "tfirst", "tfirst0",
  "dose", "podo", "dose0", "podo0", "dosenum", "dosenum0"
};
#define seNdualVarFns ((int)(sizeof(seDualVarFns)/sizeof(seDualVarFns[0])))

/* dosing-history functions: 0 or 1 argument, emitted verbatim */
static const char *seDoseFns[] = {
  "tlast", "tfirst", "dose", "podo", "tlast0", "first0", "dose0", "podo0"
};
#define seNdoseFns ((int)(sizeof(seDoseFns)/sizeof(seDoseFns[0])))

/* .stripP(): drop one redundant layer of parentheses from an argument */
static D_ParseNode *seStripP(D_ParseNode *pn) {
  for (;;) {
    int nch = d_get_number_of_children(pn);
    if (nch == 1 && seNiWrapper(pn)) {
      pn = d_get_child(pn, 0);
      continue;
    }
    if (nch == 3 && seIsLit(d_get_child(pn, 0), '(')) {
      return d_get_child(pn, 1);
    }
    return pn;
  }
}

/* collect arg_list left spine into args[], in source order; see seParseNode.h */
#define seArgs(pn, args, max) seArgsFlattenT(sePt, (pn), (args), (max))

/* the node under any single-child wrappers */
static D_ParseNode *seUnwrap(D_ParseNode *pn) {
  while (d_get_number_of_children(pn) == 1) pn = d_get_child(pn, 0);
  return pn;
}

/* '+' or '-' when pn is a binary sum/difference, else 0 */
static char seAddOp(D_ParseNode *pn) {
  pn = seUnwrap(pn);
  if (d_get_number_of_children(pn) != 3 || seIsLit(d_get_child(pn, 0), '(')) {
    return 0;
  }
  const char *op = seNodeName(d_get_child(pn, 1));
  if ((op[0] == '+' || op[0] == '-') && op[1] == '\0') return op[0];
  return 0;
}

/* .stripP(): every layer of parentheses */
static D_ParseNode *seStripAllP(D_ParseNode *pn) {
  for (;;) {
    D_ParseNode *u = seUnwrap(pn);
    if (d_get_number_of_children(u) == 3 && seIsLit(d_get_child(u, 0), '(')) {
      pn = d_get_child(u, 1);
      continue;
    }
    return pn;
  }
}

/* a bare numeric literal's value (R's inherits(x, "numeric")) */
static int seLitNum(seCtx *ctx, D_ParseNode *pn, double *v) {
  if (!seIsBareNumber(pn)) return 0;
  *v = atof(seNodeText(ctx, seUnwrap(pn)));
  return 1;
}

static int seIsInfNaN(seCtx *ctx, D_ParseNode *pn) {
  while (!seNiIs(pn, symbol) && d_get_number_of_children(pn) == 1) {
    pn = d_get_child(pn, 0);
  }
  if (!seNiIs(pn, symbol)) return 0;
  const char *t = seNodeText(ctx, pn);
  return strcmp(t, "Inf") == 0 || strcmp(t, "NaN") == 0;
}

static const char *seP1rm(seCtx *ctx, D_ParseNode *pn, int *found);

/* `lit + other` (litFirst) or `other + lit`: a literal 1 is dropped, any
   other literal is kept as as.character() renders it */
static const char *seP1rmLit(seCtx *ctx, double v, D_ParseNode *other,
                             int litFirst, int *found) {
  if (v == 1.0) {
    *found = 1;
    return seEmit(ctx, seStripAllP(other));
  }
  const char *o = seP1rm(ctx, other, found);
  if (ctx->failed) return "";
  const char *l = seDblToStr(ctx, v);
  return litFirst ? seCat(ctx, l, "+", o, NULL, NULL, NULL)
                  : seCat(ctx, o, "+", l, NULL, NULL, NULL);
}

/* `a - c`: only 1 - c is rewritten, to -c */
static const char *seP1rmMinus(seCtx *ctx, D_ParseNode *pn, D_ParseNode *a,
                               D_ParseNode *c, int *found) {
  double v;
  if (!seLitNum(ctx, a, &v) || v != 1.0) return seEmit(ctx, pn);
  *found = 1;
  const char *r = seEmit(ctx, c);
  if (ctx->failed) return "";
  return seCat(ctx, "-", r, NULL, NULL, NULL, NULL);
}

/* .rxP1rmF(): emit a +/- spine, dropping one literal 1 (sets *found).
   Numbers kept on the '+' spine use as.character(), not .rxFromSEnum(), and
   the spine is NOT folded -- this is the text .rxFromSE() builds. */
static const char *seP1rm(seCtx *ctx, D_ParseNode *pn, int *found) {
  char op = seAddOp(pn);
  D_ParseNode *b, *a, *c;
  double v;
  if (op == 0) return seEmit(ctx, pn);
  b = seUnwrap(pn);
  a = d_get_child(b, 0);
  c = d_get_child(b, 2);
  /* R parses Inf and NaN as numeric literals; this grammar sees symbols */
  if (seIsInfNaN(ctx, a) || seIsInfNaN(ctx, c)) return seFail(ctx);
  if (op == '-') return seP1rmMinus(ctx, pn, a, c, found);
  if (seLitNum(ctx, c, &v)) return seP1rmLit(ctx, v, a, 0, found);
  if (seLitNum(ctx, a, &v)) return seP1rmLit(ctx, v, c, 1, found);
  const char *l = seP1rm(ctx, a, found);
  if (ctx->failed) return "";
  const char *r = seP1rm(ctx, c, found);
  if (ctx->failed) return "";
  return seCat(ctx, l, "+", r, NULL, NULL, NULL);
}

/* text that may be one exp(...) call; R re-parses it to find out.  A quote
   means a string literal whose parentheses this scan cannot count, so yes. */
static int seIsExpCall(const char *s) {
  int depth = 0;
  const char *p;
  if (strncmp(s, "exp(", 4) != 0) return 0;
  if (strchr(s, '"') != NULL || strchr(s, '\'') != NULL) return 1;
  for (p = s + 3; *p; p++) {
    if (*p == '(') depth++;
    else if (*p == ')' && --depth == 0) return p[1] == '\0';
  }
  return 1;
}

/* log() takes .rxFromSE()'s .SE1p route: .rxP1rmF() turns log(1 + x) into
   log1p(x).  log1p(exp(x)) becomes log1pexp() by re-translating the emitted
   text, and log(beta(..)) becomes lbeta(); both go to R.
   NB: the log path passes the RAW argument, not the .stripP()ed one.
   Returns NULL when the caller should bail. */
static const char *seEmitLog(seCtx *ctx, D_ParseNode *arg) {
  D_ParseNode *a0 = seUnwrap(arg);
  int found = 0;
  if (seNiIs(a0, function_call) &&
      strcmp(seNodeText(ctx, d_get_child(a0, 0)), "beta") == 0) {
    return NULL;
  }
  const char *inner = seP1rm(ctx, arg, &found);
  if (ctx->failed) return "";
  if (found) {
    if (seIsExpCall(inner)) return NULL;
    return seCat(ctx, "log1p(", inner, ")", NULL, NULL, NULL);
  }
  return seNamedConstant(seCat(ctx, "log(", inner, ")", NULL, NULL, NULL));
}

/* the emitted name for a call, or NULL if it must go to the R walker */
static const char *seCallName(const char *name, int nargs) {
  int i;
  if (strcmp(name, "abs0") == 0 && nargs == 1) return "abs";  /* .SEsingle */
  for (i = 0; i < seNfns; i++) {
    if (name[0] != seFns[i].name[0]) continue;   /* reject before strcmp */
    if (strcmp(name, seFns[i].name) == 0) {
      /* R raises "'%s' takes %s arguments" here; let it produce the message */
      return (seFns[i].nargs == nargs) ? seFns[i].name : NULL;
    }
  }
  return NULL;
}

/* the call's arguments in source order, or -1 if there are too many */
static int seCallArgs(D_ParseNode *pn, D_ParseNode **args, int max) {
  int nch = d_get_number_of_children(pn), i;
  for (i = 0; i < nch; i++) {
    if (seNiArgList(d_get_child(pn, i))) {
      return seArgs(d_get_child(pn, i), args, max);
    }
  }
  return 0;                                   /* zero-argument call */
}

/* Zero-derivative functions (.rxSEzeroD): the delay family and the locally
   constant rounding family differentiate to 0 at every order. */
static const char *seZeroD[] = {
  "lead", "lag", "delay", "rxDelayD", "rxDelayD2", "rxDelayD3",
  "floor", "ceil", "ceiling", "round", "trunc", "ftrunc", "fround", "fprec",
  "sign"
};
#define seNzeroD ((int)(sizeof(seZeroD)/sizeof(seZeroD[0])))

/* The meaningful node under a call argument.  seArgs() hands back the arg_list
   node for a single argument, and the grammar's precedence ladder sits on top
   of every value, so descend single-child nodes until one carries meaning. */
static D_ParseNode *seArgNode(D_ParseNode *pn) {
  for (;;) {
    if (seNiIs(pn, symbol) || seNiIs(pn, number) || seNiIs(pn, function_call)) {
      return pn;
    }
    if (d_get_number_of_children(pn) != 1) return pn;
    pn = d_get_child(pn, 0);
  }
}

/* Render a derivative template, substituting @@k@@ with args[k-1]. */
static const char *seFillTemplate(seCtx *ctx, const char *tmpl,
                                  const char **args, int nargs) {
  const char *out = "";
  const char *p = tmpl, *seg = tmpl;
  for (;;) {
    if (*p == '\0') {
      if (p != seg) out = seCat(ctx, out, seDup(ctx, seg, (size_t)(p - seg)),
                                NULL, NULL, NULL, NULL);
      return out;
    }
    if (p[0] == '@' && p[1] == '@') {
      const char *d = p + 2;
      int k = 0;
      while (*d >= '0' && *d <= '9') { k = k * 10 + (*d - '0'); d++; }
      if (d != p + 2 && d[0] == '@' && d[1] == '@' && k >= 1 && k <= nargs) {
        if (p != seg) out = seCat(ctx, out, seDup(ctx, seg, (size_t)(p - seg)),
                                  NULL, NULL, NULL, NULL);
        out = seCat(ctx, out, args[k - 1], NULL, NULL, NULL, NULL);
        p = d + 2;
        seg = p;
        continue;
      }
    }
    p++;
  }
}

static const char *seFindDeriv(seCtx *ctx, const char *name, int which) {
  int i;
  for (i = 0; i < ctx->nderivs; i++) {
    if (ctx->derivs[i].which != which) continue;
    if (name[0] != ctx->derivs[i].name[0]) continue;
    if (strcmp(name, ctx->derivs[i].name) == 0) return ctx->derivs[i].tmpl;
  }
  return NULL;
}

/* which(.var == .args) must select exactly one argument; -1 otherwise */
static int seDerivWhich(const char **emitted, int nf, const char *var) {
  int i, with = -1;
  for (i = 0; i < nf; i++) {
    if (strcmp(emitted[i], var) != 0) continue;
    if (with >= 0) return -1;                  /* ambiguous; R uses .errD() */
    with = i;
  }
  return with;
}

static int seIsZeroD(const char *fname) {
  int i;
  for (i = 0; i < seNzeroD; i++) {
    if (fname[0] == seZeroD[i][0] && strcmp(fname, seZeroD[i]) == 0) return 1;
  }
  return 0;
}

/* Derivative(f(a1, ..., an), v) -- the first-order form.  Higher orders, the
   finite-difference fallbacks (.errD) and anything without a registered
   template go to the R walker. */
static const char *seDerivative(seCtx *ctx, D_ParseNode **args, int nargs) {
  int i, nf, with;
  D_ParseNode *fnNode, *fargs[8];
  const char *var, *fname, *tmpl, *emitted[8];
  if (nargs != 2) return seFail(ctx);
  fnNode = seArgNode(args[0]);
  var = seEmit(ctx, seArgNode(args[1]));
  if (ctx->failed) return "";

  if (!seNiIs(fnNode, function_call)) {
    /* Derivative(abs0, v) reaches .rxFromSE() with a bare name */
    if (seNiIs(fnNode, symbol) && strcmp(seNodeText(ctx, fnNode), "abs0") == 0) {
      return seCat(ctx, "abs(", var, ")", NULL, NULL, NULL);
    }
    return seFail(ctx);
  }

  fname = seNodeText(ctx, d_get_child(fnNode, 0));
  if (seIsZeroD(fname)) return "0";
  /* NB: no abs0 shortcut here.  .rxFromSE() guards it with
     `length(as.character(x[[2]])) == 1`, which is only true when the
     differentiated thing is the bare NAME abs0; for the call abs0(a) that
     vector is c("abs0","a") and the registered derivative wins, giving
     dabs(a) rather than abs(a). */

  nf = seCallArgs(fnNode, fargs, 8);
  if (nf <= 0) return seFail(ctx);
  for (i = 0; i < nf; i++) {
    emitted[i] = seEmit(ctx, fargs[i]);
    if (ctx->failed) return "";
  }
  with = seDerivWhich(emitted, nf, var);
  if (with < 0) return seFail(ctx);
  tmpl = seFindDeriv(ctx, fname, with + 1);
  if (tmpl == NULL) return seFail(ctx);        /* unregistered, or linCmtB */
  return seFillTemplate(ctx, tmpl, emitted, nf);
}

/* f(NaN) is how a zero-argument call survives the trip through symengine.
   Compared on the argument's source span rather than its node kind: for a
   single argument seArgs() hands back the arg_list node itself, whose span is
   exactly the argument text (a longer expression that merely contains NaN
   spans more, so this cannot false-positive).  NULL = not one of these. */
static const char *seNaNZeroArg(seCtx *ctx, const char *name,
                                D_ParseNode **args, int nargs) {
  int i;
  if (nargs != 1 || strcmp(seNodeText(ctx, args[0]), "NaN") != 0) return NULL;
  for (i = 0; i < seNdualVarFns; i++) {
    if (name[0] != seDualVarFns[i][0]) continue;
    if (strcmp(name, seDualVarFns[i]) == 0) {
      return seCat(ctx, name, "()", NULL, NULL, NULL, NULL);
    }
  }
  return NULL;
}

/* .SEdouble, checked before the generic branch as .rxFromSE() does */
static const char *seOps2Call(seCtx *ctx, const char *name,
                              D_ParseNode **args, int nargs) {
  int i;
  for (i = 0; i < seNops2; i++) {
    const char *a, *b;
    if (name[0] != seOps2[i].name[0]) continue;
    if (strcmp(name, seOps2[i].name) != 0) continue;
    if (nargs != 2) return seFail(ctx);       /* R raises its own message */
    a = seEmit(ctx, args[0]);
    if (ctx->failed) return "";
    b = seEmit(ctx, args[1]);
    if (ctx->failed) return "";
    return seNamedConstant(seCat(ctx, seOps2[i].open, a, seOps2[i].mid, b,
                                 seOps2[i].close, NULL));
  }
  return NULL;
}

/* polygamma(a, b): the order flips and small orders have their own names */
static const char *sePolygamma(seCtx *ctx, D_ParseNode **args, int nargs) {
  const char *a, *b;
  if (nargs != 2) return seFail(ctx);
  a = seEmit(ctx, args[0]);
  if (ctx->failed) return "";
  b = seEmit(ctx, args[1]);
  if (ctx->failed) return "";
  if (!strcmp(a, "0")) return seCat(ctx, "digamma(", b, ")", NULL, NULL, NULL);
  if (!strcmp(a, "1")) return seCat(ctx, "trigamma(", b, ")", NULL, NULL, NULL);
  if (!strcmp(a, "2")) return seCat(ctx, "tetragamma(", b, ")", NULL, NULL, NULL);
  if (!strcmp(a, "3")) return seCat(ctx, "pentagamma(", b, ")", NULL, NULL, NULL);
  return seCat(ctx, "psigamma(", b, ",", a, ")", NULL);
}

/* tlast()/podo()/... take 0 or 1 argument and keep their name */
static const char *seDoseFn(seCtx *ctx, const char *name,
                            D_ParseNode **args, int nargs) {
  int i;
  for (i = 0; i < seNdoseFns; i++) {
    const char *a;
    if (name[0] != seDoseFns[i][0]) continue;
    if (strcmp(name, seDoseFns[i]) != 0) continue;
    if (nargs == 0) return seCat(ctx, name, "()", NULL, NULL, NULL, NULL);
    if (nargs != 1) return seFail(ctx);
    a = seEmit(ctx, seStripP(args[0]));
    if (ctx->failed) return "";
    return seCat(ctx, name, "(", a, ")", NULL, NULL);
  }
  return NULL;
}

static const char *seGenericCall(seCtx *ctx, const char *name,
                                 D_ParseNode **args, int nargs) {
  int i;
  const char *body = "";
  const char *emitName = seCallName(name, nargs);
  if (emitName == NULL) return seFail(ctx);
  for (i = 0; i < nargs; i++) {
    const char *a = seEmit(ctx, seStripP(args[i]));
    if (ctx->failed) return "";
    body = (i == 0) ? a : seCat(ctx, body, ",", a, NULL, NULL, NULL);
  }
  return seNamedConstant(seCat(ctx, emitName, "(", body, ")", NULL, NULL));
}

static const char *seFunctionCall(seCtx *ctx, D_ParseNode *pn) {
  const char *name = seNodeText(ctx, d_get_child(pn, 0));
  const char *got;
  D_ParseNode *args[8];
  int nargs = seCallArgs(pn, args, 8);
  if (nargs < 0) return seFail(ctx);

  if (strcmp(name, "Derivative") == 0) return seDerivative(ctx, args, nargs);
  got = seNaNZeroArg(ctx, name, args, nargs);
  if (got != NULL) return got;
  got = seOps2Call(ctx, name, args, nargs);
  if (got != NULL) return got;
  if (strcmp(name, "polygamma") == 0) return sePolygamma(ctx, args, nargs);
  got = seDoseFn(ctx, name, args, nargs);
  if (got != NULL) return got;
  if (strcmp(name, "log") == 0 && nargs == 1) {
    const char *lg = seEmitLog(ctx, args[0]);
    return (lg == NULL) ? seFail(ctx) : lg;
  }
  return seGenericCall(ctx, name, args, nargs);
}

#endif /* __SE_FROM_SE_CALLS_H__ */
