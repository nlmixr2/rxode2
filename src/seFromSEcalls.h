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
   the rewrites keyed on the argument's shape.  Everything else -- log (its
   log(beta(..)) and log1p rewrites), lgamma/loggamma (lgamma1p), sin/cos/tan
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

/* .stripP(): drop one redundant layer of parentheses from an argument */
static D_ParseNode *seStripP(D_ParseNode *pn) {
  for (;;) {
    int nch = d_get_number_of_children(pn);
    if (nch == 1 && seIsWrapper(seNodeName(pn))) {
      pn = d_get_child(pn, 0);
      continue;
    }
    if (nch == 3 && strcmp(seNodeName(d_get_child(pn, 0)), "(") == 0) {
      return d_get_child(pn, 1);
    }
    return pn;
  }
}

/* collect arg_list left spine into args[], returns count or -1 if too many */
static int seArgs(D_ParseNode *pn, D_ParseNode **args, int max) {
  int n = 0;
  D_ParseNode *stack[32];
  int top = 0;
  for (;;) {
    int nch = d_get_number_of_children(pn);
    if (nch == 3 && strcmp(seNodeName(d_get_child(pn, 1)), ",") == 0) {
      if (top >= 32) return -1;
      stack[top++] = d_get_child(pn, 2);
      pn = d_get_child(pn, 0);
      continue;
    }
    break;
  }
  if (n >= max) return -1;
  args[n++] = pn;                 /* leftmost */
  while (top > 0) {
    if (n >= max) return -1;
    args[n++] = stack[--top];
  }
  return n;
}

/* log() takes .rxFromSE()'s .SE1p route, where .rxP1rmF() hunts a literal 1
   down the argument's +/- spine to build log1p().  It only recurses through
   '+' and '-', so on any other argument shape it hands back .rxFromSE(x)
   unchanged and we can emit the same text.  On an additive argument it
   rebuilds the text itself and bypasses the constant fold, so that shape goes
   to R -- as does beta(), which .rxFromSE() rewrites to lbeta().
   Returns NULL when the caller should bail. */
static const char *seEmitLog(seCtx *ctx, D_ParseNode *arg) {
  D_ParseNode *a0 = arg;
  while (d_get_number_of_children(a0) == 1) a0 = d_get_child(a0, 0);
  int nch = d_get_number_of_children(a0);
  if (nch == 3 && strcmp(seNodeName(d_get_child(a0, 0)), "(") != 0) {
    const char *op = seNodeName(d_get_child(a0, 1));
    if (op[0] == '+' || op[0] == '-') return NULL;
  }
  if (nch == 2 && strcmp(seNodeName(a0), "unary_expression") == 0) return NULL;
  if (strcmp(seNodeName(a0), "function_call") == 0 &&
      strcmp(seNodeText(ctx, d_get_child(a0, 0)), "beta") == 0) {
    return NULL;
  }
  /* NB: the log path passes the RAW argument, not the .stripP()ed one */
  const char *inner = seEmit(ctx, arg);
  if (ctx->failed) return "";
  return seNamedConstant(seCat(ctx, "log(", inner, ")", NULL, NULL, NULL));
}

/* the emitted name for a call, or NULL if it must go to the R walker */
static const char *seCallName(const char *name, int nargs) {
  int i;
  if (strcmp(name, "abs0") == 0 && nargs == 1) return "abs";  /* .SEsingle */
  for (i = 0; i < seNfns; i++) {
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
    if (strcmp(seNodeName(d_get_child(pn, i)), "arg_list") == 0) {
      return seArgs(d_get_child(pn, i), args, max);
    }
  }
  return 0;                                   /* zero-argument call */
}

static const char *seFunctionCall(seCtx *ctx, D_ParseNode *pn) {
  const char *name = seNodeText(ctx, d_get_child(pn, 0));
  D_ParseNode *args[8];
  int i, nargs = seCallArgs(pn, args, 8);
  if (nargs < 0) return seFail(ctx);

  if (strcmp(name, "log") == 0 && nargs == 1) {
    const char *lg = seEmitLog(ctx, args[0]);
    return (lg == NULL) ? seFail(ctx) : lg;
  }

  const char *emitName = seCallName(name, nargs);
  if (emitName == NULL) return seFail(ctx);

  const char *body = "";
  for (i = 0; i < nargs; i++) {
    const char *a = seEmit(ctx, seStripP(args[i]));
    if (ctx->failed) return "";
    body = (i == 0) ? a : seCat(ctx, body, ",", a, NULL, NULL, NULL);
  }
  return seNamedConstant(seCat(ctx, emitName, "(", body, ")", NULL, NULL));
}

#endif /* __SE_FROM_SE_CALLS_H__ */
