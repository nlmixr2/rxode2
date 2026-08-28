/*
 * seFromSE.c -- translate SymEngine printer output into rxode2/C syntax.
 *
 * This is the C replacement for the recursive R walker .rxFromSE()
 * (R/symengine.R).  rxFromSE() is ~90% of all symbolic-derivative time --
 * symengine::D() itself is ~0.5% -- because it re-parses each symengine string
 * with R's parse() and then emits with nested paste0() and a 9-deep sub()
 * regex chain, plus an options() save/restore per leaf.
 *
 * Correctness contract: the output must match the R implementation BYTE FOR
 * BYTE.  Anything this file is not certain it reproduces sets ctx->failed and
 * the R shim falls back to .rxFromSE().  Falling back is always safe; guessing
 * is not.  tests/testthat/test-symengine-translate-fixture.R pins the contract.
 *
 * No R API and no symengine call happens inside seFromSE1(); it is pure
 * string -> string over a private arena.  That is deliberate: the batch entry
 * point can later run the per-expression loop under OpenMP (symengine itself
 * is built without thread-safe refcounting, so only this half can be threaded).
 */
#define STRICT_R_HEADERS
#define USE_FC_LEN_T
#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

/* dparser function-pointer table.  tran.c has the dparserPtrIni definitions;
   here we only want the extern declarations. */
#include <dparserPtr.h>
#include "seFromSE.g.d_parser.h"

#include "seFromSEarena.h"
#include "seFromSEnames.h"

/* ---------------------------------------------------------------- walker -- */

static D_ParserTables *sePt = &parser_tables_rxode2seFromSE;

static const char *seNodeName(D_ParseNode *pn) {
  return (const char*) sePt->symbols[pn->symbol].name;
}

static const char *seNodeText(seCtx *ctx, D_ParseNode *pn) {
  const char *b = pn->start_loc.s, *e = pn->end;
  while (b < e && (*b == ' ' || *b == '\t' || *b == '\n')) b++;
  while (e > b && (e[-1] == ' ' || e[-1] == '\t' || e[-1] == '\n')) e--;
  return seDup(ctx, b, (size_t)(e - b));
}

static const char *seEmit(seCtx *ctx, D_ParseNode *pn);

/* Productions that only wrap a single child and carry no meaning of their own.
   The grammar spells the precedence ladder out (add -> mul -> unary -> power
   -> primary), so every walk has to be able to see through it; naming that
   once keeps seIsBareNumber(), seStripP() and seFold() from each repeating
   the ladder. */
static int seIsWrapper(const char *nm) {
  return strcmp(nm, "expression") == 0 ||
    strcmp(nm, "add_expression") == 0 ||
    strcmp(nm, "mul_expression") == 0 ||
    strcmp(nm, "unary_expression") == 0 ||
    strcmp(nm, "power_expression") == 0 ||
    strcmp(nm, "primary_expression") == 0;
}

/* a numeric literal node ("number" wraps "integer"/"float") */
static int seIsNumberNode(const char *nm) {
  return strcmp(nm, "number") == 0 || strcmp(nm, "integer") == 0 ||
    strcmp(nm, "float") == 0;
}

/* a node whose value stands on its own, with no children to combine */
static int seIsLeafNode(const char *nm) {
  return seIsNumberNode(nm) || strcmp(nm, "symbol") == 0 ||
    strcmp(nm, "identifier") == 0 || strcmp(nm, "function_call") == 0;
}

/* Does this exponent subtree reduce to a bare numeric literal?  This is the
   is.numeric(x[[3]]) test in .rxFromSE(): TRUE only for a literal, so `d^2`
   becomes Rx_pow_di(d,2) while `a^(-2)` (a call to unary minus) does not and
   falls through to "a^-2".  Reproducing that asymmetry matters -- the fixture
   pins it. */
static int seIsBareNumber(D_ParseNode *pn) {
  for (;;) {
    const char *nm = seNodeName(pn);
    if (seIsNumberNode(nm)) return 1;
    if (d_get_number_of_children(pn) == 1 && seIsWrapper(nm)) {
      pn = d_get_child(pn, 0);
      continue;
    }
    return 0;
  }
}

/* Constant fold of the right operand, mirroring
   try(eval(parse(text=.x3), envir=baseenv())).

   Three outcomes, because "R could not fold it" and "we do not know what R
   would have done" are different things:
     SE_FOLD_YES  -- pure numeric arithmetic, we computed it
     SE_FOLD_NO   -- R's eval would have failed or returned a non-number, so
                     no fold happens and we can carry on (an ordinary model
                     symbol is not bound in baseenv(), and neither is an
                     emitted name like M_PI or Rx_pow_di(a,2))
     SE_FOLD_BAIL -- R's eval MIGHT have succeeded, so hand the whole
                     expression to the R walker rather than guess.  That is
                     `pi` (bound in baseenv) and any call whose arguments are
                     all constants, since "sqrt(2)" does evaluate there. */
typedef enum { SE_FOLD_NO = 0, SE_FOLD_YES = 1, SE_FOLD_BAIL = 2 } seFoldRes;

static seFoldRes seFold(D_ParseNode *pn, double *out);

/* a call R might constant-fold: every argument folds to a number */
static seFoldRes seFoldCall(D_ParseNode *pn) {
  int nch = d_get_number_of_children(pn), i;
  for (i = 0; i < nch; i++) {
    D_ParseNode *ch = d_get_child(pn, i);
    const char *nm = seNodeName(ch);
    if (strcmp(nm, "arg_list") == 0) {
      double v;
      seFoldRes r = seFold(ch, &v);
      if (r != SE_FOLD_YES) return SE_FOLD_NO;
    }
  }
  /* no arg_list at all (zero-arg call) or every argument was constant */
  return SE_FOLD_BAIL;
}

/* leaf classification: what would R's baseenv() eval make of this node alone? */
static seFoldRes seFoldLeaf(D_ParseNode *pn, double *out) {
  const char *nm = seNodeName(pn);
  if (strcmp(nm, "function_call") == 0) return seFoldCall(pn);
  if (strcmp(nm, "symbol") == 0 || strcmp(nm, "identifier") == 0) {
    size_t n = (size_t)(pn->end - pn->start_loc.s);
    /* pi is bound in baseenv(); every other bare name we emit is not */
    if (n == 2 && strncmp(pn->start_loc.s, "pi", 2) == 0) return SE_FOLD_BAIL;
    return SE_FOLD_NO;
  }
  if (strcmp(nm, "integer") == 0 || strcmp(nm, "float") == 0) {
    char buf[64];
    size_t n = (size_t)(pn->end - pn->start_loc.s);
    if (n >= sizeof(buf)) return SE_FOLD_NO;
    memcpy(buf, pn->start_loc.s, n); buf[n] = '\0';
    *out = atof(buf);
    return SE_FOLD_YES;
  }
  return SE_FOLD_NO;   /* not a leaf; caller keeps walking */
}

/* combine two folded operands under one arithmetic operator */
static seFoldRes seFoldBinary(char op, seFoldRes ra, double a,
                              seFoldRes rb, double b, double *out) {
  if (ra == SE_FOLD_BAIL || rb == SE_FOLD_BAIL) return SE_FOLD_BAIL;
  if (ra != SE_FOLD_YES || rb != SE_FOLD_YES) return SE_FOLD_NO;
  switch (op) {
  case '+': *out = a + b; return SE_FOLD_YES;
  case '-': *out = a - b; return SE_FOLD_YES;
  case '*': *out = a * b; return SE_FOLD_YES;
  case '/': *out = a / b; return SE_FOLD_YES;
  default:  return SE_FOLD_BAIL;   /* '^' -- R's ^ vs C pow edge cases */
  }
}

static seFoldRes seFold(D_ParseNode *pn, double *out) {
  const char *nm = seNodeName(pn);
  int nch = d_get_number_of_children(pn);

  if (seIsLeafNode(nm) && strcmp(nm, "number") != 0) return seFoldLeaf(pn, out);
  if (nch == 1) return seFold(d_get_child(pn, 0), out);

  if (nch == 2 && strcmp(nm, "unary_expression") == 0) {
    double v;
    seFoldRes r = seFold(d_get_child(pn, 1), &v);
    if (r != SE_FOLD_YES) return r;
    *out = (seNodeName(d_get_child(pn, 0))[0] == '-') ? -v : v;
    return SE_FOLD_YES;
  }

  if (nch == 3) {
    /* '(' expression ')' -- the paren token is child 0, a binary node's
       operator is child 1 */
    if (strcmp(seNodeName(d_get_child(pn, 0)), "(") == 0) {
      return seFold(d_get_child(pn, 1), out);
    }
    const char *mid = seNodeName(d_get_child(pn, 1));
    double a, b;
    seFoldRes ra = seFold(d_get_child(pn, 0), &a);
    seFoldRes rb = seFold(d_get_child(pn, 2), &b);
    if (mid[0] == ',') {                    /* arg_list ',' expression */
      if (ra == SE_FOLD_YES && rb == SE_FOLD_YES) { *out = b; return SE_FOLD_YES; }
      return SE_FOLD_NO;
    }
    return seFoldBinary(mid[0], ra, a, rb, b, out);
  }
  return SE_FOLD_NO;
}


static const char *seEmitParen(seCtx *ctx, D_ParseNode *pn) {
  return seCat(ctx, "(", seEmit(ctx, d_get_child(pn, 1)), ")", NULL, NULL, NULL);
}

/* How `a ^ b` is spelled in C.  .rxFromSE() decides this from the EXPONENT's
   shape, not its value: a literal 2 becomes Rx_pow_di but a call to unary
   minus does not, which is why d^(-1) is (1/(d)) while a^(-2) stays a^-2. */
static const char *seEmitPower(seCtx *ctx, const char *x2, const char *x3,
                               D_ParseNode *rhs) {
  if (strcmp(x3, "1") == 0) return x2;
  if (strcmp(x3, "-1") == 0) return seCat(ctx, "(1/(", x2, "))", NULL, NULL, NULL);
  if (!seIsBareNumber(rhs)) return NULL;          /* caller joins with '^' */
  double d = atof(x3);
  if (d == floor(d)) return seCat(ctx, "Rx_pow_di(", x2, ",", x3, ")", NULL);
  if (strcmp(x3, "0.5") == 0) {
    if (strcmp(x2, "pi") == 0 || strcmp(x2, "M_PI") == 0) return "M_SQRT_PI";
    if (strcmp(x2, "M_2_PI") == 0 || strcmp(x2, "(M_2_PI)") == 0) return "M_SQRT_2dPI";
    return seCat(ctx, "sqrt(", x2, ")", NULL, NULL, NULL);
  }
  return seCat(ctx, "Rx_pow(", x2, ",", x3, ")", NULL);
}

static const char *seBinary(seCtx *ctx, D_ParseNode *pn) {
  const char *x2 = seEmit(ctx, d_get_child(pn, 0));
  if (ctx->failed) return "";
  D_ParseNode *rhs = d_get_child(pn, 2);
  const char *op = seNodeName(d_get_child(pn, 1));
  const char *x3 = seEmit(ctx, rhs);
  if (ctx->failed) return "";

  double fv;
  seFoldRes fr = seFold(rhs, &fv);
  if (fr == SE_FOLD_BAIL) return seFail(ctx);
  if (fr == SE_FOLD_YES) x3 = seNumToStr(ctx, fv);

  int isPow = (op[0] == '^') || (op[0] == '*' && op[1] == '*');
  if (isPow) {
    const char *pw = seEmitPower(ctx, x2, x3, rhs);
    if (pw != NULL) return pw;
  }
  return seNamedConstant(seCat(ctx, x2, isPow ? "^" : op, x3, NULL, NULL, NULL));
}

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

static const char *seFunctionCall(seCtx *ctx, D_ParseNode *pn) {
  const char *name = seNodeText(ctx, d_get_child(pn, 0));
  int nch = d_get_number_of_children(pn), i;

  D_ParseNode *argNode = NULL;
  for (i = 0; i < nch; i++) {
    if (strcmp(seNodeName(d_get_child(pn, i)), "arg_list") == 0) {
      argNode = d_get_child(pn, i);
      break;
    }
  }
  D_ParseNode *args[8];
  int nargs = 0;
  if (argNode != NULL) {
    nargs = seArgs(argNode, args, 8);
    if (nargs < 0) return seFail(ctx);
  }

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

/* one symengine symbol as rxode2 text: constant table, then the reserved
   names, then the mangling chain */
static const char *seEmitSymbol(seCtx *ctx, D_ParseNode *pn) {
  const char *raw = seNodeText(ctx, pn);
  int i;
  /* .cnst in .rxFromSE(): rx_SymPy_Res_<name> unshadows a reserved name */
  if (strncmp(raw, "rx_SymPy_Res_", 13) == 0) {
    for (i = 0; i < seNres; i++) {
      if (strcmp(raw + 13, seRes[i].name) == 0) return seRes[i].name;
    }
  }
  /* .rxSEreserved: numeric in R, emitted with sprintf("%.16f", .).  The later
     `if (.ret == "E") return("M_E")` in .rxFromSE() is dead code -- "E" is in
     .rxSEreserved and returns its numeric rendering first. */
  for (i = 0; i < seNres; i++) {
    if (strcmp(raw, seRes[i].name) == 0) {
      if (seRes[i].val == NULL) break;      /* I: complex, falls through */
      return seRes[i].val;
    }
  }
  return seUnRes(ctx, seDemangle(ctx, seFromSEnum(ctx, raw)));
}

static const char *seEmit(seCtx *ctx, D_ParseNode *pn) {
  if (ctx->failed) return "";
  const char *nm = seNodeName(pn);
  int nch = d_get_number_of_children(pn);

  if (strcmp(nm, "symbol") == 0) return seEmitSymbol(ctx, pn);
  if (seIsNumberNode(nm)) {
    /* do NOT recurse: a terminal's only child is named after its regex.
       Round-trip through a double so the rendering matches what R's parser
       plus as.character() produce (see seDblToStr). */
    return seNumToStr(ctx, atof(seNodeText(ctx, pn)));
  }
  if (strcmp(nm, "function_call") == 0) return seFunctionCall(ctx, pn);
  if (nch == 1) return seEmit(ctx, d_get_child(pn, 0));
  if (nch == 2 && strcmp(nm, "unary_expression") == 0) {
    const char *inner = seEmit(ctx, d_get_child(pn, 1));
    if (ctx->failed) return "";
    return seCat(ctx, seNodeName(d_get_child(pn, 0)), inner,
                 NULL, NULL, NULL, NULL);
  }
  if (nch == 3) {
    if (strcmp(seNodeName(d_get_child(pn, 0)), "(") == 0) {
      return seEmitParen(ctx, pn);
    }
    return seBinary(ctx, pn);
  }
  return seFail(ctx);
}

/* ------------------------------------------------------------ entry point --
   Pure string -> string: no R API, no symengine.  Returns a pointer into
   ctx's arena, or NULL when the caller must fall back to the R walker. */
static const char *seFromSE1(seCtx *ctx, const char *in) {
  ctx->failed = 0;
  D_Parser *p = new_D_Parser(&parser_tables_rxode2seFromSE,
                             sizeof(D_ParseNode_User));
  if (p == NULL) return NULL;
  p->save_parse_tree = 1;
  p->error_recovery = 0;
  D_ParseNode *pn = dparse(p, (char*) in, (int) strlen(in));
  const char *out = NULL;
  if (pn != NULL && p->syntax_errors == 0) {
    out = seEmit(ctx, pn);
    if (ctx->failed) out = NULL;
  }
  if (pn != NULL) free_D_ParseNode(p, pn);
  free_D_Parser(p);
  return out;
}

/* .Call entry: character vector in, character vector out.  Elements the C
   emitter declines are returned as NA_character_ so the R shim can route just
   those to .rxFromSE(). */
SEXP _rxode2_rxFromSEChar(SEXP strVec, SEXP numDerS) {
  if (TYPEOF(strVec) != STRSXP) {
    Rf_error("%s", "'strVec' must be a character vector");
  }
  R_xlen_t n = Rf_xlength(strVec), i;
  int numDer = Rf_asInteger(numDerS);
  SEXP ret = PROTECT(Rf_allocVector(STRSXP, n));
  seCtx ctx;
  ctx.head = NULL; ctx.failed = 0; ctx.numDer = numDer;
  for (i = 0; i < n; i++) {
    SEXP el = STRING_ELT(strVec, i);
    if (el == NA_STRING) { SET_STRING_ELT(ret, i, NA_STRING); continue; }
    const char *out = seFromSE1(&ctx, CHAR(el));
    if (out == NULL) SET_STRING_ELT(ret, i, NA_STRING);
    else SET_STRING_ELT(ret, i, Rf_mkChar(out));
  }
  seArenaFree(&ctx);
  UNPROTECT(1);
  return ret;
}
