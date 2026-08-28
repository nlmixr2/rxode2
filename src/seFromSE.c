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

/* ------------------------------------------------------------------ arena --
   Bump allocator so the recursive emitter can return strings without any
   ownership bookkeeping, and so nothing calls R's allocator (not thread safe)
   inside the walk. */
#define SE_BLK (1 << 14)

typedef struct seBlk {
  struct seBlk *next;
  size_t used, cap;
  char *mem;
} seBlk;

typedef struct {
  seBlk *head;
  int failed;          /* 1 = hand this expression back to the R walker */
  int numDer;          /* .rxFromNumDer: 0 error, 1 forward, 2 central */
} seCtx;

static seBlk *seBlkNew(size_t need) {
  size_t cap = need > SE_BLK ? need : SE_BLK;
  seBlk *b = (seBlk*) malloc(sizeof(seBlk));
  if (b == NULL) return NULL;
  b->mem = (char*) malloc(cap);
  if (b->mem == NULL) { free(b); return NULL; }
  b->used = 0; b->cap = cap; b->next = NULL;
  return b;
}

static void seArenaFree(seCtx *ctx) {
  seBlk *b = ctx->head;
  while (b != NULL) {
    seBlk *n = b->next;
    free(b->mem); free(b);
    b = n;
  }
  ctx->head = NULL;
}

static char *seAlloc(seCtx *ctx, size_t n) {
  seBlk *b = ctx->head;
  if (b == NULL || b->used + n > b->cap) {
    seBlk *nb = seBlkNew(n);
    if (nb == NULL) { ctx->failed = 1; return NULL; }
    nb->next = ctx->head; ctx->head = nb; b = nb;
  }
  char *p = b->mem + b->used;
  b->used += n;
  return p;
}

static const char *seDup(seCtx *ctx, const char *s, size_t n) {
  char *p = seAlloc(ctx, n + 1);
  if (p == NULL) return "";
  memcpy(p, s, n); p[n] = '\0';
  return p;
}

static const char *seStr(seCtx *ctx, const char *s) {
  return seDup(ctx, s, strlen(s));
}

/* concatenate up to 6 pieces */
static const char *seCat(seCtx *ctx, const char *a, const char *b,
                         const char *c, const char *d, const char *e,
                         const char *f) {
  size_t n = 0;
  const char *v[6]; int i, nv = 0;
  v[nv++] = a; v[nv++] = b; v[nv++] = c; v[nv++] = d; v[nv++] = e; v[nv++] = f;
  for (i = 0; i < nv; i++) if (v[i] != NULL) n += strlen(v[i]);
  char *p = seAlloc(ctx, n + 1);
  if (p == NULL) return "";
  char *q = p;
  for (i = 0; i < nv; i++) {
    if (v[i] == NULL) continue;
    size_t l = strlen(v[i]); memcpy(q, v[i], l); q += l;
  }
  *q = '\0';
  return p;
}

static const char *seFail(seCtx *ctx) {
  ctx->failed = 1;
  return "";
}

/* ------------------------------------------------------------- constants --
   Mirrors .rxSEcnt in R/symengine.R.  `val` is what paste() renders the
   constant as (15 significant digits), which is exactly what .rxFromSEnum()
   prefix-matches against.  Keep in the SAME ORDER as .rxSEcnt: the R loop
   returns the first match. */
typedef struct { const char *name; const char *val; } seCnt;

static const seCnt seCnts[] = {
  {"M_E",           "2.71828182845905"},
  {"M_PI",          "3.14159265358979"},
  {"M_PI_2",        "1.5707963267949"},
  {"M_PI_4",        "0.785398163397448"},
  {"M_1_PI",        "0.318309886183791"},
  {"M_2_PI",        "0.636619772367581"},
  {"M_2PI",         "6.28318530717959"},
  {"M_SQRT_PI",     "1.77245385090552"},
  {"M_2_SQRTPI",    "1.12837916709551"},
  {"M_1_SQRT_2PI",  "0.398942280401433"},
  {"M_SQRT2",       "1.4142135623731"},
  {"M_SQRT_3",      "1.73205080756888"},
  {"M_SQRT_32",     "5.65685424949238"},
  {"M_SQRT_2dPI",   "0.797884560802865"},
  {"M_LN_SQRT_PI",  "0.5723649429247"},
  {"M_LN_SQRT_2PI", "0.918938533204673"},
  {"M_LN_SQRT_PId2","0.225791352644727"},
  {"M_LOG10_2",     "0.301029995663981"},
  {"M_LOG2E",       "1.44269504088896"},
  {"M_LOG10E",      "0.434294481903252"},
  {"M_LN2",         "0.693147180559945"},
  {"M_LN10",        "2.30258509299405"}
};
#define seNcnt ((int)(sizeof(seCnts)/sizeof(seCnts[0])))

/* .rxFromSEnum(): prefix-match a rendered leaf against the constant table. */
static const char *seFromSEnum(seCtx *ctx, const char *ret) {
  size_t l = strlen(ret);
  if (l > 5) {
    int i;
    for (i = 0; i < seNcnt; i++) {
      /* substr(val, 1, l) == ret; when l > nchar(val), substr gives val */
      size_t vl = strlen(seCnts[i].val);
      size_t cmpn = l < vl ? l : vl;
      if (cmpn == l && strncmp(seCnts[i].val, ret, cmpn) == 0) {
        return seCnts[i].name;
      }
    }
  }
  return seStr(ctx, ret);
}

/* ------------------------------------------------------------ demangling --
   The sequence of sub() calls in .rxFromSE()'s leaf branch, in the SAME order
   (innermost sub() first).  Each is first-match-only, applied to the running
   string, exactly as sub() is. */

/* ^((?:TH|)ETA)_([1-9][0-9]*)_$ -> \1[\2] */
static int seThEt(seCtx *ctx, const char **s) {
  const char *p = *s;
  size_t pre;
  if (strncmp(p, "THETA_", 6) == 0) pre = 5;
  else if (strncmp(p, "ETA_", 4) == 0) pre = 3;
  else return 0;
  const char *d = p + pre + 1;
  if (*d < '1' || *d > '9') return 0;
  const char *q = d;
  while (*q >= '0' && *q <= '9') q++;
  if (*q != '_' || *(q + 1) != '\0') return 0;
  *s = seCat(ctx, seDup(ctx, p, pre), "[", seDup(ctx, d, (size_t)(q - d)),
             "]", NULL, NULL);
  return 1;
}

/* ^rx__d_dt_(.*)__$ -> d/dt(\1) */
static int sePrefixSuffix(seCtx *ctx, const char **s, const char *pre,
                          const char *suf, const char *open,
                          const char *close) {
  const char *p = *s;
  size_t lp = strlen(pre), ls = strlen(suf), l = strlen(p);
  if (l < lp + ls || strncmp(p, pre, lp) != 0) return 0;
  if (ls > 0 && strcmp(p + l - ls, suf) != 0) return 0;
  size_t inner = l - lp - ls;
  *s = seCat(ctx, open, seDup(ctx, p + lp, inner), close, NULL, NULL, NULL);
  return 1;
}

/* ^rx__df_(.*)_dy_((?:TH|)ETA)_([1-9][0-9]*)___$ -> df(\1)/dy(\2[\3])
   .* is greedy, so the LAST "_dy_" wins */
static int seDfDyTh(seCtx *ctx, const char **s) {
  const char *p = *s;
  size_t l = strlen(p);
  if (strncmp(p, "rx__df_", 7) != 0) return 0;
  if (l < 10 || strcmp(p + l - 3, "___") != 0) return 0;
  const char *body = p + 7;
  size_t bl = l - 7 - 3;
  /* greedy: search for the last "_dy_" */
  const char *dy = NULL, *q;
  for (q = body; q + 4 <= body + bl; q++) {
    if (strncmp(q, "_dy_", 4) == 0) dy = q;
  }
  if (dy == NULL) return 0;
  const char *r = dy + 4;
  size_t pre;
  if (strncmp(r, "THETA_", 6) == 0) pre = 5;
  else if (strncmp(r, "ETA_", 4) == 0) pre = 3;
  else return 0;
  const char *d = r + pre + 1;
  if (*d < '1' || *d > '9') return 0;
  const char *e = d;
  while (*e >= '0' && *e <= '9') e++;
  if (e != body + bl) return 0;
  *s = seCat(ctx, "df(", seDup(ctx, body, (size_t)(dy - body)), ")/dy(",
             seDup(ctx, r, pre), seCat(ctx, "[", seDup(ctx, d, (size_t)(e - d)),
                                       "])", NULL, NULL, NULL), NULL);
  return 1;
}

/* ^rx__df_(.*)_dy_(.*)__$ -> df(\1)/dy(\2); first .* greedy */
static int seDfDy(seCtx *ctx, const char **s) {
  const char *p = *s;
  size_t l = strlen(p);
  if (strncmp(p, "rx__df_", 7) != 0) return 0;
  if (l < 9 || strcmp(p + l - 2, "__") != 0) return 0;
  const char *body = p + 7;
  size_t bl = l - 7 - 2;
  const char *dy = NULL, *q;
  for (q = body; q + 4 <= body + bl; q++) {
    if (strncmp(q, "_dy_", 4) == 0) dy = q;
  }
  if (dy == NULL) return 0;
  *s = seCat(ctx, "df(", seDup(ctx, body, (size_t)(dy - body)), ")/dy(",
             seDup(ctx, dy + 4, (size_t)((body + bl) - (dy + 4))), ")", NULL);
  return 1;
}

/* ^rx_rate_(.*)_ etc: NOT anchored at the end, .* greedy -> last '_' wins */
static int seUnanchored(seCtx *ctx, const char **s, const char *pre,
                        const char *fun) {
  const char *p = *s;
  size_t lp = strlen(pre), l = strlen(p);
  if (l <= lp || strncmp(p, pre, lp) != 0) return 0;
  const char *last = NULL, *q;
  for (q = p + lp; *q != '\0'; q++) if (*q == '_') last = q;
  if (last == NULL) return 0;
  /* sub() replaces only the matched span; anything after it is kept */
  *s = seCat(ctx, fun, "(", seDup(ctx, p + lp, (size_t)(last - (p + lp))),
             ")", seStr(ctx, last + 1), NULL);
  return 1;
}

/* .rxSEreserved (R/symengine.R).  `val` is sprintf("%.16f", value); I is
   complex, so is.numeric() is FALSE there and it falls through as a symbol. */
static const seCnt seRes[] = {
  {"e",           "2.7182818284590451"},
  {"E",           "2.7182818284590451"},
  {"EulerGamma",  "0.5772156649015329"},
  {"Catalan",     "0.9159655941772190"},
  {"GoldenRatio", "2.1180339887498949"},
  {"I",           NULL}
};
#define seNres ((int)(sizeof(seRes)/sizeof(seRes[0])))

/* sub("[(]rx_SymPy_Res_", "(", .ret) -- first match only, as sub() is */
static const char *seUnRes(seCtx *ctx, const char *s) {
  const char *p = strstr(s, "(rx_SymPy_Res_");
  if (p == NULL) return s;
  size_t pre = (size_t)(p - s);
  return seCat(ctx, seDup(ctx, s, pre + 1), p + 14, NULL, NULL, NULL, NULL);
}

static const char *seDemangle(seCtx *ctx, const char *name) {
  const char *s = name;
  seThEt(ctx, &s);
  sePrefixSuffix(ctx, &s, "rx__d_dt_", "__", "d/dt(", ")");
  seDfDyTh(ctx, &s);
  seDfDy(ctx, &s);
  sePrefixSuffix(ctx, &s, "rx_", "_ini_0__", "", "(0)");
  seUnanchored(ctx, &s, "rx_f_", "f");
  seUnanchored(ctx, &s, "rx_lag_", "alag");
  seUnanchored(ctx, &s, "rx_dur_", "dur");
  seUnanchored(ctx, &s, "rx_rate_", "rate");
  return s;
}

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

/* Does this exponent subtree reduce to a bare numeric literal?  This is the
   is.numeric(x[[3]]) test in .rxFromSE(): TRUE only for a literal, so `d^2`
   becomes Rx_pow_di(d,2) while `a^(-2)` (a call to unary minus) does not and
   falls through to "a^-2".  Reproducing that asymmetry matters -- the fixture
   pins it. */
static int seIsBareNumber(D_ParseNode *pn) {
  for (;;) {
    const char *nm = seNodeName(pn);
    int nch = d_get_number_of_children(pn);
    if (strcmp(nm, "number") == 0) return 1;
    if (strcmp(nm, "integer") == 0 || strcmp(nm, "float") == 0) return 1;
    if (nch == 1 &&
        (strcmp(nm, "unary_expression") == 0 ||
         strcmp(nm, "power_expression") == 0 ||
         strcmp(nm, "primary_expression") == 0 ||
         strcmp(nm, "expression") == 0 ||
         strcmp(nm, "mul_expression") == 0 ||
         strcmp(nm, "add_expression") == 0)) {
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

static seFoldRes seFold(D_ParseNode *pn, double *out) {
  const char *nm = seNodeName(pn);
  int nch = d_get_number_of_children(pn);

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

  if (nch == 1) return seFold(d_get_child(pn, 0), out);

  if (nch == 2 && strcmp(nm, "unary_expression") == 0) {
    double v;
    seFoldRes r = seFold(d_get_child(pn, 1), &v);
    if (r != SE_FOLD_YES) return r;
    const char *op = seNodeName(d_get_child(pn, 0));
    *out = (op[0] == '-') ? -v : v;
    return SE_FOLD_YES;
  }

  if (nch == 3) {
    /* '(' expression ')' -- the paren token is child 0, a binary node's
       operator is child 1 */
    if (strcmp(seNodeName(d_get_child(pn, 0)), "(") == 0) {
      return seFold(d_get_child(pn, 1), out);
    }
    const char *mid = seNodeName(d_get_child(pn, 1));
    if (strcmp(mid, ",") == 0) {          /* arg_list ',' expression */
      double a, b;
      seFoldRes ra = seFold(d_get_child(pn, 0), &a);
      seFoldRes rb = seFold(d_get_child(pn, 2), &b);
      if (ra == SE_FOLD_YES && rb == SE_FOLD_YES) { *out = b; return SE_FOLD_YES; }
      return SE_FOLD_NO;
    }
    double a, b;
    seFoldRes ra = seFold(d_get_child(pn, 0), &a);
    seFoldRes rb = seFold(d_get_child(pn, 2), &b);
    if (ra == SE_FOLD_BAIL || rb == SE_FOLD_BAIL) return SE_FOLD_BAIL;
    if (ra != SE_FOLD_YES || rb != SE_FOLD_YES) return SE_FOLD_NO;
    switch (mid[0]) {
    case '+': *out = a + b; return SE_FOLD_YES;
    case '-': *out = a - b; return SE_FOLD_YES;
    case '*': *out = a * b; return SE_FOLD_YES;
    case '/': *out = a / b; return SE_FOLD_YES;
    default:  return SE_FOLD_BAIL;   /* '^' -- R's ^ vs C pow edge cases */
    }
  }
  return SE_FOLD_NO;
}

/* R's as.character() on a double: 15 significant digits, trailing zeros
   dropped.  This matters for the constant table -- R parses "2.718281828459045"
   to a double first, so .rxFromSEnum() sees the 16-character
   as.character() form "2.71828182845905" and prefix-matches M_E.  Matching
   the raw 17-character source text instead would silently miss every
   constant. */
static const char *seDblToStr(seCtx *ctx, double v) {
  char buf[64];
  snprintf(buf, sizeof(buf), "%.15g", v);
  return seStr(ctx, buf);
}

static const char *seNumToStr(seCtx *ctx, double v) {
  return seFromSEnum(ctx, seDblToStr(ctx, v));
}

static const char *seEmitParen(seCtx *ctx, D_ParseNode *pn) {
  return seCat(ctx, "(", seEmit(ctx, d_get_child(pn, 1)), ")", NULL, NULL, NULL);
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
    if (strcmp(x3, "1") == 0) return x2;
    if (strcmp(x3, "-1") == 0) return seCat(ctx, "(1/(", x2, "))", NULL, NULL, NULL);
    if (seIsBareNumber(rhs)) {
      double d = atof(x3);
      if (d == floor(d)) {
        return seCat(ctx, "Rx_pow_di(", x2, ",", x3, ")", NULL);
      }
      if (strcmp(x3, "0.5") == 0) {
        if (strcmp(x2, "pi") == 0 || strcmp(x2, "M_PI") == 0) return "M_SQRT_PI";
        if (strcmp(x2, "M_2_PI") == 0 || strcmp(x2, "(M_2_PI)") == 0) return "M_SQRT_2dPI";
        return seCat(ctx, "sqrt(", x2, ")", NULL, NULL, NULL);
      }
      return seCat(ctx, "Rx_pow(", x2, ",", x3, ")", NULL);
    }
  }
  const char *ret = seCat(ctx, x2, isPow ? "^" : op, x3, NULL, NULL, NULL);

  /* the pi peepholes from .rxFromSE(); string compares on the joined result */
  if (!strcmp(ret, "pi*2") || !strcmp(ret, "2*pi") ||
      !strcmp(ret, "M_PI*2") || !strcmp(ret, "2*M_PI")) return "M_2PI";
  if (!strcmp(ret, "pi/2") || !strcmp(ret, "pi*0.5") || !strcmp(ret, "0.5*pi") ||
      !strcmp(ret, "M_PI/2") || !strcmp(ret, "M_PI*0.5") ||
      !strcmp(ret, "0.5*M_PI")) return "M_PI_2";
  if (!strcmp(ret, "pi/4") || !strcmp(ret, "pi*0.25") || !strcmp(ret, "0.25*pi") ||
      !strcmp(ret, "M_PI/4") || !strcmp(ret, "M_PI*0.25") ||
      !strcmp(ret, "0.25*M_PI")) return "M_PI_4";
  if (!strcmp(ret, "1/pi") || !strcmp(ret, "1/M_PI")) return "M_1_PI";
  if (!strcmp(ret, "2/pi") || !strcmp(ret, "2/M_PI")) return "M_2_PI";
  if (!strcmp(ret, "log(2)/log(10)")) return "M_LOG10_2";
  if (!strcmp(ret, "1/log(10)")) return "M_LOG10E";
  if (!strcmp(ret, "1/log(2)")) return "M_LOG2E";
  if (!strcmp(ret, "2/M_SQRT_PI") || !strcmp(ret, "2/(M_SQRT_PI)")) return "M_2_SQRTPI";
  if (!strcmp(ret, "1/sqrt(M_2PI)") || !strcmp(ret, "1/(sqrt((M_2PI)))") ||
      !strcmp(ret, "1/(M_2PI^0.5)") || !strcmp(ret, "1/(M_2PI^(1/2))") ||
      !strcmp(ret, "1/((M_2PI)^0.5)") || !strcmp(ret, "1/((M_2PI)^(1/2))")) {
    return "M_1_SQRT_2PI";
  }
  return ret;
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
    if (nch == 1) {
      const char *nm = seNodeName(pn);
      if (strcmp(nm, "expression") == 0 || strcmp(nm, "add_expression") == 0 ||
          strcmp(nm, "mul_expression") == 0 || strcmp(nm, "unary_expression") == 0 ||
          strcmp(nm, "power_expression") == 0 ||
          strcmp(nm, "primary_expression") == 0) {
        pn = d_get_child(pn, 0);
        continue;
      }
      return pn;
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

static const char *seFunctionCall(seCtx *ctx, D_ParseNode *pn) {
  D_ParseNode *nameNode = d_get_child(pn, 0);
  const char *name = seNodeText(ctx, nameNode);
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

  /* .SEsingle: abs0(x) -> abs(x).  rxNot and loggamma are left to R (rxNot
     wraps in "(!(" "))" and loggamma collides with the .SE1p lgamma1p path). */
  const char *emitName = NULL;
  if (strcmp(name, "abs0") == 0 && nargs == 1) {
    emitName = "abs";
  } else {
    for (i = 0; i < seNfns; i++) {
      if (strcmp(name, seFns[i].name) == 0) {
        if (seFns[i].nargs != nargs) return seFail(ctx);  /* R raises here */
        emitName = seFns[i].name;
        break;
      }
    }
  }
  if (emitName == NULL) return seFail(ctx);

  const char *body = "";
  for (i = 0; i < nargs; i++) {
    const char *a = seEmit(ctx, seStripP(args[i]));
    if (ctx->failed) return "";
    body = (i == 0) ? a : seCat(ctx, body, ",", a, NULL, NULL, NULL);
  }
  const char *ret = seCat(ctx, emitName, "(", body, ")", NULL, NULL);

  /* the constant peepholes at the end of the generic branch */
  if (!strcmp(ret, "exp(1)")) return "M_E";
  if (!strcmp(ret, "sqrt(3)")) return "M_SQRT_3";
  if (!strcmp(ret, "sqrt(2)")) return "M_SQRT2";
  if (!strcmp(ret, "sqrt(32)")) return "M_SQRT_32";
  if (!strcmp(ret, "sqrt(pi)")) return "M_SQRT_PI";
  if (!strcmp(ret, "sqrt(M_2_PI)") || !strcmp(ret, "sqrt((M_2_PI))")) {
    return "M_SQRT_2dPI";
  }
  return ret;
}

static const char *seEmit(seCtx *ctx, D_ParseNode *pn) {
  if (ctx->failed) return "";
  const char *nm = seNodeName(pn);
  int nch = d_get_number_of_children(pn);

  if (strcmp(nm, "symbol") == 0) {
    const char *raw = seNodeText(ctx, pn);
    int i;
    /* .cnst in .rxFromSE(): rx_SymPy_Res_<name> unshadows a reserved name */
    if (strncmp(raw, "rx_SymPy_Res_", 13) == 0) {
      for (i = 0; i < seNres; i++) {
        if (strcmp(raw + 13, seRes[i].name) == 0) return seRes[i].name;
      }
    }
    /* .rxSEreserved: numeric in R, emitted with sprintf("%.16f", .).  Note the
       later `if (.ret == "E") return("M_E")` in .rxFromSE() is dead code --
       "E" is in .rxSEreserved and returns its numeric rendering first. */
    for (i = 0; i < seNres; i++) {
      if (strcmp(raw, seRes[i].name) == 0) {
        if (seRes[i].val == NULL) break;   /* I: complex, falls through */
        return seRes[i].val;
      }
    }
    return seUnRes(ctx, seDemangle(ctx, seFromSEnum(ctx, raw)));
  }
  if (strcmp(nm, "number") == 0 || strcmp(nm, "integer") == 0 ||
      strcmp(nm, "float") == 0) {
    /* do NOT recurse: a terminal's only child is named after its regex.
       Round-trip through a double so the rendering matches what R's parser
       plus as.character() produce (see seDblToStr). */
    return seNumToStr(ctx, atof(seNodeText(ctx, pn)));
  }
  if (strcmp(nm, "function_call") == 0) return seFunctionCall(ctx, pn);

  if (nch == 1) return seEmit(ctx, d_get_child(pn, 0));

  if (nch == 2 && strcmp(nm, "unary_expression") == 0) {
    const char *op = seNodeName(d_get_child(pn, 0));
    const char *inner = seEmit(ctx, d_get_child(pn, 1));
    if (ctx->failed) return "";
    return seCat(ctx, op, inner, NULL, NULL, NULL, NULL);
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
