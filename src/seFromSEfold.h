/*
 * seFromSEfold.h -- would R have constant-folded this operand?
 *
 * .rxFromSE() runs try(eval(parse(text=.x3), envir=baseenv())) on the right
 * operand it has ALREADY EMITTED, and re-renders the result when it is a
 * number.  Two things follow, and both matter:
 *
 *  - the question is asked of the emitted TEXT, not of the parse tree.  By
 *    then log(2) has become M_LN2, which does not evaluate in baseenv(), so
 *    1/log(2) stays 1/M_LN2.  Deciding from the tree instead would wrongly
 *    treat it as a constant call.
 *  - "R could not fold it" and "we do not know what R would have done" are
 *    different answers.  An ordinary model symbol is unbound in baseenv() so
 *    nothing folds, but `pi` IS bound there and gamma(2) really does evaluate
 *    (1/gamma(2) comes out as 1/1).  The third outcome, BAIL, hands the whole
 *    expression to the R walker rather than guess.
 */
#ifndef __SE_FROM_SE_FOLD_H__
#define __SE_FROM_SE_FOLD_H__

#include "rxNameChar.h"

typedef enum { SE_FOLD_NO = 0, SE_FOLD_YES = 1, SE_FOLD_BAIL = 2 } seFoldRes;

static int seIsBaseFn(const seCtx *ctx, const char *name, size_t n);

/* What R's eval(parse(text = .), envir = baseenv()) does with the text,
   reproduced for the part of R it can reach: double arithmetic (+ - * / ^),
   pi, and the one-argument base math functions, each through the routine R
   itself calls so the value is the same double.

   st is a bit set: 1 = R's eval() fails (a name baseenv() does not bind, or
   not R syntax) so nothing folds -- whatever else is in the text, so the
   walk stops there; 2 = R might produce a value this cannot reproduce
   (another base function, %%, a comparison, ...).  A name other than pi is
   a model variable, never the value baseenv() happens to bind to it (T, F;
   #1692), which is how .rxFromSE() folds too. */
typedef struct {
  const char *p;
  int st;
  const seCtx *ctx;
} seEval;

static double seEvalAdd(seEval *s);
static double seEvalUnary(seEval *s);

static void seEvalWs(seEval *s) {
  while (*s->p == ' ' || *s->p == '\t' || *s->p == '\n') s->p++;
}

static void seEvalSt(seEval *s, int st) {
  s->st |= st;
}

#define seEvalDead(s) (((s)->st & 1) != 0)

/* R's `^` (R_POW in arithmetic.c) */
static double seEvalPow(double x, double y) {
  return y == 2.0 ? x * x : R_pow(x, y);
}

/* the one-argument base functions, evaluated as R's math1() does */
static int seEvalFn1(const char *b, size_t n, double x, double *out) {
#define SE_FN1(nm, expr) \
  if (n == sizeof(nm) - 1 && strncmp(b, nm, n) == 0) { *out = (expr); return 1; }
  SE_FN1("exp", exp(x))
  SE_FN1("log", x > 0 ? log(x) : (x == 0 ? R_NegInf : R_NaN))
  SE_FN1("sqrt", sqrt(x))
  SE_FN1("log1p", log1p(x))
  SE_FN1("expm1", expm1(x))
  SE_FN1("gamma", Rf_gammafn(x))
  SE_FN1("lgamma", Rf_lgammafn(x))
  SE_FN1("digamma", Rf_digamma(x))
  SE_FN1("trigamma", Rf_trigamma(x))
  SE_FN1("sin", sin(x))
  SE_FN1("cos", cos(x))
  SE_FN1("tan", tan(x))
  SE_FN1("abs", fabs(x))
  SE_FN1("floor", floor(x))
  SE_FN1("ceiling", ceil(x))
  SE_FN1("trunc", trunc(x))
  SE_FN1("sign", Rf_sign(x))
#undef SE_FN1
  return 0;
}

/* ( expr ) */
static double seEvalParen(seEval *s) {
  s->p++;
  double v = seEvalAdd(s);
  seEvalWs(s);
  if (*s->p != ')') { seEvalSt(s, 1); return 0; }
  s->p++;
  return v;
}

/* a numeric literal, read as R's parser reads it */
static double seEvalNumber(seEval *s) {
  char *end = NULL;
  double v = R_strtod(s->p, &end);
  if (end == s->p) { seEvalSt(s, 1); return 0; }
  s->p = end;
  /* 1L, 0x10, 1i: R syntax this does not model */
  if (*s->p == 'L' || *s->p == 'i' || *s->p == 'x' || *s->p == 'X') seEvalSt(s, 2);
  return v;
}

/* skip the rest of an argument list up to and including its ")" */
static void seEvalSkipArgs(seEval *s) {
  int depth = 0;
  while (*s->p != '\0' && !(*s->p == ')' && depth == 0)) {
    if (*s->p == '(') depth++;
    if (*s->p == ')') depth--;
    s->p++;
  }
  if (*s->p == ')') s->p++;
}

/* name( ... ), with s->p on the "(": a base function of one argument this
   evaluates; another base function R might evaluate (unsure); anything else
   is unbound in baseenv() and R's eval() fails */
static double seEvalCall(seEval *s, const char *b, size_t n) {
  if (!seIsBaseFn(s->ctx, b, n)) { seEvalSt(s, 1); return 0; }
  s->p++;
  seEvalWs(s);
  if (*s->p == ')') { s->p++; seEvalSt(s, 2); return 0; }
  double a = seEvalAdd(s);
  if (seEvalDead(s)) return 0;
  seEvalWs(s);
  if (*s->p != ')') {
    /* more than one argument: some other base function */
    seEvalSt(s, 2);
    seEvalSkipArgs(s);
    return 0;
  }
  s->p++;
  double r;
  if (!seEvalFn1(b, n, a, &r)) { seEvalSt(s, 2); return 0; }
  return r;
}

/* What R reads a bare word as when it is not a name: pi (bound in
   baseenv()), and R's literal constants.  Inf and NaN are numbers -- the fold
   writes Inf back as text, and 1/Inf must fold to 0 as R folds it -- while
   NA, TRUE, FALSE and NULL are constants R may still compute with, which
   this does not model.  Every other word is a name. */
enum { SE_LIT_PI, SE_LIT_INF, SE_LIT_NAN, SE_LIT_UNSURE };
typedef struct { const char *word; int kind; } seLiteral;

/* 0 = a name; 1 = a number, in *value; 2 = a constant this does not model */
static int seEvalLiteral(const char *b, size_t n, double *value) {
  static const seLiteral lit[] = {
    {"pi", SE_LIT_PI}, {"Inf", SE_LIT_INF}, {"NaN", SE_LIT_NAN},
    {"NA", SE_LIT_UNSURE}, {"TRUE", SE_LIT_UNSURE}, {"FALSE", SE_LIT_UNSURE},
    {"NULL", SE_LIT_UNSURE}
  };
  size_t i;
  for (i = 0; i < sizeof(lit)/sizeof(lit[0]); i++) {
    if (strlen(lit[i].word) != n || strncmp(lit[i].word, b, n) != 0) continue;
    switch (lit[i].kind) {
    case SE_LIT_PI: *value = M_PI; return 1;
    case SE_LIT_INF: *value = R_PosInf; return 1;
    case SE_LIT_NAN: *value = R_NaN; return 1;
    default: return 2;
    }
  }
  return 0;
}

/* A name: a call, a literal, or a variable.  A model name is a variable even
   when baseenv() binds it (T, F; #1692) */
static double seEvalName(seEval *s) {
  const char *b = s->p;
  double v = 0;
  while (rxNameChar(*s->p)) s->p++;
  size_t n = (size_t)(s->p - b);
  seEvalWs(s);
  if (*s->p == '(') return seEvalCall(s, b, n);
  int lit = seEvalLiteral(b, n, &v);
  if (lit == 1) return v;
  seEvalSt(s, lit == 2 ? 2 : 1);
  return 0;
}

static double seEvalPrim(seEval *s) {
  if (seEvalDead(s)) return 0;
  seEvalWs(s);
  char c = *s->p;
  if (c == '(') return seEvalParen(s);
  if ((c >= '0' && c <= '9') || (c == '.' && s->p[1] >= '0' && s->p[1] <= '9')) {
    return seEvalNumber(s);
  }
  if (rxNameStart(c)) return seEvalName(s);
  /* not something this models; R may still read it */
  seEvalSt(s, c == '\0' || c == ')' ? 1 : 2);
  if (c != '\0') s->p++;
  return 0;
}

/* power binds tighter than unary minus and is right associative: -2^2 is
   -4, 2^-1 is 0.5, 2^3^2 is 2^9 */
static double seEvalPowE(seEval *s) {
  double v = seEvalPrim(s);
  if (seEvalDead(s)) return 0;
  seEvalWs(s);
  if (*s->p == '^' || (s->p[0] == '*' && s->p[1] == '*')) {
    s->p += (*s->p == '^') ? 1 : 2;
    double e = seEvalUnary(s);
    return seEvalPow(v, e);
  }
  return v;
}

static double seEvalUnary(seEval *s) {
  if (seEvalDead(s)) return 0;
  seEvalWs(s);
  if (*s->p == '-') { s->p++; return -seEvalUnary(s); }
  if (*s->p == '+') { s->p++; return seEvalUnary(s); }
  return seEvalPowE(s);
}

static double seEvalMul(seEval *s) {
  double v = seEvalUnary(s);
  for (;;) {
    if (seEvalDead(s)) return v;
    seEvalWs(s);
    char c = *s->p;
    if (c == '%') { seEvalSt(s, 2); return v; }
    if (c != '*' && c != '/') return v;
    if (c == '*' && s->p[1] == '*') return v;      /* a power, handled below */
    s->p++;
    double r = seEvalUnary(s);
    v = (c == '*') ? v * r : v / r;
  }
}

static double seEvalAdd(seEval *s) {
  double v = seEvalMul(s);
  for (;;) {
    if (seEvalDead(s)) return v;
    seEvalWs(s);
    char c = *s->p;
    if (c != '+' && c != '-') return v;
    s->p++;
    double r = seEvalMul(s);
    v = (c == '+') ? v + r : v - r;
  }
}

static seFoldRes seFoldStr(const seCtx *ctx, const char *str, double *out) {
  seEval s;
  s.p = str;
  s.st = 0;
  s.ctx = ctx;
  double v = seEvalAdd(&s);
  seEvalWs(&s);
  if (seEvalDead(&s)) return SE_FOLD_NO;
  /* anything left over (a comparison, &&, ...) R may read differently */
  if (*s.p != '\0') return SE_FOLD_BAIL;
  if (s.st == 0) {
    *out = v;
    return SE_FOLD_YES;
  }
  return SE_FOLD_BAIL;
}

#endif /* __SE_FROM_SE_FOLD_H__ */
