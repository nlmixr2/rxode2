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

typedef enum { SE_FOLD_NO = 0, SE_FOLD_YES = 1, SE_FOLD_BAIL = 2 } seFoldRes;

/* a tiny arithmetic evaluator that also validates: ok stays 1 only if the
   whole string was numbers, + - * /, parens and spaces */
typedef struct { const char *p; int ok; } seEval;

static double seEvalAdd(seEval *s);

static void seEvalWs(seEval *s) {
  while (*s->p == ' ' || *s->p == '\t') s->p++;
}

static double seEvalPrim(seEval *s) {
  seEvalWs(s);
  if (*s->p == '(') {
    s->p++;
    double v = seEvalAdd(s);
    seEvalWs(s);
    if (*s->p != ')') { s->ok = 0; return 0; }
    s->p++;
    return v;
  }
  char *end = NULL;
  double v = strtod(s->p, &end);
  if (end == s->p) { s->ok = 0; return 0; }
  s->p = end;
  return v;
}

static double seEvalUnary(seEval *s) {
  seEvalWs(s);
  if (*s->p == '-') { s->p++; return -seEvalUnary(s); }
  if (*s->p == '+') { s->p++; return seEvalUnary(s); }
  return seEvalPrim(s);
}

static double seEvalMul(seEval *s) {
  double v = seEvalUnary(s);
  for (;;) {
    seEvalWs(s);
    char c = *s->p;
    if (c != '*' && c != '/') return v;
    /* '**' is a power, which this evaluator does not do */
    if (c == '*' && s->p[1] == '*') { s->ok = 0; return v; }
    s->p++;
    double r = seEvalUnary(s);
    if (!s->ok) return v;
    v = (c == '*') ? v * r : v / r;
  }
}

static double seEvalAdd(seEval *s) {
  double v = seEvalMul(s);
  for (;;) {
    seEvalWs(s);
    char c = *s->p;
    if (c != '+' && c != '-') return v;
    s->p++;
    double r = seEvalMul(s);
    if (!s->ok) return v;
    v = (c == '+') ? v + r : v - r;
  }
}

/* name(<no letters inside>) -- the shape R can still evaluate in baseenv(),
   which is how 1/gamma(2) becomes 1/1.  Anything with a letter inside the
   parentheses names something unbound there, and anything already folded to a
   constant (M_LN2, M_SQRT2) has no parentheses at all. */
static int seConstCall(const char *s) {
  const char *p = s;
  while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') ||
         *p == '.' || *p == '_') {
    p++;
  }
  if (p == s || *p != '(') return 0;
  if (s[strlen(s) - 1] != ')') return 0;
  for (p++; *p != '\0'; p++) {
    if ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z')) return 0;
  }
  return 1;
}

static seFoldRes seFoldStr(const char *str, double *out) {
  if (strcmp(str, "pi") == 0) {         /* bound in baseenv(); R's pi == M_PI */
    *out = M_PI;
    return SE_FOLD_YES;
  }
  seEval s;
  s.p = str;
  s.ok = 1;
  double v = seEvalAdd(&s);
  seEvalWs(&s);
  if (s.ok && *s.p == '\0') {
    *out = v;
    return SE_FOLD_YES;
  }
  if (seConstCall(str)) return SE_FOLD_BAIL;
  return SE_FOLD_NO;
}

#endif /* __SE_FROM_SE_FOLD_H__ */
