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
 *    (1/gamma(2) comes out as 1/1).  seMayFold() tells the two apart.  The third outcome, BAIL, hands the whole
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

/* Names that evaluate to something other than a function in baseenv(), plus
   the parser constants.  Any other bare name is unbound there or a function,
   so arithmetic on it fails in R. */
static const char *seBaseVals[] = {
  "pi", "T", "F", "TRUE", "FALSE", "Inf", "NaN", "NA", "NA_real_",
  "NA_integer_", "NA_character_", "NA_complex_", "NULL", "letters",
  "LETTERS", "month.abb", "month.name", "R.version", "R.version.string",
  "version"
};
#define seNbaseVals ((int)(sizeof(seBaseVals)/sizeof(seBaseVals[0])))

static int seIdStart(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '.' || c == '_';
}

static int seIdChar(char c) {
  return seIdStart(c) || (c >= '0' && c <= '9');
}

static int seIsDigit(char c) {
  return c >= '0' && c <= '9';
}

/* past a quoted string literal starting at p */
static const char *seSkipString(const char *p) {
  char q = *p++;
  while (*p != '\0' && *p != q) {
    if (*p == '\\' && p[1] != '\0') p++;
    p++;
  }
  return (*p != '\0') ? p + 1 : p;
}

/* past a decimal number starting at p; NULL when an R-only literal suffix
   follows (0x1, 1i, 1L) */
static const char *seSkipNumber(const char *p) {
  while (seIsDigit(*p) || *p == '.') p++;
  if ((*p == 'e' || *p == 'E') &&
      (seIsDigit(p[1]) || ((p[1] == '+' || p[1] == '-') && seIsDigit(p[2])))) {
    p += 2;
    while (seIsDigit(*p)) p++;
  }
  return seIdChar(*p) ? NULL : p;
}

/* a non-function value in baseenv() (or a .name, conservatively) */
static int seIsBaseVal(const char *id, size_t n) {
  int i;
  if (id[0] == '.') return 1;
  for (i = 0; i < seNbaseVals; i++) {
    if (strlen(seBaseVals[i]) == n && strncmp(id, seBaseVals[i], n) == 0) return 1;
  }
  return 0;
}

/* past an identifier at p; *unbound is set when it is a variable that
   baseenv() cannot supply (a call name is not a variable) */
static const char *seSkipIdent(const char *p, int *unbound) {
  const char *id = p, *q;
  while (seIdChar(*p)) p++;
  q = p;
  while (*q == ' ' || *q == '\t') q++;
  *unbound = (*q != '(') && !seIsBaseVal(id, (size_t)(p - id));
  return p;
}

/* Could eval(parse(text = s), baseenv()) give a number?  0 only when the text
   names a variable that baseenv() cannot supply (and nothing short-circuits
   around it); then R's fold fails.  Otherwise R may fold something this file
   cannot evaluate (gamma(2), log1p(0), log(2+pi)), so the caller bails. */
static int seMayFold(const char *s) {
  const char *p = s;
  int unbound = 0;
  if (strstr(s, "||") != NULL || strstr(s, "&&") != NULL) return 1;
  while (*p != '\0') {
    if (*p == '"' || *p == '\'') {
      p = seSkipString(p);
    } else if (seIsDigit(*p) || (*p == '.' && seIsDigit(p[1]))) {
      p = seSkipNumber(p);
      if (p == NULL) return 1;
    } else if (seIdStart(*p)) {
      p = seSkipIdent(p, &unbound);
      if (unbound) return 0;
    } else {
      p++;
    }
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
  if (seMayFold(str)) return SE_FOLD_BAIL;
  return SE_FOLD_NO;
}

#endif /* __SE_FROM_SE_FOLD_H__ */
