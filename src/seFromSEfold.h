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
  "NA_integer_", "NA_character_", "NULL", "letters", "LETTERS", "month.abb",
  "month.name", "R.version", "R.version.string", "version"
};
#define seNbaseVals ((int)(sizeof(seBaseVals)/sizeof(seBaseVals[0])))

static int seIdStart(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '.' || c == '_';
}

static int seIdChar(char c) {
  return seIdStart(c) || (c >= '0' && c <= '9');
}

/* Could eval(parse(text = s), baseenv()) give a number?  0 only when the text
   names a variable that baseenv() cannot supply (and nothing short-circuits
   around it); then R's fold fails.  Otherwise R may fold something this file
   cannot evaluate (gamma(2), log1p(0), log(2+pi)), so the caller bails. */
static int seMayFold(const char *s) {
  const char *p = s;
  int i;
  if (strstr(s, "||") != NULL || strstr(s, "&&") != NULL) return 1;
  while (*p != '\0') {
    if (*p == '"' || *p == '\'') {               /* string literal */
      char q = *p++;
      while (*p != '\0' && *p != q) {
        if (*p == '\\' && p[1] != '\0') p++;
        p++;
      }
      if (*p != '\0') p++;
      continue;
    }
    if ((*p >= '0' && *p <= '9') || (*p == '.' && p[1] >= '0' && p[1] <= '9')) {
      while ((*p >= '0' && *p <= '9') || *p == '.') p++;
      if ((*p == 'e' || *p == 'E') &&
          ((p[1] >= '0' && p[1] <= '9') ||
           ((p[1] == '+' || p[1] == '-') && p[2] >= '0' && p[2] <= '9'))) {
        p += 2;
        while (*p >= '0' && *p <= '9') p++;
      }
      continue;
    }
    if (seIdStart(*p)) {
      const char *id = p;
      while (seIdChar(*p)) p++;
      size_t n = (size_t)(p - id);
      const char *q = p;
      while (*q == ' ' || *q == '\t') q++;
      if (*q == '(') continue;                    /* a call */
      if (id[0] == '.') continue;                 /* .Machine and friends */
      for (i = 0; i < seNbaseVals; i++) {
        if (strlen(seBaseVals[i]) == n && strncmp(id, seBaseVals[i], n) == 0) break;
      }
      if (i == seNbaseVals) return 0;
      continue;
    }
    p++;
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
