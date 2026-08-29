/*
 * seFromSEnames.h -- the name and constant vocabulary shared by symengine and
 * rxode2, for seFromSE.c.
 *
 * Three jobs, all pure string -> string: undo rxode2's name mangling
 * (THETA_1_ -> THETA[1], rx__d_dt_X__ -> d/dt(X), ...), render a number the
 * way R's parser plus as.character() would, and recognize emitted text that
 * stands for one of R's C constants (M_PI, M_LN2, ...).
 */
#ifndef __SE_FROM_SE_NAMES_H__
#define __SE_FROM_SE_NAMES_H__

#include "seFromSEarena.h"

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

/* --- the pieces rxode2's mangled names are built from --------------------
   Shared by the THETA/ETA symbol form and the df(..)/dy(..) forms, which
   otherwise spell the same parse out three times. */

/* "THETA_" -> 5, "ETA_" -> 3, 0 if neither: the name length before the
   ordinal's underscore */
static size_t seThEtaName(const char *p) {
  if (strncmp(p, "THETA_", 6) == 0) return 5;
  if (strncmp(p, "ETA_", 4) == 0) return 3;
  return 0;
}

/* end of a [1-9][0-9]* ordinal starting at d, or NULL */
static const char *seOrdinalEnd(const char *d) {
  if (*d < '1' || *d > '9') return NULL;
  while (*d >= '0' && *d <= '9') d++;
  return d;
}

/* the LAST "_dy_" in [b, b+n), which is what the greedy .* in
   ^rx__df_(.*)_dy_ ... selects */
static const char *seLastDy(const char *b, size_t n) {
  const char *last = NULL, *q;
  for (q = b; q + 4 <= b + n; q++) {
    if (strncmp(q, "_dy_", 4) == 0) last = q;
  }
  return last;
}

/* ^((?:TH|)ETA)_([1-9][0-9]*)_$ -> \1[\2] */
static int seThEt(seCtx *ctx, const char **s) {
  const char *p = *s;
  size_t pre = seThEtaName(p);
  if (pre == 0) return 0;
  const char *d = p + pre + 1;
  const char *q = seOrdinalEnd(d);
  if (q == NULL || *q != '_' || *(q + 1) != '\0') return 0;
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
  const char *dy = seLastDy(body, bl);
  if (dy == NULL) return 0;
  const char *r = dy + 4;
  size_t pre = seThEtaName(r);
  if (pre == 0) return 0;
  const char *d = r + pre + 1;
  const char *e = seOrdinalEnd(d);
  if (e == NULL || e != body + bl) return 0;
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
  const char *dy = seLastDy(body, bl);
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

/* Emitted text that IS a named C constant.  .rxFromSE() spells this out as
   three separate if-chains -- one in the binary-operator branch, one at the
   end of the generic call branch, one inside the log() branch -- but they are
   all the same question asked of a finished string, so they are one table
   here.  Order does not matter: the keys are distinct literals. */
typedef struct { const char *text; const char *name; } seNamed;

static const seNamed seNamedTab[] = {
  /* binary-operator branch */
  {"pi*2", "M_2PI"}, {"2*pi", "M_2PI"}, {"M_PI*2", "M_2PI"}, {"2*M_PI", "M_2PI"},
  {"pi/2", "M_PI_2"}, {"pi*0.5", "M_PI_2"}, {"0.5*pi", "M_PI_2"},
  {"M_PI/2", "M_PI_2"}, {"M_PI*0.5", "M_PI_2"}, {"0.5*M_PI", "M_PI_2"},
  {"pi/4", "M_PI_4"}, {"pi*0.25", "M_PI_4"}, {"0.25*pi", "M_PI_4"},
  {"M_PI/4", "M_PI_4"}, {"M_PI*0.25", "M_PI_4"}, {"0.25*M_PI", "M_PI_4"},
  {"1/pi", "M_1_PI"}, {"1/M_PI", "M_1_PI"},
  {"2/pi", "M_2_PI"}, {"2/M_PI", "M_2_PI"},
  {"(M_2_PI)^0.5", "M_SQRT_2dPI"}, {"(M_2_PI)^(1/2)", "M_SQRT_2dPI"},
  {"M_2_PI^0.5", "M_SQRT_2dPI"}, {"M_2_PI^(1/2)", "M_SQRT_2dPI"},
  {"sqrt((M_2_PI))", "M_SQRT_2dPI"},
  {"(pi)^0.5", "M_SQRT_PI"}, {"(pi)^(1/2)", "M_SQRT_PI"},
  {"pi^0.5", "M_SQRT_PI"}, {"pi^(1/2)", "M_SQRT_PI"},
  {"(M_PI)^0.5", "M_SQRT_PI"}, {"(M_PI)^(1/2)", "M_SQRT_PI"},
  {"M_PI^0.5", "M_SQRT_PI"}, {"M_PI^(1/2)", "M_SQRT_PI"},
  {"log(2)/log(10)", "M_LOG10_2"},
  {"1/log(10)", "M_LOG10E"},
  {"1/log(2)", "M_LOG2E"},
  {"2/M_SQRT_PI", "M_2_SQRTPI"}, {"2/(M_SQRT_PI)", "M_2_SQRTPI"},
  {"1/sqrt(M_2PI)", "M_1_SQRT_2PI"}, {"1/(sqrt((M_2PI)))", "M_1_SQRT_2PI"},
  {"1/(M_2PI^0.5)", "M_1_SQRT_2PI"}, {"1/(M_2PI^(1/2))", "M_1_SQRT_2PI"},
  {"1/((M_2PI)^0.5)", "M_1_SQRT_2PI"}, {"1/((M_2PI)^(1/2))", "M_1_SQRT_2PI"},
  /* end of the generic call branch */
  {"exp(1)", "M_E"},
  {"sqrt(3)", "M_SQRT_3"}, {"sqrt(2)", "M_SQRT2"}, {"sqrt(32)", "M_SQRT_32"},
  {"sqrt(pi)", "M_SQRT_PI"},
  {"sqrt(M_2_PI)", "M_SQRT_2dPI"},
  /* inside the log() branch */
  {"log(2)", "M_LN2"}, {"log(10)", "M_LN10"},
  {"log(M_SQRT_PI)", "M_LN_SQRT_PI"},
  {"log(sqrt((M_PI_2)))", "M_LN_SQRT_PId2"}, {"log(sqrt(M_PI_2))", "M_LN_SQRT_PId2"},
  {"log((M_PI_2)^(1/2))", "M_LN_SQRT_PId2"}, {"log((M_PI_2)^0.5)", "M_LN_SQRT_PId2"},
  {"log(M_PI_2^(1/2))", "M_LN_SQRT_PId2"}, {"log(M_PI_2^0.5)", "M_LN_SQRT_PId2"},
  {"log(sqrt((M_2PI)))", "M_LN_SQRT_2PI"}, {"log(sqrt(M_2PI))", "M_LN_SQRT_2PI"},
  {"log((M_2PI)^0.5)", "M_LN_SQRT_2PI"}, {"log((M_2PI)^(1/2))", "M_LN_SQRT_2PI"},
  {"log(M_2PI^0.5)", "M_LN_SQRT_2PI"}, {"log(M_2PI^(1/2))", "M_LN_SQRT_2PI"},
  /* trig at pi, from the generic call branch */
  {"sin(pi)", "0"}, {"cos(pi)", "1"}, {"tan(pi)", "0"}
};
#define seNnamed ((int)(sizeof(seNamedTab)/sizeof(seNamedTab[0])))

/* seNamedConstant() runs on every binary and every call node, so it must not
   walk the whole table.  Bucket the entries by first character once (a
   counting sort over ~70 short literals), then a lookup touches only the
   handful that could match -- and rejects outright when nothing in the table
   starts with that character, which is the common case for a model symbol. */
static short seNamedOrder[seNnamed];
static short seNamedStart[257];
static int seNamedReady = 0;

static void seNamedInit(void) {
  int counts[257], i, c;
  if (seNamedReady) return;
  for (i = 0; i < 257; i++) counts[i] = 0;
  for (i = 0; i < seNnamed; i++) {
    c = (unsigned char) seNamedTab[i].text[0];
    counts[c + 1]++;
  }
  for (i = 1; i < 257; i++) counts[i] += counts[i - 1];
  for (i = 0; i < 257; i++) seNamedStart[i] = (short) counts[i];
  for (i = 0; i < seNnamed; i++) {
    c = (unsigned char) seNamedTab[i].text[0];
    seNamedOrder[counts[c]++] = (short) i;
  }
  seNamedReady = 1;
}

/* the finished text if it names a constant, otherwise the text unchanged */
static const char *seNamedConstant(const char *ret) {
  int c, i;
  /* The entry point initializes before any work, so this branch is always
     taken false in the hot path (and inside a future parallel region).  It is
     here because an uninitialized bucket table would silently stop folding
     constants -- a wrong answer rather than a safe bail. */
  if (!seNamedReady) seNamedInit();
  c = (unsigned char) ret[0];
  int lo = seNamedStart[c], hi = seNamedStart[c + 1];
  for (i = lo; i < hi; i++) {
    const seNamed *e = &seNamedTab[seNamedOrder[i]];
    if (strcmp(ret, e->text) == 0) return e->name;
  }
  return ret;
}

#endif /* __SE_FROM_SE_NAMES_H__ */
