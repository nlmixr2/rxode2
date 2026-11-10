/*
 * seFromSEcalls.h -- the function-call branch of .rxFromSE() in C.
 *
 * Dispatch follows .rxFromSE()'s order exactly, because the order IS the
 * semantics (a name can qualify for several branches):
 *
 *   1. lag/lead/lag0/lead0, delay/rxDelayD*, polygamma
 *   2. .SEsingle (rxNot, loggamma, abs0) and .SEdouble (lbeta, rxEq, R_pow, ...)
 *   3. one argument: f(NaN) for a dual variable/function, log(beta(a, b))
 *   4. every argument is converted (.ret0), so an argument R cannot convert
 *      makes the whole call decline exactly where R would raise
 *   5. .SEeq = c(.rxSEeq, .rxSEeqUsr()), passed in from R: .SE1p (log ->
 *      log1p, lgamma -> lgamma1p), .SE1m (sin/cos/tan -> sinpi/...), else the
 *      generic name(args) with the call peepholes
 *   6. Derivative, Subs, max/min, the tlast()/podo() family
 *   7. anything else is an R user function (or an error): the R walker
 *
 * Names are classified through hash tables built once (seFnInfoOf() for the
 * fixed names, ctx->tab->eqHash for .SEeq), so no branch scans a list with
 * strcmp.
 *
 * Correctness contract: byte-for-byte .rxFromSE().  Anything not reproduced
 * declines with a reason (seFailWhy) and R falls back.
 */
#ifndef __SE_FROM_SE_CALLS_H__
#define __SE_FROM_SE_CALLS_H__

/* ----------------------------------------------------------- fixed names -- */

/* which early branch of .rxFromSE() a name takes */
enum {
  SE_K_NONE = 0,
  SE_K_LAG,          /* lag/lead/lag0/lead0 */
  SE_K_DELAY,        /* delay/rxDelayD/rxDelayD2/rxDelayD3 */
  SE_K_POLYGAMMA,
  SE_K_SINGLE,       /* .SEsingle */
  SE_K_DOUBLE,       /* .SEdouble */
  SE_K_DERIVATIVE,
  SE_K_SUBS,
  SE_K_MAXMIN,
  SE_K_DOSE          /* tlast/tfirst/dose/podo/tlast0/first0/dose0/podo0 */
};

/* properties that cut across the branches */
#define SE_F_DUAL  1   /* .rxToSEDualVarFunction: f(NaN) comes back as f() */
#define SE_F_ZEROD 2   /* .rxSEzeroD: every Derivative() of it is 0 */
#define SE_F_1P    4   /* .SE1p: name(1 + x) -> p(x) */
#define SE_F_1M    8   /* .SE1m: name(pi*x) -> p(x) */

typedef struct {
  const char *name;
  int kind;
  int flags;
  const char *a, *b, *c;   /* .SEsingle open/close, .SEdouble open/mid/close */
  const char *p;           /* .SE1p / .SE1m target */
} seFnInfo;

/* Mirrors .SEsingle, .SEdouble, .SE1p, .SE1m, .rxSEzeroD and
   .rxToSEDualVarFunction in R/symengine.R; test-symengine-c-tables.R checks
   them against the R objects.  A name appears once. */
static const seFnInfo seFnTab[] = {
  {"lag",       SE_K_LAG,   SE_F_ZEROD, NULL, NULL, NULL, NULL},
  {"lead",      SE_K_LAG,   SE_F_ZEROD, NULL, NULL, NULL, NULL},
  {"lag0",      SE_K_LAG,   0, NULL, NULL, NULL, NULL},
  {"lead0",     SE_K_LAG,   0, NULL, NULL, NULL, NULL},
  {"delay",     SE_K_DELAY, SE_F_ZEROD, NULL, NULL, NULL, NULL},
  {"rxDelayD",  SE_K_DELAY, SE_F_ZEROD, NULL, NULL, NULL, NULL},
  {"rxDelayD2", SE_K_DELAY, SE_F_ZEROD, NULL, NULL, NULL, NULL},
  {"rxDelayD3", SE_K_DELAY, SE_F_ZEROD, NULL, NULL, NULL, NULL},
  {"polygamma", SE_K_POLYGAMMA, 0, NULL, NULL, NULL, NULL},
  /* .SEsingle */
  {"rxNot",     SE_K_SINGLE, 0, "(!(", "))", NULL, NULL},
  {"loggamma",  SE_K_SINGLE, SE_F_1P, "lgamma(", ")", NULL, "lgamma1p"},
  {"abs0",      SE_K_SINGLE, 0, "abs(", ")", NULL, NULL},
  /* .SEdouble (the operator spellings cannot be a symengine call name) */
  {"lbeta",     SE_K_DOUBLE, 0, "lbeta(", ",", ")", NULL},
  {"rxMod",     SE_K_DOUBLE, 0, "(", "%%", ")", NULL},
  {"rxEq",      SE_K_DOUBLE, 0, "(", "==", ")", NULL},
  {"rxNeq",     SE_K_DOUBLE, 0, "(", "!=", ")", NULL},
  {"rxGeq",     SE_K_DOUBLE, 0, "(", ">=", ")", NULL},
  {"rxLeq",     SE_K_DOUBLE, 0, "(", "<=", ")", NULL},
  {"rxGt",      SE_K_DOUBLE, 0, "(", ">", ")", NULL},
  {"rxLt",      SE_K_DOUBLE, 0, "(", "<", ")", NULL},
  {"rxAnd",     SE_K_DOUBLE, 0, "(", "&&", ")", NULL},
  {"rxOr",      SE_K_DOUBLE, 0, "(", "||", ")", NULL},
  {"R_pow",     SE_K_DOUBLE, 0, "(", ")^(", ")", NULL},
  {"R_pow_di",  SE_K_DOUBLE, 0, "(", ")^(", ")", NULL},
  {"Rx_pow",    SE_K_DOUBLE, 0, "(", ")^(", ")", NULL},
  {"Rx_pow_di", SE_K_DOUBLE, 0, "(", ")^(", ")", NULL},
  /* .SE1p (loggamma is above) / .SE1m */
  {"log",       SE_K_NONE, SE_F_1P, NULL, NULL, NULL, "log1p"},
  {"lgamma",    SE_K_NONE, SE_F_1P, NULL, NULL, NULL, "lgamma1p"},
  {"sin",       SE_K_NONE, SE_F_1M, NULL, NULL, NULL, "sinpi"},
  {"cos",       SE_K_NONE, SE_F_1M, NULL, NULL, NULL, "cospi"},
  {"tan",       SE_K_NONE, SE_F_1M, NULL, NULL, NULL, "tanpi"},
  /* the branches after .SEeq */
  {"Derivative", SE_K_DERIVATIVE, 0, NULL, NULL, NULL, NULL},
  {"Subs",      SE_K_SUBS, 0, NULL, NULL, NULL, NULL},
  {"max",       SE_K_MAXMIN, 0, NULL, NULL, NULL, NULL},
  {"min",       SE_K_MAXMIN, 0, NULL, NULL, NULL, NULL},
  {"tlast",     SE_K_DOSE, SE_F_DUAL, NULL, NULL, NULL, NULL},
  {"tfirst",    SE_K_DOSE, SE_F_DUAL, NULL, NULL, NULL, NULL},
  {"dose",      SE_K_DOSE, SE_F_DUAL, NULL, NULL, NULL, NULL},
  {"podo",      SE_K_DOSE, SE_F_DUAL, NULL, NULL, NULL, NULL},
  {"tlast0",    SE_K_DOSE, SE_F_DUAL, NULL, NULL, NULL, NULL},
  {"first0",    SE_K_DOSE, 0, NULL, NULL, NULL, NULL},
  {"dose0",     SE_K_DOSE, SE_F_DUAL, NULL, NULL, NULL, NULL},
  {"podo0",     SE_K_DOSE, SE_F_DUAL, NULL, NULL, NULL, NULL},
  /* the rest of .rxToSEDualVarFunction */
  {"tad",       SE_K_NONE, SE_F_DUAL, NULL, NULL, NULL, NULL},
  {"tad0",      SE_K_NONE, SE_F_DUAL, NULL, NULL, NULL, NULL},
  {"tafd",      SE_K_NONE, SE_F_DUAL, NULL, NULL, NULL, NULL},
  {"tafd0",     SE_K_NONE, SE_F_DUAL, NULL, NULL, NULL, NULL},
  {"tfirst0",   SE_K_NONE, SE_F_DUAL, NULL, NULL, NULL, NULL},
  {"dosenum",   SE_K_NONE, SE_F_DUAL, NULL, NULL, NULL, NULL},
  {"dosenum0",  SE_K_NONE, SE_F_DUAL, NULL, NULL, NULL, NULL},
  /* the rest of .rxSEzeroD (.rxSElocallyConstant) */
  {"floor",     SE_K_NONE, SE_F_ZEROD, NULL, NULL, NULL, NULL},
  {"ceil",      SE_K_NONE, SE_F_ZEROD, NULL, NULL, NULL, NULL},
  {"ceiling",   SE_K_NONE, SE_F_ZEROD, NULL, NULL, NULL, NULL},
  {"round",     SE_K_NONE, SE_F_ZEROD, NULL, NULL, NULL, NULL},
  {"trunc",     SE_K_NONE, SE_F_ZEROD, NULL, NULL, NULL, NULL},
  {"ftrunc",    SE_K_NONE, SE_F_ZEROD, NULL, NULL, NULL, NULL},
  {"fround",    SE_K_NONE, SE_F_ZEROD, NULL, NULL, NULL, NULL},
  {"fprec",     SE_K_NONE, SE_F_ZEROD, NULL, NULL, NULL, NULL},
  {"sign",      SE_K_NONE, SE_F_ZEROD, NULL, NULL, NULL, NULL}
};
#define seNfnTab ((int)(sizeof(seFnTab)/sizeof(seFnTab[0])))

#define SE_FN_HASH 256          /* > 2 x seNfnTab, power of two */
static short seFnSlot[SE_FN_HASH];
static int seFnReady = 0;

/* main thread, before any parallel region (seNamedInit() calls it) */
static void seFnInit(void) {
  int i;
  if (seFnReady) return;
  for (i = 0; i < SE_FN_HASH; i++) seFnSlot[i] = -1;
  for (i = 0; i < seNfnTab; i++) {
    unsigned int h = seHashStr(seFnTab[i].name) & (SE_FN_HASH - 1);
    while (seFnSlot[h] != -1) h = (h + 1) & (SE_FN_HASH - 1);
    seFnSlot[h] = (short) i;
  }
  seFnReady = 1;
}

static const seFnInfo *seFnInfoOf(const char *name) {
  unsigned int h = seHashStr(name) & (SE_FN_HASH - 1);
  while (seFnSlot[h] != -1) {
    const seFnInfo *f = &seFnTab[seFnSlot[h]];
    if (strcmp(f->name, name) == 0) return f;
    h = (h + 1) & (SE_FN_HASH - 1);
  }
  return NULL;
}

/* ------------------------------------------------ R-supplied name tables -- */

/* .SEeq[name]: the argument count, or -2 when the name is not there (R's NA
   lookup) -- NA entries (max, min, tlast, ...) come across as NA_INTEGER and
   are treated the same, which is what !is.na(.nargs) does. */
static int seEqNargs(const seCtx *ctx, const char *name) {
  const seTables *t = ctx->tab;
  if (t == NULL || t->neq == 0) return -2;
  unsigned int m = (unsigned int) t->eqHash.cap - 1;
  unsigned int h = seHashStr(name) & m;
  while (t->eqHash.slot[h] != -1) {
    const seEq *e = &t->eq[t->eqHash.slot[h]];
    if (strcmp(e->name, name) == 0) {
      return (e->nargs == NA_INTEGER) ? -2 : e->nargs;
    }
    h = (h + 1) & m;
  }
  return -2;
}

/* row `name` of an R-supplied hash table, or -1 */
static int seHashFind(const seHash *h, const char **keys, const char *name,
                      size_t n) {
  if (h->cap == 0) return -1;
  unsigned int m = (unsigned int) h->cap - 1;
  unsigned int k;
  if (n == (size_t) -1) {
    k = seHashStr(name) & m;
  } else {
    unsigned int hv = 2166136261u;
    size_t i;
    for (i = 0; i < n; i++) { hv ^= (unsigned char) name[i]; hv *= 16777619u; }
    k = hv & m;
  }
  while (h->slot[k] != -1) {
    const char *key = keys[h->slot[k]];
    if (n == (size_t) -1 ? strcmp(key, name) == 0
        : (strncmp(key, name, n) == 0 && key[n] == '\0')) {
      return h->slot[k];
    }
    k = (k + 1) & m;
  }
  return -1;
}

/* the derivative row rxode2parseD()[[name]][[which]], or NULL when there is
   no usable one (absent, or a function too dynamic to be a template) */
static const seDeriv *seFindDeriv(seCtx *ctx, const char *name, int which) {
  const seTables *t = ctx->tab;
  int i;
  if (t == NULL || t->nderivs == 0) return NULL;
  /* seHashFind wants the key array; the rows' names are what was hashed */
  unsigned int m = (unsigned int) t->dHash.cap - 1;
  unsigned int h = seHashStr(name) & m;
  while (t->dHash.slot[h] != -1) {
    i = t->dHash.slot[h];
    if (strcmp(t->derivs[i].name, name) == 0) {
      for (; i < t->nderivs && strcmp(t->derivs[i].name, name) == 0; i++) {
        if (t->derivs[i].which == which) return &t->derivs[i];
      }
      return NULL;
    }
    h = (h + 1) & m;
  }
  return NULL;
}

/* length(rxode2parseD()[[name]]), or -1 when the name is not there */
static int seRxdLen(const seCtx *ctx, const char *name) {
  const seTables *t = ctx->tab;
  if (t == NULL || t->nrxd == 0) return -1;
  int r = seHashFind(&t->rxdHash, t->rxdName, name, (size_t) -1);
  return r < 0 ? -1 : t->rxdLen[r];
}

/* exists(name, envir = baseenv(), mode = "function") */
static int seIsBaseFn(const seCtx *ctx, const char *name, size_t n) {
  const seTables *t = ctx->tab;
  if (t == NULL || t->nbase == 0) return 1;   /* unknown: assume it might be */
  return seHashFind(&t->baseHash, t->baseFns, name, n) >= 0;
}

/* ------------------------------------------------------------- utilities -- */

/* .stripP(): drop every redundant layer of parentheses from an argument */
static D_ParseNode *seStripP(seCtx *ctx, D_ParseNode *pn) {
  for (;;) {
    seSub *s = seSubFind(ctx, pn);
    if (s != NULL) {             /* R's tree has w (already stripped) here */
      pn = s->w;
      continue;
    }
    int nch = d_get_number_of_children(pn);
    if (nch == 1 && seNiWrapper(pn)) {
      pn = d_get_child(pn, 0);
      continue;
    }
    if (nch == 3 && seIsLit(d_get_child(pn, 0), '(')) {
      pn = d_get_child(pn, 1);
      continue;
    }
    return pn;
  }
}

/* the node under the single-child precedence wrappers: what R's parser would
   call the expression's top-level form */
static D_ParseNode *seCore(seCtx *ctx, D_ParseNode *pn) {
  for (;;) {
    if (d_get_number_of_children(pn) == 1 && seNiWrapper(pn)) {
      pn = d_get_child(pn, 0);
      continue;
    }
    seSub *s = seSubFind(ctx, pn);
    if (s != NULL) {
      pn = s->w;
      continue;
    }
    return pn;
  }
}

/* collect arg_list left spine into args[], in source order; see seParseNode.h */
#define seArgs(pn, args, max) seArgsFlattenT(sePt, (pn), (args), (max))
#define SE_MAX_ARGS 32

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

/* The meaningful node under a call argument.  seArgs() hands back the arg_list
   node for a single argument, and the grammar's precedence ladder sits on top
   of every value, so descend single-child nodes until one carries meaning. */
static D_ParseNode *seArgNode(seCtx *ctx, D_ParseNode *pn) {
  for (;;) {
    seSub *s = seSubFind(ctx, pn);
    if (s != NULL) {
      pn = s->w;
      continue;
    }
    if (seNiIs(pn, symbol) || seNiIs(pn, number) || seNiIs(pn, function_call)) {
      return pn;
    }
    if (d_get_number_of_children(pn) != 1) return pn;
    pn = d_get_child(pn, 0);
  }
}

/* is the node, as R parses it, a bare numeric constant?  (R: inherits(x,
   "numeric")).  Negative numbers are calls to unary minus, so they are not. */
static int seIsNumLit(D_ParseNode *pn, double *v, seCtx *ctx) {
  D_ParseNode *c = seCore(ctx, pn);
  /* NaN is numeric to R, and `NaN == 1` then stops .rxP1rmF() with an
     error: decline rather than guess */
  if (seNiIs(c, symbol) && strcmp(seNodeText(ctx, c), "NaN") == 0) {
    seFailWhy(ctx, SE_WHY_LOG);
    return 0;
  }
  if (seNiNumber(c)) {
    if (v != NULL) *v = R_strtod(seNodeText(ctx, c), NULL);
    return 1;
  }
  return 0;
}

/* is the node, as R parses it, the bare symbol `nm`? (as.character(x) of
   length one equal to nm -- which a number never is, since nm is a name) */
static int seIsSym(seCtx *ctx, D_ParseNode *pn, const char *nm) {
  D_ParseNode *c = seCore(ctx, pn);
  return seNiIs(c, symbol) && strcmp(seNodeText(ctx, c), nm) == 0;
}

/* the binary operator of a 3-child operator node, or 0 */
static char seBinOp(D_ParseNode *c) {
  if (d_get_number_of_children(c) != 3) return 0;
  if (seIsLit(d_get_child(c, 0), '(')) return 0;
  if (seNiIs(c, function_call)) return 0;
  D_ParseNode *op = d_get_child(c, 1);
  if (seIsPowOp(op)) return '^';
  return seNodeName(op)[0];
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

/* join emitted arguments with sep */
static const char *seJoin(seCtx *ctx, const char **a, int n, const char *sep) {
  const char *body = "";
  int i;
  for (i = 0; i < n; i++) {
    body = (i == 0) ? a[i] : seCat(ctx, body, sep, a[i], NULL, NULL, NULL);
  }
  return body;
}

/* R reserved words: not names to str2lang(), so a chain step on one fails */
static int seIsRReserved(const char *s, size_t n) {
  static const char *w[] = {"if", "else", "repeat", "while", "function",
                            "for", "next", "break", "in"};
  size_t i;
  for (i = 0; i < sizeof(w)/sizeof(w[0]); i++) {
    if (strlen(w[i]) == n && strncmp(w[i], s, n) == 0) return 1;
  }
  return 0;
}

/* Convert nodes[0..n-1] in order into out[], each paren-stripped first when
   `strip` is set (as .rxFromSE() strips a generic call's arguments).  0 as
   soon as one declines, which declines the whole call. */
static int seEmitAll(seCtx *ctx, D_ParseNode **nodes, int n, int strip,
                     const char **out) {
  int i;
  for (i = 0; i < n; i++) {
    out[i] = seEmit(ctx, strip ? seStripP(ctx, nodes[i]) : nodes[i]);
    if (ctx->failed) return 0;
  }
  return 1;
}

/* ------------------------------------------------------- log1p / sinpi -- */

/* .rxP1rmF(): hunt a literal 1 down the +/- spine of log()/lgamma()'s RAW
   argument.  Pieces are joined with no folding and numbers are written with
   as.character(); everything that is not a + or - goes through .rxFromSE(). */
static const char *seP1rmF(seCtx *ctx, D_ParseNode *pn, int *found);

/* `lit + other` (or `other + lit`) where one side is a numeric literal: a
   literal 1 is the one being hunted, and drops out; any other literal stays,
   written back with as.character() on its own side of the '+'. */
static const char *seP1rmLiteral(seCtx *ctx, double lit, D_ParseNode *other,
                                 int litFirst, int *found) {
  if (lit == 1.0) {
    *found = 1;
    return seEmit(ctx, seStripP(ctx, other));
  }
  const char *o = seP1rmF(ctx, other, found);
  if (ctx->failed) return "";
  const char *n = seDblToStr(ctx, lit);
  return litFirst ? seCat(ctx, n, "+", o, NULL, NULL, NULL)
    : seCat(ctx, o, "+", n, NULL, NULL, NULL);
}

/* .rxP1rmF() on `l + r`: the right side is tested for a literal first */
static const char *seP1rmPlus(seCtx *ctx, D_ParseNode *l, D_ParseNode *r,
                              int *found) {
  double v;
  int rNum = seIsNumLit(r, &v, ctx);
  if (ctx->failed) return "";
  if (rNum) return seP1rmLiteral(ctx, v, l, 0, found);
  int lNum = seIsNumLit(l, &v, ctx);
  if (ctx->failed) return "";
  if (lNum) return seP1rmLiteral(ctx, v, r, 1, found);
  const char *a = seP1rmF(ctx, l, found);
  if (ctx->failed) return "";
  const char *b = seP1rmF(ctx, r, found);
  if (ctx->failed) return "";
  return seCat(ctx, a, "+", b, NULL, NULL, NULL);
}

/* .rxP1rmF() on `1 - y`: x[-2] turns it into the unary -y; NULL when the
   left side is not a literal 1 */
static const char *seP1rmMinus(seCtx *ctx, D_ParseNode *c, int *found) {
  double v;
  int lNum = seIsNumLit(d_get_child(c, 0), &v, ctx);
  if (ctx->failed) return "";
  if (!lNum || v != 1.0) return NULL;
  *found = 1;
  const char *b = seEmit(ctx, d_get_child(c, 2));
  if (ctx->failed) return "";
  return seCat(ctx, "-", b, NULL, NULL, NULL, NULL);
}

static const char *seP1rmF(seCtx *ctx, D_ParseNode *pn, int *found) {
  D_ParseNode *c = seCore(ctx, pn);
  char op = seBinOp(c);
  if (op == '+') return seP1rmPlus(ctx, d_get_child(c, 0), d_get_child(c, 2), found);
  if (op == '-') {
    const char *m = seP1rmMinus(ctx, c, found);
    if (m != NULL || ctx->failed) return m == NULL ? "" : m;
  }
  return seEmit(ctx, pn);
}

/* .rxM1rmF(): strip a pi factor out of sin/cos/tan's RAW argument */
static const char *seBinaryText(seCtx *ctx, const char *x2, char op,
                                D_ParseNode *rhs);
static const char *seM1rmF(seCtx *ctx, D_ParseNode *pn, int *found);

/* .rxM1rmF() on an operator it does not rewrite: hunt both sides, rejoin */
static const char *seM1rmJoin(seCtx *ctx, D_ParseNode *l, const char *op,
                              D_ParseNode *r, int *found) {
  const char *a = seM1rmF(ctx, l, found);
  if (ctx->failed) return "";
  const char *b = seM1rmF(ctx, r, found);
  if (ctx->failed) return "";
  return seCat(ctx, a, op, b, NULL, NULL, NULL);
}

static const char *seM1rmF(seCtx *ctx, D_ParseNode *pn, int *found) {
  D_ParseNode *c = seCore(ctx, pn);
  char op = seBinOp(c);
  if (op == '*') {
    D_ParseNode *l = d_get_child(c, 0), *r = d_get_child(c, 2);
    if (seIsSym(ctx, r, "pi")) {
      *found = 1;
      return seEmit(ctx, seStripP(ctx, l));
    }
    if (seIsSym(ctx, l, "pi")) {
      *found = 1;
      return seEmit(ctx, seStripP(ctx, r));
    }
    return seM1rmJoin(ctx, l, "*", r, found);
  }
  if (op == '/') {
    D_ParseNode *l = d_get_child(c, 0), *r = d_get_child(c, 2);
    D_ParseNode *lc = seCore(ctx, l);
    int lAtom = seNiIs(lc, symbol) || seNiNumber(lc);
    if (lAtom) {
      if (seIsSym(ctx, l, "pi")) {
        /* x[[2]] <- 1; .rxFromSE(x) */
        *found = 1;
        return seBinaryText(ctx, "1", '/', r);
      }
      return seM1rmJoin(ctx, l, "/", r, found);
    }
    /* a compound numerator: R recurses and parenthesizes it unless it
       already is.  A call numerator (f(x)) has length > 1 too. */
    const char *num = seM1rmF(ctx, l, found);
    if (ctx->failed) return "";
    int paren = d_get_number_of_children(lc) == 3 && seIsLit(d_get_child(lc, 0), '(');
    if (!paren) num = seCat(ctx, "(", num, ")", NULL, NULL, NULL);
    const char *b = seM1rmF(ctx, r, found);
    if (ctx->failed) return "";
    return seCat(ctx, num, "/", b, NULL, NULL, NULL);
  }
  return seEmit(ctx, pn);
}

/* Is r1 one call to exp() of one argument -- exp(a), not exp(a)*b?  Returns
   1 yes, 0 no, -1 exp() of several arguments (R's x[[-1]] raises there). */
static int seIsExpCall(const char *r1) {
  size_t l = strlen(r1), i;
  int depth = 0;
  if (l < 6 || strncmp(r1, "exp(", 4) != 0 || r1[l - 1] != ')') return 0;
  for (i = 4; i < l - 1; i++) {
    if (r1[i] == '(') {
      depth++;
    } else if (r1[i] == ')') {
      /* the "(" after exp closes before the end: exp(a)*b(c) */
      if (--depth < 0) return 0;
    } else if (r1[i] == ',' && depth == 0) {
      return -1;
    }
  }
  return depth == 0;
}

/* log1p(exp(x)) -> log1pexp(x): R reparses the log1p argument it has just
   WRITTEN and, when that is a call to exp(), converts exp's argument again --
   as symengine text.  Reproduce that with a second parse of our own output.
   NULL when r1 is not such a call. */
static const char *seLog1pExp(seCtx *ctx, const char *r1) {
  int is = seIsExpCall(r1);
  if (is == 0) return NULL;
  if (is < 0) return seFailWhy(ctx, SE_WHY_LOG);
  const char *inner = seDup(ctx, r1 + 4, strlen(r1) - 5);
  int top = 0;
  const char *t = seReEmit(ctx, inner, &top);
  if (t == NULL || ctx->failed) return seFailWhy(ctx, SE_WHY_LOG);
  return seCat(ctx, "log1pexp(", t, ")", NULL, NULL, NULL);
}

/* ------------------------------------------------------------ Derivative -- */

/* The end of one chain argument that deparse() writes back unchanged -- a
   name that is not an R reserved word, or a positive integer -- or NULL. */
static const char *seSimpleArgEnd(const char *p) {
  const char *b = p;
  if (rxNameStart(*p) && *p != '_') {
    while (rxNameChar(*p)) p++;
    return seIsRReserved(b, (size_t)(p - b)) ? NULL : p;
  }
  if (*p >= '1' && *p <= '9') {
    while (*p >= '0' && *p <= '9') p++;
    return p;
  }
  return NULL;
}

/* A template result `name(a1, ..., an)` whose arguments are bare names or
   integers, split up the way str2lang() + deparse() would for the next step
   of the chain.  Anything else returns 0 and the chain declines. */
static int seSplitSimpleCall(seCtx *ctx, const char *s, const char **fname,
                             const char **args, int max, int *nargs) {
  const char *p = s, *e;
  int n = 0;
  if (!rxNameStart(*p) || *p == '_') return 0;
  while (rxNameChar(*p)) p++;
  if (*p != '(' || seIsRReserved(s, (size_t)(p - s))) return 0;
  *fname = seDup(ctx, s, (size_t)(p - s));
  p++;
  if (*p == ')') return p[1] == '\0' ? (*nargs = 0, 1) : 0;
  for (;;) {
    e = seSimpleArgEnd(p);
    if (e == NULL || n >= max) return 0;
    args[n++] = seDup(ctx, p, (size_t)(e - p));
    p = e;
    if (*p == ',') { p++; continue; }
    if (*p == ')' && p[1] == '\0') break;
    return 0;
  }
  *nargs = n;
  return 1;
}

/* .errD(): the finite difference .rxFromSE() writes for a derivative it does
   not know, forward or central by unknownDerivatives=.  The step is .rxDelta,
   (.Machine$double.eps)^(1/3), written by as.character() as paste0() does.
   With unknownDerivatives = "error" R raises instead. */
static const char *seErrD(seCtx *ctx, const char *fn, const char **args,
                          int nargs, int with) {
  const char *a1[SE_MAX_ARGS], *a2[SE_MAX_ARGS];
  int i;
  double delta = pow(DBL_EPSILON, 1.0 / 3.0);
  if (ctx->numDer != 1 && ctx->numDer != 2) return seFailWhy(ctx, SE_WHY_NUMDER);
  for (i = 0; i < nargs; i++) { a1[i] = args[i]; a2[i] = args[i]; }
  const char *d = seDblToStr(ctx, delta);
  if (ctx->numDer == 1) {
    a1[with] = seCat(ctx, "(", args[with], ")+", d, NULL, NULL);
  } else {
    const char *h = seDblToStr(ctx, 0.5 * delta);
    a1[with] = seCat(ctx, args[with], "-", h, NULL, NULL, NULL);
    a2[with] = seCat(ctx, args[with], "+", h, NULL, NULL, NULL);
  }
  return seCat(ctx,
               seCat(ctx, "(", fn, "(", seJoin(ctx, a1, nargs, ","), ")-", fn),
               "(", seJoin(ctx, a2, nargs, ","), "))/", d, NULL);
}

/* Derivative() of the zero-derivative families (.rxSEzeroD), and the second
   and higher derivatives of fsign() (its first is locally constant): "0", or
   NULL when the derivative is not one of these. */
static const char *seDerivZero(seCtx *ctx, D_ParseNode **args, int nargs) {
  if (nargs < 2) return NULL;
  D_ParseNode *fnNode = seArgNode(ctx, args[0]);
  if (!seNiIs(fnNode, function_call)) return NULL;
  const char *fname = seNodeText(ctx, d_get_child(fnNode, 0));
  const seFnInfo *fi = seFnInfoOf(fname);
  if (fi != NULL && (fi->flags & SE_F_ZEROD)) return "0";
  if (nargs > 2 && strcmp(fname, "fsign") == 0) return "0";
  return NULL;
}

/* The derivative of fname with respect to argument `with` (0-based) from
   rxode2parseD(): its template, or the finite difference R takes when the
   entry is missing, too short or NULL. */
static const char *seDerivLookup(seCtx *ctx, const char *fname,
                                 const char **args, int nf, int with) {
  int len = seRxdLen(ctx, fname);
  /* not registered: an error, or a finite difference */
  if (len < 0 || len < with + 1) return seErrD(ctx, fname, args, nf, with);
  const seDeriv *row = seFindDeriv(ctx, fname, with + 1);
  /* a registered function that is not a template: R calls it */
  if (row == NULL) return seFailWhy(ctx, SE_WHY_DERIV);
  if (row->kind == 2) return seErrD(ctx, fname, args, nf, with);
  return seFillTemplate(ctx, row->tmpl, args, nf);
}

/* Derivative(f(a1, ..., an), v): the registered derivative.  Also
   Derivative(abs0, v), which reaches .rxFromSE() with a bare name.  NB: no
   abs0 shortcut for the CALL abs0(a): .rxFromSE() guards it with
   length(as.character(x[[2]])) == 1, so the registered dabs(a) wins. */
static const char *seDerivative1(seCtx *ctx, D_ParseNode *fnNode,
                                 const char *var) {
  D_ParseNode *fargs[SE_MAX_ARGS];
  const char *emitted[SE_MAX_ARGS];
  if (!seNiIs(fnNode, function_call)) {
    if (seNiIs(fnNode, symbol) && strcmp(seNodeText(ctx, fnNode), "abs0") == 0) {
      return seCat(ctx, "abs(", var, ")", NULL, NULL, NULL);
    }
    return seFailWhy(ctx, SE_WHY_DERIV);
  }
  int nf = seCallArgs(fnNode, fargs, SE_MAX_ARGS);
  if (nf <= 0) return seFailWhy(ctx, SE_WHY_DERIV);
  if (!seEmitAll(ctx, fargs, nf, 0, emitted)) return "";
  /* more or fewer than one match: .errD(force = TRUE) raises */
  int with = seDerivWhich(emitted, nf, var);
  if (with < 0) return seFailWhy(ctx, SE_WHY_DERIV);
  return seDerivLookup(ctx, seNodeText(ctx, d_get_child(fnNode, 0)),
                       emitted, nf, with);
}

/* Derivative(f(...), v1, ..., vk): the registered chain (rxTBS -> rxTBSd ->
   rxTBSd2), one template per variable, each step reading the function and
   its arguments back out of the previous result.  Anything off the chain is
   .derivMore(), which needs symengine. */
static const char *seDerivChain(seCtx *ctx, D_ParseNode *fnNode,
                                const char **vars, int nvars) {
  D_ParseNode *fargs[SE_MAX_ARGS];
  const char *emitted[SE_MAX_ARGS], *res = NULL;
  int k;
  if (!seNiIs(fnNode, function_call)) return seFailWhy(ctx, SE_WHY_DERIV);
  const char *fname = seNodeText(ctx, d_get_child(fnNode, 0));
  int nf = seCallArgs(fnNode, fargs, SE_MAX_ARGS);
  if (nf < 0) return seFailWhy(ctx, SE_WHY_DERIV);
  if (!seEmitAll(ctx, fargs, nf, 0, emitted)) return "";
  for (k = 0; k < nvars; k++) {
    int with = seDerivWhich(emitted, nf, vars[k]);
    if (with < 0) return seFailWhy(ctx, SE_WHY_DERIV);
    const seDeriv *row = seFindDeriv(ctx, fname, with + 1);
    if (row == NULL || row->kind != 1) return seFailWhy(ctx, SE_WHY_DERIV);
    res = seFillTemplate(ctx, row->tmpl, emitted, nf);
    if (k < nvars - 1 &&
        !seSplitSimpleCall(ctx, res, &fname, emitted, SE_MAX_ARGS, &nf)) {
      return seFailWhy(ctx, SE_WHY_DERIV);
    }
  }
  return res;
}

/* Derivative(f(a1, ..., an), v1, ..., vk) after .ret0 has been converted */
static const char *seDerivative(seCtx *ctx, D_ParseNode **args, int nargs) {
  const char *vars[SE_MAX_ARGS];
  if (nargs < 2 || nargs - 1 > SE_MAX_ARGS) return seFailWhy(ctx, SE_WHY_DERIV);
  const char *zero = seDerivZero(ctx, args, nargs);
  if (zero != NULL) return zero;
  if (!seEmitAll(ctx, args + 1, nargs - 1, 0, vars)) return "";
  D_ParseNode *fnNode = seArgNode(ctx, args[0]);
  if (nargs == 2) return seDerivative1(ctx, fnNode, vars[0]);
  return seDerivChain(ctx, fnNode, vars, nargs - 1);
}

/* ------------------------------------------------------------------ Subs -- */

/* Subs(F, v, w): R converts F, parses that text back, splices w's PARSE TREE
   in for the symbol v and converts the result again.  The splice has no
   parentheses, so it is reproduced at the tree level: F's text is reparsed
   and walked with v standing for w's node (seSub), structure checks
   included. */
static const char *seSubs(seCtx *ctx, D_ParseNode **args, int nargs) {
  if (nargs != 3) return seFailWhy(ctx, SE_WHY_SUBS);
  D_ParseNode *whatN = seCore(ctx, seStripP(ctx, args[1]));
  D_ParseNode *withN = seStripP(ctx, args[2]);
  if (!seNiIs(whatN, symbol)) return seFailWhy(ctx, SE_WHY_SUBS);
  const char *v = seNodeText(ctx, whatN);
  const char *e = seEmit(ctx, args[0]);
  if (ctx->failed) return "";
  int top = 0;
  const char *txt = seReEmitSub(ctx, e, &top, v, withN);
  if (txt == NULL || ctx->failed) return seFailWhy(ctx, SE_WHY_SUBS);
  /* the caller splices this into a product or after a sign, so a sum or a
     leading minus keeps its grouping */
  if (top || txt[0] == '-') return seCat(ctx, "(", txt, ")", NULL, NULL, NULL);
  return txt;
}

/* ------------------------------------------------------------- dispatch -- */

static const char *sePolygamma(seCtx *ctx, D_ParseNode **args, int nargs) {
  const char *a, *b;
  if (nargs != 2) return seFailWhy(ctx, SE_WHY_ARITY);
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

/* one call being converted: its name, what the fixed table says about it,
   its argument nodes, and (once converted) .ret0 */
typedef struct {
  const char *name;
  const seFnInfo *fi;
  int kind;
  D_ParseNode **args;
  int nargs;
  const char **ret0;
} seCall;

/* lag(x)/lag(x, n) and the lead/lag0/lead0 spellings */
static const char *seCallLag(seCtx *ctx, const seCall *c) {
  const char *ab[2];
  if (c->nargs != 1 && c->nargs != 2) return seFailWhy(ctx, SE_WHY_ARITY);
  if (!seEmitAll(ctx, c->args, c->nargs, 0, ab)) return "";
  if (c->nargs == 2) return seCat(ctx, c->name, "(", ab[0], ",", ab[1], ")");
  return seCat(ctx, c->name, "(", ab[0], ")", NULL, NULL);
}

/* .SEsingle: name(1 + x) for .SE1p names first, then the rewrite */
static const char *seCallSingle(seCtx *ctx, const seCall *c) {
  int found = 0;
  if (c->nargs != 1) return seFailWhy(ctx, SE_WHY_ARITY);
  if ((c->fi->flags & SE_F_1P) && seBinOp(seCore(ctx, c->args[0])) == '+') {
    const char *r = seP1rmF(ctx, c->args[0], &found);
    if (ctx->failed) return "";
    if (found) return seCat(ctx, c->fi->p, "(", r, ")", NULL, NULL);
  }
  const char *a = seEmit(ctx, c->args[0]);
  if (ctx->failed) return "";
  return seCat(ctx, c->fi->a, a, c->fi->b, NULL, NULL, NULL);
}

/* 1. the branches .rxFromSE() takes on the name alone, before anything
   else; NULL when the name has none */
static const char *seCallEarly(seCtx *ctx, const seCall *c) {
  const char *ab[2];
  switch (c->kind) {
  case SE_K_LAG:
    return seCallLag(ctx, c);
  case SE_K_DELAY:
    if (c->nargs != 2) return seFailWhy(ctx, SE_WHY_ARITY);
    if (!seEmitAll(ctx, c->args, 2, 0, ab)) return "";
    return seCat(ctx, c->name, "(", ab[0], ", ", ab[1], ")");
  case SE_K_POLYGAMMA:
    return sePolygamma(ctx, c->args, c->nargs);
  case SE_K_SINGLE:
    return seCallSingle(ctx, c);
  case SE_K_DOUBLE:
    if (c->nargs != 2) return seFailWhy(ctx, SE_WHY_ARITY);
    if (!seEmitAll(ctx, c->args, 2, 0, ab)) return "";
    return seCat(ctx, c->fi->a, ab[0], c->fi->b, ab[1], c->fi->c, NULL);
  default:
    return NULL;
  }
}

/* 2. the one-argument forms keyed on the argument: f(NaN) for a dual
   variable/function (R parses NaN as a number; this grammar as a name), and
   log(beta(a, b)) -> lbeta(a, b).  NULL when neither applies. */
static const char *seCallOneArg(seCtx *ctx, const seCall *c) {
  if (c->nargs != 1) return NULL;
  D_ParseNode *a0 = seCore(ctx, c->args[0]);
  if (c->fi != NULL && (c->fi->flags & SE_F_DUAL) && seNiIs(a0, symbol) &&
      strcmp(seNodeText(ctx, a0), "NaN") == 0) {
    return seCat(ctx, c->name, "()", NULL, NULL, NULL, NULL);
  }
  if (strcmp(c->name, "log") != 0 || !seNiIs(a0, function_call) ||
      strcmp(seNodeText(ctx, d_get_child(a0, 0)), "beta") != 0) {
    return NULL;
  }
  D_ParseNode *bargs[SE_MAX_ARGS];
  const char *ab[2];
  if (seCallArgs(a0, bargs, SE_MAX_ARGS) != 2) return NULL;
  if (!seEmitAll(ctx, bargs, 2, 0, ab)) return "";
  return seCat(ctx, "lbeta(", ab[0], ",", ab[1], ")", NULL);
}

/* .SE1p: log(1 + x) -> log1p(x) (or log1pexp), lgamma(1 + x) -> lgamma1p(x);
   with no literal 1, the plain call and its constant peepholes */
static const char *seCall1p(seCtx *ctx, const seCall *c) {
  int found = 0;
  const char *r = seP1rmF(ctx, c->args[0], &found);
  if (ctx->failed) return "";
  if (!found) return seNamedConstant(seCat(ctx, c->name, "(", r, ")", NULL, NULL));
  if (strcmp(c->fi->p, "log1p") == 0) {
    const char *le = seLog1pExp(ctx, r);
    if (ctx->failed) return "";
    if (le != NULL) return le;
  }
  return seCat(ctx, c->fi->p, "(", r, ")", NULL, NULL);
}

/* 4. a .SEeq function: .SE1p, .SE1m, or the generic name(args) */
static const char *seCallSEeq(seCtx *ctx, const seCall *c, int eqN) {
  if (eqN != c->nargs) return seFailWhy(ctx, SE_WHY_ARITY);
  if (c->nargs == 1 && c->fi != NULL && (c->fi->flags & SE_F_1P)) {
    return seCall1p(ctx, c);
  }
  if (c->nargs == 1 && c->fi != NULL && (c->fi->flags & SE_F_1M)) {
    int found = 0;
    const char *r = seM1rmF(ctx, c->args[0], &found);
    if (ctx->failed) return "";
    /* no constant peephole on this path: sin(pi) stays sin(pi) */
    return seCat(ctx, found ? c->fi->p : c->name, "(", r, ")", NULL, NULL);
  }
  return seNamedConstant(seCat(ctx, c->name, "(", seJoin(ctx, c->ret0, c->nargs, ","),
                               ")", NULL, NULL));
}

/* 5. the branches after .SEeq; anything else is an R user function (or an
   error R raises) */
static const char *seCallLate(seCtx *ctx, const seCall *c) {
  switch (c->kind) {
  case SE_K_DERIVATIVE:
    return seDerivative(ctx, c->args, c->nargs);
  case SE_K_SUBS:
    return seSubs(ctx, c->args, c->nargs);
  case SE_K_MAXMIN:
    return seCat(ctx, c->name, "(", seJoin(ctx, c->ret0, c->nargs, ","), ")", NULL, NULL);
  case SE_K_DOSE:
    if (c->nargs == 0) return seCat(ctx, c->name, "()", NULL, NULL, NULL, NULL);
    if (c->nargs == 1) return seCat(ctx, c->name, "(", c->ret0[0], ")", NULL, NULL);
    return seFailWhy(ctx, SE_WHY_ARITY);
  default:
    return seFailWhy(ctx, SE_WHY_UNKNOWN_FN);
  }
}

/* A call, in .rxFromSE()'s order -- the order IS the semantics, since a name
   can qualify for several branches */
static const char *seFunctionCall(seCtx *ctx, D_ParseNode *pn) {
  D_ParseNode *args[SE_MAX_ARGS];
  const char *ret0[SE_MAX_ARGS], *got;
  seCall c;
  c.name = seNodeText(ctx, d_get_child(pn, 0));
  c.fi = seFnInfoOf(c.name);
  c.kind = (c.fi == NULL) ? SE_K_NONE : c.fi->kind;
  c.args = args;
  c.ret0 = ret0;
  c.nargs = seCallArgs(pn, args, SE_MAX_ARGS);
  if (c.nargs < 0) return seFailWhy(ctx, SE_WHY_ARITY);

  if ((got = seCallEarly(ctx, &c)) != NULL) return got;
  if ((got = seCallOneArg(ctx, &c)) != NULL) return got;
  /* Derivative() of the zero-derivative families is 0 whatever the
     arguments are -- even ones the conversion below would refuse, such as
     ceiling(x, y) (test-ceiling.R pins this; the R walker raises there) */
  if (c.kind == SE_K_DERIVATIVE && (got = seDerivZero(ctx, args, c.nargs)) != NULL) {
    return got;
  }
  /* 3. .ret0: every argument converted (paren-stripped) up front, as R
     does, so an argument R cannot convert declines here as R would raise */
  if (!seEmitAll(ctx, args, c.nargs, 1, ret0)) return "";
  /* from here R keys on the CONVERTED name, .ret0[[1]]; it is the name
     itself for every real function, so anything else declines */
  const char *en = seEmitSymbol(ctx, d_get_child(pn, 0));
  if (ctx->failed) return "";
  if (strcmp(en, c.name) != 0) return seFailWhy(ctx, SE_WHY_OTHER);

  int eqN = seEqNargs(ctx, c.name);
  if (eqN >= 0) return seCallSEeq(ctx, &c, eqN);
  return seCallLate(ctx, &c);
}

#endif /* __SE_FROM_SE_CALLS_H__ */
