/*
 * rxCseA.h -- "machine A": render every node to its CANDIDATE KEY, and either
 * count it (pass 1) or replace it (pass 2).
 *
 * This is the eval DSL of R/rxOptExpr.R:1-151, where each operator closure
 * receives its children already rendered as strings, builds its own text, and
 * hands it to .addExpr().  It is NOT the renderer that produces the output --
 * that is machine B (rxCseB.h), which sees machine A's text re-parsed.  The two
 * do not agree: `1*x` is COUNTED under the key "1*x" but RENDERED as `x`.
 * Collapsing them would be byte-exactly wrong.
 *
 * Machine A never folds a constant.  It only declines to COUNT a node whose
 * operands are all numeric (R/rxOptExpr.R:50-58).
 */
#ifndef __RX_CSE_A_H__
#define __RX_CSE_A_H__

#include "rxCseCtx.h"
#include "rxCseNode.h"
#include "rxCseNum.h"

/* In `rep`, the map's `count` field carries the candidate INDEX and the name
   itself lives in ctx->repNames[i] -- one map type serves both passes. */
static inline void csUsed(csCtx *c, const char *nm) {
  int i;
  for (i = 0; i < c->nused; i++) if (c->used[i] == nm) return;
  if (c->nused >= c->usedCap) return;      /* caller sized this generously */
  c->used[c->nused++] = nm;
}

/* .addExpr (R/rxOptExpr.R:1-19).
   The position counter lives HERE, not at the top of the walk: R's closures are
   evaluated bottom up, so .addExpr() is reached in POST-order, and `firstSeen`
   has to encode that same order or the nchar tie-break -- which keeps
   insertion order, order() being stable -- names two equal-length candidates
   the other way round. */
static inline const char *csAdd(csCtx *c, const char *t) {
  if (t == NULL) return seFail(csArena(c));
  c->pos++;
  if (c->rep != NULL) {
    csEntry *e = csMapGet(c->rep, t);
    if (e != NULL) {
      const char *nm = c->repNames[e->count];
      csUsed(c, nm);
      return nm;
    }
    return t;
  }
  if (!csMapAdd(c->count, t, 1, CS_FIRST_SEEN(c->stmt, c->pos))) {
    return seFail(csArena(c));
  }
  return t;
}

static const char *csA(csCtx *c, D_ParseNode *pn);
extern int csTraceOn(void);
#define CS_FAIL(c, why) (csTraceOn() ? (void)REprintf("    A-fail: %s\n", why) : (void)0, seFail(csArena(c)))

/* `f(a, b)` -- ", " between arguments (R/rxOptExpr.R:24) */
static inline const char *csACall(csCtx *c, D_ParseNode *pn) {
  D_ParseNode *args[SE_ARGS_MAX_DEPTH];
  const char *out;
  int n = 0, i, nch = d_get_number_of_children(pn);
  out = seCat(csArena(c), csNodeText(csArena(c), d_get_child(pn, 0)), "(",
              NULL, NULL, NULL, NULL);
  if (nch == 4) {                                   /* name ( arg_list ) */
    n = seArgsFlattenT(csPt, d_get_child(pn, 2), args, SE_ARGS_MAX_DEPTH);
    if (n < 0) return CS_FAIL(c, "arg flatten");
    for (i = 0; i < n; i++) {
      const char *a = csA(c, args[i]);
      if (c->arena.failed) return seFail(csArena(c));
      out = seCat(csArena(c), out, i == 0 ? "" : ", ", a, NULL, NULL, NULL);
    }
  }
  out = seCat(csArena(c), out, ")", NULL, NULL, NULL, NULL);
  return csAdd(c, out);
}

/* `THETA[1]` / `ETA[2]` -- not a candidate, and any other name declines
   (R/rxOptExpr.R:118-130) */
static inline const char *csAIndex(csCtx *c, D_ParseNode *pn) {
  const char *nm = csNodeText(csArena(c), d_get_child(pn, 0));
  const char *ix = csNodeText(csArena(c), d_get_child(pn, 2));
  const char *p;
  if (strcmp(nm, "THETA") != 0 && strcmp(nm, "ETA") != 0) return CS_FAIL(c, "index name");
  for (p = ix; *p != '\0'; p++) if (*p < '0' || *p > '9') return CS_FAIL(c, "index digits");
  if (ix[0] == '0') return CS_FAIL(c, "index zero");   /* val > 0 */
  return seCat(csArena(c), nm, "[", ix, "]", NULL, NULL);
}

static const char *csA(csCtx *c, D_ParseNode *pn) {
  const char *name;
  csNodeInfo ni;
  int nch;
  if (c->arena.failed) return "";
  pn = csUnwrap(pn);
  name = csNodeName(pn);
  nch = d_get_number_of_children(pn);
  csNiReset(&ni);

  if (csNodeHas(integer_num) || csNodeHas(float_num)) {
    char buf[512];
    const char *raw = csNodeText(csArena(c), pn);
    /* the literal is re-parsed by R before it is rendered, so `1.0` becomes
       `1` and `1e6` becomes `1e+06`; see rxCseNum.h */
    if (!csNumFormat(buf, sizeof(buf), strtod(raw, NULL))) return CS_FAIL(c, "number");
    return seStr(csArena(c), buf);
  }
  if (csNodeHas(identifier) || csNodeHas(symbol) || csNodeHas(function_name)) {
    return csNodeText(csArena(c), pn);
  }
  if (csNodeHas(index_expression)) return csAIndex(c, pn);
  if (csNodeHas(function_call)) return csACall(c, pn);

  if (nch == 3 && csIsLit(d_get_child(pn, 0), '(')) {   /* ( expression ) */
    const char *in = csA(c, d_get_child(pn, 1));
    if (c->arena.failed) return "";
    return seCat(csArena(c), "(", in, ")", NULL, NULL, NULL);
  }

  if (nch == 2) {                                       /* unary */
    const char *op = csNodeText(csArena(c), d_get_child(pn, 0));
    const char *e1 = csA(c, d_get_child(pn, 1));
    const char *out;
    if (c->arena.failed) return "";
    if (op[0] == '!') {                                 /* !x -> "!(x)" via .rxOptFn */
      out = seCat(csArena(c), "!(", e1, ")", NULL, NULL, NULL);
      return csAdd(c, out);
    }
    if (op[0] == '+') out = e1;                         /* unary + is dropped */
    else out = seCat(csArena(c), "-", e1, NULL, NULL, NULL, NULL);
    if (csIsNum(out)) return out;                       /* a bare number is not counted */
    return csAdd(c, out);
  }

  if (nch == 3) {                                       /* binary */
    const char *op = csNodeText(csArena(c), csUnwrap(d_get_child(pn, 1)));
    const char *e1, *e2, *out;
    long p;
    /* Operand ORDER matters, because it is the order .addExpr() sees and so the
       first-encounter tie-break for two candidates of equal nchar.  R's
       .rxOptBin is lazy: for `^` the very first thing it touches is
       as.numeric(e2) (R/rxOptExpr.R:45), which forces the RIGHT operand before
       the left; every other operator forces e1 first, in the `&&` at :50.  */
    if (op[0] == '^' || (op[0] == '*' && op[1] == '*')) {
      e2 = csA(c, d_get_child(pn, 2));
      if (c->arena.failed) return "";
      e1 = csA(c, d_get_child(pn, 0));
      if (c->arena.failed) return "";
    } else {
      e1 = csA(c, d_get_child(pn, 0));
      if (c->arena.failed) return "";
      e2 = csA(c, d_get_child(pn, 2));
      if (c->arena.failed) return "";
    }
    if (op[0] == '%') {                                 /* %% is never folded */
      out = seCat(csArena(c), csModOperand(c, e1), "%%", csModOperand(c, e2),
                  NULL, NULL, NULL);
      if (csIsNum(e1) && csIsNum(e2)) return out;
      return csAdd(c, out);
    }
    if ((op[0] == '^' || (op[0] == '*' && op[1] == '*')) && csIntPow(e2, &p)) {
      /* `e1^n` for integerish n >= 2 becomes ((e1)*(e1)*...) BEFORE counting,
         so the expanded product is the candidate (R/rxOptExpr.R:44-48) */
      long i;
      out = seCat(csArena(c), "((", e1, ")", NULL, NULL, NULL);
      for (i = 1; i < p; i++) out = seCat(csArena(c), out, "*(", e1, ")", NULL, NULL);
      out = seCat(csArena(c), out, ")", NULL, NULL, NULL, NULL);
      return csAdd(c, out);
    }
    if (op[0] == '*' && op[1] == '*') op = "^";         /* R parses ** as ^ */
    out = seCat(csArena(c), e1, op, e2, NULL, NULL, NULL);
    if (csIsNum(e1) && csIsNum(e2)) return out;         /* constants stay inline */
    return csAdd(c, out);
  }

  if (csTraceOn()) REprintf("    A-fail: unhandled node=%s nch=%d\n", name, nch);
  return seFail(csArena(c));
}

#endif /* __RX_CSE_A_H__ */
