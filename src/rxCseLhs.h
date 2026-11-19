/*
 * rxCseLhs.h -- render a left-hand side.
 *
 * ..rxOptLhs() (R/rxOptExpr.R:160-202).  Only these heads are accepted; the
 * grammar lets any call through and the decision is made here, the way
 * rxToSE's emitter restricts index_expression to THETA/ETA.  Note `lag`
 * renders as `alag`, and `name(0)` is accepted for any name.
 *
 * `past()` is deliberately NOT handled: its duration goes through a nested
 * .rxOptExpr() so it shares a temporary with the matching delay() call
 * (R/rxOptExpr.R:182-191), and reproducing that coupling is not worth it for a
 * construct this rare -- it declines and the R walker runs.
 */
#ifndef __RX_CSE_LHS_H__
#define __RX_CSE_LHS_H__

#include "rxCseCtx.h"
#include "rxCseNode.h"

static const char *csLhs(csCtx *c, D_ParseNode *pn);

static inline const char *csLhsHead(const char *h) {
  if (!strcmp(h, "dt")) return "dt";
  if (!strcmp(h, "f")) return "f";
  if (!strcmp(h, "F")) return "F";
  if (!strcmp(h, "rate")) return "rate";
  if (!strcmp(h, "alag")) return "alag";
  if (!strcmp(h, "lag")) return "alag";        /* renamed */
  if (!strcmp(h, "dur")) return "dur";
  if (!strcmp(h, "dy")) return "dy";
  if (!strcmp(h, "df")) return "df";
  return NULL;
}

static const char *csLhs(csCtx *c, D_ParseNode *pn) {
  const char *name;
  csNodeInfo ni;
  int nch;
  if (c->arena.failed) return "";
  pn = csUnwrap(pn);
  name = csNodeName(pn);
  nch = d_get_number_of_children(pn);
  csNiReset(&ni);

  if (csNodeHas(identifier) || csNodeHas(symbol)) return csNodeText(csArena(c), pn);

  if (nch == 3 && csIsLit(d_get_child(pn, 1), '/')) {   /* d/dt(x), df(a)/dy(b) */
    return seCat(csArena(c), csLhs(c, d_get_child(pn, 0)), "/",
                 csLhs(c, d_get_child(pn, 2)), NULL, NULL, NULL);
  }
  if (nch == 3 && csIsLit(d_get_child(pn, 0), '(')) {
    return seCat(csArena(c), "(", csLhs(c, d_get_child(pn, 1)), ")",
                 NULL, NULL, NULL);
  }
  if (nch == 4) {                                       /* head ( args ) */
    const char *h = csNodeText(csArena(c), d_get_child(pn, 0));
    const char *mapped = csLhsHead(h);
    D_ParseNode *args[SE_ARGS_MAX_DEPTH];
    int n = seArgsFlattenT(csPt, d_get_child(pn, 2), args, SE_ARGS_MAX_DEPTH);
    if (n != 1) return seFail(csArena(c));              /* incl. past(), 2 args */
    if (mapped != NULL) {
      return seCat(csArena(c), mapped, "(", csLhs(c, args[0]), ")",
                   NULL, NULL);
    }
    /* `name(0)` -- any head, but the argument must be exactly 0 */
    {
      D_ParseNode *a = csUnwrap(args[0]);
      const char *at = csNodeText(csArena(c), a);
      if (!strcmp(at, "0")) return seCat(csArena(c), h, "(0)", NULL, NULL, NULL, NULL);
    }
    return seFail(csArena(c));
  }
  return seFail(csArena(c));
}

#endif /* __RX_CSE_LHS_H__ */
