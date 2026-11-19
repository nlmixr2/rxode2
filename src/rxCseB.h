/*
 * rxCseB.h -- "machine B": fold and render the OUTPUT text.
 *
 * This is ..rxOpt() (R/rxOptExpr.R:204-410).  It runs over machine A's output
 * after R has re-parsed it, and it is where every constant fold and
 * simplification lives.  Machine A does none of them.
 *
 * Two of its rules are bugs that are load bearing for byte-exactness and are
 * reproduced deliberately:
 *   - `x/1` is NOT simplified (there is no RHS-is-1 branch for `/`).
 *   - `x/0` does NOT error.  R/rxOptExpr.R:324 tests identical(x[[2]], quote(`/`))
 *     -- the OPERAND instead of the operator -- so the "cannot divide by zero"
 *     branch is unreachable and `a/0` falls through to the default rendering.
 */
#ifndef __RX_CSE_B_H__
#define __RX_CSE_B_H__

#include "rxCseCtx.h"
#include "rxCseNode.h"
#include "rxCseNum.h"

static const char *csB(csCtx *c, D_ParseNode *pn);

/* an unwrapped `number` node, and its value */
static inline int csBNum(csCtx *c, D_ParseNode *pn, double *v) {
  const char *name;
  csNodeInfo ni;
  pn = csUnwrap(pn);
  name = csNodeName(pn);
  csNiReset(&ni);
  if (csNodeHas(integer_num) || csNodeHas(float_num)) {
    *v = strtod(csNodeText(csArena(c), pn), NULL);
    return 1;
  }
  return 0;
}

/* unary minus applied directly to a literal, i.e. R's `length(x[[2]]) == 2`
   with an atomic inside (R/rxOptExpr.R:262-280) */
static inline int csBNegNum(csCtx *c, D_ParseNode *pn, double *v) {
  D_ParseNode *u = csUnwrap(pn);
  if (d_get_number_of_children(u) != 2) return 0;
  if (!csIsLit(d_get_child(u, 0), '-')) return 0;
  if (!csBNum(c, d_get_child(u, 1), v)) return 0;
  *v = -*v;
  return 1;
}

static inline const char *csBFold(csCtx *c, double a, const char *op, double b) {
  char buf[512];
  double r;
  switch (op[0]) {
  case '+': r = a + b; break;
  case '-': r = a - b; break;
  case '*': r = a * b; break;
  case '/': r = a / b; break;
  default: return NULL;                  /* ^ and the comparisons never fold */
  }
  if (!csNumFormat(buf, sizeof(buf), r)) return NULL;
  return seStr(csArena(c), buf);
}

static inline const char *csBCall(csCtx *c, D_ParseNode *pn) {
  D_ParseNode *args[SE_ARGS_MAX_DEPTH];
  const char *out;
  int n = 0, i, nch = d_get_number_of_children(pn);
  out = seCat(csArena(c), csNodeText(csArena(c), d_get_child(pn, 0)), "(",
              NULL, NULL, NULL, NULL);
  if (nch == 4) {
    n = seArgsFlattenT(csPt, d_get_child(pn, 2), args, SE_ARGS_MAX_DEPTH);
    if (n < 0) return seFail(csArena(c));
    for (i = 0; i < n; i++) {
      const char *a = csB(c, args[i]);
      if (c->arena.failed) return seFail(csArena(c));
      out = seCat(csArena(c), out, i == 0 ? "" : ", ", a, NULL, NULL, NULL);
    }
  }
  return seCat(csArena(c), out, ")", NULL, NULL, NULL, NULL);
}

/* collapse nested parentheses: (((y))) -> (y)  (R/rxOptExpr.R:226-240) */
static const char *csBParen(csCtx *c, D_ParseNode *pn) {
  D_ParseNode *in = csUnwrap(d_get_child(pn, 1));
  while (d_get_number_of_children(in) == 3 && csIsLit(d_get_child(in, 0), '(')) {
    in = csUnwrap(d_get_child(in, 1));
  }
  return seCat(csArena(c), "(", csB(c, in), ")", NULL, NULL, NULL);
}

/* `op` is a one-character operator (`*`, `+`, ...) */
static int csIsOp(const char *op, char ch) { return op[0] == ch && op[1] == '\0'; }

/* the simplifications that apply when the LEFT operand is a literal `va`;
   NULL when none does */
static const char *csBLhsLit(csCtx *c, double va, const char *op, D_ParseNode *r) {
  if (va == 1.0 && csIsOp(op, '*')) return csB(c, r);
  if (va != 0.0) return NULL;
  if (csIsOp(op, '*') || csIsOp(op, '/')) return seStr(csArena(c), "0");
  if (csIsOp(op, '+')) return csB(c, r);
  if (csIsOp(op, '-')) return seCat(csArena(c), "-", csB(c, r), NULL, NULL, NULL, NULL);
  return NULL;
}

/* ... and the RIGHT operand.  Note there is deliberately no `x/1` case, and no
   divide-by-zero error -- see the header. */
static const char *csBRhsLit(csCtx *c, D_ParseNode *l, const char *op, double vb) {
  if (vb == 1.0 && csIsOp(op, '*')) return csB(c, l);
  if (vb != 0.0) return NULL;
  if (csIsOp(op, '*')) return seStr(csArena(c), "0");
  if (csIsOp(op, '+') || csIsOp(op, '-')) return csB(c, l);
  return NULL;
}

static const char *csBBinary(csCtx *c, D_ParseNode *pn) {
  D_ParseNode *l = d_get_child(pn, 0), *r = d_get_child(pn, 2);
  const char *op = csNodeText(csArena(c), csUnwrap(d_get_child(pn, 1)));
  const char *got;
  double va, vb;
  if (op[0] == '%') {                             /* %% never folds */
    return seCat(csArena(c), csModOperand(c, csB(c, l)), "%%",
                 csModOperand(c, csB(c, r)), NULL, NULL, NULL);
  }
  if (op[0] == '*' && op[1] == '*') op = "^";
  /* 1. LHS is unary-minus-on-a-literal */
  if (csBNegNum(c, l, &va)) {
    if (csBNum(c, r, &vb)) {
      got = csBFold(c, va, op, vb);
      if (got != NULL) return got;
    }
    if (va == -1.0 && csIsOp(op, '*')) {                 /* -1*x -> -x */
      return seCat(csArena(c), "-", csB(c, r), NULL, NULL, NULL, NULL);
    }
  }
  /* 2. both literals, then 3. LHS only, then 4. RHS only */
  if (csBNum(c, l, &va)) {
    if (csBNum(c, r, &vb)) {
      got = csBFold(c, va, op, vb);
      if (got != NULL) return got;
    }
    got = csBLhsLit(c, va, op, r);
    if (got != NULL) return got;
  }
  if (csBNum(c, r, &vb)) {
    got = csBRhsLit(c, l, op, vb);
    if (got != NULL) return got;
  }
  /* 5. default: no spaces around the operator */
  return seCat(csArena(c), csB(c, l), op, csB(c, r), NULL, NULL, NULL);
}

static const char *csB(csCtx *c, D_ParseNode *pn) {
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
    if (!csNumFormat(buf, sizeof(buf), strtod(csNodeText(csArena(c), pn), NULL)))
      return seFail(csArena(c));
    return seStr(csArena(c), buf);
  }
  if (csNodeHas(identifier) || csNodeHas(symbol) || csNodeHas(function_name)) {
    return csNodeText(csArena(c), pn);
  }
  if (csNodeHas(index_expression)) {
    return seCat(csArena(c), csB(c, d_get_child(pn, 0)), "[",
                 csB(c, d_get_child(pn, 2)), "]", NULL, NULL);
  }
  if (csNodeHas(function_call)) return csBCall(c, pn);
  if (nch == 3 && csIsLit(d_get_child(pn, 0), '(')) return csBParen(c, pn);
  if (nch == 2) {                                   /* unary: op then operand */
    return seCat(csArena(c), csNodeText(csArena(c), d_get_child(pn, 0)),
                 csB(c, d_get_child(pn, 1)), NULL, NULL, NULL, NULL);
  }
  if (nch == 3) return csBBinary(c, pn);
  return seFail(csArena(c));
}

#endif /* __RX_CSE_B_H__ */
