/*
 * seFromSEfold.h -- would R have constant-folded this operand?
 *
 * .rxFromSE() runs try(eval(parse(text=.x3), envir=baseenv())) on every right
 * operand it emits and, when that yields a number, re-renders it.  That is not
 * the same question as "is this expression constant": an ordinary model symbol
 * is unbound in baseenv() so nothing folds, but `pi` IS bound there and
 * `sqrt(2)` does evaluate.  So the answer has three values, and the third one
 * -- BAIL -- means hand the whole expression to the R walker rather than guess.
 *
 * Included by seFromSE.c after the node classifiers it uses.
 */
#ifndef __SE_FROM_SE_FOLD_H__
#define __SE_FROM_SE_FOLD_H__

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

/* leaf classification: what would R's baseenv() eval make of this node alone? */
static seFoldRes seFoldLeaf(D_ParseNode *pn, double *out) {
  const char *nm = seNodeName(pn);
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
  return SE_FOLD_NO;   /* not a leaf; caller keeps walking */
}

/* combine two folded operands under one arithmetic operator */
static seFoldRes seFoldBinary(char op, seFoldRes ra, double a,
                              seFoldRes rb, double b, double *out) {
  if (ra == SE_FOLD_BAIL || rb == SE_FOLD_BAIL) return SE_FOLD_BAIL;
  if (ra != SE_FOLD_YES || rb != SE_FOLD_YES) return SE_FOLD_NO;
  switch (op) {
  case '+': *out = a + b; return SE_FOLD_YES;
  case '-': *out = a - b; return SE_FOLD_YES;
  case '*': *out = a * b; return SE_FOLD_YES;
  case '/': *out = a / b; return SE_FOLD_YES;
  default:  return SE_FOLD_BAIL;   /* '^' -- R's ^ vs C pow edge cases */
  }
}

/* the three-child shapes: a parenthesised expression, an argument list, or a
   binary operator */
static seFoldRes seFoldTernary(D_ParseNode *pn, double *out) {
  /* '(' expression ')' -- the paren token is child 0, a binary node's
     operator is child 1 */
  if (strcmp(seNodeName(d_get_child(pn, 0)), "(") == 0) {
    return seFold(d_get_child(pn, 1), out);
  }
  const char *mid = seNodeName(d_get_child(pn, 1));
  double a, b;
  seFoldRes ra = seFold(d_get_child(pn, 0), &a);
  seFoldRes rb = seFold(d_get_child(pn, 2), &b);
  if (mid[0] == ',') {                      /* arg_list ',' expression */
    if (ra == SE_FOLD_YES && rb == SE_FOLD_YES) { *out = b; return SE_FOLD_YES; }
    return SE_FOLD_NO;
  }
  return seFoldBinary(mid[0], ra, a, rb, b, out);
}

static seFoldRes seFold(D_ParseNode *pn, double *out) {
  const char *nm = seNodeName(pn);
  int nch = d_get_number_of_children(pn);

  if (seIsLeafNode(nm) && strcmp(nm, "number") != 0) return seFoldLeaf(pn, out);
  if (nch == 1) return seFold(d_get_child(pn, 0), out);

  if (nch == 2 && strcmp(nm, "unary_expression") == 0) {
    double v;
    seFoldRes r = seFold(d_get_child(pn, 1), &v);
    if (r != SE_FOLD_YES) return r;
    *out = (seNodeName(d_get_child(pn, 0))[0] == '-') ? -v : v;
    return SE_FOLD_YES;
  }

  if (nch == 3) return seFoldTernary(pn, out);
  return SE_FOLD_NO;
}


#endif /* __SE_FROM_SE_FOLD_H__ */
