/*
 * seFromSEemit.h -- walk a symengine parse tree and emit rxode2/C text.
 *
 * The output must match .rxFromSE() BYTE FOR BYTE.  Anything this cannot
 * reproduce sets ctx->failed and the R shim falls back to the R walker;
 * falling back is always safe, guessing is not.
 */
#ifndef __SE_FROM_SE_EMIT_H__
#define __SE_FROM_SE_EMIT_H__

#include "seFromSEnode.h"
#include "seFromSEnames.h"
#include "seFromSEfold.h"
#include "seFromSEcalls.h"

static const char *seEmitParen(seCtx *ctx, D_ParseNode *pn) {
  return seCat(ctx, "(", seEmit(ctx, d_get_child(pn, 1)), ")", NULL, NULL, NULL);
}

/* How `a ^ b` is spelled in C.  .rxFromSE() decides this from the EXPONENT's
   shape, not its value: a literal 2 becomes Rx_pow_di but a call to unary
   minus does not, which is why d^(-1) is (1/(d)) while a^(-2) stays a^-2. */
static const char *seEmitPower(seCtx *ctx, const char *x2, const char *x3,
                               D_ParseNode *rhs) {
  if (strcmp(x3, "1") == 0) return x2;
  if (strcmp(x3, "-1") == 0) return seCat(ctx, "(1/(", x2, "))", NULL, NULL, NULL);
  if (!seIsBareNumber(rhs)) return NULL;          /* caller joins with '^' */
  double d = atof(x3);
  if (d == floor(d)) return seCat(ctx, "Rx_pow_di(", x2, ",", x3, ")", NULL);
  if (strcmp(x3, "0.5") == 0) {
    if (strcmp(x2, "pi") == 0 || strcmp(x2, "M_PI") == 0) return "M_SQRT_PI";
    if (strcmp(x2, "M_2_PI") == 0 || strcmp(x2, "(M_2_PI)") == 0) return "M_SQRT_2dPI";
    return seCat(ctx, "sqrt(", x2, ")", NULL, NULL, NULL);
  }
  return seCat(ctx, "Rx_pow(", x2, ",", x3, ")", NULL);
}

/* Arithmetic identities, mirroring .rxFromSE().  Only the right operand is
   folded, which is why divide and subtract test only that side: 0-x is -x,
   not x, and 1/x is not x.  NULL when none applies. */
static const char *seBinIdentity(const char *x2, char op, const char *x3) {
  if ((op == '/' || op == '*') && !strcmp(x3, "1")) return x2;
  if (op == '*' && !strcmp(x2, "1")) return x3;
  if (op == '+' && !strcmp(x3, "0")) return x2;
  if (op == '+' && !strcmp(x2, "0")) return x3;
  if (op == '-' && !strcmp(x3, "0")) return x2;
  return NULL;
}

static const char *seBinary(seCtx *ctx, D_ParseNode *pn) {
  const char *x2 = seEmit(ctx, d_get_child(pn, 0));
  if (ctx->failed) return "";
  D_ParseNode *rhs = d_get_child(pn, 2);
  D_ParseNode *opNode = d_get_child(pn, 1);
  const char *x3 = seEmit(ctx, rhs);
  if (ctx->failed) return "";

  /* the fold is asked of the EMITTED text, exactly as .rxFromSE() asks it */
  double fv;
  seFoldRes fr = seFoldStr(x3, &fv);
  if (fr == SE_FOLD_BAIL) return seFail(ctx);
  if (fr == SE_FOLD_YES) x3 = seNumToStr(ctx, fv);

  int isPow = seIsPowOp(opNode);
  if (isPow) {
    const char *pw = seEmitPower(ctx, x2, x3, rhs);
    if (pw != NULL) return pw;
  }
  if (!isPow) {
    const char *id = seBinIdentity(x2, seNodeName(opNode)[0], x3);
    if (id != NULL) return id;
  }
  /* symengine may print '**'; rxode2 always emits '^' */
  const char *ret = seNamedConstant(seCat(ctx, x2, isPow ? "^" : seNodeName(opNode),
                                          x3, NULL, NULL, NULL));
  /* The right operand was folded above; do the same for the whole expression
     so a fully constant one collapses to its value.  AFTER the named-constant
     lookup, so pi*2 stays M_2PI rather than becoming 6.28... */
  double wv;
  seFoldRes wr = seFoldStr(ret, &wv);
  if (wr == SE_FOLD_BAIL) return seFail(ctx);
  if (wr == SE_FOLD_YES) return seNumToStr(ctx, wv);
  return ret;
}

#include "seFromSEcalls.h"

/* one symengine symbol as rxode2 text: constant table, then the reserved
   names, then the mangling chain */
static const char *seEmitSymbol(seCtx *ctx, D_ParseNode *pn) {
  const char *raw = seNodeText(ctx, pn);
  const char *lit;
  int i;
  /* .rxRepRxQ() in .rxFromSE(): an encoded character literal becomes a quoted
     string.  Checked first -- rxQ__<esc>__rxQ can be neither a reserved name
     nor a number, and the mangling chain below would leave it untouched */
  lit = seRepRxQ(ctx, raw);
  if (lit != NULL) return lit;
  /* .cnst in .rxFromSE(): rx_SymPy_Res_<name> unshadows a reserved name */
  if (strncmp(raw, "rx_SymPy_Res_", 13) == 0) {
    for (i = 0; i < seNres; i++) {
      if (strcmp(raw + 13, seRes[i].name) == 0) return seRes[i].name;
    }
  }
  /* .rxSEreserved: numeric in R, emitted with sprintf("%.16f", .).  The later
     `if (.ret == "E") return("M_E")` in .rxFromSE() is dead code -- "E" is in
     .rxSEreserved and returns its numeric rendering first. */
  for (i = 0; i < seNres; i++) {
    if (strcmp(raw, seRes[i].name) == 0) {
      if (seRes[i].val == NULL) break;      /* I: complex, falls through */
      return seRes[i].val;
    }
  }
  return seUnRes(ctx, seDemangle(ctx, seFromSEnum(ctx, raw)));
}

static const char *seEmit(seCtx *ctx, D_ParseNode *pn) {
  if (ctx->failed) return "";
  const char *name = seNodeName(pn);
  seNodeInfo ni;
  int nch = d_get_number_of_children(pn);
  seNiReset(&ni);

  if (seNodeHas(symbol)) return seEmitSymbol(ctx, pn);
  if (seNodeHas(number) || seNodeHas(integer_num) || seNodeHas(float_num)) {
    /* do NOT recurse: a terminal's only child is named after its regex.
       Round-trip through a double so the rendering matches what R's parser
       plus as.character() produce (see seDblToStr). */
    return seNumToStr(ctx, atof(seNodeText(ctx, pn)));
  }
  if (seNodeHas(function_call)) return seFunctionCall(ctx, pn);
  if (nch == 1) return seEmit(ctx, d_get_child(pn, 0));
  if (nch == 2 && seNodeHas(unary_expression)) {
    const char *inner = seEmit(ctx, d_get_child(pn, 1));
    if (ctx->failed) return "";
    return seCat(ctx, seNodeName(d_get_child(pn, 0)), inner,
                 NULL, NULL, NULL, NULL);
  }
  if (nch == 3) {
    if (seIsLit(d_get_child(pn, 0), '(')) {
      return seEmitParen(ctx, pn);
    }
    return seBinary(ctx, pn);
  }
  return seFail(ctx);
}

#endif /* __SE_FROM_SE_EMIT_H__ */
