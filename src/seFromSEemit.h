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

static const char *seBinary(seCtx *ctx, D_ParseNode *pn) {
  const char *x2 = seEmit(ctx, d_get_child(pn, 0));
  if (ctx->failed) return "";
  D_ParseNode *rhs = d_get_child(pn, 2);
  const char *op = seNodeName(d_get_child(pn, 1));
  const char *x3 = seEmit(ctx, rhs);
  if (ctx->failed) return "";

  double fv;
  seFoldRes fr = seFold(rhs, &fv);
  if (fr == SE_FOLD_BAIL) return seFail(ctx);
  if (fr == SE_FOLD_YES) x3 = seNumToStr(ctx, fv);

  int isPow = (op[0] == '^') || (op[0] == '*' && op[1] == '*');
  if (isPow) {
    const char *pw = seEmitPower(ctx, x2, x3, rhs);
    if (pw != NULL) return pw;
  }
  return seNamedConstant(seCat(ctx, x2, isPow ? "^" : op, x3, NULL, NULL, NULL));
}

#include "seFromSEcalls.h"

/* one symengine symbol as rxode2 text: constant table, then the reserved
   names, then the mangling chain */
static const char *seEmitSymbol(seCtx *ctx, D_ParseNode *pn) {
  const char *raw = seNodeText(ctx, pn);
  int i;
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
  const char *nm = seNodeName(pn);
  int nch = d_get_number_of_children(pn);

  if (strcmp(nm, "symbol") == 0) return seEmitSymbol(ctx, pn);
  if (seIsNumberNode(nm)) {
    /* do NOT recurse: a terminal's only child is named after its regex.
       Round-trip through a double so the rendering matches what R's parser
       plus as.character() produce (see seDblToStr). */
    return seNumToStr(ctx, atof(seNodeText(ctx, pn)));
  }
  if (strcmp(nm, "function_call") == 0) return seFunctionCall(ctx, pn);
  if (nch == 1) return seEmit(ctx, d_get_child(pn, 0));
  if (nch == 2 && strcmp(nm, "unary_expression") == 0) {
    const char *inner = seEmit(ctx, d_get_child(pn, 1));
    if (ctx->failed) return "";
    return seCat(ctx, seNodeName(d_get_child(pn, 0)), inner,
                 NULL, NULL, NULL, NULL);
  }
  if (nch == 3) {
    if (strcmp(seNodeName(d_get_child(pn, 0)), "(") == 0) {
      return seEmitParen(ctx, pn);
    }
    return seBinary(ctx, pn);
  }
  return seFail(ctx);
}

#endif /* __SE_FROM_SE_EMIT_H__ */
