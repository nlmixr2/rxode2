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
  if (!seIsBareNumber(ctx, rhs)) return NULL;          /* caller joins with '^' */
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

/* `x2 op rhs` once the left operand is text: the right operand is emitted,
   folded, and the identities/power rules/peepholes applied.  Split out of
   seBinary() because .rxM1rmF() rewrites pi/x to 1/x and converts THAT. */
static const char *seBinaryText(seCtx *ctx, const char *x2, char op,
                                D_ParseNode *rhs) {
  const char *x3 = seEmit(ctx, rhs);
  if (ctx->failed) return "";

  /* the fold is asked of the EMITTED text, exactly as .rxFromSE() asks it */
  double fv;
  seFoldRes fr = seFoldStr(ctx, x3, &fv);
  if (fr == SE_FOLD_BAIL) return seFailWhy(ctx, SE_WHY_FOLD);
  if (fr == SE_FOLD_YES) x3 = seNumToStr(ctx, fv);

  int isPow = (op == '^');
  if (isPow) {
    const char *pw = seEmitPower(ctx, x2, x3, rhs);
    if (pw != NULL) return pw;
  } else {
    const char *id = seBinIdentity(x2, op, x3);
    if (id != NULL) return id;
  }
  char ops[2] = {op, '\0'};
  const char *ret = seNamedConstant(seCat(ctx, x2, ops, x3, NULL, NULL, NULL));
  /* The right operand was folded above; do the same for the whole expression
     so a fully constant one collapses to its value.  AFTER the named-constant
     lookup, so pi*2 stays M_2PI rather than becoming 6.28... */
  double wv;
  seFoldRes wr = seFoldStr(ctx, ret, &wv);
  if (wr == SE_FOLD_BAIL) return seFailWhy(ctx, SE_WHY_FOLD);
  if (wr == SE_FOLD_YES) return seNumToStr(ctx, wv);
  return ret;
}

static const char *seBinary(seCtx *ctx, D_ParseNode *pn) {
  const char *x2 = seEmit(ctx, d_get_child(pn, 0));
  if (ctx->failed) return "";
  D_ParseNode *opNode = d_get_child(pn, 1);
  /* symengine may print '**'; rxode2 always emits '^' */
  char op = seIsPowOp(opNode) ? '^' : seNodeName(opNode)[0];
  return seBinaryText(ctx, x2, op, d_get_child(pn, 2));
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
  int nch = d_get_number_of_children(pn);

  if (seNodeHas(symbol)) {
    seSub *s = seSubFind(ctx, pn);
    if (s != NULL) {
      /* Subs(): the symbol is w's tree; R does not substitute inside w */
      s->active = 0;
      const char *r = seEmit(ctx, s->w);
      s->active = 1;
      return r;
    }
    return seEmitSymbol(ctx, pn);
  }
  if (seNodeHas(number) || seNodeHas(integer_num) || seNodeHas(float_num)) {
    /* do NOT recurse: a terminal's only child is named after its regex.
       Round-trip through a double so the rendering matches what R's parser
       plus as.character() produce (see seDblToStr). */
    return seNumToStr(ctx, R_strtod(seNodeText(ctx, pn), NULL));
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

/* Translate rxode2 text the way .rxFromSE(eval(parse(text = txt))) would --
   R's Subs and log1pexp handling convert text they have already written.  A
   second parse into the same arena; NULL declines.  *topSign is whether R
   would see a call to + or - at the top (the Subs grouping rule). */
/* Would R see a call to + or - at the top of this tree (binary, or a sign)?
   The Subs grouping rule asks it of the substituted result. */
static int seTopSign(seCtx *ctx, D_ParseNode *pn) {
  D_ParseNode *top = seCore(ctx, pn);
  char op = seBinOp(top);
  if (op == '+' || op == '-') return 1;
  if (d_get_number_of_children(top) == 2 && seNiIs(top, unary_expression)) {
    char c = seNodeName(d_get_child(top, 0))[0];
    return c == '-' || c == '+';
  }
  return 0;
}

/* make v stand for w while text is walked (seSubFind); 0 when too deep */
static int seSubPush(seCtx *ctx, const char *v, D_ParseNode *w) {
  if (v == NULL) return 1;
  if (ctx->nsubs >= SE_MAX_SUBS) return 0;
  ctx->subs[ctx->nsubs].v = v;
  ctx->subs[ctx->nsubs].w = w;
  ctx->subs[ctx->nsubs].active = 1;
  ctx->nsubs++;
  return 1;
}

static const char *seReEmitSub(seCtx *ctx, const char *txt, int *topSign,
                               const char *v, D_ParseNode *w) {
  const char *out = NULL;
  if (!seSubPush(ctx, v, w)) return NULL;
  D_Parser *p = new_D_Parser(sePt, sizeof(D_ParseNode_User));
  if (p != NULL) {
    p->save_parse_tree = 1;
    p->error_recovery = 0;
    D_ParseNode *pn = dparse(p, (char*) txt, (int) strlen(txt));
    if (pn != NULL && p->syntax_errors == 0) {
      *topSign = seTopSign(ctx, pn);
      out = seEmit(ctx, pn);
      if (ctx->failed) out = NULL;
    }
    if (pn != NULL) free_D_ParseNode(p, pn);
    free_D_Parser(p);
  }
  if (v != NULL) ctx->nsubs--;
  return out;
}

static const char *seReEmit(seCtx *ctx, const char *txt, int *topSign) {
  return seReEmitSub(ctx, txt, topSign, NULL, NULL);
}

#endif /* __SE_FROM_SE_EMIT_H__ */
