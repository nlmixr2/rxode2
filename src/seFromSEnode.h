/*
 * seFromSEnode.h -- what kind of parse node is this, and what text does it
 * span?
 *
 * Node kinds are tested with seNodeHas(), the same paradigm as tran.c's
 * nodeHas() (src/tran.h): the production name appears in exactly one place,
 * the SE_PRODUCTIONS list, which generates both the field and the name it is
 * resolved from, so there is nothing to fall out of step with
 * inst/seFromSE.g.  Each name is resolved to its parser-table index once
 * (seSymInit), so a walk that asks several questions of the same node --
 * is it a call? a wrapper? a bare literal? -- pays an integer compare for
 * each, never a strcmp.
 *
 * Literals ('(', ',', '+' ...) are not production names, so they are matched
 * on their first character -- an O(1) read, and a literal cannot be renamed
 * the way a production can.
 *
 * The grammar spells the precedence ladder out (add -> mul -> unary -> power
 * -> primary), so every walk has to see through the productions that only
 * wrap a single child; seNiWrapper() names that once for the fold, the call
 * handler and the emitter.
 */
#ifndef __SE_FROM_SE_NODE_H__
#define __SE_FROM_SE_NODE_H__

#include "seFromSEarena.h"
#include "seParseNode.h"

static D_ParserTables *sePt = &parser_tables_rxode2seFromSE;

/* Every production in inst/seFromSE.g, named ONCE: the list below generates
   both the memo field and its reset.  -1 = not yet asked.
   The resemblance to rtNodeInfo is not a shared abstraction waiting to be
   named: these fields ARE this grammar's productions, because seNodeHas()
   makes the field name and the compared string one token (src/tran.h).
   Sharing the struct would mean neither grammar could gain or drop a
   production without the other. */
#define SE_PRODUCTIONS(X)  \
  X(translation_unit) X(expression) X(add_expression) \
  X(mul_expression) X(unary_expression) X(power_expression) \
  X(primary_expression) X(function_call) X(arg_list) X(function_name) \
  X(symbol) X(number) X(identifier) X(integer_num) X(float_num)

/* The parser-table index of each production, looked up by name ONCE (see
   seParseNode.h).  Asking a node what it is is then an integer compare --
   the nodeHas() idea taken one step further: nodeHas() pays one strcmp per
   production per node, this pays none in the walk at all. */
typedef struct seSymIds {
#define SE_PRODUCTIONS_FIELD(what) int what;
  SE_PRODUCTIONS(SE_PRODUCTIONS_FIELD)
#undef SE_PRODUCTIONS_FIELD
} seSymIds;

static seSymIds seSym;
static int seSymReady = 0;

/* main thread, before any parallel region */
static void seSymInit(void) {
  if (seSymReady) return;
#define SE_PRODUCTIONS_FIND(what) seSym.what = seProdIdT(sePt, #what);
  SE_PRODUCTIONS(SE_PRODUCTIONS_FIND)
#undef SE_PRODUCTIONS_FIND
  seSymReady = 1;
}

/* is this node the production `what`? */
#define seNodeHas(what) (pn->symbol == seSym.what)
#define seNiIs(pn, what) ((pn)->symbol == seSym.what)

/* this grammar's spelling of the shared node readers; see seParseNode.h */
#define seNodeName(pn) seNodeNameT(sePt, (pn))
#define seNodeText(ctx, pn) seNodeTextOf((ctx), (pn))
#define seIsLit(pn, c) seIsLitT(sePt, (pn), (c))

static int seIsPowOp(D_ParseNode *pn) {
  const char *s = seNodeName(pn);
  return s[0] == '^' || (s[0] == '*' && s[1] == '*');
}

static const char *seEmit(seCtx *ctx, D_ParseNode *pn);
static const char *seEmitSymbol(seCtx *ctx, D_ParseNode *pn);
/* Translate rxode2 text the way .rxFromSE(eval(parse(text = txt))) would:
   parse it with this grammar and walk it (seFromSEemit.h) */
static const char *seReEmit(seCtx *ctx, const char *txt, int *topSign);
static const char *seReEmitSub(seCtx *ctx, const char *txt, int *topSign,
                               const char *v, D_ParseNode *w);

/* productions that only wrap a single child and carry no meaning of their own */
static int seNiWrapper(D_ParseNode *pn) {
  /* a call's single argument arrives as its arg_list node, and a whole parse
     as translation_unit: with one child each is just a wrapper too */
  if ((seNodeHas(arg_list) || seNodeHas(translation_unit)) &&
      d_get_number_of_children(pn) == 1) {
    return 1;
  }
  return seNodeHas(expression) || seNodeHas(add_expression) ||
    seNodeHas(mul_expression) || seNodeHas(unary_expression) ||
    seNodeHas(power_expression) || seNodeHas(primary_expression);
}

/* a numeric literal node ("number" wraps "integer_num"/"float_num") */
static int seNiNumber(D_ParseNode *pn) {
  return seNodeHas(number) || seNodeHas(integer_num) || seNodeHas(float_num);
}

static int seNiArgList(D_ParseNode *pn) {
  return seNodeHas(arg_list);
}

/* Does this exponent subtree reduce to a bare numeric literal?  This is the
   is.numeric(x[[3]]) test in .rxFromSE(): TRUE only for a literal, so `d^2`
   becomes Rx_pow_di(d,2) while `a^(-2)` (a call to unary minus) does not and
   falls through to "a^-2".  Reproducing that asymmetry matters -- the fixture
   pins it. */
/* The active Subs() entry whose v is this symbol node, or NULL.  Compares
   the node's source span in place: no allocation, so it is cheap enough to
   ask of every symbol the structure checks look at. */
/* Does entry s stand for the symbol spelled b[0..n), read from the text s
   was pushed for?  (Symbols of any other buffer -- w's own -- never are.) */
static int seSubMatches(const seSub *s, const char *b, size_t n) {
  if (!s->active || b < s->buf || b >= s->buf + s->len) return 0;
  return strlen(s->v) == n && strncmp(s->v, b, n) == 0;
}

static seSub *seSubFind(seCtx *ctx, D_ParseNode *pn) {
  int i;
  const char *b;
  if (ctx->nsubs == 0 || !seNiIs(pn, symbol)) return NULL;
  size_t n = seNodeSpan(pn, &b);
  for (i = ctx->nsubs - 1; i >= 0; i--) {
    if (seSubMatches(&ctx->subs[i], b, n)) return &ctx->subs[i];
  }
  return NULL;
}

/* The node a substituted symbol stands for, following each Subs() entry at
   most once per walk (`used` is that walk's record) -- Subs(x, x, x) replaces
   x with x, and following it again would never end.  NULL when pn is not a
   substituted symbol (any more). */
static D_ParseNode *seSubFollow(seCtx *ctx, D_ParseNode *pn, unsigned int *used) {
  seSub *s = seSubFind(ctx, pn);
  if (s == NULL) return NULL;
  unsigned int bit = 1u << (unsigned int)(s - ctx->subs);
  if (*used & bit) return NULL;
  *used |= bit;
  return s->w;
}

static int seIsBareNumber(seCtx *ctx, D_ParseNode *pn) {
  unsigned int used = 0;
  for (;;) {
    if (seNiNumber(pn)) return 1;
    if (d_get_number_of_children(pn) == 1 && seNiWrapper(pn)) {
      pn = d_get_child(pn, 0);
      continue;
    }
    /* a substituted symbol is whatever replaces it */
    D_ParseNode *w = seSubFollow(ctx, pn, &used);
    if (w != NULL) {
      pn = w;
      continue;
    }
    return 0;
  }
}

#endif /* __SE_FROM_SE_NODE_H__ */
