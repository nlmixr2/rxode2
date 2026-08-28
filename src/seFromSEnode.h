/*
 * seFromSEnode.h -- what kind of parse node is this, and what text does it
 * span?
 *
 * Node kinds are tested with seNodeHas(), the same paradigm as tran.c's
 * nodeHas() (src/tran.h).  A walk asks several questions about the same node
 * -- is it a call? a wrapper? a bare literal? -- and seNodeHas() answers each
 * with at most ONE strcmp per node, caching the result in a seNodeInfo whose
 * fields are the production names themselves.
 *
 * The important property is not the caching, it is that the production name
 * appears in exactly one place: STRINGIFY() turns the token in
 * seNodeHas(power_expression) into both the field it memoizes and the string
 * it compares.  There is no second table mapping names to anything, so there
 * is nothing to fall out of step with inst/seFromSE.g.
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

/* One field per production in inst/seFromSE.g; -1 = not yet asked.
   The resemblance to rtNodeInfo is not a shared abstraction waiting to be
   named: these fields ARE this grammar's productions, because seNodeHas()
   makes the field name and the compared string one token (src/tran.h).
   Sharing the struct would mean neither grammar could gain or drop a
   production without the other. */
typedef struct seNodeInfo {
  int translation_unit;
  int expression;
  int add_expression;
  int mul_expression;
  int unary_expression;
  int power_expression;
  int primary_expression;
  int function_call;
  int arg_list;
  int function_name;
  int symbol;
  int number;
  int identifier;
  int integer_num;
  int float_num;
} seNodeInfo;

static inline void seNiReset(seNodeInfo *ni) {
  ni->translation_unit = -1;
  ni->expression = -1;
  ni->add_expression = -1;
  ni->mul_expression = -1;
  ni->unary_expression = -1;
  ni->power_expression = -1;
  ni->primary_expression = -1;
  ni->function_call = -1;
  ni->arg_list = -1;
  ni->function_name = -1;
  ni->symbol = -1;
  ni->number = -1;
  ni->identifier = -1;
  ni->integer_num = -1;
  ni->float_num = -1;
}

#define seSTRINGIFY(...) seSTRINGIFY_AUX(__VA_ARGS__)
#define seSTRINGIFY_AUX(...) #__VA_ARGS__
#define seNIB(what) ni.what
#define seNodeHas(what)                                                 \
  (seNIB(what) == -1 ? (seNIB(what) = !strcmp(seSTRINGIFY(what), name)) \
   : seNIB(what))

/* this grammar's spelling of the shared node readers; see seParseNode.h */
#define seNodeName(pn) seNodeNameT(sePt, (pn))
#define seNodeText(ctx, pn) seNodeTextOf((ctx), (pn))
#define seIsLit(pn, c) seIsLitT(sePt, (pn), (c))

static int seIsPowOp(D_ParseNode *pn) {
  const char *s = seNodeName(pn);
  return s[0] == '^' || (s[0] == '*' && s[1] == '*');
}

static const char *seEmit(seCtx *ctx, D_ParseNode *pn);

/* productions that only wrap a single child and carry no meaning of their own */
static int seNiWrapper(D_ParseNode *pn) {
  const char *name = seNodeName(pn);
  seNodeInfo ni;
  seNiReset(&ni);
  return seNodeHas(expression) || seNodeHas(add_expression) ||
    seNodeHas(mul_expression) || seNodeHas(unary_expression) ||
    seNodeHas(power_expression) || seNodeHas(primary_expression);
}

/* a numeric literal node ("number" wraps "integer_num"/"float_num") */
static int seNiNumber(D_ParseNode *pn) {
  const char *name = seNodeName(pn);
  seNodeInfo ni;
  seNiReset(&ni);
  return seNodeHas(number) || seNodeHas(integer_num) || seNodeHas(float_num);
}

/* single-production tests, for the places that ask exactly one question */
#define seNiIs(pn, what)                                \
  (strcmp(seSTRINGIFY(what), seNodeName(pn)) == 0)

static int seNiArgList(D_ParseNode *pn) {
  const char *name = seNodeName(pn);
  seNodeInfo ni;
  seNiReset(&ni);
  return seNodeHas(arg_list);
}

/* Does this exponent subtree reduce to a bare numeric literal?  This is the
   is.numeric(x[[3]]) test in .rxFromSE(): TRUE only for a literal, so `d^2`
   becomes Rx_pow_di(d,2) while `a^(-2)` (a call to unary minus) does not and
   falls through to "a^-2".  Reproducing that asymmetry matters -- the fixture
   pins it. */
static int seIsBareNumber(D_ParseNode *pn) {
  for (;;) {
    if (seNiNumber(pn)) return 1;
    if (d_get_number_of_children(pn) == 1 && seNiWrapper(pn)) {
      pn = d_get_child(pn, 0);
      continue;
    }
    return 0;
  }
}

#endif /* __SE_FROM_SE_NODE_H__ */
