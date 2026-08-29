/*
 * rxCseNode.h -- parse-node inspection for src/rxCse.c.
 *
 * The third of these, after seFromSEnode.h and rxToSEnode.h, and the same
 * paradigm as tran.c's nodeHas() (src/tran.h): the production name appears in
 * exactly one place, because STRINGIFY() turns the token in
 * csNodeHas(add_expression) into both the field it memoizes and the string it
 * compares, so there is no separate table to fall out of step with
 * inst/rxCse.g.
 *
 * Literals are matched on their first character -- O(1), and a literal cannot
 * be renamed the way a production can.
 */
#ifndef __RX_CSE_NODE_H__
#define __RX_CSE_NODE_H__

#include "seFromSEarena.h"
#include "seParseNode.h"

static D_ParserTables *csPt = &parser_tables_rxode2cse;

/* Every production in inst/rxCse.g, named ONCE: the list below generates both
   the memo field and its reset, so adding a production is one line and the two
   cannot fall out of step.  -1 = not yet asked.  Kept separate from
   seNodeInfo/rtNodeInfo on purpose -- see the note in seFromSEnode.h. */
#define CS_PRODUCTIONS(X)  \
  X(translation_unit) X(statement) X(assign_op) X(end_statement) \
  X(lhs) X(lhs_primary) X(lhs_args) X(expression) X(or_expression) \
  X(and_expression) X(rel_expression) X(rel_op) X(add_expression) \
  X(mul_expression) X(unary_expression) X(power_expression) \
  X(primary_expression) X(index_expression) X(function_call) \
  X(arg_list) X(function_name) X(symbol) X(number) X(identifier) \
  X(integer_num) X(float_num)

typedef struct csNodeInfo {
#define CS_PRODUCTIONS_FIELD(what) int what;
  CS_PRODUCTIONS(CS_PRODUCTIONS_FIELD)
#undef CS_PRODUCTIONS_FIELD
} csNodeInfo;

static inline void csNiReset(csNodeInfo *ni) {
#define CS_PRODUCTIONS_CLEAR(what) ni->what = -1;
  CS_PRODUCTIONS(CS_PRODUCTIONS_CLEAR)
#undef CS_PRODUCTIONS_CLEAR
}

#define csSTRINGIFY(...) csSTRINGIFY_AUX(__VA_ARGS__)
#define csSTRINGIFY_AUX(...) #__VA_ARGS__
#define csNIB(what) ni.what
#define csNodeHas(what)                                                 \
  (csNIB(what) == -1 ? (csNIB(what) = !strcmp(csSTRINGIFY(what), name)) \
   : csNIB(what))

#define csNiIs(pn, what) (strcmp(csSTRINGIFY(what), csNodeName(pn)) == 0)

/* this grammar's spelling of the shared node readers; see seParseNode.h */
#define csNodeName(pn) seNodeNameT(csPt, (pn))
#define csNodeText(ctx, pn) seNodeTextOf((ctx), (pn))
#define csIsLit(pn, c) seIsLitT(csPt, (pn), (c))

/* A production that only wraps a single child and carries no meaning of its
   own -- the precedence ladder is spelled out (or -> and -> rel -> add -> mul
   -> unary -> power -> primary), so a walk that did not see through these
   would render, and count, the same text once per rung. */
/* the rungs of that ladder */
static inline int csIsLadderNode(D_ParseNode *pn) {
  const char *name = csNodeName(pn);
  csNodeInfo ni; csNiReset(&ni);
  return csNodeHas(expression) || csNodeHas(or_expression) ||
    csNodeHas(and_expression) || csNodeHas(rel_expression) ||
    csNodeHas(add_expression) || csNodeHas(mul_expression) ||
    csNodeHas(unary_expression) || csNodeHas(power_expression) ||
    csNodeHas(primary_expression);
}

/* the single-child wrappers that are not precedence rungs */
static inline int csIsWrapperNode(D_ParseNode *pn) {
  const char *name = csNodeName(pn);
  csNodeInfo ni; csNiReset(&ni);
  return csNodeHas(number) || csNodeHas(lhs) || csNodeHas(lhs_primary) ||
    /* seArgsFlattenT() hands back the LIST node itself when there is only one
       argument, so these have to be seen through too -- the same trap the
       seFromSE emitter hit. */
    csNodeHas(arg_list) || csNodeHas(lhs_args) ||
    csNodeHas(translation_unit);
}

static inline D_ParseNode *csUnwrap(D_ParseNode *pn) {
  while (d_get_number_of_children(pn) == 1 &&
         (csIsLadderNode(pn) || csIsWrapperNode(pn))) {
    pn = d_get_child(pn, 0);
  }
  return pn;
}

#endif /* __RX_CSE_NODE_H__ */
