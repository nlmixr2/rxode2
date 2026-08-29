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

/* One field per production in inst/rxCse.g; -1 = not yet asked.  Kept separate
   from seNodeInfo/rtNodeInfo on purpose -- see the note in seFromSEnode.h. */
typedef struct csNodeInfo {
  int translation_unit;
  int statement;
  int assign_op;
  int end_statement;
  int lhs;
  int lhs_primary;
  int lhs_args;
  int expression;
  int or_expression;
  int and_expression;
  int rel_expression;
  int rel_op;
  int add_expression;
  int mul_expression;
  int unary_expression;
  int power_expression;
  int primary_expression;
  int index_expression;
  int function_call;
  int arg_list;
  int function_name;
  int symbol;
  int number;
  int identifier;
  int integer_num;
  int float_num;
} csNodeInfo;

static inline void csNiReset(csNodeInfo *ni) {
  ni->translation_unit = -1;
  ni->statement = -1;
  ni->assign_op = -1;
  ni->end_statement = -1;
  ni->lhs = -1;
  ni->lhs_primary = -1;
  ni->lhs_args = -1;
  ni->expression = -1;
  ni->or_expression = -1;
  ni->and_expression = -1;
  ni->rel_expression = -1;
  ni->rel_op = -1;
  ni->add_expression = -1;
  ni->mul_expression = -1;
  ni->unary_expression = -1;
  ni->power_expression = -1;
  ni->primary_expression = -1;
  ni->index_expression = -1;
  ni->function_call = -1;
  ni->arg_list = -1;
  ni->function_name = -1;
  ni->symbol = -1;
  ni->number = -1;
  ni->identifier = -1;
  ni->integer_num = -1;
  ni->float_num = -1;
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
static inline D_ParseNode *csUnwrap(D_ParseNode *pn) {
  for (;;) {
    if (d_get_number_of_children(pn) != 1) return pn;
    const char *name = csNodeName(pn);
    csNodeInfo ni; csNiReset(&ni);
    if (csNodeHas(expression) || csNodeHas(or_expression) ||
        csNodeHas(and_expression) || csNodeHas(rel_expression) ||
        csNodeHas(add_expression) || csNodeHas(mul_expression) ||
        csNodeHas(unary_expression) || csNodeHas(power_expression) ||
        csNodeHas(primary_expression) || csNodeHas(number) ||
        csNodeHas(lhs) || csNodeHas(lhs_primary) ||
        /* seArgsFlattenT() hands back the LIST node itself when there is only
           one argument, so these have to be seen through too -- the same trap
           the seFromSE emitter hit. */
        csNodeHas(arg_list) || csNodeHas(lhs_args) ||
        csNodeHas(translation_unit)) {
      pn = d_get_child(pn, 0);
      continue;
    }
    return pn;
  }
}

#endif /* __RX_CSE_NODE_H__ */
