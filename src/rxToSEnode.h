/*
 * rxToSEnode.h -- parse-node inspection for src/rxToSE.c.
 *
 * The mirror of seFromSEnode.h for the other direction.  Same paradigm as
 * tran.c's nodeHas() (src/tran.h): the production name appears in exactly one
 * place, the RT_PRODUCTIONS list, which generates both the field and the name
 * it is resolved from -- so there is no separate table to fall out of step
 * with inst/rxToSE.g.  The name is resolved to its parser-table index once,
 * so a node test in the walk is an integer compare, not a strcmp.
 *
 * Literals are matched on their first character -- O(1), and a literal cannot
 * be renamed the way a production can.  Multi-character operators are
 * disambiguated by their second character.
 */
#ifndef __RX_TO_SE_NODE_H__
#define __RX_TO_SE_NODE_H__

#include "seFromSEarena.h"
#include "seParseNode.h"

static D_ParserTables *rtPt = &parser_tables_rxode2rxToSE;

/* Every production in inst/rxToSE.g, named ONCE: the list below generates both
   the memo field and its reset.  -1 = not yet asked.  Kept separate from
   seNodeInfo on purpose -- see the note there. */
#define RT_PRODUCTIONS(X)  \
  X(translation_unit) X(expression) X(or_expression) X(and_expression) \
  X(rel_expression) X(rel_op) X(add_expression) X(mul_expression) \
  X(unary_expression) X(power_expression) X(primary_expression) \
  X(index_expression) X(function_call) X(arg_list) X(function_name) \
  X(symbol) X(number) X(identifier) X(integer_num) X(float_num)

/* parser-table index of each production, resolved once; see seFromSEnode.h */
typedef struct rtSymIds {
#define RT_PRODUCTIONS_FIELD(what) int what;
  RT_PRODUCTIONS(RT_PRODUCTIONS_FIELD)
#undef RT_PRODUCTIONS_FIELD
} rtSymIds;

static rtSymIds rtSym;
static int rtSymReady = 0;

/* main thread, before any parallel region */
static void rtSymInit(void) {
  if (rtSymReady) return;
#define RT_PRODUCTIONS_FIND(what) rtSym.what = seProdIdT(rtPt, #what);
  RT_PRODUCTIONS(RT_PRODUCTIONS_FIND)
#undef RT_PRODUCTIONS_FIND
  rtSymReady = 1;
}

#define rtNodeHas(what) (pn->symbol == rtSym.what)
#define rtNiIs(pn, what) ((pn)->symbol == rtSym.what)

/* this grammar's spelling of the shared node readers; see seParseNode.h */
#define rtNodeName(pn) seNodeNameT(rtPt, (pn))
#define rtNodeText(ctx, pn) seNodeTextOf((ctx), (pn))
#define rtIsLit(pn, c) seIsLitT(rtPt, (pn), (c))

static const char *rtEmit(seCtx *ctx, D_ParseNode *pn);

#endif /* __RX_TO_SE_NODE_H__ */
