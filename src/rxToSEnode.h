/*
 * rxToSEnode.h -- parse-node inspection for src/rxToSE.c.
 *
 * The mirror of seFromSEnode.h for the other direction.  Same paradigm as
 * tran.c's nodeHas() (src/tran.h): the production name appears in exactly one
 * place, because STRINGIFY() turns the token in rtNodeHas(add_expression) into
 * both the field it memoizes and the string it compares, so there is no
 * separate table to fall out of step with inst/rxToSE.g.
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

typedef struct rtNodeInfo {
#define RT_PRODUCTIONS_FIELD(what) int what;
  RT_PRODUCTIONS(RT_PRODUCTIONS_FIELD)
#undef RT_PRODUCTIONS_FIELD
} rtNodeInfo;

static inline void rtNiReset(rtNodeInfo *ni) {
#define RT_PRODUCTIONS_CLEAR(what) ni->what = -1;
  RT_PRODUCTIONS(RT_PRODUCTIONS_CLEAR)
#undef RT_PRODUCTIONS_CLEAR
}

#define rtSTRINGIFY(...) rtSTRINGIFY_AUX(__VA_ARGS__)
#define rtSTRINGIFY_AUX(...) #__VA_ARGS__
#define rtNIB(what) ni.what
#define rtNodeHas(what)                                                 \
  (rtNIB(what) == -1 ? (rtNIB(what) = !strcmp(rtSTRINGIFY(what), name)) \
   : rtNIB(what))

#define rtNiIs(pn, what) (strcmp(rtSTRINGIFY(what), rtNodeName(pn)) == 0)

/* this grammar's spelling of the shared node readers; see seParseNode.h */
#define rtNodeName(pn) seNodeNameT(rtPt, (pn))
#define rtNodeText(ctx, pn) seNodeTextOf((ctx), (pn))
#define rtIsLit(pn, c) seIsLitT(rtPt, (pn), (c))

static const char *rtEmit(seCtx *ctx, D_ParseNode *pn);

#endif /* __RX_TO_SE_NODE_H__ */
