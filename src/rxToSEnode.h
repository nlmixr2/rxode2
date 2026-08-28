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

static D_ParserTables *rtPt = &parser_tables_rxode2rxToSE;

/* one field per production in inst/rxToSE.g; -1 = not yet asked */
typedef struct rtNodeInfo {
  int translation_unit;
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
} rtNodeInfo;

static inline void rtNiReset(rtNodeInfo *ni) {
  ni->translation_unit = -1;
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

#define rtSTRINGIFY(...) rtSTRINGIFY_AUX(__VA_ARGS__)
#define rtSTRINGIFY_AUX(...) #__VA_ARGS__
#define rtNIB(what) ni.what
#define rtNodeHas(what)                                                 \
  (rtNIB(what) == -1 ? (rtNIB(what) = !strcmp(rtSTRINGIFY(what), name)) \
   : rtNIB(what))

#define rtNiIs(pn, what) (strcmp(rtSTRINGIFY(what), rtNodeName(pn)) == 0)

static const char *rtNodeName(D_ParseNode *pn) {
  return (const char*) rtPt->symbols[pn->symbol].name;
}

static const char *rtNodeText(seCtx *ctx, D_ParseNode *pn) {
  const char *b = pn->start_loc.s, *e = pn->end;
  while (b < e && (*b == ' ' || *b == '\t' || *b == '\n')) b++;
  while (e > b && (e[-1] == ' ' || e[-1] == '\t' || e[-1] == '\n')) e--;
  return seDup(ctx, b, (size_t)(e - b));
}

static int rtIsLit(D_ParseNode *pn, char c) {
  return rtNodeName(pn)[0] == c;
}

static const char *rtEmit(seCtx *ctx, D_ParseNode *pn);

#endif /* __RX_TO_SE_NODE_H__ */
