/*
 * seFromSEnode.h -- what kind of parse node is this, and what text does it
 * span?
 *
 * The grammar spells the precedence ladder out (add -> mul -> unary -> power
 * -> primary), so every walk over the tree has to be able to see through the
 * productions that only wrap a single child.  Naming that once here keeps the
 * fold, the call handler and the emitter from each repeating the ladder.
 */
#ifndef __SE_FROM_SE_NODE_H__
#define __SE_FROM_SE_NODE_H__

#include "seFromSEarena.h"

static D_ParserTables *sePt = &parser_tables_rxode2seFromSE;

static const char *seNodeName(D_ParseNode *pn) {
  return (const char*) sePt->symbols[pn->symbol].name;
}

static const char *seNodeText(seCtx *ctx, D_ParseNode *pn) {
  const char *b = pn->start_loc.s, *e = pn->end;
  while (b < e && (*b == ' ' || *b == '\t' || *b == '\n')) b++;
  while (e > b && (e[-1] == ' ' || e[-1] == '\t' || e[-1] == '\n')) e--;
  return seDup(ctx, b, (size_t)(e - b));
}

static const char *seEmit(seCtx *ctx, D_ParseNode *pn);

/* Productions that only wrap a single child and carry no meaning of their own.
   The grammar spells the precedence ladder out (add -> mul -> unary -> power
   -> primary), so every walk has to be able to see through it; naming that
   once keeps seIsBareNumber(), seStripP() and seFold() from each repeating
   the ladder. */
static int seIsWrapper(const char *nm) {
  return strcmp(nm, "expression") == 0 ||
    strcmp(nm, "add_expression") == 0 ||
    strcmp(nm, "mul_expression") == 0 ||
    strcmp(nm, "unary_expression") == 0 ||
    strcmp(nm, "power_expression") == 0 ||
    strcmp(nm, "primary_expression") == 0;
}

/* a numeric literal node ("number" wraps "integer"/"float") */
static int seIsNumberNode(const char *nm) {
  return strcmp(nm, "number") == 0 || strcmp(nm, "integer") == 0 ||
    strcmp(nm, "float") == 0;
}

/* a node whose value stands on its own, with no children to combine */
static int seIsLeafNode(const char *nm) {
  return seIsNumberNode(nm) || strcmp(nm, "symbol") == 0 ||
    strcmp(nm, "identifier") == 0 || strcmp(nm, "function_call") == 0;
}

/* Does this exponent subtree reduce to a bare numeric literal?  This is the
   is.numeric(x[[3]]) test in .rxFromSE(): TRUE only for a literal, so `d^2`
   becomes Rx_pow_di(d,2) while `a^(-2)` (a call to unary minus) does not and
   falls through to "a^-2".  Reproducing that asymmetry matters -- the fixture
   pins it. */
static int seIsBareNumber(D_ParseNode *pn) {
  for (;;) {
    const char *nm = seNodeName(pn);
    if (seIsNumberNode(nm)) return 1;
    if (d_get_number_of_children(pn) == 1 && seIsWrapper(nm)) {
      pn = d_get_child(pn, 0);
      continue;
    }
    return 0;
  }
}

#endif /* __SE_FROM_SE_NODE_H__ */
