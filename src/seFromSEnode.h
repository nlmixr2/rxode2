/*
 * seFromSEnode.h -- what kind of parse node is this, and what text does it
 * span?
 *
 * Node dispatch never compares strings.  tran.c memoizes its comparisons per
 * node (nodeInfo + nodeHas(), src/tran.h), because a walk asks "is this an
 * assignment?", "is this a derivative?" over and over about the same node.
 * The same reasoning applies here, and dparser lets it be hoisted one level
 * further: pn->symbol is an index into the generated, compile-time-constant
 * pt->symbols[] table, so every production name can be resolved to a small
 * enum ONCE per process instead of once per node.  After that a node test is
 * an array lookup and an int compare.
 *
 * A kind table rather than one id per name, because dparser emits a SEPARATE
 * symbol for each occurrence of a literal in the grammar: "(" is symbol 24,
 * 26 and 28, "-" is 17 and 20.  Mapping ids to kinds collapses those.
 *
 * The grammar also spells the precedence ladder out (add -> mul -> unary ->
 * power -> primary), so every walk has to see through the productions that
 * only wrap a single child; seIsWrapper() names that once for the fold, the
 * call handler and the emitter.
 */
#ifndef __SE_FROM_SE_NODE_H__
#define __SE_FROM_SE_NODE_H__

#include "seFromSEarena.h"

static D_ParserTables *sePt = &parser_tables_rxode2seFromSE;

typedef enum {
  SE_K_OTHER = 0,
  /* nonterminals */
  SE_K_TRANSLATION_UNIT, SE_K_EXPRESSION, SE_K_ADD, SE_K_MUL, SE_K_UNARY,
  SE_K_POWER, SE_K_PRIMARY, SE_K_CALL, SE_K_ARGLIST, SE_K_FUNCNAME,
  SE_K_SYMBOL, SE_K_NUMBER, SE_K_IDENTIFIER, SE_K_INTEGER, SE_K_FLOAT,
  /* terminals */
  SE_K_LPAREN, SE_K_RPAREN, SE_K_COMMA,
  SE_K_PLUS, SE_K_MINUS, SE_K_TIMES, SE_K_DIVIDE, SE_K_POW
} seKind;

static unsigned char *seKindTab = NULL;
static unsigned int seKindN = 0;

/* Resolve every grammar symbol to its kind.  Called once from the batch entry
   point, before any work -- and before any parallel region, since it writes
   these statics. */
static void seKindsInit(void) {
  static const struct { const char *name; seKind kind; } map[] = {
    {"translation_unit", SE_K_TRANSLATION_UNIT},
    {"expression", SE_K_EXPRESSION},
    {"add_expression", SE_K_ADD},
    {"mul_expression", SE_K_MUL},
    {"unary_expression", SE_K_UNARY},
    {"power_expression", SE_K_POWER},
    {"primary_expression", SE_K_PRIMARY},
    {"function_call", SE_K_CALL},
    {"arg_list", SE_K_ARGLIST},
    {"function_name", SE_K_FUNCNAME},
    {"symbol", SE_K_SYMBOL},
    {"number", SE_K_NUMBER},
    {"identifier", SE_K_IDENTIFIER},
    {"integer", SE_K_INTEGER},
    {"float", SE_K_FLOAT},
    {"(", SE_K_LPAREN}, {")", SE_K_RPAREN}, {",", SE_K_COMMA},
    {"+", SE_K_PLUS}, {"-", SE_K_MINUS}, {"*", SE_K_TIMES},
    {"/", SE_K_DIVIDE}, {"**", SE_K_POW}, {"^", SE_K_POW}
  };
  const int nmap = (int)(sizeof(map)/sizeof(map[0]));
  unsigned int i;
  int j;
  if (seKindTab != NULL) return;
  seKindN = sePt->nsymbols;
  seKindTab = (unsigned char*) calloc(seKindN, sizeof(unsigned char));
  if (seKindTab == NULL) return;              /* seKindOf() then sees OTHER */
  for (i = 0; i < seKindN; i++) {
    const char *nm = (const char*) sePt->symbols[i].name;
    if (nm == NULL) continue;
    for (j = 0; j < nmap; j++) {
      if (strcmp(nm, map[j].name) == 0) {
        seKindTab[i] = (unsigned char) map[j].kind;
        break;
      }
    }
  }
}

static seKind seKindOf(D_ParseNode *pn) {
  unsigned int s = (unsigned int) pn->symbol;
  if (seKindTab == NULL || s >= seKindN) return SE_K_OTHER;
  return (seKind) seKindTab[s];
}

/* only where the raw text of a node is needed (identifiers, literals) */
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

/* productions that only wrap a single child and carry no meaning of their own */
static int seIsWrapper(seKind k) {
  return k == SE_K_EXPRESSION || k == SE_K_ADD || k == SE_K_MUL ||
    k == SE_K_UNARY || k == SE_K_POWER || k == SE_K_PRIMARY;
}

/* a numeric literal node ("number" wraps "integer"/"float") */
static int seIsNumberNode(seKind k) {
  return k == SE_K_NUMBER || k == SE_K_INTEGER || k == SE_K_FLOAT;
}

/* a node whose value stands on its own, with no children to combine */
static int seIsLeafNode(seKind k) {
  return seIsNumberNode(k) || k == SE_K_SYMBOL || k == SE_K_IDENTIFIER ||
    k == SE_K_CALL;
}

/* Does this exponent subtree reduce to a bare numeric literal?  This is the
   is.numeric(x[[3]]) test in .rxFromSE(): TRUE only for a literal, so `d^2`
   becomes Rx_pow_di(d,2) while `a^(-2)` (a call to unary minus) does not and
   falls through to "a^-2".  Reproducing that asymmetry matters -- the fixture
   pins it. */
static int seIsBareNumber(D_ParseNode *pn) {
  for (;;) {
    seKind k = seKindOf(pn);
    if (seIsNumberNode(k)) return 1;
    if (d_get_number_of_children(pn) == 1 && seIsWrapper(k)) {
      pn = d_get_child(pn, 0);
      continue;
    }
    return 0;
  }
}

#endif /* __SE_FROM_SE_NODE_H__ */
