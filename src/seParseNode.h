/*
 * seParseNode.h -- read a parse node: its name, the source text it spans, and
 * whether it is a particular literal.
 *
 * Every grammar in the package answers these the same way; only the parser
 * tables that name the symbols differ, so they are the argument.  What is NOT
 * here is nodeHas(): its memo struct is one field per production, so it is
 * necessarily per grammar (see seFromSEnode.h / rxToSEnode.h).
 */
#ifndef __SE_PARSE_NODE_H__
#define __SE_PARSE_NODE_H__

#include "seFromSEarena.h"

static const char *seNodeNameT(D_ParserTables *t, D_ParseNode *pn) {
  return (const char*) t->symbols[pn->symbol].name;
}

/* the source text the node spans, trimmed of surrounding whitespace */
static const char *seNodeTextOf(seCtx *ctx, D_ParseNode *pn) {
  const char *b = pn->start_loc.s, *e = pn->end;
  while (b < e && (*b == ' ' || *b == '\t' || *b == '\n')) b++;
  while (e > b && (e[-1] == ' ' || e[-1] == '\t' || e[-1] == '\n')) e--;
  return seDup(ctx, b, (size_t)(e - b));
}

/* literals are matched on the first character of the symbol name */
static int seIsLitT(D_ParserTables *t, D_ParseNode *pn, char c) {
  return seNodeNameT(t, pn)[0] == c;
}

/* An arg_list is left recursive -- `a, b, c` parses as `((a, b), c)` -- so the
   arguments come out of the tree rightmost first.  Walk down the left spine
   pushing each right-hand argument, then pop, and they land in source order.
   Returns the count, or -1 if there are more arguments than `max` (or than the
   fixed spine depth), which the caller treats as "decline". */
#define SE_ARGS_MAX_DEPTH 32
static int seArgsFlattenT(D_ParserTables *t, D_ParseNode *pn,
                          D_ParseNode **args, int max) {
  D_ParseNode *stack[SE_ARGS_MAX_DEPTH];
  int top = 0, n = 0;
  for (;;) {
    if (d_get_number_of_children(pn) == 3 &&
        seIsLitT(t, d_get_child(pn, 1), ',')) {
      if (top >= SE_ARGS_MAX_DEPTH) return -1;
      stack[top++] = d_get_child(pn, 2);
      pn = d_get_child(pn, 0);
      continue;
    }
    break;
  }
  if (n >= max) return -1;
  args[n++] = pn;                 /* leftmost */
  while (top > 0) {
    if (n >= max) return -1;
    args[n++] = stack[--top];
  }
  return n;
}

#endif /* __SE_PARSE_NODE_H__ */
