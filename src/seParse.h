/*
 * seParse.h -- set up a translation unit that walks one of this package's
 * dparser grammars, and parse a single expression with one.
 *
 * src/seFromSE.c and src/rxToSE.c are the same program twice over: each parses
 * one expression with its own grammar and hands the tree to its own emitter.
 * Only the parser tables and the emitter differ, so that pair is the argument
 * and everything around it lives here.
 *
 * Include this INSTEAD of the R and dparser headers, then the grammar's
 * generated .g.d_parser.h, then the emitter, then seBatch.h.
 */
#ifndef __SE_PARSE_H__
#define __SE_PARSE_H__

#define STRICT_R_HEADERS
#define USE_FC_LEN_T
#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

/* dparser function-pointer table.  tran.c has the dparserPtrIni definitions;
   here we only want the extern declarations. */
#include <dparserPtr.h>

#include "seFromSEarena.h"

/* Walk a parse tree and return the emitted text, or NULL to decline. */
typedef const char *(*seEmitFn)(seCtx *ctx, D_ParseNode *pn);

/* Parse one expression and emit it.  Returns a pointer into ctx's arena, or
   NULL when the caller must fall back to the R walker -- either because the
   text did not parse or because the emitter set ctx->failed.  Declining is
   always safe; guessing is not.  No R API and no symengine call happens here,
   which is what lets seBatch.h run this across threads. */
static const char *seParseEmit(seCtx *ctx, const char *in,
                               D_ParserTables *tables, seEmitFn emit) {
  ctx->failed = 0;
  D_Parser *p = new_D_Parser(tables, sizeof(D_ParseNode_User));
  if (p == NULL) return NULL;
  p->save_parse_tree = 1;
  p->error_recovery = 0;
  D_ParseNode *pn = dparse(p, (char*) in, (int) strlen(in));
  const char *out = NULL;
  if (pn != NULL && p->syntax_errors == 0) {
    out = emit(ctx, pn);
    if (ctx->failed) out = NULL;
  }
  if (pn != NULL) free_D_ParseNode(p, pn);
  free_D_Parser(p);
  return out;
}

#endif /* __SE_PARSE_H__ */
