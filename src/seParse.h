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
#include <float.h>
#define R_NO_REMAP_RMATH
#include <Rmath.h>
#include <R_ext/Utils.h>

/* dparser function-pointer table.  tran.c has the dparserPtrIni definitions;
   here we only want the extern declarations. */
#include <dparserPtr.h>

#include "seFromSEarena.h"

/* An upper bound on how deeply text nests, in operator levels: a chain of
   + or - adds one level per operator, the multiplicative chain inside each
   term adds one per operator plus its deepest factor, and a parenthesized
   group or call adds its own depth plus a few wrapper levels.  dparser
   builds, and frees, its tree recursively, so text this deep must not reach
   it: a 30000-factor product overflows a thread's C stack. */
typedef struct {
  int add, mulOps, maxFactor, maxTerm, maxArg;
} seDepthGrp;

static int seDepthOf(const seDepthGrp *g) {
  int term = g->mulOps + g->maxFactor;
  int d = g->add + (term > g->maxTerm ? term : g->maxTerm);
  return d > g->maxArg ? d : g->maxArg;
}

/* a group closes: its depth becomes a factor of the group around it */
static void seDepthClose(seDepthGrp *outer, const seDepthGrp *inner) {
  int d = seDepthOf(inner) + 4;
  if (d > outer->maxFactor) outer->maxFactor = d;
}

/* a new call argument: the group is as deep as its deepest argument */
static void seDepthComma(seDepthGrp *g) {
  int d = seDepthOf(g);
  memset(g, 0, sizeof(seDepthGrp));
  g->maxArg = d;
}

/* + or -: a sign (or an exponent's sign, 1e-06) nests like a factor; a
   binary one ends the term */
static void seDepthSign(seDepthGrp *g, int unary) {
  if (unary) {
    g->mulOps++;
    return;
  }
  int term = g->mulOps + g->maxFactor;
  if (term > g->maxTerm) g->maxTerm = term;
  g->mulOps = g->maxFactor = 0;
  g->add++;
}

static int seTextTooDeep(const char *s, int cap) {
  seDepthGrp st[SE_MAX_DEPTH / 4 + 1];
  int top = 0, prevOp = 1; /* at the start, a sign is unary */
  memset(&st[0], 0, sizeof(seDepthGrp));
  for (const char *c = s; *c; c++) {
    char ch = *c;
    if (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r') continue;
    if (ch == '(' || ch == '[') {
      if (++top > SE_MAX_DEPTH / 4) return 1;
      memset(&st[top], 0, sizeof(seDepthGrp));
    } else if (ch == ')' || ch == ']') {
      if (top == 0) return 0; /* unbalanced: the parser rejects it */
      top--;
      seDepthClose(&st[top], &st[top + 1]);
    } else if (ch == ',') {
      seDepthComma(&st[top]);
    } else if (ch == '+' || ch == '-') {
      seDepthSign(&st[top], prevOp || (c > s && (c[-1] == 'e' || c[-1] == 'E')));
    } else if (ch == '*' || ch == '/' || ch == '^') {
      if (!prevOp) st[top].mulOps++;
    } else {
      prevOp = 0;
      continue;
    }
    if (seDepthOf(&st[top]) > cap) return 1;
    prevOp = !(ch == ')' || ch == ']');
  }
  return 0;
}

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
  if (seTextTooDeep(in, SE_MAX_DEPTH)) {
    seFailWhy(ctx, SE_WHY_DEEP);
    return NULL;
  }
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
