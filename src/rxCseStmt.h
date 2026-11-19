/*
 * rxCseStmt.h -- one statement through the two passes, for src/rxCse.cpp.
 *
 * Everything here is about a SINGLE statement: opening a parse for it, telling
 * an assignment from a bare expression, and the two walks -- pass 1 counts the
 * subexpressions it contains (machine A), pass 2 rewrites it in terms of the
 * chosen temporaries (machine A, re-parsed, then machine B).  Neither knows
 * anything about the model as a whole; the driver in rxCse.cpp owns that.
 */
#ifndef __RX_CSE_STMT_H__
#define __RX_CSE_STMT_H__

/* --------------------------------------------------------------- one parse */
typedef struct csParse { D_Parser *p; D_ParseNode *pn; } csParse;

static int csParseOpen(csParse *o, const char *in) {
  o->p = new_D_Parser(&parser_tables_rxode2cse, sizeof(D_ParseNode_User));
  o->pn = NULL;
  if (o->p == NULL) return 0;
  o->p->save_parse_tree = 1;
  o->p->error_recovery = 0;
  o->pn = dparse(o->p, (char*) in, (int) strlen(in));
  if (o->pn == NULL || o->p->syntax_errors != 0) return 0;
  return 1;
}

static void csParseClose(csParse *o) {
  if (o->pn != NULL) free_D_ParseNode(o->p, o->pn);
  if (o->p != NULL) free_D_Parser(o->p);
  o->pn = NULL; o->p = NULL;
}

/* the `statement` node under translation_unit */
static D_ParseNode *csStmtNode(D_ParseNode *pn) {
  D_ParseNode *s = pn;
  while (d_get_number_of_children(s) == 1) s = d_get_child(s, 0);
  return s;
}

/* Is this statement `lhs <op> rhs`?  Otherwise it is a bare call such as
   dvid(3, 4), which ..rxOpt() renders with machine B alone -- machine A is
   only ever applied to an assignment's right-hand side
   (R/rxOptExpr.R:344). */
static int csStmtIsAssign(D_ParseNode *s) {
  return d_get_number_of_children(s) >= 3 &&
    !strcmp(csNodeName(csUnwrap(d_get_child(s, 1))), "assign_op");
}

/* `<-` becomes `=`; `=` and `~` are kept (R/rxOptExpr.R:355-357) */
static const char *csAssignOp(csCtx *c, D_ParseNode *s) {
  const char *op = csNodeText(csArena(c), csUnwrap(d_get_child(s, 1)));
  if (!strcmp(op, "<-")) return "=";
  return op;
}

static void csCountStmt(csCtx *c, const char *line, int idx) {
  csParse o = {NULL, NULL};
  D_ParseNode *s;
  c->stmt = idx; c->pos = 0; c->arena.failed = 0;
  if (!csParseOpen(&o, line)) {
    c->failWhy = "parse"; c->failLine = line;
    c->arena.failed = 1; csParseClose(&o); return;
  }
  s = csStmtNode(o.pn);
  if (csStmtIsAssign(s)) {
    (void) csLhs(c, d_get_child(s, 0));            /* validates the lhs form */
    if (c->arena.failed) {
      if (c->failWhy == NULL) c->failWhy = "lhs";
    } else {
      (void) csA(c, d_get_child(s, 2));
      if (c->arena.failed && c->failWhy == NULL) c->failWhy = "rhs";
    }
  }
  if (c->arena.failed) { c->anyFail = 1; c->failLine = line; }
  csParseClose(&o);
}

/* ------------------------------------------------------------------ pass 2 */
/* The right-hand side of an assignment goes through BOTH machines: A rebuilds
   it with the chosen temporaries substituted in, and its text is re-parsed so
   B can render the result.  "" on failure, with c->arena.failed set. */
static const char *csOptRhs(csCtx *c, D_ParseNode *rhs) {
  csParse o2 = {NULL, NULL};
  const char *keyed = csA(c, rhs);                   /* machine A */
  const char *out = NULL;
  /* csParseOpen() allocates the D_Parser BEFORE it can fail, so the failing
     path has to close it too */
  if (!c->arena.failed && csParseOpen(&o2, keyed)) {
    D_ParseNode *s2 = csStmtNode(o2.pn);
    if (csStmtIsAssign(s2)) c->arena.failed = 1;
    else out = csB(c, d_get_child(s2, 0));           /* machine B */
  } else {
    c->arena.failed = 1;
  }
  csParseClose(&o2);
  return c->arena.failed ? "" : out;
}

static const char *csOptStmt(csCtx *c, const char *line, int idx) {
  /* Initialized here, not by csParseOpen: the failing path below can short
     circuit past the open and still reach csParseClose. */
  csParse o = {NULL, NULL};
  D_ParseNode *s;
  const char *out = NULL;
  c->stmt = idx; c->pos = 0; c->nused = 0; c->arena.failed = 0;
  if (!csParseOpen(&o, line)) { c->arena.failed = 1; c->anyFail = 1;
    csParseClose(&o); return NULL; }
  s = csStmtNode(o.pn);
  if (csStmtIsAssign(s)) {
    const char *lhs = csLhs(c, d_get_child(s, 0));
    const char *op = csAssignOp(c, s);
    const char *rhs = csOptRhs(c, d_get_child(s, 2));
    if (!c->arena.failed) {
      out = seCat(csArena(c), lhs, op, rhs, NULL, NULL, NULL);
    }
  } else {
    out = csB(c, d_get_child(s, 0));                 /* machine B alone */
  }
  csParseClose(&o);
  if (c->arena.failed) { c->anyFail = 1; c->failLine = line; }
  return c->arena.failed ? NULL : out;
}

#endif /* __RX_CSE_STMT_H__ */
