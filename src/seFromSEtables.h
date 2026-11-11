/*
 * seFromSEtables.h -- bring .rxDtemplates() across from R into plain C.
 *
 * The derivative templates, rxode2parseD()'s names and entry lengths, .SEeq,
 * and the functions baseenv() binds, each as an array plus a hash table, so the
 * walk finds a name with one hash and one strcmp.  Done once per .Call, where
 * the R API is allowed: CHAR() pointers stay valid while the caller's vectors
 * are protected, so the walk itself never touches R.
 *
 * R_alloc, not malloc: STRING_ELT/INTEGER type check and can longjmp, and so
 * can Rf_mkChar() inside seRunBatch() afterwards, either of which would leave
 * a malloc unfreed.  R_alloc is released when the .Call unwinds either way.
 */
#ifndef __SE_FROM_SE_TABLES_H__
#define __SE_FROM_SE_TABLES_H__

/* Build an open-addressing table over names[0..n-1], keeping the FIRST row of
   each name (R's `[` on a named vector does the same). */
static void seHashBuild(seHash *h, const char **names, int n) {
  int cap = 16, i;
  while (cap < 2 * n + 1) cap <<= 1;
  h->cap = cap;
  h->slot = (int*) R_alloc((size_t) cap, sizeof(int));
  for (i = 0; i < cap; i++) h->slot[i] = -1;
  for (i = 0; i < n; i++) {
    unsigned int k = seHashStr(names[i]) & (unsigned int)(cap - 1);
    int dup = 0;
    while (h->slot[k] != -1) {
      if (strcmp(names[h->slot[k]], names[i]) == 0) { dup = 1; break; }
      k = (k + 1) & (unsigned int)(cap - 1);
    }
    if (!dup) h->slot[k] = i;
  }
}

/* element `i` of list `l` if it has type `type`, else R_NilValue */
static SEXP seListEl(SEXP l, int i, int type) {
  if (TYPEOF(l) != VECSXP || Rf_xlength(l) <= i) return R_NilValue;
  SEXP v = VECTOR_ELT(l, i);
  return TYPEOF(v) == type ? v : R_NilValue;
}

/* A name vector from R, hashed: the keys (NULL when empty) and their count.
   `len`, when given, must be an INTSXP the same length or the table is
   left empty. */
static const char **seNamesFromR(SEXP v, SEXP len, seHash *h, int *nOut) {
  int i, n = (TYPEOF(v) == STRSXP) ? (int) Rf_xlength(v) : 0;
  *nOut = 0;
  if (n == 0) return NULL;
  if (len != NULL && (TYPEOF(len) != INTSXP || Rf_xlength(len) != n)) return NULL;
  const char **keys = (const char**) R_alloc((size_t) n, sizeof(char*));
  for (i = 0; i < n; i++) keys[i] = CHAR(STRING_ELT(v, i));
  seHashBuild(h, keys, n);
  *nOut = n;
  return keys;
}

/* rows of rxode2parseD(): name, which, template, kind */
static void seDerivsFromR(seTables *t, SEXP tabs) {
  int n, i;
  SEXP dWhich = seListEl(tabs, 1, INTSXP), dTmpl = seListEl(tabs, 2, STRSXP),
    dKind = seListEl(tabs, 3, INTSXP);
  const char **keys = seNamesFromR(seListEl(tabs, 0, STRSXP), dWhich, &t->dHash, &n);
  if (n == 0 || Rf_xlength(dTmpl) != n || Rf_xlength(dKind) != n) return;
  seDeriv *d = (seDeriv*) R_alloc((size_t) n, sizeof(seDeriv));
  for (i = 0; i < n; i++) {
    d[i].name = keys[i];
    d[i].which = INTEGER(dWhich)[i];
    d[i].tmpl = CHAR(STRING_ELT(dTmpl, i));
    d[i].kind = INTEGER(dKind)[i];
  }
  /* rows of one name are adjacent (ls() order), so the first row the hash
     finds is where seFindDeriv() starts scanning */
  t->derivs = d;
  t->nderivs = n;
}

/* .SEeq: name -> argument count */
static void seEqFromR(seTables *t, SEXP tabs) {
  int n, i;
  SEXP eqN = seListEl(tabs, 7, INTSXP);
  const char **keys = seNamesFromR(seListEl(tabs, 6, STRSXP), eqN, &t->eqHash, &n);
  if (n == 0) return;
  seEq *e = (seEq*) R_alloc((size_t) n, sizeof(seEq));
  for (i = 0; i < n; i++) {
    e[i].name = keys[i];
    e[i].nargs = INTEGER(eqN)[i];
  }
  t->eq = e;
  t->neq = n;
}

/* the list .rxDtemplates() returns, in its order */
static void seTablesFromR(seTables *t, SEXP tabs) {
  memset(t, 0, sizeof(seTables));
  seDerivsFromR(t, tabs);
  SEXP rxdLen = seListEl(tabs, 5, INTSXP);
  t->rxdName = seNamesFromR(seListEl(tabs, 4, STRSXP), rxdLen, &t->rxdHash, &t->nrxd);
  if (t->nrxd > 0) t->rxdLen = INTEGER(rxdLen);
  seEqFromR(t, tabs);
  t->baseFns = seNamesFromR(seListEl(tabs, 8, STRSXP), NULL, &t->baseHash, &t->nbase);
}

#endif /* __SE_FROM_SE_TABLES_H__ */
