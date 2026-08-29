/*
 * rxCse.cpp -- the rxCse .Call.
 *
 * R strings in, one optimized model text out (or NA_character_ to decline).
 * The work is in rxCseRun.h; this file only reads the argument and hands the
 * result back to R.  Why the pipeline exists at all, and why declining is all
 * or nothing, is in rxCseRun.h.
 */
#include "rxCseRun.h"

/* Hand the finished text to R under R_UnwindProtect: Rf_mkChar() allocates and
   can longjmp, and `outText` is malloc'd, so the cleanup is what frees it on
   the error path.  Same pattern as src/seBatch.h. */
typedef struct csFinish { char *text; } csFinish;

static SEXP csFinishFun(void *data) {
  csFinish *f = (csFinish*) data;
  return Rf_ScalarString(Rf_mkChar(f->text));
}

static void csFinishClean(void *data, Rboolean jump) {
  csFinish *f = (csFinish*) data;
  (void) jump;
  free(f->text);
  f->text = NULL;
}

/* the model's statements as C strings.  CHAR() before any thread starts, with
   R_alloc so an error cannot leak them (the rule from src/seBatch.h:88-104);
   NULL when any element is NA, which declines the whole call. */
static const char **csReadLines(SEXP linesVec, R_xlen_t n) {
  const char **in = (const char**) R_alloc((size_t) n, sizeof(char*));
  R_xlen_t i;
  for (i = 0; i < n; i++) {
    SEXP el = STRING_ELT(linesVec, i);
    if (el == NA_STRING) return NULL;
    in[i] = CHAR(el);
  }
  return in;
}

extern "C" SEXP _rxode2_rxCse(SEXP linesVec) {
  R_xlen_t n;
  const char **in;
  csRun r;
  char *outText;
  SEXP ret;

  if (TYPEOF(linesVec) != STRSXP) Rf_error("%s", "'linesVec' must be character");
  n = Rf_xlength(linesVec);
  if (n == 0) return Rf_ScalarString(NA_STRING);
  in = csReadLines(linesVec, n);
  if (in == NULL) return Rf_ScalarString(NA_STRING);

  csRunInit(&r, n, in, csPickThreads(n));
  outText = csRunAll(&r);
  csRunFree(&r);

  /* every other hand-managed allocation is released by here; outText is the
     last one, and R_UnwindProtect is what frees it if Rf_mkChar longjmps */
  if (outText == NULL) return Rf_ScalarString(NA_STRING);
  {
    csFinish fin; fin.text = outText;
    SEXP cont = PROTECT(R_MakeUnwindCont());
    ret = R_UnwindProtect(csFinishFun, &fin, csFinishClean, &fin, cont);
    /* csFinishClean() NULLs the pointer, so this frees it if the cleanup did
       not already run and is a no-op if it did.  Correct either way: whether
       R_UnwindProtect runs the cleanup on a NORMAL return is not something to
       take on faith. */
    free(fin.text);
    fin.text = NULL;
    UNPROTECT(1);
  }
  return ret;
}
