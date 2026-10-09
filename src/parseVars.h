#include "strncmpi.h"

static inline int assertForbiddenVariables(const char *s) {
  if (!strcmp("printf", s)){
    updateSyntaxCol();
    trans_syntax_error_report_fn(_("'printf' cannot be a variable in an rxode2 model"));
    tb.ix=-2;
    return 0;
  }
  if (!strcmp("ID", s) || !strcmp("id", s) ||
      !strcmp("Id", s) || !strcmp("iD", s)) {
    updateSyntaxCol();
    trans_syntax_error_report_fn(_("'id' can only be used in the following ways 'id==\"id-value\"' or 'id !=\"id-value\"'"));
    tb.ix=-2;
    return 0;
  }
  if (!strcmp("Rprintf", s)){
    updateSyntaxCol();
    trans_syntax_error_report_fn(_("'Rprintf' cannot be a variable in an rxode2 model"));
    tb.ix=-2;
    return 0;
  }
  if (!strcmp("print", s)){
    updateSyntaxCol();
    trans_syntax_error_report_fn(_("'print' cannot be a variable in an rxode2 model"));
    tb.ix=-2;
    return 0;
  }
  if (!strcmp("ifelse", s)){
    updateSyntaxCol();
    _rxode2parse_unprotect();
    err_trans("'ifelse' cannot be a state in an rxode2 model");
    tb.ix=-2;
    return 0;
  }
  if (!strcmp("if", s)){
    updateSyntaxCol();
    _rxode2parse_unprotect();
    err_trans("'if' cannot be a variable/state in an rxode2 model");
    tb.ix=-2;
    return 0;
  }
  if (!rxstrcmpi("evid", s)){ // This is mangled by rxode2 so don't use it.
    updateSyntaxCol();
    trans_syntax_error_report_fn(_("'evid' cannot be a variable in an rxode2 model"));
    tb.ix=-2;
    return 0;
  }
  if (!rxstrcmpi("ii", s)){ // This is internally driven and not in the
  			 // covariate table so don't use it.
    updateSyntaxCol();
    trans_syntax_error_report_fn(_("'ii' cannot be a variable in an rxode2 model"));
    tb.ix=-2;
    return 0;
  }
  return 1;
}
static inline int isReservedVariable(const char *s) {
  return !rxstrcmpi("amt", s) ||
    !rxstrcmpi("time", s) ||
    !rxstrcmpi("mixnum", s) ||
    !rxstrcmpi("mixest", s) ||
    !rxstrcmpi("mixunif", s) ||
    mixSelNum(s, NULL) != 0 ||
    !strcmp("rx__PTR__", s) ||
    !strcmp("tlast", s) ||
    // Ignore M_ constants
    !strcmp("M_E", s) ||
    !strcmp("M_LOG2E", s) ||
    !strcmp("M_LOG10E", s) ||
    !strcmp("M_LN2", s) ||
    !strcmp("M_LN10", s) ||
    !strcmp("M_PI", s) ||
    !strcmp("M_PI_2", s) ||
    !strcmp("M_PI_4", s) ||
    !strcmp("M_1_PI", s) ||
    !strcmp("M_2_PI", s) ||
    !strcmp("M_2_SQRTPI", s) ||
    !strcmp("M_SQRT2", s) ||
    !strcmp("M_SQRT1_2", s) ||
    !strcmp("M_SQRT_3", s) ||
    !strcmp("M_SQRT_32", s) ||
    !strcmp("M_LOG10_2", s) ||
    !strcmp("M_2PI", s) ||
    !strcmp("M_SQRT_PI", s) ||
    !strcmp("M_1_SQRT_2PI", s) ||
    !strcmp("M_SQRT_2dPI", s) ||
    !strcmp("M_LN_SQRT_PI", s) ||
    !strcmp("M_LN_SQRT_2PI", s) ||
    !strcmp("M_LN_SQRT_PId2", s) ||
    !strcmp("rxFlag", s) ||
    // newind/t
    !strcmp("newind", s) ||
    !strcmp("NEWIND", s) ||
    !strcmp("t", s);
}


// Names that can never be a user variable: the reserved variables above, the
// constants skipped by skipReservedVariables(), and the names new_or_ith()
// drops or rewrites before it reaches either.  Exposed to R through
// `.rxIsReservedName()` so model piping does not promote them.
static inline int isReservedName(const char *s) {
  return isReservedVariable(s) ||
    !strcmp("pi", s) ||
    !strcmp("NA", s) ||
    !strcmp("NaN", s) ||
    !strcmp("Inf", s) ||
    !strcmp("lhs", s) ||
    !strcmp("rxlin___", s) ||
    // any spelling of CMT but the exact one is rewritten to CMT
    (strcmp("CMT", s) && !rxstrcmpi("CMT", s));
}

static inline int isKa(const char *s) {
  if (tb.hasKa) return 1;
  if (!strcmp("ka", s) || !strcmp("Ka", s) || !strcmp("KA", s) || !strcmp("kA", s)) {
    tb.hasKa=1;
    return 1;
  }
  return 0;
}

static inline int skipReservedVariables(const char *s) {
  if (isReservedVariable(s)) {
    tb.ix=-2;
    return 0;
  }
  if (!strcmp("pi", s)) tb.isPi=1;
  if (!strcmp("NA", s) || !strcmp("NaN", s) || !strcmp("Inf", s)) return 0;
  isKa(s); // To update tb.hasKa
  return 1;
}

/* new symbol? if no, find it's ith */
static inline int new_or_ith(const char *s) {
  int i;
  tb.ix=-2;
  if (tb.fn) {tb.ix=-2; return 0;}
  if (!strcmp("lhs", s)){tb.ix=-1; return 0;}
  if (strcmp("CMT", s) && !rxstrcmpi("CMT", s)) {
    return new_or_ith("CMT");
  }
  if (assertForbiddenVariables(s) == 0) return 0;
  if (skipReservedVariables(s) == 0) return 0;
  // Ignore THETA[] and ETA
  if (strstr("[", s) != NULL) {tb.ix=-2;return 0;}
  if (!strcmp("rxlin___", s)) return 0;

  for (i=0; i<NV; i++) {
    if (!strcmp(tb.ss.line[i], s)) {
      tb.ix = i;
      // if currently defining an interpolation and
      // this variable has not been assigned an interpolation
      // then assign it.
      if (tb.interpC != 0) {
        if (tb.interp[tb.ix] == 0) {
          tb.interp[tb.ix] = tb.interpC;
        } else {
          // It this variable has already been assigned an
          // interpolation error
          sPrint(&_gbuf,"'%s' cannot have more than one interpolation method", s);
          updateSyntaxCol();
          trans_syntax_error_report_fn(_gbuf.s);
        }
      }
      return 0;
    }
  }
  if (NV+1 > tb.allocNV){
    int oldNV = tb.allocNV;
    tb.allocNV += MXSYM;
    tb.lh = R_Realloc(tb.lh, tb.allocNV, int);
    tb.lho = R_Realloc(tb.lho, tb.allocNV, int);
    tb.interp = R_Realloc(tb.interp, tb.allocNV, int);
    tb.lag = R_Realloc(tb.lag, tb.allocNV, int);
    tb.alag = R_Realloc(tb.alag, tb.allocNV, int);
    tb.ini = R_Realloc(tb.ini, tb.allocNV, int);
    tb.mtime = R_Realloc(tb.mtime, tb.allocNV, int);
    tb.iniv = R_Realloc(tb.iniv, tb.allocNV, double);
    tb.ini0 = R_Realloc(tb.ini0, tb.allocNV, int);
    tb.df = R_Realloc(tb.df, tb.allocNV, int);
    tb.dy = R_Realloc(tb.dy, tb.allocNV, int);
    tb.sdfdy = R_Realloc(tb.sdfdy, tb.allocNV, int);
    // R_Realloc does not zero; the first block is R_Calloc'd and read as 0
    memset(tb.lh + oldNV, 0, MXSYM*sizeof(int));
    memset(tb.lho + oldNV, 0, MXSYM*sizeof(int));
    memset(tb.interp + oldNV, 0, MXSYM*sizeof(int));
    memset(tb.lag + oldNV, 0, MXSYM*sizeof(int));
    memset(tb.alag + oldNV, 0, MXSYM*sizeof(int));
    memset(tb.ini + oldNV, 0, MXSYM*sizeof(int));
    memset(tb.mtime + oldNV, 0, MXSYM*sizeof(int));
    memset(tb.iniv + oldNV, 0, MXSYM*sizeof(double));
    memset(tb.ini0 + oldNV, 0, MXSYM*sizeof(int));
    memset(tb.df + oldNV, 0, MXSYM*sizeof(int));
    memset(tb.dy + oldNV, 0, MXSYM*sizeof(int));
    memset(tb.sdfdy + oldNV, 0, MXSYM*sizeof(int));
  }
  return 1;
}
