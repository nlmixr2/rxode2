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
    err_trans("'ifelse' cannot be a state in an rxode2 model");
    tb.ix=-2;
    return 0;
  }
  if (!strcmp("if", s)){
    updateSyntaxCol();
    err_trans("'if' cannot be a variable/state in an rxode2 model");
    tb.ix=-2;
    return 0;
  }
  if (!strcmp("evid", s)){ // This is mangled by rxode2 so don't use it.
    updateSyntaxCol();
    trans_syntax_error_report_fn(_("'evid' cannot be a variable in an rxode2 model"));
    tb.ix=-2;
    return 0;
  }
  if (!strcmp("ii", s)){ // This is internally driven and not in the
  			 // covariate table so don't use it.
    updateSyntaxCol();
    trans_syntax_error_report_fn(_("'ii' cannot be a variable in an rxode2 model"));
    tb.ix=-2;
    return 0;
  }
  return 1;
}
static inline int isReservedVariable(const char *s) {
  return !strcmp("amt", s) ||
    !strcmp("time", s) ||
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
  if (tb.fn) {tb.ix=-2; return 0;}
  if (!strcmp("lhs", s)){tb.ix=-1; return 0;}
  if (assertForbiddenVariables(s) == 0) return 0;
  if (skipReservedVariables(s) == 0) return 0;
  // Ignore THETA[] and ETA
  if (strstr("[", s) != NULL) {tb.ix=-2;return 0;}

  for (i=0; i<NV; i++) {
    if (!strcmp(tb.ss.line[i], s)) {
      tb.ix = i;
      return 0;
    }
  }
  if (NV+1 > tb.allocS){
    tb.allocS += MXSYM;
    tb.lh = R_Realloc(tb.lh, tb.allocS, int);
    tb.lag = R_Realloc(tb.lag, tb.allocS, int);
    tb.ini= R_Realloc(tb.ini, tb.allocS, int);
    tb.mtime=R_Realloc(tb.mtime, tb.allocS, int);
    tb.iniv=R_Realloc(tb.iniv, tb.allocS, double);
    tb.ini0=R_Realloc(tb.ini0, tb.allocS, int);
    tb.df=R_Realloc(tb.df, tb.allocS, int);
    tb.dy=R_Realloc(tb.dy, tb.allocS, int);
    tb.sdfdy=R_Realloc(tb.sdfdy, tb.allocS, int);
  }
  return 1;
}
