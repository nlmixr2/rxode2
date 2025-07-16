static inline void linCmtParseAOB(linCmtStruct *lin, int verbose) {
  lin->ncmt = 2;
  lin->trans = 5;
  if (lin->v == -1) {
    sFree(&(lin->ret0));
    sFree(&(lin->ret));
    _rxode2parse_unprotect();
    err_trans("cannot figure out a central volume");
  }
  if (lin->alpha == -1) {
    sFree(&(lin->ret0));
    sFree(&(lin->ret));
    _rxode2parse_unprotect();
    err_trans("need an 'alpha' with 'aob'");
  }
  if (lin->beta == -1) {
    sFree(&(lin->ret0));
    sFree(&(lin->ret));
    _rxode2parse_unprotect();
    err_trans("need a 'beta' with 'aob'");
  }
  sAppend(&(lin->ret0), "%d, %s", lin->trans, lin->mid);
  sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->alpha)));
  sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->v)));
  sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->beta)));
  sAppend(&(lin->ret0), "%s, 0.0, 0.0, ", CHAR(STRING_ELT(lin->vars, lin->aob)));
  if (verbose) RSprintf(_("detected %d-compartment model in terms of 'alpha' and 'aob'"), lin->ncmt);
}

static inline void linCmtParseTransAlphaBeta(linCmtStruct *lin, int verbose) {
  lin->ncmt =2;
  if (lin->beta == -1) {
    sFree(&(lin->ret0));
    sFree(&(lin->ret));
    _rxode2parse_unprotect();
    err_trans("need a 'beta'");
  }
  if (lin->b == -1) {
    sFree(&(lin->ret0));
    sFree(&(lin->ret));
    _rxode2parse_unprotect();
    err_trans("need a 'b'");
  }
  sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->beta)));
  sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->b)));
  if (lin->gamma != -1 || lin->c != -1) {
    lin->ncmt = 3;
    if (lin->gamma == -1) {
      sFree(&(lin->ret0));
      sFree(&(lin->ret));
      err_trans("need a 'gamma'");
    }
    if (lin->c == -1) {
      sFree(&(lin->ret0));
      sFree(&(lin->ret));
      _rxode2parse_unprotect();
      err_trans("need a 'c'");
    }
    sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->gamma)));
    sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->c)));
  } else {
    sAppendN(&(lin->ret0), "0.0, 0.0, ", 10);
  }
}

static inline void linCmtParseTransAlpha(linCmtStruct *lin, int verbose) {
  lin->ncmt = 1;
  if (lin->a != -1){
    lin->trans = 10;
  } else {
    lin->trans = 11;
  }
  sAppend(&(lin->ret0), "%d, %s", lin->trans, lin->mid);
  sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->alpha)));
  if (lin->a != -1) {
    sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->a)));
  } else {
    if (lin->v == -1) {
      sFree(&(lin->ret0));
      sFree(&(lin->ret));
      _rxode2parse_unprotect();
      err_trans("cannot figure out a central volume");
    }
    sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->v)));
  }
  if (lin->beta != -1 || lin->b != -1) {
    linCmtParseTransAlphaBeta(lin, verbose);
  } else if (lin->gamma != -1 || lin->c != -1) {
    sFree(&(lin->ret0));
    sFree(&(lin->ret));
    _rxode2parse_unprotect();
    err_trans("a 'gamma' or 'c' specified without 'b' or 'beta'");
  } else {
    sAppendN(&(lin->ret0), "0.0, 0.0, 0.0, 0.0, ", 20);
  }
  if (verbose) {
    if (lin->a != -1){
      RSprintf(_("detected %d-compartment model in terms of 'alpha' and central volume"), lin->ncmt);
    } else {
      RSprintf(_("detected %d-compartment model in terms of 'alpha' and 'a'"), lin->ncmt);
    }
  }
}
