static inline void linCmtParseTransClVss(linCmtStruct *lin, int verbose) {
  lin->ncmt = 2;
  lin->trans = 3;
  if (lin->vStyle != -1) {
    errLin[0] = '\0';
    errOff=0;
    snprintf(errLin + errOff, errLinLen-errOff, "cannot mix 'Vss' and '");
    errOff+=22;
    linCmtVStr(lin->vStyle);
    snprintf(errLin + errOff, errLinLen-errOff, "' volumes");
    errOff+=9;
    sFree(&(lin->ret0));
    sFree(&(lin->ret));
    _rxode2parse_unprotect();
    err_trans(errLin);
  }
  if (lin->v == -1) {
    sFree(&(lin->ret0));
    sFree(&(lin->ret));
    _rxode2parse_unprotect();
    err_trans("cannot figure out a central volume");
  }
  if (lin->cl2 == -1) {
    sFree(&(lin->ret0));
    sFree(&(lin->ret));
    _rxode2parse_unprotect();
    err_trans("cannot figure out distributional clearance");
  }
  sAppend(&(lin->ret0), "%d, %s", lin->trans, lin->mid);
  sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->cl)));
  sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->v)));
  sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->cl2)));
  sAppend(&(lin->ret0), "%s, 0.0, 0.0, ", CHAR(STRING_ELT(lin->vars, lin->vss)));
}

static inline void linCmtParseTransClV(linCmtStruct *lin, int verbose) {
  if (lin->v == -1) {
    sFree(&(lin->ret0));
    sFree(&(lin->ret));
    _rxode2parse_unprotect();
    err_trans("cannot figure out a central volume");
  }
  lin->ncmt = 1;
  lin->trans = 1;
  sAppend(&(lin->ret0), "%d, %s", lin->trans, lin->mid);
  sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->cl)));
  sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->v)));
  if (lin->v2 != -1 || lin->cl2 != -1) {
    lin->ncmt = 2;
    if (lin->cl2 == -1) {
      sFree(&(lin->ret0));
      sFree(&(lin->ret));
      _rxode2parse_unprotect();
      err_trans("cannot figure out distributional clearance");
    }
    if (lin->v2 == -1) {
      sFree(&(lin->ret0));
      sFree(&(lin->ret));
      _rxode2parse_unprotect();
      err_trans("cannot figure out distributional volume");
    }
    sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->cl2)));
    sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->v2)));
    if (lin->v3 != -1 || lin->cl3 != -1) {
      lin->ncmt = 3;
      if (lin->cl3 == -1) {
        sFree(&(lin->ret0));
        sFree(&(lin->ret));
        _rxode2parse_unprotect();
        err_trans("cannot figure out 2nd distributional clearance");
      }
      if (lin->v3 == -1) {
        sFree(&(lin->ret0));
        sFree(&(lin->ret));
        _rxode2parse_unprotect();
        err_trans("cannot figure out 2nd distributional volume");
      }
      sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->cl3)));
      sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->v3)));
    } else {
      sAppendN(&(lin->ret0), "0.0, 0.0, ", 10);
    }
  } else {
    sAppendN(&(lin->ret0), "0.0, 0.0, 0.0, 0.0, ", 20);
  }
}

static inline void linCmtParseTransCl(linCmtStruct *lin, int verbose) {
  lin->trans = 1;
  if (lin->vss != -1) {
    linCmtParseTransClVss(lin, verbose);
  } else {
    linCmtParseTransClV(lin, verbose);
  }
  if (verbose) RSprintf(_("Detected %d-compartment model in terms of clearance"), lin->ncmt);
}
