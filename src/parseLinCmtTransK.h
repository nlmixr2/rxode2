static inline void assertK12andK21mustBePresent(linCmtStruct *lin, int verbose) {
  if (lin->k12 == -1) {
    if (lin->cmtc == 1){
      sFree(&(lin->ret0));
      sFree(&(lin->ret));
      _rxode2parse_unprotect();
      err_trans("'k12' not found when 'k21' present");
    } else {
      sFree(&(lin->ret0));
      sFree(&(lin->ret));
      _rxode2parse_unprotect();
      err_trans("'k23' not found when 'k32' present");
    }
  }
  if (lin->k21 == -1) {
    if (lin->cmtc == 1){
      sFree(&(lin->ret0));
      sFree(&(lin->ret));
      _rxode2parse_unprotect();
      err_trans("'k21' not found when 'k12' present");
    } else {
      sFree(&(lin->ret0));
      sFree(&(lin->ret));
      _rxode2parse_unprotect();
      err_trans("'k32' not found when 'k23' present");
    }
  }
}

static inline void assertK13andK31mustBePresent(linCmtStruct *lin, int verbose) {
  if (lin->k13 == -1) {
    if (lin->cmtc == 1){
      sFree(&(lin->ret0));
      sFree(&(lin->ret));
      _rxode2parse_unprotect();
      err_trans("'k13' not found when 'k31' present");
    } else {
      sFree(&(lin->ret0));
      sFree(&(lin->ret));
      _rxode2parse_unprotect();
      err_trans("'k24' not found when 'k42' present");
    }
  }
  if (lin->k31 == -1) {
    if (lin->cmtc == 1){
      sFree(&(lin->ret0));
      sFree(&(lin->ret));
      _rxode2parse_unprotect();
      err_trans("'k31' not found when 'k13' present");
    } else {
      sFree(&(lin->ret0));
      sFree(&(lin->ret));
      _rxode2parse_unprotect();
      err_trans("'k42' not found when 'k24' present");
    }
  }
}

static inline void linCmtParseTranKelK12(linCmtStruct *lin, int verbose) {
  assertK12andK21mustBePresent(lin, verbose);
  lin->ncmt = 2;
  sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->k12)));
  sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->k21)));
  if (lin->k13 != -1 || lin->k31 != -1) {
    assertK13andK31mustBePresent(lin, verbose);
    lin->ncmt = 3;
    sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->k13)));
    sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->k31)));
  } else {
    sAppendN(&(lin->ret0), "0.0, 0.0, ", 10);
  }
}

static inline void linCmtParseTranKel(linCmtStruct *lin, int verbose) {
  if (lin->v == -1) {
    sFree(&(lin->ret0));
    sFree(&(lin->ret));
    _rxode2parse_unprotect();
    err_trans("cannot figure out a central volume");
  }
  lin->ncmt = 1;
  lin->trans = 2;
  sAppend(&(lin->ret0), "%d, %s", lin->trans, lin->mid);
  sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->kel)));
  sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->v)));
  if (lin->k12 != -1 || lin->k21 != -1) {
    linCmtParseTranKelK12(lin, verbose);
  } else if (lin->k31 != -1 || lin->k13 != -1){
    if (lin->cmtc == 1){
      sFree(&(lin->ret0));
      sFree(&(lin->ret));
      _rxode2parse_unprotect();
      err_trans("'k13' or 'k31' present when 'k12' and 'k21' not present");
    } else {
      sFree(&(lin->ret0));
      sFree(&(lin->ret));
      _rxode2parse_unprotect();
      err_trans("'k24' or 'k42' present when 'k23' and 'k32' not present");
    }
  } else {
    sAppendN(&(lin->ret0), "0.0, 0.0, 0.0, 0.0, ", 20);
  }
  if (verbose) RSprintf(_("detected %d-compartment model in terms of micro-constants"), lin->ncmt);
}

static inline void linCmtParseTransK21(linCmtStruct *lin, int verbose) {
  if (lin->gamma != -1) {
    lin->ncmt = 3;
    err_trans("'alpha', 'beta', 'gamma', 'k21', 'k31', 'vc' OR\n'alpha', 'beta', 'gamma', 'k32', 'k42', 'vc'\nnot (yet) supported");
  } else {
    lin->ncmt = 2;
    lin->trans = 4;
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
      err_trans("need an 'alpha'");
    }
    if (lin->beta == -1) {
      sFree(&(lin->ret0));
      sFree(&(lin->ret));
      _rxode2parse_unprotect();
      err_trans("need a 'beta'");
    }
    sAppend(&(lin->ret0), "%d, %s", lin->trans, lin->mid);
    sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->alpha)));
    sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->v)));
    sAppend(&(lin->ret0), "%s, ", CHAR(STRING_ELT(lin->vars, lin->beta)));
    sAppend(&(lin->ret0), "%s, 0.0, 0.0, ", CHAR(STRING_ELT(lin->vars, lin->k21)));
    if (verbose) {
      if (lin->cmtc == 1) {
        RSprintf(_("detected %d-compartment model in terms of 'alpha' or 'k21'"), lin->ncmt);
      } else {
        RSprintf(_("detected %d-compartment model in terms of 'alpha' or 'k32'"), lin->ncmt);
      }
    }
  }
}
