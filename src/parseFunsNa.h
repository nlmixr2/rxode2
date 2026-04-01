static inline int handleFunctionIsNan(transFunctions *tf) {
  if (!strcmp("is.nan", tf->v)) {
    int ii = d_get_number_of_children(d_get_child(tf->pn,3))+1;
    D_ParseNode *xpn = d_get_child(tf->pn, 2);
    char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    int allSpace=allSpaces(v2);
    /* Free(v2); */
    if (ii != 1 || (ii == 1 && allSpace)) {
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("'is.nan' takes 1 argument"));
    }
    sAppendN(&sb, "isnan", 5);
    sAppendN(&sbDt, "isnan", 5);
    sAppendN(&sbt, "is.nan", 6);
    return 1;
  }
  return 0;
}

static inline int handleFunctionIsNa(transFunctions *tf) {
  if (!strcmp("is.na", tf->v)) {
    int ii = d_get_number_of_children(d_get_child(tf->pn,3))+1;
    D_ParseNode *xpn = d_get_child(tf->pn, 2);
    char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    int allSpace=allSpaces(v2);
    /* Free(v2); */
    if (ii != 1 || (ii == 1 && allSpace)) {
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("'is.na' takes 1 argument"));
    }
    sAppendN(&sb, "ISNA", 4);
    sAppendN(&sbDt, "ISNA", 4);
    sAppendN(&sbt, "is.na", 5);
    return 1;
  }
  return 0;
}

static inline int handleFunctionIsFinite(transFunctions *tf) {
  if (!strcmp("is.finite", tf->v)) {
    int ii = d_get_number_of_children(d_get_child(tf->pn,3))+1;
    D_ParseNode *xpn = d_get_child(tf->pn, 2);
    char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    int allSpace=allSpaces(v2);
    /* Free(v2); */
    if (ii != 1 || (ii == 1 && allSpace)) {
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("'is.finite' takes 1 argument"));
    }
    sAppendN(&sb, "R_FINITE", 8);
    sAppendN(&sbDt, "R_FINITE", 8);
    sAppendN(&sbt, "is.finite", 9);
    return 1;
  }
  return 0;
}

static inline int handleFunctionIsInfinite(transFunctions *tf) {
  if (!strcmp("is.infinite", tf->v)) {
    int ii = d_get_number_of_children(d_get_child(tf->pn,3))+1;
    D_ParseNode *xpn = d_get_child(tf->pn, 2);
    char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    int allSpace=allSpaces(v2);
    /* Free(v2); */
    if (ii != 1 || (ii == 1 && allSpace)) {
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("'is.infinite' takes 1 argument"));
    }
    if (sbt.o > 0 && sbt.s[sbt.o-1] == '!'){
      sb.o--;sbDt.o--;
      sAppendN(&sb, "R_FINITE", 8);
      sAppendN(&sbDt, "R_FINITE", 8);
    } else {
      sAppendN(&sb, "!R_FINITE", 9);
      sAppendN(&sbDt, "!R_FINITE", 9);
    }
    sAppendN(&sbt, "is.infinite", 11);
    return 1;
  }
  return 0;
}
