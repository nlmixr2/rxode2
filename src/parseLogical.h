////////////////////////////////////////////////////////////////////////////////
// parsing properties (logical expressions)
static inline int isIdentifier(nodeInfo ni, const char *name) {
  return nodeHas(identifier) || nodeHas(identifier_r) ||
    nodeHas(identifier_r_no_output)  ||
    nodeHas(theta0_noout) ||
    nodeHas(theta0);
}

static inline int isTbsVar(const char *value) {
  return !strcmp("rx_lambda_", value) || !strcmp("rx_yj_", value) ||
    !strcmp("rx_low_", value) || !strcmp("rx_hi_", value);
}

static inline int isDefiningParameterRecursively(const char *value) {
  return tb.ix == tb.ixL && tb.didEq==1 &&
    !strcmp(value, tb.ss.line[tb.ix]);
}

static inline int isOperatorOrPrintingIdentifier(nodeInfo ni, const char *name){
  return nodeHas(identifier) ||
    nodeHas(identifier_r) ||
    nodeHas(constant) ||
    nodeHas(theta0) ||
    !strcmp("+", name) ||
    !strcmp("-", name) ||
    !strcmp("*", name) ||
    !strcmp("/", name) ||

    !strcmp("&&", name) ||
    !strcmp("||", name) ||
    !strcmp("!=", name) ||
    !strcmp("==", name) ||
    !strcmp("<=", name) ||
    !strcmp(">=", name) ||
    !strcmp("!", name) ||
    !strcmp("<", name) ||
    !strcmp(">", name);
}

static inline int isSkipChild(nodeInfo ni, const char *name, int i) {
  return ((i == 3 || i == 4 || i < 2) &&
	  (nodeHas(derivative) ||nodeHas(fbio) || nodeHas(alag) ||
	   nodeHas(rate) || nodeHas(dur))) ||
    ((i == 3 || i < 2) && nodeHas(der_rhs)) ||
    (nodeHas(dfdy)     && i< 2)  ||
    (nodeHas(dfdy_rhs) && i< 2) ||
    (nodeHas(dfdy)     && i == 3) ||
    (nodeHas(dfdy_rhs) && i == 3) ||
    (nodeHas(dfdy)     && i == 5) ||
    (nodeHas(dfdy_rhs) && i == 5) ||
    (nodeHas(dfdy)     && i == 6) ||
    (nodeHas(ini0)     && i == 1) ||
    (nodeHas(dvid_statementI) && i != 0) ||
    ((nodeHas(theta) || nodeHas(eta)) && i != 2) ||
    (nodeHas(mtime) && (i == 0 || i == 1 || i == 3)) ||
    (nodeHas(cmt_statement) && (i == 0 || i == 1 || i == 3)) ||
    (i != 2 && (nodeHas(mat0) || nodeHas(matF)));
}

static inline int handleIfElse(nodeInfo ni, char *name, int i) {
  if (nodeHas(ifelse)){
    if (i == 0){
      return 1;
    } else if (i == 1){
      aAppendN("((", 2);
      sAppendN(&sbt,"ifelse(", 7);
      return 1;
    } else if (i == 3){
      aAppendN(") ? (", 5);
      sAppendN(&sbt,",", 1);
      return 1;
    } else if (i == 5){
      aAppendN(") : (", 5);
      sAppendN(&sbt,",", 1);
      return 1;
    } else if (i == 7){
      aAppendN("))", 2);
      sAppendN(&sbt,")", 1);
      return 1;
    }
  }
  if (nodeHas(ifelse_statement)){
    if (i == 0){
      return 1;
    } else if (i == 1){
      aAppendN("if (", 4);
      sAppendN(&sbt, "if (", 4);
      return 1;
    } else if (i == 3){
      aType(TLOGIC);
      aAppendN(") {", 3);
      sAppendN(&sbt,") {", 3);
      addLine(&sbPm, "%s\n", sb.s);
      addLine(&sbPmDt, "%s\n", sbDt.s);
      sAppend(&sbNrm, "%s\n", sbt.s);
      addLine(&sbNrmL, "%s\n", sbt.s);
      sb.o=0;sbDt.o=0; sbt.o=0;
      return 1;
    } else if (i == 5){
      sb.o=0;sbDt.o=0; sbt.o=0;
      aType(TLOGIC);
      aAppendN("}\nelse {", 8);
      sAppendN(&sbt,"}\nelse {", 1);
      addLine(&sbPm, "%s\n", sb.s);
      addLine(&sbPmDt, "%s\n", sbDt.s);
      sAppend(&sbNrm, "%s\n", sbt.s);
      addLine(&sbNrmL, "%s\n", sbt.s);
      return 1;
    } else if (i == 7){
      sb.o=0;sbDt.o=0; sbt.o=0;
      aType(TLOGIC);
      aAppendN("}", 1);
      sAppendN(&sbt,"}", 1);
      addLine(&sbPm, "%s\n", sb.s);
      addLine(&sbPmDt, "%s\n", sbDt.s);
      sAppend(&sbNrm, "%s\n", sbt.s);
      addLine(&sbNrmL, "%s\n", sbt.s);
      return 1;
    } else if (i == 8){
      return 1;
    }
  }
  return 0;
}

static inline int handleStringEqualRhs(nodeInfo ni, char *name, int i, D_ParseNode *xpn) {
  if (nodeHas(equality_str1)){
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    switch(i) {
    case 0:
      // string
      aAppendN("_cmp1(", 6);
      sAppend(&sb, "%s, ", v);
      sAppend(&sbDt, "%s, ", v);
      sAppend(&sbt, "%s", v);
      /* Free(v); */
      return 1;
    case 1:
      if (!strcmp(v, "==")) {
	aAppendN("1, ", 3);
      } else {
	aAppendN("0, ", 3);
      }
      sAppend(&sbt, "%s", v);
      /* Free(v); */
      return 1;
    case 2:
      // identifier_r
      // val, valstr
      if (!strcmp(v, "id") || !strcmp(v, "ID") || !strcmp(v, "Id")){
	aAppendN("(&_solveData->subjects[_cSub])->idReal, \"ID\")", 45);
	sAppendN(&sbt, "ID", 2);
      } else {
	if (new_or_ith(v)) addSymbolStr(v);
	sAppend(&sb, "%s, \"%s\")", v, v);
	sAppend(&sbDt, "%s, \"%s\")", v, v);
	sAppend(&sbt, "%s", v);
      }
      return 1;
    }
  }
  return 0;
}

static inline int handleStringEqualLhs(nodeInfo ni, char *name, int i, D_ParseNode *xpn) {
  if (nodeHas(equality_str2)){
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    switch(i) {
    case 0:
      aAppendN("_cmp2(", 6);
      if (!strcmp(v, "id") || !strcmp(v, "ID") || !strcmp(v, "Id")){
	aAppendN("(&_solveData->subjects[_cSub])->idReal, \"ID\", ", 46);
	sAppendN(&sbt, "ID", 2);
      } else {
	if (new_or_ith(v)) addSymbolStr(v);
	sAppend(&sb, "%s, \"%s\", ", v, v);
	sAppend(&sbDt, "%s, \"%s\", ", v, v);
	sAppend(&sbt, "%s", v);
      }
      return 1;
    case 1:
      if (!strcmp(v, "==")) {
	aAppendN("1, ", 3);
      } else {
	aAppendN("0, ", 3);
      }
      sAppend(&sbt, "%s", v);
      return 1;
    case 2:
      sAppend(&sb, "%s)", v);
      sAppend(&sbDt, "%s)", v);
      sAppend(&sbt, "%s", v);
      return 1;
    }
  }
  return 0;
}

static inline int handleStringEqualityStatements(nodeInfo ni, char *name, int i, D_ParseNode *xpn) {
  return handleStringEqualRhs(ni, name, i, xpn) ||
    handleStringEqualLhs(ni, name, i, xpn);
}

static inline int assertLogicalNoWhileElse(nodeInfo ni, char *name, int i, D_ParseNode *pn, D_ParseNode *xpn, int *isWhile) {
  if (nodeHas(selection_statement) && i== 0 ) {
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    *isWhile = !strcmp("while", v);
    /* Free(v); */
    if (*isWhile) {
      D_ParseNode *xpn2 = d_get_child(pn, 5);
      v = (char*)rc_dup_str(xpn2->start_loc.s, xpn2->end);
      if (v[0] == 0) {
      } else {
	updateSyntaxCol();
	trans_syntax_error_report_fn(_("'while' cannot be followed by 'else' (did you mean 'if'/'else')"));
      }
    }
    return 1;
  }
  return 0;
}

static inline int handleLogicalIfOrWhile(nodeInfo ni, char *name, int i, D_ParseNode *pn, D_ParseNode *xpn, int *isWhile) {
  if (nodeHas(selection_statement) && i==1) {
    sb.o = 0; sbDt.o = 0; sbt.o = 0;
    if (*isWhile) {
      sAppendN(&sb, "_itwhile=0;\nwhile (", 19);
      sAppendN(&sbDt, "_itwhile=0;\nwhile (", 19);
      sAppendN(&sbt,"while (", 7);
      tb.nwhile++;
    } else {
      sAppendN(&sb, "if (", 4);
      sAppendN(&sbDt, "if (", 4);
      sAppendN(&sbt,"if (", 4);
    }
    return 1;
  }
  return 0;
}

static inline int handleLogicalBreak(nodeInfo ni, char *name, int i, D_ParseNode *pn, D_ParseNode *xpn, int *isWhile) {
  if (nodeHas(break_statement) && i == 0) {
    if (tb.nwhile > 0) {
      aType(TLOGIC);
      sb.o = 0; sbDt.o = 0; sbt.o = 0;
      /* aType(100); */
      aAppendN("break;", 6);
      sAppendN(&sbt, "break;", 6);
      addLine(&sbPm, "%s\n", sb.s);
      addLine(&sbPmDt, "%s\n", sbDt.s);
      sAppend(&sbNrm, "%s\n", sbt.s);
      addLine(&sbNrmL, "%s\n", sbt.s);
      ENDLINE;
    } else {
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("'break' can only be used in  'while' statement"));
    }
    return 1;
  }
  return 0;
}

static inline int handleLogicalBeginParen(nodeInfo ni, char *name, int i, D_ParseNode *pn, D_ParseNode *xpn, int *isWhile) {
  if (nodeHas(selection_statement) && i==3) {
    aType(TLOGIC);
    /* aType(100); */
    aAppendN("{", 1);
    sAppendN(&sbt, "{", 1);
    addLine(&sbPm, "%s\n", sb.s);
    addLine(&sbPmDt, "%s\n", sbDt.s);
    sAppend(&sbNrm, "%s\n", sbt.s);
    addLine(&sbNrmL, "%s\n", sbt.s);
    ENDLINE;
    return 1;
  }
  return 0;
}

static inline int handleLogicalElse(nodeInfo ni, char *name, int i, D_ParseNode *pn, D_ParseNode *xpn, int *isWhile) {
  if (nodeHas(selection_statement__9) && i==0) {
    sb.o = 0; sbDt.o = 0; sbt.o = 0;
    aType(TLOGIC);
    aAppendN("}\nelse {", 8);
    sAppendN(&sbt,"}\nelse {", 8);
    addLine(&sbPm, "%s\n", sb.s);
    addLine(&sbPmDt, "%s\n", sbDt.s);
    sAppend(&sbNrm, "%s\n", sbt.s);
    addLine(&sbNrmL, "%s\n", sbt.s);
    ENDLINE;
    return 1;
  }
  return 0;
}

static inline int handleLogicalExpr(nodeInfo ni, char *name, int i, D_ParseNode *pn, D_ParseNode *xpn, int *isWhile) {
  int tmp = assertLogicalNoWhileElse(ni, name, i, pn, xpn, isWhile) ||
    handleLogicalIfOrWhile(ni, name, i, pn, xpn, isWhile) ||
    handleLogicalBreak(ni, name, i, pn, xpn, isWhile) ||
    handleLogicalBeginParen(ni, name, i, pn, xpn, isWhile) ||
    handleLogicalElse(ni, name, i, pn, xpn, isWhile);
  (void)tmp;
  return 0;
}

static inline int finalizeLineSelectionStatement(nodeInfo ni, char *name, int isWhile) {
  if (nodeHas(selection_statement)){
    sb.o = 0; sbDt.o = 0; sbt.o = 0;
    aType(TLOGIC);
    /* aType(300); */
    if (isWhile) {
      sAppendN(&sb,   "if (_itwhile > _solveData->maxwhile) {_solveData->whileexit=1;break;}\n}\n", 72);
      sAppendN(&sbDt, "if (_itwhile > _solveData->maxwhile) {_solveData->whileexit=1;break;}\n}\n", 72);
      sAppendN(&sbt, "}", 1);
    } else {
      sAppendN(&sb, "}", 1);
      sAppendN(&sbDt, "}", 1);
      sAppendN(&sbt, "}", 1);
    }
    addLine(&sbPm,   "%s\n", sb.s);
    addLine(&sbPmDt, "%s\n", sbDt.s);
    sAppend(&sbNrm,  "%s\n", sbt.s);
    addLine(&sbNrmL, "%s\n", sbt.s);
    ENDLINE;
    return 1;
  }
  return 0;
}
