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
           nodeHas(rate) || nodeHas(dur) || nodeHas(indLin_prop))) ||
    // past(state, tau) <- expr : visit child 2 (state) and child 7 (expr);
    // skip 'past' '(' , tau ) and the assignment operator
    (nodeHas(past) && (i < 2 || i == 3 || i == 4 || i == 5 || i == 6)) ||
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

static inline void clearStringCmpCurrent(void) {
  tb.strCmpCurCov = NULL;
  tb.strCmpCurStr = NULL;
  tb.strCmpCurType = -1;
}

static inline int isStringCmpId(const char *v) {
  return !strcmp(v, "id") || !strcmp(v, "ID") || !strcmp(v, "Id");
}

static inline void normalizeStringCmpValue(char *v) {
  size_t n = strlen(v);
  if (n >= 2 && ((v[0] == '"' && v[n-1] == '"') ||
                 (v[0] == '\'' && v[n-1] == '\''))) {
    memmove(v, v+1, n-2);
    v[n-2] = 0;
  }
}

static inline int getStringCmpCovIndex(const char *cov) {
  for (int i = 0; i < tb.strCmp.n; ++i) {
    if (!strcmp(tb.strCmp.line[i], cov)) {
      return i;
    }
  }
  if (tb.strCmp.n + 1 > tb.allocSC) {
    tb.allocSC += MXDER;
    tb.strCmpN = R_Realloc(tb.strCmpN, tb.allocSC, int);
  }
  addLine(&(tb.strCmp), "%s", cov);
  tb.strCmpN[tb.strCmp.n-1] = 0;
  return tb.strCmp.n-1;
}

static inline int addStringCmpValue(const char *cov, char *val) {
  int covIndex = getStringCmpCovIndex(cov);
  int valueIndex = 0;
  char *normVal = R_Calloc(strlen(val) + 1, char);
  strcpy(normVal, val);
  normalizeStringCmpValue(normVal);
  for (int i = 0; i < tb.strCmpVal.n; ++i) {
    if (tb.strCmpValI[i] == covIndex) {
      valueIndex++;
      if (!strcmp(tb.strCmpVal.line[i], normVal)) {
        R_Free(normVal);
        return valueIndex;
      }
    }
  }
  if (tb.strCmpVal.n + 1 > tb.allocSCV) {
    tb.allocSCV += MXDER;
    tb.strCmpValI = R_Realloc(tb.strCmpValI, tb.allocSCV, int);
  }
  tb.strCmpValI[tb.strCmpVal.n] = covIndex;
  tb.strCmpN[covIndex] += 1;
  addLine(&(tb.strCmpVal), "%s", normVal);
  R_Free(normVal);
  return valueIndex + 1;
}

static inline int handleStringEqualRhs(nodeInfo ni, char *name, int i, D_ParseNode *xpn) {
  if (nodeHas(equality_str1)){
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    switch(i) {
    case 0:
      // string
      clearStringCmpCurrent();
      tb.strCmpCurStr = v;
      sAppend(&sbt, "%s", v);
      /* Free(v); */
      return 1;
    case 1:
      if (!strcmp(v, "==")) {
        tb.strCmpCurType = 1;
      } else {
        tb.strCmpCurType = 0;
      }
      sAppend(&sbt, "%s", v);
      /* Free(v); */
      return 1;
    case 2:
      // identifier_r
      // val, valstr
      tb.strCmpCurCov = v;
      if (isStringCmpId(v)){
        aAppendN("_cmp1(", 6);
        sAppend(&sb, "%s, %d, (&_solveData->subjects[_cSub])->idReal, \"ID\")",
                tb.strCmpCurStr, tb.strCmpCurType);
        sAppend(&sbDt, "%s, %d, (&_solveData->subjects[_cSub])->idReal, \"ID\")",
                tb.strCmpCurStr, tb.strCmpCurType);
        sAppendN(&sbt, "ID", 2);
        clearStringCmpCurrent();
      } else {
        if (new_or_ith(v)) addSymbolStr(v);
        int cmpInt = 0;
        if (tb.strCmpCurStr != NULL) {
          cmpInt = addStringCmpValue(tb.strCmpCurCov, (char*)tb.strCmpCurStr);
        }
        aAppendN("_cmp1d(", 7);
        sAppend(&sb, "%s, %d, %s, %d)", tb.strCmpCurStr, tb.strCmpCurType, v, cmpInt);
        sAppend(&sbDt, "%s, %d, %s, %d)", tb.strCmpCurStr, tb.strCmpCurType, v, cmpInt);
        sAppend(&sbt, "%s", v);
        clearStringCmpCurrent();
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
      clearStringCmpCurrent();
      tb.strCmpCurCov = v;
      if (isStringCmpId(v)){
        sAppendN(&sbt, "ID", 2);
      } else {
        if (new_or_ith(v)) addSymbolStr(v);
        sAppend(&sbt, "%s", v);
      }
      return 1;
    case 1:
      if (!strcmp(v, "==")) {
        tb.strCmpCurType = 1;
      } else {
        tb.strCmpCurType = 0;
      }
      sAppend(&sbt, "%s", v);
      return 1;
    case 2:
      tb.strCmpCurStr = v;
      if (isStringCmpId(tb.strCmpCurCov)) {
        aAppendN("_cmp2(", 6);
        sAppend(&sb, "(&_solveData->subjects[_cSub])->idReal, \"ID\", %d, %s)",
                tb.strCmpCurType, v);
        sAppend(&sbDt, "(&_solveData->subjects[_cSub])->idReal, \"ID\", %d, %s)",
                tb.strCmpCurType, v);
      } else {
        int cmpInt = 0;
        if (tb.strCmpCurCov != NULL) {
          cmpInt = addStringCmpValue(tb.strCmpCurCov, v);
        }
        aAppendN("_cmp2d(", 7);
        sAppend(&sb, "%s, \"%s\", %d, %d)",
                tb.strCmpCurCov, tb.strCmpCurCov, tb.strCmpCurType, cmpInt);
        sAppend(&sbDt, "%s, \"%s\", %d, %d)",
                tb.strCmpCurCov, tb.strCmpCurCov, tb.strCmpCurType, cmpInt);
      }
      sAppend(&sbt, "%s", v);
      clearStringCmpCurrent();
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
