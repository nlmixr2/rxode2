static inline int new_assign_str(const char *s) {
  parseAllowAssignOrState(s);
  for (int i=0; i<tb.str.n; i++) {
    if (!strcmp(tb.str.line[i], s)) {
      tb.id = i;
      return 0;
    }
  }
  if (tb.str.n + 1 > tb.allocS){
    tb.allocS+=MXDER;
    tb.si=R_Realloc(tb.si, tb.allocS, int);
    tb.isi=R_Realloc(tb.isi, tb.allocS, int);
    tb.sin=R_Realloc(tb.sin, tb.allocS, int);
  }
  return 1;
}

static inline void add_assign_str(char *v) {
  tb.id=tb.str.n;
  tb.si[tb.str.n] = tb.ix;
  tb.isi[tb.str.n] = 0; // variable is not ignored by default
  tb.sin[tb.str.n] = 0; // No values added yet
  addLine(&(tb.str),"%s",v);
}


static inline int get_str_assign_int(int val, const char *s) {
  for (int i=0; i<tb.strVal.n; i++) {
    if (tb.strValI[i] == val && !strcmp(tb.strVal.line[i], s)) {
      return tb.strValII[i];
    }
  }
  if (tb.strVal.n + 1 > tb.allocSV){
    tb.allocSV+=MXDER;
    tb.strValI=R_Realloc(tb.strValI, tb.allocSV, int);
    tb.strValII=R_Realloc(tb.strValII, tb.allocSV, int);
  }
  int n = tb.sin[tb.id];
  n++;
  tb.strValI[tb.strVal.n] = val;
  tb.strValII[tb.strVal.n] = n; // R factors start with 1 instead of 0
  tb.sin[tb.id] = n;
  addLine(&(tb.strVal),"%s", s);
  return n;
}

static inline void errorStrAssign(const char *v) {
  new_assign_str(v);
  int n = tb.sin[tb.id];
  if (n > 1) {
    sPrint(&_gbuf,"the string variable '%s' can only be 1 to %d, or '",v, n);
  } else {
    sPrint(&_gbuf,"the string variable '%s' can only be 1 or '",v, n);
  }
  for (int i=0; i<tb.strVal.n; i++) {
    if (tb.strValI[i] == tb.id) {
      sAppend(&_gbuf, "%s', '", tb.strVal.line[i]);
    }
  }
  _gbuf.o-=3;
  _gbuf.s[_gbuf.o]=0;
  updateSyntaxCol();
  trans_syntax_error_report_fn(_gbuf.s);
}

static inline int handleStrAssign(nodeInfo ni, char *name, int i, D_ParseNode *pn, D_ParseNode *xpn) {
  if (nodeHas(assign_str)) {
    if (i==0) {
      // assign lhs
      char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      if (!new_de(v, 0)) {
        add_de(ni, name, v, isCmtLhsStatement(ni, name, v), fromCMTprop);
        sPrint(&_gbuf,"'%s' compartment cannot be a string variable", v);
        updateSyntaxCol();
        trans_syntax_error_report_fn(_gbuf.s);
        return 0;
      }
      new_or_ith(v); // update tb.ix for the right value
      if (tb.lh[tb.ix]!= 0 && tb.lh[tb.ix] != isLHSstr && tb.lh[tb.ix] != isSuppressedLHSstr) {
        sPrint(&_gbuf,"'%s' cannot be both a calculated and a string variable", v);
        updateSyntaxCol();
        trans_syntax_error_report_fn(_gbuf.s);
        return 0;
      }
      aProp(tb.ix);
      aType(TASSIGN);
      if (tb.lh[tb.ix] == 0 && tb.ini[tb.ix] == 0) {
        tb.lh[tb.ix] = isLHSstr;
      } else if (tb.lh[tb.ix] == isLHSstr ||
                 tb.lh[tb.ix] == isSuppressedLHSstr) {
      } else {
        sPrint(&_gbuf,"'%s' needs to be first declared as a string by assignment or labels()", v);
        updateSyntaxCol();
        trans_syntax_error_report_fn(_gbuf.s);
      }
      if (new_assign_str(v)){
        add_assign_str(v);
      }
      return 1;
    } else if (i == 1) {
      // variable type
      char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      if (v[0] == '~') {
        tb.isi[tb.id] = 1; // variable ignored
        tb.lh[tb.ix] = isSuppressedLHSstr;
      }
      return 1;
    } else if (i == 2) {
      // actual string
      sb.o =0; sbDt.o =0; sbt.o =0;
      sAppend(&sb, "%s = ", tb.str.line[tb.id]);
      sAppend(&sbDt, "%s = ", tb.str.line[tb.id]);
      sAppend(&sbt, "%s ", tb.str.line[tb.id]);
      if (tb.isi[tb.id]) {
        // ignored variable
        sAppendN(&sbt, "~", 1);
      } else {
        // output variable
        sAppendN(&sbt, "<-", 2);
      }
      char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      sAppend(&sbt,"%s;", v);
      // take out quotes
      v++;
      v[strlen(v)-1]=0;
      int n = get_str_assign_int(tb.id, v);
      sAppend(&sb, "%d; /* = '%s' */", n, v);
      sAppend(&sbDt, "%d; /* = '%s' */ ", n, v);
      char *buf = tb.str.line[tb.id];
      //addSymbolStr(buf);
      if (tb.isi[tb.id]) {
        // ignored
        tb.lh[tb.ix] = isSuppressedLHSstr;
      } else {
        // not ignored
        tb.lh[tb.ix] = isLHSstr;
      }
      return 1;
    }
  }
  return 0;
}

static inline int finalizeLineStrAssign(nodeInfo ni, char *name) {
  if (nodeHas(assign_str)){
    addLine(&sbPm,     "%s\n", sb.s);
    addLine(&sbPmDt,   "%s\n", sbDt.s);
    sAppend(&sbNrm, "%s\n", sbt.s);
    addLine(&sbNrmL, "%s\n", sbt.s);
    ENDLINE;
    return 1;
  }
  return 0;
}
