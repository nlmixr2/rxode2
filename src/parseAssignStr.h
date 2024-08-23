static inline int new_assign_str(const char *s) {
  if (!strcmp("cmt", s)) {
    _rxode2parse_unprotect();
    err_trans("'cmt' cannot be a state or lhs expression");
  }
  if (!strcmp("dvid", s)) {
    _rxode2parse_unprotect();
    err_trans("'dvid' cannot be a state or lhs expression");
  }
  if (!strcmp("addl", s)) {
    _rxode2parse_unprotect();
    err_trans("'addl' cannot be a state or lhs expression");
  }
  if (!strcmp("ii", s)) {
    _rxode2parse_unprotect();
    err_trans("'ii' cannot be a state or lhs expression");
  }
  if (!strcmp("ss", s)){
    _rxode2parse_unprotect();
    err_trans("'ss' cannot be a state or lhs expression");
  }
  if (!strcmp("amt", s)) {
    _rxode2parse_unprotect();
    err_trans("'amt' cannot be a state or lhs expression");
  }
  if (!strcmp("dur", s)) {
    _rxode2parse_unprotect();
    err_trans("'dur' cannot be a state or lhs expression");
  }
  if (!strcmp("rate", s)) {
    _rxode2parse_unprotect();
    err_trans("'rate' cannot be a state or lhs expression");
  }
  if (!strcmp("Rprintf", s)) {
    _rxode2parse_unprotect();
    err_trans("'Rprintf' cannot be a state or lhs expression");
  }
  if (!strcmp("printf", s)){
    _rxode2parse_unprotect();
    err_trans("'printf' cannot be a state or lhs expression");
  }
  if (!strcmp("print", s)) {
    _rxode2parse_unprotect();
    err_trans("'print' cannot be a state or lhs expression");
  }
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

static inline int handleStrAssign(nodeInfo ni, char *name, int i, D_ParseNode *pn, D_ParseNode *xpn) {
  if (nodeHas(assign_str)) {
    if (i==0) {
      // assign lhs
      char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      new_or_ith(v); // update tb.ix for the right value
      tb.lh[tb.ix] = isLHSstr;
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
  if (nodeHas(derivative)){
    addLine(&sbPm,     "%s\n", sb.s);
    addLine(&sbPmDt,   "%s\n", sbDt.s);
    sAppend(&sbNrm, "%s\n", sbt.s);
    addLine(&sbNrmL, "%s\n", sbt.s);
    ENDLINE;
    return 1;
  }
  return 0;
}
