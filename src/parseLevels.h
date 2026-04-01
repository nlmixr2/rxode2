static inline int handleLevelStr(nodeInfo ni, char *name, char *v) {
  if (tb.lvlStr == 1 && nodeHas(string)) {
    char *v2 = (char*)rc_dup_str(v, 0);
    v2++;
    v2[strlen(v2)-1]=0; // remove last quote
    get_str_assign_int(tb.id, v2);
    return 1;
  }
  return 0;
}

static inline int handleLevelsStr(nodeInfo ni, char *name, int i, D_ParseNode *pn, D_ParseNode *xpn) {
  if (nodeHas(levels_str)) {
    if (i == 0 || i == 1 || i == 3 || i == 5 || i == 6) {
      return 1;
    } else if (i == 2) {
      char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      new_or_ith(v); // update tb.ix for the right value
      aProp(tb.ix);
      aType(TNONE);
      tb.lh[tb.ix] = isLHSstr;
      if (tb.lho[tb.ix] == 0) {
        tb.lho[tb.ix] = tb.lhi++;
      }
      if (new_assign_str(v)){
        add_assign_str(v);
      }
      return 1;
    } else if (i == 4) {
      // variable type
      char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      if (v[0] == '~') {
        tb.isi[tb.id] = 1; // variable ignored
        tb.lh[tb.ix] = isSuppressedLHSstr;
      }
      tb.lvlStr = 1;
      return 1;
    } else if (i == 9) {
      // clear code and add line
      sbt.o = 0;
      tb.lvlStr = 0;
      tb.didEq=0; // reset the equation flag (to avoid double assign errors)
      sAppend(&sbt, "levels(%s) ", tb.str.line[tb.id]);
      if (tb.isi[tb.id]) {
        // ignored variable
        sAppendN(&sbt, "~ c(", 4);
      } else {
        // output variable
        sAppendN(&sbt, "<- c(", 5);
      }
      for (int i=0; i<tb.strVal.n; i++) {
        if (tb.strValI[i] == tb.id) {
          sAppend(&sbt, "\"%s\", ", tb.strVal.line[i]);
        }
      }
      sbt.o -= 2; // remove last comma
      sAppendN(&sbt, ");", 2);
      sb.o = sbDt.o = 0;
      sAppend(&sb, "/*  '%s' */", sbt.s);
      sAppend(&sbDt, "/*  '%s' */ ", sbt.s);
      return 1;
    }
    return 1;
  }
  return 0;
}

static inline int handleLevelsStr1(nodeInfo ni, char *name, int i, D_ParseNode *pn, D_ParseNode *xpn) {
  if (nodeHas(levels_str1)) {
    if (i == 0 || i == 1 || i == 3) {
      return 1;
    } else if (i == 2) {
      char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      new_or_ith(v); // update tb.ix for the right value
      tb.lh[tb.ix] = isLHSstr;
      if (tb.lho[tb.ix] == 0) {
        tb.lho[tb.ix] = tb.lhi++;
      }
      if (new_assign_str(v)){
        add_assign_str(v);
      }
      aProp(tb.ix);
      aType(TNONE);
      return 1;
    } else if (i == 4) {
      // variable type
      char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      if (v[0] == '~') {
        tb.isi[tb.id] = 1; // variable ignored
        tb.lh[tb.ix] = isSuppressedLHSstr;
      }
      return 1;
    } else if (i == 5) {
      // clear code and add line
      sbt.o = 0;
      char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      // take out quotes
      v++;
      v[strlen(v)-1]=0;
      int n = get_str_assign_int(tb.id, v);
      sAppend(&sbt, "levels(%s) ", tb.str.line[tb.id]);
      if (tb.isi[tb.id]) {
        // ignored variable
        sAppendN(&sbt, "~ ", 2);
      } else {
        // output variable
        sAppendN(&sbt, "<- ", 3);
      }
      sAppend(&sbt, "\"%s\";", v);
      sb.o = sbDt.o = 0;
      sAppend(&sb, "/*  '%s' */", sbt.s);
      sAppend(&sbDt, "/*  '%s' */ ", sbt.s);
      return 1;
    }
    return 1;
  }
  return 0;
}


static inline int finalizeLineLevelStr(nodeInfo ni, char *name) {
  if (nodeHas(levels_str) || nodeHas(levels_str1)){
    addLine(&sbPm,     "%s\n", sb.s);
    addLine(&sbPmDt,   "%s\n", sbDt.s);
    sAppend(&sbNrm, "%s\n", sbt.s);
    addLine(&sbNrmL, "%s\n", sbt.s);
    ENDLINE;
    return 1;
  }
  return 0;
}
