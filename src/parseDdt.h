static inline int new_de(const char *s) {
  int i;
  if (!strcmp("cmt", s))  err_trans("'cmt' cannot be a state or lhs expression");
  if (!strcmp("dvid", s)) err_trans("'dvid' cannot be a state or lhs expression");
  if (!strcmp("addl", s)) err_trans("'addl' cannot be a state or lhs expression");
  if (!strcmp("ii", s)) err_trans("'ii' cannot be a state or lhs expression");
  if (!strcmp("ss", s)) err_trans("'ss' cannot be a state or lhs expression");
  if (!strcmp("amt", s)) err_trans("'amt' cannot be a state or lhs expression");
  if (!strcmp("dur", s)) err_trans("'dur' cannot be a state or lhs expression");
  if (!strcmp("rate", s)) err_trans("'rate' cannot be a state or lhs expression");
  if (!strcmp("Rprintf", s)) err_trans("'Rprintf' cannot be a state");
  if (!strcmp("printf", s)) err_trans("'printf' cannot be a state");
  if (!strcmp("print", s)) err_trans("'print' cannot be a state");
  for (i=0; i<tb.de.n; i++) {
    if (!strcmp(tb.de.line[i], s)) {
      tb.id = i;
      return 0;
    }
  }
  if (tb.de.n + 1 > tb.allocD){
    tb.allocD+=MXDER;
    tb.di=R_Realloc(tb.di, tb.allocD, int);
    tb.idi=R_Realloc(tb.idi, tb.allocD, int);
    tb.idu=R_Realloc(tb.idu, tb.allocD, int);
    tb.dvid=R_Realloc(tb.dvid, tb.allocD, int);
  }
  return 1;
}

static inline int isCmtLhsStatement(nodeInfo ni, char *name, char *v) {
  int hasLhs = 0;
  if (nodeHas(cmt_statement)){
    new_or_ith(v);
    if (tb.lh[tb.ix] || tb.ini[tb.ix]){
      hasLhs=1;
      tb.ini[tb.ix]=2;
    }
    if (!strcmp("depot", v)){
      tb.hasDepotCmt = 1;
    } else if (!strcmp("central", v)){
      tb.hasCentralCmt = 1;
    }
  }
  return hasLhs;
}

static inline int add_deCmtProp(nodeInfo ni, char *name, char *v, int hasLhs, int fromWhere) {
  if (hasLhs == fromCMTprop) { // 1 only
    if (tb.lh[tb.ix] == isSuppressedLHS || tb.lh[tb.ix] == 29) {
      tb.lh[tb.ix] = 29;
    } else {
      tb.lh[tb.ix] = isLhsStateExtra;
    }
    new_or_ith(v);
    return 1;
  }
  return 0;
}

static inline int add_deState(nodeInfo ni, char *name, char *v, int hasLhs, int fromWhere) {
  if (fromWhere == fromDDT && strncmp(v, "rx__sens_", 3) == 0) {
    tb.sensi++;
  }
  new_or_ith(v);
  if (((tb.ini[tb.ix] == 1 && tb.ini0[tb.ix] == 0) ||
       (tb.lh[tb.ix] == isLHS || tb.lh[tb.ix] == isLHSparam))){
    updateSyntaxCol();
    sPrint(&_gbuf,_("cannot assign state variable %s; For initial condition assignment use '%s(0) = #'."),v,v);
    trans_syntax_error_report_fn0(_gbuf.s);
  }
  tb.lh[tb.ix] = isState;
  return 1;
}

static inline void add_de(nodeInfo ni, char *name, char *v, int hasLhs, int fromWhere) {
  tb.statei++;
  tb.id=tb.de.n;
  if (fromWhere == fromCMTprop && !nodeHas(cmt_statement)) {
    if (rx_syntax_require_ode_first) {
      if (!strcmp("depot", v)) {
	tb.hasDepot = 1;
      } else if (!strcmp("central", v)) {
	tb.hasCentral = 1;
      } else {
	updateSyntaxCol();
	sPrint(&_gbuf,ODEFIRST,v);
	trans_syntax_error_report_fn(_gbuf.s);
      }
    }
  }
  int tmp = add_deCmtProp(ni, name, v, hasLhs, fromWhere) ||
    add_deState(ni, name, v, hasLhs, fromWhere);
  (void) tmp;
  tb.di[tb.de.n] = tb.ix;
  addLine(&(tb.de),"%s",v);
}

static inline int handleDdtAssign(nodeInfo ni, char *name, int i, D_ParseNode *pn, D_ParseNode *xpn) {
  if (nodeHas(derivative) && i==2) {
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    if (new_de(v)) {
      add_de(ni, name, v, 0, fromDDT);
    }
    new_or_ith(v);
    tb.lastDdt = tb.id;
    /* printf("de[%d]->%s[%d]\n",tb.id,v,tb.ix); */
    sb.o =0; sbDt.o =0;
    if (tb.idu[tb.id] == 0){
      sAppend(&sb, "__DDtStateVar__[%d] = ((double)(_ON[%d]))*(_IR[%d] ", tb.id, tb.id, tb.id);
      sAppend(&sbDt, "__DDtStateVar_%d__ = ((double)(_ON[%d]))*(_IR[%d] ", tb.id, tb.id, tb.id);
    } else {
      sAppend(&sb, "__DDtStateVar__[%d] = ((double)(_ON[%d]))*(", tb.id, tb.id);
      sAppend(&sbDt, "__DDtStateVar_%d__ = ((double)(_ON[%d]))*(", tb.id, tb.id);
    }
    tb.idu[tb.id]=1;
    aType(TDDT);
    aProp(tb.id);
    sbt.o=0;
    sAppend(&sbt, "d/dt(%s)", v);
    /* Free(v); */
    xpn = d_get_child(pn,4);
    v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    if (!strcmp("~",v)){
      tb.idi[tb.id] = 1;
      sAppendN(&sbt, "~", 1);
    } else {
      // Don't switch idi back to 0; Once the state is ignored,
      // keep it ignored.
      sAppendN(&sbt, "=", 1);
    }
    return 1;
  }
  if (nodeHas(derivative) && i==5) {
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    if (!strcmp("+", v) ||
	!strcmp("-", v)){
      // = + is output  or = InfusionRate + is outupt.
    } else {
      // = + is output  or = InfusionRate + is outupt.
      aAppendN("+ ", 2);
    }
    /* Free(v); */
    return 1;
  }
  return 0;
}

static inline int handleDdtRhs(nodeInfo ni, char *name, D_ParseNode *xpn) {
  if (nodeHas(der_rhs)) {
    switch(sbPm.lType[sbPm.n]){
    case TMTIME:
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("modeling times cannot depend on state values"));
      break;
    case FBIO:
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("bioavailability cannot depend on state values"));
      break;
    case ALAG:
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("absorption lag-time cannot depend on state values"));
      break;
    case RATE:
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("model-based rate cannot depend on state values"));
      break;
    case DUR:
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("model-based duration cannot depend on state values"));
      break;
    case TMAT0:
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("model-based matricies cannot depend on state values"));
    default:
      {
	updateSyntaxCol();
	char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
	if (new_de(v)){
	  /* sPrint(&buf2,"d/dt(%s)",v); */
	  updateSyntaxCol();
	  sPrint(&_gbuf,"Tried to use d/dt(%s) before it was defined",v);
	  updateSyntaxCol();
	  trans_syntax_error_report_fn(_gbuf.s);
	} else {
	  if (sbPm.lType[sbPm.n] == TJAC){
	    sAppend(&sb,   "__DDtStateVar_%d__", tb.id);
	    sAppend(&sbDt, "__DDtStateVar_%d__", tb.id);
	  } else {
	    sAppend(&sb,   "__DDtStateVar__[%d]", tb.id);
	    sAppend(&sbDt, "__DDtStateVar_%d__", tb.id);
	    aType(TDDT);
	  }
	  aProp(tb.id);
	  sAppend(&sbt, "d/dt(%s)", v);
	}
      }
    }
    return 1;
  }
  return 0;
}

static inline int finalizeLineDdt(nodeInfo ni, char *name) {
  if (nodeHas(derivative)){
    addLine(&sbPm,     "%s);\n", sb.s);
    addLine(&sbPmDt,   "%s);\n", sbDt.s);
    sAppend(&sbNrm, "%s;\n", sbt.s);
    addLine(&sbNrmL, "%s;\n", sbt.s);
    ENDLINE;
    return 1;
  }
  return 0;
}
