static inline int handleCmtPropertyFbio(nodeInfo ni, char *name, char *v) {
  if (nodeHas(fbio)){
    sb.o=0;sbDt.o=0; sbt.o=0;
    sAppend(&sb, "_f[%d] = ", tb.id);
    sAppend(&sbDt, "_f[%d] = ", tb.id);
    sAppend(&sbt, "f(%s)=", v);
    tb.curPropN=tb.id;
    if (foundF == 0) needSort+=1;// & 1 when F
    foundF=1;
    aType(FBIO);
    return 1;
  }
  return 0;
}

static inline int handleCmtPropertyAlag(nodeInfo ni, char *name, char *v) {
  if (nodeHas(alag)){
    sb.o=0;sbDt.o=0; sbt.o=0;
    sAppend(&sb, "_alag[%d] = ", tb.id);
    sAppend(&sbDt, "_alag[%d] = ", tb.id);
    sAppend(&sbt, "alag(%s)=", v);
    tb.curPropN=tb.id;
    if (foundLag == 0) needSort+=2; // & 2 when alag
    foundLag=1;
    aType(ALAG);
    return 1;
  }
  return 0;
}

static inline int handleCmtPropertyDur(nodeInfo ni, char *name, char *v) {
  if (nodeHas(dur)) {
    sb.o=0;sbDt.o=0; sbt.o=0;
    sAppend(&sb, "_dur[%d] = ", tb.id);
    sAppend(&sbDt, "_dur[%d] = ", tb.id);
    sAppend(&sbt, "dur(%s)=", v);
    tb.curPropN=tb.id;
    if (foundDur == 0) needSort+=4;// & 4 when dur
    foundDur=1;
    aType(DUR);
    return 1;
  }
  return 0;
}

static inline int handleCmtPropertyRate(nodeInfo ni, char *name, char *v) {
  if (nodeHas(rate)){
    sb.o=0;sbDt.o=0; sbt.o=0;
    sAppend(&sb, "_rate[%d] = ", tb.id);
    sAppend(&sbDt, "_rate[%d] = ", tb.id);
    sAppend(&sbt, "rate(%s)=", v);
    tb.curPropN=tb.id;
    if (foundRate == 0) needSort+=8;// & 8 when rate
    foundRate=1;
    aType(RATE);
    return 1;
  }
  return 0;
}

static inline int handleCmtPropertyCmtOrder(nodeInfo ni, char *name, char *v) {
  if (nodeHas(cmt_statement)){
    sb.o=0;sbDt.o=0; sbt.o=0;
    sAppend(&sbt, "cmt(%s)", v);
    sAppend(&sbNrm, "%s;\n", sbt.s);
    addLine(&sbNrmL, "%s;\n", sbt.s);
    return 1;
  }
  return 0;
}

static inline int handleCmtProperty(nodeInfo ni, char *name, int i, D_ParseNode *xpn) {
  if ((nodeHas(fbio) || nodeHas(alag) ||
       nodeHas(dur) || nodeHas(rate) ||
       nodeHas(cmt_statement)) && i==2) {
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    int hasLhs=isCmtLhsStatement(ni, name, v);
    if (new_de(v)){
      add_de(ni, name, v, hasLhs, fromCMTprop);
      aProp(tb.de.n);
      handleCmtPropertyCmtOrder(ni, name, v);
    } else {
      new_or_ith(v);
      aProp(tb.ix);
      /* printf("de[%d]->%s[%d]\n",tb.id,v,tb.ix); */
    }
    int tmp = handleCmtPropertyFbio(ni, name, v) ||
      handleCmtPropertyAlag(ni, name, v) ||
      handleCmtPropertyDur(ni, name, v) ||
      handleCmtPropertyRate(ni, name, v);
    (void) tmp;
    return 1;
  }
  return 0;
}

static inline int handleRemainingAssignmentsIniProp(nodeInfo ni, char *name, int i, D_ParseNode *pn, D_ParseNode *xpn, char *v) {
  if ((rx_syntax_allow_ini && nodeHas(ini)) || nodeHas(ini0)) {
    sb.o =0; sbDt.o =0;
    /* aAppendN("(__0__)", 7); */
    aType(TINI);
    doDot2(&sb, &sbDt, v);
    if (nodeHas(ini) && !new_de(v)){
      if (tb.idu[tb.id] == 0){
        new_or_ith(v);
        if (tb.lh[tb.ix] == isSuppressedLHS || tb.lh[tb.ix] == 29){
          tb.lh[tb.ix] = 29;
        } else {
          tb.lh[tb.ix] = isLhsStateExtra;
        }
      } else {
        updateSyntaxCol();
        sPrint(&_gbuf,"Cannot assign state variable %s; For initial condition assigment use '%s(0) ='",v,v);
        trans_syntax_error_report_fn(_gbuf.s);
      }
    }
    return 1;
  }
  return 0;
}

static inline void handleRemainingAssignmentsRestProp(nodeInfo ni, char *name, int i, D_ParseNode *pn, D_ParseNode *xpn, char *v) {
  sb.o = 0; sbDt.o = 0;
  doDot2(&sb, &sbDt, v);
  if (!new_de(v)){
    if (tb.idu[tb.id] == 0){
      // Change to 19 for LHS w/stateExtra
      new_or_ith(v);
      if (tb.lh[tb.ix] == isSuppressedLHS || tb.lh[tb.ix] == 29){
	tb.lh[tb.ix] = 29;
      } else {
	tb.lh[tb.ix] = isLhsStateExtra;
      }
    } else {
      sPrint(&_gbuf,"Cannot assign state variable %s; For initial condition assigment use '%s(0) ='",v,v);
      updateSyntaxCol();
      trans_syntax_error_report_fn(_gbuf.s);
    }
  }
  aType(TASSIGN);
}

static inline int handleRemainingAssignmentsCalcPropMtime(nodeInfo ni, char *name){
  if (nodeHas(mtime)){
    tb.lh[tb.ix] = isLHS;
    tb.mtime[tb.ix] = 1;
    return 1;
  }
  return 0;
}


static inline int handleRemainingAssignmentsCalcPropComplexAssign(nodeInfo ni, char *name, char *v) {
  if (nodeHas(assignment)  || (!rx_syntax_allow_ini && nodeHas(ini))) {
    if (tb.ix+1 == NV && tb.NEnd != NV){
      // New assignment
      tb.ixL = tb.ix;
      tb.lh[tb.ix] = isLHS;
    } else if (tb.ix < 0){
      sPrint(&_gbuf,"cannot assign protected variable '%s'",v);
      updateSyntaxCol();
      trans_syntax_error_report_fn(_gbuf.s);
    } else {
      if (tb.lh[tb.ix] == notLHS){
	tb.lh[tb.ix] = isLHSparam;
      } else {
	tb.lh[tb.ix] = isLHS;
      }
      tb.ixL=-1;
    }
    return 1;
  }
  return 0;
}


static inline int handleRemainingAssignmentsCalcPropIni(nodeInfo ni, char *name, D_ParseNode *pn, char *v) {
  if (nodeHas(ini) || nodeHas(ini0)) {
    D_ParseNode *xpn;
    double d;
    if (tb.ini[tb.ix] == 0){
      // If there is only one initialzation call, then assume
      // this is a parameter with an initial value.
      tb.ini[tb.ix] = 1;
      if (nodeHas(ini0)){
	tb.ini0[tb.ix] = 1;
	xpn = d_get_child(pn, 3);
	/* Free(v); */
	v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
	sscanf(v, "%lf", &d);
	tb.iniv[tb.ix] = d;
	tb.ini_i++;
      } else {
	tb.ini0[tb.ix] = 0;
	if (strncmp(v,"rx_",3)==0){
	  tb.lh[tb.ix] = isLHS;
	} else {
	  xpn = d_get_child(pn, 2);
	  /* Free(v); */
	  v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
	  sscanf(v, "%lf", &d);
	  tb.iniv[tb.ix] = d;
	  tb.ini_i++;
	}
      }
      /* Free(v); */
      return 1;
    } else {
      // There is more than one call to this variable, it is a
      // conditional variable
      /* Rprintf("Duplicate %s; %d %d\n", v, tb.lh[tb.ix], tb.ini0[tb.ix]); */
      if (tb.lh[tb.ix] != isLHS){
	tb.lh[tb.ix] = isLHS;
	if (nodeHas(ini0) && tb.ini0[tb.ix] == 1){
	  sPrint(&_gbuf,"cannot have conditional initial conditions for '%s'",v);
	  updateSyntaxCol();
	  trans_syntax_error_report_fn(_gbuf.s);
	} else if (tb.ini0[tb.ix] == 1){
	  tb.iniv[tb.ix] = NA_REAL;
	  tb.ini_i--;
	} else if (tb.ini[tb.ix] == 1){
	  tb.iniv[tb.ix] = NA_REAL;
	  tb.ini_i--;
	}
      }
      tb.ini0[tb.ix] = 0;
    }
  }
  return 0;
}

static inline int handleRemainingAssignmentsCalcProps(nodeInfo ni, char *name, int i, D_ParseNode *pn, D_ParseNode *xpn, char *v) {
  if (!handleRemainingAssignmentsIniProp(ni, name, i, pn, xpn, v)){
    handleRemainingAssignmentsRestProp(ni, name, i, pn, xpn, v);
  }
  new_or_ith(v);
  aProp(tb.ix);
  if (!(handleRemainingAssignmentsCalcPropMtime(ni, name) ||
	handleRemainingAssignmentsCalcPropComplexAssign(ni, name, v))) {
    return handleRemainingAssignmentsCalcPropIni(ni, name, pn, v);
  }
  return 0;
}


static inline int finalizeLineParam(nodeInfo ni, char *name) {
  if (nodeHas(param_statement)) {
    sbDt.o = 0; sbt.o = 0;
    sAppend(&sbNrm, "param%s;\n", sbt.s);
    addLine(&sbNrmL, "param%s;\n", sbt.s);
    ENDLINE;
    return 1;
  }
  return 0;
}
