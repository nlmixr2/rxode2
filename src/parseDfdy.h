// Parsing df(a)/dy(b)
static inline int handleRhsDf(nodeInfo ni, char *name, int i, D_ParseNode *xpn, int *ii, int *found) {
  if (nodeHas(dfdy_rhs) && i == 2){
    // Continuation statement
    switch(sbPm.lType[sbPm.n]){
    case FBIO:
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("bioavailability cannot depend on Jacobian values"));
      break;
    case ALAG:
      updateSyntaxCol();
      trans_syntax_error_report_fn("absorption lag-time cannot depend on Jacobian values");
      break;
    case RATE:
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("model-based rate cannot depend on Jacobian values"));
      break;
    case DUR:
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("model-based duration cannot depend on Jacobian values"));
      break;
    case TMAT0:
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("model-based matricies cannot depend on Jacobian values"));
      break;
    default: {
      char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      aType(TJAC);
      sAppend(&sbDt, "__PDStateVar_%s_SeP_",v);
      sAppend(&sbt,"df(%s)/dy(",v);
      if (new_de(v)){
	updateSyntaxCol();
	sPrint(&_gbuf,_("d/dt(%s) needs to be defined before using a Jacobians for this state"),v);
	trans_syntax_error_report_fn(_gbuf.s);
      } else {
	sAppend(&sb, "__PDStateVar__[%d*(__NROWPD__)+",tb.id);
      }
    }
    }
    found_jac=1;
    return 1;
  }
  return 0;
}

static inline int handleLhsDf(nodeInfo ni, char *name, int i, D_ParseNode *xpn, int *ii, int *found) {
  if ((nodeHas(dfdy)) && i == 2) {
    found_jac = 1;
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    // New statement
    aType(TJAC);
    sb.o = 0; sbDt.o = 0;
    sbt.o = 0;
    sAppend(&sbDt,"__PDStateVar_%s_SeP_",v);
    sAppend(&sbt,"df(%s)/dy(",v);
    if (new_de(v)){
      updateSyntaxCol();
      sPrint(&_gbuf,_("d/dt(%s) needs to be defined before using a Jacobians for this state"),v);
      trans_syntax_error_report_fn(_gbuf.s);
    } else {
      sAppend(&sb,"__PDStateVar__[%d*(__NROWPD__)+",tb.id);
    }
    new_or_ith(v);
    tb.cdf = tb.ix;
    /* Free(v); */
    return 1;
  }
  return 0;
}

static inline void handleDyThetaEta(nodeInfo ni, char *name, int i, D_ParseNode *xpn, int *ii, int *found, char *v) {
  *ii = 0;
  if (strstr(v,"THETA[") != NULL){
    good_jac=0;
    sPrint(&_gbuf,"_THETA_%.*s_",(int)(strlen(v))-7,v+6);
    sAppend(&sbt, "%s)",v);
    sAppendN(&sb, "0]", 2);
    sAppend(&sbDt, "%s__",_gbuf.s);
    *ii = 1;
  } else if (strstr(v,"ETA[") != NULL) {
    good_jac=0;
    sPrint(&_gbuf,"_ETA_%.*s_",(int)(strlen(v))-5,v+4);
    sAppend(&sbt, "%s)",v);
    sAppendN(&sb, "0]",2);
    sAppend(&sbDt, "%s__",_gbuf.s);
    *ii = 1;
  } else {
    sAppend(&sbDt, "%s__",v);
    sAppend(&sbt, "%s)",v);
    new_or_ith(v);
    if (tb.lh[tb.ix] == isState){
      new_de(v);
      sAppend(&sb, "%d]",tb.id);
    } else {
      sAppendN(&sb, "0]",2);
      good_jac = 0;
    }
  }
}

static inline int handleDy(nodeInfo ni, char *name, int i, D_ParseNode *xpn, int *ii, int *found) {
  if ((nodeHas(dfdy) || nodeHas(dfdy_rhs)) && i == 4){
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    handleDyThetaEta(ni, name, i, xpn, ii, found, v);
    if (nodeHas(dfdy)){
      // If this is an assign append the = and add some information about the df/dy
      aAppendN(" = ", 3);
      sAppendN(&sbt ,"=", 1);
      if (*ii == 1){
	new_or_ith(_gbuf.s);
      } else {
	new_or_ith(v);
      }
      *found = -1;
      for (*ii = 0; *ii < tb.ndfdy; (*ii)++){
	if (tb.df[*ii] == tb.cdf && tb.dy[*ii] == tb.ix){
	  *found = *ii;
	  break;
	}
      }
      if (*found < 0){
	tb.df[tb.ndfdy] = tb.cdf;
	tb.dy[tb.ndfdy] = tb.ix;
	tb.ndfdy = tb.ndfdy+1;
	tb.cdf = -1;
      }
    }
    return 1;
  }
  return 0;
}

static inline int handleJac(nodeInfo ni, char *name, int i, D_ParseNode *xpn, int *ii, int *found) {
  return handleRhsDf(ni, name, i, xpn, ii, found)  ||
    handleLhsDf(ni, name, i, xpn, ii, found) ||
    handleDy(ni, name, i, xpn, ii, found);
}
