
static inline int isDiffFunction(transFunctions *tf) {
  return !strcmp("lag", tf->v) || (tf->isLead = !strcmp("lead", tf->v)) ||
    (tf->isDiff = !strcmp("diff", tf->v)) ||
    (tf->isFirst = !strcmp("first", tf->v)) ||
    (tf->isLast = !strcmp("last", tf->v));
}
static inline int getFunctionNargs(transFunctions *tf, int node){
  int ii = d_get_number_of_children(d_get_child(tf->pn,node))+1;
  if (ii == 1) {
    D_ParseNode *xpn = d_get_child(tf->pn, node-1);
    char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    if (allSpaces(v2)) {
      return 0;
    }
  }
  return ii;
}

static inline int assertCorrectDiffArgs(transFunctions *tf, int nargs, int *lagNo) {
  if (nargs != 1 && (tf->isFirst || tf->isLast)) {
    updateSyntaxCol();
    sPrint(&_gbuf, _("'%s' takes 1 argument %s(parameter), you have %d"),
	   tf->v, tf->v, nargs);
    trans_syntax_error_report_fn(_gbuf.s);
    return 1;
  }
  if (!(tf->isFirst || tf->isLast)) {
    if (!(nargs == 1 || nargs == 2)) {
      updateSyntaxCol();
      sPrint(&_gbuf, _("'%s' takes 1-2 arguments %s(parameter, k), you have %d"),
	     tf->v, tf->v, nargs);
      trans_syntax_error_report_fn(_gbuf.s);
      return 1;
    }
    // Check lag
    if (nargs == 2) {
      D_ParseNode *xpn = d_get_child(tf->pn, 3);
      char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      if (strlen(v2) > 2){
	*lagNo = toInt(v2+1);
	if (tf->isLead && *lagNo != NA_INTEGER) *lagNo = -*lagNo;
      }
      if (*lagNo == NA_INTEGER){
	updateSyntaxCol();
	sPrint(&_gbuf, _("'%s(parameter, k)' requires k to be an integer"), tf->v);
	trans_syntax_error_report_fn(_gbuf.s);
	return 1;
      } else if (tf->isDiff && *lagNo <= 0){
	updateSyntaxCol();
	sPrint(&_gbuf, _("'%s(parameter, k)' requires k to be an integer >= 1"), tf->v);
	trans_syntax_error_report_fn(_gbuf.s);
	return 1;
      }
    }
  }
  return 0;
}

static inline int handleFunctionDiff(transFunctions *tf) {
  if (isDiffFunction(tf)) {
    int nargs = getFunctionNargs(tf, 3);
    // lag(par, 1) => lag_par(1)
    // lag(par) => lag_par(1)
    // Header what lag_par means.
    char *v2;
    int lagNo =0;
    D_ParseNode *xpn;
    if (assertCorrectDiffArgs(tf, nargs, &lagNo)) return 1;
    switch(nargs) {
    case 1:
      xpn = d_get_child(tf->pn, 2);
      v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      lagNo=0;
      tb.fn=0;
      lagNo = 1;
      if (tf->isLead) lagNo=-1;
      if (tf->isFirst || tf->isLast) lagNo=NA_INTEGER;
      if (new_or_ith(v2)){
	addSymbolStr(v2);
	tb.lag[NV-1] = lagNo;
      } else {
	tb.lag[tb.ix] = lagNo;
      }
      tb.fn=1;
      sAppend(&sb,"%s_", tf->v);
      sAppend(&sbDt,"%s_", tf->v);
      doDot2(&sb, &sbDt, v2);
      sAppendN(&sb, "1(", 2);
      sAppendN(&sbDt, "1(", 2);
      sAppend(&sbt, "%s(", tf->v);
      break;
    case 2:
      // Check lag(x, 1);  Its OK with lhs, but nothing else is...
      xpn = d_get_child(tf->pn, 2);
      v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      tb.fn=0;
      if (new_or_ith(v2)){
	addSymbolStr(v2);
	tb.lag[NV-1] = lagNo;
      } else {
	tb.lag[tb.ix] = lagNo;
      }
      tb.fn=1;
      if (lagNo == 0){
	doDot2(&sb, &sbDt, v2);
	/* Free(v2); */
	/* Free(tf->v); */
	tf->i[0] = 4;// skip next arguments
	tf->depth[0]=1;
	return 1;
      } else {
	skipDouble=1;
	sAppend(&sb,   "%s_", tf->v);
	sAppend(&sbDt,   "%s_", tf->v);
	doDot2(&sb, &sbDt, v2);
	sAppendN(&sb,   "(", 1);
	sAppendN(&sbDt,   "(", 1);
	sAppend(&sbt,  "%s(", tf->v);
      }
      break;
    default:

      break;
    }
    tf->i[0]     = 1;// Parse next arguments
    tf->depth[0] =1;
    return 1;
  }
  return 0;
}
