// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: t; -*-
static inline int handleFunctionDosenum(transFunctions *tf) {
  if (!strcmp("dosenum", tf->v)) {
    int ii = d_get_number_of_children(d_get_child(tf->pn,3))+1;
    if (ii == 1){
      D_ParseNode *xpn = d_get_child(tf->pn, 2);
      char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      if (allSpaces(v2)){
				aAppendN("(double)(_solveData->subjects[_cSub].dosenum)", 45);
				sAppendN(&sbt, "dosenum()", 9);
      } else {
				updateSyntaxCol();
				trans_syntax_error_report_fn(_("'dosenum' does not currently take arguments 'dosenum()'"));
      }
    } else {
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("'dosenum' does not currently take arguments 'dosenum()'"));
    }
    tf->i[0] = tf->nch;
    return 1;
  }
  return 0;
}

static inline int isFunctionTadType(transFunctions *tf) {
	return (tf->isTad = !strcmp("tad", tf->v)) || (tf->isTafd = !strcmp("tafd", tf->v)) ||
		(tf->isTlast = !strcmp("tlast", tf->v)) || (tf->isTfirst = !strcmp("tfirst", tf->v)) ||
		(tf->isDose = !strcmp("dose", tf->v)) ||
		(tf->isPodo = !strcmp("podo", tf->v));
}
static inline int handleFunctionTadEmptyCcode(transFunctions *tf,char *v2) {
	if (allSpaces(v2)){
		// tad overall
		if (tf->isPodo) {
			sAppend(&sb, "_%s1(%d)", tf->v, tb.lastDdt);
			sAppend(&sbDt, "_%s1(%d)", tf->v, tb.lastDdt);
		} else {
			sAppend(&sb, "_%s0()", tf->v);
			sAppend(&sbDt, "_%s0()", tf->v);
		}
		return 1;
	} 
	return 0;
}

static inline int handleFunctionTadSingleStateCcode(transFunctions *tf,char *v2) {
	sAppend(&sb, "_%s1(", tf->v);
	sAppend(&sbDt, "_%s1(", tf->v);
	if (new_de(v2)){
		if (!strcmp("depot", v2)){
			tb.hasDepot = 1;
			aAppendN("_DEPOT_)", 8);
		} else if (!strcmp("central", v2)){
			tb.hasCentral = 1;
			aAppendN("_CENTRAL_)", 10);
		} else if (rx_syntax_require_ode_first){
			updateSyntaxCol();
			sPrint(&_gbuf,ODEFIRST,v2);
			trans_syntax_error_report_fn(_gbuf.s);
			/* Free(v2); */
			/* Free(tf->v); */
		} else {
			tb.statei++;
			sAppend(&sb, "%d)", tb.de.n);
			sAppend(&sbDt, "%d)", tb.de.n);
		}
	} else {
		new_or_ith(v2);
		sAppend(&sb, "%d)", tb.id);
		sAppend(&sbDt, "%d)", tb.id);
	}
	// tad(cmt)
	return 1;
}

static inline int handleFunctionTad(transFunctions *tf) {
  if (isFunctionTadType(tf)) {
    int ii = d_get_number_of_children(d_get_child(tf->pn,3))+1;
    if (ii == 1) {
      D_ParseNode *xpn = d_get_child(tf->pn, 2);
      char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
			handleFunctionTadEmptyCcode(tf, v2) ||
				handleFunctionTadSingleStateCcode(tf, v2);
      sAppend(&sbt, "%s(%s)", tf->v, v2);
      tf->i[0] = tf->nch;
      return 1;
    }
  }
  return 0;
}
