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
	return (tf->isTad = !strcmp("tad", tf->v)) ||
		(tf->isTad0 = !strcmp("tad0", tf->v)) ||

		(tf->isTafd = !strcmp("tafd", tf->v)) ||
		(tf->isTafd0 = !strcmp("tafd0", tf->v)) ||

		(tf->isTlast = !strcmp("tlast", tf->v)) ||
		(tf->isTlast0 = !strcmp("tlast0", tf->v)) ||

		(tf->isTfirst = !strcmp("tfirst", tf->v)) ||
    (tf->isTfirst0 = !strcmp("tfirst0", tf->v)) ||

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
	if (new_de(v2, 0)){
		// cannot be lhs statements in tad style assignments
		// also cannot be from anywhere
		// temporarily turn off that this is a function
		// This is not a function
		int fn = tb.fn;
		tb.fn = 0;
		// this is NOT an assignment
		nodeInfo ni2;
    niReset(&ni2);
    char *name = tf->v;
		ni2.assignment = 0;
		ni2.ini = 0;
		add_de(ni2, name, v2, 0, 0);
		// turn back on that this is a function
		tb.fn = fn;
	} else {
		new_or_ith(v2);
	}
  sAppend(&sb, "__DDT%d__)", tb.id);
  sAppend(&sbDt, "__DDT%d__)", tb.id);
	if (tf->isTad && (tb.dprop[tb.id] & propTad) == 0) {
    tb.dprop[tb.id] += propTad;
  } else if (tf->isTad0 && (tb.dprop[tb.id] & propTad0) == 0) {
    tb.dprop[tb.id] += propTad0;
  } else if (tf->isTafd && (tb.dprop[tb.id] & propTafd) == 0) {
		tb.dprop[tb.id] += propTafd;
	} else if (tf->isTafd0 && (tb.dprop[tb.id] & propTafd0) == 0) {
		tb.dprop[tb.id] += propTafd0;
	} else if (tf->isTlast && (tb.dprop[tb.id] & propTlast) == 0) {
		tb.dprop[tb.id] += propTlast;
	} else if (tf->isTlast0 && (tb.dprop[tb.id] & propTlast0) == 0) {
		tb.dprop[tb.id] += propTlast0;
	} else if (tf->isTfirst && (tb.dprop[tb.id] & propTfirst) == 0) {
		tb.dprop[tb.id] += propTfirst;
	} else if (tf->isTfirst0 && (tb.dprop[tb.id] & propTfirst0) == 0) {
		tb.dprop[tb.id] += propTfirst0;
	} else if (tf->isDose && (tb.dprop[tb.id] & propDose) == 0) {
		tb.dprop[tb.id] += propDose;
	} else if (tf->isPodo && (tb.dprop[tb.id] & propPodo) == 0) {
		tb.dprop[tb.id] += propPodo;
	}
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
