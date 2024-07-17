////////////////////////////////////////////////////////////////////////////////
// Parsing pieces
static inline void handleIdentifier(nodeInfo ni, char *name, char *value) {
  // Handles identifiers, add it as a symbol if needed.
  if (isIdentifier(ni, name)) {
    if (new_or_ith(value)){
      // If it is new, add it
      addSymbolStr(value);
      // Ignored variables
      if (isTbsVar(value)){
        // If it is Transform both sides, suppress printouts
        tb.lh[NV-1] = isSuppressedParam; // Suppress param printout.
      }
    } else if (isDefiningParameterRecursively(value)){
      // This is x = x*exp(matt)
      // lhs defined in terms of a parameter
      if (tb.lh[tb.ix] == isSuppressedLHS){
        tb.lh[tb.ix] = notLHS;
      } else {
        tb.lh[tb.ix] = isLHSparam;
      }
    }
  }
}

static inline void handleOperatorsOrPrintingIdentifiers(int depth, print_node_fn_t fn, void *client_data,
                                                        nodeInfo ni, char *name, char *value) {
  if (isOperatorOrPrintingIdentifier(ni, name))
    fn(depth, name, value, client_data);
  if (!strcmp("=", name)){
    tb.didEq=1;
    fn(depth, name, value, client_data);
  }

  // Operator synonyms
  if (!strcmp("<-",name)){
    if (tb.didEq) {
      updateSyntaxCol();
      trans_syntax_error_report_fn0(_("repeat assignment not allowed"));
    }
    aAppendN(" =", 2);
    sAppendN(&sbt, "=", 1);
    tb.didEq=1;
  } else if (!strcmp("~",name)){
    // Suppress LHS calculation with ~
    aAppendN(" =", 2);
    sAppendN(&sbt, "~", 1);
    tb.lh[tb.ix] = isSuppressedLHS; // Suppress LHS printout.
    tb.didEq=1;
  } else if (!strcmp("=", name)){
    tb.didEq=1;
  } else if (!strcmp("|",name)){
    aAppendN(" ||", 3);
    sAppendN(&sbt, "||", 2);
  } else if (!strcmp("&",name)){
    aAppendN(" &&", 3);
    sAppendN(&sbt, "&&", 2);
  }
}

static inline int handleTheta(nodeInfo ni, char *name, D_ParseNode *xpn) {
  if (nodeHas(theta)){
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    sPrint(&_gbuf,"_THETA_%s_",v);
    int ii = strtoimax(v,NULL,10);
    if (ii > tb.maxtheta){
      tb.maxtheta =ii;
    }
    if (new_or_ith(_gbuf.s)){
      addSymbolStr(_gbuf.s);
    }
    sAppend(&sb,"_THETA_%s_",v);
    sAppend(&sbDt,"_THETA_%s_",v);
    sAppend(&sbt,"THETA[%s]",v);
    /* Free(v); */
    return 1;
  }
  return 0;
}

static inline int handleEta(nodeInfo ni, char *name, D_ParseNode *xpn) {
  if (nodeHas(eta)){
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    int ii = strtoimax(v,NULL,10);
    if (ii > tb.maxeta){
      tb.maxeta =ii;
    }
    sPrint(&_gbuf,"_ETA_%s_",v);
    if (new_or_ith(_gbuf.s)){
      addSymbolStr(_gbuf.s);
    }
    sAppend(&sb, "_ETA_%s_",v);
    sAppend(&sbDt, "_ETA_%s_",v);
    sAppend(&sbt,"ETA[%s]",v);
    /* Free(v); */
    return 1;
  }
  return 0;
}

static inline void handleSafeZero(nodeInfo ni, char *name, int i, int *safe_zero, D_ParseNode *xpn) {
  if (nodeHas(mult_part)){
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    if (i == 0){
      if (!strcmp("/",v)){
        aAppendN("safe_zero(", 10);
        *safe_zero = 1;
      } else {
        *safe_zero = 0;
      }
    }
    if (i == 1){
      if (*safe_zero){
        aAppendN(")", 1);
      }
      *safe_zero = 0;
    }
    /* Free(v); */
  }
}
