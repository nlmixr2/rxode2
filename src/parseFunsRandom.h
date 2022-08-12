// -*- mode: C++; c-indent-level: 2; c-basic-offset: 2; indent-tabs-mode: nil; -*-
static inline int handleFunctionPnorm(transFunctions *tf) {
  if ((tf->isPnorm = !strcmp("pnorm", tf->v)) ||
      !strcmp("qnorm", tf->v)){
    int ii = d_get_number_of_children(d_get_child(tf->pn,3))+1;
    if (ii == 1) {
      D_ParseNode *xpn = d_get_child(tf->pn, 2);
      char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      int allSpace=allSpaces(v2);
      /* Free(v2); */
      if (allSpace){
        updateSyntaxCol();
        if (tf->isPnorm){
          trans_syntax_error_report_fn(_("'pnorm' in rxode2 takes 1-3 arguments pnorm(q, mean, sd)"));
        } else {
          trans_syntax_error_report_fn(_("'qnorm' in rxode2 takes 1-3 arguments pnorm(p, mean, sd)"));
        }
      } else {
        sAppend(&sb, "_%s1(", tf->v);
        sAppend(&sbDt,"_%s1(", tf->v);
        sAppend(&sbt, "%s(", tf->v);
      }
    } else if (ii == 2) {
      sAppend(&sb,"_%s2(", tf->v);
      sAppend(&sbDt,"_%s2(", tf->v);
      sAppend(&sbt, "%s(", tf->v);
    } else if (ii == 3) {
      sAppend(&sb,"_%s3(", tf->v);
      sAppend(&sbDt,"_%s3(", tf->v);
      sAppend(&sbt, "%s(", tf->v);
    } else {
      updateSyntaxCol();
      if (tf->isPnorm){
        trans_syntax_error_report_fn(_("'pnorm' in rxode2 takes 1-3 arguments pnorm(q, mean, sd)"));
      } else {
        trans_syntax_error_report_fn(_("'qnorm' in rxode2 takes 1-3 arguments pnorm(p, mean, sd)"));
      }
    }
    tf->i[0] = 1;// Parse next arguments
    tf->depth[0]=1;
    return 1;
  }
  return 0;
}

static inline int handleFunctionTransit(transFunctions *tf) {
  if (!strcmp("transit", tf->v)) {
    int ii = d_get_number_of_children(d_get_child(tf->pn,3))+1;
    if (ii == 2){
      sAppend(&sb,   "_transit3P(%d, t, _cSub, ", tb.lastDdt);
      sAppend(&sbDt, "_transit3P(%d, t, _cSub, ", tb.lastDdt);
      sAppendN(&sbt,"transit(", 8);
    } else if (ii == 3){
      sAppend(&sb,   "_transit4P(%d, t, _cSub, ", tb.lastDdt);
      sAppend(&sbDt, "_transit4P(%d, t, _cSub, ", tb.lastDdt);
      sAppendN(&sbt,"transit(", 8);
    } else {
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("'transit' takes 2-3 arguments transit(n, mtt, bio)"));
    }
    tf->i[0] = 1;// Parse next arguments
    tf->depth[0] = 1;
    return 1;
  }
  return 0;
}


static inline int isRxnormOrRelatedNode(transFunctions *tf) {
  return (tf->isNorm = !strcmp("rnorm", tf->v) ||
          !strcmp("rxnorm", tf->v) || (tf->isInd = !strcmp("rinorm", tf->v))) ||
    (tf->isNormV = !strcmp("rnormV", tf->v) ||
     !strcmp("rxnormV", tf->v) || (tf->isInd = !strcmp("rinormV", tf->v))) ||
    (tf->isCauchy = !strcmp("rxcauchy", tf->v) || (tf->isInd = !strcmp("ricauchy", tf->v)) ||
     !strcmp("rcauchy", tf->v)) ||
    (tf->isF = !strcmp("rxf", tf->v) ||
     !strcmp("rf", tf->v) || (tf->isInd = !strcmp("rif", tf->v))) ||
    (tf->isGamma = !strcmp("rxgamma", tf->v) ||
     !strcmp("rgamma", tf->v) || (tf->isInd = !strcmp("rigamma", tf->v))) ||
    (tf->isBeta = !strcmp("rxbeta", tf->v) ||
     !strcmp("rbeta", tf->v) || (tf->isInd = !strcmp("ribeta", tf->v))) ||
    (tf->isUnif = !strcmp("rxunif", tf->v) ||
     !strcmp("runif", tf->v) || (tf->isInd = !strcmp("riunif", tf->v))) ||
    (tf->isWeibull = !strcmp("rxweibull", tf->v) ||
     !strcmp("rweibull", tf->v) || (tf->isInd = !strcmp("riweibull", tf->v)));
}

static inline int assertCorrectRxnormArgs2(transFunctions *tf, int nargs) {
  if (nargs != 2) {
    if (tf->isF) {
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("'rif'/'rxf'/'rf' takes 2 arguments 'rxf(df1, df2)'"));
      return 1;
    }
    if (tf->isBeta) {
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("'ribeta'/'rxbeta'/'rbeta' takes 2 arguments 'rxbeta(shape1, shape2)'"));
      return 1;
    }
  }
  return 0;
}

static inline int assertCorrectRxnormArgs12(transFunctions *tf, int nargs) {
  if (!(nargs == 1 || nargs == 2)) {
    if (tf->isGamma){
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("'rigamma'/'rxgamma'/'rgamma' takes 1-2 arguments 'rxgamma(shape, rate)'"));
      return 1;
    }
    if (tf->isWeibull){
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("'riweibull'/'rxweibull'/'rweibull' takes 1-2 arguments 'rxweibull(shape, scale)'"));
      return 1;
    }
  }
  return 0;
}

static inline int assertCorrectRxnormArgs02(transFunctions *tf, int nargs) {
  if (nargs > 2) {
    updateSyntaxCol();
    if (tf->isNormV) {
      trans_syntax_error_report_fn(_("'rinormV'/'rxnormV'/'rnormV' takes 0-2 arguments 'rxnormV(mean, sd)'"));
      return 1;
    }
    if (tf->isNorm){
      trans_syntax_error_report_fn(_("'rinorm'/'rxnorm'/'rnorm' takes 0-2 arguments 'rxnorm(mean, sd)'"));
      return 1;
    }
    if (tf->isUnif){
      trans_syntax_error_report_fn(_("'riunif'/'rxunif'/'runif' takes 0-2 arguments 'rxunif(min, max)'"));
      return 1;
    }
    if (tf->isCauchy) {
      trans_syntax_error_report_fn(_("'ricauchy'/'rxcauchy'/'rcauchy' takes 0-2 arguments 'rxcauchy(location, scale)'"));
      return 1;
    }
  }
  return 0;
}

static inline int assertCorrectRxnormArgs(transFunctions *tf, int nargs) {
  return assertCorrectRxnormArgs2(tf, nargs) ||
    assertCorrectRxnormArgs12(tf, nargs) ||
    assertCorrectRxnormArgs02(tf, nargs);
}

static inline int handleFunctionRxnorm(transFunctions *tf) {
  if (isRxnormOrRelatedNode(tf)) {
    if (tb.thread != 0) tb.thread = 2;
    int nargs = getFunctionNargs(tf, 3);
    if (assertCorrectRxnormArgs(tf, nargs)) return 1;
    switch (nargs) {
    case 0:
      if (tf->isInd) {
        // rxnorm()
        sAppend(&sb,"%s(&_solveData->subjects[_cSub], %d, 0.0, 1.0",  tf->v, tb.nInd);
        sAppend(&sbDt,"%s(&_solveData->subjects[_cSub], %d, 0.0, 1.0", tf->v, tb.nInd++);
        foundF0=1;
      } else {
        sAppend(&sb,"%s(&_solveData->subjects[_cSub], 0.0, 1.0", tf->v);
        sAppend(&sbDt,"%s(&_solveData->subjects[_cSub], 0.0, 1.0", tf->v);
      }
      sAppend(&sbt, "%s(", tf->v);
      break;
    case 1:
      if (tf->isInd) {
        sAppend(&sb,"%s1(%d,", tf->v, tb.nInd);
        sAppend(&sbDt,"%s1(%d,", tf->v, tb.nInd++);
        foundF0=1;
      } else {
        sAppend(&sb,"%s1(", tf->v);
        sAppend(&sbDt,"%s1(", tf->v);
      }
      sAppend(&sbt, "%s(", tf->v);
      break;
    case 2:
      if (tf->isInd){
        foundF0=1;
        sAppend(&sb,"%s(&_solveData->subjects[_cSub], %d, ", tf->v, tb.nInd);
        sAppend(&sbDt,"%s(&_solveData->subjects[_cSub], %d, ", tf->v, tb.nInd++);
      } else {
        sAppend(&sb,"%s(&_solveData->subjects[_cSub], ", tf->v);
        sAppend(&sbDt,"%s(&_solveData->subjects[_cSub], ", tf->v);
      }
      sAppend(&sbt, "%s(", tf->v);
    }
    tf->i[0] = 1;// Parse next arguments
    tf->depth[0]=1;
    return 1;
  }
  return 0;
}

static inline int isRchisqOrRelatedNode(transFunctions *tf) {
  return !strcmp("rchisq", tf->v) ||
    !strcmp("rxchisq", tf->v) ||
    (tf->isInd = !strcmp("richisq", tf->v)) ||
    (tf->isExp = (!strcmp("rxexp", tf->v) ||
                  !strcmp("rexp", tf->v) ||
                  (tf->isInd = !strcmp("riexp", tf->v)))) ||
    (tf->isT = (!strcmp("rxt", tf->v) ||
                !strcmp("rt", tf->v) ||
                (tf->isInd = !strcmp("rit", tf->v))));
}

static inline int assertCorrectRxchisqArgs(transFunctions *tf, int nargs) {
  if (tf->isExp) {
    if (nargs > 1) {
      sPrint(&_gbuf, _("'%s' takes 0-1 argument"), tf->v);
      updateSyntaxCol();
      trans_syntax_error_report_fn(_gbuf.s);
      return 1;
    }
    return 0;
  }
  if (nargs != 1) {
    sPrint(&_gbuf, _("'%s' takes 1 argument"), tf->v);
    updateSyntaxCol();
    trans_syntax_error_report_fn(_gbuf.s);
    return 1;
  }
  return 0;
}

static inline int handleFunctionRchisq(transFunctions *tf) {
  if (isRchisqOrRelatedNode(tf)) {
    if (tb.thread != 0) tb.thread = 2;
    int nargs = getFunctionNargs(tf, 3);
    if (assertCorrectRxchisqArgs(tf, nargs)) return 1;
    switch(nargs){
    case 0:
      if (tf->isInd) {
        sAppend(&sb,"%s(&_solveData->subjects[_cSub], %d, 1.0", tf->v, tb.nInd);
        sAppend(&sbDt,"%s(&_solveData->subjects[_cSub], %d, 1.0", tf->v, tb.nInd++);
        foundF0=1;
      } else {
        sAppend(&sb,"%s(&_solveData->subjects[_cSub], 1.0", tf->v);
        sAppend(&sbDt,"%s(&_solveData->subjects[_cSub], 1.0", tf->v);
      }
      sAppend(&sbt, "%s(", tf->v);
      break;
    case 1:
      if (tf->isT){
        if (tf->isInd) {
          sAppend(&sb,"rit_(&_solveData->subjects[_cSub], %d, ", tb.nInd);
          sAppend(&sbDt,"rit_(&_solveData->subjects[_cSub], %d, ", tb.nInd++);
          foundF0=1;
          sAppendN(&sbt, "rit(", 4);
        } else {
          sAppendN(&sb,"rxt_(&_solveData->subjects[_cSub], ", 35);
          sAppendN(&sbDt,"rxt_(&_solveData->subjects[_cSub], ", 35);
          sAppendN(&sbt, "rxt(", 4);
        }
      } else {
        if (tf->isInd) {
          sAppend(&sb,"%s(&_solveData->subjects[_cSub], %d, ", tf->v, tb.nInd);
          sAppend(&sbDt,"%s(&_solveData->subjects[_cSub], %d, ", tf->v, tb.nInd++);
          foundF0=1;
        } else {
          sAppend(&sb,"%s(&_solveData->subjects[_cSub], ", tf->v);
          sAppend(&sbDt,"%s(&_solveData->subjects[_cSub], ", tf->v);
        }
        sAppend(&sbt, "%s(", tf->v);
      }
      break;
    }
    tf->i[0] = 1;// Parse next arguments
    tf->depth[0]=1;
    return 1;
  }
  return 0;
}

static inline int isRgeomOrRelated(transFunctions *tf) {
  return (!strcmp("rxgeom", tf->v) ||
          !strcmp("rgeom", tf->v) ||
          (tf->isInd = !strcmp("rigeom", tf->v)) ||
          (tf->isPois= !strcmp("rxpois", tf->v) ||
           !strcmp("rpois", tf->v) ||
           (tf->isInd = !strcmp("ripois", tf->v))));
}

static inline int assertCorrectGeomArgs(transFunctions *tf, int nargs) {
  if (nargs != 1) {
    updateSyntaxCol();
    if (tf->isPois){
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("'ripois'/'rxpois'/'rpois' takes 1 argument 'rxpois(lambda)'"));
    } else {
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("'rigeom'/'rxgeom'/'rgeom' takes 1 argument 'rxgeom(prob)'"));
    }
    return 1;
  }
  return 0;
}

static inline int handleFunctionRgeom(transFunctions *tf) {
  if (isRgeomOrRelated(tf)) {
    if (tb.thread != 0) tb.thread = 2;
    int nargs = getFunctionNargs(tf, 3);
    if (assertCorrectGeomArgs(tf, nargs)) return 1;
    if (tf->isInd) {
      sAppend(&sb, "(double)%s(&_solveData->subjects[_cSub], %d, ", tf->v, tb.nInd);
      sAppend(&sbDt, "(double)%s(&_solveData->subjects[_cSub], %d, ", tf->v, tb.nInd++);
      foundF0=1;
    } else {
      sAppend(&sb, "(double)%s(&_solveData->subjects[_cSub], ", tf->v);
      sAppend(&sbDt, "(double)%s(&_solveData->subjects[_cSub], ", tf->v);
    }
    sAppend(&sbt, "%s(", tf->v);
    tf->i[0] = 1;// Parse next arguments
    tf->depth[0]=1;
    return 1;
  }
  return 0;
}

static inline int handleFunctionRbinom(transFunctions *tf) {
  if (!strcmp("rbinom", tf->v) ||
      !strcmp("rxbinom", tf->v) ||
      (tf->isInd = !strcmp("ribinom", tf->v))) {
    if (tb.thread != 0) tb.thread = 2;
    int ii = d_get_number_of_children(d_get_child(tf->pn,3))+1;
    if (ii != 2){
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("'ribinom'/'rbinom'/'rxbinom' takes 2 arguments 'rxbinom(size, prob)'"));
    } else {
      if (tf->isInd){
        sAppend(&sb,   "(double)ribinom(&_solveData->subjects[_cSub], %d, (int)" , tb.nInd);
        sAppend(&sbDt, "(double)ribinom(&_solveData->subjects[_cSub], %d, (int)", tb.nInd++);
        sAppendN(&sbt, "ribinom(", 8);
      } else {
        aAppendN("(double)rxbinom(&_solveData->subjects[_cSub], (int)", 51);
        sAppendN(&sbt, "rxbinom(", 8);
      }
    }
    tf->i[0]     = 1;// Parse next arguments
    tf->depth[0] =1;
    return 1;
  }
  return 0;
}

static inline int handleFunctionRnbinomMu(transFunctions *tf) {
  if (!strcmp("rnbinomMu", tf->v) ||
      !strcmp("rxnbinomMu", tf->v) ||
      (tf->isInd = !strcmp("rinbinomMu", tf->v))) {
    if (tb.thread != 0) tb.thread = 2;
    int ii = d_get_number_of_children(d_get_child(tf->pn,3))+1;
    if (ii != 2){
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("'rinbinomMu'/'rnbinomMu'/'rxnbinomMu' takes 2 arguments 'rxnbinomMu(size, mu)'"));
    } else {
      if (tf->isInd){
        sAppend(&sb,   "(double)rinbinomMu(&_solveData->subjects[_cSub], %d, (int)" , tb.nInd);
        sAppend(&sbDt, "(double)rinbinomMu(&_solveData->subjects[_cSub], %d, (int)", tb.nInd++);
        sAppendN(&sbt, "rinbinomMu(", 10);
      } else {
        aAppendN("(double)rxnbinomMu(&_solveData->subjects[_cSub], (int)", 54);
        sAppendN(&sbt, "rxnbinomMu(", 11);
      }
    }
    tf->i[0]     = 1;// Parse next arguments
    tf->depth[0] =1;
    return 1;
  }
  return 0;
}

static inline int handleFunctionRnbinom(transFunctions *tf) {
  if (!strcmp("rnbinom", tf->v) ||
      !strcmp("rxnbinom", tf->v) ||
      (tf->isInd = !strcmp("rinbinom", tf->v))) {
    if (tb.thread != 0) tb.thread = 2;
    int ii = d_get_number_of_children(d_get_child(tf->pn,3))+1;
    if (ii != 2){
      updateSyntaxCol();
      trans_syntax_error_report_fn(_("'rinbinom'/'rnbinom'/'rxnbinom' takes 2 arguments 'rxnbinom(size, prob)'"));
    } else {
      if (tf->isInd){
        sAppend(&sb,   "(double)rinbinom(&_solveData->subjects[_cSub], %d, (int)" , tb.nInd);
        sAppend(&sbDt, "(double)rinbinom(&_solveData->subjects[_cSub], %d, (int)", tb.nInd++);
        sAppendN(&sbt, "rinbinom(", 9);
      } else {
        aAppendN("(double)rxnbinom(&_solveData->subjects[_cSub], (int)", 52);
        sAppendN(&sbt, "rxnbinom(", 9);
      }
    }
    tf->i[0]     = 1;// Parse next arguments
    tf->depth[0] =1;
    return 1;
  }
  return 0;
}

