#ifndef PARSELINCMTAPPLYCMTS_H
#define PARSELINCMTAPPLYCMTS_H
/*
 * This function adds a linear compartment from linCmt() to the model.
 *
 * @param ni is the node information (which is likely a dummy parsing node)
 *
 *@param cmt is the compartment name (const char *)
 *
 * @param linCmtErr is a pointer to an integer that will be set to 1 if there is an error
 *
 * These are called just before model variables are calculated.
 *
 */
static inline void addLinCmt(nodeInfo ni, const char *cmt, int *linCmtErr, int depotCentral) {
  if (new_de(cmt, fromDDT)) {
    add_de(ni, "linCmt()", cmt, 0, fromDDT);
    tb.idu[tb.id]=1;
  } else {
    if (tb.dprop[tb.id] == 0)  {
      // defined d/dt(depot) AND properties
      sAppend(&sbt, "'%s', ", cmt);
      if ((*linCmtErr & 1) == 0) {
        *linCmtErr += 1;
      }
    } else {
      tb.idu[tb.id]=1;
    }
  }
  int prop = tb.dprop[tb.id];

  // central and depot can have any properties
  if (depotCentral == 1) return;

  // Peripheral can have initial conditions and doses.  Rates are not allowed.
  if ((prop & propRate) != 0) {
    sAppend(&sbt2, "'rate(%s)', ", cmt);
    if ((*linCmtErr & 2) != 0) {
      *linCmtErr += 2;
    }
  }
  if ((prop & propDur) != 0) {
    sAppend(&sbt2, "'dur(%s)', ", cmt);
    if ((*linCmtErr & 2) != 0) {
      *linCmtErr += 2;
    }
  }
  if (depotCentral == 2) return;

  // All linear sensitivities should not have any properties (except
  // maybe initial conditions since they depend on initial conditions)
  /* if ((prop & prop0) != 0) { */
  /*   sAppend(&sbt2, "'%s(0)', ", cmt); */
  /* } */
  if ((prop & propF) != 0) {
    sAppend(&sbt2, "'f(%s)', ", cmt);
    if ((*linCmtErr & 2) != 0) {
      *linCmtErr += 2;
    }
  }
  if ((prop & propAlag) != 0) {
    sAppend(&sbt2, "'alag(%s)', ", cmt);
    if ((*linCmtErr & 2) != 0) {
      *linCmtErr += 2;
    }
  }
  if ((prop & propTad) != 0) {
    sAppend(&sbt2, "'tad(%s)', ", cmt);
    if ((*linCmtErr & 2) != 0) {
      *linCmtErr += 2;
    }
  }
  if ((prop & propTad0) != 0) {
    sAppend(&sbt2, "'tad0(%s)', ", cmt);
    if ((*linCmtErr & 2) != 0) {
      *linCmtErr += 2;
    }
  }
  if ((prop & propTafd) != 0) {
    sAppend(&sbt2, "'tafd(%s)', ", cmt);
    if ((*linCmtErr & 2) != 0) {
      *linCmtErr += 2;
    }
  }
  if ((prop & propTafd0) != 0) {
    sAppend(&sbt2, "'tafd0(%s)', ", cmt);
    if ((*linCmtErr & 2) != 0) {
      *linCmtErr += 2;
    }
  }
  if ((prop & propTlast) != 0) {
    sAppend(&sbt2, "'tlast(%s)', ", cmt);
    if ((*linCmtErr & 2) != 0) {
      *linCmtErr += 2;
    }
  }
  if ((prop & propTlast0) != 0) {
    sAppend(&sbt2, "'tlast0(%s)', ", cmt);
    if ((*linCmtErr & 2) != 0) {
      *linCmtErr += 2;
    }
  }
  if ((prop & propTfirst) != 0) {
    sAppend(&sbt2, "'tfirst(%s)', ", cmt);
    if ((*linCmtErr & 2) != 0) {
      *linCmtErr += 2;
    }
  }
  if ((prop & propTfirst0) != 0) {
    sAppend(&sbt2, "'tfirst0(%s)', ", cmt);
    if ((*linCmtErr & 2) != 0) {
      *linCmtErr += 2;
    }
  }
  if ((prop & propPodo) != 0) {
    sAppend(&sbt2, "'podo(%s)', ", cmt);
    if ((*linCmtErr & 2) != 0) {
      *linCmtErr += 2;
    }
  }
  if ((prop & propDose) != 0) {
    sAppend(&sbt2, "'dose(%s)', ", cmt);
    if ((*linCmtErr & 2) != 0) {
      *linCmtErr += 2;
    }
  }
  if ((prop & propPodo0) != 0) {
    sAppend(&sbt2, "'podo0(%s)', ", cmt);
    if ((*linCmtErr & 2) != 0) {
      *linCmtErr += 2;
    }
  }
  if ((prop & propDose0) != 0) {
    sAppend(&sbt2, "'dose0(%s)', ", cmt);
    if ((*linCmtErr & 2) != 0) {
      *linCmtErr += 2;
    }
  }
}

extern void calcLinCmt(void) {
  // Now we check for the linCmt extra compartments and then add them
  // as needed.
  nodeInfo ni;
  niReset(&ni);
  // we can use sbt.o since all the code has already been output
  sbt.o = 0;
  sbt.s[0] = 0;
  int linCmtErr = 0;
  int nLin = 0;
  int numSens = 0;
  int depot=0;
  if (tb.linCmt) {
    // See if all currently defined compartments are cmt()
    tb.linCmtCmt = 1;
    for (int i = 0; i < tb.statei; i++) {
      if (tb.didx[i] >= 0) {
        tb.linCmtCmt = 0;
        break;
      }
    }
    if (tb.hasKa) {
      addLinCmt(ni, "depot", &linCmtErr, 1); nLin++; depot=1;
    }
    addLinCmt(ni, "central", &linCmtErr, 1); nLin++;
    switch (tb.ncmt) {
    case 1:
      break;
    case 2:
      addLinCmt(ni, "peripheral1", &linCmtErr, 2); nLin++;
      break;
    case 3:
      addLinCmt(ni, "peripheral1", &linCmtErr, 2); nLin++;
      addLinCmt(ni, "peripheral2", &linCmtErr, 2); nLin++;
      break;
    }
    if (tb.linB) {
      // Sensitivities are also present.
      switch (tb.ncmt) {
      case 1:
        // here we have d_central
        addLinCmt(ni, "rx__sens_central_BY_p1", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_central_BY_v1", &linCmtErr, 0); numSens++;
        if (tb.hasKa) {
          addLinCmt(ni, "rx__sens_central_BY_ka", &linCmtErr, 0); numSens++;
          addLinCmt(ni, "rx__sens_depot_BY_ka", &linCmtErr, 0); numSens++;
        }
        break;
      case 2:
        // here we have d_central
        addLinCmt(ni, "rx__sens_central_BY_p1", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_central_BY_v1", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_central_BY_p2", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_central_BY_p3", &linCmtErr, 0); numSens++;
        if (tb.hasKa) {
          addLinCmt(ni, "rx__sens_central_BY_ka", &linCmtErr, 0); numSens++;
        }
        // Now d_perip1
        addLinCmt(ni, "rx__sens_peripheral1_BY_p1", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_peripheral1_BY_v1", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_peripheral1_BY_p2", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_peripheral1_BY_p3", &linCmtErr, 0); numSens++;
        if (tb.hasKa) {
          addLinCmt(ni, "rx__sens_peripheral1_BY_ka", &linCmtErr, 0); numSens++;
          addLinCmt(ni, "rx__sens_depot_BY_ka", &linCmtErr, 0); numSens++;
        }
        break;
      case 3:
        // here we have d_central
        addLinCmt(ni, "rx__sens_central_BY_p1", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_central_BY_v1", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_central_BY_p2", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_central_BY_p3", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_central_BY_p4", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_central_BY_p5", &linCmtErr, 0); numSens++;
        if (tb.hasKa) {
          addLinCmt(ni, "rx__sens_central_BY_ka", &linCmtErr, 0); numSens++;
        }
        // Now d_perip1
        addLinCmt(ni, "rx__sens_peripheral1_BY_p1", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_peripheral1_BY_v1", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_peripheral1_BY_p2", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_peripheral1_BY_p3", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_peripheral1_BY_p4", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_peripheral1_BY_p5", &linCmtErr, 0); numSens++;
        //
        if (tb.hasKa) {
          addLinCmt(ni, "rx__sens_peripheral1_BY_ka", &linCmtErr, 0); numSens++;
        }
        // Now d_perip2
        addLinCmt(ni, "rx__sens_peripheral2_BY_p1", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_peripheral2_BY_v1", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_peripheral2_BY_p2", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_peripheral2_BY_p3", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_peripheral2_BY_p4", &linCmtErr, 0); numSens++;
        addLinCmt(ni, "rx__sens_peripheral2_BY_p5", &linCmtErr, 0); numSens++;

        if (tb.hasKa) {
          addLinCmt(ni, "rx__sens_peripheral2_BY_ka", &linCmtErr, 0); numSens++;
          addLinCmt(ni, "rx__sens_depot_BY_ka", &linCmtErr, 0); numSens++;
        }
        break;
      }
    }
    // Take off trailing "',
    if (linCmtErr) {
      if ((linCmtErr & 1) != 0) {
        sbt.o -= 2;
        sbt.s[sbt.o] = 0;
        sAppendN(&sbt, " are required for linCmt() but defined in ODE too, rename ODEs\n",
                 63);
      }
      if ((linCmtErr & 2) != 0) {
        sbt2.o -= 2;
        sbt2.s[sbt2.o] = 0;
        sAppendN(&sbt2, " are properties not supported in linCmt() models, you can try ODEs instead\n",
                 75);
        sAppend(&sbt, "%s", sbt2.s);
        sbt2.o = 0;
      }
      trans_syntax_error_report_fn(sbt.s);
    }
    tb.linCmtFlg = numSens*100+nLin*10 + depot;
  }
}

#endif
