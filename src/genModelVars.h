#ifndef __genModelVars_H__
#define __genModelVars_H__
#pragma once
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>   /* dj: import intptr_t */
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rmath.h>
#include <unistd.h>
#include <errno.h>
#define _(String) (String)
#include "../inst/include/rxode2parse.h"
#include "../inst/include/rxode2parseSbuf.h"
#include "tran.h"
#include "../inst/include/rxode2parseVer.h"
#include "rxProtect.h"

static inline SEXP calcSLinCmt(void) {
  rxProtectGuard;
  SEXP sLinCmt = rxP(Rf_allocVector(INTSXP,17));
  INTEGER(sLinCmt)[0] = tb.ncmt;
  INTEGER(sLinCmt)[1] = tb.hasKa;
  INTEGER(sLinCmt)[2] = tb.linB;
  INTEGER(sLinCmt)[3] = tb.maxeta;
  INTEGER(sLinCmt)[4] = tb.maxtheta;
  INTEGER(sLinCmt)[6] = tb.linCmtN;
  INTEGER(sLinCmt)[7] = tb.linCmtFlg;
  INTEGER(sLinCmt)[8] = tb.nInd;
  INTEGER(sLinCmt)[9] = tb.simflg;
  INTEGER(sLinCmt)[10]= tb.thread;
  INTEGER(sLinCmt)[11]= tb.nLlik;
  INTEGER(sLinCmt)[12] = tb.ndiff;
  INTEGER(sLinCmt)[13] = tb.hasMix;
  INTEGER(sLinCmt)[14] = tb.evid_;
  INTEGER(sLinCmt)[15] = tb.hasDelay;
  INTEGER(sLinCmt)[16] = tb.linCmtBraw;

  SEXP sLinCmtN = rxP(Rf_allocVector(STRSXP, 17));
  SET_STRING_ELT(sLinCmtN, 0, Rf_mkChar("ncmt"));
  SET_STRING_ELT(sLinCmtN, 1, Rf_mkChar("ka"));
  SET_STRING_ELT(sLinCmtN, 2, Rf_mkChar("linB"));
  SET_STRING_ELT(sLinCmtN, 3, Rf_mkChar("maxeta"));
  SET_STRING_ELT(sLinCmtN, 4, Rf_mkChar("maxtheta"));
  SET_STRING_ELT(sLinCmtN, 5, Rf_mkChar("hasCmt"));
  SET_STRING_ELT(sLinCmtN, 6, Rf_mkChar("linCmt"));
  SET_STRING_ELT(sLinCmtN, 7, Rf_mkChar("linCmtFlg"));
  SET_STRING_ELT(sLinCmtN, 8, Rf_mkChar("nIndSim"));
  SET_STRING_ELT(sLinCmtN, 9, Rf_mkChar("simflg"));
  SET_STRING_ELT(sLinCmtN, 10, Rf_mkChar("thread"));
  SET_STRING_ELT(sLinCmtN, 11, Rf_mkChar("nLlik"));
  SET_STRING_ELT(sLinCmtN, 12, Rf_mkChar("ndiff"));
  SET_STRING_ELT(sLinCmtN, 13, Rf_mkChar("mix"));
  SET_STRING_ELT(sLinCmtN, 14, Rf_mkChar("evid_"));
  SET_STRING_ELT(sLinCmtN, 15, Rf_mkChar("hasDelay"));
  SET_STRING_ELT(sLinCmtN, 16, Rf_mkChar("linCmtBraw"));
  Rf_setAttrib(sLinCmt,   R_NamesSymbol, sLinCmtN);
  rxUPAll();
  return(sLinCmt);
}

static inline SEXP calcVersionInfo(void) {
  rxProtectGuard;
  SEXP version  = rxP(Rf_allocVector(STRSXP, 3));
  SEXP versionn = rxP(Rf_allocVector(STRSXP, 3));

  SET_STRING_ELT(versionn,0,Rf_mkChar("version"));
  SET_STRING_ELT(versionn,1,Rf_mkChar("repo"));
  SET_STRING_ELT(versionn,2,Rf_mkChar("md5"));

  SET_STRING_ELT(version,0,Rf_mkChar(__VER_ver__));
  SET_STRING_ELT(version,1,Rf_mkChar(__VER_repo__));
  SET_STRING_ELT(version,2,Rf_mkChar(__VER_md5__));
  Rf_setAttrib(version,   R_NamesSymbol, versionn);
  rxUPAll();
  return version;
}

static inline void calcNparamsNlhsNslhs(void) {
  int sli=0, li=0, pi=0;
  for (int i=0; i<NV; i++) {
    int islhs = tb.lh[i];
    if (islhs>1 &&
        islhs != isLhsStateExtra &&
        islhs != isLHSparam &&
        islhs != isSuppressedLHS &&
        islhs != isLHSstr &&
        islhs != isSuppressedLHSstr) {
      continue;      /* is a state var */
    }
    if (islhs == isSuppressedLHS ||
        islhs == isSuppressedLHSstr){
      sli++;
    } else if (islhs == isLHS ||
               islhs == isLHSstr ||
               islhs == isLhsStateExtra ||
               islhs == isLHSparam ||
               islhs == isLHSstr){
      li++;
      if (islhs == isLHSparam) pi++;
    } else {
      pi++;
    }
  }
  tb.pi=pi;
  tb.li=li;
  tb.sli=sli;
}

static inline void calcNextra(void) {
  int offCmt=0,nExtra = 0;
  char *buf=NULL, buf2[200];
  for (int i = 0; i < tb.statei; i++) {
    if (offCmt == 0 && tb.idu[i] == 0){
      buf=tb.ss.line[tb.di[i]];
      offCmt = 1;
      nExtra++;
    } else if (offCmt == 1 && tb.idu[i] == 1) {
      // There is an compartment that doesn't have a derivative
      if (tb.linCmt == 0){
        char *v = rc_dup_str(buf, 0);
        snprintf(buf2, 200, "compartment '%s' needs differential equations defined", v);
        updateSyntaxCol();
        trans_syntax_error_report_fn0(buf2);
      } else {
        char *b2=tb.ss.line[tb.di[i]];
        if (strcmp(b2, "depot") == 0 ||
            strcmp(b2, "central") == 0 ||
            strcmp(b2, "peripheral1") == 0 ||
            strcmp(b2, "peripheral2") == 0 ||
            strcmp(b2, "rx__sens_central_BY_p1") == 0 ||
            strcmp(b2, "rx__sens_central_BY_v1") == 0 ||
            strcmp(b2, "rx__sens_central_BY_p2") == 0 ||
            strcmp(b2, "rx__sens_central_BY_p3") == 0 ||
            strcmp(b2, "rx__sens_central_BY_p4") == 0 ||
            strcmp(b2, "rx__sens_central_BY_ka") == 0 ||
            strcmp(b2, "rx__sens_peripheral1_BY_p1") == 0 ||
            strcmp(b2, "rx__sens_peripheral1_BY_v1") == 0 ||
            strcmp(b2, "rx__sens_peripheral1_BY_p2") == 0 ||
            strcmp(b2, "rx__sens_peripheral1_BY_p3") == 0 ||
            strcmp(b2, "rx__sens_peripheral1_BY_p4") == 0 ||
            strcmp(b2, "rx__sens_peripheral1_BY_ka") == 0 ||
            strcmp(b2, "rx__sens_peripheral2_BY_p1") == 0 ||
            strcmp(b2, "rx__sens_peripheral2_BY_v1") == 0 ||
            strcmp(b2, "rx__sens_peripheral2_BY_p2") == 0 ||
            strcmp(b2, "rx__sens_peripheral2_BY_p3") == 0 ||
            strcmp(b2, "rx__sens_peripheral2_BY_p4") == 0 ||
            strcmp(b2, "rx__sens_peripheral2_BY_ka") == 0 ||
            strcmp(b2, "rx__sens_depot_BY_ka") == 0) {
          continue;
        }
        // If there is only a linear compartment model AND this is a cmt() item, then
        // this should be an extra compartment.
        if (tb.linCmtCmt == 1 && tb.didx[i] < 0) {
          buf=tb.ss.line[tb.di[i]];
          offCmt = 1;
          nExtra++;
        } else if (tb.linCmtCmt == 1) {
        } else {
          char *v = rc_dup_str(buf, 0);
          snprintf(buf2, 200, _("compartment '%s' needs differential equations defined"), v);
          updateSyntaxCol();
          trans_syntax_error_report_fn0(buf2);
        }
      }
    } else if (offCmt == 1 && tb.idu[i] == 0){
      nExtra++;
    }
  }
  tb.nExtra=nExtra;
}

static inline void assertNoLinCmtDepotCentral(void) {
  extraCmt = 0;
  if (tb.linCmt){
    if (tb.hasDepotCmt == -1){
      trans_syntax_error_report_fn0(_("'cmt(depot)' does not work with 'linCmt()'"));
    }
    if (tb.hasCentralCmt == -1) {
      trans_syntax_error_report_fn0("'cmt(central)' does not work with 'linCmt()'");
    }

    if (tb.hasDepotCmt == 1 && !tb.hasKa) {
      trans_syntax_error_report_fn0("'tad(depot)' and related functions require an depot compartment in 'linCmt()'");
    }
 }
}

static inline SEXP calcIniVals(void) {
  rxProtectGuard;
  SEXP inin  = rxP(Rf_allocVector(STRSXP, tb.isPi + tb.ini_i));
  SEXP ini   = rxP(Rf_allocVector(REALSXP, tb.isPi + tb.ini_i));
  char *buf;
  for (int i=tb.isPi + tb.ini_i;i--;) REAL(ini)[i] = NA_REAL;
  int ini_i=0;
  int redo = 0;
  for (int i = 0; i < NV; i++){
    buf=tb.ss.line[i];
    if (tb.ini[i] == 1 && tb.lh[i] != isLHS){
      if (tb.isPi && !strcmp("pi", buf)) {
        redo=1;
        tb.isPi=0;
        break;
      }
      SET_STRING_ELT(inin,ini_i,Rf_mkChar(buf));
      REAL(ini)[ini_i++] = tb.iniv[i];
    }
  }
  if (tb.isPi){
    SET_STRING_ELT(inin,ini_i,Rf_mkChar("pi"));
    REAL(ini)[ini_i++] = M_PI;
  } else if (redo){
    inin  = rxP(Rf_allocVector(STRSXP, tb.ini_i));
    ini   = rxP(Rf_allocVector(REALSXP, tb.ini_i));
    for (int i = tb.ini_i; i--;) REAL(ini)[i] = NA_REAL;
    ini_i=0;
    for (int i = 0; i < NV; i++){
      buf=tb.ss.line[i];
      if (tb.ini[i] == 1 && tb.lh[i] != isLHS){
        if (tb.isPi && !strcmp("pi", buf)) {
          redo=1;
          tb.isPi=0;
          break;
        }
        SET_STRING_ELT(inin,ini_i,Rf_mkChar(buf));
        REAL(ini)[ini_i++] = tb.iniv[i];
      }
    }
  }
  tb.ini_i = ini_i;

  Rf_setAttrib(ini,   R_NamesSymbol, inin);
  rxUPAll();
  return ini;
}

SEXP orderForderS1(SEXP ordIn);

static inline int sortStateVectorsErrHandle(int prop, int i) {
  if ((prop & ~(propDoseRef | propDelay)) == 0 || tb.dummyLhs == 1) {
    return 1;
  }
  char *buf = NULL;
  buf = tb.ss.line[tb.di[i]];
  // Every property that reaches here appends "'<name>', " below, and the
  // trailing "', " is then trimmed with sbt.o -= 2.  A property with no branch
  // appends nothing, so that trim would move sbt.o *before* the start of the
  // buffer and write there -- corrupting the heap rather than reporting the
  // error.  Remember where this message starts so the trim can be anchored.
  int sbtStart = sbt.o;
  if ((prop & prop0) != 0) {
    sAppend(&sbt, "'%s(0)', ", buf);
  }
  if ((prop & propF) != 0) {
    sAppend(&sbt, "'f(%s)', ", buf);
  }
  if ((prop & propAlag) != 0) {
    sAppend(&sbt, "'alag(%s)', ", buf);
  }
  if ((prop & propRate) != 0) {
    sAppend(&sbt, "'rate(%s)', ", buf);
  }
  if ((prop & propDur) != 0) {
    sAppend(&sbt, "'dur(%s)', ", buf);
  }
  if ((prop & propTad) != 0) {
    sAppend(&sbt, "'tad(%s)', ", buf);
  }
  if ((prop & propTad0) != 0) {
    sAppend(&sbt, "'tad0(%s)', ", buf);
  }
  if ((prop & propTafd) != 0) {
    sAppend(&sbt, "'tafd(%s)', ", buf);
  }
  if ((prop & propTafd0) != 0) {
    sAppend(&sbt, "'tafd0(%s)', ", buf);
  }
  if ((prop & propTlast) != 0) {
    sAppend(&sbt, "'tlast(%s)', ", buf);
  }
  if ((prop & propTlast0) != 0) {
    sAppend(&sbt, "'tlast0(%s)', ", buf);
  }
  if ((prop & propTfirst) != 0) {
    sAppend(&sbt, "'tfirst(%s)', ", buf);
  }
  if ((prop & propTfirst0) != 0) {
    sAppend(&sbt, "'tfirst0(%s)', ", buf);
  }
  if ((prop & propPodo) != 0) {
    sAppend(&sbt, "'podo(%s)', ", buf);
  }
  if ((prop & propDose) != 0) {
    sAppend(&sbt, "'dose(%s)', ", buf);
  }
  if ((prop & propPodo0) != 0) {
    sAppend(&sbt, "'podo0(%s)', ", buf);
  }
  if ((prop & propDose0) != 0) {
    sAppend(&sbt, "'dose0(%s)', ", buf);
  }
  // past(state, tau) <- expr supplies the pre-history that delay(state, tau)
  // interpolates, so it too requires the state to have a d/dt().
  if ((prop & propPast) != 0) {
    sAppend(&sbt, "'past(%s)', ", buf);
  }
  if (sbt.o == sbtStart) {
    // No branch above matched, so there is no trailing "', " to trim and
    // nothing to report; a property known only to the guard above is not an
    // error the user can act on.  Trimming here would underflow the buffer.
    return 1;
  }
  // Take off trailing "',
  sbt.o -= 2;
  sbt.s[sbt.o] = 0;
  sAppend(&sbt, " present, but d/dt(%s) not defined\n", buf);
  return 0;
}

static inline SEXP sortStateVectors(SEXP ordS) {
  int *ord = INTEGER(ordS);
  for (int i = 0; i < Rf_length(ordS); i++) {
    ord[i] = 0; // explicitly initialize to avoid valgrind warning
  }
  sbt.o = 0; // we can use sbt.o since all the code has already been output
  sbt.s[0] = 0;

  for (int i = 0; i < tb.de.n; i++) {
    int cur = tb.didx[i];
    int prop = tb.dprop[i];
    // delay(state, T) interpolates the dense history of a differential
    // state, so its first argument must be a real ODE state (idu==1, i.e.
    // it has a d/dt() equation).  A non-state (parameter/covariate) passed
    // to delay() is registered here as an extra compartment with the
    // propDelay bit but no derivative; reject it instead of silently
    // swallowing it as an algebraic observable.
    if ((prop & propDelay) != 0 && tb.idu[i] == 0) {
      char *bufd = tb.ss.line[tb.di[i]];
      sAppend(&sbt, "the first argument to 'delay()' must be an ODE state with a 'd/dt()' defined, but 'delay(%s, ...)' has no 'd/dt(%s)'\n", bufd, bufd);
      continue;
    }
    if (cur == 0) {
      // This has a property without an ODE or cmt() statement; should error here.
      if (sortStateVectorsErrHandle(prop, i)) continue;
    } else if (cur < 0) {
      // This is a compartment only defined by CMT() and is used for
      // dvid ordering, no properties should be defined.
      ord[i] = -cur;
      // Extra states (idu == 0) are algebraic observables that also appear
      // in a cmt() statement; they are not real ODE/dosing compartments and
      // etTran() numbers them after the real states (state ++ extraState).
      // Push them to the end of this ordering too so the generated
      // __DDT__/_DEPOT_/_CENTRAL_ slot indices stay aligned with the runtime
      // compartment numbering.  Otherwise an observable whose cmt() is seen
      // before the real compartments are numbered sorts ahead of them and
      // shifts every tad()/tlast(<state>) lookup.  This happens whenever the
      // cmt() precedes the d/dt() block, and always in linCmt() models, where
      // the linear compartments (depot, central, ...) are added last in
      // calcLinCmt().
      if (tb.idu[i] == 0) ord[i] += tb.de.n;
      if (sortStateVectorsErrHandle(prop, i)) continue;
    } else {
      ord[i] = cur;
      if (tb.idu[i] == 0) ord[i] += tb.de.n;
    }
  }
  if (sbt.o != 0) {
    sbt.o--; // remove last newline
    sbt.s[sbt.o] = 0;
    sPrint(&_gbuf, "%s", sbt.s);
    return R_NilValue;
  }
  return orderForderS1(ordS);
}

static inline void populateStateVectors(SEXP state, SEXP sens, SEXP normState, int *stateRm, SEXP extraState, SEXP stateProp, SEXP sensProp, SEXP normProp, int *ordFp) {
  int k=0, j=0, m=0, p=0;
  char *buf;
  int *statePropI = INTEGER(stateProp);
  int *sensPropI = INTEGER(sensProp);
  int *normPropI = INTEGER(normProp);
  for (int i=0; i<tb.de.n; i++) {                     /* name state vars */
    buf=tb.ss.line[tb.di[ordFp[i]-1]];
    /* REprintf("%s...idu[] %d\n", buf, tb.idu[ordFp[i]-1]); */
    if (tb.idu[ordFp[i]-1] == 1) {
      if (strncmp(buf,"rx__sens_", 9) == 0){
        statePropI[k] = tb.dprop[ordFp[i]-1];
        sensPropI[j] = tb.dprop[ordFp[i]-1];
        SET_STRING_ELT(sens,j++,Rf_mkChar(buf));
        SET_STRING_ELT(state,k++,Rf_mkChar(buf));
        stateRm[k-1]=tb.idi[ordFp[i]-1];
      } else {
        statePropI[k] = tb.dprop[ordFp[i]-1];
        normPropI[m] = tb.dprop[ordFp[i]-1];
        SET_STRING_ELT(normState,m++,Rf_mkChar(buf));
        SET_STRING_ELT(state,k++,Rf_mkChar(buf));
        stateRm[k-1]=tb.idi[ordFp[i]-1];
      }
    } else {
      SET_STRING_ELT(extraState, p++, Rf_mkChar(buf));
    }
  }
}

// Replay the recorded assignments and indLin() forcings in source order to work
// out which forcings depend on a compartment.  dep[i] tracks whether symbol i
// currently holds something derived from a state, so a forcing that reaches a
// state only through an assigned variable (cp = central/20; indLin(central) <-
// -vmax*cp/(km+cp)) is seen, while one whose variable was reassigned to
// something state free before it is read is not.  A statement inside an
// if/while/ifelse may not run, so it adds to what is already known instead of
// replacing it -- the conservative direction, since a missed state would be
// solved without the inductive iteration it needs.  fdep[] is the same for each
// compartment's forcing.
// A compartment is state dependent by definition; 2 marks the state itself so
// indLinStmtTarget() can refuse to overwrite it.
static inline void indLinSeedStateDep(int *dep) {
  for (int i = 0; i < NV; ++i) {
    dep[i] = 0;
    for (int d = 0; d < tb.de.n; ++d) {
      if (!strcmp(tb.ss.line[i], tb.de.line[d])) {
        dep[i] = 2;
        break;
      }
    }
  }
}

// Does statement `s` read anything that currently holds a state?
static inline int indLinStmtReadsDep(int s, int *dep) {
  int r1 = tb.stmtR0[s] + tb.stmtRn[s];
  for (int r = tb.stmtR0[s]; r < r1; ++r) {
    if (dep[tb.stmtRef[r]]) return 1;
  }
  return 0;
}

// The slot statement `s` writes, or -1 when it writes nothing trackable.  A
// state is never assigned, and must stay flagged if the parser ever routes one
// through here.
static inline int indLinStmtTarget(int s, int *dep) {
  int t = tb.stmtT[s];
  if (tb.stmtK[s] == 0) {
    if (t < 0 || t >= NV || dep[t] == 2) return -1;
  } else if (t < 0 || t >= tb.de.n) {
    return -1;
  }
  return t;
}

// `fany` (optional) records which compartments have an indLin() forcing at all,
// state dependent or not.
static inline void indLinReplay(int *dep, int *fdep, int *fany) {
  indLinSeedStateDep(dep);
  for (int d = 0; d < tb.de.n; ++d) {
    fdep[d] = 0;
    if (fany != NULL) fany[d] = 0;
  }
  for (int s = 0; s < tb.stmtN; ++s) {
    int t = indLinStmtTarget(s, dep);
    if (t < 0) continue;
    int v = indLinStmtReadsDep(s, dep);
    int *cur = (tb.stmtK[s] == 0) ? dep : fdep;
    cur[t] = tb.stmtC[s] ? (cur[t] || v) : v;
    if (tb.stmtK[s] == 1 && fany != NULL) fany[t] = 1;
  }
}

// Walk back from symbol `j` through whatever gave it its state dependence
// until a compartment is reached, so the error message can name something the
// user actually wrote.  Bounded by the statement count, so a self-referential
// chain cannot spin.
static inline int indLinDepSource(int j, int *dep) {
  int cur = j;
  for (int guard = 0; guard <= tb.stmtN; ++guard) {
    if (dep[cur] == 2) return cur;
    int found = -1;
    for (int s = tb.stmtN; s--;) {
      if (tb.stmtK[s] != 0 || tb.stmtT[s] != cur) continue;
      int r1 = tb.stmtR0[s] + tb.stmtRn[s];
      for (int r = tb.stmtR0[s]; r < r1; ++r) {
        if (dep[tb.stmtRef[r]]) { found = tb.stmtRef[r]; break; }
      }
      if (found >= 0) break;
    }
    if (found < 0) return cur;
    cur = found;
  }
  return cur;
}

// A matExp() rate constant that reads a compartment is not a rate constant: the
// matrix exponential is only valid when the rate matrix is constant over the
// step, which is also what the event-sensitivity jump code assumes
// (rxode2parseHandleEvid.h).  Report the first one, naming the constant and the
// compartment it reaches, and point at indLin() -- which is where a
// state-dependent term belongs and where the solver can iterate it.
//
// Sensitivity models are held to the same rule.  rxSensMatExp() takes its rate
// matrix from the same term-wise split the plain conversion uses, so every rate
// constant it emits -- primal, homogeneous, non-depleting cross term, at every
// order -- is state free, and the nonlinear part rides in the indLin() forcing
// (rxode2#1187).
static inline void assertNoStateDependentMicro(void) {
  if (!tb.isMexp || tb.de.n <= 0 || NV <= 0) return;
  int *dep  = (int*)R_alloc(NV, sizeof(int));
  int *fdep = (int*)R_alloc(tb.de.n, sizeof(int));
  indLinReplay(dep, fdep, NULL);
  char cmt1[100], cmt2[100];
  for (int j = 0; j < NV; ++j) {
    if (dep[j] != 1) continue;
    if (!parse_micro_constant(tb.ss.line[j], cmt1, cmt2)) continue;
    int have1 = 0, have2 = 0;
    for (int d = 0; d < tb.de.n; ++d) {
      if (!strcmp(tb.de.line[d], cmt1)) have1 = 1;
      if (!strcmp(tb.de.line[d], cmt2)) have2 = 1;
    }
    if (!have1 || !have2) continue;
    const char *src = tb.ss.line[indLinDepSource(j, dep)];
    updateSyntaxCol();
    sPrint(&_bufw,
           _("matrix exponential rate constant '%s' depends on the compartment '%s'; rate constants must be constant in the states -- put the state-dependent part in 'indLin(%s) <- ...' instead"),
           tb.ss.line[j], src, cmt1);
    trans_syntax_error_report_fn0(_bufw.s);
    break;
  }
}

// modelVars$indLin$wIndLin: the 0-indexed positions in modelVars$state whose
// indLin() forcing depends on a state (named with those states for reading).  A
// forcing built only from parameters/covariates (eg indLin(Gc) <- Gprod) stays
// unflagged and keeps the cheap non-iterating path.
static inline SEXP calcWIndLin(SEXP state) {
  rxProtectGuard;
  int ns = Rf_length(state);
  int *dep  = (int*)R_alloc(NV > 0 ? NV : 1, sizeof(int));
  int *fdep = (int*)R_alloc(tb.de.n > 0 ? tb.de.n : 1, sizeof(int));
  int *fany = (int*)R_alloc(tb.de.n > 0 ? tb.de.n : 1, sizeof(int));
  indLinReplay(dep, fdep, fany);
  int *isDep = (int*)R_alloc(ns > 0 ? ns : 1, sizeof(int));
  int n = 0;
  for (int k = 0; k < ns; ++k) {
    isDep[k] = 0;
    for (int d = 0; d < tb.de.n; ++d) {
      if (!strcmp(tb.de.line[d], CHAR(STRING_ELT(state, k)))) {
        // A linCmt() concentration moves continuously within a step, so a
        // forcing in a model that has one cannot be treated as constant over
        // the interval the way a locf covariate can -- take the iterating path
        // so the driver re-evaluates and refines it (rxode2#1215).
        isDep[k] = fdep[d] || (tb.linCmt && fany[d]);
        break;
      }
    }
    if (isDep[k]) n++;
  }
  SEXP w  = rxP(Rf_allocVector(INTSXP, n));
  SEXP wn = rxP(Rf_allocVector(STRSXP, n));
  int *wi = INTEGER(w);
  for (int k = 0, j = 0; k < ns; ++k) {
    if (!isDep[k]) continue;
    wi[j] = k;
    SET_STRING_ELT(wn, j, STRING_ELT(state, k));
    j++;
  }
  Rf_setAttrib(w, R_NamesSymbol, wn);
  rxUPAll();
  return w;
}

static inline void populateDfdy(SEXP dfdy) {
  char *df, *dy;
  for (int i=0; i<tb.ndfdy; i++) {                     /* name state vars */
    df=tb.ss.line[tb.df[i]];
    dy=tb.ss.line[tb.dy[i]];
    int foundIt=0;
    for (int j = 1; j <= tb.maxtheta;j++){
      sPrint(&_bufw,"_THETA_%d_",j);
      if (!strcmp(dy,_bufw.s)){
        sPrint(&_bufw,"THETA[%d]",j);
        foundIt=1;
        break;
      }
    }
    if (!foundIt){
      for (int j = 1; j <= tb.maxeta;j++){
        sPrint(&_bufw,"_ETA_%d_",j);
        if (!strcmp(dy,_bufw.s)){
          sPrint(&_bufw,"ETA[%d]",j);
        }
      }
    }
    if (!foundIt){
      sClear(&_bufw);
      sPrint(&_bufw,"%s",dy);
    }
    sPrint(&_bufw2,"df(%s)/dy(%s)",df,_bufw.s);
    SET_STRING_ELT(dfdy,i,Rf_mkChar(_bufw2.s));
  }
}

static inline int assertStateCannotHaveDiff(int islhs, int i, char *buf) {
  if (islhs>1 && islhs != isLhsStateExtra && islhs != isLHSparam &&
      islhs != isLHSstr) {
    if (tb.lag[i] != 0){
      buf=tb.ss.line[i];
      if (islhs == isState){
        sPrint(&_bufw, _("state '%s': 'lag', 'lead', 'first', 'last', 'diff' not legal"), buf);
        trans_syntax_error_report_fn0(_bufw.s);
      } else if (islhs == 10 || islhs == 11){
        sPrint(&_bufw, _("suppress '%s': 'lag', 'lead', 'first', 'last', 'diff' not legal"), buf);
        trans_syntax_error_report_fn0(_bufw.s);
      }
    }
    return 1;
  }
  return 0;
}

static inline int setLhsAndDualLhsParam(int islhs, SEXP lhs, SEXP params, char *buf,
                                        int *li, int *pi, SEXP lhsStr, int *lhsOrd,
                                        int *i) {
  if (islhs == isLHS || islhs == isLHSstr ||
      islhs == isLhsStateExtra || islhs == isLHSparam) {
    SET_STRING_ELT(lhs, li[0], Rf_mkChar(buf));
    lhsOrd[li[0]] = tb.lho[i[0]];
    INTEGER(lhsStr)[li[0]] = islhs == isLHSstr;
    li[0] = li[0]+1;
    if (islhs == isLHSparam) {
      if (!strcmp("CMT", buf)) {
        tb.hasCmt = 1;
      }
      SET_STRING_ELT(params, pi[0], Rf_mkChar(buf));
      pi[0] = pi[0]+1;
    }
    return 1;
  }
  return 0;
}

static inline void paramSubThetaEtaToBufw(char *buf) {
  int foundIt=0;
  for (int j = 1; j <= tb.maxtheta;j++){
    sPrint(&_bufw,"_THETA_%d_",j);
    if (!strcmp(buf, _bufw.s)){
      sPrint(&_bufw,"THETA[%d]",j);
      foundIt=1;
      break;
    }
  }
  if (!foundIt){
    for (int j = 1; j <= tb.maxeta;j++){
      sPrint(&_bufw,"_ETA_%d_",j);
      if (!strcmp(buf, _bufw.s)){
        sPrint(&_bufw,"ETA[%d]",j);
        foundIt=1;
        break;
      }
    }
  }
  if (!foundIt){
    sPrint(&_bufw, "%s", buf);
  }
  if (!strcmp("CMT", _bufw.s)) {
    tb.hasCmt = 1;
  }
}

static inline void assertLhsAndDualLhsDiffNotLegal(int islhs, int i, char *buf) {
  if (tb.lag[i] != 0){
    if (islhs == isLHSparam){
      sPrint(&_bufw, _("redefined '%s': 'lag', 'lead', 'first', 'last', 'diff' not legal"), buf);
      trans_syntax_error_report_fn0(_bufw.s);
    } else if (islhs == isLHS && tb.lag[i] != 1){
      sPrint(&_bufw, _("lhs '%s': only 'lag(%s,1)' and 'diff(%s,1)' supported"), buf, buf, buf);
      trans_syntax_error_report_fn0(_bufw.s);
    }
  }
}

static inline void populateParamsLhsSlhs(SEXP params, SEXP lhs, SEXP slhs, int *interp, SEXP lhsStr, int *lhsOrd) {
  int li=0, pi=0, sli = 0;
  char *buf;
  for (int i=0; i<NV; i++) {
    int islhs = tb.lh[i];
    if (islhs == isSuppressedLHS || islhs == isSuppressedLHSstr){
      SET_STRING_ELT(slhs, sli++, Rf_mkChar(tb.ss.line[i]));
    }
    buf=tb.ss.line[i];

    if (assertStateCannotHaveDiff(islhs, i, buf)) continue;
    assertLhsAndDualLhsDiffNotLegal(islhs, i, buf);
    /* is a state var */
    if (!setLhsAndDualLhsParam(islhs, lhs, params, buf, &li, &pi, lhsStr, lhsOrd, &i)) {
      paramSubThetaEtaToBufw(buf);
      interp[pi] = tb.interp[i] + 1; // Makes into a legible factor
      SET_STRING_ELT(params, pi++, Rf_mkChar(_bufw.s));
    }
  }
}

SEXP generateModelVars(void);

#endif  // __genModelVars_H__
