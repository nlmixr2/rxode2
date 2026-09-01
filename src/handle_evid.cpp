#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "rxomp.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "strncmp.h"
#define _(String) (String)
#include "../inst/include/rxode2.h"
#include "../inst/include/rxode2parseHandleEvid.h"
#include "../inst/include/rxode2parseGetTime.h"


extern "C" int handle_evidL(int evid, double *yp, double xout, int id, rx_solving_options_ind *ind) {
  if (ind->inLhs) {
    // In this case dosing to the extra compartments is OK so add it
    rx_solving_options *op = (ind->op ? ind->op : &op_global);
    return handle_evid(evid, op->neq + op->extraCmt, ind->BadDose,
                       ind->InfusionRate, ind->dose, yp,
                       xout, id, ind);

  } else {
    return isDose(evid);
  }
}

extern "C" void handleTlast(double *time, rx_solving_options_ind *ind) {
  handleTlastInline(time, ind);
}

// Linear compartment models/functions
// Note: Rf_errorcall is not thread-safe and cannot be called from
// within OpenMP parallel regions.  In parallel regions, return NA_REAL and
// set badSolve so the error is handled after the parallel region completes.
// In single-threaded context, call Rf_errorcall for an actionable error message.
extern "C" double _getDur(int l, rx_solving_options_ind *ind, int backward, unsigned int *p) {
  // Bounds-check before touching idose: getDoseNumber() dereferences
  // ind->idose[l], and callers can hand over a dose counter that has run past
  // the last dose (see the syncIdx() note in rxode2parseHandleEvid.h), so the
  // read has to be guarded ahead of the branch, not inside it.
  if (l < 0 || l >= ind->ndoses) {
    if (backward==2) return(NA_REAL);
    rx_solving_options *op = (ind->op ? ind->op : &op_global);
    if (omp_in_parallel()) {
      int newBadSolve = 1;
#pragma omp atomic write
      op->badSolve = newBadSolve;
      return NA_REAL;
    }
    (Rf_errorcall)(R_NilValue, l < 0 ?
                   "infusion start cannot be found (l <= 0)" :
                   "infusion end cannot be found (l >= ndoses)");
  }
  double dose = getDoseNumber(ind, l);
  // Pair on the event type as well as the amount.  getDoseNumber() returns the
  // amount alone, so a bolus of +amt matches an infusion end of -amt, and two
  // concurrent infusions running at the same rate into different compartments
  // match each other's ends.  A fixed rate/duration infusion -- the only kind
  // that reaches here, see handleTlastInlineUpateDosingInformation() -- emits
  // its stop record with the SAME internal evid as its start, so comparing the
  // evid rejects those pairings and keeps every real one.  This is the pairing
  // handleInfusionGetEndOfInfusionIndex() already performs.
  int curEvid = getEvid(ind, ind->idose[l]);
  if (backward==1){
    p[0] = 0;
    if (l != 0) {
      p[0] = l-1;
      while (p[0] > 0 &&
             (getDoseNumber(ind, p[0]) != -dose ||
              getEvid(ind, ind->idose[p[0]]) != curEvid)){
        p[0]--;
      }
    }
    // l == 0 has no earlier record to pair with, so the start is missing; it
    // must not fall through to the forward scan below.
    if (l == 0 || getDoseNumber(ind, p[0]) != -dose ||
        getEvid(ind, ind->idose[p[0]]) != curEvid){
      rx_solving_options *op = (ind->op ? ind->op : &op_global);
      if (omp_in_parallel()) {
        int newBadSolve = 1;
#pragma omp atomic write
        op->badSolve = newBadSolve;
        return NA_REAL;
      }
      (Rf_errorcall)(R_NilValue, "infusion start cannot be found");
    }
    return getAllTimes(ind, ind->idose[l]) - getAllTimes(ind, ind->idose[p[0]]);
  } else {
    p[0] = l+1;
    while (p[0] < ind->ndoses &&
           (getDoseNumber(ind, p[0]) != -dose ||
            getEvid(ind, ind->idose[p[0]]) != curEvid)){
      p[0]++;
    }
    // A scan that ran off the end must not be re-read: idose only holds ndoses
    // entries for this subject, so idose[ndoses] belongs to the next subject
    // (or is past gidose entirely for the last one) and can spuriously match.
    if (p[0] >= ind->ndoses || getDoseNumber(ind, p[0]) != -dose ||
        getEvid(ind, ind->idose[p[0]]) != curEvid){
      if (backward==2) return(NA_REAL);
      rx_solving_options *op = (ind->op ? ind->op : &op_global);
      if (omp_in_parallel()) {
        int newBadSolve = 1;
#pragma omp atomic write
        op->badSolve = newBadSolve;
        return NA_REAL;
      }
      (Rf_errorcall)(R_NilValue, "infusion end cannot be found");
    }
    return getAllTimes(ind, ind->idose[p[0]]) - getAllTimes(ind, ind->idose[l]);
  }
}

////////////////////////////////////////////////////////////////////////////////
// Test-only entry point for _getDur()
//
// _getDur()'s `backward == 1` branch is not reachable from inside rxode2 (the
// one internal caller always passes 2); it is only used through the `t_getDur`
// slot handed to generated model code and downstream packages.  This wrapper
// builds a minimal rx_solving_options_ind from R vectors so the pairing rules
// of both branches can be tested directly (nlmixr2/rxode2#1322).
extern "C" SEXP _rxode2_getDurTest(SEXP timeS, SEXP doseS, SEXP evidS,
                                   SEXP idoseS, SEXP lS, SEXP backwardS) {
  if (TYPEOF(timeS) != REALSXP || TYPEOF(doseS) != REALSXP ||
      TYPEOF(evidS) != INTSXP || TYPEOF(idoseS) != INTSXP ||
      TYPEOF(lS) != INTSXP || TYPEOF(backwardS) != INTSXP) {
    (Rf_errorcall)(R_NilValue, "'getDurTest' argument types are wrong");
  }
  int nRec = Rf_length(timeS);
  int nDose = Rf_length(idoseS);
  if (Rf_length(doseS) != nRec || Rf_length(evidS) != nRec ||
      Rf_length(lS) != 1 || Rf_length(backwardS) != 1) {
    (Rf_errorcall)(R_NilValue, "'getDurTest' argument lengths are wrong");
  }
  // this driver has no extra-dose pools, so a negative idose entry (which the
  // getDose()/getEvid() macros redirect into those pools) would dereference NULL
  int *idose = INTEGER(idoseS);
  for (int i = 0; i < nDose; ++i) {
    if (idose[i] < 0 || idose[i] >= nRec) {
      (Rf_errorcall)(R_NilValue, "'getDurTest' 'idose' is out of range");
    }
  }
  rx_solving_options_ind ind;
  rx_solving_options op;
  memset(&ind, 0, sizeof(rx_solving_options_ind));
  memset(&op, 0, sizeof(rx_solving_options));
  ind.op = &op;
  ind.all_times = REAL(timeS);
  ind.dose = REAL(doseS);
  ind.evid = INTEGER(evidS);
  ind.idose = idose;
  ind.n_all_times = nRec;
  ind.ndoses = nDose;
  unsigned int p = 0;
  double dur = _getDur(INTEGER(lS)[0], &ind, INTEGER(backwardS)[0], &p);
  SEXP ret = Rf_protect(Rf_allocVector(REALSXP, 2));
  REAL(ret)[0] = dur;
  REAL(ret)[1] = (double)p;
  Rf_unprotect(1);
  return ret;
}
