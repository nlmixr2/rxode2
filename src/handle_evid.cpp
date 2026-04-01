#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "rxomp.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "strncmp.h"
#define _(String) (String)
#include "../inst/include/rxode2.h"
#include "../inst/include/rxode2parseHandleEvid.h"
#include "../inst/include/rxode2parseGetTime.h"


extern "C" int handle_evidL(int evid, double *yp, double xout, int id, rx_solving_options_ind *ind) {
  if (ind->inLhs) {
    // In this case dosing to the extra compartments is OK so add it
    rx_solving_options *op = &op_global;
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
  double dose = getDoseNumber(ind, l);
  if (backward==1 && l != 0){
    if (l <= 0) {
      rx_solving_options *op = &op_global;
      if (omp_in_parallel()) {
        int newBadSolve = 1;
#pragma omp atomic write
        op->badSolve = newBadSolve;
        return NA_REAL;
      }
      (Rf_errorcall)(R_NilValue, "infusion start cannot be found (l <= 0)");
    }
    p[0] = l-1;
    while (p[0] > 0 && getDoseNumber(ind, p[0]) != -dose){
      p[0]--;
    }
    if (getDoseNumber(ind, p[0]) != -dose){
      rx_solving_options *op = &op_global;
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
    if (l >= ind->ndoses) {
      if (backward==2) return(NA_REAL);
      rx_solving_options *op = &op_global;
      if (omp_in_parallel()) {
        int newBadSolve = 1;
#pragma omp atomic write
        op->badSolve = newBadSolve;
        return NA_REAL;
      }
      (Rf_errorcall)(R_NilValue, "infusion end cannot be found (l >= ndoses)");
    }
    p[0] = l+1;
    while (p[0] < ind->ndoses && getDoseNumber(ind, p[0]) != -dose){
      p[0]++;
    }
    if (getDoseNumber(ind, p[0]) != -dose){
      if (backward==2) return(NA_REAL);
      rx_solving_options *op = &op_global;
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
