#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h> //Rmath includes math.
#include <R_ext/Rdynload.h>
#include "../inst/include/rxode2.h"
#include "strncmp.h"
#include "handle_evid.h"

int handle_evidL(int evid, double *yp, double xout, int id, rx_solving_options_ind *ind) {
  if (ind->inLhs) {
    // In this case dosing to the extra compartments is OK so add it
    rx_solving_options *op = &op_global;
    return handle_evid(evid, op->neq + op->extraCmt, ind->BadDose,
		       ind->InfusionRate, ind->dose, yp, 0,
		       xout, id, ind);

  } else {
    return isDose(evid);
  }
}

void handleTlast(double *time, rx_solving_options_ind *ind) {
  handleTlastInline(time, ind);
}
