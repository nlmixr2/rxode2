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
#include <rxode2parseHandleEvid.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif


int handle_evidL(int evid, double *yp, double xout, rx_solving_options_ind *ind) {
  if (ind->inLhs) {
    // In this case dosing to the extra compartments is OK so add it
    return handle_evid(evid, yp, xout, ind);
  } else {
    return isDose(evid);
  }
}

void handleTlast(double *time, rx_solving_options_ind *ind) {
  handleTlastInline(time, ind);
}
