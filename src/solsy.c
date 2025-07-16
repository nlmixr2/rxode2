#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "lsoda.h"
#include "lsoda_internal.h"
#include "common.h"
#include <stdio.h>
#include <stdlib.h>
#include "blas.h"
#include <R.h>
#include <Rinternals.h>

int solsy(struct lsoda_context_t * ctx, double *y)

/*
   This routine manages the solution of the linear system arising from
   a chord iteration.  It is called if _rxC(miter) != 0.
   If _rxC(miter) is 2, it calls dgesl to accomplish this.
   If _rxC(miter) is 5, it calls dgbsl.

   y = the right-hand side vector on input, and the solution vector
       on output.
*/

{
	const int neq = ctx->neq;
	if (_rxC(miter) != 2) {
		/* miter has to be 2. the miter=5 case is not implemented. */
		Rf_errorcall(R_NilValue, "liblsoda does not implement this. (solsy)");
	}
	if (_rxC(miter) == 2)
		dgesl0(_rxC(wm), neq, _rxC(ipvt), y, 0);
	return 1;

}
