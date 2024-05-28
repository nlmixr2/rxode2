#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <math.h>
#include "lsoda.h"
#include "blas.h"
#include "common.h"
#include "lsoda_internal.h"

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

void RSprintf(const char *format, ...);

int prja(struct lsoda_context_t * ctx, double *y)
{
	int             i, ier, j;
	double          fac, hl0, r, r0, yj;
	const int neq = ctx->neq;
/*
   prja is called by stoda to compute and process the matrix
   P = I - _rxC(h) * _rxC(el)[1] * J, where J is an approximation to the Jacobian.
   Here J is computed by finite differencin.
   J, scaled by -_rxC(h) * _rxC(el)[1], is stored in _rxC(wm).  Then the norm of J ( the
   matrix norm consistent with the weihted max-norm on vectors given
   by vmnorm ) is computed, and J is overwritten by P.  P is then
   subjected to LU decomposition in preparation for later solution
   of linear systems with p as coefficient matrix.  This is done
   by defa if _rxC(miter) = 2, and by dgbfa if _rxC(miter) = 5.
*/
	_rxC(nje)++;
	hl0 = _rxC(h) * _rxC(el)[1];
/*
   If _rxC(miter) = 2, make neq calls to f to approximate J.
*/
	if (_rxC(miter) != 2) {
	  RSprintf(_("[prja] _rxC(miter) != 2\n"));
		return 0;
	}
	if (_rxC(miter) == 2) {
		fac = vmnorm0(neq, _rxC(savf), _rxC(ewt));
		r0 = 1000. * fabs(_rxC(h)) * ETA * ((double) neq) * fac;
		if (r0 == 0.)
			r0 = 1.;
		for (j = 1; j <= neq; j++) {
			yj = y[j];
			r = fmax(SQRTETA * fabs(yj), r0 / _rxC(ewt)[j]);
			y[j] += r;
			fac = -hl0 / r;
			(*ctx->function) (_rxC(tn), y + 1, _rxC(acor) + 1, ctx->data);
			for (i = 1; i <= neq; i++)
				_rxC(wm)[i][j] = (_rxC(acor)[i] - _rxC(savf)[i]) * fac;
			y[j] = yj;
		}
		_rxC(nfe) += neq;
/*
   Compute norm of Jacobian.
*/
		_rxC(pdnorm) = fnorm0(neq, _rxC(wm), _rxC(ewt)) / fabs(hl0);
/*
   Add identity matrix.
*/
		for (i = 1; i <= neq; i++)
			_rxC(wm)[i][i] += 1.;
/*
   Do LU decomposition on P.
*/
		dgefa0(_rxC(wm), neq, _rxC(ipvt), &ier);
		if (ier != 0)
		return 0;
	}
	return 1;
}				/* end prja   */
