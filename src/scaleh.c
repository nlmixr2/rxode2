#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#include "lsoda.h"
#include "common.h"
#include "lsoda_internal.h"
#include <math.h>
void scaleh(struct lsoda_context_t * ctx, double rh)
{
	double          r;
	int             j, i;
	const int neq = ctx->neq;
	const double hmxi = ctx->opt->hmxi;
/*
   If _rxC(h) is being changed, the _rxC(h) ratio rh is checked against _rxC(rmax), hmin,
   and hmxi, and the _rxC(yh) array is rescaled.  _rxC(ialth) is set to (_rxC(nq) + 1) = _rxC(nq) + 1
   to prevent a change of _rxC(h) for that many steps, unless forced by a
   convergence or error test failure.
*/
	rh = fmin(rh, _rxC(rmax));
	rh = rh / fmax(1., fabs(_rxC(h)) * hmxi * rh);
/*
   If _rxC(meth) = 1, also restrict the new step size by the stability region.
   If this reduces _rxC(h), set _rxC(irflag) to 1 so that if there are roundoff
   problems later, we can assume that is the cause of the trouble.
*/
	if (_rxC(meth) == 1) {
		_rxC(irflag) = 0;
		double pdh = fmax(fabs(_rxC(h)) * _rxC(pdlast), 0.000001);
		if ((rh * pdh * 1.00001) >= sm1[_rxC(nq)]) {
			rh = sm1[_rxC(nq)] / pdh;
			_rxC(irflag) = 1;
		}
	}
	r = 1.;
	for (j = 2; j <= (_rxC(nq) + 1); j++) {
		r *= rh;
		for (i = 1; i <= neq; i++)
			_rxC(yh)[j][i] *= r;
	}
	_rxC(h) *= rh;
	_rxC(rc) *= rh;
	_rxC(ialth) = (_rxC(nq) + 1);

}				/* end scaleh   */
