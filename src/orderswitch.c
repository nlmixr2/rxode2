#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#include "lsoda.h"
#include "common.h"
#include "lsoda_internal.h"
#include <math.h>
#include "blas.h"

/* rh is an output. pdh is an output and only for meth=1*/
int orderswitch(struct lsoda_context_t * ctx, double rhup, double dsm, double *rh, int kflag, int maxord)

/*
   Regardless of the success or failure of the step, factors
   rhdn, rhsm, and rhup are computed, by which _rxC(h) could be multiplied
   at order _rxC(nq) - 1, order _rxC(nq), or order _rxC(nq) + 1, respectively.
   In the case of a failure, rhup = 0. to avoid an order increase.
   The largest of these is determined and the new order chosen
   accordingly.  If the order is to be increased, we compute one
   additional scaled derivative.

   orderflag = 0  : no change in _rxC(h) or _rxC(nq),
               1  : change in _rxC(h) but not _rxC(nq),
               2  : change in both _rxC(h) and _rxC(nq).
*/

{
	int             newq, i;
	double          exsm, rhdn, rhsm, ddn, exdn, r;
	double pdh;
	const int neq = ctx->neq;
	exsm = 1. / (double) (_rxC(nq) + 1);
	rhsm = 1. / (1.2 * pow(dsm, exsm) + 0.0000012);

	rhdn = 0.;
	if (_rxC(nq) != 1) {
		ddn = vmnorm0(neq, _rxC(yh)[(_rxC(nq) + 1)], _rxC(ewt)) / _rxC(tesco)[_rxC(nq)][1];
		exdn = 1. / (double) _rxC(nq);
		rhdn = 1. / (1.3 * pow(ddn, exdn) + 0.0000013);
	}
/*
   If _rxC(meth) = 1, limit rh accordinfg to the stability region also.
*/
	if (_rxC(meth) == 1) {
		pdh = max(fabs(_rxC(h)) * _rxC(pdlast), 0.000001);
		if ((_rxC(nq) + 1) < maxord + 1)
			rhup = min(rhup, sm1[(_rxC(nq) + 1)] / pdh);
		rhsm = min(rhsm, sm1[_rxC(nq)] / pdh);
		if (_rxC(nq) > 1)
			rhdn = min(rhdn, sm1[_rxC(nq) - 1] / pdh);
		_rxC(pdest) = 0.;
	}
	if (rhsm >= rhup) {
		if (rhsm >= rhdn) {
			newq = _rxC(nq);
			*rh = rhsm;
		} else {
			newq = _rxC(nq) - 1;
			*rh = rhdn;
			if (kflag < 0 && *rh > 1.)
				*rh = 1.;
		}
	} else {
		if (rhup <= rhdn) {
			newq = _rxC(nq) - 1;
			*rh = rhdn;
			if (kflag < 0 && *rh > 1.)
				*rh = 1.;
		} else {
			*rh = rhup;
			if (*rh >= 1.1) {
				r = _rxC(el)[(_rxC(nq) + 1)] / (double) (_rxC(nq) + 1);
				_rxC(nq) = _rxC(nq) + 1;
				for (i = 1; i <= neq; i++)
					_rxC(yh)[_rxC(nq) + 1][i] = _rxC(acor)[i] * r;
				return 2;
			} else {
				_rxC(ialth) = 3;
				return 0;
			}
		}
	}
/*
   If _rxC(meth) = 1 and _rxC(h) is restricted by stability, bypass 10 percent test.
*/
	if (_rxC(meth) == 1) {
		if ((*rh * pdh * 1.00001) < sm1[newq])
			if (kflag == 0 && *rh < 1.1) {
				_rxC(ialth) = 3;
				return 0;
			}
	} else {
		if (kflag == 0 && *rh < 1.1) {
			_rxC(ialth) = 3;
			return 0 ;
		}
	}
	if (kflag <= -2)
		*rh = min(*rh, 0.2);
/*
   If there is a change of order, reset _rxC(nq), (_rxC(nq) + 1), and the coefficients.
   In any case _rxC(h) is reset according to rh and the _rxC(yh) array is rescaled.
   Then exit or redo the step.
*/
	if (newq == _rxC(nq)) {
		return 1;
	}
	_rxC(nq) = newq;
	return 2;
}				/* end orderswitch   */
