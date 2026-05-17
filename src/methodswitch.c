#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#include <math.h>
#include "lsoda.h"
#include "common.h"
#include "lsoda_internal.h"
#include "blas.h"


/*
 * cm1 and cm2 are calculated from tesco/elco 1 and 2 formula in cfode.c:printcm12.
 *
 * */
static double cm1[13] = {
0x0p+0, 0x1p+1, 0x1.7ffffffffffffp+2, 0x1p+2,
  0x1.9435e50d79434p+0, 0x1.c71c71c71c721p-2, 0x1.8eaf0473189ecp-4, 0x1.1df9ab7934513p-6,
  0x1.5b6f81b154515p-9, 0x1.6e1dd3d149b81p-12, 0x1.54a9415f71629p-15, 0x1.1bcb8f930a98p-18,
  0x1.ac0fa4b46f6c6p-22, };

/* only useful till cm2[5] */
static double cm2[13] = {
0x0p+0, 0x1p+1, 0x1.8p+0, 0x1.5555555555556p-1,
  0x1.aaaaaaaaaaaacp-3, 0x1.9999999999999p-5, 0x1.8eaf0473189ecp-4, 0x1.1df9ab7934513p-6,
  0x1.5b6f81b154515p-9, 0x1.6e1dd3d149b81p-12, 0x1.54a9415f71629p-15, 0x1.1bcb8f930a98p-18,
  0x1.ac0fa4b46f6c6p-22, };

/* rh is an output. */
void methodswitch(struct lsoda_context_t * ctx, double dsm, double pnorm, double *rh)
{
	int             lm1, lm1p1, lm2, lm2p1, nqm1, nqm2;
	double          rh1, rh2, rh1it, exm2, dm2, exm1, dm1, alpha, exsm;
	double pdh;
	const int neq = ctx->neq;
	const int mxordn = ctx->opt->mxordn;
	const int mxords = ctx->opt->mxords;
/*
   We are current using an Adams method.  Consider switching to bdf.
   If the current order is greater than 5, assume the problem is
   not stiff, and skip this section.
   If the Lipschitz constant and error estimate are not polluted
   by roundoff, perform the usual test.
   Otherwise, switch to the bdf methods if the last step was
   restricted to insure stability ( _rxC(irflag) = 1 ), and stay with Adams
   method if not.  When switching to bdf with polluted error estimates,
   in the absence of other information, double the step size.

   When the estimates are ok, we make the usual test by computing
   the step size we could have (ideally) used on this step,
   with the current (Adams) method, and also that for the bdf.
   If _rxC(nq) > mxords, we consider changing to order mxords on switching.
   Compare the two step sizes to decide whether to switch.
   The step size advantage must be at least ratio = 5 to switch.
*/
	if (_rxC(meth) == 1) {
		if (_rxC(nq) > 5)
			return;
		if (dsm <= (100. * pnorm * ETA) || _rxC(pdest) == 0.) {
			if (_rxC(irflag) == 0)
				return;
			rh2 = 2.;
			nqm2 = min(_rxC(nq), mxords);
		} else {
			exsm = 1. / (double) (_rxC(nq) + 1);
			rh1 = 1. / (1.2 * pow(dsm, exsm) + 0.0000012);
			rh1it = 2. * rh1;
			pdh = _rxC(pdlast) * fabs(_rxC(h));
			if ((pdh * rh1) > 0.00001)
				rh1it = sm1[_rxC(nq)] / pdh;
			rh1 = min(rh1, rh1it);
			if (_rxC(nq) > mxords) {
				nqm2 = mxords;
				lm2 = mxords + 1;
				exm2 = 1. / (double) lm2;
				lm2p1 = lm2 + 1;
				dm2 = vmnorm0(neq, _rxC(yh)[lm2p1], _rxC(ewt)) / cm2[mxords];
				rh2 = 1. / (1.2 * pow(dm2, exm2) + 0.0000012);
			} else {
				dm2 = dsm * (cm1[_rxC(nq)] / cm2[_rxC(nq)]);
				rh2 = 1. / (1.2 * pow(dm2, exsm) + 0.0000012);
				nqm2 = _rxC(nq);
			}
			if (rh2 < RATIO * rh1)
				return;
		}
/*
   The method switch test passed.  Reset relevant quantities for bdf.
*/
		*rh = rh2;
		_rxC(icount) = 20;
		_rxC(meth) = 2;
		_rxC(miter) = 2;
		_rxC(pdlast) = 0.;
		_rxC(nq) = nqm2;
		return;
	}			/* end if ( _rxC(meth) == 1 )   */
	/*
	   We are currently using a bdf method, considering switching to Adams.
	   Compute the step size we could have (ideally) used on this step,
	   with the current (bdf) method, and also that for the Adams.
	   If _rxC(nq) > mxordn, we consider changing to order mxordn on switching.
	   Compare the two step sizes to decide whether to switch.
	   The step size advantage must be at least 5/ratio = 1 to switch.
	   If the step size for Adams would be so small as to cause
	   roundoff pollution, we stay with bdf.
	*/
	exsm = 1. / (double) (_rxC(nq) + 1);
	if (mxordn < _rxC(nq)) {
		nqm1 = mxordn;
		lm1 = mxordn + 1;
		exm1 = 1. / (double) lm1;
		lm1p1 = lm1 + 1;
		dm1 = vmnorm0(neq, _rxC(yh)[lm1p1], _rxC(ewt)) / cm1[mxordn];
		rh1 = 1. / (1.2 * pow(dm1, exm1) + 0.0000012);
	} else {
		dm1 = dsm * (cm2[_rxC(nq)] / cm1[_rxC(nq)]);
		rh1 = 1. / (1.2 * pow(dm1, exsm) + 0.0000012);
		nqm1 = _rxC(nq);
		exm1 = exsm;
	}
	rh1it = 2. * rh1;
	pdh = _rxC(pdnorm) * fabs(_rxC(h));
	if ((pdh * rh1) > 0.00001)
		rh1it = sm1[nqm1] / pdh;
	rh1 = min(rh1, rh1it);
	rh2 = 1. / (1.2 * pow(dsm, exsm) + 0.0000012);
	if ((rh1 * RATIO) < (5. * rh2))
		return;
	alpha = fmax(0.001, rh1);
	dm1 *= pow(alpha, exm1);
	if (dm1 <= 1000. * ETA * pnorm)
		return;
/*
   The switch test passed.  Reset relevant quantities for Adams.
*/
	*rh = rh1;
	_rxC(icount) = 20;
	_rxC(meth) = 1;
	_rxC(miter) = 0;
	_rxC(pdlast) = 0.;
	_rxC(nq) = nqm1;

}				/* end methodswitch   */
