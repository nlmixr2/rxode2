#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#include "lsoda.h"
#include "common.h"
#include "lsoda_internal.h"

#include <math.h>
#include "blas.h"

/*
   This routine returns from stoda to lsoda.  Hence freevectors() is
   not executed.
*/
#define endstoda() \
{ \
	double          r; \
	int             i; \
 \
	r = 1. / _rxC(tesco)[_rxC(nqu)][2]; \
	for (i = 1; i <= neq; i++) \
		_rxC(acor)[i] *= r; \
	_rxC(hold) = _rxC(h); \
}

/*
   The _rxC(el) vector and related constants are reset
   whenever the order _rxC(nq) is changed, or at the start of the problem.
*/
#define resetcoeff() \
{ \
	int             i; \
 \
	double el0 = _rxC(el)[1]; \
	for (i = 1; i <= (_rxC(nq) + 1); i++) \
		_rxC(el)[i] = _rxC(elco)[_rxC(nq)][i]; \
	_rxC(rc) = _rxC(rc) * _rxC(el)[1] / el0; \
}


int stoda(struct lsoda_context_t * ctx, double *y, int jstart)
{
	int kflag;
	int             i, i1, j, m;
	double          del, delp, dsm, dup, exup, r, rh, told;
	double          pnorm;

	const double hmin = ctx->opt->hmin;
	const int mxords = ctx->opt->mxords;
	const int mxordn = ctx->opt->mxordn;
	const int neq = ctx->neq;

/*
   stoda performs one step of the integration of an initial value
   problem for a system of ordinary differential equations.
   Note.. stoda is independent of the value of the iteration method
   indicator _rxC(miter), when this is != 0, and hence is independent
   of the type of chord method used, or the Jacobian structure.
   Communication with stoda is done with the following variables:

   jstart = an integer used for input only, with the following
            values and meanings:

               0  perform the first step,
             > 0  take a new step continuing from the last,
              -1  take the next step with a new value of _rxC(h),
                  neq, _rxC(meth), _rxC(miter), and/or matrix parameters.
              -2  take the next step with a new value of _rxC(h),
                  but with other inputs unchanged.

   kflag = a completion code with the following meanings:

             0  the step was successful,
            -1  the requested error could not be achieved,
            -2  corrector convergence could not be achieved,
            -3  fatal error in prja or solsy.

   _rxC(miter) = corrector iteration method:

             0  functional iteration,
            >0  a chord method corresponding to jacobian type jt.

*/
	kflag = 0;
	told = _rxC(tn);
	_rxC(ncf) = 0;
	delp = 0.;

/*
   On the first call, the order is set to 1, and other variables are
   initialized.  _rxC(rmax) is the maximum ratio by which _rxC(h) can be increased
   in a single step.  It is initially 1.e4 to compensate for the small
   initial _rxC(h), but then is normally equal to 10.  If a filure occurs
   (in corrector convergence or error test), _rxC(rmax) is set at 2 for
   the next increase.
   cfode is called to get the needed coefficients for both methods.
*/
	int maxord = mxordn;
	if (_rxC(meth) == 2)
		maxord = mxords;

	if (jstart == 0) {
		_rxC(nq) = 1;
		_rxC(ialth) = 2;
		_rxC(rmax) = 10000.;
		_rxC(rc) = 0.;
		_rxC(crate) = 0.7;
		_rxC(hold) = _rxC(h);
		_rxC(nslp) = 0;
		_rxC(ipup) = _rxC(miter);
		_rxC(el)[1] = 1.0;
/*
   Initialize switching parameters.  _rxC(meth) = 1 is assumed initially.
*/
		_rxC(icount) = 20;
		_rxC(irflag) = 0;
		_rxC(pdest) = 0.;
		_rxC(pdlast) = 0.;

		cfode(ctx, 1);
		resetcoeff();
	}			/* end if ( jstart == 0 )   */
	/*
	   The following block handles preliminaries needed when jstart = -1.
	   _rxC(ipup) is set to _rxC(miter) to force a matrix update.
	   If an order increase is about to be considered ( _rxC(ialth) = 1 ),
	   _rxC(ialth) is reset to 2 to postpone consideration one more step.
	   If the caller has changed _rxC(meth), cfode is called to reset
	   the coefficients of the method.
	   If _rxC(h) is to be changed, _rxC(yh) must be rescaled.
	   If _rxC(h) or _rxC(meth) is being changed, _rxC(ialth) is reset to (_rxC(nq) + 1) = _rxC(nq) + 1
	   to prevent further changes in _rxC(h) for that many steps.
	*/
	if (jstart == -1) {
		_rxC(ipup) = _rxC(miter);
		if (_rxC(ialth) == 1)
			_rxC(ialth) = 2;
		if (_rxC(meth) != _rxC(mused)) {
			cfode(ctx, _rxC(meth));
			_rxC(ialth) = (_rxC(nq) + 1);
			resetcoeff();
		}
		if (_rxC(h) != _rxC(hold)) {
			rh = _rxC(h) / _rxC(hold);
			_rxC(h) = _rxC(hold);
			scaleh(ctx, rh);
		}
	}			/* if ( jstart == -1 )   */
	if (jstart == -2) {
		if (_rxC(h) != _rxC(hold)) {
			rh = _rxC(h) / _rxC(hold);
			_rxC(h) = _rxC(hold);
			scaleh(ctx, rh);
		}
	}			/* if ( jstart == -2 )   */
	/*
	   Prediction.
	   This section computes the predicted values by effectively
	   multiplying the _rxC(yh) array by the pascal triangle matrix.
	   _rxC(rc) is the ratio of new to old values of the coefficient _rxC(h) * _rxC(el)[1].
	   When _rxC(rc) differs from 1 by more than ccmax, _rxC(ipup) is set to _rxC(miter)
	   to force pjac to be called, if a jacobian is involved.
	   In any case, prja is called at least every msbp steps.
	*/
	dsm = 0.0;
	while (1) {
/*
   Before the corrector starts.  _rxC(jcur) is set to 0
   to signal that the Jacobian involved may need updating later.
*/
		_rxC(jcur) = 0;
		while (1) {
			if (fabs(_rxC(rc) - 1.) > CCMAX)
				_rxC(ipup) = _rxC(miter);
			if (_rxC(nst) >= _rxC(nslp) + MSBP)
				_rxC(ipup) = _rxC(miter);
			_rxC(tn) += _rxC(h);
			for (j = _rxC(nq); j >= 1; j--)
				for (i1 = j; i1 <= _rxC(nq); i1++) {
					for (i = 1; i <= neq; i++)
						_rxC(yh)[i1][i] += _rxC(yh)[i1 + 1][i];
				}
			pnorm = vmnorm0(neq, _rxC(yh)[1], _rxC(ewt));

			int corflag = correction(ctx, y, pnorm, &del, &delp, told, &m);
			if (corflag == 0)
				break;
			if (corflag == 1) {
				rh = fmax(0.25, hmin / fabs(_rxC(h)));
				scaleh(ctx, rh);
				continue;
			}
			if (corflag == 2) {
				kflag = -2;
				_rxC(hold) = _rxC(h);
				jstart = 1;
				return kflag;
			}
		}		/* end inner while ( corrector loop )   */
/*
   The local error test is done now.
*/
		if (m == 0)
			dsm = del / _rxC(tesco)[_rxC(nq)][2];
		if (m > 0)
			dsm = vmnorm0(neq, _rxC(acor), _rxC(ewt)) / _rxC(tesco)[_rxC(nq)][2];
		if (dsm <= 1.) {
/*
   After a successful step, update the _rxC(yh) array.
   Decrease _rxC(icount) by 1, and if it is -1, consider switching methods.
   If a method switch is made, reset various parameters,
   rescale the _rxC(yh) array, and exit.  If there is no switch,
   consider changing _rxC(h) if _rxC(ialth) = 1.  Otherwise decrease _rxC(ialth) by 1.
   If _rxC(ialth) is then 1 and _rxC(nq) < maxord, then _rxC(acor) is saved for
   use in a possible order increase on the next step.
   If a change in _rxC(h) is considered, an increase or decrease in order
   by one is considered also.  A change in _rxC(h) is made only if it is by
   a factor of at least 1.1.  If not, _rxC(ialth) is set to 3 to prevent
   testing for that many steps.
*/
			kflag = 0;
			_rxC(nst)++;
			_rxC(hu) = _rxC(h);
			_rxC(nqu) = _rxC(nq);
			_rxC(mused) = _rxC(meth);
			for (j = 1; j <= (_rxC(nq) + 1); j++) {
				r = _rxC(el)[j];
				for (i = 1; i <= neq; i++)
					_rxC(yh)[j][i] += r * _rxC(acor)[i];
			}
			_rxC(icount)--;
			if (_rxC(icount) < 0) {
				methodswitch(ctx, dsm, pnorm, &rh);
				if (_rxC(meth) != _rxC(mused)) {
					rh = fmax(rh, hmin / fabs(_rxC(h)));
					scaleh(ctx, rh);
					_rxC(rmax) = 10.;
					endstoda();
					break;
				}
			}
/*
   No method switch is being made.  Do the usual step/order selection.
*/
			_rxC(ialth)--;
			if (_rxC(ialth) == 0) {
				double rhup = 0.;
				if ((_rxC(nq) + 1) != maxord + 1) {
					for (i = 1; i <= neq; i++)
						_rxC(savf)[i] = _rxC(acor)[i] - _rxC(yh)[maxord + 1][i];
					dup = vmnorm0(neq, _rxC(savf), _rxC(ewt)) / _rxC(tesco)[_rxC(nq)][3];
					exup = 1. / (double) ((_rxC(nq) + 1) + 1);
					rhup = 1. / (1.4 * pow(dup, exup) + 0.0000014);
				}
				int orderflag = orderswitch(ctx, rhup, dsm, &rh, kflag, maxord);
/*
   No change in _rxC(h) or _rxC(nq).
*/
				if (orderflag == 0) {
					endstoda();
					break;
				}
/*
   _rxC(h) is changed, but not _rxC(nq).
*/
				if (orderflag == 1) {
					rh = fmax(rh, hmin / fabs(_rxC(h)));
					scaleh(ctx, rh);
					_rxC(rmax) = 10.;
					endstoda();
					break;
				}
/*
   both _rxC(nq) and _rxC(h) are changed.
*/
				if (orderflag == 2) {
					resetcoeff();
					rh = fmax(rh, hmin / fabs(_rxC(h)));
					scaleh(ctx, rh);
					_rxC(rmax) = 10.;
					endstoda();
					break;
				}
			}	/* end if ( _rxC(ialth) == 0 )   */
			if (_rxC(ialth) > 1 || (_rxC(nq) + 1) == maxord + 1) {
				endstoda();
				break;
			}
			for (i = 1; i <= neq; i++)
				_rxC(yh)[maxord + 1][i] = _rxC(acor)[i];
			endstoda();
			break;
		}
		/* end if ( dsm <= 1. )   */
		/*
		   The error test failed.  kflag keeps track of multiple failures.
		   Restore _rxC(tn) and the _rxC(yh) array to their previous values, and prepare
		   to try the step again.  Compute the optimum step size for this or
		   one lower.  After 2 or more failures, _rxC(h) is forced to decrease
		   by a factor of 0.2 or less.
		 */
		else {
			kflag--;
			_rxC(tn) = told;
			for (j = _rxC(nq); j >= 1; j--)
				for (i1 = j; i1 <= _rxC(nq); i1++) {
					for (i = 1; i <= neq; i++)
						_rxC(yh)[i1][i] -= _rxC(yh)[i1 + 1][i];
				}
			_rxC(rmax) = 2.;
			if (fabs(_rxC(h)) <= hmin * 1.00001) {
				kflag = -1;
				_rxC(hold) = _rxC(h);
				jstart = 1;
				break;
			}
			if (kflag > -3) {
				int orderflag = orderswitch(ctx, 0., dsm, &rh, kflag, maxord);
				if (orderflag == 1 || orderflag == 0) {
					if (orderflag == 0)
						rh = fmin(rh, 0.2);
					rh = fmax(rh, hmin / fabs(_rxC(h)));
					scaleh(ctx, rh);
				}
				if (orderflag == 2) {
					resetcoeff();
					rh = fmax(rh, hmin / fabs(_rxC(h)));
					scaleh(ctx, rh);
				}
				continue;
			}
			/* if ( kflag > -3 )   */
			/*
			   Control reaches this section if 3 or more failures have occurred.
			   If 10 failures have occurred, exit with kflag = -1.
			   It is assumed that the derivatives that have accumulated in the
			   _rxC(yh) array have errors of the wrong order.  Hence the first
			   derivative is recomputed, and the order is set to 1.  Then
			   _rxC(h) is reduced by a factor of 10, and the step is retried,
			   until it succeeds or _rxC(h) reaches hmin.
			 */
			else {
				if (kflag == -10) {
					kflag = -1;
					_rxC(hold) = _rxC(h);
					jstart = 1;
					break;
				} else {
					rh = 0.1;
					rh = fmax(hmin / fabs(_rxC(h)), rh);
					_rxC(h) *= rh;
					for (i = 1; i <= neq; i++)
						y[i] = _rxC(yh)[1][i];
					(*ctx->function) (_rxC(tn), y + 1, _rxC(savf) + 1, ctx->data);
					_rxC(nfe)++;
					for (i = 1; i <= neq; i++)
						_rxC(yh)[2][i] = _rxC(h) * _rxC(savf)[i];
					_rxC(ipup) = _rxC(miter);
					_rxC(ialth) = 5;
					if (_rxC(nq) == 1)
						continue;
					_rxC(nq) = 1;
					resetcoeff();
					continue;
				}
			}	/* end else -- kflag <= -3 */
		}		/* end error failure handling   */
	}			/* end outer while   */

	return kflag;
}				/* end stoda   */
