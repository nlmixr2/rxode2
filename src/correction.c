#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#include <math.h>
#include "lsoda.h"
#include "common.h"
#include "blas.h"
#include "lsoda_internal.h"

/* m is the correction count */
int correction(struct lsoda_context_t * ctx, double *y, double pnorm, double *del, double *delp, double told, int *m)
/*
   *corflag = 0 : corrector converged,
              1 : step size to be reduced, redo prediction,
              2 : corrector cannot converge, failure flag.
*/

{
	int             i;
	double          rm, rate, dcon;
	const int neq = ctx->neq;
/*
   Up to maxcor corrector iterations are taken.  A convergence test is
   made on the r.m.s. norm of each correction, weighted by the error
   weight vector _rxC(ewt).  The sum of the corrections is accumulated in the
   vector _rxC(acor)[i].  The _rxC(yh) array is not altered in the corrector loop.
*/

	*m = 0;
	rate = 0.;
	*del = 0.;
	for (i = 1; i <= neq; i++)
		y[i] = _rxC(yh)[1][i];
	(*ctx->function) (_rxC(tn), y + 1, _rxC(savf) + 1, ctx->data);
	_rxC(nfe)++;
/*
   If indicated, the matrix P = I - _rxC(h) * _rxC(el)[1] * J is reevaluated and
   preprocessed before starting the corrector iteration.  _rxC(ipup) is set
   to 0 as an indicator that this has been done.
*/
	while (1) {
		if (*m == 0) {
			if (_rxC(ipup) > 0) {
				int ierpj = prja(ctx, y);
				_rxC(jcur) = 1;
				_rxC(ipup) = 0;
				_rxC(rc) = 1.;
				_rxC(nslp) = _rxC(nst);
				_rxC(crate) = 0.7;
				if (!ierpj) {
					return corfailure(ctx, told);
				}
			}
			for (i = 1; i <= neq; i++)
				_rxC(acor)[i] = 0.;
		}		/* end if ( *m == 0 )   */
		if (_rxC(miter) == 0) {
/*
   In case of functional iteration, update y directly from
   the result of the last function evaluation.
*/
			for (i = 1; i <= neq; i++) {
				_rxC(savf)[i] = _rxC(h) * _rxC(savf)[i] - _rxC(yh)[2][i];
				y[i] = _rxC(savf)[i] - _rxC(acor)[i];
			}
			*del = vmnorm0(neq, y, _rxC(ewt));
			for (i = 1; i <= neq; i++) {
				y[i] = _rxC(yh)[1][i] + _rxC(el)[1] * _rxC(savf)[i];
				_rxC(acor)[i] = _rxC(savf)[i];
			}
		}
		/* end functional iteration   */
		/*
		   In the case of the chord method, compute the corrector error,
		   and solve the linear system with that as right-hand side and
		   P as coefficient matrix.
		 */
		else {
			for (i = 1; i <= neq; i++)
				y[i] = _rxC(h) * _rxC(savf)[i] - (_rxC(yh)[2][i] + _rxC(acor)[i]);
			solsy(ctx, y);
			*del = vmnorm0(neq, y, _rxC(ewt));
			for (i = 1; i <= neq; i++) {
				_rxC(acor)[i] += y[i];
				y[i] = _rxC(yh)[1][i] + _rxC(el)[1] * _rxC(acor)[i];
			}
		}		/* end chord method   */
/*
   Test for convergence.  If *m > 0, an estimate of the convergence
   rate constant is stored in _rxC(crate), and this is used in the test.

   We first check for a change of iterates that is the size of
   roundoff error.  If this occurs, the iteration has converged, and a
   new rate estimate is not formed.
   In all other cases, force at least two iterations to estimate a
   local Lipschitz constant estimate for Adams method.
   On convergence, form _rxC(pdest) = local maximum Lipschitz constant
   estimate.  _rxC(pdlast) is the most recent nonzero estimate.
*/
		if (*del <= 100. * pnorm * ETA)
			break;
		if (*m != 0 || _rxC(meth) != 1) {
			if (*m != 0) {
				rm = 1024.0;
				if (*del <= (1024. * *delp))
					rm = *del / *delp;
				rate = fmax(rate, rm);
				_rxC(crate) = fmax(0.2 * _rxC(crate), rm);
			}
			double conit = 0.5 / (double) (_rxC(nq) + 2);
			dcon = *del * fmin(1., 1.5 * _rxC(crate)) / (_rxC(tesco)[_rxC(nq)][2] * conit);
			if (dcon <= 1.) {
				_rxC(pdest) = fmax(_rxC(pdest), rate / fabs(_rxC(h) * _rxC(el)[1]));
				if (_rxC(pdest) != 0.)
					_rxC(pdlast) = _rxC(pdest);
				break;
			}
		}
/*
   The corrector iteration failed to converge.
   If _rxC(miter) != 0 and the Jacobian is out of date, prja is called for
   the next try.   Otherwise the _rxC(yh) array is retracted to its values
   before prediction, and _rxC(h) is reduced, if possible.  If _rxC(h) cannot be
   reduced or mxncf failures have occured, exit with corflag = 2.
*/
		(*m)++;
		if (*m == MAXCOR || (*m >= 2 && *del > 2. * *delp)) {
			if (_rxC(miter) == 0 || _rxC(jcur) == 1) {
				return corfailure(ctx, told);
			}
			_rxC(ipup) = _rxC(miter);
/*
   Restart corrector if Jacobian is recomputed.
*/
			*m = 0;
			rate = 0.;
			*del = 0.;
			for (i = 1; i <= neq; i++)
				y[i] = _rxC(yh)[1][i];
			(*ctx->function) (_rxC(tn), y + 1, _rxC(savf) + 1, ctx->data);
			_rxC(nfe)++;
		}
/*
   Iterate corrector.
*/
		else {
			*delp = *del;
			(*ctx->function) (_rxC(tn), y + 1, _rxC(savf) + 1, ctx->data);
			_rxC(nfe)++;
		}
	}			/* end while   */
	return 0;
}				/* end correction   */
