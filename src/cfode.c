#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define STRICT_R_HEADERS
#include "lsoda.h"
#include "common.h"

void cfode(struct lsoda_context_t * ctx, int meth)
{
	int             i, nq, nqm1, nqp1;
	double          agamq, fnq, fnqm1, pc[13], pint, ragq, rqfac, rq1fac, tsign, xpin;
/*
   cfode is called by the integrator routine to set coefficients
   needed there.  The coefficients for the current method, as
   given by the value of meth, are set for all orders and saved.
   The maximum order assumed here is 12 if meth = 1 and 5 if meth = 2.
   ( A smaller value of the maximum order is also allowed. )
   cfode is called once at the beginning of the problem, and
   is not called again unless and until meth is changed.

   The _rxC(elco) array contains the basic method coefficients.
   The coefficients el[i], 1 < i < nq+1, for the method of
   order nq are stored in _rxC(elco)[nq][i].  They are given by a generating
   polynomial, i.e.,

      l(x) = el[1] + el[2]*x + ... + el[nq+1]*x^nq.

   For the implicit Adams method, l(x) is given by

      dl/dx = (x+1)*(x+2)*...*(x+nq-1)/factorial(nq-1),   l(-1) = 0.

   For the bdf methods, l(x) is given by

      l(x) = (x+1)*(x+2)*...*(x+nq)/k,

   where   k = factorial(nq)*(1+1/2+...+1/nq).

   The _rxC(tesco) array contains test constants used for the
   local error test and the selection of step size and/or order.
   At order nq, _rxC(tesco)[nq][k] is used for the selection of step
   size at order nq-1 if k = 1, at order nq if k = 2, and at order
   nq+1 if k = 3.
*/
	if (meth == 1) {
		_rxC(elco)[1][1] = 1.;
		_rxC(elco)[1][2] = 1.;
		_rxC(tesco)[1][1] = 0.;
		_rxC(tesco)[1][2] = 2.;
		_rxC(tesco)[2][1] = 1.;
		_rxC(tesco)[12][3] = 0.;
		pc[1] = 1.;
		rqfac = 1.;
		for (nq = 2; nq <= 12; nq++) {
/*
   The pc array will contain the coefficients of the polynomial

      p(x) = (x+1)*(x+2)*...*(x+nq-1).

   Initially, p(x) = 1.
*/
			rq1fac = rqfac;
			rqfac = rqfac / (double) nq;
			nqm1 = nq - 1;
			fnqm1 = (double) nqm1;
			nqp1 = nq + 1;
/*
   Form coefficients of p(x)*(x+nq-1).
*/
			pc[nq] = 0.;
			for (i = nq; i >= 2; i--)
				pc[i] = pc[i - 1] + fnqm1 * pc[i];
			pc[1] = fnqm1 * pc[1];
/*
   Compute integral, -1 to 0, of p(x) and x*p(x).
*/
			pint = pc[1];
			xpin = pc[1] / 2.;
			tsign = 1.;
			for (i = 2; i <= nq; i++) {
				tsign = -tsign;
				pint += tsign * pc[i] / (double) i;
				xpin += tsign * pc[i] / (double) (i + 1);
			}
/*
   Store coefficients in _rxC(elco) and _rxC(tesco).
*/
			_rxC(elco)[nq][1] = pint * rq1fac;
			_rxC(elco)[nq][2] = 1.;
			for (i = 2; i <= nq; i++)
				_rxC(elco)[nq][i + 1] = rq1fac * pc[i] / (double) i;
			agamq = rqfac * xpin;
			ragq = 1. / agamq;
			_rxC(tesco)[nq][2] = ragq;
			if (nq < 12)
				_rxC(tesco)[nqp1][1] = ragq * rqfac / (double) nqp1;
			_rxC(tesco)[nqm1][3] = ragq;
		}		/* end for   */
		return;
	}			/* end if ( meth == 1 )   */
	/*
	   meth = 2.
	*/
	pc[1] = 1.;
	rq1fac = 1.;
/*
   The pc array will contain the coefficients of the polynomial

      p(x) = (x+1)*(x+2)*...*(x+nq).

   Initially, p(x) = 1.
*/
	for (nq = 1; nq <= 5; nq++) {
		fnq = (double) nq;
		nqp1 = nq + 1;
/*
   Form coefficients of p(x)*(x+nq).
*/
		pc[nqp1] = 0.;
		for (i = nq + 1; i >= 2; i--)
			pc[i] = pc[i - 1] + fnq * pc[i];
		pc[1] *= fnq;
/*
   Store coefficients in _rxC(elco) and _rxC(tesco).
*/
		for (i = 1; i <= nqp1; i++)
			_rxC(elco)[nq][i] = pc[i] / pc[2];
		_rxC(elco)[nq][2] = 1.;
		_rxC(tesco)[nq][1] = rq1fac;
		_rxC(tesco)[nq][2] = ((double) nqp1) / _rxC(elco)[nq][1];
		_rxC(tesco)[nq][3] = ((double) (nq + 2)) / _rxC(elco)[nq][1];
		rq1fac /= fnq;
	}
	return;

}				/* end cfode   */
