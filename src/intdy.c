#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <stdio.h>
#include <math.h>
#include "lsoda.h"
#include "common.h"
#include "lsoda_internal.h"

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

#include <R.h>
#include <Rinternals.h>


void RSprintf(const char *format, ...);

int intdy(struct lsoda_context_t * ctx, double t, int k, double *dky)

/*
   Intdy computes interpolated values of the k-th derivative of the
   dependent variable vector y, and stores it in dky.  This routine
   is called within the package with k = 0 and *t = tout, but may
   also be called by the user for any k up to the current order.
   ( See detailed instructions in the usage documentation. )

   The computed values in dky are gotten by interpolation using the
   Nordsieck history array _rxC(yh).  This array corresponds uniquely to a
   vector-valued polynomial of degree nqcur or less, and dky is set
   to the k-th derivative of this polynomial at t.
   The formula for dky is

             q
   dky[i] = sum c[k][j] * ( t - _rxC(tn) )^(j-k) * _rxC(h)^(-j) * _rxC(yh)[j+1][i]
            j=k

   where c[k][j] = j*(j-1)*...*(j-k+1), q = nqcur, _rxC(tn) = tcur, _rxC(h) = hcur.
   The quantities _rxC(nq) = nqcur, l = _rxC(nq)+1, neq = neq, _rxC(tn), and _rxC(h) are declared
   static globally.  The above sum is done in reverse order.
   *iflag is returned negative if either k or t is out of bounds.
*/

{
	int             i, ic, j, jj, jp1;
	double          c, r, s, tp;

	const int neq = ctx->neq;
	if (k < 0 || k > _rxC(nq)) {
	  RSprintf(_("[intdy] k = %d illegal\n"), k);
	  return -1;
	}
	tp = _rxC(tn) - _rxC(hu) - 100. * ETA * (_rxC(tn) + _rxC(hu));
	if ((t - tp) * (t - _rxC(tn)) > 0.) {
	  RSprintf(_("intdy -- t = %g illegal. t not in interval tcur - _rxC(hu) to tcur\n"), t);
	  return -2;
	}
	s = (t - _rxC(tn)) / _rxC(h);
	ic = 1;
	for (jj = (_rxC(nq) + 1) - k; jj <= _rxC(nq); jj++)
		ic *= jj;
	c = (double) ic;
	for (i = 1; i <= neq; i++)
		dky[i] = c * _rxC(yh)[_rxC(nq) + 1][i];
	for (j = _rxC(nq) - 1; j >= k; j--) {
		jp1 = j + 1;
		ic = 1;
		for (jj = jp1 - k; jj <= j; jj++)
			ic *= jj;
		c = (double) ic;
		for (i = 1; i <= neq; i++)
			dky[i] = c * _rxC(yh)[jp1][i] + s * dky[i];
	}
	if (k == 0)
		return 0;
	r = pow(_rxC(h), (double) (-k));
	for (i = 1; i <= neq; i++)
		dky[i] *= r;
	return 0;
}				/* end intdy   */
