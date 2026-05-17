#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#include "lsoda.h"
#include "common.h"
#include "lsoda_internal.h"
#include <math.h>

int corfailure(struct lsoda_context_t * ctx, double told)
{
	int             j, i1, i;
	const int neq = ctx->neq;
	const double hmin = ctx->opt->hmin;
	_rxC(ncf)++;
	_rxC(rmax) = 2.;
	_rxC(tn) = told;
	for (j = _rxC(nq); j >= 1; j--)
		for (i1 = j; i1 <= _rxC(nq); i1++) {
			for (i = 1; i <= neq; i++)
				_rxC(yh)[i1][i] -= _rxC(yh)[i1 + 1][i];
		}
	if (fabs(_rxC(h)) <= hmin * 1.00001 || _rxC(ncf) == MXNCF) {
		return 2;
	}
	_rxC(ipup) = _rxC(miter);
	return 1;
}
