#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define STRICT_R_HEADERS
#include "lsoda.h"
#include "common.h"
#include "cfode_static.h"
#include <R.h>
#include <Rinternals.h>

void RSprintf(const char *format, ...);

void cfode_static (struct lsoda_context_t * ctx, int meth)
{
#ifdef CFODE_STATIC
	if (meth == 1) {
		_rxC(elco) = elco1;
		_rxC(tesco) = tesco1;

	} else {
		_rxC(elco) = elco2;
		_rxC(tesco) = tesco2;
	}
#endif
}
#include <stdio.h>
void printcm12(void) {
	RSprintf("static double cm1[13] = {\n");
	int i;
	for(i = 0; i < 13; i++) {
		RSprintf("%a, ", (tesco1)[i][2] *(elco1)[i][i + 1]);
		if((i + 1) % 4 == 0) RSprintf("\n  ");
	}
	RSprintf("};\n");
	RSprintf("static double cm2[13] = {\n");
	for(i = 0; i < 13; i++) {
		RSprintf("%a, ", (tesco2)[i][2] *(elco2)[i][i + 1]);
		if((i + 1) % 4 == 0) RSprintf("\n  ");
	}
	RSprintf("};\n");

}
