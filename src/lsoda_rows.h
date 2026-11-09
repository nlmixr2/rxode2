#ifndef __LSODA_ROWS_H__
#define __LSODA_ROWS_H__

/*
   The element-wise vector updates of stoda() and correction(), on lsoda's
   1-based vectors of length n.  The vectors never overlap (rows of the
   Nordsieck array yh, acor, savf and the caller's y), hence restrict.

   Each loop takes two elements a pass, with a scalar tail.  R compiles
   packages at -O2, where GCC's vectorizer uses its "very-cheap" cost model:
   it vectorizes a loop only when the vector code replaces all of the scalar
   code, so it leaves a loop of unknown trip count taken one element a pass
   scalar, restrict or not.  The two-element body is vectorized (16-byte
   vectors on x86-64).  Every element is computed by the same expression as
   one at a time, so the results are the same bit for bit.
*/

/* to += from */
static inline void lsodaRowAdd(int n, double *restrict to, const double *restrict from) {
	int i = 1;
	for (; i < n; i += 2) {
		to[i] += from[i];
		to[i + 1] += from[i + 1];
	}
	if (i == n) to[i] += from[i];
}

/* to -= from */
static inline void lsodaRowSub(int n, double *restrict to, const double *restrict from) {
	int i = 1;
	for (; i < n; i += 2) {
		to[i] -= from[i];
		to[i + 1] -= from[i + 1];
	}
	if (i == n) to[i] -= from[i];
}

/* to += r * x */
static inline void lsodaRowAxpy(int n, double *restrict to, double r, const double *restrict x) {
	int i = 1;
	for (; i < n; i += 2) {
		to[i] += r * x[i];
		to[i + 1] += r * x[i + 1];
	}
	if (i == n) to[i] += r * x[i];
}

/* savf = h * savf - yh2;  y = savf - acor */
static inline void lsodaCorrFunctional(int n, double *restrict savf, double h,
				       const double *restrict yh2, const double *restrict acor,
				       double *restrict y) {
	int i = 1;
	for (; i < n; i += 2) {
		savf[i] = h * savf[i] - yh2[i];
		savf[i + 1] = h * savf[i + 1] - yh2[i + 1];
		y[i] = savf[i] - acor[i];
		y[i + 1] = savf[i + 1] - acor[i + 1];
	}
	if (i == n) {
		savf[i] = h * savf[i] - yh2[i];
		y[i] = savf[i] - acor[i];
	}
}

/* y = yh1 + el1 * savf;  acor = savf */
static inline void lsodaCorrFunctionalY(int n, double *restrict y, const double *restrict yh1,
					double el1, const double *restrict savf,
					double *restrict acor) {
	int i = 1;
	for (; i < n; i += 2) {
		y[i] = yh1[i] + el1 * savf[i];
		y[i + 1] = yh1[i + 1] + el1 * savf[i + 1];
		acor[i] = savf[i];
		acor[i + 1] = savf[i + 1];
	}
	if (i == n) {
		y[i] = yh1[i] + el1 * savf[i];
		acor[i] = savf[i];
	}
}

/* y = h * savf - (yh2 + acor) */
static inline void lsodaCorrNewtonRhs(int n, double *restrict y, double h,
				      const double *restrict savf, const double *restrict yh2,
				      const double *restrict acor) {
	int i = 1;
	for (; i < n; i += 2) {
		y[i] = h * savf[i] - (yh2[i] + acor[i]);
		y[i + 1] = h * savf[i + 1] - (yh2[i + 1] + acor[i + 1]);
	}
	if (i == n) y[i] = h * savf[i] - (yh2[i] + acor[i]);
}

/* acor += y;  y = yh1 + el1 * acor */
static inline void lsodaCorrNewtonY(int n, double *restrict acor, double *restrict y,
				    const double *restrict yh1, double el1) {
	int i = 1;
	for (; i < n; i += 2) {
		acor[i] += y[i];
		acor[i + 1] += y[i + 1];
		y[i] = yh1[i] + el1 * acor[i];
		y[i + 1] = yh1[i + 1] + el1 * acor[i + 1];
	}
	if (i == n) {
		acor[i] += y[i];
		y[i] = yh1[i] + el1 * acor[i];
	}
}

#endif
