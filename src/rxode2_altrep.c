/*
 * ALTREP class rx_seqrep
 * ======================
 * Compact representation of rep(1:n_vals, each=run_len, times=n_reps).
 *
 * Used for the output data frame's 'id' and 'sim.id' columns so that
 * large integer vectors are never materialised during rxSolve().
 *
 * data1: INTSXP[3] = { n_vals, run_len, total_len }
 *   where total_len = n_vals * run_len * n_reps
 * data2: R_NilValue until materialised, then the full INTSXP.
 *
 * Element i: ((i / run_len) % n_vals) + 1
 */

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Altrep.h>

#include "rxode2_altrep.h"

static R_altrep_class_t rx_seqrep_class;

/* ---- length ------------------------------------------------------------ */
static R_xlen_t rx_seqrep_Length(SEXP x) {
  return (R_xlen_t)INTEGER(R_altrep_data1(x))[2];
}

/* ---- single element ---------------------------------------------------- */
static int rx_seqrep_Elt(SEXP x, R_xlen_t i) {
  const int *d = INTEGER(R_altrep_data1(x));
  int n_vals  = d[0];
  int run_len = d[1];
  return (int)((i / run_len) % n_vals) + 1;
}

/* ---- materialise ------------------------------------------------------- */
static SEXP rx_seqrep_materialize(SEXP x) {
  SEXP d2 = R_altrep_data2(x);
  if (d2 != R_NilValue) return d2;

  R_xlen_t len = rx_seqrep_Length(x);
  SEXP mat = PROTECT(Rf_allocVector(INTSXP, len));
  int *p = INTEGER(mat);
  const int *d = INTEGER(R_altrep_data1(x));
  int n_vals  = d[0];
  int run_len = d[1];
  for (R_xlen_t i = 0; i < len; i++) {
    p[i] = (int)((i / run_len) % n_vals) + 1;
  }
  R_set_altrep_data2(x, mat);
  UNPROTECT(1);
  return mat;
}

/* ---- DATAPTR_OR_NULL: return NULL when not yet materialised ------------ */
static const void *rx_seqrep_Dataptr_or_null(SEXP x) {
  SEXP d2 = R_altrep_data2(x);
  if (d2 == R_NilValue) return NULL;
  return DATAPTR_RO(d2);
}

/* ---- DATAPTR: materialise on demand ------------------------------------ */
static void *rx_seqrep_Dataptr(SEXP x, Rboolean writeable) {
  SEXP mat = rx_seqrep_materialize(x);
  return DATAPTR_RW(mat);
}

/* ---- Sum, Min, Max: can be computed analytically ---------------------- */
static SEXP rx_seqrep_Sum(SEXP x, Rboolean narm) {
  const int *d = INTEGER(R_altrep_data1(x));
  int n_vals  = d[0];
  int run_len = d[1];
  R_xlen_t total_len = (R_xlen_t)d[2];
  /* Sum of rep(1:n_vals, each=run_len, times=n_reps)
   * = n_reps * run_len * n_vals*(n_vals+1)/2 */
  double nreps = (double)total_len / (double)(n_vals * (R_xlen_t)run_len);
  double s = nreps * run_len * ((double)n_vals * (n_vals + 1) / 2.0);
  return Rf_ScalarReal(s);
}

static SEXP rx_seqrep_Min(SEXP x, Rboolean narm) {
  return Rf_ScalarInteger(1);
}

static SEXP rx_seqrep_Max(SEXP x, Rboolean narm) {
  const int *d = INTEGER(R_altrep_data1(x));
  return Rf_ScalarInteger(d[0]);
}

/* ---- Registration ------------------------------------------------------ */
void rxode2_init_altrep_class(DllInfo *info) {
  rx_seqrep_class = R_make_altinteger_class("rx_seqrep", "rxode2", info);
  R_set_altrep_Length_method(rx_seqrep_class, rx_seqrep_Length);
  R_set_altinteger_Elt_method(rx_seqrep_class, rx_seqrep_Elt);
  R_set_altvec_Dataptr_method(rx_seqrep_class, rx_seqrep_Dataptr);
  R_set_altvec_Dataptr_or_null_method(rx_seqrep_class, rx_seqrep_Dataptr_or_null);
  R_set_altinteger_Sum_method(rx_seqrep_class, rx_seqrep_Sum);
  R_set_altinteger_Min_method(rx_seqrep_class, rx_seqrep_Min);
  R_set_altinteger_Max_method(rx_seqrep_class, rx_seqrep_Max);
}

/* ---- Factory ----------------------------------------------------------- */
SEXP rxode2_make_seqrep(int n_vals, int run_len, R_xlen_t total_len) {
  SEXP d1 = PROTECT(Rf_allocVector(INTSXP, 3));
  INTEGER(d1)[0] = n_vals;
  INTEGER(d1)[1] = run_len;
  INTEGER(d1)[2] = (int)total_len; /* safe: rx->nr <= INT_MAX (checked earlier) */
  SEXP out = R_new_altrep(rx_seqrep_class, d1, R_NilValue);
  UNPROTECT(1);
  return out;
}
