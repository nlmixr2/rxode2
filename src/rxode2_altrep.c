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
#include <Rversion.h>
#if R_VERSION < R_Version(4, 6, 0)
# define DATAPTR_RW(x) DATAPTR(x)
#endif

#include <Rinternals.h>
#include <R_ext/Altrep.h>
#include <string.h>

#include "rxode2_altrep.h"

static R_altrep_class_t rx_seqrep_class;
static R_altrep_class_t rx_rep_int_class;
static R_altrep_class_t rx_rep_lgl_class;
static R_altrep_class_t rx_rep_real_class;
static R_altrep_class_t rx_rep_str_class;

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

/* ---- repeated block ALTREP helpers ------------------------------------- */
static inline R_xlen_t rx_rep_times(SEXP x) {
  SEXP times = VECTOR_ELT(R_altrep_data1(x), 0);
  return (R_xlen_t)INTEGER(times)[0];
}

static inline SEXP rx_rep_base(SEXP x) {
  return VECTOR_ELT(R_altrep_data1(x), 1);
}

static inline R_xlen_t rx_rep_base_len(SEXP x) {
  return XLENGTH(rx_rep_base(x));
}

static inline R_xlen_t rx_rep_total_len(SEXP x) {
  return rx_rep_base_len(x) * rx_rep_times(x);
}

static R_xlen_t rx_rep_int_Length(SEXP x) {
  return rx_rep_total_len(x);
}

static int rx_rep_int_Elt(SEXP x, R_xlen_t i) {
  SEXP base = rx_rep_base(x);
  R_xlen_t n = XLENGTH(base);
  if (n == 0) return NA_INTEGER;
  return INTEGER(base)[i % n];
}

static SEXP rx_rep_int_materialize(SEXP x) {
  SEXP mat = R_altrep_data2(x);
  if (TYPEOF(mat) == INTSXP && XLENGTH(mat) == rx_rep_total_len(x)) return mat;
  SEXP base = VECTOR_ELT(R_altrep_data1(x), 1);
  R_xlen_t n = XLENGTH(base);
  R_xlen_t times = rx_rep_times(x);
  mat = PROTECT(Rf_allocVector(INTSXP, n * times));
  int *out = INTEGER(mat);
  const int *src = INTEGER(base);
  for (R_xlen_t t = 0; t < times; ++t) {
    memcpy(out + t * n, src, (size_t)n * sizeof(int));
  }
  R_set_altrep_data2(x, mat);
  UNPROTECT(1);
  return mat;
}

static const void *rx_rep_int_Dataptr_or_null(SEXP x) {
  SEXP d2 = R_altrep_data2(x);
  if (TYPEOF(d2) == INTSXP && XLENGTH(d2) == rx_rep_total_len(x)) return DATAPTR_RO(d2);
  return NULL;
}

static void *rx_rep_int_Dataptr(SEXP x, Rboolean writeable) {
  SEXP mat = rx_rep_int_materialize(x);
  return DATAPTR_RW(mat);
}

static R_xlen_t rx_rep_lgl_Length(SEXP x) {
  return rx_rep_total_len(x);
}

static int rx_rep_lgl_Elt(SEXP x, R_xlen_t i) {
  SEXP base = rx_rep_base(x);
  R_xlen_t n = XLENGTH(base);
  if (n == 0) return NA_LOGICAL;
  return LOGICAL(base)[i % n];
}

static SEXP rx_rep_lgl_materialize(SEXP x) {
  SEXP mat = R_altrep_data2(x);
  if (TYPEOF(mat) == LGLSXP && XLENGTH(mat) == rx_rep_total_len(x)) return mat;
  SEXP base = VECTOR_ELT(R_altrep_data1(x), 1);
  R_xlen_t n = XLENGTH(base);
  R_xlen_t times = rx_rep_times(x);
  mat = PROTECT(Rf_allocVector(LGLSXP, n * times));
  int *out = LOGICAL(mat);
  const int *src = LOGICAL(base);
  for (R_xlen_t t = 0; t < times; ++t) {
    memcpy(out + t * n, src, (size_t)n * sizeof(int));
  }
  R_set_altrep_data2(x, mat);
  UNPROTECT(1);
  return mat;
}

static const void *rx_rep_lgl_Dataptr_or_null(SEXP x) {
  SEXP d2 = R_altrep_data2(x);
  if (TYPEOF(d2) == LGLSXP && XLENGTH(d2) == rx_rep_total_len(x)) return DATAPTR_RO(d2);
  return NULL;
}

static void *rx_rep_lgl_Dataptr(SEXP x, Rboolean writeable) {
  SEXP mat = rx_rep_lgl_materialize(x);
  return DATAPTR_RW(mat);
}

static R_xlen_t rx_rep_real_Length(SEXP x) {
  return rx_rep_total_len(x);
}

static double rx_rep_real_Elt(SEXP x, R_xlen_t i) {
  SEXP base = rx_rep_base(x);
  R_xlen_t n = XLENGTH(base);
  if (n == 0) return NA_REAL;
  return REAL(base)[i % n];
}

static SEXP rx_rep_real_materialize(SEXP x) {
  SEXP mat = R_altrep_data2(x);
  if (TYPEOF(mat) == REALSXP && XLENGTH(mat) == rx_rep_total_len(x)) return mat;
  SEXP base = VECTOR_ELT(R_altrep_data1(x), 1);
  R_xlen_t n = XLENGTH(base);
  R_xlen_t times = rx_rep_times(x);
  mat = PROTECT(Rf_allocVector(REALSXP, n * times));
  double *out = REAL(mat);
  const double *src = REAL(base);
  for (R_xlen_t t = 0; t < times; ++t) {
    memcpy(out + t * n, src, (size_t)n * sizeof(double));
  }
  R_set_altrep_data2(x, mat);
  UNPROTECT(1);
  return mat;
}

static const void *rx_rep_real_Dataptr_or_null(SEXP x) {
  SEXP d2 = R_altrep_data2(x);
  if (TYPEOF(d2) == REALSXP && XLENGTH(d2) == rx_rep_total_len(x)) return DATAPTR_RO(d2);
  return NULL;
}

static void *rx_rep_real_Dataptr(SEXP x, Rboolean writeable) {
  SEXP mat = rx_rep_real_materialize(x);
  return DATAPTR_RW(mat);
}

static R_xlen_t rx_rep_str_Length(SEXP x) {
  return rx_rep_total_len(x);
}

static SEXP rx_rep_str_Elt(SEXP x, R_xlen_t i) {
  SEXP base = rx_rep_base(x);
  R_xlen_t n = XLENGTH(base);
  if (n == 0) return NA_STRING;
  return STRING_ELT(base, i % n);
}

static SEXP rx_rep_str_materialize(SEXP x) {
  SEXP mat = R_altrep_data2(x);
  if (TYPEOF(mat) == STRSXP && XLENGTH(mat) == rx_rep_total_len(x)) return mat;
  SEXP base = rx_rep_base(x);
  R_xlen_t n = XLENGTH(base);
  R_xlen_t times = rx_rep_times(x);
  mat = PROTECT(Rf_allocVector(STRSXP, n * times));
  for (R_xlen_t t = 0; t < times; ++t)
    for (R_xlen_t j = 0; j < n; ++j)
      SET_STRING_ELT(mat, t * n + j, STRING_ELT(base, j));
  R_set_altrep_data2(x, mat);
  UNPROTECT(1);
  return mat;
}

static const void *rx_rep_str_Dataptr_or_null(SEXP x) {
  SEXP d2 = R_altrep_data2(x);
  if (TYPEOF(d2) == STRSXP && XLENGTH(d2) == rx_rep_total_len(x)) return DATAPTR_RO(d2);
  return NULL;
}

static void *rx_rep_str_Dataptr(SEXP x, Rboolean writeable) {
  SEXP mat = rx_rep_str_materialize(x);
  return DATAPTR_RW(mat);
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

  rx_rep_int_class = R_make_altinteger_class("rx_rep_int", "rxode2", info);
  R_set_altrep_Length_method(rx_rep_int_class, rx_rep_int_Length);
  R_set_altinteger_Elt_method(rx_rep_int_class, rx_rep_int_Elt);
  R_set_altvec_Dataptr_method(rx_rep_int_class, rx_rep_int_Dataptr);
  R_set_altvec_Dataptr_or_null_method(rx_rep_int_class, rx_rep_int_Dataptr_or_null);

  rx_rep_lgl_class = R_make_altlogical_class("rx_rep_lgl", "rxode2", info);
  R_set_altrep_Length_method(rx_rep_lgl_class, rx_rep_lgl_Length);
  R_set_altlogical_Elt_method(rx_rep_lgl_class, rx_rep_lgl_Elt);
  R_set_altvec_Dataptr_method(rx_rep_lgl_class, rx_rep_lgl_Dataptr);
  R_set_altvec_Dataptr_or_null_method(rx_rep_lgl_class, rx_rep_lgl_Dataptr_or_null);

  rx_rep_real_class = R_make_altreal_class("rx_rep_real", "rxode2", info);
  R_set_altrep_Length_method(rx_rep_real_class, rx_rep_real_Length);
  R_set_altreal_Elt_method(rx_rep_real_class, rx_rep_real_Elt);
  R_set_altvec_Dataptr_method(rx_rep_real_class, rx_rep_real_Dataptr);
  R_set_altvec_Dataptr_or_null_method(rx_rep_real_class, rx_rep_real_Dataptr_or_null);

  rx_rep_str_class = R_make_altstring_class("rx_rep_str", "rxode2", info);
  R_set_altrep_Length_method(rx_rep_str_class, rx_rep_str_Length);
  R_set_altstring_Elt_method(rx_rep_str_class, rx_rep_str_Elt);
  R_set_altvec_Dataptr_method(rx_rep_str_class, rx_rep_str_Dataptr);
  R_set_altvec_Dataptr_or_null_method(rx_rep_str_class, rx_rep_str_Dataptr_or_null);
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

SEXP rxode2_make_rep_int(SEXP base, int times) {
  SEXP d1 = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP ti = PROTECT(Rf_ScalarInteger(times < 0 ? 0 : times));
  SET_VECTOR_ELT(d1, 0, ti);
  SET_VECTOR_ELT(d1, 1, base);
  SEXP out = R_new_altrep(rx_rep_int_class, d1, R_NilValue);
  UNPROTECT(2);
  return out;
}

SEXP rxode2_make_rep_lgl(SEXP base, int times) {
  SEXP d1 = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP ti = PROTECT(Rf_ScalarInteger(times < 0 ? 0 : times));
  SET_VECTOR_ELT(d1, 0, ti);
  SET_VECTOR_ELT(d1, 1, base);
  SEXP out = R_new_altrep(rx_rep_lgl_class, d1, R_NilValue);
  UNPROTECT(2);
  return out;
}

SEXP rxode2_make_rep_real(SEXP base, int times) {
  SEXP d1 = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP ti = PROTECT(Rf_ScalarInteger(times < 0 ? 0 : times));
  SET_VECTOR_ELT(d1, 0, ti);
  SET_VECTOR_ELT(d1, 1, base);
  SEXP out = R_new_altrep(rx_rep_real_class, d1, R_NilValue);
  UNPROTECT(2);
  return out;
}

SEXP rxode2_make_rep_str(SEXP base, int times) {
  SEXP d1 = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP ti = PROTECT(Rf_ScalarInteger(times < 0 ? 0 : times));
  SET_VECTOR_ELT(d1, 0, ti);
  SET_VECTOR_ELT(d1, 1, base);
  SEXP out = R_new_altrep(rx_rep_str_class, d1, R_NilValue);
  UNPROTECT(2);
  return out;
}

// API for integers and reals for accessing without materialising the
// full vector

extern Rboolean is_rx_seqrep(SEXP x) {
  return ALTREP(x) && R_altrep_inherits(x, rx_seqrep_class);
}

extern Rboolean is_rx_rep_int(SEXP x) {
  return ALTREP(x) && R_altrep_inherits(x, rx_rep_int_class);
}

extern int rxInt(SEXP x, R_xlen_t i) {
  if (ALTREP(x)) {
    if (R_altrep_inherits(x, rx_seqrep_class)) {
      return rx_seqrep_Elt(x, i);
    } else if (R_altrep_inherits(x, rx_rep_int_class)) {
      return rx_rep_int_Elt(x, i);
    } else {
      return NA_INTEGER;
    }
  } else {
    if (TYPEOF(x) != INTSXP) return NA_INTEGER;
    return INTEGER(x)[i];
  }
  return NA_INTEGER; // nocov
}

extern double rxReal(SEXP x, R_xlen_t i) {
  if (ALTREP(x)) {
    if (R_altrep_inherits(x, rx_rep_real_class)) {
      return rx_rep_real_Elt(x, i);
    } else {
      return NA_REAL;
    }
  } else {
    if (TYPEOF(x) != REALSXP) return NA_REAL;
    return REAL(x)[i];
  }
  return NA_REAL; // nocov
}
