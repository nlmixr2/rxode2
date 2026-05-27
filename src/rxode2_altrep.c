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

// While this gives a NOTE in the 4.5 build,
// it is what is suggested in the WRE manual
// so it should be OK for CRAN.
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

/* col-view: zero-copy wrapper around an existing R column */
static R_altrep_class_t rx_col_view_int_class;
static R_altrep_class_t rx_col_view_real_class;

/* cmt-trans: lazy CMT name/number → solver compartment integer */
static R_altrep_class_t rx_cmt_trans_class;

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
  R_xlen_t block = (R_xlen_t)n_vals * run_len;

  if (block <= 0) {
    for (R_xlen_t i = 0; i < len; i++) p[i] = NA_INTEGER;
    R_set_altrep_data2(x, mat);
    UNPROTECT(1);
    return mat;
  }

  /* Build the first repeating block element-by-element. */
  if (block > len) block = len;
  R_xlen_t idx = 0;
  for (int v = 0; v < n_vals && idx < block; v++) {
    for (int r = 0; r < run_len && idx < block; r++) {
      p[idx++] = v + 1;
    }
  }

  /* Exponential doubling: each memcpy at least doubles the filled region. */
  for (R_xlen_t filled = block; filled < len; ) {
    R_xlen_t chunk = filled;
    if (filled + chunk > len) chunk = len - filled;
    memcpy(p + filled, p, chunk * sizeof(int));
    filled += chunk;
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
  int n_vals       = d[0];
  R_xlen_t total_len = (R_xlen_t)d[2];
  /* Sum of rep(1:n_vals, each=run_len, times=n_reps)
   * = total_len * (n_vals+1) / 2  (division-free; safe when n_vals or run_len is 0) */
  double s = (double)total_len * (n_vals + 1) / 2.0;
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
  return INTEGER_ELT(base, i % n);
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
  return LOGICAL_ELT(base, i % n);
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
  return REAL_ELT(base, i % n);
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

/* ---- Get_region --------------------------------------------------------- */

/* rx_seqrep: fill buf[0..size) without element-by-element modulo overhead.
 * Advance through runs, filling each run value with a tight inner loop that
 * compilers can auto-vectorise (constant value, sequential write). */
static R_xlen_t rx_seqrep_Get_region(SEXP x, R_xlen_t i, R_xlen_t size, int *buf) {
  const int *d    = INTEGER(R_altrep_data1(x));
  int n_vals      = d[0];
  int run_len     = d[1];
  R_xlen_t total  = (R_xlen_t)d[2];

  if (i >= total) return 0;
  if (i + size > total) size = total - i;

  R_xlen_t period  = (R_xlen_t)n_vals * run_len;
  R_xlen_t pos     = i % period;
  int val_idx      = (int)(pos / run_len);        /* 0-based */
  R_xlen_t rem     = (R_xlen_t)run_len - pos % (R_xlen_t)run_len;

  R_xlen_t written = 0;
  while (written < size) {
    R_xlen_t chunk = rem < size - written ? rem : size - written;
    int val = val_idx + 1;
    for (R_xlen_t k = 0; k < chunk; k++) buf[written + k] = val;
    written += chunk;
    val_idx  = (val_idx + 1) % n_vals;
    rem      = run_len;
  }
  return size;
}

/* rx_rep_int/lgl/real: tile the base vector using the base's own Get_region,
 * so an ALTREP base (e.g. seqrep) is never forced to materialise. */
static R_xlen_t rx_rep_int_Get_region(SEXP x, R_xlen_t i, R_xlen_t size, int *buf) {
  SEXP base      = rx_rep_base(x);
  R_xlen_t n     = XLENGTH(base);
  R_xlen_t total = rx_rep_total_len(x);

  if (n == 0 || i >= total) return 0;
  if (i + size > total) size = total - i;

  R_xlen_t pos = i % n, written = 0;
  while (written < size) {
    R_xlen_t chunk = n - pos;
    if (chunk > size - written) chunk = size - written;
    INTEGER_GET_REGION(base, pos, chunk, buf + written);
    written += chunk;
    pos      = 0;
  }
  return size;
}

static R_xlen_t rx_rep_lgl_Get_region(SEXP x, R_xlen_t i, R_xlen_t size, int *buf) {
  SEXP base      = rx_rep_base(x);
  R_xlen_t n     = XLENGTH(base);
  R_xlen_t total = rx_rep_total_len(x);

  if (n == 0 || i >= total) return 0;
  if (i + size > total) size = total - i;

  R_xlen_t pos = i % n, written = 0;
  while (written < size) {
    R_xlen_t chunk = n - pos;
    if (chunk > size - written) chunk = size - written;
    LOGICAL_GET_REGION(base, pos, chunk, buf + written);
    written += chunk;
    pos      = 0;
  }
  return size;
}

static R_xlen_t rx_rep_real_Get_region(SEXP x, R_xlen_t i, R_xlen_t size, double *buf) {
  SEXP base      = rx_rep_base(x);
  R_xlen_t n     = XLENGTH(base);
  R_xlen_t total = rx_rep_total_len(x);

  if (n == 0 || i >= total) return 0;
  if (i + size > total) size = total - i;

  R_xlen_t pos = i % n, written = 0;
  while (written < size) {
    R_xlen_t chunk = n - pos;
    if (chunk > size - written) chunk = size - written;
    REAL_GET_REGION(base, pos, chunk, buf + written);
    written += chunk;
    pos      = 0;
  }
  return size;
}

/* ======================================================================
 * rx_col_view_int — zero-copy passthrough for an INTSXP column
 * data1 = original column SEXP (holds GC reference)
 * data2 = unused
 * ====================================================================== */
static R_xlen_t rx_col_view_int_Length(SEXP x) {
  return XLENGTH(R_altrep_data1(x));
}
static int rx_col_view_int_Elt(SEXP x, R_xlen_t i) {
  return INTEGER_ELT(R_altrep_data1(x), i);
}
static void *rx_col_view_int_Dataptr(SEXP x, Rboolean writeable) {
  return DATAPTR_RW(R_altrep_data1(x));
}
static const void *rx_col_view_int_Dataptr_or_null(SEXP x) {
  return DATAPTR_OR_NULL(R_altrep_data1(x));
}
static R_xlen_t rx_col_view_int_Get_region(SEXP x, R_xlen_t i, R_xlen_t size, int *buf) {
  return INTEGER_GET_REGION(R_altrep_data1(x), i, size, buf);
}

/* ======================================================================
 * rx_col_view_real — zero-copy passthrough for a REALSXP column
 * ====================================================================== */
static R_xlen_t rx_col_view_real_Length(SEXP x) {
  return XLENGTH(R_altrep_data1(x));
}
static double rx_col_view_real_Elt(SEXP x, R_xlen_t i) {
  return REAL_ELT(R_altrep_data1(x), i);
}
static void *rx_col_view_real_Dataptr(SEXP x, Rboolean writeable) {
  return DATAPTR_RW(R_altrep_data1(x));
}
static const void *rx_col_view_real_Dataptr_or_null(SEXP x) {
  return DATAPTR_OR_NULL(R_altrep_data1(x));
}
static R_xlen_t rx_col_view_real_Get_region(SEXP x, R_xlen_t i, R_xlen_t size, double *buf) {
  return REAL_GET_REGION(R_altrep_data1(x), i, size, buf);
}

/* ======================================================================
 * rx_cmt_trans — lazy CMT translation (name/number → solver cmt int)
 *
 * data1 = VECSXP[3]:
 *   [0] original CMT column (STRSXP or INTSXP from input data frame)
 *   [1] INTSXP of solver compartment numbers (parallel to [2])
 *   [2] STRSXP of compartment names (parallel to [1])
 *       For integer input: [2] may be R_NilValue, look up by 1-based index
 * data2 = R_NilValue until materialised, then cached INTSXP of full length
 * ====================================================================== */
static R_xlen_t rx_cmt_trans_Length(SEXP x) {
  return XLENGTH(VECTOR_ELT(R_altrep_data1(x), 0));
}

static int rx_cmt_trans_lookup(SEXP data1, R_xlen_t i) {
  SEXP orig    = VECTOR_ELT(data1, 0);
  SEXP nums    = VECTOR_ELT(data1, 1);
  SEXP names   = VECTOR_ELT(data1, 2);
  int  n_table = LENGTH(nums);

  if (TYPEOF(orig) == STRSXP) {
    /* character CMT: linear scan of compartment name table */
    const char *name = CHAR(STRING_ELT(orig, i));
    for (int j = 0; j < n_table; j++) {
      if (names != R_NilValue &&
          strcmp(name, CHAR(STRING_ELT(names, j))) == 0) {
        return INTEGER(nums)[j];
      }
    }
    return NA_INTEGER;
  } else {
    /* integer CMT: 1-based direct lookup */
    int cmt_val = INTEGER_ELT(orig, i);
    if (cmt_val >= 1 && cmt_val <= n_table) {
      return INTEGER(nums)[cmt_val - 1];
    }
    return cmt_val; /* out-of-range: pass through */
  }
}

static int rx_cmt_trans_Elt(SEXP x, R_xlen_t i) {
  /* use cached materialisation if available */
  SEXP d2 = R_altrep_data2(x);
  if (TYPEOF(d2) == INTSXP) return INTEGER_ELT(d2, i);
  return rx_cmt_trans_lookup(R_altrep_data1(x), i);
}

static SEXP rx_cmt_trans_materialize(SEXP x) {
  SEXP d2 = R_altrep_data2(x);
  if (TYPEOF(d2) == INTSXP) return d2;
  R_xlen_t n = rx_cmt_trans_Length(x);
  SEXP mat = PROTECT(Rf_allocVector(INTSXP, n));
  SEXP d1  = R_altrep_data1(x);
  for (R_xlen_t i = 0; i < n; i++) INTEGER(mat)[i] = rx_cmt_trans_lookup(d1, i);
  R_set_altrep_data2(x, mat);
  UNPROTECT(1);
  return mat;
}

static void *rx_cmt_trans_Dataptr(SEXP x, Rboolean writeable) {
  return DATAPTR_RW(rx_cmt_trans_materialize(x));
}

static const void *rx_cmt_trans_Dataptr_or_null(SEXP x) {
  SEXP d2 = R_altrep_data2(x);
  if (TYPEOF(d2) == INTSXP) return DATAPTR_RO(d2);
  return NULL; /* not yet materialised */
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
  R_set_altinteger_Get_region_method(rx_seqrep_class, rx_seqrep_Get_region);

  rx_rep_int_class = R_make_altinteger_class("rx_rep_int", "rxode2", info);
  R_set_altrep_Length_method(rx_rep_int_class, rx_rep_int_Length);
  R_set_altinteger_Elt_method(rx_rep_int_class, rx_rep_int_Elt);
  R_set_altvec_Dataptr_method(rx_rep_int_class, rx_rep_int_Dataptr);
  R_set_altvec_Dataptr_or_null_method(rx_rep_int_class, rx_rep_int_Dataptr_or_null);
  R_set_altinteger_Get_region_method(rx_rep_int_class, rx_rep_int_Get_region);

  rx_rep_lgl_class = R_make_altlogical_class("rx_rep_lgl", "rxode2", info);
  R_set_altrep_Length_method(rx_rep_lgl_class, rx_rep_lgl_Length);
  R_set_altlogical_Elt_method(rx_rep_lgl_class, rx_rep_lgl_Elt);
  R_set_altvec_Dataptr_method(rx_rep_lgl_class, rx_rep_lgl_Dataptr);
  R_set_altvec_Dataptr_or_null_method(rx_rep_lgl_class, rx_rep_lgl_Dataptr_or_null);
  R_set_altlogical_Get_region_method(rx_rep_lgl_class, rx_rep_lgl_Get_region);

  rx_rep_real_class = R_make_altreal_class("rx_rep_real", "rxode2", info);
  R_set_altrep_Length_method(rx_rep_real_class, rx_rep_real_Length);
  R_set_altreal_Elt_method(rx_rep_real_class, rx_rep_real_Elt);
  R_set_altvec_Dataptr_method(rx_rep_real_class, rx_rep_real_Dataptr);
  R_set_altvec_Dataptr_or_null_method(rx_rep_real_class, rx_rep_real_Dataptr_or_null);
  R_set_altreal_Get_region_method(rx_rep_real_class, rx_rep_real_Get_region);

  rx_rep_str_class = R_make_altstring_class("rx_rep_str", "rxode2", info);
  R_set_altrep_Length_method(rx_rep_str_class, rx_rep_str_Length);
  R_set_altstring_Elt_method(rx_rep_str_class, rx_rep_str_Elt);
  R_set_altvec_Dataptr_method(rx_rep_str_class, rx_rep_str_Dataptr);
  R_set_altvec_Dataptr_or_null_method(rx_rep_str_class, rx_rep_str_Dataptr_or_null);

  rx_col_view_int_class = R_make_altinteger_class("rx_col_view_int", "rxode2", info);
  R_set_altrep_Length_method(rx_col_view_int_class, rx_col_view_int_Length);
  R_set_altinteger_Elt_method(rx_col_view_int_class, rx_col_view_int_Elt);
  R_set_altvec_Dataptr_method(rx_col_view_int_class, rx_col_view_int_Dataptr);
  R_set_altvec_Dataptr_or_null_method(rx_col_view_int_class, rx_col_view_int_Dataptr_or_null);
  R_set_altinteger_Get_region_method(rx_col_view_int_class, rx_col_view_int_Get_region);

  rx_col_view_real_class = R_make_altreal_class("rx_col_view_real", "rxode2", info);
  R_set_altrep_Length_method(rx_col_view_real_class, rx_col_view_real_Length);
  R_set_altreal_Elt_method(rx_col_view_real_class, rx_col_view_real_Elt);
  R_set_altvec_Dataptr_method(rx_col_view_real_class, rx_col_view_real_Dataptr);
  R_set_altvec_Dataptr_or_null_method(rx_col_view_real_class, rx_col_view_real_Dataptr_or_null);
  R_set_altreal_Get_region_method(rx_col_view_real_class, rx_col_view_real_Get_region);

  rx_cmt_trans_class = R_make_altinteger_class("rx_cmt_trans", "rxode2", info);
  R_set_altrep_Length_method(rx_cmt_trans_class, rx_cmt_trans_Length);
  R_set_altinteger_Elt_method(rx_cmt_trans_class, rx_cmt_trans_Elt);
  R_set_altvec_Dataptr_method(rx_cmt_trans_class, rx_cmt_trans_Dataptr);
  R_set_altvec_Dataptr_or_null_method(rx_cmt_trans_class, rx_cmt_trans_Dataptr_or_null);
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

extern Rboolean is_rx_rep_str(SEXP x) {
  return ALTREP(x) && R_altrep_inherits(x, rx_rep_str_class);
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

/* ---- Factory functions for new ALTREP classes -------------------------- */

/* Create a zero-copy integer column view */
SEXP make_rx_col_view_int(SEXP col) {
  return R_new_altrep(rx_col_view_int_class, col, R_NilValue);
}

/* Create a zero-copy real column view */
SEXP make_rx_col_view_real(SEXP col) {
  return R_new_altrep(rx_col_view_real_class, col, R_NilValue);
}

/*
 * Create a lazy CMT translation ALTREP.
 *
 * orig_col : original CMT column from input data (STRSXP or INTSXP)
 * cmt_nums : INTSXP of solver compartment numbers (1-indexed parallel to cmt_names)
 * cmt_names: STRSXP of compartment names (parallel to cmt_nums); may be R_NilValue
 *            for integer-only input where direct indexing is used
 */
SEXP make_rx_cmt_trans(SEXP orig_col, SEXP cmt_nums, SEXP cmt_names) {
  SEXP d1 = PROTECT(Rf_allocVector(VECSXP, 3));
  SET_VECTOR_ELT(d1, 0, orig_col);
  SET_VECTOR_ELT(d1, 1, cmt_nums);
  SET_VECTOR_ELT(d1, 2, cmt_names);
  SEXP out = R_new_altrep(rx_cmt_trans_class, d1, R_NilValue);
  UNPROTECT(1);
  return out;
}
