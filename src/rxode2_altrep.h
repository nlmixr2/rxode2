#ifndef RXODE2_ALTREP_H
#define RXODE2_ALTREP_H
#ifdef __cplusplus
extern "C" {
#endif
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Altrep.h>

/* Register the rx_seqrep ALTREP class; call once from R_init_rxode2. */
void rxode2_init_altrep_class(DllInfo *info);

/*
 * Create a compact ALTREP integer vector representing
 *   rep(1:n_vals, each = run_len, times = n_reps)
 *
 * Total length = n_vals * run_len * n_reps
 * Element i  = ((i / run_len) % n_vals) + 1
 *
 * Used for the 'id' column   (n_vals=nsub,  run_len=rows_per_sub,  n_reps=nsim)
 * and the 'sim.id' column    (n_vals=nsim,  run_len=rows_per_sim,  n_reps=1)
 */
SEXP rxode2_make_seqrep(int n_vals, int run_len, R_xlen_t total_len);

/*
 * Create compact ALTREP vectors representing rep(base, times=times)
 * for integer, logical and real vectors.
 */
SEXP rxode2_make_rep_int(SEXP base, int times);
SEXP rxode2_make_rep_lgl(SEXP base, int times);
SEXP rxode2_make_rep_real(SEXP base, int times);
SEXP rxode2_make_rep_str(SEXP base, int times);

int rxInt(SEXP x, R_xlen_t i);
double rxReal(SEXP x, R_xlen_t i);
Rboolean is_rx_rep_int(SEXP x);
Rboolean is_rx_seqrep(SEXP x);
Rboolean is_rx_rep_str(SEXP x);

/*
 * Zero-copy ALTREP column view constructors.
 * make_rx_col_view_int/real wrap an existing R vector; Dataptr() returns
 * the original column pointer directly (no allocation).
 */
SEXP make_rx_col_view_int(SEXP col);
SEXP make_rx_col_view_real(SEXP col);

/*
 * Lazy CMT translation ALTREP.
 * orig_col: original CMT column (INTSXP integer codes or STRSXP names)
 * cmt_nums: INTSXP of solver compartment numbers (1-based) parallel to cmt_names
 * cmt_names: STRSXP of compartment names matching cmt_nums
 * Elt() translates one element without allocating n elements.
 * Dataptr() forces full materialization on first call; cached after.
 */
SEXP make_rx_cmt_trans(SEXP orig_col, SEXP cmt_nums, SEXP cmt_names);

#ifdef __cplusplus
}
#endif
#endif /* RXODE2_ALTREP_H */
