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

#ifdef __cplusplus
}
#endif
#endif /* RXODE2_ALTREP_H */
