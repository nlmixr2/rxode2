// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: nil; -*-
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
// [[Rcpp::interfaces(r,cpp)]]
#include <RcppArmadillo.h>
#include "../inst/include/rxode2.h"       /* rxLlikSaveSize; pulls in rxode2parseStruct.h */
#include "../inst/include/rxMemoryCalc.h" /* rx_mem_layout, rxFillMemLayout()             */

using namespace Rcpp;

//' Report the byte sizes of every rxode2 solver memory allocation
//'
//' Returns a named numeric vector of byte counts for every buffer rxode2
//' allocates during \code{rxSolve()}.  Values are computed with the same
//' formulas used by the actual allocator in \code{rxData.cpp} via the shared
//' \code{rxFillMemLayout()} function, so any change to the allocator
//' automatically changes the estimate.
//'
//' @param neq       Number of ODE states (\code{length(rxModelVars(model)$state)}).
//' @param stateSize Effective \code{state.size()} passed to the solver.
//'   For pure ODE models this equals \code{neq}; for linCmt-only models it
//'   may be 0.  Use \code{neq} when in doubt.
//' @param nlhs      Number of LHS (calculated) outputs.
//' @param npars     Number of model parameters (for \code{gpars} estimate).
//' @param neta      Number of random effects (etas).
//' @param neps      Number of residual-error levels (epsilons).
//' @param ncov      Number of time-varying covariates.
//' @param nsim      Number of simulations.
//' @param cores     Number of parallel OMP threads.
//' @param nMtime    Number of model measurement times.
//' @param extraCmt  Extra compartments (0, 1 = depot, 2 = depot+central).
//' @param linB      1 if using an analytical linear-compartment model, else 0.
//' @param nLlik     Number of log-likelihood terms (FOCEi).
//' @param nIndSim   Per-individual simulation count (typically \code{neta+neps}).
//' @param numLinSens Number of linear sensitivity parameters (FOCEi mixed models).
//' @param numLin    Number of linear compartment terms (FOCEi mixed models).
//' @param nsub      Number of subjects.
//' @param nallTotal Total events across all subjects (sum of obs + doses).
//' @param ndosesTotal Total dose events across all subjects.
//' @param maxAllTimes Maximum events for any single subject.
//' @param indOwnAlloc 1 if every individual gets its own event/solve arrays
//'   (\code{op$indOwnAlloc}), else 0.  These are allocated ON TOP of
//'   \code{gsolve}, whose \code{n0} region then goes unused.
//' @param stiff     The solving method (\code{op$stiff}); only 3
//'   (\code{"indLin"}) allocates anything extra here.
//' @param doIndLin  Which matrix-exponential driver runs: 0 not a
//'   \code{matExp()} model, 1 pure matrix exponential, 2 plus a state-free
//'   \code{indLin()} forcing, 3/4 true inductive linearization (the adaptive,
//'   iterating driver).  These cost very different amounts.
//' @return Named numeric vector; each element is bytes for that allocation.
//'   Also includes \code{sizeofInd} (bytes per \code{rx_solving_options_ind}
//'   struct) and \code{rxLlikSaveSize} (the compile-time constant).
//' @noRd
// [[Rcpp::export]]
NumericVector rxMemoryComponents_(
  int    neq,
  int    stateSize,
  int    nlhs,
  int    npars,
  int    neta,
  int    neps,
  int    ncov,
  int    nsim,
  int    cores,
  int    nMtime,
  int    extraCmt,
  int    linB,
  int    nLlik,
  int    nIndSim,
  int    numLinSens,
  int    numLin,
  int    nsub,
  double nallTotal,
  double ndosesTotal,
  double maxAllTimes,
  int    stiff,
  int    doIndLin,
  int    indOwnAlloc)
{
  rx_mem_layout _mem;
  rxFillMemLayout(
    neq,
    stateSize,
    nlhs,
    nsim,
    cores,
    nMtime,
    extraCmt,
    linB,
    nLlik,
    nIndSim,
    nsub,
    (int64_t)nallTotal,
    (int)maxAllTimes,
    numLinSens,
    numLin,
    (int64_t)neq,          /* n4_actual: neq proxy for estimate */
    (int64_t)neq,          /* n6_actual: neq proxy for estimate */
    &_mem);

  /* Byte counts for allocations outside gsolve/gon */
  double b_gall_times  = 5.0  * nallTotal * sizeof(double);
  double b_gevid       = 3.0  * nallTotal * sizeof(int);
  double b_gcov        = (double)ncov * nallTotal * sizeof(double);
  double b_gpars       = (double)npars * nsub * sizeof(double);
  double b_gomega      = (double)(2 * neta + neta * neta) * sizeof(double);
  double b_gsigma      = (double)(2 * neps + neps * neps) * sizeof(double);
  double b_gall_timesS = (nsim > 1)
                           ? 2.0 * (nsim - 1) * nallTotal * sizeof(double)
                           : 0.0;
  double b_ordId       = nallTotal * sizeof(int);
  double b_gInfRate    = (double)cores * (neq + extraCmt) * sizeof(double);
  double b_inds        = (double)nsub  * sizeof(rx_solving_options_ind);

  /* -- method="indLin" (src/expm.cpp) ---------------------------------------
   *
   * Unlike everything above, these scale with CORES and not with subjects: the
   * exponential cache and the solver's Armadillo scratch are per thread, and a
   * subject is solved start to finish on one thread.  `.rxOomChunkSize()` has
   * to hold them out of its per-subject division for that reason.
   *
   * `m` is the dimension of the matrix actually exponentiated, which is NOT
   * `neq`: `meOnly()` augments by one row per compartment carrying a nonzero
   * forcing, and the iterating schemes augment further (exprb2 neq+1, exprb32
   * neq+3, and the `indLinPmat()` phi fallback neq*(p+1), up to 3*neq).  Take
   * the worst case each driver can reach, since the estimate exists to answer
   * "will this fit", and an estimate that is too low is the useless kind.
   */
  double b_indLinCache = 0.0;
  double b_indLinWork  = 0.0;
  if (stiff == 3 && doIndLin > 0) {
    const double dneq = (double) neq;
    double m;
    switch (doIndLin) {
    case 1:  m = dneq + 1.0;  break;   /* pure matExp: augmented while infusing */
    case 2:  m = 2.0 * dneq;  break;   /* + a forcing that may fill every row   */
    default: m = 3.0 * dneq;  break;   /* iterating: the indLinPmat p=2 fallback */
    }
    /* The cache holds RX_INDLIN_EXPCACHE_N slots per thread, each a key and a
     * value of m*m doubles -- but caching is SKIPPED above
     * RX_INDLIN_EXPCACHE_MAXN2, so past that only the empty slots cost
     * anything.  Both constants live in src/expm.cpp. */
    const double slots  = 16.0;                     /* RX_INDLIN_EXPCACHE_N     */
    const double maxN2  = 16384.0;                  /* RX_INDLIN_EXPCACHE_MAXN2 */
    const double m2     = m * m;
    const double perSlot = (m2 <= maxN2) ? 2.0 * m2 * sizeof(double) : 0.0;
    /* An empty slot is still two std::vector headers plus the key fields. */
    const double slotHdr = 2.0 * 24.0 + 2.0 * sizeof(double);
    b_indLinCache = (double)cores * slots * (perSlot + slotHdr);
    /* Scratch live at once in one thread.  The fixed-grid drivers (1/2) only
     * ever hold meOnly()'s rate matrix and its augmented exponential; the
     * iterating ones additionally hold the Jacobian, P(h), its inverse, the
     * ramp and the Richardson table. */
    const double sq = (doIndLin <= 2) ? (dneq*dneq + 2.0*m2)
                                      : (12.0*dneq*dneq + 2.0*m2);
    const double vec = (doIndLin <= 2) ? (4.0*dneq) : (25.0*dneq);
    b_indLinWork = (double)cores * (sq + vec) * sizeof(double);
  }

  /* -- op->indOwnAlloc (rxAllocInd() in src/rxData.cpp) ----------------------
   *
   * Each individual gets its own event and solve arrays; gsolve is still
   * calloc'd at gsolve_total, so this is memory on top of it rather than
   * instead of it.  Zero for the ordinary case, where ind->solve just points
   * into gsolve's n0 region.
   */
  double b_indOwn = 0.0;
  if (indOwnAlloc) {
    rx_ind_alloc _ia;
    rxFillIndAllocTotal(neq, (int64_t)nallTotal, (int64_t)ndosesTotal,
                        nsub, nsim, &_ia);
    b_indOwn = (double)_ia.dbl * sizeof(double) + (double)_ia.i32 * sizeof(int);
  }

  NumericVector out = NumericVector::create(
    Named("gsolve")        = (double)_mem.gsolve_total * sizeof(double),
    Named("gsolve_n0")     = (double)_mem.n0           * sizeof(double),
    Named("gon")           = (double)_mem.gon_total    * sizeof(int),
    Named("gall_times")    = b_gall_times,
    Named("gevid")         = b_gevid,
    Named("gcov")          = b_gcov,
    Named("gpars")         = b_gpars,
    Named("gomega")        = b_gomega,
    Named("gsigma")        = b_gsigma,
    Named("gall_timesS")   = b_gall_timesS,
    Named("ordId")         = b_ordId,
    Named("gInfusionRate") = b_gInfRate,
    Named("inds_global")   = b_inds,
    Named("indLinExpCache")= b_indLinCache,
    Named("indLinWork")    = b_indLinWork,
    Named("indOwnAlloc")   = b_indOwn,
    Named("sizeofInd")    = (double)sizeof(rx_solving_options_ind),
    Named("rxLlikSaveSize")= (double)rxLlikSaveSize);

  return out;
}
