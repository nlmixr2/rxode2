#ifndef __RX2API_H__
#define __RX2API_H__
#include <stddef.h>
#if defined(__cplusplus)
extern "C" {
#endif
  // rx2api.h

  // Stride, in bytes, of one entry of the subject array (`rx->subjects`).
  // Written by rxOptionsIniEnsure() (par_solve.cpp) from the same
  // `sizeof(rx_solving_options_ind)` it allocates the array with, and read
  // back by getSolvingOptionsInd() below rather than that function's own
  // `sizeof`.  This file is the package's ABI surface and is a separate
  // translation unit from the one that owns the array, so taking the stride
  // from its own view of the struct made a correct subject pointer depend on
  // the two views agreeing.  They do not have to: R's default make rules
  // track no header dependencies, so an object file whose own source did not
  // change is linked in unchanged after a change to the struct, and the two
  // views then differ by exactly the appended bytes.  The array is laid out
  // at the allocator's stride, so that is the one to walk it with
  // (nlmixr2/nlmixr2est#1039).  A plain global, not a field of `rx_solve`:
  // reading a field would put the layout back in the loop.
  extern size_t rxIndSize;

  // This function gets the global rx solving options
  rx_solving_options* getSolvingOptions(rx_solve* rx);

  // This function gets the individual solving options for id
  rx_solving_options_ind* getSolvingOptionsInd(rx_solve *rx, int id);

  // This gets the current transformation of both sides variables

  // First is the lambda from Cox-Box and Yeo-Johnson transformations
  double getIndLambda(rx_solving_options_ind* ind);

  // this gets the transformation type
  int getIndLambdaYj(rx_solving_options_ind* ind);

  // transformation high boundary
  double getIndLogitLow(rx_solving_options_ind* ind);

  // transformation lower boundary
  double getIndLogitHi(rx_solving_options_ind* ind);

  // Set individual parameters in the par_ptr double vector
  void setIndParPtr(rx_solving_options_ind* ind, int i, double val);

  // Get individual parameters in the ptr_ptr double vector
  double getIndParPtr(rx_solving_options_ind* ind, int i);

  // The the individual's number of time and dosing points
  int getIndNallTimes(rx_solving_options_ind* ind);

  // Set the individual's index
  void setIndIdx(rx_solving_options_ind* ind, int j);

  // Get the sorted index (ix) j for the individual
  int getIndIx(rx_solving_options_ind* ind, int j);

  // Get the event id for the individual
  int getIndEvid(rx_solving_options_ind* ind, int kk);

  // Get Individual Left Hand Side (LHS) vector
  double *getIndLhs(rx_solving_options_ind* ind);

  // Get the number of doses for the individual
  int getIndNdoses(rx_solving_options_ind* ind);

  // This gets the number of events with EVID=2 in the individual
  int getIndNevid2(rx_solving_options_ind* ind);

  // This sets the index of where the linear compartment solver is solved to.
  void setIndSolve(rx_solving_options_ind* ind, int solve);

  // This gets the double vector of the individual's solver
  double *getIndSolve(rx_solving_options_ind* ind);

  // This gets the individual's dv at position j
  double getIndDv(rx_solving_options_ind* ind, int j);

  // Gets the individual's transformation type at the current time-point.
  int getIndYj(rx_solving_options_ind* ind);

  // Gets the individual's censoring limit at time index kk
  double getIndLimit(rx_solving_options_ind* ind, int kk);

  // Get the individual's censoring flag should be (0, -1, -1) at time index kk
  int getIndCens(rx_solving_options_ind* ind, int kk);

  // Get the index of the current solve
  int getIndIdx(rx_solving_options_ind* ind);

  // Per-observation endpoint from the CMT covariate (cached op->cmtCov); 1 if none
  int getIndCmt(rx_solving_options* op, rx_solving_options_ind* ind, int kk);

  // Get the mixest of the current solve
  int getIndMixest(rx_solving_options_ind* ind);

  // Set the individual mixest
  void setIndMixest(rx_solving_options_ind* ind, int mixest);

  // Get the number of mixtures in the rxode2 problem
  int getRxMixnum(rx_solve *rx);

  // Set the number of mixtures in the rxode2 problem
  void setRxMixnum(rx_solve *rx, int mixnum);

  // Get the problems number of ode equatons
  int getOpNeq(rx_solving_options* op);

  // Set the problems number of ode equations; This should be used
  // with cation This is used in the inner problem to calculate the
  // likelihood for finite differences.  It corrupts the solving
  // structure, so should be used with extreme caution.
  void setOpNeq(rx_solving_options* op, int neq);

  // Does this problem have a bad solve?
  int hasOpBadSolve(rx_solving_options* op);

  // Get the number of linear-related compartments
  int getOpNlin(rx_solving_options* op);

  // Get the number of cores from the rxode2 solving options
  int getOpCores(rx_solving_options* op);

  // Get the number of lhs in the rxode2 solving options
  int getOpNlhs(rx_solving_options* op);

  // Get the solving method (historically called stiff) from the
  // rxode2 solving options
  int getOpStiff(rx_solving_options* op);

  // reset the bad solve flag (that way you can repeat with different
  // options)
  void resetOpBadSolve(rx_solving_options* op);

  // Get the number of subjects in the rx_solve structure
  int getRxNsub(rx_solve *rx);

  // Get if the rxode2 has the limit (for censoring)
  int hasRxLimit(rx_solve *rx);

  //Does the rxode2 problem have censoring column?
  int hasRxCens(rx_solve *rx);

  // Get the number of all times in the rxode2 problem
  int getRxNall(rx_solve *rx);

  // Get the number of observation sin the rxode2 problem
  int getRxNobs(rx_solve *rx);

  // Get the number of observations excluding evid=2
  int getRxNobs2(rx_solve *rx);

  // Get the number of simulations (nsim); equals nPopPar/nsub when evenly divisible
  int getRxNsim(rx_solve *rx);

  // Get the number of parameters loaded
  int getRxNpars(rx_solve *rx);

  double * getOpIndSolve(rx_solving_options* op, rx_solving_options_ind* ind, int idx);

  // Get the per-individual sticky tolerance factor (initialized to 1.0).
  // This factor is re-applied to the thread-local tolerance arrays every
  // time iniSubject() re-initializes this individual.
  double getIndTolFactor(rx_solving_options_ind *ind);

  // Set the per-individual sticky tolerance factor.  Values > 1.0 loosen
  // tolerances; use when an individual is too stiff to solve at the
  // requested tolerance so the factor persists across re-solves.
  void setIndTolFactor(rx_solving_options_ind *ind, double tolFactor);

  // Get the per-individual neq override.  Returns -1 when no override
  // is in effect (caller should fall back to op->neq).  Use to solve
  // a single individual with a different effective neq without
  // mutating the shared op->neq from a parallel worker thread.
  int getIndNeqOverride(rx_solving_options_ind *ind);

  // Set the per-individual neq override.  Pass -1 to clear the
  // override.  Caller is responsible for restoring the prior value
  // (RAII guard in nlmixr2est).
  void setIndNeqOverride(rx_solving_options_ind *ind, int neq);

  void rxSetSilentErr(int silent);

  int getOrdId(rx_solve *rx, int solveid);

  int solveMethodThreadSafe(rx_solving_options* op);

  void atolRtolFactor_(double factor);

#if defined(__cplusplus)
}
#endif
#endif // __RX2API_H__
