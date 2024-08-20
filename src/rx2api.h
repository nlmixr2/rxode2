#ifndef __RX2API_H__
#define __RX2API_H__
#if defined(__cplusplus)
extern "C" {
#endif
  // rx2api.h

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

#if defined(__cplusplus)
}
#endif
#endif // __RX2API_H__
