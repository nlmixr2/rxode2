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

#if defined(__cplusplus)
}
#endif
#endif // __RX2API_H__
