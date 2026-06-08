// File: rhs_func.h

#ifndef RHS_FUNC_H
#define RHS_FUNC_H

// struct to use if R or Rcpp function is input as RHS function
struct rhs_func{
  Rcpp::Function rhs_eqn;
  Rcpp::NumericVector params;
};

int rhs_function(sunrealtype t, N_Vector y, N_Vector ydot, void* user_data);

#endif /* rhs_func */
