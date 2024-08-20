#include "../inst/include/rxode2.h"
#include "rx2api.h"

rx_solving_options* getSolvingOptions(rx_solve* rx) {
  return rx->op;
}

rx_solving_options_ind *getSolvingOptionsInd(rx_solve *rx, int id) {
  return &(rx->subjects[id]);
}

double getIndLambda(rx_solving_options_ind* ind) {
  return ind->lambda;
}

int getIndLambdaYj(rx_solving_options_ind* ind) {
  return (int)(ind->yj);
}

double getIndLogitLow(rx_solving_options_ind* ind) {
  return ind->logitLow;
}

double getIndLogitHi(rx_solving_options_ind* ind) {
  return ind->logitHi;
}


void setIndParPtr(rx_solving_options_ind* ind, int i, double val) {
  ind->par_ptr[i] = val;
}

double getIndParPtr(rx_solving_options_ind* ind, int i) {
  return ind->par_ptr[i];
}

int getIndNallTimes(rx_solving_options_ind* ind) {
  return ind->n_all_times;
}

void setIndIdx(rx_solving_options_ind* ind, int j) {
  ind->idx = j;
}

int getIndIx(rx_solving_options_ind* ind, int j) {
  return ind->ix[j];
}
