#include "../inst/include/rxode2.h"
#include "rx2api.h"

rx_solving_options* getSolvingOptions(rx_solve* rx) {
  return rx->op;
}

rx_solving_options_ind *getSolvingOptionsInd(rx_solve *rx, int id) {
  return &(rx->subjects[id]);
}

////////////////////////////////////////////////////////////////////////
// Individual solving options
////////////////////////////////////////////////////////////////////////

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

int getIndEvid(rx_solving_options_ind* ind, int kk) {
  return ind->evid[kk];
}

double *getIndLhs(rx_solving_options_ind* ind) {
  return ind->lhs;
}

int getIndNdoses(rx_solving_options_ind* ind) {
  return ind->ndoses;
}

int getIndNevid2(rx_solving_options_ind* ind) {
  return ind->nevid2;
}

void setIndSolve(rx_solving_options_ind* ind, int solve) {
  ind->solved = solve;
}

double *getIndSolve(rx_solving_options_ind* ind) {
  return ind->solve;
}

double getIndDv(rx_solving_options_ind* ind, int j) {
  return ind->dv[j];
}

int getIndYj(rx_solving_options_ind* ind) {
  return (int)(ind->yj);
}

double getIndLimit(rx_solving_options_ind* ind, int kk) {
  return ind->limit[kk];
}

int getIndCens(rx_solving_options_ind* ind, int kk) {
  return ind->cens[kk];
}

int getIndIdx(rx_solving_options_ind* ind) {
  return ind->idx;
}

////////////////////////////////////////////////////////////////////////
// Solving options (rx->op)
////////////////////////////////////////////////////////////////////////

int getOpNeq(rx_solving_options* op) {
  return op->neq;
}

void setOpNeq(rx_solving_options* op, int neq) {
  op->neq = neq;
}

int hasOpBadSolve(rx_solving_options* op) {
  return op->badSolve;
}

int getOpNlin(rx_solving_options* op) {
  return op->nlin;
}

int getOpCores(rx_solving_options* op) {
  return op->cores;
}

int getOpNlhs(rx_solving_options* op) {
  return op->nlhs;
}
////////////////////////////////////////////////////////////////////////
// Solving options in rx
////////////////////////////////////////////////////////////////////////
