#include "../inst/include/rxode2.h"
#include "rx2api.h"

rx_solve *getRxSolve_(void);

rx_solving_options* getSolvingOptions(rx_solve* rx) {
  return rx->op;
}

rx_solving_options_ind *getSolvingOptionsInd(rx_solve *rx, int id) {
  int nall = rx->nsub*rx->nsim;
  if (id < 0 || id >= nall) {
    Rf_error("[getSolvingOptionsInd]: id (%d) should be between [0, %d); nsub: %d nsim: %d", id, nall, rx->nsub, rx->nsim);
  }
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
  rx_solve* rx = getRxSolve_();
  if (i < 0 || i >= rx->npars) {
    Rf_error("[setIndParPtr]: i (%d) should be between [0, %d) when assigning  %f", i, rx->npars, val);
  }
  ind->par_ptr[i] = val;
}

double getIndParPtr(rx_solving_options_ind* ind, int i) {
  rx_solve* rx = getRxSolve_();
  if (i < 0 || i >= rx->npars) {
    Rf_error("[getIndParPtr]: i (%d) should be between [0, %d)", i, rx->npars);
  }
  return ind->par_ptr[i];
}

int getIndNallTimes(rx_solving_options_ind* ind) {
  return ind->n_all_times;
}

void setIndIdx(rx_solving_options_ind* ind, int j) {
  ind->idx = j;
}

int getIndIx(rx_solving_options_ind* ind, int j) {
  if (j < 0 || j >= ind->n_all_times) {
    Rf_error("[getIndIx]: j (%d) should be between [0, %d)", j, ind->n_all_times);
  }
  return ind->ix[j];
}

int getIndEvid(rx_solving_options_ind* ind, int kk) {
  if (kk < 0 || kk >= ind->n_all_times) {
    Rf_error("[getIndEvid]: kk (%d) should be between [0, %d)", kk, ind->n_all_times);
  }
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
  if (j < 0 || j >= ind->n_all_times) {
    Rf_error("[getIndDv]: j (%d) should be between [0, %d)", j, ind->n_all_times);
  }
  return ind->dv[j];
}

int getIndYj(rx_solving_options_ind* ind) {
  return (int)(ind->yj);
}

double getIndLimit(rx_solving_options_ind* ind, int kk) {
  if (kk < 0 || kk >= ind->n_all_times) {
    Rf_error("[getIndLimit]: kk (%d) should be between [0, %d)", kk, ind->n_all_times);
  }
  return ind->limit[kk];
}

int getIndCens(rx_solving_options_ind* ind, int kk) {
  if (kk < 0 || kk >= ind->n_all_times) {
    Rf_error("[getIndCens]: kk (%d) should be between [0, %d)", kk, ind->n_all_times);
  }
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

int getOpStiff(rx_solving_options* op) {
  return op->stiff;
}

void resetOpBadSolve(rx_solving_options* op) {
  op->badSolve = 0;
}
////////////////////////////////////////////////////////////////////////
// Solving options in rx
////////////////////////////////////////////////////////////////////////

int getRxNsub(rx_solve *rx) {
  return rx->nsub;
}

int hasRxLimit(rx_solve *rx) {
  return rx->limit;
}

int hasRxCens(rx_solve *rx) {
  return rx->cens;
}

int getRxNall(rx_solve *rx) {
  return rx->nall;
}

int getRxNobs(rx_solve *rx) {
  return rx->nobs;
}

int getRxNobs2(rx_solve *rx) {
  return rx->nobs2;
}
////////////////////////////////////////////////////////////////////////
// Get solve vector for ith solve
////////////////////////////////////////////////////////////////////////
double * getOpIndSolve(rx_solving_options* op, rx_solving_options_ind* ind, int idx) {
  return ind->solve + (op->neq + op->nlin)*(idx);
}
