// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: nil; -*-
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <Rcpp.h>
#include <R.h>

using namespace Rcpp;

RObject evCur;

Function getRxFn(std::string name);

extern "C" SEXP orderForderS1(SEXP ordIn) {
  Function order1 = getRxFn(".order1");
  return order1(ordIn);
}

extern "C" SEXP _rxode2_etDollarNames(SEXP obj) {
  return CharacterVector::create();
}

extern "C" SEXP _rxode2_etUpdate(SEXP obj, SEXP arg, SEXP value, SEXP exact) {
  return R_NilValue;
}

extern "C" SEXP _rxode2_et_(SEXP input, SEXP et__) {
  Rf_error("old-style event table is no longer supported");
  return R_NilValue;
}
