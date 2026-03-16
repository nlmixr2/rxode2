#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "rxomp.h"
#define min2( a , b )  ( (a) < (b) ? (a) : (b) )
#include <RcppArmadillo.h>
#include "../inst/include/rxode2.h"
#include "../inst/include/rxode2parse.h"
#include <R.h>
#define _(String) (String)
using namespace Rcpp;
using namespace arma;

bool useRxSeed = false;

uint32_t rxSeed = 0;

extern "C" SEXP _rxode2_rxSetSeed(SEXP intIn) {
  int type = TYPEOF(intIn);
  if (Rf_length(intIn) != 1) {
    (Rf_errorcall)(R_NilValue, "%s", _("'seed' must be an integer of length 1"));
  }
  if (type == REALSXP) {
    double in = REAL(intIn)[0];
    if (in < 0) {
      rxSeed = 0;
      useRxSeed = false;
    } else {
      rxSeed = (uint32_t)(in);
      useRxSeed = true;
    }
  } else if (type == INTSXP) {
    int in = INTEGER(intIn)[0];
    if (in < 0) {
      rxSeed = 0;
      useRxSeed = false;
    } else {
      rxSeed = (uint32_t)(in);
      useRxSeed = true;
    }
  } else {
    (Rf_errorcall)(R_NilValue, "%s", _("'seed' must be an integer of length 1"));
  }
  return R_NilValue;
}

extern "C" uint32_t getRxSeed1(int ncores) {
  uint32_t seed;
  if (useRxSeed) {
    seed = rxSeed;
    rxSeed += ncores;
  } else {
    double seedD = runif(1, 1.0, std::numeric_limits<uint32_t>::max())[0];
    seed = static_cast<uint32_t>(seedD);
    seed = min2(seed, std::numeric_limits<uint32_t>::max() - ncores - 1);
  }
  return seed;
}

extern "C" void setRxSeedFinal(uint32_t seed) {
  if (useRxSeed) {
    rxSeed = seed;
  }
}

extern "C" void _rxode2_setGlobalSeed(SEXP seed) {
  Rcpp::Environment g = Rcpp::Environment::global_env();
  if (Rf_isNull(seed)) {
    g.remove(".Random.seed");
  } else {
    g[".Random.seed"] = Rcpp::as<Rcpp::IntegerVector>(seed);
  }
}


extern "C" SEXP _rxode2_rxGetSeed() {
  IntegerVector ret(1);
  if (useRxSeed) {
    ret[0] = rxSeed;
  } else {
    ret[1] =  -1;
  }
  return Rcpp::wrap(ret);
}

int rxGetSeed() {
  if (useRxSeed) {
    return rxSeed;
  } else {
    return -1;
  }
}
