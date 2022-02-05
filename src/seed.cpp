#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "rxomp.h"
#define min2( a , b )  ( (a) < (b) ? (a) : (b) )
#include <RcppArmadillo.h>
#include "../inst/include/rxode2.h"
#include <R.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif
using namespace Rcpp;
using namespace arma;

bool useRxSeed = false;

uint32_t rxSeed = 0;

extern "C" SEXP _rxSetSeed(SEXP intIn) {
  int type = TYPEOF(intIn);
  if (Rf_length(intIn) != 1) {
    Rf_errorcall(R_NilValue, _("'seed' must be an integer of length 1."));
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
    int in = REAL(intIn)[0];
    if (in < 0) {
      rxSeed = 0;
      useRxSeed = false;
    } else {
      rxSeed = (uint32_t)(in);
      useRxSeed = true;
    }
  } else {
    Rf_errorcall(R_NilValue, _("'seed' must be an integer of length 1."));
  }
  return R_NilValue;
}

uint32_t getRxSeed1(int ncores) {
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

//' Get the rxode2 seed
//'
//' @return rxode2 seed state or -1 when the seed isn't set
//'
//' @export
//' @seealso rxSetSeed, rxWithSeed, rxWithPreserveSeed
//' @examples
//'
//' # without setting seed
//'
//' rxGetSeed()
//' # Now set the seed
//' rxSetSeed(42)
//'
//' rxGetSeed()
//'
//' rxnorm()
//'
//' rxGetSeed()
//'
//' # don't use the rxode2 seed again
//'
//' rxSetSeed(-1)
//'
//' rxGetSeed()
//'
//' rxnorm()
//'
//' rxGetSeed()
//'
//[[Rcpp::export]]
int rxGetSeed() {
  if (useRxSeed) {
    return rxSeed;
  } else {
    return -1;
  }
}
