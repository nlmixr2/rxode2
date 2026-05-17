#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define ARMA_WARN_LEVEL 1
#define STRICT_R_HEADER
#include <RcppArmadillo.h>
#include "nearPD.h"
#include <lotri.h>

lotriNearPDarmaSetup

using namespace arma;
using namespace Rcpp;

bool rxNearPD(arma::mat &ret, const arma::mat in) {
  if (lotriNearPDarma(ret,  in)) {
    return true;
  } else {
    ret = in;
    return false;
  }
  return false;  // nocov
}

unsigned int rxNearPdChol(Rcpp::NumericMatrix &ret, Rcpp::NumericMatrix x, bool isChol) {
  arma::mat tmpM = as<arma::mat>(x);
  arma::mat reta;
  if (!x.hasAttribute("dimnames")) {
    return rxNearPdChol_not_named;
  } else if (isChol) {
    ret = x;
    return rxNearPdChol_isChol;
  } else if (tmpM.size() == 0) {
    ret = x;
    return rxNearPdChol_zero_size;
  } else if (tmpM.is_zero()){
    ret = x;
    return rxNearPdChol_zero;
  } else if (tmpM.is_sympd()){
    if (chol(reta, tmpM)) {
      ret = wrap(reta);
      ret.attr("dimnames") =  x.attr("dimnames");
      return rxNearPdChol_sympd_chol;
    }
    ret = wrap(tmpM);
    ret.attr("dimnames") =  x.attr("dimnames");
    return rxNearPdChol_sympd_bad_chol;
  }
  mat tmpMS = 0.5*(tmpM+tmpM.t());
  if (rxNearPD(reta, tmpMS)) {
    if (chol(tmpM, reta)) {
      ret = wrap(tmpM);
      ret.attr("dimnames") =  x.attr("dimnames");
      return rxNearPdChol_nearpd_chol;
    }
    ret = wrap(reta);
    ret.attr("dimnames") =  x.attr("dimnames");
    return rxNearPdChol_nearpd_bad_chol;
  } else {
    ret = x;
    return rxNearPdChol_bad_nearpd;
  }
}
