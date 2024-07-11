#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <Rcpp.h>
#include "../inst/include/rxode2parse.h"

Rcpp::Function loadNamespaceQs("loadNamespace", R_BaseNamespace);
Rcpp::Environment qsNs;
Rcpp::Environment rxode2parseNs;
bool loadQsC = false;
bool loadRxode2parse = false;


static void loadQs() {
  if (!loadQsC) {
    qsNs = loadNamespaceQs("qs");
    loadQsC = true;
  }
  if (!loadRxode2parse) {
    rxode2parseNs = loadNamespaceQs("rxode2parse");
    loadRxode2parse=true;
  }
}

extern "C" SEXP getRxode2ParseDf(void) {
  loadQs();
  Rcpp::Function getTran = Rcpp::as<Rcpp::Function>(rxode2parseNs["rxode2parseGetTranslation"]);
  return getTran();
}

extern "C" SEXP getRxode2ParseDfBuiltin(void) {
  loadQs();
  Rcpp::Function getTran = Rcpp::as<Rcpp::Function>(rxode2parseNs["rxode2parseGetTranslationBuiltin"]);
  return getTran();
}

extern "C" SEXP getRxode2ParseGetPointerAssignment(void) {
  loadQs();
  Rcpp::Function getPtr = Rcpp::as<Rcpp::Function>(rxode2parseNs["rxode2parseGetPointerAssignment"]);
  return getPtr();
}

//[[Rcpp::export]]
Rcpp::CharacterVector rxQs(SEXP const x) {
  loadQs();
  Rcpp::Function base91_encode = Rcpp::as<Rcpp::Function>(qsNs["base91_encode"]);
  Rcpp::Function qserialize = Rcpp::as<Rcpp::Function>(qsNs["qserialize"]);
  return base91_encode(qserialize(x, Rcpp::CharacterVector::create("high"), Rcpp::CharacterVector::create("zstd"),
				    Rcpp::IntegerVector::create(22),
				    Rcpp::IntegerVector::create(15), Rcpp::LogicalVector::create(true)));
}

//[[Rcpp::export]]
SEXP rxQr(const std::string& encoded_string) {
  loadQs();
  Rcpp::Function base91_decode = Rcpp::as<Rcpp::Function>(qsNs["base91_decode"]);
  Rcpp::Function qdeserialize = Rcpp::as<Rcpp::Function>(qsNs["qdeserialize"]);
  return qdeserialize(base91_decode(Rcpp::wrap(encoded_string)), false, false);
}



int rxode2parseIsRstudioI = 0;

//[[Rcpp::export]]
SEXP rxode2parseSetRstudio(bool isRstudio=false){
  if (isRstudio) rxode2parseIsRstudioI=1;
  else rxode2parseIsRstudioI=0;
  return Rcpp::wrap(rxode2parseIsRstudioI);
}

extern "C" void setSilentErr(int silent);

//' Silence some of rxode2's C/C++ messages
//'
//' @param silent can be 0L "noisy"  or 1L "silent"
//'
//' @keywords internal
//' @return TRUE; called for side effects
//' @export
//[[Rcpp::export]]
bool rxParseSetSilentErr(int silent){
  setSilentErr(silent);
  return true;
}
