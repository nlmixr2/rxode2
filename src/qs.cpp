#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <Rcpp.h>

Rcpp::Function loadNamespaceQs("loadNamespace", R_BaseNamespace);
Rcpp::Environment qsNs;
bool loadQsC = false;

static void loadQs() {
  if (!loadQsC) {
    qsNs = loadNamespaceQs("qs");
    loadQsC = true;
  }
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
