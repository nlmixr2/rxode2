#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <Rcpp.h>
#include "../inst/include/rxode2parse.h"

Rcpp::Function loadNamespaceQs("loadNamespace", R_BaseNamespace);
Rcpp::Environment qsNs;
bool loadQsC = false;


static void loadQs() {
  if (!loadQsC) {
    qsNs = loadNamespaceQs("qs2");
    loadQsC = true;
  }
}

Rcpp::Function getRxFn(std::string name);

extern "C" SEXP getRxode2ParseDf(void) {
  loadQs();
  Rcpp::Function getTran = Rcpp::as<Rcpp::Function>(getRxFn("rxode2parseGetTranslation"));
  return getTran();
}

extern "C" SEXP getRxode2ParseDfBuiltin(void) {
  loadQs();
  Rcpp::Function getTran = Rcpp::as<Rcpp::Function>(getRxFn("rxode2parseGetTranslationBuiltin"));
  return getTran();
}

extern "C" SEXP getRxode2ParseGetPointerAssignment(void) {
  loadQs();
  Rcpp::Function getPtr = Rcpp::as<Rcpp::Function>(getRxFn("rxode2parseGetPointerAssignment"));
  return getPtr();
}

//[[Rcpp::export]]
Rcpp::CharacterVector rxQs(SEXP const x) {
  loadQs();
  Rcpp::Function base91_encode = Rcpp::as<Rcpp::Function>(qsNs["base91_encode"]);
  Rcpp::Function qserialize = Rcpp::as<Rcpp::Function>(qsNs["qs_serialize"]);
  return base91_encode(qserialize(x));
}

//[[Rcpp::export]]
SEXP rxQr(const std::string& encoded_string) {
  loadQs();
  Rcpp::Function base91_decode = Rcpp::as<Rcpp::Function>(qsNs["base91_decode"]);
  Rcpp::Function qdeserialize = Rcpp::as<Rcpp::Function>(qsNs["qs_deserialize"]);
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


//' Get the serialization type from raw vector
//'
//' @param raw A raw vector
//'
//' @keywords internal
//'
//' @return a string indicating the serialization type:
//'    "qs2", "qdata", "qs", "base", or "unknown"
//[[Rcpp::export]]
Rcpp::CharacterVector rxGetSerialType_(SEXP raw) {
  unsigned char QS2_MAGIC_BITS[] = {0x0B,0x0E,0x0A,0xC1};
  unsigned char QDATA_MAGIC_BITS[] = {0x0B,0x0E,0x0A,0xCD};
  unsigned char QS_LEGACY_MAGIC_BITS[] = {0x0B,0x0E,0x0A,0x0C};
  Rcpp::CharacterVector ret(1);
  if (Rf_length(raw) < 4) {
    ret[0] = "unknown";
  } else if (RAW(raw)[0] == QS2_MAGIC_BITS[0] &&
             RAW(raw)[1] == QS2_MAGIC_BITS[1] &&
             RAW(raw)[2] == QS2_MAGIC_BITS[2] &&
             RAW(raw)[3] == QS2_MAGIC_BITS[3]) {
    ret[0] = "qs2";
  } else if (RAW(raw)[0] == QDATA_MAGIC_BITS[0] &&
             RAW(raw)[1] == QDATA_MAGIC_BITS[1] &&
             RAW(raw)[2] == QDATA_MAGIC_BITS[2] &&
             RAW(raw)[3] == QDATA_MAGIC_BITS[3]) {
    ret[0] = "qdata";
  } else if (RAW(raw)[0] == QS_LEGACY_MAGIC_BITS[0] &&
             RAW(raw)[1] == QS_LEGACY_MAGIC_BITS[1] &&
             RAW(raw)[2] == QS_LEGACY_MAGIC_BITS[2] &&
             RAW(raw)[3] == QS_LEGACY_MAGIC_BITS[3]) {
    ret[0] = "qs";
  } else {
    ret[0] = "base";
  }
  return ret;
}
