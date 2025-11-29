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
  Rcpp::Function f = getRxFn("rxRawToC");
  return f(x);
}

//[[Rcpp::export]]
SEXP rxQr(SEXP const y) {
  Rcpp::Function f = getRxFn("rxDeserialize");
  return f(y);
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
  if (TYPEOF(raw) != RAWSXP) {
    Rcpp::stop("Expected a raw vector");
  }
  unsigned char QS2_MAGIC_BITS[] = {0x0B,0x0E,0x0A,0xC1};
  unsigned char QDATA_MAGIC_BITS[] = {0x0B,0x0E,0x0A,0xCD};
  unsigned char QS_LEGACY_MAGIC_BITS[] = {0x0B,0x0E,0x0A,0x0C};
  // Version-2 serialization first writes a header indicating the format (normally ‘X\n’ for an XDR
  // format binary save, but ‘A\n’, ASCII, and ‘B\n’, native word-order binary, can also occur)
  unsigned char BASE_MAGIC_BITSX[] = {0x58,0x0A};
  unsigned char BASE_MAGIC_BITSA[] = {0x41,0x0A};
  unsigned char BASE_MAGIC_BITSB[] = {0x42,0x0A};
  Rcpp::CharacterVector ret(1);
  ret[0] = "unknown";
  if (Rf_length(raw) < 2) {
  } else if ((RAW(raw)[0] == BASE_MAGIC_BITSX[0] &&
              RAW(raw)[1] == BASE_MAGIC_BITSX[1]) ||
             (RAW(raw)[0] == BASE_MAGIC_BITSA[0] &&
              RAW(raw)[1] == BASE_MAGIC_BITSA[1]) ||
             (RAW(raw)[0] == BASE_MAGIC_BITSB[0] &&
              RAW(raw)[1] == BASE_MAGIC_BITSB[1])) {
    ret[0] = "base";
  } else if (Rf_length(raw) < 4) {
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
  }
  return ret;
}
