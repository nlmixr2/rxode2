/*
 * seCache.cpp -- see seCache.h.
 */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define STRICT_R_HEADERS
#include <R.h>
#include <Rinternals.h>
#include "rxProtect.h"
#include <string>
#include <unordered_map>
#include <vector>
#include "seCache.h"

/* the decline reasons in seFromSEarena.h (seWhy); 0 is "accepted" */
#define SE_NWHY 14 /* SE_WHY_N */

namespace {

struct seDirCache {
  std::unordered_map<std::string, std::string> map;
};

seDirCache seC[2];
size_t seBytes = 0;
std::vector<int> seKey;        /* the stamp the cached entries belong to */
int seEnabled = 1;
double seCapBytes = 256.0 * 1024.0 * 1024.0;

/* per direction: [0] accepted by C, [1..] declined for that reason */
double seStats[2][SE_NWHY];
double seHits[2] = {0.0, 0.0};

void seClear() {
  seC[0].map.clear();
  seC[1].map.clear();
  seBytes = 0;
}

} // namespace

extern "C" int seCacheBegin(SEXP stamp) {
  if (TYPEOF(stamp) != INTSXP || Rf_xlength(stamp) < 2) return 0;
  R_xlen_t n = Rf_xlength(stamp);
  const int *s = INTEGER(stamp);
  /* the last two entries configure; everything before them is the key */
  seEnabled = s[n - 2] != 0;
  if (s[n - 1] != NA_INTEGER && s[n - 1] > 0) {
    seCapBytes = (double) s[n - 1] * 1024.0 * 1024.0;
  }
  std::vector<int> key(s, s + (n - 2));
  if (key != seKey) {
    seClear();
    seKey = key;
  }
  if (!seEnabled) seClear();
  return seEnabled;
}

extern "C" const char *seCacheGet(int dir, const char *key) {
  if (!seEnabled) return NULL;
  auto &m = seC[dir].map;
  auto it = m.find(key);
  if (it == m.end()) return NULL;
  return it->second.c_str();
}

extern "C" void seCachePut(int dir, const char *key, const char *val) {
  if (!seEnabled) return;
  size_t add = strlen(key) + strlen(val) + 64;
  if ((double)(seBytes + add) > seCapBytes) seClear();
  if ((double) add > seCapBytes) return;
  auto res = seC[dir].map.emplace(key, val);
  if (res.second) seBytes += add;
}

extern "C" void seStatsAdd(int dir, int why) {
  if (why < 0 || why >= SE_NWHY) why = SE_NWHY - 1;
  seStats[dir][why] += 1.0;
}

extern "C" void seStatsHit(int dir) {
  seHits[dir] += 1.0;
}

/* .Call: store a translation the R walker produced */
extern "C" SEXP _rxode2_seCachePutR(SEXP dirS, SEXP keyS, SEXP valS) {
  int dir = Rf_asInteger(dirS);
  if (dir != SE_DIR_FROM && dir != SE_DIR_TO) return R_NilValue;
  if (TYPEOF(keyS) != STRSXP || TYPEOF(valS) != STRSXP) return R_NilValue;
  R_xlen_t n = Rf_xlength(keyS), i;
  if (Rf_xlength(valS) != n) return R_NilValue;
  for (i = 0; i < n; i++) {
    SEXP k = STRING_ELT(keyS, i), v = STRING_ELT(valS, i);
    if (k == NA_STRING || v == NA_STRING) continue;
    seCachePut(dir, CHAR(k), CHAR(v));
  }
  return R_NilValue;
}

/* .Call: forget everything (a new translation session) */
extern "C" SEXP _rxode2_seCacheResetR(void) {
  seClear();
  return R_NilValue;
}

/* .Call: list(fromSE=, toSE=) decline counts by reason, cache hits, size;
   reset = TRUE zeroes the counters after reading them */
extern "C" SEXP _rxode2_seStatsR(SEXP resetS) {
  rxProtect rx_protect;
  SEXP ret = rx_protect.protect(Rf_allocVector(VECSXP, 6));
  SEXP nm = rx_protect.protect(Rf_allocVector(STRSXP, 6));
  for (int d = 0; d < 2; d++) {
    SEXP v = rx_protect.protect(Rf_allocVector(REALSXP, SE_NWHY));
    for (int i = 0; i < SE_NWHY; i++) REAL(v)[i] = seStats[d][i];
    SET_VECTOR_ELT(ret, d, v);
    rx_protect.unprotect(1);
  }
  SET_VECTOR_ELT(ret, 2, Rf_ScalarReal(seHits[0]));
  SET_VECTOR_ELT(ret, 3, Rf_ScalarReal(seHits[1]));
  SET_VECTOR_ELT(ret, 4, Rf_ScalarReal((double) seBytes));
  SET_VECTOR_ELT(ret, 5, Rf_ScalarReal((double)(seC[0].map.size() + seC[1].map.size())));
  SET_STRING_ELT(nm, 0, Rf_mkChar("fromSE"));
  SET_STRING_ELT(nm, 1, Rf_mkChar("toSE"));
  SET_STRING_ELT(nm, 2, Rf_mkChar("fromSEhits"));
  SET_STRING_ELT(nm, 3, Rf_mkChar("toSEhits"));
  SET_STRING_ELT(nm, 4, Rf_mkChar("bytes"));
  SET_STRING_ELT(nm, 5, Rf_mkChar("entries"));
  Rf_setAttrib(ret, R_NamesSymbol, nm);
  if (Rf_asLogical(resetS) == TRUE) {
    for (int d = 0; d < 2; d++) {
      for (int i = 0; i < SE_NWHY; i++) seStats[d][i] = 0.0;
      seHits[d] = 0.0;
    }
  }
  return ret;
}
