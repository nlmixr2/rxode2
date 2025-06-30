// modified from: https://raw.githubusercontent.com/berndbischl/BBmisc/eb18a51d30603b55bb791debd93d26c15db104f8/src/itostr.c
#include <string.h>
#include <math.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

static const char base36[37] = "0123456789abcdefghijklmnopqrstuvwxyz";

SEXP _rxode2_itostr(SEXP x, SEXP base) {
  const R_len_t n = Rf_length(x);
  const R_len_t b = INTEGER(base)[0];
  SEXP res = PROTECT(Rf_allocVector(STRSXP, n));

  const R_len_t buflen = ceil(log(exp2(64) / log(b)));
  char buffer[buflen + 1];
  buffer[buflen] = '\0';

  for (R_len_t i = 0; i < n; i++) {
    R_len_t offset = buflen;
    int xi = INTEGER(x)[i];
    do {
      buffer[--offset] = base36[xi % b];
    } while (xi /= b);

    SET_STRING_ELT(res, i, Rf_mkChar(&buffer[offset]));
  }

  UNPROTECT(1);
  return res;
}

static const char base26[27] = "abcdefghijklmnopqrstuvwxyz";

SEXP _rxode2_itoletter(SEXP x, SEXP base) {
  const R_len_t n = Rf_length(x);
  const R_len_t b = INTEGER(base)[0];
  SEXP res = PROTECT(Rf_allocVector(STRSXP, n));

  const R_len_t buflen = ceil(log(exp2(64) / log(b)));
  char buffer[buflen + 1];
  buffer[buflen] = '\0';

  for (R_len_t i = 0; i < n; i++) {
    R_len_t offset = buflen;
    int xi = INTEGER(x)[i];
    do {
      buffer[--offset] = base26[xi % b];
    } while (xi /= b);

    SET_STRING_ELT(res, i, Rf_mkChar(&buffer[offset]));
  }

  UNPROTECT(1);
  return res;
}
