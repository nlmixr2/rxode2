/* dvode_support.c
 *
 * C-side support routines for dvode.f (DVODE BDF solver from deSolve).
 *
 * DVODE calls deSolve-specific Fortran wrappers (rprintf, rprintfd2,
 * rprintfi1, rprintfi2, rprintfdi, rprintfd2) that format diagnostic
 * messages via C printf-style calls.  Since these are diagnostic/warning
 * messages only (not used for solver correctness), we implement them as
 * simple Rprintf wrappers that print the message and the numeric values.
 *
 * Also provides dvnorm_ (weighted RMS norm) and d1mach_ (machine constants)
 * which DVODE references as external functions.
 *
 * Fortran calls C using the FC convention: character arguments are passed
 * as (char*, len_t) pairs.  The dvode.f code null-terminates all strings
 * with // char(0) before passing them, so we can safely use strlen().
 */

#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <float.h>
#include <string.h>

/* dvnorm_ removed: now provided by dlsode.f (DVNORM from ODEPACK opkda1). */

/* ---- d1mach: machine constants (standard ODEPACK interface) ----------
 * Fortran signature: DOUBLE PRECISION FUNCTION D1MACH(I)
 *   I=1: smallest positive double
 *   I=2: largest finite double
 *   I=3: smallest x such that 1+x > 1  (= eps/2 in IEEE 754)
 *   I=4: unit roundoff  (machine epsilon = b^(1-t))
 *   I=5: log10(base) = log10(2)
 */
double F77_NAME(d1mach)(int *i) {
  switch (*i) {
  case 1: return DBL_MIN;
  case 2: return DBL_MAX;
  case 3: return DBL_EPSILON / 2.0;
  case 4: return DBL_EPSILON;
  case 5: return 0.30102999566398119521;   /* log10(2) */
  default: return 0.0;
  }
}

/* ---- deSolve print helpers -----------------------------------------
 * DVODE uses these to emit diagnostic messages.  We just forward to
 * Rprintf; correctness is not affected.
 *
 * All character arguments arrive null-terminated because dvode.f uses
 * the  msg // char(0)  pattern before every call.
 */

/* rprintf(msg) -- print a plain string */
void F77_NAME(rprintf)(const char *msg
#ifdef USE_FC_LEN_T
                       , size_t msg_len
#endif
                       ) {
  (void)msg_len;
  Rprintf("%s", msg);
}

/* rprintfd2(msg, r1, r2) -- message with two doubles */
void F77_NAME(rprintfd2)(const char *msg, double *r1, double *r2
#ifdef USE_FC_LEN_T
                          , size_t msg_len
#endif
                          ) {
  (void)msg_len;
  Rprintf(msg, *r1, *r2);
}

/* rprintfi1(msg, i1) -- message with one integer */
void F77_NAME(rprintfi1)(const char *msg, int *i1
#ifdef USE_FC_LEN_T
                          , size_t msg_len
#endif
                          ) {
  (void)msg_len;
  Rprintf(msg, *i1);
}

/* rprintfi2(msg, i1, i2) -- message with two integers */
void F77_NAME(rprintfi2)(const char *msg, int *i1, int *i2
#ifdef USE_FC_LEN_T
                          , size_t msg_len
#endif
                          ) {
  (void)msg_len;
  Rprintf(msg, *i1, *i2);
}

/* rprintfdi(msg, d1, i1) -- message with one double, one integer */
void F77_NAME(rprintfdi)(const char *msg, double *d1, int *i1
#ifdef USE_FC_LEN_T
                          , size_t msg_len
#endif
                          ) {
  (void)msg_len;
  Rprintf(msg, *d1, *i1);
}

/* rprintfd1(msg, d1) -- printf-style message with one double */
void F77_NAME(rprintfd1)(const char *msg, double *d1
#ifdef USE_FC_LEN_T
                          , size_t msg_len
#endif
                          ) {
  (void)msg_len;
  Rprintf(msg, *d1);
}

/* rprintfid(msg, i1, d1) -- printf-style message with integer then double */
void F77_NAME(rprintfid)(const char *msg, int *i1, double *d1
#ifdef USE_FC_LEN_T
                          , size_t msg_len
#endif
                          ) {
  (void)msg_len;
  Rprintf(msg, *i1, *d1);
}

/* rprintd1(msg, d1) -- print label + one double (R dblepr style) */
void F77_NAME(rprintd1)(const char *msg, double *d1
#ifdef USE_FC_LEN_T
                         , size_t msg_len
#endif
                         ) {
  (void)msg_len;
  Rprintf("%s %g\n", msg, *d1);
}

/* rprintd2(msg, d1, d2) -- print label + two doubles (R dblepr style) */
void F77_NAME(rprintd2)(const char *msg, double *d1, double *d2
#ifdef USE_FC_LEN_T
                         , size_t msg_len
#endif
                         ) {
  (void)msg_len;
  Rprintf("%s %g %g\n", msg, *d1, *d2);
}

/* rprinti1(msg, i1) -- print label + one integer (R intpr style) */
void F77_NAME(rprinti1)(const char *msg, int *i1
#ifdef USE_FC_LEN_T
                         , size_t msg_len
#endif
                         ) {
  (void)msg_len;
  Rprintf("%s %d\n", msg, *i1);
}

/* ---- dlsode_errwd: full XERRWD for DLSODE (renamed to avoid conflict) -----
 * Signature: DLSODE_ERRWD(MSG, NMES, NERR, LEVEL, NI, I1, I2, NR, R1, R2)
 * MSG is a character string (Fortran), NMES = length.
 * Prints the message plus optional integers/reals.
 */
void F77_NAME(dlsode_errwd)(const char *msg, int *nmes, int *nerr,
                             int *level, int *ni, int *i1, int *i2,
                             int *nr, double *r1, double *r2
#ifdef USE_FC_LEN_T
                             , size_t msg_len
#endif
                             ) {
  (void)nerr; (void)msg_len;
  int n = *nmes;
  /* Print message (trim trailing spaces) */
  while (n > 0 && msg[n-1] == ' ') n--;
  Rprintf("%.*s", n, msg);
  if (*ni >= 1) Rprintf("  I1=%d", *i1);
  if (*ni >= 2) Rprintf("  I2=%d", *i2);
  if (*nr >= 1) Rprintf("  R1=%g", *r1);
  if (*nr >= 2) Rprintf("  R2=%g", *r2);
  Rprintf("\n");
  if (*level >= 2) (Rf_error)("DLSODE fatal error (level=%d)", *level);
}
