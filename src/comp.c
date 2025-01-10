#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>   /* dj: import intptr_t */
//#include "ode.h"
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rmath.h>
#include <R_ext/Lapack.h>
#include <R_ext/BLAS.h>
#ifndef FCONE
# define FCONE
#endif

#include "../inst/include/rxode2.h"
#include "comp.h"
#include "parTrans.h"

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

int comp1c(int ncmt,
           int trans,
           double *yp, // prior y
           double xp, // initial time point
           double xout, // final time point
           double *p,
           double *rate) {
  // no update for same time
  if (isSameTime(xout, xp)) return 1;
  lin_context_c_t lin;
  lin.ncmt = ncmt;
  lin.ka=lin.k10=
    lin.k12=lin.k21=
    lin.k13=lin.k31=lin.v=
    lin.dt=xout-xp;
  lin.rate = rate;
  if (!parTrans(&trans, &p[0], &p[1], &p[2], &p[3], &p[4], &p[5],
                &ncmt, &(lin.k10), &(lin.v), &(lin.k12),
                &(lin.k21), &(lin.k13), &(lin.k31))){
    return 0;
  }
  return comp1(yp, &lin);
}

// This is a R wrapper for comp1c
SEXP _rxode2_comp1c(SEXP prior, SEXP xpS, SEXP xoutS,
                    SEXP rateS, SEXP transS, SEXP parS) {
  SEXP retS = PROTECT(Rf_allocVector(INTSXP, 1));
  int *ret = INTEGER(retS);
  ret[0] = 0;
  int ncmt = INTEGER(VECTOR_ELT(transS, 1))[0];
  int trans = INTEGER(VECTOR_ELT(transS, 2))[0];
  comp1c(ncmt, trans, REAL(prior), // prior y
         REAL(xpS)[0], // initial time point
         REAL(xoutS)[0], // final time point
         REAL(rateS),
         REAL(rateS));
  UNPROTECT(1);
  return retS;
}
