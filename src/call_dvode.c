#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#include <float.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "dop853.h"
#define max(a, b) ((a) > (b) ? (a) : (b))
#define STRICT_R_HEADERS
#include <PreciseSums.h>
#include "../inst/include/rxode2.h"

//--------------------------------------------------------------------------

// These are now allocated via R structures in Rcpp.
extern void rxode2_ode_free(void){
}

void rxode2_ode_alloc(void){
}

char __mv[1000];
extern void rxode2_assign_fn_pointers_(const char *mv){
  snprintf(__mv, 1000, "%s", mv);
}

void rxAssignPtrC(SEXP obj);
int rxode2_current_fn_pointer_id_ = 0;
extern int rxode2_current_fn_pointer_id(void){
  return rxode2_current_fn_pointer_id_;
}
extern void rxode2_assign_fn_pointers(SEXP mv){
  int cur = INTEGER(VECTOR_ELT(mv, 13))[0];
  if (rxode2_current_fn_pointer_id_ != cur){
    rxAssignPtrC(mv);
    rxode2_current_fn_pointer_id_ = cur;
  }
}

SEXP rxModelVarsC(char *ptr);

extern SEXP rxode2_get_mv(void){
  return rxModelVarsC(__mv);
}

/* extern void rxode2_assign_rx(rx_solve *rx); */

extern void rxode2_assign_rx(rx_solve *rx);

rx_solve *getRxSolve_(void);
int *global_BadDose(unsigned int mx);
double *global_InfusionRate(unsigned int mx);

extern double rxode2_sum(double *input, int len){
  return PreciseSums_sum(input, len);
}

extern double rxode2_sumV(int n, ...){
  va_list valist;
  va_start(valist, n);
  double *p = R_Calloc(n, double);
  for (unsigned int i = (unsigned int)n; i--;){
    p[i] = va_arg(valist, double);
  }
  va_end(valist);
  double s = PreciseSums_sum(p, n);
  R_Free(p);
  return s;
}

extern double rxode2_sumV_r(double *p, long double *pld, int m, int type, int n, ...){
  va_list valist;
  va_start(valist, n);
  for (unsigned int i = (unsigned int)n; i--;){
    p[i] = va_arg(valist, double);
  }
  va_end(valist);
  return PreciseSums_sum_r(p, n, pld, m, type);
}

extern double rxode2_prod(double *input, int len){
  return PreciseSums_prod(input, len);
}

extern double rxode2_prodV(int n, ...){
  va_list valist;
  va_start(valist, n);
  double *p = R_Calloc(n, double);
  for (unsigned int i = (unsigned int)n; i--;){
    p[i] = va_arg(valist, double);
  }
  va_end(valist);
  double s = PreciseSums_prod(p, n);
  R_Free(p);
  return s;
}

extern double rxode2_prodV_r(double *input, double *p, int type, int n, ...){
  va_list valist;
  va_start(valist, n);
  for (unsigned int i = (unsigned int)n; i--;){
    input[i] = va_arg(valist, double);
  }
  va_end(valist);
  return PreciseSums_prod_r(input, p, n, type);
}
