#define STRICT_R_HEADERS
#include "../inst/include/rxode2.h"
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rmath.h>

double llikNorm(double x, double mu, double sigma) {
  // https://github.com/wch/r-source/blob/bd0c212e502c13439e01b864929534ab7aa830dc/src/nmath/dnorm.c
  
  // While it seems thread safe it has some warnings issued, which means it isn't in certain circumstances.
  if (sigma < 0) return R_NaN;
  // #define R_D__0	(log_p ? ML_NEGINF : 0.)
  if(!R_FINITE(sigma)) return R_NegInf;
  if(!R_FINITE(x) && mu == x) return R_NaN;/* x-mu is NaN */
  if (sigma == 0) {
    return (x == mu) ? R_PosInf : -R_NegInf;
  }
  double x2 = (x - mu) / sigma;
  return -(M_LN_SQRT_2PI + 0.5 * x2 * x2 + log(sigma));
}
