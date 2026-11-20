// Shape transforms for random effects (eta), usable in model blocks.
//
// Each keeps eta = 0 at 0 and has a shape value giving the identity, so the
// shape can be estimated through it.  The derivative rules live in R/d.R.
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#include <R.h>
#include <Rmath.h>
#include <cmath>

// n-th derivative in z of exprel(z) = (exp(z)-1)/z = int_0^1 exp(z*t) dt,
// ie I_n(z) = int_0^1 t^n exp(z*t) dt.  Forward recurrence
// I_n = (exp(z) - n*I_{n-1})/z is stable when |z| > n/2; otherwise the
// series sum_k z^k/(k! (n+k+1)), which has no cancellation there.
extern "C" double exprelD(double z, double nd) {
  if (ISNAN(z) || ISNAN(nd)) return NA_REAL;
  if (nd < 0 || nd != std::floor(nd)) return R_NaN;
  int n = (int)nd;
  double az = std::fabs(z);
  if (az > 1.0 && az > 0.5 * n) {
    double ez = std::exp(z);
    double cur = std::expm1(z) / z;
    for (int j = 1; j <= n; ++j) {
      cur = (ez - j * cur) / z;
    }
    return cur;
  }
  double term = 1.0, sum = 1.0 / (n + 1.0);
  for (int k = 1; k < 200; ++k) {
    term *= z / k;
    double add = term / (n + k + 1.0);
    sum += add;
    if (std::fabs(add) <= 1e-17 * std::fabs(sum)) break;
  }
  return sum;
}

// Box-Cox of exp(eta) (Petersson et al. 2009, Eq. 2; Manly's transform):
// (exp(eta)^lambda - 1)/lambda = eta*exprel(lambda*eta), eta at lambda = 0
extern "C" double etaBoxCox(double eta, double lambda) {
  double z = lambda * eta;
  if (z == 0.0) return eta;
  return eta * (std::expm1(z) / z);
}

// John-Draper modulus transform:
// sign(eta)*((|eta|+1)^lambda - 1)/lambda, the identity at lambda = 1
extern "C" double etaJohnDraper(double eta, double lambda) {
  double a = std::log1p(std::fabs(eta));
  double r = etaBoxCox(a, lambda);
  return eta < 0 ? -r : r;
}
