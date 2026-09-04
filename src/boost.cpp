// Ignore error so that boost doesn't abort
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define BOOST_MATH_DOMAIN_ERROR_POLICY ignore_error
#define BOOST_MATH_POLE_ERROR_POLICY ignore_error
#define BOOST_MATH_OVERFLOW_ERROR_POLICY ignore_error
#define BOOST_MATH_UNDERFLOW_ERROR_POLICY ignore_error
#define BOOST_MATH_DENORM_ERROR_POLICY ignore_error
#define BOOST_MATH_EVALUATION_ERROR_POLICY ignore_error
#define BOOST_MATH_INDETERMINATE_RESULT_ERROR_POLICY ignore_error
#define STRICT_R_HEADERS
// Include boost and R
#include <boost/math/special_functions/gamma.hpp>
#include <boost/math/special_functions/beta.hpp>
#include <boost/math/policies/error_handling.hpp>
#include <stdarg.h>
#include <RcppArmadillo.h>
#include <R.h>
#define _(String) (String)

extern "C" double gamma_p(double a, double z) {
  return boost::math::gamma_p<double, double>(a, z);
}

extern "C" double gamma_q(double a, double z) {
  return boost::math::gamma_q<double, double>(a, z);
}

extern "C" double tgamma_lower(double a, double z) {
  return boost::math::tgamma_lower<double, double>(a, z);
}

extern "C" double tgamma_upper(double a, double z) {
  return boost::math::tgamma<double, double>(a, z);
}

extern "C" double gamma_p_derivative(double a, double x) {
  return boost::math::gamma_p_derivative<double, double>(a, x);
}

extern "C" double gamma_q_inv(double a, double q) {
  return boost::math::gamma_q_inv<double, double>(a, q);
}

extern "C" double gamma_q_inva(double x, double q) {
  return boost::math::gamma_q_inva<double, double>(x, q);
}

extern "C" double gamma_p_inv(double a, double p) {
  return boost::math::gamma_p_inv<double, double>(a, p);
}

extern "C" double gamma_p_inva(double x, double p) {
  return boost::math::gamma_p_inva<double, double>(x, p);
}

////////////////////////////////////////////////////////////////////////////
// Regularized incomplete beta and its inverse.
//
// `ibetaInv()` is qbeta(); it is also what the Student t quantile is
// built from below.  Together with `gammapInv()` above these are the two
// non-elementary inverse CDFs a declared non-normal random effect needs
// (see `rxEtaDistExpand()`): every other family in `lotriEtaDists()` has
// an elementary quantile function that the model text can spell out.

extern "C" double ibeta_(double a, double b, double x) {
  return boost::math::ibeta<double, double, double>(a, b, x);
}

extern "C" double ibetaDer(double a, double b, double x) {
  return boost::math::ibeta_derivative<double, double, double>(a, b, x);
}

extern "C" double ibetaInv(double a, double b, double p) {
  return boost::math::ibeta_inv<double, double, double>(a, b, p);
}

////////////////////////////////////////////////////////////////////////////
// Partial derivatives of the incomplete gamma/beta with respect to their
// SHAPE parameters.
//
// These have no elementary closed form and boost does not supply them, so
// they are central differences with one Richardson extrapolation: the
// leading O(h^2) error term cancels, leaving roughly O(h^4) ~ 1e-13
// relative on the integrand's own accuracy (~1e-15).  That is far better
// than the single sided finite difference rxode2's symbolic
// differentiation would otherwise fall back to, and -- the reason these
// exist at all -- it makes the derivative table COMPLETE, so a model
// using these functions never silently degrades to a numeric derivative
// without saying so.
//
// They are only reached by the analytic outer gradient and analytic
// covariance; the derivative with respect to the probability argument,
// which is the one the inner (eta) problem needs, is exact and
// elementary -- 1/density at the quantile -- and is written directly into
// the derivative table.

// step scaled to the argument, floored so a shape near zero still moves
static inline double rxShapeStep(double a) {
  double h = 1.0e-4 * (fabs(a) > 1.0 ? fabs(a) : 1.0);
  // make h exactly representable so (a+h)-(a-h) is exactly 2h
  volatile double tmp = a + h;
  h = tmp - a;
  return h;
}

// d1 is the central difference at step h, d2 the one at step h/2 (whose
// denominator is therefore 2*(h/2) = h); (4*d2 - d1)/3 cancels the shared
// O(h^2) term
#define RX_RICHARDSON(EXPR_PLUS, EXPR_MINUS, EXPR_PLUS2, EXPR_MINUS2, H)  \
  do {                                                                    \
    double d1 = ((EXPR_PLUS) - (EXPR_MINUS)) / (2.0 * (H));               \
    double d2 = ((EXPR_PLUS2) - (EXPR_MINUS2)) / (H);                     \
    return (4.0 * d2 - d1) / 3.0;                                         \
  } while (0)

extern "C" double gammapDera(double a, double z) {
  double h = rxShapeStep(a);
  RX_RICHARDSON(gamma_p(a + h, z), gamma_p(a - h, z),
                gamma_p(a + 0.5 * h, z), gamma_p(a - 0.5 * h, z), h);
}

extern "C" double ibetaDera(double a, double b, double x) {
  double h = rxShapeStep(a);
  RX_RICHARDSON(ibeta_(a + h, b, x), ibeta_(a - h, b, x),
                ibeta_(a + 0.5 * h, b, x), ibeta_(a - 0.5 * h, b, x), h);
}

extern "C" double ibetaDerb(double a, double b, double x) {
  double h = rxShapeStep(b);
  RX_RICHARDSON(ibeta_(a, b + h, x), ibeta_(a, b - h, x),
                ibeta_(a, b + 0.5 * h, x), ibeta_(a, b - 0.5 * h, x), h);
}

////////////////////////////////////////////////////////////////////////////
// Student t: density, CDF, quantile and the CDF's derivative in nu.
//
// Written on the incomplete beta rather than through boost's
// `students_t` distribution so that all four share one code path and one
// error policy, and so `studentTInv()` is exactly the inverse of
// `studentTCdf()` at the same tolerance.
//
//   P(|T| > t) = I_{nu/(nu + t^2)}(nu/2, 1/2)

extern "C" double studentTDen(double x, double nu) {
  return exp(lgamma(0.5 * (nu + 1.0)) - lgamma(0.5 * nu) -
             0.5 * log(nu * M_PI) -
             0.5 * (nu + 1.0) * log1p(x * x / nu));
}

extern "C" double studentTCdf(double x, double nu) {
  double tail = 0.5 * ibeta_(0.5 * nu, 0.5, nu / (nu + x * x));
  return (x <= 0.0) ? tail : 1.0 - tail;
}

extern "C" double studentTCdfDnu(double x, double nu) {
  double h = rxShapeStep(nu);
  RX_RICHARDSON(studentTCdf(x, nu + h), studentTCdf(x, nu - h),
                studentTCdf(x, nu + 0.5 * h), studentTCdf(x, nu - 0.5 * h), h);
}

extern "C" double studentTInv(double p, double nu) {
  if (ISNAN(p) || ISNAN(nu)) return NA_REAL;
  if (p <= 0.0) return R_NegInf;
  if (p >= 1.0) return R_PosInf;
  if (p == 0.5) return 0.0;
  int lower = (p < 0.5);
  double q = lower ? 2.0 * p : 2.0 * (1.0 - p);
  double x = boost::math::ibeta_inv<double, double, double>(0.5 * nu, 0.5, q);
  // guard the p -> 0/1 limit, where x underflows to zero
  if (!(x > 0.0)) return lower ? R_NegInf : R_PosInf;
  double t = sqrt(nu * (1.0 - x) / x);
  return lower ? -t : t;
}
