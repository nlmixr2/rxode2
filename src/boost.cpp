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
