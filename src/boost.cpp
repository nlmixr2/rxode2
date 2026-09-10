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
// Do NOT promote double -> long double inside boost's special functions.
//
// Boost's DEFAULT policy promotes, so every gamma_p/gamma_p_inv/ibeta call here
// computed in 80-bit long double and paid glibc's long-double transcendentals.
// Profiled on an est="imp" fit of a declared gamma model (`dist(cl) ~ dgamma`),
// 300 subjects x 15 obs, perf -F 199:
//
//   27.4%  __GI___powl_helper        (long double pow)
//   25.2%  __expl_finite             (long double exp)
//   11.3%  igamma_temme_large<long double>
//    8.4%  gamma_incomplete_imp_final<long double>
//    3.6%  __ieee754_logl
//     ...
//    0.4%  linCmtB                   (the actual ODE/solve work)
//
// i.e. ~85% of the fit was the incomplete gamma in long double, over half of it
// in powl/expl alone.  The inverse's root finder is instantiated on `double`,
// but the forward gamma_p it evaluates at every Halley step landed in the long
// double instantiation, so each iteration paid for precision nothing here asks
// for: these feed a random-effect quantile, not a tolerance-critical loop.
//
// Accuracy is checked against R's own pgamma/qgamma, which are an independent
// implementation -- see the ACC lines in the commit message.
#define BOOST_MATH_PROMOTE_DOUBLE_POLICY false
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
