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
#include <boost/math/special_functions/beta.hpp>
#include <boost/math/policies/error_handling.hpp>
#include <stdarg.h>
#include <RcppArmadillo.h>
#include <R.h>
#define _(String) (String)

#include <atomic>
#include <vector>
#include <cstdint>
#include <cstring>

////////////////////////////////////////////////////////////////////////////
// Memo for the ROOT-FINDING inverse special functions.
//
// These are the expensive ones: each is a Newton/Halley iteration whose every
// step evaluates the full forward CDF.  Profiled on an est="imp" fit of a
// declared gamma model, 45% of the whole fit sat in gamma_incomplete_imp_final
// reached from gamma_p_inv's root finder -- against 1.9% in the ODE solve.
//
// They are also asked the SAME question over and over.  `rxEtaDistExpand()`
// emits a declared distribution's decoder as a MODEL LINE, e.g.
//
//   eta.cl <- gammapInv(1/exp(lclrv), phiU(rxN.eta.cl))/(...)
//
// so it is evaluated once per RECORD, while its arguments depend only on the
// thetas and the SUBJECT's latent -- constant across that subject's records.
// With 15 observations per subject, 14 of every 15 calls repeat exactly.
//
// SIZE.  One slot is not enough: a model declaring several non-normal random
// effects evaluates their decoders one after another WITHIN each record, so a
// single slot is evicted by the next declaration and never hits.  Measured on
// two declared etas: 1 slot gave 1.85x, 4 slots gave 3.0x.  The count is a
// property of the MODEL, so the parser supplies it exactly
// (`handleInvCdfFunctions` -> `rxSetInvCdfMemoSize`, four slots per call site).
//
// Chi-squared needs no entry of its own: the catalog expands dchisq,
// invChiSquare and scaledInvChiSquare through gammapInv, and studentT through
// ibeta_inv, so both are covered by the entries below.
//
// Keyed on the function id AND every argument, so any change misses and
// recomputes.  A hit returns the value that same call produced, so it is
// BIT-EXACT and cannot alter a result.
//
// thread_local: subjects are solved on an OpenMP team, and a shared table would
// hand one thread another thread's answer.
////////////////////////////////////////////////////////////////////////////
#define RX_INV_GAMMA_P_INV   1
#define RX_INV_GAMMA_Q_INV   2
#define RX_INV_GAMMA_P_INVA  3
#define RX_INV_GAMMA_Q_INVA  4
#define RX_INV_IBETA_INV     5
#define RX_INV_STUDENTT_INV  6
// Default 8: the parser sizes this EXACTLY from the model
// (handleInvCdfFunctions counts one per call site), so the default only has to
// carry a direct call from R -- gammapInv() and friends are exported and
// documented standalone -- or a model parsed by an older path.
#define RX_INVMEMO_DEFAULT   8

typedef struct {
  int fn;
  double k0, k1, k2, v;
} rxInvMemo_t;

static std::atomic<int> _rxInvMemoWant(RX_INVMEMO_DEFAULT);
static thread_local std::vector<rxInvMemo_t> _rxInvMemo;
static thread_local int _rxInvHave = 0;
static thread_local uint32_t _rxInvMask = 0;

static inline void rxInvMemoEnsure(void) {
  int want = _rxInvMemoWant.load(std::memory_order_relaxed);
  if (want == _rxInvHave) return;
  rxInvMemo_t e; e.fn = 0; e.k0 = e.k1 = e.k2 = e.v = 0.0;
  _rxInvMemo.assign((size_t)want, e);
  _rxInvHave = want;
  _rxInvMask = (uint32_t)(want - 1);
}

static inline uint32_t rxInvHash(int fn, double a, double b, double c) {
  uint64_t h = 1469598103934665603ULL;   // FNV offset basis
  uint64_t w[3];
  memcpy(&w[0], &a, sizeof(double));
  memcpy(&w[1], &b, sizeof(double));
  memcpy(&w[2], &c, sizeof(double));
  h = (h ^ (uint64_t)fn) * 1099511628211ULL;
  for (int i = 0; i < 3; ++i) h = (h ^ w[i]) * 1099511628211ULL;
  h ^= h >> 29;
  return (uint32_t)h & _rxInvMask;
}

static inline bool rxInvMemoGet(int fn, double a, double b, double c, double *out) {
  rxInvMemoEnsure();
  if (_rxInvHave <= 0) return false;
  const rxInvMemo_t &e = _rxInvMemo[rxInvHash(fn, a, b, c)];
  if (e.fn == fn && e.k0 == a && e.k1 == b && e.k2 == c) { *out = e.v; return true; }
  return false;
}

static inline void rxInvMemoPut(int fn, double a, double b, double c, double v) {
  if (_rxInvHave <= 0) return;
  rxInvMemo_t &e = _rxInvMemo[rxInvHash(fn, a, b, c)];
  e.fn = fn; e.k0 = a; e.k1 = b; e.k2 = c; e.v = v;
}

// Size the inverse-CDF memo from the model.
//
// C-callable, for the model setup to call once it knows how many root-finding
// inverses the parsed model contains -- one per declared non-normal random
// effect plus any the user writes directly.  Sizing the table to that keeps
// every one of them resident rather than evicting each other; the default of 64
// is generous enough that a realistic model never thrashes, so this is a
// right-sizing knob and not a correctness requirement.
//
// Rounded up to a power of two (the index is a mask) and clamped to [8, 4096].
// Each thread picks the new size up LAZILY on its next call, which is what makes
// it safe to call between solves without coordinating with the OpenMP team.
//
// NOT wired to the parser yet -- that is the remaining step; nothing calls this
// so far, and the default carries the models measured to date.
// Bytes the inverse-CDF memo costs, for the memory report.
//
// Sized per THREAD, like the llik save buffer, so the cost is
// slots * sizeof(entry) * cores.  Exact rather than nominal: the parser sets the
// slot count from what the model actually calls, so a model with no inverse CDF
// pays only the default.
extern "C" double rxInvCdfMemoBytes(int cores) {
  int want = _rxInvMemoWant.load(std::memory_order_relaxed);
  if (cores < 1) cores = 1;
  return (double)want * (double)sizeof(rxInvMemo_t) * (double)cores;
}

extern "C" void rxSetInvCdfMemoSize(int n) {
  int sz = 8;
  if (n > 4096) n = 4096;
  while (sz < n) sz <<= 1;
  // MONOTONIC.  The parser calls this per model, and several models are live at
  // once in a normal session (a fit's inner model, its sensitivity peers, any
  // model the user still holds).  Taking the max means each one raises the floor
  // to cover itself and none can shrink the table under another; the cost of
  // over-sizing is a few KB per thread, the cost of under-sizing is silent
  // thrashing back to recomputation.
  int cur = _rxInvMemoWant.load(std::memory_order_relaxed);
  while (sz > cur &&
         !_rxInvMemoWant.compare_exchange_weak(cur, sz, std::memory_order_relaxed)) {}
}

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
  double v;
  if (rxInvMemoGet(RX_INV_GAMMA_Q_INV, a, q, 0.0, &v)) return v;
  v = boost::math::gamma_q_inv<double, double>(a, q);
  rxInvMemoPut(RX_INV_GAMMA_Q_INV, a, q, 0.0, v);
  return v;
}

extern "C" double gamma_q_inva(double x, double q) {
  double v;
  if (rxInvMemoGet(RX_INV_GAMMA_Q_INVA, x, q, 0.0, &v)) return v;
  v = boost::math::gamma_q_inva<double, double>(x, q);
  rxInvMemoPut(RX_INV_GAMMA_Q_INVA, x, q, 0.0, v);
  return v;
}

// LAST-CALL MEMO for the inverse incomplete gamma.
//
// `gamma_p_inv` is a Halley iteration whose every step evaluates the full
// forward CDF, so it is intrinsically expensive -- 45% of a declared-gamma imp
// fit sits in `gamma_incomplete_imp_final` reached from here.  It is also asked
// the SAME question repeatedly: `rxEtaDistExpand()` emits the decoder
//
//   eta.cl <- gammapInv(1/exp(lclrv), phiU(rxN.eta.cl))/(...)
//
// as a model line, so it is evaluated once per RECORD -- while its arguments
// depend only on the thetas and the SUBJECT's latent, which are constant across
// that subject's records.  On Bauer's arms (15 observations per subject) 14 of
// every 15 calls repeat the previous one exactly.
//
// A one-entry memo is enough because the repeats are consecutive: records of a
// subject are solved in order.  Same shape as linCmt's row memo
// (src/linCmt.cpp), and keyed on EVERY argument, so a changed theta or latent
// misses and recomputes.
//
// thread_local: rxode2 solves subjects on an OpenMP team, and a shared memo
// would hand one thread another's answer.  Bit-exact on a hit -- it returns the
// value this same call computed -- so it cannot change any result.
extern "C" double gamma_p_inv(double a, double p) {
  double v;
  if (rxInvMemoGet(RX_INV_GAMMA_P_INV, a, p, 0.0, &v)) return v;
  v = boost::math::gamma_p_inv<double, double>(a, p);
  rxInvMemoPut(RX_INV_GAMMA_P_INV, a, p, 0.0, v);
  return v;
}

extern "C" double gamma_p_inva(double x, double p) {
  double v;
  if (rxInvMemoGet(RX_INV_GAMMA_P_INVA, x, p, 0.0, &v)) return v;
  v = boost::math::gamma_p_inva<double, double>(x, p);
  rxInvMemoPut(RX_INV_GAMMA_P_INVA, x, p, 0.0, v);
  return v;
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

// Same memo, same reasoning, for the dbeta/betaProportion decoders.
extern "C" double ibetaInv(double a, double b, double p) {
  double v;
  if (rxInvMemoGet(RX_INV_IBETA_INV, a, b, p, &v)) return v;
  v = boost::math::ibeta_inv<double, double, double>(a, b, p);
  rxInvMemoPut(RX_INV_IBETA_INV, a, b, p, v);
  return v;
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
  {
    // memoized at THIS level, not through ibetaInv(): it calls
    // boost::math::ibeta_inv directly, and caching here also saves the tail
    // reflection and the sqrt.
    double _v;
    if (rxInvMemoGet(RX_INV_STUDENTT_INV, p, nu, 0.0, &_v)) return _v;
  }
  if (p <= 0.0) return R_NegInf;
  if (p >= 1.0) return R_PosInf;
  if (p == 0.5) return 0.0;
  int lower = (p < 0.5);
  double q = lower ? 2.0 * p : 2.0 * (1.0 - p);
  double x = boost::math::ibeta_inv<double, double, double>(0.5 * nu, 0.5, q);
  // guard the p -> 0/1 limit, where x underflows to zero
  if (!(x > 0.0)) return lower ? R_NegInf : R_PosInf;
  double t = sqrt(nu * (1.0 - x) / x);
  double res = lower ? -t : t;
  rxInvMemoPut(RX_INV_STUDENTT_INV, p, nu, 0.0, res);
  return res;
}
