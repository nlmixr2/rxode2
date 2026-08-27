#ifndef __LINCMTDUALN_H__
#define __LINCMTDUALN_H__
// Multi-directional forward-mode dual number for the linCmt() kernels.
//
// stan::math::fvar<double> carries ONE tangent, so a p-direction Jacobian
// costs p seeded passes through the closed form: every exp(), sqrt(),
// division and eigen-decomposition is recomputed p times when only the
// tangent differs.  dualN<N> carries N tangents alongside one value, so a
// single pass computes the primal once and propagates every direction
// through it.  The kernels in linCmt.h are already template <typename T>,
// so this is a re-instantiation, not a rewrite, and it reaches the
// steady-state kernels that have no constants/tail factorization.
//
// Every rule below reproduces the OPERATION ORDER of the corresponding
// stan/math/fwd rule (referenced per function), so a dualN result is
// bitwise identical to the fvar result it replaces.  Changing an order
// here silently changes solve output in the last digits.
#include <cmath>
#include <cfloat>
#include <limits>
#include <type_traits>

// The widest a linCmt() model can request: 2*ncmt + oral0 with ncmt <= 3.
#define RX_LINCMT_DUAL_MAX 7

namespace stan {
  namespace math {

    template <int N>
    struct dualN {
      double v_;
      double d_[N];

      inline dualN() : v_(0.0) {
        for (int i = 0; i < N; ++i) d_[i] = 0.0;
      }
      // Non-explicit on purpose: the kernels write `T x = 0.0;` and mix
      // doubles into T expressions throughout.
      inline dualN(double v) : v_(v) {
        for (int i = 0; i < N; ++i) d_[i] = 0.0;
      }
      inline double val() const { return v_; }
      inline double d(int i) const { return d_[i]; }
      // Seeded unit tangent in direction j (the analogue of fvar(v, 1.0)).
      static inline dualN<N> seed(double v, int j) {
        dualN<N> r(v);
        r.d_[j] = 1.0;
        return r;
      }
    };

    template <typename T> struct isDualN { enum { value = 0 }; };
    template <int N> struct isDualN<dualN<N> > { enum { value = 1 }; };

    // ---- addition (fwd/core/operator_addition.hpp) ------------------------
    template <int N>
    inline dualN<N> operator+(const dualN<N>& a, const dualN<N>& b) {
      dualN<N> r(a.v_ + b.v_);
      for (int i = 0; i < N; ++i) r.d_[i] = a.d_[i] + b.d_[i];
      return r;
    }
    template <int N>
    inline dualN<N> operator+(double a, const dualN<N>& b) {
      dualN<N> r(a + b.v_);
      for (int i = 0; i < N; ++i) r.d_[i] = b.d_[i];
      return r;
    }
    template <int N>
    inline dualN<N> operator+(const dualN<N>& a, double b) {
      dualN<N> r(a.v_ + b);
      for (int i = 0; i < N; ++i) r.d_[i] = a.d_[i];
      return r;
    }

    // ---- subtraction (fwd/core/operator_subtraction.hpp) ------------------
    template <int N>
    inline dualN<N> operator-(const dualN<N>& a, const dualN<N>& b) {
      dualN<N> r(a.v_ - b.v_);
      for (int i = 0; i < N; ++i) r.d_[i] = a.d_[i] - b.d_[i];
      return r;
    }
    template <int N>
    inline dualN<N> operator-(double a, const dualN<N>& b) {
      dualN<N> r(a - b.v_);
      for (int i = 0; i < N; ++i) r.d_[i] = -b.d_[i];
      return r;
    }
    template <int N>
    inline dualN<N> operator-(const dualN<N>& a, double b) {
      dualN<N> r(a.v_ - b);
      for (int i = 0; i < N; ++i) r.d_[i] = a.d_[i];
      return r;
    }
    // fwd/core/operator_unary_minus.hpp
    template <int N>
    inline dualN<N> operator-(const dualN<N>& a) {
      dualN<N> r(-a.v_);
      for (int i = 0; i < N; ++i) r.d_[i] = -a.d_[i];
      return r;
    }
    template <int N>
    inline dualN<N> operator+(const dualN<N>& a) { return a; }

    // ---- multiplication (fwd/core/operator_multiplication.hpp) ------------
    template <int N>
    inline dualN<N> operator*(const dualN<N>& a, const dualN<N>& b) {
      dualN<N> r(a.v_ * b.v_);
      for (int i = 0; i < N; ++i) r.d_[i] = a.d_[i] * b.v_ + a.v_ * b.d_[i];
      return r;
    }
    template <int N>
    inline dualN<N> operator*(double a, const dualN<N>& b) {
      dualN<N> r(a * b.v_);
      for (int i = 0; i < N; ++i) r.d_[i] = a * b.d_[i];
      return r;
    }
    template <int N>
    inline dualN<N> operator*(const dualN<N>& a, double b) {
      dualN<N> r(a.v_ * b);
      for (int i = 0; i < N; ++i) r.d_[i] = a.d_[i] * b;
      return r;
    }

    // The cross term a'b - ab' over a denominator: the tangent of both the
    // quotient rule and atan2, which differ only in the denominator (b^2
    // against a^2 + b^2).  Written once because the OPERATION ORDER is the
    // contract in this header -- it has to match stan/math/fwd exactly, and a
    // second copy is a second place for that order to drift.
    template <int N>
    inline void dualCrossOver(const dualN<N>& a, const dualN<N>& b,
                              double den, dualN<N>& r) {
      for (int i = 0; i < N; ++i) r.d_[i] = (a.d_[i] * b.v_ - a.v_ * b.d_[i]) / den;
    }

    // ---- division (fwd/core/operator_division.hpp) ------------------------
    template <int N>
    inline dualN<N> operator/(const dualN<N>& a, const dualN<N>& b) {
      dualN<N> r(a.v_ / b.v_);
      dualCrossOver(a, b, b.v_ * b.v_, r);
      return r;
    }
    template <int N>
    inline dualN<N> operator/(const dualN<N>& a, double b) {
      dualN<N> r(a.v_ / b);
      for (int i = 0; i < N; ++i) r.d_[i] = a.d_[i] / b;
      return r;
    }
    template <int N>
    inline dualN<N> operator/(double a, const dualN<N>& b) {
      dualN<N> r(a / b.v_);
      const double den = b.v_ * b.v_;
      for (int i = 0; i < N; ++i) r.d_[i] = -a * b.d_[i] / den;
      return r;
    }

    // ---- compound assignment ---------------------------------------------
    template <int N> inline dualN<N>& operator+=(dualN<N>& a, const dualN<N>& b) { a = a + b; return a; }
    template <int N> inline dualN<N>& operator+=(dualN<N>& a, double b)          { a = a + b; return a; }
    template <int N> inline dualN<N>& operator-=(dualN<N>& a, const dualN<N>& b) { a = a - b; return a; }
    template <int N> inline dualN<N>& operator-=(dualN<N>& a, double b)          { a = a - b; return a; }
    template <int N> inline dualN<N>& operator*=(dualN<N>& a, const dualN<N>& b) { a = a * b; return a; }
    template <int N> inline dualN<N>& operator*=(dualN<N>& a, double b)          { a = a * b; return a; }
    template <int N> inline dualN<N>& operator/=(dualN<N>& a, const dualN<N>& b) { a = a / b; return a; }
    template <int N> inline dualN<N>& operator/=(dualN<N>& a, double b)          { a = a / b; return a; }

    // ---- comparisons (value only, as fvar does) --------------------------
#define RX_DUALN_CMP(OP)                                                \
    template <int N> inline bool operator OP (const dualN<N>& a, const dualN<N>& b) { return a.v_ OP b.v_; } \
    template <int N> inline bool operator OP (const dualN<N>& a, double b)          { return a.v_ OP b; } \
    template <int N> inline bool operator OP (double a, const dualN<N>& b)          { return a OP b.v_; } \
    template <int N> inline bool operator OP (const dualN<N>& a, int b)             { return a.v_ OP b; } \
    template <int N> inline bool operator OP (int a, const dualN<N>& b)             { return a OP b.v_; }
    RX_DUALN_CMP(<)
    RX_DUALN_CMP(>)
    RX_DUALN_CMP(<=)
    RX_DUALN_CMP(>=)
    RX_DUALN_CMP(==)
    RX_DUALN_CMP(!=)
#undef RX_DUALN_CMP

    // ---- elementary functions --------------------------------------------
    // fwd/fun/exp.hpp: fvar(exp(v), d * exp(v))
    template <int N>
    inline dualN<N> exp(const dualN<N>& a) {
      const double e = std::exp(a.v_);
      dualN<N> r(e);
      for (int i = 0; i < N; ++i) r.d_[i] = a.d_[i] * e;
      return r;
    }
    // fwd/fun/log.hpp
    template <int N>
    inline dualN<N> log(const dualN<N>& a) {
      if (a.v_ < 0.0) {
        dualN<N> r(std::numeric_limits<double>::quiet_NaN());
        for (int i = 0; i < N; ++i) r.d_[i] = std::numeric_limits<double>::quiet_NaN();
        return r;
      }
      dualN<N> r(std::log(a.v_));
      for (int i = 0; i < N; ++i) r.d_[i] = a.d_[i] / a.v_;
      return r;
    }
    // fwd/fun/sqrt.hpp: fvar(sqrt(v), 0.5 * d * inv_sqrt(v)), inv_sqrt = 1/sqrt
    template <int N>
    inline dualN<N> sqrt(const dualN<N>& a) {
      const double s = std::sqrt(a.v_);
      const double is = 1.0 / std::sqrt(a.v_);
      dualN<N> r(s);
      for (int i = 0; i < N; ++i) r.d_[i] = 0.5 * a.d_[i] * is;
      return r;
    }
    // fwd/fun/square.hpp: fvar(v*v, d * 2 * v)
    template <int N>
    inline dualN<N> square(const dualN<N>& a) {
      dualN<N> r(a.v_ * a.v_);
      for (int i = 0; i < N; ++i) r.d_[i] = a.d_[i] * 2 * a.v_;
      return r;
    }
    // fwd/fun/pow.hpp
    template <int N>
    inline dualN<N> pow(const dualN<N>& a, double b) {
      dualN<N> r(std::pow(a.v_, b));
      const double dp = std::pow(a.v_, b - 1);
      for (int i = 0; i < N; ++i) r.d_[i] = a.d_[i] * b * dp;
      return r;
    }
    template <int N>
    inline dualN<N> pow(const dualN<N>& a, const dualN<N>& b) {
      const double u = std::pow(a.v_, b.v_);
      const double la = std::log(a.v_);
      dualN<N> r(u);
      for (int i = 0; i < N; ++i) r.d_[i] = (b.d_[i] * la + b.v_ * a.d_[i] / a.v_) * u;
      return r;
    }
    template <int N>
    inline dualN<N> pow(double a, const dualN<N>& b) {
      const double u = std::pow(a, b.v_);
      const double la = std::log(a);
      dualN<N> r(u);
      for (int i = 0; i < N; ++i) r.d_[i] = b.d_[i] * la * u;
      return r;
    }
    // fwd/fun/sin.hpp, cos.hpp, acos.hpp
    template <int N>
    inline dualN<N> sin(const dualN<N>& a) {
      const double c = std::cos(a.v_);
      dualN<N> r(std::sin(a.v_));
      for (int i = 0; i < N; ++i) r.d_[i] = a.d_[i] * c;
      return r;
    }
    template <int N>
    inline dualN<N> cos(const dualN<N>& a) {
      const double s = -std::sin(a.v_);
      dualN<N> r(std::cos(a.v_));
      for (int i = 0; i < N; ++i) r.d_[i] = a.d_[i] * s;
      return r;
    }
    template <int N>
    inline dualN<N> acos(const dualN<N>& a) {
      const double den = -std::sqrt(1 - a.v_ * a.v_);
      dualN<N> r(std::acos(a.v_));
      for (int i = 0; i < N; ++i) r.d_[i] = a.d_[i] / den;
      return r;
    }
    // fwd/fun/atan2.hpp
    template <int N>
    inline dualN<N> atan2(const dualN<N>& a, const dualN<N>& b) {
      dualN<N> r(std::atan2(a.v_, b.v_));
      dualCrossOver(a, b, b.v_ * b.v_ + a.v_ * a.v_, r);
      return r;
    }
    template <int N>
    inline dualN<N> atan2(double a, const dualN<N>& b) {
      const double den = a * a + b.v_ * b.v_;
      dualN<N> r(std::atan2(a, b.v_));
      for (int i = 0; i < N; ++i) r.d_[i] = (-a * b.d_[i]) / den;
      return r;
    }
    template <int N>
    inline dualN<N> atan2(const dualN<N>& a, double b) {
      const double den = b * b + a.v_ * a.v_;
      dualN<N> r(std::atan2(a.v_, b));
      for (int i = 0; i < N; ++i) r.d_[i] = (a.d_[i] * b) / den;
      return r;
    }
    // fwd/fun/fabs.hpp and abs.hpp: identical branch structure
    template <int N>
    inline dualN<N> fabs(const dualN<N>& a) {
      if (std::isnan(a.v_)) {
        dualN<N> r(std::fabs(a.v_));
        for (int i = 0; i < N; ++i) r.d_[i] = std::numeric_limits<double>::quiet_NaN();
        return r;
      } else if (a.v_ > 0.0) {
        return a;
      } else if (a.v_ < 0.0) {
        return -a;
      }
      return dualN<N>(0.0);
    }
    template <int N>
    inline dualN<N> abs(const dualN<N>& a) { return fabs(a); }

    template <int N> inline double value_of(const dualN<N>& a) { return a.v_; }

  }
}

namespace std {
  template <int N>
  struct numeric_limits<stan::math::dualN<N> > : public numeric_limits<double> {};
}

namespace Eigen {
  // The costs are deliberately IDENTICAL to Eigen_NumTraits.hpp's for
  // stan::math::fvar, not scaled by the tangent count.  Eigen uses them as
  // heuristics -- among other things to decide how far to unroll a small
  // matrix product -- and a different unrolling reassociates the sum, which
  // showed up as ~1e-15 disagreements against the fvar path on the 3x3
  // products of the three compartment kernels.  Matching fvar's numbers
  // makes Eigen take the same decisions, which is what makes the dualN
  // result bitwise identical rather than merely equal to round-off.
  template <int N>
  struct NumTraits<stan::math::dualN<N> >
    : GenericNumTraits<stan::math::dualN<N> > {
    enum {
      RequireInitialization = 1,
      ReadCost = 2 * NumTraits<double>::ReadCost,
      AddCost = 2 * NumTraits<double>::AddCost,
      MulCost = 3 * NumTraits<double>::MulCost + NumTraits<double>::AddCost
    };
    static int digits10() { return std::numeric_limits<double>::digits10; }
  };

  template <int N, typename BinaryOp>
  struct ScalarBinaryOpTraits<stan::math::dualN<N>, double, BinaryOp> {
    typedef stan::math::dualN<N> ReturnType;
  };
  template <int N, typename BinaryOp>
  struct ScalarBinaryOpTraits<double, stan::math::dualN<N>, BinaryOp> {
    typedef stan::math::dualN<N> ReturnType;
  };
  template <int N, typename BinaryOp>
  struct ScalarBinaryOpTraits<stan::math::dualN<N>, int, BinaryOp> {
    typedef stan::math::dualN<N> ReturnType;
  };
  template <int N, typename BinaryOp>
  struct ScalarBinaryOpTraits<int, stan::math::dualN<N>, BinaryOp> {
    typedef stan::math::dualN<N> ReturnType;
  };
}

#endif // __LINCMTDUALN_H__
