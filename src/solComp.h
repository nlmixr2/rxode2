#ifndef __SOLCOMP_H__
#ifndef NDEBUG
#define NDEBUG // just in case
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <stan/math.hpp>
#ifndef NDEBUG
#define NDEBUG // just in case
#endif
#include <Rcpp.h>
#include <RcppEigen.h>

#include "../inst/include/rxode2parse.h"

namespace stan {
  namespace math {

    using std::exp;
    using stan::math::exp;
    using std::sqrt;
    using stan::math::sqrt;
    using std::pow;
    using stan::math::pow;
    using std::acos;
    using stan::math::acos;
    using std::cos;
    using stan::math::cos;

    template <typename T>
    struct solComp2struct {
      Eigen::Matrix<T, 2, 1> L; // 2-element vector
      Eigen::Matrix<T, 2, 2> C1; // 2x2 matrix
      Eigen::Matrix<T, 2, 2> C2; // 2x2 matrix
      bool success = false;
    };

    template <typename T>
    struct solComp3struct {
      Eigen::Matrix<T, 3, 1> L; // 3-element vector
      Eigen::Matrix<T, 3, 3> C1; // 3x3 matrix
      Eigen::Matrix<T, 3, 3> C2; // 3x3 matrix
      Eigen::Matrix<T, 3, 3> C3; // 3x3 matrix
      bool success = false;
    };


    template <typename T>
    solComp2struct<T> computeSolComp2(T k10, T k12, T k21) {
      solComp2struct<T> out;
      T sum = k10 + k12 + k21;
      T disc = sqrt(sum*sum - 4*k10*k21);
      T L0 = 0.5*(sum + disc);
      T L1 = 0.5*(sum - disc);

      if (abs(L1 - L0) <= DBL_EPSILON) {
        out.success = false;
        return out;
      }

      T invD0 = 1.0/(L1 - L0);
      T invD1 = -invD0;

      T tmpSum = k10 + k12;

      // C1
      out.C1(0, 0) = (k21 - L0)*invD0;
      out.C1(1, 0) = k12*invD0;
      out.C1(0, 1) = (k21 - L1)*invD1;
      out.C1(1, 1) = k12*invD1;

      // C2
      out.C2(0, 0) = k21*invD0;
      out.C2(1, 0) = (tmpSum - L0)*invD0;
      out.C2(0, 1) = k21*invD1;
      out.C2(1, 1) = (tmpSum - L1)*invD1;

      // L
      out.L(0, 0) = L0;
      out.L(1, 0) = L1;
      out.success = true;
      return out;
    }

    template <typename T>
    solComp3struct<T> computeSolComp3(T k10, T k12, T k21, T k13, T k31) {
      solComp3struct<T> out;
      // Get lambas
      T A1 = k10 + k12 + k13 + k21 + k31;
      T A2 = k10*k21 + k10*k31 + k12*k31 + k13*k21 + k21*k31;
      T A3 = k21*k31*k10;
      T Q  = (A1*A1 - 3.0*A2)/9.0;
      T RQ = 2.0*sqrt(Q);
      T R  = (2.0*A1*A1*A1 - 9.0*A1*A2 + 27.0*A3)/54.0;
      T M  = Q*Q*Q - R*R;
      if (M < 0) {
        out.success = false;
        return out;
      }
      T Th = acos(8.0*R/(RQ*RQ*RQ));
      out.L(0, 0) = RQ*cos(Th/3.0) + A1/3.0;
      out.L(1, 0) = RQ*cos((Th + M_2PI)/3.0) + A1/3.0;
      out.L(2, 0) = RQ*cos((Th + 2*M_2PI)/3.0) + A1/3.0;
      Eigen::Matrix<T, 3, 1> D;
      D(0, 0) = (out.L(1, 0) - out.L(0, 0))*(out.L(2, 0) - out.L(0, 0));
      D(1, 0) = (out.L(0, 0) - out.L(1, 0))*(out.L(2, 0) - out.L(1, 0));
      D(2, 0) = (out.L(0, 0) - out.L(2, 0))*(out.L(1, 0) - out.L(2, 0));
      if (D(0, 0)*D(1, 0)*D(2, 0) == 0.0) {
        out.success = false;
        return out;
      }
      out.C1(0, 0) = (k21 - out.L(0, 0))*(k31 - out.L(0, 0));
      out.C1(0, 1) = (k21 - out.L(1, 0))*(k31 - out.L(1, 0));
      out.C1(0, 2) = (k21 - out.L(2, 0))*(k31 - out.L(2, 0));

      out.C2(0, 0) = k21*(k31 - out.L(0, 0));
      out.C2(0, 1) = k21*(k31 - out.L(1, 0));
      out.C2(0, 2) = k21*(k31 - out.L(2, 0));

      out.C3(0, 0) = k31*(k21 - out.L(0, 0));
      out.C3(0, 1) = k31*(k21 - out.L(1, 0));
      out.C3(0, 2) = k31*(k21 - out.L(2, 0));

      out.C1(1, 0) = k12*(k31 - out.L(0, 0));
      out.C1(1, 1) = k12*(k31 - out.L(1, 0));
      out.C1(1, 2) = k12*(k31 - out.L(2, 0));

      out.C2(1, 0) = (k10 + k12 + k13 - out.L(0, 0))*(k31 - out.L(0, 0)) - k31*k13;
      out.C2(1, 1) = (k10 + k12 + k13 - out.L(1, 0))*(k31 - out.L(1, 0)) - k31*k13;
      out.C2(1, 2) = (k10 + k12 + k13 - out.L(2, 0))*(k31 - out.L(2, 0)) - k31*k13;

      out.C3(1, 0) = out.C3(1, 1) = out.C3(1, 2) = k12*k31;

      out.C1(2, 0) = k13*(k21 - out.L(0, 0));
      out.C1(2, 1) = k13*(k21 - out.L(1, 0));
      out.C1(2, 2) = k13*(k21 - out.L(2, 0));

      out.C2(2, 0) = out.C2(2, 1) = out.C2(2, 2) = k21*k13;

      out.C3(2, 0) = (k10 + k12 + k13 - out.L(0, 0))*(k21 - out.L(0, 0)) - k21*k12;
      out.C3(2, 1) = (k10 + k12 + k13 - out.L(1, 0))*(k21 - out.L(1, 0)) - k21*k12;
      out.C3(2, 2) = (k10 + k12 + k13 - out.L(2, 0))*(k21 - out.L(2, 0)) - k21*k12;

      out.C1(0, 0) = out.C1(0, 0)/D(0, 0);
      out.C1(1, 0) = out.C1(1, 0)/D(0, 0);
      out.C1(2, 0) = out.C1(2, 0)/D(0, 0);

      out.C2(0, 0) = out.C2(0, 0)/D(0, 0);
      out.C2(1, 0) = out.C2(1, 0)/D(0, 0);
      out.C2(2, 0) = out.C2(2, 0)/D(0, 0);

      out.C3(0, 0) = out.C3(0, 0)/D(0, 0);
      out.C3(1, 0) = out.C3(1, 0)/D(0, 0);
      out.C3(2, 0) = out.C3(2, 0)/D(0, 0);

      out.C1(0, 1) = out.C1(0, 1)/D(1, 0);
      out.C1(1, 1) = out.C1(1, 1)/D(1, 0);
      out.C1(2, 1) = out.C1(2, 1)/D(1, 0);

      out.C2(0, 1) = out.C2(0, 1)/D(1, 0);
      out.C2(1, 1) = out.C2(1, 1)/D(1, 0);
      out.C2(2, 1) = out.C2(2, 1)/D(1, 0);

      out.C3(0, 1) = out.C3(0, 1)/D(1, 0);
      out.C3(1, 1) = out.C3(1, 1)/D(1, 0);
      out.C3(2, 1) = out.C3(2, 1)/D(1, 0);

      out.C1(0, 2) = out.C1(0, 2)/D(2, 0);
      out.C1(1, 2) = out.C1(1, 2)/D(2, 0);
      out.C1(2, 2) = out.C1(2, 2)/D(2, 0);

      out.C2(0, 2) = out.C2(0, 2)/D(2, 0);
      out.C2(1, 2) = out.C2(1, 2)/D(2, 0);
      out.C2(2, 2) = out.C2(2, 2)/D(2, 0);

      out.C3(0, 2) = out.C3(0, 2)/D(2, 0);
      out.C3(1, 2) = out.C3(1, 2)/D(2, 0);
      out.C3(2, 2) = out.C3(2, 2)/D(2, 0);

      out.success = true;
      return out;

    }
  }
}

#endif
