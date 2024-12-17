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
    solComp2struct<T> computeSolComp2(T k10, T k12, T k21) {
      solComp2struct<T> out;
      T sum = k10 + k12 + k21;
      T disc = sqrt(sum*sum - 4*k10*k21);
      Eigen::Matrix<T, 2, 1> div;
      out.L(0, 0) = 0.5*(sum + disc);
      out.L(1, 0) = 0.5*(sum - disc);

      div(0, 0) = out.L(1, 0) - out.L(0, 0);
      div(1, 0) = out.L(0, 0) - out.L(1, 0);
      T tmp = div(0, 0)*div(1, 0);
      if (tmp == 0) {
        out.success = false;
        return out;
      }
      out.C1(0, 0) = k21 - out.L(0, 0);
      out.C1(0, 1) = k21 - out.L(1, 0);

      out.C2(0, 0) = out.C2(0, 1) = k21;
      out.C1(1, 0) = out.C1(1, 1) = k12;

      tmp = k10 + k12;

      out.C2(1, 0) = tmp - out.L(0, 0);
      out.C2(1, 1) = tmp - out.L(1, 0);

      out.C1(0, 0) = out.C1(0, 0)/div(0, 0);
      out.C1(1, 0) = out.C1(1, 0)/div(0, 0);

      out.C2(0, 0) = out.C2(0, 0)/div(0, 0);
      out.C2(1, 0) = out.C2(1, 0)/div(0, 0);

      out.C1(0, 1) = out.C1(0, 1)/div(1, 0);
      out.C1(1, 1) = out.C1(1, 1)/div(1, 0);

      out.C2(0, 1) = out.C2(0, 1)/div(1, 0);
      out.C2(1, 1) = out.C2(1, 1)/div(1, 0);
      out.success = true;
      return out;
    }
  }
}

#endif
