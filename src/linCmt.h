#ifndef __LINCMT_H__
#define __LINCMT_H__

#include "macros2micros.h"
#include "solComp.h"

// Global linear compartment model parameters:
// p1, v, p2, p3, p3, p4, ka

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

    // For every ODE solving step, we need to:
    //
    // #1.  Reset the global linear compartment model parameters.
    //
    // #2.  On the first call, set the parameters and the parameter
    //      translation.
    //
    // #3.  Restore the Alast with gradients (if supported by method)
    //
    // #4.  This can be called as many times as needed with ODE, as if
    //      it were simply a function
    //
    // #5.  On the last call, we need to save the last

    struct linCmtStan {

      const int ncmt_, oral0_, trans_;
      double *rate_;
      linCmtStan(const int ncmt,
                 const int oral0,
                 const int trans) :
        ncmt_(ncmt),
        oral0_(oral0),
        trans_(trans)
      { }

      // For stan Jacobian to work the class needs to take 1 argument
      // (the parameters)

      template <typename T>
      Eigen::Matrix<T, -1, 1> linCmtStan1(Eigen::Matrix<T, -1, 1>& g,
                                          Eigen::Matrix<T, -1, 1>& yp,
                                          T ka,
                                          double dt) {
#define k10   g(0, 1)
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )
        T E      = exp(-k10 * dt);
        T Ea     = E;
        T pDepot = 0.0;
        T rDepot = 0.0;
        T R      = rate_[oral0_];
        // In the derivation used in https://doi.org/10.12793/tcp.2019.27.2.43
        // the expression for the Ka contribution (Eq 9, Eq 13 and Eq 33) is given by
        //
        // Ka*Xg(0)*exp(-Ka*t)
        //
        // This is true as long as there is not an infusion in the depot
        //
        // When there is an infusion (Rg) in the depot the ka would be:
        //
        // Ka*[Kg(0)*exp(-Ka*t) + Rg/ka*(1-exp(-Ka*t))]
        //
        // as implied by equation #12 (which is the eq in the bracket)
        //
        // expanding this becomes:
        //
        // (Ka*Kg(0) - Rg)*exp(-Ka*t) + Rg
        //
        // Both Ka*Kg(0) and Ka*kg(0)-Rg in general are not dependent on
        // time.  Also Rg is simply a time invariant constant
        //
        // Which means to get equations where infusions into a depot are supported
        // You need to simply change 2 items:
        //
        // - in Eq 11, 32 and 41 you change Ka*Kg(0) to (Ka*Kg(0)- Rg)
        // - in Eq 11, 32 and 41 you change R to (R+Rg)
        //
        // This was observed after solving a few systems manually
        if (oral0_  == 1) {
          Ea = exp(-ka*dt);
          pDepot = yp(0, 0);
          rDepot = rate_[0];
          R = rDepot + R;
        }
        Eigen::Matrix<T, -1, 1> ret = yp;
        ret(oral0_, 0) = yp(oral0_, 0)*E + R*(1.0-E)/(k10);
        bool isSme = (abs(ka-k10)  <= DBL_EPSILON*max2(abs(ka), abs(k10)));
        if (diff <=) {
          ret(oral0_, 0) += (pDepot*k10 - rDepot)*dt*E;
        } else {
          yp(oral0_, 0) += (pDepot*ka - rDepot)*(E - Ea)/(ka - k10);
        }
        if (oral0_ == 1) {
          yp(0, 0) = rDepot*(1.0 - Ea)/ka + pDepot*Ea;
        }

#undef k10
      }

      template <typename T>
      Eigen::Matrix<T, -1, 1> linCmtStan2(Eigen::Matrix<T, -1, 1>& g,
                                          Eigen::Matrix<T, -1, 1>& yp,
                                          T ka,
                                          double dt) {

#define k12   g(1, 0)
#define k21   g(1, 1)
#define k10   g(0, 1)
        stan::math::solComp2struct<T> sol2 =
          stan::math::computeSolComp2(k10, k12, k21);

        Eigen::Matrix<T, -1, 1> ret = yp;

        T rDepot = 0.0;
        T R      = rate_[oral0_];

        Eigen::Matrix<T, 2, 1> Xo;
        Eigen::Matrix<T, 2, 1> Rm;
        Eigen::Matrix<T, 2, 1> E = exp(-sol2.L * dt);
        Eigen::Matrix<T, 2, 1> Ea = E;

        Xo = (yp(oral0_, 0)*sol2.C1) * E + (yp(oral0_ + 1, 0)*sol2.C2) * E;

        if (oral0_ == 1 && yp(0, 0) > 0.0) {
          // Xo = Xo + Ka*pX[1]*(Co[, , 1] %*% ((E - Ea)/(Ka - L)))
          rDepot = rate_[0];
          R += rDepot;
          T expa = exp(-ka*dt);
          Ea =  (E - expa)/(ka - L);
          T cf = ka*yp(0, 0) - rDepot;
          Xo += (cf*sol2.C1)*Ea;
          ret(0, 0) = rDepot*(1.0-expa)/ka + yp(0, 0)*expa;
        }
        if (R > 0.0) {
          // Xo = Xo + ((cR*Co[, , 1]) %*% ((1 - E)/L)) # Infusion
          Rm = (1.0 - E)/L;
          Xo += (R*sol2.C1)*Rm
        }
        ret(oral0_, 0)     = Xo(0, 0);
        ret(oral0_ + 1, 0) = Xo(1, 0);
#undef k12
#undef k21
#undef k10
        return ret;
      }

      template <typename T>
      Eigen::Matrix<T, -1, 1> linCmtStan3(Eigen::Matrix<T, -1, 1>& g,
                                          Eigen::Matrix<T, -1, 1>& yp,
                                          T ka,
                                          double dt) {

      }

      template <typename T>
      Eigen::Matrix<T, -1, 1> operator()(const Eigen::Matrix<T, -1, 1>& theta) const {
        Eigen::Matrix<double, Eigen::Dynamic, 2> g = stan::math::macros2micros(theta, ncmt, trans);

      }

    };


  }
}


#endif
