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
    // #2.  Restore the Alast with gradients (if supported by method)
    //
    // #3.  This can be called as many times as needed with ODE, as if
    //      it were simply a function
    //
    // #4.  On the last call, we need to save the last

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
    //
    // - in Eq 11, 32 and 41 you change R to (R+Rg)
    //
    // This was observed after solving a few systems manually
    //

    struct linCmtStan {

      const int ncmt_, oral0_, trans_;
      double *rate_; // This comes from the ode system
      double *A_;    // This comes from the ode system
      double dt_;
      linCmtStan(const int ncmt,
                 const int oral0,
                 const int trans) :
        ncmt_(ncmt),
        oral0_(oral0),
        trans_(trans)
      { }

      template <typename T>
      Eigen::Matrix<T, Eigen::Dynamic, 1> linCmtStan1(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                                                      Eigen::Matrix<T, Eigen::Dynamic, 1> yp,
                                                      T ka) const {
#define k10   g(0, 1)
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )
        T E      = exp(-k10 * dt_);
        T Ea     = E;
        T pDepot = 0.0;
        T rDepot = 0.0;
        T R      = rate_[oral0_];
        if (oral0_  == 1) {
          Ea = exp(-ka*dt_);
          pDepot = yp(0, 0);
          rDepot = rate_[0];
          R = rDepot + R;
        }
        Eigen::Matrix<T, Eigen::Dynamic, 1> ret = yp;
        ret(oral0_, 0) = yp(oral0_, 0)*E + R*(1.0-E)/(k10);
        bool isSme = (abs(ka-k10)  <= DBL_EPSILON*max2(abs(ka), abs(k10)));
        if (isSme) {
          ret(oral0_, 0) += (pDepot*k10 - rDepot)*dt_*E;
        } else {
          ret(oral0_, 0) += (pDepot*ka - rDepot)*(E - Ea)/(ka - k10);
        }
        if (oral0_ == 1) {
          ret(0, 0) = rDepot*(1.0 - Ea)/ka + pDepot*Ea;
        }
#undef k10
        return ret;
      }

      template <typename T>
      Eigen::Matrix<T, Eigen::Dynamic, 1> linCmtStan2(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                                                      Eigen::Matrix<T, Eigen::Dynamic, 1> yp,
                                                      T ka) const {
#define k12   g(1, 0)
#define k21   g(1, 1)
#define k10   g(0, 1)

        stan::math::solComp2struct<T> sol2 =
          stan::math::computeSolComp2(k10, k12, k21);

        Eigen::Matrix<T, Eigen::Dynamic, 1> ret = yp;

        T rDepot = 0.0;
        T R      = rate_[oral0_];

        Eigen::Matrix<T, 2, 1> Xo;
        Eigen::Matrix<T, 2, 1> Rm;
        Eigen::Matrix<T, 2, 1> E = exp(-sol2.L * dt_);
        Eigen::Matrix<T, 2, 1> Ea = E;

        Xo =(yp(oral0_, 0)*sol2.C1) * E +
          (yp(oral0_ + 1, 0)*sol2.C2) * E;

        if (oral0_ == 1 && yp(0, 0) > 0.0) {
          // Xo = Xo + Ka*pX[1]*(Co[, , 1] %*% ((E - Ea)/(Ka - L)))
          rDepot = rate_[0];
          R += rDepot;
          Eigen::Matrix<T, 2, 1> expa = Eigen::Matrix<T, 2, 1>::Constant(2, 1, exp(-ka*dt_));
          Eigen::Matrix<T, 2, 1> ka2 = Eigen::Matrix<T, 2, 1>::Constant(2, 1, ka);
          Ea =  (E - expa).array()/(ka2 - sol2.L).array();
          T cf = ka*yp(0, 0) - rDepot;
          Xo += (cf*sol2.C1)*Ea;
          ret(0, 0) = rDepot*(1.0-expa(0, 0))/ka + yp(0, 0)*expa(0, 0);
        }
        if (R > 0.0) {
          // Xo = Xo + ((cR*Co[, , 1]) %*% ((1 - E)/L)) # Infusion
          Eigen::Matrix<T, 2, 1> o2 = Eigen::Matrix<T, 2, 1>::Constant(2, 1, 1.0);
          Rm = (o2 - E).array()/sol2.L.array();
          Xo += (R*sol2.C1)*Rm;
        }
        ret(oral0_, 0)     = Xo(0, 0);
        ret(oral0_ + 1, 0) = Xo(1, 0);
#undef k12
#undef k21
#undef k10
        return ret;
      }

      template <typename T>
      Eigen::Matrix<T, Eigen::Dynamic, 1> linCmtStan3(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                                                      Eigen::Matrix<T, Eigen::Dynamic, 1> yp,
                                                      T ka) const {
#define k12   g(1, 0)
#define k21   g(1, 1)
#define k13   g(2, 0)
#define k31   g(2, 1)
#define k10   g(0, 1)
        stan::math::solComp3struct<T> sol3 =
          stan::math::computeSolComp3(k10, k12, k21, k13, k31);

        Eigen::Matrix<T, Eigen::Dynamic, 1> ret = yp;

        T rDepot = 0.0;
        T R      = rate_[oral0_];

        Eigen::Matrix<T, 3, 1> Xo;
        Eigen::Matrix<T, 3, 1> Rm;
        Eigen::Matrix<T, 3, 1> E = exp(-sol3.L * dt_);
        Eigen::Matrix<T, 3, 1> Ea = E;

        Xo = (yp(oral0_, 0)*sol3.C1) * E  +
          (yp(oral0_ + 1, 0)*sol3.C2) * E +
          (yp(oral0_ + 2, 0)*sol3.C3) * E ;

        if (oral0_ == 1 && yp(0, 0) > 0.0) {
          // Xo = Xo + Ka*pX[1]*(Co[, , 1] %*% ((E - Ea)/(Ka - L)))
          rDepot = rate_[0];
          R += rDepot;
          Eigen::Matrix<T, 3, 1> expa = Eigen::Matrix<T, 3, 1>::Constant(3, 1, exp(-ka*dt_));
          Eigen::Matrix<T, 3, 1> ka3 = Eigen::Matrix<T, 3, 1>::Constant(3, 1, ka);

          Ea =  (E - expa).array()/(ka3 - sol3.L).array();
          T cf = ka*yp(0, 0) - rDepot;
          Xo += (cf*sol3.C1)*Ea;
          ret(0, 0) = rDepot*(1.0-expa(0, 0))/ka + yp(0, 0)*expa(0, 0);
        }
        if (R > 0.0) {
          // Xo = Xo + ((cR*Co[, , 1]) %*% ((1 - E)/L)) # Infusion
          Eigen::Matrix<T, 3, 1> o3 = Eigen::Matrix<T, 3, 1>::Constant(3, 1, 1.0);
          Rm = (o3 - E).array()/sol3.L.array();
          Xo += (R*sol3.C1)*Rm;
        }
        ret(oral0_, 0)     = Xo(0, 0);
        ret(oral0_ + 1, 0) = Xo(1, 0);
        ret(oral0_ + 2, 0) = Xo(2, 0);
#undef k12
#undef k21
#undef k13
#undef k31
#undef k10
        return ret;
      }

      void setPtr(double *A, double *R) {
        A_ = A;
        rate_ = R;
      }


      template <typename T>
      Eigen::Matrix<T, Eigen::Dynamic, 1> getAlast(const Eigen::Matrix<T, Eigen::Dynamic, 1>& theta) const {
        if (typeid(T) == typeid(double)) {
          Eigen::Matrix<double, Eigen::Dynamic, 1> Alast(ncmt_ + oral0_, 1);
          for (int i = oral0_ + ncmt_; i--;){
            Alast(i, 0) = A_[i];
          }
          return Alast;
        } else {
          Eigen::Matrix<double, Eigen::Dynamic, 1> AlastG(ncmt_ + oral0_,
                                                                       ncmt_*2 + oral0_);
          Eigen::Matrix<double, Eigen::Dynamic, 1> AlastA(ncmt_ + oral0_, 1);
          Eigen::Matrix<T, Eigen::Dynamic, 1> Alast(ncmt_ + oral0_, 1);

          double p1_ = theta[0];
          double v1_ = theta[1];
          double p2_ = theta[2];
          double p3_ = theta[3];
          double p4_ = theta[4];
          double p5_ = theta[5];
          double ka_ = 0.0;
          if (oral0_) {
            ka_ = theta[6];
          }

          for (int i = 0; i < ncmt_ + oral0_; i++) {
            AlastG(i, 0) = A_[ncmt_ + oral0_ +
                              (2*ncmt_ + oral0_)*(i+1) + 0];
            AlastG(i, 1) = A_[ncmt_ + oral0_ +
                              (2*ncmt_ + oral0_)*(i+1) + 1];
            // Alast Adjusted
            AlastA(i, 0) = A_[i];
            AlastA(i, 0) -= AlastG(i, 0)*p1_;
            AlastA(i, 0) -= AlastG(i, 1)*v1_;
            if (ncmt_ >=2){
              AlastG(i, 2) = A_[ncmt_ + oral0_ +
                                (2*ncmt_ + oral0_)*(i+1) + 2];
              AlastG(i, 3) = A_[ncmt_ + oral0_ +
                                (2*ncmt_ + oral0_)*(i+1) + 3];
              // Adjust alast
              AlastA(i, 0) -= AlastG(i, 2)*p2_;
              AlastA(i, 0) -= AlastG(i, 3)*p3_;
              if (ncmt_ >= 3){
                AlastG(i, 4) = A_[ncmt_ + oral0_ +
                                  (2*ncmt_ + oral0_)*(i+1) + 4];
                AlastG(i, 5) = A_[ncmt_ + oral0_ +
                                  (2*ncmt_ + oral0_)*(i+1) + 5];
                // Adjust Alast
                AlastA(i, 0) -= AlastG(i, 4) * p4_;
                AlastA(i, 0) -= AlastG(i, 5) * p5_;
              }
            }
            if (oral0_) {
              AlastG(i, 2*ncmt_) = A_[ncmt_ + oral0_ +
                                      (2*ncmt_ + oral0_)*(i+1) +
                                      2*ncmt_];
              AlastA(i, 0) -= AlastG(i, 2*ncmt_)*ka_;
            }
          }
          for (int i = oral0_ + ncmt_; i--;){
            Alast(i, 0) = AlastA(i, 0) +
              theta(0, 0)*AlastG(i, 0) +
              theta(1, 0)*AlastG(i, 1);
            if (ncmt_ >= 2) {
              Alast(i, 0) += theta(2, 0)*AlastG(i, 2) +
                theta(3, 0)*AlastG(i, 3);
              if (ncmt_ == 3) {
                Alast(i, 0) += theta(4, 0)*AlastG(i, 4) +
                  theta(5, 0)*AlastG(i, 5);
              }
            }
            if (oral0_) {
              Alast(i, 0) += theta(2*ncmt_, 0)*AlastG(i, 2*ncmt_);
            }
          }
          return Alast;
        }
      }


      void setAlast(Eigen::Matrix<double, Eigen::Dynamic, 1> Alast) {
        for (int i = 0; i < ncmt_ + oral0_; i++) {
          //(3*ncmt+2*oral0)+0
          A_[i] = Alast(i, 0);
        }
      }

      void setAlast(Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> Alast,
                    Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> J) {
        A_[ncmt_ + oral0_ + 0] = J(0, 0);
        A_[ncmt_ + oral0_ + 1] = J(0, 1);
        if (ncmt_ >=2){
          A_[ncmt_ + oral0_ + 2] = J(0, 2);
          A_[ncmt_ + oral0_ + 3] = J(0, 3);
          if (ncmt_ == 3){
            A_[ncmt_ + oral0_ + 4] = J(0, 4);
            A_[ncmt_ + oral0_ + 5] = J(0, 5);
          }
        }
        if (oral0_) {
          A_[ncmt_ + oral0_ + 2*ncmt_] = J(0, 2*ncmt_);
        }
        // Save A1-A4
        for (int i = 0; i < ncmt_ + oral0_; i++) {
          //(3*ncmt+2*oral0)+0
          stan::math::var smv = Alast(i, 0);
          A_[i] = smv.val();
          A_[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i+1) + 0] = J(i+1, 0);
          A_[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i+1) + 1] = J(i+1, 1);
          if (ncmt_ >=2){
            A_[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i+1) + 2] = J(i+1, 2);
            A_[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i+1) + 3] = J(i+1, 3);
            if (ncmt_ == 3){
              A_[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i+1)+ 4] = J(i+1, 4);
              A_[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i+1)+ 5] = J(i+1, 5);
            }
          }
          if (oral0_) {
            //(3*ncmt+oral0)+2*ncmt
            A_[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i+1)+ 2*ncmt_] = J(i+1, 2*ncmt_);
          }
        }
      }

      void setRate(double *R) {
        rate_ = R;
      }

      void setDt(double dt) {
        dt_ = dt;
      }
      // For stan Jacobian to work the class needs to take 1 argument
      // (the parameters)
      template <typename T>
      Eigen::Matrix<T, 1, 1> operator()(const Eigen::Matrix<T, Eigen::Dynamic, 1>& theta) const {
        Eigen::Matrix<double, Eigen::Dynamic, 2> g =
          stan::math::macros2micros(theta, ncmt_, trans_);

        T ka = 0.0;
        if (oral0_) {
          ka = theta[ncmt_*2];
        }
        Eigen::Matrix<T, Eigen::Dynamic, 1> yp(ncmt_ + oral0_, 1);
        yp = getAlast(theta);
        Eigen::Matrix<T, Eigen::Dynamic, 1> ret0(ncmt_ + oral0_, 1);
        if (ncmt_ == 1) {
          ret0 = linCmtStan1<T>(g, yp, ka);
        } else if (ncmt_ == 2) {
          ret0 = linCmtStan2<T>(g, yp, ka);
        } else if (ncmt_ == 3) {
          ret0 = linCmtStan3<T>(g, yp, ka);
        }
        Eigen::Matrix<T, 1, 1> ret(1, 1);
        if (trans_ != 10 || ncmt_ == 1) {
          ret(0, 0) = ret0(oral0_, 0) / theta(1, 0);
        } else if (ncmt_ == 2) {
          ret(0, 0) = ret0(oral0_, 0) / (theta(1, 0) + theta(3, 0));
        } else if (ncmt_ == 3) {
          ret(0, 0) = ret0(oral0_, 0) / (theta(1, 0) + theta(3, 0) + theta(5, 0));
        } else {
          ret(0, 0) = ret0(oral0_, 0);
        }
        return ret;
      }
    };

  }
}

#endif
