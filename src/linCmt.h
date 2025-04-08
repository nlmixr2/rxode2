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
      double *Asave_; // This comes from the ode system
      double dt_;
      bool grad_;
      linCmtStan(const int ncmt,
                 const int oral0,
                 const int trans,
                 const bool grad) :
        ncmt_(ncmt),
        oral0_(oral0),
        trans_(trans),
        grad_(grad)
      { }

      int getNpars() {
        if (oral0_) {
          return 2*ncmt_ + 1;
        } else {
          return 2*ncmt_;
        }
      }

      int getNalast() {
        if (grad_) {
          return ncmt_ + oral0_ + ncmt_*getNpars() + oral0_;
        } else {
          return ncmt_ + oral0_;
        }
        return 0;
      }

      int getNrate() {
        return 1 + oral0_;
      }

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

        if (oral0_ == 1 && yp(0, 0) >= 0.0) {
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

        if (oral0_ == 1 && yp(0, 0) >= 0.0) {
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

      void setPtr(double *A, double *R, double *Asave) {
        A_ = A;
        rate_ = R;
        Asave_ = Asave;
      }

      Eigen::Matrix<double,Eigen::Dynamic, 1> restoreFx(double *A) const {
        // Save A1-A4
        Eigen::Matrix<double, Eigen::Dynamic, 1> Alast(ncmt_ + oral0_, 1);
        for (int i = 0; i < ncmt_ + oral0_; i++) {
          Alast(i, 0) = A[i];
        }
        return Alast;
      }

      Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> restoreJac(double *A) const {
        // Save A1-A4
        Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> J(ncmt_ + oral0_,
                                                                2*ncmt_ + oral0_);
        for (int i = oral0_; i < ncmt_ + oral0_; i++) {
          J(i, 0) = A[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i-oral0_) + 0];
          J(i, 1) = A[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i-oral0_) + 1];
          if (ncmt_ >=2) {
            J(i, 2) = A[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i-oral0_) + 2];
            J(i, 3) = A[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i-oral0_) + 3];
            if (ncmt_ == 3){
              J(i, 4) = A[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i-oral0_) + 4];
              J(i, 5) = A[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i-oral0_) + 5];
            }
          }
          if (oral0_) {
            J(i, 2*ncmt_) = A[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i-oral0_) + 2*ncmt_];
          }
        }
        // save Ka; for oral only ka affects values
        if (oral0_) {
          for (int i = 0; i < 2*ncmt_; ++i) {
            J(0, i) = 0;
          }
          J(0, 2*ncmt_) = A[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*ncmt_];
        }
        return J;
      }

      template <typename T>
      Eigen::Matrix<T, Eigen::Dynamic, 1> getAlast(const Eigen::Matrix<T, Eigen::Dynamic, 1>& theta) const {
        Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> J(ncmt_ + oral0_,
                                                                ncmt_*2 + oral0_);
        J = restoreJac(&A_[0]);

        Eigen::Matrix<double, Eigen::Dynamic, 1> AlastA(ncmt_ + oral0_, 1);
        // AlastA.setZero();

        Eigen::Matrix<T, Eigen::Dynamic, 1> Alast(ncmt_ + oral0_, 1);
        // Alast.setZero();

        T cur = theta(0, 0);
        double val = cur.val();
        double p1_ = val;
        cur = theta(1, 0);
        val = cur.val();
        double v1_ = val;
        double p2_, p3_, p4_, p5_, ka_;
        p2_ = p3_ = p4_ = p5_ = ka_ = 0.0;
        if (ncmt_ >= 2) {
          cur = theta(2, 0);
          val = cur.val();
          p2_ = val;
          cur = theta(3, 0);
          val = cur.val();
          p3_ = val;
        }
        if (ncmt_ >= 3) {
          cur = theta(4, 0);
          val = cur.val();
          p4_ = val;
          cur = theta(5, 0);
          val = cur.val();
          p5_ = val;

        }
        if (oral0_) {
          cur = theta(ncmt_*2, 0);
          val = cur.val();
          ka_ = val;
        }
        for (int i = 0; i < ncmt_ + oral0_; i++) {
          // Alast Adjusted
          AlastA(i, 0) = A_[i];

          AlastA(i, 0) -= J(i, 0)*p1_;

          AlastA(i, 0) -= J(i, 1)*v1_;

          if (ncmt_ >=2){
            // Adjust alast
            AlastA(i, 0) -= J(i, 2)*p2_;
            AlastA(i, 0) -= J(i, 3)*p3_;
            if (ncmt_ >= 3){
              // Adjust Alast
              AlastA(i, 0) -= J(i, 4) * p4_;
              AlastA(i, 0) -= J(i, 5) * p5_;
            }
          }
          if (oral0_) {
            AlastA(i, 0) -= J(i, 2*ncmt_)*ka_;
          }
        }
        for (int i = oral0_ + ncmt_; i--;){
          Alast(i, 0) = AlastA(i, 0) +
            theta(0, 0)*J(i, 0) +
            theta(1, 0)*J(i, 1);
          if (ncmt_ >= 2) {
            Alast(i, 0) += theta(2, 0)*J(i, 2) +
              theta(3, 0)*J(i, 3);
            if (ncmt_ == 3) {
              Alast(i, 0) += theta(4, 0)*J(i, 4) +
                theta(5, 0)*J(i, 5);
            }
          }
          if (oral0_) {
            Alast(i, 0) += theta(2*ncmt_, 0)*J(i, 2*ncmt_);
          }
        }
        return Alast;
      }

      void setAlast(Eigen::Matrix<double, Eigen::Dynamic, 1> Alast, const int N) {
        for (int i = 0; i < N; i++) {
          A_[i] = Alast(i, 0);
        }
      }

      void saveJac(Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> J) {
        // Save A1-A4
        for (int i = oral0_; i < ncmt_ + oral0_; i++) {
          Asave_[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i-oral0_) + 0] = J(i, 0);
          Asave_[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i-oral0_) + 1] = J(i, 1);
          if (ncmt_ >=2) {
            Asave_[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i-oral0_) + 2] = J(i, 2);
            Asave_[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i-oral0_) + 3] = J(i, 3);
            if (ncmt_ == 3){
              Asave_[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i-oral0_) + 4] = J(i, 4);
              Asave_[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i-oral0_) + 5] = J(i, 5);
            }
          }
          if (oral0_) {
            Asave_[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*(i-oral0_) + 2*ncmt_] =
              J(i, 2*ncmt_);
          }
        }
        // save Ka; for oral only ka affects values
        if (oral0_) {
          Asave_[ncmt_ + oral0_ + (2*ncmt_ + oral0_)*ncmt_] =
            J(0, 2*ncmt_);
        }
      }

      void setRate(double *R) {
        rate_ = R;
      }

      void setDt(double dt) {
        dt_ = dt;
      }

      template <typename T>
      void saveAlast(Eigen::Matrix<T, Eigen::Dynamic, 1> ret0) const {
        for (int i = 0; i < ncmt_ + oral0_; i++) {
          T smv = ret0(i, 0);
          Asave_[i] = smv.val();
        }
      }

      // For stan Jacobian to work the class needs to take 1 argument
      // (the parameters)
      template <typename T>
      Eigen::Matrix<T, Eigen::Dynamic, 1> operator()(const Eigen::Matrix<T, Eigen::Dynamic, 1>& theta) const {
        Eigen::Matrix<T, Eigen::Dynamic, 2> g =
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
        saveAlast<T>(ret0);
        return ret0;
      }

      double getVc(const Eigen::Matrix<double, Eigen::Dynamic, 1>& theta) {
        int sw = ncmt_*300 + trans_;
        switch (sw) {
        case 311:
          return 1.0/(1.0/theta(1, 0) + theta(3, 0) + theta(5, 0));
          break;
        case 211:
          return 1.0/(1.0/theta(1, 0) + theta(3, 0));
          break;
          // Note 111 is the as simply using volume (included below)

        case 310:
          return 1.0/(theta(1, 0) + theta(3, 0) + theta(5, 0));
          break;
        case 210:
          return  1.0/(theta(1, 0) + theta(3, 0));
          break;
        case 110:
          return 1.0/(theta(1, 0));
          break;

        case 101:
        case 102:
        case 111:
        case 201:
        case 202:
        case 203:
        case 204:
        case 205:
        case 301:
        case 302:
        default:
          return theta(1, 0);
        }
        return NA_REAL;
      }

      Eigen::Matrix<double, -1, -1> getJacCp(const Eigen::Matrix<double, -1, -1> J0,
                                             const Eigen::VectorXd ret0,
                                             const Eigen::Matrix<double, Eigen::Dynamic, 1>& theta) {
        Eigen::Matrix<double, Eigen::Dynamic, 1> J = J0.row(oral0_);
        Eigen::Matrix<double, Eigen::Dynamic, 1> Jf = J;

        double v = getVc(theta);

        for (int i = 0; i < getNpars(); i++) {
          if (((ncmt_ == 1 && i == 1) ||
               (ncmt_ == 2 && (i == 1 || i ==3)) ||
               (ncmt_ == 3 && (i == 1 || i ==3 || i == 5)))) {
            if (trans_ == 11 && ncmt_ >= 2 && i == 1) {
              // > D(S("f(v1)/(1/v1+v2+v3)"), "v1")
              //   (Add) Derivative(f(v1), v1)/(v2 + v3 + v1^(-1)) + f(v1)/(v1^2*(v2 + v3 + v1^(-1))^2)
              // Noting that (v2 + v3 + v1^(-1)) = v
              //   (Add) Derivative(f(v1), v1)/v + f(v1)/(v1^2*v^2)

              // > D(S("f(v1)/(1/v1+v2)"), "v1")
              //   (Add)Derivative(f(v1), v1)/(v2 + v1^(-1)) + f(v1)/(v1^2*(v2 + v1^(-1))^2)
              // Noting that (v2 + v1^(-1)) = v
              //   (Add) Derivative(f(v1), v1)/v + f(v1)/(v1^2*v^2)
              Jf(i, 0) = J(i, 0) / v +
                ret0(oral0_, 0)/(theta(1, 0)*theta(1, 0)*v*v);
            } else {
              Jf(i, 0) = -ret0(oral0_, 0)/(v*v) +
                J(i, 0) / v;
            }
          } else {
            Jf(i, 0) = J(i, 0)  / v;
          }
        }
        return Jf;
      }


      double adjustF(const Eigen::VectorXd ret0,
                     const Eigen::Matrix<double, Eigen::Dynamic, 1>& theta) {
        return ret0(oral0_, 0) / getVc(theta);
      }

    };

    // Double initialization need to be outside of the linCmtStan struct
    template <>
    void linCmtStan::saveAlast<double>(Eigen::Matrix<double, Eigen::Dynamic, 1> ret0) const {
      for (int i = 0; i < ncmt_ + oral0_; i++) {
        Asave_[i] = ret0(i, 0);
      }
    }

    template <>
    Eigen::Matrix<double, Eigen::Dynamic, 1> linCmtStan::getAlast(const Eigen::Matrix<double, Eigen::Dynamic, 1>& theta) const {
      Eigen::Matrix<double, Eigen::Dynamic, 1> Alast(ncmt_ + oral0_, 1);
      for (int i = oral0_ + ncmt_; i--;){
        Alast(i, 0) = A_[i];
      }
      return Alast;
    }


  } // namespace math
} // namespace stan

#endif
