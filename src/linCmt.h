#ifndef __LINCMT_H__
#define __LINCMT_H__

#include "macros2micros.h"
#include "solComp.h"

// Global linear compartment model parameters:
// p1, v, p2, p3, p3, p4, ka

#define linCmtNormal 0
#define linCmtSsInf8 1
#define linCmtSsInf 2
#define linCmtSsBolus 3

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
      double tinf_ = 0.0;
      double tau_ = 0.0;
      double bolusAmt_ = 0.0;
      int bolusCmt_ = 0;
      int type_ = 0;
      linCmtStan(const int ncmt,
                 const int oral0,
                 const int trans,
                 const bool grad,
                 const int type) :
        ncmt_(ncmt),
        oral0_(oral0),
        trans_(trans),
        grad_(grad),
        type_(type)
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

      //////////////////////////////////////////////////////////////////
      // Solved One compartment steady state solutions
      //////////////////////////////////////////////////////////////////

      template <typename T>
      Eigen::Matrix<T, Eigen::Dynamic, 1>
      linCmtStan1ssInf8(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                       T ka) const {
#define v     g(0, 0)
#define k     g(0, 1)
        Eigen::Matrix<T, Eigen::Dynamic, 1> ret;
        if (oral0_  == 1) {
          T rDepot = rate_[0];
          T rCentral = rate_[1];
          ret.resize(2, 1);
          if (rate_[0] > 0) {
            ret(0, 0) = rDepot/ka;
            ret(1, 0) = rDepot/k;
            return ret;
          } else if (rate_[1] > 0) {
            ret(0, 0) = 0;
            ret(1, 0) = rCentral/k;
            return ret;
          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            return ret;
          }
        } else {
          ret.resize(1, 1);
          T rCentral = rate_[0];
          if (rate_[0] > 0) {
            ret(0, 0) = rCentral/ka;
          } else {
            ret(0, 0) = NA_REAL;
          }
          return ret;
        }
#undef v
#undef k
      }

      template <typename T>
      Eigen::Matrix<T, Eigen::Dynamic, 1>
      linCmtStan1ssInf(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                       T ka) const {
#define v     g(0, 0)
#define k     g(0, 1)
        Eigen::Matrix<T, Eigen::Dynamic, 1> ret;
        if (oral0_  == 1) {
          T rDepot = rate_[0];
          T rCentral = rate_[1];
          ret.resize(2, 1);
          if (rate_[0] > 0) {
            T eKa     = exp(-(ka)*((tau_)-(tinf_)))/(1.0-exp(-(tau_)*(ka)));
            T eiKa    = exp(-(ka)*(tinf_));
            T eiK     = exp(-(k)*(tinf_));
            T eK      = exp(-(k)*((tau_)-(tinf_)))/(1.0-exp(-(tau_)*(k)));
            ret(0, 0) = eKa*((rDepot)/(ka) - eiKa*(rDepot)/(ka));
            ret(1, 0) = eK*((rDepot)/(k) + eiKa*(rDepot)/(-(k) + (ka)) - eiK*(rDepot)*(ka)/((ka)*(k) - (k)*(k))) + (ka)*(eK - eKa)*((rDepot)/(ka) - eiKa*(rDepot)/(ka))/(-(k) + (ka));
            return ret;
          } else if (rate_[1] > 0) {
            T eiK = exp(-(k)*(tinf_));
            T eK = exp(-(k)*(tau_-tinf_))/(1.0-exp(-(k)*tau_));
            ret(0, 0) = 0.0;
            ret(1, 0) = (rCentral)*(1-eiK)*eK/(k);
            return ret;
          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            return ret;
          }
        } else {
          ret.resize(1, 1);
          T rCentral = rate_[0];
          T eiK = exp(-(k)*(tinf_));
          T eK = exp(-(k)*(tau_-tinf_))/(1.0-exp(-(k)*tau_));
          ret(0, 0) = (rCentral)*(1-eiK)*eK/(k);
          return ret;
        }
#undef v
#undef k
        return ret;
      }

      template <typename T>
      Eigen::Matrix<T, Eigen::Dynamic, 1>
      linCmtStan1ssBolus(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                         T ka) const {
#define v     g(0, 0)
#define k     g(0, 1)
        Eigen::Matrix<T, Eigen::Dynamic, 1> ret;
        if (oral0_  == 1) {
          ret.resize(2, 1);
          if (bolusCmt_ == 0) {
            T eKa = 1.0/(1.0-exp(-(tau_)*(ka)));
            T eK =  1.0/(1.0-exp(-(tau_)*(k)));
            ret(0, 0) = eKa*(bolusAmt_);
            ret(1, 0) = (ka)*(bolusAmt_)*(eK - eKa)/(-(k) + (ka));
            return ret;
          } else if (bolusCmt_ == 1) {
            T eK =  1.0/(1.0-exp(-(tau_)*(k)));
            ret(0, 0)=0.0;
            ret(1, 0)=eK*(bolusAmt_);
            return ret;
          } else {
            ret(0, 0)=NA_REAL;
            ret(1, 0)=NA_REAL;
            return ret;
          }
        } else {
          ret.resize(1, 1);
          if (bolusCmt_ == 0) {
            T eK =  1.0/(1.0-exp(-(tau_)*(k)));
            ret(0, 0)=eK*(bolusAmt_);
            return ret;
          } else {
            ret(0, 0)=NA_REAL;
            return ret;
          }
        }
#undef v
#undef k
        return ret;
      }

      //////////////////////////////////////////////////////////////////
      // Solved One compartment wnl solutions
      //////////////////////////////////////////////////////////////////
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
      //////////////////////////////////////////////////////////////////
      // Solved Two compartment steady state solutions
      //////////////////////////////////////////////////////////////////

      template <typename T>
      Eigen::Matrix<T, Eigen::Dynamic, 1>
      linCmtStan2ssInf8(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                        T ka) const {
#define v     g(0, 0)
#define k     g(0, 1)
#define k20   g(0, 1)

#define k12   g(1, 0)
#define k23   g(1, 0)

#define k21   g(1, 1)
#define k32   g(1, 1)

        Eigen::Matrix<T, Eigen::Dynamic, 1> ret;
        if (oral0_  == 1) {
          T rDepot = rate_[0];
          T rCentral = rate_[1];
          ret.resize(3, 1);
          if (rate_[0] > 0) {
            T s = (k23)+(k32)+(k20);
            T beta  = 0.5*(s - sqrt(s*s - 4*(k32)*(k20)));
            T alpha = (k32)*(k20)/beta;
            ret(0, 0) = rDepot/(ka);
            ret(1, 0) = (rDepot)*(k32)/(beta*alpha);
            ret(2, 0)=(rDepot)*(k23)/(beta*alpha);
            return ret;
          } else if (rate_[1] > 0) {
            T s = (k23)+(k32)+(k20);
            T beta  = 0.5*(s - sqrt(s*s - 4*(k32)*(k20)));
            T alpha = (k32)*(k20)/beta;
            ret(0, 0) = 0;
            ret(1, 0) = (rCentral)*(k32)/(beta*alpha);
            ret(2, 0) = (rCentral)*(k23)/(beta*alpha);
            return ret;
          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            ret(3, 0) = NA_REAL;
            return ret;
          }
        } else {
          ret.resize(2, 1);
          T rCentral = rate_[0];
          if (rate_[0] > 0) {
            T s = (k23)+(k32)+(k20);
            T beta  = 0.5*(s - sqrt(s*s - 4*(k32)*(k20)));
            T alpha = (k32)*(k20)/beta;
            ret(0, 0) = (rCentral)*(k32)/(beta*alpha);
            ret(1, 0) = (rCentral)*(k23)/(beta*alpha);
            return ret;
          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            return ret;
          }
          return ret;
        }
#undef k21
#undef k32
#undef k12
#undef k23
#undef v
#undef k
      }

      template <typename T>
      Eigen::Matrix<T, Eigen::Dynamic, 1>
      linCmtStan2ssInf(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                       T ka) const {
#define v     g(0, 0)
#define k     g(0, 1)
#define k20   g(0, 1)

#define k12   g(1, 0)
#define k23   g(1, 0)

#define k21   g(1, 1)
#define k32   g(1, 1)
        Eigen::Matrix<T, Eigen::Dynamic, 1> ret;
        if (oral0_  == 1) {
          T rDepot = rate_[0];
          T rCentral = rate_[1];
          ret.resize(3, 1);
          if (rate_[0] > 0) {
            T s = (k23)+(k32)+(k20);
            T beta  = 0.5*(s - sqrt(s*s - 4*(k32)*(k20)));
            T alpha = (k32)*(k20)/beta;

            T eA = exp(-alpha*((tau_)-(tinf_)))/(1.0-exp(-alpha*(tau_)));
            T eB = exp(-beta* ((tau_)-(tinf_)))/(1.0-exp(-beta*(tau_)));

            T eiA = exp(-alpha*(tinf_));
            T eiB = exp(-beta *(tinf_));

            T alpha2 = alpha*alpha;
            T alpha3 = alpha2*alpha;

            T beta2 = beta*beta;
            T beta3 = beta2*beta;

            T ka2 = (ka)*(ka);

            T eKa = exp(-(ka)*(tau_)-(tinf_))/(1.0-exp(-(ka)*(tau_)));
            T eiKa = exp(-(ka)*(tinf_));

            ret(0, 0) = eKa*((rDepot)/(ka) - eiKa*(rDepot)/(ka));
            ret(1, 0) = (eA*(-alpha*((rDepot)*(k32)/(beta*alpha) + eiKa*(rDepot)*(-(k32) + (ka))/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(-alpha + (k32))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(-beta + (k32))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k32)*((rDepot)*(k23)/(beta*alpha) - eiKa*(rDepot)*(k23)/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(k23)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(k23)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k32)*((rDepot)*(k32)/(beta*alpha) + eiKa*(rDepot)*(-(k32) + (ka))/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(-alpha + (k32))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(-beta + (k32))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3))) - eB*(-beta*((rDepot)*(k32)/(beta*alpha) + eiKa*(rDepot)*(-(k32) + (ka))/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(-alpha + (k32))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(-beta + (k32))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k32)*((rDepot)*(k23)/(beta*alpha) - eiKa*(rDepot)*(k23)/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(k23)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(k23)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k32)*((rDepot)*(k32)/(beta*alpha) + eiKa*(rDepot)*(-(k32) + (ka))/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(-alpha + (k32))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(-beta + (k32))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3))))/(-alpha + beta) + (ka)*(eA*(-alpha + (k32))/((-alpha + beta)*(-alpha + (ka))) + eB*(-beta + (k32))/((-beta + (ka))*(alpha - beta)) + eKa*((k32) - (ka))/((beta - (ka))*(alpha - (ka))))*((rDepot)/(ka) - eiKa*(rDepot)/(ka));
            ret(2,0) = (eA*(-alpha*((rDepot)*(k23)/(beta*alpha) - eiKa*(rDepot)*(k23)/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(k23)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(k23)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k23)*((rDepot)*(k32)/(beta*alpha) + eiKa*(rDepot)*(-(k32) + (ka))/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(-alpha + (k32))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(-beta + (k32))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + ((k20) + (k23))*((rDepot)*(k23)/(beta*alpha) - eiKa*(rDepot)*(k23)/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(k23)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(k23)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3))) - eB*(-beta*((rDepot)*(k23)/(beta*alpha) - eiKa*(rDepot)*(k23)/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(k23)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(k23)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k23)*((rDepot)*(k32)/(beta*alpha) + eiKa*(rDepot)*(-(k32) + (ka))/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(-alpha + (k32))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(-beta + (k32))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + ((k20) + (k23))*((rDepot)*(k23)/(beta*alpha) - eiKa*(rDepot)*(k23)/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(k23)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(k23)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3))))/(-alpha + beta) + (ka)*(k23)*(eA/((-alpha + beta)*(-alpha + (ka))) + eB/((-beta + (ka))*(alpha - beta)) + eKa/((beta - (ka))*(alpha - (ka))))*((rDepot)/(ka) - eiKa*(rDepot)/(ka));
            return ret;
          } else if (rate_[1] > 0) {
            T E2 = (k20)+(k23);
            T E3 = (k32);
            T s = (k23)+(k32)+(k20);
            T beta  = 0.5*(s - sqrt(s*s - 4*(k32)*(k20)));
            T alpha = (k32)*(k20)/beta;

            T eA = exp(-alpha*((tau_)-(tinf_)))/(1.0-exp(-alpha*(tau_)));
            T eB = exp(-beta*((tau_)-(tinf_)))/(1.0-exp(-beta*(tau_)));

            T eiA = exp(-alpha*(tinf_));
            T eiB = exp(-beta*(tinf_));

            T alpha2 = alpha*alpha;
            T alpha3 = alpha2*alpha;

            T beta2 = beta*beta;
            T beta3 = beta2*beta;
            ret(0, 0) = 0.0;
            ret(1, 0) = (eA*(E3*((rCentral)*(k32)/(beta*alpha) - eiA*(rCentral)*(-(k32)*alpha + (ka)*(-alpha + (k32)) + alpha2)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k32)*beta + (ka)*(-beta + (k32)) + beta2)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) - alpha*((rCentral)*(k32)/(beta*alpha) - eiA*(rCentral)*(-(k32)*alpha + (ka)*(-alpha + (k32)) + alpha2)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k32)*beta + (ka)*(-beta + (k32)) + beta2)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k32)*((rCentral)*(k23)/(beta*alpha) - eiA*(rCentral)*(-(k23)*alpha + (ka)*(k23))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k23)*beta + (ka)*(k23))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3))) - eB*(E3*((rCentral)*(k32)/(beta*alpha) - eiA*(rCentral)*(-(k32)*alpha + (ka)*(-alpha + (k32)) + alpha2)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k32)*beta + (ka)*(-beta + (k32)) + beta2)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) - beta*((rCentral)*(k32)/(beta*alpha) - eiA*(rCentral)*(-(k32)*alpha + (ka)*(-alpha + (k32)) + alpha2)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k32)*beta + (ka)*(-beta + (k32)) + beta2)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k32)*((rCentral)*(k23)/(beta*alpha) - eiA*(rCentral)*(-(k23)*alpha + (ka)*(k23))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k23)*beta + (ka)*(k23))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3))))/(-alpha + beta);
            ret(2, 0)=(eA*(E2*((rCentral)*(k23)/(beta*alpha) - eiA*(rCentral)*(-(k23)*alpha + (ka)*(k23))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k23)*beta + (ka)*(k23))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) - alpha*((rCentral)*(k23)/(beta*alpha) - eiA*(rCentral)*(-(k23)*alpha + (ka)*(k23))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k23)*beta + (ka)*(k23))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k23)*((rCentral)*(k32)/(beta*alpha) - eiA*(rCentral)*(-(k32)*alpha + (ka)*(-alpha + (k32)) + alpha2)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k32)*beta + (ka)*(-beta + (k32)) + beta2)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3))) - eB*(E2*((rCentral)*(k23)/(beta*alpha) - eiA*(rCentral)*(-(k23)*alpha + (ka)*(k23))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k23)*beta + (ka)*(k23))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) - beta*((rCentral)*(k23)/(beta*alpha) - eiA*(rCentral)*(-(k23)*alpha + (ka)*(k23))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k23)*beta + (ka)*(k23))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k23)*((rCentral)*(k32)/(beta*alpha) - eiA*(rCentral)*(-(k32)*alpha + (ka)*(-alpha + (k32)) + alpha2)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k32)*beta + (ka)*(-beta + (k32)) + beta2)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3))))/(-alpha + beta);
            return ret;
          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            ret(2, 0) = NA_REAL;
            return ret;
          }
        } else {
          ret.resize(2, 1);

          T rCentral = rate_[0];

          T E2 = (k20)+(k23);
          T E3 = (k32);
          T s = (k23)+(k32)+(k20);
          T beta  = 0.5*(s - sqrt(s*s - 4*(k32)*(k20)));
          T alpha = (k32)*(k20)/beta;

          T eA = exp(-alpha*((tau_)-(tinf_)))/(1.0-exp(-alpha*(tau_)));
          T eB = exp(-beta*((tau_)-(tinf_)))/(1.0-exp(-beta*(tau_)));

          T eiA = exp(-alpha*(tinf_));
          T eiB = exp(-beta*(tinf_));

          T alpha2 = alpha*alpha;
          T alpha3 = alpha2*alpha;

          T beta2 = beta*beta;
          T beta3 = beta2*beta;

          ret(0, 0) = (eA*(E3*((rCentral)*(k32)/(beta*alpha) - eiA*(rCentral)*(-(k32)*alpha + (ka)*(-alpha + (k32)) + alpha2)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k32)*beta + (ka)*(-beta + (k32)) + beta2)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) - alpha*((rCentral)*(k32)/(beta*alpha) - eiA*(rCentral)*(-(k32)*alpha + (ka)*(-alpha + (k32)) + alpha2)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k32)*beta + (ka)*(-beta + (k32)) + beta2)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k32)*((rCentral)*(k23)/(beta*alpha) - eiA*(rCentral)*(-(k23)*alpha + (ka)*(k23))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k23)*beta + (ka)*(k23))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3))) - eB*(E3*((rCentral)*(k32)/(beta*alpha) - eiA*(rCentral)*(-(k32)*alpha + (ka)*(-alpha + (k32)) + alpha2)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k32)*beta + (ka)*(-beta + (k32)) + beta2)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) - beta*((rCentral)*(k32)/(beta*alpha) - eiA*(rCentral)*(-(k32)*alpha + (ka)*(-alpha + (k32)) + alpha2)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k32)*beta + (ka)*(-beta + (k32)) + beta2)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k32)*((rCentral)*(k23)/(beta*alpha) - eiA*(rCentral)*(-(k23)*alpha + (ka)*(k23))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k23)*beta + (ka)*(k23))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3))))/(-alpha + beta);
          ret(1, 0)=(eA*(E2*((rCentral)*(k23)/(beta*alpha) - eiA*(rCentral)*(-(k23)*alpha + (ka)*(k23))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k23)*beta + (ka)*(k23))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) - alpha*((rCentral)*(k23)/(beta*alpha) - eiA*(rCentral)*(-(k23)*alpha + (ka)*(k23))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k23)*beta + (ka)*(k23))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k23)*((rCentral)*(k32)/(beta*alpha) - eiA*(rCentral)*(-(k32)*alpha + (ka)*(-alpha + (k32)) + alpha2)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k32)*beta + (ka)*(-beta + (k32)) + beta2)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3))) - eB*(E2*((rCentral)*(k23)/(beta*alpha) - eiA*(rCentral)*(-(k23)*alpha + (ka)*(k23))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k23)*beta + (ka)*(k23))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) - beta*((rCentral)*(k23)/(beta*alpha) - eiA*(rCentral)*(-(k23)*alpha + (ka)*(k23))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k23)*beta + (ka)*(k23))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k23)*((rCentral)*(k32)/(beta*alpha) - eiA*(rCentral)*(-(k32)*alpha + (ka)*(-alpha + (k32)) + alpha2)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rCentral)*(-(k32)*beta + (ka)*(-beta + (k32)) + beta2)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3))))/(-alpha + beta);
          return ret;
        }
#undef k21
#undef k32
#undef k12
#undef k23
#undef v
#undef k
        return ret;
      }

      template <typename T>
      Eigen::Matrix<T, Eigen::Dynamic, 1>
      linCmtStan2ssBolus(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                         T ka) const {

#define v     g(0, 0)
#define k     g(0, 1)
#define k20   g(0, 1)

#define k12   g(1, 0)
#define k23   g(1, 0)

#define k21   g(1, 1)
#define k32   g(1, 1)
        Eigen::Matrix<T, Eigen::Dynamic, 1> ret;
        if (oral0_  == 1) {
          ret.resize(3, 1);
          if (bolusCmt_ == 0) {

            T E2 = (k20)+(k23);
            T E3 = (k32);
            T e2e3 = E2+E3;
            T s = sqrt(e2e3*e2e3-4*(E2*E3-(k23)*(k32)));

            T lambda1 = 0.5*(e2e3+s);
            T lambda2 = 0.5*(e2e3-s);
            T eKa=1.0/(1.0-exp(-(tau_)*(ka)));
            T eL1=1.0/(1.0-exp(-(tau_)*lambda1));
            T eL2=1.0/(1.0-exp(-(tau_)*lambda2));

            ret(0, 0) = eKa*(bolusAmt_);
            ret(1, 0) = (ka)*(bolusAmt_)*(eL1*(E3 - lambda1)/((-lambda1 + lambda2)*((ka) - lambda1)) + eL2*(E3 - lambda2)/((lambda1 - lambda2)*((ka) - lambda2)) + eKa*(E3 - (ka))/((-(ka) + lambda2)*(-(ka) + lambda1)));
            ret(2, 0) = (ka)*(bolusAmt_)*(k23)*(eL1/((-lambda1 + lambda2)*((ka) - lambda1)) + eL2/((lambda1 - lambda2)*((ka) - lambda2)) + eKa/((-(ka) + lambda2)*(-(ka) + lambda1)));
            return ret;
          } else if (bolusCmt_ == 1) {

            T E2 = (k20)+(k23);
            T E3 = (k32);
            T e2e3 = E2+E3;
            T s = sqrt(e2e3*e2e3-4*(E2*E3-(k23)*(k32)));

            T lambda1 = 0.5*(e2e3+s);
            T lambda2 = 0.5*(e2e3-s);
            T eL1=1.0/(1.0-exp(-(tau_)*lambda1));
            T eL2=1.0/(1.0-exp(-(tau_)*lambda2));

            ret(0, 0) = 0.0;
            ret(1, 0) = (eL1*((bolusAmt_)*E3 - (bolusAmt_)*lambda1) - eL2*((bolusAmt_)*E3 - (bolusAmt_)*lambda2))/(-lambda1 + lambda2);
            ret(2, 0) = (eL1*(bolusAmt_)*(k23) - eL2*(bolusAmt_)*(k23))/(-lambda1 + lambda2);

            return ret;
          } else {
            ret(0, 0)=NA_REAL;
            ret(1, 0)=NA_REAL;
            ret(2, 0)=NA_REAL;
            return ret;
          }
        } else {
          ret.resize(2, 1);
          if (bolusCmt_ == 0) {

            T E2 = (k20)+(k23);
            T E3 = (k32);
            T e2e3 = E2+E3;
            T s = sqrt(e2e3*e2e3-4*(E2*E3-(k23)*(k32)));

            T lambda1 = 0.5*(e2e3+s);
            T lambda2 = 0.5*(e2e3-s);
            T eL1=1.0/(1.0-exp(-(tau_)*lambda1));
            T eL2=1.0/(1.0-exp(-(tau_)*lambda2));

            ret(0, 0) = (eL1*((bolusAmt_)*E3 - (bolusAmt_)*lambda1) - eL2*((bolusAmt_)*E3 - (bolusAmt_)*lambda2))/(-lambda1 + lambda2);
            ret(1, 0) = (eL1*(bolusAmt_)*(k23) - eL2*(bolusAmt_)*(k23))/(-lambda1 + lambda2);

            return ret;
          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            return ret;
          }
        }
#undef k21
#undef k32
#undef k12
#undef k23
#undef v
#undef k
        return ret;
      }

      //////////////////////////////////////////////////////////////////
      // Solved Two compartment wnl solutions
      //////////////////////////////////////////////////////////////////
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

      //////////////////////////////////////////////////////////////////
      // Solved Three compartment steady state solutions
      //////////////////////////////////////////////////////////////////
      template <typename T>
      Eigen::Matrix<T, Eigen::Dynamic, 1>
      linCmtStan3ssInf8(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                        T ka) const {
#define v     g(0, 0)
        //#define k     g(0, 1)
#define k20   g(0, 1)

#define k12   g(1, 0)
#define k23   g(1, 0)

#define k21   g(1, 1)
#define k32   g(1, 1)

#define k13   g(2, 0)
#define k24   g(2, 0)

#define k31   g(2, 1)
#define k42   g(2, 1)
        Eigen::Matrix<T, Eigen::Dynamic, 1> ret;
        if (oral0_  == 1) {
          T rDepot = rate_[0];
          T rCentral = rate_[1];
          ret.resize(4, 1);
          if (rate_[0] > 0) {
            T E2 =  (k20)+ (k23) + (k24);
            T E3 = (k32);
            T E4 = (k42);
            T j  = (k23) + (k20) + (k32) + (k42) + (k24);
            T k  = (k23)*(k42)+(k20)*(k32)+(k20)*(k42)+(k32)*(k42)+(k24)*(k32);
            T l  = (k20)*(k32)*(k42);

            T m = 0.3333333333333333*(3.0*k - j*j);
            T n = 0.03703703703703703*(2.0*j*j*j - 9.0*j*k + 27.0*l);
            T Q = 0.25*n*n + 0.03703703703703703*m*m*m;

            T alpha = sqrt(-Q);
            T beta = -0.5*n;
            T rho=sqrt(beta*beta+alpha*alpha);
            T theta = atan2(alpha,beta);
            T ct3 = cos(0.3333333333333333*theta);
            T rho3 = pow(rho,0.3333333333333333);
            T st3 = 1.732050807568877193177*sin(0.3333333333333333*theta);
            T j3 = 0.3333333333333333*j;
            T lam1 = j3  + rho3*(ct3 + st3);
            T lam2 = j3 + rho3*(ct3 - st3);
            T lam3 = j3 -(2.0*rho3*ct3);

            T eKa = exp(-(ka)*((tau_)-(tinf_)))/(1.0-exp(-(ka)*(tau_)));
            T eiKa = exp(-(ka)*(tinf_));

            T eL1 = exp(-lam1*((tau_)-(tinf_)))/(1.0-exp(-lam1*(tau_)));
            T eiL1 = exp(-lam1*(tinf_));

            T eL2 = exp(-lam2*((tau_)-(tinf_)))/(1.0-exp(-lam2*(tau_)));
            T eiL2 = exp(-lam2*(tinf_));

            T eL3 = exp(-lam3*((tau_)-(tinf_)))/(1.0-exp(-lam3*(tau_)));
            T eiL3 = exp(-lam3*(tinf_));

            T ka2 = (ka)*(ka);
            T ka3 = ka2*(ka);

            T lam12 = lam1*lam1;
            T lam13 = lam12*lam1;
            T lam14 = lam13*lam1;

            T lam22 = lam2*lam2;
            T lam23 = lam22*lam2;
            T lam24 = lam23*lam2;

            T lam32 = lam3*lam3;
            T lam33 = lam32*lam3;
            T lam34 = lam33*lam3;

            ret(0, 0) = eKa*((rDepot)/(ka) - eiKa*(rDepot)/(ka));
            ret(1, 0) = (eL1*(E4 - lam1)*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E3 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) + (ka)*(eL1*(E4 - lam1)*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)*((ka) - lam1)) + eL2*(E4 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)*((ka) - lam2)) + eL3*(E3 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)*((ka) - lam3)) + eKa*(E3 - (ka))*(E4 - (ka))/((-(ka) + lam1)*(-(ka) + lam2)*(-(ka) + lam3)))*((rDepot)/(ka) - eiKa*(rDepot)/(ka)) + eL1*(-lam1*((k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3))) + E3*(k42)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*(lam3*((k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3))) - (E3*(k42)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*(lam2*((k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3))) - (E3*(k42)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            ret(2, 0)=(eL1*(E4 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E2 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)) + (ka)*(k23)*(eL1*(E4 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)*((ka) - lam1)) + eL2*(E4 - lam2)/((-lam2 + lam3)*(lam1 - lam2)*((ka) - lam2)) + eL3*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)*((ka) - lam3)) + eKa*(E4 - (ka))/((-(ka) + lam1)*(-(ka) + lam2)*(-(ka) + lam3)))*((rDepot)/(ka) - eiKa*(rDepot)/(ka)) + eL1*(E4*(k23)*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*lam1*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(k24)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*((k23)*lam3*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (E4*(k23)*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(k24)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*((k23)*lam2*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (E4*(k23)*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(k24)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            ret(3, 0)=(eL1*(E3 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E2 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E3 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + (ka)*(k24)*(eL1*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)*((ka) - lam1)) + eL2*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)*((ka) - lam2)) + eL3*(E3 - lam3)/((lam2 - lam3)*(lam1 - lam3)*((ka) - lam3)) + eKa*(E3 - (ka))/((-(ka) + lam1)*(-(ka) + lam2)*(-(ka) + lam3)))*((rDepot)/(ka) - eiKa*(rDepot)/(ka)) + eL1*(E3*(k24)*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + (k24)*(k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)) - (k24)*lam1*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*((k24)*lam3*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (E3*(k24)*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + (k24)*(k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*((k24)*lam2*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (E3*(k24)*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + (k24)*(k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            return ret;
          } else if (rate_[1] > 0) {

            T j = (k23)+(k20)+(k32)+(k42)+(k24);
            T k = (k23)*(k42)+(k20)*(k32)+(k20)*(k42)+(k32)*(k42)+(k24)*(k32);
            T l = (k20)*(k32)*(k42);

            T m = 0.3333333333333333*(3.0*k - j*j);
            T n = 0.03703703703703703*(2.0*j*j*j - 9.0*j*k + 27.0*l);
            T Q = 0.25*n*n + 0.03703703703703703*m*m*m;

            T alpha = sqrt(-Q);
            T beta = -0.5*n;
            T rho=sqrt(beta*beta+alpha*alpha);
            T theta = atan2(alpha,beta);
            T ct3 = cos(0.3333333333333333*theta);
            T rho3 = pow(rho,0.3333333333333333);
            T st3 = 1.732050807568877193177*sin(0.3333333333333333*theta);
            T j3 = 0.3333333333333333*j;
            T lam1 = j3  + rho3*(ct3 + st3);
            T lam2 = j3 + rho3*(ct3 - st3);
            T lam3 = j3 -(2.0*rho3*ct3);
            T l123 = 1.0/(lam1*lam2*lam3);

            ret(0, 0)=0;
            ret(1, 0)=(rCentral)*(k42)*(k32)*l123;
            ret(2, 0)=(rCentral)*(k42)*(k23)*l123;
            ret(3, 0)=(rCentral)*(k24)*(k32)*l123;

            return ret;
          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            ret(3, 0) = NA_REAL;
            ret(4, 0) = NA_REAL;
            return ret;
          }
        } else {
          ret.resize(3, 1);
          T rCentral = rate_[0];
          if (rate_[0] > 0) {
            T j = (k23)+(k20)+(k32)+(k42)+(k24);
            T k = (k23)*(k42)+(k20)*(k32)+(k20)*(k42)+(k32)*(k42)+(k24)*(k32);
            T l = (k20)*(k32)*(k42);

            T m = 0.3333333333333333*(3.0*k - j*j);
            T n = 0.03703703703703703*(2.0*j*j*j - 9.0*j*k + 27.0*l);
            T Q = 0.25*n*n + 0.03703703703703703*m*m*m;

            T alpha = sqrt(-Q);
            T beta = -0.5*n;
            T rho=sqrt(beta*beta+alpha*alpha);
            T theta = atan2(alpha,beta);
            T ct3 = cos(0.3333333333333333*theta);
            T rho3 = pow(rho,0.3333333333333333);
            T st3 = 1.732050807568877193177*sin(0.3333333333333333*theta);
            T j3 = 0.3333333333333333*j;
            T lam1 = j3  + rho3*(ct3 + st3);
            T lam2 = j3 + rho3*(ct3 - st3);
            T lam3 = j3 -(2.0*rho3*ct3);
            T l123 = 1.0/(lam1*lam2*lam3);

            ret(0, 0)=(rCentral)*(k42)*(k32)*l123;
            ret(1, 0)=(rCentral)*(k42)*(k23)*l123;
            ret(2, 0)=(rCentral)*(k24)*(k32)*l123;

            return ret;

          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            ret(2, 0) = NA_REAL;

            return ret;
          }
          return ret;
        }

#undef k13
#undef k24
#undef k31
#undef k42
#undef k21
#undef k32
#undef k12
#undef k23
#undef v
        //#undef k
      }

      template <typename T>
      Eigen::Matrix<T, Eigen::Dynamic, 1>
      linCmtStan3ssInf(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                       T ka) const {
#define v     g(0, 0)
#define k20   g(0, 1)

#define k12   g(1, 0)
#define k23   g(1, 0)

#define k21   g(1, 1)
#define k32   g(1, 1)

#define k13   g(2, 0)
#define k24   g(2, 0)

#define k31   g(2, 1)
#define k42   g(2, 1)
        Eigen::Matrix<T, Eigen::Dynamic, 1> ret;
        if (oral0_  == 1) {
          T rDepot = rate_[0];
          T rCentral = rate_[1];
          ret.resize(4, 1);
          if (rate_[0] > 0) {
            T E2 =  (k20)+ (k23) + (k24);
            T E3 = (k32);
            T E4 = (k42);
            T j = (k23)+(k20)+(k32)+(k42)+(k24);
            T k = (k23)*(k42)+(k20)*(k32)+(k20)*(k42)+(k32)*(k42)+(k24)*(k32);
            T l = (k20)*(k32)*(k42);

            T m = 0.3333333333333333*(3.0*k - j*j);
            T n = 0.03703703703703703*(2.0*j*j*j - 9.0*j*k + 27.0*l);
            T Q = 0.25*n*n + 0.03703703703703703*m*m*m;

            T alpha = sqrt(-Q);
            T beta = -0.5*n;
            T rho=sqrt(beta*beta+alpha*alpha);
            T theta = atan2(alpha,beta);
            T ct3 = cos(0.3333333333333333*theta);
            T rho3 = pow(rho,0.3333333333333333);
            T st3 = 1.732050807568877193177*sin(0.3333333333333333*theta);
            T j3 = 0.3333333333333333*j;
            T lam1 = j3  + rho3*(ct3 + st3);
            T lam2 = j3 + rho3*(ct3 - st3);
            T lam3 = j3 -(2.0*rho3*ct3);

            T eKa = exp(-(ka)*((tau_)-(tinf_)))/(1.0-exp(-(ka)*(tau_)));
            T eiKa = exp(-(ka)*(tinf_));

            T eL1 = exp(-lam1*((tau_)-(tinf_)))/(1.0-exp(-lam1*(tau_)));
            T eiL1 = exp(-lam1*(tinf_));

            T eL2 = exp(-lam2*((tau_)-(tinf_)))/(1.0-exp(-lam2*(tau_)));
            T eiL2 = exp(-lam2*(tinf_));

            T eL3 = exp(-lam3*((tau_)-(tinf_)))/(1.0-exp(-lam3*(tau_)));
            T eiL3 = exp(-lam3*(tinf_));

            T ka2 = (ka)*(ka);
            T ka3 = ka2*(ka);

            T lam12 = lam1*lam1;
            T lam13 = lam12*lam1;
            T lam14 = lam13*lam1;

            T lam22 = lam2*lam2;
            T lam23 = lam22*lam2;
            T lam24 = lam23*lam2;

            T lam32 = lam3*lam3;
            T lam33 = lam32*lam3;
            T lam34 = lam33*lam3;

            ret(0, 0) = eKa*((rDepot)/(ka) - eiKa*(rDepot)/(ka));
            ret(1, 0) = (eL1*(E4 - lam1)*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E3 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) + (ka)*(eL1*(E4 - lam1)*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)*((ka) - lam1)) + eL2*(E4 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)*((ka) - lam2)) + eL3*(E3 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)*((ka) - lam3)) + eKa*(E3 - (ka))*(E4 - (ka))/((-(ka) + lam1)*(-(ka) + lam2)*(-(ka) + lam3)))*((rDepot)/(ka) - eiKa*(rDepot)/(ka)) + eL1*(-lam1*((k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3))) + E3*(k42)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*(lam3*((k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3))) - (E3*(k42)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*(lam2*((k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3))) - (E3*(k42)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            ret(2, 0) = (eL1*(E4 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E2 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)) + (ka)*(k23)*(eL1*(E4 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)*((ka) - lam1)) + eL2*(E4 - lam2)/((-lam2 + lam3)*(lam1 - lam2)*((ka) - lam2)) + eL3*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)*((ka) - lam3)) + eKa*(E4 - (ka))/((-(ka) + lam1)*(-(ka) + lam2)*(-(ka) + lam3)))*((rDepot)/(ka) - eiKa*(rDepot)/(ka)) + eL1*(E4*(k23)*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*lam1*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(k24)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*((k23)*lam3*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (E4*(k23)*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(k24)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*((k23)*lam2*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (E4*(k23)*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(k24)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            ret(3, 0) = (eL1*(E3 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E2 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E3 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + (ka)*(k24)*(eL1*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)*((ka) - lam1)) + eL2*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)*((ka) - lam2)) + eL3*(E3 - lam3)/((lam2 - lam3)*(lam1 - lam3)*((ka) - lam3)) + eKa*(E3 - (ka))/((-(ka) + lam1)*(-(ka) + lam2)*(-(ka) + lam3)))*((rDepot)/(ka) - eiKa*(rDepot)/(ka)) + eL1*(E3*(k24)*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + (k24)*(k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)) - (k24)*lam1*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*((k24)*lam3*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (E3*(k24)*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + (k24)*(k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*((k24)*lam2*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (E3*(k24)*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + (k24)*(k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            return ret;
          } else if (rate_[1] > 0) {
            T E2 =  (k20)+ (k23) + (k24);
            T E3 = (k32);
            T E4 = (k42);
            T j = (k23)+(k20)+(k32)+(k42)+(k24);
            T k = (k23)*(k42)+(k20)*(k32)+(k20)*(k42)+(k32)*(k42)+(*k24)*(k32);
            T l = (k20)*(k32)*(k42);

            T m = 0.3333333333333333*(3.0*k - j*j);
            T n = 0.03703703703703703*(2.0*j*j*j - 9.0*j*k + 27.0*l);
            T Q = 0.25*n*n + 0.03703703703703703*m*m*m;

            T alpha = sqrt(-Q);
            T beta = -0.5*n;
            T rho=sqrt(beta*beta+alpha*alpha);
            T theta = atan2(alpha,beta);
            T ct3 = cos(0.3333333333333333*theta);
            T rho3 = pow(rho,0.3333333333333333);
            T st3 = 1.732050807568877193177*sin(0.3333333333333333*theta);
            T j3 = 0.3333333333333333*j;
            T lam1 = j3  + rho3*(ct3 + st3);
            T lam2 = j3 + rho3*(ct3 - st3);
            T lam3 = j3 -(2.0*rho3*ct3);

            T eL1 = exp(-lam1*((tau_)-(tinf_)))/(1.0-exp(-lam1*(tau_)));
            T eiL1 = exp(-lam1*(tinf_));

            T eL2 = exp(-lam2*((tau_)-(tinf_)))/(1.0-exp(-lam2*(tau_)));
            T eiL2 = exp(-lam2*(tinf_));

            T eL3 = exp(-lam3*((tau_)-(tinf_)))/(1.0-exp(-lam3*(tau_)));
            T eiL3 = exp(-lam3*(tinf_));

            T lam12 = lam1*lam1;
            T lam13 = lam12*lam1;
            T lam14 = lam13*lam1;

            T lam22 = lam2*lam2;
            T lam23 = lam22*lam2;
            T lam24 = lam23*lam2;

            T lam32 = lam3*lam3;
            T lam33 = lam32*lam3;
            T lam34 = lam33*lam3;
            ret(0, 0) = 0.0;
            ret(1, 0) =(eL1*(E4 - lam1)*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E3 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) + eL1*(-lam1*((k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3))) + E3*(k42)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*(lam3*((k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3))) - (E3*(k42)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*(lam2*((k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3))) - (E3*(k42)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            ret(2, 0)=(eL1*(E4 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E2 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) + eL1*(E4*(k23)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*lam1*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(*k24)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*((k23)*lam3*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (E4*(k23)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(*k24)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*((k23)*lam2*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (E4*(k23)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(*k24)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            ret(3, 0)=(eL1*(E3 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E2 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E3 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) + eL1*(E3*(*k24)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) + (*k24)*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) - (*k24)*lam1*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*((*k24)*lam3*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (E3*(*k24)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) + (*k24)*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*((*k24)*lam2*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (E3*(*k24)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) + (*k24)*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            return ret;
          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            ret(3, 0) = NA_REAL;
            ret(4, 0) = NA_REAL;
            return ret;
          }
        } else {
          ret.resize(3, 1);
          T rCentral = rate_[0];
          if (rate_[0] > 0) {
            T E2 =  (k20)+ (k23) + (k24);
            T E3 = (k32);
            T E4 = (k42);
            T j = (k23)+(k20)+(k32)+(k42)+(k24);
            T k = (k23)*(k42)+(k20)*(k32)+(k20)*(k42)+(k32)*(k42)+(*k24)*(k32);
            T l = (k20)*(k32)*(k42);

            T m = 0.3333333333333333*(3.0*k - j*j);
            T n = 0.03703703703703703*(2.0*j*j*j - 9.0*j*k + 27.0*l);
            T Q = 0.25*n*n + 0.03703703703703703*m*m*m;

            T alpha = sqrt(-Q);
            T beta = -0.5*n;
            T rho=sqrt(beta*beta+alpha*alpha);
            T theta = atan2(alpha,beta);
            T ct3 = cos(0.3333333333333333*theta);
            T rho3 = pow(rho,0.3333333333333333);
            T st3 = 1.732050807568877193177*sin(0.3333333333333333*theta);
            T j3 = 0.3333333333333333*j;
            T lam1 = j3  + rho3*(ct3 + st3);
            T lam2 = j3 + rho3*(ct3 - st3);
            T lam3 = j3 -(2.0*rho3*ct3);

            T eL1 = exp(-lam1*((tau_)-(tinf_)))/(1.0-exp(-lam1*(tau_)));
            T eiL1 = exp(-lam1*(tinf_));

            T eL2 = exp(-lam2*((tau_)-(tinf_)))/(1.0-exp(-lam2*(tau_)));
            T eiL2 = exp(-lam2*(tinf_));

            T eL3 = exp(-lam3*((tau_)-(tinf_)))/(1.0-exp(-lam3*(tau_)));
            T eiL3 = exp(-lam3*(tinf_));

            T lam12 = lam1*lam1;
            T lam13 = lam12*lam1;
            T lam14 = lam13*lam1;

            T lam22 = lam2*lam2;
            T lam23 = lam22*lam2;
            T lam24 = lam23*lam2;

            T lam32 = lam3*lam3;
            T lam33 = lam32*lam3;
            T lam34 = lam33*lam3;
            ret(0, 0) =(eL1*(E4 - lam1)*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E3 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) + eL1*(-lam1*((k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3))) + E3*(k42)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*(lam3*((k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3))) - (E3*(k42)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*(lam2*((k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3))) - (E3*(k42)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            ret(1, 0)=(eL1*(E4 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E2 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) + eL1*(E4*(k23)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*lam1*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(*k24)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*((k23)*lam3*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (E4*(k23)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(*k24)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*((k23)*lam2*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (E4*(k23)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(*k24)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            ret(2, 0)=(eL1*(E3 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E2 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E3 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) + eL1*(E3*(*k24)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) + (*k24)*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) - (*k24)*lam1*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*((*k24)*lam3*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (E3*(*k24)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) + (*k24)*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*((*k24)*lam2*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (E3*(*k24)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiL1*(rCentral)*((*k24)*lam12 + lam1*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((*k24)*lam22 + lam2*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((*k24)*lam32 + lam3*(-(*k24)*(k32) - (ka)*(*k24)) + (ka)*(*k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(*k24)*(k32)/(lam2*lam1*lam3)) + (*k24)*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            return ret;
          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            ret(2, 0) = NA_REAL;
            return ret;
          }
          return ret;
        }

#undef k13
#undef k24
#undef k31
#undef k42
#undef k21
#undef k32
#undef k12
#undef k23
#undef v
      }


      template <typename T>
      Eigen::Matrix<T, Eigen::Dynamic, 1>
      linCmtStan3ssBolus(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                         T ka) const {
#define v     g(0, 0)
#define k20   g(0, 1)

#define k12   g(1, 0)
#define k23   g(1, 0)

#define k21   g(1, 1)
#define k32   g(1, 1)

#define k13   g(2, 0)
#define k24   g(2, 0)

#define k31   g(2, 1)
#define k42   g(2, 1)

        Eigen::Matrix<T, Eigen::Dynamic, 1> ret;
        if (oral0_  == 1) {
          ret.resize(4, 1);
          if (bolusCmt_ == 0) {
            T E2 = (k20)+(k23)+(k24);
            T E3 = (k32);
            T E4 = (k42);

            T a = E2+E3+E4;
            T b = E2*E3+E4*(E2+E3)-(k23)*(k32)-(k24)*(k42);
            T c = E2*E3*E4-E4*(k23)*(k32)-E3*(k24)*(k42);

            T a2 = a*a;
            T m = 0.333333333333333*(3.0*b - a2);
            T n = 0.03703703703703703*(2.0*a2*a - 9.0*a*b + 27.0*c);
            T Q = 0.25*(n*n) + 0.03703703703703703*(m*m*m);

            T alpha = sqrt(-Q);
            T beta = -0.5*n;
            T gamma = sqrt(beta*beta+alpha*alpha);
            T theta = atan2(alpha,beta);
            T theta3 = 0.333333333333333*theta;
            T ctheta3 = cos(theta3);
            T stheta3 = 1.7320508075688771932*sin(theta3);
            T gamma3 = pow(gamma,0.333333333333333);

            T lambda1 = 0.333333333333333*a + gamma3*(ctheta3 + stheta3);
            T lambda2 = 0.333333333333333*a + gamma3*(ctheta3 -stheta3);
            T lambda3 = 0.333333333333333*a -(2.0*gamma3*ctheta3);

            T eKa = 1.0/(1.0-exp(-(tau_)*(*ka)));
            T eL1 = 1.0/(1.0-exp(-(tau_)*lambda1));
            T eL2 = 1.0/(1.0-exp(-(tau_)*lambda2));
            T eL3 = 1.0/(1.0-exp(-(tau_)*lambda3));

            ret(0, 0) = eKa*(bolusAmt_);
            ret(1, 0) = (ka)*(bolusAmt_)*(eL1*(E3 - lambda1)*(E4 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*((ka) - lambda1)) + eL2*(E3 - lambda2)*(E4 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*((ka) - lambda2)) + eL3*(E3 - lambda3)*(E4 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*((ka) - lambda3)) + eKa*(E3 - (ka))*(E4 - (ka))/((-(ka) + lambda1)*(-(ka) + lambda3)*(-(ka) + lambda2)));
            ret(2, 0) = (ka)*(bolusAmt_)*(k23)*(eL1*(E4 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*((ka) - lambda1)) + eL2*(E4 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*((ka) - lambda2)) + eL3*(E4 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*((ka) - lambda3)) + eKa*(E4 - (ka))/((-(ka) + lambda1)*(-(ka) + lambda3)*(-(ka) + lambda2)));
            ret(3, 0) = (ka)*(bolusAmt_)*(k24)*(eL1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*((ka) - lambda1)) + eL2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*((ka) - lambda2)) + eL3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*((ka) - lambda3)) + eKa*(E3 - (ka))/((-(ka) + lambda1)*(-(ka) + lambda3)*(-(ka) + lambda2)));
            return ret;
          } else if (bolusCmt_ == 1) {

            T E2 = (k20)+(k23)+(k24);
            T E3 = (k32);
            T E4 = (k42);

            T a = E2+E3+E4;
            T b = E2*E3+E4*(E2+E3)-(k23)*(k32)-(k24)*(k42);
            T c = E2*E3*E4-E4*(k23)*(k32)-E3*(k24)*(k42);

            T a2 = a*a;
            T m = 0.333333333333333*(3.0*b - a2);
            T n = 0.03703703703703703*(2.0*a2*a - 9.0*a*b + 27.0*c);
            T Q = 0.25*(n*n) + 0.03703703703703703*(m*m*m);

            T alpha = sqrt(-Q);
            T beta = -0.5*n;
            T gamma = sqrt(beta*beta+alpha*alpha);
            T theta = atan2(alpha,beta);
            T theta3 = 0.333333333333333*theta;
            T ctheta3 = cos(theta3);
            T stheta3 = 1.7320508075688771932*sin(theta3);
            T gamma3 = pow(gamma,0.333333333333333);

            T lambda1 = 0.333333333333333*a + gamma3*(ctheta3 + stheta3);
            T lambda2 = 0.333333333333333*a + gamma3*(ctheta3 -stheta3);
            T lambda3 = 0.333333333333333*a -(2.0*gamma3*ctheta3);

            T eL1 = 1.0/(1.0-exp(-(tau_)*lambda1));
            T eL2 = 1.0/(1.0-exp(-(tau_)*lambda2));
            T eL3 = 1.0/(1.0-exp(-(tau_)*lambda3));

            ret(0, 0)=0.0;
            ret(1, 0)=(bolusAmt_)*(eL1*(E3 - lambda1)*(E4 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)) + eL2*(E3 - lambda2)*(E4 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)) + eL3*(E3 - lambda3)*(E4 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)));
            ret(2, 0)=eL2*(-(bolusAmt_)*E4*(k23) + (bolusAmt_)*(k23)*lambda2)/((lambda1 - lambda2)*(lambda2 - lambda3)) + eL1*((bolusAmt_)*E4*(k23) - (bolusAmt_)*(k23)*lambda1)/((lambda1 - lambda3)*(lambda1 - lambda2)) + eL3*(-(bolusAmt_)*E4*(k23) + (bolusAmt_)*(k23)*lambda3)/((lambda1 - lambda3)*(-lambda2 + lambda3));
            ret(3, 0)=eL2*(-(bolusAmt_)*E3*(k24) + (bolusAmt_)*(k24)*lambda2)/((lambda1 - lambda2)*(lambda2 - lambda3)) + eL1*((bolusAmt_)*E3*(k24) - (bolusAmt_)*(k24)*lambda1)/((lambda1 - lambda3)*(lambda1 - lambda2)) + eL3*(-(bolusAmt_)*E3*(k24) + (bolusAmt_)*(k24)*lambda3)/((lambda1 - lambda3)*(-lambda2 + lambda3));
            return ret;
          } else {
            ret(0, 0)=NA_REAL;
            ret(1, 0)=NA_REAL;
            ret(2, 0)=NA_REAL;
            ret(3, 0)=NA_REAL;
            return ret;
          }
        } else {
          ret.resize(3, 1);
          if (bolusCmt_ == 0) {
            T E2 = (k20)+(k23)+(k24);
            T E3 = (k32);
            T E4 = (k42);

            T a = E2+E3+E4;
            T b = E2*E3+E4*(E2+E3)-(k23)*(k32)-(k24)*(k42);
            T c = E2*E3*E4-E4*(k23)*(k32)-E3*(k24)*(k42);

            T a2 = a*a;
            T m = 0.333333333333333*(3.0*b - a2);
            T n = 0.03703703703703703*(2.0*a2*a - 9.0*a*b + 27.0*c);
            T Q = 0.25*(n*n) + 0.03703703703703703*(m*m*m);

            T alpha = sqrt(-Q);
            T beta = -0.5*n;
            T gamma = sqrt(beta*beta+alpha*alpha);
            T theta = atan2(alpha,beta);
            T theta3 = 0.333333333333333*theta;
            T ctheta3 = cos(theta3);
            T stheta3 = 1.7320508075688771932*sin(theta3);
            T gamma3 = pow(gamma,0.333333333333333);

            T lambda1 = 0.333333333333333*a + gamma3*(ctheta3 + stheta3);
            T lambda2 = 0.333333333333333*a + gamma3*(ctheta3 -stheta3);
            T lambda3 = 0.333333333333333*a -(2.0*gamma3*ctheta3);

            T eL1 = 1.0/(1.0-exp(-(tau_)*lambda1));
            T eL2 = 1.0/(1.0-exp(-(tau_)*lambda2));
            T eL3 = 1.0/(1.0-exp(-(tau_)*lambda3));

            ret(0, 0)=(bolusAmt_)*(eL1*(E3 - lambda1)*(E4 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)) + eL2*(E3 - lambda2)*(E4 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)) + eL3*(E3 - lambda3)*(E4 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)));
            ret(1, 0)=eL2*(-(bolusAmt_)*E4*(k23) + (bolusAmt_)*(k23)*lambda2)/((lambda1 - lambda2)*(lambda2 - lambda3)) + eL1*((bolusAmt_)*E4*(k23) - (bolusAmt_)*(k23)*lambda1)/((lambda1 - lambda3)*(lambda1 - lambda2)) + eL3*(-(bolusAmt_)*E4*(k23) + (bolusAmt_)*(k23)*lambda3)/((lambda1 - lambda3)*(-lambda2 + lambda3));
            ret(2, 0)=eL2*(-(bolusAmt_)*E3*(k24) + (bolusAmt_)*(k24)*lambda2)/((lambda1 - lambda2)*(lambda2 - lambda3)) + eL1*((bolusAmt_)*E3*(k24) - (bolusAmt_)*(k24)*lambda1)/((lambda1 - lambda3)*(lambda1 - lambda2)) + eL3*(-(bolusAmt_)*E3*(k24) + (bolusAmt_)*(k24)*lambda3)/((lambda1 - lambda3)*(-lambda2 + lambda3));
            return ret;
          } else {
            ret(0, 0)=NA_REAL;
            ret(1, 0)=NA_REAL;
            ret(2, 0)=NA_REAL;
            return ret;
          }
        }

#undef k13
#undef k24
#undef k31
#undef k42
#undef k21
#undef k32
#undef k12
#undef k23
#undef v
        return ret;
      }

      //////////////////////////////////////////////////////////////////
      // Solved Three compartment wnl solutions
      //////////////////////////////////////////////////////////////////

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
        Eigen::Matrix<T, Eigen::Dynamic, 1> ret0(ncmt_ + oral0_, 1);
        switch(type_) {
        case linCmtNormal:
          {
            Eigen::Matrix<T, Eigen::Dynamic, 1> yp(ncmt_ + oral0_, 1);
            yp = getAlast(theta);
            if (ncmt_ == 1) {
              ret0 = linCmtStan1<T>(g, yp, ka);
            } else if (ncmt_ == 2) {
              ret0 = linCmtStan2<T>(g, yp, ka);
            } else if (ncmt_ == 3) {
              ret0 = linCmtStan3<T>(g, yp, ka);
            }
          }
          break;
        case linCmtSsInf8:
          {
            Eigen::Matrix<T, Eigen::Dynamic, 1> ret0(ncmt_ + oral0_, 1);
            if (ncmt_ == 1) {
              ret0 = linCmtStan1ssInf8(g, ka);
            } else if (ncmt_ == 2) {
              ret0 = linCmtStan2ssInf8(g, ka);
            } else if (ncmt_ == 3) {
              ret0 = linCmtStan2ssInf8(g, ka);
            }
          }
        case linCmtSsInf:
          {
            Eigen::Matrix<T, Eigen::Dynamic, 1> ret0(ncmt_ + oral0_, 1);
            if (ncmt_ == 1) {
              ret0 = linCmtStan1ssInf(g, ka);
            } else if (ncmt_ == 2) {
              ret0 = linCmtStan2ssInf(g, ka);
            } else if (ncmt_ == 3) {
              ret0 = linCmtStan2ssInf(g, ka);
            }
          }
        case linCmtSsBolus:
          {
            Eigen::Matrix<T, Eigen::Dynamic, 1> ret0(ncmt_ + oral0_, 1);
            if (ncmt_ == 1) {
              ret0 = linCmtStan1ssBolus(g, ka);
            } else if (ncmt_ == 2) {
              ret0 = linCmtStan2ssBolus(g, ka);
            } else if (ncmt_ == 3) {
              ret0 = linCmtStan2ssBolus(g, ka);
            }
          }
        }
        saveAlast<T>(ret0);
        return ret0;
      }

      double getVc(const Eigen::Matrix<double, Eigen::Dynamic, 1>& theta) {
        int sw = ncmt_*100 + trans_;
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
          return 1.0/(theta(1, 0) + theta(3, 0));
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
          return theta(1, 0);
          break;
        default:
          REprintf("Unknown linCmt; sw: %d cmt: %d trans: %d\n", sw, ncmt_, trans_);
          return NA_REAL;
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
