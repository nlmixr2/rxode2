#ifndef __LINCMT_H__
#define __LINCMT_H__

#include "macros2micros.h"
#include "solComp.h"
#include "linCmtDiffConstant.h"

#define min2( a , b )  ( (a) < (b) ? (a) : (b) )
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )


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
      int ncmt_, oral0_, trans_;
      double *rate_; // This comes from the ode system
      double *A_;    // This comes from the ode system
      double *Asave_; // This comes from the ode system
      double dt_;
      const bool grad_;
      double tinf_ = 0.0;
      double tau_ = 0.0;
      double bolusAmt_ = 0.0;
      int bolusCmt_ = 0;
      int type_ = 0;
      int numDiff_ = 0;

      bool isAD_ = false;
      bool scaleSetup_ = false;

      Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> J_;
      Eigen::Matrix<double, Eigen::Dynamic, 1> AlastA_;
      Eigen::Matrix<double, Eigen::Dynamic, 1> yp_;
      Eigen::Matrix<double, Eigen::Dynamic, 2> g_;

      Eigen::Matrix<double, Eigen::Dynamic, 1> trueTheta_;
      Eigen::Matrix<double, Eigen::Dynamic, 1> scaleC_;
      Eigen::Matrix<double, Eigen::Dynamic, 1> initPar_;

      Eigen::Matrix<double, Eigen::Dynamic, 1> fx_;
      bool fxIsZero_ = false;

      double c1_, c2_;

      //' The initialization of this class
      //'
      //' @param ncmt The number of compartments
      //'
      //' @param oral0 A indicator of 0 or 1 saying if this was an oral dose
      //'
      //' @param trans the transformation id
      //'
      //' @param grad A boolean saying if this is a gradient calculation
      //'
      //' @param type The type of steady-state calculation
      //'
      //' @param numDiff The number Jacobian differences calculated
      //'
      linCmtStan(const int ncmt,
                 const int oral0,
                 const int trans,
                 const bool grad,
                 const int type,
                 const int numDiff) :
        ncmt_(ncmt),
        oral0_(oral0),
        trans_(trans),
        grad_(grad),
        type_(type),
        numDiff_(numDiff)
      { }

      //' The initialization of this class without arguments
      //'
      linCmtStan() :
        ncmt_(0),
        oral0_(0),
        trans_(0),
        grad_(false),
        type_(0),
        numDiff_(0)
      { }

      // This resents the flags so that scales ar
      void resetFlags() {
        isAD_ = false;
        scaleSetup_ = false;
      }


      // set the steady-state help

      //' Set the current Steady-state type
      //'
      //' @param type The type of steady-state calculation
      //'
      void setSsType(const int type) {
        type_ = type;
      }

      //' Resize the model
      //'
      void resizeModel() {
        J_.resize(ncmt_ + oral0_, getNpars());
        AlastA_.resize(ncmt_ + oral0_);
      }

      //' Set the model type
      //'
      //' @param ncmt The number of compartments
      //'
      //' @param oral0 A indicator of 0 or 1 saying if this was an oral dose
      //'
      //' @param trans the translation type for the model
      //'
      //' @param type The type of steady-state calculation
      //'
      //' @param numDiff The number of differenes calculated for Jacobians
      //'
      void setModelType(const int ncmt, const int oral0, const int trans, const int type,
                        const int numDiff) {
        // The cached variables need to expire
        ncmt_ = ncmt;
        oral0_ = oral0;
        trans_ = trans;
        type_  = type;
        numDiff_ = numDiff;
        if (grad_) {
          resizeModel();
        }
      }

      //' Get the number of sensitivity parameters
      //'
      //' @return The number of sensitivity parameters
      //'
      int numSens() {
        if (numDiff_ == 0) {
          return 2*ncmt_ + 1;
        }
        int ka = (diffKa & numDiff_) != 0;
        int p1 = (diffP1 & numDiff_) != 0;
        int v1 = (diffV1 & numDiff_) != 0;
        int p2 = (diffP2 & numDiff_) != 0;
        int p3 = (diffP3 & numDiff_) != 0;
        int p4 = (diffP4 & numDiff_) != 0;
        int p5 = (diffP5 & numDiff_) != 0;
        int sw = ncmt_ + 10*oral0_;
        switch (sw) {
        case 11: return ka+p1+v1;
        case 12: return ka+p1+v1+p2+p3;
        case 13: return ka+p1+v1+p2+p3+p4+p5;
        case 1: return p1+v1;
        case 2: return p1+v1+p2+p3;
        case 3: return p1+v1+p2+p3+p4+p5;
        }
        return 0;
      }


      //' This sets up the sensitivity theta
      //'
      //' This assumes that sensTheta will be updated as well as `i` and
      //' `j` or `mn` or `mx`
      //'
      //' @param d The integer that helps if this is a sensitivity parameter
      //'      that we will estimate the gradient of... (binary & determines if it is)
      //'
      //' @param theta The full, un-scaled parameter for the linear compartment model.
      //'
      //' @param sensTheta The sensitivity theta parameter that will be updated.  This
      //'      will be scaled.
      //'
      //' @param nd the current sensitivity number that determines which parameters
      //'    the gradient is provedid
      //'
      //' @param i The current index of the sensitivity parameter being updated
      //'
      //' @param j The current index of the theta parameter being queried for
      //'        sensitivity parameter.
      //'
      //' @param mn The minimum value of the sensitivity parameter.
      //'
      //' @param mx The maximum value of the sensitivity parameter.
      //'
      //'  This also updates `initPar_` and `scaleC_` for scaling.
      //'
      //'  If the scaling is already setup, (scaleSetup_ == true) then
      //'  the scaled sensitivity is calculated.
      //'
      void sensThetaElt(int d,
                        const Eigen::Matrix<double, Eigen::Dynamic, 1> theta,
                        Eigen::Matrix<double, Eigen::Dynamic, 1>& sensTheta,
                        int nd, int& i, int& j,
                        double &mn, double &mx) {
        if ((nd & d) != 0) {
          // mn = min2(scale->initPar[k],mn);
          // mx = max2(scale->initPar[k],mx);
          if (isAD_) {
            sensTheta(i, 0) = theta(j, 0);
          } else {
            if (scaleSetup_) {
              sensTheta(i, 0) =
                (theta(j, 0) - initPar_(i, 0))/scaleC_(i, 0) +
                (initPar_(i, 0) - c1_)/c2_;
            } else {
              initPar_(i, 0) = theta(j, 0);
              if (d == diffV1) {
                scaleC_(i, 0) = 1.0 / theta(j, 0);
              } else if (d == diffP3) {
                scaleC_(i, 0) = 1.0 / (theta(j, 0)*theta(j, 0));
              } else {
                scaleC_(i, 0) = 1.0;
              }
              mn = min2(theta(j, 0), mn);
              mx = max2(theta(j, 0), mx);
            }
          }
          i++;
        }
        j++;
      }

      //' Get the sensitivity theta parameters
      //'
      //' This is used for the Jacobian using stan math; this allows the
      //' full theta to be the calculation, but have a narrowed number of parameters
      //' for a Jacobian calculation.
      //'
      //' @param theta -- full theta matrix
      //'
      //' @param sensTheta -- The sensitivity theta matrix
      //'
      //' @param isAD -- A boolean saying if this is an Automatic Differentiation (AD)
      //'         calculation
      //'
      void sensTheta(const Eigen::Matrix<double, Eigen::Dynamic, 1> theta,
                     Eigen::Matrix<double, Eigen::Dynamic, 1>& sensTheta,
                     bool isAD) {
        trueTheta_ = theta;
        isAD_ = isAD;
        int nd = numDiff_;
        if (nd == 0) nd = 127; // all terms
        int i = 0, j=0;

        if (!isAD && !scaleSetup_) {
          // Setup the sizes for scales
          initPar_.resize(sensTheta.size());
          scaleC_.resize(sensTheta.size());
        }

        double mx=R_NegInf, mn = R_PosInf;

        switch (ncmt_) {
        case 1: {
          sensThetaElt(diffP1, theta, sensTheta, nd, i, j, mn, mx);
          sensThetaElt(diffV1, theta, sensTheta, nd, i, j, mn, mx);
          if (oral0_) sensThetaElt(diffKa, theta, sensTheta, nd, i, j, mn, mx);
        }
          break;
        case 2: {
          sensThetaElt(diffP1, theta, sensTheta, nd, i, j, mn, mx);
          sensThetaElt(diffV1, theta, sensTheta, nd, i, j, mn, mx);
          sensThetaElt(diffP2, theta, sensTheta, nd, i, j, mn, mx);
          sensThetaElt(diffP3, theta, sensTheta, nd, i, j, mn, mx);
          if (oral0_) sensThetaElt(diffKa, theta, sensTheta, nd, i, j, mn, mx);
        }
          break;
        case 3: {
          sensThetaElt(diffP1, theta, sensTheta, nd, i, j, mn, mx);
          sensThetaElt(diffV1, theta, sensTheta, nd, i, j, mn, mx);
          sensThetaElt(diffP2, theta, sensTheta, nd, i, j, mn, mx);
          sensThetaElt(diffP3, theta, sensTheta, nd, i, j, mn, mx);
          sensThetaElt(diffP4, theta, sensTheta, nd, i, j, mn, mx);
          sensThetaElt(diffP5, theta, sensTheta, nd, i, j, mn, mx);
          if (oral0_) sensThetaElt(diffKa, theta, sensTheta, nd, i, j, mn, mx);
          }
          break;
        }
        // This finishes the scaling setup
        if (!isAD_ && !scaleSetup_) {
          if (fabs(mx-mn) < DBL_EPSILON) {
            c1_ = 0.0;
            c2_ = 1.0;
          } else {
            c1_ = (mx+mn)/2.0;
            c2_ = (mx-mn)/2.0;
          }
          for (int i = 0; i < initPar_.size(); i++) {
            sensTheta(i, 0) = (initPar_(i, 0) - c1_)/c2_;
          }
          scaleSetup_ = true;
        }
      }

      //' This function changes the sensitivity theta (possibly scaled) to the full theta
      //'
      //' @param d The integer that helps if this is a sensitivity parameter
      //'      that we will estimate the gradient of... (binary & determines if it is)
      //'
      //' @param theta The sensitivity theta
      //'
      //' @param fullTheta The full theta that will be updated.
      //'
      //' @param nd the current sensitivity number that determines which parameters
      //'    the gradient is calculated
      //'
      //' @param i The current index of the sensitivity parameter being updated
      //'
      //' @param j The current index of the full theta parameter being updated
      //'
      void trueThetaElt(int d,
                        const Eigen::Matrix<double, Eigen::Dynamic, 1> theta,
                        Eigen::Matrix<double, Eigen::Dynamic, 1>& fullTheta,
                        int nd, int& i, int& j) const {
        if ((nd & d) != 0) {
          if (!scaleSetup_) {
            fullTheta(j, 0) = theta(i, 0);
          } else {
            fullTheta(j, 0) =
              (theta(i, 0) - (initPar_(i, 0) - c1_)/c2_)*scaleC_(i, 0) +
              initPar_(i, 0);
          }
          i++;
        } else {
          fullTheta(j, 0) = trueTheta_(j, 0);
        }
        j++;
      }

      //' This function changes the sensitivity theta to the full theta
      //'
      //' This is the same function as above, but applied to the stan::math::var
      //' instead of double
      //'
      void trueThetaElt(int d,
                        const Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> theta,
                        Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1>& fullTheta,
                        int &nd, int& i, int& j) const {

        if ((nd & d) != 0) {
          fullTheta(j, 0) = theta(i, 0);
          i++;
        } else {
          fullTheta(j, 0) = trueTheta_(j, 0);
        }
        j++;
      }

      //' This function changes the sensitivity theta to the full theta
      //' @param theta The sensitivity theta
      //'
      //' @return The full theta
      //'
      template <typename T>
      Eigen::Matrix<T, Eigen::Dynamic, 1>
      trueTheta(const Eigen::Matrix<T, Eigen::Dynamic, 1>& theta) const {
        Eigen::Matrix<T, Eigen::Dynamic, 1> fullTheta(ncmt_*2 + oral0_);
        int nd = numDiff_;
        if (nd == 0) nd = 127; // all terms
        int i = 0, j=0;

        switch (ncmt_) {
        case 1: {
          trueThetaElt(diffP1, theta, fullTheta, nd, i, j);
          trueThetaElt(diffV1, theta, fullTheta, nd, i, j);
          if (oral0_) trueThetaElt(diffKa, theta, fullTheta, nd, i, j);
          return fullTheta;
        }
        case 2: {
          trueThetaElt(diffP1, theta, fullTheta, nd, i, j);
          trueThetaElt(diffV1, theta, fullTheta, nd, i, j);
          trueThetaElt(diffP2, theta, fullTheta, nd, i, j);
          trueThetaElt(diffP3, theta, fullTheta, nd, i, j);
          if (oral0_) trueThetaElt(diffKa, theta, fullTheta, nd, i, j);
          return fullTheta;
        }

        case 3: {
          trueThetaElt(diffP1, theta, fullTheta, nd, i, j);
          trueThetaElt(diffV1, theta, fullTheta, nd, i, j);
          trueThetaElt(diffP2, theta, fullTheta, nd, i, j);
          trueThetaElt(diffP3, theta, fullTheta, nd, i, j);
          trueThetaElt(diffP4, theta, fullTheta, nd, i, j);
          trueThetaElt(diffP5, theta, fullTheta, nd, i, j);
          if (oral0_) trueThetaElt(diffKa, theta, fullTheta, nd, i, j);
          return fullTheta;
        }
        }
        return fullTheta;
      }

      //' This updates the full Jacobian matrix from the smaller Jacobian
      //'
      //' @param J The full Jacobian matrix
      //'
      //' @param Js The smaller Jacobian matrix only focusing on needed sensitivity
      //'
      void updateJfromJs(Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>& J,
                         const Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>& Js) {
        int nd = numDiff_;
        if (nd == 0) nd = 127; // all terms
        int i = 0, j=0;

        switch (ncmt_) {
        case 1: {
          if ((nd & diffP1) != 0) {
            J.col(j) = Js.col(i);
            i++;
          }
          j++;

          if ((nd & diffV1) != 0) {
            J.col(j) = Js.col(i);
            i++;
          }
          j++;

          if (oral0_ && (nd & diffKa) != 0) {
            J.col(j) = Js.col(i);
            i++;
          }
          return;
        }

        case 2: {
          if ((nd & diffP1) != 0) {
            J.col(j) = Js.col(i);
            i++;
          }
          j++;

          if ((nd & diffV1) != 0) {
            J.col(j) = Js.col(i);
            i++;
          }
          j++;

          if ((nd & diffP2) != 0) {
            J.col(j) = Js.col(i);
            i++;
          }
          j++;

          if ((nd & diffP3) != 0) {
            J.col(j) = Js.col(i);
            i++;
          }
          j++;

          if (oral0_ && (nd & diffKa) != 0) {
            J.col(j) = Js.col(i);
            i++;
          }
          j++;
          return;
        }

        case 3: {
          if ((nd & diffP1) != 0) {
            J.col(j) = Js.col(i);
            i++;
          }
          j++;

          if ((nd & diffV1) != 0) {
            J.col(j) = Js.col(i);
            i++;
          }
          j++;

          if ((nd & diffP2) != 0) {
            J.col(j) = Js.col(i);
            i++;
          }
          j++;

          if ((nd & diffP3) != 0) {
            J.col(j) = Js.col(i);
            i++;
          }
          j++;

          if ((nd & diffP4) != 0) {
            J.col(j) = Js.col(i);
            i++;
          }
          j++;

          if ((nd & diffP5) != 0) {
            J.col(j) = Js.col(i);
            i++;
          }
          j++;

          if (oral0_ && (nd & diffKa) != 0) {
            J.col(j) = Js.col(i);
            i++;
          }
          j++;
          return;
        }
        }
      }

      //' Is this linear compartment model the same as the current one?
      //'
      //' @param ncmt number of compartments
      //'
      //' @param oral0 0 or 1 for oral dose
      //'
      //' @param trans the translation indictor
      //'
      //'
      //' @param numDiff the number of differential equations that we
      //         are calculating gradients for.
      //'
      bool isSame(const int ncmt, const int oral0, const int trans,
                  const int numDiff) {
        return (ncmt == ncmt_ && oral0 == oral0_ && trans_ == trans &&
                numDiff_ == numDiff);
      }

      //' Get the number of parameters
      int getNpars() {
        if (oral0_) {
          return 2*ncmt_ + 1;
        } else {
          return 2*ncmt_;
        }
      }

      //' Get the number of compartments
      int getNalast() {
        if (grad_) {
          return ncmt_ + oral0_ + ncmt_*getNpars() + oral0_;
        } else {
          return ncmt_ + oral0_;
        }
        return 0;
      }

      //' Get the number of rate compartments
      int getNrate() {
        return 1 + oral0_;
      }

      //////////////////////////////////////////////////////////////////
      // Solved One compartment steady state solutions
      //////////////////////////////////////////////////////////////////

      //' This function calculates the steady state for a one compartment
      //' infinite infusion
      //'
      //' @param g The micro-constants matrix
      //'
      //' @param ret The returned matrix
      //'
      //' @return nothing, updates ret instead
      //'
      template <typename T>
      void
      linCmtStan1ssInf8(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                        T ka, Eigen::Matrix<T, Eigen::Dynamic, 1>& ret) const {
#define v     g(0, 0)
#define k     g(0, 1)
        if (oral0_  == 1) {
          T rDepot = rate_[0];
          T rCentral = rate_[1];
          if (rate_[0] > 0) {
            ret(0, 0) = rDepot/ka;
            ret(1, 0) = rDepot/k;
            return;
          } else if (rate_[1] > 0) {
            ret(0, 0) = 0;
            ret(1, 0) = rCentral/k;
            return;
          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            return;
          }
        } else {
          T rCentral = rate_[0];
          if (rate_[0] > 0) {
            ret(0, 0) = rCentral/k;
          } else {
            ret(0, 0) = NA_REAL;
          }
          return;
        }
#undef v
#undef k
      }

      //' This function calculates the steady state for a one compartment
      //' infusion
      //'
      //' This uses the the tau_ and tinf_ to calculate the steady state
      //'
      //' These parameters are set in the linCmtStan class
      //'
      //' @param g The micro-constants matrix
      //'
      //' @param ka The absorption rate
      //'
      //' @param ret The returned matrix
      //'
      //' @return nothing, updates ret instead
      //'
      template <typename T>
      void
      linCmtStan1ssInf(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                       T ka,
                       Eigen::Matrix<T, Eigen::Dynamic, 1>& ret) const {
#define v     g(0, 0)
#define k     g(0, 1)
        if (oral0_  == 1) {
          T rDepot = rate_[0];
          T rCentral = rate_[1];
          if (rate_[0] > 0) {
            T eKa     = exp(-(ka)*((tau_)-(tinf_)))/(1.0-exp(-(tau_)*(ka)));
            T eiKa    = exp(-(ka)*(tinf_));
            T eiK     = exp(-(k)*(tinf_));
            T eK      = exp(-(k)*((tau_)-(tinf_)))/(1.0-exp(-(tau_)*(k)));
            ret(0, 0) = eKa*(rDepot)*(1.0 - eiKa)/(ka);
            ret(1, 0) = eK*((rDepot)/(k) + eiKa*(rDepot)/(-(k) + (ka)) - eiK*(rDepot)*(ka)/((ka)*(k) - (k)*(k))) + (ka)*(eK - eKa)*((rDepot)/(ka) - eiKa*(rDepot)/(ka))/(-(k) + (ka));
            return;
          } else if (rate_[1] > 0) {
            T eiK = exp(-(k)*(tinf_));
            T eK = exp(-(k)*(tau_-tinf_))/(1.0-exp(-(k)*tau_));
            ret(0, 0) = 0.0;
            ret(1, 0) = (rCentral)*(1-eiK)*eK/(k);
            return;
          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            return;
          }
        } else {
          T rCentral = rate_[0];
          T eiK = exp(-(k)*(tinf_));
          T eK = exp(-(k)*(tau_-tinf_))/(1.0-exp(-(k)*tau_));
          ret(0, 0) = (rCentral)*(1.0-eiK)*eK/(k);
          return;
        }
#undef v
#undef k
        return;
      }

      //' This function calculates the steady state for a one compartment
      //' bolus compartment
      //'
      //' This uses the the tau_ and bolusAmt_ to calculate the steady state
      //'
      //' @param g The micro-constants matrix
      //'
      //' @param ka The absorption rate
      //'
      //' @param ret The returned matrix
      //'
      //' @return nothing, updates ret instead
      //'
      template <typename T>
      void
      linCmtStan1ssBolus(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                         T ka, Eigen::Matrix<T, Eigen::Dynamic, 1>& ret) const {
#define v     g(0, 0)
#define k     g(0, 1)
        if (oral0_  == 1) {
          if (bolusCmt_ == 0) {
            T eKa = 1.0/(1.0 - exp(-(tau_)*(ka)));
            T eK =  1.0/(1.0 - exp(-(tau_)*(k)));
            ret(0, 0) = eKa*(bolusAmt_);
            ret(1, 0) = (ka)*(bolusAmt_)*(eK - eKa)/(-(k) + (ka));
            return;
          } else if (bolusCmt_ == 1) {
            T eK =  1.0/(1.0-exp(-(tau_)*(k)));
            ret(0, 0)=0.0;
            ret(1, 0)=eK*(bolusAmt_);
            return;
          } else {
            ret(0, 0)=NA_REAL;
            ret(1, 0)=NA_REAL;
            return;
          }
        } else {
          if (bolusCmt_ == 0) {
            T eK =  1.0/(1.0 - exp(-(tau_)*(k)));
            ret(0, 0) = eK * (bolusAmt_);
            return;
          } else {
            ret(0, 0) = NA_REAL;
            return;
          }
        }
#undef v
#undef k
        return;
      }

      //////////////////////////////////////////////////////////////////
      // Solved One compartment wnl solutions
      //////////////////////////////////////////////////////////////////

      //' This calculates the one compartment wnl solution
      //'
      //' The dt_ represents the time step
      //'
      //' @param g The micro-constants matrix
      //'
      //' @param yp The prior state
      //'
      //' @param ka The absorption rate
      //'
      //' @param ret The returned matrix
      //'
      //' @return nothing, updates ret instead
      //'
      template <typename T>
      void linCmtStan1(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                       Eigen::Matrix<T, Eigen::Dynamic, 1> yp,
                       T ka,
                       Eigen::Matrix<T, Eigen::Dynamic, 1> &ret) const {
#define k10   g(0, 1)
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )
        // Constants that would be in common and could be calculated once:
        const T E            = exp(-k10 * dt_);

        ret(oral0_, 0) = yp(oral0_, 0)*E;
        T R            = rate_[0];

        // Handle oral absorption case
        if (oral0_ == 1) {
          const T Ea =  exp(-ka*dt_);
          const T ka10 = ka - k10;

          R += rate_[1];
          ret(0, 0) = rate_[0] * (1.0 - Ea) / ka + yp(0, 0) * Ea;

          if (abs(ka10) <= sqrt(DBL_EPSILON)) {
            ret(1, 0) += (yp(0, 0) * k10 - rate_[0]) * dt_ * E;
          } else {
            ret(1, 0) += (yp(0, 0) * ka  - rate_[0]) * (E - Ea) / ka10;
          }
        }
        // Handle the case with infusion
        if (abs(R) > sqrt(DBL_EPSILON)) {
          ret(oral0_, 0) += R * (1.0 - E) / k10;
        }
#undef k10
      }

      //////////////////////////////////////////////////////////////////
      // Solved Two compartment steady state solutions
      //////////////////////////////////////////////////////////////////

      //' This calculates the two compartment steady state solution for
      //' infinite infusion
      //'
      //' @param g The micro-constants matrix
      //'
      //' @param ka The absorption rate
      //'
      //' @param ret The returned matrix
      //'
      //' @return nothing, updates ret instead
      //'
      template <typename T>
      void
      linCmtStan2ssInf8(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                        T ka,
                        Eigen::Matrix<T, Eigen::Dynamic, 1>& ret) const {
#define v     g(0, 0)
#define k     g(0, 1)
#define k20   g(0, 1)

#define k12   g(1, 0)
#define k23   g(1, 0)

#define k21   g(1, 1)
#define k32   g(1, 1)

        if (oral0_  == 1) {
          T rDepot = rate_[0];
          T rCentral = rate_[1];
          if (rate_[0] > 0) {
            T s = (k23)+(k32)+(k20);
            T beta  = 0.5*(s - sqrt(s*s - 4*(k32)*(k20)));
            T alpha = (k32)*(k20)/beta;
            ret(0, 0) = rDepot/(ka);
            ret(1, 0) = (rDepot)*(k32)/(beta*alpha);
            ret(2, 0)=(rDepot)*(k23)/(beta*alpha);
            return;
          } else if (rate_[1] > 0) {
            T s = (k23)+(k32)+(k20);
            T beta  = 0.5*(s - sqrt(s*s - 4*(k32)*(k20)));
            T alpha = (k32)*(k20)/beta;
            ret(0, 0) = 0;
            ret(1, 0) = (rCentral)*(k32)/(beta*alpha);
            ret(2, 0) = (rCentral)*(k23)/(beta*alpha);
            return;
          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            ret(3, 0) = NA_REAL;
            return;
          }
        } else {
          T rCentral = rate_[0];
          if (rate_[0] > 0) {
            T s = (k23)+(k32)+(k20);
            T beta  = 0.5*(s - sqrt(s*s - 4*(k32)*(k20)));
            T alpha = (k32)*(k20)/beta;
            ret(0, 0) = (rCentral)*(k32)/(beta*alpha);
            ret(1, 0) = (rCentral)*(k23)/(beta*alpha);
            return;
          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            return;
          }
          return;
        }
#undef k21
#undef k32
#undef k12
#undef k23
#undef v
#undef k
      }

      //' This calculates the two compartment steady state solution for
      //' infusion with tinf_ and tau_
      //'
      //' @param g The micro-constants matrix
      //'
      //' @param ka The absorption rate
      //'
      //' @param ret The returned matrix
      //'
      //' @return nothing, updates ret instead
      template <typename T>
      void
      linCmtStan2ssInf(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                       T ka,
                       Eigen::Matrix<T, Eigen::Dynamic, 1>& ret) const {
#define v     g(0, 0)
#define k     g(0, 1)
#define k20   g(0, 1)

#define k12   g(1, 0)
#define k23   g(1, 0)

#define k21   g(1, 1)
#define k32   g(1, 1)
        if (oral0_  == 1) {
          T rDepot = rate_[0];
          T rCentral = rate_[1];
          if (rate_[0] > 0) {
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

            T ka2 = (ka)*(ka);

            T eKa = exp(-(ka)*((tau_)-(tinf_)))/(1.0-exp(-(ka)*(tau_)));
            T eiKa = exp(-(ka)*(tinf_));

            ret(0, 0) = eKa*((rDepot)/(ka) - eiKa*(rDepot)/(ka));
            ret(1, 0) =(eA*(-alpha*((rDepot)*(k32)/(beta*alpha) + eiKa*(rDepot)*(-(k32) + (ka))/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(-alpha + (k32))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(-beta + (k32))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k32)*((rDepot)*(k23)/(beta*alpha) - eiKa*(rDepot)*(k23)/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(k23)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(k23)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k32)*((rDepot)*(k32)/(beta*alpha) + eiKa*(rDepot)*(-(k32) + (ka))/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(-alpha + (k32))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(-beta + (k32))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3))) - eB*(-beta*((rDepot)*(k32)/(beta*alpha) + eiKa*(rDepot)*(-(k32) + (ka))/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(-alpha + (k32))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(-beta + (k32))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k32)*((rDepot)*(k23)/(beta*alpha) - eiKa*(rDepot)*(k23)/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(k23)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(k23)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k32)*((rDepot)*(k32)/(beta*alpha) + eiKa*(rDepot)*(-(k32) + (ka))/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(-alpha + (k32))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(-beta + (k32))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3))))/(-alpha + beta) + (ka)*(eA*(-alpha + (k32))/((-alpha + beta)*(-alpha + (ka))) + eB*(-beta + (k32))/((-beta + (ka))*(alpha - beta)) + eKa*((k32) - (ka))/((beta - (ka))*(alpha - (ka))))*((rDepot)/(ka) - eiKa*(rDepot)/(ka));
            ret(2, 0)=(eA*(-alpha*((rDepot)*(k23)/(beta*alpha) - eiKa*(rDepot)*(k23)/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(k23)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(k23)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k23)*((rDepot)*(k32)/(beta*alpha) + eiKa*(rDepot)*(-(k32) + (ka))/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(-alpha + (k32))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(-beta + (k32))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + ((k20) + (k23))*((rDepot)*(k23)/(beta*alpha) - eiKa*(rDepot)*(k23)/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(k23)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(k23)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3))) - eB*(-beta*((rDepot)*(k23)/(beta*alpha) - eiKa*(rDepot)*(k23)/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(k23)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(k23)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + (k23)*((rDepot)*(k32)/(beta*alpha) + eiKa*(rDepot)*(-(k32) + (ka))/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(-alpha + (k32))/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(-beta + (k32))/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3)) + ((k20) + (k23))*((rDepot)*(k23)/(beta*alpha) - eiKa*(rDepot)*(k23)/(beta*alpha + (ka)*(-alpha - beta) + ka2) - eiA*(rDepot)*(ka)*(k23)/(-beta*alpha2 + (ka)*(beta*alpha - alpha2) + alpha3) + eiB*(rDepot)*(ka)*(k23)/(beta2*alpha + (ka)*(-beta*alpha + beta2) - beta3))))/(-alpha + beta) + (ka)*(k23)*(eA/((-alpha + beta)*(-alpha + (ka))) + eB/((-beta + (ka))*(alpha - beta)) + eKa/((beta - (ka))*(alpha - (ka))))*((rDepot)/(ka) - eiKa*(rDepot)/(ka));
            return;
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
            return;
          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            ret(2, 0) = NA_REAL;
            return;
          }
        } else {
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
          return;
        }
#undef k21
#undef k32
#undef k12
#undef k23
#undef v
#undef k
        return;
      }

      //' This calculates the two compartment steady state solution for
      //' bolus with tinf_ and bolusAmt_ to the bolusCmt_
      //'
      //' @param g The micro-constants matrix
      //'
      //' @param ka The absorption rate
      //'
      //' @param ret The returned matrix
      //'
      //' @return nothing, updates ret instead
      //'
      template <typename T>
      void
      linCmtStan2ssBolus(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                         T ka,
                         Eigen::Matrix<T, Eigen::Dynamic, 1>& ret) const {

#define v     g(0, 0)
#define k     g(0, 1)
#define k20   g(0, 1)

#define k12   g(1, 0)
#define k23   g(1, 0)

#define k21   g(1, 1)
#define k32   g(1, 1)
        if (oral0_  == 1) {
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
            return;
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

            return;
          } else {
            ret(0, 0)=NA_REAL;
            ret(1, 0)=NA_REAL;
            ret(2, 0)=NA_REAL;
            return;
          }
        } else {
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

            return;
          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            return;
          }
        }
#undef k21
#undef k32
#undef k12
#undef k23
#undef v
#undef k
        return;
      }

      //////////////////////////////////////////////////////////////////
      // Solved Two compartment wnl solutions
      //////////////////////////////////////////////////////////////////

      //' This calculates the two compartment wnl solution for
      //' the two compartment model
      //'
      //' @param g The micro-constants matrix
      //'
      //' @param yp The prior amount matrix
      //'
      //' @param ka The absorption rate
      //'
      //' @param ret The returned matrix
      //'
      //' @return nothing, updates ret instead
      //'
      template <typename T>
      void
      linCmtStan2(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                  Eigen::Matrix<T, Eigen::Dynamic, 1> yp,
                  T ka,
                  Eigen::Matrix<T, Eigen::Dynamic, 1>& ret) const {
#define k12   g(1, 0)
#define k21   g(1, 1)
#define k10   g(0, 1)

        if (abs(k12-k21) < DBL_EPSILON) {
          linCmtStan1(g, yp, ka, ret);
          ret(oral0_ + 1, 0) = yp(oral0_ + 1, 0);
          return;
        }

        stan::math::solComp2struct<T> sol2 =
          stan::math::computeSolComp2(k10, k12, k21, ka);

        T rDepot = 0.0;
        T R      = rate_[oral0_];

        Eigen::Matrix<T, 2, 1> Xo;
        Eigen::Matrix<T, 2, 1> Rm;
        Eigen::Matrix<T, 2, 1> E = exp(-sol2.L * dt_);
        Eigen::Matrix<T, 2, 1> Ea = E;

        Xo =(yp(oral0_, 0)*sol2.C1) * E +
          (yp(oral0_ + 1, 0)*sol2.C2) * E;

        if (oral0_ == 1) {
          // Xo = Xo + Ka*pX[1]*(Co[, , 1] %*% ((E - Ea)/(Ka - L)))
          rDepot = rate_[0];
          R += rDepot;
          Eigen::Matrix<T, 2, 1> expa = Eigen::Matrix<T, 2, 1>::Constant(2, 1, exp(-ka*dt_));
          Eigen::Matrix<T, 2, 1> ka2 = Eigen::Matrix<T, 2, 1>::Constant(2, 1, ka);
          Ea =  (E - expa).array()/(ka2 - sol2.L).array();
          T cf = ka*yp(0, 0) - rDepot;
          Xo += (cf*sol2.C1)*Ea;
          ret(0, 0) = yp(0, 0)*expa(0, 0);
          if (rate_[0] > 0) {
            ret(0, 0) += rDepot*(1.0-expa(0, 0))/ka;
          }
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
        return;
      }

      //////////////////////////////////////////////////////////////////
      // Solved Three compartment steady state solutions
      //////////////////////////////////////////////////////////////////

      //' This calculates the three compartment steady state solution for
      //' the three compartment model infinite infusion
      //'
      //' @param g The micro-constants matrix
      //'
      //' @param ka The absorption rate
      //'
      //' @param ret The returned matrix
      //'
      //' @return nothing, updates ret instead
      //'
      template <typename T>
      void
      linCmtStan3ssInf8(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                        T ka,
                        Eigen::Matrix<T, Eigen::Dynamic, 1>& ret) const {
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
        if (oral0_  == 1) {
          T rDepot = rate_[0];
          T rCentral = rate_[1];
          if (rate_[0] > 0) {
            T j = (k23)+(k20)+(k32)+(k42)+(k24);
            T k = (k23)*(k42)+(k20)*(k32)+(k20)*(k42)+(k32)*(k42)+(k24)*(k32);
            T l = (k20)*(k32)*(k42);

            T m = (3.0*k- j*j)/3.0;
            T n = (2.0*j*j*j - 9.0*j*k + 27.0*l)/27.0;
            T Q = 0.25*n*n + m*m*m/27.0;

            T alpha = sqrt(-Q);
            T beta = -0.5*n;
            T rho=sqrt(beta*beta+alpha*alpha);
            T theta = atan2(alpha,beta);
            T ct3 = cos(theta/3.0);
            T rho3 = pow(rho,1.0/3.0);
            T st3 = 1.732050807568877193177*sin(theta/3.0);
            T j3 = j/3.0;
            T lam1 = j3  + rho3*(ct3 + st3);
            T lam2 = j3 + rho3*(ct3 - st3);
            T lam3 = j3 -(2.0*rho3*ct3);
            T l123 = 1.0/(lam1*lam2*lam3);
            ret(0, 0)=rDepot/(ka);
            ret(1, 0)=rDepot*(k42)*(k32)*l123;
            ret(2, 0)=rDepot*(k42)*(k23)*l123;
            ret(3, 0)=rDepot*(k24)*(k32)*l123;
            return;
          } else if (rate_[1] > 0) {
            T j = (k23)+(k20)+(k32)+(k42)+(k24);
            T k = (k23)*(k42)+(k20)*(k32)+(k20)*(k42)+(k32)*(k42)+(k24)*(k32);
            T l = (k20)*(k32)*(k42);

            T m = (3.0*k - j*j)/3.0;
            T n = (2.0*j*j*j - 9.0*j*k + 27.0*l)/27.0;
            T Q = 0.25*n*n + m*m*m/27.0;

            T alpha = sqrt(-Q);
            T beta = -0.5*n;
            T rho=sqrt(beta*beta+alpha*alpha);
            T theta = atan2(alpha,beta);
            T ct3 = cos(theta/3.0);
            T rho3 = pow(rho,1.0/3.0);
            T st3 = 1.732050807568877193177*sin(theta/3.0);
            T j3   = j/3.0;
            T lam1 = j3  + rho3*(ct3 + st3);
            T lam2 = j3 + rho3*(ct3 - st3);
            T lam3 = j3 -(2.0*rho3*ct3);
            T l123 = 1.0/(lam1*lam2*lam3);

            ret(0, 0)=0.0;
            ret(1, 0)=(rCentral)*(k42)*(k32)*l123;
            ret(2, 0)=(rCentral)*(k42)*(k23)*l123;
            ret(3, 0)=(rCentral)*(k24)*(k32)*l123;
            return;
          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            ret(2, 0) = NA_REAL;
            ret(3, 0) = NA_REAL;
            return;
          }
        } else {
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

            return;
          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            ret(2, 0) = NA_REAL;

            return;
          }
          return;
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
      void
      linCmtStan3ssInf(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                       T ka,
                       Eigen::Matrix<T, Eigen::Dynamic, 1>& ret) const {
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
        if (oral0_  == 1) {
          T rDepot = rate_[0];
          T rCentral = rate_[1];
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
            ret(0, 0) =eKa*((rDepot)/(ka) - eiKa*(rDepot)/(ka));
            ret(1, 0)=(eL1*(E4 - lam1)*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E3 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) + (ka)*(eL1*(E4 - lam1)*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)*((ka) - lam1)) + eL2*(E4 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)*((ka) - lam2)) + eL3*(E3 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)*((ka) - lam3)) + eKa*(E3 - (ka))*(E4 - (ka))/((-(ka) + lam1)*(-(ka) + lam2)*(-(ka) + lam3)))*((rDepot)/(ka) - eiKa*(rDepot)/(ka)) + eL1*(-lam1*((k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3))) + E3*(k42)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*(lam3*((k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3))) - (E3*(k42)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*(lam2*((k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3))) - (E3*(k42)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            ret(2, 0)=(eL1*(E4 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E2 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)) + (ka)*(k23)*(eL1*(E4 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)*((ka) - lam1)) + eL2*(E4 - lam2)/((-lam2 + lam3)*(lam1 - lam2)*((ka) - lam2)) + eL3*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)*((ka) - lam3)) + eKa*(E4 - (ka))/((-(ka) + lam1)*(-(ka) + lam2)*(-(ka) + lam3)))*((rDepot)/(ka) - eiKa*(rDepot)/(ka)) + eL1*(E4*(k23)*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*lam1*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(k24)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*((k23)*lam3*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (E4*(k23)*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(k24)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*((k23)*lam2*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (E4*(k23)*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(k24)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            ret(3, 0)=(eL1*(E3 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E2 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E3 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + (ka)*(k24)*(eL1*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)*((ka) - lam1)) + eL2*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)*((ka) - lam2)) + eL3*(E3 - lam3)/((lam2 - lam3)*(lam1 - lam3)*((ka) - lam3)) + eKa*(E3 - (ka))/((-(ka) + lam1)*(-(ka) + lam2)*(-(ka) + lam3)))*((rDepot)/(ka) - eiKa*(rDepot)/(ka)) + eL1*(E3*(k24)*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + (k24)*(k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3)) - (k24)*lam1*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*((k24)*lam3*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (E3*(k24)*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + (k24)*(k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*((k24)*lam2*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (E3*(k24)*(-eiKa*(rDepot)*((k42)*(k32) + (-(k32) - (k42))*(ka) + ka2)/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) - eiL1*(rDepot)*(-(ka)*lam12 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rDepot)*(-(ka)*lam22 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rDepot)*(-(ka)*lam32 - (ka)*(k42)*(k32) + ((k32) + (k42))*(ka)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiKa*(rDepot)*(-(k24)*(k32) + (ka)*(k24))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam1)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam2)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*((ka)*(k24)*(k32) - (ka)*(k24)*lam3)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k24)*(k32)/(lam2*lam1*lam3)) + (k24)*(k32)*(eiKa*(rDepot)*(-(k42)*(k23) + (ka)*(k23))/(ka2*lam1 + lam2*(-(ka)*lam1 + ka2) + lam3*(-(ka)*lam1 + lam2*(-(ka) + lam1) + ka2) - ka3) + eiL1*(rDepot)*(-(ka)*(k23)*lam1 + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rDepot)*(-(ka)*(k23)*lam2 + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rDepot)*(-(ka)*(k23)*lam3 + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rDepot)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            return;
          } else if (rate_[1] > 0) {
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
            ret(1, 0) =(eL1*(E4 - lam1)*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E3 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) + eL1*(-lam1*((k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3))) + E3*(k42)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*(lam3*((k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3))) - (E3*(k42)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*(lam2*((k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3))) - (E3*(k42)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            ret(2, 0)=(eL1*(E4 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E2 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) + eL1*(E4*(k23)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*lam1*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(k24)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*((k23)*lam3*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (E4*(k23)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(k24)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*((k23)*lam2*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (E4*(k23)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(k24)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            ret(3, 0)=(eL1*(E3 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E2 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E3 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) + eL1*(E3*(k24)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) + (k24)*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) - (k24)*lam1*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*((k24)*lam3*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (E3*(k24)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) + (k24)*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*((k24)*lam2*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (E3*(k24)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) + (k24)*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            return;
          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            ret(2, 0) = NA_REAL;
            ret(3, 0) = NA_REAL;
            return;
          }
        } else {
          T rCentral = rate_[0];
          if (rate_[0] > 0) {
            T E2 = (k20) + (k23) + (k24);
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
            ret(0, 0) =(eL1*(E4 - lam1)*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E3 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) + eL1*(-lam1*((k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3))) + E3*(k42)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*(lam3*((k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3))) - (E3*(k42)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*(lam2*((k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) + (k42)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3))) - (E3*(k42)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) + E4*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            ret(1, 0)=(eL1*(E4 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E2 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) + eL1*(E4*(k23)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*lam1*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(k24)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*((k23)*lam3*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (E4*(k23)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(k24)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*((k23)*lam2*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (E4*(k23)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) + (k42)*(k23)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) - (k42)*(k24)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            ret(2, 0)=(eL1*(E3 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E2 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E3 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) + eL1*(E3*(k24)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) + (k24)*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3)) - (k24)*lam1*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*((k24)*lam3*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (E3*(k24)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) + (k24)*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*((k24)*lam2*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (E3*(k24)*(-eiL1*(rCentral)*(lam1*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam12 - (ka)*(k42)*(k32) + lam13)/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(rCentral)*(lam2*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam22 - (ka)*(k42)*(k32) + lam23)/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) - eiL3*(rCentral)*(lam3*((k42)*(k32) + ((k32) + (k42))*(ka)) + (-(k32) - (k42) - (ka))*lam32 - (ka)*(k42)*(k32) + lam33)/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k32)/(lam2*lam1*lam3)) - (k23)*(k32)*(eiL1*(rCentral)*((k24)*lam12 + lam1*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k24)*lam22 + lam2*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k24)*lam32 + lam3*(-(k24)*(k32) - (ka)*(k24)) + (ka)*(k24)*(k32))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k24)*(k32)/(lam2*lam1*lam3)) + (k24)*(k32)*(eiL1*(rCentral)*((k23)*lam12 + lam1*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(-(ka)*lam13 + lam2*((ka)*lam12 - lam13) + lam3*((ka)*lam12 + lam2*(-(ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(rCentral)*((k23)*lam22 + lam2*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam23*((ka) + lam1) + lam3*(lam22*(-(ka) - lam1) + (ka)*lam2*lam1 + lam23) - (ka)*lam22*lam1 - lam24) + eiL3*(rCentral)*((k23)*lam32 + lam3*(-(k42)*(k23) - (ka)*(k23)) + (ka)*(k42)*(k23))/(lam32*((ka)*lam1 + lam2*((ka) + lam1)) + (-(ka) - lam1 - lam2)*lam33 - (ka)*lam2*lam1*lam3 + lam34) + (rCentral)*(k42)*(k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
            return;
          } else {
            ret(0, 0) = NA_REAL;
            ret(1, 0) = NA_REAL;
            ret(2, 0) = NA_REAL;
            return;
          }
          return;
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

      //' Three compartment steady state bolus
      //' tau_ and bolusAmt_ are used
      //'
      //' @param g macro constants matrix
      //'
      //' @param ka absorption rate constant
      //'
      //' @param ret returned matrix
      //'
      //' @return nothing, called for side effects
      //'
      template <typename T>
      void
      linCmtStan3ssBolus(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                         T ka,
                         Eigen::Matrix<T, Eigen::Dynamic, 1>& ret) const {
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

        if (oral0_  == 1) {
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

            T eKa = 1.0/(1.0-exp(-(tau_)*(ka)));
            T eL1 = 1.0/(1.0-exp(-(tau_)*lambda1));
            T eL2 = 1.0/(1.0-exp(-(tau_)*lambda2));
            T eL3 = 1.0/(1.0-exp(-(tau_)*lambda3));

            ret(0, 0) = eKa*(bolusAmt_);
            ret(1, 0) = (ka)*(bolusAmt_)*(eL1*(E3 - lambda1)*(E4 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*((ka) - lambda1)) + eL2*(E3 - lambda2)*(E4 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*((ka) - lambda2)) + eL3*(E3 - lambda3)*(E4 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*((ka) - lambda3)) + eKa*(E3 - (ka))*(E4 - (ka))/((-(ka) + lambda1)*(-(ka) + lambda3)*(-(ka) + lambda2)));
            ret(2, 0) = (ka)*(bolusAmt_)*(k23)*(eL1*(E4 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*((ka) - lambda1)) + eL2*(E4 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*((ka) - lambda2)) + eL3*(E4 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*((ka) - lambda3)) + eKa*(E4 - (ka))/((-(ka) + lambda1)*(-(ka) + lambda3)*(-(ka) + lambda2)));
            ret(3, 0) = (ka)*(bolusAmt_)*(k24)*(eL1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*((ka) - lambda1)) + eL2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*((ka) - lambda2)) + eL3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*((ka) - lambda3)) + eKa*(E3 - (ka))/((-(ka) + lambda1)*(-(ka) + lambda3)*(-(ka) + lambda2)));
            return;
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
            return;
          } else {
            ret(0, 0)=NA_REAL;
            ret(1, 0)=NA_REAL;
            ret(2, 0)=NA_REAL;
            ret(3, 0)=NA_REAL;
            return;
          }
        } else {
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
            return;
          } else {
            ret(0, 0)=NA_REAL;
            ret(1, 0)=NA_REAL;
            ret(2, 0)=NA_REAL;
            return;
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
      }

      //////////////////////////////////////////////////////////////////
      // Solved Three compartment wnl solutions
      //////////////////////////////////////////////////////////////////

      //' Calculate the 3 compartment model
      //'
      //' @param g micro-constants matrix
      //'
      //' @param yp the amount in the last compartment
      //'
      //' @param ret the output matrix
      //'
      //' @return nothing, called for side effects
      template <typename T>
      void
      linCmtStan3(Eigen::Matrix<T, Eigen::Dynamic, 2> g,
                  Eigen::Matrix<T, Eigen::Dynamic, 1> yp,
                  T ka,
                  Eigen::Matrix<T, Eigen::Dynamic, 1>& ret) const {
#define k12   g(1, 0)
#define k21   g(1, 1)
#define k13   g(2, 0)
#define k31   g(2, 1)
#define k10   g(0, 1)
        stan::math::solComp3struct<T> sol3 =
          stan::math::computeSolComp3(k10, k12, k21, k13, k31);

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
      }

      //' This sets the pointer for the linear compartment model
      //'
      //' @param A is the last amount known
      //'
      //' @param R is the rate
      //'
      //' @param Asave is the double array that will save the amounts
      //'
      //' @return set the pointers
      //'
      void setPtr(double *A, double *R, double *Asave) {
        A_ = A;
        rate_ = R;
        Asave_ = Asave;
      }

      void setSsInf(double tinf, double tau) {
        tinf_ = tinf;
        tau_ = tau;
      }

      void setSsBolus(double bolusAmt, double tau, int bolusCmt) {
        bolusAmt_ = bolusAmt;
        tau_ = tau;
        bolusCmt_ = bolusCmt;
      }

      Eigen::Matrix<double,Eigen::Dynamic, 1> restoreFx(double *A) const {
        // Save A1-A4
        Eigen::Matrix<double, Eigen::Dynamic, 1> Alast(ncmt_ + oral0_, 1);
        for (int i = 0; i < ncmt_ + oral0_; i++) {
          Alast(i, 0) = A[i];
        }
        return Alast;
      }

      Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>
      restoreJac(double *A) const {
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

      // Double initialization need to be outside of the linCmtStan struct
      Eigen::Matrix<double, Eigen::Dynamic, 1>
      getAlast(const Eigen::Matrix<double, Eigen::Dynamic, 1>& theta) const {
        Eigen::Matrix<double, Eigen::Dynamic, 1> Alast(ncmt_ + oral0_, 1);
        for (int i = oral0_ + ncmt_; i--;){
          Alast(i, 0) = A_[i];
        }
        return Alast;
      }

      Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1>
      getAlast(const Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1>& theta) const {
        Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> J  = restoreJac(&A_[0]);

        Eigen::Matrix<double, Eigen::Dynamic, 1> AlastA(ncmt_ + oral0_, 1);
        // AlastA.setZero();

        Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> Alast(ncmt_ + oral0_, 1);
        // Alast.setZero();

        stan::math::var cur = theta(0, 0);
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
      void printDouble(Eigen::Matrix<T, Eigen::Dynamic, 1> ret0) const {
      }

      template <typename T>
      void printDouble(T ret0) const {
      }

      template <typename T>
      void saveAlast(Eigen::Matrix<T, Eigen::Dynamic, 1> ret0) const {
        for (int i = 0; i < ncmt_ + oral0_; i++) {
          T smv = ret0(i, 0);
          Asave_[i] = smv.val();
        }
      }

      void linAcalcAlast(Eigen::Matrix<double, Eigen::Dynamic, 1> yp,
                         Eigen::Matrix<double, Eigen::Dynamic, 2> g,
                         const Eigen::Matrix<double, Eigen::Dynamic, 1>& theta) {
        yp_ = yp;
        g_ = g;
        yp_ = getAlast(theta);
        g_ = stan::math::macros2micros(theta, ncmt_, trans_);
      }

      // For stan Jacobian to work the class needs to take 1 argument
      // (the parameters)
      Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1>
      operator()(const Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1>& thetaIn) const {
        Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> theta = trueTheta(thetaIn);
        Eigen::Matrix<stan::math::var, Eigen::Dynamic, 2> g =
          stan::math::macros2micros(theta, ncmt_, trans_);

        stan::math::var ka = 0.0;
        if (oral0_) {
          ka = theta[ncmt_*2];
        }
        Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> ret0(ncmt_ + oral0_, 1);
        Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> yp(ncmt_ + oral0_, 1);
        if (type_ == linCmtNormal) {
          yp = getAlast(theta);
          if (ncmt_ == 1) {
            linCmtStan1<stan::math::var>(g, yp, ka, ret0);
          } else if (ncmt_ == 2) {
            linCmtStan2<stan::math::var>(g, yp, ka, ret0);
          } else if (ncmt_ == 3) {
            linCmtStan3<stan::math::var>(g, yp, ka, ret0);
          }
        } else if (type_ == linCmtSsInf8)  {
          if (ncmt_ == 1) {
            linCmtStan1ssInf8(g, ka, ret0);
          } else if (ncmt_ == 2) {
            linCmtStan2ssInf8(g, ka, ret0);
          } else if (ncmt_ == 3) {
            linCmtStan3ssInf8(g, ka, ret0);
          }
        } else if (type_ == linCmtSsInf) {
          if (ncmt_ == 1) {
            linCmtStan1ssInf(g, ka, ret0);
          } else if (ncmt_ == 2) {
            linCmtStan2ssInf(g, ka, ret0);
          } else if (ncmt_ == 3) {
            linCmtStan3ssInf(g, ka, ret0);
          }
        } else if (type_ == linCmtSsBolus) {
          if (ncmt_ == 1) {
            linCmtStan1ssBolus(g, ka, ret0);
          } else if (ncmt_ == 2) {
            linCmtStan2ssBolus(g, ka, ret0);
          } else if (ncmt_ == 3) {
            linCmtStan3ssBolus(g, ka, ret0);
          }
        }
        saveAlast<stan::math::var>(ret0);
        return ret0;
      }

      Eigen::Matrix<double, Eigen::Dynamic, 1>
      fdouble(const Eigen::Matrix<double, Eigen::Dynamic, 1>& theta,
              Eigen::Matrix<double, Eigen::Dynamic, 2> g,
              Eigen::Matrix<double, Eigen::Dynamic, 1> yp) {

        double ka = 0.0;
        if (oral0_) {
          ka = theta[ncmt_*2];
        }
        Eigen::Matrix<double, Eigen::Dynamic, 1> ret0(ncmt_ + oral0_, 1);
        if (type_ == linCmtNormal) {
          if (ncmt_ == 1) {
            linCmtStan1<double>(g, yp, ka, ret0);
          } else if (ncmt_ == 2) {
            linCmtStan2<double>(g, yp, ka, ret0);
          } else if (ncmt_ == 3) {
            linCmtStan3<double>(g, yp, ka, ret0);
          }
        } else if (type_ == linCmtSsInf8)  {
          if (ncmt_ == 1) {
            linCmtStan1ssInf8(g, ka, ret0);
          } else if (ncmt_ == 2) {
            linCmtStan2ssInf8(g, ka, ret0);
          } else if (ncmt_ == 3) {
            linCmtStan3ssInf8(g, ka, ret0);
          }
        } else if (type_ == linCmtSsInf) {
          if (ncmt_ == 1) {
            linCmtStan1ssInf(g, ka, ret0);
          } else if (ncmt_ == 2) {
            linCmtStan2ssInf(g, ka, ret0);
          } else if (ncmt_ == 3) {
            linCmtStan3ssInf(g, ka, ret0);
          }
        } else if (type_ == linCmtSsBolus) {
          if (ncmt_ == 1) {
            linCmtStan1ssBolus(g, ka, ret0);
          } else if (ncmt_ == 2) {
            linCmtStan2ssBolus(g, ka, ret0);
          } else if (ncmt_ == 3) {
            linCmtStan3ssBolus(g, ka, ret0);
          }
        }
        return ret0;
      }

      Eigen::Matrix<double, Eigen::Dynamic, 1> operator()(const Eigen::Matrix<double, Eigen::Dynamic, 1>& theta) {
        Eigen::Matrix<double, Eigen::Dynamic, 1> ret0 = fdouble(theta, g_, yp_);
        for (int i = 0; i < ncmt_ + oral0_; i++) {
          Asave_[i] = ret0(i, 0);
        }
        return ret0;
      }

      // Senstivity function value
      //
      // @param thetaIn the input parameters
      //
      // @return the compartment values
      //
      Eigen::Matrix<double, Eigen::Dynamic, 1> fdoubles(const Eigen::Matrix<double, Eigen::Dynamic, 1>& thetaIn) {
        Eigen::Matrix<double, Eigen::Dynamic, 1> theta = trueTheta(thetaIn);
        Eigen::Matrix<double, Eigen::Dynamic, 2> g = stan::math::macros2micros(theta, ncmt_, trans_);
        Eigen::Matrix<double, Eigen::Dynamic, 1> yp = getAlast(theta);
        return fdouble(theta, g, yp);
      }

      // This function calculates a geometric mean to optimize the step size
      //
      // This takes the compartment values at 4 time points (spaced by the half
      // life) and takes the geometric mean of all the values.  This is taken with
      // a bolus dose to the central (and possibly depot compartment).  It also has
      // an infusion to the central and depot compartment.
      //
      // @param thetaIn the input parameters
      //
      // @return the geometric mean of the compartments at key time-points
      //
      double fdoubleh(const Eigen::Matrix<double, Eigen::Dynamic, 1>& thetaIn) {

        Eigen::Matrix<double, Eigen::Dynamic, 1> cur;
        Eigen::Matrix<double, Eigen::Dynamic, 1> theta = trueTheta(thetaIn);

        // double vc = getVc(theta);

        // int nt12 = 4;
        // double t12 = M_LN2/g_(0, 1);
        // double saveDt = dt_;

        // double ret = 0.0;
        // yp_.setZero();

        // double r0 = rate_[0], r1 = (oral0_ == 0 ? rate_[1] : 0.0);
        // double sum = 0.0;
        // int nzero = 0;
        // int n = 0;
        // yp_[0] = 120000;
        // dt_ = 0.0;
        // for (int i = 0; i < nt12; i++) {
        //   dt_ += t12;
        //   cur = fdoubles(thetaIn);
        //   if (cur(oral0_, 0) > 0) {
        //     sum += log(cur(oral0_, 0)/vc);
        //     n++;
        //   }
        //   for (int j = 0; j < cur.size(); j++) {
        //     if (cur(j, 0) > 0) {
        //       sum += log(cur(j, 0));
        //       n++;
        //     }
        //   }
        // }

        // rate_[0] = 100;
        // yp_[0] = 0;
        // for (int i = 0; i < nt12; i++) {
        //   dt_ += t12;
        //   cur = fdoubles(thetaIn);
        //   if (cur(oral0_, 0) > 0) {
        //     sum += log(cur(oral0_, 0)/vc);
        //     n++;
        //   }
        //   for (int j = 0; j < cur.size(); j++) {
        //     if (cur(j, 0) > 0) {
        //       sum += log(cur(j, 0));
        //       n++;
        //     }
        //   }
        // }

        // if (oral0_) {
        //   dt_ = 0.0;
        //   yp_[0] = 0;
        //   yp_[1] = 120000;
        //   rate_[0] = 0;
        //   for (int i = 0; i < nt12; i++) {
        //     dt_ += t12;
        //     cur = fdoubles(thetaIn);
        //     if (cur(oral0_, 0) > 0) {
        //       sum += log(cur(oral0_, 0)/vc);
        //       n++;
        //     }
        //     for (int j = 0; j < cur.size(); j++) {
        //       if (cur(j, 0) > 0) {
        //         sum += log(cur(j, 0));
        //         n++;
        //       }
        //     }
        //   }

        //   rate_[1] = 100;
        //   yp_[1] = 0;

        //   for (int i = 0; i < nt12; i++) {
        //     dt_ += t12;
        //     cur = fdoubles(thetaIn);
        //     if (cur(oral0_, 0) > 0) {
        //       sum += log(cur(oral0_, 0)/vc);
        //       n++;
        //     }
        //     for (int j = 0; j < cur.size(); j++) {
        //       if (cur(j, 0) > 0) {
        //         sum += log(cur(j, 0));
        //         n++;
        //       }
        //     }
        //   }
        //   rate_[1] = r1;
        // }
        // rate_[0] = r0;
        // yp_ = getAlast(theta);
        // dt_ = saveDt;
        // g_ = stan::math::macros2micros(theta, ncmt_, trans_);
        // return exp((double)(sum)/((double)n));
        double vc = getVc(theta);
        cur =  fdoubles(thetaIn);
        double gm=0.0;
        int n=0;
        for (int i = 0; i < oral0_ + ncmt_; ++i) {
          if (cur(i, 0) > 0) {
            gm += log(cur(i, 0));
            n++;
            if (i == oral0_) {
              gm += log(cur(oral0_, 0)/vc);
              n++;
            }
          }
        }

        return exp(gm/n);
      }


      // Gill 1983 Chat
      static inline double Chat(double phi, double h, double epsA){
        if (phi == 0) return 2*epsA/(h*h);
        return 2*epsA/(h*fabs(phi));
      }

      static inline double ChatP(double phi, double h, double epsA){
        if (phi == 0) return 4*epsA/(h*h*h);
        return 4*epsA/(h*h*fabs(phi));
      }

      static inline double Phi(double fp, double f, double fn, double h){
        return (fp-2*f+fn)/(h*h);
      }
      static inline double phiC(double fp, double fn, double h){
        return (fp-fn)/(2*h);
      }
      static inline double phiF(double f, double fp, double h){
        return (fp-f)/h;
      }
      static inline double phiB(double f, double fn, double h){
        return (f-fn)/h;
      }

      //' @param *hf is the forward difference final estimate
      //' @param *hphif is central difference final estimate (when switching from forward to central differences)
      //' @param *df is the derivative estimate
      //' @param *df2 is the 2nd derivative estimate, useful for pre-conditioning.
      //' @param *ef is the err of the final estimate.
      //' @param thetaSens is the sensitivity vector
      //' @param cpar (integer) is the parameter we are considering
      //'
      //' @param epsR (err) is the relative error for the problem
      //'
      //' @param K is the maximum number of iterations before giving up on searching for the best interval.

      //' @param fTol gradient error tolerance that
      //'     is acceptable before issuing a warning/error about the gradient estimates.
      //'
      //' @param gillStep When looking for the optimal forward difference
      //'     step size, this is This is the step size to increase the
      //'     initial estimate by.  So each iteration the new step size =
      //'     (prior step size)*gillStep
      //'
      //' @param gillF This is the f value at the current estimate
      //'
      //' Returns 1 -- Success
      //'         2 -- Large error; Derivative estimate error 50% or more of the derivative
      //'         3 -- Function constant or nearly constant for this parameter
      //'         4 -- Function odd or nearly linear, df = K, df2 ~ 0
      //'         5 -- df2 increases rapidly as h decreases
      int gill83(double *hf, double *hphif, double *df, double *df2, double *ef,
                 Eigen::Matrix<double, Eigen::Dynamic, 1>& theta,
                 int cpar, double epsR, int K, double gillStep,
                 double fTol, double gillF) {
        double f , x, hbar, h0, fp, fn=NA_REAL,
          phif, phib, phic, phicc = 0, phi, Chf, Chb,
          Ch, hs, hphi, hk, tmp, ehat, lasth,
          lastht=NA_REAL, lastfpt=NA_REAL, phict=NA_REAL;
        f = gillF;
        int k = 0;
        // Relative error should be given by the tolerances, I believe.
        double epsA=std::fabs(f)*epsR;
        x = theta(cpar, 0);
        // FD1: // Initialization
        hbar = 2*(1+std::fabs(x))*sqrt(epsA/(1+std::fabs(f)));
        h0 = gillStep*hbar;
        lasth=h0;
        theta(cpar, 0) = x + h0;
        fp = fdoubleh(theta);
        theta(cpar, 0) = x - h0;
        fn = fdoubleh(theta);
        phif = phiF(f, fp, h0);
        phib = phiB(f, fn, h0);
        phic = phiC(fp, fn, h0);
        phi = Phi(fp, f, fn, h0);

        Chf = Chat(phif, h0, epsA);
        Chb = Chat(phib, h0, epsA);
        Ch  = ChatP(phi, h0, epsA);
        hs  = -1;
        hphi=hbar; // Not defined in Gill, but used for central difference switch if there are problems
        // FD2:  // Decide if to accept the interval
        hk = h0;
        if (max2(Chf, Chb) <= 0.1){
          hs=h0;
        }
        if (0.001 <= Ch && Ch <= 0.1){
          phicc=phic;
          hphi=h0;
          if (fTol != 0 && fabs(phif) < fTol){
            lastfpt = fp;
            phict=phic;
            lastht  = lasth;
          }
          goto FD5;
        }
        if (fTol != 0 && fabs(phif) < fTol){
          lastfpt = fp;
          lastht  = lasth;
          phict=phic;
        }
        if (Ch < 0.001){
          goto FD4;
        }
      FD3: // Increase h
        k++;
        hk=hk*gillStep;
        lasth=hk;
        // Compute the associated finite difference estimates and their
        // relative condition errors.
        theta(cpar, 0) = x + hk;
        fp = fdoubleh(theta);
        theta(cpar, 0) = x-hk;
        fn = fdoubleh(theta);
        phif = phiF(f, fp, hk);
        phib = phiB(f, fn, hk);
        phic = phiC(fp, fn, hk);
        phi = Phi(fp, f, fn, hk);
        Chf = Chat(phif, hk, epsA);
        Chb = Chat(phib, hk, epsA);
        Ch = ChatP(phi, hk, epsA);
        if (hs < 0 && max2(Chf, Chb) <= 0.1){
          hs = hk;
        }
        if (Ch <= 0.1){
          phicc=phic;
          hphi = hk;
          if (fTol != 0 && fabs(phif) < fTol){
            lastfpt = fp;
            lastht  = lasth;
            phict=phic;
          }
          goto FD5;
        }
        if (fTol != 0 && fabs(phif) < fTol){
          lastfpt = fp;
          lastht  = lasth;
          phict=phic;
        }
        if (k == K) goto FD6;
        goto FD3;
      FD4: // Decrease h
        k++;
        hk=hk/gillStep;
        lasth=hk;
        // Compute the associated finite difference estimates and their
        // relative condition errors.
        theta(cpar, 0) = x + hk;
        fp = fdoubleh(theta);
        theta(cpar, 0) = x-hk;
        fn = fdoubleh(theta);
        phif = phiF(f, fp, hk);
        phib = phiB(f, fn, hk);
        tmp=phic;
        phic = phiC(fp, fn, hk);
        phi = Phi(fp, f, fn, hk);
        Chf = Chat(phif, hk, epsA);
        Chb = Chat(phib, hk, epsA);
        Ch = ChatP(phi, hk, epsA);
        if (Ch > .1){
          phicc=tmp;
          hphi=hk*gillStep; // hphi = h_k-1
          if (fTol != 0 && fabs(phif) < fTol){
            lastfpt = fp;
            lastht  = lasth;
            phict=phic;
          }
          goto FD5;
        }
        if (max2(Chf, Chb) <= 0.1){
          hs = hk;
        }
        if (0.001 <= Ch && Ch <= 1){
          hphi = hk;
          if (fTol != 0 && fabs(phif) < fTol){
            lastfpt = fp;
            lastht  = lasth;
            phict=phic;
          }
          goto FD5;
        }
        if (fTol != 0 && fabs(phif) < fTol){
          lastfpt = fp;
          lastht  = lasth;
          phict=phic;
        }
        if (k == K) goto FD6;
        goto FD4;
      FD5: // Compute the estimate of the optimal interval
        *df2 = phi;
        *hf = 2*sqrt(epsA/fabs(phi));
        theta(cpar, 0) = x + *hf;
        fp = fdoubleh(theta);
        // Restore theta
        theta(cpar, 0) = x;
        *df = phiF(f, fp, *hf);
        *ef = (*hf)*fabs(phi)/2+2*epsA/(*hf);
        *hphif=hphi;
        ehat = fabs(*df-phicc);
        if (max2(*ef, ehat) <= 0.5*(*df)){
          return 1;
        } else {
          // warning("The finite difference derivative err more than 50%% of the slope; Consider a different starting point.");
          if (!ISNA(lastht)){
            // Could be used;  Stick with the last below Ftol
            // *hf = lasth;
            // fp = lastfp;
            // *df = phiF(f, fp, *hf);
            // *df2=0;
            // // *df = 0.0; // Doesn't move.
            // *hphif=2*(*hf);
            // } else {
            *hf = lastht;
            fp = lastfpt;
            *df = phiF(f, fp, *hf);
            *df2=phic;
            // *df = 0.0; // Doesn't move.
            *hphif=phict;
          }
          return 2;
        }
        //
      FD6: // Check unsatisfactory cases
        if (hs < 0){
          // F nearly constant.
          // Use sqrt(h0) as a last ditch effort.
          *hf = pow(DBL_EPSILON, 0.25);//hbar;
          // *df=phic;
          theta(cpar, 0) = x + *hf;
          fp = fdoubleh(theta);
          *df = phiF(f, fp, *hf);
          *df2=0;
          // *df = 0.0; // Doesn't move.
          *hphif= sqrt(h0);
          // warning("The surface around the initial estimate is nearly constant in one parameter grad=0.  Consider a different starting point.");
          return 3;
        }
        if (Ch > 0.1){ // Odd or nearly linear.
          *hf = h0;
          *df = phic;
          *df2 = 0;
          *ef = 2*epsA/(*hf);
          *hphif=hphi;
          // warning("The surface odd or nearly linear for one parameter; Check your function.");
          return 4;
        }
        // f'' is increasing rapidly as h decreases
        *hf = h0;
        *df = phic;
        *df2 = phi;
        *hphif=hphi;
        *ef = (*hf)*fabs(phi)/2+2*epsA/(*hf);
        // warning("The surface around the initial estimate is highly irregular in at least one parameter.  Consider a different starting point.");
        return 5;
      }

      void calcFx(Eigen::Matrix<double, Eigen::Dynamic, 1>& thetaIn) {
        fx_ = fdoubles(thetaIn);
        for (int i = 0; i < ncmt_ + oral0_; i++) {
          Asave_[i] = fx_(i, 0);
        }
        fxIsZero_ = fx_.isZero();
      }

      void constH(Eigen::Matrix<double, Eigen::Dynamic, 1>& thetaIn,
                  double *hh, double hv) {
        calcFx(thetaIn);
        if (hh[0] != 0) return; // keep calculated hh
        if (fxIsZero_) return;
        std::fill_n(hh, thetaIn.size(), hv);
      }

      void gillForwardH(Eigen::Matrix<double, Eigen::Dynamic, 1>& thetaIn,
                        double *hh,
                        double epsR, int K,
                        double gillStep,
                        double gillFtol) {
        calcFx(thetaIn);
        if (hh[0] != 0) return; // keep calculated hh
        if (fxIsZero_) return;
        Eigen::Matrix<double, Eigen::Dynamic, 2> gin = g_;
        double h = 0.0;
        double f0 = fdoubleh(thetaIn);
        double hf=0, hphif=0, df=0, df2=0, ef=0;
        int ret=0;
        for (int i = 0; i <thetaIn.size(); i++) {
          h = 0.0;
          gill83(&hf, &hphif, &df, &df2, &ef,
                 thetaIn, i, epsR, K, gillStep,
                 gillFtol, f0);
          // ret = gill83(&hf, &hphif, &df, &df2, &ef,
          //              thetaIn, epsR, K, gillStep,
          //              gillFtol, f0);
          hh[i] = hf;
        }
        g_ = gin;
      }


      double shiRF(double &h,
                   double ef,
                   Eigen::Matrix<double, Eigen::Dynamic, 1>& thetaIn,
                   int &idx,
                   double &f0, double &f1, double &l, double &u,
                   bool &finiteF1, bool &finiteF4) {
        Eigen::Matrix<double, Eigen::Dynamic, 1> tp4 = thetaIn;
        Eigen::Matrix<double, Eigen::Dynamic, 1> tp1 = thetaIn;
        tp4[idx] += 4*h;
        tp1[idx] += h;
        f1 = fdoubleh(tp1);
        finiteF1 = std::isfinite(f1);
        if (!finiteF1) {
          finiteF4 = true;
          return -1.0;
        }
        double f4 = fdoubleh(tp4);
        finiteF4 = std::isfinite(f4);
        if (!finiteF4) {
          return -1.0;
        }
        // REprintf("f0 = %f f1 = %f f4 = %f\n", f0, f1, f4);
        return abs(f4-4*f1+3*f0)/(8.0*ef);
      }

      double shi21Forward(Eigen::Matrix<double, Eigen::Dynamic, 1> &t,
                          double &h,
                          double &f0,
                          // arma::vec &gr,
                          int idx,
                          double ef,
                          double rl,
                          double ru,
                          int maxiter) {
        // Algorithm 2.1 in paper
        // q=2, alpha=4, r=3
        // s = 0, 1
        // w = -1, 1
        if (h == 0) {
          // 2/sqrt(3) = 1.154700538379251684162
          h = 1.154700538379251684162 * sqrt(ef);
        } else {
          h = fabs(h);
        }
        double l = 0, u = R_PosInf, rcur = NA_REAL;
        double f1;
        double lasth = h;
        int iter=0;
        bool finiteF1 = true, finiteF4 = true, calcGrad = false;
        while(true) {
          iter++;
          if (iter > maxiter) {
            h = lasth;
            break;
          }
          rcur = shiRF(h, ef, t, idx, f0, f1, l, u,
                       finiteF1, finiteF4);
          if (rcur == -1) {
            if (!finiteF1) {
              // hnew = t + 2.5*hold
              h = 0.5*h;
              continue;
            }
            h = 3.5*h;
            if (!calcGrad) {
              lasth = h;
              // gr = (f1-f0)/h;
            }
            continue;
          } else {
            lasth = h;
            // gr = (f1-f0)/h;
          }
          if (rcur < rl) {
            l = h;
          } else if (rcur > ru) {
            u = h;
          } else {
            break;
          }
          if (!R_finite(u)) {
            h = 4.0*h;
          } else if (l == 0) {
            h = h/4.0;
          } else {
            h = (l + u)/2.0;
          }
        }
        return h;
      }

      void fCentralJac(const Eigen::Matrix<double, Eigen::Dynamic, 1>& thetaIn,
                       double *h,
                       Eigen::Matrix<double, Eigen::Dynamic, 1>& fx,
                       Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>& Js) {
        Eigen::Matrix<double, Eigen::Dynamic, 1> fup;
        Eigen::Matrix<double, Eigen::Dynamic, 1> fdown;
        Eigen::Matrix<double , Eigen::Dynamic, 1> thetaCur;
        fx = fx_;
        if (fxIsZero_) {
          Js.setZero();
          return;
        }
        for (int i = 0; i < thetaIn.size(); i++) {
          thetaCur = thetaIn;
          thetaCur(i, 0) += h[i];
          fup = fdoubles(thetaCur);
          thetaCur(i, 0) -= 2*h[i];
          fdown = fdoubles(thetaCur);
          Js.col(i) = (fup - fdown).array()/(2*h[i])*scaleC_(i);
        }
        fx = fdoubles(thetaIn);
        for (int i = 0; i < ncmt_ + oral0_; i++) {
          Asave_[i] = fx(i, 0);
        }
      }

      void fF3Jac(const Eigen::Matrix<double, Eigen::Dynamic, 1>& thetaIn,
                         double *h,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& fx,
                         Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>& Js) {
        Eigen::Matrix<double, Eigen::Dynamic, 1> fh;
        Eigen::Matrix<double, Eigen::Dynamic, 1> f2h;
        Eigen::Matrix<double , Eigen::Dynamic, 1> thetaCur;
        fx = fx_;
        if (fxIsZero_) {
          Js.setZero();
          return;
        }
        for (int i = 0; i < thetaIn.size(); i++) {
          thetaCur = thetaIn;
          thetaCur(i, 0) += h[i];
          fh = fdoubles(thetaCur);
          thetaCur(i, 0) += h[i];
          f2h = fdoubles(thetaCur);
          Js.col(i) = (-3.0*fx + 4.0*fh - f2h).array()/(2*h[i])*scaleC_(i, 0);
        }
      }

      void fEndpoint5Jac(const Eigen::Matrix<double, Eigen::Dynamic, 1>& thetaIn,
                         double *h,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& fx,
                         Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>& Js) {
        Eigen::Matrix<double, Eigen::Dynamic, 1> fh;
        Eigen::Matrix<double, Eigen::Dynamic, 1> f2h;
        Eigen::Matrix<double, Eigen::Dynamic, 1> f3h;
        Eigen::Matrix<double, Eigen::Dynamic, 1> f4h;
        Eigen::Matrix<double , Eigen::Dynamic, 1> thetaCur;
        fx = fx_;
        if (fxIsZero_) {
          Js.setZero();
          return;
        }
        for (int i = 0; i < thetaIn.size(); i++) {
          thetaCur = thetaIn;
          thetaCur(i, 0) += h[i];
          fh = fdoubles(thetaCur);
          thetaCur(i, 0) += h[i];
          f2h = fdoubles(thetaCur);
          thetaCur(i, 0) += h[i];
          f3h = fdoubles(thetaCur);
          thetaCur(i, 0) += h[i];
          f4h = fdoubles(thetaCur);
          Js.col(i) = (-25.0*fx + 48.0*fh -
                       36.0*f2h + 16.0*f3h -
                       3*f4h).array()/(12.0*h[i])*scaleC_(i, 0);
        }
      }


      void fForwardJac(const Eigen::Matrix<double, Eigen::Dynamic, 1>& thetaIn,
                       double *h,
                       Eigen::Matrix<double, Eigen::Dynamic, 1>& fx,
                       Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>& Js) {
        Eigen::Matrix<double, Eigen::Dynamic, 1> fup;
        Eigen::Matrix<double , Eigen::Dynamic, 1> thetaCur;
        fx = fx_;
        if (fxIsZero_) {
          Js.setZero();
          return;
        }
        for (int i = 0; i < thetaIn.size(); i++) {
          thetaCur = thetaIn;
          thetaCur(i, 0) += h[i];
          fup = fdoubles(thetaCur);
          Js.col(i) = (fup - fx).array()/(h[i])*scaleC_(i, 0);
        }
        fx = fdoubles(thetaIn); // This also restores g_
        for (int i = 0; i < ncmt_ + oral0_; i++) {
          Asave_[i] = fx(i, 0);
        }
      }


      void shi21ForwardH(Eigen::Matrix<double, Eigen::Dynamic, 1>& thetaIn,
                         double *hh,
                         double shiErr, int shi21maxFD) {
        calcFx(thetaIn);
        if (hh[0] != 0) return; // keep calculated hh
        if (fxIsZero_) return;
        Eigen::Matrix<double, Eigen::Dynamic, 2> gin = g_;
        double h = 0.0;
        double f0 = fdoubleh(thetaIn);
        for (int i = 0; i <thetaIn.size(); i++) {
          h = 0.0;
          hh[i] = shi21Forward(thetaIn, h, f0, i,
                               shiErr,
                               1.5,
                               6.0,
                               shi21maxFD);
        }
        g_ = gin;
      }

      double shiRC(double &h, double ef,
                   Eigen::Matrix<double, Eigen::Dynamic, 1> &t,
                   int &idx,
                   double &fp1, double &fm1,
                   double &l, double &u,
                   bool &finiteFp1, bool &finiteFp3,
                   bool &finiteFm1, bool &finiteFm3) {
        Eigen::Matrix<double, Eigen::Dynamic, 1> tp3 = t;
        Eigen::Matrix<double, Eigen::Dynamic, 1> tp1 = t;
        Eigen::Matrix<double, Eigen::Dynamic, 1> tm3 = t;
        Eigen::Matrix<double, Eigen::Dynamic, 1> tm1 = t;
        tp3(idx)  += 3*h;
        tp1(idx)  += h;
        tm3(idx)  -= 3*h;
        tm1(idx)  -= h;
        fp1 = fdoubleh(tp1);
        finiteFp1 = std::isfinite(fp1);
        if (!finiteFp1) {
          finiteFm1 = true;
          finiteFp3 = true;
          finiteFm3 = true;
          return -1.0;
        }
        fm1 = fdoubleh(tm1);
        finiteFm1 = std::isfinite(fp1);
        if (!finiteFm1) {
          finiteFp3 = true;
          finiteFm3 = true;
          return -1.0;
        }
        double fp3 = fdoubleh(tp3);
        finiteFp3 = std::isfinite(fp3);
        if (!finiteFp3) {
          finiteFp3 = true;
          return -1.0;
        }
        double fm3 = fdoubleh(tm3);
        finiteFm3 = std::isfinite(fp3);
        if (!finiteFm3) {
          return -1.0;
        }
        return abs(fp3-3*fp1+3*fm1-fm3)/(8.0*ef);
      }

      double shi21Central(Eigen::Matrix<double, Eigen::Dynamic, 1> &t,
                          double &h,
                          double &f0, int idx,
                          double ef, double rl, double ru, double nu,
                          int maxiter) {
        // Algorithm 3.1
        // weights = -0.5, 0.5
        // s = -1, 1
        // Equation 3.3
        //
        if (h == 0.0) {
          h = pow(3.0*ef, 0.3333333333333333333333);
        } else {
          h = fabs(h);
        }
        double l = 0, u = R_PosInf, rcur = NA_REAL;
        double hlast = h;

        double fp1;
        double fm1;

        int iter=0;
        bool finiteFp1 = true, finiteFp3 = true,
          finiteFm1=true, finiteFm3=true, calcGrad=false;
        while(true) {
          iter++;
          if (iter > maxiter) {
            h=hlast;
            break;
          }
          rcur = shiRC(h, ef, t, idx, fp1, fm1, l, u,
                       finiteFp1, finiteFp3, finiteFm1, finiteFm3);
          // Need f1 from shiRF to compute forward difference
          if (rcur == -1.0) {
            if (!finiteFp1) {
              // hnew*3 = hold*0.5
              h = h*0.5/3.0;
              continue;
            } else if (!finiteFm1) {
              if (!calcGrad) {
                // forward difference
                calcGrad = true;
                // gr = (fp1-f0)/h;
              }
              h = h*0.5/3.0;
              continue;
            }
            // hnew*3 = hold*2
            h = h*2.0/3.0;
            if (!calcGrad) {
              // central difference
              calcGrad = true;
              // gr = (fp1-fm1)/(2*h);
              hlast = h;
            }
            continue;
          } else {
            calcGrad = true;
            // gr = (fp1-fm1)/(2*h);
            hlast = h;
          }
          if (rcur < rl) {
            l = h;
          } else if (rcur > ru) {
            u = h;
          } else {
            break;
          }
          if (!R_finite(u)) {
            h = nu*h;
          } else if (l == 0) {
            h = h/nu;
          } else {
            h = (l + u)/2.0;
          }
        }
        return h;
      }

      void shi21CentralH(Eigen::Matrix<double, Eigen::Dynamic, 1>& thetaIn,
                         double *hh, double shiErr, int shi21maxFD) {
        calcFx(thetaIn);
        if (hh[0] != 0) return; // keep calculated hh
        if (fxIsZero_) return;
        Eigen::Matrix<double, Eigen::Dynamic, 2> gin = g_;
        double h = 0.0;
        double f0 = fdoubleh(thetaIn);
        for (int i = 0; i < thetaIn.size(); i++) {
          h = 0.0;
          hh[i] = shi21Central(thetaIn, h, f0, i,
                               shiErr,
                               1.5,
                               4.5,
                               3.0,
                               shi21maxFD);
        }
        g_ = gin;
      }

      void shi21fF3H(Eigen::Matrix<double, Eigen::Dynamic, 1>& thetaIn,
                     double *hh,  double shiErr, int shi21maxFD) {
        calcFx(thetaIn);
        if (hh[0] != 0) return; // keep calculated hh
        if (fxIsZero_) return;
        shi21ForwardH(thetaIn, hh, shiErr, shi21maxFD);
        for (int i = 0; i < thetaIn.size(); i++) {
          hh[i] /= 2.0;
        }
      }

      void shi21fEndpoint5H(Eigen::Matrix<double, Eigen::Dynamic, 1>& thetaIn,
                            double *hh,  double shiErr, int shi21maxFD) {
        calcFx(thetaIn);
        if (hh[0] != 0) return; // keep calculated hh
        if (fxIsZero_) return;
        shi21ForwardH(thetaIn, hh, shiErr, shi21maxFD);
        for (int i = 0; i < thetaIn.size(); i++) {
          hh[i] /= 4.0;
        }
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

      Eigen::Matrix<double, -1, -1> getJacCp(const Eigen::Matrix<double, -1, -1>& J0,
                                             const Eigen::VectorXd& ret0,
                                             const Eigen::Matrix<double, Eigen::Dynamic, 1>& theta,
                                             Eigen::Matrix<double, Eigen::Dynamic, 1>& Jf) {
        Eigen::Matrix<double, Eigen::Dynamic, 1> J = J0.row(oral0_);
        Jf = J;

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
    void linCmtStan::printDouble<double>(Eigen::Matrix<double, Eigen::Dynamic, 1> ret0) const {
      Rcpp::print(Rcpp::wrap(ret0));
    }


    template <>
    void linCmtStan::printDouble<double>(double ret0) const {
      Rcpp::print(Rcpp::wrap(ret0));
    }

  } // namespace math
} // namespace stan

#endif
