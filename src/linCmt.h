#ifndef __LINCMT_H__
#define __LINCMT_H__

#include "macros2micros.h"
#include "solComp.h"
#include "linCmtDiffConstant.h"
#include "../inst/include/rxode2parseHandleEvid.h"
#include "../inst/include/rxode2parseGetTime.h"
#include "par_solve.h"


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
      int ncmt_, oral0_, trans_, id_;
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
      double suspect_ = 1e-6;
      int forwardMax_ = 3;

      Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> J_;
      Eigen::Matrix<double, Eigen::Dynamic, 1> AlastA_;
      Eigen::Matrix<double, Eigen::Dynamic, 1> yp_;
      Eigen::Matrix<double, Eigen::Dynamic, 2> g_;

      Eigen::Matrix<double, Eigen::Dynamic, 1> trueTheta_;
      Eigen::Matrix<double, Eigen::Dynamic, 1> scaleC_;
      Eigen::Matrix<double, Eigen::Dynamic, 1> initPar_;

      Eigen::Matrix<double, Eigen::Dynamic, 1> fx_;
      bool fxIsZero_ = false;
      int sensV1_ = -1;

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
        fxIsZero_ = false;
        sensV1_ = -1;
      }

      void setId(const int id) {
        id_ = id;
      }

      void setForwardOpts(double suspect, int forwardMax) {
        double suspect_ = suspect;
        int forwardMax_ = forwardMax;

      }

      // This will get the initial parameter value
      // when the linear compartment structure is setup.
      Eigen::Matrix<double, Eigen::Dynamic, 1> initPar() {
        return initPar_;
      }

      double initPar(int which) {
        return (initPar_(which, 0) - c1_)/c2_;
      }

      int parDepV1(int i) {
        return (i == sensV1_ && !amtDepV1());
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

      double Rx_pow_di(double base, double exp) {
        if (exp == 2) {
          return base * base;
        }
        if (base == 0 && exp < 0) {
          return 0.0;
        }
        return pow(base, exp);
      }

      double _div0(double x) {
        if (x == 0.0) {
          return 1e-10; // avoid division by zero
        }
        return x;
      }

#define p1 trueTheta_(0, 0)
#define v1 trueTheta_(1, 0)
#define p2 trueTheta_(2, 0)
#define p3 trueTheta_(3, 0)
#define ka trueTheta_(4, 0)

      double scaleC_tran1_2_p1_ka() {
        double rx_expr_0;
        double rx_expr_1;
        double rx_expr_2;
        double rx_expr_3;
        double rx_expr_4;
        double rx_expr_5;
        double rx_expr_6;
        double rx_expr_7;
        double rx_expr_8;
        double rx_expr_9;
        double rx_expr_10;
        double rx_expr_11;
        double rx_expr_12;
        double rx_expr_13;
        double rx_expr_14;
        double rx_expr_15;
        double A0;

        rx_expr_0 =4*p2;
        rx_expr_1 =p1/_div0(v1);
        rx_expr_2 =p2/_div0(p3);
        rx_expr_3 =p2/_div0(v1);
        rx_expr_4 =p3*v1;
        rx_expr_5 =1/_div0((v1));
        rx_expr_6 =exp(-ka);
        rx_expr_7 =rx_expr_1+rx_expr_2;
        rx_expr_8 =rx_expr_0/_div0((rx_expr_4));
        rx_expr_9 =0.5*(rx_expr_5);
        rx_expr_10 =rx_expr_7+rx_expr_3;
        rx_expr_11 =2*(rx_expr_10);
        rx_expr_12 =rx_expr_11/_div0(v1);
        rx_expr_13 =Rx_pow_di((rx_expr_10),2);
        rx_expr_14 =rx_expr_12-rx_expr_8;
        rx_expr_15 =0.25*(rx_expr_14);
        A0=((-rx_expr_6+exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))))*(-0.25*(rx_expr_14)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))-rx_expr_9)/_div0(((ka-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))))-(rx_expr_15/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))-rx_expr_9)*(-rx_expr_6+exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))))/_div0(((ka-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))))+(rx_expr_2-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))*(rx_expr_15/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))-rx_expr_9)*(-rx_expr_6+exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))))/_div0((Rx_pow_di((ka-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))),2)*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))))-(rx_expr_2-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))*(-rx_expr_6+exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))))*(-0.25*(rx_expr_14)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))-rx_expr_9)/_div0((Rx_pow_di((ka-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))),2)*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))))+0.5*exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))*(rx_expr_2-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))*((-0.5)*(rx_expr_14)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))+(rx_expr_5))/_div0(((ka-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))))-0.5*exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))*(rx_expr_2-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))*((0.5)*(rx_expr_14)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))+(rx_expr_5))/_div0(((ka-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))))-0.5*(rx_expr_2-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))*(rx_expr_14)*(-rx_expr_6+exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))))/_div0(((ka-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))*sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)*Rx_pow_di((0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))),2)))+0.5*(rx_expr_2-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))*(rx_expr_14)*(-rx_expr_6+exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))))/_div0(((ka-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))*sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)*Rx_pow_di((0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))),2))))/_div0((-(rx_expr_2-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))*(-rx_expr_6+exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))))/_div0(((ka-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))))+(rx_expr_2-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))*(-rx_expr_6+exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))))/_div0(((ka-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_13)))))));
        return fabs(A0);
      }

      double scaleC_tran1_2_v1_ka() {
        double rx_expr_0;
        double rx_expr_1;
        double rx_expr_2;
        double rx_expr_3;
        double rx_expr_4;
        double rx_expr_5;
        double rx_expr_6;
        double rx_expr_7;
        double rx_expr_8;
        double rx_expr_9;
        double rx_expr_10;
        double rx_expr_11;
        double rx_expr_12;
        double rx_expr_13;
        double rx_expr_14;
        double rx_expr_15;
        double rx_expr_16;
        double rx_expr_17;
        double A0;

        rx_expr_0 =4*p2;
        rx_expr_1 =p2/_div0(p3);
        rx_expr_2 =p1/_div0(v1);
        rx_expr_3 =p2/_div0(v1);
        rx_expr_4 =p3*v1;
        rx_expr_5 =0.5*p1;
        rx_expr_6 =0.5*p2;
        rx_expr_7 =rx_expr_0*p1;
        rx_expr_8 =exp(-ka);
        rx_expr_9 =rx_expr_2+rx_expr_1;
        rx_expr_10 =Rx_pow_di(v1,2);
        rx_expr_11 =rx_expr_9+rx_expr_3;
        rx_expr_12 =p2/_div0(rx_expr_10);
        rx_expr_13 =p3*rx_expr_10;
        rx_expr_14 =rx_expr_5/_div0(rx_expr_10);
        rx_expr_15 =rx_expr_6/_div0(rx_expr_10);
        rx_expr_16 =rx_expr_7/_div0((rx_expr_13));
        rx_expr_17 =Rx_pow_di((rx_expr_11),2);
        A0=v1*(-ka*(-(rx_expr_1-0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(-rx_expr_8+exp(-0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))))/_div0(((ka-0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))))+(rx_expr_1-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(-rx_expr_8+exp(-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))))/_div0(((ka-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))))))/_div0(rx_expr_10)+ka*(-(rx_expr_14+rx_expr_15+0.25*(2*(-p1/_div0(rx_expr_10)-rx_expr_12)*(rx_expr_11)+rx_expr_16)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(-rx_expr_8+exp(-0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))))/_div0(((ka-0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))))+(-rx_expr_8+exp(-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))))*(rx_expr_14+rx_expr_15-0.25*(2*(-p1/_div0(rx_expr_10)-rx_expr_12)*(rx_expr_11)+rx_expr_16)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))/_div0(((ka-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))))+(rx_expr_1-0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(rx_expr_14+rx_expr_15+0.25*(2*(-p1/_div0(rx_expr_10)-rx_expr_12)*(rx_expr_11)+rx_expr_16)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(-rx_expr_8+exp(-0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))))/_div0((Rx_pow_di((ka-0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))),2)*(0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))))-(rx_expr_1-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(-rx_expr_8+exp(-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))))*(rx_expr_14+rx_expr_15-0.25*(2*(-p1/_div0(rx_expr_10)-rx_expr_12)*(rx_expr_11)+rx_expr_16)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))/_div0((Rx_pow_di((ka-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))),2)*(0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))))+0.5*exp(-0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(rx_expr_1-0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(-p1/_div0(rx_expr_10)-rx_expr_12+(-0.5)*(2*(-p1/_div0(rx_expr_10)-rx_expr_12)*(rx_expr_11)+rx_expr_16)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))/_div0(((ka-0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))))-0.5*exp(-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(rx_expr_1-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(-p1/_div0(rx_expr_10)-rx_expr_12+(0.5)*(2*(-p1/_div0(rx_expr_10)-rx_expr_12)*(rx_expr_11)+rx_expr_16)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))/_div0(((ka-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))))-0.5*(rx_expr_1-0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(-rx_expr_8+exp(-0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))))*(2*(-p1/_div0(rx_expr_10)-rx_expr_12)*(rx_expr_11)+rx_expr_16)/_div0(((ka-0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)*Rx_pow_di((0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))),2)))+0.5*(rx_expr_1-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(-rx_expr_8+exp(-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))))*(2*(-p1/_div0(rx_expr_10)-rx_expr_12)*(rx_expr_11)+rx_expr_16)/_div0(((ka-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)*Rx_pow_di((0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))),2))))/_div0(v1))/_div0((ka*(-(rx_expr_1-0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(-rx_expr_8+exp(-0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))))/_div0(((ka-0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))))+(rx_expr_1-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(-rx_expr_8+exp(-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))))/_div0(((ka-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17)))*(0.5*(rx_expr_11-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))-0.5*(rx_expr_11+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_17))))))));
        return fabs(A0);
      }

      double scaleC_tran1_2_p2_ka() {
        double rx_expr_0;
        double rx_expr_1;
        double rx_expr_2;
        double rx_expr_3;
        double rx_expr_4;
        double rx_expr_5;
        double rx_expr_6;
        double rx_expr_7;
        double rx_expr_8;
        double rx_expr_9;
        double rx_expr_10;
        double rx_expr_11;
        double rx_expr_12;
        double rx_expr_13;
        double rx_expr_14;
        double rx_expr_15;
        double rx_expr_16;
        double rx_expr_17;
        double rx_expr_18;
        double rx_expr_19;
        double A0;
        rx_expr_0 =4*p1;
        rx_expr_1 =p1/_div0(v1);
        rx_expr_2 =p2/_div0(p3);
        rx_expr_3 =p2/_div0(v1);
        rx_expr_4 =p3*v1;
        rx_expr_5 =1/_div0((p3));
        rx_expr_6 =1/_div0((v1));
        rx_expr_7 =exp(-ka);
        rx_expr_8 =rx_expr_1+rx_expr_2;
        rx_expr_9 =rx_expr_0/_div0((rx_expr_4));
        rx_expr_10 =0.5*(rx_expr_5);
        rx_expr_11 =0.5*(rx_expr_6);
        rx_expr_12 =(rx_expr_5)+(rx_expr_6);
        rx_expr_13 =rx_expr_8+rx_expr_3;
        rx_expr_14 =2*(rx_expr_12);
        rx_expr_15 =Rx_pow_di((rx_expr_13),2);
        rx_expr_16 =rx_expr_14*(rx_expr_13);
        rx_expr_17 =rx_expr_16-rx_expr_9;
        rx_expr_18 =sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_15);
        rx_expr_19 =0.25*(rx_expr_17);
        A0=(-(rx_expr_19/_div0(rx_expr_18)+rx_expr_10-rx_expr_11)*(-rx_expr_7+exp(-0.5*(rx_expr_13-rx_expr_18)))/_div0(((ka-0.5*(rx_expr_13-rx_expr_18))*(0.5*(rx_expr_13-rx_expr_18)-0.5*(rx_expr_13+rx_expr_18))))+(-rx_expr_7+exp(-0.5*(rx_expr_13+rx_expr_18)))*(-0.25*(rx_expr_17)/_div0(rx_expr_18)+rx_expr_10-rx_expr_11)/_div0(((ka-0.5*(rx_expr_13+rx_expr_18))*(0.5*(rx_expr_13-rx_expr_18)-0.5*(rx_expr_13+rx_expr_18))))+(rx_expr_2-0.5*(rx_expr_13-rx_expr_18))*(-rx_expr_7+exp(-0.5*(rx_expr_13-rx_expr_18)))*(rx_expr_19/_div0(rx_expr_18)-rx_expr_10-rx_expr_11)/_div0((Rx_pow_di((ka-0.5*(rx_expr_13-rx_expr_18)),2)*(0.5*(rx_expr_13-rx_expr_18)-0.5*(rx_expr_13+rx_expr_18))))-(rx_expr_2-0.5*(rx_expr_13+rx_expr_18))*(-0.25*(rx_expr_17)/_div0(rx_expr_18)-rx_expr_10-rx_expr_11)*(-rx_expr_7+exp(-0.5*(rx_expr_13+rx_expr_18)))/_div0((Rx_pow_di((ka-0.5*(rx_expr_13+rx_expr_18)),2)*(0.5*(rx_expr_13-rx_expr_18)-0.5*(rx_expr_13+rx_expr_18))))+0.5*exp(-0.5*(rx_expr_13-rx_expr_18))*(rx_expr_2-0.5*(rx_expr_13-rx_expr_18))*((-0.5)*(rx_expr_17)/_div0(rx_expr_18)+(rx_expr_5)+(rx_expr_6))/_div0(((ka-0.5*(rx_expr_13-rx_expr_18))*(0.5*(rx_expr_13-rx_expr_18)-0.5*(rx_expr_13+rx_expr_18))))-0.5*exp(-0.5*(rx_expr_13+rx_expr_18))*(rx_expr_2-0.5*(rx_expr_13+rx_expr_18))*((0.5)*(rx_expr_17)/_div0(rx_expr_18)+(rx_expr_5)+(rx_expr_6))/_div0(((ka-0.5*(rx_expr_13+rx_expr_18))*(0.5*(rx_expr_13-rx_expr_18)-0.5*(rx_expr_13+rx_expr_18))))-0.5*(rx_expr_2-0.5*(rx_expr_13-rx_expr_18))*(-rx_expr_7+exp(-0.5*(rx_expr_13-rx_expr_18)))*(rx_expr_17)/_div0(((ka-0.5*(rx_expr_13-rx_expr_18))*rx_expr_18*Rx_pow_di((0.5*(rx_expr_13-rx_expr_18)-0.5*(rx_expr_13+rx_expr_18)),2)))+0.5*(rx_expr_2-0.5*(rx_expr_13+rx_expr_18))*(-rx_expr_7+exp(-0.5*(rx_expr_13+rx_expr_18)))*(rx_expr_17)/_div0(((ka-0.5*(rx_expr_13+rx_expr_18))*rx_expr_18*Rx_pow_di((0.5*(rx_expr_13-rx_expr_18)-0.5*(rx_expr_13+rx_expr_18)),2))))/_div0((-(rx_expr_2-0.5*(rx_expr_13-rx_expr_18))*(-rx_expr_7+exp(-0.5*(rx_expr_13-rx_expr_18)))/_div0(((ka-0.5*(rx_expr_13-rx_expr_18))*(0.5*(rx_expr_13-rx_expr_18)-0.5*(rx_expr_13+rx_expr_18))))+(rx_expr_2-0.5*(rx_expr_13+rx_expr_18))*(-rx_expr_7+exp(-0.5*(rx_expr_13+rx_expr_18)))/_div0(((ka-0.5*(rx_expr_13+rx_expr_18))*(0.5*(rx_expr_13-rx_expr_18)-0.5*(rx_expr_13+rx_expr_18))))));
        return fabs(A0);
      }

      double scaleC_tran1_2_p3_ka() {
        double rx_expr_0;
        double rx_expr_1;
        double rx_expr_2;
        double rx_expr_3;
        double rx_expr_4;
        double rx_expr_5;
        double rx_expr_6;
        double rx_expr_7;
        double rx_expr_8;
        double rx_expr_9;
        double rx_expr_10;
        double rx_expr_11;
        double rx_expr_12;
        double rx_expr_13;
        double rx_expr_14;
        double A0;


        rx_expr_0 =4*p2;
        rx_expr_1 =p1/_div0(v1);
        rx_expr_2 =p2/_div0(p3);
        rx_expr_3 =p2/_div0(v1);
        rx_expr_4 =p3*v1;
        rx_expr_5 =0.5*p2;
        rx_expr_6 =rx_expr_0*p1;
        rx_expr_7 =exp(-ka);
        rx_expr_8 =rx_expr_1+rx_expr_2;
        rx_expr_9 =Rx_pow_di(p3,2);
        rx_expr_10 =rx_expr_8+rx_expr_3;
        rx_expr_11 =rx_expr_9*v1;
        rx_expr_12 =rx_expr_5/_div0(rx_expr_9);
        rx_expr_13 =rx_expr_6/_div0((rx_expr_11));
        rx_expr_14 =Rx_pow_di((rx_expr_10),2);
        A0=(-(-rx_expr_7+exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))))*(-0.5*p2/_div0(rx_expr_9)+0.25*(-2*p2*(rx_expr_10)/_div0(rx_expr_9)+rx_expr_13)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))/_div0(((ka-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))))+(-rx_expr_7+exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))))*(-0.5*p2/_div0(rx_expr_9)-0.25*(-2*p2*(rx_expr_10)/_div0(rx_expr_9)+rx_expr_13)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))/_div0(((ka-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))))+(rx_expr_2-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(-rx_expr_7+exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))))*(rx_expr_12+0.25*(-2*p2*(rx_expr_10)/_div0(rx_expr_9)+rx_expr_13)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))/_div0((Rx_pow_di((ka-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))),2)*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))))-(rx_expr_2-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(-rx_expr_7+exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))))*(rx_expr_12-0.25*(-2*p2*(rx_expr_10)/_div0(rx_expr_9)+rx_expr_13)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))/_div0((Rx_pow_di((ka-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))),2)*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))))+0.5*exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(rx_expr_2-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(-p2/_div0(rx_expr_9)+(-0.5)*(-2*p2*(rx_expr_10)/_div0(rx_expr_9)+rx_expr_13)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))/_div0(((ka-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))))-0.5*exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(rx_expr_2-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(-p2/_div0(rx_expr_9)+(0.5)*(-2*p2*(rx_expr_10)/_div0(rx_expr_9)+rx_expr_13)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))/_div0(((ka-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))))-0.5*(-2*p2*(rx_expr_10)/_div0(rx_expr_9)+rx_expr_13)*(rx_expr_2-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(-rx_expr_7+exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))))/_div0(((ka-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)*Rx_pow_di((0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))),2)))+0.5*(-2*p2*(rx_expr_10)/_div0(rx_expr_9)+rx_expr_13)*(rx_expr_2-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(-rx_expr_7+exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))))/_div0(((ka-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)*Rx_pow_di((0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))),2))))/_div0((-(rx_expr_2-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(-rx_expr_7+exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))))/_div0(((ka-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))))+(rx_expr_2-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(-rx_expr_7+exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))))/_div0(((ka-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))))));
        return fabs(A0);
      }

      double scaleC_tran1_2_ka() {
        double rx_expr_0;
        double rx_expr_1;
        double rx_expr_2;
        double rx_expr_3;
        double rx_expr_4;
        double rx_expr_5;
        double rx_expr_6;
        double rx_expr_7;
        double rx_expr_8;
        double rx_expr_9;
        double rx_expr_10;
        double rx_expr_11;
        double rx_expr_12;
        double rx_expr_13;
        double rx_expr_14;
        double A0;


        rx_expr_0 =4*p2;
        rx_expr_1 =p1/_div0(v1);
        rx_expr_2 =p2/_div0(p3);
        rx_expr_3 =p2/_div0(v1);
        rx_expr_4 =p3*v1;
        rx_expr_5 =0.5*p2;
        rx_expr_6 =rx_expr_0*p1;
        rx_expr_7 =exp(-ka);
        rx_expr_8 =rx_expr_1+rx_expr_2;
        rx_expr_9 =Rx_pow_di(p3,2);
        rx_expr_10 =rx_expr_8+rx_expr_3;
        rx_expr_11 =rx_expr_9*v1;
        rx_expr_12 =rx_expr_5/_div0(rx_expr_9);
        rx_expr_13 =rx_expr_6/_div0((rx_expr_11));
        rx_expr_14 =Rx_pow_di((rx_expr_10),2);
        A0=(-(-rx_expr_7+exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))))*(-0.5*p2/_div0(rx_expr_9)+0.25*(-2*p2*(rx_expr_10)/_div0(rx_expr_9)+rx_expr_13)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))/_div0(((ka-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))))+(-rx_expr_7+exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))))*(-0.5*p2/_div0(rx_expr_9)-0.25*(-2*p2*(rx_expr_10)/_div0(rx_expr_9)+rx_expr_13)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))/_div0(((ka-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))))+(rx_expr_2-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(-rx_expr_7+exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))))*(rx_expr_12+0.25*(-2*p2*(rx_expr_10)/_div0(rx_expr_9)+rx_expr_13)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))/_div0((Rx_pow_di((ka-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))),2)*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))))-(rx_expr_2-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(-rx_expr_7+exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))))*(rx_expr_12-0.25*(-2*p2*(rx_expr_10)/_div0(rx_expr_9)+rx_expr_13)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))/_div0((Rx_pow_di((ka-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))),2)*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))))+0.5*exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(rx_expr_2-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(-p2/_div0(rx_expr_9)+(-0.5)*(-2*p2*(rx_expr_10)/_div0(rx_expr_9)+rx_expr_13)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))/_div0(((ka-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))))-0.5*exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(rx_expr_2-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(-p2/_div0(rx_expr_9)+(0.5)*(-2*p2*(rx_expr_10)/_div0(rx_expr_9)+rx_expr_13)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))/_div0(((ka-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))))-0.5*(-2*p2*(rx_expr_10)/_div0(rx_expr_9)+rx_expr_13)*(rx_expr_2-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(-rx_expr_7+exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))))/_div0(((ka-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)*Rx_pow_di((0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))),2)))+0.5*(-2*p2*(rx_expr_10)/_div0(rx_expr_9)+rx_expr_13)*(rx_expr_2-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(-rx_expr_7+exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))))/_div0(((ka-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)*Rx_pow_di((0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))),2))))/_div0((-(rx_expr_2-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(-rx_expr_7+exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))))/_div0(((ka-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))))+(rx_expr_2-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(-rx_expr_7+exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))))/_div0(((ka-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))*(0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14)))))));
        return fabs(A0);
      }

      double scaleC_tran1_2_p1() {
        // generated and modified
        double rx_expr_0;
        double rx_expr_1;
        double rx_expr_2;
        double rx_expr_3;
        double rx_expr_4;
        double rx_expr_5;
        double rx_expr_6;
        double rx_expr_7;
        double rx_expr_8;
        double rx_expr_9;
        double rx_expr_10;
        double rx_expr_11;
        double rx_expr_12;
        double rx_expr_13;
        double A0;

        rx_expr_0 =4*p2;
        rx_expr_1 =p1/_div0(v1);
        rx_expr_2 =p2/_div0(p3);
        rx_expr_3 =p2/_div0(v1);
        rx_expr_4 =p3*v1;
        rx_expr_5 =1/_div0((v1));
        rx_expr_6 =rx_expr_1+rx_expr_2;
        rx_expr_7 =rx_expr_0/_div0((rx_expr_4));
        rx_expr_8 =0.5*(rx_expr_5);
        rx_expr_9 =rx_expr_6+rx_expr_3;
        rx_expr_10 =2*(rx_expr_9);
        rx_expr_11 =rx_expr_10/_div0(v1);
        rx_expr_12 =Rx_pow_di((rx_expr_9),2);
        rx_expr_13 =rx_expr_11-rx_expr_7;
        A0=(-exp(-0.5*(rx_expr_9-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12)))*(0.25*(rx_expr_13)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))-rx_expr_8)/_div0((0.5*(rx_expr_9-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))-0.5*(rx_expr_9+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))))+exp(-0.5*(rx_expr_9+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12)))*(-0.25*(rx_expr_13)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))-rx_expr_8)/_div0((0.5*(rx_expr_9-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))-0.5*(rx_expr_9+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))))+0.5*exp(-0.5*(rx_expr_9-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12)))*(rx_expr_2-0.5*(rx_expr_9-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12)))*((-0.5)*(rx_expr_13)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))+(rx_expr_5))/_div0((0.5*(rx_expr_9-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))-0.5*(rx_expr_9+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))))-0.5*exp(-0.5*(rx_expr_9+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12)))*(rx_expr_2-0.5*(rx_expr_9+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12)))*((0.5)*(rx_expr_13)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))+(rx_expr_5))/_div0((0.5*(rx_expr_9-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))-0.5*(rx_expr_9+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))))-0.5*exp(-0.5*(rx_expr_9-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12)))*(rx_expr_2-0.5*(rx_expr_9-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12)))*(rx_expr_13)/_div0((sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12)*Rx_pow_di((0.5*(rx_expr_9-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))-0.5*(rx_expr_9+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))),2)))+0.5*exp(-0.5*(rx_expr_9+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12)))*(rx_expr_2-0.5*(rx_expr_9+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12)))*(rx_expr_13)/_div0((sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12)*Rx_pow_di((0.5*(rx_expr_9-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))-0.5*(rx_expr_9+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))),2))))/_div0((-exp(-0.5*(rx_expr_9-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12)))*(rx_expr_2-0.5*(rx_expr_9-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12)))/_div0((0.5*(rx_expr_9-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))-0.5*(rx_expr_9+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))))+exp(-0.5*(rx_expr_9+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12)))*(rx_expr_2-0.5*(rx_expr_9+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12)))/_div0((0.5*(rx_expr_9-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))-0.5*(rx_expr_9+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_12))))));
        return fabs(A0);
      }

      double scaleC_tran1_2_v1() {
        double rx_expr_0;
        double rx_expr_1;
        double rx_expr_2;
        double rx_expr_3;
        double rx_expr_4;
        double rx_expr_5;
        double rx_expr_6;
        double rx_expr_7;
        double rx_expr_8;
        double rx_expr_9;
        double rx_expr_10;
        double rx_expr_11;
        double rx_expr_12;
        double rx_expr_13;
        double rx_expr_14;
        double rx_expr_15;
        double rx_expr_16;
        double A0;
        rx_expr_0 =4*p2;
        rx_expr_1 =p1/_div0(v1);
        rx_expr_2 =p2/_div0(p3);
        rx_expr_3 =p2/_div0(v1);
        rx_expr_4 =p3*v1;
        rx_expr_5 =0.5*p1;
        rx_expr_6 =0.5*p2;
        rx_expr_7 =rx_expr_0*p1;
        rx_expr_8 =rx_expr_1+rx_expr_2;
        rx_expr_9 =Rx_pow_di(v1,2);
        rx_expr_10 =rx_expr_8+rx_expr_3;
        rx_expr_11 =p2/_div0(rx_expr_9);
        rx_expr_12 =p3*rx_expr_9;
        rx_expr_13 =rx_expr_5/_div0(rx_expr_9);
        rx_expr_14 =rx_expr_6/_div0(rx_expr_9);
        rx_expr_15 =rx_expr_7/_div0((rx_expr_12));
        rx_expr_16 =Rx_pow_di((rx_expr_10),2);
        A0=v1*(-(-exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))*(rx_expr_2-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))/_div0((0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16))))+exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))*(rx_expr_2-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))/_div0((0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))))/_div0(rx_expr_9)+(-exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))*(rx_expr_13+rx_expr_14+0.25*(2*(-p1/_div0(rx_expr_9)-rx_expr_11)*(rx_expr_10)+rx_expr_15)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))/_div0((0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16))))+exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))*(rx_expr_13+rx_expr_14-0.25*(2*(-p1/_div0(rx_expr_9)-rx_expr_11)*(rx_expr_10)+rx_expr_15)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))/_div0((0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16))))+0.5*exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))*(rx_expr_2-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))*(-p1/_div0(rx_expr_9)-rx_expr_11+(-0.5)*(2*(-p1/_div0(rx_expr_9)-rx_expr_11)*(rx_expr_10)+rx_expr_15)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))/_div0((0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16))))-0.5*exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))*(rx_expr_2-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))*(-p1/_div0(rx_expr_9)-rx_expr_11+(0.5)*(2*(-p1/_div0(rx_expr_9)-rx_expr_11)*(rx_expr_10)+rx_expr_15)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))/_div0((0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16))))-0.5*exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))*(rx_expr_2-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))*(2*(-p1/_div0(rx_expr_9)-rx_expr_11)*(rx_expr_10)+rx_expr_15)/_div0((sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)*Rx_pow_di((0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16))),2)))+0.5*exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))*(rx_expr_2-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))*(2*(-p1/_div0(rx_expr_9)-rx_expr_11)*(rx_expr_10)+rx_expr_15)/_div0((sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)*Rx_pow_di((0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16))),2))))/_div0(v1))/_div0((-exp(-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))*(rx_expr_2-0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))/_div0((0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16))))+exp(-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))*(rx_expr_2-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16)))/_div0((0.5*(rx_expr_10-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16))-0.5*(rx_expr_10+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_16))))));
        return fabs(A0);
      }

      double scaleC_tran1_2_p2() {
        double rx_expr_0;
        double rx_expr_1;
        double rx_expr_2;
        double rx_expr_3;
        double rx_expr_4;
        double rx_expr_5;
        double rx_expr_6;
        double rx_expr_7;
        double rx_expr_8;
        double rx_expr_9;
        double rx_expr_10;
        double rx_expr_11;
        double rx_expr_12;
        double rx_expr_13;
        double rx_expr_14;
        double rx_expr_15;
        double rx_expr_16;
        double rx_expr_17;
        double A0;
        rx_expr_0 =4*p1;
        rx_expr_1 =p1/_div0(v1);
        rx_expr_2 =p2/_div0(p3);
        rx_expr_3 =p2/_div0(v1);
        rx_expr_4 =p3*v1;
        rx_expr_5 =1/_div0((p3));
        rx_expr_6 =1/_div0((v1));
        rx_expr_7 =rx_expr_1+rx_expr_2;
        rx_expr_8 =rx_expr_0/_div0((rx_expr_4));
        rx_expr_9 =0.5*(rx_expr_5);
        rx_expr_10 =0.5*(rx_expr_6);
        rx_expr_11 =rx_expr_7+rx_expr_3;
        rx_expr_12 =(rx_expr_5)+(rx_expr_6);
        rx_expr_13 =2*(rx_expr_12);
        rx_expr_14 =Rx_pow_di((rx_expr_11),2);
        rx_expr_15 =rx_expr_13*(rx_expr_11);
        rx_expr_16 =rx_expr_15-rx_expr_8;
        rx_expr_17 =sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_14);
        A0=(-exp(-0.5*(rx_expr_11-rx_expr_17))*(0.25*(rx_expr_16)/_div0(rx_expr_17)+rx_expr_9-rx_expr_10)/_div0((0.5*(rx_expr_11-rx_expr_17)-0.5*(rx_expr_11+rx_expr_17)))+exp(-0.5*(rx_expr_11+rx_expr_17))*(-0.25*(rx_expr_16)/_div0(rx_expr_17)+rx_expr_9-rx_expr_10)/_div0((0.5*(rx_expr_11-rx_expr_17)-0.5*(rx_expr_11+rx_expr_17)))+0.5*exp(-0.5*(rx_expr_11-rx_expr_17))*(rx_expr_2-0.5*(rx_expr_11-rx_expr_17))*((-0.5)*(rx_expr_16)/_div0(rx_expr_17)+(rx_expr_5)+(rx_expr_6))/_div0((0.5*(rx_expr_11-rx_expr_17)-0.5*(rx_expr_11+rx_expr_17)))-0.5*exp(-0.5*(rx_expr_11+rx_expr_17))*(rx_expr_2-0.5*(rx_expr_11+rx_expr_17))*((0.5)*(rx_expr_16)/_div0(rx_expr_17)+(rx_expr_5)+(rx_expr_6))/_div0((0.5*(rx_expr_11-rx_expr_17)-0.5*(rx_expr_11+rx_expr_17)))-0.5*exp(-0.5*(rx_expr_11-rx_expr_17))*(rx_expr_2-0.5*(rx_expr_11-rx_expr_17))*(rx_expr_16)/_div0((rx_expr_17*Rx_pow_di((0.5*(rx_expr_11-rx_expr_17)-0.5*(rx_expr_11+rx_expr_17)),2)))+0.5*exp(-0.5*(rx_expr_11+rx_expr_17))*(rx_expr_2-0.5*(rx_expr_11+rx_expr_17))*(rx_expr_16)/_div0((rx_expr_17*Rx_pow_di((0.5*(rx_expr_11-rx_expr_17)-0.5*(rx_expr_11+rx_expr_17)),2))))/_div0((-exp(-0.5*(rx_expr_11-rx_expr_17))*(rx_expr_2-0.5*(rx_expr_11-rx_expr_17))/_div0((0.5*(rx_expr_11-rx_expr_17)-0.5*(rx_expr_11+rx_expr_17)))+exp(-0.5*(rx_expr_11+rx_expr_17))*(rx_expr_2-0.5*(rx_expr_11+rx_expr_17))/_div0((0.5*(rx_expr_11-rx_expr_17)-0.5*(rx_expr_11+rx_expr_17)))));
        return fabs(A0);
      }

      double scaleC_tran1_2_p3() {
        double rx_expr_0;
        double rx_expr_1;
        double rx_expr_2;
        double rx_expr_3;
        double rx_expr_4;
        double rx_expr_5;
        double rx_expr_6;
        double rx_expr_7;
        double rx_expr_8;
        double rx_expr_9;
        double rx_expr_10;
        double rx_expr_11;
        double A0;

        rx_expr_0 =4*p2;
        rx_expr_1 =p1/_div0(v1);
        rx_expr_2 =p2/_div0(p3);
        rx_expr_3 =p2/_div0(v1);
        rx_expr_4 =p3*v1;
        rx_expr_5 =rx_expr_0*p1;
        rx_expr_6 =rx_expr_1+rx_expr_2;
        rx_expr_7 =Rx_pow_di(p3,2);
        rx_expr_8 =rx_expr_6+rx_expr_3;
        rx_expr_9 =rx_expr_7*v1;
        rx_expr_10 =rx_expr_5/_div0((rx_expr_9));
        rx_expr_11 =Rx_pow_di((rx_expr_8),2);
        A0=(-exp(-0.5*(rx_expr_8-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)))*(-0.5*p2/_div0(rx_expr_7)+0.25*(-2*p2*(rx_expr_8)/_div0(rx_expr_7)+rx_expr_10)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)))/_div0((0.5*(rx_expr_8-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11))-0.5*(rx_expr_8+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11))))+exp(-0.5*(rx_expr_8+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)))*(-0.5*p2/_div0(rx_expr_7)-0.25*(-2*p2*(rx_expr_8)/_div0(rx_expr_7)+rx_expr_10)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)))/_div0((0.5*(rx_expr_8-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11))-0.5*(rx_expr_8+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11))))+0.5*exp(-0.5*(rx_expr_8-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)))*(rx_expr_2-0.5*(rx_expr_8-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)))*(-p2/_div0(rx_expr_7)+(-0.5)*(-2*p2*(rx_expr_8)/_div0(rx_expr_7)+rx_expr_10)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)))/_div0((0.5*(rx_expr_8-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11))-0.5*(rx_expr_8+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11))))-0.5*exp(-0.5*(rx_expr_8+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)))*(rx_expr_2-0.5*(rx_expr_8+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)))*(-p2/_div0(rx_expr_7)+(0.5)*(-2*p2*(rx_expr_8)/_div0(rx_expr_7)+rx_expr_10)/_div0(sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)))/_div0((0.5*(rx_expr_8-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11))-0.5*(rx_expr_8+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11))))-0.5*exp(-0.5*(rx_expr_8-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)))*(-2*p2*(rx_expr_8)/_div0(rx_expr_7)+rx_expr_10)*(rx_expr_2-0.5*(rx_expr_8-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)))/_div0((sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)*Rx_pow_di((0.5*(rx_expr_8-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11))-0.5*(rx_expr_8+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11))),2)))+0.5*exp(-0.5*(rx_expr_8+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)))*(-2*p2*(rx_expr_8)/_div0(rx_expr_7)+rx_expr_10)*(rx_expr_2-0.5*(rx_expr_8+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)))/_div0((sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)*Rx_pow_di((0.5*(rx_expr_8-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11))-0.5*(rx_expr_8+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11))),2))))/_div0((-exp(-0.5*(rx_expr_8-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)))*(rx_expr_2-0.5*(rx_expr_8-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)))/_div0((0.5*(rx_expr_8-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11))-0.5*(rx_expr_8+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11))))+exp(-0.5*(rx_expr_8+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)))*(rx_expr_2-0.5*(rx_expr_8+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11)))/_div0((0.5*(rx_expr_8-sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11))-0.5*(rx_expr_8+sqrt(-4*p2*p1/_div0((rx_expr_4))+rx_expr_11))))));
        return fabs(A0);
      }

#undef p1
#undef v1
#undef p2
#undef p3
#undef ka
      double scaleC_tran1_2(int d) {
        if (oral0_ == 0) {
          switch (d) {
          case diffP1:
            return scaleC_tran1_2_p1();
          case diffV1:
            return scaleC_tran1_2_v1();
          case diffP2:
            return scaleC_tran1_2_p2();
          case diffP3:
            return scaleC_tran1_2_p3();
          default:
            return 1.0; // Default scaling if no match
          }
        } else {
          switch (d) {
          case diffP1:
            return scaleC_tran1_2_p1_ka();
          case diffV1:
            return scaleC_tran1_2_v1_ka();
          case diffP2:
            return scaleC_tran1_2_p2_ka();
          case diffP3:
            return scaleC_tran1_2_p3_ka();
          case diffKa:
            return scaleC_tran1_2_ka();
          default:
            return 1.0; // Default scaling if no match
          }
        }
        return 1.0;
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
                        double &mn, double &mx, double *scale,
                        bool &unscaled) {
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
              scaleC_(i, 0) = 1.0;
//               int sw = ncmt_ + 10*trans_;
//               switch (sw) {
//               case 11: // cl v
// //                 if (d == diffP1) {
// //                   // exp(-cl/v)/v
// //                   // > D(S("log(exp(-cl/v)/v)"), "cl")
// //                   // (Mul) -1/v
// //                   scaleC_(i, 0) = 1.0/trueTheta_(1, 0);
// //                 } else if (d == diffV1) {
// //                   // > D(S("log(exp(-cl/v)/v)"), "v")
// //                   //   (Mul)   v*exp(cl/v)*(-exp(-cl/v)/v^2 + exp(-cl/v)*cl/v^3)
// // #define v trueTheta_(1, 0)
// // #define cl trueTheta_(0, 0)
// //                   scaleC_(i, 0) = fabs(v*exp(cl/v)*(-exp(-cl/v)/(v*v) + exp(-cl/v)*cl/(v*v*v)));
// // #undef v
// // #undef cl

// //                 }
//                 break;
//               case 21: // Kel v
//                 // if (d == diffP1) {
//                   // > D(S("log(exp(-kel)/v)"), "kel")
//                   // (Integer)  -1
//                   // already 1
//                 // } else
//                 // if (d == diffV1) {
//                 //   // D(S("log(exp(-kel)/v)"), "v")
//                 //   // (Mul)-1/v
//                 //   scaleC_(i,0) = 1.0/trueTheta_(1, 0);
//                 // }
//                 break;
//               case 101: // alpha a
//                 // > D(S("log(exp(-alpha)*a)"), "a")
//                 // (Pow)   a^(-1)
//                 // if (d == diffV1) {
//                 //   scaleC_(i,0) = 1.0/trueTheta_(1, 0);
//                 // }
//                 break;

//               case 12: //cl v q vp
//                 // scaleC_(i, 0) = scaleC_tran1_2(d);
//                 break;
//               case 22:// k=(*p1) v=(*v1) k12=(*p2) k21=(*p3)
//                 break;
//               case 32: // cl=(*p1) v=(*v1) q=(*p2) vss=(*p3)
//                 break;
//               case 42: // alpha=(*p1) beta=(*p2) k21=(*p3)
//                 break;
//               case 52:// alpha=(*p1) beta=(*p2) aob=(*p3)
//                 break;
//               case 112: // A2 V, alpha=(*p1), beta=(*p2), k21
//                 break;
//               case 102: // A=(*v1), alpha=(*p1), beta=(*p2), B=(*p3)
//                 break;

//                 //
//               case 13: // cl v q vp q2 vp2
//                 break;
//               case 23: // k=(*p1) v=(*v1) k12=(*p2) k21=(*p3) k13=(*p4) k31=(*p5)
//                 break;
//               case 113: //A B and C, alpha, beta, gamma, 3 compartment model
//                 break;
//               case 103: // vc B and C, alpha, beta, gamma, 3 compartment model
//                 break;
//               }


              switch (d) {
              case diffP1:
                if (scale[0] > 0) {
                  scaleC_(i, 0) *= scale[0];
                } else {
                  unscaled = true;
                }
                break;
              case diffV1:
                if (scale[1] > 0) {
                  scaleC_(i, 0) *= scale[1];
                } else {
                  unscaled = true;
                }
                break;
              case diffP2:
                if (scale[2] > 0) {
                  scaleC_(i, 0) *= scale[2];
                } else {
                  unscaled = true;
                }
                break;
              case diffP3:
                if (scale[3] > 0) {
                  scaleC_(i, 0) *= scale[3];
                } else {
                  unscaled = true;
                }
                break;
              case diffP4:
                if (scale[4] > 0) {
                  scaleC_(i, 0) *= scale[4];
                } else {
                  unscaled = true;
                }
                break;
              case diffP5:
                if (scale[5] > 0) {
                  scaleC_(i, 0) *= scale[5];
                } else {
                  unscaled = true;
                }
                break;
              case diffKa:
                if (scale[6] > 0) {
                  scaleC_(i, 0) *= scale[6];
                } else {
                  unscaled = true;
                }
                break;
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
                     bool isAD, double *scale) {
        trueTheta_ = theta;
        isAD_ = isAD;
        int nd = numDiff_;
        if (nd == 0) {
          int sw = ncmt_*10+oral0_;
          switch (sw) {
          case 11: // cl v ka
            nd = diffP1 + diffV1 + diffKa;
            break;
          case 10: // cl v
            nd = diffP1 + diffV1;
            break;
          case 21: // cl v q v2 ka
            nd = diffP1 + diffV1 + diffP2 + diffP3 + diffKa;
            break;
          case 20: // cl v q v2
            nd = diffP1 + diffV1 + diffP2 + diffP3;
            break;
          case 31: // cl v q v2 q2 v3 ka
            nd = diffP1 + diffV1 + diffP2 + diffP3 + diffP4 + diffP5 + diffKa;
            break;
          case 30: // cl v q v2 q2 v3
            nd = diffP1 + diffV1 + diffP2 + diffP3 + diffP4 + diffP5;
            break;
          default:
            Rcpp::stop("Unknown linear compartment model: %d %d", ncmt_, oral0_);
          }
        }
        int i = 0, j=0;

        if (!isAD && !scaleSetup_) {
          // Setup the sizes for scales
          initPar_.resize(sensTheta.size());
          scaleC_.resize(sensTheta.size());
        }

        double mx=R_NegInf, mn = R_PosInf;

        bool unscaled = false;

        switch (ncmt_) {
        case 1: {
          sensThetaElt(diffP1, theta, sensTheta, nd, i, j, mn, mx, scale, unscaled);
          sensThetaElt(diffV1, theta, sensTheta, nd, i, j, mn, mx, scale, unscaled);
          if (oral0_) sensThetaElt(diffKa, theta, sensTheta, nd, i, j, mn, mx, scale, unscaled);
        }
          break;
        case 2: {
          sensThetaElt(diffP1, theta, sensTheta, nd, i, j, mn, mx, scale, unscaled);
          sensThetaElt(diffV1, theta, sensTheta, nd, i, j, mn, mx, scale, unscaled);
          sensThetaElt(diffP2, theta, sensTheta, nd, i, j, mn, mx, scale, unscaled);
          sensThetaElt(diffP3, theta, sensTheta, nd, i, j, mn, mx, scale, unscaled);
          if (oral0_) sensThetaElt(diffKa, theta, sensTheta, nd, i, j, mn, mx, scale, unscaled);
        }
          break;
        case 3: {
          sensThetaElt(diffP1, theta, sensTheta, nd, i, j, mn, mx, scale, unscaled);
          sensThetaElt(diffV1, theta, sensTheta, nd, i, j, mn, mx, scale, unscaled);
          sensThetaElt(diffP2, theta, sensTheta, nd, i, j, mn, mx, scale, unscaled);
          sensThetaElt(diffP3, theta, sensTheta, nd, i, j, mn, mx, scale, unscaled);
          sensThetaElt(diffP4, theta, sensTheta, nd, i, j, mn, mx, scale, unscaled);
          sensThetaElt(diffP5, theta, sensTheta, nd, i, j, mn, mx, scale, unscaled);
          if (oral0_) sensThetaElt(diffKa, theta, sensTheta, nd, i, j, mn, mx, scale, unscaled);
          }
          break;
        }
        // This finishes the scaling setup
        if (!isAD_ && !scaleSetup_) {
          if (unscaled || fabs(mx-mn) < DBL_EPSILON) {
            c1_ = 0.0;
            c2_ = 1.0;
          } else {
            c1_ = (mx+mn)/2.0;
            c2_ = (mx-mn)/2.0;
          }
          for (int i = 0; i < initPar_.size(); i++) {
            sensTheta(i, 0) = (initPar_(i, 0) - c1_)/c2_;
            if (unscaled) {
              scaleC_(i, 0) = 1.0; // No scaling
            }
          }
          // REprintf("scaleC_: nd: %d\n", nd);
          // Rcpp::print(Rcpp::wrap(scaleC_));
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

      //' This function checks if the model has amounts that depend on the volume parameter
      bool amtDepV1() {
        int sw = ncmt_ + 10*trans_;
        switch (sw) {
        case 11: // one compartment cl v
        case 102: // A=(*v1), alpha=(*p1), beta=(*p2), B=(*p3)
        case 112: // A1 V1, alpha=(*p1), beta=(*p2), k21
        case 123: // v1, B, C, 3 compartment model
        case 113: // A B and C, alpha, beta, gamma, 3 compartment model
        case 13: // 3 compartment in terms of Cl, V
        case 12: // 2 compartment in terms of Cl, V
          return true;
        case 111: // 1 compartment alpha v
        case 21: // 1 compartment in terms of ks
        case 42: // 2 compartment, alpha, beta k21
        case 52: // 2 compartment, alpha, beta aob
        case 22: // 2 compartment in terms of ks
        case 23: // 3 compartment in terms of ks
          return false;
        }
        return false;
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
        // if (abs(k12-k21) < DBL_EPSILON) {
        //   linCmtStan1(g, yp, ka, ret);
        //   ret(oral0_ + 1, 0) = yp(oral0_ + 1, 0);
        //   return;
        // }

        stan::math::solComp2struct<T> sol2 =
          stan::math::computeSolComp2(k10, k12, k21);

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
        // if (abs(k13 - k31) < DBL_EPSILON) {
        //   linCmtStan2(g, yp, ka, ret);
        //   ret(oral0_ + 2, 0) = yp(oral0_ + 2, 0);
        //   return;
        // }
        // if (abs(k12 - k21) < DBL_EPSILON) {
        //   //here we swap
        //   T kk31 = k31;
        //   T kk13 = k13;
        //   // T kk21 = k21;
        //   // T kk12 = k12;
        //   T yp3 = yp(oral0_ + 2, 0);
        //   T yp2 = yp(oral0_ + 1, 0);
        //   yp(oral0_ + 1, 0) = yp3;
        //   yp(oral0_ + 2, 0) = yp2;

        //   g(1, 1) = k31; // was  k21
        //   g(1, 0) = k13; // was k12
        //   linCmtStan2(g, yp, ka, ret);
        //   // now swap back
        //   // periph cmt 3 was actually 2 cmt perip compartment
        //   ret(oral0_ + 2, 0) = ret(oral0_ + 1, 0);
        //   // periph cmt 2 remains the same
        //   ret(oral0_ + 1, 0) = yp2;
        //   return;
        // }

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

      void calcFx(Eigen::Matrix<double, Eigen::Dynamic, 1>& thetaIn) {
        fx_ = fdoubles(thetaIn);
        for (int i = 0; i < ncmt_ + oral0_; i++) {
          Asave_[i] = fx_(i, 0);
        }
        fxIsZero_ = fx_.isZero();
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
        double hhh;
        for (int i = 0; i < thetaIn.size(); i++) {
          if (i == sensV1_ && !amtDepV1()) {
            Js.col(i) = Eigen::Matrix<double, Eigen::Dynamic, 1>::Zero(Js.rows());
          } else {
            thetaCur = thetaIn;
            hhh = h[i]*thetaIn[i] + h[i];
            thetaCur(i, 0) += hhh;
            fup = fdoubles(thetaCur);
            thetaCur(i, 0) -= 2*hhh;
            fdown = fdoubles(thetaCur);
            Js.col(i) = (fup - fdown).array()/(2*hhh)*scaleC_(i);
          }
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
        double hhh;
        for (int i = 0; i < thetaIn.size(); i++) {
          if (i == sensV1_ && !amtDepV1()) {
            Js.col(i) = Eigen::Matrix<double, Eigen::Dynamic, 1>::Zero(Js.rows());
          } else {
            thetaCur = thetaIn;
            hhh = h[i]*thetaIn[i] + h[i];
            thetaCur(i, 0) += hhh;
            fh = fdoubles(thetaCur);
            thetaCur(i, 0) += hhh;
            f2h = fdoubles(thetaCur);
            Js.col(i) = (-3.0*fx + 4.0*fh - f2h).array()/(2*hhh)*scaleC_(i, 0);
          }
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
        double hhh;
        for (int i = 0; i < thetaIn.size(); i++) {
          if (i == sensV1_ && !amtDepV1()) {
            Js.col(i) = Eigen::Matrix<double, Eigen::Dynamic, 1>::Zero(Js.rows());
          } else {
            thetaCur = thetaIn;
            hhh = h[i]*thetaIn[i] + h[i];
            thetaCur(i, 0) += hhh;
            fh = fdoubles(thetaCur);
            thetaCur(i, 0) += hhh;
            f2h = fdoubles(thetaCur);
            thetaCur(i, 0) += hhh;
            f3h = fdoubles(thetaCur);
            thetaCur(i, 0) += hhh;
            f4h = fdoubles(thetaCur);
            Js.col(i) = (-25.0*fx + 48.0*fh -
                         36.0*f2h + 16.0*f3h -
                         3*f4h).array()/(12.0*hhh)*scaleC_(i, 0);
          }
        }
      }

      bool anySuspect(Eigen::Matrix<double, Eigen::Dynamic, 1>& fx) {
        for (int i = oral0_; i < fx.size(); i++) {
          double afx = std::abs(fx(i, 0));
          if (afx <= suspect_) {
            return true;
          }
        }
        return false;
      }

      void fHCalcJac(const Eigen::Matrix<double, Eigen::Dynamic, 1>& thetaIn,
                     double *h,
                     Eigen::Matrix<double, Eigen::Dynamic, 1>& fx,
                     Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>& Js) {
        fx = fx_;
        Js.setZero();
      }

      void fForwardJac(const Eigen::Matrix<double, Eigen::Dynamic, 1>& thetaIn,
                       double *h,
                       Eigen::Matrix<double, Eigen::Dynamic, 1>& fx,
                       Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>& Js) {
        Eigen::Matrix<double, Eigen::Dynamic, 1> fup;
        Eigen::Matrix<double, Eigen::Dynamic, 1> fnext;
        Eigen::Matrix<double, Eigen::Dynamic, 1> fnext2;
        Eigen::Matrix<double, Eigen::Dynamic, 1> fcur;
        Eigen::Matrix<double , Eigen::Dynamic, 1> thetaCur;
        fx = fx_;
        if (fxIsZero_) {
          Js.setZero();
          return;
        }
        double hhh;
        for (int i = 0; i < thetaIn.size(); i++) {
          if (i == sensV1_ && !amtDepV1()) {
            Js.col(i) = Eigen::Matrix<double, Eigen::Dynamic, 1>::Zero(Js.rows());
          } else {
            thetaCur = thetaIn;
            hhh = h[i];
            thetaCur(i, 0) += hhh;
            fup = fdoubles(thetaCur);
            fcur = (fup - fx).array()/(hhh)*scaleC_(i, 0);
            if (forwardMax_ >= 2 &&
                anySuspect(fcur)) {
              thetaCur(i, 0) += hhh;
              fnext           = fdoubles(thetaCur);
              fcur = (-3.0*fx + 4.0*fup - fnext).array()/(2.0*hhh)*scaleC_(i, 0);
              if (forwardMax_ >= 3 &&
                  anySuspect(fcur)) {
                thetaCur(i, 0) += hhh;
                fnext2          = fdoubles(thetaCur);
                fcur = (-11.0*fx + 18*fup -
                        9.0*fnext + 6.0*fnext2).array()/(6.0*hhh)*scaleC_(i, 0);
                Js.col(i) = fcur;
              } else {
                Js.col(i)       =  fcur;
              }
            } else {
              Js.col(i)       = fcur;
            }
          }
        }
        fx = fdoubles(thetaIn); // This also restores g_
        for (int i = 0; i < ncmt_ + oral0_; i++) {
          Asave_[i] = fx(i, 0);
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
                     const Eigen::Matrix<double, Eigen::Dynamic, 1>& theta,
                     double &vc) {
        vc = getVc(theta);
        return ret0(oral0_, 0) / vc;
      }

      double adjustF(const Eigen::VectorXd ret0,
                     const Eigen::Matrix<double, Eigen::Dynamic, 1>& theta) {
        double vc=0;
        return adjustF(ret0, theta, vc);
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
