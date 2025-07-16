#ifndef NDEBUG
#define NDEBUG // just in case
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "rxomp.h"
#include "../inst/include/rxode2.h"
#include "timsort.h"
#define SORT gfx::timsort
#include "linCmt.h"


extern rx_solving_options op_global;
extern t_update_inis update_inis;


#define getLinRate ind->InfusionRate + op->linOffset
#define isSameTime(xout, xp) (fabs((xout)-(xp)) <= DBL_EPSILON*max2(fabs(xout),fabs(xp)))

// Create linear compartment models for testing
using namespace Rcpp;

// Global linear compartment A model object Since this CAN be
// threaded, this needs to be a std::vector.  This is created once to
// reduce memory allocation and deallocation time.
typedef struct {
  stan::math::linCmtStan lc;
  Eigen::Matrix<double, -1, 1> theta;
  Eigen::Matrix<double, Eigen::Dynamic, 1> fx;
  Eigen::Matrix<double, Eigen::Dynamic, 1> yp;
  Eigen::Matrix<double, Eigen::Dynamic, 2> gg;
} linA_t;

std::vector<linA_t> __linCmtA;

extern "C" void ensureLinCmtA(int nCores) {
  if (__linCmtA.size() < nCores) {
    __linCmtA.resize(nCores);
  }
}

// Global linear compartment B model object
// Since this cannot be threaded, this is not a vector
// object.  This is created once to reduce memory allocation
// and deallocation time.
stan::math::linCmtStan __linCmtB(0, 0, 0, true, 0, 0);
// Maximum size can be 2*ncmt + 1;
double __linCmtBdata[14];
int __linCmtBnumSens=0;

#define linCmtBaddrTheta 0
#define linCmtBaddrThetaSens 1
static inline double * getLinCmtDoubleAddr(int type) {
  switch (type) {
  case linCmtBaddrTheta: // max 7
    return __linCmtBdata;
  case linCmtBaddrThetaSens:  // max 7
    return &__linCmtBdata[7];
  // note fx needs cannot be a Map for use in stan :(
  }
  return NULL;
}

Eigen::Matrix<double, Eigen::Dynamic, 1> __linCmtBfx;

Eigen::Matrix<double, Eigen::Dynamic, 1> __linCmtByp;
Eigen::Matrix<double, Eigen::Dynamic, 2> __linCmtBg;

Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> __linCmtBJ;
Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> __linCmtBJs;
Eigen::Matrix<double, Eigen::Dynamic, 1> __linCmtBJg;

// [[Rcpp::export]]
RObject linCmtModelDouble(double dt,
                          double p1, double v1, double p2,
                          double p3, double p4, double p5,
                          double ka,
                          NumericVector alastNV, NumericVector rateNV,
                          const int ncmt, const int oral0, const int trans,
                          bool deriv,
                          int type,
                          double tau, double tinf, double amt,
                          int bolusCmt,
                          int ndiff,
                          int sensType=3,
                          double sensH=0.001) {
  stan::math::linCmtStan lc(ncmt, oral0, trans, deriv, type, ndiff);
  if (type == linCmtSsInf) {
    lc.setSsInf(tinf, tau);
  } else if (type == linCmtSsBolus) {
    lc.setSsBolus(amt, tau, bolusCmt);
  }
  Eigen::Matrix<double, -1, 1> theta0;
  Eigen::Matrix<double, -1, 1> alast0 = as<Eigen::Matrix<double, -1, 1> >(alastNV);
  Eigen::Matrix<double, -1, 1> rate = as<Eigen::Matrix<double, -1, 1> >(rateNV);
  int nAlast = lc.getNalast();

  if (alast0.size() != nAlast) {
    Rcpp::stop("Alast0 size needs to be %d", nAlast);
  }
  theta0.resize(lc.getNpars());
  Eigen::Map<Eigen::Matrix<double, -1, 1>> theta(theta0.data(), theta0.size());

  int sw = ncmt + 10*oral0;
  switch (sw) {
  case 1:  theta << p1, v1; break;
  case 11: theta << p1, v1, ka; break;
  case 2:  theta << p1, v1, p2, p3; break;
  case 12: theta << p1, v1, p2, p3, ka; break;
  case 3:  theta << p1, v1, p2, p3, p4, p5; break;
  case 13: theta << p1, v1, p2, p3, p4, p5, ka; break;
  }

  int numSens = lc.numSens();
  Eigen::Matrix<double, Eigen::Dynamic, 1> thetaSens0(numSens);
  Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1>> thetaSens(thetaSens0.data(), thetaSens0.size());

  Eigen::Matrix<double, 7, 1> scale;
  scale.setZero();

  lc.sensTheta(theta, thetaSens, sensType == 3, scale.data());

  double *a = new double[nAlast];
  double *asave = new double[nAlast];
  double *r = new double[lc.getNrate()];
  lc.setPtr(a, r, asave);
  lc.setAlast(alast0, nAlast);
  lc.setRate(rate.data());
  lc.setDt(dt);
  List retList;
  if (deriv) {
    Eigen::Matrix<double, Eigen::Dynamic, 1> fx;
    Eigen::Matrix<double, -1, -1> Js(ncmt+ oral0, numSens);//(ncmt + oral0, 2*ncmt + oral0);
    Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> J =
      Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>::Constant(ncmt + oral0, 2*ncmt+ oral0, NA_REAL);
    lc.resizeModel();

    // Getting the sensitivity with numerical differencs
    Eigen::Matrix<double, Eigen::Dynamic, 1> yp(ncmt+oral0, 1);
    Eigen::Matrix<double, Eigen::Dynamic, 2> g(ncmt, 2);
    lc.linAcalcAlast(yp, g, theta);

    // double d = lc.fdoubleh(thetaSens);

    Eigen::Matrix<double, Eigen::Dynamic, 1> h = Eigen::Matrix<double, Eigen::Dynamic, 1>::Constant(thetaSens.size(), 1, 0.001);
    h.setZero();

    switch (sensType) {
    case 1: // forward
      lc.fForwardJac(thetaSens, h.data(), fx, Js);
      break;
    case 2:  // central
      lc.fCentralJac(thetaSens, h.data(), fx, Js);
      break;
    case 3:
      stan::math::jacobian(lc, thetaSens, fx, Js);
      break;
    case 10:
      h = Eigen::Matrix<double, Eigen::Dynamic, 1>::Constant(thetaSens.size(), sensH);
      lc.fForwardJac(thetaSens, h.data(), fx, Js);
      break;
    case 20:
      h = Eigen::Matrix<double, Eigen::Dynamic, 1>::Constant(thetaSens.size(), sensH);
      lc.fCentralJac(thetaSens, h.data(), fx, Js);
      break;
    }
    lc.updateJfromJs(J, Js);
    lc.saveJac(J);
    Eigen::Matrix<double, -1, 1> Jg(ncmt+oral0);
    lc.getJacCp(J, fx, theta, Jg);
    double val = lc.adjustF(fx, theta);
    NumericVector Alast(nAlast);
    for (int i = 0; i < nAlast; i++) {
      Alast[i] = asave[i];
    }
    retList = List::create(_["val"] = wrap(val),
                           _["J"] = wrap(J),
                           _["Jg"] = wrap(Jg),
                           _["Alast"] = Alast);
  } else {
    Eigen::Matrix<double, Eigen::Dynamic, 1> fx;
    Eigen::Matrix<double, Eigen::Dynamic, 1> yp(oral0+ncmt, 1);
    Eigen::Matrix<double, Eigen::Dynamic, 2> g(ncmt, 2);
    lc.linAcalcAlast(yp, g, theta);
    fx = lc(theta);
    double val = lc.adjustF(fx, theta);
    NumericVector Alast(nAlast);
    for (int i = 0; i < nAlast; i++) {
      Alast[i] = asave[i];
    }
    retList = List::create(_["val"] = wrap(val),
                           _["Alast"] = Alast);
  }
  delete[] a;
  delete[] r;
  delete[] asave;
  return retList;
}

/*
 *  linCmtA
 *
 *  This function is called from rxode2 to compute the linear function
 *  values as well as the compartment amounts.
 *
 *  @param rx The rxSolve object
 *
 *  @param id The subject id
 *
 *  @param linCmt the compartment number of the linear compartment model
 *
 *  @param trans The transformation id
 *
 *  @param ncmt The number of compartments
 *
 *  @param oral0 A indicator of 0 or 1 saying if this was an oral dose
 *
 *  @param which1 The index of the amount to be returned. When less
 *  than zero, this returns the linear compartment model value for the
 *  time.  When greater than zero it returns the amount in the linear
 *  compartment models which can be:
 *
 *   depot, central, peripheral, second peripheral
 *
 *  @param _t The time where the function/jacobian is evaluated
 *
 *  @param p1 The first parameter, can be clearance
 *
 *  @param v1 The central volume
 *
 *  @param p2 The second parameter, can be inter-comparmental clearance
 *
 *  @param p3 The third parameter, can be second peripheral volume
 *
 *  @param p4 The fourth parameter, can be second inter-compartmental
 *            clearance
 *
 *  @param p5 The fifth parameter, can be second peripheral volume
 *
 *  @param ka The first order oral absorption rate constant
 *
 *  @return The function value or the jacobian value
 *
 * This function can bebe called multiple times in the same function.
 *
 * The first time linCmtA is called time _t and a specific id
 * called the function and gradients are calculated.
 *
 * @author Matthew Fidler
 *
 */
extern "C" double linCmtA(rx_solve *rx, int id,
                          double _t,
                          int linCmt, int ncmt,
                          int oral0, int which,
                          int trans,
                          double p1, double v1,
                          double p2, double p3,
                          double p4, double p5,
                          // Oral parameters
                          double ka) {
#define fx    lca.fx
#define J     lca.J
#define Jg    lca.Jg
#define lc    lca.lc
#define theta lca.theta
#define yp    lca.yp
  rx_solving_options_ind *ind = &(rx->subjects[id]);
  rx_solving_options *op = rx->op;
  // get the linear solved system object.
  linA_t lca = __linCmtA[omp_get_thread_num()];
  int idx = ind->idx;
  // Create the solved system object
  if (!lc.isSame(ncmt, oral0, trans, rx->ndiff)) {
    lc.setModelType(ncmt, oral0, trans, ind->linSS, rx->ndiff);
    // only resize when needed
    theta = Eigen::Matrix<double, Eigen::Dynamic, 1>(lc.getNpars());
    fx = Eigen::Matrix<double, Eigen::Dynamic, 1>(ncmt + oral0);
    yp = Eigen::Matrix<double, Eigen::Dynamic, 1>(ncmt + oral0, 1);
    lca.gg = Eigen::Matrix<double, Eigen::Dynamic, 2>(ncmt, 2);
  } else {
    lc.setSsType(ind->linSS);
  }
  if (ind->linSS == linCmtSsInf) {
    lc.setSsInf(ind->linSSvar, ind->linSStau);
  } else if (ind->linSS == linCmtSsBolus) {
    lc.setSsBolus(ind->linSSvar, ind->linSStau, ind->linSSbolusCmt);
  }

  // Get number of items in Alast
  int nAlast = lc.getNalast();

  // Get/Set the pointers
  double *asave = ind->linCmtSave;
  double *r = getLinRate;
  double *a;
  if (ind->linCmtAlast == NULL) {
    a = getAdvan(ind->solvedIdx);
  } else {
    a = ind->linCmtAlast;
  }
  lc.setPtr(a, r, asave);
  // Setup parameter matrix
  int sw = ncmt + 10*oral0;
  switch (sw) {
  case 1:  theta << p1, v1; break;
  case 11: theta << p1, v1, ka; break;
  case 2:  theta << p1, v1, p2, p3; break;
  case 12: theta << p1, v1, p2, p3, ka; break;
  case 3:  theta << p1, v1, p2, p3, p4, p5; break;
  case 13: theta << p1, v1, p2, p3, p4, p5, ka; break;
  }

  // Here we restore the last solved value
  if (!ind->doSS && ind->solvedIdx >= idx) {
    double *acur = getAdvan(idx);
    if (which < 0) {
      fx = lc.restoreFx(acur);
      return lc.adjustF(fx, theta);
    } else {
      return acur[which];
    }
  }
  // Currently this may not have been calculated, calculate now
  if (which < 0) {
    if (ind->_rxFlag == 11) {
      // If we are calculating the LHS values or other values, these are
      // stored in the corresponding compartments.
      //
      // This also handles the case where _t = ind->tcur, where the
      // solution is already known
      // ind->linCmtSave = getAdvan(idx);
      fx = lc.restoreFx(getAdvan(idx));
    } else {
      // Here we are doing ODE solving OR only linear solving
      // so we calculate these values here.
      //
      // For these cases:

      // ind->tprior gives the prior known time or current time solved to
      //
      // ind->tout gives the time solved
      //
      // _t gives the time requested to solve for (which with ODE
      // solving may not be tout); note that if _t = ind->tprior the
      // solution is the last solution solved or initial conditions
      //

      // Get/Set the dt; This is only applicable in the ODE/linCmt() case

      double dt;
      if (ind->doSS) {
        dt = ind->tout - ind->tprior;
      } else {
        dt =  _t - ind->tprior;
      }
      lc.setDt(dt);

      lc.linAcalcAlast(yp, lca.gg, theta);

      fx = lc(theta);
    }
    return lc.adjustF(fx, theta, ind->linCmtHV);
  } else if (which >= 0 && which < nAlast) {
    // Return the amount in the linear compartment model
    // which can be depot, central, peripheral, second peripheral
    // This assumes that the function value is the first
    if (ind->_rxFlag != 11) {
      return ind->linCmtSave[which];
    } else {
      double *acur = getAdvan(idx);
      return acur[which];
    }
  }
  // Invalid index
  return NA_REAL;
#undef fx
#undef J
#undef Jg
#undef lc
#undef theta
#undef yp
}

extern "C" double linCmtScaleInitPar(int which) {
  return __linCmtB.initPar(which);
}

extern "C" double linCmtScaleInitN() {
  Eigen::Matrix<double, Eigen::Dynamic, 1> theta = __linCmtB.initPar();
  return theta.size();
}

extern "C" int linCmtZeroJac(int i) {
  return __linCmtB.parDepV1(i);
}



/*
 *  linCmtB
 *
 *  This function is called from rxode2 to compute both the jacobian of
 *  the linear model and the function value.
 *
 *  @param rx The rxSolve object
 *
 *  @param id The subject id
 *
 *  @param linCmt the compartment number of the linear compartment model
 *
 *  @param trans The transformation id
 *
 *  @param ncmt The number of compartments
 *
 *  @param oral0 A indicator of 0 or 1 saying if this was an oral dose
 *
 *  @param which1 The first index of the Jacobian (0 indexed; compartment number)

 *  @param which2 The second index of the Jacobian (0 indexed; parameter number)

 *
 *  When which1 and which2 are both -1, the solved linear compartment
 *  model value returned
 *
 *  When which2 is -2, the amounts in the saved function are returned
 *  with which1 (zero indexed)
 *
 *  The order of the amounts is as follows:
 *
 *   (depot if present), central, peripheral, second peripheral
 *
 *  When which1 is -2, the gradient of the linear compartment model
 *  with respect to the parameter is returned.
 *
 *  The parameter order is as follows:
 *
 *   p1, v1, p2, p3, p4, p5, ka; for 3 compartment models
 *
 *   p1, v1, p2, p3, ka; for 2 compartment models
 *
 *   p1, v1, ka; for 1 compartment models
 *
 *  The ka is only appended for oral model
 *
 *  @param _t The time where the function/jacobian is evaluated
 *
 *  @param p1 The first parameter, can be clearance
 *
 *  @param v1 The central volume
 *
 *  @param p2 The second parameter, can be inter-comparmental clearance
 *
 *  @param p3 The third parameter, can be second peripheral volume
 *
 *  @param p4 The fourth parameter, can be second inter-compartmental
 *            clearance
 *
 *  @param p5 The fifth parameter, can be second peripheral volume
 *
 *  @param ka The first order oral absorption rate constant
 *
 *  @return The function value or the jacobian value
 *
 * This function will likely be called multiple times in the same ODE
 * system when running focei.
 *
 * The first time linCmtB is called time _t and a specific id
 * called the function and gradients are calculated.
 *
 * @author Matthew Fidler
 *
*/
extern "C" double linCmtB(rx_solve *rx, int id,
                          double _t, int linCmt,
                          int ncmt, int oral0,
                          int which1, int which2,
                          int trans,
                          double p1, double v1,
                          double p2, double p3,
                          double p4, double p5,
                          // Oral parameters
                          double ka) {
#define fx        __linCmtBfx
#define Jg        __linCmtBJg
#define lc        __linCmtB
#define AlastA    __linCmtBAlastA
#define J         __linCmtBJ
#define Js        __linCmtBJs
#define yp        __linCmtByp
#define g         __linCmtBg
  rx_solving_options_ind *ind = &(rx->subjects[id]);
  rx_solving_options *op = rx->op;
  int idx = ind->idx;
  bool resized = false;
  // Create the solved system object
  if (which1 != -1 || which2 != -1) {
    // If we are calculating the LHS values or other values, these are
    // stored in the corresponding compartments.
    //
    // This assumes that the linear compartment solution of which=-1,
    // -1 has already been called
    //
    // This also handles the case where _t = ind->tcur, where the
    // solution is already known
    // double *acur = getAdvan(idx);
    // J  = lc.restoreJac(acur);
    // fx = lc.restoreFx(acur);
    if (which1 >= 0 && which2 >= 0) {
      // w1, w2 are > 0
      return J(which1, which2);
    } else if (which1 >= 0 && which2 == -2) {
      // w2 < 0
      return fx(which1);
    } else if (which1 == -2 && which2 >= 0) {
      return Jg(which2);
    }
  } else if (!lc.isSame(ncmt, oral0, trans, rx->ndiff)) {
    lc.setModelType(ncmt, oral0, trans, ind->linSS, rx->ndiff);
    // only resize when needed
    fx = Eigen::Matrix<double, Eigen::Dynamic, 1>(ncmt + oral0);
    int npars = lc.getNpars();
    // NA fill and resize
    J = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>::Constant(ncmt + oral0, npars, NA_REAL);

    __linCmtBnumSens = lc.numSens();
    Js = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(ncmt+oral0, __linCmtBnumSens);//(ncmt + oral0, 2*ncmt + oral0);
    // thetaSens.resize(numSens);

    // AlastA.resize(ncmt + oral0);
    Jg = Eigen::Matrix<double, Eigen::Dynamic, 1>(lc.getNpars());

    yp = Eigen::Matrix<double, Eigen::Dynamic, 1>(ncmt + oral0);
    g = Eigen::Matrix<double, Eigen::Dynamic, 2>(ncmt, 2);
    lc.setForwardOpts(rx->linCmtSuspect, rx->linCmtForwardMax);
  } else {
    lc.setSsType(ind->linSS);
  }
  if (id == 0 && ind->linH[0] == 0) {
    lc.resetFlags();
  }
  lc.setId(id);

  Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1> >
    theta(getLinCmtDoubleAddr(linCmtBaddrTheta), lc.getNpars());

  int sw = ncmt + 10*oral0;
  switch (sw) {
  case 1:  theta << p1, v1; break;
  case 11: theta << p1, v1, ka; break;
  case 2:  theta << p1, v1, p2, p3; break;
  case 12: theta << p1, v1, p2, p3, ka; break;
  case 3:  theta << p1, v1, p2, p3, p4, p5; break;
  case 13: theta << p1, v1, p2, p3, p4, p5, ka; break;
  }

  Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1> >
    thetaSens(getLinCmtDoubleAddr(linCmtBaddrThetaSens), __linCmtBnumSens);

  lc.sensTheta(theta, thetaSens, rx->sensType == 3, rx->linCmtScale);
  if (ind->linSS == linCmtSsInf) {
    lc.setSsInf(ind->linSSvar, ind->linSStau);
  } else if (ind->linSS == linCmtSsBolus) {
    lc.setSsBolus(ind->linSSvar, ind->linSStau, ind->linSSbolusCmt);
  }

  // Get number of items in Alast
  int nAlast = lc.getNalast();

  // Get/Set the pointers
  double *asave = ind->linCmtSave;
  double *r = getLinRate;
  double *a;

  if (ind->linCmtAlast == NULL) {
    a = getAdvan(ind->solvedIdx);
  } else {
    a = ind->linCmtAlast;
  }
  lc.setPtr(a, r, asave);

  // Setup parameter matrix


  // Here we restore the last solved value
  if (!ind->doSS && ind->solvedIdx >= idx) {
    double *acur = getAdvan(idx);
    J = lc.restoreJac(acur);
    fx = lc.restoreFx(acur);
  } else {
    // Calculate everything while solving using linCmt()
    if (ind->_rxFlag == 11) {
      // If we are calculating the LHS values or other values, these are
      // stored in the corresponding compartments.
      //
      // This also handles the case where _t = ind->tcur, where the
      // solution is already known
      // ind->linCmtSave = getAdvan(idx);
      double *acur = getAdvan(idx);
      J = lc.restoreJac(acur);
      fx = lc.restoreFx(acur);
    } else {
      // Here we are doing ODE solving OR only linear solving
      // so we calculate these values here.
      //
      // For these cases:

      // ind->tprior gives the prior known time or current time solved to
      //
      // ind->tout gives the time solved
      //
      // _t gives the time requested to solve for (which with ODE
      // solving may not be tout); note that if _t = ind->tprior the
      // solution is the last solution solved or initial conditions
      //

      // Get/Set the dt; This is only applicable in the ODE/linCmt() case
      double dt;
      if (ind->doSS) {
        dt = ind->tout - ind->tprior;
      } else {
        dt =  _t - ind->tprior;
      }
      lc.setDt(dt);
      if (rx->ndiff == 0) {
        lc.linAcalcAlast(yp, g, theta);
        lc.calcFx(thetaSens);
        lc.fHCalcJac(thetaSens,ind->linH, fx, Js);
      } else if (ind->linCmtHparIndex >= -1) {
        if (ind->linCmtHparIndex >= 0) {
          thetaSens(ind->linCmtHparIndex, 0) += ind->linCmtH;
        }
        lc.linAcalcAlast(yp, g, theta);
        lc.calcFx(thetaSens);
        lc.fHCalcJac(thetaSens,ind->linH, fx, Js);
      } else {
        switch (rx->sensType) {
        case 1: // forward
        case 10:
        case 6: // forward difference with gill H est
          lc.linAcalcAlast(yp, g, theta);
          lc.calcFx(thetaSens);
          lc.fForwardJac(thetaSens, ind->linH, fx, Js);
          break;

        case 20:
        case 2:  // central
          lc.linAcalcAlast(yp, g, theta);
          lc.calcFx(thetaSens);
          lc.fCentralJac(thetaSens, ind->linH, fx, Js);
          break;

        case 40: // 3-point forward difference
        case 4:  // 3-point forward difference
        case 7:  // 3-point forward difference with gill H est
          lc.linAcalcAlast(yp, g, theta);
          lc.calcFx(thetaSens);
          lc.fF3Jac(thetaSens, ind->linH, fx, Js);
          break;

        case 50: // 5-point endpoint difference
        case 5: // 5-point endpoint difference
          lc.linAcalcAlast(yp, g, theta);
          lc.calcFx(thetaSens);
          lc.fEndpoint5Jac(thetaSens, ind->linH, fx, Js);
          break;

        case 3:
        default:
          stan::math::jacobian(lc, thetaSens, fx, Js);
          break;
        }
        lc.updateJfromJs(J, Js);
        lc.saveJac(J);
      }
    }
  }
  lc.getJacCp(__linCmtBJ, fx, theta, Jg);
  return lc.adjustF(fx, theta, ind->linCmtHV);
#undef fx
#undef J
#undef Jg
#undef lc
#undef theta
#undef AlastA
#undef yp
}
