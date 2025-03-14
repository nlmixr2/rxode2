#ifndef NDEBUG
#define NDEBUG // just in case
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "linCmt.h"

#define getAdvan(idx) ind->solve + (op->neq)*(idx) - op->numLinSens - op->numLin;

// Create linear compartment models for testing

using namespace Rcpp;

// [[Rcpp::export]]
RObject linCmtModelDouble(double dt,
                          double p1, double v1, double p2,
                          double p3, double p4, double p5,
                          double ka,
                          NumericVector alastNV, NumericVector rateNV,
                          const int ncmt, const int oral0, const int trans,
                          bool deriv) {

  stan::math::linCmtStan lc(ncmt, oral0, trans, deriv);
  Eigen::Matrix<double, -1, 1> theta;
  Eigen::Matrix<double, -1, 1> alast0 = as<Eigen::Matrix<double, -1, 1> >(alastNV);
  Eigen::Matrix<double, -1, 1> rate = as<Eigen::Matrix<double, -1, 1> >(rateNV);
  int nAlast = lc.getNalast();

  if (alast0.size() != nAlast) {
    Rcpp::stop("Alast0 size needs to be %d", nAlast);
  }
  theta.resize(lc.getNpars());

  switch (ncmt) {
  case 1:
    if (oral0 == 1) {
      theta << p1, v1, ka;
    } else {
      theta << p1, v1;
    }
    break;
  case 2:
    if (oral0 == 1) {
      theta << p1, v1, p2, p3, ka;
    } else {
      theta << p1, v1, p2, p3;
    }
    break;
  case 3:
    if (oral0 == 1) {
      theta << p1, v1, p2, p3, p4, p5, ka;
    } else {
      theta << p1, v1, p2, p3, p4, p5;
    }
    break;
  default:
    stop("Invalid number of compartments");
  }
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
    Eigen::Matrix<double, -1, -1> J;
    stan::math::jacobian(lc, theta, fx, J);
    lc.saveJac(J);
    Eigen::Matrix<double, -1, 1> Jg = lc.getJacCp(J, fx, theta);
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
 *  linCmtB
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
 * called the function and gradients are calculated. The time
 * is then stored in linCmtLastT.
 *
 * For this reason, the initialization of an ID sets linCmtLastT to
 * NA_REAL
 *
 * If the time is the same as linCmtLastT, then the function value
 * and the amounts are restored from the last call (or calculated simply)
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
  rx_solving_options_ind *ind = &(rx->subjects[id]);
  rx_solving_options *op = rx->op;
  int idx = ind->idx;
  double t = _t - ind->curShift;
  if (ind->linCmtLastT != _t) {
    Eigen::Matrix<double, -1, 1> theta;

    stan::math::linCmtStan lc(ncmt, oral0, trans, false);
    int nAlast = lc.getNalast();
    int nPars =  lc.getNpars();
    theta.resize(nPars);
    switch (ncmt) {
    case 1:
      if (oral0 == 1) {
        theta << p1, v1, ka;
      } else {
        theta << p1, v1;
      }
      break;
    case 2:
      if (oral0 == 1) {
        theta << p1, v1, p2, p3, ka;
      } else {
        theta << p1, v1, p2, p3;
      }
      break;
    case 3:
      if (oral0 == 1) {
        theta << p1, v1, p2, p3, p4, p5, ka;
      } else {
        theta << p1, v1, p2, p3, p4, p5;
      }
      break;
    default:
      return NA_REAL;
    }
    double *aLastPtr = getAdvan(idx);
    lc.setPtr(aLastPtr, ind->linCmtRate, ind->linCmtSave);
    Eigen::Matrix<double, Eigen::Dynamic, 1> Alast(nAlast);
    std::copy(aLastPtr, aLastPtr + nAlast, Alast.data());
    lc.setAlast(Alast, nAlast);
    lc.setRate(ind->linCmtRate);
    lc.setDt(_t - ind->curShift);
    Eigen::Matrix<double, Eigen::Dynamic, 1> fx;
    fx = lc(theta);
    ind->linCmtSave[0] = lc.adjustF(fx, theta);
    ind->linCmtLastT = _t;
  }
  if (which < 0) {
    double ret = ind->linCmtSave[oral0];
    if (trans != 10 || ncmt == 1) {
      ret = ret / v1;
    } else if (ncmt == 2) {
      ret = ret / (v1 + p3);
    } else if (ncmt == 3) {
      ret = ret / (v1 + p3 + p5);
    }
    return ret;
  } else {
    return ind->linCmtSave[which];
  }
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
 *  @param which1 The first index of the Jacobian (0 indexed)
 *  @param which2 The second index of the Jacobian (0 indexed)
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
 * called the function and gradients are calculated. The time
 * is then stored in linCmtLastT.
 *
 * For this reason, the initialization of an ID sets linCmtLastT to
 * NA_REAL
 *
 * If the time is the same as linCmtLastT, then the function value
 * and the jacobian are restored from the last call (or calculated simply)
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
  rx_solving_options_ind *ind = &(rx->subjects[id]);
  rx_solving_options *op = rx->op;
  int idx = ind->idx;
  double t = _t - ind->curShift;
  Eigen::Matrix<double, Eigen::Dynamic, 1> fx;
  Eigen::Matrix<double, -1, -1> J;
  Eigen::Matrix<double, -1, 1> theta;
  stan::math::linCmtStan lc(ncmt, oral0, trans, true);
  int nAlast = lc.getNalast();
  int nPars =  lc.getNpars();
  theta.resize(nPars);
  switch (ncmt) {
  case 1:
    if (oral0 == 1) {
      theta << p1, v1, ka;
    } else {
      theta << p1, v1;
    }
    break;
  case 2:
    if (oral0 == 1) {
      theta << p1, v1, p2, p3, ka;
    } else {
      theta << p1, v1, p2, p3;
    }
    break;
  case 3:
    if (oral0 == 1) {
      theta << p1, v1, p2, p3, p4, p5, ka;
    } else {
      theta << p1, v1, p2, p3, p4, p5;
    }
    break;
  default:
    return NA_REAL;
  }
  double *aLastPtr = getAdvan(idx);
  lc.setPtr(aLastPtr, ind->linCmtRate, ind->linCmtSave);
  Eigen::Matrix<double, Eigen::Dynamic, 1> Alast(nAlast);
  std::copy(aLastPtr, aLastPtr + nAlast, Alast.data());
  lc.setAlast(Alast, nAlast);
  lc.setRate(ind->linCmtRate);
  lc.setDt(_t - ind->curShift);
  if (ind->linCmtLastT != _t) {
    stan::math::jacobian(lc, theta, fx, J);
    lc.saveJac(J);
    // Eigen::Matrix<double, -1, 1> Jg = lc.getJacCp(J, fx, theta);
    ind->linCmtLastT = _t;
  } else {
    fx = lc.restoreFx(aLastPtr);
    J = lc.restoreJac(aLastPtr);
  }
  if (which1 >= 0 && which2 >= 0) {
    // w1, w2 are > 0
    return J(which1, which2);
  } else if (which1 == -1 && which2 == -1) {
    // -1, -1 is the function value
    double ret = fx(oral0);
    if (trans != 10 || ncmt == 1) {
      ret = ret / v1;
    } else if (ncmt == 2) {
      ret = ret / (v1 + p3);
    } else if (ncmt == 3) {
      ret = ret / (v1 + p3 + p5);
    }
    return ret;
  } else if (which1 >= 0 && which2 == -2) {
    // w2 < 0
    return fx(which1);
  } else if (which1 == -2 && which2 >= 0) {
    Eigen::Matrix<double, -1, 1> Jg = lc.getJacCp(J, fx, theta);
    return Jg(which2);
  }
  return NA_REAL;
}
