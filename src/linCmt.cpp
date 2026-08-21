#ifndef NDEBUG
#define NDEBUG // just in case
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <cstdlib>
#include <cstring>
#include <algorithm>
#include "rxomp.h"
#include "../inst/include/rxode2.h"
#include "timsort.h"
#define SORT gfx::timsort
#include "linCmt.h"
#include "linCmtSensType.h"
#include "../inst/include/rxode2EventTranslate.h"

#ifdef RXODE2_NO_STAN_TBB_OBSERVER
// stan-math's init_chainablestack.hpp is kept out of the build (no linkable
// TBB, see rxode2_sundials_stan_compat.h).  Its global ad_tape_observer
// was also what created the main thread's AD tape; without STAN_THREADS the
// tape is a plain (shared) global, so constructing one ChainableStack here
// initializes it for the whole DLL.
namespace {
stan::math::ChainableStack rxode2MainThreadAdTape;
}
#endif

extern rx_solving_options op_global;
extern t_update_inis update_inis;
extern "C" rx_solve *getRxSolve_(void);
extern "C" double linCmtB(rx_solve *rx, int id,
                          double _t, int linCmt,
                          int ncmt, int oral0,
                          int which1, int which2,
                          int trans,
                          double p1, double v1,
                          double p2, double p3,
                          double p4, double p5,
                          double ka);


#define getLinRate ind->InfusionRate + op->linOffset
#define isSameTime(xout, xp) (fabs((xout)-(xp)) <= 2.0*DBL_EPSILON*max2(fabs(xout),fabs(xp)))

// Does this individual have a steady-state infusion (rate/dur) dose?
//
// The dose-time sensitivity (linCmtB which1 = -3) needs dA/dt, which includes
// the infusion rate.  A regular (non-SS) infusion's rate is recovered at
// output time via the linCmtRateHist cache (see linCmtBRateSlot() /
// linCmtBdoseTime() below, nlmixr2/rxode2#1236).  A steady-state infusion
// establishes its amounts analytically (handleSSinf8()/solveSSinf(), setting
// ind->linSS only transiently around that single call) and, for a one-shot
// SS dose with no following schedule, deliberately leaves ind->InfusionRate
// at 0 afterward -- the closed-form amount is exact but is not the limit of
// an ongoing rate, so -dA/dt is not well defined by the cache at the SS
// dose's own index; report NA there instead of a value that depends on
// solve-order happenstance.  A steady-state BOLUS (linCmtSsBolus) never
// touches InfusionRate and is unaffected -- it stays exact (see
// test-lincmt-dose-time-sens.R).
static inline int linCmtHasSsInfusion(rx_solving_options_ind *ind) {
  for (int i = 0; i < ind->ndoses; ++i) {
    int wh, cmt, wh100, whI, wh0;
    getWh(getEvid(ind, ind->idose[i]), &wh, &cmt, &wh100, &whI, &wh0);
    if ((wh0 == EVID0_SS0 || wh0 == EVID0_SS || wh0 == EVID0_SS20 ||
         wh0 == EVID0_SS2 || wh0 == EVID0_SSINF) &&
        whI != EVIDF_NORMAL && whI != EVIDF_REPLACE && whI != EVIDF_MULT) {
      return 1;
    }
  }
  return 0;
}

// Per-idx cache of the infusion rate feeding a linCmt() model, keyed the same
// way as the amounts cached via getAdvan()/Alast: written once, while the
// index is genuinely being solved (ind->InfusionRate live and correct), and
// read back on any later re-query of that same index (the output pass in
// rxode2_df.cpp, which clears ind->InfusionRate before recomputing lhs, or a
// backward re-query within the same solve).  width is op->numLin, the size of
// the rate vector linCmtBdoseTime()'s dAdt() expects.  linCmtB() writes this
// for every genuinely-solved idx (whether or not the model actually has an
// infusion -- it is simply 0 for a bolus-only regimen), so a re-query (grow =
// 0) should always find it; NULL is a defensive fallback for an idx that
// somehow was not, rather than a case expected to occur.
static inline double *linCmtBRateSlot(rx_solving_options_ind *ind, int idx, int width, int grow) {
  if (idx < 0 || width <= 0) return NULL;
  if (ind->linCmtRateHistW != width) {
    free(ind->linCmtRateHist);
    ind->linCmtRateHist = NULL;
    ind->linCmtRateHistCap = 0;
    ind->linCmtRateHistW = width;
  }
  if (idx >= ind->linCmtRateHistCap) {
    if (!grow) return NULL;
    int newCap = ind->linCmtRateHistCap > 0 ? ind->linCmtRateHistCap : 64;
    while (idx >= newCap) newCap *= 2;
    double *np = (double*) realloc(ind->linCmtRateHist, (size_t)newCap * width * sizeof(double));
    if (np == NULL) (Rf_error)("cannot allocate linCmt rate history");
    memset(np + (size_t)ind->linCmtRateHistCap * width, 0,
           (size_t)(newCap - ind->linCmtRateHistCap) * width * sizeof(double));
    ind->linCmtRateHist = np;
    ind->linCmtRateHistCap = newCap;
  }
  return ind->linCmtRateHist + (size_t)idx * width;
}

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
// Refactored to per-thread vector for thread safety, matching linCmtA pattern.
typedef struct {
  stan::math::linCmtStan lc;
  double data[14];
  int numSens;
  Eigen::Matrix<double, Eigen::Dynamic, 1> fx;
  Eigen::Matrix<double, Eigen::Dynamic, 1> yp;
  Eigen::Matrix<double, Eigen::Dynamic, 2> g;
  Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> J;
  Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> Js;
  Eigen::Matrix<double, Eigen::Dynamic, 1> Jg;
  // d(Alast_i)/d(Alast_{i-1}) -- the state-transition Jacobian, only ever
  // populated on demand (which1 == -4, see linCmtB() below), not on every
  // ordinary call -- this is exclusively for the time-varying-covariate AD
  // fix (see project_lincmt_timevarying_covariate_bug); an ordinary,
  // constant-theta solve never requests it and pays nothing extra.
  Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> JAlast;
} linB_t;

std::vector<linB_t> __linCmtB;

extern "C" void ensureLinCmtB(int nCores) {
  if ((int)__linCmtB.size() < nCores) {
    __linCmtB.resize(nCores);
  }
}

#define linCmtBaddrTheta 0
#define linCmtBaddrThetaSens 1
static inline double * getLinCmtDoubleAddr(linB_t &lcb, int type) {
  switch (type) {
  case linCmtBaddrTheta: // max 7
    return lcb.data;
  case linCmtBaddrThetaSens:  // max 7
    return &lcb.data[7];
  // note fx needs cannot be a Map for use in stan :(
  }
  return NULL;
}

// Fill a macro-parameter vector in the order macros2micros() expects, which
// depends on the compartment count and whether the model is oral.  Templated
// on the target because the callers hold theta either as its own Matrix or as
// an Eigen::Map over per-thread scratch.  Returns 0 for a shape that is not a
// linCmt() model, leaving the vector untouched.
template <typename T>
static inline int linCmtFillTheta(T &th, int ncmt, int oral0,
                                  double p1, double v1,
                                  double p2, double p3,
                                  double p4, double p5,
                                  double ka) {
  switch (ncmt + 10*oral0) {
  case 1:  th << p1, v1; return 1;
  case 11: th << p1, v1, ka; return 1;
  case 2:  th << p1, v1, p2, p3; return 1;
  case 12: th << p1, v1, p2, p3, ka; return 1;
  case 3:  th << p1, v1, p2, p3, p4, p5; return 1;
  case 13: th << p1, v1, p2, p3, p4, p5, ka; return 1;
  }
  return 0;
}

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

  linCmtFillTheta(theta, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka);

  int numSens = lc.numSens();
  Eigen::Matrix<double, Eigen::Dynamic, 1> thetaSens0(numSens);
  Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1>> thetaSens(thetaSens0.data(), thetaSens0.size());

  Eigen::Matrix<double, 7, 1> scale;
  scale.setZero();

  // sensType 3 (reverse AD) and 30 (forward AD, fvar) both use the unscaled
  // (isAD = true) path so the Jacobian comes out in true-theta units.
  lc.sensTheta(theta, thetaSens, sensType == 3 || sensType == 30, scale.data());

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
    case 30:  // forward-mode AD (fvar); should match case 3 to round-off
      lc.linCmtFwdJac(thetaSens, fx, Js);
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

// PROTOTYPE (not a shipping code path): persistent per-subject reverse-mode AD.
//
// Builds ONE var-typed theta and carries the var-typed compartment state
// ("yp") directly from one interval's output into the next interval's input,
// never round-tripping through Asave_/A_ doubles and never re-seeding theta.
// This is the mathematical equivalent of the production getAlastAD<T>()
// reconstruction trick (which is exact, not an approximation, because theta
// is fixed for the whole subject-solve pass) but without needing the saved
// Jacobian J at all -- the tape already carries the true dependency chain.
//
// Jacobian extraction per interval: one reverse sweep (var::grad()) per
// output compartment, followed by a global (non-nested) adjoint zero. This
// is a deliberately naive prototype to measure whether the per-interval
// set_zero_all_adjoints() cost (which walks the WHOLE persistent tape, not
// a nested sub-scope) is acceptable, before investing in a nested-scope
// design that would need to keep the live "yp" var alive across a nest
// boundary.
//
// [[Rcpp::export]]
List linCmtSubjectReverseADProto(NumericVector dtVec, NumericVector amtVec,
                                 double p1, double v1, double p2,
                                 double p3, double p4, double p5,
                                 double ka,
                                 NumericVector rateNV,
                                 int ncmt, int oral0, int trans,
                                 int bolusCmt) {
  stan::math::recover_memory(); // fresh tape -- simulate a new subject's solve boundary
  int nIv = dtVec.size();
  stan::math::linCmtStan lc(ncmt, oral0, trans, true, 0, 0);
  int npars = lc.getNpars();
  int m = ncmt + oral0;

  Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> theta(npars);
  linCmtFillTheta(theta, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka);

  Eigen::Matrix<double, Eigen::Dynamic, 1> rate =
    as<Eigen::Matrix<double, Eigen::Dynamic, 1> >(rateNV);

  Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> yp =
    Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1>::Zero(m);

  List out(nIv);
  for (int iv = 0; iv < nIv; iv++) {
    if (amtVec[iv] != 0.0) {
      yp(bolusCmt, 0) = yp(bolusCmt, 0) + amtVec[iv];
    }
    lc.setDt(dtVec[iv]);
    lc.setRate(rate.data());

    Eigen::Matrix<stan::math::var, Eigen::Dynamic, 2> g =
      stan::math::macros2micros(theta, ncmt, trans);
    stan::math::var kaV = 0.0;
    if (oral0) kaV = theta(ncmt*2, 0);

    Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> ret(m);
    if (ncmt == 1) {
      lc.linCmtStan1<stan::math::var>(g, yp, kaV, ret);
    } else if (ncmt == 2) {
      lc.linCmtStan2<stan::math::var>(g, yp, kaV, ret);
    } else if (ncmt == 3) {
      lc.linCmtStan3<stan::math::var>(g, yp, kaV, ret);
    }

    NumericVector fx(m);
    NumericMatrix J(m, npars);
    for (int k = 0; k < m; k++) fx[k] = ret(k, 0).val();
    for (int k = 0; k < m; k++) {
      stan::math::set_zero_all_adjoints();
      ret(k, 0).grad();
      for (int j = 0; j < npars; j++) {
        J(k, j) = theta(j, 0).adj();
      }
    }
    out[iv] = List::create(_["val"] = fx, _["J"] = J);
    yp = ret; // live carry -- stays on the SAME persistent tape
  }
  return out;
}

// PROTOTYPE (mathematical sanity check, 1-cmt IV only, ncmt=1/oral0=0
// hard-coded): tests whether "d(pred_i)/d(theta_i) only" (the fix candidate
// below) is actually the quantity needed, versus the TRUE cumulative
// derivative w.r.t. a single SHARED ETA that drives every interval's own
// covariate-adjusted CL. eta is the ONE persistent root (like theta was in
// linCmtSubjectReverseADProto); CL_iv = tcl*(cov_iv/refCov)^covExp*exp(eta)
// is recomputed fresh each interval from that SAME eta, so reverse-mode AD's
// grad() naturally sums eta's influence across every interval it appears in
// -- exactly what a continuously-integrated ODE sensitivity state would give,
// and exactly what a plain FD perturbation of eta (rerunning the whole
// multi-interval sequence) computes. No nesting needed here (small nIv,
// pure sanity check, not a performance design) -- set_zero_all_adjoints()
// covers eta same as everything else since nothing is nested.
//
// [[Rcpp::export]]
List linCmtSubjectReverseADEtaCovariateProto(NumericVector dtVec, NumericVector amtVec,
                                             NumericVector covVec,
                                             double tcl, double tv,
                                             double refCov, double covExp,
                                             double etaVal) {
  stan::math::recover_memory();
  int nIv = dtVec.size();
  stan::math::linCmtStan lc(1, 0, 1, true, 0, 0); // 1-cmt IV, trans=1 (CL/V)
  int m = 1;

  stan::math::var eta = etaVal; // the ONE shared root

  Eigen::Matrix<double, Eigen::Dynamic, 1> rate =
    Eigen::Matrix<double, Eigen::Dynamic, 1>::Zero(1);

  Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> yp =
    Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1>::Zero(m);

  List out(nIv);
  for (int iv = 0; iv < nIv; iv++) {
    if (amtVec[iv] != 0.0) {
      yp(0, 0) = yp(0, 0) + amtVec[iv];
    }
    // Fresh theta each interval, derived from the SAME shared eta.
    stan::math::var cl = tcl * pow(covVec[iv] / refCov, covExp) * exp(eta);
    stan::math::var v1v = tv;
    Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> theta(2);
    theta(0, 0) = cl; theta(1, 0) = v1v;

    lc.setDt(dtVec[iv]);
    lc.setRate(rate.data());

    Eigen::Matrix<stan::math::var, Eigen::Dynamic, 2> g =
      stan::math::macros2micros(theta, 1, 1);
    stan::math::var kaV = 0.0;

    Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> ret(m);
    lc.linCmtStan1<stan::math::var>(g, yp, kaV, ret);

    double fx = ret(0, 0).val();
    stan::math::set_zero_all_adjoints();
    ret(0, 0).grad();
    double dEta = eta.adj(); // d(pred_iv)/d(eta), cumulative through history

    out[iv] = List::create(_["val"] = fx, _["dEta"] = dEta);
    yp = ret;
  }
  return out;
}

// PROTOTYPE: forward-mode (fvar) equivalent of the eta-covariate sanity
// check above. fvar has no shared/growing tape and no adjoint-zeroing step
// at all -- eta is seeded ONCE (tangent=1) and carried, along with yp, as a
// live fvar<double> chain through every interval. If this matches the same
// FD ground truth the reverse-mode version matched, it is a CHEAPER way to
// get the correct cumulative time-varying-covariate sensitivity: O(1) tangent
// propagation per interval, no growing per-call cost, unlike reverse mode
// which cannot bound its per-call tape/adjoint-zero cost here (superposition
// cannot apply when theta itself varies with time, so there is no cheap
// per-observation nest to fall back on for this case).
//
// [[Rcpp::export]]
List linCmtSubjectForwardADEtaCovariateProto(NumericVector dtVec, NumericVector amtVec,
                                             NumericVector covVec,
                                             double tcl, double tv,
                                             double refCov, double covExp,
                                             double etaVal) {
  int nIv = dtVec.size();
  stan::math::linCmtStan lc(1, 0, 1, true, 0, 0);
  int m = 1;
  typedef stan::math::fvar<double> fv;

  fv eta(etaVal, 1.0); // seeded once -- differentiating w.r.t. eta

  Eigen::Matrix<double, Eigen::Dynamic, 1> rate =
    Eigen::Matrix<double, Eigen::Dynamic, 1>::Zero(1);

  Eigen::Matrix<fv, Eigen::Dynamic, 1> yp =
    Eigen::Matrix<fv, Eigen::Dynamic, 1>::Zero(m);

  List out(nIv);
  for (int iv = 0; iv < nIv; iv++) {
    if (amtVec[iv] != 0.0) {
      yp(0, 0) = yp(0, 0) + amtVec[iv];
    }
    fv cl = tcl * pow(covVec[iv] / refCov, covExp) * exp(eta);
    fv v1v(tv, 0.0);
    Eigen::Matrix<fv, Eigen::Dynamic, 1> theta(2);
    theta(0, 0) = cl; theta(1, 0) = v1v;

    lc.setDt(dtVec[iv]);
    lc.setRate(rate.data());

    Eigen::Matrix<fv, Eigen::Dynamic, 2> g = stan::math::macros2micros(theta, 1, 1);
    fv kaV(0.0, 0.0);

    Eigen::Matrix<fv, Eigen::Dynamic, 1> ret(m);
    lc.linCmtStan1<fv>(g, yp, kaV, ret);

    double fx = ret(0, 0).val();
    double dEta = ret(0, 0).d_; // tangent w.r.t. eta, accumulated through history

    out[iv] = List::create(_["val"] = fx, _["dEta"] = dEta);
    yp = ret;
  }
  return out;
}

// PROTOTYPE: the missing ingredient for a general (arbitrary covariate
// formula) time-varying-covariate fix. Confirmed by tracing nlmixr2est's
// actual generated code (see project_lincmt_timevarying_covariate_bug):
// d(theta_row)/d(eta) is ALREADY computed correctly today (it's an ordinary
// symbolic derivative of the plain covariate formula, not something opaque),
// and is simply multiplied against linCmtB()'s existing (buggy-in-isolation)
// d(pred)/d(theta_row) with no carry across rows. Nothing needs to change
// about how theta's own eta-sensitivity is computed -- the FIX is to also
// carry d(Alast)/d(eta) across rows, via:
//
//   T_i = dAlastDAlastPrev * T_{i-1} + dPredDTheta_i * dThetaDEta_i
//
// dPredDTheta_i already exists (linCmtB()'s ordinary Jacobian). This
// prototype computes the ONE missing piece: dAlastDAlastPrev, the (m x m)
// state-transition Jacobian -- d(this row's Alast)/d(the PREVIOUS row's
// Alast), holding theta fixed. Because linCmtStan1/2/3's closed form is
// LINEAR in its yp/Alast input (e.g. `ret = yp*E + ...`), this Jacobian is a
// CONSTANT matrix (independent of yp's actual value) -- exactly a linear
// system's own state-transition matrix -- computed here via m cheap
// forward-mode (fvar) passes, one unit tangent seed per compartment.
//
// [[Rcpp::export]]
NumericMatrix linCmtAlastTransitionMatrixProto(double p1, double v1, double p2,
                                               double p3, double p4, double p5,
                                               double ka, NumericVector rateNV,
                                               double dt,
                                               int ncmt, int oral0, int trans) {
  stan::math::linCmtStan lc(ncmt, oral0, trans, true, 0, 0);
  int npars = lc.getNpars();
  int m = ncmt + oral0;
  typedef stan::math::fvar<double> fv;

  Eigen::Matrix<double, Eigen::Dynamic, 1> thetaDbl(npars);
  linCmtFillTheta(thetaDbl, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka);
  Eigen::Matrix<double, Eigen::Dynamic, 1> rate =
    as<Eigen::Matrix<double, Eigen::Dynamic, 1> >(rateNV);

  NumericMatrix transMat(m, m);
  for (int dir = 0; dir < m; dir++) {
    Eigen::Matrix<fv, Eigen::Dynamic, 1> thetaF(npars);
    for (int k = 0; k < npars; k++) thetaF(k, 0) = fv(thetaDbl(k, 0), 0.0); // theta not being differentiated here
    Eigen::Matrix<fv, Eigen::Dynamic, 1> yp =
      Eigen::Matrix<fv, Eigen::Dynamic, 1>::Zero(m);
    yp(dir, 0) = fv(0.0, 1.0); // seed unit tangent in direction `dir`

    lc.setDt(dt);
    lc.setRate(rate.data());
    Eigen::Matrix<fv, Eigen::Dynamic, 2> g = stan::math::macros2micros(thetaF, ncmt, trans);
    fv kaV(0.0, 0.0);
    if (oral0) kaV = thetaF(ncmt*2, 0);
    Eigen::Matrix<fv, Eigen::Dynamic, 1> ret(m);
    if (ncmt == 1) lc.linCmtStan1<fv>(g, yp, kaV, ret);
    else if (ncmt == 2) lc.linCmtStan2<fv>(g, yp, kaV, ret);
    else if (ncmt == 3) lc.linCmtStan3<fv>(g, yp, kaV, ret);

    for (int k = 0; k < m; k++) transMat(k, dir) = ret(k, 0).d_;
  }
  return transMat;
}

// Test-only entry point for Phase 2 of the sensitivity-carry subsystem
// (project_lincmt_timevarying_covariate_bug / the linCmt-subject-ad plan):
// exercises linCmtB()'s which1=-5/-6 (cumulative carry) sentinels through a
// REAL, already-solved subject context, not a fabricated ind/rx_solve. Must
// be called from R right after an rxSolve() of a real linCmt() model in the
// SAME session (getRxSolve_() returns whatever the most recent solve left
// behind, exactly like every other post-solve accessor in this package --
// see rxSerialize.cpp/rxData.cpp). `id` is the 0-based subject index.
// `t`/`tPrior` are that subject's real per-row output time and the real
// time of the PRECEDING row (0 for the first row), read back from the
// solved data.frame -- ind->idx/ind->tprior are set here to mirror exactly
// what rxode2_df.cpp's own per-row output-pass loop already does before
// calling calc_lhs for row i, so linCmtB's dt computation
// (ind->doSS ? tout-tprior : _t-tprior) sees the same values a real
// generated model's calc_lhs would produce.
// [[Rcpp::export]]
NumericVector linCmtCarryLiveTest(int id, NumericVector t, NumericVector tPrior,
                                   NumericVector p1, double v1,
                                   int ncmt, int oral0, int trans,
                                   IntegerVector which1, IntegerVector which2,
                                   Nullable<NumericVector> addVal = R_NilValue) {
  rx_solve *rx = getRxSolve_();
  rx_solving_options_ind *ind = &(rx->subjects[id]);
  int n = t.size();
  NumericVector out(n);
  NumericVector av = addVal.isNull() ? NumericVector(n, 0.0) : NumericVector(addVal);
  for (int i = 0; i < n; i++) {
    ind->idx = i;
    ind->tprior = tPrior[i];
    // p2 only means anything for which1=-7 (the additive-carry sentinel),
    // where it carries the caller-supplied local contribution to add.
    out[i] = linCmtB(rx, id, t[i], 0, ncmt, oral0, which1[i], which2[i], trans,
                      p1[i], v1, av[i], 0.0, 0.0, 0.0, 0.0);
  }
  return out;
}

// PROTOTYPE: phase-aware hybrid WITHIN one subject. Real subjects often look
// like this: many closely-spaced doses (a loading regimen up to steady
// state) followed by a rich, dense observation history. Neither single
// strategy covers both parts cheaply -- the sequential/forward-mode carry
// handles many doses fine but its per-observation Jacobian work doesn't
// shrink just because there are few doses; the nested-superposition design
// handles many observations at flat cost but degrades badly once active
// doses pile up (see linCmtSubjectSuperpositionADProto's own documented
// worst case). So this is not a per-SUBJECT dispatch -- it is a dispatch
// WITHIN the subject's own timeline:
//
//   Phase 1 (dose-heavy): rolled through ONCE with a cheap forward-mode
//   (fvar) carry -- no growing tape, no adjoint-zeroing cost, so its cost is
//   flat per dose regardless of how many doses precede it. This yields the
//   phase's END STATE (value + Jacobian w.r.t. theta), computed via p
//   forward passes total (not p per dose).
//
//   Bridge: that end state is reconstructed as a var-typed "virtual dose"
//   inside EACH phase-2 observation's own nest, using the EXACT SAME linear
//   algebra as the existing production getAlastAD<T>() reconstruction
//   (AlastA = endVal - J*thetaAtEnd; virtualDose = AlastA + theta*J). That
//   reconstruction is mathematically EXACT here specifically because theta
//   is constant across phase 2 (unlike the time-varying-covariate case,
//   where the same trick silently conflates -- see
//   linCmtSubjectReverseADEtaCovariateProto/ForwardADEtaCovariateProto).
//   Recomputed fresh inside each nest (cheap: O(m*npars) arithmetic), so no
//   stale-adjoint risk and no need to re-roll phase 1 per observation.
//
//   Phase 2 (observation-heavy): the bridged virtual dose plus any ordinary
//   phase-2-only doses (bolus OR infusion, via the same two-phase
//   during/after decomposition as linCmtSubjectSuperpositionADProto) are
//   summed as independent superposition terms inside each observation's own
//   nested_rev_autodiff scope -- flat per-observation cost regardless of how
//   many observations follow.
//
// phase1RateVec[iv] is the infusion rate (into bolusCmt) active WHILE
// evolving phase1DtVec[iv] -- 0 for an ordinary step. This mirrors the
// production per-timepoint event loop's own convention (rate set, then
// evolve), so an infusion in phase 1 is just several consecutive steps with
// a nonzero rate followed by steps with rate=0 -- no special-casing needed
// in the roll-through itself, unlike phase 2's superposition terms, which
// each need the explicit two-phase treatment since they must stand alone as
// independent closed-form contributions.
//
// [[Rcpp::export]]
List linCmtSubjectHybridDoseObsADProto(NumericVector phase1DtVec, NumericVector phase1AmtVec,
                                       NumericVector phase1RateVec,
                                       NumericVector obsT, NumericVector doseT,
                                       NumericVector doseAmt, NumericVector doseDur,
                                       double p1, double v1, double p2,
                                       double p3, double p4, double p5,
                                       double ka,
                                       int ncmt, int oral0, int trans,
                                       int bolusCmt) {
  stan::math::linCmtStan lc(ncmt, oral0, trans, true, 0, 0);
  int npars = lc.getNpars();
  int m = ncmt + oral0;

  Eigen::Matrix<double, Eigen::Dynamic, 1> thetaDbl(npars);
  linCmtFillTheta(thetaDbl, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka);

  int nP1 = phase1DtVec.size();
  std::vector<double> zeroRate(m, 0.0);

  // ---- Phase 1: forward-mode roll-through, p forward passes total (not per dose) ----
  typedef stan::math::fvar<double> fv;
  Eigen::Matrix<double, Eigen::Dynamic, 1> endVal(m);
  Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> endJac(m, npars);

  for (int dir = 0; dir < npars; dir++) {
    Eigen::Matrix<fv, Eigen::Dynamic, 1> thetaF(npars);
    for (int k = 0; k < npars; k++) thetaF(k, 0) = fv(thetaDbl(k, 0), (k == dir) ? 1.0 : 0.0);

    Eigen::Matrix<fv, Eigen::Dynamic, 1> yp =
      Eigen::Matrix<fv, Eigen::Dynamic, 1>::Zero(m);

    std::vector<double> stepRate(m, 0.0);
    for (int iv = 0; iv < nP1; iv++) {
      if (phase1AmtVec[iv] != 0.0) yp(bolusCmt, 0) = yp(bolusCmt, 0) + phase1AmtVec[iv];
      lc.setDt(phase1DtVec[iv]);
      std::fill(stepRate.begin(), stepRate.end(), 0.0);
      stepRate[bolusCmt] = phase1RateVec[iv];
      lc.setRate(stepRate.data());
      Eigen::Matrix<fv, Eigen::Dynamic, 2> g = stan::math::macros2micros(thetaF, ncmt, trans);
      fv kaV(0.0, 0.0);
      if (oral0) kaV = thetaF(ncmt*2, 0);
      Eigen::Matrix<fv, Eigen::Dynamic, 1> ret(m);
      if (ncmt == 1) lc.linCmtStan1<fv>(g, yp, kaV, ret);
      else if (ncmt == 2) lc.linCmtStan2<fv>(g, yp, kaV, ret);
      else if (ncmt == 3) lc.linCmtStan3<fv>(g, yp, kaV, ret);
      yp = ret;
    }
    if (dir == 0) for (int k = 0; k < m; k++) endVal(k, 0) = yp(k, 0).val();
    for (int k = 0; k < m; k++) endJac(k, dir) = yp(k, 0).d_;
  }

  // ---- Phase 2: nested superposition, bridging phase 1's end state ----
  stan::math::recover_memory();
  int nObs = obsT.size();
  int nDose = doseT.size();

  List out(nObs);
  for (int io = 0; io < nObs; io++) {
    stan::math::nested_rev_autodiff nested;
    Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> theta(npars);
    for (int k = 0; k < npars; k++) theta(k, 0) = thetaDbl(k, 0);
    Eigen::Matrix<stan::math::var, Eigen::Dynamic, 2> g =
      stan::math::macros2micros(theta, ncmt, trans);
    stan::math::var kaV = 0.0;
    if (oral0) kaV = theta(ncmt*2, 0);

    Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> total =
      Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1>::Zero(m);

    // Bridge term: reconstruct phase 1's end state as a var, exact because
    // theta is constant across phase 2 (getAlastAD<T>()'s own precondition).
    {
      Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> virtualDose(m);
      for (int k = 0; k < m; k++) {
        stan::math::var alastA = endVal(k, 0);
        for (int j = 0; j < npars; j++) alastA -= endJac(k, j) * thetaDbl(j, 0);
        stan::math::var v = alastA;
        for (int j = 0; j < npars; j++) v += theta(j, 0) * endJac(k, j);
        virtualDose(k, 0) = v;
      }
      lc.setDt(obsT[io]); // phase-1 end is t=0 for phase-2's own clock
      lc.setRate(zeroRate.data());
      Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> retTerm(m);
      if (ncmt == 1) lc.linCmtStan1<stan::math::var>(g, virtualDose, kaV, retTerm);
      else if (ncmt == 2) lc.linCmtStan2<stan::math::var>(g, virtualDose, kaV, retTerm);
      else if (ncmt == 3) lc.linCmtStan3<stan::math::var>(g, virtualDose, kaV, retTerm);
      total = total + retTerm;
    }

    // Ordinary phase-2 doses -- bolus and infusion, same two-phase pattern
    // as linCmtSubjectSuperpositionADProto (during-infusion term direct;
    // end-of-infusion state as a virtual bolus for everything after).
    for (int jd = 0; jd < nDose; jd++) {
      if (doseT[jd] > obsT[io] + 1e-9) continue;
      double dur = doseDur[jd];
      Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> retTerm(m);
      if (dur <= 0.0) {
        // Plain bolus.
        double elapsed = obsT[io] - doseT[jd];
        lc.setDt(elapsed);
        lc.setRate(zeroRate.data());
        Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> yp0 =
          Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1>::Zero(m);
        yp0(bolusCmt, 0) = doseAmt[jd];
        if (ncmt == 1) lc.linCmtStan1<stan::math::var>(g, yp0, kaV, retTerm);
        else if (ncmt == 2) lc.linCmtStan2<stan::math::var>(g, yp0, kaV, retTerm);
        else if (ncmt == 3) lc.linCmtStan3<stan::math::var>(g, yp0, kaV, retTerm);
      } else if (obsT[io] < doseT[jd] + dur - 1e-9) {
        // Phase 1 of this dose: still infusing.
        double elapsed = obsT[io] - doseT[jd];
        lc.setDt(elapsed);
        std::vector<double> infRate(m, 0.0);
        infRate[bolusCmt] = doseAmt[jd] / dur;
        lc.setRate(infRate.data());
        Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> yp0 =
          Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1>::Zero(m);
        if (ncmt == 1) lc.linCmtStan1<stan::math::var>(g, yp0, kaV, retTerm);
        else if (ncmt == 2) lc.linCmtStan2<stan::math::var>(g, yp0, kaV, retTerm);
        else if (ncmt == 3) lc.linCmtStan3<stan::math::var>(g, yp0, kaV, retTerm);
      } else {
        // Phase 2 of this dose: infusion complete -- recompute the
        // end-of-infusion state fresh (cheap, one extra kernel call), then
        // decay it as a virtual bolus.
        lc.setDt(dur);
        std::vector<double> infRate(m, 0.0);
        infRate[bolusCmt] = doseAmt[jd] / dur;
        lc.setRate(infRate.data());
        Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> ypZero =
          Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1>::Zero(m);
        Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> endOfInf(m);
        if (ncmt == 1) lc.linCmtStan1<stan::math::var>(g, ypZero, kaV, endOfInf);
        else if (ncmt == 2) lc.linCmtStan2<stan::math::var>(g, ypZero, kaV, endOfInf);
        else if (ncmt == 3) lc.linCmtStan3<stan::math::var>(g, ypZero, kaV, endOfInf);

        double elapsedAfter = obsT[io] - (doseT[jd] + dur);
        lc.setDt(elapsedAfter);
        lc.setRate(zeroRate.data());
        if (ncmt == 1) lc.linCmtStan1<stan::math::var>(g, endOfInf, kaV, retTerm);
        else if (ncmt == 2) lc.linCmtStan2<stan::math::var>(g, endOfInf, kaV, retTerm);
        else if (ncmt == 3) lc.linCmtStan3<stan::math::var>(g, endOfInf, kaV, retTerm);
      }
      total = total + retTerm;
    }

    NumericVector fx(m);
    NumericMatrix J(m, npars);
    for (int k = 0; k < m; k++) fx[k] = total(k, 0).val();
    for (int k = 0; k < m; k++) {
      nested.set_zero_all_adjoints();
      for (int j = 0; j < npars; j++) theta(j, 0).adj() = 0.0;
      total(k, 0).grad();
      for (int j = 0; j < npars; j++) J(k, j) = theta(j, 0).adj();
    }
    out[io] = List::create(_["val"] = fx, _["J"] = J);
  }
  return out;
}

// PROTOTYPE (bug-fix candidate): persistent per-subject reverse-mode AD with
// a FRESH, INDEPENDENT theta per interval, allowing theta (p1/v1/.../ka) to
// differ between intervals -- i.e. a time-varying covariate. This is the
// direct fix for a confirmed production bug: getAlastAD<T>()'s reconstruction
// (and this file's own linCmtSubjectReverseADProto above, which shares one
// theta across the whole subject) hard-codes "theta constant across the
// subject." When a covariate makes theta legitimately differ between
// intervals, both the current production forward/reverse AD paths silently
// return d(pred_i)/d(theta_i) + d(pred_i)/d(theta_{i-1}) + ... (summed, as if
// every interval's theta were literally the same parameter) instead of each
// interval's own local sensitivity. Giving each interval its OWN theta var
// (not shared) fixes this automatically: Alast is still carried forward LIVE
// (chain-ruled correctly through however many distinct theta values preceded
// it), but grad() from interval i's output only accumulates onto interval
// i's OWN theta vars -- earlier intervals' theta vars get zero adjoint here
// because they are separate roots the current output does not (by
// construction) have a direct edge back to for ITS OWN column, even though
// Alast's value numerically reflects their history.
//
// thetaMat is nIv x npars (row-major via R, one theta row per interval).
// Returns, per interval, val + a J matrix of size m x npars for THAT
// interval's own theta only (matching what a per-row covariate chain rule
// downstream actually needs: d(pred_i)/d(theta_i), not a cross-interval sum).
//
// [[Rcpp::export]]
List linCmtSubjectReverseADTimeVaryingProto(NumericVector dtVec, NumericVector amtVec,
                                            NumericMatrix thetaMat,
                                            NumericVector rateNV,
                                            int ncmt, int oral0, int trans,
                                            int bolusCmt) {
  stan::math::recover_memory();
  int nIv = dtVec.size();
  stan::math::linCmtStan lc(ncmt, oral0, trans, true, 0, 0);
  int npars = lc.getNpars();
  int m = ncmt + oral0;

  Eigen::Matrix<double, Eigen::Dynamic, 1> rate =
    as<Eigen::Matrix<double, Eigen::Dynamic, 1> >(rateNV);

  Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> yp =
    Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1>::Zero(m);

  List out(nIv);
  for (int iv = 0; iv < nIv; iv++) {
    if (amtVec[iv] != 0.0) {
      yp(bolusCmt, 0) = yp(bolusCmt, 0) + amtVec[iv];
    }
    // Fresh, independent theta for THIS interval -- not shared with any
    // earlier interval's theta, even though yp (Alast) still carries the
    // true numeric/AD history forward from them.
    Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> theta(npars);
    for (int j = 0; j < npars; j++) theta(j, 0) = thetaMat(iv, j);

    lc.setDt(dtVec[iv]);
    lc.setRate(rate.data());

    Eigen::Matrix<stan::math::var, Eigen::Dynamic, 2> g =
      stan::math::macros2micros(theta, ncmt, trans);
    stan::math::var kaV = 0.0;
    if (oral0) kaV = theta(ncmt*2, 0);

    Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> ret(m);
    if (ncmt == 1) {
      lc.linCmtStan1<stan::math::var>(g, yp, kaV, ret);
    } else if (ncmt == 2) {
      lc.linCmtStan2<stan::math::var>(g, yp, kaV, ret);
    } else if (ncmt == 3) {
      lc.linCmtStan3<stan::math::var>(g, yp, kaV, ret);
    }

    NumericVector fx(m);
    NumericMatrix J(m, npars);
    for (int k = 0; k < m; k++) fx[k] = ret(k, 0).val();
    for (int k = 0; k < m; k++) {
      stan::math::set_zero_all_adjoints();
      ret(k, 0).grad();
      for (int j = 0; j < npars; j++) {
        J(k, j) = theta(j, 0).adj(); // THIS interval's theta only
      }
    }
    out[iv] = List::create(_["val"] = fx, _["J"] = J);
    yp = ret;
  }
  return out;
}

// PROTOTYPE variant: build the ENTIRE subject's tape first (no extraction
// interleaved), then extract every interval's Jacobian in one batch at the
// end. Tests whether deferring extraction avoids the O(n^2) cost measured in
// linCmtSubjectReverseADProto, or whether set_zero_all_adjoints() walking the
// whole (now-maximal-size) tape makes every extraction pay the full cost
// instead of just the late ones.
//
// [[Rcpp::export]]
List linCmtSubjectReverseADBatchProto(NumericVector dtVec, NumericVector amtVec,
                                      double p1, double v1, double p2,
                                      double p3, double p4, double p5,
                                      double ka,
                                      NumericVector rateNV,
                                      int ncmt, int oral0, int trans,
                                      int bolusCmt) {
  stan::math::recover_memory(); // fresh tape -- simulate a new subject's solve boundary
  int nIv = dtVec.size();
  stan::math::linCmtStan lc(ncmt, oral0, trans, true, 0, 0);
  int npars = lc.getNpars();
  int m = ncmt + oral0;

  Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> theta(npars);
  linCmtFillTheta(theta, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka);

  Eigen::Matrix<double, Eigen::Dynamic, 1> rate =
    as<Eigen::Matrix<double, Eigen::Dynamic, 1> >(rateNV);

  Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> yp =
    Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1>::Zero(m);

  // Phase 1: build the whole tape, no extraction yet.
  std::vector<Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> > allRet(nIv);
  for (int iv = 0; iv < nIv; iv++) {
    if (amtVec[iv] != 0.0) {
      yp(bolusCmt, 0) = yp(bolusCmt, 0) + amtVec[iv];
    }
    lc.setDt(dtVec[iv]);
    lc.setRate(rate.data());

    Eigen::Matrix<stan::math::var, Eigen::Dynamic, 2> g =
      stan::math::macros2micros(theta, ncmt, trans);
    stan::math::var kaV = 0.0;
    if (oral0) kaV = theta(ncmt*2, 0);

    Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> ret(m);
    if (ncmt == 1) {
      lc.linCmtStan1<stan::math::var>(g, yp, kaV, ret);
    } else if (ncmt == 2) {
      lc.linCmtStan2<stan::math::var>(g, yp, kaV, ret);
    } else if (ncmt == 3) {
      lc.linCmtStan3<stan::math::var>(g, yp, kaV, ret);
    }
    allRet[iv] = ret;
    yp = ret;
  }

  // Phase 2: extract every interval's value/Jacobian in one batch at the end.
  List out(nIv);
  for (int iv = 0; iv < nIv; iv++) {
    NumericVector fx(m);
    NumericMatrix J(m, npars);
    for (int k = 0; k < m; k++) fx[k] = allRet[iv](k, 0).val();
    for (int k = 0; k < m; k++) {
      stan::math::set_zero_all_adjoints();
      allRet[iv](k, 0).grad();
      for (int j = 0; j < npars; j++) {
        J(k, j) = theta(j, 0).adj();
      }
    }
    out[iv] = List::create(_["val"] = fx, _["J"] = J);
  }
  return out;
}

// PROTOTYPE variant: exploit the superposition principle of linear
// compartment models. Each dose's contribution to a later observation is an
// INDEPENDENT function of theta and elapsed time since that dose -- it does
// NOT need to be chained through the previous observation's output the way
// linCmtSubjectReverseADProto's "yp = ret" carry does. Only theta itself
// survives outside every nest; g = macros2micros(theta) is recomputed FRESH
// inside each observation's nest (cheap) so its adjoint gets cleaned up by
// the nested zero along with everything else -- an earlier version hoisted
// g outside the nest and its adjoint silently accumulated stale values
// across observations/sweeps, corrupting every column that flowed through it
// (only ka's column was unaffected, because kaV there is a bare alias of
// theta's own vari, not a derived node). theta's own adjoint is zeroed by
// hand each sweep since it lives outside every nest and the nested zero does
// not reach it. Each observation's sum over active doses runs inside its own
// nested_rev_autodiff scope, so set_zero_all_adjoints_nested() only ever
// walks THIS observation's own (small, bounded) local graph, not the whole
// subject's history -- the per-observation cost should stay flat regardless
// of how many observations precede it.
//
// Infusions (doseDur[j] > 0) are split into exactly the two phases the user
// described: (1) "during" -- evaluated directly with the infusion's own rate
// active, yp0=0, dt=elapsed since infusion start; (2) "after" -- the state at
// the moment the infusion ends is computed (one extra, cheap, rate-active
// kernel call with dt=doseDur[j]) and used as the initial condition for a
// plain rate=0 decay, exactly like a virtual bolus placed at the infusion's
// end time. This keeps every dose (bolus or infusion) a bounded, independent,
// theta-and-elapsed-time-only term -- no new tape-growth source.
//
// [[Rcpp::export]]
List linCmtSubjectSuperpositionADProto(NumericVector obsT, NumericVector doseT,
                                       NumericVector doseAmt,
                                       NumericVector doseDur,
                                       double p1, double v1, double p2,
                                       double p3, double p4, double p5,
                                       double ka,
                                       int ncmt, int oral0, int trans,
                                       int bolusCmt) {
  stan::math::recover_memory();
  int nObs = obsT.size();
  int nDose = doseT.size();
  stan::math::linCmtStan lc(ncmt, oral0, trans, true, 0, 0);
  int npars = lc.getNpars();
  int m = ncmt + oral0;

  Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> theta(npars);
  linCmtFillTheta(theta, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka);

  int numLin = m; // rate_ is indexed per-compartment; m is an over-generous but safe bound
  std::vector<double> zeroRate(numLin, 0.0);
  std::vector<double> infRate(numLin, 0.0);

  List out(nObs);
  for (int io = 0; io < nObs; io++) {
    stan::math::nested_rev_autodiff nested; // per-observation, recycled at scope exit
    // Recomputed fresh inside the nest -- see comment above.
    Eigen::Matrix<stan::math::var, Eigen::Dynamic, 2> g =
      stan::math::macros2micros(theta, ncmt, trans);
    stan::math::var kaV = 0.0;
    if (oral0) kaV = theta(ncmt*2, 0);

    Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> total =
      Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1>::Zero(m);
    for (int jd = 0; jd < nDose; jd++) {
      if (doseT[jd] > obsT[io] + 1e-9) continue; // dose hasn't happened yet
      double dur = doseDur[jd];
      Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> retTerm(m);
      if (dur <= 0.0) {
        // Plain bolus: single decay-only term, initial condition = dose amount.
        double elapsed = obsT[io] - doseT[jd];
        lc.setDt(elapsed);
        lc.setRate(zeroRate.data());
        Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> yp0 =
          Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1>::Zero(m);
        yp0(bolusCmt, 0) = doseAmt[jd]; // dose amount is DATA, a fresh constant leaf
        if (ncmt == 1) lc.linCmtStan1<stan::math::var>(g, yp0, kaV, retTerm);
        else if (ncmt == 2) lc.linCmtStan2<stan::math::var>(g, yp0, kaV, retTerm);
        else if (ncmt == 3) lc.linCmtStan3<stan::math::var>(g, yp0, kaV, retTerm);
      } else if (obsT[io] < doseT[jd] + dur - 1e-9) {
        // Phase 1: still infusing -- evaluate directly with the rate active.
        double elapsed = obsT[io] - doseT[jd];
        lc.setDt(elapsed);
        std::fill(infRate.begin(), infRate.end(), 0.0);
        infRate[bolusCmt] = doseAmt[jd] / dur;
        lc.setRate(infRate.data());
        Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> yp0 =
          Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1>::Zero(m);
        if (ncmt == 1) lc.linCmtStan1<stan::math::var>(g, yp0, kaV, retTerm);
        else if (ncmt == 2) lc.linCmtStan2<stan::math::var>(g, yp0, kaV, retTerm);
        else if (ncmt == 3) lc.linCmtStan3<stan::math::var>(g, yp0, kaV, retTerm);
      } else {
        // Phase 2: infusion complete. Recompute the end-of-infusion state
        // fresh (cheap, one extra kernel call) rather than caching it across
        // observations -- same reasoning as recomputing g fresh: anything
        // that survives multiple nests needs manual adjoint bookkeeping, and
        // recomputing avoids that entirely.
        lc.setDt(dur);
        std::fill(infRate.begin(), infRate.end(), 0.0);
        infRate[bolusCmt] = doseAmt[jd] / dur;
        lc.setRate(infRate.data());
        Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> ypZero =
          Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1>::Zero(m);
        Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> endOfInf(m);
        if (ncmt == 1) lc.linCmtStan1<stan::math::var>(g, ypZero, kaV, endOfInf);
        else if (ncmt == 2) lc.linCmtStan2<stan::math::var>(g, ypZero, kaV, endOfInf);
        else if (ncmt == 3) lc.linCmtStan3<stan::math::var>(g, ypZero, kaV, endOfInf);

        double elapsedAfter = obsT[io] - (doseT[jd] + dur);
        lc.setDt(elapsedAfter);
        lc.setRate(zeroRate.data());
        if (ncmt == 1) lc.linCmtStan1<stan::math::var>(g, endOfInf, kaV, retTerm);
        else if (ncmt == 2) lc.linCmtStan2<stan::math::var>(g, endOfInf, kaV, retTerm);
        else if (ncmt == 3) lc.linCmtStan3<stan::math::var>(g, endOfInf, kaV, retTerm);
      }
      total = total + retTerm;
    }
    NumericVector fx(m);
    NumericMatrix J(m, npars);
    for (int k = 0; k < m; k++) fx[k] = total(k, 0).val();
    for (int k = 0; k < m; k++) {
      nested.set_zero_all_adjoints();      // bounded to THIS observation's nodes
      for (int j = 0; j < npars; j++) theta(j, 0).adj() = 0.0; // theta lives outside every nest
      total(k, 0).grad();
      for (int j = 0; j < npars; j++) {
        J(k, j) = theta(j, 0).adj();
      }
    }
    out[io] = List::create(_["val"] = fx, _["J"] = J);
  }
  return out;
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
  // rx_get_thread() honors the cross-DLL thread-id override (see rxData.cpp /
  // setRxThreadId): under an external OpenMP team rxode2's omp_get_thread_num()
  // would return 0 for every worker, collapsing this per-thread linCmt scratch
  // onto slot 0.  __linCmtB (line ~489) already uses rx_get_thread().
  linA_t &lca = __linCmtA[rx_get_thread((int)__linCmtA.size())];
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
  linCmtFillTheta(theta, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka);

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

// These scaling/step-size helpers are read from the finite-difference H setup
// (shi21ForwardH/gillForwardH) after ind_linCmtFH has populated THIS thread's
// linCmtB slot, so they must read the same per-thread slot rx_get_thread()
// selects -- not the hardcoded [0] slot, which another thread mutates (and may
// resize) during a parallel linCmt solve.  rx_get_thread() takes an int, so the
// size_t pool size is cast once to avoid a narrowing conversion.
extern "C" double linCmtScaleInitPar(int which) {
  int tid = rx_get_thread((int)__linCmtB.size());
  return __linCmtB[tid].lc.initPar(which);
}

extern "C" double linCmtScaleInitN() {
  int tid = rx_get_thread((int)__linCmtB.size());
  Eigen::Matrix<double, Eigen::Dynamic, 1> theta = __linCmtB[tid].lc.initPar();
  return theta.size();
}

extern "C" int linCmtZeroJac(int i) {
  int tid = rx_get_thread((int)__linCmtB.size());
  return __linCmtB[tid].lc.parDepV1(i);
}



// linCmtB's which1 = -3 case: the dose-time (moving boundary) sensitivity.
//
// `amt` holds the amounts at the requested time from the which1=-1/which2=-1
// call the caller is required to have made, and the linear system's own
// right-hand side gives d/dL exactly (see linCmtStan::dAdt()).  `rate` is
// either the live ind->InfusionRate slice (while genuinely solving) or the
// linCmtBRateSlot() cache for that idx (a re-query, e.g. the output pass);
// see the caller in linCmtB().  Returns NA_REAL for a call that does not
// describe the model `lc` is set up for, when `rate` could not be recovered
// (NULL -- see linCmtBRateSlot()), for a steady-state infusion
// (linCmtHasSsInfusion()), or for an out of range `which2`.
static inline double linCmtBdoseTime(stan::math::linCmtStan &lc,
                                     const Eigen::Matrix<double, Eigen::Dynamic, 1> &amt,
                                     rx_solving_options_ind *ind,
                                     const double *rate,
                                     int ncmt, int oral0, int which2, int trans,
                                     double p1, double v1,
                                     double p2, double p3,
                                     double p4, double p5,
                                     double ka) {
  if (lc.ncmt_ != ncmt || lc.oral0_ != oral0 || lc.trans_ != trans ||
      (int)amt.size() != ncmt + oral0) {
    return NA_REAL;
  }
  if (rate == NULL) return NA_REAL;
  if (linCmtHasSsInfusion(ind)) return NA_REAL;
  Eigen::Matrix<double, Eigen::Dynamic, 1> th(lc.getNpars());
  if (!linCmtFillTheta(th, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka)) {
    return NA_REAL;
  }
  Eigen::Matrix<double, Eigen::Dynamic, 2> gm =
    stan::math::macros2micros(th, ncmt, trans);
  Eigen::Matrix<double, Eigen::Dynamic, 1> dot(ncmt + oral0);
  lc.dAdt(amt, gm, ka, rate, dot);
  if (which2 == -3) return -dot(oral0, 0) / lc.getVc(th);
  if (which2 >= 0 && which2 < ncmt + oral0) return -dot(which2, 0);
  return NA_REAL;
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
 *  When which1 is -3, the DOSE-TIME (moving boundary) sensitivity is
 *  returned -- the derivative with respect to a delay applied to every dose
 *  feeding the linear system, i.e. what a modeled `alag()` on its dosed
 *  compartment produces (nlmixr2/rxode2#1119).  which2 = -3 gives it for the
 *  reported concentration, which2 >= 0 for the amount in that compartment.
 *  The system is linear and its whole input is delayed together, so
 *  A(t; L) = A(t - L; 0) and the derivative is exactly -dA/dt; chain-rule it
 *  with d(alag)/dp to get the sensitivity wrt a model parameter.  This
 *  requires that EVERY dose reaching the linear system carries the same
 *  `alag()`; it is not the per-compartment derivative of a model that lags
 *  its compartments differently (nlmixr2/rxode2#1237).  A model that would
 *  violate this -- calling `linCmtB(which1 = -3)` while its linCmt()
 *  compartments carry more than one distinct `alag()` expression -- is
 *  refused at build time by `.rxLinCmtDoseTimeSensCheck()` (R/eventSens.R)
 *  rather than silently given the single-delay answer.  A regular infusion's
 *  rate is recovered at output time via the linCmtBRateSlot() per-idx cache
 *  (nlmixr2/rxode2#1236); an individual with a steady-state infusion still
 *  gets `NA_REAL` -- see linCmtHasSsInfusion().
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
// linCmtSensIsAD() (the AD jacobian classifier; forward-mode fvar 3/30,
// reverse-mode 31, auto 100 -> unscaled thetaSens + passthrough trueTheta; the
// finite-difference methods keep the scaled path) lives in linCmtSensType.h so
// linCmt.cpp, par_solve.cpp and rxData.cpp share one definition.
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
  // Per-thread linCmtB state (matching linCmtA pattern for thread safety)
  linB_t &lcb = __linCmtB[rx_get_thread(__linCmtB.size())];
#define fx        lcb.fx
#define Jg        lcb.Jg
#define lc        lcb.lc
#define J         lcb.J
#define Js        lcb.Js
#define yp        lcb.yp
#define g         lcb.g
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
    } else if (which1 == -3) {
      // Mirrors the fx/J restore-vs-solve condition above (idx already
      // solved -> a re-query, e.g. the output pass, where ind->InfusionRate
      // has since been cleared/moved on; use the cached rate instead).
      const double *rate = (!ind->doSS && ind->solvedIdx >= idx) ?
        linCmtBRateSlot(ind, idx, op->numLin, 0) : getLinRate;
      return linCmtBdoseTime(lc, fx, ind, rate, ncmt, oral0, which2,
                             trans, p1, v1, p2, p3, p4, p5, ka);
    } else if (which1 == -4) {
      // d(Alast_i)/d(Alast_{i-1}) -- the state-transition Jacobian, the one
      // missing ingredient for a general (arbitrary covariate formula)
      // time-varying-covariate fix (project_lincmt_timevarying_covariate_bug).
      // Self-contained (recomputes theta/g fresh from the passed-in p1/v1/...
      // rather than relying on any state cached by a prior -1,-1 call) so it
      // does not depend on call-ordering assumptions the other sentinels
      // above rely on. which2 packs (row, col) as row + m*col, row = output
      // compartment, col = which PREVIOUS-timepoint compartment is being
      // differentiated against; m = ncmt+oral0 is derivable by the caller
      // from its own model knowledge. A constant matrix (the closed form is
      // linear in Alast) recomputed via one forward-mode (fvar) pass per
      // call -- opt-in only: an ordinary constant-theta solve never requests
      // which1=-4, so this adds no cost to the common case. Validated (as
      // linCmtAlastTransitionMatrixProto) against rxode2's own
      // linToOde()-generated equivalent ODE model to machine epsilon across
      // all 1/2/3-cmt IV/oral configs -- see
      // feedback_lincmt_verify_against_linToOde_not_invented_odes.
      int m = ncmt + oral0;
      int row = which2 % m;
      int col = which2 / m;
      int npars = lc.getNpars();
      typedef stan::math::fvar<double> fv;
      Eigen::Matrix<double, Eigen::Dynamic, 1> thetaD(npars);
      linCmtFillTheta(thetaD, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka);
      Eigen::Matrix<fv, Eigen::Dynamic, 1> thetaF(npars);
      for (int k = 0; k < npars; k++) thetaF(k, 0) = fv(thetaD(k, 0), 0.0);
      Eigen::Matrix<fv, Eigen::Dynamic, 2> gF = stan::math::macros2micros(thetaF, ncmt, trans);
      fv kaV(0.0, 0.0);
      if (oral0) kaV = thetaF(ncmt*2, 0);

      double dt = ind->doSS ? (ind->tout - ind->tprior) : (_t - ind->tprior);
      lc.setDt(dt);
      const double *rate = (!ind->doSS && ind->solvedIdx >= idx) ?
        linCmtBRateSlot(ind, idx, op->numLin, 0) : getLinRate;
      lc.setRate(const_cast<double*>(rate));

      Eigen::Matrix<fv, Eigen::Dynamic, 1> yp0 =
        Eigen::Matrix<fv, Eigen::Dynamic, 1>::Zero(m);
      yp0(col, 0) = fv(0.0, 1.0);
      Eigen::Matrix<fv, Eigen::Dynamic, 1> ret(m);
      if (ncmt == 1) lc.linCmtStan1<fv>(gF, yp0, kaV, ret);
      else if (ncmt == 2) lc.linCmtStan2<fv>(gF, yp0, kaV, ret);
      else if (ncmt == 3) lc.linCmtStan3<fv>(gF, yp0, kaV, ret);
      return ret(row, 0).d_;
    } else if (which1 == -7) {
      // Add a caller-supplied local contribution (dPredDTheta_i *
      // dThetaDEta_i, computed by the caller -- R for now, generated model
      // code once Phase 3 wires this in) into the stored cumulative carry
      // at (row,col), ON TOP OF whatever which1=-5 already accumulated for
      // this row via the state-transition multiply. which2 packs (row,col)
      // as row + m*col like -4/-5/-6; the value to add rides in p2 (unused
      // by this sentinel otherwise -- no theta is read here).
      int m = ncmt + oral0;
      int row = which2 % m;
      int col = which2 / m;
      ind->linCmtCarryT[row*4 + col] += p2;
      return ind->linCmtCarryT[row*4 + col];
    } else if (which1 == -5 || which1 == -6) {
      // Cumulative-carry sentinels (per-subject storage: ind->linCmtCarryT,
      // see rxode2parseStruct.h; reset at iniSubject() in par_solve.h).
      // which2 packs (row, col) as row + m*col exactly like which1=-4, but
      // reads/writes are into the STORED 4x4 (stride-4) submatrix, not a
      // one-off local matrix.
      //
      // which1=-6 is a pure read: return the current cumulative
      // ind->linCmtCarryT[row*4+col] with no recomputation -- lets a caller
      // inspect T_i without re-triggering an advance.
      //
      // which1=-5 is the mutating advance: T_i = M_i * T_{i-1}, where M_i is
      // THIS interval's local transition matrix (the same quantity which1=-4
      // returns one column of, recomputed here for every column since the
      // full m x m matrix is needed for the multiply). Must be called
      // EXACTLY ONCE per row per subject (documented contract, mirroring
      // which1=-3/-4's own single-evaluation-per-row assumption) -- calling
      // it more than once for the same row would apply the same transition
      // twice. Composing with this row's OWN local contribution
      // (dPredDTheta_i * dThetaDEta_i) is left to the caller (R for now,
      // generated model code once Phase 3 wires this in) -- this sentinel
      // only carries the state-transition part forward.
      int m = ncmt + oral0;
      int row = which2 % m;
      int col = which2 / m;
      if (which1 == -6) {
        return ind->linCmtCarryT[row*4 + col];
      }
      // Unlike which1=-4 (which always follows a which1=-1,-1 call earlier
      // in the same calc_lhs invocation, guaranteeing lc is already sized
      // for this ncmt/oral0/trans), which1=-5 may be the first touch of lc
      // on this thread for a standalone re-query -- size it defensively.
      if (!lc.isSame(ncmt, oral0, trans, rx->ndiff)) {
        lc.setModelType(ncmt, oral0, trans, ind->linSS, rx->ndiff);
      }
      int npars = lc.getNpars();
      typedef stan::math::fvar<double> fv;
      Eigen::Matrix<double, Eigen::Dynamic, 1> thetaD(npars);
      linCmtFillTheta(thetaD, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka);
      Eigen::Matrix<fv, Eigen::Dynamic, 1> thetaF(npars);
      for (int k = 0; k < npars; k++) thetaF(k, 0) = fv(thetaD(k, 0), 0.0);
      Eigen::Matrix<fv, Eigen::Dynamic, 2> gF = stan::math::macros2micros(thetaF, ncmt, trans);
      fv kaV(0.0, 0.0);
      if (oral0) kaV = thetaF(ncmt*2, 0);

      double dt = ind->doSS ? (ind->tout - ind->tprior) : (_t - ind->tprior);
      lc.setDt(dt);
      const double *rate = (!ind->doSS && ind->solvedIdx >= idx) ?
        linCmtBRateSlot(ind, idx, op->numLin, 0) : getLinRate;
      // linCmtBRateSlot() returns NULL when this idx's rate was never
      // cached live -- expected for -5 called as a standalone re-query well
      // after the whole subject finished solving (the cache is only ever
      // written while an idx is genuinely being solved for the first time,
      // src/linCmt.cpp ~line 1698), which never happens again once
      // ind->solvedIdx has reached its final value. In that situation
      // ind->InfusionRate (getLinRate) is still a safe, defined per-thread
      // buffer to fall back to -- same defensive intent already documented
      // on linCmtBRateSlot() itself ("NULL is a defensive fallback ... not
      // a case expected to occur"), just now actually handled by a caller.
      if (rate == NULL) rate = getLinRate;
      lc.setRate(const_cast<double*>(rate));

      // Full local m x m transition matrix: one forward-mode pass per column.
      double localM[16];
      for (int c = 0; c < m; c++) {
        Eigen::Matrix<fv, Eigen::Dynamic, 1> yp0c =
          Eigen::Matrix<fv, Eigen::Dynamic, 1>::Zero(m);
        yp0c(c, 0) = fv(0.0, 1.0);
        Eigen::Matrix<fv, Eigen::Dynamic, 1> retc(m);
        if (ncmt == 1) lc.linCmtStan1<fv>(gF, yp0c, kaV, retc);
        else if (ncmt == 2) lc.linCmtStan2<fv>(gF, yp0c, kaV, retc);
        else if (ncmt == 3) lc.linCmtStan3<fv>(gF, yp0c, kaV, retc);
        for (int r = 0; r < m; r++) localM[r*4 + c] = retc(r, 0).d_;
      }
      // T_new = M * T_old (only the top-left m x m submatrix participates;
      // untouched entries of the 4x4 buffer stay 0 from iniSubject()'s reset).
      double tNew[16];
      for (int r = 0; r < m; r++) {
        for (int c = 0; c < m; c++) {
          double s = 0.0;
          for (int k2 = 0; k2 < m; k2++) {
            s += localM[r*4 + k2] * ind->linCmtCarryT[k2*4 + c];
          }
          tNew[r*4 + c] = s;
        }
      }
      for (int r = 0; r < m; r++) {
        for (int c = 0; c < m; c++) {
          ind->linCmtCarryT[r*4 + c] = tNew[r*4 + c];
        }
      }
      return ind->linCmtCarryT[row*4 + col];
    }
  } else if (!lc.isSame(ncmt, oral0, trans, rx->ndiff)) {
    lc.setModelType(ncmt, oral0, trans, ind->linSS, rx->ndiff);
    // only resize when needed
    fx = Eigen::Matrix<double, Eigen::Dynamic, 1>(ncmt + oral0);
    int npars = lc.getNpars();
    // NA fill and resize
    J = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>::Constant(ncmt + oral0, npars, NA_REAL);

    lcb.numSens = lc.numSens();
    Js = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(ncmt+oral0, lcb.numSens);//(ncmt + oral0, 2*ncmt + oral0);
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
    theta(getLinCmtDoubleAddr(lcb, linCmtBaddrTheta), lc.getNpars());

  linCmtFillTheta(theta, ncmt, oral0, p1, v1, p2, p3, p4, p5, ka);

  Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, 1> >
    thetaSens(getLinCmtDoubleAddr(lcb, linCmtBaddrThetaSens), lcb.numSens);

  // isAD (unscaled thetaSens + passthrough trueTheta) is used by every AD
  // jacobian path: forward-mode (3/30/auto), reverse-mode (31). The finite
  // difference methods keep the scaled path.
  lc.sensTheta(theta, thetaSens, linCmtSensIsAD(rx->sensType), rx->linCmtScale);
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

  // idx is genuinely being solved right now (not a re-query of an
  // already-solved index, e.g. the output pass) -- ind->InfusionRate is live
  // and correct, so cache it for linCmtBdoseTime() (which1 = -3) to read back
  // on a later re-query, once ind->InfusionRate has been cleared/moved on.
  if (op->numLin > 0 && (ind->doSS || ind->solvedIdx < idx)) {
    double *rslot = linCmtBRateSlot(ind, idx, op->numLin, 1);
    if (rslot != NULL) std::copy(r, r + op->numLin, rslot);
  }

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

        case 31: // reverse-mode AD (escape hatch / validation)
          stan::math::jacobian(lc, thetaSens, fx, Js);
          break;

        case 3:  // "AD": now forward-mode (fvar); matches reverse to round-off
        case 30: // explicit forward-mode AD
        default: // auto and anything unspecified -> forward-mode AD
          lc.linCmtFwdJac(thetaSens, fx, Js);
          break;
        }
        lc.updateJfromJs(J, Js);
        lc.saveJac(J);
      }
    }
  }
  lc.getJacCp(J, fx, theta, Jg);
  return lc.adjustF(fx, theta, ind->linCmtHV);
#undef fx
#undef J
#undef Jg
#undef lc
#undef Js
#undef yp
#undef g
}
