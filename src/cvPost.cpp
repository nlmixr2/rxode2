//#undef NDEBUG
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <RcppArmadillo.h>
#include "../inst/include/rxode2.h"
#include <R.h>
#include <threefry.h>
#include <libintl.h>
#include "checkmate.h"
#include <boost/algorithm/string/predicate.hpp>
#include "../inst/include/rxode2parse.h"
#include "../inst/include/rxode2_as.h"
#include <lotri.h>

extern "C" SEXP chin(SEXP a, SEXP b);

// place links here
iniLotri;

List etTrans(List inData, const RObject &mv, bool addCmt,
             bool dropUnits, bool allTimeVar,
             bool keepDosingOnly, Nullable<LogicalVector> combineDvid,
             CharacterVector keep,
             bool addlKeepsCov,
             bool addlDropSs,
             bool ssAtDoseTime,
             Nullable<List> iCovIn);
SEXP nestingInfo_(SEXP omega, List data);

List rxExpandNesting(const RObject& obj, List& nestingInfo,
                     bool compile=false);

List rxModelVars_(const RObject &obj);

extern "C" int get_sexp_uniqueL( SEXP s );

extern "C" SEXP _cbindOme(SEXP et_, SEXP mat_, SEXP n_);

extern "C" SEXP _vecDF(SEXP cv, SEXP n_);
SEXP convertId_(SEXP x);

SEXP rxRmvnSEXP(SEXP nS, SEXP muS, SEXP sigmaS,
                SEXP lowerS, SEXP upperS, SEXP ncoresS, SEXP isCholS,
                SEXP keepNamesS,
                SEXP aS, SEXP tolS, SEXP nlTolS, SEXP nlMaxiterS);

void rxModelsAssign(std::string str, SEXP assign);

LogicalVector rxSolveFree();

bool gotLotriMat=false;

using namespace Rcpp;
using namespace arma;

arma::mat rwish5(double nu, int p){
  // GetRNGstate();
  arma::mat Z(p,p, fill::zeros);
  double curp = nu;
  double tmp =sqrt(Rf_rchisq(curp--));
  Z(0,0) = (tmp < 1e-100) ? 1e-100 : tmp;
  int i, j;
  if (p > 1){
    for (i = 1; i < (int)p; i++){
      tmp = sqrt(Rf_rchisq(curp--));
      Z(i,i) = (tmp < 1e-100) ? 1e-100 : tmp;
      for (j = 0; j < i; j++){
        // row,col
        Z(j,i) = norm_rand();
      }
    }
  }
  // PutRNGstate();
  return Z;
}

NumericMatrix cvPost0(double nu, NumericMatrix omega, bool omegaIsChol = false,
                      bool returnChol = false){
  arma::mat S =as<arma::mat>(omega);
  if (S.is_zero()){
    return omega;
  }
  int p = S.n_rows;
  if (p == 1){
    // GetRNGstate();
    NumericMatrix ret(1,1);
    if (S.is_zero()) {
      ret[0] = 0.0;
    } else {
      bool retIsGood = false;
      while (!retIsGood) {
        if (omegaIsChol){
          ret[0] = nu*omega[0]*omega[0]/(Rf_rgamma(nu/2.0,2.0));
        } else {
          ret[0] = nu*omega[0]/(Rf_rgamma(nu/2.0,2.0));
        }
        if (R_FINITE(ret[0])) {
          retIsGood = true;
        }
      }
      if (returnChol) ret[0] = sqrt(ret[0]);
    }
    // PutRNGstate();
    return ret;
  } else {
    arma::mat Z = rwish5(nu, p);
    arma::mat Z2 = arma::trans(arma::solve(trimatu(Z), eye(p, p)));
    arma::mat cv5;
    if (omegaIsChol){
      cv5 = S;
    } else {
      cv5 = arma::chol(S);
    }
    arma::mat mat1 = Z2 * cv5;
    mat1 = mat1.t() * mat1;
    mat1 = mat1 * nu;
    if (returnChol) mat1 = arma::chol(mat1);
    return wrap(mat1);
  }
}

//' Scaled Inverse Chi Squared distribution
//'
//' @param n Number of random samples
//'
//' @param nu degrees of freedom of inverse chi square
//'
//' @param scale  Scale of inverse chi squared distribution
//'         (default is 1).
//'
//' @return a vector of inverse chi squared deviates.
//'
//' @examples
//' rinvchisq(3, 4, 1) ## Scale = 1, degrees of freedom = 4
//' rinvchisq(2, 4, 2) ## Scale = 2, degrees of freedom = 4
//' @export
//[[Rcpp::export]]
NumericVector rinvchisq(const int n = 1, const double &nu = 1.0, const double &scale = 1){
  NumericVector ret(n);
  // GetRNGstate();
  for (int i = 0; i < n; i++){
    ret[i] = nu*scale/(Rf_rgamma(nu/2.0,2.0));
  }
  // PutRNGstate();
  return ret;
}

// Adapted from banocc and ported to armadillo for speed.
// https://github.com/biobakery/banocc/blob/master/R/rlkj.R
void rgbeta(int d, double shape, double* out){
  if (std::isinf(shape)) {
    std::fill_n(out, d, 0.0);
  } else if (shape > 0){
    for (int j = d; j--;){
      out[j] = 2.0*Rf_rbeta(shape, shape) - 1.0;
    }
  } else if (shape == 0){
    for (int j = d; j--;){
      out[j] = 2.0*Rf_rbinom(1, 0.5) - 1.0;
    }
  } else {
    stop(_("'shape' must be non-negative"));
  }
}
//' One correlation sample from the LKJ distribution
//'
//' @param d The dimension of the correlation matrix
//'
//' @param eta The scaling parameter of the LKJ distribution.
//'   Must be > 1.  Also related to the degrees of freedom nu.
//'   eta = (nu-1)/2.
//'
//' @param cholesky boolean; If `TRUE` return the cholesky
//'   decomposition.
//'
//' @return A correlation sample from the LKJ distribution
//'
//' @author Matthew Fidler (translated to RcppArmadillo) and Emma Schwager
//' @export
//' @keywords internal
//[[Rcpp::export]]
arma::mat rLKJ1(int d, double eta = 1.0, bool cholesky = false){
  if (d < 2){
    stop(_("dimension, 'd' of correlation matrix must be > 1"));
  }
  if (eta < 1){
    stop(_("'eta' must be >= 1"));
  }
  double alpha = eta + ((double)(d) - 2.0)/2.0;
  arma::mat L(d,d,arma::fill::zeros);
  L(0,0) = 1.0;
  arma::vec partials(d-1);
  rgbeta(d-1, alpha, partials.memptr());
  std::copy(partials.begin(), partials.end(), L.memptr()+1);
  if (d == 2){
    L(1,1) = sqrt(1-L(1,0)*L(1,0));
    if (!cholesky){
      L = L * L.t();
    }
    return L;
  }
  arma::vec W = log(1-partials%partials);
  for (int i = 2; i <= d-1; i++){
    alpha -= 0.5;
    rgbeta(d-i, alpha, partials.memptr());
    // construct a vector pointing to the partials vector without
    // allocating new memory:
    arma::vec partials2 = arma::vec(partials.memptr(), d-i, false, true);
    L(i-1,i-1) = exp(0.5*W(i-2));
    L(arma::span(i,d-1),i-1) = partials2 % exp(0.5*W(arma::span(i-1,d-2)));
    W(arma::span(i-1,d-2)) = W(arma::span(i-1,d-2)) +log(1-partials2%partials2);
  }
  L(d-1,d-1) = exp(0.5*W(d-2));
  if (!cholesky){
    L = L * L.t();
  }
  return L;
}

//[[Rcpp::export]]
arma::mat rLKJcv1(arma::vec sd, double eta = 1.0){
  int d = sd.size();
  arma::mat r = rLKJ1(d, eta, false);
  arma::mat dSd = diagmat(sd);
  return dSd*r*dSd;
}

//[[Rcpp::export]]
arma::mat rLKJcvLsd1(arma::vec logSd, arma::vec logSdSD, double eta = 1.0){
  unsigned int d = logSd.size();
  if (d != logSdSD.size()){
    stop(_("log standard deviation size needs to be the same size as the log standard error of the estimate"));
  }
  arma::vec sd(d);
  for (unsigned int j = d; j--;){
    sd[j] = exp(Rf_rnorm(logSd[j], logSdSD[j]));
  }
  return rLKJcv1(sd, eta);
}

//' One correlation sample from the Inverse Wishart distribution
//'
//' This correlation is constructed by transformation of the Inverse Wishart
//' random covariate to a correlation.
//'
//' @inheritParams rLKJ1
//'
//' @param nu Degrees of freedom of the Wishart distribution
//'
//' @inheritParams cvPost
//'
//' @return One correlation sample from the inverse wishart
//'
//' @author Matthew Fidler
//' @keywords internal
//' @export
//[[Rcpp::export]]
arma::mat invWR1d(int d, double nu, bool omegaIsChol = false){
  if (nu <= d - 1) stop(_("'nu' must be greater than 'd'-1"));
  arma::mat I(d,d,arma::fill::eye);
  arma::mat invW = as<arma::mat>(cvPost0(nu, wrap(I),
                                         omegaIsChol, false));
  arma::mat Dinv = diagmat(1/sqrt(invW.diag()));
  return Dinv * invW * Dinv;
}

arma::mat rinvWRcv1(arma::vec sd, double nu = 1.0){
  int d = sd.size();
  arma::mat r = invWR1d(nu, d, false);
  arma::mat dSd = diagmat(sd);
  return dSd*r*dSd;
}

//[[Rcpp::export]]
arma::mat rcvC1(arma::vec sdEst, double nu = 3.0,
                int diagXformType = 1, int rType = 1,
                bool returnChol = false){
  // the sdEst should come from the multivariate normal distribution
  // with the appropriate transformation.
  unsigned int d = sdEst.size();
  // Nlmixr models variances as chol(omega^1)
  // var = diag(omega)
  // Assuming off-diagonals are zero
  // diag(omega^1) = (1/var)
  // chol(diag(omega^1)) = sqrt(1/var) = 1/sd
  // With diagonals this becomes
  // diagXform = c("sqrt", "log", "identity")
  //
  arma::vec sd(d);
  switch(diagXformType){
  case 1:
    // sqrt; In this case we estimate x^2
    // x^2 = 1/sd
    // sd = 1/x^2
    for (int j = d; j--;){
      sd[j] = 1/(sdEst[j]*sdEst[j]);
    }
    break;
  case 2:
    // log
    // In this case we estimate exp(x)
    // exp(x) = 1/sd
    // sd = 1/exp(x)
    for (int j = d; j--;){
      sd[j] = 1/exp(sdEst[j]);
    }
    break;
  case 3:
    // identity
    // In this case we estimate x
    // sd = 1/x
    for (int j = d; j--;){
      sd[j] = 1/sdEst[j];
    }
    break;
  case 4: // direct identity
    for (int j = d; j--;){
      sd[j] = sdEst[j];
    }
    break;
  case 5: // lognormal
    for (int j = d; j--;){
      sd[j] = exp(sdEst[j]);
    }
    break;
  case 6: // direct variance
    for (int j = d; j--;){
      sd[j] = sqrt(sdEst[j]);
    }
    break;
  default:
    stop(_("unknown 'diagXformType' transformation"));
  }
  arma::mat ret;
  if (sd.size() == 1) {
    ret = ret(1,1);
    ret(0,0) = sd[0]*sd[0];
  } else {
    if (rType == 1) {
      ret = rLKJcv1(sd, (nu-1.0)/2.0);
    } else {
      ret = rinvWRcv1(sd, nu);
    }
    if (returnChol){
      ret = arma::chol(ret);
    }
  }
  return ret;
}

double getDbl(SEXP in, const char *var){
  double ret = 0;
  if (qtest(in, "I1")) {
    ret = INTEGER(in)[0];
  } else {
    qassertS(as<RObject>(in), "R1", var);
    ret = REAL(in)[0];
  }
  return ret;
}

//[[Rcpp::export]]
SEXP cvPost_(SEXP nuS, SEXP omegaS, SEXP nS, SEXP omegaIsCholS,
             SEXP returnCholS, SEXP typeS, SEXP diagXformTypeS) {
  int diagXformType = 1;
  qassertS(nS, "X1[1,)", "n");
  qassertS(omegaIsCholS, "B1", "omegaIsChol");
  bool omegaIsChol = as<bool>(omegaIsCholS);
  qassertS(returnCholS, "B1", "returnChol");
  bool returnChol = as<bool>(returnCholS);
  int n = as<int>(nS);
  int type=1;
  if (qtest(typeS, "X1[1,3]")){
    type = as<int>(typeS);
  } else if (qtest(typeS, "S1")){
    std::string typeStr = as<std::string>(typeS);
    if (typeStr == "invWishart") {
      type = 1;
    } else if (typeStr == "lkj") {
      type = 2;
    } else if (typeStr == "separation") {
      type = 3;
    } else {
      stop(_("variable 'type': Unrecognized cvPost type='%s'"), typeStr.c_str());
    }
  } else {
    stop(_("variable 'type': Can only use type string or integer[1,3]"));
  }
  if (n == 1 && type == 1){
    if (qtest(omegaS, "M")) {
      double nu = getDbl(nuS, "nu");
      RObject omega = omegaS;
      RObject ret = as<RObject>(cvPost0(nu, as<NumericMatrix>(omegaS), omegaIsChol, returnChol));
      ret.attr("dimnames") = omega.attr("dimnames");
      return as<SEXP>(ret);
    } else if (Rf_isReal(omegaS) || Rf_isInteger(omegaS)){
      double nu = getDbl(nuS, "nu");
      NumericVector om1 = as<NumericVector>(omegaS);
      if (om1.size() % 2 == 0){
        int n1 = om1.size()/2;
        NumericMatrix om2(n1,n1);
        for (int i = 0; i < om1.size();i++){
          om2[i] = om1[i];
        }
        return as<SEXP>(cvPost0(nu, om2, omegaIsChol, returnChol));
      }
    } else if (!Rf_isNull(omegaS) && isLotri(omegaS)) {
      RObject omega = as<RObject>(omegaS);
      List omegaIn = as<List>(omega);
      RObject omegaInNamesRO = Rf_getAttrib(omega, R_NamesSymbol);
      if (Rf_isNull(omegaInNamesRO)) {
        print(omega);
        stop(_("the 'lotri' omega matrix needs to be named (printed above)"));
      }
      CharacterVector omegaInNames = as<CharacterVector>(omegaInNamesRO);
      int nOmega = omegaIn.size();
      List omegaLst(nOmega);
      List lotriLst = as<List>(omega.attr("lotri"));
      // type = 1
      if (lotriLst.size() > 0) {
        for (int ii = 0; ii < nOmega; ++ii) {
          List curOmegaLst;
          int nsame = 1;
          double nu = 1.0;
          if (lotriLst.containsElementNamed((as<std::string>(omegaInNames[ii])).c_str())){
            curOmegaLst = lotriLst[as<std::string>(omegaInNames[ii])];
            if (curOmegaLst.containsElementNamed("nu")){
              nu = asDouble(curOmegaLst["nu"], "nu");
            }
            if (curOmegaLst.containsElementNamed("same")){
              nsame = asInt(curOmegaLst["same"], "same");
            }
          }
          RObject cur;
          if (nu > 1) {
            NumericMatrix tmp = as<NumericMatrix>(omegaIn[ii]);
            cur = as<RObject>(cvPost0(nu, tmp, false, false));
            cur.attr("dimnames") = tmp.attr("dimnames"); // Preserve dimnames
          } else {
            cur = omegaIn[ii];
          }
          if (nsame > 1) {
            List curl(2);
            curl[0] = cur;
            curl[1] = nsame;
            omegaLst[ii] = curl;
          } else {
            omegaLst[ii] = cur;
          }
        }
      }
      IntegerVector startAt(1);
      if (omega.hasAttribute("start")) {
        startAt[0] = asInt(omega.attr("start"), "start");
      } else {
        startAt[0] = 1;
      }
      SEXP format = R_NilValue;
      if (omega.hasAttribute("format")) {
        format = omega.attr("format");
      }
      return as<SEXP>(lotriLstToMat(as<SEXP>(omegaLst), format,
                                    as<SEXP>(startAt),
                                    as<SEXP>(CharacterVector::create("matrix", "array"))));
    }
  } else {
    if (type == 1){
      List ret(n);
      IntegerVector nIS = IntegerVector::create(1);
      for (int i = 0; i < n; i++){
        ret[i] = cvPost_(nuS, omegaS, nIS, omegaIsCholS,
                         returnCholS, nIS, wrap(nIS));
      }
      return(as<SEXP>(ret));
    } else {
      if (qtest(omegaS, "M")){
        double nu = getDbl(nuS, "nu");
        if (qtest(diagXformTypeS, "S1")) {
          //("log", "identity", "variance", "nlmixrSqrt", "nlmixrLog", "nlmixrIdentity")
          std::string diagXformTypeStr = as<std::string>(diagXformTypeS);
          if (diagXformTypeStr == "nlmixrSqrt"){
            diagXformType=1;
          } else if (diagXformTypeStr == "nlmixrLog"){
            diagXformType=2;
          } else if (diagXformTypeStr == "nlmixrIdentity"){
            diagXformType=3;
          } else if (diagXformTypeStr == "identity") {
            diagXformType=4;
          } else if (diagXformTypeStr == "log") {
            diagXformType=5;
          } else if (diagXformTypeStr == "variance") {
            diagXformType=6;
          } else {
            stop(_("variable 'diagXformType': Unrecognized transformation '%s'"), diagXformTypeStr.c_str());
          }
        } else if (qtest(diagXformTypeS, "X1[1,6]")) {
          diagXformType = as<int>(diagXformTypeS);
        } else {
          stop(_("variable 'diagXformType': Can only use transformation string or integer[1,6]"));
        }
        RObject omega = omegaS;
        arma::mat om0 = as<arma::mat>(omega);
        om0 = om0.t();
        List ret(om0.n_cols);
        if (n != 1) Rf_warningcall(R_NilValue, "%s", _("'n' is determined by the 'omega' argument which contains the simulated standard deviations"));
        for (unsigned int i = 0; i < om0.n_cols; i++){
          arma::vec sd = om0.col(i);
          if (nu < 3){
            stop("'nu' must be >= 3");
          }
          bool goodSolve = false;
          arma::mat reti;
          while (!goodSolve) {
            reti = rcvC1(sd, nu, diagXformType, type-1, returnChol);
            if (reti.is_finite()) {
              goodSolve = true;
            }
          }
          RObject retc = wrap(reti);
          retc.attr("dimnames") = omega.attr("dimnames");
          ret[i] = retc;
        }
        return(as<SEXP>(ret));
      } else {
        stop(_("when sampling from correlation priors to create covariance matrices, the input must be a matrix of standard deviations"));
      }
    }
  }
  stop(_("'omega' needs to be a matrix or a numeric vector that can be converted to a matrix"));
  return R_NilValue;
}


extern "C" void setZeroMatrix(int which);

//[[Rcpp::export]]
SEXP expandTheta_(SEXP thetaS, SEXP thetaMatS,
                  SEXP thetaLowerS, SEXP thetaUpperS,
                  SEXP nStudS, SEXP nCoresRVS) {
  if (Rf_isNull(thetaS)) {
    if (!Rf_isNull(thetaMatS)){
      stop(_("'thetaMat' needs 'params' to be non-NULL"));
    }
    return R_NilValue;
  }
  qassertS(nStudS, "X1[1,)", "nStud");
  if (Rf_isNull(thetaMatS)) {
    if (Rf_isMatrix(thetaS)) {
      return as<SEXP>(as<DataFrame>(thetaS));
    } else if (Rf_inherits(thetaS, "data.frame")) {
      return thetaS;
    } else {
      return _vecDF(thetaS, nStudS);
    }
  }
  if (qtest(thetaS, "M")){
    stop(_("when specifying 'thetaMat', 'omega', or 'sigma' the parameters cannot be a 'data.frame'/'matrix'"));
  }
  // int nStud = as<int>(nStudS);
  // thetaMat
  qassertS(thetaMatS, "M", "thetaMat");
  NumericMatrix thetaMat = as<NumericMatrix>(thetaMatS);
  arma::mat tmpM = as<arma::mat>(thetaMat);
  if (tmpM.is_zero()){
    setZeroMatrix(1);
  } else if (!tmpM.is_sympd()){
    rxSolveFree();
    stop(_("'thetaMat' must be a symmetric, positive definite matrix"));
  }
  CharacterVector thetaMatDimNames = as<CharacterVector>(as<List>(thetaMat.attr("dimnames"))[1]);
  qstrictS(as<SEXP>(thetaMatDimNames), "thetaMat dimnames");
  // theta
  qassertS(thetaS, "R+", "theta");
  qstrictSn(thetaS, "theta names");
  NumericVector theta00 = as<NumericVector>(thetaS);
  NumericVector theta0;
  NumericVector theta1;
  CharacterVector theta1n;
  NumericVector theta;
  if (theta00.size() == thetaMat.nrow()) {
    theta0 = theta00;
    theta = NumericVector(theta0.size());
    // Order 'theta' to have same order as 'thetaMat'
    for (R_xlen_t i = 0; i < theta0.size(); ++i) {
      // Will throw an error if not found.
      int cur = theta0.findName(as<std::string>(thetaMatDimNames[i]));
      theta[i]= theta0[cur];
    }
    Rf_setAttrib(theta, R_NamesSymbol, thetaMatDimNames);
  } else if (theta00.size() > thetaMat.nrow()) {
    theta0 = NumericVector(thetaMat.nrow());
    theta1n = CharacterVector(theta00.size() - thetaMat.nrow());
    theta1 = NumericVector(theta00.size() - thetaMat.nrow());
    CharacterVector theta00n = Rf_getAttrib(theta00, R_NamesSymbol);
    R_xlen_t k = 0;
    for (R_xlen_t j = theta00.size(); j--;){
      bool found = false;
      std::string curS = as<std::string>(theta00n[j]);
      for (R_xlen_t i = thetaMat.nrow(); i--;){
        if (curS == as<std::string>(thetaMatDimNames[i])) {
          theta0[i] = theta00[j];
          found = true;
          break;
        }
      }
      if (!found) {
        theta1[k] = theta00[j];
        theta1n[k++] = theta00n[j];
      }
    }
    theta1.names() = theta1n;
    theta = theta0;
    theta.names() = thetaMatDimNames;
  } else {
    stop(_("'theta' must be the same size as 'thetaMat'"));
  }
  // Theta and thetaMat are correct, assign ".theta"
  rxModelsAssign(".theta", thetaMatS);

  qassertS(nCoresRVS, "X1[1,)", "nCoresRV");
  NumericMatrix retNM = rxRmvnSEXP(nStudS, as<SEXP>(theta), as<SEXP>(thetaMat),
                                   thetaLowerS, thetaUpperS, nCoresRVS,
                                   as<SEXP>(LogicalVector::create(false)), // isChol
                                   as<SEXP>(LogicalVector::create(true)), // keepNames
                                   as<SEXP>(NumericVector::create(0.4)), // a
                                   as<SEXP>(NumericVector::create(2.05)), // tol
                                   as<SEXP>(NumericVector::create(1e-10)), // nlTol
                                   as<SEXP>(IntegerVector::create(100))
                                   );
  int nrow = retNM.nrow();
  List ret(retNM.ncol()+theta1.size());
  CharacterVector retN(retNM.ncol()+theta1.size());
  for (R_xlen_t i = theta1.size(); i--;) {
    NumericVector cur(nrow);
    std::fill(cur.begin(), cur.end(), theta1[i]);
    ret[i] = cur;
    retN[i] = theta1n[i];
  }
  for (R_xlen_t i = retNM.ncol(); i--;){
    NumericVector cur(nrow);
    std::copy(retNM.begin()+nrow*i, retNM.begin()+nrow*(i+1),
              cur.begin());
    ret[theta1.size()+i] = cur;
    retN[theta1.size()+i] = thetaMatDimNames[i];
  }
  ret.names() = retN;
  Rf_setAttrib(ret, R_RowNamesSymbol,
               wrap(IntegerVector::create(NA_INTEGER, -nrow)));
  Rf_setAttrib(ret, R_ClassSymbol,
               wrap("data.frame"));
  return as<SEXP>(ret);
}


static inline int getMethodInt(std::string& methodStr, CharacterVector& allNames, SEXP et) {
  int methodInt=1;
  if (methodStr == "auto") {
    // FIXME don't use %in%/%chin% from R
    LogicalVector inL = as<LogicalVector>(chin(allNames, Rf_getAttrib(et, R_NamesSymbol)));
    bool allIn = true;
    for (int j = inL.size(); j--;){
      if (!inL[j]) {
        allIn = false;
        break;
      }
    }
    if (allIn) {
      if (allNames.size() > 9) {
        methodInt=3;//"separation";
      } else {
        methodInt = 2; //"ijk";
      }
    } else {
      methodInt = 1;//"invWishart";
    }
  } else {
    if (methodStr == "ijk") {
      methodInt = 2;
    } else if (methodStr == "separation") {
      methodInt = 3;
    } else {
      methodInt = 4;
    }
  }
  return methodInt;
}

//[[Rcpp::export]]
SEXP expandPars_(SEXP objectS, SEXP paramsS, SEXP eventsS, SEXP controlS) {
  // SEXP events = as<DataFrame>(events);
  qassertS(controlS, "l+", "control");
  List control = as<List>(controlS);
  int pro = 0;
  SEXP nStudS = PROTECT(control[Rxc_nStud]); pro++;
  int nStud = as<int>(nStudS);
  int nSub = as<int>(control[Rxc_nSub]);
  SEXP nestObj = objectS;
  rxModelsAssign(".nestObj", objectS);
  rxModelsAssign(".nestEvents", eventsS);
  List et = expandTheta_(paramsS, control[Rxc_thetaMat],
                         control[Rxc_thetaLower], control[Rxc_thetaUpper],
                         nStudS, control[Rxc_nCoresRV]);
  SEXP omegaS = PROTECT(control[Rxc_omega]); pro++;
  SEXP omegaLotri = R_NilValue;
  if (qtest(omegaS, "M")) {
    RObject omegaR = as<RObject>(omegaS);
    RObject dimnames = omegaR.attr("dimnames");
    qstrictSdn(omegaS, "omega");
    SEXP omegaIsCholS = PROTECT(control[Rxc_omegaIsChol]); pro++;
    qassertS(omegaIsCholS, "b1", "omega");
    bool omegaIsChol = as<bool>(omegaIsCholS);
    arma::mat omega = as<arma::mat>(omegaS);
    if (omegaIsChol) {
      omega = omega * omega.t();
    }
    if (omega.is_zero()){
      setZeroMatrix(2);
    } else if (!omega.is_sympd()){
      rxSolveFree();
      stop(_("'omega' must be symmetric, positive definite"));
    }
    SEXP omegaPre = PROTECT(wrap(omega)); pro++;
    Rf_setAttrib(omegaPre, R_DimNamesSymbol, as<SEXP>(dimnames));

    // Convert to a lotri matrix
    omegaLotri = PROTECT(asLotriMat(omegaPre,
                                    as<SEXP>(List::create(_["lower"] = control[Rxc_omegaLower],
                                                          _["upper"] = control[Rxc_omegaUpper],
                                                          _["nu"]    = control[Rxc_dfSub])),
                                    as<SEXP>(CharacterVector::create("id")))); pro++;
  } else if (isLotri(omegaS)) {
    omegaLotri = omegaS;
  } else if (Rf_isNull(omegaS)){
    rxModelsAssign(".theta", R_NilValue);
    rxModelsAssign(".nestInfo", R_NilValue);
  } else {
    rxSolveFree();
    UNPROTECT(pro);
    stop(_("'omega' needs to be a matrix or lotri matrix"));
  }
  SEXP aboveSEXP, belowSEXP;
  SEXP lotriAbove = R_NilValue,
    lotriBelow = R_NilValue;
  int nid = 1;
  List ni;
  CharacterVector allNames;
  std::string methodStr;
  int methodInt = 1;
  List mv = rxModelVars_(objectS);
  SEXP events = R_NilValue;
  if (!Rf_isNull(omegaS)) {
    // At this point omegaLotri is a lotri matrix, so you can see if you
    // can skip getting the nesting information when there isn't any
    // nesting levels to be worked out.
    //
    // When there is only one level in omega, there is no nesting and
    // the nesting information can be inferred
    RObject omegaLotriRO = as<RObject>(omegaLotri);
    CharacterVector omegaLotriNames;
    SEXP tmp = Rf_getAttrib(omegaLotriRO, R_NamesSymbol);
    if (!Rf_isNull(tmp)) {
      omegaLotriNames=tmp;
    }
    if (omegaLotriNames.size() > 1) {
      List eventsL = as<List>(eventsS);
      CharacterVector eventNames = eventsL.names();
      ni = nestingInfo_(omegaLotri, eventsL);
      IntegerVector idIV =  as<IntegerVector>(ni["id"]);//length(levels(.ni$id))
      nid = Rf_length(Rf_getAttrib(idIV, R_LevelsSymbol));
      if (nid <= 1) {
      } else if (nSub <= 1) {
        // control[Rxc_nSub] = nid;
        nSub = nid;
      } else if ((nSub*nStud) % nid != 0) {
        rxSolveFree();
        UNPROTECT(pro);
        stop(_("provided multi-subject data (n=%d) trying to simulate a different number of subjects (n=%d)"),
             nid, nSub*nStud);
      }
      RObject objectRO = as<RObject>(objectS);
      IntegerVector flags = as<IntegerVector>(mv[RxMv_flags]);
      int cureta = flags[RxMvFlag_maxeta]+1;
      int curtheta = flags[RxMvFlag_maxtheta]+1;
      List en = rxExpandNesting(objectRO, ni, true);
      aboveSEXP = PROTECT(as<SEXP>(ni["above"])); pro++;
      belowSEXP = PROTECT(as<SEXP>(ni["below"])); pro++;
      List lotriSepMat = as<List>(PROTECT(lotriSep(omegaLotri,aboveSEXP,
                                                   belowSEXP,
                                                   as<SEXP>(IntegerVector::create(cureta)),
                                                   as<SEXP>(IntegerVector::create(curtheta))))); pro++;
      lotriAbove = lotriSepMat["above"];
      lotriBelow = lotriSepMat["below"];
      events = ni["data"];
      rxModelsAssign(".nestObj",    en["mod"]);
      nestObj = en["mod"];
      rxModelsAssign(".nestEvents", ni["data"]);
      rxModelsAssign(".nestTheta",  en["theta"]);
      rxModelsAssign(".nestEta",    en["eta"]);
    } else {
      aboveSEXP = R_NilValue;
      belowSEXP = omegaS;
      lotriBelow = omegaLotri;
      events = PROTECT(etTrans(as<List>(eventsS), nestObj,
                               (INTEGER(mv[RxMv_flags])[RxMvFlag_hasCmt] == 1),
                               false, false, true, R_NilValue,
                               control[Rxc_keepF],
                               control[Rxc_addlKeepsCov],
                               control[Rxc_addlDropSs],
                               control[Rxc_ssAtDoseTime],
                               control[Rxc_iCov])); pro++;
      rxModelsAssign(".nestEvents", events);
      RObject cls = Rf_getAttrib(events, R_ClassSymbol);
      List rxLst = cls.attr(".rxode2.lst");
      rxLst.attr("class") = R_NilValue;
      nid = rxLst[RxTrans_nid];
      if (nid <= 1) {
      } else if (nSub <= 1) {
        nSub = nid;
      } else if ((nSub*nStud) % nid != 0) {
        rxSolveFree();
        UNPROTECT(pro);
        stop(_("provided multi-subject data (n=%d) trying to simulate a different number of subjects (n=%d)"),
             nid, nSub*nStud);
      }
      rxModelsAssign(".nestEta",    R_NilValue);
      rxModelsAssign(".nestTheta",  R_NilValue);
    }
    allNames = as<CharacterVector>(PROTECT(lotriAllNames(omegaLotri))); pro++;
    methodStr = as<std::string>(control[Rxc_omegaSeparation]);
    methodInt = getMethodInt(methodStr, allNames, et);

    if (Rf_isNull(lotriAbove)) {
      aboveSEXP = R_NilValue;
      rxModelsAssign(".nestEvents", events);
      rxModelsAssign(".nestEta",    R_NilValue);
      rxModelsAssign(".nestTheta",  R_NilValue);
    }

    if (!Rf_isNull(aboveSEXP)) {
      // Create an extra theta matrix list
      //
      // Note this is for between study variability and the
      // method/specification comes from omega, so methodInt comes from
      // omegaSeparation

      SEXP thetaList = PROTECT(cvPost_(et, lotriAbove,
                                       nStudS,
                                       LogicalVector::create(false),
                                       LogicalVector::create(false),
                                       IntegerVector::create(methodInt),
                                       control[Rxc_omegaXform])); pro++;
      if (Rf_length(thetaList) >= 1 &&
          asDouble(lotriMaxNu(lotriAbove), "lotriMaxNu(lotriAbove)") > 1.0) {
        rxModelsAssign(".thetaL", thetaList);
      } else {
        rxModelsAssign(".thetaL", R_NilValue);
      }
      List bounds = PROTECT(lotriGetBounds(lotriAbove, R_NilValue, R_NilValue)); pro++;
      NumericVector upper = bounds[0];
      NumericVector lower = bounds[1];
      // With
      NumericMatrix aboveMat = rxRmvnSEXP(IntegerVector::create(1),
                                          R_NilValue, thetaList,
                                          upper, lower, // lower upper
                                          control[Rxc_nCoresRV],
                                          LogicalVector::create(false), // isChol
                                          LogicalVector::create(true), // keepNames
                                          NumericVector::create(0.4), // a
                                          NumericVector::create(2.05), // tol
                                          NumericVector::create(1e-10), // nlTol
                                          IntegerVector::create(100)); // nlMaxiter
      DataFrame newLst = as<DataFrame>(aboveMat);
      if (!Rf_isNull(et) && Rf_length(et) != 0){
        CharacterVector etListNames = asCv(Rf_getAttrib(et, R_NamesSymbol), "names(et)");
        int baseSize = et.size();
        List etFinal(baseSize + newLst.size());
        CharacterVector etFinalNames(baseSize + newLst.size());
        CharacterVector newLstNames = newLst.names();
        for (int j = baseSize; j--;){
          etFinalNames[j] = etListNames[j];
          etFinal[j] = et[j];
        }
        for (int j = newLst.size(); j--;) {
          etFinalNames[baseSize+j] = newLstNames[j];
          etFinal[baseSize+j] = newLst[j];
        }
        Rf_setAttrib(etFinal, R_NamesSymbol, etFinalNames);
        Rf_setAttrib(etFinal, R_RowNamesSymbol,
                     IntegerVector::create(NA_INTEGER, -nStud));
        Rf_setAttrib(etFinal, R_ClassSymbol,
                     CharacterVector::create("data.frame"));
        et = etFinal;
      }
    } else {
      rxModelsAssign(".thetaL", R_NilValue);
    }
    if (Rf_isNull(lotriBelow)) {
      belowSEXP = R_NilValue;
    }
    if (!Rf_isNull(belowSEXP)) {
      // below to sample matrix
      SEXP omegaList = PROTECT(cvPost_(et, // In case needed
                                       lotriBelow,
                                       nStudS,
                                       LogicalVector::create(false),
                                       LogicalVector::create(false),
                                       IntegerVector::create(methodInt),
                                       control[Rxc_omegaXform])); pro++;
      if (Rf_length(omegaList) >= 1 &&
          asDouble(lotriMaxNu(lotriBelow), "lotriMaxNu(lotriBelow)") > 1.0) {
        rxModelsAssign(".omegaL", omegaList);
      } else {
        rxModelsAssign(".omegaL", R_NilValue);
      }
      List bounds = PROTECT(lotriGetBounds(lotriBelow, R_NilValue, R_NilValue)); pro++;
      NumericVector upper = bounds[0];
      NumericVector lower = bounds[1];
      NumericMatrix belowMat = rxRmvnSEXP(IntegerVector::create(nSub),
                                          R_NilValue, omegaList,
                                          upper, lower, // lower upper
                                          control[Rxc_nCoresRV],
                                          LogicalVector::create(false), // isChol
                                          LogicalVector::create(true), // keepNames
                                          NumericVector::create(0.4), // a
                                          NumericVector::create(2.05), // tol
                                          NumericVector::create(1e-10), // nlTol
                                          IntegerVector::create(100)); // nlMaxiter
      et = _cbindOme(wrap(et), belowMat, IntegerVector::create(nSub));
    } else {
      rxModelsAssign(".omegaL", R_NilValue);
      et = _cbindOme(wrap(et), R_NilValue, IntegerVector::create(nSub));
    }
  } else {
    rxModelsAssign(".omegaL", R_NilValue);
    rxModelsAssign(".thetaL", R_NilValue);
    et = _cbindOme(wrap(et), R_NilValue, IntegerVector::create(nSub));
  }
  SEXP sigmaS = PROTECT(control[Rxc_sigma]); pro++;
  SEXP sigmaLotri = R_NilValue;
  if (qtest(sigmaS, "M")) {
    RObject sigmaR = as<RObject>(sigmaS);
    RObject dimnames = Rf_getAttrib(sigmaR, R_DimNamesSymbol);
    qstrictSdn(sigmaS, "sigma");
    SEXP sigmaIsCholS = PROTECT(control[Rxc_sigmaIsChol]); pro++;
    qassertS(sigmaIsCholS, "b1", "sigma");
    bool sigmaIsChol = as<bool>(sigmaIsCholS);
    arma::mat sigma = as<arma::mat>(sigmaS);
    if (sigmaIsChol) {
      sigma = sigma * sigma.t();
    }
    if (sigma.is_zero()){
      setZeroMatrix(3);
    } else if (!sigma.is_sympd()){
      rxSolveFree();
      stop(_("'sigma' must be symmetric, positive definite"));
    }
    // Convert to a lotri matrix
    SEXP sigmaPre = PROTECT(wrap(sigma)); pro++;
    Rf_setAttrib(sigmaPre, R_DimNamesSymbol, as<SEXP>(dimnames));
    sigmaLotri = PROTECT(asLotriMat(sigmaPre,
                                    as<SEXP>(List::create(_["lower"] = control[Rxc_sigmaLower],
                                                          _["upper"] = control[Rxc_sigmaUpper],
                                                          _["nu"]    = control[Rxc_dfObs])),
                                    as<SEXP>(CharacterVector::create("id")))); pro++;
  } else if (isLotri(sigmaS)) {
    sigmaLotri = sigmaS;
  } else if (!Rf_isNull(sigmaS)){
    rxSolveFree();
    UNPROTECT(pro);
    stop(_("'sigma' needs to be a matrix or lotri matrix"));
  }
  if (!Rf_isNull(sigmaS)) {
    allNames = as<CharacterVector>(PROTECT(lotriAllNames(sigmaLotri))); pro++;
    methodStr = as<std::string>(control[Rxc_sigmaSeparation]);
    methodInt = getMethodInt(methodStr, allNames, et);
    SEXP sigmaList = PROTECT(cvPost_(et, // In case needed
                                     sigmaLotri,
                                     nStudS,
                                     LogicalVector::create(false),
                                     LogicalVector::create(false),
                                     IntegerVector::create(methodInt),
                                     control[Rxc_omegaXform])); pro++;
    // To get the right number of sigma observations to match the potential request
    // expand the events to the translated events
    if (Rf_isNull(events)) {
      events = PROTECT(etTrans(as<List>(eventsS), nestObj,
                               (INTEGER(mv[RxMv_flags])[RxMvFlag_hasCmt] == 1),
                               false, false, true, R_NilValue,
                               control[Rxc_keepF],
                               control[Rxc_addlKeepsCov],
                               control[Rxc_addlDropSs],
                               control[Rxc_ssAtDoseTime],
                               control[Rxc_iCov])); pro++;
      rxModelsAssign(".nestEvents", events);
    } else if (!Rf_inherits(events, "rxEtTrans")){
      events = PROTECT(etTrans(as<List>(events), nestObj,
                               (INTEGER(mv[RxMv_flags])[RxMvFlag_hasCmt] == 1),
                               false, false, true, R_NilValue,
                               control[Rxc_keepF],
                               control[Rxc_addlKeepsCov],
                               control[Rxc_addlDropSs],
                               control[Rxc_ssAtDoseTime],
                               control[Rxc_iCov])); pro++;
      rxModelsAssign(".nestEvents", events);
    }
    int nobs =  Rf_length(VECTOR_ELT(events, 0));
    IntegerVector n2(1);
    if (nid == 1) {
      n2[0] = nobs*nSub;
    } else {
      n2[0] = nobs;
    }
    List bounds = PROTECT(lotriGetBounds(sigmaLotri, R_NilValue, R_NilValue)); pro++;
    NumericVector upper = bounds[0];
    NumericVector lower = bounds[1];
    SEXP sigmaMat = rxRmvnSEXP(n2, R_NilValue, sigmaList,
                               upper, lower, // lower upper
                               control[Rxc_nCoresRV],
                               LogicalVector::create(false), // isChol
                               LogicalVector::create(true), // keepNames
                               NumericVector::create(0.4), // a
                               NumericVector::create(2.05), // tol
                               NumericVector::create(1e-10), // nlTol
                               IntegerVector::create(100)); // nlMaxiter
    if (Rf_length(sigmaList) >= 1 &&
        asDouble(lotriMaxNu(sigmaLotri), "lotriMaxNu(sigmaLotri)") > 1.0) {
      rxModelsAssign(".sigmaL", sigmaList);
    } else {
      rxModelsAssign(".sigmaL", R_NilValue);
    }
    rxModelsAssign(".sigma", sigmaMat);
    // Now add 0 value sigma columns to et
    // This is required as a placeholder for the random EPS values
    // Add blank columns for sigma values
    RObject sigmaR = as<RObject>(sigmaS);
    SEXP dimnames0 = Rf_getAttrib(sigmaS, R_DimNamesSymbol);
    CharacterVector dimnames;
    if (Rf_isNull(VECTOR_ELT(dimnames0, 0))) {
      dimnames = VECTOR_ELT(dimnames0, 1);
    } else {
      dimnames = VECTOR_ELT(dimnames0, 0);
    }
    int base = Rf_length(et);
    List et2(base + dimnames.size());
    CharacterVector et2n(base + dimnames.size());
    CharacterVector etn = Rf_getAttrib(et, R_NamesSymbol);
    for (int i = base; i--; ) {
      et2[i] = VECTOR_ELT(et, i);
      et2n[i] = etn[i];
    }
    int nrow = Rf_length(VECTOR_ELT(et, 0));
    for (int i = dimnames.size(); i--;) {
      NumericVector cur(nrow);
      std::fill(cur.begin(), cur.end(), 0.0);
      et2[base+i] = cur;
      et2n[base+i] = dimnames[i];
    }
    et2.names() = et2n;
    et2.attr("class") = CharacterVector::create("data.frame");
    et2.attr("row.names") = IntegerVector::create(NA_INTEGER, -nrow);
    et = wrap(et2);
  } else {
    rxModelsAssign(".sigmaL", R_NilValue);
    rxModelsAssign(".sigma", R_NilValue);
  }
  UNPROTECT(pro);
  return et;
}

int factor2( IntegerVector col, IntegerVector id) {
  IntegerVector x1(id.size());
  for (int i = id.size(); i--;) {
    x1[i] = (col[i]+id[i])*(col[i]+id[i]+1)/2+id[i];
  }
  return get_sexp_uniqueL(x1);
}


SEXP nestingInfoSingle_(SEXP col, IntegerVector id) {
  SEXP f2 = PROTECT(convertId_(col));
  int l1 = factor2(f2, id);
  int lid = Rf_length(Rf_getAttrib(id, R_LevelsSymbol));
  if (l1 == lid) {
    // Case:
    //  study id
    //  1     1
    //  1     2
    //  1     3
    //  2     4
    //  2     5
    //
    // The factor(paste(study,id)) will have the same number of levels
    //  as factor(paste(id))
    UNPROTECT(1);
    return f2;
  } else if (l1 > lid) {
    // Case:
    //  id  occ
    //  1     1
    //  1     2
    //  1     3
    //  2     1
    //  2     2
    //
    // The factor(paste(occ,id)) will have more levels than
    // factor(paste(id))
    Rf_setAttrib(f2, Rf_install("nu"), wrap(IntegerVector::create(l1)));
    UNPROTECT(1);
    return f2;
  } else {
    rxSolveFree();
    stop(_("un-handled nesting information"));
  }
}

//[[Rcpp::export]]
SEXP nestingInfo_(SEXP omega, List data) {
  // Might need to clone...
  int pro = 0;
  CharacterVector lName = data.names();
  int wid = -1;
  std::string tmpS;
  std::string idName;
  for (int i = 0; i < lName.size(); ++i){
    tmpS = as<std::string>(lName[i]);
    std::transform(tmpS.begin(), tmpS.end(), tmpS.begin(), ::tolower);
    if (tmpS == "id"){
      idName=as<std::string>(lName[i]);
      wid = i;
      break;
    }
  }
  if (wid == -1){
    rxSolveFree();
    UNPROTECT(pro);
    stop(_("cannot find 'id' column in dataset"));
  }
  SEXP idS = PROTECT(data[wid]); pro++;
  SEXP id = PROTECT(convertId_(idS)); pro++;
  SEXP lotriOmega = R_NilValue;
  if (Rf_isNewList(omega)){
    lotriOmega = omega;
  } else if (Rf_isMatrix(omega)) {
    lotriOmega = PROTECT(asLotriMat(omega, R_NilValue,
                                    wrap(CharacterVector::create(idName)))); pro++;
  } else {
    rxSolveFree();
    stop(_("'omega' must be a list/lotri/matrix"));
  }
  SEXP lvls = Rf_getAttrib(lotriOmega, R_NamesSymbol);
  int nlvl = Rf_length(lvls);
  List aboveVars0(nlvl-1);
  List belowVars0(nlvl-1);
  IntegerVector below(nlvl-1);
  CharacterVector belowN(nlvl-1);
  IntegerVector above(nlvl-1);
  CharacterVector aboveN(nlvl-1);
  SEXP lvl;
  int extraTheta = 0;
  int extraEta = 0;
  SEXP s = R_NilValue, dn = R_NilValue;
  SEXP NuSymbol = PROTECT(Rf_install("nu"));pro++;
  int aboveI=0;
  int belowI=0;
  int l1=0;
  SEXP idChar = PROTECT(Rf_mkChar("id")); pro++;
  SEXP lNameSEXP=wrap(lName);
  for (int i = 0; i < nlvl; ++i) {
    lvl = STRING_ELT(lvls, i);
    if (lvl != idChar) {
      int found=-1;
      for (int j = lName.size(); j--;){
        if (STRING_ELT(lNameSEXP,j) == lvl){
          found = j;
          break;
        }
      }
      if (found == -1) stop("could not find '%s' in data", CHAR(lvl));
      s = nestingInfoSingle_(data[found], id);
      l1 = Rf_length(Rf_getAttrib(s, R_LevelsSymbol));
      dn = VECTOR_ELT(Rf_getAttrib(VECTOR_ELT(lotriOmega, i),
                                   R_DimNamesSymbol), 0);
      SEXP nuSEXP = Rf_getAttrib(s, NuSymbol);
      if (Rf_isNull(nuSEXP)) {
        aboveVars0[aboveI] = dn;
        above[aboveI] = l1;
        aboveN[aboveI++] = lvl;
        extraTheta += Rf_length(dn) * l1;
      } else {
        belowVars0[belowI] = dn;
        below[belowI] = l1;
        belowN[belowI++] = lvl;
        extraEta += Rf_length(dn) * l1;
      }
      data[found] = s;
    }
  }
  IntegerVector belowF(belowI);
  CharacterVector belowFN(belowI);
  List belowVars(belowI);
  for (int i = belowI; i--;) {
    belowF[i] = below[i];
    belowFN[i] = belowN[i];
    belowVars[i] = belowVars0[i];
  }
  belowF.names() = belowFN;
  belowVars.names() = belowFN;
  IntegerVector aboveF(aboveI);
  CharacterVector aboveFN(aboveI);
  List aboveVars(aboveI);
  for (int i = aboveI; i--;) {
    aboveF[i] = above[i];
    aboveFN[i] = aboveN[i];
    aboveVars[i] = aboveVars0[i];
  }
  aboveF.names() = aboveFN;
  aboveVars.names() = aboveFN;
  UNPROTECT(pro);
  return wrap(List::create(_["data"]=data,
                           _["omega"]=lotriOmega,
                           _["idName"]=CharacterVector::create(idName),
                           _["id"]=id,
                           _["above"]=aboveF,
                           _["below"]=belowF,
                           _["aboveVars"]=aboveVars,
                           _["belowVars"]=belowVars,
                           _["extraTheta"]=extraTheta,
                           _["extraEta"]=extraEta));
}

//[[Rcpp::export]]
RObject swapMatListWithCube_(RObject inO) {
  Rcpp::List omegaList;
  if (inO.hasAttribute("dimnames")) {
    Rcpp::List dimNamesFinal(2);
    Rcpp::List dimNamesCur = inO.attr("dimnames");
    dimNamesFinal[0] = dimNamesCur[0];
    dimNamesFinal[1] = dimNamesCur[1];
    arma::cube omegaCube = as<arma::cube>(inO);
    Rcpp::List omegaList(omegaCube.n_slices);
    for (int i = 0; i < omegaList.size(); ++i) {
      RObject cur = wrap(omegaCube.slice(i));
      cur.attr("dimnames") = dimNamesFinal;
      omegaList[i] = cur;
    }
    return wrap(omegaList);
  } else {
    Rcpp::List omegaList = as<Rcpp::List>(inO);
    RObject ro0 = omegaList[0];
    arma::mat n0 = as<arma::mat>(omegaList[0]);
    Rcpp::List dimNamesFinal(3);
    bool hasDim = false;
    if (ro0.hasAttribute("dimnames")) {
      Rcpp::List dimNamesCur = ro0.attr("dimnames");
      if (dimNamesCur.size() == 2) {
        dimNamesFinal[0] = dimNamesCur[0];
        dimNamesFinal[1] = dimNamesCur[1];
        dimNamesFinal[2] = R_NilValue;
      }
      hasDim = true;
    }
    arma::cube omegaCube(n0.n_rows, n0.n_cols, omegaList.size());
    for (int i = 0; i < omegaList.size(); ++i) {
      omegaCube.slice(i) = as<arma::mat>(omegaList[i]);
    }
    RObject cur = wrap(omegaCube);
    if (hasDim) cur.attr("dimnames") = dimNamesFinal;
    return cur;
  }
  return R_NilValue;
}

//[[Rcpp::export]]
Rcpp::List omegaListRse(RObject omegaIn) {
  arma::cube omegaCube;
  Rcpp::List omegaList;
  bool useList = false;
  unsigned int ntot=0;
  if (omegaIn.hasAttribute("dim")) {
    omegaCube = as<arma::cube>(omegaIn);
    ntot = omegaCube.n_slices;
  } else {
    omegaList = as<Rcpp::List>(omegaIn);
    useList = true;
    ntot = omegaList.size();
  }
  arma::mat oldM;
  if (useList) {
    oldM = as<arma::mat>(omegaList[0]);
  } else {
    oldM = omegaCube.slice(0);
  }
  arma::mat newM = oldM;
  arma::mat oldS(oldM.n_rows, oldM.n_rows, arma::fill::zeros);
  arma::mat newS = oldS;
  int m = 1;
  for (unsigned int i = 1; i < ntot; i++) {
    m++;
    arma::mat x;
    if (useList) {
      x = as<arma::mat>(omegaList[i]);
    } else {
      x = omegaCube.slice(i);
    }
    newM = oldM + (x-oldM)/m;
    newS = oldS + (x-oldM)*(x-newM);
    oldM = newM;
    oldS = newS;
  }
  arma::mat var = newS/(m-1);
  arma::mat sd = sqrt(var);
  return List::create(_["mean"] = wrap(newM),
                      _["var"] = wrap(var),
                      _["sd"] = wrap(sd),
                      _["rse"]= wrap(sd/newM));
}
