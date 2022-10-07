#define USE_FC_LEN_T
// [[Rcpp::interfaces(r,cpp)]]
// [[Rcpp::depends(RcppArmadillo)]]
//#undef NDEBUG
#define STRICT_R_HEADER
#include <stdarg.h>
#include <RcppArmadillo.h>
#include <R.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif
using namespace Rcpp;
using namespace R;
using namespace arma;
extern "C" SEXP _rxCholInv(SEXP dms, SEXP theta, SEXP tn);

//' Invert matrix using RcppArmadillo.  
//'
//' @param matrix matrix to be inverted.
//' 
//' @return inverse or pseudo inverse of matrix.
//' 
//' @export
// [[Rcpp::export]]
NumericVector rxInv(SEXP matrix){
  mat smatrix= as<mat>(matrix);
  mat imat;
  bool success;
  success = inv(imat, smatrix);
  if (!success){
    imat = pinv(smatrix);
    Rprintf(_("matrix seems singular; Using pseudo-inverse\n"));
  }
  NumericVector ret;
  ret = wrap(imat);
  return(ret);
}

arma::mat rxToCholOmega(arma::mat cholMat){
  // Only the cholesky is needed for the liklihood calculation
  // trimatu is faster, but it seems to have problems sometimes with certain BLAS combinations:
  // See https://github.com/nlmixrdevelopment/rxode2/issues/84
    // Only the cholesky is needed for the liklihood calculation
  // trimatu is faster, but it seems to have problems sometimes with certain BLAS combinations:
  // See https://github.com/nlmixrdevelopment/rxode2/issues/84
  arma::mat cholO;
  bool success;
  try {
    success = inv(cholO, trimatu(cholMat));
    if (success) return cholO;
    success = inv(cholO, cholMat);
    if (success) return cholO;
    stop(_("can not invert in 'rxToCholOmega'"));
  } catch (...) {
    success = inv(cholO, cholMat);
    if (success) return cholO;
    stop(_("can not invert in 'rxToCholOmega'"));
  }
  // should not get here.
  return cholO;
}

//' Get Omega^-1 and derivatives
//'
//' @param invObjOrMatrix Object for inverse-type calculations.  If
//'   this is a matrix, setup the object for inversion
//'   [rxSymInvCholCreate()] with the default arguments and return a
//'   reactive s3 object.  Otherwise, use the inversion object to
//'   calculate the requested derivative/inverse.
//' 
//' @param theta Thetas to be used for calculation.  If missing (`NULL`), a
//'     special s3 class is created and returned to access `Omega^1`
//'     objects as needed and cache them based on the theta that is
//'     used.
//' 
//' @param type The type of object.  Currently the following types are
//'     supported:
//' 
//' * `cholOmegaInv` gives the
//'     Cholesky decomposition of the Omega Inverse matrix.
//' * `omegaInv` gives the Omega Inverse matrix.
//' * `d(omegaInv)` gives the `d(Omega^-1)` withe respect to the
//'     theta parameter specified in `thetaNumber`.
//' * `d(D)` gives the `d(diagonal(Omega^-1))` with respect to
//'     the theta parameter specified in the `thetaNumber`
//'     parameter
//' 
//' @param thetaNumber For types `d(omegaInv)` and `d(D)`,
//'     the theta number that the derivative is taken against.  This
//'     must be positive from 1 to the number of thetas defining the
//'     Omega matrix.
//' 
//' @return Matrix based on parameters or environment with all the
//'     matrixes calculated in variables `omega`, `omegaInv`, `dOmega`,
//'     `dOmegaInv`.
//' 
//' @author Matthew L. Fidler
//' 
//' @export
// [[Rcpp::export]]
RObject rxSymInvChol(RObject invObjOrMatrix, Nullable<NumericVector> theta = R_NilValue, std::string type = "cholOmegaInv", int thetaNumber = 0){
  if (invObjOrMatrix.isObject()){
    List invObj  = as<List>(invObjOrMatrix);
    if (theta.isNull()){
      // Missing theta
      Environment base = R_BaseEnv;
      Function newEnv = as<Function>(base["new.env"]);
      Environment e = newEnv(_["parent"] = R_EmptyEnv);
      e["invobj"] = invObj;
      List ret = Rcpp::List::create(Rcpp::Named("env")=e);
      ret.attr("class") = "rxSymInvCholEnv";
      return ret;
    } else {
      NumericVector par(theta);
      int tn = thetaNumber;
      if (type == "xType"){
	tn = NA_INTEGER;
      } else if (type == "cholOmegaInv"){
        tn = 0;
      } else if (type == "omegaInv"){
        tn = -1;
      } else if (type == "d(omegaInv)"){
        if (tn <= 0){
          stop(_("theta number must be positive for 'd(omegaInv)'"));
        }
      } else if (type == "d(D)"){
        if (tn <= 0){
          stop(_("theta number must be positive for 'd(D)'"));
        }
        tn = -2 - tn;
      } else if (type == "ntheta"){
        tn = -2;
      }
      // try {
        Function fn = as<Function>(invObj["fn"]);
        return fn(par, tn);
      // } catch (...) {
      // }
    }
  } else  {
    Environment rxode2("package:rxode2");
    Function rxSymInvCholCreate = as<Function>(rxode2["rxSymInvCholCreate"]);
    return rxSymInvChol(rxSymInvCholCreate(invObjOrMatrix), R_NilValue, "cholOmegaInv", 0);
  }
  return R_NilValue;
}

// [[Rcpp::export]]
RObject rxSymInvCholEnvCalculate(List obj, std::string what, Nullable<NumericVector> theta = R_NilValue){
  Environment e = as<Environment>(obj["env"]);
  if (theta.isNull()){
    if (e.exists(what)){
      return e[what];
    } else if (what == "theta"){
      return R_NilValue;
    } else {
      List invObj;
      if (e.exists("invobj")){
        invObj = as<List>(e["invobj"]);
      } else {
        stop(_("error in 'rxSymInvCholEnvCalculate' environment"));
      }
      if (what == "xType"){
	e["xType"] = rxSymInvChol(invObj,NumericVector::create(1),"xType",0);
      }
      if (what == "ntheta"){
        e["ntheta"] = rxSymInvChol(invObj,NumericVector::create(1),"ntheta",0);
        return(e["ntheta"]);
      }
      NumericVector theta;
      if (e.exists("theta")){
        theta = as<NumericVector>(e["theta"]);
      } else {
        stop(_("theta for omega calculations not setup yet"));
      }
      int ntheta = theta.size(), i=0;
      if (what == "theta.diag"){
	Function fn = as<Function>(invObj["fn"]);
	e["theta.diag"] = fn(R_NilValue, R_NilValue);
      } else if (what == "chol.omegaInv"){
        e["chol.omegaInv"]=as<NumericMatrix>(rxSymInvChol(invObj, theta, "cholOmegaInv"));
      } else if (what == "omegaInv"){
        e["omegaInv"]= as<NumericMatrix>(rxSymInvChol(invObj, theta, "omegaInv",-1));
      } else if (what == "d.omegaInv"){
        List ret(ntheta);
        for (i = ntheta; i--; ){
          ret[i] = as<NumericMatrix>(rxSymInvChol(invObj, theta, "d(omegaInv)",i+1));
        }
        e["d.omegaInv"] = ret;
      } else if (what == "d.D.omegaInv"){
        List ret(ntheta);
        for (i = ntheta; i--; ){
          ret[i] = as<NumericVector>(rxSymInvChol(invObj, theta, "d(D)",i+1));
        }
        e["d.D.omegaInv"] = ret;
      } else if (what == "chol.omega1"){
        rxSymInvCholEnvCalculate(obj, "chol.omegaInv", R_NilValue);
        arma::mat ret = rxToCholOmega(as<arma::mat>(e["chol.omegaInv"]));
        e["chol.omega1"] = ret; 
      } else if (what == "omega"){
        rxSymInvCholEnvCalculate(obj, "chol.omega1", R_NilValue);
        arma::mat U1 = as<mat>(e["chol.omega1"]);
        arma::mat omega = U1*trans(U1);
        e["omega"] = omega;
      } else if (what == "chol.omega"){
	rxSymInvCholEnvCalculate(obj, "omega", R_NilValue);
        arma::mat omega = as<mat>(e["omega"]);
        e["chol.omega"] = chol(omega);
      } else if (what == "log.det.OMGAinv.5"){
        // Note this does NOT include the 2 pi bit
        rxSymInvCholEnvCalculate(obj,"chol.omegaInv", R_NilValue);
        arma::mat c = as<arma::mat>(e["chol.omegaInv"]);
        arma::vec diag = c.diag();
        arma::vec ldiag = log(diag);
        NumericVector ret = as<NumericVector>(wrap(sum(ldiag)));
        e["log.det.OMGAinv.5"] = ret;
      } else if (what == "tr.28"){
	// 1/2*tr(d(Omega^-1)*Omega);
        rxSymInvCholEnvCalculate(obj,"d.omegaInv", R_NilValue);
        rxSymInvCholEnvCalculate(obj,"omega", R_NilValue);
	List dOmegaInv = as<List>(e["d.omegaInv"]);
	arma::mat omega = as<arma::mat>(e["omega"]);
	NumericVector tr28(dOmegaInv.size());
	arma::mat cur;
        arma::vec diag;
	for (i = tr28.size();i--;){
	  cur = as<arma::mat>(dOmegaInv[i]) * omega;
	  diag = cur.diag();
	  tr28[i] = 0.5*sum(diag);
	}
	e["tr.28"] = tr28;
      } else if (what == "omega.47"){
	rxSymInvCholEnvCalculate(obj,"d.omegaInv", R_NilValue);
        rxSymInvCholEnvCalculate(obj,"chol.omegaInv", R_NilValue);
	unsigned int j;
	arma::mat cholO = as<arma::mat>(e["chol.omegaInv"]);
        int neta = cholO.n_rows;
        List dOmegaInv = as<List>(e["d.omegaInv"]);
        arma::mat cEta = zeros(neta,1);
        arma::mat c;
        List prod2(ntheta);
        for (i = dOmegaInv.size(); i--;){
	  c = as<arma::mat>(dOmegaInv[i]);
          List prodI(neta);
          for (j = neta; j--;){
            cEta(j,0) = 1;
            prodI[j] = c*cEta;
            cEta(j,0) = 0;
          }
          prod2[i] = prodI;
        }
	e["omega.47"] = prod2;
      }
      return e[what];
    }
  } else {
    if (what == "theta"){
      NumericVector par(theta);
      int ntheta = as<int>(rxSymInvCholEnvCalculate(obj, "ntheta", R_NilValue));
      if (par.size() == ntheta){
        // Clear cache with the exception of
        CharacterVector sym = e.ls(TRUE);
	// Clear the cache
        for (int i = 0; i < sym.size(); i++){
          if (sym[i] != "invobj" && sym[i] != "ntheta") {
            e.remove(as<std::string>(sym[i]));
          }
        }
        e["theta"] = par;
        return(obj);
      } else {
	stop(_("theta has to have %d elements"), ntheta);
      }
    } else {
      stop(_("Can only assign 'theta' in this environment"));
    }
  }
  return R_NilValue;
}
