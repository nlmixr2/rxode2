// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: t; -*-
#define USE_FC_LEN_T
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppArmadillo)]]
//#undef NDEBUG
#define STRICT_R_HEADER
#define NCMT 100
// NONMEM 7.1 has a max of 50 obesrrvations/individual
#define MAXIDS 500
#define NALL 500
#define NDOSES 50
//#define rxSolveT 1
// NONMEM nTHETA=20
// NONMEM nETA=30
// NONMEM nSIGMA=10

#define NPARS 60
#include <RcppArmadillo.h>
#include <Rmath.h>
#include <thread>
#include <string>
#include <vector>
#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>
#include <climits>
#include "checkmate.h"
#include <stdint.h>    // for uint64_t rather than unsigned long long
#include "../inst/include/rxode2.h"
#include "ode.h"
#define rxModelVars(a) rxModelVars_(a)
#define min2( a , b )  ( (a) < (b) ? (a) : (b) )
void resetSolveLinB();
using namespace Rcpp;
using namespace arma;

#include "cbindThetaOmega.h"
#include "handle_evid.h"
#include "rxThreadData.h"

extern "C" uint64_t dtwiddle(const void *p, int i);
extern "C" void calcNradix(int *nbyte, int *nradix, int *spare, uint64_t *maxD, uint64_t *minD);
extern "C" void RSprintf(const char *format, ...);
extern "C" int getRxThreads(const int64_t n, const bool throttle);
extern "C" double *global_InfusionRate(unsigned int mx);
extern "C" void rxOptionsFree();
extern "C" void rxOptionsIni();
extern "C" void rxOptionsIniEnsure(int mx);
extern "C" void parseFree(int last);
extern "C" void rxClearFuns();
extern "C" void rxFreeLast();
extern "C" void lineFree(vLines *sbb);
extern "C" void rxode2_assign_fn_pointers(SEXP);
extern "C" int getThrottle();
extern "C" void lineIni(vLines *sbb);
extern "C" void addLine(vLines *sbb, const char *format, ...);
extern "C" void seedEng(int ncores);
extern "C" int getRxThreads(const int64_t n, const bool throttle);
extern "C" void rxode2_assign_fn_pointers_(const char *mv);
extern "C" void setSilentErr(int silent);

bool useForder();
Function getForder();


// https://github.com/Rdatatable/data.table/blob/588e0725320eacc5d8fc296ee9da4967cee198af/src/forder.c#L193-L211
// range_d is modified because it DOES NOT count na/inf because rxode2 assumes times cannot be NA, NaN, -Inf, Inf
// Also can integrate with prior range information (like prior integer range)
static void range_d(double *x, int n, uint64_t *out_min, uint64_t *out_max)
// return range of finite numbers (excluding NA, NaN, -Inf, +Inf), a count of NA and a count of Inf|-Inf|NaN
{
  uint64_t min=*out_min, max=*out_max;
  int i=0;
  max = min = dtwiddle(x, i++);
  for(; i<n; i++) {
    uint64_t tmp = dtwiddle(x, i);
    if (tmp>max) max=tmp;
    else if (tmp<min) min=tmp;
  }
  *out_min = min;
  *out_max = max;
}

#include "../inst/include/rxode2_as.h"

SEXP qassertS(SEXP in, const char *test, const char *what);

RObject rxSolveFreeObj=R_NilValue;
LogicalVector rxSolveFree();

List etTrans(List inData, const RObject &obj, bool addCmt=false,
						 bool dropUnits=false, bool allTimeVar=false,
						 bool keepDosingOnly=false, Nullable<LogicalVector> combineDvid=R_NilValue,
						 CharacterVector keep = CharacterVector(0));
RObject et_(List input, List et__);
void setEvCur(RObject cur);

SEXP cvPost_(SEXP nuS, SEXP omega, SEXP n, SEXP omegaIsChol, SEXP returnChol,
						 SEXP type, SEXP diagXformType);

RObject rxUnlock(RObject obj);
RObject rxLock(RObject obj);


RObject setupOnlyObj = R_NilValue;

int rxcEvid  = -1;
int rxcTime  = -1;
int rxcAmt   = -1;
int rxcId    = -1;
int rxcDv    = -1;
int rxcLimit = -1;
int rxcCens  = -1;
int rxcLen   = -1;
int rxcIi    = -1;
bool resetCache = true;
bool rxHasEventNames(CharacterVector &nm){
  int len = nm.size();
  bool reset  = resetCache;
  if (reset || len != rxcLen){
    reset    = resetCache;
    rxcEvid  = -1;
    rxcTime  = -1;
    rxcAmt   = -1;
    rxcId    = -1;
    rxcDv    = -1;
    rxcIi    = -1;
    rxcLimit = -1;
    rxcCens  = -1;
    rxcLen   = len;
    for (unsigned int i = len; i--;){
      if (as<std::string>(nm[i]) == "evid" ||
					as<std::string>(nm[i]) == "EVID" ||
					as<std::string>(nm[i]) == "Evid"){
        rxcEvid = i;
      } else if (as<std::string>(nm[i]) == "time" ||
								 as<std::string>(nm[i]) == "TIME" ||
								 as<std::string>(nm[i]) == "Time"){
        rxcTime = i;
      } else if (as<std::string>(nm[i]) == "amt" ||
								 as<std::string>(nm[i]) == "AMT" ||
								 as<std::string>(nm[i]) == "Amt"){
        rxcAmt = i;
      } else if (as<std::string>(nm[i]) == "id" ||
								 as<std::string>(nm[i]) == "ID" ||
								 as<std::string>(nm[i]) == "Id"){
        rxcId = i;
      } else if (as<std::string>(nm[i]) == "dv" ||
								 as<std::string>(nm[i]) == "DV" ||
								 as<std::string>(nm[i]) == "Dv"){
        rxcDv = i;
      } else if (as<std::string>(nm[i]) == "ii" ||
								 as<std::string>(nm[i]) == "II" ||
								 as<std::string>(nm[i]) == "Ii"){
				rxcIi = i;
      } else if (as<std::string>(nm[i]) == "cens" ||
								 as<std::string>(nm[i]) == "CENS" ||
								 as<std::string>(nm[i]) == "Cens"){
				rxcCens = i;
      } else if (as<std::string>(nm[i]) == "limit" ||
								 as<std::string>(nm[i]) == "LIMIT" ||
								 as<std::string>(nm[i]) == "Limit"){
				rxcLimit = i;
      }
    }
  }
  resetCache = true;
  if (rxcTime >= 0){
    return true;
  } else {
    return false;
  }
}

bool rxIs_list(const RObject &obj, std::string cls){
  bool hasCls = obj.hasAttribute("class");
  if (hasCls){
    CharacterVector classattr = obj.attr("class");
    bool hasDf = false;
    bool hasEt = false;
    std::string cur;
    for (unsigned int i = classattr.size(); i--; ){
      cur = as<std::string>(classattr[i]);
      if (cur == cls) {
				if (cls == "rxEt") {
					List ce = as<List>(classattr.attr(".rxode2.lst"));
					List lobj = List(obj);
					int nobs = asInt(ce["nobs"], "nobs");
					int ndose = asInt(ce["ndose"], "ndose");
					if (lobj.size() != 12) {
						lobj.attr("class") = CharacterVector::create("data.frame");
						return false;
					}
					if ( (as<IntegerVector>(lobj[0])).size() != ndose + nobs) {
						lobj.attr("class") = CharacterVector::create("data.frame");
						return false;
					}
					return true;
				} else if (cls == "rxSolve") {
					Environment e = as<Environment>(classattr.attr(".rxode2.env"));
					List lobj = List(obj);
					CharacterVector cls2= CharacterVector::create("data.frame");
					if (as<int>(e[".check.ncol"]) != lobj.size()){
						lobj.attr("class") = cls2;
						return false;
					}
					int nrow = (as<NumericVector>(lobj[0])).size();
					if (as<int>(e[".check.nrow"]) != nrow){
						lobj.attr("class") = cls2;
						return false;
					}
					CharacterVector cn = CharacterVector(e[".check.names"]);
					if (cn.size() != lobj.size()){
						lobj.attr("class") = cls2;
						return false;
					}
					CharacterVector cn2 = CharacterVector(lobj.names());
					for (int j = 0; j < cn.size();j++){
						if (cn[j] != cn2[j]){
							lobj.attr("class") = cls2;
							return false;
						}
					}
					return true;
				} else {
					return true;
				}
      } else if (cur == "data.frame"){
				hasDf=true;
      }
    }
    rx_solve* rx = getRxSolve_();
    if (hasDf && (cls == "rx.event" || cls == "event.data.frame")){
      if (classattr[0] == "rxEtTran"){
				rxcEvid = 2;
				rxcTime = 1;
				rxcAmt  = 3;
				rxcId   = 0;
				rxcDv   = 5;
				rxcIi   = 4;
				List e = as<List>(classattr.attr(".rxode2.lst"));
				int censAdd = asInt(e[RxTrans_censAdd], "censAdd");
				int limitAdd = asInt(e[RxTrans_limitAdd], "limitAdd");
				rx->maxShift = asDouble(e[RxTrans_maxShift],"maxShift");
				if (censAdd == 1 && limitAdd == 1) {
					rxcCens = 6;
					rxcLimit = 7;
				} else if (censAdd == 1){
					rxcCens = 6;
					rxcLimit = -1;
				} else if (limitAdd == 1){
					rxcCens  = -1;
					rxcLimit = 6;
				} else {
					rxcCens  = -1;
					rxcLimit = -1;
				}
				return true;
      } else {
				// Check for event.data.frame
				CharacterVector cv =as<CharacterVector>((as<DataFrame>(obj)).names());
				return rxHasEventNames(cv);
      }
    } else if (hasEt) {
      return (cls == "rx.event");
    } else {
      return false;
    }
  } else {
    return (cls == "list");
  }
}

bool rxDropB = false;

List rxDrop(CharacterVector drop, List input, bool warnDrop) {
  rxDropB=false;
  CharacterVector inNames = input.attr("names");
  std::vector<int> keepI;
  int ndrop=0;
  for (unsigned int i = 0; i < inNames.size(); ++i) {
    std::string curName = as<std::string>(inNames[i]);
    bool dropCur = false;
    for (int j = drop.size();j--;){
      if (as<std::string>(drop[j]) == curName){
        dropCur = true;
        break;
      }
    }
    if (dropCur) ndrop++;
    else keepI.push_back(i);
    if (dropCur && !rxDropB && i < 10){
      if (curName == "time" ||
          curName == "sim.id" ||
          curName == "id") {
        rxDropB=true;
      }
    }
  }
  if (warnDrop && ndrop != drop.size()) {
    warning(_("column(s) in 'drop' were not in solved data"));
  }
  List ret(keepI.size());
  CharacterVector retN(keepI.size());
  for (unsigned int i = keepI.size(); i--;){
    ret[i] = input[keepI[i]];
    retN[i] = inNames[keepI[i]];
  }
  ret.attr("names") = retN;
  ret.attr("row.names")=input.attr("row.names");
  return ret;
}

//' Check the type of an object using Rcpp
//'
//' @param obj Object to check
//' @param cls Type of class.  Only s3 classes for lists/environments and primitive classes are checked.
//'    For matrix types they are distinguished as `numeric.matrix`, `integer.matrix`,
//'    `logical.matrix`, and `character.matrix` as well as the traditional `matrix`
//'    class. Additionally checks for `event.data.frame` which is an `data.frame` object
//'    with `time`,  `evid` and `amt`. (UPPER, lower or Title cases accepted)
//'
//' @return A boolean indicating if the object is a member of the class.
//'
//' @keywords internal
//'
//' @author Matthew L. Fidler
//'
//' @export
//'
// [[Rcpp::export]]
bool rxIs(const RObject &obj, std::string cls){
  if (obj == NULL) return false;
  if (cls == "units"){
    if (obj.hasAttribute("class")){
      CharacterVector cls = obj.attr("class");
      return as<std::string>(cls[0]) == "units";
    }
  }
  if (obj == NULL) return false;
  int type = obj.sexp_type();
  bool hasDim = false;
  bool hasCls = false;
  switch (type){
  case 0: return (cls == "NULL");
  case REALSXP:
    hasDim = obj.hasAttribute("dim");
    if (hasDim){
      if (cls == "event.matrix" || cls ==  "rx.event"){
        if (obj.hasAttribute("dimnames")){
          List dn = as<List>(obj.attr("dimnames"));
          if (dn.size() == 2){
            CharacterVector cv = as<CharacterVector>(dn[1]);
            return rxHasEventNames(cv);
          } else {
            return false;
          }
        } else {
          return false;
        }
      } else {
        return (cls == "matrix" || cls == "numeric.matrix");
      }
    } else {
      return (cls == "numeric");
    }
  case 13: // integer vectors
    // An integer vector cannot be an event matrix.
    hasDim = obj.hasAttribute("dim");
    if (hasDim){
      return (cls == "matrix" || cls == "integer.matrix");
    } else {
      if (cls == "integer") return true;
      hasCls = obj.hasAttribute("class");
      if (hasCls){
        CharacterVector classattr = obj.attr("class");
        if (as<std::string>(classattr[0]) == cls){
          return true;
        }
        return false;
      }
      return false;
    }
  case LGLSXP:
    hasDim = obj.hasAttribute("dim");
    if (hasDim){
      return (cls == "matrix" ||  cls == "logical.matrix");
    } else {
      return (cls == "logical");
    }
  case STRSXP:
    hasDim = obj.hasAttribute("dim");
    if (hasDim){
      return (cls == "matrix" || cls == "character.matrix");
    } else {
      return (cls == "character");
    }
  case VECSXP:
    return rxIs_list(obj, cls);
  case 4: // environment
    hasCls = obj.hasAttribute("class");
    if (hasCls){
      CharacterVector classattr = obj.attr("class");
      std::string cur;
      for (unsigned int i = classattr.size(); i--; ){
        cur = as<std::string>(classattr[i]);
        if (cur == cls) return true;
      }
    } else if (cls == "environment"){
      return true;
    }
    return false;
  case 22: // external pointer
    return (cls == "externalptr" || cls == "refObject");
  }
  return false;
}

Function loadNamespace("loadNamespace", R_BaseNamespace);

Environment cliNS = loadNamespace("cli");
Function cliAlert0 = as<Function>(cliNS["cli_alert_info"]);

extern "C" void cliAlert(const char *format, ...) {
  va_list args;
  char buffer[256];
  va_start(args, format);
  vsnprintf(buffer, 256, format, args);
  cliAlert0(wrap(buffer));
  va_end (args);
}

SEXP rxRmvn0(NumericMatrix& A_, arma::rowvec mu, arma::mat sigma,
						 arma::vec lower, arma::vec upper, int ncores=1, bool isChol=false,
						 double a=0.4, double tol = 2.05, double nlTol=1e-10, int nlMaxiter=100);

Function getRxFn(std::string name);
RObject rxSimSigma(const RObject &sigma,
                   const RObject &df,
                   int ncores,
                   const bool &isChol,
                   int nObs,
                   const bool checkNames = true,
                   NumericVector lowerIn =NumericVector::create(R_NegInf),
                   NumericVector upperIn = NumericVector::create(R_PosInf),
                   double a=0.4, double tol = 2.05, double nlTol=1e-10, int nlMaxiter=100){
  
  if (nObs < 1){
    rxSolveFree();
    stop(_("refusing to simulate %d items"),nObs);
  }
  NumericMatrix sigmaM;
  if (rxIs(sigma, "numeric.matrix")) {
    sigmaM = as<NumericMatrix>(sigma);
  } else if (rxIs(sigma, "numeric")) {
    int dim = sqrt(Rf_length(sigma));
    if (dim*dim == Rf_length(sigma)) {
      RObject sigma2 = sigma;
      sigma2.attr("dim") = IntegerVector::create(dim, dim);
      sigmaM = as<NumericMatrix>(sigma2);
    } else {
      rxSolveFree();
      stop(_("'sigma' is not a matrix"));
    }
  } else {
    rxSolveFree(); 
    stop(_("'sigma' is not a matrix"));
  }
  if (sigmaM.nrow() != sigmaM.ncol()){
    rxSolveFree();
    stop(_("matrix must be a square matrix"));
  }
  List dimnames;
  StringVector simNames;
  bool addNames = false;
  if (checkNames){
    if (!sigmaM.hasAttribute("dimnames")) {
      rxSolveFree();
      stop(_("matrix must have named dimensions (try 'lotri')"));
    }
    dimnames = sigmaM.attr("dimnames");
    simNames = as<StringVector>(dimnames[1]);
    addNames=true;
  } else if (sigmaM.hasAttribute("dimnames")){
    dimnames = sigmaM.attr("dimnames");
    simNames = as<StringVector>(dimnames[1]);
    addNames = true;
  }
  NumericMatrix simMat(nObs,sigmaM.ncol());
  arma::rowvec m(sigmaM.ncol(),arma::fill::zeros);
  NumericVector lower(sigmaM.ncol());
  NumericVector upper(sigmaM.ncol());
  if (lowerIn.hasAttribute("names")){
    // Named
    CharacterVector lowN = lowerIn.attr("names");
    for (int j = simNames.size(); j--;){
      lower[j] = R_NegInf;
      for (int k = lowN.size(); k--;){
        if (lowN[k] == simNames[j]){
          lower[j] = lowerIn[k];
          break;
        }
      }
    }
  } else if (lowerIn.size() == 1){
    std::fill_n(lower.begin(), lower.size(), lowerIn[0]);
  } else if (lowerIn.size() == lower.size()){
    lower = lowerIn;
  } else {
    rxSolveFree();
    stop(_("lower bounds needs to be a named vector, a single value or exactly the same size"));
  }
  if (upperIn.hasAttribute("names")){
    // Named
    CharacterVector upN = upperIn.attr("names");
    for (int j = simNames.size(); j--;){
      upper[j] = R_NegInf;
      for (int k = upN.size(); k--;){
        if (upN[k] == simNames[j]){
          upper[j] = upperIn[k];
          break;
        }
      }
    }
  } else if (upperIn.size() == 1){
    std::fill_n(upper.begin(), upper.size(), upperIn[0]);
  } else if (!upperIn.hasAttribute("names") && upperIn.size() == upper.size()){
    upper = upperIn;
  } else {
    rxSolveFree();
    stop(_("upper bounds needs to be a named vector, a single value or exactly the same size"));
  }

  // Ncores = 1?  Should it be parallelized when it can be...?
  // Note that if so, the number of cores also affects the output.
  // while (totSim < nObs){
  if (df.isNULL()){
    rxRmvn0(simMat, m, as<arma::mat>(sigmaM), as<arma::vec>(lower),
            as<arma::vec>(upper), ncores, isChol, a, tol, nlTol, nlMaxiter);
    // Function rmvn = as<Function>(mvnfast["rmvn"]);
    // rmvn(_["n"]=curSimN, _["mu"]=m, _["sigma"]=sigmaM, _["ncores"]=ncores,
    //      _["isChol"]=isChol, _["A"] = simMat0); // simMat is updated with the random deviates
  } else {
    double df2 = asDouble(df, "df");
    if (R_FINITE(df2)){
      rxSolveFree();
      stop(_("t distribution not yet supported"));
      // Function rmvt = as<Function>(mvnfast["rmvt"]);
      // rmvt(_["n"]=curSimN, _["mu"]=m, _["sigma"]=sigmaM, _["df"] = df,
      //      _["ncores"]=ncores, _["isChol"]=isChol, _["A"] = simMat0);
    } else {
      // Function rmvn = as<Function>(mvnfast["rmvn"]);
      // rmvn(_["n"]=curSimN, _["mu"]=m, _["sigma"]=sigmaM, _["ncores"]=ncores,
      //      _["isChol"]=isChol, _["A"] = simMat0);
      rxRmvn0(simMat, m, as<arma::mat>(sigmaM), as<arma::vec>(lower),
              as<arma::vec>(upper), ncores, isChol, a, tol, nlTol, nlMaxiter);
    }
  }
  if (addNames){
    simMat.attr("dimnames") = List::create(R_NilValue, simNames);
  }
  return wrap(simMat);
}

bool foundEnv = false;
Environment _rxModels;
bool _rxode2_found = false;
Environment _rxode2;

Environment rxode2env(){
  Function loadNamespace("loadNamespace", R_BaseNamespace);
  _rxode2 = loadNamespace("rxode2");
  _rxode2_found = true;
  return _rxode2;
}
// Export for C.
//[[Rcpp::export]]
Function getRxFn(std::string name){
  Environment rx = rxode2env();
  return as<Function>(rx[name]);
}

void getRxModels(){
  if (!foundEnv){ // minimize R call
    Function f = getRxFn("rxModels_");
    _rxModels = f();
    foundEnv = true;
  }
}

void rxModelsAssign(std::string str, SEXP assign){
  getRxModels();
  _rxModels[str] = assign;
}

//
inline bool fileExists(const std::string& name) {
  struct stat buffer;
  return (stat (name.c_str(), &buffer) == 0);
}

// Use this function to keep dynLoad options consistent.
//[[Rcpp::export]]
SEXP dynLoad(std::string dll){
  Function dl("dyn.load", R_BaseNamespace);
  SEXP ret = dl(dll, _["local"]=false, _["now"]=true);
  return ret;
}

List rxModelVars_(const RObject &obj); // model variables section

List rxModelVars_lastChance(const RObject &obj) {
	Function getMV = getRxFn("rxModelVarsS3");
	return getMV(obj);
}

List rxModelVars_rxode2(const RObject &obj){
  Environment e = asEnv(obj, "obj");
  List rxDll = asList(e["rxDll"], "e[\"rxDll\"]");
  List ret = asList(rxDll["modVars"], "rxDll[\"modVars\"]");
  RObject pkgR = e["package"];
  if (rxIsNull(pkgR)){
    return ret;
  } else {
    bool isV;
    Function isValid = e["isValid"];
    isV = asBool(isValid(), "$isValid()");
    if (isV){
      return ret;
    } else {
      std::string sobj = asStr(e["modName"], "e[\"modName\"]");
      if (sobj.find("_new")!=std::string::npos){
				return ret;
      }
      Function pkgLoaded = getRxFn(".rxPkgLoaded");
      isV = asBool(pkgLoaded(pkgR), ".rxPkgLoaded(pkgR)");
      if (isV){
				Environment ns;
				if (as<std::string>(pkgR) == "rxode2"){
					return ret;
				} else {
					ns=loadNamespace(pkgR);
				}
				if (ns.exists(".rxUpdated")){
					Environment rxU = ns[".rxUpdated"];
					if (rxU.exists(sobj)){
						e = rxU[sobj];
						rxDll = e["rxDll"];
						ret = rxDll["modVars"];
						return ret;
					} else {
						Function rxode2 = getRxFn("rxode2");
						e["modName"] = sobj + "_new"; // For rxode2 parsing add "new"
						Environment newRx = rxode2(e);
						e["modName"] = sobj; // Put back
						rxU[sobj] = newRx;
						return rxModelVars_(as<RObject>(newRx));
					}
				} else {
					return ret;
				}
      } else {
				return ret;
      }
    }
  }
}
List rxModelVars_blank(){
  List ret(20);
  CharacterVector retN(20);
  ret[0]  = CharacterVector::create(); // params
  retN[0] = "params";
  ret[1]  = CharacterVector::create(); // lhs
  retN[1] = "lhs";
  ret[2]  = CharacterVector::create(); // state
  retN[2] = "state";
  ret[3]  = CharacterVector::create(); // trans
  retN[3] = "trans";
  ret[4]  = CharacterVector::create(_["normModel"] = ""); // model
  retN[4] = "model";
  ret[5]  = NumericVector::create(); // ini
  retN[5] = "ini";
  ret[6]  = LogicalVector::create(); // dfdy
  retN[6] = "dfdy";
  ret[7]  = LogicalVector::create(); // sens
  retN[7] = "sens";
  ret[8] = IntegerVector::create(); // state.ignore
  retN[8] = "state.ignore";
  ret[9] = CharacterVector::create(); // version
  retN[9] = "version";
  ret[10] = CharacterVector::create(); // normal.state
  retN[10] = "normal.state";
  ret[11] = IntegerVector::create(0); // need sort
  retN[11] = "needSort";
  ret[12] = IntegerVector::create(0); // nMtime
  retN[12] = "nMtime";
  ret[13] = IntegerVector::create(0); // extraCmt
  retN[13] = "extraCmt";
  ret[14] = CharacterVector::create(); // stateExtra
  retN[14] = "stateExtra";
  ret[15] = IntegerVector::create(); // dvid
  retN[15] = "dvid";
  ret[16] = List::create();
  retN[16] = "indLin";
  ret[17] = IntegerVector::create(0); // timeId
  retN[17] = "timeId";
  ret[18] =CharacterVector::create(_["file_md5"] = "", _["parsed_md5"] = ""); // md5
  retN[18] = "md5";
  ret.attr("names") = retN;
  ret.attr("class") = "rxModelVars";
  return ret;
}

List rxModelVars_character(const RObject &obj){
  CharacterVector modList = asCv(obj, "rxModelVars_character(obj)");
  if (modList.size() == 1){
    std::string sobj =as<std::string>(obj);
    if (sobj == ""){
      // Blank rxode2 model
      return rxModelVars_blank();
    } else if (fileExists(sobj)){
      // From file
      Function f = getRxFn(".rxModelVarsCharacter");
      return f(obj);
    } else if ((sobj.find("=") == std::string::npos) &&
               (sobj.find("<-") == std::string::npos) &&
               (sobj.find("~") == std::string::npos)){
      if (_rxModels.exists(sobj)){
        RObject obj1 = _rxModels.get(sobj);
        if (rxIs(obj1, "rxModelVars")){
          return asList(obj1, "obj1");
        } else if (rxIs(obj1, "rxode2")){
          return rxModelVars_(obj1);
        }
      }
      std::string sobj1 = sobj + "_model_vars";
      if (_rxModels.exists(sobj1)){
        RObject obj1 = _rxModels.get(sobj1);
        if (rxIs(obj1, "rxModelVars")){
          return asList(obj1, "obj1");
        }
      }
      Function get("get",R_BaseNamespace);
      List platform = get(_["x"]=".Platform", _["envir"] = R_BaseEnv);
      sobj1 = sobj + "_" + as<std::string>(platform["r_arch"]) + "_model_vars";
      if (_rxModels.exists(sobj1)){
        RObject obj1 = _rxModels.get(sobj1);
        if (rxIs(obj1, "rxModelVars")){
          return asList(obj1, "obj1");
        }
      }
      Function filePath("file.path", R_BaseNamespace);
      Function wd("getwd", R_BaseNamespace);
      sobj1 = as<std::string>(wd());
      std::string sobj2 = sobj + ".d";
      std::string sobj3 = sobj + "_" + as<std::string>(platform["r_arch"]) +
        as<std::string>(platform["dynlib.ext"]);
      sobj1 = as<std::string>(filePath(sobj1,sobj2, sobj3));
      if (fileExists(sobj1)){
        Rcout << "Path: " << sobj1 << "\n";
        dynLoad(sobj1);
        sobj1 = sobj + "_" + as<std::string>(platform["r_arch"]) +
          "_model_vars";
        Function call(".Call", R_BaseNamespace);
        List ret = asList(call(sobj1), "call(sobj1)");
        return ret;
      }
    }
  } else if (modList.hasAttribute("names")){
    bool containsPrefix = false;
    CharacterVector modListNames = modList.names();
    for (unsigned int i = 0; i < modListNames.size(); i++){
      if (modListNames[i] == "prefix"){
        containsPrefix=true;
        break;
      }
    }
    if (containsPrefix){
      std::string mvstr = asStr(modList["prefix"], "modList[\"prefix\"]") +
        "model_vars";
      if(_rxModels.exists(mvstr)){
        RObject obj1 = _rxModels.get(mvstr);
        if (rxIs(obj1, "rxModelVars")){
          return asList(obj1, "obj1");
        }
      }
    }
  }
  if (obj == NULL) {
    return rxModelVars_blank();
  }
  // fileExists(const std::string& name)
  Function f = getRxFn(".rxModelVarsCharacter");
  return f(obj);
}

List rxModelVars_list(const RObject &obj){
  bool params=false, lhs=false, state=false, trans=false, ini=false, model=false, md5=false, dfdy=false;
  List lobj  = asList(obj, "rxModelVars_list");
  CharacterVector nobj = lobj.names();
  for (unsigned int i = 0; i < nobj.size(); i++){
    if (nobj[i] == "modVars"){
      return(rxModelVars_(lobj["modVars"]));
    } else if (!params && nobj[i]== "params"){
      params=true;
    } else if (!lhs && nobj[i] == "lhs"){
      lhs=true;
    } else if (!state && nobj[i] == "state"){
      state=true;
    } else if (!trans && nobj[i] == "trans"){
			trans=true;
		} else if (!ini && nobj[i] == "ini"){
			ini = true;
		} else if (!model && nobj[i] == "model"){
			model = true;
		} else if (!md5 && nobj[i] == "md5"){
			md5 = true;
		} else if (!dfdy && nobj[i] == "dfdy"){
			dfdy = true;
		} else if (params && lhs && state && trans && ini && model && md5 && dfdy) {
			return lobj;
		}
	}
	return rxModelVars_lastChance(obj);
}

// [[Rcpp::export]]
List rxModelVars_(const RObject &obj){
  if (obj == NULL) return rxModelVars_blank();
  getRxModels();
  if (rxIs(obj, "rxModelVars")){
    List ret(obj);
    return ret;
  } else if (rxIs(obj,"rxode2")) {
    return rxModelVars_rxode2(obj);
	} else if (Rf_isFunction(obj)){
		Function getUi = getRxFn("assertRxUi");
		Environment e = asEnv(getUi(obj), "rxode2(obj)");
		List ret = asList(e["mv0"], "e[\"mv0\"]");
    return ret;
	} else if (rxIs(obj, "rxUi")){
		Environment e = asEnv(obj, "obj");
    List ret = asList(e["mv0"], "e[\"mv0\"]");
    return ret;
  } else if (rxIs(obj, "rxS")){
    Environment e = asEnv(obj, "obj");
    List ret = asList(e["..mv"], "e[\"..mv\"]");
    return ret;
  } else if (rxIs(obj,"rxSolve")){
    CharacterVector cls = obj.attr("class");
    Environment e = asEnv(cls.attr(".rxode2.env"), ".rxode2.env");
    return  rxModelVars_(as<RObject>(e[".args.object"]));
  } else if (rxIs(obj,"rxDll")){
    List lobj = (asList(obj, "obj"))["modVars"];
    return lobj;
  } else if (rxIs(obj, "environment")){
    Environment e = asEnv(obj, "obj");
    if (e.exists(".args.object")){
      return rxModelVars_(e[".args.object"]);
    } else {
			return rxModelVars_lastChance(obj);
    }
  } else if (rxIsChar(obj)){
    return rxModelVars_character(obj);
  } else if (rxIsList(obj)){
    return rxModelVars_list(obj);
  } else if (rxIsNull(obj)) {
    rxSolveFree();
    stop(_("a NULL object does not have any rxode2 model variables"));
  } else {
		return rxModelVars_lastChance(obj);
  }
}

//' State variables
//'
//' This returns the model's compartments or states.
//'
//' @inheritParams rxModelVars
//'
//' @param state is a string indicating the state or compartment that
//'     you would like to lookup.
//'
//' @return If state is missing, return a character vector of all the states.
//'
//' If state is a string, return the compartment number of the named state.
//'
//' @seealso [rxode2()]
//' @family Query model information
//' @author Matthew L.Fidler
//' @export
// [[Rcpp::export]]
RObject rxState(const RObject &obj = R_NilValue, RObject state = R_NilValue){
  List modVar = rxModelVars(obj);
  CharacterVector states = modVar["state"];
  if (state.isNULL()){
    return states;
  }
  else if (rxIsChar(state)){
    CharacterVector lookup = asCv(state, "state");
    if (lookup.size() > 1){
      // Fixme?
      rxSolveFree();
      stop(_("can only lookup one state at a time"));
    }
    if (states.size() == 1){
      warning(_("only one state variable should be input"));
    }
    IntegerVector ret(1);
    for (unsigned int i = 0; i < states.size(); i++){
      if (states[i] == lookup[0]){
				ret[0] = i+1;
				return ret;
      }
    }
    rxSolveFree();
    stop(_("cannot locate compartment \"%s\""),as<std::string>(lookup[0]).c_str());
  }
  return R_NilValue;
}

//[[Rcpp::export]]
CharacterVector rxParams_(const RObject &obj){
  List modVar = rxModelVars(obj);
  CharacterVector ret = modVar["params"];
  return ret;
}

//' Jacobian and parameter derivatives
//'
//' Return Jacobain and parameter derivatives
//'
//' @inheritParams rxModelVars
//'
//' @return A list of the jacobian parameters defined in this rxode2
//'     object.
//'
//' @author Matthew L. Fidler
//'
//' @family Query model information
//' @export
//[[Rcpp::export]]
CharacterVector rxDfdy(const RObject &obj){
  List modVar = rxModelVars(obj);
  CharacterVector ret = modVar["dfdy"];
  return ret;
}

//' Left handed Variables
//'
//' This returns the model calculated variables
//'
//' @inheritParams rxModelVars
//'
//' @return a character vector listing the calculated parameters
//' @seealso \code{\link{rxode2}}
//' @family Query model information
//' @author Matthew L.Fidler
//' @export
//[[Rcpp::export]]
CharacterVector rxLhs(const RObject &obj){
  List modVar = rxModelVars(obj);
  CharacterVector ret = modVar["lhs"];
  return ret;
}
NumericVector rxInits0(const RObject &obj,
											 Nullable<NumericVector> vec = R_NilValue,
											 Nullable<CharacterVector> req = R_NilValue,
											 double defaultValue = 0,
											 bool noerror = false,
											 bool noini=false){
  NumericVector oini;
  CharacterVector cini;
  List modVar = rxModelVars(obj);
  if (!noini){
    oini = (modVar["ini"]);
    cini = oini.names();
  }
  int i, j, k;
  CharacterVector nreq;
  NumericVector miss;
  if (!req.isNull()){
    nreq = CharacterVector(req);
    if ((ISNA(defaultValue) && noerror) || !ISNA(defaultValue)){
      miss = NumericVector(nreq.size());
      for (i = 0; i < nreq.size(); i++) {
				miss[i] = defaultValue;
      }
      miss.attr("names") = CharacterVector(nreq);
    }
  }
  NumericVector nvec;
  CharacterVector nvecNames;
  if (!vec.isNull()){
    nvec = NumericVector(vec);
    if (nvec.size() > 0){
      if (!nvec.hasAttribute("names")){
				if (!req.isNull() && nreq.size() == nvec.size()){
					nvec.attr("names") = req;
					nvecNames = req;
					std::string wstr = "Assumed order of inputs: ";
					for (i = 0; i < nreq.size(); i++){
						wstr += (i == 0 ? "" : ", ") + nreq[i];
					}
					warning(wstr);
				} else {
					std::string sstr = "Length mismatch\nreq: c(";
					for (i = 0; i < nreq.size(); i++){
						sstr += (i == 0 ? "" : ", ") + nreq[i];
					}
					sstr += ")\nvec: c(";
					for (i = 0; i < nvec.size(); i++){
            sstr += (i == 0 ? "" : ", ") + std::to_string((double)(nvec[i]));
          }
					sstr += ")";
					rxSolveFree();
					stop(sstr);
				}
      } else {
				nvecNames = nvec.names();
      }
    }
  }
  // Prefer c(vec, ini, miss)
  NumericVector ret;
  CharacterVector nret;
  if (!req.isNull()){
    ret =  NumericVector(nreq.size());
    bool found = false;
    for (i = 0; i < nreq.size(); i++){
      found = false;
      for (j = 0; !found && j < nvec.size(); j++){
				if (nreq[i] == nvecNames[j]){
					found =  true;
					ret[i] = nvec[j];
					break;
				}
      }
      for (j = 0; !found && j < cini.size(); j++){
				if (nreq[i] == cini[j]){
          found =  true;
          ret[i] = oini[j];
          break;
        }
      }
      if (!found)
				ret[i] = miss[i];
    }
    ret.attr("names")= nreq;
  } else {
    // In this case
    // vec <- c(vec, ini);
    // vec <- vec[!duplicated(names(vec))]
    CharacterVector dupnames(nvec.size()+oini.size()+miss.size());
    j = 0;
    for (i = 0; i < nvec.size(); i++){
      dupnames[j] = nvecNames[i];
      j++;
    }
    for (i = 0; i < oini.size(); i++){
      dupnames[j] = cini[i];
      j++;
    }
    LogicalVector dups = duplicated(dupnames);
    j = 0;
    for (i = 0; i < dups.size(); i++){
      if (!dups[i]) j++;
    }
    ret = NumericVector(j);
    CharacterVector retn(j);
    k = 0, j = 0;
    for (i = 0; i < nvec.size(); i++){
      if (!dups[j]){
				ret[k] = nvec[i];
				retn[k] = nvecNames[i];
				k++;
      }
      j++;
    }
    for (i = 0; i < oini.size(); i++){
      if (!dups[j]){
				ret[k] = oini[i];
        retn[k] = cini[i];
        k++;
      }
      j++;
    }
    ret.attr("names") = retn;
  }
  return ret;
}

//' Initial Values and State values for a rxode2 object
//'
//' Returns the initial values of the rxDll object
//'
//' @param obj rxDll, rxode2, or named vector representing default
//'     initial arguments
//'
//' @param vec If supplied, named vector for the model.
//'
//' @param req Required names, and the required order for the ODE solver
//'
//' @param defaultValue a number or NA representing the default value for
//'     parameters missing in `vec`, but required in `req`.
//'
//' @param noerror is a boolean specifying if an error should be thrown
//'     for missing parameter values when `default` = `NA`
//'
//' @return Initial values of the rxDll object
//'
//' @keywords internal
//' @family Query model information
//' @author Matthew L.Fidler
//' @export
//[[Rcpp::export]]
SEXP rxInits(const RObject &obj,
						 RObject vec = R_NilValue,
						 Nullable<CharacterVector> req = R_NilValue,
						 double defaultValue = 0,
						 bool noerror = false,
						 bool noini=false,
						 bool rxLines=false){
  if (rxLines){
    if (rxIsNull(obj)){
      CharacterVector ret = "";
      return ret;
    }
    NumericVector inits = rxInits(obj, vec, req, defaultValue, noerror,noini,false);
		if (!obj.hasAttribute("names")) {
			CharacterVector ret = "";
      return ret;
		}
    CharacterVector nms = inits.names();
    List mv = rxModelVars(obj);
    CharacterVector state = mv[RxMv_state];
    std::string ret="";
    bool isState;
    for (int j=inits.size(); j--;){
      isState=false;
      for (int k=state.size(); k--;){
				if (nms[j] == state[k]){
					isState=true;
					break;
				}
      }
      if (as<std::string>(nms[j]) != ""){
				ret += as<std::string>(nms[j]);
				if (isState) ret += "(0)";
				ret += "=" + std::to_string(inits[j]) + ";\n";
      }
    }
    return wrap(ret);
  } else if (vec.isNULL()){
    return wrap(rxInits0(obj, R_NilValue, req, defaultValue, noerror,noini));
  } else if (rxIsList(vec)){
    List vecL = asList(vec, "vec");
    Function unlist("unlist", R_BaseNamespace);
    NumericVector vec2 = as<NumericVector>(unlist(vec));
    if (vec2.size() != vecL.size()){
      rxSolveFree();
      stop(_("only one estimate per named list item; use 'list(x=1)' instead of 'list(x=1:2)'"));
    }
    return wrap(rxInits0(obj, vec2, req, defaultValue, noerror,noini));
  } else if (rxIsNumInt(vec)){
    return wrap(rxInits0(obj, as<NumericVector>(vec), req, defaultValue, noerror,noini));
  } else {
    rxSolveFree();
    stop(_("incompatible initial estimate"));
  }
}

//' Setup the initial conditions.
//'
//' @param obj rxode2 object
//' @param inits A numeric vector of initial conditions.
//' @return initial conditions that were setup
//' @author Matthew L. Fidler
//' @keywords internal
//' @export
//[[Rcpp::export]]
NumericVector rxSetupIni(const RObject &obj,
												 RObject inits = R_NilValue){
  List modVars = rxModelVars(obj);
  CharacterVector state = modVars["state"];
  return rxInits(obj, inits, state, 0.0);
}

//' Setup the initial conditions.
//'
//' @param obj rxode2 object
//'
//' @param inits A numeric vector of initial conditions.
//'
//' @param extraArgs A list of extra args to parse for initial conditions.
//'
//' @author Matthew L. Fidler
//'
//' @keywords internal
//'
//' @return setup scale for changing compartment values
//'
//' @export
//[[Rcpp::export]]
NumericVector rxSetupScale(const RObject &obj,
													 RObject scale = R_NilValue,
													 Nullable<List> extraArgs = R_NilValue){
  List modVars = rxModelVars(obj);
  CharacterVector state = modVars["state"];
  NumericVector ret = rxInits(obj, scale, state, 1.0, false, true);
  unsigned int usedS = 0, foundS = 0;
  if (!extraArgs.isNull()){
    List xtra = as<List>(extraArgs);
    int i, n=state.size();
    std::string cur;
    for (i = 0; i < n; i++){
      cur = "S" + std::to_string(i+1);
      if (xtra.containsElementNamed(cur.c_str())){
				if (ret[i] == 1.0){
					ret[i] = asDouble(xtra[cur], "xtra[cur]");
					usedS++;
				} else {
					rxSolveFree();
					stop(_("trying to scale the same compartment by 'scale=c(%s=%f,...)' and 'S%d=%f' choose one"),
							 (as<std::string>(state[i])).c_str(), ret[i], i+1,asDouble(xtra[i], "xtra[i]"));
				}
      } else {
				cur = "s" + std::to_string(i+1);
        if (xtra.containsElementNamed(cur.c_str())){
          if (ret[i] == 1.0){
            ret[i] = asDouble(xtra[cur], "xtra[cur]");
						usedS++;
          } else {
						rxSolveFree();
            stop(_("trying to scale the same compartment by 'scale=c(%s=%f,...)' and 's%d=%f' choose one"),
                 (as<std::string>(state[i])).c_str(), ret[i], i+1,
								 asDouble(xtra[i], "xtra[i]"));
          }
        }
      }
    }
    Nullable<StringVector> xtraN2 = xtra.names();
    if (!xtraN2.isNull()){
      StringVector xtraN = StringVector(xtraN2);
      n = xtraN.size();
      std::string tmpstr;
      for (i = 0; i < n; i++){
				tmpstr = (as<std::string>(xtraN[i])).substr(0,1);
				if (tmpstr == "S" || tmpstr == "s"){
					tmpstr = (as<std::string>(xtraN[i])).substr(1,1);
					if (tmpstr == "0" || tmpstr == "1" || tmpstr == "2" || tmpstr == "3" || tmpstr == "4" ||
							tmpstr == "5" || tmpstr == "6" || tmpstr == "7" || tmpstr == "8" || tmpstr == "9"){
						foundS++;
          }
        }
      }
    }
    if (foundS > usedS){
      warning(_("scaled a compartment that is not defined by the rxode2 model"));
    }
  }
  return ret;
}

typedef struct {
  int *gon;
  double *gIndSim;
  double *gsolve;
  double *gInfusionRate;
  double *gTlastS;
  double *gTfirstS;
	double *gCurDoseS;
  double *gAlag;
  double *gF;
  double *gRate;
  double *gDur;
  double *gall_times;
  double *gall_times2;
  int *gix;
  double *gdv;
  double *glimit;
  int *gcens;
  double *gamt;
  double *gii;
  double *glhs;
  double *gcov;
  double *ginits;
  double *gscale;
  double *gatol2;
  double *grtol2;
  double *gssAtol;
  double *gssRtol;
  double *gpars;
  //ints
  int *gevid;
  int *gBadDose;
  int *grc;
  int *gidose;
  int *gpar_cov;

  int *gParPos;
  int *gParPos2;

  int *gsvar;
  int *govar;
  int *gsiV;
  //
  int *slvr_counter;
  int *dadt_counter;
  int *jac_counter;
  int *gSampleCov;
  double *gmtime;
  double *gsigma = NULL;
  int nSigma = 0;
  double *gomega = NULL;
  int nOmega = 0;
  int *ordId = NULL;
  int *nradix = NULL;
  uint8_t *** keys = NULL;
  uint8_t * UGRP = NULL;
  int * TMP = NULL;
  bool zeroTheta = false;
  bool zeroOmega = false;
  bool zeroSigma = false;
  int *gindLin = NULL;
} rx_globals;

rx_globals _globals;

extern "C" double *getAlagThread() {
  rx_solve* rx = getRxSolve_();
  rx_solving_options* op = rx->op;
  return _globals.gAlag + (op->neq + op->extraCmt)*omp_get_thread_num();
}

extern "C" void setZeroMatrix(int which) {
  switch(which){
  case 1:
    _globals.zeroTheta = true;
    break;
  case 2:
    _globals.zeroOmega = true;
    break;
  case 3:
    _globals.zeroSigma = true;
    break;
  }
}

double maxAtolRtolFactor = 0.1;

//[[Rcpp::export]]
void atolRtolFactor_(double factor){
  rx_solve* rx = getRxSolve_();
  rx_solving_options* op = rx->op;
  for (int i = op->neq;i--;){
    _globals.grtol2[i] = min2(_globals.grtol2[i]*factor, maxAtolRtolFactor);
    _globals.gatol2[i] = min2(_globals.gatol2[i]*factor, maxAtolRtolFactor);
  }
  op->ATOL = min2(op->ATOL*factor, maxAtolRtolFactor);
  op->RTOL = min2(op->RTOL*factor, maxAtolRtolFactor);
}

extern "C" double * getAol(int n, double atol){
  return _globals.gatol2;
}

extern "C" double * getRol(int n, double rtol) {
  return _globals.grtol2;
}
// This sets up the constant covariates if ev1 is rxEtTran
static inline void gparsCovSetupConstant(RObject &ev1, int npars){
  if (rxIs(ev1, "rxEtTran")) {
    rx_solve* rx = getRxSolve_();
    CharacterVector tmpCls = ev1.attr("class");
    List envCls = tmpCls.attr(".rxode2.lst");
    NumericMatrix iniPars = envCls[RxTrans_pars];
    // Copy the pre-filled covariates into the parameter values.
    for (int j = rx->nsim;j--;){
      std::copy(iniPars.begin(), iniPars.end(), &_globals.gpars[0]+rx->nsub*npars*j);
    }
    IntegerVector parPos = envCls["covParPos0"];
    std::copy(parPos.begin(),parPos.end(), &_globals.gParPos2[0]);
    rx->nCov0 = parPos.size();
    rx->cov0 = _globals.gParPos2;
  }
}
void gparsCovSetup(int npars, int nPopPar, int nsub, RObject ev1,rx_solve* rx){
  if (_globals.gpars != NULL) free(_globals.gpars);
  _globals.gpars = (double*)calloc(npars*max2(nsub, nPopPar), sizeof(double));
  if (_globals.gpars == NULL){
    rxSolveFree();
    stop(_("could not allocate memory for solving parameters"));
  }
  // Fill the parameters with NA.
  // std::fill_n(&_globals.gpars[0], npars*nPopPar, NA_REAL);
  gparsCovSetupConstant(ev1, npars);
}

double *_rxGetErrs = NULL;
void rxFreeErrs(){
  free(_rxGetErrs);
  _rxGetErrs=NULL;
}

extern "C" void gFree(){
	// Free cov_sample
  if (_globals.gSampleCov!=NULL) free(_globals.gSampleCov);
  _globals.gSampleCov=NULL;
  if (_globals.gsolve != NULL) free(_globals.gsolve);
  _globals.gsolve=NULL;
  if (_globals.gon != NULL) free(_globals.gon);
  _globals.gon=NULL;
  if (_globals.gpars != NULL) free(_globals.gpars);
  _globals.gpars=NULL;
  if (_globals.gParPos != NULL) free(_globals.gParPos);
  _globals.gParPos=NULL;
  if (_globals.gevid != NULL) free(_globals.gevid);
  _globals.gevid=NULL;
  if (_globals.gall_times != NULL) free(_globals.gall_times);
  _globals.gall_times=NULL;
  if (_globals.gcov != NULL) free(_globals.gcov);
  _globals.gcov=NULL;
  if (_rxGetErrs != NULL) free(_rxGetErrs);
  _rxGetErrs=NULL;
}

extern "C" double *rxGetErrs(){
  getRxModels();
  if (_rxModels.exists(".sigma")){
    NumericMatrix sigma = _rxModels[".sigma"];
    if (_rxGetErrs == NULL){
      if (_rxGetErrs != NULL) free(_rxGetErrs);
      _rxGetErrs = (double*)calloc(sigma.ncol()*sigma.nrow(), sizeof(double));
      if (_rxGetErrs == NULL) {
				rxSolveFree();
				stop(_("memory for residual errors could not be allocated"));
      }
    }
    else {
      double *tmpErr = (double*)realloc(_rxGetErrs, sigma.ncol()*sigma.nrow()*sizeof(double));
      if (tmpErr == NULL){
				rxSolveFree();
				stop(_("cannot allocate memory to simulate the residuals"));
      }
      _rxGetErrs  = tmpErr;
    }
    std::copy(sigma.begin(),sigma.end(),&_rxGetErrs[0]);
    return _rxGetErrs;
  }
  return NULL;
}

extern "C" int rxGetErrsNcol(){
  getRxModels();
  if (_rxModels.exists(".sigma")){
    NumericMatrix sigma = _rxModels[".sigma"];
    int ret = sigma.ncol();
    return ret;
  }
  return 0;
}

extern "C" int rxGetErrsNrow(){
  getRxModels();
  if (_rxModels.exists(".sigma")){
    NumericMatrix sigma = _rxModels[".sigma"];
    int ret = sigma.nrow();
    return ret;
  }
  return 0;
}

SEXP rxGetFromChar(char *ptr, std::string var){
  // Rcout << str << "\n";
  CharacterVector cv(1);
  cv[0] = Rf_mkChar(ptr);
  List mv = rxModelVars(as<RObject>(cv));
  if (var == ""){
    return wrap(mv);
  } else {
    return wrap(mv[var]);
  }
}

void rxSimTheta(CharacterVector &thetaN,
                CharacterVector& parN,
                IntegerVector &thetaPar,
                NumericMatrix &thetaM,
                bool &simTheta,
                const Nullable<NumericMatrix> &thetaMat = R_NilValue,
                const NumericVector &thetaLower = NumericVector::create(R_NegInf),
                const NumericVector &thetaUpper = NumericVector::create(R_PosInf),
                const Nullable<NumericVector> &thetaDf  = R_NilValue,
                const bool &thetaIsChol = false,
                int nStud = 1,
                int nCoresRV = 1,
                const LogicalVector &simVariability = LogicalVector::create(NA_LOGICAL)){
  int i, j;
  bool simVar;
  if (simVariability[0] == NA_LOGICAL) {
    simVar = nStud > 1;
  } else {
    simVar = simVariability[0];
  }
  if (!thetaMat.isNull() && simVar){
    thetaM = as<NumericMatrix>(thetaMat);
    if (!thetaM.hasAttribute("dimnames")){
      rxSolveFree();
      stop(_("'thetaMat' must be a named matrix"));
    }
    if (!thetaIsChol){
      arma::mat tmpM = as<arma::mat>(thetaMat);
      if (tmpM.is_zero()) {
        setZeroMatrix(1);
      } else if (!tmpM.is_sympd()){
        rxSolveFree();
        stop(_("'thetaMat' must be symmetric"));
      }
    }
    
    thetaM = as<NumericMatrix>(rxSimSigma(wrap(thetaMat), wrap(thetaDf),
																					nCoresRV, thetaIsChol, nStud, true,
																					thetaLower, thetaUpper));
    thetaN = as<CharacterVector>((as<List>(thetaM.attr("dimnames")))[1]);
    // Now put in theta parameter position; This will be used for
    // alternate types of simulation strategies A position of -1 means
    // that the parameter is not defined in the theta matrix.
    for (i = 0; i < parN.size(); i++){
      thetaPar[i] = -1;
      for (j = 0; j < thetaN.size(); j++){
        if (parN[i] == thetaN[j]){
          thetaPar[i] = j;
          break;
        }
      }
    }
    simTheta = true;
  } else if (!thetaMat.isNull() && simVariability[0] == NA_LOGICAL && !simVar){
    warning(_("'thetaMat' is ignored since nStud <= 1\nuse 'simVariability = TRUE' to override."));
  }
}


void rxSimOmega(bool &simOmega,
                bool &omegaSep,
                NumericMatrix &omegaM,
                CharacterVector &omegaN,
                NumericMatrix &omegaMC,
                List &omegaList,
                CharacterVector &thetaN,
                NumericMatrix &thetaM,
                std::string omegatxt="omega",
                const RObject &omega= R_NilValue,
                const Nullable<NumericVector> &omegaDf= R_NilValue,
                const NumericVector &omegaLower = NumericVector::create(R_NegInf),
                const NumericVector &omegaUpper = NumericVector::create(R_PosInf),
                const bool &omegaIsChol = false,
                std::string omegaSeparation= "auto",//("lkj", "separation")
                const int omegaXform = 1,
                double dfSub = 0,
                int nStud = 1,
                int nSub = 1,
                const LogicalVector &simVariability = LogicalVector::create(NA_LOGICAL)) {
  int j;
  bool simVar;
  if (simVariability[0] == NA_LOGICAL) {
    simVar = nStud > 1;
  } else {
    simVar = simVariability[0];
  }
  if (Rf_isNull(omega)){
  } else if (rxIsChar(omega)){
    // Create a matrix in order of the names.
    omegaN = as<CharacterVector>(omega);
    omegaSep=true;
    omegaMC = NumericMatrix(nStud, omegaN.size());
    j=0;
    for (unsigned int i = 0; i < omegaN.size(); i++){
      for (j = 0; j < thetaN.size(); j++){
        if (omegaN[i] == thetaN[j]){
          omegaMC(_,i) = thetaM(_,j);
          break;
        }
      }
      if (j == thetaN.size()){
        stop(_("parameter '%s' was not simulated in 'thetaMat'"), (as<std::string>(omegaN[i])).c_str());
      }
    }
    simOmega = true;
  } else if (rxIs(omega,"matrix") && nSub >= 1){
    omegaM = as<NumericMatrix>(omega);
    if (omegaM.size() == 0) {
    } else {
      simOmega = true;
      if (!omegaM.hasAttribute("dimnames")){
        rxSolveFree();
        stop(_("'%s' must be a named matrix"),omegatxt.c_str());
      }
      if (omegaIsChol){
        omegaMC = omegaM;
      } else {
        arma::mat tmpM = as<arma::mat>(omegaM);
        if (tmpM.is_zero()){
          omegaMC = omegaM;
        } else if (!tmpM.is_sympd()){
          rxSolveFree();
          stop(_("'%s' must be symmetric, positive definite"),omegatxt.c_str());
        } else {
          omegaMC = wrap(arma::chol(as<arma::mat>(omegaM)));
        }
      }
      omegaN = as<CharacterVector>((as<List>(omegaM.attr("dimnames")))[1]);
    }
  }
  if (simVar){
    if (dfSub > 0 && simOmega) {
      if (omegaSep) {
        int defaultType = 2;
        if (omegaSeparation == "auto"){
          if (omegaN.size() >= 10){
            defaultType = 3;
          }
        } else if (omegaSeparation == "separation") {
          defaultType = 3;
        }
        RObject ol = cvPost_(as<SEXP>(NumericVector::create(dfSub)),
                             as<SEXP>(omegaMC),
                             as<SEXP>(IntegerVector::create(1)),
                             as<SEXP>(LogicalVector::create(false)),
                             as<SEXP>(LogicalVector::create(false)),
                             as<SEXP>(IntegerVector::create(defaultType)),
                             as<SEXP>(IntegerVector::create(omegaXform)));
        if (TYPEOF(ol) == VECSXP) {
          omegaList = ol;
        } else {
          omegaList = List::create(ol);
        }
      } else {
        RObject ol = cvPost_(as<SEXP>(NumericVector::create(dfSub)),
                             as<SEXP>(omegaMC),
                             as<SEXP>(IntegerVector::create(nStud)),
                             as<SEXP>(LogicalVector::create(true)),
                             as<SEXP>(LogicalVector::create(false)),
                             as<SEXP>(IntegerVector::create(1)),
                             as<SEXP>(IntegerVector::create(1)));
        if (TYPEOF(ol) == VECSXP) {
          omegaList = ol;
        } else {
          omegaList = List::create(ol);
        }
      }
      RObject cur = omegaList[0];
      int dim = omegaN.size();
      if (Rf_length(cur)== dim*dim) {
        for (int i = 0; i < omegaList.size(); i++) {
          RObject cur = omegaList[i];
          cur.attr("dim") = IntegerVector::create(dim, dim);
          cur.attr("dimnames") = List::create(omegaN, omegaN);
        }
      } else {
        REprintf("Simulated 'cvPost()' does not match the required dimensions:\n");
        print(omegaMC);
        print(omegaN);
        print(wrap(cur));
        stop("cvPost() mismatch");
      }
    }
  }
}

arma::vec getLowerVec(int type, rx_solve* rx) {
  if (type == 0) { // eps
    return arma::vec(_globals.gsigma, rx->neps, false, true);
  } else { // eta
    return arma::vec(_globals.gomega, rx->neta, false, true);
  }
}

arma::vec getUpperVec(int type, rx_solve* rx) {
  if (type == 0) { // eps
    return arma::vec(_globals.gsigma + rx->neps, rx->neps, false, true);
  } else { // eta
    return arma::vec(_globals.gomega + rx->neta, rx->neta, false, true);
  }
}

arma::mat getArmaMat(int type, int csim, rx_solve* rx) {
  if (type == 0) { // eps
    if (_globals.nSigma == 1) {
      return arma::mat(_globals.gsigma + 2 * rx->neps + csim * rx->neps * rx->neps, rx->neps, rx->neps, false, true);
    } else {
      return arma::mat(_globals.gsigma + 2 * rx->neps, rx->neps, rx->neps, false, true);
    }
  } else { // eta
    if (_globals.nOmega == 1) {
      return arma::mat(_globals.gomega + 2 * rx->neta  + csim * rx->neta * rx->neta, rx->neta,  rx->neta, false, true);
    } else {
      return arma::mat(_globals.gomega + 2 * rx->neta, rx->neta,  rx->neta, false, true);
    }
  }
}

arma::vec fillVec(arma::vec& in, int len);

//' Simulate Parameters from a Theta/Omega specification
//'
//' @param params Named Vector of rxode2 model parameters
//'
//' @param nObs Number of observations to simulate (with `sigma` matrix)
//'
//' @inheritParams rxSolve
//'
//' @param simSubjects boolean indicated rxode2 should simulate subjects in studies (`TRUE`,
//'         default) or studies (`FALSE`)
//'
//' @return a data frame with the simulated subjects
//'
//' @author Matthew L.Fidler
//'
//' @export
//[[Rcpp::export]]
List rxSimThetaOmega(const Nullable<NumericVector> &params    = R_NilValue,
                     const RObject &omega= R_NilValue,
                     const Nullable<NumericVector> &omegaDf= R_NilValue,
                     const NumericVector &omegaLower = NumericVector::create(R_NegInf),
                     const NumericVector &omegaUpper = NumericVector::create(R_PosInf),
                     const bool &omegaIsChol = false,
                     std::string omegaSeparation= "auto",//("lkj", "separation")
                     const int omegaXform = 1,
                     int nSub = 1,
                     const Nullable<NumericMatrix> &thetaMat = R_NilValue,
                     const NumericVector &thetaLower = NumericVector::create(R_NegInf),
                     const NumericVector &thetaUpper = NumericVector::create(R_PosInf),
                     const Nullable<NumericVector> &thetaDf  = R_NilValue,
                     const bool &thetaIsChol = false,
                     int nStud = 1,
                     const RObject sigma = R_NilValue,
                     const NumericVector &sigmaLower = NumericVector::create(R_NegInf),
                     const NumericVector &sigmaUpper = NumericVector::create(R_PosInf),
                     const Nullable<NumericVector> &sigmaDf= R_NilValue,
                     const bool &sigmaIsChol = false,
                     std::string sigmaSeparation= "auto",//("lkj", "separation")
                     const int sigmaXform = 1,
                     int nCoresRV = 1,
                     int nObs = 1,
                     double dfSub = 0,
                     double dfObs = 0,
                     bool simSubjects=true,
                     const LogicalVector &simVariability = LogicalVector::create(NA_LOGICAL)) {
  rx_solve* rx = getRxSolve_();
  NumericVector par;
  CharacterVector parN;
  bool simVar;
  if (simVariability[0] == NA_LOGICAL) {
    simVar = nStud > 1;
  } else {
    simVar = simVariability[0];
  }

  if (params.isNull()){
  } else {
    par = NumericVector(params);
    if (!par.hasAttribute("names")){
      rxSolveFree();
      stop(_("'params' must be a named vector"));
    }
    parN = CharacterVector(par.attr("names"));
  }
  NumericMatrix thetaM;
  CharacterVector thetaN;
  bool simTheta = false;
  IntegerVector thetaPar(parN.size());
  int i, j, k;
  rxSimTheta(thetaN, parN, thetaPar, thetaM, simTheta,
             thetaMat, thetaLower, thetaUpper, thetaDf,
             thetaIsChol, nStud, nCoresRV, simVariability);
  bool simOmega = false;
  bool omegaSep=false;
  NumericMatrix omegaM;
  CharacterVector omegaN;
  NumericMatrix omegaMC;
  List omegaList;

  if (nSub > 1 && rxIsNull(omega)){
    // rxSolveFree();
    warning(_("multi-subject simulation without without 'omega'"));
  }
  rxSimOmega(simOmega, omegaSep, omegaM, omegaN, omegaMC,
             omegaList, thetaN, thetaM, "omega", omega, omegaDf,
             omegaLower, omegaUpper, omegaIsChol,
             omegaSeparation, omegaXform, dfSub, nStud, nSub, simVariability);
  arma::mat tmp = as<arma::mat>(omegaM);
  if (tmp.is_zero()) {
    setZeroMatrix(2);
  }
  bool simSigma = false;
  bool sigmaSep=false;
  NumericMatrix sigmaM;
  CharacterVector sigmaN;
  NumericMatrix sigmaMC;
  List sigmaList;
  rxSimOmega(simSigma, sigmaSep, sigmaM, sigmaN, sigmaMC,
             sigmaList, thetaN, thetaM, "sigma", sigma, sigmaDf,
             sigmaLower, sigmaUpper, sigmaIsChol,
             sigmaSeparation, sigmaXform, dfObs, nStud, nObs, simVariability);

  tmp = as<arma::mat>(sigmaM);
  if (tmp.is_zero()) {
    setZeroMatrix(3);
  }
  // Now create data frame of parameter values
  List iovList;

  int pcol = par.size();
  int ocol = 0;
  int ncol = pcol;
  if (simOmega){
    ocol = omegaMC.ncol();
    ncol += ocol;
  }
  int scol = 0;
  if (simSigma){
    scol = sigmaMC.ncol();
  }
  NumericMatrix ret1;
  if (simSigma){
    ncol += scol;
    if (simSubjects){
      ret1 = NumericMatrix(nObs*nStud*nSub, scol);
    } else {
      ret1 = NumericMatrix(nObs*nStud, scol);
    }
  }
  List ret0(ncol);
  NumericVector nm;
  NumericMatrix nm1;
  for (i = 0; i < ncol; i++){
    nm = NumericVector(nSub*nStud);
    ret0[i] = nm;
  }
  for (i = 0; i < nStud; i++) {
    for (j = 0; j < pcol; j++) {
      nm = ret0[j];
      for (k = 0; k < nSub; k++) {
        nm[nSub*i + k] = par[j];
      }
      if (simTheta) {
        if(thetaPar[j] != -1) {
          for (k = 0; k < nSub; k++) {
            nm[nSub*i + k] += thetaM(i, thetaPar[j]);
          }
        }
      }
      ret0[j] = nm;
    }
    // Now Omega Covariates
    if (ocol > 0) {
      if (dfSub > 0 && simVar) {
        // nm = ret0[j]; // parameter column
        RObject ol = wrap(omegaList[i]);
        RObject nm0 = rxSimSigma(wrap(omegaList[i]), wrap(omegaDf), nCoresRV, false, nSub,
                                 false, omegaLower, omegaUpper);
        nm1 = as<NumericMatrix>(nm0);
      } else {
        nm1 = as<NumericMatrix>(rxSimSigma(wrap(omegaMC), wrap(omegaDf), nCoresRV, true, nSub,
																					 false, omegaLower, omegaUpper));
      }
      for (j=pcol; j < pcol+ocol; j++) {
        nm = ret0[j];
        for (k = 0; k < nSub; k++) {
          nm[nSub*i + k] = nm1(k, j-pcol);
        }
        ret0[j] = nm;
      }
    }
    if (scol > 0) {
      if (simSubjects) {
        if (dfObs > 0  && simVar) {
          nm1 = as<NumericMatrix>(rxSimSigma(as<RObject>(sigmaList[i]), as<RObject>(sigmaDf), nCoresRV, false, nObs*nSub,
																						 false, sigmaLower, sigmaUpper));
        } else {
          nm1 = as<NumericMatrix>(rxSimSigma(as<RObject>(sigmaMC), as<RObject>(sigmaDf), nCoresRV, true, nObs*nSub,
																						 false, sigmaLower, sigmaUpper));
        }
        for (j = 0; j < scol; j++) {
          for (k = 0; k < nObs*nSub; k++){
            // ret1 = NumericMatrix(nObs*nStud, scol);
            ret1(nObs*nSub*i+k, j) = nm1(k, j);
          }
        }
      } else {
        if (dfObs > 0  && simVar) {
          nm1 = as<NumericMatrix>(rxSimSigma(as<RObject>(sigmaList[i]), as<RObject>(sigmaDf), nCoresRV, false, nObs,
																						 false, sigmaLower, sigmaUpper));
        } else {
          nm1 = as<NumericMatrix>(rxSimSigma(as<RObject>(sigmaMC), as<RObject>(sigmaDf), nCoresRV, true, nObs,
																						 false, sigmaLower, sigmaUpper));
        }
        for (j = 0; j < scol; j++) {
          for (k = 0; k < nObs; k++) {
            // ret1 = NumericMatrix(nObs*nStud, scol);
            ret1(nObs*i+k, j) = nm1(k, j);
          }
        }
      }
    }
  }
  CharacterVector dfName(ncol);
  for (i = 0; i < pcol; i++){
    dfName[i] = parN[i];
  }
  for (i = pcol; i < pcol+ocol; i++) {
    dfName[i] = omegaN[i-pcol];
  }
  for (i = pcol+ocol; i < ncol; i++) {
    dfName[i] = sigmaN[i-pcol-ocol];
  }
  ret0.attr("names") = dfName;
  ret0.attr("class") = "data.frame";
  ret0.attr("row.names") = IntegerVector::create(NA_INTEGER,-nSub*nStud);
  getRxModels();
  if (ret1.nrow() > 1) {
    ret1.attr("dimnames") = List::create(R_NilValue, sigmaN);
    _rxModels[".sigma"] = ret1;
  }
  if (simTheta) {
    _rxModels[".theta"] = thetaM;
  }
  if (dfSub > 0 && simVar) {
    _rxModels[".omegaL"] = omegaList;
    _rxModels[".omegaN"] = omegaN;
  } else if (Rf_isMatrix(omega)) {
    _rxModels[".omegaN"]= as<CharacterVector>((as<List>((as<NumericMatrix>(omega)).attr("dimnames")))[1]);
  }
  if (dfObs > 0 && simVar) {
    _rxModels[".sigmaL"] = sigmaList;
  }
  if (Rf_isNull(sigma) || rxIsChar(sigma) || Rf_length(sigma) == 0) {
  } else {
    // Fill in sigma information for simeta()
    arma::mat sigma0;
    if (dfObs > 0 && simVar) {
      sigma0 = as<arma::mat>(sigmaList[0]);
      if (_globals.gsigma != NULL) free(_globals.gsigma);
      rx->neps = sigma0.n_rows;
      _globals.gsigma = (double*)malloc((rx->neps * rx->neps * sigmaList.size() + 2 * rx->neps) * sizeof(double));
      for (int i = 0; i < sigmaList.size(); i++) {
				sigma0 = as<arma::mat>(sigmaList[i]);
				std::copy(&sigma0[0], &sigma0[0] + rx->neps * rx->neps, _globals.gsigma + 2 * rx->neps + i * rx->neps * rx->neps);
      }
      _globals.nSigma = 1;
    } else {
      if (sigmaIsChol) {
				sigma0 = as<arma::mat>(sigmaM);
				sigma0 = sigma0 * arma::trans(sigma0);
      } else {
				sigma0 = as<arma::mat>(sigmaM);
      }
      if (_globals.gsigma != NULL) free(_globals.gsigma);
      rx->neps = sigma0.n_rows;
      _globals.gsigma = (double*)malloc((rx->neps * rx->neps + 2 * rx->neps)* sizeof(double));
      std::copy(&sigma0[0], &sigma0[0] + rx->neps * rx->neps, _globals.gsigma + 2 * rx->neps);
      _globals.nSigma = 0;
    }
    arma::vec in = as<arma::vec>(sigmaLower);
    arma::vec lowerSigmaV = fillVec(in, sigma0.n_rows);
    arma::vec upperSigmaV = fillVec(in, sigma0.n_rows);
    std::copy(&lowerSigmaV[0], &lowerSigmaV[0] + rx->neps, _globals.gsigma);
    std::copy(&upperSigmaV[0], &upperSigmaV[0] + rx->neps, _globals.gsigma + rx->neps);
    // structure of _globals.gsigma is
    // lower
    // upper
    // matrix list (n x n;  nStud matrices)
  }

  if (Rf_isNull(omega) || rxIsChar(omega)) {
  } else {
    // Fill in omega information for simeta()
    arma::mat omega0;
    if (dfSub > 0 && simVar) {
      omega0 = as<arma::mat>(omegaList[0]);
      rx->neta = omega0.n_rows;
      if (_globals.gomega != NULL) free(_globals.gomega);
      _globals.gomega = (double*)malloc((2 * rx->neta + rx->neta * rx->neta * omegaList.size())*sizeof(double));
      for (int i = 0; i < omegaList.size(); i++) {
        omega0 = as<arma::mat>(omegaList[i]);
        std::copy(&omega0[0], &omega0[0] + rx->neta * rx->neta, _globals.gomega + 2 * rx->neta + i * rx->neta * rx->neta);
      }
      _globals.nOmega = 1;
    } else {
      if (omegaIsChol) {
				omega0 = as<arma::mat>(omegaM);
				omega0 = omega0 * arma::trans(omega0);
      } else {
				omega0 = as<arma::mat>(omegaM);
      }
      if (_globals.gomega != NULL) free(_globals.gomega);
      rx->neta = omega0.n_rows;
      _globals.gomega = (double*)malloc((2 * rx->neta + rx->neta * rx->neta)*sizeof(double));
      std::copy(&omega0[0], &omega0[0] + rx->neta * rx->neta, _globals.gomega + 2 * rx->neta);
      _globals.nOmega = 0;
    }
    arma::vec in = as<arma::vec>(omegaLower);
    arma::vec lowerOmegaV = fillVec(in, rx->neta);
    in = as<arma::vec>(omegaUpper);
    arma::vec upperOmegaV = fillVec(in, rx->neta);
    std::copy(&lowerOmegaV[0], &lowerOmegaV[0] + rx->neta, _globals.gomega);
    std::copy(&upperOmegaV[0], &upperOmegaV[0] + rx->neta, _globals.gomega + rx->neta);
    // structure of _globals.gomega is
    // lower
    // upper
    // matrix list (n x n;  nStud matrices)
  }
  return ret0;
}

#define defrx_params R_NilValue
#define defrx_events R_NilValue
#define defrx_inits R_NilValue
#define defrx_covs R_NilValue
#define defrx_method 2
#define defrx_atol 1.0e-8
#define defrx_rtol 1.0e-8
#define defrx_maxsteps 5000
#define defrx_hmin 0
#define defrx_hmax R_NilValue
#define defrx_hini 0
#define defrx_maxordn 12
#define defrx_maxords 5
#define defrx_cores 1
#define defrx_covs_interpolation 0
#define defrx_addCov false
#define defrx_matrix 0
#define defrx_sigma  R_NilValue
#define defrx_sigmaDf R_NilValue
#define defrx_nCoresRV 1
#define defrx_sigmaIsChol false
#define defrx_nDisplayProgress 10000
#define defrx_amountUnits NA_STRING
#define defrx_timeUnits "hours"
#define defrx_addDosing false
#define defrx_stateTrim R_PosInf


RObject rxCurObj;

Nullable<Environment> rxrxode2env(RObject obj);

std::string rxDll(RObject obj);

bool rxDynLoad(RObject obj);

List getEtRxsolve(Environment e){
  if (!e.exists(".et")){
    RObject eventso = e[".args.events"];
    List emptyLst(0);
    RObject et = et_(emptyLst, emptyLst);
    setEvCur(et);
    et_(List::create(_["data"] = eventso), List::create("importQuiet"));
    e[".et"] = et;
    Function parse2("parse", R_BaseNamespace);
    Function eval2("eval", R_BaseNamespace);
    // eventTable style methods
    e["get.EventTable"] = eval2(_["expr"]   = parse2(_["text"]="function() .et$get.EventTable()"),
																_["envir"]  = e);
    e["get.obs.rec"] = eval2(_["expr"]   = parse2(_["text"]="function() .et$get.obs.rec()"),
														 _["envir"]  = e);
    e["get.nobs"] = eval2(_["expr"]   = parse2(_["text"]="function() .et$get.nobs()"),
													_["envir"]  = e);
    e["add.dosing"] = eval2(_["expr"]   = parse2(_["text"]="function(...) {.newEt <- .et; .newEt$add.dosing(...); invisible(rxSolve(.args.object,events=.newEt, updateObject=TRUE))}"),
														_["envir"]  = e);
    e["clear.dosing"] = eval2(_["expr"]   = parse2(_["text"]="function(...) {.newEt <- .et; .newEt$clear.dosing(...); invisible(rxSolve(.args.object,events=.newEt, updateObject=TRUE))}"),
															_["envir"]  = e);
    e["get.dosing"] = eval2(_["expr"]   = parse2(_["text"]="function() .et$get.dosing()"),
														_["envir"]  = e);

    e["add.sampling"] = eval2(_["expr"]   = parse2(_["text"]="function(...) {.newEt <- .et; .newEt$add.sampling(...); invisible(rxSolve(.args.object,events=.et,updateObject=TRUE))}"),
															_["envir"]  = e);

    e["clear.sampling"] = eval2(_["expr"]   = parse2(_["text"]="function(...) {.newEt <- .et; .newEt$clear.sampling(...); invisible(rxSolve(.args.object,events=.newEt,updateObject=TRUE))}"),
																_["envir"]  = e);

    e[".replace.sampling"] = eval2(_["expr"]   = parse2(_["text"]="function(...) {.newEt <- .et; .newEt$clear.sampling(); .newEt$add.sampling(...); invisible(rxSolve(.args.object,events=.newEt,updateObject=TRUE))}"),
																	 _["envir"]  = e);

    e["get.sampling"] = eval2(_["expr"]   = parse2(_["text"]="function() .et$get.sampling()"),
															_["envir"]  = e);

    e["get.units"] = eval2(_["expr"]   = parse2(_["text"]="function() .et$get.units()"),
													 _["envir"]  = e);

    e["import.EventTable"] = eval2(_["expr"]   = parse2(_["text"]="function(imp) {.et <- as.et(imp); invisible(rxSolve(.args.object,events=.et,updateObject=TRUE))}"),
																	 _["envir"]  = e);

    // Note event.copy doesn't really make sense...?  The create.eventTable does basically the same thing.
  }
  return e[".et"];
}

inline void updateParNames0(CharacterVector &ret, Environment &e,
														const std::string& what){
  if (e.exists(what)){
    CharacterVector out = e[what];
    CharacterVector in = out.names();
    for (unsigned int i = ret.size(); i--;){
      for (int j = in.size(); j--;){
				if (as<std::string>(ret[i]) == as<std::string>(in[j])){
					ret[i] = out[j];
					break;
				}
      }
    }
  }
}

CharacterVector updateParNames(CharacterVector parNames, Environment e){
  CharacterVector ret = parNames;
  updateParNames0(ret, e, ".nestTheta");
  updateParNames0(ret, e, ".nestEta");
  return parNames;
}

// This updates the evironment post solve after running.  Defers some
// computational cost until the rxSolve is looked at by the user.
void updateSolveEnvPost(Environment e){
  if (!e.exists(".params.dat")){
    List mv = rxModelVars(as<RObject>(e));
    NumericVector mvIni = mv[RxMv_ini];
    CharacterVector pars = mv[RxMv_params];
    RObject parso = e[".args.params"];
    IntegerVector ppos = e[".par.pos"];
    bool IsIni = e[".par.pos.ini"];
    CharacterVector idLevels = as<CharacterVector>(e[".idLevels"]);
    if (rxIsNumInt(parso) || rxIsNull(parso)) {
      double *tmp=(double*)calloc(ppos.size(),sizeof(double));
      if (tmp == NULL){
				rxSolveFree();
				stop(_("ran out of memory during 'updateSolveEnvPost'"));
      }
      // NumericVector   prs(ppos.size()-nrm);
      // CharacterVector prsn(ppos.size()-nrm+1);
      NumericVector parNumeric;
      if (!rxIsNull(parso)){
        parNumeric= as<NumericVector>(parso);
      }
      unsigned int i, j=0;
      for (i = 0; i < ppos.size(); i++){
				if (ppos[i] > 0){ // User specified parameter
					if (IsIni){
						tmp[j] = mvIni[ppos[i]-1];
					} else {
						tmp[j] = parNumeric[ppos[i]-1];
					}
          // prsn[j] = pars[i];
					j++;
        } else if (ppos[i] < 0) { // ini specified parameter.
          tmp[j] = mvIni[-ppos[i]-1];
          // prsn[j] = pars[i];
					j++;
        }
      }
      NumericVector prs(j);
      CharacterVector prsn(j);
      std::copy(&tmp[0],&tmp[0]+j,prs.begin());
      free(tmp);
      j=0;
      for (i = 0; i < ppos.size(); i++){
        if (ppos[i] != 0){ // User specified parameter
          prsn[j] = pars[i];
          j++;
        }
      }
      prsn = updateParNames(prsn, e);
      prs.names() = prsn;
      e[".params.single"] = prs;
      List pd(prs.size());
      for (unsigned int j = prs.size();j--;){
				pd[j] = NumericVector::create(prs[j]);
      }
      pd.names() = prsn;
      pd.attr("class") = "data.frame";
      pd.attr("row.names") = IntegerVector::create(NA_INTEGER,-1);
      e[".params.dat"] = pd;
      e["counts"] = DataFrame::create(_["slvr"]=e[".slvr.counter"],
																			_["dadt"]=e[".dadt.counter"],
																			_["jac"]=e[".jac.counter"]);
    } else {
      DataFrame parsdf = as<DataFrame>(parso);
      int extran = 0;
      int nsub = e[".nsub"], nsim=e[".nsim"];
      int nrm=0;
      for (unsigned int i = ppos.size(); i--;){
				if (ppos[i] == 0){ // Covariate or simulated variable.
					nrm++;
				}
      }
      if (nsub > 1) extran++;
      if (nsim > 1) extran++;
      CharacterVector prsn(ppos.size()-nrm+extran);
      List prsl(ppos.size()-nrm+extran);
      unsigned int i, j=0;
      if (nsim > 1) {
				IntegerVector tmp(parsdf.nrow());
				for (unsigned int k = parsdf.nrow(); k--;){
					tmp[k] = (k / nsub)+1;
				}
        prsn[j]="sim.id";
				prsl[j] = tmp;
				j++;
      }
      if (nsub > 1) {
        IntegerVector tmp(parsdf.nrow());
				for (unsigned int k = parsdf.nrow(); k--;){
					tmp[k] = (k % nsub)+1;
				}
				if (idLevels.size() > 0){
					tmp.attr("class") = CharacterVector::create("factor");
					tmp.attr("levels") = idLevels;
				}
				prsl[j] = tmp;
				prsn[j]="id";
				j++;
      }
      for (i = 0; i < ppos.size();i++){
        if (ppos[i] > 0){ // User specified parameter
          prsl[j] = parsdf[ppos[i]-1];
          prsn[j] = pars[i];
          j++;
        } else if (ppos[i] < 0) { // ini specified parameter.
					NumericVector tmp(parsdf.nrow(), mvIni[-ppos[i]-1]);
					prsl[j] = tmp;
          prsn[j] = pars[i];
          j++;
        }
      }
      prsn = updateParNames(prsn, e);
      prsl.names() = prsn;
      prsl.attr("class") = "data.frame";
      prsl.attr("row.names") = IntegerVector::create(NA_INTEGER,-parsdf.nrow());
      e[".params.dat"] = prsl;
      if (parsdf.nrow() == 1){
				NumericVector prsnv(prsl.size());
				for (j = prsl.size(); j--;){
					prsnv[j] = (as<NumericVector>(prsl[j]))[0];
				}
				prsnv.names() = prsn;
        e[".params.single"] = prsnv;
      } else {
				e[".params.single"] = R_NilValue;
      }
      List cnt(extran+3);
      CharacterVector cntn(extran+3);
      j = 0;
      if (nsim > 1) {
				cntn[j]="sim.id";
        cnt[j] = prsl["sim.id"];
				j++;
      }
      if (nsub > 1) {
        cntn[j]="id";
        cnt[j] = prsl["id"];
				j++;
      }
      cntn[j] = "slvr";
      cnt[j++] = e[".slvr.counter"];
      cntn[j] = "dadt";
      cnt[j++] = e[".dadt.counter"];
      cntn[j] = "jac";
      cnt[j++] = e[".jac.counter"];
      cnt.names() = cntn;
      cnt.attr("class") = "data.frame";
      cnt.attr("row.names") = IntegerVector::create(NA_INTEGER,-parsdf.nrow());
      e["counts"] = cnt;
    }
  }
  getEtRxsolve(e);
}

void resetFkeep();
//' Free the C solving/parsing information.
//'
//' Take the ODE C system and free it.
//'
//' @keywords internal
//' @return logical indicating if the memory was successfully freed
//' @export
// [[Rcpp::export]]
LogicalVector rxSolveFree(){
  resetFkeep();
  rx_solve* rx = getRxSolve_();
  // Free the solve id order
  if (rx->par_sample != NULL) free(rx->par_sample);
  rx->par_sample=NULL;
  if (_globals.ordId != NULL) free(_globals.ordId);
  _globals.ordId = rx->ordId = NULL;
  if (_globals.nradix != NULL) free(_globals.nradix);
  _globals.nradix=NULL;
  // Free the omega info
  if (_globals.gomega != NULL) free(_globals.gomega);
  _globals.gomega = NULL;
  if (_globals.gsigma != NULL) free(_globals.gsigma);
  _globals.gsigma = NULL;
  // Free the allocated keys
  if (_globals.keys != NULL) {
    int i=0;
    while (_globals.keys[i] != NULL){
      int j = 0;
      while(_globals.keys[i][j] != NULL){
				free(_globals.keys[i][j]);
				_globals.keys[i][j++] = NULL;
      }
      free(_globals.keys[i]);
      _globals.keys[i++] = NULL;
    }
    free(_globals.keys);
    _globals.keys=NULL;
  }

  if (_globals.TMP != NULL) {
    free(_globals.TMP);
  }
  _globals.TMP = NULL;

  _globals.zeroTheta = false;
  _globals.zeroOmega = false;
  _globals.zeroSigma = false;

  if (_globals.UGRP != NULL) {
    free(_globals.UGRP);
  }
  _globals.UGRP = rx->UGRP = NULL;

  if (_globals.ordId != NULL) {
    free(_globals.ordId);
  }
  _globals.ordId = rx->ordId = NULL;

  if (rx->hasFactors == 1){
    lineFree(&(rx->factors));
    lineFree(&(rx->factorNames));
  }
  if (!rxIsNull(rxSolveFreeObj)) {
    rxUnlock(rxSolveFreeObj);
    rxSolveFreeObj=R_NilValue;
  }
  if (_globals.gindLin != NULL) R_Free(_globals.gindLin);
  rxOptionsFree(); // f77 losda free
  rxOptionsIni();// realloc f77 lsoda cache
  parseFree(0); //free parser
  rxClearFuns(); // Assign all the global ODE solving functions to NULL pointers
  gFree();// Frees all the global pointers
  return LogicalVector::create(true);
}

extern "C" void rxSolveFreeC() {
  rxSolveFree();
}


List keepFcov;

extern void resetFkeep() {
  keepFcov = List::create();
}


extern "C" double get_fkeep(int col, int id, rx_solving_options_ind *ind) {
  List keepFcovI= keepFcov.attr("keepCov");
  int idx = keepFcovI[col];
  if (idx == 0) return REAL(keepFcov[col])[id];
  return ind->par_ptr[idx-1];
}

extern "C" SEXP get_fkeepn() {
  return as<SEXP>(keepFcov.attr("names"));
}

extern "C" void sortIds(rx_solve* rx, int ini) {
  rx_solving_options_ind* ind;
  int nall = rx->nsub*rx->nsim;
  // Perhaps throttle this to nall*X
  if (ini) {
    if (_globals.ordId != NULL) free(_globals.ordId);
    _globals.ordId=NULL;
    rx->ordId = _globals.ordId = (int*)malloc(nall*sizeof(int));
    std::iota(rx->ordId,rx->ordId+nall,1);
  } else if (rx->op->cores > 1 && rx->op->cores >= nall*getThrottle()) {
    // Here we order based on run times.  This way this iteratively
    // changes the order based on run-time.
    NumericVector solveTime(nall);
    IntegerVector ord;
    for (int i = 0; i < nall; i++) {
      ind = &(rx->subjects[i]);
      solveTime[i] = ind->solveTime;
    }
    Function order = getForder();
    if (useForder()) {
      ord = order(solveTime, _["na.last"] = LogicalVector::create(NA_LOGICAL),
									_["decreasing"] = LogicalVector::create(true));
    } else {
      ord = order(solveTime, _["na.last"] = LogicalVector::create(NA_LOGICAL),
									_["method"]="radix",
									_["decreasing"] = LogicalVector::create(true));
    }
    // This assumes that this has already been created
    std::copy(ord.begin(), ord.end(), rx->ordId);
  }
}

typedef struct{
  bool updateObject;
  bool isRxSolve;
  bool isEnvironment;
  bool hasCmt;
  List mv;
  Nullable<LogicalVector> addDosing = R_NilValue;
  RObject timeUnitsU;
  bool addTimeUnits;
  List covUnits;
  RObject par1;
  bool usePar1 = false;
  bool finalPar1 = false;
  bool par1Keep = false;
  bool swappedEvents = false;
  RObject par1ini;
  NumericVector initsC;
  int nSize;
  bool fromIni = false;
  IntegerVector eGparPos;
  CharacterVector sigmaN;
  CharacterVector omegaN;
  NumericVector parNumeric;
  DataFrame parDf;
  NumericMatrix parMat;
  int parType = 1;
  int nPopPar = 1;
  CharacterVector nmP;
  int npars;
  NumericVector mvIni;
  int nsvar;
  bool idFactor;
  bool labelID;
  bool warnIdSort;
  CharacterVector idLevels;
  bool convertInt = false;
  bool throttle = false;
} rxSolve_t;

SEXP rxSolve_(const RObject &obj, const List &rxControl, const Nullable<CharacterVector> &specParams,
							const Nullable<List> &extraArgs, const RObject &params, const RObject &events,
							const RObject &inits, const int setupOnly);


// This will update the rxSolve object with any new features
//
static inline SEXP rxSolve_update(const RObject &object,
																	const List &rxControl,
																	const Nullable<CharacterVector> &specParams,
																	const Nullable<List> &extraArgs,
																	const RObject &params,
																	const RObject &events,
																	const RObject &inits,
																	rxSolve_t* rxSolveDat){
  bool update_params = false,
    update_events = false,
    update_inits = false;
  if (specParams.isNull()){
    warning(_("No additional parameters were specified, returning original object"));
    return object;
  }
  CharacterVector specs = CharacterVector(specParams);
  int n = specs.size(), i;
  for (i = n; i--;){
    if (as<std::string>(specs[i]) == "params")
      update_params = true;
    else if (as<std::string>(specs[i]) == "events")
      update_events = true;
    else if (as<std::string>(specs[i]) == "inits")
      update_inits = true;
  }
  // Now update
  Environment e;
  List lobj;
  if (rxSolveDat->isRxSolve){
    lobj = as<List>(object);
    CharacterVector classattr = object.attr("class");
    e = as<Environment>(classattr.attr(".rxode2.env"));
  } else  { // if (rxIs(object, "environment"))
    e = as<Environment>(object);
    lobj = as<List>(e["obj"]);
  }
  getRxModels();
  if (e.exists(".params.dat")){
    e.remove(".params.dat");
  }
  if (e.exists(".et")){
    e.remove(".et");
  }
  if(e.exists(".sigma")){
    if (Rf_isMatrix(e[".sigma"])) {
      _rxModels[".sigma"]=as<NumericMatrix>(e[".sigma"]);
    }
  }
  if(e.exists(".sigmaL")){
    _rxModels[".sigmaL"]=as<List>(e[".sigmaL"]);
  }
  if(e.exists(".omegaL")){
    _rxModels[".omegaL"] = as<List>(e[".omegaL"]);
    _rxModels[".omegaN"] = as<CharacterVector>(e[".omegaN"]);
  }
  if(e.exists(".thetaL")){
    _rxModels[".thetaL"] = as<List>(e[".thetaL"]);
  }
  if(e.exists(".theta")){
    _rxModels[".theta"] = as<NumericMatrix>(e[".theta"]);
  }
  RObject new_params;
  RObject new_events;
  if (rxIs(params, "rx.event")){
    new_events = params;
    new_params = events;
  } else {
    new_params = update_params ? params : e[".args.params"];
    new_events = update_events ? events : e[".args.events"];
  }

  RObject new_inits = update_inits ? inits : e[".args.inits"];
  List newRxControl = clone(rxControl);
  RObject new_object = as<RObject>(e[".args.object"]);
  newRxControl[Rxc_updateObject] = false;
  List dat = as<List>(rxSolve_(new_object, newRxControl, R_NilValue, extraArgs,
															 new_params, new_events, new_inits, 0));
  if (rxSolveDat->updateObject && as<bool>(e[".real.update"])){
    List old = as<List>(rxCurObj);
    //Should I zero out the List...?
    CharacterVector oldNms = old.names();
    CharacterVector nms = dat.names();
    if (oldNms.size() == nms.size()){
      int i;
      for (i = 0; i < nms.size(); i++){
				old[as<std::string>(nms[i])] = as<SEXP>(dat[as<std::string>(nms[i])]);
      }
      old.attr("class") = dat.attr("class");
      old.attr("row.names") = dat.attr("row.names");
      return old;
    } else {
      warning(_("can not update object"));
      return dat;
    }
  }
  e[".real.update"] = true;
  return dat;
}

// This updates the event table with sequences and other similar items
// This is useful when there is an event table without any
// observations in it.  This is the way that deSolve/mrgsolve handles
// events.
static inline void rxSolve_ev1Update(const RObject &obj,
																		 const List &rxControl,
																		 const Nullable<CharacterVector> &specParams,
																		 const Nullable<List> &extraArgs,
																		 const RObject &params,
																		 RObject &ev1,
																		 const RObject &inits,
																		 rxSolve_t* rxSolveDat){
  rx_solve* rx = getRxSolve_();
  if (rxIs(ev1, "rxEt")){
    CharacterVector cls = ev1.attr("class");
    List etE = cls.attr(".rxode2.lst");
    int nobs = asInt(etE["nobs"], "nobs");
    if (nobs == 0){
      // KEEP/DROP?
      List ev1a = etTrans(as<List>(ev1), obj, rxSolveDat->hasCmt,
													false, false, true, R_NilValue,
													rxControl[Rxc_keepF]);
      rxSolveDat->labelID=true;
      CharacterVector tmpC = ev1a.attr("class");
      List tmpL = tmpC.attr(".rxode2.lst");
      rxSolveDat->idLevels = asCv(tmpL[RxTrans_idLvl], "idLvl");
      List keep = tmpL[RxTrans_keepL];
      keepFcov=keep;
      rx->nKeepF = keepFcov.size();
      int lenOut = 200;
      double by = NA_REAL;
      double to;
      double from = 0.0;
      NumericVector tmp;
      IntegerVector tmpI;
      if (rxIsNumInt(rxControl[Rxc_from])){
				tmp = asNv(rxControl[Rxc_from], "from");
				if (tmp.size() != 1){
					rxSolveFree();
					stop(_("'from' must be of length 1"));
				}
				from = tmp[0];
      }
      if (rxIsNumInt(rxControl[Rxc_to])){
				tmp = asNv(rxControl[Rxc_to], "to");
				if (tmp.size() != 1){
					rxSolveFree();
					stop(_("'to' must be of length 1"));
				}
				to = tmp[0];
      } else {
				to = (max(as<NumericVector>(ev1a["TIME"]))+24);
      }
      if (rxIsNumInt(rxControl[Rxc_by])){
				tmp = asNv(rxControl[Rxc_by], "by");
				if (tmp.size() != 1){
					rxSolveFree();
					stop(_("'by' must be of length 1"));
				}
				by = tmp[0];
      }
      if (rxIsNumInt(rxControl[Rxc_length_out])){
				tmpI = asIv(rxControl[Rxc_length_out], "length.out");
				if (tmpI.size() != 1){
					rxSolveFree();
					stop(_("'length.out' must be of length 1"));
				}
				lenOut = tmpI[0];
				if (!ISNA(by)){
					// Matches seq(0,1,by=0.1,length.out=3)
					rxSolveFree();
					stop(_("cannot use both 'by' and 'length.out' for rxode2 simulations"));
				}
				by = (to-from)/(lenOut-1);
      } else if (ISNA(by)) {
				lenOut=200;
				by = (to-from)/(lenOut-1);
      } else {
				lenOut= (int)((to-from)/by+1.0);
      }
      NumericVector newObs(lenOut);
      // ((to - from)/(length.out - 1))
      List et = as<List>(ev1);
      for (int i = lenOut; i--;){
				newObs[i]=by*i+from;
      }
      rx->nobs2 = lenOut;
      ev1 = et_(List::create(newObs), as<List>(ev1));
    }
  }
  if (rxIs(ev1, "data.frame") && !rxIs(ev1, "rxEtTrans")){
    ev1 = as<List>(etTrans(as<List>(ev1), obj, rxSolveDat->hasCmt,
													 false, false, true, R_NilValue,
													 rxControl[Rxc_keepF]));
    rxSolveDat->labelID=true;
    CharacterVector tmpC = ev1.attr("class");
    List tmpL = tmpC.attr(".rxode2.lst");
    rxSolveDat->idLevels = asCv(tmpL[RxTrans_idLvl], "idLvl");
    List keep = tmpL[RxTrans_keepL];
    _rxModels[".fkeep"] = keep;
    keepFcov=keep;
    rx->nKeepF = keepFcov.size();
    rxcEvid = 2;
    rxcTime = 1;
    rxcAmt  = 3;
    rxcId   = 0;
    rxcDv   = 5;
    rxcIi   = 4;
    int censAdd = as<int>(tmpL["censAdd"]);
    int limitAdd = as<int>(tmpL["limitAdd"]);
    if (censAdd == 1 && limitAdd == 1) {
      rxcCens = 6;
      rxcLimit = 7;
    } else if (censAdd == 1){
      rxcCens = 6;
      rxcLimit = -1;
    } else if (limitAdd == 1){
      rxcCens  = -1;
      rxcLimit = 6;
    } else {
      rxcCens  = -1;
      rxcLimit = -1;
    }
  }
  rx->hasFactors=0;
  if (rxIs(ev1, "rxEtTran")){
    CharacterVector cls = ev1.attr("class");
    List tmpL = cls.attr(".rxode2.lst");
    rx->nobs2 = asInt(tmpL[RxTrans_nobs], "nobs");
    rxSolveDat->convertInt = (asInt(tmpL[RxTrans_idInfo], "idInfo")==1);
    CharacterVector clsEt = Rf_getAttrib(ev1, R_ClassSymbol);
    List e   = clsEt.attr(".rxode2.lst");
    // SETUP factors for ID=="" and CMT="" etc
    lineIni(&(rx->factors));
    lineIni(&(rx->factorNames));
    SEXP idLvl = e[RxTrans_idLvl];
    // Extract ID levels so you can be more explicit in errors/call ID==""
    int len = Rf_length(idLvl);
    // ID factors are always present in etTrans
    addLine(&(rx->factorNames), "%s", "ID");
    for (int i = 0; i < len; i++) {
      addLine(&(rx->factors), "%s", CHAR(STRING_ELT(idLvl, i)));
    }
    rx->factorNs[rx->hasFactors++] = len;
    // CMT may or may not be present, but the compartment info is present
    SEXP cmtLvl = e[RxTrans_cmtInfo];
    len = Rf_length(cmtLvl);
    addLine(&(rx->factorNames), "%s", "CMT");
    for (int i = 0; i < len; i++) {
      addLine(&(rx->factors), "%s",
							CHAR(STRING_ELT(cmtLvl, i)));
    }
    rx->factorNs[rx->hasFactors++] = len;
    List lvlInfo = e[RxTrans_levelInfo];
    SEXP lvlInfoN = lvlInfo.attr("names");
    for (int i = Rf_length(lvlInfoN);i--;){
      SEXP cur = lvlInfo[i];
      if (!Rf_isNull(cur)){
				len = Rf_length(cur);
				addLine(&(rx->factorNames), "%s",
								CHAR(STRING_ELT(lvlInfoN, i)));
				for (int j = 0; j < len; ++j) {
					addLine(&(rx->factors), "%s",
									CHAR(STRING_ELT(cur, j)));
				}
				rx->factorNs[rx->hasFactors++] = len;
				if (rx->hasFactors >= 500){
					rxSolveFree();
					stop(_("rxode2 only supports 500 factors"));
				}
      }
    }
  }
  _rxModels[".lastEv1"] = ev1;
}

// This function simulates individual parameter values and residual
// parameter values and then converts them to a data.frame.  This
// allows rxSolve_ to solve as if the user specified these parameters
// directly
static inline void rxSolve_simulate(const RObject &obj,
                                    const List &rxControl,
                                    const Nullable<CharacterVector> &specParams,
                                    const Nullable<List> &extraArgs,
                                    const RObject &params,
                                    const RObject &ev1,
                                    const RObject &inits,
                                    rxSolve_t* rxSolveDat){
  rx_solve* rx = getRxSolve_();
  rx_solving_options* op = rx->op;

  RObject omega = rxControl[Rxc_omega];
  Nullable<NumericVector> omegaDf = asNNv(rxControl[Rxc_omegaDf], "omegaDf");
  bool omegaIsChol = asBool(rxControl[Rxc_omegaIsChol], "Rxc_omegaIsChol");

  LogicalVector simVariability = rxControl[Rxc_simVariability];

  Nullable<NumericMatrix> thetaMat = as<Nullable<NumericMatrix>>(rxControl[Rxc_thetaMat]);
  Nullable<NumericVector> thetaDf = asNNv(rxControl[Rxc_thetaDf], "thetaDf");
  bool thetaIsChol = asBool(rxControl[Rxc_thetaIsChol], "thetaIsChol");

  RObject sigma= rxControl[Rxc_sigma];
  Nullable<NumericVector> sigmaDf= asNNv(rxControl[Rxc_sigmaDf], "sigmaDf");
  bool sigmaIsChol= asBool(rxControl[Rxc_sigmaIsChol], "sigmaIsChol");

  op->isChol = (int)(sigmaIsChol);
  SEXP tmp = rxControl[Rxc_linDiff];
  LogicalVector linLV;
  if (Rf_isLogical(tmp)) {
    linLV = as<LogicalVector>(tmp);
  }
  tmp = rxControl[Rxc_linDiffCentral];
  NumericVector linNV;
  if (Rf_isReal(tmp)) {
    linNV = as<NumericVector>(tmp);
  }
  //LogicalVector
  if (linLV.containsElementNamed("tlag")) {
    op->cTlag = as<bool>(linLV["tlag"]);
  } else {
    op->cTlag = true;
  }
  if (linNV.containsElementNamed("tlag")){
    op->hTlag = as<double>(linNV["tlag"]);
  } else {
    op->hTlag = 1.5e-5;
  }
  if (linLV.containsElementNamed("f")) {
    op->cF = as<bool>(linLV["f"]);
  } else {
    op->cF = true;
  }
  if (linNV.containsElementNamed("f")) {
    op->hF = as<double>(linNV["f"]);
  } else {
    op->hF = 1.5e-5;
  }
  if (linLV.containsElementNamed("rate")) {
    op->cRate = as<bool>(linLV["rate"]);
  } else {
    op->cRate = true;
  }
  if (linNV.containsElementNamed("rate")) {
    op->hRate = as<double>(linNV["rate"]);
  } else {
    op->hRate = 1.5e-5;
  }
  if (linLV.containsElementNamed("dur")) {
    op->cDur = as<bool>(linLV["dur"]);
  } else {
    op->cDur = false;
  }
  if (linNV.containsElementNamed("dur")) {
    op->hDur = as<double>(linNV["dur"]);
  } else {
    op->hDur = 1.5e-5;
  }
  if (linLV.containsElementNamed("tlag2")) {
    op->cTlag2 = as<bool>(linLV["tlag2"]);
  } else {
    op->cTlag2 = false;
  }
  if (linNV.containsElementNamed("tlag2")) {
    op->hTlag2 = as<double>(linNV["tlag2"]);
  } else {
    op->hTlag2 = 1.5e-5;
  }
  if (linLV.containsElementNamed("f2")) {
    op->cF2 = as<bool>(linLV["f2"]);
  } else {
    op->cF2 = false;
  }
  if (linNV.containsElementNamed("f2")) {
    op->hF2 = as<double>(linNV["f2"]);
  } else {
    op->hF2 = 1.5e-5;
  }
  if (linLV.containsElementNamed("rate2")) {
    op->cRate2 = as<bool>(linLV["rate2"]);
  } else {
    op->cRate2 = false;
  }
  if (linNV.containsElementNamed("rate2")) {
    op->hRate2 = as<double>(linNV["rate2"]);
  } else {
    op->hRate2 = 1.5e-5;
  }
  if (linLV.containsElementNamed("dur2")) {
    op->cDur2 = as<bool>(linLV["dur2"]);
  } else {
    op->cDur2 = false;
  }
  if (linNV.containsElementNamed("dur2")) {
    op->hDur2 = as<double>(linNV["dur2"]);
  } else {
    op->hDur2 = 1.5e-5;
  }
  unsigned int nSub = asUnsignedInt(rxControl[Rxc_nSub], "nSub");
  unsigned int nStud = asUnsignedInt(rxControl[Rxc_nStud], "nStud");
  double dfSub=asDouble(rxControl[Rxc_dfSub], "dfSub");
  double dfObs=asDouble(rxControl[Rxc_dfObs], "dfObs");

  bool simVar;
  if (simVariability[0] == NA_LOGICAL) {
    simVar = nStud > 1;
  } else {
    simVar = simVariability[0];
  }


  unsigned int nCoresRV = asUnsignedInt(rxControl[Rxc_nCoresRV], "nCoresRV");

  bool simSubjects = false;

  op->ncoresRV = nCoresRV;
  rx->nevid9 = 0;

  if (!thetaMat.isNull() || !rxIsNull(omega) || !rxIsNull(sigma)){
    // Simulated Variable3
    bool cbindPar1 = false;
    if (!rxIsNum(rxSolveDat->par1)){
      if (!thetaMat.isNull()) {
        rxSolveFree();
        stop(_("when specifying 'thetaMat' the parameters cannot be a 'data.frame'/'matrix'."));
      } else {
        cbindPar1 = true;
      }
    }
    unsigned int nSub0 = 0;
    int curObs = 0;
    rx->nall = 0;
    rx->nobs = 0;
    rx->nobs2 = 0;
    if (rxIs(ev1,"event.data.frame")||
        rxIs(ev1,"event.matrix")){
      if (rxcId > -1){
        DataFrame dataf = as<DataFrame>(ev1);
        IntegerVector id = as<IntegerVector>(dataf[rxcId]);
        IntegerVector evid  = as<IntegerVector>(dataf[rxcEvid]);
        int lastid= id[id.size()-1]+42;
        rx->nall = evid.size();
        int evid9=0;
        for (unsigned int j = rx->nall; j--;){
          if (lastid != id[j]){
            lastid=id[j];
            nSub0++;
          }
          if (isObs(evid[j])) rx->nobs++;
          if (evid[j] == 0) rx->nobs2++;
          if (evid[j] == 9) evid9++;
        }
        rx->nevid9 = evid9;
      } else {
        nSub0 =1;
        DataFrame dataf = as<DataFrame>(ev1);
        IntegerVector evid  = as<IntegerVector>(dataf[rxcEvid]);
        rx->nall = evid.size();
        int evid9=0;
        for (unsigned int j =rx->nall; j--;){
          if (isObs(evid[j])) rx->nobs++;
          if (evid[j] == 0) rx->nobs2++;
          if (evid[j] == 9) evid9++;
        }
        rx->nevid9= evid9;
      }
    }
    if (simVar && nSub0 == nSub*nStud){
    } else if (nSub > 1 && nSub0 > 1 && nSub != nSub0){
      rxSolveFree();
      stop(_("provided multi-subject data (n=%d) trying to simulate a different number of subjects (n=%d)"), nSub0, nSub);
    } else if (nSub > 1 && nSub0 == 1) {
      nSub0 = nSub;
      simSubjects = true;
    } else if (nSub >= 1 && nSub0 == 1 && !rxIsNull(omega)) {
      nSub0 = nSub;
      simSubjects = true;
    }
    if (rxSolveDat->addDosing.isNull()){
      // only evid=0
      curObs= rx->nobs2;
    } else {
      LogicalVector addDosing1 = as<LogicalVector>(rxSolveDat->addDosing);
      if (LogicalVector::is_na(addDosing1[0])){
        curObs = rx->nall - rx->nevid9;
      } if (addDosing1[0]){
        curObs = rx->nall - rx->nevid9;
      } else {
        curObs = rx->nobs - rx->nevid9;
      }
    }
    if (rxIs(as<RObject>(thetaMat), "matrix")){
      if (!thetaIsChol){
        arma::mat tmpM = as<arma::mat>(thetaMat);
        if (tmpM.is_zero()) {
          setZeroMatrix(1);
        } else if (!tmpM.is_sympd()){
          rxSolveFree();
          stop(_("'thetaMat' must be symmetric"));
        }
      }
    }
    Nullable<NumericVector> params0 = R_NilValue;
    if (!cbindPar1) {
      params0 = as<Nullable<NumericVector>>(rxSolveDat->par1);
    }
    List lst = rxSimThetaOmega(params0,
                               omega,
                               omegaDf,
                               asNv(rxControl[Rxc_omegaLower], "omegaLower"),
                               asNv(rxControl[Rxc_omegaUpper], "omegaUpper"),
                               omegaIsChol,
                               asStr(rxControl[Rxc_omegaSeparation], "omegaSeparation"),
                               asInt(rxControl[Rxc_omegaXform], "omegaXform"),
                               nSub0, thetaMat,
                               asNv(rxControl[Rxc_thetaLower], "thetaLower"),
                               asNv(rxControl[Rxc_thetaUpper], "thetaUpper"),
                               thetaDf, thetaIsChol, nStud,
                               sigma,
                               asNv(rxControl[Rxc_sigmaLower], "sigmaLower"),
                               asNv(rxControl[Rxc_sigmaUpper], "sigmaUpper"),
                               sigmaDf, sigmaIsChol,
                               asStr(rxControl[Rxc_sigmaSeparation], "sigmaSeparation"),
                               asInt(rxControl[Rxc_sigmaXform], "sigmaXform"),
                               nCoresRV, curObs,
                               dfSub, dfObs, simSubjects,
                               simVariability);
    if (cbindPar1) {
      lst = cbindThetaOmega(rxSolveDat->par1, lst);
    }
    rxSolveDat->warnIdSort = false;
    rxSolveDat->par1 =  as<RObject>(lst);
    rxSolveDat->usePar1=true;
    // The parameters are in the same format as they would be if they were
    // specified as part of the original dataset.
  }
}

// This will setup the parNumeric, parDf, or parMat for solving. It
// will also set the parameter type.
static inline void rxSolve_parSetup(const RObject &obj,
                                    const List &rxControl,
                                    const Nullable<CharacterVector> &specParams,
                                    const Nullable<List> &extraArgs,
                                    const CharacterVector& pars,
                                    const RObject &ev1,
                                    const RObject &inits,
                                    rxSolve_t* rxSolveDat){
  //  determine which items will be sampled from
  if (rxIsNumInt(rxSolveDat->par1)){
    rxSolveDat->parNumeric = as<NumericVector>(rxSolveDat->par1);
    if (rxSolveDat->parNumeric.hasAttribute("names")){
      rxSolveDat->nmP = rxSolveDat->parNumeric.names();
    } else if (rxSolveDat->parNumeric.size() == pars.size()) {
      rxSolveDat->nmP = pars;
    } else {
      rxSolveFree();
      stop(_("if parameters are not named, they must match the order and size of the parameters in the model"));
    }
  } else if (rxIs(rxSolveDat->par1, "data.frame")) {
    rxSolveDat->parDf = as<DataFrame>(rxSolveDat->par1);
    rxSolveDat->parType = 2;
    rxSolveDat->nmP = rxSolveDat->parDf.names();
    rxSolveDat->nPopPar = rxSolveDat->parDf.nrows();
  } else if (rxIs(rxSolveDat->par1, "matrix")) {
    rxSolveDat->parMat = as<NumericMatrix>(rxSolveDat->par1);
    rxSolveDat->nPopPar = rxSolveDat->parMat.nrow();
    rxSolveDat->parType = 3;
    if (rxSolveDat->parMat.hasAttribute("dimnames")){
      Nullable<CharacterVector> colnames0 = as<Nullable<CharacterVector>>((as<List>(rxSolveDat->parMat.attr("dimnames")))[1]);
      if (colnames0.isNull()){
        if (rxSolveDat->parMat.ncol() == pars.size()){
          rxSolveDat->nmP = pars;
        } else {
          rxSolveFree();
          stop(_("if parameters are not named, they must match the order and size of the parameters in the model"));
        }
      } else {
        rxSolveDat->nmP = CharacterVector(colnames0);
      }
    } else if (rxSolveDat->parMat.ncol() == pars.size()) {
      rxSolveDat->nmP = pars;
    } else {
      rxSolveFree();
      stop(_("if parameters are not named, they must match the order and size of the parameters in the model"));
    }
  }
}

extern "C" void setupRxInd(rx_solving_options_ind* ind, int first) {
  ind->_newind		= -1;
  ind->allCovWarn	= 0;
  ind->bT		= NA_REAL;
  ind->cacheME		= 0;
  ind->doSS		= 0;
  ind->dosenum		= 0;
  ind->err		= 0;
  ind->idx		= 0;
  ind->ixds		= 0;
  ind->lambda		= 1.0;
  ind->solved		= -1;
  ind->tfirst		= NA_REAL;
  ind->tlast		= NA_REAL;
	ind->curDose  = NA_REAL;
  ind->yj		= 0;
  ind->logitLow         = 0;
  ind->logitHi          = 1;
  ind->isIni            = 0;
  ind->_update_par_ptr_in = 0;
  if (first){
    ind->solveTime	= 0.0;
    ind->nBadDose	= 0;
    ind->wrongSSDur	= 0;
  }
}

// This loops through the data to put each individual into the
// approiate data structure.
// At the same time calculate hmax per individual as well
// in the overall population.
static inline void rxSolve_datSetupHmax(const RObject &obj, const List &rxControl,
																				const Nullable<CharacterVector> &specParams,
																				const Nullable<List> &extraArgs,
																				CharacterVector& pars,
																				const RObject &ev1,
																				const RObject &inits,
																				rxSolve_t* rxSolveDat){
  Nullable<NumericVector> hmax = asNNv(rxControl[Rxc_hmax], "hmax");
  bool doMean=true;
  rx_solve* rx = getRxSolve_();
  rx_solving_options* op = rx->op;
  rx_solving_options_ind* ind;
  double tmp, hmax1 = 0.0, hmax1m = 0.0, hmax1mo, hmax1s=0.0,
		hmax1n=0.0, hmax2 = 0.0, hmax2m = 0.0, hmax2mo, hmax2s=0.0,
		hmax2n=0.0, tlast;
  int i, j;
  unsigned int nsub = 0;
  unsigned int nobs = 0, ndoses = 0;

  int ncov =0, curcovi = 0;

  if (rxIs(ev1,"event.data.frame")||
      rxIs(ev1,"event.matrix")){
    // data.frame or matrix
    double hmax0 = 0.0;
    if (!hmax.isNull()){
      NumericVector hmax00(hmax);
      hmax0 = hmax00[0];
      if (NumericVector::is_na(hmax00[0])){
				hmax0 = 0.0;
				doMean = true;
      } else {
				hmax0 = hmax00[0];
      }
    }
    DataFrame dataf = as<DataFrame>(ev1);
    CharacterVector dfNames = dataf.names();
    int dfN = dfNames.size();
    IntegerVector evid  = as<IntegerVector>(dataf[rxcEvid]);
    if (_globals.gevid != NULL) free(_globals.gevid);
    _globals.gevid = (int*)calloc(3*evid.size()+dfN, sizeof(int));
    if (_globals.gevid == NULL){
      rxSolveFree();
      stop(_("can not allocate enough memory to load 'evid'"));
    }
    std::copy(evid.begin(),evid.end(), &_globals.gevid[0]);
    _globals.gidose = _globals.gevid + evid.size();
    _globals.gcens = _globals.gidose + evid.size();
    _globals.gpar_cov = _globals.gidose + evid.size();//[dfN];

    int ntot = 1;

    IntegerVector id(evid.size(), 1);
    if (rxcId > -1){
      id    = as<IntegerVector>(dataf[rxcId]);
      int lastid = NA_INTEGER;
      int nid = 0;
      for (int ii = 0; ii < id.size(); ii++){
				if (id[ii] != lastid){
					lastid = id[ii];
					nid++;
				}
      }
      // if (nid == 0){
      // } else
      if (nid == rxSolveDat->nPopPar || rxSolveDat->nPopPar == 1){
				ntot = nid;
      } else {
				ntot = nid*rxSolveDat->nPopPar;
      }
    } else {
      if (rxSolveDat->nPopPar != 0) ntot = rxSolveDat->nPopPar;
    }

    NumericVector time0 = dataf[rxcTime];
    // Get the range
    range_d(REAL(dataf[rxcTime]), time0.size(), &(rx->minD), &(rx->maxD));

    if (rxIs(time0, "units")){
      rxSolveDat->addTimeUnits=true;
      rxSolveDat->timeUnitsU=time0.attr("units");
    }

    tlast = time0[0];
    // - all_times
    if (ntot <= 0) {
      rxSolveFree();
      stop(_("nothing to solve"));
    }
    rxOptionsIniEnsure(ntot);
    if (_globals.gall_times != NULL) free(_globals.gall_times);
    _globals.gall_times = (double*)calloc(6*time0.size(), sizeof(double));
    std::copy(time0.begin(), time0.end(), &_globals.gall_times[0]);
    _globals.gall_times2 = _globals.gall_times + time0.size();
    _globals.gdv = _globals.gall_times2 + time0.size(); // Perhaps allocate zero size if missing?
    _globals.gamt = _globals.gdv + time0.size();
    _globals.gii = _globals.gamt + time0.size();
    _globals.glimit=_globals.gii + time0.size();
    NumericVector dv;
    if (rxcDv > -1){
      dv = as<NumericVector>(dataf[rxcDv]);
      std::copy(dv.begin(), dv.end(), &_globals.gdv[0]);
    }
    IntegerVector cens;
    rx->cens=0;
    if (rxcCens > -1){
      cens = as<IntegerVector>(dataf[rxcCens]);
      std::copy(cens.begin(), cens.end(), &_globals.gcens[0]);
      rx->cens=1;
    }
    NumericVector limit;
    rx->limit=0;
    if (rxcLimit > -1){
      rx->limit=1;
      limit = as<NumericVector>(dataf[rxcLimit]);
      std::copy(limit.begin(), limit.end(), &_globals.glimit[0]);
    }
    NumericVector amt   = dataf[rxcAmt];
    NumericVector datIi(amt.size());
    if (rxcIi > -1){
      datIi = as<NumericVector>(dataf[rxcIi]);
    } else {
      std::fill_n(datIi.begin(), datIi.size(), 0.0);
    }
    ncov = 0;
    std::vector<int> covPos(dfN);
    curcovi=0;
    for (i = dfN; i--;){
      for (j = rxSolveDat->npars; j--;){
				if (pars[j] == dfNames[i]){
					_globals.gpar_cov[ncov] = j+1;
					covPos[ncov] = i;
					ncov++;
				}
      }
    }
    // Make sure the covariates are a #ncov * all times size
    if (_globals.gcov != NULL) free(_globals.gcov);
    _globals.gcov = (double*)calloc(ncov * amt.size(), sizeof(double));
    if (_globals.gcov == NULL){
      rxSolveFree();
      stop(_("can not allocate memory for the covariates"));
    }
    op->par_cov=&(_globals.gpar_cov[0]);
    op->ncov=ncov;
    op->do_par_cov = (ncov > 0);
    // Make sure the covariates are a #ncov * all times size
    int ids = id.size();
    // Get the number of subjects
    // Get the number of observations
    // Get the number of doses
    int nall = 0, nobst=0, lasti =0, ii=0, nobs2t=0, nevid9=0;
    nsub = 0;
    ind = &(rx->subjects[0]);
    setupRxInd(ind, 1);
    j=0;
    rx->maxAllTimes=0;
    int lastId = id[0]-42;
    for (i = 0; i < ids; i++) {
      if (lastId != id[i]) {
				if (nall != 0) {
					// Finalize last solve.
					ind->n_all_times    = ndoses+nobs;
					if (ind->n_all_times > rx->maxAllTimes) rx->maxAllTimes= ind->n_all_times;
					ind->cov_ptr = &(_globals.gcov[curcovi]);
					for (ii = 0; ii < ncov; ii++){
						NumericVector cur = as<NumericVector>(dataf[covPos[ii]]);
						std::copy(cur.begin()+lasti, cur.begin()+lasti+ind->n_all_times,
											_globals.gcov+curcovi);
						curcovi += ind->n_all_times;
					}
					nsub++;
					if (doMean){
						hmax1s = hmax1s/(hmax1n-1);
						hmax1  = hmax1m;
						ind->HMAX = hmax1;
					} else if (hmax0 == 0.0){
						ind->HMAX = hmax1;
					} else {
						ind->HMAX = hmax0;
					}
					ind = &(rx->subjects[nsub]);
					setupRxInd(ind, 1);
				}
				// Setup the pointers.
				ind->id             = nsub+1;
				ind->idReal         = id[i];
				ind->all_times   = &_globals.gall_times[i];
				ind->dv = &_globals.gdv[i];
				ind->limit = &_globals.glimit[i];
				ind->cens = &_globals.gcens[i];
				ind->evid           = &_globals.gevid[i];
				ind->idose          = &_globals.gidose[i];
				ind->dose           = &_globals.gamt[i];
				ind->ii             = &_globals.gii[i];
				lasti = i;

				hmax1m=0.0;
				hmax1s=0.0;
				hmax1n=0.0;
				hmax1 = 0.0;
				lastId=id[i];
				j=i;
				ind->ndoses=0;
				ind->nevid2=0;
				ndoses=0;
				nobs=0;
				tlast = NA_REAL;
      }
      // Create index
      _globals.gii[i] = datIi[i];
      _globals.gamt[i] = amt[i];

      if (isDose(_globals.gevid[i])){
				_globals.gidose[j] = i-lasti;
				ind->ndoses++;
				ndoses++; nall++; j++;
      } else {
				nobs++; nobst++; nall++;
				if (_globals.gevid[i] == 2) {
					ind->nevid2++;
					rx->hasEvid2 = 1;
				}
				if (_globals.gevid[i] == 0) nobs2t++;
				if (_globals.gevid[i] == 9) nevid9++;
				if (!ISNA(tlast)){
					tmp = time0[i]-tlast;
					if (tmp < 0){
						rxSolveFree();
						stop(_("data must be ordered by 'ID' and 'TIME' variables"));
					}
					hmax1n++;
					hmax1mo = hmax1m;
					hmax1m += (tmp-hmax1m)/hmax1n;
					hmax1s += (tmp-hmax1m)*(tmp-hmax1mo);
					hmax2n++;
					hmax2mo = hmax2m;
					hmax2m += (tmp-hmax2m)/hmax2n;
					hmax2s += (tmp-hmax2m)*(tmp-hmax2mo);
					if (tmp > hmax1){
						hmax1 = tmp;
						if (hmax1 > hmax2){
							hmax2=hmax1;
						}
					}
				}
      }
      tlast = time0[i];
    }
    if (doMean){
      hmax2  = hmax2m;
    }
    rx->nobs = nobst;
    rx->nobs2 = nobs2t;
    rx->nall = nall;
    rx->nevid9 = nevid9;
    // Finalize the prior individual
    ind->n_all_times    = ndoses+nobs;
    if (ind->n_all_times > rx->maxAllTimes) rx->maxAllTimes= ind->n_all_times;
    ind->cov_ptr = &(_globals.gcov[curcovi]);
    for (ii = 0; ii < ncov; ii++){
      NumericVector cur = as<NumericVector>(dataf[covPos[ii]]);
      std::copy(cur.begin()+lasti, cur.begin()+lasti+ind->n_all_times,
								_globals.gcov+curcovi);
      curcovi += ind->n_all_times;
    }
    if (doMean || hmax0 == 0.0){
      op->hmax2 = hmax2;
    } else {
      op->hmax2 = hmax0;
    }
    nsub++;
    rx->nsub= nsub;
    if (doMean){
      hmax1s = hmax1s/(hmax1n-1);
      hmax1  = hmax1m;
      ind->HMAX = hmax1 + asDouble(rxControl[Rxc_hmaxSd], "hmaxSd")*sqrt(hmax1s);
    } else if (hmax0 == 0.0){
      ind->HMAX = hmax1;
    } else {
      ind->HMAX = hmax0;
    }
  }
}

// Setup the parameter order and covariate from the dataset and input parameters
static inline void rxSolve_parOrder(const RObject &obj, const List &rxControl,
																		const Nullable<CharacterVector> &specParams,
																		const Nullable<List> &extraArgs,
																		CharacterVector& pars,
																		const RObject &ev1,
																		const RObject &inits,
																		rxSolve_t* rxSolveDat){
  rx_solve* rx = getRxSolve_();
  rx_solving_options* op = rx->op;
  if (_globals.gParPos != NULL) free(_globals.gParPos);
  _globals.gParPos = (int*)calloc(rxSolveDat->npars*2 + rxSolveDat->sigmaN.size() + rxSolveDat->omegaN.size(), sizeof(int));// [npars]
  if (_globals.gParPos == NULL){
    rxSolveFree();
    stop(_("cannot allocate enough memory to sort input parameters"));
  }
  _globals.gParPos2 =  _globals.gParPos + rxSolveDat->npars; // [npars]
  _globals.gsvar =  _globals.gParPos2 + rxSolveDat->npars;//[sigmaN.size()]
  _globals.govar =  _globals.gsvar + rxSolveDat->sigmaN.size(); // [omegaN.size()]
  std::string errStr = "";
  bool allPars = true;
  bool curPar = false;
  CharacterVector mvIniN = rxSolveDat->mvIni.names();
  CharacterVector mvCov1N;
  if (rxIs(ev1, "rxEtTran")){
    CharacterVector tmpCls = ev1.attr("class");
    List e = tmpCls.attr(".rxode2.lst");
    List tmpCov1 = e[RxTrans_cov1];
    mvCov1N = tmpCov1.attr("names");
  }
  int i, j;
  for (i = rxSolveDat->npars; i--;){
    curPar = false;
    // Check for the omega-style simulated parameters.
    for (j = rxSolveDat->omegaN.size(); j--;){
      if (rxSolveDat->omegaN[j] == pars[i]){
				_globals.govar[j] = i;
				break;
      }
    }
    // Check to see if this is a covariate.
    for (j = op->ncov; j--;){
      if (_globals.gpar_cov[j] == (int)(i + 1)){
				_globals.gParPos[i] = 0; // These are set at run-time and "dont" matter.
				curPar = true;
				rxSolveDat->eGparPos[i]=_globals.gParPos[i];
				break;
      }
    }
    // Check for the sigma-style simulated parameters.
    if (!curPar){
      for (j = rxSolveDat->sigmaN.size(); j--;){
				if (rxSolveDat->sigmaN[j] == pars[i]){
					_globals.gsvar[j] = i;
					rxSolveDat->nsvar++;
					_globals.gParPos[i] = 0; // These are set at run-time and "dont" matter.
					curPar = true;
					rxSolveDat->eGparPos[i]=_globals.gParPos[i];
					break;
				}
      }
    }
    // Next, check to see if this is a user-specified parameter
    if (!curPar){
      for (j = rxSolveDat->nmP.size(); j--;){
				if (rxSolveDat->nmP[j] == pars[i]){
					curPar = true;
					_globals.gParPos[i] = j + 1;
					rxSolveDat->eGparPos[i]=_globals.gParPos[i];
					break;
				}
      }
    }
    // last, check for $ini values
    if (!curPar){
      for (j = mvIniN.size(); j--;){
				if (mvIniN[j] == pars[i]){
					curPar = true;
					_globals.gParPos[i] = -j - 1;
					rxSolveDat->eGparPos[i]=_globals.gParPos[i];
					break;
				}
      }
    }
    if (!curPar){
      for (j = 1; j < mvCov1N.size(); j++){
				if (mvCov1N[j] == pars[i]){
					// These are setup once and don't need to be updated
					_globals.gParPos[i] = 0; // These are set at run-time and "dont" matter.
					curPar = true;
					rxSolveDat->eGparPos[i]=_globals.gParPos[i];
				}
      }
    }
    if (!curPar){
      if (errStr == ""){
				errStr = "The following parameter(s) are required for solving: " + pars[i];
      } else {
				errStr = errStr + ", " + pars[i];
      }
      allPars = false;
    }
  }
  if (!allPars){
    CharacterVector modSyntax = rxSolveDat->mv[RxMv_model];
    Rcout << "Model:\n\n" + modSyntax[0] + "\n";
    rxSolveFree();
    stop(errStr);
  }
}

static inline void rxSolve_assignGpars(rxSolve_t* rxSolveDat);

static inline void rxSolve_resample(const RObject &obj,
																		const List &rxControl,
																		const Nullable<CharacterVector> &specParams,
																		const Nullable<List> &extraArgs,
																		const RObject &params, const RObject &events,
																		const RObject &inits,
																		rxSolve_t* rxSolveDat){
  rx_solve* rx = getRxSolve_();
  rx_solving_options* op = rx->op;
  rx->sample = false;
  if (!Rf_isNull(rxControl[Rxc_resample])) {
    SEXP sampleVars = rxControl[Rxc_resample];
    bool doSample = true;
    CharacterVector pars = rxSolveDat->mv[RxMv_params];
    if (TYPEOF(sampleVars) == LGLSXP) {
      doSample = asBool(rxControl[Rxc_resample], "resample");
      if (doSample){
				CharacterVector vars(op->ncov+rx->nCov0);
				int *par_cov = op->par_cov;
				int jj = 0;
				for (int i = 0; i < op->ncov; i++){
					vars[jj++] = pars[par_cov[i]-1];
				}
				par_cov = rx->cov0;
				for (int i = 0; i < rx->nCov0; i++){
					vars[jj++] = pars[par_cov[i]];
				}
				sampleVars = wrap(vars);
      }
    } else if (TYPEOF(sampleVars) != STRSXP){
      rxSolveFree();
      stop(_("'resample' must be NULL or a character vector"));
    }
    if (doSample){
      bool resampleID = asBool(rxControl[Rxc_resampleID], "resampleID");
      rx->sample = true;
      if (rx->par_sample != NULL) free(rx->par_sample);
      rx->par_sample = (int*)calloc(pars.size(), sizeof(int));
      for (int ip = rxSolveDat->npars; ip--;){
				for (int is = Rf_length(sampleVars); is--;){
					if (!strcmp(pars[ip],CHAR(STRING_ELT(sampleVars, is)))){
						rx->par_sample[ip] = 1;
						break;
					}
				}
      }
      List parList;
      CharacterVector parNames;
      CharacterVector keepNames;
      List parListF;
      bool updatePar=false;
      if (rxSolveDat->usePar1 && (rxSolveDat->par1).sexp_type()==VECSXP) {
				parList = as<List>(rxSolveDat->par1);
				parNames = parList.names();
				parListF = clone(parList);
				updatePar = true;
      }
      int nrow = rxSolveDat->npars;
      int ncol = rx->nsub;
      int size = rx->nsub*rx->nsim;
      NumericMatrix iniPars(nrow, ncol,&_globals.gpars[0]);
      NumericMatrix ret(nrow, size);
      IntegerVector idSel(size);
      std::fill(idSel.begin(),idSel.end(),0);
      const char *cur;
      // add sample indicators
      if (_globals.gSampleCov != NULL) free(_globals.gSampleCov);
      _globals.gSampleCov = (int*)calloc(op->ncov*rx->nsub*rx->nsim, sizeof(int));
      for (int ir = nrow; ir--;){
				// For sampling  (with replacement)
				if (rx->par_sample[ir]) {
					cur = CHAR(pars[ir]);
					for (int is = size; is--;){
						int ic = is % ncol;
						int val;
						// Retrieve or generate sample
						// Only need to resample from the number of input items (ncol)
						// Probably doesn't make much difference, though.
						if (resampleID) {
							val = idSel[is];
							if (val == 0){
								val = (int)(unif_rand()*ncol)+1;
								idSel[is] = val;
								// Fill in the selected ID for the time-varying covariate(s)
								// This is filled in reverse order because ind->cov_sample is filled in reverse order
								std::fill_n(&_globals.gSampleCov[0]+(size-is-1)*op->ncov, op->ncov, val);
							}
							val--;
						} else {
							val = (int)(unif_rand()*ncol);
						}
						for (int ip = parList.size(); ip--;){
							if (!strcmp(CHAR(parNames[ip]), cur)) {
								SEXP curIn = parList[ip];
								SEXP curOut = parListF[ip];
								if (TYPEOF(curIn) == INTSXP) {
									INTEGER(curOut)[ic] = INTEGER(curIn)[val];
								} else if (TYPEOF(curIn) == REALSXP){
									REAL(curOut)[ic] = REAL(curIn)[val];
								}
								break;
							}
						}
						ret(ir, is) = iniPars(ir, val);
					}
				} else {
					// This is for copying
					for (int is = size; is--;){
						ret(ir, is) = iniPars(ir, is % ncol);
					}
				}
      }
      if (updatePar){
				rxSolveDat->par1 = as<RObject>(parListF);
      }
      // Put sampled dataset in gpars
      std::copy(ret.begin(),ret.end(),&_globals.gpars[0]);
    }
  }
}

// Normalizes parameter input
// It will also load the parameters into the solving data structure.
static inline void rxSolve_normalizeParms(const RObject &obj, const List &rxControl,
																					const Nullable<CharacterVector> &specParams,
																					const Nullable<List> &extraArgs,
																					const CharacterVector& pars,
																					const RObject &ev1,
																					const RObject &inits,
																					rxSolve_t* rxSolveDat) {
  int i;
  rx_solve* rx = getRxSolve_();
  rx_solving_options* op = rx->op;
  rx_solving_options_ind* ind;
  int curEvent = 0, curIdx = 0, curSolve=0;
  switch(rxSolveDat->parType){
  case 1: // NumericVector
    {
      if (rxSolveDat->nPopPar != 1) {
				rxSolveFree();
				stop(_("nPopPar != 1 but parameters are specified as a NumericVector"));
      }
      // Convert to DataFrame to simplify code.
      List parDfL(rxSolveDat->parNumeric.size());
      for (i = rxSolveDat->parNumeric.size(); i--;){
				parDfL[i] = NumericVector(rx->nsub,rxSolveDat->parNumeric[i]);
      }
      parDfL.attr("names") = rxSolveDat->nmP;
      parDfL.attr("row.names") = IntegerVector::create(NA_INTEGER,-rx->nsub);
      parDfL.attr("class") = CharacterVector::create("data.frame");
      rxSolveDat->parDf = as<DataFrame>(parDfL);
      rxSolveDat->parType = 2;
      rxSolveDat->nPopPar = rxSolveDat->parDf.nrows();
      rx->nsim=1;
    }
  case 2: // DataFrame
    // Convert to NumericMatrix
    {
      unsigned int parDfi = rxSolveDat->parDf.size();
      rxSolveDat->parMat = NumericMatrix(rxSolveDat->nPopPar,parDfi);
      while (parDfi--){
				rxSolveDat->parMat(_,parDfi)=NumericVector(rxSolveDat->parDf[parDfi]);
      }
      rxSolveDat->parMat.attr("dimnames") = List::create(R_NilValue, rxSolveDat->parDf.names());
    }
  case 3: // NumericMatrix
    {
      gparsCovSetup(rxSolveDat->npars, rxSolveDat->nPopPar, rx->nsub*rx->nsim, ev1, rx);
      rxSolve_assignGpars(rxSolveDat);
      rxSolve_resample(obj, rxControl, specParams, extraArgs, pars, ev1,
											 inits, rxSolveDat);
      curSolve=0;
      curEvent=0;
      curIdx=0;
      int curCov=0;
      int curOn=0;
      int curSimIni=0;
      rx_solving_options_ind indS;
      int linCmt = INTEGER(rxSolveDat->mv[RxMv_flags])[RxMvFlag_linCmt];
      int nIndSim = rx->nIndSim;
      for (unsigned int simNum = rx->nsim; simNum--;){
				for (unsigned int id = rx->nsub; id--;){
					unsigned int cid = id+simNum*rx->nsub;
					ind = &(rx->subjects[cid]);
					ind->linCmt = linCmt;
					setupRxInd(ind, 1);
					ind->par_ptr = &_globals.gpars[cid*rxSolveDat->npars];
					ind->mtime   = &_globals.gmtime[rx->nMtime*cid];
					if (rx->nMtime > 0) ind->mtime[0]=-1;
					ind->InfusionRate = &_globals.gInfusionRate[(op->neq+op->extraCmt)*cid];
					ind->linCmtRate = ind->InfusionRate + op->neq;
					ind->tlastS = &_globals.gTlastS[(op->neq + op->extraCmt)*cid];
					ind->tfirstS = &_globals.gTfirstS[(op->neq + op->extraCmt)*cid];
					ind->curDoseS = &_globals.gCurDoseS[(op->neq + op->extraCmt)*cid];
					//ind->alag = &_globals.gAlag[(op->neq + op->extraCmt)*cid];
					ind->cF = &_globals.gF[(op->neq + op->extraCmt)*cid];
					ind->cRate = &_globals.gRate[(op->neq + op->extraCmt)*cid];
					ind->cDur = &_globals.gDur[(op->neq + op->extraCmt)*cid];
					ind->BadDose = &_globals.gBadDose[op->neq*cid];
					// Hmax defined above.
					ind->sim = simNum+1;
					ind->lhs = &_globals.glhs[cid*op->nlhs];
					ind->rc = &_globals.grc[cid];
					ind->slvr_counter = &_globals.slvr_counter[cid];
					ind->dadt_counter = &_globals.dadt_counter[cid];
					ind->jac_counter = &_globals.jac_counter[cid];
					if (simNum){
						// Assign the pointers to the shared data
						indS = rx->subjects[id];
						if (op->do_par_cov){
							ind->cov_ptr = indS.cov_ptr;
						}
						ind->n_all_times =indS.n_all_times;
						if (ind->n_all_times > rx->maxAllTimes) rx->maxAllTimes= ind->n_all_times;
						ind->HMAX = indS.HMAX;
						ind->idose = &(indS.idose[0]);
						ind->ndoses = indS.ndoses;
						ind->nevid2 = indS.nevid2;
						ind->dose = &(indS.dose[0]);
						ind->ii   = &(indS.ii[0]);
						ind->evid =&(indS.evid[0]);
						ind->all_times = &(indS.all_times[0]);
						ind->id=id+1;
						ind->idReal = indS.idReal;
					}
					int eLen = op->neq*ind->n_all_times;
					ind->solve = &_globals.gsolve[curSolve];
					ind->simIni = &_globals.gIndSim[curSimIni];
					curSimIni += nIndSim;
					curSolve += (op->neq + op->nlin)*ind->n_all_times;
					ind->solveLast = &_globals.gsolve[curSolve];
					curSolve += op->neq;
					ind->solveLast2 = &_globals.gsolve[curSolve];
					curSolve += op->neq;
					ind->solveSave = &_globals.gsolve[curSolve];
					curSolve += op->neq;
					ind->ix = &_globals.gix[curIdx];
					std::iota(ind->ix,ind->ix+ind->n_all_times,0);
					curEvent += eLen;
					ind->on=&_globals.gon[curOn];
					curOn +=op->neq+op->extraCmt;
					curIdx += ind->n_all_times;
					if (rx->sample) {
						ind->cov_sample = &_globals.gSampleCov[curCov];
						curCov += op->ncov;
					}
				}
      }
    }
    break;
  default:
    rxSolveFree();
    stop(_("Something is wrong"));
  }
  // Get the data range required for radix sort
  // This is adapted from forder:
  ////////////////////////////////////////////////////////////////////////////////
  // https://github.com/Rdatatable/data.table/blob/master/src/forder.c
  // in data.table keyAlloc=(ncol+n_cplx)*8+1 which translates to 9
  // Since the key is constant we can pre-allocate with stack instead key[9]
  // NA, NaN, and -Inf +Inf not supported
  int nbyte=0, nradix=0, spare=0;
  calcNradix(&nbyte, &nradix, &spare, &(rx->maxD), &(rx->minD));
  if (_globals.nradix != NULL) free(_globals.nradix);
  rx->nradix = _globals.nradix = (int*)malloc(sizeof(int));//nbyte-1 + (rx->spare==0); // lost
  std::fill_n(rx->nradix, 1, nradix);
  ////////////////////////////////////////////////////////////////////////////////
  if (_globals.keys!=NULL) {
    int i=0;
    while (_globals.keys[i] != NULL){
      int j = 0;
      while(_globals.keys[i][j] != NULL){
				free(_globals.keys[i][j]);
				_globals.keys[i][j++] = NULL;
      }
      free(_globals.keys[i]);
      _globals.keys[i++] = NULL;
    }
    free(_globals.keys);
  }
  rx->keys = _globals.keys = NULL;
  rx->keys = _globals.keys = (uint8_t ***)calloc(2, sizeof(uint8_t **)); // lost
  rx->keys[1] = NULL;
  // In rxode2 the keyAlloc size IS 9
  rx->keys[0] = (uint8_t **)calloc(10, sizeof(uint8_t *));
  for (int j = 0; j < 10; j++) rx->keys[0][j] = NULL;
  for (int b = 0; b < nbyte; b++){
    rx->keys[0][b] = (uint8_t *)calloc(rx->maxAllTimes+1, sizeof(uint8_t));
  }
  // Use same variables from data.table
  if (_globals.TMP != NULL) free(_globals.TMP);
  _globals.TMP = NULL;
  rx->TMP = _globals.TMP =  (int *)malloc(UINT16_MAX*sizeof(int)); // used by counting sort (my_n<=65536) in radix_r()
  if (_globals.UGRP != NULL) free(_globals.UGRP);
  _globals.UGRP = NULL;
  rx->UGRP = _globals.UGRP = (uint8_t *) malloc(256); // TODO: align TMP and UGRP to cache lines (and do the same for stack allocations too)
  // Now there is a key per core
}

// This creates the final dataset from the currently solved object.
// Most of this is a direct C call, but some items are done in C++
List rxSolve_df(const RObject &obj,
								const List &rxControl,
								const Nullable<CharacterVector> &specParams,
								const Nullable<List> &extraArgs,
								const RObject &params,
								const RObject &events,
								const RObject &inits,
								rxSolve_t* rxSolveDat){
  rx_solve* rx = getRxSolve_();
  rx_solving_options* op = rx->op;
  if (op->abort){
    rxSolveFree();
    stop(_("aborted solve"));
  }
  int doDose = 0;
  if (rxSolveDat->addDosing.isNull()){
    // only evid=0
    doDose=-1;
  } else {
    LogicalVector addDosing1 = as<LogicalVector>(rxSolveDat->addDosing);
    if (LogicalVector::is_na(addDosing1[0])){
      doDose = 1;
    } else if (addDosing1[0]){
      doDose = 2;
      if (asBool(rxControl[Rxc_subsetNonmem], "subsetNonmem")) doDose = 3;
    } else {
      doDose = 0;
    }
  }
  IntegerVector si = rxSolveDat->mv[RxMv_state_ignore];
  rx->stateIgnore = &si[0];
  int doTBS = (rx->matrix == 3);
  if (doTBS) rx->matrix=2;
  if (rx->matrix == 4 || rx->matrix == 5) rx->matrix=2;
  List dat = rxode2_df(doDose, doTBS);
  if (rx->whileexit) {
    warning(_("exited from at least one while after %d iterations, (increase with `rxSolve(..., maxwhile=#)`)"), rx->maxwhile);
  }
  if (!rxIsNull(rxControl[Rxc_drop])) {
    dat = rxDrop(asCv(rxControl[Rxc_drop], "drop"), dat, asBool(rxControl[Rxc_warnDrop], "warnDrop"));
  }
  if (rxSolveDat->idFactor && rxSolveDat->labelID && rx->nsub > 1){
    IntegerVector did = as<IntegerVector>(dat["id"]);
    did.attr("class") = "factor";
    did.attr("levels") = rxSolveDat->idLevels;
  }
  if (rxSolveDat->convertInt && rx->nsub > 1){
    CharacterVector lvlC = rxSolveDat->idLevels;
    IntegerVector lvlI(lvlC.size());
    for (int j = lvlC.size(); j--;) {
      lvlI[j] = atoi(CHAR(lvlC[j]));
    }
    IntegerVector did = as<IntegerVector>(dat["id"]);
    IntegerVector did2(did.size());
    for (int j = did.size(); j--;){
      did2[j] = lvlI[did[j]-1];
    }
    dat["id"] = did2;
  }
  if (rxSolveDat->addTimeUnits){
    NumericVector tmpN = as<NumericVector>(dat["time"]);
    tmpN.attr("class") = "units";
    tmpN.attr("units") = rxSolveDat->timeUnitsU;
  }
  dat.attr("class") = CharacterVector::create("data.frame");
  if (rx->add_cov && (rx->matrix == 2 || rx->matrix == 0) &&
      rxSolveDat->covUnits.hasAttribute("names")){
    CharacterVector nmC = rxSolveDat->covUnits.attr("names");
    NumericVector tmpN, tmpN2;
    for (unsigned int i = nmC.size(); i--;){
      tmpN = rxSolveDat->covUnits[i];
      if (rxIs(tmpN, "units")){
				tmpN2 = dat[as<std::string>(nmC[i])];
				tmpN2.attr("class") = "units";
				tmpN2.attr("units") = tmpN.attr("units");
      }
    }
  }
  return dat;
}

// Generate environment for saving solving information
static inline Environment rxSolve_genenv(const RObject &object,
																				 const List &rxControl,
																				 const Nullable<CharacterVector> &specParams,
																				 const List &dat,
																				 const RObject &params,
																				 const RObject &events,
																				 const RObject &inits,
																				 rxSolve_t* rxSolveDat){
  rx_solve* rx = getRxSolve_();
  IntegerVector slvr_counterIv(rxSolveDat->nSize);
  IntegerVector dadt_counterIv(rxSolveDat->nSize);
  IntegerVector  jac_counterIv(rxSolveDat->nSize);
  CharacterVector timeUnits = asCv(rxControl[Rxc_timeUnits], "timeUnits");
  CharacterVector amountUnits = asCv(rxControl[Rxc_amountUnits], "amountUnits");

  std::copy(&_globals.slvr_counter[0], &_globals.slvr_counter[0] + rxSolveDat->nSize, slvr_counterIv.begin());
  std::copy(&_globals.dadt_counter[0], &_globals.dadt_counter[0] + rxSolveDat->nSize, dadt_counterIv.begin());
  std::copy(&_globals.jac_counter[0], &_globals.jac_counter[0] + rxSolveDat->nSize, jac_counterIv.begin());
  Function newEnv("new.env", R_BaseNamespace);
  Environment e = newEnv(_["size"] = 29, _["parent"] = rxode2env());
  getRxModels();
  if(_rxModels.exists(".theta")){
    if (Rf_isMatrix(_rxModels[".theta"])) {
      e[".theta"] = as<NumericMatrix>(_rxModels[".theta"]);
    }
    _rxModels.remove(".theta");
  }
  if(_rxModels.exists(".sigma")){
    if (Rf_isMatrix(_rxModels[".sigma"])) {
      e[".sigma"] = as<NumericMatrix>(_rxModels[".sigma"]);
    }
    _rxModels.remove(".sigma");
  }
  if(_rxModels.exists(".thetaL")){
    SEXP tmp = _rxModels[".thetaL"];
    if (TYPEOF(tmp) == VECSXP && Rf_length(tmp) > 0) {
      e[".thetaL"] = as<List>(tmp);
    } else {
      e[".thetaL"] = R_NilValue;
    }
    _rxModels.remove(".thetaL");
  }
  if(_rxModels.exists(".omegaL")){
    SEXP tmp = _rxModels[".omegaL"];
    if (TYPEOF(tmp) == VECSXP && Rf_length(tmp) > 0) {
      e[".omegaL"] = as<List>(tmp);
    } else {
      e[".omegaL"] = R_NilValue;
    }
    _rxModels.remove(".omegaL");
  }
  if(_rxModels.exists(".sigmaL")){
    SEXP tmp = _rxModels[".sigmaL"];
    if (TYPEOF(tmp) == VECSXP && Rf_length(tmp) > 0) {
      e[".sigmaL"] = as<List>(_rxModels[".sigmaL"]);
    } else {
      e[".sigmaL"] = R_NilValue;
    }
    _rxModels.remove(".sigmaL");
  }
  if(_rxModels.exists(".nestEta")){
    SEXP tmp = _rxModels[".nestEta"];
    if (TYPEOF(tmp) == STRSXP) {
      e[".nestEta"] = tmp;
    }
    _rxModels.remove(".nestEta");
  }
  if(_rxModels.exists(".nestTheta")){
    SEXP tmp = _rxModels[".nestTheta"];
    if (TYPEOF(tmp) == STRSXP) {
      e[".nestTheta"] = tmp;
    }
    _rxModels.remove(".nestTheta");
  }
  e[".check.nrow"] = rx->nr;
  e[".check.ncol"] = dat.size();
  e[".check.names"] = dat.names();
  e[".idLevels"] = as<CharacterVector>(rxSolveDat->idLevels);
  e[".par.pos"] = rxSolveDat->eGparPos;
  e[".par.pos.ini"] = rxSolveDat->fromIni;
  e[".slvr.counter"] = slvr_counterIv;
  e[".dadt.counter"] = dadt_counterIv;
  e[".jac.counter"] = jac_counterIv;
  e[".nsub"] = rx->nsub;
  e[".nsim"] = rx->nsim;
  e[".init.dat"] = rxSolveDat->initsC;
  CharacterVector units = CharacterVector::create(amountUnits[0], timeUnits[0]);
  units.names() = CharacterVector::create("dosing","time");
  e["units"] = units;
  e["nobs"] = rx->nobs - rx->nevid9;
  e[".args.object"] = object;
  e["dll"] = rxDll(object);
  e[".args.par0"] = rxSolveDat->par1ini;
  if (!rxSolveDat->swappedEvents){
    if (rxSolveDat->usePar1){
      e[".args.params"] = rxSolveDat->par1;
    } else {
      e[".args.params"] = params;
    }
    e[".args.events"] = events;
  } else {
    if (rxSolveDat->usePar1){
      e[".args.params"] = rxSolveDat->par1;
    } else {
      e[".args.params"] = events;
    }
    e[".args.events"] = params;
  }
  e[".args.inits"] = inits;
  e[".args"] = rxControl;
  e[".real.update"] = true;
  return e;
}

void rxAssignPtr(SEXP object);

int getNRows(RObject obj){
  switch (obj.sexp_type()){
  case 13: // Integer Vectors
  case REALSXP:
    {
      bool hasDim = obj.hasAttribute("dim");
      if (hasDim){
				// matrix
				IntegerVector ret0  = obj.attr("dim");
				return ret0[0];
      } else {
				return 1;
      }
    }
  case VECSXP:
    {
      bool rn = obj.hasAttribute("row.names");
      if (rn){
				RObject ret0 = obj.attr("row.names");
				if (rxIsInt(ret0)){
					IntegerVector ret1 = IntegerVector(ret0);
					if (ret1.size() == 2 && IntegerVector::is_na(ret1[0])){
						return(ret1[1]);
					} else {
						return ret1.size();
					}
				} else if (rxIsChar(ret0)){
					CharacterVector ret1 = CharacterVector(ret0);
					return ret1.size();
				} else {
					return NA_INTEGER;
				}
      } else {
				return NA_INTEGER;
      }
    }
  default:
    return NA_INTEGER;
  }
}

rxSolve_t rxSolveDatLast;

static inline void rxSolveSaveRxSolve(rxSolve_t* rxSolveDat){
  rxSolveDatLast = rxSolveDat[0];
  // Assigned as a precaution against R's gc()
  _rxModels[".rxSolveDat.mv"] = rxSolveDat->mv;
  _rxModels[".rxSolveDat.addDosing"] = rxSolveDat->addDosing;
  _rxModels[".rxSolveDat.timeUnitsU"] = rxSolveDat->timeUnitsU;
  _rxModels[".rxSolveDat.covUnits"] = rxSolveDat->covUnits;
  _rxModels[".rxSolveDat.par1"] = rxSolveDat->par1;
  _rxModels[".rxSolveDat.par1ini"] = rxSolveDat->par1ini;
  _rxModels[".rxSolveDat.initsC"] = rxSolveDat->initsC;
  _rxModels[".rxSolveDat.eGparPos"] = rxSolveDat->eGparPos;
  _rxModels[".rxSolveDat.sigmaN"] = rxSolveDat->sigmaN;
  _rxModels[".rxSolveDat.omegaN"] = rxSolveDat->omegaN;
  _rxModels[".rxSolveDat.parNumeric"] = rxSolveDat->parNumeric;
  _rxModels[".rxSolveDat.parDf"] = rxSolveDat->parDf;
  _rxModels[".rxSolveDat.parMat"] = rxSolveDat->parMat;
  _rxModels[".rxSolveDat.nmP"] = rxSolveDat->nmP;
  _rxModels[".rxSolveDat.mvIni"] = rxSolveDat->mvIni;
  _rxModels[".rxSolveDat.idLevels"] = rxSolveDat->idLevels;
}

// static inline void rxSolveSaveRxRestore(rxSolve_t* rxSolveDat){
//   // Assigned as a precaution against R's gc()
//   rxSolveDat->mv = as<List>(_rxModels[".rxSolveDat.mv"]);
//   rxSolveDat->addDosing = as<Nullable<LogicalVector>>(_rxModels[".rxSolveDat.addDosing"]);
//   rxSolveDat->timeUnitsU = as<RObject>(_rxModels[".rxSolveDat.timeUnitsU"]);
//   rxSolveDat->covUnits = as<List>(_rxModels[".rxSolveDat.covUnits"]);
//   rxSolveDat->par1 = as<RObject>(_rxModels[".rxSolveDat.par1"]);
//   rxSolveDat->par1ini = as<RObject>(_rxModels[".rxSolveDat.par1ini"]);
//   rxSolveDat->initsC = as<NumericVector>(_rxModels[".rxSolveDat.initsC"]);
//   rxSolveDat->eGparPos = as<IntegerVector>(_rxModels[".rxSolveDat.eGparPos"]);
//   rxSolveDat->sigmaN = as<CharacterVector>(_rxModels[".rxSolveDat.sigmaN"]);
//   rxSolveDat->omegaN = as<CharacterVector>(_rxModels[".rxSolveDat.omegaN"]);
//   rxSolveDat->parNumeric = as<NumericVector>(_rxModels[".rxSolveDat.parNumeric"]);
//   rxSolveDat->parDf = as<DataFrame>(_rxModels[".rxSolveDat.parDf"]);
//   rxSolveDat->parMat = as<NumericMatrix>(_rxModels[".rxSolveDat.parMat"]);
//   rxSolveDat->nmP = as<CharacterVector>(_rxModels[".rxSolveDat.nmP"]);
//   rxSolveDat->mvIni = as<NumericVector>(_rxModels[".rxSolveDat.mvIni"]);
//   rxSolveDat->idLevels = as<CharacterVector>(_rxModels[".rxSolveDat.idLevels"]);
// }

RObject _curPar;
static inline void rxSolve_assignGpars(rxSolve_t* rxSolveDat){
  unsigned int i;
  for (unsigned int j = 0; j < (unsigned int)rxSolveDat->nPopPar; j++){
    for (unsigned int k = 0; k < (unsigned int)rxSolveDat->npars; k++){
      i = k+rxSolveDat->npars*j;
      if (ISNA(_globals.gpars[i])){
				if (_globals.gParPos[k] == 0){
					_globals.gpars[i] = 0;
				} else if (_globals.gParPos[k] > 0){
					// posPar[i] = j + 1;
					_globals.gpars[i] = rxSolveDat->parMat(j, _globals.gParPos[k]-1);
				} else {
					// posPar[i] = -j - 1;
					_globals.gpars[i] = rxSolveDat->mvIni[-_globals.gParPos[k]-1];
				}
      }
    }
  }
}

// This updates the parameters based on the already loaded event table.
// static inline void rxSolve_updateParams(RObject &trueParams,
// 					const RObject &obj,
// 					const List &rxControl,
// 					const Nullable<CharacterVector> &specParams,
// 					const Nullable<List> &extraArgs,
// 					const RObject &params,
// 					const RObject &events,
// 					const RObject &inits,
// 					rxSolve_t* rxSolveDat){
//   rxSolveDat->par1 = trueParams;
//   rxSolveDat->usePar1=true;
//   rx_solve* rx = getRxSolve_();
//   rx_solving_options* op = rx->op;
//   if (rxIs(rxSolveDat->par1, "data.frame")){
//     if (rxSolveDat->idLevels.size() > 0){
//       // FIXME: check to see if IDs are dropped.
//       Function sort = getRxFn(".sortId");
//       rxSolveDat->parDf = clone(sortId(rxSolveDat->par1, rxSolveDat->idLevels, "parameters",
// 				      rxSolveDat->warnIdSort));
//       rxSolveDat->par1 = rxSolveDat->parDf;
//     } else {
//       rxSolveDat->parDf = as<DataFrame>(rxSolveDat->par1);
//     }
//     rxSolveDat->parType = 2;
//     rxSolveDat->nPopPar = rxSolveDat->parDf.nrows();
//     unsigned int parDfi = rxSolveDat->parDf.size();
//     rxSolveDat->parMat = NumericMatrix(rxSolveDat->nPopPar, parDfi);
//     while (parDfi--){
//       rxSolveDat->parMat(_,parDfi)=NumericVector(rxSolveDat->parDf[parDfi]);
//     }
//     rxSolveDat->parMat.attr("dimnames") = List::create(R_NilValue, rxSolveDat->parDf.names());
//   } else if (rxIs(rxSolveDat->par1, "matrix")){
//     rxSolveDat->parMat = as<NumericMatrix>(rxSolveDat->par1);
//     rxSolveDat->nPopPar = rxSolveDat->parMat.nrow();
//     rxSolveDat->parType = 3;
//   } else if (!rxIsNull(rxSolveDat->par1)) {
//     rxSolveDat->parNumeric = rxSolveDat->par1;
//     // Convert NumericVector to a matrix

//     rxSolveDat->parMat = NumericMatrix(rx->nsub, rxSolveDat->parNumeric.size());
//     unsigned int parDfi = rxSolveDat->parNumeric.size();
//     while (parDfi--){
//       std::fill_n(rxSolveDat->parMat.begin()+parDfi*rx->nsub, rx->nsub, rxSolveDat->parNumeric[parDfi]);
//     }
//     rxSolveDat->parMat.attr("dimnames") = List::create(R_NilValue, rxSolveDat->parNumeric.attr("names"));
//   }
//   RObject ev1=_rxModels[".lastEv1"];
//   std::fill_n(&_globals.gpars[0], rxSolveDat->npars*rxSolveDat->nPopPar, NA_REAL);
//   gparsCovSetupConstant(ev1, rxSolveDat->npars);
//   // Setup a possibly new scale.
//   RObject scale = rxControl[Rxc_scale];
//   NumericVector scaleC = rxSetupScale(obj, scale, extraArgs);
//   std::copy(scaleC.begin(),scaleC.end(),&_globals.gscale[0]);
//   rxSolve_assignGpars(rxSolveDat);
//   // std::fill_n(&_globals.gsolve[0], rx->nall*op->neq*rx->nsim, 0.0);
//   seedEng(op->cores); // Reseed
// }

static inline SEXP rxSolve_finalize(const RObject &obj,
																		const List &rxControl,
																		const Nullable<CharacterVector> &specParams,
																		const Nullable<List> &extraArgs,
																		const RObject &params, const RObject &events,
																		const RObject &inits,
																		rxSolve_t* rxSolveDat){
#ifdef rxSolveT
  clock_t _lastT0 = clock();
#endif

  rxSolveSaveRxSolve(rxSolveDat);
  rx_solve* rx = getRxSolve_();
  // if (rxSolveDat->throttle){
  //   rx->op->cores = getRxThreads(rx->nsim*rx->nsub, true);
  // }
  if (_globals.zeroTheta) {
    cliAlert(_("zero 'thetaMat' specified, no uncertainty in fixed effects"));
  }
  if (_globals.zeroOmega) {
    cliAlert(_("zero 'omega', no variability from random-effects"));
  }
  if (_globals.zeroSigma) {
    cliAlert(_("zero 'sigma', no unexplained variability"));
  }
  par_solve(rx);
#ifdef rxSolveT
	RSprintf("  Time1: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
	_lastT0 = clock();
#endif// rxSolveT

  List dat = rxSolve_df(obj, rxControl, specParams, extraArgs,
												params, events, inits, rxSolveDat);

#ifdef rxSolveT
	RSprintf("  Time2: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
	_lastT0 = clock();
#endif// rxSolveT
  rxUnlock(obj);
  if (rx->matrix == 0 && rxDropB){
    rx->matrix=2;
    warning(_("dropped key column, returning data.frame instead of special solved data.frame"));
  }
#ifdef rxSolveT
	RSprintf("  Time3: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
	_lastT0 = clock();
#endif// rxSolveT
  if (rx->matrix){
    // rxSolveFree();
    if(_rxModels.exists(".sigma")){
      _rxModels.remove(".sigma");
    }
    if(_rxModels.exists(".sigmaL")){
      _rxModels.remove(".sigmaL");
    }
    if(_rxModels.exists(".omegaL")){
      _rxModels.remove(".omegaL");
    }
    if(_rxModels.exists(".thetaL")){
      _rxModels.remove(".thetaL");
    }
    if(_rxModels.exists(".theta")){
      _rxModels.remove(".theta");
    }
    if (rx->matrix == 2){
      dat.attr("class") = "data.frame";
#ifdef rxSolveT
			RSprintf("  Time4: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
			_lastT0 = clock();
#endif
      return dat;
    } else {
      NumericMatrix tmpM(rx->nr, dat.size());
      for (unsigned int i = dat.size(); i--;){
				tmpM(_,i) = as<NumericVector>(dat[i]);
      }
      tmpM.attr("dimnames") = List::create(R_NilValue,dat.names());
#ifdef rxSolveT
			RSprintf("  Time4: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
			_lastT0 = clock();
#endif
      return tmpM;
    }
  } else {
    CharacterVector cls= CharacterVector::create("rxSolve", "rxSolveParams","rxSolveCovs",
																								 "rxSolveInits", "rxSolveSimType",
																								 "data.frame");
    cls.attr(".rxode2.env") = rxSolve_genenv(obj, rxControl, specParams,
																						 dat, params, events,
																						 inits, rxSolveDat);
    // eGparPos
    dat.attr("class") = cls;
#ifdef rxSolveT
    RSprintf("  Time4: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif
    return(dat);
  }
}

SEXP expandPars_(SEXP objectS, SEXP paramsS, SEXP eventsS, SEXP controlS);

static inline void iniRx(rx_solve* rx) {
  rx->hasEvid2 = 0;
  rx->nsub = 0;
  rx->nsim = 0;
  rx->neta=0;
  rx->neps=0;
  rx->nIndSim=0;
  rx->simflg = 0;
  rx->nall = 0;
  rx->nevid9 = 0;
  rx->nobs = 0;
  rx->nobs2 = 0;
  rx->nr = 0;
  rx->add_cov = 0;
  rx->matrix = 0;
  rx->needSort = 0;
  rx->nMtime = 0;
  rx->stateTrimU = R_PosInf;
  rx->stateTrimL = R_NegInf;
  rx->stateIgnore = NULL;
  rx->nCov0 = 0;
  rx->cov0 = NULL;
  rx->nKeepF = 0;
  rx->istateReset=1;
  rx->cens = 0;
  rx->limit = 0;
  rx->safeZero = 1;
  rx->sumType = 1; // pairwise
  rx->prodType = 1; // long double
  rx->sensType = 4; // advan
  rx->hasFactors = 0;
  rx->keys = NULL; // keys per thread
  rx->TMP = NULL;
  rx->ordId = NULL;
  rx->UGRP = NULL;
  rx->nradix = NULL;
  rx->ypNA = NULL;
  rx->sample = false;
  rx->par_sample = NULL;
  rx->maxShift = 0.0;
  rx->linKa  = 0;
  rx->linNcmt = 0;
  rx->maxwhile = 100000;
  rx->whileexit= 0;

  rx_solving_options* op = rx->op;
  op->badSolve = 0;
  op->naTime = 0;
  op->ATOL = 1e-8; //absolute error
  op->RTOL = 1e-8; //relative error
  op->H0  = 0;
  op->HMIN = 0;
  op->mxstep = 70000;
  op->MXORDN =12;
  op->MXORDS = 5;
  //
  op->nlhs = 0;
  op->neq = 0;
  op->stiff = 0;
  op->ncov = 0;
  op->par_cov = NULL;
  op->inits = NULL;
  op->scale = NULL;
  op->do_par_cov=false;
  // approx fun options
  op->f1   = 0.0;
  op->f2   = 1.0;
  op->kind = 0;
  op->is_locf = 1;
  op->cores = 0;
  op->extraCmt = 0;
  op->hmax2 = 0; // Determined by diff
  op->rtol2 = NULL;
  op->atol2 = NULL;
  op->ssRtol = NULL;
  op->ssAtol = NULL;
  op->indLin = NULL;
  op->indLinN = 0;
  op->indLinPhiTol = 1e-7;
  op->indLinPhiM = 0;
  op->indLinMatExpType = 2;
  op->indLinMatExpOrder = 6;
  op->nDisplayProgress = 10000;
  op->isChol = 0;
  op->nsvar = 0;
  op->abort = 0;
  op->minSS = 10;
  op->maxSS = 1000;
  op->doIndLin  = 0;
  op->strictSS = 1;
  op->infSSstep = 12;
  op->ncoresRV = 1;
  op->mxhnil = 0;
  op->hmxi = 0.0;
  op->nlin = 0;
  op->nlin2 = 0;
  op->nlinR = 0;
  op->linBflag = 0;
  op->cTlag = false;
  op->hTlag = 0;
  op->cF = false;
  op->hF = 0;
  op->cRate = false;
  op->hRate = 0;
  op->cDur = false;
  op->hDur = 0;
  op->cTlag2 = false;
  op->hTlag2 = 0;
  op->cF2 = false;
  op->hF2 = 0;
  op->cRate2 = false;
  op->hRate2 = 0;
  op->cDur2 = false;
  op->hDur2 = 0;
  rx->svar = _globals.gsvar;
  rx->ovar = _globals.govar;
}

// [[Rcpp::export]]
SEXP rxSolve_(const RObject &obj, const List &rxControl,
							const Nullable<CharacterVector> &specParams,
							const Nullable<List> &extraArgs,
							const RObject &params, const RObject &events, const RObject &inits,
              const int setupOnly){
  if (setupOnly == 0){
    rxSolveFree();
  }
  rxDropB = false;
#ifdef rxSolveT
  clock_t _lastT0 = clock();
#endif
  if (!rxIs(rxControl,"rxControl")){
    rxSolveFree();
    stop(_("control list not setup correctly"));
  }
  maxAtolRtolFactor = asDouble(rxControl[Rxc_maxAtolRtolFactor], "maxAtolRtolFactor");
  RObject scale = rxControl[Rxc_scale];
  int method = asInt(rxControl[Rxc_method], "method");
  NumericVector atolNV = asNv(rxControl[Rxc_atol], "atol");
  NumericVector rtolNV = asNv(rxControl[Rxc_rtol], "rtol");
  NumericVector atolNVss = asNv(rxControl[Rxc_ssAtol], "ssAtol");
  NumericVector rtolNVss = asNv(rxControl[Rxc_ssRtol], "ssRtol");
  int maxsteps = asInt(rxControl[Rxc_maxsteps], "maxsteps");
  double hmin = asDouble(rxControl[Rxc_hmin], "hmin");
  double hini = asDouble(rxControl[Rxc_hini], "hini");
  int maxordn = asInt(rxControl[Rxc_maxordn], "maxordn");
  int maxords = asInt(rxControl[Rxc_maxords], "maxords");
  int covs_interpolation = asInt(rxControl[Rxc_covsInterpolation], "covsInterpolation");
  bool addCov = asBool(rxControl[Rxc_addCov], "addCov");
  int matrix = asInt(rxControl[Rxc_returnType], "returnType");
  int nDisplayProgress = asInt(rxControl[Rxc_nDisplayProgress], "nDisplayProgress");
  NumericVector stateTrim = asNv(rxControl[Rxc_stateTrim], "stateTrim");
  double stateTrimU= R_PosInf;
  double stateTrimL= R_NegInf;
  if (stateTrim.size() == 2){
    if (stateTrim[0] > stateTrim[1]){
      stateTrimU = stateTrim[0];
      stateTrimL = stateTrim[1];
    } else {
      stateTrimU = stateTrim[1];
      stateTrimL = stateTrim[0];
    }
  } else if (stateTrim.size() == 1){
    if (!ISNA(stateTrimU)){
      stateTrimU = fabs(stateTrim[0]);
      stateTrimL = -stateTrimU;
    }
  } else {
    RSprintf("rxControl\n");
    print(rxControl[Rxc_stateTrim]);
    RSprintf("stateTrim\n");
    print(stateTrim);
    rxSolveFree();
    stop("'stateTrim' must be a vector of 1-2 elements");
  }
  rxSolve_t rxSolveDat0;
  rxSolve_t* rxSolveDat = &rxSolveDat0;
  RObject object;
  rxSolveDat->updateObject = asBool(rxControl[Rxc_updateObject], "updateObject");
  rxSolveDat->isRxSolve = rxIs(obj, "rxSolve");
  rxSolveDat->isEnvironment = rxIs(obj, "environment");
  rxSolveDat->idFactor= asBool(rxControl[Rxc_idFactor], "idFactor");
  rxSolveDat->warnIdSort = asBool(rxControl[Rxc_warnIdSort], "warnIdSort");
  RObject trueParams;
  RObject trueEvents;
  bool swappedEvents=false;
  if (rxIs(params, "rx.event")){
    trueEvents = params;
    trueParams = events;
    swappedEvents=true;
  } else if (rxIs(events, "rx.event")){
    trueEvents = events;
    trueParams = params;
  } else {
    rxSolveFree();
    stop(_("cannot solve without event information"));
  }
  getRxModels();
  bool didNesting=false;
  if (rxSolveDat->updateObject && !rxSolveDat->isRxSolve && !rxSolveDat->isEnvironment){
    if (rxIs(rxCurObj, "rxSolve")){
      object = rxCurObj;
      rxSolveDat->isRxSolve = true;
    } else {
      rxSolveFree();
      stop(_("cannot update this object"));
    }
  } else {
    object = obj;
    // Update rxode2 model (if needed) and simulate nesting
    if ((!Rf_isNull(rxControl[Rxc_thetaMat]) ||
         !Rf_isNull(rxControl[Rxc_omega]) ||
         !Rf_isNull(rxControl[Rxc_sigma])) &&
        rxIs(rxControl[Rxc_omega], "lotri") &&
        TYPEOF(rxControl[Rxc_sigma]) != STRSXP
        ) {
      // Update model, events and parameters based on nesting
      _rxModels[".nestPars"] = expandPars_(wrap(object), wrap(trueParams),
                                           wrap(trueEvents), wrap(rxControl));
      object = _rxModels[".nestObj"];
      trueEvents = _rxModels[".nestEvents"];
      didNesting=true;
    } else {
      object = obj;
    }
    if (method == 3){
      rxSolveDat->mv = rxModelVars(object);
      rxSolveFreeObj = object;
      List indLin = rxSolveDat->mv[RxMv_indLin];
      if (indLin.size() == 0){
        Function rxode2 = getRxFn("rxode2");
        object = rxode2(object, _["indLin"]=true);
        rxSolveDat->mv = rxModelVars(object);
        rxSolveFreeObj = object;
      } // else {
      // 	object =obj;
      // }
    } else {
      rxSolveDat->mv = rxModelVars(object);
      rxSolveFreeObj = object;
    }
  }
  if (rxSolveDat->isRxSolve || rxSolveDat->isEnvironment){
    rx_solve* rx = getRxSolve_();
    iniRx(rx);
    rx->maxwhile = asInt(rxControl[Rxc_maxwhile], "maxwhile");
    rx->sumType = asInt(rxControl[Rxc_sumType], "sumType");
    rx->prodType = asInt(rxControl[Rxc_prodType], "prodType");
    rx->sensType = asInt(rxControl[Rxc_sensType], "sensType");
    return rxSolve_update(object, rxControl, specParams,
                          extraArgs, params, events, inits,
                          rxSolveDat);
  } else {
    rxLock(object);
#ifdef rxSolveT
    RSprintf("Time1: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif// rxSolveT
    // Load model
    if (!rxDynLoad(object)){
      rxSolveFree();
      stop(_("cannot load rxode2 dlls for this model"));
    }
    // Get model
    // Get the C solve object
    rx_solve* rx = getRxSolve_();
    iniRx(rx);
    rx->nIndSim = INTEGER(rxSolveDat->mv[RxMv_flags])[RxMvFlag_nIndSim];
    rx->simflg  = INTEGER(rxSolveDat->mv[RxMv_flags])[RxMvFlag_simflg];
    rx->sumType = asInt(rxControl[Rxc_sumType], "sumType");
    rx->prodType = asInt(rxControl[Rxc_prodType], "prodType");
    rx->sensType = asInt(rxControl[Rxc_sensType], "sensType");
    rx->maxwhile = asInt(rxControl[Rxc_maxwhile], "maxwhile");
    rx_solving_options* op = rx->op;
#ifdef rxSolveT
    RSprintf("Time2: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif// rxSolveT
    _rxModels[".lastEvents"] = trueEvents;
    _rxModels[".lastParNames"] = trueParams.attr("names");
    _rxModels[".lastNrow"] = getNRows(trueParams);
    _rxModels[".lastMv"] = rxSolveDat->mv;
    _rxModels[".lastControl"] = rxControl;
    _rxModels[".lastInits"] = inits;
    rxSolveDat->addDosing = asNLv(rxControl[Rxc_addDosing], "addDosing");
#ifdef rxSolveT
    RSprintf("Time3: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif// rxSolveT
    CharacterVector pars = rxSolveDat->mv[RxMv_params];
    rxSolveDat->npars = pars.size();
    rxSolveDat->hasCmt = INTEGER(rxSolveDat->mv[RxMv_flags])[RxMvFlag_hasCmt] == 1;
    // Assign Pointers
    rxAssignPtr(rxSolveDat->mv);
    rx->nKeepF = 0;
    rx->stateTrimU = stateTrimU;
    rx->stateTrimL = stateTrimL;
    rx->matrix = matrix;
    rx->needSort = as<int>(rxSolveDat->mv[RxMv_needSort]);
    rx->nMtime = as<int>(rxSolveDat->mv[RxMv_nMtime]);
    rx->add_cov = (int)(addCov);
    rx->istateReset = asInt(rxControl[Rxc_istateReset], "istateReset");
    rx->safeZero = asInt(rxControl[Rxc_safeZero], "safeZero");
    op->stiff = method;
    rxSolveDat->throttle = false;
    if (method != 2 || rx->needSort != 0){
      op->cores = 1;//getRxThreads(1, false);
    } else {
      op->cores = asInt(rxControl[Rxc_cores], "cores");
      int thread = INTEGER(rxSolveDat->mv[RxMv_flags])[RxMvFlag_thread];
      if (op->cores == 0) {
        switch (thread) {
        case 2:
          // Thread safe, but possibly not reproducible
          if (op->cores > 1) {
            op->stiff = method = 4;
          }
          rxSolveDat->throttle = false;
          break;
        case 1:
          // Thread safe, and reproducible
          op->cores = getRxThreads(INT_MAX, false);
          rxSolveDat->throttle = true;
          break;
        case 0:
          // Not thread safe.
          warning(_("not thread safe method, using 1 core"));
          op->cores = 1;
          rxSolveDat->throttle = false;
          break;
        }
      } else {
        switch (thread) {
        case 2:
          // Thread safe, but possibly not reproducible
          if (op->cores > 1) {
            op->stiff = method = 4;
          }
          break;
        case 1:
          // Thread safe, and reproducible
          rxSolveDat->throttle = true;
          break;
        case 0:
          // Not thread safe.
          warning(_("not thread safe method, using 1 core"));
          op->cores = 1;
          rxSolveDat->throttle = false;
          break;
        }
      }
    }
    seedEng(max2(op->cores, 1));
    // Now set up events and parameters
    RObject par0 = params;
    RObject ev0  = events;
    RObject ev1;
    rxSolveDat->swappedEvents = false;
    rxSolveDat->nsvar = 0;
    rxSolveDat->labelID=false;
    rxSolveDat->swappedEvents=swappedEvents;
    ev1 = trueEvents;
    rxSolveDat->par1 = trueParams;
    if (rxIsNull(rxSolveDat->par1)){
      rxSolveDat->par1=rxInits(obj);
      rxSolveDat->fromIni=true;
    }
#ifdef rxSolveT
    RSprintf("Time5: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif// rxSolveT
    // Update event table with observations if they are missing
    rxSolve_ev1Update(object, rxControl, specParams, extraArgs, params,
                      ev1, inits, rxSolveDat);

#ifdef rxSolveT
    RSprintf("Time6: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif// rxSolveT
    // Now get the parameters (and covariates)
    //
    // Unspecified parameters can be found in the modVars["ini"]
    rxSolveDat->mvIni = rxSolveDat->mv[RxMv_ini];
    // The event table can contain covariate information, if it is acutally a data frame or matrix.
    Nullable<CharacterVector> covnames0, simnames0;
    CharacterVector covnames, simnames;
    rxSolveDat->eGparPos = IntegerVector(rxSolveDat->npars);
    CharacterVector state = rxSolveDat->mv[RxMv_state];
    CharacterVector lhs = rxSolveDat->mv[RxMv_lhs];
    op->neq = state.size();
    op->badSolve = 0;
    op->naTime = 0;
    op->abort = 0;
    op->ATOL = atolNV[0];          //absolute error
    op->RTOL = rtolNV[0];          //relative error

    op->minSS = asInt(rxControl[Rxc_minSS], "minSS");
    op->maxSS = asInt(rxControl[Rxc_maxSS], "maxSS");
    op->infSSstep = asDouble(rxControl[Rxc_infSSstep], "infSSstep");
    if (op->infSSstep <= 0) {
      rxSolveFree();
      stop(_("'infSSstep' needs to be positive"));
    }
    op->indLinPhiTol=asDouble(rxControl[Rxc_indLinPhiTol], "indLinPhiTol");
    op->indLinMatExpType=asInt(rxControl[Rxc_indLinMatExpType], "indLinMatExpType");
    op->indLinPhiM = asInt(rxControl[Rxc_indLinPhiM],"indLinPhiM");
    op->indLinMatExpOrder=asInt(rxControl[Rxc_indLinMatExpOrder], "indLinMatExpOrder");
    List indLin = rxSolveDat->mv[RxMv_indLin];
    op->doIndLin=0;
    if (indLin.size() == 4){
      int me = rxIsNull(indLin[1]);
      if (as<bool>(indLin[2])){
        // Inductive linearization
        IntegerVector indLinItems = as<IntegerVector>(indLin[3]);
        op->indLinN = indLinItems.size();
        if (_globals.gindLin != NULL) R_Free(_globals.gindLin);
        _globals.gindLin = R_Calloc(op->indLinN,int);
        op->indLin = _globals.gindLin;
        std::copy(indLinItems.begin(), indLinItems.end(), op->indLin);
        if (me){
          // homogenous ME + IndLin
          op->doIndLin=3;
        } else {
          // inhomogenous ME + IndLin
          op->doIndLin=4;
        }
      } else {
        op->indLinN = 0;
        op->indLin = NULL;
        if (me){
          op->doIndLin=1;
        } else {
          op->doIndLin=2;
        }
      }
    } else if (indLin.size() == 3) {
      // f is NULL
    }
    op->H0 = hini;
    op->HMIN = hmin;
    op->mxstep = maxsteps;
    op->MXORDN = maxordn;
    op->MXORDS = maxords;
    // The initial conditions cannot be changed for each individual; If
    // they do they need to be a parameter.
    op->nlhs = lhs.size();
    CharacterVector trans = rxSolveDat->mv[RxMv_trans];
    // Make sure the model variables are assigned...
    // This fixes random issues on windows where the solves are done and the data set cannot be solved.
    getRxModels();
    _rxModels[as<std::string>(trans[RxMvTrans_model_vars])] = rxSolveDat->mv;
    sprintf(op->modNamePtr, "%s", (as<std::string>(trans[RxMvTrans_model_vars])).c_str());
    // approx fun options
    op->is_locf = covs_interpolation;
    if (op->is_locf == 0){//linear
      op->f1 = 1.0;
      op->f2 = 0.0;
      op->kind=1;
    } else if (op->is_locf == 1){ // locf
      op->f2 = 0.0;
      op->f1 = 1.0;
      op->kind = 0;
      op->is_locf=1;
    } else if (op->is_locf == 2){ //nocb
      op->f2 = 1.0;
      op->f1 = 0.0;
      op->kind = 0;
    }  else if (op->is_locf == 3){ // midpoint
      op->f1 = op->f2 = 0.5;
      op->kind = 0;
    } else {
      rxSolveFree();
      stop(_("unknown covariate interpolation specified"));
    }
    op->extraCmt=as<int>(rxSolveDat->mv[RxMv_extraCmt]);
    op->nDisplayProgress = nDisplayProgress;
    // Covariate options
    // Simulation variabiles
    // int *svar;
    rxSolveDat->usePar1 = false;
    rxSolveDat->addTimeUnits = false;
    if (rxIs(ev1, "rxEtTran")){
      CharacterVector cls = ev1.attr("class");
      List evT = cls.attr(".rxode2.lst");
      evT.attr("class") = R_NilValue;
      rxSolveDat->covUnits = evT[RxTrans_covUnits];
    }
    rxSolveDat->par1ini = rxSolveDat->par1;
    // This will update par1 with simulated values
#ifdef rxSolveT
    RSprintf("Time7: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif// rxSolveT
    if (!didNesting) {
      rxSolve_simulate(object, rxControl, specParams, extraArgs,
                       params, ev1, inits, rxSolveDat);
      // RSprintf("\nold method:\n");
    } else {
      rxSolveDat->warnIdSort = false;
      rxSolveDat->par1 =  as<RObject>(_rxModels[".nestPars"]);
      rxSolveDat->usePar1=true;
      // RSprintf("\nnesting:\n");
    }
    // .sigma could be reassigned in an update, so check outside simulation function.
    if (_rxModels.exists(".sigma")){
      if (Rf_isMatrix(_rxModels[".sigma"]) && Rf_length(_rxModels[".sigma"]) > 0) {
        rxSolveDat->sigmaN=  as<CharacterVector>((as<List>((as<NumericMatrix>(_rxModels[".sigma"])).attr("dimnames")))[1]);
      } else {
        _rxModels.remove(".sigma");
      }
    } else if (Rf_isMatrix(rxControl[Rxc_sigma]) && Rf_length(rxControl[Rxc_sigma]) > 0) {
      rxSolveDat->sigmaN= as<CharacterVector>((as<List>((as<NumericMatrix>(rxControl[Rxc_sigma])).attr("dimnames")))[1]);
      _rxModels[".sigma"] = rxControl[Rxc_sigma];
    }
    if (_rxModels.exists(".omega")){
      if (Rf_isMatrix(_rxModels[".omega"]) && Rf_length(_rxModels[".omega"]) > 0) {
        rxSolveDat->omegaN= as<CharacterVector>((as<List>((as<NumericMatrix>(_rxModels[".omega"])).attr("dimnames")))[1]);
      } else {
        _rxModels.remove(".omega");
      }
    } else if (_rxModels.exists(".omegaN")) {
      rxSolveDat->omegaN = as<CharacterVector>(_rxModels[".omegaN"]);
    } else if (Rf_isMatrix(rxControl[Rxc_omega]) && Rf_length(rxControl[Rxc_omega]) > 0) {
      _rxModels[".omega"] = rxControl[Rxc_omega];
      rxSolveDat->omegaN= as<CharacterVector>((as<List>((as<NumericMatrix>(rxControl[Rxc_omega])).attr("dimnames")))[1]);
    }
#ifdef rxSolveT
    RSprintf("Time8: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif// rxSolveT
    // This will setup the parameters
    rxSolve_parSetup(object, rxControl, specParams, extraArgs,
                     pars, ev1, inits, rxSolveDat);
#ifdef rxSolveT
    RSprintf("Time9: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif// rxSolveT
    rxOptionsIniEnsure(rxSolveDat->nPopPar); // 1 simulation per parameter specification

    // Setup some data-based parameters like hmax
    rxSolve_datSetupHmax(object, rxControl, specParams, extraArgs,
                         pars, ev1, inits, rxSolveDat);

#ifdef rxSolveT
    RSprintf("Time10: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif // rxSolveT

    // Make sure the user input all the parameters.
    rxSolve_parOrder(object, rxControl, specParams, extraArgs,
                     pars, ev1, inits, rxSolveDat);

#ifdef rxSolveT
    RSprintf("Time11: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif // rxSolveT

    rx->svar = _globals.gsvar;
    rx->ovar = _globals.govar;
    op->nsvar = rxSolveDat->nsvar;
    if (op->nsvar == 0){
      getRxModels();
      if(_rxModels.exists(".sigma")){
        _rxModels.remove(".sigma");
      }
    }
    // Now setup the rest of the rx_solve object
    if (rxSolveDat->nPopPar != 1 && rxSolveDat->nPopPar % rx->nsub != 0){
      rxSolveFree();
      stop(_("number of parameters (%d) solved by rxode2 for multi-subject data needs to be a multiple of the number of subjects (%d)"),rxSolveDat->nPopPar, rx->nsub);
    }
#ifdef rxSolveT
    RSprintf("Time12: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif // rxSolveT
    rxSolveDat->nSize = rxSolveDat->nPopPar*rx->nsub;
    if (rxSolveDat->nPopPar % rx->nsub == 0) rx->nsim = rxSolveDat->nPopPar / rx->nsub;
    else rx->nsim=1;
    IntegerVector linCmtI = rxSolveDat->mv[RxMv_flags];
    int linNcmt = linCmtI[RxMvFlag_ncmt];
    int linKa = linCmtI[RxMvFlag_ka];
    int linB = INTEGER(rxSolveDat->mv[RxMv_flags])[RxMvFlag_linB];
    op->linBflag=0;
    rx->linKa = linKa;
    rx->linNcmt = linNcmt;
    if (linB) {
      int linBflag = INTEGER(rxSolveDat->mv[RxMv_flags])[RxMvFlag_linCmtFlg];
      if (rx->sensType == 4) {
        // This is the ADVAN senstivities
        if (linKa) {
          switch (linNcmt) {
          case 1:
            op->nlin = 8;
            break;
          case 2:
            op->nlin = linNcmt + linKa + (2*linNcmt+linNcmt)*(linNcmt+linKa+1) + 2*linNcmt+1;
            break;
          case 3:
            op->nlin = linNcmt + linKa + (2*linNcmt+linNcmt)*(linNcmt+linKa+1) + 2*linNcmt+1;
            break;
          }
        } else {
          switch (linNcmt) {
          case 1:
            op->nlin = 3;
            break;
          case 2:
            op->nlin = linNcmt + linKa + (2*linNcmt+linNcmt)*(linNcmt+linKa+1) + 2*linNcmt+1;
            break;
          case 3:
            op->nlin = linNcmt + linKa + (2*linNcmt+linNcmt)*(linNcmt+linKa+1) + 2*linNcmt+1;
            break;
          }
        }
        op->nlin2 = op->nlin;
        op->linBflag = linBflag;
        // Add the other components
        if (linBflag & 64){ // tlag 64= bitwShiftL(1, 7-1)
          op->nlin++;
        }
        if (linBflag & 128){ // f 128 = 1 << 8-1
          op->nlin++;
        }
        if (linBflag & 256){ // rate 256 = 1 << 9-1
          op->nlin++;
        }
        if (linBflag & 512){ // dur 512 = 1 << 10-1
          op->nlin++;
        }
        if (linBflag & 2048) { // tlag2 2048 = 1 << 12 - 1
          op->nlin++;
        }
        if (linBflag & 4096) { // f2 4096 = 1 << 13 - 1
          op->nlin++;
        }
        if (linBflag & 8192) { // rate2 8192 = 1 << 14 - 1
          op->nlin++;
        }
        if (linBflag & 16384) { // dur2 16384 = 1 << 15 - 1
          op->nlin++;
        }
      } else {
        op->nlin = linNcmt + linKa + (2*linNcmt+linNcmt)*(linNcmt+linKa+1) + 2*linNcmt+1;//(4+linNcmt+linKa)*linNcmt+(2+linNcmt+linKa)*linKa+1;
        // ncmt + oral0 + (2*ncmt+oral)*(ncmt+oral0+1) + 2*ncmt
      }
    } else {
      op->nlin = linNcmt+linKa;
    }
    op->nlinR = 0;
    int n0 = (rx->nall+3*rx->nsub)*(state.size())*rx->nsim;
    int nLin = op->nlin;
    if (nLin != 0) {
      op->nlinR = 1+linKa;
      nLin = rx->nall*nLin*rx->nsim;// Number of linear compartments * number of solved points
    }
    int n2  = rx->nMtime*rx->nsub*rx->nsim;
    int n3  = op->neq*rxSolveDat->nSize;
    int n3a = (op->neq + op->extraCmt)*rxSolveDat->nSize;
    int n3a_c = (op->neq + op->extraCmt)*op->cores;
#ifdef rxSolveT
    RSprintf("Time12a: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif // rxSolveT

    rxSolveDat->initsC = rxInits(object, inits, state, 0.0);

#ifdef rxSolveT
    RSprintf("Time12b: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif // rxSolveT

    int n4 = rxSolveDat->initsC.size();
    int n5 = lhs.size()*rxSolveDat->nSize;
    // The initial conditions cannot be changed for each individual; If
    // they do they need to be a parameter.
    NumericVector scaleC = rxSetupScale(object, scale, extraArgs);
    int n6 = scaleC.size();
    int nIndSim = rx->nIndSim;
    int n7 =  nIndSim * rx->nsub * rx->nsim;
    if (_globals.gsolve != NULL) free(_globals.gsolve);
    _globals.gsolve = (double*)calloc(n0+nLin+n2+ n4+n5+n6+ n7 +
                                      5*op->neq + 7*n3a + 1*n3a_c,
                                      sizeof(double));// [n0]
#ifdef rxSolveT
    RSprintf("Time12c (double alloc %d): %f\n",n0+nLin+n2+7*n3+n4+n5+n6+ 5*op->neq,((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif // rxSolveT
    if (_globals.gsolve == NULL){
      rxSolveFree();
      stop(_("could not allocate enough memory for solving"));
    }
    _globals.gmtime = _globals.gsolve + n0+ nLin; // [n2]
    _globals.gInfusionRate = _globals.gmtime + n2; //[n3a]
    _globals.gAlag  = _globals.gInfusionRate + n3a; // [n3a_c]
    _globals.gF  = _globals.gAlag + n3a_c; // [n3a]
    _globals.gRate  = _globals.gF + n3a; // [n3a]
    _globals.gDur  = _globals.gRate + n3a; // [n3a]
    _globals.ginits = _globals.gDur + n3a; // [n4]
    std::copy(rxSolveDat->initsC.begin(), rxSolveDat->initsC.end(), &_globals.ginits[0]);
    op->inits = &_globals.ginits[0];
    _globals.glhs = _globals.ginits + n4; // [n5]
    // initially NA_REAL
    //std::fill_n(_globals.glhs,n5, NA_REAL); // TOO slow
    _globals.gscale = _globals.glhs + n5; //[n6]
    std::copy(scaleC.begin(),scaleC.end(),&_globals.gscale[0]);
    op->scale = &_globals.gscale[0];
    _globals.gatol2=_globals.gscale   + n6; //[op->neq]
    _globals.grtol2=_globals.gatol2   + op->neq;  //[op->neq]
    _globals.gssRtol=_globals.grtol2  + op->neq; //[op->neq]
    _globals.gssAtol=_globals.gssRtol + op->neq; //[op->neq]
    // All NA_REAL fill are below;  one statement to initialize them all
    rx->ypNA = _globals.gssAtol + op->neq; // [op->neq]
    _globals.gTlastS = rx->ypNA + op->neq; // [n3a]
    _globals.gTfirstS  = _globals.gTlastS + n3a; // [n3a]
		_globals.gCurDoseS = _globals.gTfirstS + n3a; // [n3a]
    _globals.gIndSim   = _globals.gCurDoseS + n3a;// [n7]
    std::fill_n(rx->ypNA, op->neq + 2*n3a, NA_REAL);

    std::fill_n(&_globals.gatol2[0],op->neq, atolNV[0]);
    std::fill_n(&_globals.grtol2[0],op->neq, rtolNV[0]);
    std::copy(atolNV.begin(), atolNV.begin() + min2(op->neq, atolNV.size()), &_globals.gatol2[0]);
    std::copy(rtolNV.begin(), rtolNV.begin() + min2(op->neq, rtolNV.size()), &_globals.grtol2[0]);

    std::fill_n(&_globals.gssAtol[0],op->neq, atolNVss[0]);
    std::fill_n(&_globals.gssRtol[0],op->neq, rtolNVss[0]);
    std::copy(atolNVss.begin(), atolNVss.begin() + min2(op->neq, atolNVss.size()), &_globals.gssAtol[0]);
    std::copy(rtolNVss.begin(), rtolNVss.begin() + min2(op->neq, rtolNVss.size()), &_globals.gssRtol[0]);
    op->atol2 = &_globals.gatol2[0];
    op->rtol2 = &_globals.grtol2[0];
    op->ssAtol = _globals.gssAtol;
    op->ssRtol = _globals.gssRtol;
    // Not needed since we use Calloc.
    // std::fill_n(&_globals.gsolve[0], rx->nall*state.size()*rx->nsim, 0.0);
    int n1 = rx->nsub*rx->nsim*(state.size() + op->extraCmt);
#ifdef rxSolveT
    RSprintf("Time12d (fill in!): %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif // rxSolveT
    if (_globals.gon != NULL) free(_globals.gon);
    _globals.gon = (int*)calloc(n1+n3 + 4*rxSolveDat->nSize + 2*rx->nall*rx->nsim, sizeof(int)); // [n1]
#ifdef rxSolveT
    RSprintf("Time12e (int alloc %d): %f\n", n1+n3 + 4*rxSolveDat->nSize, ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif // rxSolveT
    std::fill_n(&_globals.gon[0], n1, 1);
    _globals.gBadDose = _globals.gon+n1; // [n3]
    _globals.grc = _globals.gBadDose + n3; //[nSize]
    _globals.slvr_counter = _globals.grc + rxSolveDat->nSize; //[nSize]
    _globals.dadt_counter = _globals.slvr_counter + rxSolveDat->nSize; // [nSize]
    _globals.jac_counter = _globals.dadt_counter + rxSolveDat->nSize; // [nSize]
    _globals.gix=_globals.jac_counter+rxSolveDat->nSize; // rx->nall*rx->nsim

#ifdef rxSolveT
    RSprintf("Time13: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif // rxSolveT
    rxSolve_normalizeParms(object, rxControl, specParams, extraArgs,
                           pars, ev1, inits, rxSolveDat);
    if (op->stiff == 2 || op->stiff == 4) { // liblsoda
      // Order by the number of times per subject
      sortIds(rx, 1);
    }
    if (setupOnly){
      setupOnlyObj = obj;
      return as<SEXP>(LogicalVector::create(true));
    }
#ifdef rxSolveT
    RSprintf("Time14: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif// rxSolveT

    SEXP ret = rxSolve_finalize(object, rxControl, specParams, extraArgs, params, events,
                                inits, rxSolveDat);
    if (!rxIsNull(setupOnlyObj)) {
      rxUnlock(setupOnlyObj);
      setupOnlyObj = R_NilValue;
    }
#ifdef rxSolveT
    RSprintf("Time15: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
    _lastT0 = clock();
#endif // rxSolveT
    return ret;
  }
  return R_NilValue;
}


RObject rxSolveGet_rxSolve(RObject &obj, std::string &sarg, LogicalVector &exact,
                           List &lst) {
  int i, j, n;
  rxCurObj = obj;
  CharacterVector cls = lst.attr("class");
  Environment e = as<Environment>(cls.attr(".rxode2.env"));
  if (sarg == "env"){
    return as<RObject>(e);
  }
  if (sarg == "model"){
    List mv = rxModelVars(obj);
    CharacterVector mods = mv[RxMv_model];
    CharacterVector retS = as<std::string>(mods["normModel"]);
    retS.attr("class") = "rxModelText";
    return(retS);
  }
  updateSolveEnvPost(e);
  if (e.exists(sarg)){
    return e[sarg];
  }
  if (sarg == "params" || sarg == "par" || sarg == "pars" || sarg == "param"){
    List ret = clone(as<List>(e[".params.dat"]));
    return ret;
  } else if (sarg == "inits" || sarg == "init"){
    NumericVector ret = clone(as<NumericVector>(e[".init.dat"]));
    return ret;
  } else if (sarg == "t"){
    return lst["time"];
  } else if ((sarg == "theta.mat" || sarg == "thetaMat") && e.exists(".theta")){
    return e[".theta"];
  } else if ((sarg == "sigma.list" || sarg == "sigmaList") && e.exists(".sigmaL")){
    return e[".sigmaL"];
  } else if ((sarg == "omega.list" || sarg == "omegaList") && e.exists(".omegaL")){
    return e[".omegaL"];
  } else if ((sarg == "theta.list" || sarg == "thetaList")) {
    return e[".thetaL"];
  }
  // Now parameters
  List pars = clone(List(e[".params.dat"]));
  CharacterVector nmp = pars.names();
  n = pars.size();
  for (i = n; i--;){
    if (nmp[i] == sarg){
      return pars[sarg];
    }
  }
  // // Now inis.
  // Function sub("sub", R_BaseNamespace);
  NumericVector ini = clone(NumericVector(e[".init.dat"]));
  CharacterVector nmi = ini.names();
  n = ini.size();
  std::string cur;
  NumericVector retN(1);
  for (i = n; i--; ){
    cur = as<std::string>(nmi[i]) + "0";
    if (cur == sarg){
      retN = ini[i];
      return as<RObject>(retN);
    }
    cur = as<std::string>(nmi[i]) + "_0";
    if (cur == sarg){
      retN = ini[i];
      return as<RObject>(retN);
    }
    cur = as<std::string>(nmi[i]) + ".0";
    if (cur == sarg){
      retN = ini[i];
      return as<RObject>(retN);
    }
    cur = as<std::string>(nmi[i]) + "[0]";
    if (cur == sarg){
      retN = ini[i];
      return as<RObject>(retN);
    }
    cur = as<std::string>(nmi[i]) + "(0)";
    if (cur == sarg){
      retN = ini[i];
      return as<RObject>(retN);
    }
    cur = as<std::string>(nmi[i]) + "{0}";
    if (cur == sarg){
      retN = ini[i];
      return as<RObject>(retN);
    }
  }
  List mv = rxModelVars(obj);
  if (sarg == "rx" || sarg == "rxode2" || sarg == "rxode2"){
    CharacterVector trans = mv[RxMv_trans];
    getRxModels();
    std::string pre = as<std::string>(trans[RxMvTrans_prefix]);
    if (_rxModels.exists(pre)){
      return as<RObject>(_rxModels[pre]);
    }
  }
  CharacterVector normState = mv[RxMv_normal_state];
  CharacterVector parsC = mv[RxMv_params];
  CharacterVector lhsC = mv[RxMv_lhs];
  for (i = normState.size(); i--;){
    for (j = parsC.size(); j--; ){
      std::string test = "_sens_" + as<std::string>(normState[i]) + "_" + as<std::string>(parsC[j]);
      if (test == sarg){
        test = "rx__sens_" + as<std::string>(normState[i]) + "_BY_" + as<std::string>(parsC[j]) + "__";
        return lst[test];
      }
      test = as<std::string>(normState[i]) + "_" + as<std::string>(parsC[j]);
      if (test == sarg){
        test = "rx__sens_" + as<std::string>(normState[i]) + "_BY_" + as<std::string>(parsC[j]) + "__";
        return lst[test];
      }
      test = as<std::string>(normState[i]) + "." + as<std::string>(parsC[j]);
      if (test == sarg){
        test = "rx__sens_" + as<std::string>(normState[i]) + "_BY_" + as<std::string>(parsC[j]) + "__";
        return lst[test];
      }
    }
    for (j = lhsC.size(); j--;){
      std::string test = "_sens_" + as<std::string>(normState[i]) + "_" + as<std::string>(lhsC[j]);
      if (test == sarg){
        test = "rx__sens_" + as<std::string>(normState[i]) + "_BY_" + as<std::string>(lhsC[j]) + "__";
        return lst[test];
      }
      test = as<std::string>(normState[i]) + "_" + as<std::string>(lhsC[j]);
      if (test == sarg){
        test = "rx__sens_" + as<std::string>(normState[i]) + "_BY_" + as<std::string>(lhsC[j]) + "__";
        return lst[test];
      }
      test = as<std::string>(normState[i]) + "." + as<std::string>(lhsC[j]);
      if (test == sarg){
        test = "rx__sens_" + as<std::string>(normState[i]) + "_BY_" + as<std::string>(lhsC[j]) + "__";
        return lst[test];
      }
    }
  }
  return R_NilValue;
}

//[[Rcpp::export]]
CharacterVector rxSolveDollarNames(RObject obj){
  CharacterVector names1 = obj.attr("names");
  List lst = as<List>(obj);
  CharacterVector cls = lst.attr("class");
  Environment e = as<Environment>(cls.attr(".rxode2.env"));
  updateSolveEnvPost(e);
  int nExtra = 6;
  if (e.exists(".theta")) nExtra++;
  if (e.exists(".sigmaL")) nExtra++;
  if (e.exists(".thetaL")) nExtra++;
  if (e.exists(".omegaL")) nExtra++;

  List pars = List(e[".params.dat"]);
  CharacterVector pn = pars.attr("names");

  NumericVector ini = NumericVector(e[".init.dat"]);
  CharacterVector nmi = ini.names();

  CharacterVector envl = e.ls(false);

  CharacterVector ret(nExtra + names1.size() +
											pn.size() + nmi.size() + envl.size());

  int j = 0;
  for (unsigned int i = names1.size(); i--;){
    ret[j++] = names1[i];
  }

  for (unsigned int i = pn.size(); i--;){
    ret[j++] = pn[i];
  }

  for (unsigned int i = nmi.size(); i--;){
    ret[j++] = as<std::string>(nmi[i]) + "0";
  }

  for (unsigned int i = envl.size(); i--;){
    ret[j++] = envl[i];
  }

  ret[j++] = "env";
  ret[j++] = "model";
  ret[j++] = "params";
  ret[j++] = "inits";
  ret[j++] = "t";
  ret[j++] = "rxode2";
  if (e.exists(".theta")) ret[j++] = "thetaMat";
  if (e.exists(".sigmaL")) ret[j++] = "sigmaList";
  if (e.exists(".thetaL")) ret[j++] = "thetaList";
  if (e.exists(".omegaL")) ret[j++] = "omegaList";
  return ret;
}

//[[Rcpp::export]]
RObject rxSolveGet(RObject obj, RObject arg, LogicalVector exact = true){
  std::string sarg;
  int i, n;
  if (rxIs(obj, "data.frame")){
    List lst = as<List>(obj);
    if (rxIsChar(arg)){
      sarg = as<std::string>(arg);
      CharacterVector nm = lst.names();
      n = nm.size();
      unsigned int slen = strlen(sarg.c_str());
      int dexact = -1;
      if (exact[0] == TRUE){
				dexact = 1;
      } else if (exact[0] == FALSE){
				dexact = 0;
      }
      unsigned int slen2;
      int possible = -1;
      for (i = 0; i < n; i++){
				slen2 = strlen((as<std::string>(nm[i])).c_str());
				if (slen <= slen2 &&
						(strncmp((as<std::string>(nm[i])).c_str(), sarg.c_str(), slen)  == 0 ) &&
						(dexact != 1 || (dexact == 1 && slen == slen2))){
					if (slen != slen2){
						possible = i;
					} else {
						return lst[i];
					}
				}
      }
      if (possible != -1){
				if (dexact == -1){
					warning(_("partial match of '%s' to '%s'"),sarg.c_str(),
									(as<std::string>(nm[possible])).c_str());
				}
				return lst[possible];
      }
      if (rxIs(obj, "rxSolve")){
				RObject ret0 = rxSolveGet_rxSolve(obj, sarg, exact, lst);
				if (!rxIsNull(ret0)){
					return ret0;
				}
      }
    } else {
      if (rxIsNumInt(arg)){
				int iarg = as<int>(arg);
				if (iarg <= lst.size()){
					return lst[iarg-1];
				}
      }
    }
  }
  return R_NilValue;
}

//[[Rcpp::export]]
RObject rxSolveUpdate(RObject obj,
											RObject arg = R_NilValue,
											RObject value = R_NilValue){
  if (rxIs(obj,"rxSolve")){
    rxCurObj = obj;
    if (rxIsChar(arg)){
      CharacterVector what = CharacterVector(arg);
      CharacterVector classattr = obj.attr("class");
      Environment e = as<Environment>(classattr.attr(".rxode2.env"));
      List rxControl = e[".args"];
      // updateSolveEnvPost(e);
      if (what.size() == 1){
				std::string sarg = as<std::string>(what[0]);
				// Now check to see if this is something that can be updated...
				if (sarg == "params"){
					// rxControl[Rxc_params] = value;
					return rxSolve_(obj,rxControl,
                          CharacterVector::create("params"),
													R_NilValue,
                          value, //defrx_params,
													List(e[".args.events"]),
                          defrx_inits, 0);
				} else if (sarg == "events"){
					return rxSolve_(obj,rxControl,
													CharacterVector::create("events"),
													R_NilValue,
													List(e[".params.dat"]),
													value, // defrx_events,
													defrx_inits, 0);
				} else if (sarg == "inits"){
					return rxSolve_(obj, rxControl,
                          CharacterVector::create("inits"),
													R_NilValue,
													List(e[".params.dat"]),
													List(e[".args.events"]),
                          as<RObject>(value), //defrx_inits,
													0);
				} else if (sarg == "t" || sarg == "time"){
					CharacterVector classattr = obj.attr("class");
          Environment e = as<Environment>(classattr.attr(".rxode2.env"));
          updateSolveEnvPost(e);
					Function f = as<Function>(e[".replace.sampling"]);
					return f(value);
        } else {
					CharacterVector classattr = obj.attr("class");
					Environment e = as<Environment>(classattr.attr(".rxode2.env"));
          updateSolveEnvPost(e);
          if (rxIsNull(e[".params.dat"])) {
						rxSolveFree();
						stop(_("cannot update nonexistent parameters"));
					}
          List pars = List(e[".params.dat"]);
					CharacterVector nmp = pars.names();
					int i, n, np, nc, j;
					np = (as<NumericVector>(pars[0])).size();
					List events = List(e[".args.events"]);
					CharacterVector nmc;
					nmc = events.names();
					nc = (as<NumericVector>(events[0])).size();
					////////////////////////////////////////////////////////
					// Update Parameters by name
					n = pars.size();
					for (i = n; i--; ) {
						if (nmp[i] == sarg) {
							// Update solve.
							NumericVector val = NumericVector(value);
							if (val.size() == np){
								// Update Parameter
								pars[i] = val;
								return rxSolve_(obj,rxControl,
																CharacterVector::create("params"),
																R_NilValue,
																pars, //defrx_params,
																List(e[".args.events"]),
																defrx_inits, 0);
							} else if (val.size() == nc){
								// Change parameter -> Covariate
								List newPars(pars.size()-1);
								CharacterVector newParNames(pars.size()-1);
								for (j = i; j--;){
									newPars[j]     = pars[j];
									newParNames[j] = nmp[j];
								}
								for (j=i+1; j < pars.size(); j++){
									newPars[j-1]     = pars[j];
									newParNames[j-1] = nmp[j];
								}
								newPars.attr("names") = newParNames;
								newPars.attr("class") = "data.frame";
								newPars.attr("row.names") = IntegerVector::create(NA_INTEGER,-np);
								List newEvents(events.size()+1);
								CharacterVector newEventsNames(events.size()+1);
								for (j = events.size(); j--;){
									newEvents[j]      = events[j];
									newEventsNames[j] = nmc[j];
								}
								j = events.size();
								newEvents[j]      = val;
								newEventsNames[j] = nmp[i];
								newEvents.attr("names") = newEventsNames;
								newEvents.attr("class") = "data.frame";
								newEvents.attr("row.names") = IntegerVector::create(NA_INTEGER,-nc);
								return rxSolve_(obj, rxControl,
																CharacterVector::create("params","events"),
																R_NilValue,
																newPars, //defrx_params,
																newEvents,
																defrx_inits, 0);
							}
							return R_NilValue;
						}
					}
					////////////////////////////////////////////////////////////////
					// Update Covariates by covariate name
					n = events.size();
					for (i = n; i--;){
						if (nmc[i] == sarg){
							// Update solve.
							NumericVector val = NumericVector(value);
							if (val.size() == nc){
								// Update Covariate
								events[i]=val;
								return rxSolve_(obj, rxControl,
																CharacterVector::create("events"),
																R_NilValue,
																defrx_params,
																events,
																defrx_inits, 0);
							} else if (val.size() == np){
								// Change Covariate -> Parameter
								List newPars(pars.size()+1);
								CharacterVector newParNames(pars.size()+1);
								for (j = 0; j < pars.size(); j++){
									newPars[j]     = pars[j];
									newParNames[j] = nmp[j];
								}
								newPars[j]     = val;
								newParNames[j] = nmc[i];
								newPars.attr("names") = newParNames;
								newPars.attr("class") = "data.frame";
								newPars.attr("row.names") = IntegerVector::create(NA_INTEGER,-np);
								// if ()
								List newEvents(events.size()-1);
								CharacterVector newEventsNames(events.size()-1);
								for (j = 0; j < i; j++){
									newEvents[j]      = events[j];
									newEventsNames[j] = nmc[j];
								}
								for (j=i+1; j < events.size(); j++){
									newEvents[j-1]      = events[j];
									newEventsNames[j-1] = nmc[j];
								}
								newEvents.attr("names") = newEventsNames;
								newEvents.attr("class") = "data.frame";
								newEvents.attr("row.names") = IntegerVector::create(NA_INTEGER,-nc);
								return rxSolve_(obj,rxControl,
																CharacterVector::create("events", "params"),
																R_NilValue,
																newPars,//defrx_params,
																newEvents,
																defrx_inits, 0);
							}
						}
					}
					////////////////////////////////////////////////////
          // Update Initial Conditions
					NumericVector ini = NumericVector(e[".init.dat"]);
          CharacterVector nmi = ini.names();
          n = ini.size();
          std::string cur;
					bool doIt = false;
          for (i = 0; i < n; i++){
            cur = as<std::string>(nmi[i]) + "0";
						if (cur == sarg){
							doIt = true;
						} else {
              cur = as<std::string>(nmi[i]) + ".0";
              if (cur == sarg){
                doIt = true;
							} else {
								cur = as<std::string>(nmi[i]) + "_0";
                if (cur == sarg){
									doIt = true;
								} else {
									cur = as<std::string>(nmi[i]) + "(0)";
                  if (cur == sarg){
                    doIt = true;
                  } else {
										cur = as<std::string>(nmi[i]) + "[0]";
                    if (cur == sarg){
											doIt = true;
										}
									}
								}
							}
						}
						if (doIt){
							cur=as<std::string>(nmi[i]);
              NumericVector ini = NumericVector(e[".init.dat"]);
							double v = asDouble(value, "value");
							for (j = 0; j < n; j++){
								if (cur == as<std::string>(nmi[j])){
									ini[j] = v;
								}
							}
              return rxSolve_(obj, rxControl,
															CharacterVector::create("inits"),
															R_NilValue,
															List(e[".params.dat"]),
															List(e[".args.events"]),
															ini, 0);
						}
					}
					return R_NilValue;
				}
      }
    }
  }
  return R_NilValue;
}

//[[Rcpp::export]]
SEXP rxSolveSEXP(SEXP objS,
								 SEXP rxControlS,
								 SEXP specParamsS,
								 SEXP extraArgsS,
								 SEXP paramsS,
								 SEXP eventsS,
								 SEXP initsS,
								 SEXP setupOnlyS) {
  const RObject obj = as<const RObject>(objS);
  qassertS(rxControlS, "l", "rxControl");
  const List rxControl=as<const List>(rxControlS);
  const Nullable<CharacterVector> specParams = as<const Nullable<CharacterVector>>(specParamsS);
  const Nullable<List> extraArgs= as<const Nullable<List>>(extraArgsS);
  const RObject params = as<const RObject>(paramsS);
  const RObject events = as<const RObject>(eventsS);
  const RObject inits = as<const RObject>(initsS);
  const int setupOnly = as<const int>(setupOnlyS);
  return rxSolve_(obj, rxControl, specParams, extraArgs,
									params, events, inits, setupOnly);
}


extern "C" SEXP rxGetModelLib(const char *s){
  std::string str(s);
  getRxModels();
  if (_rxModels.exists(str)){
    return wrap(_rxModels.get(str));
  } else {
    return R_NilValue;
  }
}

//[[Rcpp::export]]
void rxRmModelLib_(std::string str){
  getRxModels();
  if (_rxModels.exists(str)){
    List trans =(as<List>(as<List>(_rxModels[str]))["trans"]);
    std::string rxlib = as<std::string>(trans[RxMvTrans_prefix]);
    _rxModels.remove(str);
    if (_rxModels.exists(rxlib)){
      _rxModels.remove(rxlib);
    }
  }
}
extern "C" void rxRmModelLib(const char* s){
  std::string str(s);
  rxClearFuns();
  rxRmModelLib_(str);
}

Nullable<Environment> rxrxode2env(RObject obj){
  if (rxIs(obj, "rxode2")){
    return(as<Environment>(obj));
  } else if (rxIs(obj, "rxSolve")){
    CharacterVector cls = obj.attr("class");
    Environment e = as<Environment>(cls.attr(".rxode2.env"));
    return rxrxode2env(as<RObject>(e["args.object"]));
  } else if (rxIs(obj, "rxModelVars")){
    List mv = as<List>(obj);
    CharacterVector trans = mv[RxMv_trans];
    getRxModels();
    std::string prefix = as<std::string>(trans[RxMvTrans_prefix]);
    if (_rxModels.exists(prefix)){
      return as<Environment>(_rxModels[prefix]);
    } else {
      return R_NilValue;
    }
  } else {
    return rxrxode2env(as<RObject>(rxModelVars(obj)));
  }
}

//' Get rxode2 model from object
//' @param obj rxode2 family of objects
//' @return rxode2 model
//' @export
//[[Rcpp::export]]
RObject rxGetrxode2(RObject obj){
  Nullable<Environment> rxode21 = rxrxode2env(obj);
  if (rxode21.isNull()){
    // FIXME compile if needed.
    rxSolveFree();
    stop(_("Can not figure out the rxode2 object"));
  } else {
    Environment e = as<Environment>(rxode21);
    e.attr("class") = "rxode2";
    return as<RObject>(e);
  }
}

//' Checks if the rxode2 object was built with the current build
//'
//' @inheritParams rxModelVars
//'
//' @return boolean indicating if this was built with current rxode2
//'
//' @export
//[[Rcpp::export]]
bool rxIsCurrent(RObject obj){
  List mv = rxModelVars(obj);
  if (mv.containsElementNamed("version")){
    CharacterVector version = mv[RxMv_version];
    std::string str = __VER_md5__;
    std::string str2 = as<std::string>(version["md5"]);
    if (str2 == str){
      return true;
    } else {
      return false;
    }
  } else {
    return false;
  }
}

//' Assign pointer based on model variables
//' @param object rxode2 family of objects
//' @return nothing, called for side effects
//' @export
//[[Rcpp::export]]
void rxAssignPtr(SEXP object = R_NilValue){
  List mv=rxModelVars(as<RObject>(object));
  CharacterVector trans = mv[RxMv_trans];
  rxode2_assign_fn_pointers_((as<std::string>(trans[RxMvTrans_model_vars])).c_str());
  rxUpdateFuns(as<SEXP>(trans));
  getRxSolve_();
  // Update rxModels environment.
  getRxModels();

  std::string ptr = as<std::string>(trans[RxMvTrans_model_vars]);
  if (!_rxModels.exists(ptr)){
    _rxModels[ptr] = mv;
  } else if (!rxIsCurrent(as<RObject>(_rxModels[ptr]))) {
    _rxModels[ptr] = mv;
  }
  Nullable<Environment> e1 = rxrxode2env(object);
  if (!e1.isNull()){
    std::string prefix = as<std::string>(trans[RxMvTrans_prefix]);
    if (!_rxModels.exists(prefix)){
      Environment e = as<Environment>(e1);
      _rxModels[prefix] = e;
    }
  }
}

extern "C" void rxAssignPtrC(SEXP obj){
  rxAssignPtr(obj);
}

//' Return the DLL associated with the rxode2 object
//'
//' This will return the dynamic load library or shared object used to
//' run the C code for rxode2.
//'
//' @param obj A rxode2 family of objects or a character string of the
//'     model specification or location of a file with a model
//'     specification.
//'
//' @return a path of the library
//'
//' @keywords internal
//' @author Matthew L.Fidler
//' @export
//[[Rcpp::export]]
std::string rxDll(RObject obj){
  if (rxIs(obj,"rxode2")){
    Environment e = as<Environment>(obj);
    Nullable<CharacterVector>pkgn = e["package"];
    if (!pkgn.isNull()){
      Function rxPkgDll = getRxFn(".rxPkgDll");
      return(as<std::string>(rxPkgDll(wrap(obj))));
    }
    return as<std::string>((as<List>(e["rxDll"]))["dll"]);
  } else if (rxIs(obj,"rxSolve")) {
    CharacterVector cls = obj.attr("class");
    Environment e = as<Environment>(cls.attr(".rxode2.env"));
    return(as<std::string>(e["dll"]));
  } else if (rxIs(obj, "rxDll")){
    return as<std::string>(as<List>(obj)["dll"]);
  } else if (rxIs(obj, "character")){
    Function f = getRxFn("rxCompile.character");
    RObject newO = f(as<std::string>(obj));
    return(rxDll(newO));
  } else {
    List mv = rxModelVars(obj);
    Nullable<Environment> en = rxrxode2env(mv);
    if (en.isNull()){
      rxSolveFree();
      stop(_("can not figure out the DLL for this object"));
    } else {
      Environment e = as<Environment>(en);
      return as<std::string>((as<List>(e["rxDll"]))["dll"]);
    }
  }
}

//' Return the C file associated with the rxode2 object
//'
//' This will return C code for generating the rxode2 DLL.
//'
//' @param obj A rxode2 family of objects or a character string of the
//'     model specification or location of a file with a model
//'     specification.
//'
//' @return a path of the library
//'
//' @keywords internal
//' @author Matthew L.Fidler
//' @export
//[[Rcpp::export]]
CharacterVector rxC(RObject obj){
  std::string rets;
  CharacterVector ret(1);
  if (rxIs(obj,"rxode2")){
    Environment e = as<Environment>(obj);
    rets = as<std::string>((as<List>(e["rxDll"]))["c"]);
  } else if (rxIs(obj,"rxSolve")) {
    CharacterVector cls = obj.attr("class");
    Environment e = as<Environment>(cls.attr(".rxode2.env"));
    rets = as<std::string>(e["c"]);
  } else if (rxIs(obj, "rxDll")){
    rets = as<std::string>(as<List>(obj)["c"]);
  } else if (rxIs(obj, "character")){
    Function f = getRxFn("rxCompile.character");
    RObject newO = f(as<std::string>(obj));
    rets = rxDll(newO);
  } else {
    List mv = rxModelVars(obj);
    Nullable<Environment> en = rxrxode2env(mv);
    if (en.isNull()){
      rxSolveFree();
      stop(_("can not figure out the DLL for this object"));
    } else {
      Environment e = as<Environment>(en);
      rets = as<std::string>((as<List>(e["rxDll"]))["dll"]);
    }
  }
  ret[0] = rets;
  ret.attr("class") = "rxC";
  return ret;
}


//' Determine if the DLL associated with the rxode2 object is loaded
//'
//' @param obj A rxode2 family of objects
//'
//' @return Boolean returning if the rxode2 library is loaded.
//'
//' @keywords internal
//' @author Matthew L.Fidler
//' @export
//[[Rcpp::export]]
bool rxIsLoaded(RObject obj){
  if (obj.isNULL()) return false;
  Function isLoaded("is.loaded", R_BaseNamespace);
  List mv = rxModelVars(obj);
  CharacterVector trans = mv[RxMv_trans];
  std::string dydt = as<std::string>(trans[RxMvTrans_model_vars]);
  bool ret = asBool(isLoaded(dydt), "isLoaded(dydt)");
  return ret;
}

//' Load rxode2 object
//'
//' @param obj A rxode2 family of objects
//'
//' @return Boolean returning if the rxode2 library is loaded.
//'
//' @keywords internal
//' @author Matthew L.Fidler
//' @export
//[[Rcpp::export]]
bool rxDynLoad(RObject obj){
  if (!rxIsLoaded(obj)){
    std::string file = rxDll(obj);
    if (fileExists(file)){
      dynLoad(file);
    } else {
      Nullable<Environment> e1 = rxrxode2env(obj);
      if (!e1.isNull()){
				Environment e = asEnv(e1, "e1");
				Function compile = as<Function>(e["compile"]);
				compile();
      }
    }
  }
  if (rxIsLoaded(obj)){
    rxAssignPtr(obj);
    return true;
  } else {
    return false;
  }
}

//' Lock/unlocking of rxode2 dll file
//'
//' @param obj A rxode2 family of objects
//' 
//' @return nothing; called for side effects
//' 
//' @export
//[[Rcpp::export]]
RObject rxLock(RObject obj){
  getRxModels();
  std::string file = rxDll(obj);
  int ret = 1;
  if (_rxModels.exists(file)){
    ret = asInt(_rxModels[file], "_rxModels[file]");
    ret = ret+1;
    _rxModels[file] = ret;
  } else {
    _rxModels[file] = ret;
  }
  return R_NilValue;
}

//' @rdname rxLock
//' @export
//[[Rcpp::export]]
RObject rxUnlock(RObject obj){
  getRxModels();
  std::string file = rxDll(obj);
  int ret;
  if (_rxModels.exists(file)){
    ret = asInt(_rxModels[file], "_rxModels[file]");
    ret = ret - 1;
    if (ret > 0) _rxModels[file] = ret;
    else  _rxModels[file] = 0;
  }
  return R_NilValue;
}

bool rxCanUnload(RObject obj){
  getRxModels();
  std::string file = rxDll(obj);
  if(_rxModels.exists(file)){
    int ret = asInt(_rxModels[file], "_rxModels[file]");
    return (ret == 0L);
  }
  return true;
}

void rmRxModelsFromDll(std::string str){
  Function getInfo = getRxFn(".rxGetModelInfoFromDll");
  CharacterVector extra = getInfo(str);
  for (int j = extra.size(); j--;){
    if (_rxModels.exists(as<std::string>(extra[j]))){
      _rxModels.remove(as<std::string>(extra[j]));
    }
  }
  if (_rxModels.exists(str)){
    _rxModels.remove(str);
  }
}
bool rxUnload_ = true;
//' Allow unloading of dlls
//'
//' @param allow boolean indicating if garbage collection will unload of rxode2 dlls.
//'
//' @return Boolean allow; called for side effects
//'
//' @examples
//'
//' # Garbage collection will not unload un-used rxode2 dlls
//' rxAllowUnload(FALSE);
//'
//' # Garbage collection will unload unused rxode2 dlls
//' rxAllowUnload(TRUE);
//' @export
//' @author Matthew Fidler
//[[Rcpp::export]]
bool rxAllowUnload(bool allow){
  rxUnload_=allow;
  return rxUnload_;
}

//[[Rcpp::export]]
RObject rxUnloadAll_(){
  getRxModels();
  Function dynUnload("dyn.unload", R_BaseNamespace);
  CharacterVector vars = _rxModels.ls(true);
  std::string exclude = ".rxSolveDat.";
  for (unsigned int i = vars.size(); i--;) {
    std::string varC = as<std::string>(vars[i]);
    if (varC.find(exclude) == std::string::npos){
      if (rxIsInt(_rxModels[varC])){
        int val = asInt(_rxModels[varC], "_rxModels[varC]");
        if (val > 1){
        } else if (val == 0 && rxUnload_){
          dynUnload(varC);
          rmRxModelsFromDll(varC);
        }
      }
    }
  }
  // Now get
  return R_NilValue;
}

//' Unload rxode2 object
//'
//' @param obj A rxode2 family of objects
//'
//' @return Boolean returning if the rxode2 library is loaded.
//'
//' @keywords internal
//' @author Matthew L.Fidler
//' @export
//[[Rcpp::export]]
bool rxDynUnload(RObject obj){
  if (!rxUnload_) return false;
  if (rxIs(obj, "rxode2")){
    Environment e = asEnv(obj, "rxDynUnload(obj)");
    Nullable<CharacterVector> pkg = e["package"];
    if (!pkg.isNull()){
      std::string sobj = asStr(e["modName"], "e[\"modName\"]");
      if (sobj.find("_new")==std::string::npos){
				rxSolveFree();
				stop(_("package-based models cannot be unloaded"));
      }
    }
  }
  List mv = rxModelVars(obj);
  CharacterVector trans = mv[RxMv_trans];
  std::string ptr = asStr(trans[RxMvTrans_model_vars], "trans[\"model_vars\"]");
  if (rxIsLoaded(obj)){
    Function dynUnload("dyn.unload", R_BaseNamespace);
    std::string file = rxDll(obj);
    rxUnlock(obj);
    if (rxCanUnload(obj)){
      dynUnload(file);
    } else {
      rxLock(obj);
      return false;
    }
  }
  rxRmModelLib_(ptr); // Clears all pointers
  std::string file = rxDll(obj);
  getRxModels();
  if(_rxModels.exists(file)){
    _rxModels.remove(file);
  }
  return !(rxIsLoaded(obj));
}

//' Delete the DLL for the model
//'
//' This function deletes the DLL, but doesn't delete the model
//' information in the object.
//'
//' @param obj rxode2 family of objects
//'
//' @return A boolean stating if the operation was successful.
//'
//' @author Matthew L.Fidler
//' @export
//[[Rcpp::export]]
bool rxDelete(RObject obj){
  if (rxIs(obj, "rxode2")){
    Environment e = asEnv(obj, "rxDelete(obj)");
    Nullable<CharacterVector> pkg = e["package"];
    if (!pkg.isNull()){
      std::string sobj = asStr(e["modName"], "e[\"modName\"]");
      if (sobj.find("_new")==std::string::npos){
				rxSolveFree();
				stop(_("package-based models cannot be deleted"));
      }
    }
  }
  std::string file = rxDll(obj);
  if (rxDynUnload(obj)){
    CharacterVector cfileV = rxC(obj);
    std::string cfile = asStr(cfileV[0], "cfileV[0]");
    if (fileExists(cfile)) remove(cfile.c_str());
    if (!fileExists(file)) return true;
    if (remove(file.c_str()) == 0) return true;
  }
  return false;
}

extern "C" SEXP rxModelVarsC(char *ptr) {
  // Rcout << "rxModelVars C: ";
  return rxGetFromChar(ptr, "");
}

extern "C" SEXP rxStateNames(char *ptr) {
  // Rcout << "State: ";
  return rxGetFromChar(ptr, "state");
}

extern "C" SEXP rxLhsNames(char *ptr) {
  // Rcout << "Lhs: ";
  return rxGetFromChar(ptr, "lhs");
}

extern "C" SEXP rxParamNames(char *ptr) {
  // Rcout << "Param: ";
  return rxGetFromChar(ptr, "params");
}

extern "C" int rxIsCurrentC(SEXP obj) {
  RObject robj = as<RObject>(obj);
  if (robj.isNULL()) return 0;
  bool ret = rxIsCurrent(robj);
  if (ret) return 1;
  return 0;
}

extern "C" int Rcat(char *msg) {
  std::string str(msg);
  Rcpp::Function msgF("message");
  msgF(str, _["appendLF"]=false);
  return 1;
}

int isRstudioI = 0;

//[[Rcpp::export]]
SEXP setRstudio(bool isRstudio=false){
  if (isRstudio) isRstudioI=1;
  else isRstudioI=0;
  return wrap(isRstudioI);
}

extern "C" int isRstudio(){
  return isRstudioI;
}


int isProgSupportedI = 1;

//[[Rcpp::export]]
SEXP setProgSupported(int isSupported=1){
  isProgSupportedI=isSupported;
  return wrap(isProgSupportedI);
}

extern "C" int isProgSupported() {
  return isProgSupportedI;
}

//[[Rcpp::export]]
SEXP getProgSupported(){
  return wrap(isProgSupportedI);
}
//[[Rcpp::export]]
List rxUpdateTrans_(List ret, std::string prefix, std::string libName){
  CharacterVector oldTrans = asCv(ret["trans"], "ret[\"trans\"]");
  CharacterVector newLst(22);
  CharacterVector newNme(22);
  newNme[0] ="lib.name";
  newLst[0] = libName;

  newNme[1] = "jac";
  newLst[1] = asStr(oldTrans[1], "oldTrans[1]");

  newNme[2] = "prefix";
  newLst[2] = prefix;

  newNme[3] = "dydt";
  newLst[3] = prefix + "dydt";

  newNme[4] = "calc_jac";
  newLst[4] = prefix + "calc_jac";

  newNme[5] = "calc_lhs";
  newLst[5] = prefix + "calc_lhs";

  newNme[6] = "model_vars";
  newLst[6] = prefix + "model_vars";

  newNme[7] = "theta";
  newLst[7] = prefix + "theta";

  newNme[8]="inis";
  newLst[8]= prefix + "inis";

  newNme[9]="dydt_lsoda";
  newLst[9]= prefix + "dydt_lsoda";

  newNme[10]="calc_jac_lsoda";
  newLst[10]= prefix + "calc_jac_lsoda";

  newNme[11]= "ode_solver_solvedata";
  newLst[11]= prefix + "ode_solver_solvedata";

  newNme[12] = "ode_solver_get_solvedata";
  newLst[12] = prefix+"ode_solver_get_solvedata";

  newNme[13]= "dydt_liblsoda";
  newLst[13]= prefix + "dydt_liblsoda";

  newNme[14]="F";
  newLst[14]= prefix + "F";

  newNme[15] ="Lag";
  newLst[15] = prefix + "Lag";

  newNme[16]= "Rate";
  newLst[16] = prefix + "Rate";

  newNme[17] ="Dur";
  newLst[17] = prefix + "Dur";

  newNme[18] = "mtime";
  newLst[18] = prefix + "mtime";

  newNme[19] = "assignFuns";
  newLst[19] = prefix + "assignFuns";

  newNme[20] = "ME";
  newLst[20] = prefix + "ME";

  newNme[21] = "IndF";
  newLst[21]= prefix + "IndF";

  newLst.attr("names") = newNme;

  ret[3] = newLst;

  return(ret);
}

//[[Rcpp::export]]
List dropUnitsRxSolve(List x){
  List ret;
  if (rxIs(x, "rxSolve")){
    ret = clone(x);
    for (int j = (int)ret.size(); j--;){
      if (rxIs(ret[j],"units")){
				RObject cur = ret[j];
				cur.attr("units") = R_NilValue;
				cur.attr("class") = R_NilValue;
      }
    }
  }
  return ret;
}

//' Silence some of rxode2's C/C++ messages
//'
//' @param silent can be 0L "noisy"  or 1L "silent"
//'
//' @keywords internal
//' @return TRUE; called for side effects
//' @export
//[[Rcpp::export]]
bool rxSetSilentErr(int silent){
  setSilentErr(silent);
  return true;
}

#undef MV
