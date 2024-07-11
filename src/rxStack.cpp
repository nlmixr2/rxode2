#define USE_FC_LEN_T
//#undef NDEBUG
#define STRICT_R_HEADERS
#include <Rcpp.h>
#include "../inst/include/rxode2parse.h"

using namespace Rcpp;

#define rxModelVars(a) rxModelVars_(a)


List rxModelVars(const RObject &obj);

bool hasElement(CharacterVector one, std::string what){
  for (unsigned int i = one.size(); i--;){
    if (as<std::string>(one[i]) == what) return true;
  }
  return false;
}

//[[Rcpp::export]]
List rxStack_(List Data, Nullable<CharacterVector> vars=R_NilValue){
  List mv = rxModelVars(Data);
  CharacterVector lhs = mv[RxMv_lhs];
  CharacterVector state = mv[RxMv_state];
  IntegerVector stateIgnore = mv[RxMv_state_ignore];
  int nfactor = lhs.size();
  int j, k;
  bool allVars = vars.isNull();
  CharacterVector keep;
  if (!allVars){
    keep = CharacterVector(vars);
    nfactor = 0;
    for (j = 0; j < lhs.size(); ++j){
      if (hasElement(keep, as<std::string>(lhs[j]))) nfactor++;
    }
  }
  for (j = 0; j < state.size(); ++j){
    if (stateIgnore[j] == 0){
      if (allVars || hasElement(keep, as<std::string>(state[j])))
        nfactor++;
    }
  }
  CharacterVector lvl(nfactor);
  k=0;
  for (j = 0; j < state.size(); ++j){
    if (stateIgnore[j] == 0) {
      if (allVars || hasElement(keep, as<std::string>(state[j]))){
        lvl[k++] = state[j];
      }
    }
  }
  for (j = 0; j < lhs.size(); ++j){
    if (allVars || hasElement(keep, as<std::string>(lhs[j]))){
      lvl[k++] = lhs[j];
    }
  }
  // See if the factors are added to your stacked data frame.
  int ncols=3;
  bool bSimId=Data.containsElementNamed("sim.id");
  if (bSimId) ncols++;
  bool bId=Data.containsElementNamed("id");
  if (bId) ncols++;
  bool bReset=Data.containsElementNamed("resetno");
  if (bReset) ncols++;
  bool bEvid=Data.containsElementNamed("evid");
  if (bEvid) ncols++;
  bool bAmt=Data.containsElementNamed("amt");
  if (bAmt) ncols++;
  List ret;

  IntegerVector inSimId;
  IntegerVector outSimId;
  if (bSimId){
    inSimId = Data["sim.id"];
    outSimId = IntegerVector(inSimId.size()*nfactor);
    for (j = nfactor; j--;){
      std::copy(inSimId.begin(),inSimId.end(),outSimId.begin()+j*inSimId.size());
    }
    ret["sim.id"] = outSimId;
  }
  IntegerVector inId;
  IntegerVector outId;
  bool hasLvl = false;
  CharacterVector lvls;
  if (bId){
    inId = Data["id"];
    RObject inIdRO = Data["id"];
    if (inIdRO.hasAttribute("levels")) {
      hasLvl = true;
      lvls = inIdRO.attr("levels");
    }
    outId = IntegerVector(inId.size()*nfactor);
    for (j = nfactor; j--;){
      std::copy(inId.begin(),inId.end(),outId.begin()+j*inId.size());
    }
    if (hasLvl) {
      outId.attr("levels") = lvls;
      outId.attr("class") = "factor";
    }
    ret["id"] = outId;
  }
  IntegerVector inReset;
  IntegerVector outReset;
  if (bReset){
    inReset = Data["resetno"];
    outReset = IntegerVector(inReset.size()*nfactor);
    for (j = nfactor; j--;){
      std::copy(inReset.begin(),inReset.end(),outReset.begin()+j*inReset.size());
    }
    ret["resetno"] = outReset;
  }
  IntegerVector inEvid;
  IntegerVector outEvid;
  if (bEvid){
    inEvid = Data["evid"];
    outEvid = IntegerVector(inEvid.size()*nfactor);
    for (j = nfactor; j--;){
      std::copy(inEvid.begin(),inEvid.end(),outEvid.begin()+j*inEvid.size());
    }
    ret["evid"] = outEvid;
  }

  NumericVector inAmt;
  NumericVector outAmt;
  if (bAmt){
    inAmt = Data["amt"];
    outAmt = NumericVector(inAmt.size()*nfactor);
    for (j = nfactor; j--;){
      std::copy(inAmt.begin(),inAmt.end(),outAmt.begin()+j*inAmt.size());
    }
    if (Rf_inherits(inAmt, "units")){
      outAmt.attr("class") = "units";
      outAmt.attr("units") = inAmt.attr("units");
    }
    ret["amt"] = outAmt;
  }

  NumericVector inTime = Data["time"];
  NumericVector outTime(inTime.size()*nfactor);
  for (j = nfactor; j--;){
    std::copy(inTime.begin(),inTime.end(),outTime.begin()+j*inTime.size());
  }
  if (Rf_inherits(inTime, "units")){
    outTime.attr("class") = "units";
    outTime.attr("units") = inTime.attr("units");
  }
  ret["time"] = outTime;

  NumericVector outValue(inTime.size()*nfactor);
  IntegerVector outLvl(inTime.size()*nfactor);
  NumericVector tmpV;
  for (j = 0; j < nfactor; j++){
    tmpV = Data[as<std::string>(lvl[j])];
    std::copy(tmpV.begin(),tmpV.end(), outValue.begin()+j*inTime.size());
    std::fill_n(outLvl.begin()+j*inTime.size(), inTime.size(), j+1);
  }
  outLvl.attr("class") = "factor";
  outLvl.attr("levels") = lvl;
  ret["value"] = outValue;
  ret["trt"] = outLvl;
  ret.attr("class") = "data.frame";
  ret.attr("row.names") = IntegerVector::create(NA_INTEGER, -inTime.size()*nfactor);
  return ret;
}


Function getRxFn(std::string name);

//[[Rcpp::export]]
RObject rxModelVarsStack(RObject x) {
  Function fn = getRxFn("rxModelVars");
  return fn(x);
}
