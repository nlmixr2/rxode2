// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: nil; -*-
//#undef NDEBUG
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <Rcpp.h>
#include <R.h>
#include "timsort.h"
#include "../inst/include/rxode2parse.h"
extern "C" rx_solve rx_global;
extern "C" rx_solving_options op_global;
#include "../inst/include/rxode2parseHandleEvid.h"
#include "checkmate.h"
#define SORT gfx::timsort

using namespace Rcpp;

Function getRxFn(std::string name);

SEXP convertId_(SEXP id);

extern "C" int _rxIsEt(SEXP objSexp);
#include "../inst/include/rxode2_as.h"

static inline bool rxIsCleanList(RObject obj) {
  int type = obj.sexp_type();
  if (type == VECSXP) {
    bool hasCls = obj.hasAttribute("class");
    if (!hasCls) return true;
    if (Rf_inherits(obj, "data.frame")) return false;
    if (Rf_inherits(obj, "rxEt")) return false;
    if (Rf_inherits(obj,  "rxEtTran")) return false;
    return true;
  }
  return false;
}


#undef _
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

Environment rxode2env ();

extern "C" SEXP orderForderS1(SEXP ordIn) {
BEGIN_RCPP
  Function order1 = getRxFn(".order1");
 IntegerVector ord = order1(ordIn);
  return wrap(ord);
END_RCPP
}


RObject evCur;
RObject curSolve;

Function loadNamespace2("loadNamespace", R_BaseNamespace);
Environment unitsPkg;
bool _assignUnits = false;
bool _assignUnitsVal = false;
bool assignUnits() {
  if (!_assignUnits) {
    Function rn("requireNamespace", R_BaseNamespace);
    if (as<bool>(rn("units",_["quietly"]=true))) {
      unitsPkg = loadNamespace2("units");
      _assignUnits = true;
      _assignUnitsVal=true;
      return true;
    } else {
      _assignUnits = true;
      _assignUnitsVal=false;
      return false;
    }
  } else {
    return _assignUnitsVal;
  }
}
NumericVector setUnits(NumericVector obj, std::string unit){
  if (assignUnits()) {
    Function f = as<Function>(unitsPkg["set_units"]);
    if (unit == ""){
      obj.attr("class") = R_NilValue;
      obj.attr("units") = R_NilValue;
      return obj;
    } else {
      return asNv(f(_["x"] = obj, _["value"] = unit, _["mode"] = "standard"),
                  "set_units(obj)");
    }
  } else {
    obj.attr("class") = R_NilValue;
    obj.attr("units") = R_NilValue;
    return obj;
  }
}

//[[Rcpp::export]]
CharacterVector etDollarNames(RObject obj){
  if (_rxIsEt(obj)){
    CharacterVector cls = asCv(obj.attr("class"), "class");
    List e = asList((cls.attr(".rxode2.lst")), ".rxode2.lst");
    CharacterVector c1 = e.attr("names");
    CharacterVector c2 = obj.attr("names");
    int j = 0;
    CharacterVector ret(c1.size()+c2.size()+1);
    for (int i = c1.size();i--;){
      ret[j++] = c1[i];
    }
    for (int i = c2.size();i--;){
      ret[j++] = c2[i];
    }
    ret[j++] = "env";
    return ret;
  } else {
    return CharacterVector::create();
  }
}

//[[Rcpp::export]]
RObject etUpdate(RObject obj,
                 RObject arg = R_NilValue,
                 RObject value = R_NilValue,
                 LogicalVector exact = true){
  if (_rxIsEt(obj)){
    evCur = obj;
    if (Rf_isNull(value)){
      CharacterVector cls = clone(asCv(obj.attr("class"), "class"));
      List e = clone(asList(cls.attr(".rxode2.lst"), ".rxode2.lst"));
      if (rxIsChar(arg)){
        CharacterVector carg = asCv(arg, "arg");
        std::string sarg = as<std::string>(carg[0]);
        if (sarg == "env"){
          e.attr("class") = R_NilValue;
          return as<RObject>(e);
        } else if (e.containsElementNamed(sarg.c_str())){
          return as<RObject>(e[sarg]);
        }
        List lst = asList(obj, "obj");
        if (lst.containsElementNamed(sarg.c_str())){
          return(as<RObject>(lst[sarg]));
        }
      }
    } else {
      // Assign
    }
  } else {
    if (Rf_isNull(value)){
      List lst = List(obj);
      if (rxIsNumInt(arg)){
        int iarg = asInt(arg, "arg");
        if (iarg <= lst.size()){
          return lst[iarg-1];
        }
      } else if (rxIsChar(arg)){
        std::string sarg = as<std::string>(arg);
        CharacterVector nm = lst.names();
        int n = nm.size(), i;
        unsigned int slen = strlen(sarg.c_str());
        int dexact = -1;
        if (exact[0] == TRUE){
          dexact = 1;
        } else if (exact[0] == FALSE){
          dexact = 0;
        }
        unsigned int slen2;
        for (i = 0; i < n; i++){
          slen2 = strlen((as<std::string>(nm[i])).c_str());
          if (slen <= slen2 &&
              (strncmp((as<std::string>(nm[i])).c_str(), sarg.c_str(), slen)  == 0 ) &&
              (dexact != 1 || (dexact == 1 && slen == slen2))){
            if (dexact == -1){
              Rf_warningcall(R_NilValue, "partial match of '%s' to '%s'", sarg.c_str(), (as<std::string>(nm[i])).c_str());
            }
            return lst[i];
          }
        }
      }
    }
  }
  return R_NilValue;
}

List etEmpty(CharacterVector units){
  CharacterVector cls = CharacterVector::create("rxEt","data.frame");
  List e(34);
  CharacterVector en(34);
  List envir = R_BaseNamespace;
  e[0] = clone(units);
  en[0] = "units";
  //e["units"] = clone(units);
  Function parse2("parse", R_BaseNamespace);
  Function eval2("eval", R_BaseNamespace);
  // eventTable style methods
  std::string getUnits= "function() rxode2:::et_(list(getUnits=TRUE) ,list('last'))";
  en[1] = "get.units";
  e[1] = eval2(_["expr"]   = parse2(_["text"]=getUnits),
               _["envir"]  = envir);

  en[2] = "getUnits";
  e[2] = eval2(_["expr"]   = parse2(_["text"]=getUnits),
               _["envir"]  = envir);
  en[3] = "get_units";
  e[3] = eval2(_["expr"]   = parse2(_["text"]=getUnits),
               _["envir"]  = envir);

  std::string addDosing= "function (dose, nbr.doses = 1L, dosing.interval = 24, \n    dosing.to = 1L, rate = NULL, amount.units = NA_character_, \n    start.time = 0, do.sampling = FALSE, time.units = NA_character_, \n    ...) \n{\n    .lst <- list(dose = dose, nbr.doses = nbr.doses, start.time = start.time, \n        do.sampling = do.sampling, ...)\n    if (!is.na(amount.units)) \n        .lst$amount.units <- amount.units\n    if (!is.na(time.units)) \n        .lst$time.units <- time.units\n    if (dosing.to != 1) \n        .lst$dosing.to <- dosing.to\n    if (!is.null(rate)) \n        .lst$rate <- rate\n    .lst$dosing.interval <- dosing.interval\n    invisible(rxode2:::et_(.lst, list('last')))\n}";

  en[4] = "add.dosing";
  e[4] = eval2(_["expr"] = parse2(_["text"] = addDosing),
               _["envir"]  = envir);

  en[5] = "add_dosing";
  e[5] = eval2(_["expr"] = parse2(_["text"] = addDosing),
               _["envir"]  = envir);
  en[6] = "addDosing";
  e[6] = eval2(_["expr"] = parse2(_["text"] = addDosing),
               _["envir"]  = envir);

  std::string addSampling="function(time, time.units = NA) {\n  .lst <- list();\n  .lst$time<-time;\n  if(!is.na(time.units)) .lst$time.units<- time.units\n  invisible(rxode2:::et_(.lst, list('last')));\n}";

  en[7] = "add.sampling";
  e[7] = eval2(_["expr"] = parse2(_["text"] = addSampling),
               _["envir"]  = envir);
  en[8] = "add_sampling";
  e[8] = eval2(_["expr"] = parse2(_["text"] = addSampling),
               _["envir"]  = envir);

  en[9] = "addSampling";
  e[9] = eval2(_["expr"] = parse2(_["text"] = addSampling),
               _["envir"]  = envir);

  en[10]= "clear.sampling";
  e[10] = eval2(_["expr"] = parse2(_["text"] = "function() invisible(rxode2:::et_(list(clearSampling=TRUE),list('last')))"),
                _["envir"]  = envir);

  en[11] = "clear_sampling";
  e[11] = eval2(_["expr"] = parse2(_["text"] = "function() invisible(rxode2:::et_(list(clearSampling=TRUE),list('last')))"),
                _["envir"]  = envir);

  en[12] = "clearSampling";
  e[12] = eval2(_["expr"] = parse2(_["text"] = "function() invisible(rxode2:::et_(list(clearSampling=TRUE),list('last')))"),
                             _["envir"]  = envir);

  en[13] = "clear.dosing";
  e[13] = eval2(_["expr"] = parse2(_["text"] = "function() invisible(rxode2:::et_(list(clearDosing=TRUE),list('last')))"),
                            _["envir"]  = envir);

  en[14] = "clear_dosing";
  e[14] = eval2(_["expr"] = parse2(_["text"] = "function() invisible(rxode2:::et_(list(clearDosing=TRUE),list('last')))"),
                            _["envir"]  = envir);

  en[15] = "clearDosing";
  e[15] = eval2(_["expr"] = parse2(_["text"] = "function() invisible(rxode2:::et_(list(clearDosing=TRUE),list('last')))"),
                           _["envir"]  = envir);

  en[16] = "simulate";
  e[16] = eval2(_["expr"] = parse2(_["text"] = "function(object, nsim = 1, seed = NULL, ...){if (!missing(nsim)) Rf_warningcall(R_NilValue, \"'nsim' is ignored when simulating event tables\");if(!is.null(seed)) set.seed(seed); invisible(rxode2:::et_(list(simulate=TRUE),list('last')))}"));

  std::string importET = "function(data) invisible(rxode2:::et_(list(data=data),list('import')))";

  en[17] = "import.EventTable";
  e[17] = eval2(_["expr"] = parse2(_["text"] = importET),
                _["envir"]  = envir);

  en[18] = "importEventTable";
  e[18] = eval2(_["expr"] = parse2(_["text"] = importET),
                _["envir"]  = envir);

  en[19] = "copy";
  e[19] = eval2(_["expr"] = parse2(_["text"] = "function() rxode2:::et_(list(copy=TRUE),list('last'))"),
                    _["envir"]  = envir);

  en[20] = "get.EventTable";
  e[20] = eval2(_["expr"] = parse2(_["text"] = "function() rxode2:::et_(list(get.EventTable=TRUE),list('last'))"),
                              _["envir"]  = envir);
  en[21] = "getEventTable";
  e[21] = eval2(_["expr"] = parse2(_["text"] = "function() rxode2:::et_(list(get.EventTable=TRUE),list('last'))"),
                             _["envir"]  = envir);

  en[22] = "get.obs.rec";
  e[22] = eval2(_["expr"] = parse2(_["text"] = "function() rxode2:::et_(list(get.obs.rec=TRUE),list('last'))"),
                           _["envir"]  = envir);
  en[23] = "get.nobs";
  e[23] = eval2(_["expr"] = parse2(_["text"] = "function() rxode2:::et_(list(get.nobs=TRUE),list('last'))"),
                        _["envir"]  = envir);


  en[24] = "get.dosing";
  e[24] = eval2(_["expr"] = parse2(_["text"] = "function() rxode2:::et_(list(get.dosing=TRUE),list('last'))"),
                          _["envir"]  = envir);
  en[25] = "getDosing";
  e[25] = eval2(_["expr"] = parse2(_["text"] = "function() rxode2:::et_(list(get.dosing=TRUE),list('last'))"),
                         _["envir"]  = envir);

  en[26] = "get.sampling";
  e[26] = eval2(_["expr"] = parse2(_["text"] = "function() rxode2:::et_(list(get.sampling=TRUE),list('last'))"),
                            _["envir"]  = envir);

  en[27] = "getSampling";
  e[27] = eval2(_["expr"] = parse2(_["text"] = "function() rxode2:::et_(list(get.sampling=TRUE),list('last'))"),
                           _["envir"]  = envir);

  en[28] = "expand";
  e[28] = eval2(_["expr"] = parse2(_["text"] = "function() invisible(rxode2:::et_(list(expand=TRUE) ,list('last')))"),
                      _["envir"]  = envir);

  en[29] = "nobs";
  e[29] = 0;

  en[30] = "ndose";
  e[30] = 0;

  en[31] ="show";
  e[31]  = LogicalVector::create(_["id"] = false, _["low"] = false,
                                    _["time"] = true, _["high"] = false,
                                    _["cmt"] =false, _["amt"]=false,
                                    _["rate"] = false, _["ii"] = false,
                                    _["addl"] = false, _["evid"] = true,
                                    _["ss"] = false, _["dur"] = false);
  en[32] = "IDs";
  e[32] = IntegerVector::create(1);

  en[33] = "canResize";
  e[33] = true;

  // Return an empty data frame.
  List lst(12);
  CharacterVector nme(12);
  nme[0] = "id";
  lst[0] = IntegerVector(0);

  nme[2] = "time";
  lst[2] = NumericVector(0);

  nme[1] = "low";
  lst[1] = NumericVector(0);

  nme[3] = "high";
  lst[3] = NumericVector(0);

  nme[4] = "cmt";
  lst[4] = CharacterVector(0);

  nme[5] = "amt";
  lst[5] = NumericVector(0);

  nme[6] = "rate";
  lst[6] = NumericVector(0);

  nme[7] = "ii";
  lst[7] = NumericVector(0);

  nme[8] = "addl";
  lst[8] = IntegerVector(0);

  nme[9] = "evid";
  lst[9] = IntegerVector(0);

  nme[10] = "ss";
  lst[10] = IntegerVector(0);

  nme[11] = "dur";
  lst[11] = NumericVector(0);

  e.attr("class") = "rxHidden";
  e.attr("names") = en;

  cls.attr(".rxode2.lst") = e;

  lst.attr("names") = nme;
  lst.attr("class") = cls;
  lst.attr("row.names") = IntegerVector::create(NA_INTEGER, 0);
  if (!CharacterVector::is_na(units[1])){
    lst["low"]  = setUnits(lst["low"],  as<std::string>(units[1]));
    lst["time"] = setUnits(lst["time"], as<std::string>(units[1]));
    lst["high"] = setUnits(lst["high"], as<std::string>(units[1]));
    lst["ii"]  = setUnits(lst["ii"],  as<std::string>(units[1]));
    lst["dur"]  = setUnits(lst["dur"],  as<std::string>(units[1]));
  } else {
    lst["low"]  = setUnits(lst["low"], "");
    lst["ii"]  = setUnits(lst["ii"], "");
    lst["dur"]  = setUnits(lst["dur"], "");
    lst["time"] = setUnits(lst["time"],"");
    lst["high"] = setUnits(lst["high"], "");
  }
  if (!CharacterVector::is_na(units[0])){
    lst["amt"] = setUnits(lst["amt"], as<std::string>(units[0]));
  } else {
    lst["amt"] = setUnits(lst["amt"], "");
  }
  if (!CharacterVector::is_na(units[1]) && !CharacterVector::is_na(units[0])){
    std::string rateUnit = as<std::string>(units[0]) + "/" + as<std::string>(units[1]);
    lst["rate"] = setUnits(lst["rate"], rateUnit);
  } else {
    lst["rate"] = setUnits(lst["rate"], "");
  }
  return lst;
}

List etSort(List& curEt){
  std::vector<double> time;
  NumericVector curTime = asNv(curEt["time"], "curEt[\"time\"]");
  int size = curTime.size();
  if (size == 0) return curEt;
  time.reserve(size);
  std::copy(curTime.begin(), curTime.end(),std::back_inserter(time));
  std::vector<int> id;
  id.reserve(size);
  IntegerVector curId = asIv(curEt["id"], "curEt[\"id\"]");
  std::copy(curId.begin(), curId.end(), std::back_inserter(id));
  std::vector<int> evid;
  evid.reserve(size);
  IntegerVector curEvid = asIv(curEt["evid"], "curEt[\"evid\"]");
  std::copy(curEvid.begin(), curEvid.end(), std::back_inserter(evid));
  std::vector<int> idx(id.size());
  Environment b=Rcpp::Environment::base_namespace();
  IntegerVector ivId=wrap(id);
  NumericVector nvTime=wrap(time);
  IntegerVector ivEvid=wrap(evid);
  Function order3 = getRxFn(".order3");
  IntegerVector ord;
  ord = order3(ivId, nvTime, ivEvid);
  ord = ord - 1;
  idx = as<std::vector<int>>(ord);
  List newEt(curEt.size());
  int i, j, newSize = time.size();
  for (int j = time.size(); j--;) {
    if (ISNA(time[j])) {
      newSize--;
    }
  }
  IntegerVector tmpI, tmpI2;
  CharacterVector tmpC, tmpC2;
  NumericVector tmpN, tmpN2;
  int ii = 0;
  for (j = newEt.size(); j--;){
    ii = 0;
    for (i = 0; i < (int)time.size(); i++){
      if (ISNA(time[i])) continue;
      if (rxIsNum(curEt[j])) {
        if (i == 0) newEt[j] = NumericVector(newSize);
        tmpN=newEt[j];
        tmpN2 = curEt[j];
        tmpN[ii] = tmpN2[idx[i]];
      } else if (rxIsInt(curEt[j])) {
        if (i == 0) newEt[j] = IntegerVector(newSize);
        tmpI=newEt[j];
        tmpI2 = curEt[j];
        tmpI[ii] = tmpI2[idx[i]];
      } else if (rxIsChar(curEt[j])){
        if (i == 0) newEt[j] = CharacterVector(newSize);
        tmpC=newEt[j];
        tmpC2 = curEt[j];
        tmpC[ii] = tmpC2[idx[i]];
      }
      ii++;
    }
  }
  newEt.attr("class") = clone(asCv(curEt.attr("class"), "class"));
  newEt.attr("names") = curEt.attr("names");
  newEt.attr("row.names") = IntegerVector::create(NA_INTEGER, -newSize);
  if (newSize != (int)time.size()) {
    Rf_warningcall(R_NilValue, "while importing the dataset, some times are NA and ignored");
  }
  return newEt;
}

List etSimulate(List curEt){
  CharacterVector cls = clone(asCv(curEt.attr("class"), "class"));
  List lst = clone(curEt);
  NumericVector time = lst["time"];
  NumericVector low = lst["low"];
  NumericVector high = lst["high"];
  bool recalcTime=false;
  for (int i = time.size(); i--;){
    if (!ISNA(low[i]) && !ISNA(high[i])){
      time[i] = Rf_runif(low[i], high[i]);
      recalcTime=true;
    }
  }
  if (!recalcTime){
    Rf_warningcall(R_NilValue, "%s", _("event table was not updated (no dose/sampling windows)"));
    return curEt;
  } else {
    lst.attr("class") = cls;
    return etSort(lst);
  }
}

static inline void etUpdateCanResize(List &lst, LogicalVector& show, List& eOld, IntegerVector& IDs,
                                     List& e){
  bool showId = show["id"];
  std::vector<int> uIds = as<std::vector<int>>(eOld["IDs"]);
  bool turnOnShowId = false;
  if (!showId && uIds.size() == 1 && IDs.size() >= 1 && uIds[0] != IDs[0]) {
    uIds[0] = IDs[0];
    IntegerVector tmpI = asIv(lst[0], "lst[0]");
    std::fill(tmpI.begin(), tmpI.end(), IDs[0]);
    turnOnShowId = true;
  }
  for (int i = IDs.size(); i--;){
    if (std::find(uIds.begin(), uIds.end(), IDs[i]) == uIds.end()){
      uIds.push_back(IDs[i]);
    }
  }
  if ((int)IDs.size() == (int)uIds.size() &&
      asBool(eOld["canResize"], "eOld[\"canResize\"]")){
    e["canResize"] = true;
  } else {
    e["canResize"] = false;
  }
  e["IDs"] = wrap(uIds);
  if (turnOnShowId || uIds.size() > 1){
    show["id"] = true;
  }
}

List etAddWindow(List windowLst, IntegerVector IDs, RObject cmt, bool turnOnShowCmt, List curEt){
  NumericVector curTime = asNv(curEt["time"], "curEt[\"time\"]");
  int oldSize = curTime.size();
  int size = oldSize + windowLst.size()*IDs.size();
  std::vector<double> time;
  time.reserve(size);
  std::copy(curTime.begin(),curTime.end(), std::back_inserter(time));
  std::vector<double> low;
  NumericVector curLow = asNv(curEt["low"], "curEt[\"low\"]");
  low.reserve(size);
  std::copy(curLow.begin(),curLow.end(), std::back_inserter(low));
  std::vector<double> high;
  high.reserve(size);
  NumericVector curHigh = asNv(curEt["high"], "curEt[\"high\"]");
  std::copy(curHigh.begin(), curHigh.end(),std::back_inserter(high));
  std::vector<int> id;
  id.reserve(size);
  IntegerVector curId = asIv(curEt["id"], "curEt[\"id\"]");
  std::copy(curId.begin(), curId.end(),std::back_inserter(id));
  std::vector<int> idx(size);
  IntegerVector curEvid = asIv(curEt["evid"], "curEt[\"evid\"]");
  std::vector<int> evid;
  evid.reserve(size);
  std::copy(curEvid.begin(), curEvid.end(), std::back_inserter(evid));
  double c = 0;
  CharacterVector cls = clone(asCv(curEt.attr("class"), "class"));
  List eOld = cls.attr(".rxode2.lst");
  List e = clone(eOld);
  CharacterVector units = e["units"];
  int nobs=0;
  for (int j = IDs.size(); j--;){
    for (int i = windowLst.size(); i--;){
      NumericVector cur = asNv(windowLst[i], "windowLst[i]");
      if (Rf_inherits(cur, "units")){
        if (!CharacterVector::is_na(units["time"])){
          cur = setUnits(cur, as<std::string>(units["time"]));
        } else {
          cur = setUnits(cur, "");
        }
      }
      if (cur.size() != 2)
        stop(_("windows need to be a list of observation windows, each of 2 elements e.g. list(c(0,2), c(2,7))"));
      if (cur[0]> cur[1])
        stop(_("windows need to be ordered list(c(2,0)) is invalid"));
      id.push_back(IDs[j]);
      low.push_back(cur[0]);
      high.push_back(cur[1]);
      c = Rf_runif(cur[0], cur[1]);
      time.push_back(c);
      evid.push_back(0);
      nobs++;
    }
  }
  IntegerVector ivId=wrap(id);
  NumericVector nvTime=wrap(time);
  IntegerVector ivEvid=wrap(evid);
  Function order3 = getRxFn(".order3");
  IntegerVector ord;
  ord = order3(ivId, nvTime, ivEvid);
  ord = ord - 1;
  idx = as<std::vector<int>>(ord);
  List lst(curEt.size());
  IntegerVector tmpI = asIv(curEt["id"], "curEt[\"id\"]"), tmpI2;
  NumericVector tmpN, tmpN2;
  CharacterVector tmpC, tmpC2;
  lst.attr("names") = curEt.attr("names");
  int i, j;
  // nme[0] = "id";
  lst[0] = IntegerVector(id.size());

  // nme[1] = "time";
  lst[2] = NumericVector(id.size());

  // nme[2] = "low";
  lst[1] = NumericVector(id.size());

  // nme[3] = "high";
  lst[3] = NumericVector(id.size());

  // nme[4] = "cmt";
  bool isCmtInt=false;
  if (!rxIsInt(cmt)){
    lst[4] = CharacterVector(id.size());
  } else {
    lst[4] = IntegerVector(id.size());
    isCmtInt=true;
  }
  // nme[5] = "amt";
  lst[5] = NumericVector(id.size());

  // nme[6] = "rate";
  lst[6] = NumericVector(id.size());

  // nme[7] = "ii";
  lst[7] = NumericVector(id.size());

  // nme[8] = "addl";
  lst[8] = IntegerVector(id.size());

  // nme[9] = "evid";
  lst[9] = IntegerVector(id.size());

  // nme[10] = "ss";
  lst[10] = IntegerVector(id.size());

  // nme[11]= "dur"
  lst[11] = NumericVector(id.size());
  for (i = idx.size(); i--;){
    tmpI = asIv(lst[0], "lst[0]"); // id
    tmpI[i] = id[idx[i]];

    tmpI = asIv(lst[9], "lst[9]"); // evid
    tmpI[i] = evid[idx[i]];

    tmpN = asNv(lst[2], "lst[2]"); // time
    tmpN[i] = time[idx[i]];

    // low
    tmpN = asNv(lst[1], "lst[1]");
    tmpN[i] = low[idx[i]];

    // hi
    tmpN = asNv(lst[3], "lst[3]");
    tmpN[i] = high[idx[i]];
    if (idx[i] >= oldSize){
      if (isCmtInt){
        tmpI = asIv(lst[4], "lst[4]");
        tmpI[i] = asInt(cmt, "cmt");
      } else {
        tmpC = asCv(lst[4], "lst[4]");
        tmpC2 = asCv(cmt, "cmt");
        tmpC[i] = tmpC2[0];
      }

      // nme[5] = "amt";
      tmpN = asNv(lst[5], "lst[5]");
      tmpN[i] = NA_REAL;

      // nme[6] = "rate";
      tmpN = asNv(lst[6], "lst[6]");
      tmpN[i] = NA_REAL;

      // nme[7] = "ii";
      tmpN = asNv(lst[7], "lst[7]");
      tmpN[i] = NA_REAL;

      // nme[8] = "addl";
      tmpI = asIv(lst[8], "lst[8]");
      tmpI[i] = NA_INTEGER;

      // nme[10] = "ss";
      tmpI = asIv(lst[10], "lst[10]");
      tmpI[i] = NA_INTEGER;

      // nme[11] = "dur";
      tmpN = asNv(lst[11], "lst[11]");
      tmpN[i] = NA_REAL;
    } else {
      for (j = 12; j--;){
        if (rxIsNum(curEt[j])){
          tmpN = asNv(lst[j], "lst[j]");
          tmpN2 = asNv(curEt[j], "curEt[j]");
          tmpN[i] = tmpN2[idx[i]];
        } else if (rxIsInt(curEt[j])) {
          tmpI = asIv(lst[j], "lst[j]");
          tmpI2 = asIv(curEt[j], "curEt[j]");
          tmpI[i] = tmpI2[idx[i]];
        } else if (rxIsChar(curEt[j])){
          // Char
          tmpC = asCv(lst[j], "lst[j]");
          tmpC2 = asCv(curEt[j], "curEt[j]");
          tmpC[i] = tmpC2[idx[i]];
        }
      }
    }
  }
  e["nobs"] = asInt(e["nobs"], "e[\"nobs\"]") + nobs;
  LogicalVector show = e["show"];
  if (turnOnShowCmt){
    show["cmt"] = true;
  }
  etUpdateCanResize(lst, show, eOld, IDs, e);
  show["low"] = true;
  show["high"] = true;
  e["show"] = show;

  e.attr("class") = "rxHidden";
  cls.attr(".rxode2.lst") = e;
  lst.attr("class") = cls;
  int len = asInt(e["nobs"], "e[\"nobs\"]") +
    asInt(e["ndose"], "e[\"ndose\"]");
  lst.attr("row.names") = IntegerVector::create(NA_INTEGER, -len);
  return lst;
}

List etAddTimes(NumericVector newTimes, IntegerVector IDs, RObject cmt, bool turnOnShowCmt, List curEt){
  CharacterVector cls = clone(asCv(curEt.attr("class"), "class"));
  List eOld = cls.attr(".rxode2.lst");
  List e = clone(eOld);

  std::vector<double> time;
  IntegerVector curId = asIv(curEt["id"], "curEt[\"id\"]");
  int oldSize =curId.size();
  int extraSize = newTimes.size()*IDs.size();
  time.reserve(extraSize);
  NumericVector curTime = asNv(curEt["time"], "curEt[\"time\"]");
  std::vector<int> id;
  id.reserve(extraSize);
  std::vector<int> idx(oldSize+extraSize);
  std::vector<int> evid;
  evid.reserve(extraSize);
  IntegerVector curEvid = asIv(curEt["evid"], "curEt[\"evid\"]");
  std::iota(idx.begin(),idx.end(),0);
  int nobs = 0;
  for (int j = IDs.size(); j--;){
    for (int i = newTimes.size(); i--;){
      id.push_back(IDs[j]);
      time.push_back(newTimes[i]);
      evid.push_back(0);
      nobs++;
    }
  }
  SORT(idx.begin(),idx.end(),
       [id, time, evid, curId, curTime, curEvid, oldSize](int a, int b){
         int ida, evida, idb, evidb;
         double timea, timeb;
         if (a < oldSize){
           ida = curId[a];
           timea = curTime[a];
           evida = curEvid[a];
         } else {
           ida = id[a-oldSize];
           timea = time[a-oldSize];
           evida = evid[a-oldSize];
         }
         if (b < oldSize){
           idb = curId[b];
           timeb = curTime[b];
           evidb = curEvid[b];
         } else {
           idb = id[b-oldSize];
           timeb = time[b-oldSize];
           evidb = evid[b-oldSize];
         }
         if (ida == idb){
           if (timea == timeb){
             if (evida == evidb){
               return a < b;
             }
             return evida < evidb;
           }
           return timea < timeb;
         }
         return ida < idb;
       });

  List lst(curEt.size());
  IntegerVector tmpI = asIv(curEt["id"], "curEt[\"id\"]"), tmpI2;
  NumericVector tmpN, tmpN2;
  CharacterVector tmpC, tmpC2;
  lst.attr("names") = curEt.attr("names");
  int i, j;
  // nme[0] = "id";
  lst[0] = IntegerVector(idx.size());

  // nme[1] = "time";
  lst[2] = NumericVector(idx.size());

  // nme[2] = "low";
  lst[1] = NumericVector(idx.size());

  // nme[3] = "high";
  lst[3] = NumericVector(idx.size());

  // nme[4] = "cmt";
  bool isCmtInt = false;
  if (rxIsInt(cmt)){
    lst[4] = IntegerVector(idx.size());
    isCmtInt=true;
  } else {
    lst[4] = CharacterVector(idx.size());
  }

  // nme[5] = "amt";
  lst[5] = NumericVector(idx.size());

  // nme[6] = "rate";
  lst[6] = NumericVector(idx.size());

  // nme[7] = "ii";
  lst[7] = NumericVector(idx.size());

  // nme[8] = "addl";
  lst[8] = IntegerVector(idx.size());

  // nme[9] = "evid";
  lst[9] = IntegerVector(idx.size());

  // nme[10] = "ss";
  lst[10] = IntegerVector(idx.size());

  // nme[11] = "dur";
  lst[11] = NumericVector(idx.size());
  for (i = idx.size(); i--;){
    if (idx[i] >= oldSize){
      tmpI = asIv(lst[0], "lst[0]"); // id
      tmpI[i] = id[idx[i]-oldSize];

      tmpI = asIv(lst[9], "lst[9]"); // evid
      tmpI[i] = evid[idx[i]-oldSize];

      tmpN = asNv(lst[2], "lst[2]"); // time
      tmpN[i] = time[idx[i]-oldSize];

      // low
      tmpN = asNv(lst[1], "lst[1]");
      tmpN[i] = NA_REAL;

      // hi
      tmpN = asNv(lst[3], "lst[3]");
      tmpN[i] = NA_REAL;

      if (isCmtInt){
        tmpI = asIv(lst[4], "lst[4]");
        tmpI[i] = asInt(cmt, "cmt");
      } else {
        tmpC = asCv(lst[4], "lst[4]");
        tmpC2 = asCv(cmt, "cmt");
        tmpC[i] = tmpC2[0];
      }

      // nme[5] = "amt";
      tmpN = asNv(lst[5], "lst[5]");
      tmpN[i] = NA_REAL;

      // nme[6] = "rate";
      tmpN = asNv(lst[6], "lst[6]");
      tmpN[i] = NA_REAL;

      // nme[7] = "ii";
      tmpN = asNv(lst[7], "lst[7]");
      tmpN[i] = NA_REAL;

      // nme[8] = "addl";
      tmpI = asIv(lst[8], "lst[8]"); // id
      tmpI[i] = NA_INTEGER;

      // nme[10] = "ss";
      tmpI = asIv(lst[10], "lst[10]"); // id
      tmpI[i] = NA_INTEGER;

      // nme[11] = "dur";
      tmpN = asNv(lst[11], "lst[11]"); // id
      tmpN[i] = NA_REAL;
    } else {
      tmpI = asIv(lst[0], "lst[0]"); // id
      tmpI[i] = curId[idx[i]];

      tmpI = asIv(lst[9], "lst[9]"); // evid
      tmpI[i] = curEvid[idx[i]];

      tmpN = asNv(lst[2], "lst[2]"); // time
      tmpN[i] = curTime[idx[i]];
      // low
      for (j = 12; j--;){
        if (rxIsNum(curEt[j])){
          tmpN = asNv(lst[j], "lst[j]");
          tmpN2 = asNv(curEt[j], "curEt[j]");
          tmpN[i] = tmpN2[idx[i]];
        } else if (rxIsInt(curEt[j])) {
          tmpI = asIv(lst[j], "lst[j]");
          tmpI2 = asIv(curEt[j], "lst[j]");
          tmpI[i] = tmpI2[idx[i]];
        } else if (rxIsChar(curEt[j])){
          // Char
          tmpC = asCv(lst[j], "lst[j]");
          tmpC2 = asCv(curEt[j], "lst[j]");
          tmpC[i] = tmpC2[idx[i]];
        }
      }
    }
  }
  e["nobs"] = asInt(e["nobs"], "e[\"nobs\"]") + nobs;
  LogicalVector show = e["show"];
  if (turnOnShowCmt){
    show["cmt"] = true;
  }
  etUpdateCanResize(lst, show, eOld, IDs, e);
  e["show"] = show;
  e.attr("names") = eOld.attr("names");
  e.attr("class") = "rxHidden";//eOld.attr("class");
  cls.attr(".rxode2.lst") = e;
  lst.attr("class") = cls;
  int len = asInt(e["nobs"], "e[\"nobs\"]") +
    asInt(e["ndose"], "e[\"ndose\"]");
  lst.attr("row.names") = IntegerVector::create(NA_INTEGER, -len);
  return lst;
}

CharacterVector deparseUnit(NumericVector nv){
  if (Rf_inherits(nv, "units")) {
    if (assignUnits()) {
      Function f = as<Function>(unitsPkg["deparse_unit"]);
      NumericVector nv0 = NumericVector::create(0); // Create to fix NA issues.
      nv0.attr("units") = nv.attr("units");
      nv0.attr("class") = "units";
      CharacterVector ret = f(nv0);
      if (as<std::string>(ret) == "NA"){
        return CharacterVector::create(NA_STRING);
      } else {
        return ret;
      }
    } else {
      return CharacterVector::create(NA_STRING);
    }
  } else {
    return CharacterVector::create(NA_STRING);
  }
}


IntegerVector convertMethod(RObject method);

List etImportEventTable(List inData, bool warnings = true){
  CharacterVector lName0 = asCv(inData.attr("names"), "names");
  CharacterVector lName = clone(lName0);
  //var=cmt time value=amt method->evid from deSolve
  int i, idCol = -1, evidCol=-1, timeCol=-1, amtCol=-1, cmtCol=-1,
    ssCol=-1, rateCol=-1, addlCol=-1, iiCol=-1, durCol = -1, j,
    mdvCol =-1, methodCol=-1;
  std::string tmpS;
  for (i = lName.size(); i--;){
    tmpS = as<std::string>(lName[i]);
    std::transform(tmpS.begin(), tmpS.end(), tmpS.begin(), ::tolower);
    lName[i] = tmpS;
    if (tmpS == "id") idCol=i;
    else if (tmpS == "evid") evidCol=i;
    else if (tmpS == "time") timeCol=i;
    else if (tmpS == "amt" || tmpS == "value") {
      if (amtCol != -1) stop(_("can only specify either 'amt' or 'value'"));
      amtCol=i;
    }
    else if (tmpS == "cmt" || tmpS == "ytype" || tmpS == "state" || tmpS == "var"){
      if (cmtCol != -1) stop(_("can only specify either 'cmt', 'ytype', 'state' or 'var'"));
      cmtCol=i;
    }
    else if (tmpS == "ss")   ssCol=i;
    else if (tmpS == "rate") rateCol=i;
    else if (tmpS == "addl") addlCol=i;
    else if (tmpS == "ii" || tmpS == "by")   iiCol=i;
    else if (tmpS == "dur" || tmpS == "duration") {
      if (durCol !=-1) stop(_("can only specify either 'duration' or 'dur'"));
      durCol=i;
    }
    else if (tmpS == "mdv") mdvCol=i;
    else if (tmpS == "method") methodCol=i;
  }
  NumericVector oldTime;
  if (timeCol == -1){
    stop(_("need a 'time' column"));
  } else {
    oldTime=asNv(inData[timeCol], "inData[timeCol]");
  }
  IntegerVector oldEvid;
  if (evidCol == -1) {
    if (mdvCol != -1){
      evidCol = mdvCol;
      oldEvid=asIv(inData[evidCol], "inData[evidCol]");
      if (methodCol != -1 && warnings){
        Rf_warningcall(R_NilValue, "%s", _("using 'mdv' instead of 'method'"));
      }
    } else if (methodCol != -1){
      oldEvid = convertMethod(inData[methodCol]);
    } else {
      oldEvid = IntegerVector(oldTime.size());
      std::fill(oldEvid.begin(), oldEvid.end(), 0);
    }
  } else {
    if (mdvCol != -1 && warnings){
      Rf_warningcall(R_NilValue, "%s", _("using 'evid' instead of 'mdv'"));
    }
    oldEvid=asIv(inData[evidCol], "inData[evidCol]");
  }
  std::vector<int> evid;

  IntegerVector oldId;
  if (idCol == -1){
    oldId=IntegerVector(oldEvid.size(), 1);
  } else {
    if (rxIsNumInt(inData[idCol])){
      oldId=asIv(inData[idCol], "inData[idCol]");
    } else if (rxIsChar(inData[idCol])){
      oldId = convertId_(inData[idCol]);
    } else {
      stop(_("'ID' type is unknown"));
    }
  }
  std::vector<int> id;

  std::vector<double> low;

  std::vector<double> time;
  std::vector<double> high;

  std::vector<double> rate;
  RObject rateUnits;
  bool haveRateUnits=false;
  NumericVector oldRate;
  if (rateCol == -1){
    oldRate = NumericVector(oldEvid.size(), 0.0);
  } else {
    oldRate = asNv(inData[rateCol], "inData[rateCol]");
    if (Rf_inherits(oldRate, "units")){
      rateUnits = oldRate.attr("units");
      haveRateUnits=true;
    }
  }

  std::vector<double> dur;
  NumericVector oldDur;
  if (durCol == -1){
    oldDur = NumericVector(oldEvid.size(), 0.0);
  } else {
    oldDur = asNv(inData[durCol], "inData[durCol]");
    if (Rf_inherits(oldTime, "units")){
      oldDur = setUnits(oldDur, as<std::string>(deparseUnit(oldTime)));
    }
  }

  std::vector<double> ii;
  NumericVector oldIi;
  if (iiCol == -1){
    oldIi = NumericVector(oldEvid.size(), 0.0);
  } else {
    oldIi = asNv(inData[iiCol], "inData[iiCol]");
    if (Rf_inherits(oldTime, "units")){
      oldIi = setUnits(oldIi, as<std::string>(deparseUnit(oldTime)));
    }
  }

  std::vector<int> addl;
  IntegerVector oldAddl;
  if (addlCol == -1){
    oldAddl = IntegerVector(oldEvid.size(), 0);
  } else {
    oldAddl = asIv(inData[addlCol], "inData[addlCol]");
  }

  std::vector<int> ss;
  IntegerVector oldSs;
  if (ssCol == -1){
    oldSs = IntegerVector(oldEvid.size(), 0);
  } else {
    oldSs = asIv(inData[ssCol], "inData[ssCol]");
  }

  std::vector<double> amt;
  NumericVector oldAmt;
  if (amtCol == -1){
    oldAmt = NumericVector(oldEvid.size());
    std::fill(oldAmt.begin(), oldAmt.end(), 0);
  } else {
    oldAmt = asNv(inData[amtCol], "inData[amtCol]");
  }

  std::vector<int> cmt;
  IntegerVector oldCmt;
  CharacterVector oldCmtC;
  bool cmtC = false;
  if (cmtCol == -1){
    oldCmt = IntegerVector(oldEvid.size(), 0);
  } else {
    if (Rf_inherits(inData[cmtCol], "factor")){
      oldCmtC = as<CharacterVector>(inData[cmtCol]);
      cmtC=true;
    } else if (rxIsNumInt(inData[cmtCol])){
      oldCmt = as<IntegerVector>(inData[cmtCol]);
    } else if (rxIsChar(inData[cmtCol])){
      oldCmtC = as<CharacterVector>(inData[cmtCol]);
      cmtC=true;
    } else {
      stop(_("can not figure out how to import the compartment variable"));
    }
  }
  int wh, cmtI, wh100, whI, wh0, ndose=0, nobs=0;

  CharacterVector units = CharacterVector::create(_["dosing"]=NA_STRING,
                                                  _["time"]=NA_STRING);
  List lst = etEmpty(units);
  CharacterVector cls = lst.attr("class");
  List e = cls.attr(".rxode2.lst");
  LogicalVector show = e["show"];
  show["id"] = true;
  show["amt"] = true;
  std::vector<int> uIds;
  int curevid;
  for (int i = 0; i < oldEvid.size(); i++) {
    curevid = oldEvid[i];
    // Handle missing evid
    if (evidCol == -1 && methodCol == -1 && amtCol != -1){
      if (oldAmt[i] != 0) curevid = 1;
    }
    if (curevid == 0) {
      id.push_back(oldId[i]);
      if (std::find(uIds.begin(), uIds.end(), oldId[i]) == uIds.end()){
        uIds.push_back(oldId[i]);
      }
      low.push_back(NA_REAL);
      if (ISNA(oldTime[i])){
        time.push_back(oldTime[i]);
        high.push_back(NA_REAL);
        if (!cmtC){
          cmt.push_back(oldCmt[i]);
          if (oldCmt[i] > 1) show["cmt"] = true;
        }
        amt.push_back(NA_REAL);
        rate.push_back(NA_REAL);
        dur.push_back(NA_REAL);
        ii.push_back(NA_REAL);
        addl.push_back(NA_INTEGER);
        evid.push_back(2);
        ss.push_back(NA_INTEGER);
        ndose++;
      } else {
        time.push_back(oldTime[i]);
        high.push_back(NA_REAL);
        if (!cmtC){
          cmt.push_back(oldCmt[i]);
          if (oldCmt[i] > 1) show["cmt"] = true;
        }
        amt.push_back(NA_REAL);
        rate.push_back(NA_REAL);
        dur.push_back(NA_REAL);
        ii.push_back(NA_REAL);
        addl.push_back(NA_INTEGER);
        evid.push_back(0);
        ss.push_back(NA_INTEGER);
        nobs++;
      }
    } else if (curevid <= 7) {
      id.push_back(oldId[i]);
      if (std::find(uIds.begin(), uIds.end(), oldId[i]) == uIds.end()){
        uIds.push_back(oldId[i]);
      }
      low.push_back(NA_REAL);
      time.push_back(oldTime[i]);
      high.push_back(NA_REAL);
      if (!cmtC){
        cmt.push_back(oldCmt[i]);
        if (oldCmt[i] > 1) show["cmt"] = true;
      }
      amt.push_back(oldAmt[i]);
      if (curevid >= 5 && oldRate[i] != 0) stop(_("replacement/multiplication events cannot be combined with infusions"));
      if (curevid >= 5 && oldDur[i] != 0) stop(_("replacement/multiplication events cannot be combined with infusions"));
      dur.push_back(oldDur[i]);
      if (oldRate[i] > 0) show["rate"] = true;
      if (oldRate[i] < 0) show["rate"] = true;
      rate.push_back(oldRate[i]);
      if (oldDur[i] > 0) show["dur"] = true;
      ii.push_back(oldIi[i]);
      if (oldIi[i] > 0) show["ii"] = true;
      addl.push_back(oldAddl[i]);
      if (oldAddl[i] > 0) show["addl"] = true;
      evid.push_back(curevid);
      ss.push_back(oldSs[i]);
      if (oldSs[i] > 0) show["ss"] = true;
      ndose++;
    } else {
      // Convert evid
      if (cmtC) {
        stop(_("old rxode2 'evid' values are not supported with string compartments"));
      }
      getWh(curevid, &wh, &cmtI, &wh100, &whI, &wh0);
      cmtI++;
      if (cmtI != 1) show["cmt"] = true;
      if (oldIi[i] > 0) show["ii"] = true;
      switch (whI){
      case 6:
        break;
      case 8:
        // 8 = Duration is modeled, AMT=dose; Rate = AMT/(Modeled Duration) NONMEM RATE=-2
        id.push_back(oldId[i]);
        if (std::find(uIds.begin(), uIds.end(), oldId[i]) == uIds.end()){
          uIds.push_back(oldId[i]);
        }
        low.push_back(NA_REAL);
        time.push_back(oldTime[i]);
        high.push_back(NA_REAL);
        cmt.push_back(cmtI);
        amt.push_back(oldAmt[i]);
        rate.push_back(-2.0);
        dur.push_back(0.0);
        ii.push_back(oldIi[i]);
        addl.push_back(0);
        evid.push_back(1);
        if (whI == 10){
          ss.push_back(1);
          show["ss"] = true;
        } else if (whI == 20){
          ss.push_back(2);
          show["ss"] = true;
        } else {
          ss.push_back(0);
        }
        show["rate"] = true;
        ndose++;
        break;
      case 7:
        break;
      case 9:
        // 9 = Rate is modeled, AMT=dose; Duration = AMT/(Modeled Rate) NONMEM RATE=-1
        id.push_back(oldId[i]);
        if (std::find(uIds.begin(), uIds.end(), oldId[i]) == uIds.end()){
          uIds.push_back(oldId[i]);
        }
        low.push_back(NA_REAL);
        time.push_back(oldTime[i]);
        high.push_back(NA_REAL);
        cmt.push_back(cmtI);
        amt.push_back(oldAmt[i]);
        rate.push_back(-1.0);
        dur.push_back(0.0);
        ii.push_back(oldIi[i]);
        addl.push_back(0);
        evid.push_back(1);
        if (whI == 10){
          ss.push_back(1);
          show["ss"] = true;
        } else if (whI == 20){
          ss.push_back(2);
          show["ss"] = true;
        } else {
          ss.push_back(0);
        }
        show["rate"] = true;
        ndose++;
        break;
      case 2:
      case 1:
        if (oldAmt[i] > 0){
          for (j = i; j < oldEvid.size(); j++){
            if (curevid == oldEvid[j] && oldAmt[i] == -oldAmt[j]){
              double durC = oldTime[j] - oldTime[i];
              id.push_back(oldId[i]);
              if (std::find(uIds.begin(), uIds.end(), oldId[i]) == uIds.end()){
                uIds.push_back(oldId[i]);
              }
              low.push_back(NA_REAL);
              time.push_back(oldTime[i]);
              high.push_back(NA_REAL);
              cmt.push_back(cmtI);
              amt.push_back(oldAmt[i] * durC);
              if (whI == 1){
                rate.push_back(oldAmt[i]);
                dur.push_back(0);
                show["rate"] = true;
              } else {
                rate.push_back(0);
                dur.push_back(durC);
                show["dur"] = true;
              }
              ii.push_back(oldIi[i]);
              addl.push_back(0);
              evid.push_back(1);
              if (whI == 10){
                ss.push_back(1);
                show["ss"] = true;
              } else if (whI == 20){
                ss.push_back(2);
                show["ss"] = true;
              } else {
                ss.push_back(0);
              }
              ndose++;
              break;
            }
          }
        }// else {
        break;
        // }
      case 0:
        // No infusion
        id.push_back(oldId[i]);
        if (std::find(uIds.begin(), uIds.end(), oldId[i]) == uIds.end()){
          uIds.push_back(oldId[i]);
        }
        low.push_back(NA_REAL);
        time.push_back(oldTime[i]);
        high.push_back(NA_REAL);
        cmt.push_back(cmtI);
        amt.push_back(oldAmt[i]);
        rate.push_back(0);
        dur.push_back(0);
        ii.push_back(oldIi[i]);
        addl.push_back(0);
        evid.push_back(1);
        if (whI == 10){
          ss.push_back(1);
          show["ss"] = true;
        } else if (whI == 20){
          ss.push_back(2);
          show["ss"] = true;
        } else {
          ss.push_back(0);
        }
        ndose++;
        break;
      }
    }
  }

  if (uIds.size() > 1){
    show["id"] = true;
  }
  //
  RObject timeUnitInfo;
  bool doTime = false;
  if (Rf_inherits(oldTime, "units")){
    timeUnitInfo = oldTime.attr("units");
    doTime = true;
    oldTime = wrap(time);
    oldTime.attr("class") = "units";
    oldTime.attr("units") = timeUnitInfo;
    units["time"] = as<std::string>(deparseUnit(oldTime));
  } else {
    oldTime = wrap(time);
  }

  // nme[0] = "id";
  lst[0] = wrap(id);

  // nme[2] = "time";
  lst[2] = oldTime;

  NumericVector tmpNv = wrap(low);
  if (doTime){
    tmpNv.attr("class") = "units";
    tmpNv.attr("units") = timeUnitInfo;
  }
  // nme[1] = "low";
  lst[1] = tmpNv;

  tmpNv = wrap(high);
  if (doTime){
    tmpNv.attr("class") = "units";
    tmpNv.attr("units") = timeUnitInfo;
  }
  // nme[3] = "high";
  lst[3] = tmpNv;

  // nme[4] = "cmt";
  if (cmtC){
    show["cmt"] = true;
    lst[4] = oldCmtC;
  } else {
    lst[4] = wrap(cmt);
  }

  RObject amtUnitInfo;
  bool doAmt = false;
  if (Rf_inherits(oldAmt, "units")){
    amtUnitInfo = oldAmt.attr("units");
    doAmt = true;
    oldAmt = wrap(amt);
    oldAmt.attr("class") = "units";
    oldAmt.attr("units") = amtUnitInfo;
    units[0] = as<std::string>(deparseUnit(oldAmt));
  } else {
    oldAmt = wrap(amt);
  }

  // nme[5] = "amt";
  lst[5] = oldAmt;

  oldRate = wrap(rate);
  if (doAmt && doTime){
    std::string rateUnit = as<std::string>(units[0]) + "/" + as<std::string>(units[1]);
    if (haveRateUnits){
      oldRate.attr("class") = "units";
      oldRate.attr("units") = rateUnits;
    }
    oldRate = setUnits(oldRate, rateUnit);
  } else if (haveRateUnits){
    stop(_("'amt'/'time' needs units to convert the rate to the right units to import the data"));
  }

  // nme[6] = "rate";
  lst[6] = oldRate;

  tmpNv = wrap(ii);
  if (doTime){
    tmpNv.attr("class") = "units";
    tmpNv.attr("units") = timeUnitInfo;
  }
  // nme[7] = "ii";
  lst[7] = tmpNv;

  // nme[8] = "addl";
  lst[8] = wrap(addl);

  // nme[9] = "evid";
  lst[9] = wrap(evid);

  // nme[10] = "ss";
  lst[10] = wrap(ss);

  // nme[11] = "dur"
  tmpNv = wrap(dur);
  if (doTime){
    tmpNv.attr("class") = "units";
    tmpNv.attr("units") = timeUnitInfo;
  }
  lst[11] = tmpNv;

  e["units"] = units;
  e["ndose"] = ndose;
  e["nobs"] = nobs;
  e["show"]  = show;
  e["canResize"] = false;
  e["IDs"] = wrap(uIds);
  lst = etSort(lst);
  cls.attr(".rxode2.lst") = e;
  lst.attr("class") = cls;
  lst.attr("row.names") = IntegerVector::create(NA_INTEGER, -(nobs+ndose));
  return lst;
}

List etExpandAddl(List curEt){

  RObject tmp = curEt["id"];
  qassertS(tmp, "X", "id");
  std::vector<int> id = as<std::vector<int>>(tmp);

  tmp  = curEt["low"];
  qassertS(tmp, "n", "low");
  std::vector<double> low = as<std::vector<double>>(tmp);

  tmp  = curEt["high"];
  qassertS(tmp, "n", "high");
  std::vector<double> high = as<std::vector<double>>(tmp);

  tmp  = curEt["time"];
  qassertS(tmp, "N", "time");
  std::vector<double> time = as<std::vector<double>>(tmp);

  tmp  = curEt["evid"];
  qassertS(tmp, "X", "evid");
  std::vector<int> evid = as<std::vector<int>>(tmp);

  tmp  = curEt["addl"];
  qassertS(tmp, "x", "addl");
  std::vector<int> addl = as<std::vector<int>>(tmp);

  tmp  = curEt["ii"];
  qassertS(tmp, "n", "ii");
  std::vector<double> ii = as<std::vector<double>>(tmp);
  RObject cmt = curEt["cmt"];

  std::vector<int> idx0(time.size());
  std::iota(idx0.begin(),idx0.end(),0);

  int i, j;
  int ndose=0;
  int caddl;
  double cii = 0;
  for (j = id.size(); j--;){
    if (addl[j] != 0){
      caddl= addl[j];
      addl[j] = 0;
      cii = ii[j];
      ii[j] = 0;
      for (i = caddl; i--;){
        id.push_back(id[j]);
        evid.push_back(evid[j]);
        low.push_back(low[j]);
        high.push_back(high[j]);
        time.push_back(time[j] + (i+1)*cii);
        addl.push_back(0);
        ii.push_back(0);
        idx0.push_back(j);
        ndose++;
      }
    }
  }
  std::vector<int> idx(time.size());
  IntegerVector ivId=wrap(id);
  NumericVector nvTime=wrap(time);
  IntegerVector ivEvid=wrap(evid);
  Function order3 = getRxFn(".order3");
  IntegerVector ord;
  ord = order3(ivId, nvTime, ivEvid);
  ord = ord - 1;
  idx = as<std::vector<int>>(ord);
  List lst(curEt.size());

  lst.attr("names") = curEt.attr("names");
  // nme[0] = "id";
  lst[0] = IntegerVector(id.size());

  // nme[1] = "time";
  lst[2] = NumericVector(id.size());

  // nme[2] = "low";
  lst[1] = NumericVector(id.size());

  // nme[3] = "high";
  lst[3] = NumericVector(id.size());

  // nme[4] = "cmt";
  if (Rf_inherits(cmt,"integer")){
    lst[4] = IntegerVector(id.size());
  } else {
    lst[4] = CharacterVector(id.size());
  }

  // nme[5] = "amt";
  lst[5] = NumericVector(id.size());

  // nme[6] = "rate";
  lst[6] = NumericVector(id.size());

  // nme[7] = "ii";
  lst[7] = NumericVector(id.size());

  // nme[8] = "addl";
  lst[8] = IntegerVector(id.size());

  // nme[9] = "evid";
  lst[9] = IntegerVector(id.size());

  // nme[10] = "ss";
  lst[10] = IntegerVector(id.size());

  //nme[11] = "dur"
  lst[11] = NumericVector(id.size());
  IntegerVector tmpI, tmpI2;
  NumericVector tmpN, tmpN2;
  CharacterVector tmpC, tmpC2;
  for (i = idx.size(); i--;){
    for (j = 12; j--;){
      if (j == 0){
        tmpI = asIv(lst[0], "lst[0]");
        tmpI[i] = id[idx[i]];
      } else if (j == 1){
        tmpN = asNv(lst[1], "lst[1]");
        tmpN[i] = low[idx[i]];
      } else if (j == 2){
        tmpN = asNv(lst[2], "lst[2]");
        tmpN[i] = time[idx[i]];
      } else if (j == 3){
        tmpN = asNv(lst[3], "lst[3]");
        tmpN[i] = high[idx[i]];
      } else if (j == 8){
        tmpI = asIv(lst[8], "lst[8]");
        tmpI[i] = addl[idx[i]];
      } else if (j == 7){
        tmpN = asNv(lst[7], "lst[7]");
        tmpN[i] = ii[idx[i]];
      } else if (rxIsNum(curEt[j])){
        tmpN = asNv(lst[j], "lst[j]");
        tmpN2 = asNv(curEt[j], "curEt[j]");
        tmpN[i] = tmpN2[idx0[idx[i]]];
      } else if (rxIsInt(curEt[j])) {
        tmpI = asIv(lst[j], "lst[j]");
        tmpI2 = asIv(curEt[j], "curEt[j]");
        tmpI[i] = tmpI2[idx0[idx[i]]];
      } else if (rxIsChar(curEt[j])){
        // Char
        tmpC = asCv(lst[j], "lst[j]");
        tmpC2 = asCv(curEt[j], "curEt[j]");
        tmpC[i] = tmpC2[idx0[idx[i]]];
      }
    }
  }
  CharacterVector cls = clone(asCv(curEt.attr("class"), "class"));
  List eOld = cls.attr(".rxode2.lst");
  List e = clone(eOld);
  LogicalVector show = e["show"];
  e["ndose"] = asInt(e["ndose"], "e[\"ndose\"]")+ndose;
  e["nobs"] = asInt(e["nobs"], "e[\"nobs\"]");
  show["addl"] = false;
  e["show"] = show;
  e.attr("names") = eOld.attr("names");
  cls.attr(".rxode2.lst") = e;
  lst.attr("class") = cls;
  int len = asInt(e["nobs"], "e[\"nobs\"]") +
    asInt(e["ndose"], "e[\"ndose\"]");
  lst.attr("row.names") = IntegerVector::create(NA_INTEGER, -len);
  return lst;
}

List etAddDose(NumericVector curTime, RObject cmt,  double amt, double rate, double ii,
               int addl, int curEvid, int ss, double dur,
               IntegerVector IDs, bool turnOnShowCmt, bool doSampling, List curEt){
  std::vector<double> time = as<std::vector<double>>(curEt["time"]);
  std::vector<int> id = as<std::vector<int>>(curEt["id"]);
  std::vector<int> evid = as<std::vector<int>>(curEt["evid"]);
  std::vector<double> low = as<std::vector<double>>(curEt["low"]);
  std::vector<double> high = as<std::vector<double>>(curEt["high"]);
  int oldSize = id.size();
  int i, j;
  double a, b, c;
  int ndose=0, nobs = 0;
  bool unroll=false;
  for (j = IDs.size(); j--;){
    if (curTime.size() == 1){
      id.push_back(IDs[j]);
      evid.push_back(curEvid);
      time.push_back(curTime[0]);
      low.push_back(NA_REAL);
      high.push_back(NA_REAL);
      ndose++;
      if (doSampling){
        id.push_back(IDs[j]);
        evid.push_back(0);
        time.push_back(curTime[0]);
        low.push_back(NA_REAL);
        high.push_back(NA_REAL);
        nobs++;
        for (i = addl; i--;){
          id.push_back(IDs[j]);
          evid.push_back(0);
          low.push_back(NA_REAL);
          high.push_back(NA_REAL);
          time.push_back(curTime[0] + (i+1)*ii);
          nobs++;
        }
      }
    } else if (curTime.size() == 2) {
      if (curTime[0] < curTime[1]){
        if (doSampling){
          stop(_("'do.sampling' is not supported with dose windows"));
        }
        id.push_back(IDs[j]);
        evid.push_back(curEvid);
        low.push_back(curTime[0]);
        high.push_back(curTime[1]);
        c = Rf_runif(curTime[0], curTime[1]);
        time.push_back(c);
        ndose++;
        for (i = addl; i--;){
          id.push_back(IDs[j]);
          evid.push_back(curEvid);
          a = curTime[0]+ (i+1)*ii;
          b = curTime[1]+ (i+1)*ii;
          low.push_back(a);
          high.push_back(b);
          c = Rf_runif(a, b);
          time.push_back(c);
          ndose++;
          unroll=true;
        }
      } else {
        stop(_("dosing window you need to specify window in order, e.g. 'et(time=list(c(0,2)),amt=3)'"));
      }
    } else {
      stop(_("dosing time or time windows must only be 1-2 elements"));
    }
  }
  std::vector<int> idx(time.size());
  IntegerVector ivId=wrap(id);
  NumericVector nvTime=wrap(time);
  IntegerVector ivEvid=wrap(evid);
  Function order3 = getRxFn(".order3");
  IntegerVector ord = order3(ivId, nvTime, ivEvid);
  ord = ord - 1;
  idx = as<std::vector<int>>(ord);

  List lst(curEt.size());
  IntegerVector tmpI = asIv(curEt["id"], "curEt[\"id\"]"), tmpI2;
  NumericVector tmpN, tmpN2;
  CharacterVector tmpC, tmpC2;
  lst.attr("names") = curEt.attr("names");
  // nme[0] = "id";
  lst[0] = IntegerVector(id.size());

  // nme[1] = "time";
  lst[2] = NumericVector(id.size());

  // nme[2] = "low";
  lst[1] = NumericVector(id.size());

  // nme[3] = "high";
  lst[3] = NumericVector(id.size());

  // nme[4] = "cmt";
  bool isCmtInt=false;
  if (rxIsInt(cmt)){
    lst[4] = IntegerVector(id.size());
    isCmtInt=true;
  } else {
    lst[4] = CharacterVector(id.size());
  }

  // nme[5] = "amt";
  lst[5] = NumericVector(id.size());

  // nme[6] = "rate";
  lst[6] = NumericVector(id.size());

  // nme[7] = "ii";
  lst[7] = NumericVector(id.size());

  // nme[8] = "addl";
  lst[8] = IntegerVector(id.size());

  // nme[9] = "evid";
  lst[9] = IntegerVector(id.size());

  // nme[10] = "ss";
  lst[10] = IntegerVector(id.size());

  //nme[11] = "dur"
  lst[11] = NumericVector(id.size());

  for (i = idx.size(); i--;){
    tmpI = asIv(lst[0], "lst[0]"); // id
    tmpI[i] = id[idx[i]];

    tmpI = asIv(lst[9], "lst[9]"); // evid
    tmpI[i] = evid[idx[i]];

    tmpN = asNv(lst[1], "lst[1]"); // low
    tmpN[i] = low[idx[i]];

    tmpN = asNv(lst[2], "lst[2]"); // time
    tmpN[i] = time[idx[i]];

    tmpN = asNv(lst[3], "lst[3]"); // high
    tmpN[i] = high[idx[i]];

    if (idx[i] >= oldSize){
      if (isCmtInt){
        tmpI = asIv(lst[4], "lst[4]");
        tmpI[i] = asInt(cmt, "cmt");
      } else {
        tmpC = asCv(lst[4], "lst[4]");
        tmpC2 = asCv(cmt, "cmt");
        tmpC[i] = tmpC2[0];
      }

      // nme[5] = "amt";
      tmpN = asNv(lst[5], "lst[5]");
      tmpN[i] = amt;

      // nme[6] = "rate";
      tmpN = asNv(lst[6], "lst[6]");
      tmpN[i] = rate;

      // nme[7] = "ii";
      tmpN = asNv(lst[7], "lst[7]");
      if (unroll){
        tmpN[i] = 0;
      } else {
        tmpN[i] = ii;
      }
      // nme[8] = "addl";
      tmpI = asIv(lst[8], "lst[8]"); // id
      if (unroll){
        tmpI[i] = 0;
      } else {
        tmpI[i] = addl;
      }

      // nme[10] = "ss";
      tmpI = asIv(lst[10], "lst[10]"); // id
      tmpI[i] = ss;

      // nme[11] = "dur";
      tmpN = asNv(lst[11], "lst[11]");
      tmpN[i] = dur;
    } else {
      // low
      for (j = 12; j--;){
        if (rxIsNum(curEt[j])){
          tmpN = asNv(lst[j], "lst[j]");
          tmpN2 = asNv(curEt[j], "curEt[j]");
          tmpN[i] = tmpN2[idx[i]];
        } else if (rxIsInt(curEt[j])) {
          tmpI = asIv(lst[j], "lst[j]");
          tmpI2 = asIv(curEt[j], "curEt[j]");
          tmpI[i] = tmpI2[idx[i]];
        } else if (rxIsChar(curEt[j])){
          // Char
          tmpC = asCv(lst[j], "lst[j]");
          tmpC2 = asCv(curEt[j], "curEt[j]");
          tmpC[i] = tmpC2[idx[i]];
        }
      }
    }
  }
  CharacterVector cls = clone(asCv(curEt.attr("class"), "class"));
  List eOld = cls.attr(".rxode2.lst");
  List e = clone(eOld);
  LogicalVector show = e["show"];
  if (turnOnShowCmt){
    show["cmt"] = true;
  }
  etUpdateCanResize(lst, show, eOld, IDs, e);
  show["amt"] = true;
  if (rate != 0){
    show["rate"] = true;
  }
  if (ss != 0){
    show["ss"] = true;
    show["ii"] = true;
  }
  if (dur != 0){
    show["dur"] = true;
  }
  e["ndose"] = asInt(e["ndose"], "e[\"ndose\"]")+ndose;
  e["nobs"] = asInt(e["nobs"], "e[\"nobs\"]")+nobs;
  if (curTime.size() == 2){
    show["low"] = true;
    show["high"] = true;
  } else {
    if (ii != 0){
      show["ii"] = true;
    }
    if (addl != 0){
      show["addl"] = true;
      show["ii"] = true;
    }
  }
  e["show"] = show;
  e.attr("names") = eOld.attr("names");
  cls.attr(".rxode2.lst") = e;
  lst.attr("class") = cls;
  int len = asInt(e["nobs"], "e[\"nobs\"]") +
    asInt(e["ndose"], "e[\"ndose\"]");
  lst.attr("row.names") = IntegerVector::create(NA_INTEGER, -len);
  return lst;
}

RObject etUpdateObj(List curEt, bool& update, bool& rxSolve, const bool& turnOnId){
  List lst = clone(curEt);
  CharacterVector cls=clone(asCv(curEt.attr("class"), "class"));
  List e = clone(asList(cls.attr(".rxode2.lst"), ".rxode2.lst"));
  CharacterVector units = e["units"];
  if (!CharacterVector::is_na(units[1])){
    lst["ii"]  = setUnits(lst["ii"],  as<std::string>(units[1]));
    lst["dur"]  = setUnits(lst["dur"],  as<std::string>(units[1]));
    lst["low"]  = setUnits(lst["low"],  as<std::string>(units[1]));
    lst["time"] = setUnits(lst["time"], as<std::string>(units[1]));
    lst["high"] = setUnits(lst["high"], as<std::string>(units[1]));
    if (units[1] == "") units[1] = NA_STRING;
  } else {
    lst["ii"]  = setUnits(lst["ii"], "");
    lst["dur"]  = setUnits(lst["dur"], "");
    lst["low"]  = setUnits(lst["low"],  "");
    lst["time"] = setUnits(lst["time"], "");
    lst["high"] = setUnits(lst["high"], "");
  }
  if (!CharacterVector::is_na(units[0])){
    lst["amt"] = setUnits(lst["amt"], as<std::string>(units[0]));
    if (units[0] == "") units[0] = NA_STRING;
  } else {
    lst["amt"] = setUnits(lst["amt"], "");
  }
  if (!CharacterVector::is_na(units[1]) && !CharacterVector::is_na(units[0])){
    std::string rateUnit = as<std::string>(units[0]) + "/" + as<std::string>(units[1]);
    NumericVector oldRate = as<NumericVector>(lst["rate"]);
    if (Rf_inherits(oldRate, "units")){
      oldRate=setUnits(oldRate, rateUnit);
      lst["rate"] = oldRate;
    } else {
      lst["rate"] = setUnits(oldRate, rateUnit);
    }
  } else {
    lst["rate"] = setUnits(lst["rate"], "");
  }
  e["units"] = units;
  if (turnOnId){
    LogicalVector show = asLv(e["show"], "e[\"show\"]");
    show["id"] = true;
  }
  cls.attr(".rxode2.lst") = e;
  lst.attr("class") = cls;
  int len = asInt(e["nobs"], "e[\"nobs\"]") +
    asInt(e["ndose"], "e[\"ndose\"]");
  lst.attr("row.names") = IntegerVector::create(NA_INTEGER, -len);
  if (update){
    List cmp = asList(evCur, "evCur");
    for (int j = lst.size(); j--;){
      cmp[j] = lst[j];
    }
    cmp.attr("class") = clone(asCv(lst.attr("class"), "class"));
    cmp.attr("names") = lst.attr("names");
    cmp.attr("row.names") = lst.attr("row.names");
    cmp.attr("row.names") = IntegerVector::create(NA_INTEGER, -len);
  }
  if (rxSolve){
    Function rxs("rxSolve.default", rxode2env());
    return rxs(_["object"]=curSolve, _["events"]=lst);
  } else {
    return as<RObject>(lst);
  }
}

RObject etCmtInt(RObject et){
  List cur = asList(et, "et");
  List newEt;
  if (rxIsChar(cur[4])){
    newEt = clone(cur);
    CharacterVector oldCmt = CharacterVector(cur[4]);
    IntegerVector newCmt = IntegerVector(oldCmt.size());
    for (int j = newCmt.size();j--;){
      if (oldCmt[j] == "(default)") newCmt[j] = 1;
      else if (oldCmt[j] == "(obs)") newCmt[j] = NA_INTEGER;
      else stop(_("cannot mix named compartments and integer compartments"));
    }
    newEt[4] = newCmt;
  } else {
    newEt = cur;
  }
  return (as<RObject>(newEt));
}

RObject etSetUnit(List curEt, CharacterVector units){
  CharacterVector cls = clone(asCv(curEt.attr("class"), "class"));
  List eOld = cls.attr(".rxode2.lst");
  List e = clone(eOld);
  CharacterVector oldUnits = clone(asCv(e["units"], "e[\"units\"]"));
  e["units"] = units;
  List lst = clone(curEt);
  if (as<std::string>(oldUnits[1]) != as<std::string>(units[1])){
    if (!CharacterVector::is_na(units[1])){
      lst["ii"]  = setUnits(lst["ii"],  as<std::string>(units[1]));
      lst["dur"]  = setUnits(lst["dur"],  as<std::string>(units[1]));
      lst["low"]  = setUnits(lst["low"],  as<std::string>(units[1]));
      lst["time"] = setUnits(lst["time"], as<std::string>(units[1]));
      lst["high"] = setUnits(lst["high"], as<std::string>(units[1]));
    } else {
      lst["ii"]  = setUnits(lst["ii"],  "");
      lst["dur"]  = setUnits(lst["dur"],  "");
      lst["low"]  = setUnits(lst["low"],  "");
      lst["time"] = setUnits(lst["time"], "");
      lst["high"] = setUnits(lst["high"], "");
    }
  }
  if (as<std::string>(oldUnits[0]) != as<std::string>(units[0])){
    if (!CharacterVector::is_na(units[0])){
      lst["amt"] = setUnits(lst["amt"], as<std::string>(units[0]));
    } else {
      lst["amt"] = setUnits(lst["amt"], "");
    }
  }
  if (!CharacterVector::is_na(units[1]) && !CharacterVector::is_na(units[0])){
    std::string rateUnit = as<std::string>(units[0]) + "/" + as<std::string>(units[1]);
    NumericVector oldRate = asNv(lst["rate"], "lst[\"rate\"]");
    if (Rf_inherits(oldRate, "units")){
      oldRate=setUnits(oldRate, rateUnit);
      lst["rate"] = oldRate;
    } else {
      lst["rate"] = setUnits(oldRate, rateUnit);
    }
  } else {
    lst["rate"] = setUnits(lst["rate"], "");
  }
  cls.attr(".rxode2.lst") = e;
  lst.attr("class") = cls;
  return as<RObject>(lst);
}

List etResizeId(List curEt, IntegerVector IDs){
  // Calculate size
  CharacterVector cls = clone(asCv(curEt.attr("class"), "class"));
  List eOld = cls.attr(".rxode2.lst");
  List e = clone(eOld);
  LogicalVector show = asLv(e["show"], "e[\"show\"]");
  bool showId = asBool(show["id"], "show[\"id\"]");
  std::vector<int> oldIDs = as<std::vector<int>>(e["IDs"]);
  if (!showId && oldIDs.size() == 1 && IDs.size() >= 1){
    oldIDs[0] = IDs[0];
    IntegerVector tmpI = asIv(curEt[0],"curEt[0]");
    std::fill(tmpI.begin(), tmpI.end(),IDs[0]);
  }
  // Check IDs to remove
  int i;
  std::vector<int> rmIds;
  std::vector<int> newIds;
  for (i = IDs.size(); i--;){
    if (std::find(oldIDs.begin(), oldIDs.end(), IDs[i]) == oldIDs.end()){
      if (IDs[i] < 0){
        rmIds.push_back(-IDs[i]);
      } else {
        newIds.push_back(IDs[i]);
      }
    }
  }
  if (rmIds.size() == 0 && newIds.size() == 0){
    return curEt;
  }
  if (rmIds.size() > 0){
    List newEt(curEt.size());
    // Remove ids
    std::vector<int> finalIds;
    for (i = oldIDs.size(); i--;){
      if (std::find(rmIds.begin(), rmIds.end(), oldIDs[i]) == rmIds.end()){
        finalIds.push_back(oldIDs[i]);
      }
    }
    int rmId = NA_INTEGER, newId = NA_INTEGER;
    if (finalIds.size() == 0 && newIds.size() == 0){
      return etEmpty(e["units"]);
    } else if (finalIds.size() == 0 && newIds.size() > 0){
      if (asBool(e["canResize"], "e[\"canResize\"]")){
        newId = newIds.back();
        newIds.pop_back();
        finalIds.push_back(newId);
        rmId = rmIds.back();
        rmIds.pop_back();
      } else {
        stop(_("cannot add more 'ID's to this event table"));
      }
    }
    int nobs = 0;
    int ndose = 0;
    std::vector<int> id = as<std::vector<int>>(curEt["id"]);
    std::vector<int> evid = as<std::vector<int>>(curEt["evid"]);
    std::vector<bool> isIn;
    for (i = 0; i < (int)id.size(); i++){
      if (std::find(rmIds.begin(), rmIds.end(), id[i]) == rmIds.end()){
        if (evid[i] == 0){
          nobs++;
        } else {
          ndose++;
        }
        isIn.push_back(true);
      } else {
        isIn.push_back(false);
      }
    }
    int j, newSize = nobs+ndose, k=0;
    IntegerVector tmpI, tmpI2;
    CharacterVector tmpC, tmpC2;
    NumericVector tmpN, tmpN2;
    for (j = newEt.size(); j--;){
      k=newSize-1;
      for (i = (int)id.size();i--;){
        if (rxIsNum(curEt[j])) {
          if (i == (int)id.size()-1) newEt[j] = NumericVector(newSize);
          if (isIn[i]){
            tmpN=newEt[j];
            tmpN2   = curEt[j];
            tmpN[k] = tmpN2[i];
          }
        } else if (rxIsInt(curEt[j])) {
          if (i == (int)id.size()-1) newEt[j] = IntegerVector(newSize);
          if (isIn[i]){
            tmpI  = newEt[j];
            tmpI2 = curEt[j];
            // Replace ID if needed
            if (j == 0 && tmpI2[i] == rmId){
              tmpI[k] = newId;
            } else {
              tmpI[k] = tmpI2[i];
            }
          }
        } else if (rxIsChar(curEt[j])){
          if (i == (int)id.size()-1) newEt[j] = CharacterVector(newSize);
          if (isIn[i]){
            tmpC=newEt[j];
            tmpC2 = curEt[j];
            tmpC[k] = tmpC2[i];
          }
        }
        if (isIn[i]){
          k--;
        }
      }
    }
    CharacterVector cls = clone(asCv(curEt.attr("class"), "class"));
    List e = asList(cls.attr(".rxode2.lst"), ".rxode2.lst");
    e["nobs"] = nobs;
    e["ndose"] = ndose;
    e["IDs"] = wrap(finalIds);
    cls.attr(".rxode2.lst") = e;
    newEt.attr("class") = cls;
    newEt.attr("names") = curEt.attr("names");
    newEt.attr("row.names") = IntegerVector::create(NA_INTEGER,-(nobs+ndose));
    if (newIds.size() > 0){
      // Need to add ids, call recursively.
      return etResizeId(newEt, wrap(newIds));
    } else {
      // Note that if newId is NA and no more are added, then it is a
      // simple ID replacement.  Otherwise the event table is sorted;
      // Overall there is no need to sort here.
      return newEt;
    }
  } else {
    // Add ids?
    if (asBool(eOld["canResize"],"eOld[\"canResize\"]")){
      // Enlarge data-set
      int oldMaxId = oldIDs.size();
      int maxId = oldMaxId + newIds.size();
      double c = (double)(maxId)/(double)(oldMaxId);
      int oldSize = asInt(e["nobs"], "e[\"nobs\"]") +
        asInt(e["ndose"], "e[\"ndose\"]");
      int newSize = (int)(oldSize*c);
      int idSize = (int)((double)(oldSize)/(double)(oldMaxId));
      // Add new ids.
      for (i = newIds.size(); i--;){
        oldIDs.push_back(newIds[i]);
      }
      SORT(oldIDs.begin(),oldIDs.end()); // Less expensive, then whole table doesn't need to be sorted.
      int j;
      IntegerVector tmpI, tmpI2;
      NumericVector tmpN, tmpN2;
      CharacterVector tmpC, tmpC2;
      List newEt(curEt.size());
      for (j = newEt.size(); j--;){
        if (rxIsInt(curEt[j])) {
          tmpI = IntegerVector(newSize);
          tmpI2 = asIv(curEt[j], "curEt[j]");
          std::copy(tmpI2.begin(), tmpI2.end(), tmpI.begin());
          if (j == 0){
            for (i = oldMaxId+1; i <= maxId; i++){
              std::fill_n(tmpI.begin() + oldSize + (i-oldMaxId-1)*idSize, idSize, oldIDs[i-1]);
            }
          } else {
            for (i = newSize - oldSize; i--;){
              tmpI[oldSize+i] = tmpI2[i % oldSize];
            }
          }
          newEt[j] = tmpI;
        } else if (rxIsChar(curEt[j])){
          // Char
          tmpC = CharacterVector(newSize);
          tmpC2 = asCv(curEt[j], "curEt[j]");
          std::copy(tmpC2.begin(), tmpC2.end(), tmpC.begin());
          for (i = newSize - oldSize; i--;){
            tmpC[oldSize+i] = tmpC2[i % oldSize];
          }
          newEt[j] = tmpC;
        } else {
          tmpN = NumericVector(newSize);
          tmpN2 = asNv(curEt[j], "curEt[j]");
          std::copy(tmpN2.begin(), tmpN2.end(), tmpN.begin());
          for (i = newSize - oldSize; i--;){
            tmpN[oldSize+i] = tmpN2[i % oldSize];
          }
          newEt[j] = tmpN;
        }
      }
      newEt.attr("names")     = curEt.attr("names");
      bool recalcTime = false;
      tmpN = asNv(newEt["time"], "newEt[\"time\"]");
      NumericVector tmpN1 = asNv(newEt["low"], "newEt[\"low\"]");
      tmpN2 = asNv(newEt["high"], "newEt[\"high\"]");
      // Update new observations with recalculated windows
      for (i = newSize - oldSize; i--;){
        if (!ISNA(tmpN1[oldSize+i]) && !ISNA(tmpN2[oldSize+i])){
          tmpN[oldSize+i] = Rf_runif(tmpN1[oldSize+i], tmpN2[oldSize+i]);
          recalcTime=true;
        }
      }
      e["nobs"]   = (int)((double)(asInt(e["nobs"], "e[\"nobs\"]"))*c);
      e["ndose"]  = (int)((double)(asInt(e["ndose"], "e[\"ndose\"]"))*c);
      LogicalVector show = e["show"];
      show["id"] = true;
      e["show"] = show;
      e["IDs"] = wrap(oldIDs);
      e.attr("class")         = "rxHidden";
      cls.attr(".rxode2.lst")  = e;
      newEt.attr("class")     = cls;
      int len = asInt(e["nobs"], "e[\"nobs\"]") +
        asInt(e["ndose"], "e[\"ndose\"]");
      newEt.attr("row.names") = IntegerVector::create(NA_INTEGER, -len);
      if (recalcTime){
        // Will have to sort with new times.
        newEt = etSort(newEt);
      }
      return newEt;
    } else {
      stop(_("cannot add more 'ID's to this event table"));
    }
  }
}

List getEtRxsolve(Environment e);

//[[Rcpp::export]]
RObject et_(List input, List et__) {
  // Create or modify new event table
  bool doRet = false;
  bool turnOnId = false;
  bool turnOnShowCmt = false;
  bool doUpdateObj = false;
  RObject curEt;
  bool foundEt = false;
  bool inputSolve = false;
  if (et__.size() > 0) {
    if (rxIsChar(et__[0])) {
      if (as<std::string>(et__[0]) == "last") {
        doUpdateObj=true;
        curEt = evCur;
        foundEt=true;
      } else if (as<std::string>(et__[0]) == "import") {
        bool bt = true, bf = false;
        return etUpdateObj(etImportEventTable(as<List>(input["data"]), true), bt, bf, bt);
      } else if (as<std::string>(et__[0]) == "importQuiet") {
        bool bt = true, bf = false;
        return etUpdateObj(etImportEventTable(as<List>(input["data"]), false), bt, bf, bt);
      }
    } else if (_rxIsEt(et__)) {
      foundEt=true;
      curEt = as<RObject>(et__);
    } else if (Rf_inherits(et__, "rxSolve")){
      foundEt=true;
      RObject c = et__.attr("class");
      c = c.attr(".rxode2.env");
      curEt = getEtRxsolve(as<Rcpp::Environment>(c));
      inputSolve=true;
      curSolve=et__;
    }
  }
  CharacterVector inN;
  if (input.hasAttribute("names")){
    inN = input.attr("names");
  }
  int i, amtIx = -1, iiIx = -1, addlIx = -1,
    untilIx = -1, evidIx=-1, idIx=-1, cmtIx=-1,
    amtUnitIx=-1, timeUnitIx=-1, doSamplingIdx=-1, timeIx=-1,
    rateIx = -1,  durIx = -1, nbrIx=-1, ssIx=-1;
  const char *amtChar = "amt";
  const char *amtUnitChar = "amtUnit";
  const char *timeUnitChar = "timeUnit";
  const char *cmtChar = "cmt";
  const char *evidChar = "evid";
  const char *ssChar = "ss";
  const char *iiChar = "ii";
  const char *untilChar = "until";
  const char *addlChar = "addl";
  const char *nbrChar = "nbrDoses";
  const char *doSamplingChar = "doSampling";
  const char *durChar = "dur";
  const char *rateChar = "rate";
  // Wait should be in sequences and rep
  for (i = (int)inN.size(); i--;){
    if (inN[i] == "amt" || inN[i] == "dose") {
      if (amtIx != -1){
        stop(_("can only have one of the following: 'amt', 'dose'"));
      }
      amtIx=i;
      amtChar = CHAR(STRING_ELT(inN, amtIx));
    }
    else if (inN[i] == "ii" || inN[i] == "dosing.interval" || inN[i] == "dosingInterval" || inN[i] == "dosing_interval"){
      if (iiIx != -1){
        stop(_("can only have one of the following: 'ii', 'dosing.interval', 'dosingInterval' or 'dosing_interval'"));
      }
      iiIx=i;
    }
    else if (inN[i] == "addl") {
      addlIx = i;
      addlChar=CHAR(STRING_ELT(inN, i));
    }
    else if (inN[i] == "until" || inN[i] == "to") {
      untilIx = i;
      untilChar=CHAR(STRING_ELT(inN, i));
    }
    else if (inN[i] == "evid") {
      evidIx = i;
      evidChar=CHAR(STRING_ELT(inN, i));
    }
    else if (inN[i] == "ID" || inN[i] == "id") idIx=i;
    else if (inN[i] == "cmt" || inN[i] == "dosing.to" || inN[i] == "dosingTo" || inN[i] =="dosing_to" ||
             inN[i] == "dose.to" || inN[i] == "doseTo" || inN[i] == "dose_to" || inN[i] == "state"){
      if (cmtIx != -1){
        stop(_("can only have one of the following: 'cmt', 'dosing.to', 'dose.to', 'state'"));
      }
      cmtIx=i;
      cmtChar=CHAR(STRING_ELT(inN, cmtIx));
    }
    else if (inN[i] == "amount.units" || inN[i] == "amountUnits" || inN[i] == "amount_units" ||
             inN[i] == "amt.units" || inN[i] == "amtUnits" || inN[i] == "amt_units" ||
             inN[i] == "dose.units" || inN[i] == "doseUnits" || inN[i] == "dose_units") {
      if (amtUnitIx != -1){
        stop(_("can only have one of the following: 'amount.units', 'amt.units', 'dose.units'"));
      }
      amtUnitIx=i;
      amtUnitChar = CHAR(STRING_ELT(inN, amtUnitIx));
    }
    else if (inN[i] == "time.units" || inN[i] == "timeUnits" || inN[i] == "time_units") {
      if (timeUnitIx != -1) {
        stop(_("can only have one of the following: 'time.units', 'timeUnits', 'time_units'"));
      }
      timeUnitIx=i;
      timeUnitChar=CHAR(STRING_ELT(inN, i));
    }
    else if (inN[i] == "do.sampling" || inN[i] == "doSampling" || inN[i] == "do_sampling" ||
             inN[i] == "add.sampling" || inN[i] == "addSampling" || inN[i] == "add_sampling") {
      if (doSamplingIdx != -1) {
        stop(_("can only have one of the following: 'add.sampling', 'do.sampling'"));
      }
      doSamplingIdx=i;
      doSamplingChar=CHAR(STRING_ELT(inN, i));
    }
    else if (inN[i] == "time" || inN[i] == "start.time" || inN[i] == "startTime" || inN[i] == "start_time" ||
             inN[i] == "start" || inN[i] == "from"){
      if (timeIx != -1){
        stop(_("can only have one of the following: 'time', 'start.time', 'from'"));
      }
      timeIx = i;
    }
    else if (inN[i] == "nbr.doses" || inN[i] == "nbrDoses" || inN[i] == "nbr") {
      if (nbrIx != -1){
        stop(_("can only have one of the following: 'nbrDoses', 'nbr.doses'"));
      }
      nbrIx=i;
      nbrChar = CHAR(STRING_ELT(inN, i));
    }
    else if (inN[i] == "ss") {
      ssIx = i;
      ssChar = CHAR(STRING_ELT(inN, ssIx));
    }
    else if (inN[i] == "rate") {
      rateIx = i;
      rateChar = CHAR(STRING_ELT(inN, i));
    }
    else if (inN[i] == "dur" || inN[i] == "duration") {
      if (durIx != -1){
        stop(_("can only have one of the following: 'dur', 'duration'"));
      }
      durIx = i;
      durChar = CHAR(STRING_ELT(inN, i));
    }
    else if (inN[i] != "" && inN[i] != "simulate" && inN[i] != "envir" &&
             inN[i] != "expand" && !doUpdateObj){
      stop(_("unused argument '%s'"), (as<std::string>(inN[i])).c_str());
    }
  }
  // missing argument name handling.
  for (i = 0; i <(int)inN.size(); i++){
    if (inN[i] == ""){
      if (rxIsChar(input[i]) && cmtIx == -1) cmtIx = i;
      if (rxIsNum(input[i]) && timeIx == -1) timeIx = i;
      if (rxIsInt(input[i]) && timeIx == -1) timeIx = i;
      if (rxIsCleanList(input[i]) && timeIx == -1) timeIx=i;
      if (_rxIsEt(input[i])){
        if (foundEt) stop(_("multiple event tables supplied, not sure what to do; try 'c', 'rbind', 'seq' or 'rep'"));
        foundEt=true;
        curEt = input[i];
      }
      if (Rf_inherits(input[i], "rxSolve")){
        if (foundEt) stop(_("multiple event tables supplied, not sure what to do; try 'c', 'rbind', 'seq' or 'rep'"));
        foundEt=true;
        inputSolve=true;
        curEt = getEtRxsolve(input[i]);
        curSolve=et__;
      }
    }
  }
  if (inN.size() == 0 && input.size() != 0){
    for (i = 0; i <(int)input.size(); i++){
      if (rxIsChar(input[i])) cmtIx = i;
      if (rxIsNum(input[i]) && timeIx == -1) timeIx = i;
      if (rxIsInt(input[i]) && timeIx == -1) timeIx = i;
      if (rxIsCleanList(input[i]) && timeIx == -1) timeIx=i;
      if (_rxIsEt(input[i])){
        if (foundEt) stop(_("multiple event tables supplied, not sure what to do; try 'c', 'rbind', 'seq' or 'rep'"));
        foundEt=true;
        curEt = input[i];
      }
      if (Rf_inherits(input[i], "rxSolve")){
        if (foundEt) stop(_("multiple event tables supplied, not sure what to do; try 'c', 'rbind', 'seq' or 'rep'"));
        foundEt=true;
        inputSolve=true;
        curEt = getEtRxsolve(input[i]);
        curSolve=et__;
      }
    }
  }
  if (_rxIsEt(curEt)){
    CharacterVector cls = clone(asCv(curEt.attr("class"), "class"));
    List e = cls.attr(".rxode2.lst");
    CharacterVector oldUnits = e["units"];
    CharacterVector units(2);
    bool foundUnits = false;
    if (amtUnitIx == -1){
      units[0] = oldUnits[0];
    } else  {
      CharacterVector tmpS = asCv(input[amtUnitIx], "input[amtUnitIx]");
      if (tmpS.size() != 1) stop(_("'%s' cannot be a vector"), amtUnitChar);
      units[0] = tmpS[0];
      foundUnits = true;
    }
    if (timeUnitIx == -1){
      units[1] = oldUnits[1];
    } else  {
      CharacterVector tmpS = asCv(input[timeUnitIx], "input[timeUnitIx]");
      if (tmpS.size() != 1) stop(_("'%s' cannot be a vector"), timeUnitChar);
      units[1] = tmpS[0];
      foundUnits = true;
    }
    units.attr("names") = CharacterVector::create("dosing", "time");
    if (foundUnits){
      curEt = etSetUnit(asList(curEt, "curEt"), units);
      doRet=true;
    }
    // This is a modification to an existing event table.
    if (TYPEOF(input[0]) == LGLSXP){
      LogicalVector in0 = asLv(input[0], "input[0]");
      if (in0[0]){
        CharacterVector nm = input.attr("names");
        if (nm[0] == "getUnits"){
          return(e["units"]);
        } else if (nm[0] == "get.nobs"){
          return e["nobs"];
        } else if (nm[0] == "get.dose"){
          return e["nobs"];
        } else if (nm[0] == "simulate"){
          bool bt = true;
          return etUpdateObj(etSimulate(asList(curEt, "curEt")),
                             doUpdateObj, inputSolve, bt);
        } else if (nm[0] == "copy"){
          // Make sure that the object is cloned
          bool bf=false;
          return etUpdateObj(asList(curEt, "curEt"),bf, bf, bf);
        } else if (nm[0] == "get.EventTable"){
          e.attr("class") = R_NilValue;
          if (asInt(e["nobs"], "e[\"nobs\"]") == 0 &&
              asInt(e["ndose"], "e[\"ndose\"]") == 0){
            return R_NilValue;
          } else {
            CharacterVector cls = clone(asCv(curEt.attr("class"), "class"));
            List e = clone(asList(cls.attr(".rxode2.lst"), ".rxode2.lst"));
            LogicalVector show = e["show"];
            List cur = clone(asList(curEt, "curEt"));
            CharacterVector curN = cur.attr("names");
            int lenShow=0;
            for (int i = show.size();i--;){
              if (show[i]) lenShow++;
            }
            List ret(lenShow);
            CharacterVector nm(lenShow);
            int j=0;
            for (int i = 0; i < show.size(); i++){
              if (show[i]){
                ret[j] = cur[i];
                nm[j]= curN[i];
                j++;
              }
            }
            ret.attr("names") = nm;
            ret.attr("row.names") = cur.attr("row.names");
            ret.attr("class") = "data.frame";
            return as<RObject>(ret);
          }
        } else if (nm[0] == "get.obs.rec"){
          List lst = asList(curEt, "curEt");
          IntegerVector evid = asIv(lst["evid"], "lst[\"evid\"]");
          LogicalVector ret(evid.size());
          for (int i = evid.size(); i--;) ret[i] = (evid[i] == 0);
          return as<RObject>(ret);
        } else if (nm[0] == "expand"){
          RObject ret = etUpdateObj(etExpandAddl(asList(curEt, "curEt")),
                                    doUpdateObj, inputSolve, turnOnId);
          return ret;
        } else if (nm[0] == "get.sampling" || nm[0] == "get.dosing" ||
                   nm[0] == "clearSampling" || nm[0] == "clearDosing"){
          // Need to update
          bool doDose = (nm[0] == "get.dosing" || nm[0] == "clearSampling");
          bool updateObj = (nm[0] == "clearSampling" || nm[0] == "clearDosing");
          int n = doDose ? asInt(e["ndose"], "ndose") :
            asInt(e["nobs"], "e[\"nobs\"]");
          List cmp;
          if (n == 0) {
            if (updateObj){
              cmp = asList(curEt,"curEt");
              List em =etEmpty(asCv(e["units"], "e[\"units\"]"));
              cls = clone(asCv(em.attr("class"), "class"));
              for (int j = cmp.size(); j--;){
                if (rxIsNum(cmp[j])){
                  cmp[j] = NumericVector(0);
                } else if (rxIsInt(cmp[j])){
                  cmp[j] = IntegerVector(0);
                } else if (rxIsChar(cmp[j])) {
                  cmp[j] = CharacterVector(0);
                }
              }
              cmp.attr("row.names") = IntegerVector::create(NA_INTEGER, -n);
              cmp.attr("class") = cls;
              return R_NilValue;
            } else {
              return R_NilValue;
            }
          } else {
            cmp = asList(curEt, "curEt");
            List ret(cmp.size());
            IntegerVector evid = asIv(cmp["evid"], "cmp[\"evid\"]");
            int i, j, k;
            for (j = ret.size(); j--;){
              if (rxIsNum(cmp[j])){
                ret[j] = NumericVector(n);
              } else if (rxIsInt(cmp[j])){
                ret[j] = IntegerVector(n);
              } else if (rxIsChar(cmp[j])){
                ret[j] = CharacterVector(n);
              }
            }
            k = 0;
            NumericVector tmpN, tmpN2;
            IntegerVector tmpI, tmpI2;
            CharacterVector tmpC, tmpC2;
            for (i = 0; i < evid.size(); i++){
              if ((doDose && evid[i] != 0) || (!doDose && evid[i] == 0)){
                for (j = ret.size(); j--;){
                  if (rxIsNum(cmp[j])){
                    tmpN = ret[j];
                    tmpN2 = cmp[j];
                    tmpN[k] = tmpN2[i];
                  } else if (rxIsInt(cmp[j])){
                    tmpI = ret[j];
                    tmpI2 = cmp[j];
                    tmpI[k] = tmpI2[i];
                  } else if (rxIsChar(cmp[j])) {
                    tmpC = ret[j];
                    tmpC2 = cmp[j];
                    tmpC[k] = tmpC2[i];
                  }
                }
                k++;
              }
            }
            ret.attr("names") = cmp.attr("names");
            ret.attr("row.names") = IntegerVector::create(NA_INTEGER, -n);
            if (updateObj){
              // This updates the object in place.
              if (doDose){
                e["nobs"] = 0;
              } else {
                e["ndose"] = 0;
              }
              cls.attr(".rxode2.lst") = e;
              for (j = cmp.size(); j--;){
                cmp[j] = ret[j];
              }
              cmp.attr("row.names") = IntegerVector::create(NA_INTEGER, -n);
              cmp.attr("class") = cls;
              return as<RObject>(ret);
            } else {
              ret.attr("class") = "data.frame";
            }
            return as<RObject>(ret);
          }
        } else {
          Rf_warningcall(R_NilValue, "%s", _("nothing done"));
          return  asList(curEt, "curEt");
        }
      } else {
        Rf_warningcall(R_NilValue, "%s", _("nothing done"));
        return  asList(curEt, "curEt");
      }
    } else {
      // We are updating the event table
      IntegerVector id; // = e["IDs"];
      if (idIx != -1){
        id    = asIv(input[idIx], "input[idIx]");
        CharacterVector cls = clone(asCv(curEt.attr("class"), "class"));
        List e = clone(asList(cls.attr(".rxode2.lst"), ".rxode2.lst"));
        LogicalVector show = e["show"];
        if (!show["id"] || id[0] == 1 || id[0] < 0 ) {
          curEt = as<RObject>(etResizeId(as<List>(curEt), id));
          turnOnId=true;
          doRet=true;
        } else {
          e["canResize"] = false;
          cls.attr(".rxode2.lst") = e;
          curEt.attr("class") = cls;
        }
      } else {
        id = asIv(e["IDs"], "e[\"IDs\"]");
      }
      CharacterVector cls = clone(asCv(curEt.attr("class"), "class"));
      List e = cls.attr(".rxode2.lst");
      CharacterVector cmtS;
      IntegerVector cmtI;
      RObject cmt;
      bool cmtNeg = false;
      // Dose
      if (cmtIx == -1){
        List tmp = asList(curEt, "curEt");
        if (rxIsInt(tmp[4])){
          cmtI = IntegerVector(1);
          if (amtIx == -1){
            cmtI[0] = NA_INTEGER;
          } else {
            cmtI[0] = 1;
          }
          cmt = as<RObject>(cmtI);
        } else {
          cmtS = CharacterVector(1);
          if (amtIx == -1){
            cmtS[0] = "(obs)";
          } else {
            cmtS[0] = "(default)";
          }
          cmt = as<RObject>(cmtS);
        }
      } else {
        if (rxIsChar(input[cmtIx])){
          cmtS = asCv(input[cmtIx], "input[cmtIx]");
          if (cmtS.size() == 1){
            List tmp = as<List>(curEt);
            if (rxIsInt(tmp[4])){
              stop(_("cannot mix named and integer compartments in '%s'"), cmtChar);
            }
            std::string curCmt = as<std::string>(cmtS[0]);
            if (curCmt.substr(0, 1) == "-"){
              cmtNeg = true;
            }
            cmt = as<RObject>(cmtS);
          } else {
            stop(_("'%s' compartment cannot be a vector"), cmtChar);
          }
        } else if (rxIsNumInt(input[cmtIx])){
          cmtI = asIv(input[cmtIx], "input[cmtIx]");
          if (cmtI.size() == 1){
            curEt=etCmtInt(curEt);
            cls = clone(asCv(curEt.attr("class"), "class"));
            e = cls.attr(".rxode2.lst");
            if (cmtI[0] < 0){
              cmtNeg = true;
            }
            if (cmtI[0] == 0){
              stop(_("'%s' cannot be zero"), cmtChar);
            }
            cmt = as<RObject>(cmtI);
          } else {
            stop(_("'%s' cannot be an vector"), cmtChar);
          }
        } else {
          stop(_("'%s' must be an integer or a character"), cmtChar);
        }
        turnOnShowCmt=true;
      }
      NumericVector amt(1);
      bool isObs=false;
      bool zeroAmt=false;
      if (amtIx == -1){
        isObs=true;
      } else {
        NumericVector tmpNV = asNv(input[amtIx], "input[amtIx]");
        if (amt.size() != 1){
          stop(_("'%s' cannot be a vector"), amtChar);
        }
        amt[0] = tmpNV[0];
        if (Rf_inherits(tmpNV, "units")){
          CharacterVector cls = clone(asCv(curEt.attr("class"), "class"));
          List e = clone(asList(cls.attr(".rxode2.lst"), ".rxode2.lst"));
          CharacterVector units = e["units"];
          if (!CharacterVector::is_na(units["dosing"])){
            amt = setUnits(tmpNV, as<std::string>(units["dosing"]));
          } else {
            amt = setUnits(amt, "");
          }
        }
        if (amt[0]==0) zeroAmt=true;
        isObs = false;
      }
      if (cmtNeg && isObs){
        isObs = false;
      }
      IntegerVector evid(1);
      if (evidIx != -1){
        IntegerVector tmp = as<IntegerVector>(input[evidIx]);
        if (evid.size()!= 1){
          stop(_("'%s' cannot be a vector"), evidChar);
        }
        evid[0] = tmp[0];
        if (cmtNeg && evid[0] != 2){
          stop(_("turning off compartments can only be done when '%s'=2"), evidChar);
        }
        if (evid[0] == 0 && isObs){
          stop(_("zero '%s' cannot be used with '%s'"), evidChar, amtChar);
        }
        if ((evid[0] == 1 || evid[0] == 4) && isObs){
          stop(_("'%s' requires an '%s'"), evidChar, amtChar);
        } else if (evid[0] == 2 || evid[0] == 3) {
          if (amtIx == -1){
            if (amt[0] != 0 && NumericVector::is_na(amt[0])){
              Rf_warningcall(R_NilValue, "'%s' is ignored when '%s'=2 or '%s'=3", amtChar,
                             evidChar, evidChar);
            }
          }
          amt[0] = NA_REAL;
          isObs = false;
        }
      } else {
        evid = IntegerVector(1);
      }
      bool ssInf = false;
      if (isObs && rateIx != -1 && ssIx!=-1){
        isObs = false;
        amt[0] = 0.0;
        ssInf=true;
      }
      if (isObs){
        if (evidIx == -1) evid[0]=0;
        IntegerVector addl;// = 0;
        if (addlIx != -1){
          addl = as<IntegerVector>(input[addlIx]);
          if (addl.size() != 1) stop(_("'%s' cannot be a vector"), addlChar);
          if (addl[0] != 0){
            stop(_("'%s' needs a '%s'"), addlChar, amtChar);
          }
        } else {
          addl = IntegerVector(1);
          addl[0] = 0;
        }
        if (untilIx != -1){
          stop(_("'%s' needs a '%s'"), untilChar, amtChar);
        }
        if (nbrIx != -1){
          stop(_("'%s' needs a '%s'"), nbrChar, amtChar);
        }
        NumericVector rate;
        if (rateIx != -1){
          if (durIx != -1){
            stop(_("can not specify '%s' and '%s' for a dose, please pick one"),
                 durChar, rateChar);
          }
          rate = as<NumericVector>(input[rateIx]);
          if (rate.size() != 1) stop(_("'%s' cannot be a vector"), rateChar);
          if (rate[0] != 0.0){
            stop(_("'%s' needs a '%s'"), rateChar, amtChar);
          }
        } else {
          if (durIx != -1){
            NumericVector dur = as<NumericVector>(input[durIx]);
            if (dur.size() != 1) stop(_("'%s' cannot be a vector"), durChar);
            if (dur[0] != 0){
              stop(_("'%s' needs a '%s'"), durChar, amtChar);
            }
          }
          rate = NumericVector(1);
          rate[0]=0.0;
        }
        NumericVector ii(1);
        if (iiIx != -1){
          NumericVector tmpNV = as<NumericVector>(input[iiIx]);
          if (tmpNV.size() != 1) stop(_("'%s' cannot be a vector"), iiChar);
          if (tmpNV[0] != 0.0){
            stop(_("'ii' needs a 'dose'/'amt'"));
          }
          if (Rf_inherits(tmpNV, "units")){
            ii = setUnits(tmpNV, as<std::string>(units[1]));
          } else {
            ii[0] = tmpNV[0];
          }
        } else {
          ii[0] = 0.0;
        }
        if (addl[0] > 0 && ii[0] <= 0){
          stop(_("dosing interval of zero makes no sense with multiple dose events"));
        }
        IntegerVector ss;// = 0;
        if (ssIx != -1){
          ss = as<IntegerVector>(input[ssIx]);
          if (ss.size() != 1) stop(_("'%s' cannot be a vector"), ssChar);
          if (ss[0] != 0){
            stop(_("non-zero '%s' needs a '%s'"), ssChar, amtChar);
          }
        }
        if (timeIx != -1) {
          if (rxIsNumInt(input[timeIx]) ||
              Rf_inherits(input[timeIx], "units")){
            NumericVector time = clone(as<NumericVector>(input[timeIx]));
            if (Rf_inherits(time, "units")){
              CharacterVector cls = clone(as<CharacterVector>(curEt.attr("class")));
              List e = clone(as<List>(cls.attr(".rxode2.lst")));
              CharacterVector units = e["units"];
              if (!CharacterVector::is_na(units["time"])){
                time = setUnits(time, as<std::string>(units["time"]));
              } else {
                time = setUnits(time, "");
              }
            }
            RObject ret = etUpdateObj(etAddTimes(time, id, cmt, turnOnShowCmt, as<List>(curEt)),
                                      doUpdateObj, inputSolve, turnOnId);
            return ret;
          } else if (rxIsCleanList(input[timeIx])){
            RObject ret = etUpdateObj(etAddWindow(as<List>(input[timeIx]), id, cmt, turnOnShowCmt, as<List>(curEt)),
                                      doUpdateObj, inputSolve, turnOnId);
            return ret;
          }
        }
      } else {
        bool doWindow=false;
        if (evidIx == -1 && !cmtNeg) evid[0]=1;
        else if (evidIx == -1 && cmtNeg){
          evid[0]=2;
          amt[0] = NA_REAL; // This is where the changed
        }
        ////////////////////////////////////////////////////////////////////////////////
        // Dose
        ////////////////////////////////////////////////////////////////////////////////
        if (addlIx != -1 && untilIx != -1){
          stop(_("can only specify '%s' or '%s', not both"), untilChar, addlChar);
        }
        if (addlIx != -1 && nbrIx != -1){
          stop(_("can only specify '%s' or '%s', not both"), addlChar, nbrChar);
        }
        if (nbrIx != -1 && untilIx != -1){
          stop(_("can only specify '%s' or '%s', not both"), nbrChar, untilChar);
        }
        bool doSampling = false;
        if (doSamplingIdx != -1){
          if (TYPEOF(input[doSamplingIdx]) == LGLSXP){
            LogicalVector tmpL = as<LogicalVector>(input[doSamplingIdx]);
            if (tmpL.size() != 1) stop(_("'%s' can only have one item"), doSamplingChar);
            doSampling = tmpL[0];
          } else {
            stop(_("'%s' must be logical"), doSamplingChar);
          }
        }
        NumericVector rate(1);
        NumericVector dur(1);
        if (rateIx != -1){
          if (durIx != -1){
            stop(_("cannot specify '%s' and '%s' for a dose, please pick one"),
                 durChar, rateChar);
          }
          NumericVector tmpNV = as<NumericVector>(input[rateIx]);
          if (tmpNV.size() != 1) stop(_("'%s' cannot be a vector"), rateChar);
          if (Rf_inherits(tmpNV, "units")){
            CharacterVector cls = clone(as<CharacterVector>(curEt.attr("class")));
            List e = clone(as<List>(cls.attr(".rxode2.lst")));
            CharacterVector units = e["units"];
            if (!CharacterVector::is_na(units[1]) && !CharacterVector::is_na(units[0])){
              if (rate[0] < 0){
                stop(_("-1 and -2 rates do not make sense with units"));
              }
              std::string rateUnit = as<std::string>(units[0]) + "/" + as<std::string>(units[1]);
              rate = setUnits(tmpNV,rateUnit);
            } else {
              stop(_("'%s' is cannot be converted and added to this table"), rateChar); //
            }
          } else {
            rate[0] = tmpNV[0];
          }
          if (ssInf){
            if (rate[0] != -1 && rate[0] <= 0.0){
              stop(_("steady state constant infusion dosing records must have '%s=-1' or positive rate"), rateChar);
            }
          }
          dur[0] = 0;
        } else {
          if (durIx != -1){
            NumericVector tmpNV = as<NumericVector>(input[durIx]);
            rate = NumericVector(1);
            rate[0] = 0;
            if (tmpNV.size() != 1) stop(_("'%s' cannot be a vector"), durChar);
            if (Rf_inherits(tmpNV, "units")){
              CharacterVector cls = clone(as<CharacterVector>(curEt.attr("class")));
              List e = clone(as<List>(cls.attr(".rxode2.lst")));
              CharacterVector units = e["units"];
              if (!CharacterVector::is_na(units[1])){
                std::string durUnit = as<std::string>(units[0]) + "/" + as<std::string>(units[1]);
                dur = setUnits(tmpNV,as<std::string>(units[1]));
                // While units are converted, the units that matter are the rate units.
              } else {
                stop(_("'%s' cannot be converted and added to this table"), durChar);
              }
            } else {
              dur = tmpNV[0];
            }
          } else {
            rate[0] = 0.0;
            dur[0] = 0.0;
          }
        }
        NumericVector ii(1);// =0.0;
        if (iiIx != -1){
          NumericVector tmpNV = as<NumericVector>(input[iiIx]);
          if (tmpNV.size() != 1) stop(_("'%s' cannot be a vector"), iiChar);
          if (Rf_inherits(tmpNV, "units")){
            ii = setUnits(tmpNV, as<std::string>(units[1]));
          } else {
            ii[0] = tmpNV[0];
          }
        } else {
          ii[0] = 0.0;
        }
        bool ssRateInf = false;
        IntegerVector ss;// = 0;
        if (ssIx != -1){
          ss = as<IntegerVector>(input[ssIx]);
          if (ss.size() != 1) stop(_("'%s' cannot be a vector"), ssChar);
          if (zeroAmt && ss[0] != 0){
            if (ss[0] == 1 && (rate[0] > 0 || rate[0] == -1)) {
              ssRateInf=true;
            } else {
              stop(_("non-zero '%s' needs a '%s'"), ssChar, amtChar);
            }
          }
        } else {
          ss = IntegerVector(1);
          ss[0] = 0;
        }
        NumericVector time;
        List timeList;
        if (timeIx != -1){
          if (rxIsCleanList(input[timeIx])){
            doWindow=true;
            timeList = input[timeIx];
            time = as<NumericVector>(timeList[0]);
          } else {
            time = as<NumericVector>(input[timeIx]);
          }
        } else {
          time = NumericVector(1);
          time[0] = 0;
        }
        IntegerVector addl(1);//=0;
        if (addlIx != -1){
          IntegerVector tmpIV = as<IntegerVector>(input[addlIx]);
          if (tmpIV.size() != 1) stop(_("'%s' cannot be a vector"), addlChar);
          addl[0] = tmpIV[0];
        } else if (nbrIx != -1){
          IntegerVector tmpIV = as<IntegerVector>(input[nbrIx]);
          if (tmpIV.size() != 1) stop(_("'%s' cannot be a vector"), nbrChar);
          if (tmpIV[0] < 1){
            stop(_("number of doses must be at least one ('%s': %d)"), nbrChar, addl[0]);
          }
          addl[0] = tmpIV[0]-1;
        } else if (untilIx != -1){
          // Need time for this
          NumericVector tmpNV = as<NumericVector>(input[untilIx]);
          NumericVector until(1);
          if (tmpNV.size() != 1) stop(_("'%s' cannot be a vector"), untilChar);
          if (ii[0] < 0){
            stop(_("'%s' can only be used with positive inter-dose intervals ('%s')"),
                 untilChar, iiChar);
          }
          if (Rf_inherits(tmpNV, "units")){
            until = setUnits(tmpNV, as<std::string>(units[1]));
          } else {
            until = tmpNV[0];
          }
          if (!doWindow || time.size() == 1){
            if (time.size() != 1){
              stop(_("'%s' does not make sense with multiple dosing times"),
                   untilChar);
            }
            double tmp = until[0] - time[0] - ii[0];
            if (tmp > 0){
              tmp = tmp/ii[0];
              double tmp2 = std::ceil(tmp);
              if (tmp2 == tmp){
                addl[0] = tmp + 1;
              } else {
                addl[0] = tmp2;
              }
            } else {
              warning(_("'time'+'ii' is greater than 'until', no additional doses added"));
              addl[0] = 0;
            }
          } else if (doWindow && time.size() == 2){
            double tmp = until[0] - time[1] - ii[0];
            if (tmp > 0){
              tmp = tmp/ii[0];
              double tmp2 = std::ceil(tmp);
              if (tmp2 == tmp){
                addl[0] = tmp + 1;
              } else {
                addl[0] = tmp2;
              }
            } else {
              addl[0] = 0;
            }
          } else {
            stop(_("dosing windows can only have 1-2 items in them"));
          }
        } else {
          addl[0]=0;
        }
        if (ii[0] > 0 && ss[0] == 0 && addl[0] == 0){
          if (doUpdateObj && ii[0] == 24){
            ii[0]=0;
          } else {
            Rf_warningcall(R_NilValue, "'%s' requires non zero additional doses ('%s') or steady state dosing ('%s': %f, '%s': %d; '%s': %d), reset '%s' to zero", iiChar, addlChar, iiChar, ii[0], ssChar, ss[0], addlChar, addl[0],
                           iiChar);
            ii[0]=0;
          }
        }
        if (ssInf && ss[0] != 1){
          stop(_("'%s' must be 1 when specifying a steady-state constant infusion"), ssChar);
        }
        if (ss[0] < 0 || ss[0] > 2){
          stop(_("'%s' must be 0, 1 or 2"), ssChar);
        } if (ss[0] > 0 && ii[0] <= 0){
          if (!ssInf && !ssRateInf){
            stop(_("'%s' required with '%s'"), iiChar, ssChar);
          }
        }
        if (ssInf && ii[0] > 0){
          stop(_("'%s' cannot be used with steady state constant infusion"), iiChar);
        }
        if (ss[0] > 1 && time.size() > 1){
          stop(_("steady state ('%s') is not supported with dosing windows"), ssChar);
        }
        if (addl[0] < 0){
          stop(_("additional doses must be positive ('%s'=%d)"), addlChar, addl[0]);
        }
        if (addl[0] > 0 && ii[0] <= 0){
          stop(_("additional doses require an inter-dose interval ('%s')"), iiChar);
        }
        // Defer ss errors to end to check for rate infusion steady state
        if (ssRateInf) {
          if (ii[0] != 0.0) {
            stop(_("for steady state infusions, you need %s=0, %s>0, %s=1, %s=0"),
                 iiChar, rateChar, ssChar, amtChar);
          }
        }
        List ret;
        if (doWindow){
          ret = etAddDose(time, cmt, amt[0], rate[0], ii[0], addl[0], evid[0], ss[0], dur[0],
                          id, turnOnShowCmt, doSampling, as<List>(curEt));
          for (int i = 1; i < timeList.size(); i++){
            if (rxIsNumInt(timeList[i]) ||
                Rf_inherits(timeList[i],"units")){
              time = as<NumericVector>(timeList[i]);
              if (time.size() > 2){
                stop(_("dosing time window lists can have 1-2 numeric entries in them"));
              }
              ret = etAddDose(time, cmt, amt[0], rate[0], ii[0], addl[0], evid[0], ss[0], dur[0],
                              id, turnOnShowCmt, doSampling, ret);
            } else {
              stop(_("dosing window list needs to be numeric values only"));
            }
          }
        } else {
          NumericVector time0(1);
          time0[0] = time[0];
          ret = etAddDose(time0, cmt, amt[0], rate[0], ii[0], addl[0], evid[0], ss[0], dur[0],
                          id, turnOnShowCmt, doSampling, as<List>(curEt));
          for (int i = 1; i < time.size(); i++){
            time0[0] = time[i];
            ret = etAddDose(time0, cmt, amt[0], rate[0], ii[0], addl[0], evid[0], ss[0], dur[0],
                            id, turnOnShowCmt, doSampling, ret);
          }
        }
        return etUpdateObj(ret, doUpdateObj, inputSolve, turnOnId);
      }
    }
  } else {
    // This is a new event table.
    // Get the amtUnit
    CharacterVector units(2);
    int foundArgs = 0;
    if (amtUnitIx == -1){
      units[0] = NA_STRING;
    } else  {
      CharacterVector tmpS = as<CharacterVector>(input[amtUnitIx]);
      if (tmpS.size() != 1) stop(_("'%s' cannot be a vector"), amtUnitChar);
      units[0] = tmpS[0];
      foundArgs++;
    }
    if (timeUnitIx == -1){
      units[1] = NA_STRING;
    } else  {
      CharacterVector tmpS = as<CharacterVector>(input[timeUnitIx]);
      if (tmpS.size() != 1) stop(_("'%s' cannot be a vector"), timeUnitChar);
      units[1] = tmpS[0];
      foundArgs++;
    }
    units.attr("names") = CharacterVector::create("dosing", "time");
    List et = etEmpty(units);
    if (input.size() == foundArgs){
      return et;
    } else {
      return et_(input, et);
    }
  }
  if (doRet){
    return etUpdateObj(as<List>(curEt),doUpdateObj, inputSolve,
                       turnOnId);
  }
  if (input.size() == 1 && Rf_inherits(input[0], "data.frame")){
    return etImportEventTable(as<List>(input[0]), true);
  }
  stop(_("cannot figure out what type of 'EventTable' you are trying to create"));
  // Should never get here...
  List ret(0);
  return ret;
}

Function getRxFn(std::string name);

// Sequence event tables
//[[Rcpp::export]]
List etSeq_(List ets, int handleSamples=0, int waitType = 0,
            double defaultIi = 0,bool rbind = false,
            int uniqueId=0,
            int reserveLen=0, bool needSort=true,
            CharacterVector newUnits = CharacterVector::create(),
            LogicalVector newShow = LogicalVector::create(),
            bool isCmtIntIn = false) {
  double timeDelta = 0;
  double maxTime = 0;
  double lastDose = 0;
  double lastIi = 0;
  double trueLastIi =0;
  std::vector<int> id;
  std::vector<double> time;
  std::vector<double> low;
  std::vector<double> high;
  std::vector<double> ii;
  std::vector<double> amt;
  std::vector<int> evid;
  std::vector<int> addl;
  std::vector<int> idxEts;
  std::vector<int> idxEt;
  std::vector<int> idx;
  std::vector<int> IDs;

  NumericVector curTime, curLow, curHigh, curIi,curAmt;
  IntegerVector curEvid, curAddl, curId;

  int i, j, k=0, nobs = 0, ndose=0;
  List et;
  bool isCmtInt=false;
  CharacterVector units;
  LogicalVector show;
  bool gotUnits = false;
  List e;
  int lastId = -1;
  int thisId = 0;

  CharacterVector cls;

  if (reserveLen != 0){
    // preallocate vector
    id.reserve(reserveLen);
    time.reserve(reserveLen);
    low.reserve(reserveLen);
    high.reserve(reserveLen);
    ii.reserve(reserveLen);
    amt.reserve(reserveLen);
    evid.reserve(reserveLen);
    addl.reserve(reserveLen);
    idxEts.reserve(reserveLen);
    idxEt.reserve(reserveLen);
    idx.reserve(reserveLen);
    if (newUnits.size() == 2){
      units = newUnits;
      show = newShow;
      isCmtInt = isCmtIntIn;
      gotUnits=true;
    }
  }
  bool firstDoseOfEt=true;
  for (i = 0 ;i < ets.size(); i++){
    lastId=-1;// New id of each event table will trigger id change for unique
    if (_rxIsEt(ets[i])) {
      maxTime = 0;
      List et = ets[i];
      cls = as<CharacterVector>(et.attr("class"));
      e = cls.attr(".rxode2.lst");
      if (!gotUnits){
        units = e["units"];
        show = e["show"];
        if (rxIsInt(et["cmt"])){
          isCmtInt = true;
        }
        gotUnits=true;
      } else {
        if (isCmtInt && !rxIsInt(et["cmt"])){
          stop(_("cannot have event tables with integer and character 'cmt'"));
        }
        if (!isCmtInt && rxIsInt(et["cmt"])){
          stop(_("cannot have event tables with integer and character 'cmt'"));
        }
        LogicalVector newShow = e["show"];
        for (j = show.size(); j--;){
          show[j] = show[j] || newShow[j];
        }
      }
      //
      // Load time, low, high and amt and convert units if needed.
      curTime = et["time"];
      if (Rf_inherits(curTime, "units")){
        if (!CharacterVector::is_na(units["time"])){
          curTime = setUnits(curTime, as<std::string>(units["time"]));
        } else {
          curTime = setUnits(curTime, "");
        }
      }
      curLow=et["low"];
      if (Rf_inherits(curLow, "units")){
        if (!CharacterVector::is_na(units["time"])){
          curLow = setUnits(curLow, as<std::string>(units["time"]));
        } else {
          curLow = setUnits(curLow, "");
        }
      }
      curHigh=et["high"];
      if (Rf_inherits(curHigh, "units")){
        if (!CharacterVector::is_na(units["time"])){
          curHigh = setUnits(curHigh, as<std::string>(units["time"]));
        } else {
          curHigh = setUnits(curHigh, "");
        }
      }
      curIi=et["ii"];
      if (Rf_inherits(curIi, "units")){
        if (!CharacterVector::is_na(units["time"])){
          curIi = setUnits(curIi, as<std::string>(units["time"]));
        } else {
          curIi = setUnits(curIi, "");
        }
      }
      curAmt = et["amt"];
      if (Rf_inherits(curAmt, "units")){
        if (!CharacterVector::is_na(units[0])){
          curAmt = setUnits(curAmt, as<std::string>(units[0]));
        } else {
          curAmt = setUnits(curAmt, "");
        }
      }
      curId = et["id"];
      curEvid = et["evid"];
      curAddl = et["addl"];
      firstDoseOfEt = true;
      for (j = 0; j < curTime.size(); j++){
        if (handleSamples == 0 && curEvid[j]== 0) continue;
        if (curEvid[j] == 0) nobs++;
        else ndose++;
        if (uniqueId==1){
          if (lastId != curId[j]){
            thisId++;
            lastId = curId[j];
            IDs.push_back(thisId);
          }
          id.push_back(thisId);
        } else {
          id.push_back(curId[j]);
          if (std::find(IDs.begin(), IDs.end(), curId[j]) == IDs.end()){
            IDs.push_back(curId[j]);
          }
        }
        addl.push_back(curAddl[j]);
        if (!ISNA(curHigh[j])) {
          if (curAddl[j] > 0){
            if (i != 0 && trueLastIi == 0 && firstDoseOfEt && curTime[j] < curIi[j]){
              maxTime += curIi[j];
              timeDelta += curIi[j];
            }
            lastIi = curIi[j];
            trueLastIi=lastIi;
            lastDose = curHigh[j] + curAddl[j]*curIi[j];
            double tmp = curHigh[j] + (curAddl[j]+1)*curIi[j];
            if (tmp > maxTime) maxTime = tmp;
            firstDoseOfEt = false;
          } else if (curEvid[j] != 0 && curEvid[j] != 2 && curEvid[j] != 3) {
            if (!rbind && i != 0 && trueLastIi == 0 && firstDoseOfEt && curTime[j] < defaultIi){
              Rf_warningcall(R_NilValue, "assumed a dose interval of %.1f between event tables; use 'ii' to adjust", defaultIi);
              maxTime += defaultIi;
              timeDelta += defaultIi;
            }
            lastIi = (curHigh[j] - lastDose);
            trueLastIi=0;
            maxTime = curHigh[j] + (curHigh[j] - lastDose); //Use last interval
            lastDose = curHigh[j];
            firstDoseOfEt = false;
          } else {
            if (curHigh[j] > maxTime) maxTime = curHigh[j];
          }
          high.push_back(curHigh[j]+timeDelta);
          time.push_back(curTime[j]+timeDelta);
          low.push_back(curLow[j]+timeDelta);
        } else {
          if (curAddl[j] > 0){
            if (i != 0 && trueLastIi == 0 && firstDoseOfEt && curTime[j] < curIi[j]){
              maxTime += curIi[j];
              timeDelta += curIi[j];
            }
            firstDoseOfEt = false;
            lastIi = curIi[j];
            trueLastIi=lastIi;
            lastDose = curTime[j] + curAddl[j] * curIi[j];
            double tmp = curTime[j] + (curAddl[j]+1) * curIi[j];
            if (tmp > maxTime) maxTime = tmp;
          } else if (curEvid[j] != 0 && curEvid[j] != 2 && curEvid[j] != 3){
            if (!rbind && i != 0 && trueLastIi == 0 && firstDoseOfEt && curTime[j] < defaultIi){
              Rf_warningcall(R_NilValue, "assumed a dose interval of %.1f between event tables; use 'ii' to adjust", defaultIi);
              maxTime += defaultIi;
              timeDelta += defaultIi;
            }
            lastIi = (curTime[j] - lastDose);
            trueLastIi=0;
            lastDose = curTime[j];
            maxTime = curTime[j] + (curTime[j] - lastDose); //Use last interval
            firstDoseOfEt = false;
          } else {
            if (curTime[j] > maxTime) maxTime = curTime[j];
          }
          high.push_back(NA_REAL);
          time.push_back(curTime[j]+timeDelta);
          low.push_back(NA_REAL);
        }
        ii.push_back(curIi[j]);
        amt.push_back(curAmt[j]);
        evid.push_back(curEvid[j]);
        idxEts.push_back(i);
        idxEt.push_back(j);
        idx.push_back(k++);
      }
      if (rbind){
        maxTime = 0;
      }
      timeDelta += maxTime;
    } else if (rxIsNumInt(ets[i])) {
      if (Rf_inherits(ets[i], "units")) {
        maxTime = asDouble(setUnits(ets[i], as<std::string>(units["time"])), "units[\"time\"]");
      } else {
        maxTime = asDouble(ets[i], "ets[i]");
      }
      if (waitType == 0 && maxTime > lastIi) {
        maxTime -= lastIi;
      }
      timeDelta += maxTime;
    }
  }
  if (needSort){
    IntegerVector ivId=wrap(id);
    NumericVector nvTime=wrap(time);
    IntegerVector ivEvid=wrap(evid);
    Function order3 = getRxFn(".order3");
    IntegerVector ord;
    ord = order3(ivId, nvTime, ivEvid);
    ord = ord - 1;
    idx = as<std::vector<int>>(ord);
  }
  if (!gotUnits){
    stop(_("no events table found for 'seq'/'rep'/'rbind'/'c'"));
  }
  List lst = etEmpty(units);
  // nme[0] = "id";
  lst[0] = IntegerVector(id.size());

  // nme[1] = "time";
  lst[2] = NumericVector(id.size());

  // nme[2] = "low";
  lst[1] = NumericVector(id.size());

  // nme[3] = "high";
  lst[3] = NumericVector(id.size());

  // nme[4] = "cmt";
  if (!isCmtInt){
    lst[4] = CharacterVector(id.size());
  } else {
    lst[4] = IntegerVector(id.size());
  }
  // nme[5] = "amt";
  lst[5] = NumericVector(id.size());

  // nme[6] = "rate";
  lst[6] = NumericVector(id.size());

  // nme[7] = "ii";
  lst[7] = NumericVector(id.size());

  // nme[8] = "addl";
  lst[8] = IntegerVector(id.size());

  // nme[9] = "evid";
  lst[9] = IntegerVector(id.size());

  // nme[10] = "ss";
  lst[10] = IntegerVector(id.size());

  // nme[11] = "dur"
  lst[11] = NumericVector(id.size());

  IntegerVector tmpI, tmpI2;
  NumericVector tmpN, tmpN2;
  CharacterVector tmpC, tmpC2;
  // Now fill everything in.
  for (i = idx.size(); i--;){
    et = ets[idxEts[idx[i]]];
    j = idxEt[idx[i]];

    tmpI = as<IntegerVector>(lst[0]); // id
    tmpI[i] = id[idx[i]];

    tmpN = as<NumericVector>(lst[1]); // low
    tmpN[i] = low[idx[i]];

    tmpN = as<NumericVector>(lst[2]); // time
    tmpN[i] = time[idx[i]];

    tmpN = as<NumericVector>(lst[3]); // high
    tmpN[i] = high[idx[i]];
    // 4 is cmt
    if (!isCmtInt){
      tmpC = as<CharacterVector>(lst[4]);
      tmpC2 = as<CharacterVector>(et[4]);
      tmpC[i] = tmpC2[j];
    } else {
      tmpI = as<IntegerVector>(lst[4]);
      tmpI2 = as<IntegerVector>(et[4]);
      tmpI[i] = tmpI2[j];
    }

    tmpN = as<NumericVector>(lst[5]); //  amt
    tmpN[i] = amt[idx[i]];

    // 6 is rate
    tmpN = as<NumericVector>(lst[6]); // rate
    tmpN2 = as<NumericVector>(et[6]);
    tmpN[i] = tmpN2[j];

    tmpN = as<NumericVector>(lst[7]); // ii
    tmpN[i] = ii[idx[i]];

    // 8 is addl
    tmpI = as<IntegerVector>(lst[8]);
    tmpI[i] = addl[idx[i]];

    tmpI = as<IntegerVector>(lst[9]); // evid
    tmpI[i] = evid[idx[i]];

    tmpI = as<IntegerVector>(lst[10]); // rate
    tmpI2 = as<IntegerVector>(et[10]);
    tmpI[i] = tmpI2[j];

    tmpN = as<NumericVector>(lst[11]); // dur
    tmpN2 = as<NumericVector>(et[11]);
    tmpN[i] = tmpN2[j];
  }
  cls = lst.attr("class");
  e = cls.attr(".rxode2.lst");
  e["ndose"] = ndose;
  e["nobs"]  = nobs;
  e["show"]  = show;
  if (IDs.size() > 1){
    show["id"] = true;
    e["IDs"] = wrap(IDs);
  } else {
    show["id"] = false;
    e["IDs"] = wrap(IDs);
  }
  cls.attr(".rxode2.lst") = e;
  lst.attr("class") = cls;
  lst.attr("row.names") = IntegerVector::create(NA_INTEGER, -(nobs+ndose));
  return lst;
}

// Sequence event tables
//[[Rcpp::export]]
List etRep_(RObject curEt, int times, NumericVector wait, IntegerVector ids, int handleSamples, int waitType,
            double ii){
  if (wait.size() != 1){
    stop(_("'wait' cannot be a vector"));
  }
  CharacterVector cls = as<CharacterVector>(curEt.attr("class"));
  List e = cls.attr(".rxode2.lst");
  CharacterVector units = e["units"];
  if (Rf_inherits(wait, "units")){
    wait = setUnits(wait, as<std::string>(units["time"]));
  }
  int len = asInt(e["nobs"], "e[\"nobs\"]") +
    asInt(e["ndose"], "e[\"ndose\"]");
  IntegerVector IDs = e["IDs"];
  List seqLst(times*2);
  for (int i = times; i--;){
    seqLst[i*2] = curEt;
    seqLst[i*2+1] = wait;
  }
  return etSeq_(seqLst, handleSamples, waitType, ii, false,0,
                len*times, (IDs.size() != 1), e["units"],
                e["show"], rxIsInt(curEt));
}
