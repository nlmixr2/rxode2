// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: nil; -*-
//#undef NDEBUG
#define USE_FC_LEN_T
#define STRICT_R_HEADERS

#include <Rcpp.h>
#include <algorithm>
#include "../inst/include/rxode2parse.h"
#include "timsort.h"
#include "needSortDefines.h"
#define SORT gfx::timsort

#define min2( a , b )  ( (a) < (b) ? (a) : (b) )
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )


#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2parse", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

using namespace Rcpp;
#define rxIsNumIntLgl rx2parseIsNumIntLgl
#define rxIsFactor rx2parseIsFactor
#define rxIsChar rx2parseIsChar
#define rxIsInt rx2parseIsInt
#define rxIsNumInt rx2parseIsNumInt
#define asIv rx2parseAsIv
#define asBool rx2parseAsBool
#define asCv rx2parseAsCv

extern int fastFactorDataHasNa;

SEXP convertId_(SEXP x);

static inline bool rxIsNumInt(RObject obj) {
  int type = obj.sexp_type();
  if (type == REALSXP || type == 13) {
    return (!obj.hasAttribute("dim"));
  }
  return false;
}

static inline CharacterVector asCv(SEXP in, const char *what) {
  int type = TYPEOF(in);
  if (type != STRSXP){
    REprintf("'%s'\n", what);
    Rcpp::print(in);
#ifdef asError
    REprintf(_("'%s' needs to be a vector of strings"), what);
    abort();
#else
    Rcpp::stop(_("'%s' needs to be a vector of strings"), what);
#endif
  }
  return as<CharacterVector>(in);
}


static inline bool rxIsNumIntLgl(RObject obj) {
  int type = obj.sexp_type();
  if (type == REALSXP || type == INTSXP || type == LGLSXP) {
    return (!obj.hasAttribute("dim"));
  }
  return false;
}

static inline bool rxIsFactor(RObject obj) {
  return !Rf_isNull(Rf_getAttrib(as<SEXP>(obj), R_LevelsSymbol));
}

static inline bool rxIsChar(RObject obj) {
  int type = obj.sexp_type();
  if (type == STRSXP) {
    return (!obj.hasAttribute("dim"));
  }
  return false;
}

static inline bool rxIsInt(RObject obj) {
  if (obj.sexp_type() == 13) {
    return (!obj.hasAttribute("dim"));
  }
  return false;
}

static inline IntegerVector asIv(SEXP in, const char* what) {
  int type = TYPEOF(in);
  if (type != INTSXP && type != REALSXP) {
    REprintf("'%s':\n", what);
    Rcpp::print(in);
#ifdef asError
    REprintf(_("'%s' needs to be a integer vector"), what);
    abort();
#else
    Rcpp::stop(_("'%s' needs to be a integer vector"), what);
#endif
  }
  return as<IntegerVector>(in);
}

static inline bool asBool(SEXP in, const char *what) {
  int type = TYPEOF(in);
  if (Rf_length(in) != 1 && type != LGLSXP){
    REprintf("'%s'\n", what);
    Rcpp::print(in);
#ifdef asError
    REprintf(_("'%s' needs to be a boolean"), what);
    abort();
#else
    Rcpp::stop(_("'%s' needs to be a boolean"), what);
#endif
  }
  return as<bool>(in);
}

Function getRxFn(std::string name);
Environment rxode2env();

Environment dataTable;
bool getForder_b=false;
Function getRxFn(std::string name);
bool dtForder = false;
bool forderForceBase_ = false;

//' Force using base order for rxode2 radix sorting
//'
//' @param forceBase boolean indicating if rxode2 should use R's
//'   [order()] for radix sorting instead of
//'   `data.table`'s parallel radix sorting.
//'
//' @return NILL; called for side effects
//'
//' @examples
//' \donttest{
//' forderForceBase(TRUE) # Use base `order` for rxode2 sorts
//' forderForceBase(FALSE) # Use `data.table` for rxode2 sorts
//' }
//' @export
//' @keywords internal
//[[Rcpp::export]]
RObject forderForceBase(bool forceBase = false){
  forderForceBase_=forceBase;
  return R_NilValue;
}

IntegerVector convertDvid_(SEXP inCmt, int maxDvid=0){
  IntegerVector id = asIv(inCmt, "inCmt");
  IntegerVector udvid = sort_unique(id);
  int mDvid = udvid[udvid.size()-1];
  if (mDvid > maxDvid) {
    return Rcpp::match(id, udvid);
  }
  return id;
}
extern "C" SEXP getForder(void) {
  if (!getForder_b){
    Function fn = getRxFn(".getDTEnv");
    dataTable = fn();
    getForder_b=true;
  }
  if (!forderForceBase_ && dataTable.exists("forder")){
    dtForder=true;
    return wrap(dataTable["forder"]);
  }
  Environment b=Rcpp::Environment::base_namespace();
  dtForder=false;
  return wrap(b["order"]);
}

Function getChin() {
  if (!getForder_b){
    Function fn = getRxFn(".getDTEnv");
    dataTable = fn();
    getForder_b=true;
  }
  if (!forderForceBase_ && dataTable.exists("%chin%")){
    return dataTable["%chin%"];
  }
  Environment b=Rcpp::Environment::base_namespace();
  return b["%in%"];
}

extern "C" SEXP chin(SEXP x, SEXP table) {
  Function chin_ = getChin();
  return chin_(x, table);
}

extern "C" int useForder(void){
  return (int)(getForder_b);
}

IntegerVector toCmt(RObject inCmt, CharacterVector& state, const bool isDvid,
                    const int stateSize, const int sensSize, IntegerVector& curDvid,
                    const IntegerVector& inId, const CharacterVector& idLvl){
  RObject cmtInfo = R_NilValue;
  List extraCmt;
  if (rxIsNumIntLgl(inCmt)){
    if (rxIsFactor(inCmt)){
      CharacterVector lvl = Rf_getAttrib(as<SEXP>(inCmt), R_LevelsSymbol);
      IntegerVector lvlI(lvl.size());
      int i, j, k=0;
      std::string curLvl, curState,negSub;
      bool foundState = false;
      bool isNeg = false;
      for (i = 0; i < lvlI.size(); i++){
        curLvl = as<std::string>(lvl[i]);
        negSub = curLvl.substr(0,1);
        if (negSub == "-"){
          isNeg = true;
          curLvl = curLvl.substr(1, std::string::npos);
        } else {
          isNeg = false;
        }
        foundState=false;
        for (j = state.size(); j--;){
          curState = as<std::string>(state[j]);
          if (curState == curLvl){
            if (isNeg){
              lvlI[i] = -j-1;
            } else {
              lvlI[i] = j+1;
            }
            foundState = true;
            break;
          }
        }
        if (!foundState){
          if (curLvl == "(default)" || curLvl == "(obs)"){
            lvlI[i] = 1;
          } else {
            k++;
            if (isNeg){
              stop(_("negative compartments on non-ode 'cmt' (%s) does not make sense (id: %s, row: %d)"),
                   curLvl.c_str(), CHAR(idLvl[((inId.size() == 0) ? 1 : inId[i])-1]), i+1);
            } else {
              List tmpList(extraCmt.size()+1);
              for (int i = extraCmt.size(); i--;) tmpList[i] = extraCmt[i];
              tmpList[extraCmt.size()]= curLvl;
              extraCmt = tmpList;
              lvlI[i] = state.size() + k;
            }
          }
        }
      }
      IntegerVector cmtIn = IntegerVector(inCmt);
      IntegerVector ret(cmtIn.size());
      for (j=ret.size(); j--;){
        if (IntegerVector::is_na(cmtIn[j])){
          ret[j] = NA_INTEGER;
        } else {
          ret[j] = lvlI[cmtIn[j]-1];
        }
      }
      CharacterVector newCmt(state.size()+extraCmt.size());
      for (int j = state.size(); j--;) newCmt[j]=state[j];
      for (int j = extraCmt.size(); j--;)
        newCmt[j+state.size()] = as<std::string>(extraCmt[j]);
      ret.attr("cmtNames") = newCmt;
      return ret;
    } else {
      if (isDvid){
        // This converts DVID to cmt; Things that don't match become -9999
        Environment rx = rxode2env();
        IntegerVector in = convertDvid_(inCmt, curDvid.length());
        IntegerVector out(in.size());
        IntegerVector conv = curDvid;
        std::vector<int> warnDvid;
        std::vector<int> warnConvertDvid;
        std::string warnC = "'dvid' were not numbered 1, 2, etc\nThey were converted from->to:";
        IntegerVector cmtIn = IntegerVector(inCmt);
        int curConv;
        for (int i = in.size(); i--;){
          if (in[i] != cmtIn[i]){
            if (std::find(warnConvertDvid.begin(), warnConvertDvid.end(), in[i]) == warnConvertDvid.end()){
              warnConvertDvid.push_back(in[i]);
              warnC = warnC + " " + std::to_string(cmtIn[i]) + "->" + std::to_string(in[i]);
            }
          }
          if (in[i] > 0 && in[i] <= conv.size()){
            curConv= conv[in[i]-1];
            if (curConv > 0){
              out[i] = curConv;
            } else {
              out[i] = curConv +state.size()+1;
            }
          } else {
            if (std::find(warnDvid.begin(), warnDvid.end(), in[i]) == warnDvid.end()){
              warnDvid.push_back(in[i]);
            }
            out[i] = -9999;
          }
        }
        if (warnDvid.size() > 1){
          std::string warn = "Undefined 'dvid' integer values in data: ";
          SORT(warnDvid.begin(), warnDvid.end());
          for (int i = 0; i < (int)(warnDvid.size()-1); i++){
            warn = warn + std::to_string(warnDvid[i]) + ", ";
          }
          warn = warn + std::to_string(warnDvid[warnDvid.size()-1]);
          Rf_warningcall(R_NilValue, "%s", warn.c_str());
        }
        if (warnConvertDvid.size() > 0){
          Rf_warningcall(R_NilValue, "%s", warnC.c_str());
        }
        return out;
      } else {
        IntegerVector in = as<IntegerVector>(inCmt);
        IntegerVector out(in.size());
        int baseSize = stateSize - sensSize;
        // Sensitivity equations are ignored in CMT data items.
        for (int i = in.size(); i--;){
          if (in[i] == NA_INTEGER) {
            out[i] = in[i];
          } else if (in[i] > 0 && in[i] <= baseSize){
            out[i] = in[i];
          } else if (in[i] > 0) {
            out[i] = in[i]+sensSize;
          } else if (in[i] < 0 && in[i] >= -baseSize){
            out[i] = in[i];
          } else if (in[i] < 0){
            out[i] = in[i] - sensSize;
          } else {
            out[i] = 0;
          }
        }
        return out;
      }
    }
  } else if (rxIsChar(inCmt)) {
    CharacterVector iCmt = as<CharacterVector>(inCmt);
    std::vector<int> newCmt;
    newCmt.reserve(iCmt.size());
    std::string strCmt, negSub;
    bool foundState=false;
    bool isNeg = false;
    int i, j, k = 0;
    for (i = 0; i < iCmt.size(); i++){
      strCmt = as<std::string>(iCmt[i]);
      negSub = strCmt.substr(0,1);
      if (negSub == "-"){
        isNeg = true;
        strCmt = strCmt.substr(1, std::string::npos);
      } else {
        isNeg = false;
      }
      foundState=false;
      if (strCmt == "(default)" || strCmt == "(obs)" || CharacterVector::is_na(iCmt[i])){
        foundState=true;
        newCmt.push_back(1);
      } else {
        for (j = state.size(); j--;){
          if (as<std::string>(state[j]) == strCmt){
            foundState = true;
            if (isNeg){
              newCmt.push_back(-j-1);
            } else {
              newCmt.push_back(j+1);
            }
            break;
          }
        }
        if (!foundState){
          for (j = k; j--;){
            CharacterVector cur = extraCmt[j];
            if (as<std::string>(cur) == strCmt){
              foundState = true;
              if (isNeg){
                stop(_("negative compartments on non-ode 'cmt' (%s) does not make sense (id: %s row: %d)"), strCmt.c_str(),
                     CHAR(idLvl[((inId.size() == 0) ? 1 : inId[i])-1]), i+1);
              } else {
                newCmt.push_back(state.size()+j+1);
              }
              break;
            }
          }
          if (!foundState){
            if (isNeg){
              stop(_("negative compartments on non-ode 'cmt' (%s) does not make sense (id: %s row: %d)"), strCmt.c_str(),
                   CHAR(idLvl[((inId.size() == 0) ? 1 : inId[i])-1]), i+1);
            } else {
              List tmpList(extraCmt.size()+1);
              for (int i = extraCmt.size(); i--;) tmpList[i] = extraCmt[i];
              extraCmt = tmpList;
              newCmt.push_back(state.size()+k+1);
              extraCmt[k++] = strCmt;
            }
          }
        }
      }
      CharacterVector newCmt(state.size()+extraCmt.size());
      for (int j = state.size(); j--;) newCmt[j]=state[j];
      for (int j = extraCmt.size(); j--;) newCmt[j+state.size()] = as<std::string>(extraCmt[j]);
      cmtInfo = as<RObject>(newCmt);
    }
    IntegerVector ret = wrap(newCmt);
    ret.attr("cmtNames") = cmtInfo;
    return ret;
  }
  stop(_("should not reach here"));
  return IntegerVector::create(0);
}

bool _ini0=true;
//' Set Initial conditions to time zero instead of the first observed/dosed time
//'
//' @param ini0 When `TRUE` (default), set initial conditions to time
//'   zero. Otherwise the initial conditions are the first observed
//'   time.
//'
//' @return the boolean ini0, though this is called for its side effects
//'
//' @export
//[[Rcpp::export]]
bool rxSetIni0(bool ini0 = true) {
  _ini0=ini0;
  return _ini0;
}

IntegerVector convertMethod(RObject method) {
  IntegerVector oldEvid;
  if (rxIsChar(method)){
    CharacterVector tmp = asCv(method, "method");
    oldEvid = IntegerVector(tmp.size());
    for (int jj = tmp.size(); jj--;){
      std::string cur = (as<std::string>(tmp[jj])).substr(0,1);
      // (1 = replace, 2 = add, 3 = multiply)
      if (cur == "A" || cur == "a" || cur == "2"){
        oldEvid[jj] = 1;
      } else if (cur == "m" || cur == "M" || cur == "3"){
        oldEvid[jj] = 6;
      } else if (cur == "r" || cur == "R" || cur == "1"){
        oldEvid[jj] = 5;
      } else {
        stop(_("unknown method: '%s'"), (as<std::string>(tmp[jj])).c_str());
      }
    }
  } else if (Rf_inherits(method, "factor")){
    IntegerVector tmp = asIv(method, "method");
    oldEvid = IntegerVector(tmp.size());
    CharacterVector lvl = tmp.attr("levels");
    IntegerVector trans(lvl.size());
    for (int jj = lvl.size(); jj--;){
      std::string cur = (as<std::string>(lvl[jj])).substr(0,1);
      if (cur == "A" || cur == "a" || cur == "2"){
        trans[jj] = 1;
      } else if (cur == "m" || cur == "M" || cur == "3"){
        trans[jj] = 6;
      } else if (cur == "r" || cur == "R" || cur == "1"){
        trans[jj] = 5;
      } else {
        stop(_("unknown method: '%s'"), (as<std::string>(lvl[jj])).c_str());
      }
    }
    for (int jj = tmp.size(); jj--;){
      oldEvid[jj] = trans[tmp[jj]-1];
    }
  } else if (rxIsNumInt(method)){
    IntegerVector tmp = as<IntegerVector>(method);
    oldEvid = IntegerVector(tmp.size());
    for (int jj = tmp.size(); jj--;){
      // (1 = replace, 2 = add, 3 = multiply)
      if (tmp[jj] == 1.){
        oldEvid[jj] = 5;
      } else if (tmp[jj] == 2.){
        oldEvid[jj] = 1;
      } else if (tmp[jj] == 3.){
        oldEvid[jj] = 6;
      }
    }
  }
  return oldEvid;
}

extern "C" SEXP _rxode2parse_convertId_(SEXP id);

bool warnedNeg=false;
bool evid2isObs=true;

//[[Rcpp::export]]
RObject etTransEvidIsObs(SEXP isObsSexp) {
  evid2isObs=INTEGER(isObsSexp)[0];
  return R_NilValue;
}
bool rxode2parseIsIntegerish(SEXP in) {
  Environment rx = rxode2env();
  Function isIntegerish = rx[".isIntegerish"];
  return as<bool>(isIntegerish(in));
}

RObject etTranGetAttrKeep(SEXP in) {
  RObject cur = as<RObject>(in);
  std::vector<std::string> attr = cur.attributeNames();
  if (cur.hasAttribute("levels")) {
    List ret(attr.size()-1);
    CharacterVector retN(attr.size()-1);
    unsigned int j = 0;
    for (unsigned int i = 0; i < attr.size(); ++i) {
      if (attr[i] != "levels") {
        retN[j] = attr[i];
        ret[j] = cur.attr(attr[i]);
        j++;
      }
    }
    ret.attr("names") = retN;
    return as<RObject>(ret);
  }
  List ret(attr.size());
  CharacterVector retN(attr.size());
  for (unsigned int i = 0; i < attr.size(); ++i) {
    retN[i] = attr[i];
    ret[i] = cur.attr(attr[i]);
  }
  ret.attr("names") = retN;
  return as<RObject>(ret);
}

List rxModelVars_(const RObject &obj); // model variables section
//' Event translation for rxode2
//'
//' @param inData Data frame to translate
//'
//' @param obj object where model variables can be extracted from
//'
//' @param addCmt Add compartment to data frame (default `FALSE`).
//'
//' @param dropUnits Boolean to drop the units (default `FALSE`).
//'
//' @param allTimeVar Treat all covariates as if they were time-varying
//'
//' @param keepDosingOnly keep the individuals who only have dosing records and any
//'   trailing dosing records after the last observation.
//'
//' @param combineDvid is a boolean indicating if rxode2 will use `DVID` on observation
//'     records to change the `cmt` value; Useful for multiple-endpoint nlmixr models.  By default
//'     this is determined by `option("rxode2.combine.dvid")` and if the option has not been set,
//'     this is `TRUE`. This typically does not affect rxode2 simulations.
//'
//' @param keep This is a named vector of items you want to keep in the final rxode2 dataset.
//'     For added rxode2 event records (if seen), last observation carried forward will be used.
//'
//' @param addlKeepsCov This determines if the additional dosing items
//'   repeats the dose only (`FALSE`) or keeps the covariates at the
//'   record of the dose (`TRUE`)
//'
//' @param addlDropSs When there are steady state doses with an `addl`
//'   specification the steady state flag is dropped with repeated
//'   doses (when `TRUE`) or retained (when `FALSE`)
//'
//' @param ssAtDoseTime Boolean that when `TRUE` back calculates the
//'   steady concentration at the actual time of dose, otherwise when
//'   `FALSE` the doses are shifted
//'
//' @inheritParams rxSolve
//'
//' @return Object for solving in rxode2
//'
//' @keywords internal
//'
//' @export
//[[Rcpp::export]]
List etTrans(List inData, const RObject &obj, bool addCmt=false,
             bool dropUnits=false, bool allTimeVar=false,
             bool keepDosingOnly=false, Nullable<LogicalVector> combineDvid=R_NilValue,
             CharacterVector keep = CharacterVector(0),
             bool addlKeepsCov=false,
             bool addlDropSs = true,
             bool ssAtDoseTime=true,
             Nullable<List> iCov = R_NilValue) {
#ifdef rxSolveT
  clock_t _lastT0 = clock();
#endif
  List mv = rxModelVars_(obj);
  bool hasIcov = false;
  List iCov_;
  if (!iCov.isNull()){
    iCov_ = as<List>(iCov);
    hasIcov = true;
  }
  Environment rx = rxode2env();
  bool combineDvidB = false;
  Environment b=Rcpp::Environment::base_namespace();
  if (!combineDvid.isNull()){
    combineDvidB = (as<LogicalVector>(combineDvid))[1];
  } else {
    Function getOption = b["getOption"];
    combineDvidB = as<bool>(getOption("rxode2.combine.dvid", true));
  }
  IntegerVector curDvid = clone(as<IntegerVector>(mv[RxMv_dvid]));
  IntegerVector curAlag = clone(as<IntegerVector>(mv[RxMv_alag]));
  CharacterVector trans = mv[RxMv_trans];
  if (Rf_inherits(inData,"rxEtTran")){
    CharacterVector cls = Rf_getAttrib(inData, R_ClassSymbol);
    List e0 = cls.attr(".rxode2.lst");
    if (as<std::string>(trans[RxMvTrans_lib_name]) ==
        as<std::string>(e0[RxTrans_lib_name])){
      if (asBool(e0[RxTrans_allTimeVar], "allTimeVar") && !allTimeVar){
        LogicalVector sub0 = as<LogicalVector>(e0[RxTrans_sub0]);
        int baseSize = as<int>(e0[RxTrans_baseSize]);
        int nTv = as<int>(e0[RxTrans_nTv]);
        List lst = as<List>(e0[RxTrans_lst]);
        CharacterVector nme = as<CharacterVector>(e0[RxTrans_nme]);
        List e = clone(e0);
        e[RxTrans_baseSize] = R_NilValue;
        e[RxTrans_nTv] = R_NilValue;
        e[RxTrans_lst] = R_NilValue;
        e[RxTrans_nme] = R_NilValue;
        e[RxTrans_sub0] = R_NilValue;
        e[RxTrans_allTimeVar] = false;
        cls.attr(".rxode2.lst") = e;
        List lstF = List(baseSize+nTv);
        CharacterVector nmeF = CharacterVector(baseSize+nTv);
        int j=0;
        for (int i = 0; i < lst.size();i++){
          if (sub0[i]){
            lstF[j]=lst[i];
            nmeF[j]=nme[i];
            j++;
          }
        }
        Rf_setAttrib(lstF, R_NamesSymbol, nmeF);
        Rf_setAttrib(lstF, R_ClassSymbol, cls);
        IntegerVector tmp = lstF[0];
        Rf_setAttrib(lstF, R_RowNamesSymbol,
                     IntegerVector::create(NA_INTEGER,-tmp.size()));
        return(lstF);
      } else {
        return inData;
      }
    }
  }
#ifdef rxSolveT
  REprintf("  Time1: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
  _lastT0 = clock();
#endif
  // Translates events + model into translated events
  CharacterVector dName = as<CharacterVector>(Rf_getAttrib(inData, R_NamesSymbol));
  CharacterVector lName = clone(dName);
  CharacterVector diName;
  CharacterVector liName;
  if (hasIcov) {
    diName = as<CharacterVector>(Rf_getAttrib(iCov_, R_NamesSymbol));
    liName = clone(diName);
  }
  int i, idCol = -1, evidCol=-1, timeCol=-1, amtCol=-1, cmtCol=-1,
    dvCol=-1, ssCol=-1, rateCol=-1, addlCol=-1, iiCol=-1, durCol=-1, j,
    mdvCol=-1, dvidCol=-1, censCol=-1, limitCol=-1, methodCol = -1,
    idIcovCol = -1;
  std::string tmpS;

  CharacterVector pars = as<CharacterVector>(mv[RxMv_params]);
  std::vector<int> covCol;
  std::vector<int> covParPos;
  std::vector<int> keepCol;
  std::string tmpS0;
  bool allBolus = true;
  bool allInf = true;
  int mxCmt = 0;
  std::vector<int> keepI(keep.size(), 0);
  for (i = lName.size(); i--;) {
    tmpS0= as<std::string>(lName[i]);
    tmpS = as<std::string>(lName[i]);
    std::transform(tmpS.begin(), tmpS.end(), tmpS.begin(), ::tolower);
    lName[i] = tmpS;
    if (tmpS == "id") idCol=i;
    else if (tmpS == "evid") evidCol=i;
    else if (tmpS == "time") timeCol=i;
    else if (tmpS == "amt" || tmpS == "value"){
      if (amtCol != -1) stop(_("can only specify either 'amt' or 'value'"));
      amtCol=i;
    }
    else if (tmpS == "cmt" || tmpS == "ytype" || tmpS == "state" || tmpS == "var"){
      if (cmtCol != -1) stop(_("can only specify either 'cmt', 'ytype', 'state' or 'var'"));
      cmtCol=i;
    }
    else if (tmpS == "dv") dvCol=i;
    else if (tmpS == "ss")   ssCol=i;
    else if (tmpS == "rate") rateCol=i;
    else if (tmpS == "dur") durCol=i;
    else if (tmpS == "addl") addlCol=i;
    else if (tmpS == "ii")   iiCol=i;
    else if (tmpS == "mdv") mdvCol=i;
    else if (tmpS == "dvid") dvidCol=i;
    else if (tmpS == "cens") censCol=i;
    else if (tmpS == "limit") limitCol=i;
    else if (tmpS == "method") methodCol=i;
    if (tmpS != "dv") {
      for (j = pars.size(); j--;){
        // Check lower case
        if (tmpS == as<std::string>(pars[j])){
          // Covariate found; dv not considered covariate
          covCol.push_back(i);
          covParPos.push_back(j);
          break;
        }
        if (tmpS0 == as<std::string>(pars[j])){
          // Covariate found.
          covCol.push_back(i);
          covParPos.push_back(j);
          break;
        }
        // Check upper case.
        std::transform(tmpS.begin(), tmpS.end(), tmpS.begin(), ::toupper);
        if (tmpS == as<std::string>(pars[j])){
          // Covariate found.
          covCol.push_back(i);
          covParPos.push_back(j);
          break;
        }
      }
    }
    for (j = keep.size(); j--;){
      if (as<std::string>(dName[i]) == as<std::string>(keep[j])){
        if (tmpS == "evid") stop(_("cannot keep 'evid'; try 'addDosing'"));
        keepCol.push_back(i);
        keepI[j] = 1;
        break;
      }
    }
  }
  if (hasIcov) {
    for (i = liName.size(); i--;) {
      tmpS0= as<std::string>(liName[i]);
      tmpS = as<std::string>(liName[i]);
      std::transform(tmpS.begin(), tmpS.end(), tmpS.begin(), ::tolower);
      liName[i] = tmpS;
      if (tmpS == "id") {
        idIcovCol=i;
        continue;
      }
      else if (tmpS == "evid") {
        stop(_("cannot specify 'evid' in 'iCov'"));
      }
      else if (tmpS == "time") {
        stop(_("cannot specify 'time' in 'iCov'"));
      }
      else if (tmpS == "amt") {
        stop(_("cannot specify 'amt' in 'iCov'"));
      } else if (tmpS == "value") {
        stop(_("cannot specify 'value' in 'iCov'"));
      } else if (tmpS == "cmt") {
        stop(_("cannot specify 'cmt' in 'iCov'"));
      } else if (tmpS=="ytype") {
        stop(_("cannot specify 'ytype' in 'iCov'"));
      } else if (tmpS=="state") {
        stop(_("cannot specify 'state' in 'iCov'"));
      } else if (tmpS == "var") {
        stop(_("cannot specify 'var' in 'iCov'"));
      } else if (tmpS == "dv") {
        stop(_("cannot specify 'dv' in 'iCov'"));
      } else if (tmpS == "ss") {
        stop(_("cannot specify 'ss' in 'iCov'"));
      } else if (tmpS == "rate") {
        stop(_("cannot specify 'rate' in 'iCov'"));
      } else if (tmpS == "dur") {
        stop(_("cannot specify 'dur' in 'iCov'"));
      } else if (tmpS == "addl") {
        stop(_("cannot specify 'addl' in 'iCov'"));
      } else if (tmpS == "ii") {
        stop(_("cannot specify 'ii' in 'iCov'"));
      } else if (tmpS == "mdv") {
        stop(_("cannot specify 'mdv' in 'iCov'"));
      } else if (tmpS == "dvid") {
        stop(_("cannot specify 'dvid' in 'iCov'"));
      } else if (tmpS == "cens") {
        stop(_("cannot specify 'cens' in 'iCov'"));
      } else if (tmpS == "limit") {
        stop(_("cannot specify 'limit' in 'iCov'"));
      } else if (tmpS == "method") {
        stop(_("cannot specify 'method' in 'iCov'"));
      }
      for (j = pars.size(); j--;){
        // Check lower case
        if (tmpS == as<std::string>(pars[j])){
          // Covariate found; dv not considered covariate
          covCol.push_back(-i-1);
          covParPos.push_back(j);
          break;
        }
        if (tmpS0 == as<std::string>(pars[j])){
          // Covariate found.
          covCol.push_back(-i-1);
          covParPos.push_back(j);
          break;
        }
        // Check upper case.
        std::transform(tmpS.begin(), tmpS.end(), tmpS.begin(), ::toupper);
        if (tmpS == as<std::string>(pars[j])){
          // Covariate found.
          covCol.push_back(-i-1);
          covParPos.push_back(j);
          break;
        }
      }
      for (j = keep.size(); j--;){
        if (as<std::string>(diName[i]) == as<std::string>(keep[j])){
          keepCol.push_back(-i-1);
          keepI[j] = 2;
          break;
        }
      }
    }
    if (idIcovCol == -1)  {
      stop(_("'iCov' must have an 'id' column"));
    }
  }
#ifdef rxSolveT
  REprintf("  Time2: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
  _lastT0 = clock();
#endif
  std::string wKeep = "";
  if ((int)(keepCol.size()) != (int)keep.size()){
    wKeep = "Cannot keep missing columns:";
    for (j = 0; j < keep.size(); j++){
      if (keepI[j] == 0){
        wKeep += " " + as<std::string>(keep[j]);
      }
    }
  }
  List covUnits(covCol.size());
  CharacterVector covUnitsN(covCol.size());
  NumericVector nvTmp, nvTmp2;
  bool hasCmt = false;
  int cmtI =0;
  List strAssign = mv[RxMv_strAssign];
  List strAssignN = strAssign.names();
  List inDataF(covCol.size());
  List inDataLvlN(covCol.size()+strAssign.size());
  List inDataLvl(covCol.size()+strAssign.size());
  for (i = covCol.size(); i--;){
    int covColi = covCol[i];
    if (covColi >= 0) {
      inDataLvlN[i] = covUnitsN[i] = lName[covColi];
    } else {
      // Get from iCov; when found the i was pushed back as:
      //   covCol.push_back(-i-1);
      // Using some algebra you have
      // -i-1 = covColi
      // i    = -covColi-1
      inDataLvlN[i] = covUnitsN[i] = liName[-covColi-1];
    }
    nvTmp2 = NumericVector::create(1.0);
    if (hasCmt || covColi >= 0 && as<std::string>(lName[covColi]) != "cmt"){
      RObject cur;
      if (covColi >= 0) {
        cur = inData[covColi];
      } else {
        cur = iCov_[-covColi-1];
      }
      if (TYPEOF(cur) == INTSXP){
        RObject lvls = cur.attr("levels");
        if (!Rf_isNull(lvls)){
          inDataLvl[i] = lvls;
        }
      } else if (TYPEOF(cur) == STRSXP) {
        cur = convertId_(cur);
        inDataF[i] = cur;
        RObject lvls = cur.attr("levels");
        inDataLvl[i] = lvls;
      }
      nvTmp = as<NumericVector>(cur);
      if (!dropUnits && Rf_inherits(nvTmp, "units")){
        Rf_setAttrib(nvTmp2, R_ClassSymbol, wrap("units"));
        nvTmp2.attr("units") = nvTmp.attr("units");
      }
    } else {
      hasCmt=true;
      cmtI = i;
    }
    covUnits[i] = nvTmp2;
  }
  for (i = 0; i < strAssign.size(); ++i){
    inDataLvlN[i+covCol.size()] = strAssignN[i];
    inDataLvl[i+covCol.size()] = strAssign[i];
  }
  Rf_setAttrib(inDataLvl, R_NamesSymbol, inDataLvlN);
  Rf_setAttrib(covUnits, R_NamesSymbol, covUnitsN);
  // EVID = 0; Observations
  // EVID = 1; is illegal, but converted from NONMEM
  // EVID = 2; Non-observation, possibly covariate
  // EVID = 3; Reset ODE states to zero; Non-observation event
  // EVID = 4; Reset and then dose event;  Illegal
  // EVID = 9; Non-observation event to ini system at time zero; This is to set the INIs at the correct place.
  // EVID = 10-99; mtime events (from ODE system)
  // When EVID > 100
  // EVID: ## # ## ##
  //       c2 I c1 xx
  // c2 = Compartment numbers over 100
  //  I = Infusion Flag/ Special event flag
  //      0 = no Infusion
  //      1 = Infusion, AMT=rate (mg/hr for instance)
  //      2 = Infusion, duration is fixed
  //      4 = Replacement event
  //      5 = Multiplication event
  //      6 = Turn off modeled duration
  //      7 = Turn off modeled rate compartment
  //      8 = Duration is modeled, AMT=dose; Rate = AMT/(Modeled Duration) NONMEM RATE=-2
  //      9 = Rate is modeled, AMT=dose; Duration = AMT/(Modeled Rate) NONMEM RATE=-1
  // c1 = Compartment numbers below 99
  // xx = 1, regular event
  // xx = 9, Hidden Zero event to make sure that X(0) happens at time 0
  // xx = 10, steady state event SS=1
  // xx = 20, steady state event + last observed info.
  // xx = 30, Turn off compartment
  // xx = 40, Steady state constant infusion
  // Steady state events need a II data item > 0

  CharacterVector state0 = as<CharacterVector>(mv[RxMv_state]);
  CharacterVector stateE = as<CharacterVector>(mv[RxMv_stateExtra]);
  CharacterVector stateS = as<CharacterVector>(mv[RxMv_sens]);
  int extraCmt  = as<int>(mv[RxMv_extraCmt]);
  // Enlarge compartments
  if (extraCmt == 2){
    CharacterVector newState(state0.size()+2);
    newState[0] = "depot";
    newState[1] = "central";
    for (int j = state0.size();j--;) newState[j+2] = state0[j];
    // for (int j = state0.size();j--;) newState[j] = state0[j];
    // newState[state0.size()] = "depot";
    // newState[state0.size()+1] = "central";
    state0 = newState;
  } else if (extraCmt==1){
    CharacterVector newState(state0.size()+1);
    newState[0] = "central";
    for (int j = state0.size();j--;) newState[j+1] = state0[j];
    // newState[state0.size()] = "central";
    state0 = newState;
  }
  CharacterVector state(state0.size() + stateE.size());
  for (int i = 0; i < state0.size(); i++){
    state[i] = state0[i];
  }
  for (int i = 0; i < stateE.size(); i++){
    state[i+state0.size()] = stateE[i];
  }
  // Now adjust the compartment numbers if needed.
  int baseSize = state0.size() - stateS.size();
  for (int i = curDvid.size(); i--;){
    if (curDvid[i] > baseSize){
      curDvid[i] = stateS.size()+curDvid[i];
    }
  }
  if (timeCol== -1){
    stop(_("'time' is required in dataset"));
  }
  NumericVector inTime;
  if (rxIsNumIntLgl(inData[timeCol])){
    inTime = as<NumericVector>(inData[timeCol]);
  } else {
    List newInData = clone(inData);
    Function convDate = rx[".convertExtra"];
    newInData =  convDate(newInData);
    return etTrans(newInData, mv, addCmt, dropUnits, allTimeVar, keepDosingOnly,
                   combineDvid, CharacterVector(0), false, true, true, iCov);
  }
  size_t resSize = inTime.size()+256;
  std::vector<int> id;
  id.reserve(resSize);
  std::vector<int> idIcov;
  std::vector<int> allId;
  allId.reserve(resSize);
  std::vector<int> obsId;
  obsId.reserve(resSize);
  std::vector<int> zeroId;
  obsId.reserve(resSize);
  std::vector<int> doseId;
  doseId.reserve(resSize);
  std::vector<int> evid;
  evid.reserve(resSize);
  std::vector<double> time;
  time.reserve(resSize);
  std::vector<double> amt;
  amt.reserve(resSize);
  std::vector<double> ii;
  ii.reserve(resSize);
  std::vector<double> limit;
  limit.reserve(resSize);
  std::vector<int> idxInput;
  idxInput.reserve(resSize);
  std::vector<int> cmtF; // Final compartment
  cmtF.reserve(resSize);
  std::vector<int> dvidF;
  dvidF.reserve(resSize);
  std::vector<double> dv;
  dv.reserve(resSize);
  std::vector<int> idxOutput;
  std::vector<int> idxIcov;
  std::vector<int> cens;
  cens.reserve(resSize);
  std::vector<int> nObsId;


#ifdef rxSolveT
  REprintf("  Time3: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
  _lastT0 = clock();
#endif

  // save units information
  bool addTimeUnits = false;
  RObject timeUnits;
  if (Rf_inherits(inTime, "units")){
    addTimeUnits=true;
    timeUnits=inTime.attr("units");
  }
  int tmpCmt = 1;
  IntegerVector inId;
  CharacterVector idLvl;
  IntegerVector inIdCov;
  int idInt=0;
  if (idCol != -1){
    if (rxode2parseIsIntegerish(inData[idCol])){
      idInt = 1;
    }
    inId = convertId_(inData[idCol]);//as<IntegerVector>();
    if (fastFactorDataHasNa == 1) stop(_("'id' cannot have NA values"));
    idLvl = Rf_getAttrib(inId, R_LevelsSymbol);
  } else {
    idLvl = CharacterVector::create("1");
  }
  if (hasIcov) {
    if (!rxode2parseIsIntegerish(iCov_[idIcovCol]) &&
        idInt == 1) {
      stop(_("data 'id' column is an integer; 'iCov' 'id' also needs to be an integer"));
    }
    bool badSize = false;
    if (Rf_length(iCov_[idIcovCol]) != idLvl.size()) badSize=true;
    if (!badSize) {
      Function factor2("factor", R_BaseNamespace);
      inIdCov = factor2(iCov_[idIcovCol], _["levels"]=idLvl);
      if (inIdCov.size() != idLvl.size()) badSize=true;
    }
    if (badSize) {
      stop(_("the 'id' in the iCov must have 1 unique match to the event table"));
    }
    for (int i=0; i < inIdCov.size(); ++i) {
      idxIcov.push_back(i);
      if (inIdCov[i] == NA_INTEGER) {
        stop(_("some of the 'id' values do not match the input event table"));
      }
    }
  }
  IntegerVector inCmt;
  RObject cmtInfo = R_NilValue;
  if (cmtCol != -1){
    inCmt = as<IntegerVector>(toCmt(inData[cmtCol], state, false,
                                    state0.size(), stateS.size(), curDvid,
                                    inId, idLvl));//as<IntegerVector>();
    cmtInfo = inCmt.attr("cmtNames");
    inCmt.attr("cmtNames") = R_NilValue;
  }
  IntegerVector inDvid;
  if (dvidCol != -1){
    inDvid = as<IntegerVector>(toCmt(inData[dvidCol], state, true,
                                     state0.size(), stateS.size(),
                                     curDvid, inId, idLvl));//as<IntegerVector>();
    inDvid.attr("cmtNames") = R_NilValue;
  }
  IntegerVector inSs;
  if (ssCol != -1){
    if (rxIsNumIntLgl(inData[ssCol])){
      // NA by default is NA_logical
      inSs = as<IntegerVector>(inData[ssCol]);
    } else {
      stop(_("steady state column ('ss') needs to be an integer"));
    }
  }
  IntegerVector inEvid;
  bool evidIsMDV = false;
  bool hasEvid=false;
  if (evidCol != -1){
    if (rxIsNumIntLgl(inData[evidCol])){
      inEvid = as<IntegerVector>(inData[evidCol]);
      hasEvid=true;
    } else {
      stop(_("event id ('evid') needs to be an integer"));
    }
  } else if (mdvCol != -1){
    evidCol = mdvCol;
    mdvCol=-1;
    evidIsMDV=true;
    if (rxIsNumIntLgl(inData[evidCol])){
      inEvid = as<IntegerVector>(inData[evidCol]);
    } else {
      stop(_("missing DV ('mdv') needs to be an integer"));
    }
    hasEvid=true;
  } else if (methodCol != -1){
    inEvid = convertMethod(inData[methodCol]);
    evidCol= methodCol;
    //hasEvid=true; The EVID is not present in the right form.
    // This allows mixing of deSolve with rate info
  }
  IntegerVector inMdv;
  if (mdvCol != -1){
    if (rxIsNumIntLgl(inData[mdvCol])){
      inMdv = as<IntegerVector>(inData[mdvCol]);
    } else {
      stop(_("missing dependent variable ('mdv') needs to be an integer"));
    }
  }
  NumericVector inRate;
  if (rateCol != -1){
    if (rxIsNumIntLgl(inData[rateCol])) {
      inRate = as<NumericVector>(inData[rateCol]);
    } else {
      stop(_("'rate' needs to be a number"));
    }
  }

  NumericVector inDur;
  if (durCol != -1){
    if (rxIsNumIntLgl(inData[durCol])) {
      inDur = as<NumericVector>(inData[durCol]);
    } else {
      stop(_("'dur' needs to be a number"));
    }
  }

  bool addAmtUnits = false;
  RObject amtUnits;
  NumericVector inAmt;
  if (amtCol != -1){
    if (rxIsNumIntLgl(inData[amtCol])){
      inAmt = as<NumericVector>(inData[amtCol]);
      if (Rf_inherits(inAmt, "units")){
        addAmtUnits=true;
        amtUnits=inAmt.attr("units");
      }
    } else {
      stop(_("amount ('amt') needs to be a number"));
    }
  }
  NumericVector inIi;
  if (iiCol != -1){
    if (rxIsNumIntLgl(inData[iiCol])){
      inIi = as<NumericVector>(inData[iiCol]);
    } else {
      stop(_("inter-dose interval ('ii') needs to be a number"));
    }
  }
  IntegerVector inAddl;
  if (addlCol != -1){
    if (rxIsNumIntLgl(inData[addlCol])){
      inAddl = as<IntegerVector>(inData[addlCol]);
    } else {
      stop(_("number of additional doses ('addl') needs to be an integer"));
    }
  }
  NumericVector inDv;
  if (dvCol != -1){
    if (rxIsNumIntLgl(inData[dvCol])) {
      inDv = as<NumericVector>(inData[dvCol]);
    } else {
      stop(_("dependent variable ('dv') needs to be a number"));
    }
  }
  IntegerVector inCens;
  if (censCol != -1){
    if (rxIsNumIntLgl(inData[censCol])){
      inCens = as<IntegerVector>(inData[censCol]);
    } else {
      stop(_("censoring variable ('cens') needs to be a number"));
    }
  }
  NumericVector inLimit;
  if (limitCol != -1){
    if (rxIsNumIntLgl(inData[limitCol])) {
      inLimit = as<NumericVector>(inData[limitCol]);
    } else {
      stop(_("limit variable ('limit') needs to be a number"));
    }
  }

  int flg = 0;
  int cid = 0;
  int nMtime = as<int>(mv[RxMv_nMtime]);
  double rate = 0.0;
  int nid=0;
  int cmt = 0;
  int rateI = 0;
  int cmt100; //= amt[i]/100;
  int cmt99;  //= amt[i]-amt100*100;
  int cevid;
  int nevid;
  int nevidLag;
  int caddl;
  double ctime;
  double cii;
  double dur =0.0;
  double camt;
  double cdv, climit;
  int nobs=0, ndose=0;

  int ccens=0;
  bool warnCensNA=false;
  bool censNone=true;
  bool swapDvLimit=false;
  // cens = NA_INTEGER with LIMIT is M2
  bool doWarnNeg=false;
#ifdef rxSolveT
  REprintf("  Time4: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
  _lastT0 = clock();
#endif

  bool isSorted = true;
  int lastId = NA_INTEGER;
  double lastTime = NA_REAL;
  bool hasReset = false;
  double maxShift = 0;
  bool warnNaTime=false;
  // This section of the code:
  // - Loops over the input dataset
  // - Changes nonmem-style evid to classic rxode events
  // - adds infusion endings and additional doses
  for (int i = 0; i < inTime.size(); i++) {
    if (idCol == -1) cid = 1;
    else cid = inId[i];
    if (dvCol == -1) cdv = NA_REAL;
    else cdv = inDv[i];
    if (censCol == -1) ccens = NA_INTEGER;
    else ccens = inCens[i];
    if (ccens != 0 && ccens != 1 &&
        ccens != -1 && !IntegerVector::is_na(ccens))
      stop(_("censoring column can only be -1, 0 or 1 (id: %s, row: %d)"), CHAR(idLvl[cid-1]), i+1);
    if (ISNA(cdv) && ccens != 0) {
      if (!IntegerVector::is_na(ccens)) {
        warnCensNA=true;
        ccens=0;
      }
    }
    ctime=inTime[i];
    if (ISNA(ctime)) {
      warnNaTime=true;
      continue;
    }
    // REprintf("lastId: %d; cid: %d, lastTime: %f, ctime %f\n", lastId, cid, lastTime, ctime);
    if (IntegerVector::is_na(lastId)) {
      lastId = cid;
    } else if (lastId != cid) {
      lastId = cid;
    } else if (lastTime > ctime) {
      if (inEvid[i] != 3 && inEvid[i] != 4) {
        isSorted = false; // The prior EVID=3 w/reset a reset time
        // REprintf("\t not sorted");
      } else if (lastTime > ctime) {
        maxShift = max2(maxShift, lastTime-ctime);
      }
    }
    lastTime = ctime;
    if (limitCol == -1) climit = R_NegInf;
    else climit = inLimit[i];
    if (ISNA(climit)) climit = R_NegInf;
    if (std::isinf(ctime)){
      stop(_("infinite times are not allowed (id: %s, row: %d)"), CHAR(idLvl[cid-1]), i+1);
    }
    if (ctime < 0 && _ini0){
      doWarnNeg=true;
    }
    if (iiCol == -1) cii = 0;
    else cii = inIi[i];
    if (ISNA(cii)) cii=0.0;

    if (cid == NA_INTEGER) stop(_("'id' cannot be 'NA'"));
    if (std::find(allId.begin(), allId.end(), cid) == allId.end()){
      allId.push_back(cid);
      // New ID
      // Add mtime records
      for (j = nMtime; j--;){
        id.push_back(cid);
        evid.push_back(j+10);
        cmtF.push_back(0);
        time.push_back(0.0);
        amt.push_back(NA_REAL);
        ii.push_back(0.0);
        dv.push_back(NA_REAL);
        cens.push_back(0);
        limit.push_back(NA_REAL);

        idxInput.push_back(-1);
      }
      nid++;
    }

    // Amt
    if (amtCol == -1) camt = 0.0;
    else camt = inAmt[i];

    // SS flag
    flg=1;
    if (ssCol == -1) flg=1;
    else if (inSs[i] == 0) flg=1;
    else if (IntegerVector::is_na(inSs[i])) flg=1;
    else if (inSs[i] == 1 && cii > 0) flg=10;
    else if (inSs[i] == 2 && cii > 0) flg=20;
    else if (inSs[i] == 1 && cii == 0 && camt == 0.0){
      flg=40;
    }

    if (cmtCol != -1) {
      tmpCmt = inCmt[i];
      if (inCmt[i] == 0 || IntegerVector::is_na(inCmt[i])){
        if (evidCol == -1){
          tmpCmt=1;
        } else if (inEvid[i] == 0){
          tmpCmt=1;
        } else {
          tmpCmt=1;
        }
      }
      if (IntegerVector::is_na(inCmt[i])){
        tmpCmt = 1;
      } else if (inCmt[i] < 0){
        if (flg != 1) stop(_("steady state records cannot be on negative compartments (id: %s, row: %d)"), CHAR(idLvl[cid-1]), i+1);
        flg = 30;
        tmpCmt = -tmpCmt;
      }
      cmt = tmpCmt;
    } else if (cmtCol == -1) {
      cmt = 1;
    }
    // CMT flag
    else cmt = tmpCmt;
    // see if you need to adjust for lagged
    if (ssAtDoseTime) {
      if (flg == 10 || flg == 20) {
        for (int jj = 0; jj < curAlag.size(); ++jj) {
          if (cmt == curAlag[jj]) {
            if (flg == 10) {
              flg = 9;
            } else {
              flg = 19;
            }
            break;
          }
        }
      }
    }
    if (cmt <= 99){
      cmt100=0;
      cmt99=cmt;
    } else {
      cmt100=cmt/100;
      cmt99=cmt-cmt100*100;
    }
    mxCmt = max2(cmt,mxCmt);

    rateI = 0;
    // Rate
    if (durCol == -1 || inDur[i] == 0 || ISNA(inDur[i])){
      if (rateCol == -1 || inRate[i] == 0 || ISNA(inRate[i])) rate = 0.0;
      else rate = inRate[i];
      if (rate == -1.0){
        // rate is modeled
        rateI = 9;
        // if (!(needSort & needSortRate)) {
        //   warning(_("data specified modeled rate (=-1) but no rate() in the model (id: %s, row: %d)"), CHAR(idLvl[cid-1]), i+1);
        // }
        //rateModeled = true;
      } else if (rate == -2.0){
        // duration is modeled
        if (flg == 40){
          stop(_("when using steady state constant infusion modeling duration does not make sense (id: %s, row: %d)"), CHAR(idLvl[cid-1]), i+1);
        }
        // if (!(needSort & needSortDur)) {
        //   warning(_("data specified modeled duration (=-2) but no dur() in the model (id: %s, row: %d)"), CHAR(idLvl[cid-1]), i+1);
        // }
        rateI = 8;
        //durModeled = true;
      } else if (rate > 0){
        // Rate is fixed
        if (evidCol == -1 || inEvid[i] == 1 || inEvid[i] == 4){
          rateI = 1;
        } else {
          rateI = 0;
          rate = 0.0;
        }
      }
    } else if (rateCol == -1 || inRate[i] == 0 || ISNA(inRate[i])) {
      if (durCol == -1) rate = 0.0;
      if (inDur[i] == 0) rate = 0;
      // if (inDur[i] > 0)
      if (inDur[i] == -1.0){
        // rate is modeled
        if (flg == 40){
          stop(_("specifying duration with a steady state constant infusion makes no sense (id: %s row: %d)"), CHAR(idLvl[cid-1]), i+1);
        }
        rateI = 9;
      } else if (inDur[i] == -2.0){
        // duration is modeled
        if (flg == 40){
          stop(_("specifying duration with a steady state constant infusion makes no sense (id: %d row: %d)"), CHAR(idLvl[cid-1]), i+1);
        }
        rateI = 8;
      } else if (inDur[i] > 0){
        // Duration is fixed
        if (flg == 40){
          stop(_("specifying duration with a steady state constant infusion makes no sense (id: %d row: %d)"), CHAR(idLvl[cid-1]), i+1);
        }
        if (evidCol == -1 || inEvid[i] == 1 || inEvid[i] == 4){
          rateI = 2;
          rate = camt/inDur[i];
        } else if (inEvid[i] > 4){
          rateI=0;
          rate = 0.0;
        }
      }
    } else {
      stop(_("'rate' and/or 'dur' are not specified correctly (id: %d row: %d)"), CHAR(idLvl[cid-1]), i+1);
    }
    if (addlCol == -1) caddl=0;
    else caddl = inAddl[i];
    if (IntegerVector::is_na(caddl)) caddl = 0;
    // EVID flag
    if (evidCol == -1) {
      // Missing EVID
      if (rateI == 0 && (ISNA(camt) || camt == 0.0)){
        cevid = 0;
        if (mdvCol != -1 && inMdv[i] == 1){
          cevid=2;
        }
        if ((cevid == 0 || (cevid == 2 && evid2isObs)) &&
            std::find(obsId.begin(), obsId.end(), cid) == obsId.end()){
          obsId.push_back(cid);
        }
      } else {
        if (mdvCol != -1 && (inMdv[i] == 0 || IntegerVector::is_na(inMdv[i]))){
          stop(_("'amt' or 'dur'/'rate' are non-zero therefore MDV cannot = 0 (id: %s row: %d)"), CHAR(idLvl[cid-1]), i+1);
        }
        // For Rates and non-zero amts, assume dosing event
        cevid = cmt100*100000+rateI*10000+cmt99*100+flg;
        allBolus=false;
      }
    } else {
      cevid = inEvid[i];
    }
    if (evidIsMDV && cevid == 1 && camt == 0){
      cevid=2;
    }
    if (IntegerVector::is_na(cevid)){
      if (evidIsMDV){
        cevid=1;
      } else {
        cevid=0;
      }
    }
    switch(cevid) {
    case 0:
      // Observation
      nobs++;
      cevid = 0;
      if (mdvCol != -1 && inMdv[i] == 1){
        cevid = 2;
      }
      if (dvCol != -1 && ISNA(inDv[i])){
        if (amtCol==-1){
          cevid=2;
        } else if (ISNA(inAmt[i]) || inAmt[i] == 0){
          cevid=2;
        }
      }
      if ((cevid == 0 || (cevid == 2 && evid2isObs)) &&
          std::find(obsId.begin(), obsId.end(), cid) == obsId.end()){
        obsId.push_back(cid);
      }
      if (caddl > 0){
        Rf_warningcall(R_NilValue, "%s", _("'addl' is ignored with observations"));
      }
      if (flg != 1){
        flg=1;
      }
      {
        bool goodCmt = false;
        int cmpCmt;
        if ((curDvid.size()) > 1){
          for (j=curDvid.size();j--;){
            if (curDvid[j] > 0){
              if (curDvid[j] == cmt || curDvid[j] == -cmt){
                goodCmt=true;
                break;
              }
            } else {
              cmpCmt = state.size()+curDvid[j]+1;
              if (cmpCmt == cmt || cmpCmt == -cmt){
                goodCmt=true;
                break;
              }
            }
          }
          if (combineDvidB && dvidCol != -1 &&
              !IntegerVector::is_na(inDvid[i]) &&
              inDvid[i]>0){
            if (goodCmt && cmt != inDvid[i] && cmt != 1 && cmt != 0){
              stop(_("'cmt' and 'dvid' specify different compartments (id: %s row: %d)"), CHAR(idLvl[cid-1]), i+1);
            }
            cmt = inDvid[i];
            goodCmt=true;
          }
        } else {
          goodCmt = true;
          if (combineDvidB && dvidCol != -1 && !IntegerVector::is_na(inDvid[i]) &&
              inDvid[i]>0){
            cmt = inDvid[i];
          }
        }
        if (!goodCmt){
          IntegerVector dvidDF(curDvid.size());
          for (i = dvidDF.size(); i--;){
            dvidDF[i] = i+1;
          }
          List dvidTrans = List::create(_["dvid"]=dvidDF, _["modeled cmt"]=curDvid);
          Rf_setAttrib(dvidTrans, R_ClassSymbol, wrap("data.frame"));
          Rf_setAttrib(dvidTrans, R_RowNamesSymbol,
                       IntegerVector::create(NA_INTEGER, -dvidDF.size()));
          Rprintf("%s", _("'DVID'/'CMT' translation:\n"));
          print(dvidTrans);
          if (dvidCol != -1){
            Rprintf("'DVID': %d\t", inDvid[i]);
          }
          Rprintf("'CMT': %d\n", cmt);
          stop(_("'dvid'->'cmt' or 'cmt' on observation record on a undefined compartment (use 'cmt()' 'dvid()') id: %s row: %d"),
               CHAR(idLvl[cid-1]), i+1);
        }
        id.push_back(cid);
        evid.push_back(cevid);
        cmtF.push_back(cmt);
        time.push_back(ctime);
        if (ctime == 0){
          if (std::find(zeroId.begin(), zeroId.end(), cid) == zeroId.end()){
            zeroId.push_back(cid);
          }
        }
        amt.push_back(NA_REAL);
        ii.push_back(0.0);
        cens.push_back(ccens);
        if (ccens!=0) censNone=false;
        if (ccens == 1 && !std::isinf(climit)){
          // limit should be lower than dv
          if (cdv < climit){
            dv.push_back(climit);
            limit.push_back(cdv);
            swapDvLimit=true;
          } else if (cdv == climit){
            stop(_("'limit' (%f) cannot equal 'dv' (%f) id: %s row: %d"), climit, cdv, CHAR(idLvl[cid-1]), i+1);
          } else {
            dv.push_back(cdv);
            limit.push_back(climit);
          }
        } else {
          dv.push_back(cdv);
          limit.push_back(climit);
        }

        idxInput.push_back(i);
        cevid = -1;
      }
      break;
    case 7:
    case 1:
      if (mdvCol != -1 && (inMdv[i] == 0 || IntegerVector::is_na(inMdv[i]))){
        stop(_("'mdv' cannot be 0 when 'evid'=%d id: %s row: %d"), cevid, CHAR(idLvl[cid-1]), i+1);
      }
      if (amtCol == -1) {
        stop(_("'amt' column missing with dosing event (EVID=%d, id: %s row: %d)"), cevid, CHAR(idLvl[cid-1]), i+1);
      }
      if (cevid == 7) {
        flg = 50;
        cevid=1;
      }
      cevid = cmt100*100000+rateI*10000+cmt99*100+flg;
      if (rateI == 0) allInf=false;
      else allBolus=false;
      break;
    case 2:
      cevid = 2;
      if ((cevid == 0 || (cevid == 2 && evid2isObs)) && std::find(obsId.begin(), obsId.end(), cid) == obsId.end()){
        obsId.push_back(cid);
      }
      if (flg == 30){
        rateI = 0;
        cevid = cmt100*100000+rateI*10000+cmt99*100+flg;
        allInf=false; allBolus=false;
      } else {
        cevid = cmt100*100000+rateI*10000+cmt99*100+1;
        if (rateI == 0) allInf=false;
        else allBolus=false;
        if (caddl > 0){
          Rf_warningcall(R_NilValue, "%s", _("'addl' is ignored with 'EVID=2'"));
        }
        if (flg != 1){
          Rf_warningcall(R_NilValue, "%s", _("'ss' is ignored with 'EVID=2'"));
        }
        id.push_back(cid);
        evid.push_back(2);
        cmtF.push_back(cmt);
        time.push_back(ctime);
        if (ctime == 0){
          if (std::find(zeroId.begin(), zeroId.end(), cid) == zeroId.end()){
            zeroId.push_back(cid);
          }
        }
        amt.push_back(NA_REAL);
        ii.push_back(0.0);
        dv.push_back(NA_REAL);
        limit.push_back(NA_REAL);
        cens.push_back(0);

        idxInput.push_back(i);
        ndose++;
        // + cmt needs to turn on cmts.
        // This gives a zero dose to cmt
        if (cmtCol != -1 && cmt > 0 && cmt <= baseSize) {
          // Turn on state with dose
          id.push_back(cid);
          evid.push_back(cevid-flg+60);
          cmtF.push_back(cmt);
          time.push_back(ctime);
          amt.push_back(0.0);
          ii.push_back(0.0);
          dv.push_back(NA_REAL);
          limit.push_back(NA_REAL);
          cens.push_back(0);

          idxInput.push_back(i);
          ndose++;
        }
        cevid = -1;
      }
      break;
    case 3:
      cevid = 3;
      if (caddl > 0){
        Rf_warningcall(R_NilValue, "%s", _("'addl' is ignored with 'EVID=3'"));
      }
      if (flg != 1){
        Rf_warningcall(R_NilValue, "%s", _("'ss' is ignored with 'EVID=3'"));
      }
      id.push_back(cid);
      evid.push_back(3);
      cmtF.push_back(cmt);
      time.push_back(ctime);
      hasReset = true;
      if (ctime == 0){
        if (std::find(zeroId.begin(), zeroId.end(), cid) == zeroId.end()){
          zeroId.push_back(cid);
        }
      }
      amt.push_back(NA_REAL);
      ii.push_back(0.0);
      dv.push_back(NA_REAL);
      limit.push_back(NA_REAL);
      cens.push_back(0);

      idxInput.push_back(i);
      ndose++;
      cevid = -1;
      break;
    case 4:
      if (mdvCol != -1 && (inMdv[i] == 0 || IntegerVector::is_na(inMdv[i]))){
        stop(_("'mdv' cannot be 0 when 'evid'=4 id: %s row: %d"), CHAR(idLvl[cid-1]), i+1);
      }
      if (amtCol == -1) {
        stop(_("'amt' column missing with dosing event (EVID=%d, id: %s row: %d)"), cevid, CHAR(idLvl[cid-1]), i+1);
      }

      id.push_back(cid);
      evid.push_back(3);
      cmtF.push_back(cmt);
      time.push_back(ctime);
      hasReset = true;
      if (ctime == 0){
        if (std::find(zeroId.begin(), zeroId.end(), cid) == zeroId.end()){
          zeroId.push_back(cid);
        }
      }
      amt.push_back(NA_REAL);
      ii.push_back(0.0);
      dv.push_back(NA_REAL);
      limit.push_back(NA_REAL);
      cens.push_back(0);

      idxInput.push_back(-1);
      ndose++;
      // Now use the transformed compartment
      cevid = cmt100*100000+rateI*10000+cmt99*100+flg;
      if (rateI == 0) allInf=false;
      else allBolus=false;
      break;
    case 5: // replace
      if (rateI != 0) stop(_("cannot have an infusion event with a replacement event (id: %s row: %d)"), CHAR(idLvl[cid-1]), i+1);
      if (amtCol == -1) {
        stop(_("'amt' column missing with dosing event (EVID=%d, id: %s row: %d)"), cevid, CHAR(idLvl[cid-1]), i+1);
      }
      rateI=4;
      cevid = cmt100*100000+rateI*10000+cmt99*100+flg;
      allInf=false;
      allBolus=false;
      break;
    case 6: // multiply
      if (rateI != 0) stop(_("cannot have an infusion event with a multiplication event (id: %s row: %d)"), CHAR(idLvl[cid-1]), i+1);
      if (amtCol == -1) {
        stop(_("'amt' column missing with dosing event (EVID=%d, id: %s row: %d)"), cevid, CHAR(idLvl[cid-1]), i+1);
      }
      rateI=5;
      cevid = cmt100*100000+rateI*10000+cmt99*100+flg;
      allInf=false;
      allBolus=false;
      break;
    default:
      if (cevid > 10 && cevid < 99){
        continue;
      }
      if (rateI != 0 && hasEvid){
        Rf_warningcall(R_NilValue, "%s", _("'rate' or 'dur' is ignored with classic rxode2 'EVID's"));
        rateI = 0;
      }
      if (flg!=1 && hasEvid){ // ss=1 is the same as ss=0 for NONMEM
        Rf_warningcall(R_NilValue, "%s", _("'ss' is ignored with classic rxode2 'EVID's"));
        flg=1;
      }
    }
    if (cevid != -1){
      if (rateI == 9){
        nevid = cmt100*100000+70001+cmt99*100;
      } else if (rateI == 8) {
        nevid = cmt100*100000+60001+cmt99*100;
      }
      id.push_back(cid);
      evid.push_back(cevid);
      cmtF.push_back(cmt);
      time.push_back(ctime);
      if (ctime == 0){
        if (std::find(zeroId.begin(), zeroId.end(), cid) == zeroId.end()){
          zeroId.push_back(cid);
        }
      }
      ii.push_back(cii);
      bool keepIIadl = false;
      bool addLagged = false;
      if (flg == 9 || flg == 19) {
        keepIIadl = true;
        addLagged = true;
      } else if ((flg == 10 || flg == 20 || flg == 40) && caddl > 0){
        keepIIadl = true;
        //stop(_("'ss' with 'addl' not supported (id: %s row: %d)"), CHAR(idLvl[cid-1]), i+1);
      }
      dv.push_back(NA_REAL);
      limit.push_back(NA_REAL);
      cens.push_back(0);

      idxInput.push_back(i);
      ndose++;
      if (rateI > 2 && rateI != 4 && rateI != 5 && flg != 40){
        // modeled rate/duration
        if (ISNA(camt) || camt == 0.0) {
          if (nevid != 2){
            stop(_("'amt' value NA or 0 for dose event (id: %s row: %d)"), CHAR(idLvl[cid-1]), i+1);
          }
        }
        amt.push_back(camt);
        if (addLagged) {
          // add lagged dose for steady state
          // note that steady state is already calculated with 09 or 19
          // add lagged dose to continue ss tau
          // if this is not a calculated rate/dur:
          nevidLag = cmt100*100000+rateI*10000+cmt99*100+1;
          id.push_back(cid);
          evid.push_back(nevidLag);
          cmtF.push_back(cmt);
          amt.push_back(camt);
          time.push_back(ctime);
          //ii.push_back(cii);
          ii.push_back(0.0);
          dv.push_back(NA_REAL);
          limit.push_back(NA_REAL);
          cens.push_back(0);

          idxInput.push_back(-1);
          ndose++;
        }
        // turn off
        id.push_back(cid);
        evid.push_back(nevid);
        cmtF.push_back(cmt);
        time.push_back(ctime);
        amt.push_back(camt);
        ii.push_back(0.0);
        dv.push_back(NA_REAL);
        limit.push_back(NA_REAL);
        cens.push_back(0);

        idxInput.push_back(-1);
        ndose++;
      } else if (rateI == 1 || rateI == 2){
        // In this case amt needs to be changed.
        // specified rate/duration
        dur = camt/rate;
        amt.push_back(rate); // turn on
        if (addLagged) {
          // add lagged dose for steady state
          // note that steady state is already calculated with 09 or 19
          // add lagged dose to continue ss tau
          // if this is not a calculated rate/dur:
          id.push_back(cid);
          evid.push_back(cevid-flg+8);
          cmtF.push_back(cmt);
          amt.push_back(-rate);
          time.push_back(ctime);
          ii.push_back(cii);
          dv.push_back(NA_REAL);
          limit.push_back(NA_REAL);
          cens.push_back(0);

          idxInput.push_back(-1);
          ndose++;

          nevidLag = cmt100*100000+rateI*10000+cmt99*100+1;
          id.push_back(cid);
          evid.push_back(nevidLag);
          cmtF.push_back(cmt);
          amt.push_back(rate);
          time.push_back(ctime);
          //ii.push_back(cii);
          ii.push_back(0.0);
          dv.push_back(NA_REAL);
          limit.push_back(NA_REAL);
          cens.push_back(0);

          idxInput.push_back(-1);
          ndose++;
        }
        // turn off
        if (flg != 40){
          id.push_back(cid);
          if (flg == 9 || flg == 19) {
            evid.push_back(cevid-flg+1);
          } else {
            evid.push_back(cevid);
          }
          cmtF.push_back(cmt);
          time.push_back(ctime+dur);
          amt.push_back(-rate);
          ii.push_back(0.0);
          dv.push_back(NA_REAL);
          limit.push_back(NA_REAL);
          cens.push_back(0);

          idxInput.push_back(-1);
          ndose++;
        }
      } else {
        if (cevid != 0 && cevid != 2 && cevid != 9 && flg != 30 && ISNA(camt)) {
          stop(_("'amt' value NA for dose event; (id: %s, amt: %f, evid: %d rxode2 evid: %d, row: %d)"), CHAR(idLvl[cid-1]), camt, inEvid[i], cevid, (int)i+1);
        }
        amt.push_back(camt);
        if (addLagged) {
          nevidLag = cmt100*100000+rateI*10000+cmt99*100+1;
          id.push_back(cid);
          evid.push_back(nevidLag);
          cmtF.push_back(cmt);
          amt.push_back(camt);
          time.push_back(ctime);
          //ii.push_back(cii);
          ii.push_back(0.0);
          dv.push_back(NA_REAL);
          limit.push_back(NA_REAL);
          cens.push_back(0);

          idxInput.push_back(-1);
          ndose++;
        }
      }
      if (cii > 0 && caddl > 0) {
        if (!keepIIadl) {
          ii.pop_back();ii.push_back(0.0);
        }
        int cevidAddl = cevid;
        if (addlDropSs) {
          if (addLagged) {
            if (rateI != 8 && rateI != 9 && rateI != 6 && rateI != 7) {
              cevidAddl = nevid = cmt100*100000+rateI*10000+cmt99*100+1;
              flg = 0;
            } else {
              cevidAddl = cmt100*100000+rateI*10000+cmt99*100+1;
            }
            addLagged = false;
            keepIIadl=false;
          } else if (flg == 10 || flg == 20) {
            cevidAddl = cmt100*100000+rateI*10000+cmt99*100+1;
            keepIIadl=false;
          }
        }
        for (j=caddl;j--;) {
          ctime+=cii;
          id.push_back(cid);
          evid.push_back(cevidAddl);
          cmtF.push_back(cmt);
          time.push_back(ctime);
          if (keepIIadl) {
            ii.push_back(cii);
          } else {
            ii.push_back(0.0);
          }
          dv.push_back(NA_REAL);
          limit.push_back(NA_REAL);
          cens.push_back(0);

          if (addlKeepsCov) {
            idxInput.push_back(i);
          } else {
            idxInput.push_back(-1);
          }
          ndose++;
          if (rateI > 2 && rateI != 4 && rateI != 5) {
            amt.push_back(camt);
            if (addLagged) {
              id.push_back(cid);
              evid.push_back(nevidLag);
              cmtF.push_back(cmt);
              time.push_back(ctime);
              ii.push_back(0.0);
              dv.push_back(NA_REAL);
              limit.push_back(NA_REAL);
              cens.push_back(0);
              amt.push_back(camt);

              if (addlKeepsCov) {
                idxInput.push_back(i);
              } else {
                idxInput.push_back(-1);
              }
              ndose++;
            }
            // turn off
            id.push_back(cid);
            evid.push_back(nevid);
            cmtF.push_back(cmt);
            time.push_back(ctime);
            amt.push_back(camt);
            ii.push_back(0.0);
            dv.push_back(NA_REAL);
            limit.push_back(NA_REAL);
            cens.push_back(0);

            if (addlKeepsCov) {
              idxInput.push_back(i);
            } else {
              idxInput.push_back(-1);
            }
            ndose++;
          } else if (rateI == 1 || rateI == 2){
            amt.push_back(rate);
            if (addLagged) {
              id.push_back(cid);
              evid.push_back(cevid-flg+8);
              cmtF.push_back(cmt);
              amt.push_back(-rate);
              time.push_back(ctime);
              if (keepIIadl) {
                ii.push_back(cii);
              } else {
                ii.push_back(0.0);
              }
              //ii.push_back(0.0);
              dv.push_back(NA_REAL);
              limit.push_back(NA_REAL);
              cens.push_back(0);

              if (addlKeepsCov) {
                idxInput.push_back(i);
              } else {
                idxInput.push_back(-1);
              }
              ndose++;

              id.push_back(cid);
              evid.push_back(nevidLag);
              cmtF.push_back(cmt);
              time.push_back(ctime);
              ii.push_back(0.0);
              dv.push_back(NA_REAL);
              limit.push_back(NA_REAL);
              cens.push_back(0);
              amt.push_back(rate);

              if (addlKeepsCov) {
                idxInput.push_back(i);
              } else {
                idxInput.push_back(-1);
              }
              ndose++;
            }
            // turn off
            id.push_back(cid);
            if (flg == 9 || flg == 19) {
              evid.push_back(cevid-flg+1);
            } else {
              evid.push_back(cevidAddl);
            }
            cmtF.push_back(cmt);
            time.push_back(ctime+dur);
            amt.push_back(-rate);
            ii.push_back(0.0);
            dv.push_back(NA_REAL);
            limit.push_back(NA_REAL);
            cens.push_back(0);

            if (addlKeepsCov) {
              idxInput.push_back(i);
            } else {
              idxInput.push_back(-1);
            }
            ndose++;
          } else {
            amt.push_back(camt);
            if (addLagged) {
              id.push_back(cid);
              evid.push_back(nevidLag);
              cmtF.push_back(cmt);
              time.push_back(ctime);
              ii.push_back(0.0);
              dv.push_back(NA_REAL);
              limit.push_back(NA_REAL);
              cens.push_back(0);
              amt.push_back(camt);

              if (addlKeepsCov) {
                idxInput.push_back(i);
              } else {
                idxInput.push_back(-1);
              }
              ndose++;
            }
          }
        }
      }
    }
  }
  if (hasReset && isSorted) {
    // Here EVID=3 resets time
    // need to reset times here based on maxShift
    if (maxShift > 0) {
      maxShift += 0.1;
      lastId = NA_INTEGER;
      lastTime = time[0];
      double curShift = 0.0;
      for (int j = 0; j < (int)evid.size(); ++j) {
        if (lastId != id[j]) {
          lastId = id[j];
          curShift = 0.0;
          lastTime = time[j];
        }
        if (evid[j] == 3) {
          curShift += maxShift;
        }
        time[j] += curShift;
        lastTime = time[j];
      }
    }
  } else if (hasReset && !isSorted && maxShift > 0) {
    warning(_("there are evid=3/4 records in an incorrectly sorted dataset, system is reset, but time is not reset"));
    maxShift = 0.0;
  }
#ifdef rxSolveT
  REprintf("  Time5: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
  _lastT0 = clock();
#endif
  bool redoId=false;
  if (!keepDosingOnly){
    if (obsId.size() != allId.size()){
      std::string idWarn = "IDs without observations dropped:";
      for (j = allId.size(); j--;){
        if (std::find(obsId.begin(), obsId.end(), allId[j]) == obsId.end()){
          idWarn = idWarn + " " +as<std::string>(idLvl[allId[j]-1]);
          doseId.push_back(allId[j]);
        }
      }
      Rf_warningcall(R_NilValue, "%s", idWarn.c_str());
      redoId=true;
    }
  }
#ifdef rxSolveT
  REprintf("  Time6: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
  _lastT0 = clock();
#endif
  if (zeroId.size() != allId.size()){
    std::string idWarn = "IDs without zero-time start at the first observed time:";
    for (j = allId.size(); j--;){
      if (std::find(zeroId.begin(), zeroId.end(), allId[j]) == zeroId.end()){
        bool skipIt=false;
        if (!keepDosingOnly){
          // Excluded from list
          if (std::find(obsId.begin(), obsId.end(), allId[j]) == obsId.end()){
            skipIt=true;
          }
        }
        if (!skipIt){
          if (!_ini0){
            idWarn = idWarn + " " +as<std::string>(idLvl[allId[j]-1]);
          } else {
            id.push_back(allId[j]);
            evid.push_back(9);
            cmtF.push_back(0);
            time.push_back(0.0);
            amt.push_back(NA_REAL);
            ii.push_back(0.0);
            dv.push_back(NA_REAL);
            limit.push_back(NA_REAL);
            cens.push_back(0);

            idxInput.push_back(-1);
          }
        }
      }
    }
    if (!_ini0) Rf_warningcall(R_NilValue, "%s", idWarn.c_str());
  }
  if (warnCensNA) Rf_warningcall(R_NilValue, "%s", _("censoring missing 'DV' values do not make sense"));
  if (warnNaTime) Rf_warningcall(R_NilValue, "%s", _("missing 'TIME' values do not make sense (ignored)"));
#ifdef rxSolveT
  REprintf("  Time7: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
  _lastT0 = clock();
#endif
  IntegerVector ivId = wrap(id);
  NumericVector nvTime = wrap(time);
  IntegerVector ivEvid = clone(wrap(evid));

  if (!keepDosingOnly && doseId.size() > 0){
#define sortID if (ivEvid[j]==3){               \
      ivEvid[j] = NA_INTEGER+1;                 \
    } else if (amt[j] == 0){                    \
      ivEvid[j] = NA_INTEGER+2;                 \
    } else {                                    \
      ivEvid[j] = -ivEvid[j];                   \
    }
    for (int j = ivId.size(); j--; ){
      if (!(std::find(doseId.begin(), doseId.end(), ivId[j]) == doseId.end())){
        ivId[j] = NA_INTEGER; // Drop
      }
      // Duplicated below for speed.
      sortID
        }
  } else {
    for (int j = ivEvid.size(); j--; ){
      sortID
        }
  }
#undef sortID
  Function order = as<Function>(getForder());
  IntegerVector ord;
  IntegerVector ordI;
  if (useForder()){
    ord = order(ivId, nvTime, ivEvid,
                _["na.last"] = LogicalVector::create(true));
    ord = ord - 1;
    // na.last isn't =NA isn't quite working
    idxOutput = as<std::vector<int>>(ord);
    while (idxOutput.size() > 0 && IntegerVector::is_na(ivId[idxOutput.back()])){
      idxOutput.pop_back();
    }
    if (hasIcov) {
      ordI = order(inIdCov, _["na.last"] = LogicalVector::create(true));
      ordI = ordI-1;
      idxIcov = as<std::vector<int>>(ordI);
    }
  } else {
    ord = order(ivId, nvTime, ivEvid,
                _["na.last"] = LogicalVector::create(NA_LOGICAL),
                _["method"]="radix");
    ord = ord - 1;
    idxOutput = as<std::vector<int>>(ord);
    if (hasIcov) {
      ordI = order(inIdCov, _["na.last"] = LogicalVector::create(true),
                   _["method"]="radix");
      ordI = ordI-1;
      idxIcov = as<std::vector<int>>(ordI);
    }
  }
#ifdef rxSolveT
  REprintf("  Time8: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
  _lastT0 = clock();
#endif
  if (idxOutput.size()==0) stop(_("no rows in event table or input data"));
  lastId = id[idxOutput.back()]+42;
  int rmAmt = 0;
  // Remove trailing doses
  if (!keepDosingOnly){
    for (j =idxOutput.size(); j--; ){
      if (lastId != id[idxOutput[j]]){
        // New id
        lastId = id[idxOutput[j]];
        while (isDose(evid[idxOutput[j]]) && amt[idxOutput[j]] > 0 && j--){
          // keep negative doses or turning infusion off
          idxOutput[j+1] = -1;
          rmAmt++;
        }
        if (j <= 0) break;
      }
    }
    // Remove trailing dose from idO
    while(idxOutput.size() > 0 && idxOutput.back() == -1){
      idxOutput.pop_back();
      rmAmt--;
    }
  }
  if (idxOutput.size()-rmAmt <= 0) stop(_("empty data"));
  if (!keepDosingOnly){
    nid = obsId.size();
  } else {
    nid = allId.size();
  }
  NumericVector fPars = NumericVector(pars.size()*nid, NA_REAL);
  // sorted create the vectors/list
  if (addCmt && !hasCmt){
    baseSize = 7;
  } else {
    baseSize = 6;
  }
  int censAdd = 0;
  if (censCol != -1) censAdd=1;
  if (censAdd && censNone) {
    Rf_warningcall(R_NilValue, "%s", _("while censoring is included in dataset, no observations are censored"));
    censAdd=0;
  }
  if (swapDvLimit){
    Rf_warningcall(R_NilValue, "%s", _("'dv' and 'limit' swapped since 'limit' > 'dv'"));
  }
  int limitAdd = 0;
  if (limitCol != -1) limitAdd=1;

  List lst = List(baseSize+censAdd+limitAdd+covCol.size());
  std::vector<bool> sub0(baseSize+censAdd+limitAdd+covCol.size(), true);
  CharacterVector nme(baseSize+censAdd+limitAdd+covCol.size());
#ifdef rxSolveT
  REprintf("  Time9: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
  _lastT0 = clock();
#endif

  lst[0] = IntegerVector(idxOutput.size()-rmAmt);
  nme[0] = "ID";

  lst[1] = NumericVector(idxOutput.size()-rmAmt);
  nme[1] = "TIME";

  lst[2] = IntegerVector(idxOutput.size()-rmAmt);
  nme[2] = "EVID";

  lst[3] = NumericVector(idxOutput.size()-rmAmt);
  nme[3] = "AMT";

  lst[4] = NumericVector(idxOutput.size()-rmAmt);
  nme[4] = "II";

  lst[5] = NumericVector(idxOutput.size()-rmAmt);
  nme[5] = "DV";

  int cmtAdd = 0;
  if (baseSize == 7){
    lst[6] = IntegerVector(idxOutput.size()-rmAmt);
    nme[6] = "CMT";
    cmtAdd=1;
  }

  if (censAdd){
    lst[baseSize] = IntegerVector(idxOutput.size()-rmAmt);
    nme[baseSize] = "CENS";
    baseSize++;
  }

  if (limitAdd){
    lst[baseSize] = NumericVector(idxOutput.size()-rmAmt);
    nme[baseSize] = "LIMIT";
    baseSize++;
  }

  List lst1(1+covCol.size());
  CharacterVector nme1(1+covCol.size());
  std::vector<bool> sub1(1+covCol.size(), true);

  lst1[0] = IntegerVector(nid);
  nme1[0] = "ID";

  for (j = 0; j < (int)(covCol.size()); j++){
    int covColj = covCol[j];
    if (hasCmt && covColj >= 0 && j == cmtI) {
      lst[baseSize+j] = IntegerVector(idxOutput.size()-rmAmt);
      nme[baseSize+j] = pars[covParPos[j]];
      sub0[baseSize+j] = false;
      lst1[1+j] = IntegerVector(nid);
      nme1[1+j] = nme[baseSize+j];
      sub1[1+j] = true;
    } else {
      nvTmp = NumericVector(nid);
      nme[baseSize+j] = pars[covParPos[j]];
      if (covColj < 0) {
        // invariant covariates so don't allocate space
        if (allTimeVar) {
          lst1[1+j] = NumericVector(nid);
        } else {
          lst[baseSize+j] = NumericVector(0);
        }
        NumericVector curNV = iCov_[-covColj-1];
        for (int idx1c=curNV.size(); idx1c--;) {
          double vcur = curNV[idxIcov[idx1c]];
          fPars[idx1c*pars.size()+covParPos[j]] = vcur;
          nvTmp[idx1c] = vcur;
        }
      } else {
        lst[baseSize+j] = NumericVector(idxOutput.size()-rmAmt);
      }
      lst1[1+j] = nvTmp;
      sub0[baseSize+j] = false;
      nme1[1+j] = nme[baseSize+j];
      sub1[1+j] = true;
    }
  }
  List keepL = List(keepCol.size());
  CharacterVector keepN(keepCol.size());
  IntegerVector keepLc(keepCol.size());
  for (j = 0; j < (int)(keepCol.size()); j++){
    // size = the same size as the output with exception of the rmAmt or tr
    keepL[j] = NumericVector(idxOutput.size()-rmAmt);
    int keepColj = keepCol[j];
    const char* cmp;
    if (keepColj >= 0) {
      keepN[j] = dName[keepColj];
      cmp = CHAR(dName[keepColj]);
    } else {
      keepN[j] = diName[-keepColj-1];
      cmp = CHAR(diName[-keepColj-1]);
    }
    keepLc[j] = 0;
    for (int ip = pars.size(); ip--;){
      if (!strcmp(cmp, CHAR(pars[ip]))) {
        keepLc[j] = ip+1;
        break;
      }
    }
  }
#ifdef rxSolveT
  REprintf("  Time10: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
  _lastT0 = clock();
#endif
  IntegerVector ivTmp;
  lastId = NA_INTEGER;
  bool addId = false, added=false;
  int idx1=nid, nTv=0;
  std::vector<int> covParPosTV;
  bool cmtFadd = false;
  int jj = idxOutput.size()-rmAmt;
  int kk;
  List inDataFK(keepCol.size());
  List inDataFKL(keepCol.size());
  bool calcIcovKeepIdx = false;
  IntegerVector iCovKeepIdx;
  std::vector<int> covParInterpMv = as<std::vector<int>>(mv[RxMv_interp]);
  for (j = 0; j < (int)(keepCol.size()); j++) {
    int keepColj = keepCol[j];
    SEXP cur;
    if (keepColj >= 0) {
      cur = inData[keepColj];
    } else {
      // Need to create a new vector that as if it was a covariate from the data
      if (!calcIcovKeepIdx) {
        List inDf(2);
        inDf[0] = inId;
        IntegerVector inDfIdx(inId.size());
        std::iota(inDfIdx.begin(), inDfIdx.end(), 0);
        inDf[1]  = inDfIdx;
        inDf.attr("names") = CharacterVector::create("id", "idx0");
        inDf.attr("class") = "data.frame";
        inDf.attr("row.names") = IntegerVector::create(NA_INTEGER, -inId.size());
        List iDf(2);
        iDf[0] = inIdCov;
        IntegerVector iDfIdx(inIdCov.size());
        std::iota(iDfIdx.begin(), iDfIdx.end(), 0);
        iDf[1]  = iDfIdx;
        iDf.attr("names") = CharacterVector::create("id", "idxi");
        iDf.attr("class") = "data.frame";
        iDf.attr("row.names") = IntegerVector::create(NA_INTEGER, -inIdCov.size());
        Function getIcovIdx = getRxFn(".getIcovIdx");
        iCovKeepIdx = getIcovIdx(inDf, iDf);
      }
      SEXP cur2 = iCov_[-keepColj-1];
      if (TYPEOF(cur2) == STRSXP) {
        CharacterVector cur3 = as<CharacterVector>(cur2);
        CharacterVector cur4(iCovKeepIdx.size());
        for (int i = iCovKeepIdx.size(); i--;) {
          cur4[i] = cur3[iCovKeepIdx[i]];
        }
        cur = wrap(cur4);
        // save attributes
        Rf_copyMostAttrib(cur2, cur);
      } else if (TYPEOF(cur2) == INTSXP){
        // Create an integer vector for each element
        IntegerVector cur3 = as<IntegerVector>(cur2);
        IntegerVector cur4(iCovKeepIdx.size());
        for (int i = iCovKeepIdx.size(); i--;) {
          cur4[i] = cur3[iCovKeepIdx[i]];
        }
        cur = wrap(cur4);
        // save attributes
        Rf_copyMostAttrib(cur2, cur);
      } else if (TYPEOF(cur2) == REALSXP) {
        NumericVector cur3 = as<NumericVector>(cur2);
        NumericVector cur4(iCovKeepIdx.size());
        for (int i = iCovKeepIdx.size(); i--;) {
          cur4[i] = cur3[iCovKeepIdx[i]];
        }
        // save attributes
        std::vector<std::string> attr = cur3.attributeNames();
        for (int i = attr.size(); i--;) {
          cur4.attr(attr[i]) = cur3.attr(attr[i]);
        }
        cur = wrap(cur4);
        // save attributes
        Rf_copyMostAttrib(cur2, cur);
      } else if (TYPEOF(cur2) == LGLSXP) {
        LogicalVector cur3 = as<LogicalVector>(cur2);
        LogicalVector cur4(iCovKeepIdx.size());
        for (int i = iCovKeepIdx.size(); i--;) {
          cur4[i] = cur3[iCovKeepIdx[i]];
        }
        cur = wrap(cur4);
        // save attributes
        Rf_copyMostAttrib(cur2, cur);
      } else {
        stop(_("the columns that are kept must be either an underlying logical, string, factor, integer number, or real number"));
      }
    }
    RObject calc;
    List curType(3);
    if (TYPEOF(cur) == STRSXP){
      calc = convertId_(cur);
      curType[0] = IntegerVector::create(1);
      curType[1] = calc.attr("levels");
      curType[2] = etTranGetAttrKeep(cur);
      calc.attr("levels") = R_NilValue;
      calc.attr("class") = R_NilValue;
      inDataFK[j] = as<NumericVector>(calc);
      inDataFKL[j] = curType;
    } else if (TYPEOF(cur) == INTSXP){
      calc = cur;
      if (calc.hasAttribute("levels")) {
        // need to check type
        calc = clone(cur); // make sure they don't affect changes
        curType[0] = IntegerVector::create(2);
        curType[1] = calc.attr("levels");
        curType[2] = etTranGetAttrKeep(cur);
        calc.attr("levels") = R_NilValue;
        calc.attr("class") = R_NilValue;
        inDataFK[j] = as<NumericVector>(calc);
      } else {
        curType[0] = IntegerVector::create(3);
        curType[1] = R_NilValue;
        curType[2] = etTranGetAttrKeep(cur);
        inDataFK[j] = as<NumericVector>(calc);
      }
      inDataFKL[j] = curType;
    } else if (TYPEOF(cur) == REALSXP) {
      curType[0] = IntegerVector::create(4);
      curType[1] = R_NilValue;
      curType[2] = etTranGetAttrKeep(cur);
      inDataFK[j] = cur;
      inDataFKL[j] = curType;
    } else if (TYPEOF(cur) == LGLSXP) {
      curType[0]  = IntegerVector::create(5);
      curType[1]  = R_NilValue;
      curType[2] = etTranGetAttrKeep(cur);
      inDataFK[j] = as<NumericVector>(cur);
      inDataFKL[j] = curType;
    } else {
      stop(_("the columns that are kept must be either an underlying logical, string, factor, integer number, or real number"));
    }
  }
  int maxItemsPerId = 0;
  int curItems = 0;
  for (i =idxOutput.size(); i--;){
    if (idxOutput[i] != -1) {
      jj--;
      ivTmp = as<IntegerVector>(lst[0]);
      ivTmp[jj] = id[idxOutput[i]];
      if (lastId != id[idxOutput[i]]) {
        maxItemsPerId = max2(curItems, maxItemsPerId);
        curItems=0;
        addId=true;
        idx1--;
        if (idx1 < 0) stop(_("number of individuals not calculated correctly"));
        // Add ID
        ivTmp = as<IntegerVector>(lst1[0]);
        ivTmp[idx1] = id[idxOutput[i]];
        lastId=id[idxOutput[i]];
      }
      // retId[i]=id[idxOutput[i]];
      nvTmp = as<NumericVector>(lst[1]);
      // retTime[i]=time[idxOutput[i]];
      nvTmp[jj] = time[idxOutput[i]];
      ivTmp = as<IntegerVector>(lst[2]);
      ivTmp[jj] = evid[idxOutput[i]];
      // retEvid[i]=evid[idxOutput[i]];
      nvTmp = as<NumericVector>(lst[3]);
      // retAmt[i]=amt[idxOutput[i]];
      nvTmp[jj] = amt[idxOutput[i]];
      nvTmp = as<NumericVector>(lst[4]);
      nvTmp[jj]=ii[idxOutput[i]];
      nvTmp = as<NumericVector>(lst[5]);
      nvTmp[jj]=dv[idxOutput[i]];
      kk = 6;
      if (cmtAdd){
        ivTmp = as<IntegerVector>(lst[kk]);
        ivTmp[jj] = cmtF[idxOutput[i]];
        kk++;
      }
      if (censAdd){
        ivTmp = as<IntegerVector>(lst[kk]);
        ivTmp[jj] = cens[idxOutput[i]];
        kk++;
      }
      if (limitAdd){
        nvTmp = as<NumericVector>(lst[kk]);
        nvTmp[jj] = limit[idxOutput[i]];
        kk++;
      }
      // Now add the other items.
      added=false;
      if (idxInput[idxOutput[i]] == -1) {
        // not in data
        for (j = 0; j < (int)(keepCol.size()); j++){
          nvTmp = as<NumericVector>(keepL[j]);
          nvTmp[jj] = NA_REAL;
        }
      } else {
        for (j = 0; j < (int)(keepCol.size()); j++){
          // idxOutput is the output id
          nvTmp = as<NumericVector>(keepL[j]);
          // These keep variables are added.
          SEXP cur = inDataFK[j];
          nvTmp[jj] = REAL(cur)[idxInput[idxOutput[i]]];
        }
      }
      for (j = 0; j < (int)(covCol.size()); j++){
        int covColj = covCol[j];
        if (hasCmt && covColj >= 0 &&j == cmtI) {
          ivTmp = as<IntegerVector>(lst[baseSize+j]);
          ivTmp[jj] = cmtF[idxOutput[i]];
          if (!cmtFadd){
            sub0[baseSize+j] = true;
            sub1[1+j] = false;
            covParPosTV.push_back(covParPos[j]);
            cmtFadd=true;
            nTv++;
          }
        } else {
          if (!allTimeVar && covColj < 0) continue;
          nvTmp = as<NumericVector>(lst[baseSize+j]);
          if (idxInput[idxOutput[i]] == -1){
            // These should be ignored for interpolation.
            nvTmp[jj] = NA_REAL;
          } else {
            // These covariates are added.
            SEXP cur;
            if (covColj >= 0) {
              // This comes from the original data
              cur = inData[covColj];
            } else {
              // this comes from the individual covariate table
              cur = iCov_[-covColj-1];
            }
            if (TYPEOF(cur) == STRSXP) {
              // Strings are converted to numbers
              cur = inDataF[j];
            }
            nvTmp2   = as<NumericVector>(cur);
            if (covColj < 0) {
              // By definition, not time varying, no need to check
              // nvTmp2 in this case is the column sorted by id
              // so we get the current
              // idx1 = the index of the current id
              if (allTimeVar) {
                nvTmp[jj] = nvTmp2[idxIcov[idx1]];
              }
              // nvTmp = as<NumericVector>(lst1[1+j]);
              // double vCur = nvTmp2[idx1];
              // nvTmp[idx1] = vCur;
              // fPars[idx1*pars.size()+covParPos[j]] = nvTmp[idx1];
            } else {
              nvTmp[jj] = nvTmp2[idxInput[idxOutput[i]]];
              if (addId) {
                nvTmp = as<NumericVector>(lst1[1+j]);
                int iCur = i;
                int idxOut = i;
                int idxIn = i;
                double vCur = nvTmp2[idxInput[idxOutput[i]]];
                // Could be NA, look for non NA value OR beginning of subject
                while (ISNA(vCur) && iCur != 0 &&
                       id.size() > (idxOut = idxOutput[iCur]) &&
                       idxOut >= 0 &&
                       lastId == id[idxOut] &&
                       idxInput.size() > idxOut &&
                       nvTmp2.size() > (idxIn = idxInput[idxOut]) &&
                       idxIn >= 0) {
                  vCur = nvTmp2[idxIn];
                  iCur--;
                }
                if (ISNA(vCur)) iCur = i;
                while (ISNA(vCur) && iCur+1 != (int)(covCol.size()) &&
                       idxOutput.size() < iCur+1 &&
                       id.size() > (idxOut = idxOutput[iCur+1]) &&
                       idxOut >= 0 &&
                       lastId == id[idxOut] &&
                       idxInput.size() > idxOut &&
                       nvTmp2.size() > (idxIn = idxInput[idxOut]) &&
                       idxIn >= 0) {
                  vCur = nvTmp2[idxIn];
                  iCur++;
                }
                if (ISNA(vCur)) {
                  Rf_warningcall(R_NilValue,
                                 "column '%s' has only 'NA' values for id '%s'" , CHAR(nme1[1+j]),
                                 CHAR(idLvl[((inId.size() == 0) ? 1 : lastId)-1]));
                }
                nvTmp[idx1] = vCur;
                fPars[idx1*pars.size()+covParPos[j]] = nvTmp[idx1];
                added = true;
              } else if (sub1[1+j]) {
                nvTmp = as<NumericVector>(lst1[1+j]);
                //double cur1 = nvTmp[idx1];
                double cur2 = nvTmp2[idxInput[idxOutput[i]]];
                if (!ISNA(cur2) && nvTmp[idx1] != cur2){
                  sub0[baseSize+j] = true;
                  sub1[1+j] = false;
                  fPars[idx1*pars.size()+covParPos[j]] = NA_REAL;
                  if (std::find(covParPosTV.begin(), covParPosTV.end(), covParPos[j]) == covParPosTV.end()){
                    covParPosTV.push_back(covParPos[j]);
                  }
                  nTv++;
                }
              }
            }
          }
        }
      }
      if (added && addId){
        addId=false;
        added=false;
      }
      curItems++;
    }
  }
  maxItemsPerId = max2(curItems, maxItemsPerId);
#ifdef rxSolveT
  REprintf("  Time11: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
  _lastT0 = clock();
#endif

  if (!dropUnits && addTimeUnits){
    NumericVector tmpN = as<NumericVector>(lst[1]);
    Rf_setAttrib(tmpN, R_ClassSymbol, wrap("units"));
    tmpN.attr("units") = timeUnits;
  }
  if (!dropUnits && addAmtUnits){
    NumericVector tmpN = as<NumericVector>(lst[3]);
    Rf_setAttrib(tmpN, R_ClassSymbol, wrap("units"));
    tmpN.attr("units") = amtUnits;
  }
  // Now subset based on time-varying covariates
  List lstF;
  CharacterVector nmeF;
  if (allTimeVar){
    lstF = List(baseSize+covCol.size());
    nmeF = CharacterVector(baseSize+covCol.size());
  } else {
    lstF = List(baseSize+nTv);
    nmeF = CharacterVector(baseSize+nTv);
  }
  j=0;
  for (i = 0; i < lst.size();i++){
    if (allTimeVar || sub0[i]){
      lstF[j]=lst[i];
      nmeF[j]=nme[i];
      j++;
    }
  }
  j=0;
  List lst1F(1+covCol.size()-nTv);
  CharacterVector nme1F(1+covCol.size()-nTv);
  for (i = 0; i < lst1.size();i++){
    if (sub1[i]){
      lst1F[j]=lst1[i];
      nme1F[j]=nme1[i];
      j++;
    }
  }
  CharacterVector cls = CharacterVector::create("rxEtTran","data.frame");
  if (covCol.size() == 0 && !rxIsInt(lst1F[0]) && !redoId){
    stop(_("corrupted event table"));
  }
#ifdef rxSolveT
  REprintf("  Time12: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
  _lastT0 = clock();
#endif
  IntegerVector tmp = lst1F[0];
  CharacterVector idLvl2;
  if (redoId){
    Rf_setAttrib(tmp, R_ClassSymbol, wrap("factor"));
    Rf_setAttrib(tmp, R_LevelsSymbol, idLvl);
    tmp = convertId_(tmp);//as<IntegerVector>();
    idLvl2 = tmp.attr("levels");
    Rf_setAttrib(tmp, R_ClassSymbol, R_NilValue);
    Rf_setAttrib(tmp, R_LevelsSymbol, R_NilValue);
    lst1F[0] = tmp;
  }

#ifdef rxSolveT
  REprintf("  Time13: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
  _lastT0 = clock();
#endif

  Rf_setAttrib(lst1F, R_NamesSymbol, nme1F);
  Rf_setAttrib(lst1F, R_ClassSymbol, wrap("data.frame"));
  Rf_setAttrib(lst1F, R_RowNamesSymbol,
               IntegerVector::create(NA_INTEGER, -nid));
  List e(30);
  RxTransNames;
  e[RxTrans_ndose] = IntegerVector::create(ndose);
  e[RxTrans_nobs]  = IntegerVector::create(nobs);
  e[RxTrans_nid]   = IntegerVector::create(nid);
  e[RxTrans_cov1] = lst1F;
  e[RxTrans_covParPos]  = wrap(covParPos);
  e[RxTrans_covParPosTV] = wrap(covParPosTV); // Time-varying pos
  // We minus 2 here because that way that the covariates
  // interpolation will match the interpolation method defined in
  // the rxControl() object
  if (allTimeVar){
    e[RxTrans_sub0] = wrap(sub0);
    e[RxTrans_baseSize] = baseSize;
    e[RxTrans_nTv] = IntegerVector::create(nTv);
    e[RxTrans_lst] = lst;
    e[RxTrans_nme] = nme;
  } else {
    e[RxTrans_sub0] = R_NilValue;
    e[RxTrans_baseSize] = R_NilValue;
    e[RxTrans_nTv] = R_NilValue;
    e[RxTrans_lst] = R_NilValue;
    e[RxTrans_nme] = R_NilValue;
  }
  std::vector<int> covParPos0;
  // flag the time-varying covariates
  for (j = covParPos.size();j--;) {
    if (std::find(covParPosTV.begin(), covParPosTV.end(), covParPos[j]) == covParPosTV.end()) {
      covParPos0.push_back(covParPos[j]);
    }
  }
  e[RxTrans_covParPos0] = wrap(covParPos0);
  e[RxTrans_covUnits] = covUnits;
  Rf_setAttrib(fPars, R_DimSymbol,
               IntegerVector::create(pars.size(), nid));
  Rf_setAttrib(fPars, R_DimNamesSymbol,
               List::create(pars, R_NilValue));
  e[RxTrans_pars] = fPars;
  e[RxTrans_allBolus] = allBolus;
  e[RxTrans_allInf] = allInf;
  e[RxTrans_mxCmt] = mxCmt;
  e[RxTrans_lib_name] = trans["lib.name"]; // FIXME
  e[RxTrans_addCmt] = addCmt;
  e[RxTrans_cmtInfo] = cmtInfo;
  if (redoId){
    e[RxTrans_idLvl] = idLvl2;
  } else {
    e[RxTrans_idLvl] = idLvl;
  }
  e[RxTrans_allTimeVar] = allTimeVar;
  e[RxTrans_keepDosingOnly] = true;
  e[RxTrans_censAdd] = censAdd;
  e[RxTrans_limitAdd] = limitAdd;
  e[RxTrans_levelInfo] = inDataLvl;
  e[RxTrans_idInfo] = idInt;
  e[RxTrans_maxShift] = maxShift;
  Rf_setAttrib(keepL, R_NamesSymbol, keepN);
  Rf_setAttrib(keepL, R_ClassSymbol, wrap("data.frame"));
  Rf_setAttrib(keepL, R_RowNamesSymbol,
               IntegerVector::create(NA_INTEGER,-idxOutput.size()+rmAmt));
  Rf_setAttrib(keepL, Rf_install("keepCov"), wrap(keepLc));
  e[RxTrans_keepL] = List::create(_["keepL"]=keepL, _["keepLtype"]=inDataFKL);
  e[RxTrans_maxItemsPerId] = maxItemsPerId;
  Rf_setAttrib(e, R_ClassSymbol, wrap("rxHidden"));
  cls.attr(".rxode2.lst") = e;
  tmp = lstF[0];
  if (redoId){
    Rf_setAttrib(tmp, R_ClassSymbol, wrap("factor"));
    Rf_setAttrib(tmp, R_LevelsSymbol, idLvl);
    tmp = convertId_(tmp);
    //as<IntegerVector>();
    Rf_setAttrib(tmp, R_ClassSymbol, R_NilValue);
    Rf_setAttrib(tmp, R_LevelsSymbol, R_NilValue);
    lstF[0]=tmp;
  }
  if (!dropUnits){
    Rf_setAttrib(tmp, R_ClassSymbol, wrap("factor"));
    if (redoId){
      Rf_setAttrib(tmp, R_LevelsSymbol, idLvl2);
    } else {
      Rf_setAttrib(tmp, R_LevelsSymbol, idLvl);
    }
  }
#ifdef rxSolveT
  REprintf("  Time14: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
  _lastT0 = clock();
#endif
  Rf_setAttrib(lstF, R_NamesSymbol, nmeF);
  Rf_setAttrib(lstF, R_ClassSymbol, cls);
  Rf_setAttrib(lstF, R_RowNamesSymbol, IntegerVector::create(NA_INTEGER,-idxOutput.size()+rmAmt));
  if (doWarnNeg){
    if (!warnedNeg){
      Rf_warningcall(R_NilValue, "%s", _("\nwith negative times, compartments initialize at first negative observed time\nwith positive times, compartments initialize at time zero\nuse 'rxSetIni0(FALSE)' to initialize at first observed time\nthis warning is displayed once per session"));
      warnedNeg=true;
    }
  }
#ifdef rxSolveT
  REprintf("  Time15: %f\n", ((double)(clock() - _lastT0))/CLOCKS_PER_SEC);
  _lastT0 = clock();
#endif
  if (wKeep != "")
    Rf_warningcall(R_NilValue, "%s", wKeep.c_str());
  return lstF;
}


//[[Rcpp::export]]
List rxEtTransAsDataFrame_(List inData1) {
  List inData = clone(inData1);
  CharacterVector cls = Rf_getAttrib(inData, R_ClassSymbol);
  List e = cls.attr(".rxode2.lst");
  double maxShift = as<double>(e[RxTrans_maxShift]);
  if (maxShift > 0) {
  IntegerVector id = as<IntegerVector>(inData[0]);
  NumericVector time = as<NumericVector>(inData[1]);
  IntegerVector evid = as<IntegerVector>(inData[2]);
  int lastId = NA_INTEGER;
  double curShift = 0.0;
  for (int j = 0; j < (int)evid.size(); ++j) {
    if (lastId != id[j]) {
      lastId = id[j];
      curShift = 0.0;
      //lastTime = time[j];
    }
    if (evid[j] == 3) {
      curShift -= maxShift;
    }
    time[j] += curShift;
    //lastTime = time[j];
    }
  }
  cls = CharacterVector::create("data.frame");
  Rf_setAttrib(inData, R_ClassSymbol, cls);
  return inData;
}
