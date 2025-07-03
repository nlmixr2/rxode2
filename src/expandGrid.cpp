// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: t; -*-
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
// [[Rcpp::interfaces(r,cpp)]]
//#undef NDEBUG
#include <Rcpp.h>
using namespace Rcpp;

#define _(String) (String)
bool rxIs(const RObject &obj, std::string cls);
Function getRxFn(std::string name);

std::string symengineRes(std::string val);
//' Expand grid internal function
//'
//' @param c1 character vector of items to be expanded
//' @param c2 second character vector of items to be expanded
//' @param type 0 for a typical data frame, 1 for symengine sensitivity expansion
//' @return data frame (when type = 0) or symengine string (when type=1)
//' @export
//' @keywords internal
//[[Rcpp::export]]
List rxExpandGrid_(RObject &c1, RObject &c2, RObject &type){
  if (rxIs(c1, "character") && rxIs(c2, "character")){
    CharacterVector in1 = as<CharacterVector>(c1);
    CharacterVector in2 = as<CharacterVector>(c2);
    int len1 = in1.size();
    int len2 = in2.size();
    int lenF = len1*len2;
    CharacterVector out1(lenF);
    CharacterVector out2(lenF);
    int i1, i2;
    int iType = as<int>(type);
    if (iType == 0){
      for (int i = lenF; i--;){
        i1 = i % len1;
        i2 = std::floor(i / len1);
        out1[i] = in1[i1];
        out2[i] = in2[i2];
      }
      List out(2);
      out[0] = out1;
      out[1] = out2;
      out.attr("class") = "data.frame";
      out.attr("row.names") = IntegerVector::create(NA_INTEGER, -lenF);
      out.attr("names") = CharacterVector::create("Var1","Var2");
      return out;
    } else if (iType == 1) {
      CharacterVector out3(lenF);
      CharacterVector out4(lenF);
      CharacterVector out5(lenF);
      for (int i = lenF; i--;){
        i1 = i % len1;
        i2 = std::floor(i / len1);
        std::string s1 = as<std::string>(in1[i1]);
        std::string s2 = as<std::string>(in2[i2]);
        out1[i] = s1;
        out2[i] = s2;
        out3[i] = "df(" + s1 + ")/dy(" + s2 + ")";
        std::string sDf = "rx__df_" + s1 + "_dy_" + s2 + "__";
        out4[i] = sDf;
        out5[i] = "assign(\"" + sDf + "\",with(model,D(rx__d_dt_" +
          s1 + "__, \"" + symengineRes(s2) + "\")), envir=model)";
      }
      List out(5);
      out[0] = out1;
      out[1] = out2;
      out[2] = out3;
      out[3] = out4;
      out[4] = out5;
      out.attr("class") = "data.frame";
      out.attr("row.names") = IntegerVector::create(NA_INTEGER, -lenF);
      out.attr("names") = CharacterVector::create("s1", "s2", "rx", "sym", "line");
      return out;
    }
  } else {
    stop(_("unanticipated input for rxExpandGrid_"));
  }
  List tmp = List(0);
  return tmp;
}
//' Expand sensitivity
//'
//' @param state is the state to expand
//' @param calcSens is the variables to calculate sensitivity
//' @keywords internal
//' @return symengine string for expanded sensitivity
//' @export
//[[Rcpp::export]]
List rxExpandSens_(CharacterVector state, CharacterVector calcSens){
  // rx__d_dt_rx__sens_V1_BY_V2____
  int len1 = state.size();
  int len2 = calcSens.size();
  int lenF = len1*len2;
  int i1, i2;
  CharacterVector ddt(lenF);
  CharacterVector ddtS(lenF);
  CharacterVector ddtS2(lenF);
  CharacterVector line(lenF);
  CharacterVector state0(lenF);
  CharacterVector state0r(lenF);
  CharacterVector stateD(lenF);
  CharacterVector rateS(lenF);
  CharacterVector rateR(lenF);
  CharacterVector durS(lenF);
  CharacterVector durR(lenF);
  CharacterVector lagS(lenF);
  CharacterVector lagR(lenF);
  CharacterVector FS(lenF);
  CharacterVector FR(lenF);
  for (int i = lenF; i--;){
    //"d/dt(rx__sens_%s_BY_%s__)"
    i1 = i % len1;
    i2 = std::floor(i / len1);
    std::string curState = as<std::string>(state[i1]);
    std::string curSens = as<std::string>(calcSens[i2]);
    std::string sensSp = "rx__sens_" + curState + "_BY_" + curSens + "__";
    ddt[i] = "d/dt("+ sensSp + ")";
    std::string curLine= "rx__d_dt_rx__sens_" + curState + "_BY_" +
      curSens + "____";
    ddtS[i] = curLine;
    ddtS2[i] = sensSp;
    curLine = "assign(\"" + curLine + "\", with(model,";
    for (int j = len1; j--;){
      std::string curState2=as<std::string>(state[j]);
      // sprintf("df(%s)/dy(%s)*rx__sens_%s_BY_%s__", s1, rxToSymPy(s2), s2, rxToSymPy(sns))
      curLine += "rx__df_"+curState+"_dy_"+curState2+
        "__*rx__sens_"+curState2+"_BY_"+curSens+"__+";
    }
    curLine += "rx__df_"+curState+ "_dy_"+curSens+"__),envir=model)";
    line[i] = curLine;
    std::string tmp = "rx_" + curState  + "_ini_0__";
    state0[i] = tmp;
    state0r[i] = sensSp + "(0)";
    stateD[i] = "assign(\"rx_"+sensSp+"_ini_0__\",with(model,D(" +
      tmp + ",\"" + symengineRes(curSens) + "\")),envir=model)";
    rateS[i] = "rx_rate_" + curState + "_";
    rateR[i] = "rate(" + curState + ")";
    durS[i] = "rx_dur_" + curState + "_";
    durR[i] = "dur(" + curState + ")";
    lagS[i] = "rx_lag_" + curState + "_";
    lagR[i] = "lag(" + curState + ")";
    FS[i] = "rx_f_" + curState + "_";
    FR[i] = "f(" + curState + ")";
  }
  List out(15);
  out[0] = ddt;
  out[1] = ddtS;
  out[2] = line;
  out[3] = state0;
  out[4] = stateD;
  out[5] = state0r;
  out[6] = ddtS2;
  out[7] = rateS;
  out[8] = rateR;
  out[9] = durS;
  out[10] = durR;
  out[11] = lagS;
  out[12] = lagR;
  out[13] = FS;
  out[14] = FR;
  out.attr("names") = CharacterVector::create("ddt","ddtS","line","s0", "s0D","s0r", "ddS2",
                                              "rateS","rateR", "durS","durR","lagS", "lagR",
                                              "fS","fR");
  out.attr("class") = "data.frame";
  out.attr("row.names") = IntegerVector::create(NA_INTEGER, -lenF);
  return out;
}

//' Expand second order sensitivity
//'
//' @param state is the state to expand
//' @param s1 is the variables to calculate sensitivity
//' @param s2 is the variables to calculate second order sensitivity
//' @keywords internal
//' @return string for symengine second order sensitivity
//' @export
//[[Rcpp::export]]
List rxExpandSens2_(CharacterVector state, CharacterVector s1, CharacterVector s2){
  // rx__d_dt_rx__sens_V1_BY_V2____
  int len1 = state.size();
  int len2 = s1.size();
  int len3 = s2.size();
  int lenF = len1*len2*len3;
  CharacterVector ddt(lenF);
  CharacterVector ddtS(lenF);
  CharacterVector ddtS2(lenF);
  CharacterVector line(lenF);
  CharacterVector state0r(lenF);
  CharacterVector stateD(lenF);
  CharacterVector s0(lenF);
  int i1, i2, i3;
  for (int i = lenF; i--;){
    i1 = i % len1;
    i2 = (int)(std::floor(i / len1)) % len2;
    i3 = std::floor(i / (len1*len2));
    std::string cS = as<std::string>(state[i1]);
    std::string cS1 = as<std::string>(s1[i2]);
    std::string cS2 = as<std::string>(s2[i3]);
    std::string sensSp = "rx__sens_" + cS + "_BY_" +
      cS1 + "_BY_" + cS2 + "__";
    ddt[i] = "d/dt("+ sensSp + ")";
    std::string curLine= "rx__d_dt_rx__sens_" + cS +
      "_BY_" + cS1 + "_BY_" + cS2 + "____";
    ddtS[i] = curLine;
    ddtS2[i] = sensSp;
    curLine = "assign(\"" + curLine + "\", with(model,";
    std::string v1 = "rx__d_dt_rx__sens_"+cS+"_BY_"+
      cS2+"____";
    curLine += "D("+v1+",\""+symengineRes(cS1)+"\")";
    for (int j = len1; j--;){
      std::string s2 = as<std::string>(state[j]);
      curLine += "+D("+v1+",\""+symengineRes(s2)+"\")*rx__sens_"+s2+"_BY_"+cS1+
        "__+rx__sens_"+s2+"_BY_"+cS1+"_BY_"+cS2+
        "__*rx__df_"+
        cS + "_dy_"+s2+"__";
    }
    curLine += "),envir=model)";
    line[i] = curLine;
    state0r[i] = sensSp + "(0)";
    s0[i] = "rx_" + cS + "_ini_0__";
    stateD[i] = "assign(\"rx_"+sensSp+"_ini_0__\",with(model,D(D(rx_" +
      cS + "_ini_0__,\"" + symengineRes(cS1) + "\"),\""+symengineRes(cS2)+
      "\")),envir=model)";
  }
  List out(7);
  out[0] = ddt;
  out[1] = ddtS;
  out[2] = ddtS2;
  out[3] = line;
  out[4] = state0r;
  out[5] = stateD;
  out[6] = s0;
  out.attr("names") = CharacterVector::create("ddt","ddtS","ddS2","line","s0r","s0D","s0");
  out.attr("class") = "data.frame";
  out.attr("row.names") = IntegerVector::create(NA_INTEGER, -lenF);
  return out;
}

//' Expand d(f)/d(eta)
//'
//' @param state is the state to expand
//' @param neta is the number of etas
//' @param pred type of prediction
//' @param isTheta logical, is the expansion actually for thetas instead of etas
//' @keywords internal
//' @return String of symengine expressions to evaluate to calculate df/deta
//' @export
//[[Rcpp::export]]
List rxExpandFEta_(CharacterVector state, int neta, int pred, bool isTheta=false){
  CharacterVector fe(neta);
  CharacterVector calcS(neta);
  int nstate = state.size();
	std::string th = "";
	if (isTheta) th = "TH";
  for (int i = 0; i < neta; i++){
    std::string etaN = std::to_string(i+1);
    std::string feta;
    std::string calc;

    switch(pred){
    case 2:
      feta = "rx__sens_rx_pred__BY_" + th + "ETA_" +
        etaN + "___";
      calc = "assign(\"" + feta + "\",with(.s,-D(rx_pred_, " + th + "ETA_" +
        etaN + "_)";
      for (int j = nstate; j--;){
        calc += "-rx__sens_" + as<std::string>(state[j]) + "_BY_" + th + "ETA_" + etaN +
          "___*D(rx_pred_,\""+ symengineRes(as<std::string>(state[j])) + "\")";
      }
      calc += "), envir=.s)";
      break;
    case 1:
      feta = "rx__sens_rx_pred__BY_" + th + "ETA_" +
        etaN + "___";
      calc = "assign(\"" + feta + "\",with(.s,D(rx_pred_, " + th + "ETA_" +
        etaN + "_)";
      for (int j = nstate; j--;){
        calc += "+rx__sens_" + as<std::string>(state[j]) + "_BY_" + th + "ETA_" + etaN +
          "___*D(rx_pred_,"+ symengineRes(as<std::string>(state[j])) + ")";
      }
      calc += "), envir=.s)";
      break;
    case 0:
      feta = "rx__sens_rx_r__BY_" + th + "ETA_" +
        etaN + "___";
      calc = "assign(\"" + feta + "\",with(.s,D(rx_r_," + th +"ETA_" +
        etaN + "_)";
      for (int j = nstate; j--;){
        calc += "+rx__sens_" + as<std::string>(state[j]) + "_BY_" + th + "ETA_" + etaN +
          "___*D(rx_r_,"+ symengineRes(as<std::string>(state[j])) + ")";
      }
      calc += "), envir=.s)";
      break;
    }
    fe[i] = feta;
    calcS[i] = calc;
  }
  List out(2);
  out[0] = fe;
  out[1] = calcS;
  out.attr("names") = CharacterVector::create("dfe","calc");
  out.attr("class") = "data.frame";
  out.attr("row.names") = IntegerVector::create(NA_INTEGER, -neta);
  return out;
}

//' Rep R0 for foce
//'
//' @param neta ETA to substitute
//'
//' @return Returns a string of R code to substitute the rx_r expression in the symengine environment .s
//'
//' @keywords internal
//'
//' @export
//[[Rcpp::export]]
std::string rxRepR0_(int neta){
  std::string cur = "rx_r_";
  for (int j = neta; j--;){
    cur = "symengine::subs(" + cur+ ",ETA_" + std::to_string(j+1) + "_,..s0)";
  }
  cur = "assign(\"rx_r_\",with(.s,"+cur+"),envir=.s)";
  return cur;
}

List rxModelVars_(const RObject &obj);

// Expands nesting for theta/eta;
//
// @param thetaNest
//
// @noRd
void rxExpandNestingRep(CharacterVector &thetaNest,
                        CharacterVector &thetaNestTran,
                        CharacterVector &thetaNestFull,
                        int &thCnt, int &curtheta,
                        List &aboveVars, NumericVector& above,
                        std::string &retS,
                        List &data, std::string thetaVar = "THETA[") {
  std::string theta;
  int lastTheta;
  int firstTheta = curtheta;
  for (int j = 0; j < thetaNest.size(); ++j) {
    std::string curNest = as<std::string>(thetaNest[j]);
    CharacterVector nestVars = as<CharacterVector>(aboveVars[curNest]);
    RObject curNestV = as<RObject>(data[curNest]);
    CharacterVector curNestLvl = curNestV.attr("levels");
    int nnest = as<int>(above[curNest]);
    // This is the base theta count
    lastTheta = thCnt;
    for (int i = 0; i < nestVars.size(); ++i) {
      std::string curPar = as<std::string>(nestVars[i]);
      retS += curPar + "=";
      for (int k = 0; k < nnest; ++k) {
        theta = thetaVar + std::to_string(lastTheta+k+i*nnest+firstTheta) + "]";
        retS += "(" + curNest + "==" + std::to_string(k+1)+")*" + theta;
        thetaNestTran[thCnt] = curPar + "(" + curNest + "==" +
          as<std::string>(curNestLvl[k])+")";
        thetaNestFull[thCnt] = theta;
        curtheta++; thCnt++;
        if (k != nnest-1) retS += "+";
        else retS += ";\n";
      }
    }
  }
  thetaNestTran.attr("names") = thetaNestFull;
}


//[[Rcpp::export]]
List rxExpandNesting(const RObject& obj, List& nestingInfo,
                     bool compile=false){
  std::string retS="";
  List mv = rxModelVars_(obj);
  IntegerVector flags = as<IntegerVector>(mv["flags"]);
  int cureta = as<int>(flags["maxeta"])+1;
  int curtheta = as<int>(flags["maxtheta"])+1;
  int extraTheta = as<int>(nestingInfo["extraTheta"]);
  int extraEta = as<int>(nestingInfo["extraEta"]);
  CharacterVector thetaNames(extraTheta);
  CharacterVector etaNames(extraEta);
  List aboveVars = as<List>(nestingInfo["aboveVars"]);
  NumericVector above;
  if (!rxIs(nestingInfo["above"], "NULL")) {
    above = as<NumericVector>(nestingInfo["above"]);
  }
  List data = as<List>(nestingInfo["data"]);
  CharacterVector thetaNest;
  Nullable<CharacterVector> thetaNest0 = aboveVars.attr("names");
  if (!thetaNest0.isNull()) {
    thetaNest=CharacterVector(thetaNest0);
  }
  CharacterVector thetaNestTran(extraTheta);
  CharacterVector thetaNestFull(extraTheta);

  List belowVars = as<List>(nestingInfo["belowVars"]);
  NumericVector below;
  if (!rxIs(nestingInfo["below"], "NULL")) {
    below = as<NumericVector>(nestingInfo["below"]);
  }
  CharacterVector etaNest;
  Nullable<CharacterVector> etaNest0 = belowVars.attr("names");
  if (!etaNest0.isNull()) {
    etaNest=CharacterVector(etaNest0);
  }
  CharacterVector etaNestTran(extraEta);
  CharacterVector etaNestFull(extraEta);
  int thCnt=0;
  std::string idName=nestingInfo["idName"];

  if (thetaNest.size() > 0) {
    rxExpandNestingRep(thetaNest, thetaNestTran, thetaNestFull,
                       thCnt, curtheta, aboveVars, above, retS,  data,
                       "THETA[");
  } else if (etaNest.size() == 0) {
    // const RObject& obj, List& nestingInfo,
    //         bool compile=false
    CharacterVector blank;
    blank.attr("names") = CharacterVector::create();
    return List::create(_["mod"] = obj,
                        _["theta"] = blank,
                        _["eta"] = blank);
  }

  int etCnt = 0;
  rxExpandNestingRep(etaNest, etaNestTran, etaNestFull,
                     etCnt, cureta,
                     belowVars, below, retS, data,
                     "ETA[");
  CharacterVector mod = mv["model"];
  List ret(3);
  retS += as<std::string>(mod[0]);
  if (compile){
    Function rxode2 = getRxFn("rxode2");
    RObject retO = rxode2(retS);
    ret[0] = retO;
  } else {
    ret[0] = retS;
  }
  ret[1] = thetaNestTran;
  ret[2] = etaNestTran;
  ret.attr("names") = CharacterVector::create("mod","theta","eta");
  return(ret);
}
