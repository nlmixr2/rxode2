#ifndef __rxode2parseConvertMethod_h__
#define __rxode2parseConvertMethod_h__
IntegerVector convertMethod(RObject method){
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
#endif
