#define R_NO_REMAP
#define USE_FC_LEN_T
//#undef NDEBUG
#define STRICT_R_HEADERS
#include <Rcpp.h>
using namespace Rcpp;
// bool rxIs(const RObject &obj, std::string cls);

bool replace1(std::string& str, const std::string& from, const std::string& to) {
  // https://stackoverflow.com/questions/3418231/replace-part-of-a-string-with-another-string#3418285
  size_t start_pos = str.find(from);
  if(start_pos == std::string::npos)
    return false;
  if (start_pos == 0){
    size_t fLen = from.length();
    size_t strLen = str.length();
    if (start_pos+fLen == strLen){
      str.replace(start_pos, from.length(), to);
      return true;
    } else if (str[start_pos+fLen] == '(' ||
	       str[start_pos+fLen] == ')' ||
	       str[start_pos+fLen] == '+' ||
	       str[start_pos+fLen] == '*' ||
	       str[start_pos+fLen] == '/' ||
	       str[start_pos+fLen] == '-' ||
	       str[start_pos+fLen] == '^' ||
	       str[start_pos+fLen] == '=' ||
	       str[start_pos+fLen] == '<' ||
	       str[start_pos+fLen] == '>' ||
	       str[start_pos+fLen] == '&' ||
	       str[start_pos+fLen] == '|') {
      str.replace(start_pos, from.length(), to);
      return true;
    }
  } else if (str[start_pos-1] == '(' ||
	     str[start_pos-1] == ')' ||
	     str[start_pos-1] == '+' ||
	     str[start_pos-1] == '*' ||
	     str[start_pos-1] == '/' ||
	     str[start_pos-1] == '-' ||
	     str[start_pos-1] == '^' ||
	     str[start_pos-1] == '=' ||
	     str[start_pos-1] == '<' ||
	     str[start_pos-1] == '>' ||
	     str[start_pos-1] == '&' ||
	     str[start_pos-1] == '|'){
    size_t fLen = from.length();
    size_t strLen = str.length();
    if (start_pos+fLen == strLen){
      str.replace(start_pos, from.length(), to);
      return true;
    } else if (str[start_pos+fLen] == '(' ||
	       str[start_pos+fLen] == ')' ||
	       str[start_pos+fLen] == '+' ||
	       str[start_pos+fLen] == '*' ||
	       str[start_pos+fLen] == '/' ||
	       str[start_pos+fLen] == '-' ||
	       str[start_pos+fLen] == '^' ||
	       str[start_pos+fLen] == '=' ||
	       str[start_pos+fLen] == '<' ||
	       str[start_pos+fLen] == '>' ||
	       str[start_pos+fLen] == '&' ||
	       str[start_pos+fLen] == '|') {
      str.replace(start_pos, from.length(), to);
      return true;
    }
  }
  return false;
}

//[[Rcpp::export]]
RObject rxOptRep_(RObject input){
  CharacterVector inp = as<CharacterVector>(input);
  int len = inp.length();
  CharacterVector out(len);
  CharacterVector outn(len);
  if (len == 1) {
    List ret(2);
    CharacterVector outn = CharacterVector::create("rx_expr_0");
    outn.attr("names") = inp;
    ret[0] = outn;
    std::string outE = "rx_expr_0~" + as<std::string>(inp[0]) + "\n";
    ret[1] = CharacterVector::create(outE);
    return as<RObject>(ret);
  }
  out[0] = inp[0];
  outn[0] = "rx_expr_0";
  std::string outE = "rx_expr_0~" + as<std::string>(inp[0]) + "\n";
  for (int i = 1; i < len; i++){
    std::string cur = as<std::string>(inp[i]);
    for (int j = 0; j < i; j++){
      std::string last = as<std::string>(out[j]);
      std::string expr = "rx_expr_" + std::to_string(j);
      replace1(cur,last,expr);
    }
    out[i] = cur;
    outn[i] = "rx_expr_" + std::to_string(i);
    outE += as<std::string>(outn[i]) + "~" + as<std::string>(out[i]) +"\n";
  }
  outn.attr("names") = out;
  List ret(2);
  ret[0] = outn;
  ret[1] = CharacterVector::create(outE);
  return as<RObject>(ret);
}
