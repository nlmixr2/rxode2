// C++ port of the R walker `.rxPrune()` (R/rxPrune.R): turns if/else into
// arithmetic and string values into their integer levels.  The output text
// matches the R walker; any construct the port does not reproduce exactly makes
// it return NULL so rxPrune() falls back to the R walker.
#ifndef USE_FC_LEN_T
#define USE_FC_LEN_T
#endif
#ifndef STRICT_R_HEADERS
#define STRICT_R_HEADERS
#endif
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <R.h>
#include <Rinternals.h>
#include <string>
#include <vector>
#include <unordered_set>
#include "rxProtect.h"

namespace {

// Thrown for anything the port does not handle; rxPrune() then uses R.
struct rxPruneUnsupported {};

struct rxPruneSyms {
  SEXP sIf, sParen, sBrace, sEq, sNe, sGe, sLe, sGt, sLt, sAnd2, sOr2, sAnd,
    sOr, sAssign, sArrow, sTilde, sMod, sMul, sPow, sAdd, sSub, sDiv, sIfelse,
    sBracket, sDeparse1, sQuote;
};

struct rxPruneCapture {
  std::string original, condition, capVar;
};

struct rxPruneState {
  rxPruneSyms s;
  SEXP strAssign;
  SEXP strNames;
  bool capture;
  std::vector<std::string> ifs;
  std::unordered_set<std::string> def1;
  // the lhs of the enclosing `else` branch's `then` branch (`.else`)
  const std::unordered_set<std::string> *els;
  // every symbol assigned so far, in order; `.else` is a slice of it
  std::vector<std::string> lhsLog;
  std::vector<rxPruneCapture> captures;
};

const char *rxPruneAdaptiveFns[] = {"evid_", "bolus", "infuse", "infuseDur",
                                    "reset", "replace", "multiply", "phantom",
                                    "obs"};

// deparse1(x), evaluated by R so quoting/escaping is identical
std::string rxPruneDeparse(rxPruneState &st, SEXP x) {
  rxProtect rx_protect;
  SEXP q = rx_protect.protect(Rf_lang2(st.s.sQuote, x));
  SEXP call = rx_protect.protect(Rf_lang2(st.s.sDeparse1, q));
  int err = 0;
  SEXP ret = R_tryEvalSilent(call, R_BaseEnv, &err);
  if (err || TYPEOF(ret) != STRSXP || Rf_length(ret) != 1) {
    throw rxPruneUnsupported();
  }
  return std::string(CHAR(STRING_ELT(ret, 0)));
}

// as.character() of a length-one non-string atomic
std::string rxPruneAsChar(SEXP x) {
  if (Rf_length(x) != 1) {
    throw rxPruneUnsupported();
  }
  rxProtect rx_protect;
  SEXP ret = rx_protect.protect(Rf_coerceVector(x, STRSXP));
  return std::string(CHAR(STRING_ELT(ret, 0)));
}

std::string rxPruneNum(double d) {
  rxProtect rx_protect;
  SEXP x = rx_protect.protect(Rf_ScalarReal(d));
  return rxPruneAsChar(x);
}

bool rxPruneIsAtomic(SEXP x) {
  switch (TYPEOF(x)) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case RAWSXP:
    return true;
  default:
    return false;
  }
}

bool rxPruneIsAscii(const char *x) {
  for (; *x; ++x) {
    if ((unsigned char)*x > 127) return false;
  }
  return true;
}

// .rxPruneStrAssign(): the 1-based level of `val` in strAssign[[lhs]], or 0
int rxPruneStrLevel(rxPruneState &st, SEXP lhs, SEXP val) {
  if (TYPEOF(lhs) != SYMSXP || TYPEOF(val) != STRSXP || Rf_length(val) != 1 ||
      Rf_isNull(st.strNames)) {
    return 0;
  }
  const char *name = CHAR(PRINTNAME(lhs));
  int n = Rf_length(st.strNames);
  for (int i = 0; i < n; ++i) {
    SEXP nm = STRING_ELT(st.strNames, i);
    if (nm != NA_STRING && strcmp(CHAR(nm), name) == 0) {
      SEXP lev = VECTOR_ELT(st.strAssign, i);
      if (TYPEOF(lev) != STRSXP) return 0;
      SEXP v = STRING_ELT(val, 0);
      int found = 0, nFound = 0;
      for (int j = 0; j < Rf_length(lev); ++j) {
        SEXP lj = STRING_ELT(lev, j);
        // identical(): NA only matches NA
        bool same;
        if (lj == NA_STRING || v == NA_STRING) {
          same = lj == v;
        } else {
          // non-ASCII needs translation (which may error); leave it to R
          if (!rxPruneIsAscii(CHAR(lj)) || !rxPruneIsAscii(CHAR(v))) {
            throw rxPruneUnsupported();
          }
          same = strcmp(CHAR(lj), CHAR(v)) == 0;
        }
        if (same) {
          found = j + 1;
          nFound++;
        }
      }
      return nFound == 1 ? found : 0;
    }
  }
  return 0;
}

// .rxModOperand()
bool rxPruneIsNumber(const std::string &x) {
  size_t i = 0, n = x.size();
  size_t d1 = 0;
  while (i < n && isdigit((unsigned char)x[i])) {
    ++i;
    ++d1;
  }
  if (d1 > 0) {
    if (i < n && x[i] == '.') ++i;
    while (i < n && isdigit((unsigned char)x[i])) ++i;
  } else {
    if (i >= n || x[i] != '.') return false;
    ++i;
    size_t d2 = 0;
    while (i < n && isdigit((unsigned char)x[i])) {
      ++i;
      ++d2;
    }
    if (d2 == 0) return false;
  }
  if (i < n && (x[i] == 'e' || x[i] == 'E')) {
    ++i;
    if (i < n && (x[i] == '-' || x[i] == '+')) ++i;
    size_t d3 = 0;
    while (i < n && isdigit((unsigned char)x[i])) {
      ++i;
      ++d3;
    }
    if (d3 == 0) return false;
  }
  return i == n;
}

bool rxPruneIsIdent(const std::string &x) {
  if (x.empty()) return false;
  unsigned char c = (unsigned char)x[0];
  if (!(isalpha(c) || c == '.' || c == '_') || c > 127) return false;
  for (size_t i = 1; i < x.size(); ++i) {
    c = (unsigned char)x[i];
    if (c > 127 || !(isalnum(c) || c == '.' || c == '_')) return false;
  }
  return true;
}

std::string rxPruneModOperand(const std::string &x) {
  if (rxPruneIsNumber(x) || rxPruneIsIdent(x)) return x;
  return "(" + x + ")";
}

std::string rxPruneIfProduct(rxPruneState &st) {
  std::string ret;
  for (size_t i = 0; i < st.ifs.size(); ++i) {
    if (i > 0) ret += "*";
    ret += "(" + st.ifs[i] + ")";
  }
  return ret;
}

SEXP rxPruneArg(SEXP x, int i) {
  for (int j = 0; j < i; ++j) x = CDR(x);
  return CAR(x);
}

std::string rxPruneWalk(rxPruneState &st, SEXP x);

std::string rxPruneIf(rxPruneState &st, SEXP x, int n) {
  st.ifs.push_back(rxPruneWalk(st, rxPruneArg(x, 1)));
  std::string ret;
  if (n == 4) {
    size_t logStart = st.lhsLog.size();
    std::string ret1 = rxPruneWalk(st, rxPruneArg(x, 2));
    st.ifs.back() = "1-(" + st.ifs.back() + ")";
    std::unordered_set<std::string> newEls(st.lhsLog.begin() + logStart,
                                           st.lhsLog.end());
    const std::unordered_set<std::string> *oldEls = st.els;
    st.els = &newEls;
    std::string ret2 = rxPruneWalk(st, rxPruneArg(x, 3));
    st.els = oldEls;
    ret = ret1 + "\n" + ret2;
  } else if (n == 3) {
    ret = rxPruneWalk(st, rxPruneArg(x, 2));
  } else {
    throw rxPruneUnsupported();
  }
  st.ifs.pop_back();
  return ret;
}

std::string rxPruneAssign(rxPruneState &st, SEXP x, const char *op) {
  SEXP lhs = rxPruneArg(x, 1), rhs = rxPruneArg(x, 2);
  int lev = rxPruneStrLevel(st, lhs, rhs);
  if (TYPEOF(lhs) == SYMSXP) st.lhsLog.push_back(CHAR(PRINTNAME(lhs)));
  if (!st.ifs.empty()) {
    std::string f2 = rxPruneWalk(st, lhs);
    std::string ifs = rxPruneIfProduct(st);
    // R checks `.def1` before pruning the rhs
    bool defined = st.def1.count(f2) > 0;
    std::string r = lev ? rxPruneNum(lev) : rxPruneWalk(st, rhs);
    std::string ret;
    if (defined) {
      ret = f2 + op + ifs + "*(" + r + ")+(1-(" + ifs + "))*(" + f2 + ")";
    } else {
      ret = f2 + op + ifs + "*(" + r + ")";
      if (st.els != nullptr && st.els->count(f2)) ret += "+" + f2;
    }
    st.def1.insert(f2);
    return ret;
  }
  std::string f2 = rxPruneWalk(st, lhs);
  st.def1.insert(f2);
  return f2 + op + (lev ? rxPruneNum(lev) : rxPruneWalk(st, rhs));
}

bool rxPruneIsAdaptive(SEXP head) {
  const char *name = CHAR(PRINTNAME(head));
  for (const char *fn : rxPruneAdaptiveFns) {
    if (strcmp(fn, name) == 0) return true;
  }
  return false;
}

std::string rxPruneCaptureCall(rxPruneState &st, SEXP x) {
  rxPruneCapture c;
  c.condition = st.ifs.empty() ? std::string("1") : rxPruneIfProduct(st);
  c.capVar = "rxCaptureId" + std::to_string(st.captures.size() + 1);
  c.original = rxPruneDeparse(st, x);
  for (unsigned char ch : c.original) {
    if (ch > 127) throw rxPruneUnsupported();
  }
  st.lhsLog.push_back(c.capVar);
  st.captures.push_back(c);
  return c.capVar + " <- " + c.condition;
}

std::string rxPruneBracket(SEXP x, int n) {
  if (n < 3) throw rxPruneUnsupported();
  SEXP type = rxPruneArg(x, 1), idx = rxPruneArg(x, 2);
  if (TYPEOF(type) != SYMSXP) throw rxPruneUnsupported();
  std::string t = CHAR(PRINTNAME(type));
  for (char &ch : t) {
    if ((unsigned char)ch > 127) throw rxPruneUnsupported();
    ch = (char)toupper((unsigned char)ch);
  }
  std::string i;
  if (TYPEOF(idx) == SYMSXP) {
    i = CHAR(PRINTNAME(idx));
  } else if (TYPEOF(idx) == STRSXP && Rf_length(idx) == 1) {
    i = CHAR(STRING_ELT(idx, 0));
  } else if (rxPruneIsAtomic(idx)) {
    i = rxPruneAsChar(idx);
  } else {
    throw rxPruneUnsupported();
  }
  return t + "[" + i + "]";
}

std::string rxPruneGeneric(rxPruneState &st, SEXP x) {
  std::string ret = rxPruneWalk(st, CAR(x)) + "(";
  bool first = true;
  for (SEXP a = CDR(x); a != R_NilValue; a = CDR(a)) {
    if (!first) ret += ", ";
    ret += rxPruneWalk(st, CAR(a));
    first = false;
  }
  return ret + ")";
}

std::string rxPruneCall(rxPruneState &st, SEXP x) {
  const rxPruneSyms &s = st.s;
  SEXP h = CAR(x);
  int n = Rf_length(x);
  if (TYPEOF(h) != SYMSXP) {
    // R's adaptive check errors on a non-symbol head; let R report it
    if (st.capture) throw rxPruneUnsupported();
    return rxPruneGeneric(st, x);
  }
  const char *op = CHAR(PRINTNAME(h));
  if (h == s.sIf) return rxPruneIf(st, x, n);
  if (h == s.sParen) {
    if (n < 2) throw rxPruneUnsupported();
    return "(" + rxPruneWalk(st, rxPruneArg(x, 1)) + ")";
  }
  if (h == s.sBrace) {
    std::string ret;
    bool first = true;
    for (SEXP a = CDR(x); a != R_NilValue; a = CDR(a)) {
      if (!first) ret += "\n";
      ret += rxPruneWalk(st, CAR(a));
      first = false;
    }
    return ret;
  }
  if (h == s.sEq || h == s.sNe) {
    if (n < 3) throw rxPruneUnsupported();
    SEXP a = rxPruneArg(x, 1), b = rxPruneArg(x, 2);
    int levA = rxPruneStrLevel(st, b, a);
    int levB = rxPruneStrLevel(st, a, b);
    std::string ra = levA ? rxPruneNum(levA) : rxPruneWalk(st, a);
    std::string rb = levB ? rxPruneNum(levB) : rxPruneWalk(st, b);
    return ra + op + rb;
  }
  if (h == s.sGe || h == s.sLe || h == s.sGt || h == s.sLt || h == s.sAnd2 ||
      h == s.sOr2 || h == s.sAnd || h == s.sOr) {
    if (n < 3) throw rxPruneUnsupported();
    std::string ra = rxPruneWalk(st, rxPruneArg(x, 1));
    return ra + op + rxPruneWalk(st, rxPruneArg(x, 2));
  }
  if (h == s.sAssign || h == s.sArrow || h == s.sTilde) {
    if (n < 3) throw rxPruneUnsupported();
    return rxPruneAssign(st, x, op);
  }
  if (h == s.sMod) {
    if (n < 3) throw rxPruneUnsupported();
    std::string ra = rxPruneModOperand(rxPruneWalk(st, rxPruneArg(x, 1)));
    return ra + "%%" + rxPruneModOperand(rxPruneWalk(st, rxPruneArg(x, 2)));
  }
  if (h == s.sMul || h == s.sPow || h == s.sAdd || h == s.sSub ||
      h == s.sDiv) {
    if (n == 3) {
      std::string ra = rxPruneWalk(st, rxPruneArg(x, 1));
      return ra + op + rxPruneWalk(st, rxPruneArg(x, 2));
    }
    if (n < 2) throw rxPruneUnsupported();
    return op + rxPruneWalk(st, rxPruneArg(x, 1));
  }
  if (h == s.sIfelse) {
    if (n < 4) throw rxPruneUnsupported();
    std::string f2 = rxPruneWalk(st, rxPruneArg(x, 1));
    std::string f3 = rxPruneWalk(st, rxPruneArg(x, 2));
    std::string f4 = rxPruneWalk(st, rxPruneArg(x, 3));
    return "((" + f2 + ")*(" + f3 + ")+(1-(" + f2 + "))*(" + f4 + "))";
  }
  if (h == s.sBracket) return rxPruneBracket(x, n);
  if (st.capture && rxPruneIsAdaptive(h)) return rxPruneCaptureCall(st, x);
  return rxPruneGeneric(st, x);
}

std::string rxPruneWalk(rxPruneState &st, SEXP x) {
  switch (TYPEOF(x)) {
  case SYMSXP:
    // a missing argument errors in R's lapply()
    if (x == R_MissingArg) throw rxPruneUnsupported();
    return std::string(CHAR(PRINTNAME(x)));
  case STRSXP:
    return rxPruneDeparse(st, x);
  case LANGSXP:
    return rxPruneCall(st, x);
  default:
    if (rxPruneIsAtomic(x)) return rxPruneAsChar(x);
    throw rxPruneUnsupported();
  }
}

rxPruneSyms rxPruneGetSyms() {
  rxPruneSyms s;
  s.sIf = Rf_install("if");
  s.sParen = Rf_install("(");
  s.sBrace = Rf_install("{");
  s.sEq = Rf_install("==");
  s.sNe = Rf_install("!=");
  s.sGe = Rf_install(">=");
  s.sLe = Rf_install("<=");
  s.sGt = Rf_install(">");
  s.sLt = Rf_install("<");
  s.sAnd2 = Rf_install("&&");
  s.sOr2 = Rf_install("||");
  s.sAnd = Rf_install("&");
  s.sOr = Rf_install("|");
  s.sAssign = Rf_install("=");
  s.sArrow = Rf_install("<-");
  s.sTilde = Rf_install("~");
  s.sMod = Rf_install("%%");
  s.sMul = Rf_install("*");
  s.sPow = Rf_install("^");
  s.sAdd = Rf_install("+");
  s.sSub = Rf_install("-");
  s.sDiv = Rf_install("/");
  s.sIfelse = Rf_install("ifelse");
  s.sBracket = Rf_install("[");
  s.sDeparse1 = Rf_install("deparse1");
  s.sQuote = Rf_install("quote");
  return s;
}

// Runs the walk; false when the expression needs the R walker.
bool rxPruneRun(rxPruneState &st, SEXP expr, std::string &out) {
  try {
    out = rxPruneWalk(st, expr);
  } catch (...) {
    // rxPruneUnsupported, or std::bad_alloc
    return false;
  }
  // R marks the encoding of non-ASCII text; leave that to the R walker
  for (unsigned char c : out) {
    if (c > 127) return false;
  }
  return true;
}

} // namespace

// Prune the parsed expression `expr` (quoted `{...}` of rxNorm() text) with the
// model's `strAssign`; `capture` turns on adaptive-dosing capture.  Returns the
// pruned text (with a `capturedEvid` attribute when any call was captured), or
// NULL when the R walker has to be used.
extern "C" SEXP _rxode2_rxPruneC(SEXP expr, SEXP strAssign, SEXP capture) {
  rxProtect rx_protect;
  rxPruneState st;
  st.s = rxPruneGetSyms();
  st.strAssign = strAssign;
  st.strNames = TYPEOF(strAssign) == VECSXP
                    ? Rf_getAttrib(strAssign, R_NamesSymbol)
                    : R_NilValue;
  if (TYPEOF(strAssign) != VECSXP && !Rf_isNull(strAssign)) return R_NilValue;
  if (TYPEOF(strAssign) == VECSXP && Rf_length(strAssign) > 0 &&
      Rf_isNull(st.strNames)) {
    return R_NilValue;
  }
  if (TYPEOF(expr) != LANGSXP) return R_NilValue;
  st.capture = Rf_asLogical(capture) == 1;
  st.els = nullptr;
  std::string out;
  if (!rxPruneRun(st, expr, out)) return R_NilValue;
  SEXP ret = rx_protect.protect(Rf_mkString(out.c_str()));
  if (!st.captures.empty()) {
    int nc = (int)st.captures.size();
    SEXP lst = rx_protect.protect(Rf_allocVector(VECSXP, nc));
    SEXP nms = rx_protect.protect(Rf_allocVector(STRSXP, 4));
    SET_STRING_ELT(nms, 0, Rf_mkChar("id"));
    SET_STRING_ELT(nms, 1, Rf_mkChar("original"));
    SET_STRING_ELT(nms, 2, Rf_mkChar("condition"));
    SET_STRING_ELT(nms, 3, Rf_mkChar("capVar"));
    for (int i = 0; i < nc; ++i) {
      SEXP e = Rf_allocVector(VECSXP, 4);
      SET_VECTOR_ELT(lst, i, e);
      SET_VECTOR_ELT(e, 0, Rf_ScalarInteger(i + 1));
      SET_VECTOR_ELT(e, 1, Rf_mkString(st.captures[i].original.c_str()));
      SET_VECTOR_ELT(e, 2, Rf_mkString(st.captures[i].condition.c_str()));
      SET_VECTOR_ELT(e, 3, Rf_mkString(st.captures[i].capVar.c_str()));
      Rf_setAttrib(e, R_NamesSymbol, nms);
    }
    Rf_setAttrib(ret, Rf_install("capturedEvid"), lst);
  }
  return ret;
}
