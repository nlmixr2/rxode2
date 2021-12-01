#ifndef rxode2_as
#define rxode2_as
//#define asError

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

using namespace Rcpp;

static inline bool rxIsNull(RObject obj) {
  return obj.sexp_type() == 0;
}
static inline bool rxIsNum(RObject obj) {
  if (obj.sexp_type() == REALSXP) {
    return (!obj.hasAttribute("dim"));
  }
  return false;
}
static inline bool rxIsNum1(RObject obj) {
  if (obj.sexp_type() == REALSXP) {
    if (!obj.hasAttribute("dim")){
      return (Rf_length(obj) == 1);
    }
  }
  return false;
}
static inline bool rxIsInt(RObject obj) {
  if (obj.sexp_type() == 13) {
    return (!obj.hasAttribute("dim"));
  }
  return false;
}

static inline bool rxIsNumInt(RObject obj) {
  int type = obj.sexp_type();
  if (type == REALSXP || type == 13) {
    return (!obj.hasAttribute("dim"));
  }
  return false;
}
static inline bool rxIsChar(RObject obj) {
  int type = obj.sexp_type();
  if (type == STRSXP) {
    return (!obj.hasAttribute("dim"));
  }
  return false;
}

static inline bool rxIsList(RObject obj) {
  int type = obj.sexp_type();
  return (type == VECSXP);
}

inline bool rxIsFactor(RObject obj) {
  return !Rf_isNull(Rf_getAttrib(as<SEXP>(obj), R_LevelsSymbol));
}

inline bool rxIsNumIntLgl(RObject obj) {
  int type = obj.sexp_type();
  if (type == REALSXP || type == INTSXP || type == LGLSXP) {
    return (!obj.hasAttribute("dim"));
  }
  return false;
}

static inline List asList(SEXP in, const char* what) {
  if (TYPEOF(in) != VECSXP) {
    REprintf("'%s'\n", what);
    Rcpp::print(in);
#ifdef asError
    REprintf(_("'%s' needs to be a list"), what);
    abort();
#else 
    Rcpp::stop(_("'%s' needs to be a list"), what);
#endif
  }
  return as<List>(in);
}

static inline int asInt(SEXP in, const char* what) {
  if (Rf_length(in) != 1 || !qtest(in,"x")) {
    REprintf("'%s'\n", what);
    Rcpp::print(in);
#ifdef asError
    REprintf(_("'%s' needs to be an integer"), what);
    abort();
#else 
    Rcpp::stop(_("'%s' needs to be an integer"), what);
#endif
  }
  return as<int>(in);
}

static inline unsigned int asUnsignedInt(SEXP in, const char* what) {
  if (Rf_length(in) != 1 || !qtest(in, "x[0,)")) {
    REprintf("'%s'\n", what);
    Rcpp::print(in);
#ifdef asError
    REprintf(_("'%s' needs to be an integer greater than 0"), what);
    abort();
#else 
    Rcpp::stop(_("'%s' needs to be an integer greater than 0"), what);
#endif
  }
  return as<unsigned int>(in);
}

static inline double asDouble(SEXP in, const char* what) {
  int type = TYPEOF(in);
  int goodType =  type == REALSXP || type == INTSXP;
  if (Rf_length(in) != 1 || !goodType) {
    REprintf("'%s'\n", what);
    Rcpp::print(in);
#ifdef asError
    REprintf(_("'%s' needs to be an double"), what);
    abort();
#else 
    Rcpp::stop(_("'%s' needs to be an double"), what);
#endif
  }
  return as<double>(in);
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

static inline std::string asStr(SEXP in, const char *what) {
  int type = TYPEOF(in);
  if (type == CHARSXP) {
    return as<std::string>(in);
  }
  if (Rf_length(in) != 1 || type != STRSXP){
    REprintf("'%s'\n", what);
    Rcpp::print(in);
#ifdef asError
    REprintf(_("'%s' needs to be a boolean"), what);
    abort();
#else
    Rcpp::stop(_("'%s' needs to be a string"), what);
#endif
  }
  return as<std::string>(in);
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

static inline Nullable<LogicalVector> asNLv(SEXP in, const char* what) {
  int type = TYPEOF(in);
  if (!(type == 0 || type == LGLSXP)){
    REprintf("'%s'\n", what);
    Rcpp::print(in);
#ifdef asError
    REprintf(_("'%s' needs to be an logical vector or NULL"), what);
    abort();
#else
    Rcpp::stop(_("'%s' needs to be an logical vector or NULL"), what);
#endif
  }
  return as<Nullable<LogicalVector>>(in);
}

static inline LogicalVector asLv(SEXP in, const char* what) {
  int type = TYPEOF(in);
  if (type != LGLSXP){
    REprintf("'%s'\n", what);
    Rcpp::print(in);
#ifdef asError
    REprintf(_("'%s' needs to be an logical vector or NULL"), what);
    abort();
#else 
    Rcpp::stop(_("'%s' needs to be an logical vector or NULL"), what);
#endif
  }
  return as<LogicalVector>(in);
}

static inline NumericVector asNv(SEXP in, const char* what) {
  if (TYPEOF(in) != REALSXP && TYPEOF(in) != INTSXP) {
    REprintf("'%s'\n", what);
    Rcpp::print(in);
#ifdef asError
    REprintf(_("'%s' needs to be a numeric vector"), what);
    abort();
#else 
    Rcpp::stop(_("'%s' needs to be a numeric vector"), what);
#endif
  }
  return as<NumericVector>(in);
}

static inline Nullable<NumericVector> asNNv(SEXP in, const char* what) {
  int type = TYPEOF(in);
  if (!(type == 0 || type == REALSXP)){
    REprintf("'%s'\n", what);
    Rcpp::print(in);
#ifdef asError
    REprintf(_("'%s' needs to be a numeric vector"), what);
    abort();
#else 
    Rcpp::stop(_("'%s' needs to be an numeric vector or NULL"), what);
#endif
  }
  return as<Nullable<NumericVector>>(in);
}

static inline Environment asEnv(SEXP in, const char* what) {
  if (!Rf_isEnvironment(in)) {
    REprintf("'%s'\n", what);
    Rcpp::print(in);
#ifdef asError
    REprintf(_("'%s' needs to be an environment"), what);
    abort();
#else 
    Rcpp::stop(_("'%s' needs to be an environment"), what);
#endif
  }
  return as<Environment>(in);
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

#endif
