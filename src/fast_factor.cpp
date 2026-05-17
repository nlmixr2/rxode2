// Author: Kevin Ushey, David Cooley
// Modified by Matthew Fidler
#define USE_FC_LEN_T
#define STRICT_R_HEADER
#include <Rcpp.h>
using namespace Rcpp;

int fastFactorDataHasNa = 0;

//
// The unique ordered by occurrence comes from David Cooley:
// It is found https://stackoverflow.com/questions/44697544/rcpp-unique-order-output
template < typename T, int RTYPE >
inline SEXP sexp_unique( Rcpp::Vector< RTYPE > x ) {
  std::set< T > seen;
  auto newEnd = std::remove_if( x.begin(), x.end(), [&seen]( const T value ) {
    if ( seen.find( value ) != std::end( seen ) ) {
      return true;
    }
    seen.insert( value );
    return false;
  });
  x.erase( newEnd, x.end() );
  return x;
}

#define get_sexp_unique _rxode2parse_get_sexp_unique

// returns unique values in their original input order
extern "C" SEXP get_sexp_unique( SEXP s ) {

  SEXP s2 = PROTECT(Rcpp::clone( s ));

  switch( TYPEOF( s2 ) ) {
  case LGLSXP: {
    UNPROTECT(1);
    return sexp_unique< bool, LGLSXP >( s2 );
  }
  case REALSXP: {
    UNPROTECT(1);
    return sexp_unique< double, REALSXP >( s2 );
  }
  case INTSXP: {
    UNPROTECT(1);
    return sexp_unique< int, INTSXP >( s2 );
  }
  case STRSXP: {
    UNPROTECT(1);
    return sexp_unique< char* , STRSXP >( s2 );
  }
  default: {
    UNPROTECT(1);
    Rcpp::stop("unknown vector type");
  }
  }
  UNPROTECT(1);
  return 0;
}

template < typename T, int RTYPE >
inline int sexp_uniqueL( Rcpp::Vector< RTYPE > x ) {
  std::set< T > seen;
  auto newEnd = std::remove_if( x.begin(), x.end(), [&seen]( const T value ) {
    if ( seen.find( value ) != std::end( seen ) ) {
      return true;
    }
    seen.insert( value );
    return false;
  });
  x.erase( newEnd, x.end() );
  return x.size();
}

extern "C" int get_sexp_uniqueL( SEXP s ) {

  SEXP s2 = PROTECT(Rcpp::clone( s ));

  switch( TYPEOF( s2 ) ) {
  case LGLSXP: {
    UNPROTECT(1);
    return sexp_uniqueL< bool, LGLSXP >( s2 );
  }
  case REALSXP: {
    UNPROTECT(1);
    return sexp_uniqueL< double, REALSXP >( s2 );
  }
  case INTSXP: {
    UNPROTECT(1);
    return sexp_uniqueL< int, INTSXP >( s2 );
  }
  case STRSXP: {
    UNPROTECT(1);
    return sexp_uniqueL< char* , STRSXP >( s2 );
  }
  default: {
    UNPROTECT(1);
    Rcpp::stop("unknown vector type");
  }
  }
  UNPROTECT(1);
  return 0;
}

// adapted from https://gallery.rcpp.org/articles/fast-factor-generation/
// This was modified to use symbols for levels and class.
template <int RTYPE>
SEXP fast_factor_unsorted( const Vector<RTYPE>& x, SEXP oldLvl) {
  Vector<RTYPE> levs = get_sexp_unique(x);
  if (RTYPE == INTSXP) {
    int *cur = INTEGER(levs);
    for (int i = 0; i < Rf_length(levs); i++) {
      if (cur[i] == NA_INTEGER) {
        fastFactorDataHasNa = 1;
        break;
      }
    }
  } else if (RTYPE == STRSXP) {
    for (int i = 0; i < Rf_length(levs); i++) {
      if (STRING_ELT(levs, i) == NA_STRING) {
        fastFactorDataHasNa = 1;
        break;
      }
    }
  }
  IntegerVector out = match(x, levs);
  int pro = 0;
  SEXP outS = PROTECT(wrap(out)); pro++;
  SEXP lvl = PROTECT(R_NilValue); pro++; // Obsessive about protection; probably dosen't need to be protected.
  SEXP fac = PROTECT(wrap(CharacterVector("factor"))); pro++;
  if (Rf_isNull(oldLvl)) {
    lvl = PROTECT(wrap(as<CharacterVector>(levs))); pro++;
  } else {
    // RTYPE should be INTSXP
    SEXP levsSEXP = PROTECT(wrap(levs)); pro++;
    IntegerVector lvlI = as<IntegerVector>(levsSEXP);
    int hasNa = 0;
    for (int i = lvlI.size(); i--;) {
      if (lvlI[i] == NA_INTEGER) {
        hasNa = 1;
        break;
      }
    }
    CharacterVector lvlC(lvlI.size()-hasNa);
    lvl = wrap(lvlC);
    int cur=0;
    int j=0;
    for (int i = 0; i < lvlI.size(); ++i) {
      cur = lvlI[i];
      if (cur != NA_INTEGER) {
        SET_STRING_ELT(lvl, j++, STRING_ELT(oldLvl, lvlI[i]-1));
      }
    }
  }
  Rf_setAttrib(outS, R_LevelsSymbol, lvl);
  SEXP cls = PROTECT(Rf_install("class")); pro++;
  Rf_setAttrib(outS, cls, fac);
  UNPROTECT(pro);
  return outS;
}
//[[Rcpp::export]]
SEXP convertId_(SEXP x) {
  fastFactorDataHasNa = 0;
  SEXP oldLvl = R_NilValue;
  switch( TYPEOF(x) ) {
  case INTSXP: {
    oldLvl = Rf_getAttrib(x, R_LevelsSymbol);
    return fast_factor_unsorted<INTSXP>(x, oldLvl);
  }
  case REALSXP: return fast_factor_unsorted<REALSXP>(x, oldLvl);
  case STRSXP: return fast_factor_unsorted<STRSXP>(x, oldLvl);
  }
  return R_NilValue;
}
