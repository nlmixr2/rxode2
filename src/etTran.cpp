// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: nil; -*-
//#undef NDEBUG
#define USE_FC_LEN_T
#define STRICT_R_HEADERS

#include <RcppArmadillo.h>
#include <algorithm>
#include "../inst/include/rxode2.h"
#define SORT gfx::timsort

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

#define rxModelVars(a) rxModelVars_(a)
using namespace Rcpp;
#include "checkmate.h"
#include "../inst/include/rxode2_as.h"

void RSprintf(const char *format, ...);

List rxModelVars_(const RObject &obj);

extern "C" {
  typedef SEXP (*_rxode2_etTransParse_type)(SEXP, SEXP, SEXP, SEXP, SEXP,
                                            SEXP, SEXP, SEXP, SEXP, SEXP,
                                            SEXP);
  extern _rxode2_etTransParse_type _rxode2_etTransParseP;
}

extern "C" SEXP assignRxode2ParsePtrs(void);




//' Event translation for rxode2
//'
//' @param inData Data frame to translate
//'
//' @param obj Model to translate data
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
//' @inheritParams rxode2parse::etTransParse
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
             bool ssAtDoseTime = true) {
  assignRxode2ParsePtrs();
  return as<List>(_rxode2_etTransParseP(inData, rxModelVars_(obj), wrap(addCmt),
                                        wrap(dropUnits), wrap(allTimeVar), wrap(keepDosingOnly),
                                        wrap(combineDvid), keep, wrap(addlKeepsCov),
                                        wrap(addlDropSs), wrap(ssAtDoseTime)));
}
