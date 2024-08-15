#ifndef __RXODE2PTR_H__
#define __RXODE2PTR_H__

#if defined(__cplusplus)
extern "C" {
#endif

typedef SEXP (*_rxode2_rxRmvnSEXP_t)(SEXP nSSEXP, SEXP muSSEXP, SEXP sigmaSSEXP, SEXP lowerSSEXP, SEXP upperSSEXP, SEXP ncoresSSEXP, SEXP isCholSSEXP, SEXP keepNamesSSEXP, SEXP aSSEXP, SEXP tolSSEXP, SEXP nlTolSSEXP, SEXP nlMaxiterSSEXP);
  extern _rxode2_rxRmvnSEXP_t rxRmvnSEXP;

  static inline SEXP iniRxodePtrs0(SEXP p) {
    if (rxRmvnSEXP == NULL) {
      rxRmvnSEXP = (_rxode2_rxRmvnSEXP_t) R_ExternalPtrAddrFn(VECTOR_ELT(pt, 0));
    }
    return R_NilValue;
  }

#define iniRxode2ptr                                \
  _rxode2_rxRmvnSEXP_t rxode2_rxRmvnSEXP = NULL;    \
  SEXP iniRxodePtrs(SEXP ptr) {                     \
  return iniRxodePtrs0(ptr);                        \
}                                                   \

#if defined(__cplusplus)
  extern "C" {
#endif

#endif
