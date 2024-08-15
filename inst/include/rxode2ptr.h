#ifndef __RXODE2PTR_H__
#define __RXODE2PTR_H__

#if defined(__cplusplus)
extern "C" {
#endif

  typedef SEXP (*_rxode2_rxRmvnSEXP_t)(SEXP nSSEXP, SEXP muSSEXP, SEXP sigmaSSEXP, SEXP lowerSSEXP, SEXP upperSSEXP, SEXP ncoresSSEXP, SEXP isCholSSEXP, SEXP keepNamesSSEXP, SEXP aSSEXP, SEXP tolSSEXP, SEXP nlTolSSEXP, SEXP nlMaxiterSSEXP);
  extern _rxode2_rxRmvnSEXP_t rxRmvnSEXP;

  typedef int (*par_progress_t)(int c, int n, int d, int cores, clock_t t0, int stop);
  extern par_progress_t par_progress;


  static inline SEXP iniRxodePtrs0(SEXP p) {
    if (rxRmvnSEXP == NULL) {
      rxRmvnSEXP = (_rxode2_rxRmvnSEXP_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 0));
      par_progress = (par_progress_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 1));
    }
    return R_NilValue;
  }

#define iniRxode2ptr                                \
  _rxode2_rxRmvnSEXP_t rxode2_rxRmvnSEXP = NULL;    \
  par_progress_t par_progress = NULL;               \
  SEXP iniRxodePtrs(SEXP ptr) {                     \
  return iniRxodePtrs0(ptr);                        \
}                                                   \

#if defined(__cplusplus)
  extern "C" {
#endif

#endif
