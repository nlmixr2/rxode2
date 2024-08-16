#ifndef __RXODE2PTR_H__
#define __RXODE2PTR_H__

#if defined(__cplusplus)
extern "C" {
#endif

  #include <rxode2parseStruct.h>

  typedef SEXP (*_rxode2_rxRmvnSEXP_t)(SEXP nSSEXP, SEXP muSSEXP, SEXP sigmaSSEXP, SEXP lowerSSEXP, SEXP upperSSEXP, SEXP ncoresSSEXP, SEXP isCholSSEXP, SEXP keepNamesSSEXP, SEXP aSSEXP, SEXP tolSSEXP, SEXP nlTolSSEXP, SEXP nlMaxiterSSEXP);
  extern _rxode2_rxRmvnSEXP_t rxRmvnSEXP;

  typedef int (*par_progress_t)(int c, int n, int d, int cores, clock_t t0, int stop);
  extern par_progress_t par_progress;

  typedef rx_solve *(getRxSolve_t)(void);
  extern getRxSolve_t getRxSolve_;

  typedef void (*ind_solve_t)(rx_solve *rx, unsigned int cid, t_dydt_liblsoda dydt_lls,
                              t_dydt_lsoda_dum dydt_lsoda, t_jdum_lsoda jdum,
                              t_dydt c_dydt, t_update_inis u_inis, int jt);
  extern ind_solve_t ind_solve;

  typedef double (*getTime_t)(int idx, rx_solving_options_ind *ind);
  extern getTime_t getTime;

  typedef int (*isRstudio_t)(void);
  extern isRstudio_t isRstudio;

  typedef int (*iniSubjectI_t)(int solveid, int inLhs, rx_solving_options_ind *ind, rx_solving_options *op, rx_solve *rx,
                               t_update_inis u_inis);
  extern iniSubjectI_t iniSubjectI;

  typedef void (*sortIds_t)(rx_solve* rx, int ini);
  extern sortIds_t sortIds;

  extern t_calc_lhs getRxLhs;
  extern t_update_inis getUpdateInis;

  typedef SEXP (*_rxode2_rxModelVars_t)(SEXP);
  extern _rxode2_rxModelVars_t _rxode2_rxModelVars_;

  typedef void (*par_solve_t)(rx_solve *rx);
  extern par_solve_t par_solve;


  /* getUpdateInis = (getUpdateInis_t) R_GetCCallable("rxode2", "getUpdateInis"); */
  /*  rxModelVarsS = (mv_t)R_GetCCallable("rxode2", "_rxode2_rxModelVars_"); */
  /* R_GetCCallable("rxode2","par_solve"); */


  static inline SEXP iniRxodePtrs0(SEXP p) {
    if (rxRmvnSEXP == NULL) {
      rxRmvnSEXP = (_rxode2_rxRmvnSEXP_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 0));
      par_progress = (par_progress_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 1));
      getRxSolve_ = (getRxSolve_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 2));
      ind_solve = (ind_solve_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 3));
      getTime = (getTime_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 4));
      isRstudio = (isRstudio_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 5));
      iniSubjectI = (iniSubjectI_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 6));
      sortIds = (sortIds_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 7));
      getRxLhs = (t_calc_lhs) R_ExternalPtrAddrFn(VECTOR_ELT(p, 8));
      getUpdateInis = (t_update_inis) R_ExternalPtrAddrFn(VECTOR_ELT(p, 9));
      _rxode2_rxModelVars = (_rxode2_rxModelVars_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 10));
      par_solve = (par_solve_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 11));
    }
    return R_NilValue;
  }

#define iniRxode2ptr                                \
  _rxode2_rxRmvnSEXP_t rxode2_rxRmvnSEXP = NULL;    \
  par_progress_t par_progress = NULL;               \
  getRxSolve_t getRxSolve_ = NULL;                  \
  ind_solve_t ind_solve = NULL;                     \
  getTime_t getTime = NULL;                         \
  isRstudio_t isRstudio = NULL;                     \
  iniSubjectI_t iniSubjectI = NULL;                 \
  sortIds_t sortIds = NULL;                         \
  t_calc_lhs getRxLhs = NULL;                       \
  t_update_inis getUpdateInis = NULL;               \
  _rxode2_rxModelVars_t _rxode2_rxModelVars = NULL; \
  SEXP iniRxodePtrs(SEXP ptr) {                     \
  return iniRxodePtrs0(ptr);                        \
}                                                   \

#if defined(__cplusplus)
  extern "C" {
#endif

#endif
