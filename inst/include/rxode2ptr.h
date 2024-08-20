#ifndef __RXODE2PTR_H__
#define __RXODE2PTR_H__

#include "rxode2.h"

#if defined(__cplusplus)
extern "C" {
#endif

  typedef SEXP (*_rxode2_rxRmvnSEXP_t)(SEXP nSSEXP, SEXP muSSEXP, SEXP sigmaSSEXP, SEXP lowerSSEXP, SEXP upperSSEXP, SEXP ncoresSSEXP, SEXP isCholSSEXP, SEXP keepNamesSSEXP, SEXP aSSEXP, SEXP tolSSEXP, SEXP nlTolSSEXP, SEXP nlMaxiterSSEXP);
  extern _rxode2_rxRmvnSEXP_t _rxode2_rxRmvnSEXP_;

  typedef int (*par_progress_t)(int c, int n, int d, int cores, clock_t t0, int stop);
  extern par_progress_t par_progress;

  typedef rx_solve *(*getRxSolve_t)(void);
  extern getRxSolve_t getRxSolve_;

  typedef void (*ind_solve_t)(rx_solve *rx, unsigned int cid, t_dydt_liblsoda dydt_lls,
                              t_dydt_lsoda_dum dydt_lsoda, t_jdum_lsoda jdum,
                              t_dydt c_dydt, t_update_inis u_inis, int jt);
  extern ind_solve_t ind_solve;

  typedef double (*getTime_t)(int idx, rx_solving_options_ind *ind);
  extern getTime_t getTime;

  typedef int (*isRstudio_t)(void);
  extern isRstudio_t isRstudio;

  typedef int (*iniSubjectE_t)(int solveid, int inLhs, rx_solving_options_ind *ind, rx_solving_options *op, rx_solve *rx,
                               t_update_inis u_inis);
  extern iniSubjectE_t iniSubjectE;

  typedef void (*sortIds_t)(rx_solve* rx, int ini);
  extern sortIds_t sortIds;

  typedef rx_solving_options* (*getSolvingOptions_t)(rx_solve* rx);
  extern getSolvingOptions_t getSolvingOptions;

  typedef rx_solving_options_ind *(*getSolvingOptionsInd_t)(rx_solve *rx, int id);
  extern getSolvingOptionsInd_t getSolvingOptionsInd;

  typedef SEXP (*_rxode2_rxModelVars_t)(SEXP);
  extern _rxode2_rxModelVars_t _rxode2_rxModelVars_;

  typedef void (*par_solve_t)(rx_solve *rx);
  extern par_solve_t par_solve;

  typedef const char *(*rxGetId_t)(int id);
  extern rxGetId_t rxGetId;

  typedef double (*getIndLambda_t)(rx_solving_options_ind* ind);
  extern getIndLambda_t getIndLambda;

  typedef int (*getIndLambdaYj_t)(rx_solving_options_ind* ind);
  extern getIndLambdaYj_t getIndLambdaYj;

  typedef double (*getIndLogitLow_t)(rx_solving_options_ind* ind);
  extern getIndLogitLow_t getIndLogitLow;

  typedef double (*getIndLogitHi_t)(rx_solving_options_ind* ind);
  extern getIndLogitHi_t getIndLogitHi;

  typedef void (*setIndParPtr_t)(rx_solving_options_ind* ind, int i, double val);
  extern setIndParPtr_t setIndParPtr;

  typedef double (*getIndParPtr_t)(rx_solving_options_ind* ind, int i);
  extern getIndParPtr_t getIndParPtr;

  typedef int (*getIndNallTimes_t)(rx_solving_options_ind* ind);
  extern getIndNallTimes_t getIndNallTimes;

  typedef void (*setIndIdx_t)(rx_solving_options_ind* ind, int j);
  extern setIndIdx_t setIndIdx;

  typedef int (*getIndIx_t)(rx_solving_options_ind* ind, int j);
  extern getIndIx_t getIndIx;

  typedef int (*getIndEvid_t)(rx_solving_options_ind* ind, int kk);
  extern getIndEvid_t getIndEvid;

  typedef double *(*getIndLhs_t)(rx_solving_options_ind* ind);
  extern getIndLhs_t getIndLhs;

  typedef int (*getIndNdoses_t)(rx_solving_options_ind* ind);
  extern getIndNdoses_t getIndNdoses;

  typedef int (*getIndNevid2_t)(rx_solving_options_ind* ind);
  extern getIndNevid2_t getIndNevid2;

  typedef void (*setIndSolve_t)(rx_solving_options_ind* ind, int solve);
  extern setIndSolve_t setIndSolve;

  typedef double *(*getIndSolve_t)(rx_solving_options_ind* ind);
  extern getIndSolve_t getIndSolve;

  typedef double (*getIndDv_t)(rx_solving_options_ind* ind, int j);
  extern getIndDv_t getIndDv;

  typedef int (*getIndYj_t)(rx_solving_options_ind* ind);
  extern getIndYj_t getIndYj;

  typedef double (*getIndLimit_t)(rx_solving_options_ind* ind, int kk);
  extern getIndLimit_t getIndLimit;

  typedef int (*getIndCens_t)(rx_solving_options_ind* ind, int kk);
  extern getIndCens_t getIndCens;

  typedef int (*getIndIdx_t)(rx_solving_options_ind* ind);
  extern getIndIdx_t getIndIdx;

  typedef int (*getOpNeq_t)(rx_solving_options* op);
  extern getOpNeq_t getOpNeq;

#define getRxNeq(rx) getOpNeq(getSolvingOptions(rx))

  typedef void (*setOpNeq_t)(rx_solving_options* op, int neq);
  extern setOpNeq_t setOpNeq;

#define setRxNeq(rx, neq)  setOpNeq(getSolvingOptions(rx), neq)

  typedef int (*hasOpBadSolve_t)(rx_solving_options* op);
  extern hasOpBadSolve_t hasOpBadSolve;
#define hasRxBadSolve(rx)  hasOpBadSolve(getSolvingOptions(rx))

  typedef int (*getOpNlin_t)(rx_solving_options* op);
  extern getOpNlin_t getOpNlin;
#define getRxNlin(rx) getOpNlin(getSolvingOptions(rx))

  typedef int (*getOpCores_t)(rx_solving_options* op);
  extern getOpCores_t getOpCores;
#define getRxCores(rx) getOpCores(getSolvingOptions(rx))

  typedef int (*getOpNlhs_t)(rx_solving_options* op);
  extern getOpNlhs_t getOpNlhs;
#define getRxNlhs(rx) getOpNlhs(getSolvingOptions(rx))

  typedef int (*getOpStiff_t)(rx_solving_options* op);
  extern getOpStiff_t getOpStiff;
#define getRxStiff(rx) getOpStiff(getSolvingOptions(rx))

  static inline SEXP iniRxodePtrs0(SEXP p) {
    if (_rxode2_rxRmvnSEXP_ == NULL) {
      _rxode2_rxRmvnSEXP_ = (_rxode2_rxRmvnSEXP_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 0));
      par_progress = (par_progress_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 1));
      getRxSolve_ = (getRxSolve_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 2));
      ind_solve = (ind_solve_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 3));
      getTime = (getTime_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 4));
      isRstudio = (isRstudio_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 5));
      iniSubjectE = (iniSubjectE_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 6));
      sortIds = (sortIds_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 7));
      getSolvingOptions = (getSolvingOptions_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 8));
      getSolvingOptionsInd = (getSolvingOptionsInd_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 9));
      _rxode2_rxModelVars_ = (_rxode2_rxModelVars_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 10));
      par_solve = (par_solve_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 11));
      rxGetId = (rxGetId_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 12));
      getIndLambda = (getIndLambda_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 13));
      getIndLambdaYj = (getIndLambdaYj_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 14));
      getIndLogitLow = (getIndLogitLow_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 15));
      getIndLogitHi = (getIndLogitHi_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 16));
      setIndParPtr =  (setIndParPtr_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 17));
      getIndParPtr = (getIndParPtr_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 18));
      getIndNallTimes = (getIndNallTimes_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 19));
      setIndIdx = (setIndIdx_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 20));
      getIndIx = (getIndIx_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 21));
      getIndEvid= (getIndEvid_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 22));
      getIndLhs = (getIndLhs_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 23));
      getIndNdoses = (getIndNdoses_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 24));
      getIndNevid2 = (getIndNevid2_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 25));
      setIndSolve = (setIndSolve_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 26));
      getIndSolve = (getIndSolve_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 27));
      getIndDv = (getIndDv_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 28));
      getIndYj = (getIndYj_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 29));
      getIndLimit = (getIndLimit_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 30));
      getIndCens = (getIndCens_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 31));
      getIndIdx = (getIndIdx_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 32));
      getOpNeq = (getOpNeq_t)  R_ExternalPtrAddrFn(VECTOR_ELT(p, 33));
      setOpNeq= (setOpNeq_t)  R_ExternalPtrAddrFn(VECTOR_ELT(p, 34));
      hasOpBadSolve = (hasOpBadSolve_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 35));
      getOpNlin = (getOpNlin_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 36));
      getOpCores = (getOpCores_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 37));
      getOpNlhs = (getOpNlhs_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 38));
      getOpStiff = (getOpStiff_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, 39));
    }
    return R_NilValue;
  }

#define iniRxode2ptr                                    \
  _rxode2_rxRmvnSEXP_t _rxode2_rxRmvnSEXP_ = NULL;      \
  par_progress_t par_progress = NULL;                   \
  getRxSolve_t getRxSolve_ = NULL;                      \
  ind_solve_t ind_solve = NULL;                         \
  par_solve_t par_solve = NULL;                         \
  getTime_t getTime = NULL;                             \
  isRstudio_t isRstudio = NULL;                         \
  iniSubjectE_t iniSubjectE = NULL;                     \
  sortIds_t sortIds = NULL;                             \
  getSolvingOptions_t getSolvingOptions = NULL;         \
  getSolvingOptionsInd_t getSolvingOptionsInd = NULL;   \
  _rxode2_rxModelVars_t _rxode2_rxModelVars_ = NULL;    \
  getIndLambda_t getIndLambda = NULL;                   \
  getIndLambdaYj_t getIndLambdaYj = NULL;               \
  getIndLogitLow_t getIndLogitLow = NULL;               \
  getIndLogitHi_t getIndLogitHi = NULL;                 \
  rxGetId_t rxGetId = NULL;                             \
  setIndParPtr_t setIndParPtr = NULL;                   \
  getIndParPtr_t getIndParPtr = NULL;                   \
  getIndNallTimes_t getIndNallTimes = NULL;             \
  setIndIdx_t setIndIdx = NULL;                         \
  getIndIx_t getIndIx = NULL;                           \
  getIndEvid_t getIndEvid = NULL;                       \
  getIndLhs_t getIndLhs = NULL;                         \
  getIndNdoses_t getIndNdoses = NULL;                   \
  getIndNevid2_t getIndNevid2 = NULL;                   \
  setIndSolve_t setIndSolve = NULL;                     \
  getIndSolve_t getIndSolve = NULL;                     \
  getIndDv_t getIndDv = NULL;                           \
  getIndYj_t getIndYj = NULL;                           \
  getIndLimit_t getIndLimit = NULL;                     \
  getIndCens_t getIndCens = NULL;                       \
  getIndIdx_t getIndIdx = NULL;                         \
  getOpNeq_t getOpNeq = NULL;                           \
  setOpNeq_t setOpNeq = NULL;                           \
  hasOpBadSolve_t hasOpBadSolve = NULL;                 \
  getOpNlin_t getOpNlin = NULL;                         \
  getOpCores_t getOpCores = NULL;                       \
  getOpNlhs_t getOpNlhs = NULL;                         \
  getOpStiff_t getOpStiff = NULL;                       \
  SEXP iniRxodePtrs(SEXP ptr) {                         \
    return iniRxodePtrs0(ptr);                          \
  }                                                     \

#if defined(__cplusplus)
}
#endif

#endif
