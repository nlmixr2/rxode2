#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdlib.h> // for NULL
#define __DOINIT__
#include "cbindThetaOmega.h"
#include "../inst/include/rxode2.h"
#include "../inst/include/rxode2parseGetTime.h"
#include "rxthreefry.h"
#include "rx2api.h"

SEXP _rxHasOpenMp(void);

SEXP _vecDF(SEXP cv, SEXP n_);
SEXP _rxode2_dropUnitsRxSolve(SEXP);
SEXP _rxode2_atolRtolFactor_(SEXP);
SEXP _rxode2_etRep_(SEXP, SEXP, SEXP, SEXP, SEXP,
                    SEXP, SEXP);
SEXP _rxode2_rxSolveSEXP(SEXP, SEXP, SEXP, SEXP, SEXP,
                         SEXP, SEXP, SEXP);
SEXP _rxode2_etTrans(SEXP, SEXP, SEXP, SEXP, SEXP,
                     SEXP, SEXP, SEXP, SEXP, SEXP,
                     SEXP, SEXP);
SEXP _rxode2_rxUpdateTrans_(SEXP, SEXP, SEXP);
double powerDi(double x, double lambda, int yj);
double powerD(double x, double lambda, int yj);
double powerDD(double x, double lambda, int yj);
double powerDDD(double x, double lambda, int yj);
double powerL(double x, double lambda, int yj);
double powerDL(double x, double lambda, int yj);

SEXP _rxProgress(SEXP num, SEXP core);
SEXP _rxTick(void);
SEXP _rxProgressStop(SEXP);
SEXP _rxProgressAbort(SEXP);
SEXP _rxode2_codeLoaded(void);

SEXP _rxode2_codegen(SEXP c_file, SEXP prefix, SEXP libname, SEXP pMd5, SEXP timeId, SEXP lastMv, SEXP goodFuns);
SEXP _rxode2_parseModel(SEXP type);
SEXP _rxode2_isLinCmt(void);
SEXP _rxode2_RcppExport_registerCCallable(void);
SEXP _rxode2_setRstudio(SEXP);
SEXP _rxode2_rxSolveFree(void);
SEXP _rxode2_linCmtEnv(SEXP rho);
SEXP _rxode2_rxInv(SEXP matrix);
SEXP _rxCholInv(SEXP dms, SEXP theta, SEXP tn);
SEXP _rxode2_rxSymInvCholEnvCalculate(SEXP, SEXP, SEXP);
SEXP _rxode2_rxSymInvChol(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxIs(SEXP,SEXP);
SEXP _rxode2_rxModelVars_(SEXP);
SEXP _rxode2_rxState(SEXP, SEXP);
SEXP _rxode2_rxParams_(SEXP);
SEXP _rxode2_rxDfdy(SEXP);
SEXP _rxode2_rxLhs(SEXP);
SEXP _rxode2_rxInits(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxSetupIni(SEXP, SEXP);
SEXP _rxode2_rxSetupScale(SEXP,SEXP,SEXP);
SEXP _rxode2_rxSolveGet(SEXP, SEXP, SEXP);
SEXP _rxode2_rxSolveUpdate(SEXP, SEXP, SEXP);
SEXP _rxode2_rxAssignPtr(SEXP);
SEXP _rxode2_rxAssignPtr(SEXP objectSEXP);
SEXP _rxode2_dynLoad(SEXP dllSEXP);
SEXP _rxode2_rxOptRep_(SEXP);
SEXP _rxode2_rxIndLin_(SEXP);
SEXP _rxParProgress(SEXP);
SEXP _rxode2_rxRmvn_(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxCholperm(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxGradpsi(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxNleq(SEXP, SEXP, SEXP);
SEXP _rxode2_rxMvnrnd(SEXP, SEXP, SEXP, SEXP, SEXP,
                      SEXP, SEXP);
SEXP _rxode2_rxMvrandn_(SEXP, SEXP, SEXP, SEXP, SEXP,
                        SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP _rxode2_rxSolveDollarNames(SEXP);
SEXP _rxode2_rxExpandNesting(SEXP, SEXP, SEXP);
SEXP _rxode2_expandTheta_(SEXP, SEXP, SEXP, SEXP, SEXP,
                          SEXP);

SEXP _rxode2_rxRmvnSEXP(SEXP, SEXP, SEXP, SEXP, SEXP,
                        SEXP, SEXP, SEXP, SEXP, SEXP,
                        SEXP, SEXP);

SEXP _rxode2_itostr(SEXP, SEXP);
SEXP _rxode2_itoletter(SEXP, SEXP);

SEXP _cbindOme(SEXP et_, SEXP mat_, SEXP n_);

SEXP _rxode2_nestingInfo_(SEXP omega, SEXP data);

SEXP _rxode2_isNullZero(SEXP in);

SEXP rxode2_get_mv(void);
SEXP _rxode2_rxGetSeed(void);

SEXP _gammap(SEXP, SEXP);
SEXP _gammaq(SEXP, SEXP);
SEXP _lowergamma(SEXP, SEXP);
SEXP _uppergamma(SEXP, SEXP);
SEXP _gammapDer(SEXP, SEXP);

SEXP _gammapInv(SEXP, SEXP);
SEXP _gammaqInv(SEXP, SEXP);

SEXP _gammapInva(SEXP, SEXP);
SEXP _gammaqInva(SEXP, SEXP);

double expit(double, double, double);
double logit(double, double, double);
double ReLU(double x);
double dReLU(double x);
double GELU(double x);
double dGELU(double x);
double d2GELU(double x);
double d3GELU(double x);
double d4GELU(double x);
double ELU(double x, double alpha);
double dELU(double x, double alpha);
double d2ELU(double x, double alpha);
double d2aELU(double x, double alpha);
double dELUa(double x, double alpha);
double d2ELUa(double x, double alpha);
double softplus(double x);
double dsoftplus(double x);
double d2softplus(double x);
double d3softplus(double x);
double d4softplus(double x);
double SELU(double x);
double dSELU(double x);
double lReLU(double x);
double dlReLU(double x);
double PReLU(double x, double alpha);
double dPReLU(double x, double alpha);
double dPReLUa(double x, double alpha);
double dPReLUa1(double x, double alpha);
double Swish(double x);
double dSwish(double x);

SEXP _linCmtParse(SEXP vars, SEXP inStr, SEXP verbose);
SEXP _rxode2_linCmtGen(SEXP linCmt, SEXP vars, SEXP linCmtSens, SEXP verbose);

SEXP _rxode2_rxordSelect(SEXP, SEXP);
SEXP _rxode2_isIntel(void);

static R_NativePrimitiveArgType rxode2_Sum_t[] = {
  REALSXP, INTSXP
};

extern int rxode2_current_fn_pointer_id(void);
extern double rxode2_sum(double *input, int len);
extern double rxode2_prod(double *input, int len);

extern void rxode2_assign_fn_pointers(SEXP mv);


// Need to change to remove global variables
extern void rxode2_ode_free(void);

// Changed for Parallel
extern void rxode2_ode_freeP(rx_solve *rx, unsigned int id);

extern void rxRmModelLib(const char* s);
extern SEXP rxGetModelLib(const char *s);

extern SEXP _rxode2_rxRmModelLib_(SEXP);
extern SEXP _rxode2_rxDll(SEXP);
extern SEXP _rxode2_rxIsLoaded(SEXP);
extern SEXP _rxode2_rxDynUnload(SEXP);
extern SEXP _rxode2_rxDynLoad(SEXP);
extern SEXP _rxode2_rxDelete(SEXP);
extern SEXP _rxode2_rxGetrxode2(SEXP);
extern SEXP _rxode2_rxC(SEXP);

extern SEXP _rxode2_rxIsCurrent(SEXP);

SEXP _rxode2_cvPost_(SEXP, SEXP, SEXP, SEXP, SEXP,
                     SEXP, SEXP);
SEXP _rxode2_expandPars_(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rinvchisq(SEXP, SEXP, SEXP);

SEXP _rxode2_setProgSupported(SEXP);
SEXP _rxode2_getProgSupported(void);
SEXP _rxode2_rxSetSilentErr(SEXP silentSEXP);
SEXP _rxode2_rxUnloadAll_(void);
SEXP _rxode2_rxLock(SEXP);
SEXP _rxode2_rxUnlock(SEXP);
SEXP _rxode2_rxAllowUnload(SEXP);

SEXP _rxode2_rxExpandGrid_(SEXP, SEXP, SEXP);
SEXP _rxode2_rxExpandSens_(SEXP, SEXP);
SEXP _rxode2_rxExpandSens2_(SEXP, SEXP, SEXP);
SEXP _rxode2_rxExpandFEta_(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxRepR0_(SEXP);
SEXP _rxode2_rLKJ1(SEXP, SEXP, SEXP);
SEXP _rxode2_rLKJcv1(SEXP, SEXP);
SEXP _rxode2_rLKJcvLsd1(SEXP, SEXP, SEXP);
SEXP _rxode2_rcvC1(SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP _rxode2_rxSeedEng(SEXP);
SEXP _rxode2_rxnorm_(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxpois_(SEXP, SEXP, SEXP);
SEXP _rxode2_rxt__(SEXP, SEXP, SEXP);
SEXP _rxode2_rxunif_(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxweibull_(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxgeom_(SEXP, SEXP, SEXP);
SEXP _rxode2_rxbeta_(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxgamma_(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxf_(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxexp_(SEXP, SEXP, SEXP);
SEXP _rxode2_rxchisq_(SEXP, SEXP, SEXP);
SEXP _rxode2_rxcauchy_(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxbinom_(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxnbinomMu_(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxnbinom_(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxRmvn0(SEXP, SEXP, SEXP, SEXP, SEXP,
                     SEXP, SEXP, SEXP, SEXP, SEXP,
                     SEXP);
SEXP _rxode2_invWR1d(SEXP dSEXP, SEXP nuSEXP, SEXP omegaIsCholSEXP);

SEXP _rxode2_rxSimThetaOmega(SEXP paramsSEXP, SEXP omegaSEXP, SEXP omegaDfSEXP, SEXP omegaLowerSEXP, SEXP omegaUpperSEXP,
                             SEXP omegaIsCholSEXP, SEXP omegaSeparationSEXP, SEXP omegaXformSEXP, SEXP nSubSEXP, SEXP thetaMatSEXP,
                             SEXP thetaLowerSEXP, SEXP thetaUpperSEXP, SEXP thetaDfSEXP, SEXP thetaIsCholSEXP, SEXP nStudSEXP,
                             SEXP sigmaSEXP, SEXP sigmaLowerSEXP, SEXP sigmaUpperSEXP, SEXP sigmaDfSEXP, SEXP sigmaIsCholSEXP,
                             SEXP sigmaSeparationSEXP, SEXP sigmaXformSEXP, SEXP nCoresRVSEXP, SEXP nObsSEXP, SEXP dfSubSEXP,
                             SEXP dfObsSEXP, SEXP simSubjectsSEXP, SEXP simVar);

SEXP _rxode2_convertId_(SEXP);

SEXP _rxode2_rpp_(SEXP nS, SEXP lambdaS, SEXP gammaS, SEXP probS, SEXP t0S,
                  SEXP tmaxS, SEXP randomOrderS);

extern int rxIsCurrentC(SEXP obj);

rx_solve *getRxSolve_(void);

void avoid_openmp_hang_within_fork(void);
void initRxThreads(void);

// Remove these functions later...

void rxOptionsIni(void);
/* void rxOptionsIniFocei(void); */

void _update_par_ptr(double t, unsigned int id, rx_solve *rx, int idx);
double _getParCov(unsigned int id, rx_solve *rx, int parNo, int idx);

int par_progress(int c, int n, int d, int cores, clock_t t0, int stop);
void ind_solve(rx_solve *rx, unsigned int cid, t_dydt_liblsoda dydt_lls,
               t_dydt_lsoda_dum dydt_lsoda, t_jdum_lsoda jdum,
               t_dydt c_dydt, t_update_inis u_inis, int jt);
void par_solve(rx_solve *rx);
int isRstudio(void);

const char *rxGetId(int id);


double phi(double q);
SEXP _rxode2_phi(SEXP q);
SEXP _calcDerived(SEXP transSXP, SEXP ncmt, SEXP inp, SEXP dig);

double gamma_p(double, double z);
double gamma_q(double, double z);
double tgamma_lower(double a, double z);
double tgamma_upper(double a, double z);
double gamma_p_derivative(double a, double x);
double gamma_q_inv(double a, double q);
double gamma_p_inv(double a, double p);
double gamma_q_inva(double a, double q);
double gamma_p_inva(double a, double p);

int compareFactorVal(int val, const char *factor, const char *value);
SEXP _rxode2_rxSolve_(SEXP, SEXP, SEXP, SEXP, SEXP,
                      SEXP, SEXP, SEXP);

SEXP getRxThreads_R(SEXP verbose);
SEXP setRxthreads(SEXP threads, SEXP percent, SEXP throttle);

int getSilentErr(void);

int iniSubjectE(int solveid, int inLhs, rx_solving_options_ind *ind, rx_solving_options *op, rx_solve *rx,
                t_update_inis u_inis);

t_update_inis getUpdateInis(void);

void sortIds(rx_solve* rx, int ini);

void handleTlast(double *time, rx_solving_options_ind *ind);

SEXP _rxode2_rxrandnV(SEXP, SEXP);
SEXP _rxode2_rxErf(SEXP);

void simeps(int id);
void simeta(int id);

void nullGlobals(void);
SEXP _rxode2_codeLoaded(void);
SEXP _rxode2_parseModel(SEXP type);
SEXP _rxode2_isLinCmt(void);
SEXP _rxode2_trans(SEXP parse_file, SEXP prefix, SEXP model_md5, SEXP parseStr,
                   SEXP isEscIn, SEXP inME, SEXP goodFuns, SEXP fullPrintIn);
SEXP _rxode2_rxSetSeed(SEXP);
SEXP _rxode2_meanProbs_(SEXP x, SEXP probs, SEXP naRm, SEXP useT, SEXP pred, SEXP inN);
SEXP _rxode2_binomProbs_(SEXP x, SEXP probs, SEXP naRm, SEXP inN, SEXP cont);
SEXP _rxode2_binomProbsPredVec_(SEXP n, SEXP m, SEXP Y, SEXP M, SEXP doP, SEXP tol);

typedef SEXP (*rxSolveFreeSexp_t)(void);
extern void setZeroMatrix(int which);
extern rx_solve rx_global;
extern rx_solving_options op_global;
extern void setZeroMatrix(int which);
extern void rxModelsAssignC(const char *str0, SEXP assign);

SEXP _rxode2_rxSolveSetup(void);

SEXP _rxode2_etDollarNames(SEXP);
SEXP _rxode2_rxIsEt2(SEXP);
SEXP _rxode2_et_(SEXP, SEXP);
SEXP _rxode2_etUpdate(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_etSeq_(SEXP, SEXP, SEXP, SEXP, SEXP,
                    SEXP, SEXP, SEXP, SEXP, SEXP,
                    SEXP);
SEXP _rxode2_etRep_(SEXP, SEXP, SEXP, SEXP, SEXP,
                    SEXP, SEXP);
SEXP _rxode2_RcppExport_registerCCallable(void);
SEXP _rxode2_rxParseSetSilentErr(SEXP silentSEXP);

double _rxode2_evalUdf(const char *fun, int n, const double *args);

double linCmtA(rx_solve *rx, unsigned int id, double t, int linCmt,
               int ncmt, int trans, double d_ka,
               double p1, double v1,
               double p2, double p3,
               double p4, double p5,
               double d_tlag, double d_tlag2, double d_F, double d_F2,
               double d_rate, double d_dur, double d_rate2, double d_dur2);

double linCmtC(rx_solve *rx, unsigned int id, double t, int linCmt,
               int ncmt, int trans, double d_ka,
               double p1, double v1,
               double p2, double p3,
               double p4, double p5,
               double d_tlag, double d_tlag2, double d_F, double d_F2,
               double d_rate, double d_dur, double d_rate2, double d_dur2);

double linCmtB(rx_solve *rx, unsigned int id, double t, int linCmt,
               int i_cmt, int trans, int val,
               double dd_p1, double dd_v1,
               double dd_p2, double dd_p3,
               double dd_p4, double dd_p5,
               double dd_ka,
               double dd_tlag, double dd_tlag2,
               double dd_F, double dd_F2,
               double dd_rate, double dd_dur,
               double dd_rate2, double dd_dur2);

SEXP _rxode2_rxode2parseSetRstudio(SEXP isRstudioSEXP);
SEXP _rxode2_rxQr(SEXP);

SEXP _rxode2_parse_strncmpci(void);

SEXP _rxode2_etTransEvidIsObs(SEXP);
SEXP _rxode2_rxSetIni0(SEXP ini0SEXP);
SEXP _rxode2_rxEtTransAsDataFrame_(SEXP inData1SEXP);
SEXP _rxode2_swapMatListWithCube_(SEXP inOSEXP);
SEXP _rxode2_omegaListRse(SEXP omegaInSEXP);

SEXP _rxode2_rxCbindStudyIndividual(SEXP inputParameters, SEXP individualParameters);
SEXP _rxode2_rxModelVarsStack(SEXP xSEXP);
SEXP _rxode2_rxStack_(SEXP DataSEXP, SEXP varsSEXP);

SEXP _rxode2parse_linCmtB(void);

SEXP _rxode2_getWh(SEXP in);

SEXP _rxode2_parseFreeSexp(SEXP last);
SEXP _rxode2_getClassicEvid(SEXP, SEXP, SEXP, SEXP, SEXP,
                            SEXP, SEXP);
SEXP _rxode2_rxQs(SEXP);

SEXP _rxode2_solComp2(SEXP, SEXP, SEXP);
SEXP _rxode2_solComp3(SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP _rxode2_comp1c(SEXP, SEXP, SEXP, SEXP, SEXP,
                    SEXP);

SEXP _rxode2_solComp2cpp(SEXP, SEXP, SEXP);
SEXP _rxode2_solComp3cpp(SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP iniLotriPtr(SEXP ptr);
SEXP iniPreciseSumsPtr(SEXP ptr);

SEXP _rxode2_iniDparserPtr(SEXP ptr);

SEXP _rxode2_rxode2Ptr(void) {
  int pro = 0;  // Counter for the number of PROTECT calls
  // Create an external pointer for _lotriLstToMat
  SEXP rxode2rxRmvnSEXP = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&_rxode2_rxRmvnSEXP, R_NilValue, R_NilValue)); pro++;

  SEXP rxode2rxParProgress = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&par_progress, R_NilValue, R_NilValue)); pro++;

  SEXP rxode2getRxSolve_ = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getRxSolve_, R_NilValue,
                                                       R_NilValue)); pro++;
  SEXP rxode2indSolve = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&ind_solve, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getTime = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getTime, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2isRstudio = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&isRstudio, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2iniSubjectE = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&iniSubjectE, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2sortIds = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&sortIds, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2_rxode2_rxModelVars_ = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&_rxode2_rxModelVars_, R_NilValue, R_NilValue)); pro++;

  SEXP rxode2_par_solve = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&par_solve, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2rxGetId = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&rxGetId, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getSolvingOptions = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getSolvingOptions, R_NilValue, R_NilValue)); pro++;

  SEXP rxode2getSolvingOptionsInd = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getSolvingOptionsInd, R_NilValue, R_NilValue)); pro++;

  SEXP rxode2getIndLambda = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getIndLambda, R_NilValue, R_NilValue)); pro++;

  SEXP rxode2getIndLambdaYj =  PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getIndLambdaYj, R_NilValue, R_NilValue)); pro++;

  SEXP rxode2getIndLogitLow = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getIndLogitLow, R_NilValue, R_NilValue)); pro++;

  SEXP rxode2getIndLogitHi = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getIndLogitHi, R_NilValue, R_NilValue)); pro++;

  SEXP rxode2setIndParPtr = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&setIndParPtr, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getIndParPtr = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getIndParPtr, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getIndNallTimes = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getIndNallTimes, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2setIndIdx = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&setIndIdx, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getIndIx = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getIndIx, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getIndEvid = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getIndEvid, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getIndLhs = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getIndLhs, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getIndNdoses = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getIndNdoses, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getIndNevid2 = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getIndNevid2, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2setIndSolve = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&setIndSolve, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getIndSolve = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getIndSolve, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getIndDv = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getIndDv, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getIndYj = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getIndYj, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getIndLimit = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getIndLimit, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getIndCens = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getIndCens, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getIndIdx = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getIndIdx, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getOpNeq = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getOpNeq, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2setOpNeq =  PROTECT(R_MakeExternalPtrFn((DL_FUNC)&setOpNeq, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2hasOpBadSolve = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&hasOpBadSolve, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getOpNlin = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getOpNlin, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getOpCores = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getOpCores, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getOpNlhs = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getOpNlhs, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getOpStiff = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getOpStiff, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2resetOpBadSolve = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&resetOpBadSolve, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getRxNsub = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getRxNsub, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2hasRxLimit = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&hasRxLimit, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2hasRxCens = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&hasRxCens, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getRxNall = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getRxNall, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getRxNobs = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getRxNobs, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getRxNobs2 = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getRxNobs2, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getOpIndSolve = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getOpIndSolve, R_NilValue, R_NilValue)); pro++;
  SEXP rxode2getRxNpars = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&getRxNpars,
                                                      R_NilValue, R_NilValue)); pro++;


#define nVec 49

  SEXP ret = PROTECT(Rf_allocVector(VECSXP, nVec)); pro++;
  SET_VECTOR_ELT(ret, 0, rxode2rxRmvnSEXP);
  SET_VECTOR_ELT(ret, 1, rxode2rxParProgress);
  SET_VECTOR_ELT(ret, 2, rxode2getRxSolve_);
  SET_VECTOR_ELT(ret, 3, rxode2indSolve);
  SET_VECTOR_ELT(ret, 4, rxode2getTime);
  SET_VECTOR_ELT(ret, 5, rxode2isRstudio);
  SET_VECTOR_ELT(ret, 6, rxode2iniSubjectE);
  SET_VECTOR_ELT(ret, 7, rxode2sortIds);
  SET_VECTOR_ELT(ret, 8, rxode2getSolvingOptions);
  SET_VECTOR_ELT(ret, 9, rxode2getSolvingOptionsInd);
  SET_VECTOR_ELT(ret, 10, rxode2_rxode2_rxModelVars_);
  SET_VECTOR_ELT(ret, 11, rxode2_par_solve);
  SET_VECTOR_ELT(ret, 12, rxode2rxGetId);
  SET_VECTOR_ELT(ret, 13, rxode2getIndLambda);
  SET_VECTOR_ELT(ret, 14, rxode2getIndLambdaYj);
  SET_VECTOR_ELT(ret, 15, rxode2getIndLogitLow);
  SET_VECTOR_ELT(ret, 16, rxode2getIndLogitHi);
  SET_VECTOR_ELT(ret, 17, rxode2setIndParPtr);
  SET_VECTOR_ELT(ret, 18, rxode2getIndParPtr);
  SET_VECTOR_ELT(ret, 19, rxode2getIndNallTimes);
  SET_VECTOR_ELT(ret, 20, rxode2setIndIdx);
  SET_VECTOR_ELT(ret, 21, rxode2getIndIx);
  SET_VECTOR_ELT(ret, 22, rxode2getIndEvid);
  SET_VECTOR_ELT(ret, 23, rxode2getIndLhs);
  SET_VECTOR_ELT(ret, 24, rxode2getIndNdoses);
  SET_VECTOR_ELT(ret, 25, rxode2getIndNevid2);
  SET_VECTOR_ELT(ret, 26, rxode2setIndSolve);
  SET_VECTOR_ELT(ret, 27, rxode2getIndSolve);
  SET_VECTOR_ELT(ret, 28, rxode2getIndDv);
  SET_VECTOR_ELT(ret, 29, rxode2getIndYj);
  SET_VECTOR_ELT(ret, 30, rxode2getIndLimit);
  SET_VECTOR_ELT(ret, 31, rxode2getIndCens);
  SET_VECTOR_ELT(ret, 32, rxode2getIndIdx);
  SET_VECTOR_ELT(ret, 33, rxode2getOpNeq);
  SET_VECTOR_ELT(ret, 34, rxode2setOpNeq);
  SET_VECTOR_ELT(ret, 35, rxode2hasOpBadSolve);
  SET_VECTOR_ELT(ret, 36, rxode2getOpNlin);
  SET_VECTOR_ELT(ret, 37, rxode2getOpCores);
  SET_VECTOR_ELT(ret, 38, rxode2getOpNlhs);
  SET_VECTOR_ELT(ret, 39, rxode2getOpStiff);
  SET_VECTOR_ELT(ret, 40, rxode2resetOpBadSolve);
  SET_VECTOR_ELT(ret, 41, rxode2getRxNsub);
  SET_VECTOR_ELT(ret, 42, rxode2hasRxLimit);
  SET_VECTOR_ELT(ret, 43, rxode2hasRxCens);
  SET_VECTOR_ELT(ret, 44, rxode2getRxNall);
  SET_VECTOR_ELT(ret, 45, rxode2getRxNobs);
  SET_VECTOR_ELT(ret, 46, rxode2getRxNobs2);
  SET_VECTOR_ELT(ret, 47, rxode2getOpIndSolve);
  SET_VECTOR_ELT(ret, 48, rxode2getRxNpars);

  SEXP retN = PROTECT(Rf_allocVector(STRSXP, nVec)); pro++;
  SET_STRING_ELT(retN, 0, Rf_mkChar("rxode2rxRmvnSEXP"));
  SET_STRING_ELT(retN, 1, Rf_mkChar("rxode2rxParProgress"));
  SET_STRING_ELT(retN, 2, Rf_mkChar("rxode2getRxSolve_"));
  SET_STRING_ELT(retN, 3, Rf_mkChar("rxode2indSolve"));
  SET_STRING_ELT(retN, 4, Rf_mkChar("rxode2getTime"));
  SET_STRING_ELT(retN, 5, Rf_mkChar("rxode2isRstudio"));
  SET_STRING_ELT(retN, 6, Rf_mkChar("rxode2iniSubjectE"));
  SET_STRING_ELT(retN, 7, Rf_mkChar("rxode2sortIds"));
  SET_STRING_ELT(retN, 8, Rf_mkChar("getSolvingOptionsInd"));
  SET_STRING_ELT(retN, 9, Rf_mkChar("rxode2getUpdateInis"));
  SET_STRING_ELT(retN, 10, Rf_mkChar("rxode2_rxode2_rxModelVars_"));
  SET_STRING_ELT(retN, 11, Rf_mkChar("rxode2_par_solve"));
  SET_STRING_ELT(retN, 12, Rf_mkChar("rxode2rxGetId"));
  SET_STRING_ELT(retN, 13, Rf_mkChar("rxode2getIndLambda"));
  SET_STRING_ELT(retN, 14, Rf_mkChar("rxode2getIndLambdaYj"));
  SET_STRING_ELT(retN, 15, Rf_mkChar("rxode2getIndLogitLow"));
  SET_STRING_ELT(retN, 16, Rf_mkChar("rxode2getIndLogitHi"));
  SET_STRING_ELT(retN, 17, Rf_mkChar("rxode2setIndParPtr"));
  SET_STRING_ELT(retN, 18, Rf_mkChar("rxode2getIndParPtr"));
  SET_STRING_ELT(retN, 19, Rf_mkChar("rxode2getIndNallTimes"));
  SET_STRING_ELT(retN, 20, Rf_mkChar("rxode2setIndIdx"));
  SET_STRING_ELT(retN, 21, Rf_mkChar("rxode2getIndIx"));
  SET_STRING_ELT(retN, 22, Rf_mkChar("rxode2getIndEvid"));
  SET_STRING_ELT(retN, 23, Rf_mkChar("rxode2getIndLhs"));
  SET_STRING_ELT(retN, 24, Rf_mkChar("rxode2getIndNdoses"));
  SET_STRING_ELT(retN, 25, Rf_mkChar("rxode2getIndNevid2"));
  SET_STRING_ELT(retN, 26, Rf_mkChar("rxode2setIndSolve"));
  SET_STRING_ELT(retN, 27, Rf_mkChar("rxode2getIndSolve"));
  SET_STRING_ELT(retN, 28, Rf_mkChar("rxode2getIndDv"));
  SET_STRING_ELT(retN, 29, Rf_mkChar("rxode2getIndYj"));
  SET_STRING_ELT(retN, 30, Rf_mkChar("rxode2getIndLimit"));
  SET_STRING_ELT(retN, 31, Rf_mkChar("rxode2getIndCens"));
  SET_STRING_ELT(retN, 32, Rf_mkChar("rxode2getIndIdx"));
  SET_STRING_ELT(retN, 33, Rf_mkChar("rxode2getOpNeq"));
  SET_STRING_ELT(retN, 34, Rf_mkChar("rxode2setOpNeq"));
  SET_STRING_ELT(retN, 35, Rf_mkChar("rxode2hasOpBadSolve"));
  SET_STRING_ELT(retN, 36, Rf_mkChar("rxode2getOpNlin"));
  SET_STRING_ELT(retN, 37, Rf_mkChar("rxode2getOpCores"));
  SET_STRING_ELT(retN, 38, Rf_mkChar("rxode2getOpNlhs"));
  SET_STRING_ELT(retN, 39, Rf_mkChar("rxode2getOpStiff"));
  SET_STRING_ELT(retN, 40, Rf_mkChar("rxode2resetOpBadSolve"));
  SET_STRING_ELT(retN, 41, Rf_mkChar("rxode2getRxNsub"));
  SET_STRING_ELT(retN, 42, Rf_mkChar("rxode2hasRxLimit"));
  SET_STRING_ELT(retN, 43, Rf_mkChar("rxode2hasRxCens"));
  SET_STRING_ELT(retN, 44, Rf_mkChar("rxode2getRxNall"));
  SET_STRING_ELT(retN, 45, Rf_mkChar("rxode2getRxNobs"));
  SET_STRING_ELT(retN, 46, Rf_mkChar("rxode2getRxNobs2"));
  SET_STRING_ELT(retN, 47, Rf_mkChar("rxode2getOpIndSolve"));
  SET_STRING_ELT(retN, 48, Rf_mkChar("rxode2getRxNpars"));
#undef nVec

  // Set the names attribute of the list
  Rf_setAttrib(ret, R_NamesSymbol, retN);

  // Unprotect all protected objects
  UNPROTECT(pro);

  // Return the list of external pointers
  return ret;
}

SEXP _rxode2_powerD(SEXP, SEXP, SEXP, SEXP, SEXP,
                    SEXP);
SEXP _rxode2_activationF(SEXP xS, SEXP typeS);
SEXP _rxode2_activationF2(SEXP xS, SEXP aS, SEXP typeS);
void R_init_rxode2(DllInfo *info){
  R_CallMethodDef callMethods[]  = {
    {"_rxode2_solComp2cpp", (DL_FUNC) &_rxode2_solComp2cpp, 3},
    {"_rxode2_solComp3cpp", (DL_FUNC) &_rxode2_solComp3cpp, 5},
    {"_rxode2_comp1c", (DL_FUNC) &_rxode2_comp1c, 6},
    {"_rxode2_solComp2", (DL_FUNC) &_rxode2_solComp2, 3},
    {"_rxode2_solComp3", (DL_FUNC) &_rxode2_solComp3, 5},
    {"_rxode2_activationF2", (DL_FUNC) &_rxode2_activationF2, 3},
    {"_rxode2_activationF", (DL_FUNC) &_rxode2_activationF, 2},
    {"_rxode2_itoletter", (DL_FUNC) &_rxode2_itoletter, 2},
    {"_rxode2_itostr", (DL_FUNC) &_rxode2_itostr, 2},
    {"_rxode2_powerD", (DL_FUNC) &_rxode2_powerD, 6},
    {"_rxode2_rxode2Ptr", (DL_FUNC) &_rxode2_rxode2Ptr, 0},
    {"_rxode2_iniDparserPtr", (DL_FUNC) &_rxode2_iniDparserPtr, 1},
    {"_iniPreciseSumsPtr", (DL_FUNC) &iniPreciseSumsPtr, 1},
    {"_iniLotriPtr", (DL_FUNC) &iniLotriPtr, 1},
    {"_rxode2_rxode2parseSetRstudio", (DL_FUNC) &_rxode2_rxode2parseSetRstudio, 1},
    {"_rxode2_rxQs", (DL_FUNC) &_rxode2_rxQs, 1},
    {"_rxode2_rxQr", (DL_FUNC) &_rxode2_rxQr, 1},
    {"_rxode2_getClassicEvid", (DL_FUNC) &_rxode2_getClassicEvid, 7},
    {"_rxode2_parseFreeSexp", (DL_FUNC) &_rxode2_parseFreeSexp, 1},
    {"_rxode2_getWh", (DL_FUNC) &_rxode2_getWh, 1},
    {"_rxode2parse_linCmtB", (DL_FUNC) &_rxode2parse_linCmtB, 0},
    {"_rxode2_rxStack_", (DL_FUNC) &_rxode2_rxStack_, 2},
    {"_rxode2_rxCbindStudyIndividual", (DL_FUNC) &_rxode2_rxCbindStudyIndividual, 2},
    {"_rxode2_rxModelVarsStack", (DL_FUNC) &_rxode2_rxModelVarsStack, 1},
    {"_rxode2_omegaListRse", (DL_FUNC) &_rxode2_omegaListRse, 1},
    {"_rxode2_swapMatListWithCube_", (DL_FUNC) &_rxode2_swapMatListWithCube_, 1},
    {"_rxode2_rxEtTransAsDataFrame_", (DL_FUNC) &_rxode2_rxEtTransAsDataFrame_, 1},
    {"_rxode2_rxSetIni0", (DL_FUNC) &_rxode2_rxSetIni0, 1},
    {"_rxode2_etTransEvidIsObs", (DL_FUNC) &_rxode2_etTransEvidIsObs, 1},
    {"_rxode2_parse_strncmpci",(DL_FUNC) &_rxode2_parse_strncmpci, 0},
    {"_rxode2_rxParseSetSilentErr", (DL_FUNC) &_rxode2_rxParseSetSilentErr, 1},
    {"_rxode2_etRep_", (DL_FUNC) &_rxode2_etRep_, 7},
    {"_rxode2_etSeq_", (DL_FUNC) &_rxode2_etSeq_, 11},
    {"_rxode2_etUpdate", (DL_FUNC) &_rxode2_etUpdate, 4},
    {"_rxode2_et_", (DL_FUNC) &_rxode2_et_, 2},
    {"_rxode2_rxIsEt2", (DL_FUNC) &_rxode2_rxIsEt2, 1},
    {"_rxode2_etDollarNames", (DL_FUNC) &_rxode2_etDollarNames, 1},
    {"_rxode2_rxSolveSetup", (DL_FUNC) &_rxode2_rxSolveSetup, 0},
    {"_rxode2_isIntel", (DL_FUNC) &_rxode2_isIntel, 0},
    {"_rxode2_binomProbsPredVec_", (DL_FUNC) &_rxode2_binomProbsPredVec_, 6},
    {"_rxode2_binomProbs_", (DL_FUNC) &_rxode2_binomProbs_, 5},
    {"_rxode2_meanProbs_", (DL_FUNC) &_rxode2_meanProbs_, 6},
    {"_rxProgress", (DL_FUNC) &_rxProgress, 2},
    {"_rxTick", (DL_FUNC) &_rxTick, 0},
    {"_rxProgressStop", (DL_FUNC) &_rxProgressStop, 1},
    {"_rxProgressAbort", (DL_FUNC) &_rxProgressAbort, 1},
    {"_rxode2_trans", (DL_FUNC) &_rxode2_trans, 8},
    {"_rxode2_codegen", (DL_FUNC) &_rxode2_codegen, 7},
    {"_rxode2_codeLoaded", (DL_FUNC) &_rxode2_codeLoaded, 0},
    {"_rxode2_parseModel", (DL_FUNC) &_rxode2_parseModel, 1},
    {"_rxode2_isLinCmt", (DL_FUNC) &_rxode2_isLinCmt, 0},
    {"rxode2_get_mv", (DL_FUNC) &rxode2_get_mv, 0},
    {"_rxode2_rxGetSeed", (DL_FUNC) &_rxode2_rxGetSeed, 0},
    {"_rxode2_rxInv", (DL_FUNC) &_rxode2_rxInv, 1},
    {"_rxCholInv", (DL_FUNC) &_rxCholInv, 3},
    {"_rxode2_rxSymInvCholEnvCalculate", (DL_FUNC) &_rxode2_rxSymInvCholEnvCalculate, 3},
    {"_rxode2_rxSymInvChol", (DL_FUNC) &_rxode2_rxSymInvChol, 4},
    {"_rxode2_rxIs", (DL_FUNC) &_rxode2_rxIs, 2},
    {"_rxode2_rxModelVars_", (DL_FUNC) &_rxode2_rxModelVars_, 1},
    {"_rxode2_rxState", (DL_FUNC) &_rxode2_rxState, 2},
    {"_rxode2_rxParams_", (DL_FUNC) &_rxode2_rxParams_, 1},
    {"_rxode2_rxDfdy", (DL_FUNC) &_rxode2_rxDfdy, 1},
    {"_rxode2_rxLhs", (DL_FUNC) &_rxode2_rxLhs, 1},
    {"_rxode2_rxInits", (DL_FUNC) &_rxode2_rxInits, 7},
    {"_rxode2_rxSetupIni", (DL_FUNC) &_rxode2_rxSetupIni, 2},
    {"_rxode2_rxSetupScale", (DL_FUNC) &_rxode2_rxSetupScale, 3},
    {"_rxode2_rxSolveGet", (DL_FUNC) &_rxode2_rxSolveGet, 3},
    {"_rxode2_rxSolveUpdate", (DL_FUNC) &_rxode2_rxSolveUpdate, 3},
    {"_rxode2_rxAssignPtr", (DL_FUNC) &_rxode2_rxAssignPtr, 1},
    {"_rxode2_rxRmModelLib_",(DL_FUNC) &_rxode2_rxRmModelLib_, 1},
    {"_rxode2_rxDll",(DL_FUNC) &_rxode2_rxDll, 1},
    {"_rxode2_rxC",(DL_FUNC) &_rxode2_rxC, 1},
    {"_rxode2_rxIsLoaded", (DL_FUNC) &_rxode2_rxIsLoaded, 1},
    {"_rxode2_rxDynUnload", (DL_FUNC) &_rxode2_rxDynUnload, 1},
    {"_rxode2_rxDynLoad", (DL_FUNC) &_rxode2_rxDynLoad, 1},
    {"_rxode2_rxDelete", (DL_FUNC) &_rxode2_rxDelete, 1},
    {"_rxode2_rxGetrxode2", (DL_FUNC) &_rxode2_rxGetrxode2, 1},
    {"_rxode2_rxIsCurrent", (DL_FUNC) &_rxode2_rxIsCurrent, 1},
    {"_rxode2_cvPost_", (DL_FUNC) &_rxode2_cvPost_, 7},
    {"_rxode2_rinvchisq", (DL_FUNC) &_rxode2_rinvchisq, 3},
    {"_rxode2_dynLoad", (DL_FUNC) &_rxode2_dynLoad, 1},
    {"_rxode2_rxSolveFree", (DL_FUNC) &_rxode2_rxSolveFree, 0},
    {"_rxode2_setRstudio", (DL_FUNC) &_rxode2_setRstudio, 1},
    {"_rxode2_RcppExport_registerCCallable", (DL_FUNC) &_rxode2_RcppExport_registerCCallable, 0},
    {"_rxode2_setProgSupported", (DL_FUNC) &_rxode2_setProgSupported, 1},
    {"_rxode2_getProgSupported", (DL_FUNC) &_rxode2_getProgSupported, 0},
    {"_rxode2_rxUpdateTrans_", (DL_FUNC) &_rxode2_rxUpdateTrans_, 3},
    {"_rxode2_etTrans", (DL_FUNC) &_rxode2_etTrans, 12},
    {"_rxode2_rxSolveSEXP", (DL_FUNC) &_rxode2_rxSolveSEXP, 8},
    {"_rxode2_dropUnitsRxSolve", (DL_FUNC) &_rxode2_dropUnitsRxSolve, 1},
    {"_rxode2_atolRtolFactor_", (DL_FUNC) &_rxode2_atolRtolFactor_, 1},
    {"_rxode2_rxExpandGrid_", (DL_FUNC) &_rxode2_rxExpandGrid_, 3},
    {"_rxode2_rxExpandSens_", (DL_FUNC) &_rxode2_rxExpandSens_, 2},
    {"_rxode2_rxExpandSens2_",(DL_FUNC) &_rxode2_rxExpandSens2_, 3},
    {"_rxode2_rxExpandFEta_", (DL_FUNC) &_rxode2_rxExpandFEta_, 4},
    {"_rxode2_rxRepR0_", (DL_FUNC) &_rxode2_rxRepR0_, 1},
    {"_rxode2_rxOptRep_", (DL_FUNC) &_rxode2_rxOptRep_, 1},
    {"_rxode2_rxSetSilentErr", (DL_FUNC) &_rxode2_rxSetSilentErr, 1},
    {"_rxode2_rxIndLin_",(DL_FUNC) &_rxode2_rxIndLin_, 1},
    {"_rxode2_rxUnloadAll_", (DL_FUNC) &_rxode2_rxUnloadAll_, 0},
    {"_rxode2_rxLock", (DL_FUNC) &_rxode2_rxLock, 1},
    {"_rxode2_rxUnlock", (DL_FUNC) &_rxode2_rxUnlock, 1},
    {"_rxode2_rxAllowUnload", (DL_FUNC) &_rxode2_rxAllowUnload, 1},
    {"_rxParProgress", (DL_FUNC) &_rxParProgress, 1},
    {"_rxode2_rLKJ1", (DL_FUNC) &_rxode2_rLKJ1, 3},
    {"_rxode2_rLKJcv1", (DL_FUNC) &_rxode2_rLKJcv1, 2},
    {"_rxode2_rLKJcvLsd1", (DL_FUNC) &_rxode2_rLKJcvLsd1, 3},
    {"_rxode2_rcvC1", (DL_FUNC) &_rxode2_rcvC1, 5},
    {"_rxode2_rxRmvn_", (DL_FUNC) &_rxode2_rxRmvn_, 5},
    {"_rxode2_rxCholperm", (DL_FUNC) &_rxode2_rxCholperm, 4},
    {"_rxode2_rxGradpsi", (DL_FUNC) &_rxode2_rxGradpsi, 4},
    {"_rxode2_rxNleq", (DL_FUNC) &_rxode2_rxNleq, 3},
    {"_rxode2_rxMvnrnd", (DL_FUNC) &_rxode2_rxMvnrnd, 7},
    {"_rxode2_rxMvrandn_", (DL_FUNC) &_rxode2_rxMvrandn_, 10},
    {"_rxode2_rxSeedEng", (DL_FUNC) &_rxode2_rxSeedEng, 1},
    {"_rxode2_rxnorm_", (DL_FUNC) &_rxode2_rxnorm_, 4},
    {"_rxode2_rxpois_", (DL_FUNC) &_rxode2_rxpois_, 3},
    {"_rxode2_rxt__", (DL_FUNC) &_rxode2_rxt__, 3},
    {"_rxode2_rxunif_", (DL_FUNC) &_rxode2_rxunif_, 4},
    {"_rxode2_rxweibull_", (DL_FUNC) &_rxode2_rxweibull_, 4},
    {"_rxode2_rxgeom_", (DL_FUNC) &_rxode2_rxgeom_, 3},
    {"_rxode2_rxbeta_", (DL_FUNC) &_rxode2_rxbeta_, 4},
    {"_rxode2_rxgamma_", (DL_FUNC) &_rxode2_rxgamma_, 4},
    {"_rxode2_rxf_", (DL_FUNC) &_rxode2_rxf_, 4},
    {"_rxode2_rxexp_", (DL_FUNC) &_rxode2_rxexp_, 3},
    {"_rxode2_rxchisq_", (DL_FUNC) &_rxode2_rxchisq_, 3},
    {"_rxode2_rxcauchy_", (DL_FUNC) &_rxode2_rxcauchy_, 4},
    {"_rxode2_rxbinom_", (DL_FUNC) &_rxode2_rxbinom_, 4},
    {"_rxode2_rxnbinomMu_", (DL_FUNC) &_rxode2_rxnbinomMu_, 4},
    {"_rxode2_rxnbinom_", (DL_FUNC) &_rxode2_rxnbinomMu_, 4},
    {"_rxode2_rxSolveDollarNames", (DL_FUNC) _rxode2_rxSolveDollarNames, 1},
    {"_rxode2_rxExpandNesting", (DL_FUNC) _rxode2_rxExpandNesting, 3},
    {"_rxode2_rxRmvn0", (DL_FUNC) _rxode2_rxRmvn0, 11},
    {"_vecDF", (DL_FUNC) _vecDF, 2},
    {"_cbindOme", (DL_FUNC) _cbindOme, 3},
    {"_rxode2_rxRmvnSEXP", (DL_FUNC) _rxode2_rxRmvnSEXP, 12},
    {"_rxode2_expandPars_", (DL_FUNC) _rxode2_expandPars_, 4},
    {"_rxode2_convertId_", (DL_FUNC) _rxode2_convertId_, 1},
    {"_rxode2_nestingInfo_", (DL_FUNC) _rxode2_nestingInfo_, 2},
    {"_phi", (DL_FUNC) _rxode2_phi, 1},
    {"_rxode2_expandTheta_", (DL_FUNC) _rxode2_expandTheta_, 6},
    {"_gammap", (DL_FUNC) _gammap, 2},
    {"_gammaq", (DL_FUNC) _gammaq, 2},
    {"_uppergamma", (DL_FUNC) _uppergamma, 2},
    {"_lowergamma", (DL_FUNC) _lowergamma, 2},
    {"_gammapDer", (DL_FUNC) _gammapDer, 2},
    {"_gammapInv", (DL_FUNC) _gammapInv, 2},
    {"_gammaqInv", (DL_FUNC) _gammaqInv, 2},
    {"_gammapInva", (DL_FUNC) _gammapInv, 2},
    {"_gammaqInva", (DL_FUNC) _gammaqInv, 2},
    {"_calcDerived", (DL_FUNC) _calcDerived, 4},
    {"_linCmtParse", (DL_FUNC) _linCmtParse, 3},
    {"_rxode2_linCmtGen", (DL_FUNC) _rxode2_linCmtGen, 4},
    {"_rxode2_rpp_", (DL_FUNC) _rxode2_rpp_, 7},
    {"_rxode2_rxSolve_", (DL_FUNC) _rxode2_rxSolve_, 8},
    {"getRxThreads_R", (DL_FUNC) getRxThreads_R, 1},
    {"setRxthreads", (DL_FUNC) setRxthreads, 3},
    {"_rxHasOpenMp", (DL_FUNC) _rxHasOpenMp, 0},
    {"_rxode2_isNullZero", (DL_FUNC) _rxode2_isNullZero, 1},
    {"_rxode2_invWR1d", (DL_FUNC) _rxode2_invWR1d, 3},
    {"_rxode2_rxSimThetaOmega", (DL_FUNC) _rxode2_rxSimThetaOmega, 28},
    {"_rxSetSeed", (DL_FUNC) _rxode2_rxSetSeed, 1},
    {"_rxode2_rxordSelect", (DL_FUNC) _rxode2_rxordSelect, 2},
    {"_rxode2_rxErf", (DL_FUNC) &_rxode2_rxErf, 1},
    {NULL, NULL, 0}
  };
  // C callable to assign environments.
  R_RegisterCCallable("rxode2", "_rxode2_rxRmvnSEXP",
                      (DL_FUNC) &_rxode2_rxRmvnSEXP);
  R_RegisterCCallable("rxode2", "_rxode2_evalUdf", (DL_FUNC) &_rxode2_evalUdf);
  R_RegisterCCallable("rxode2", "_rxode2_rxQr", (DL_FUNC) &_rxode2_rxQr);

  R_RegisterCCallable("rxode2", "linCmtA", (DL_FUNC) &linCmtA);
  R_RegisterCCallable("rxode2", "linCmtB", (DL_FUNC) &linCmtB);
  R_RegisterCCallable("rxode2", "linCmtC", (DL_FUNC) &linCmtC);


  R_RegisterCCallable("rxode2", "_rxode2_rxModelVars_", (DL_FUNC) &_rxode2_rxModelVars_);
  R_RegisterCCallable("rxode2", "getSilentErr", (DL_FUNC) &getSilentErr);
  R_RegisterCCallable("rxode2", "logit", (DL_FUNC) &logit);
  R_RegisterCCallable("rxode2", "expit", (DL_FUNC) &expit);
  R_RegisterCCallable("rxode2", "ReLU", (DL_FUNC) &ReLU);
  R_RegisterCCallable("rxode2", "dReLU", (DL_FUNC) &dReLU);
  R_RegisterCCallable("rxode2", "GELU", (DL_FUNC) &GELU);
  R_RegisterCCallable("rxode2", "dGELU", (DL_FUNC) &dGELU);
  R_RegisterCCallable("rxode2", "d2GELU", (DL_FUNC) &d2GELU);
  R_RegisterCCallable("rxode2", "d3GELU", (DL_FUNC) &d3GELU);
  R_RegisterCCallable("rxode2", "d4GELU", (DL_FUNC) &d4GELU);
  R_RegisterCCallable("rxode2", "ELU", (DL_FUNC) &ELU);
  R_RegisterCCallable("rxode2", "dELU", (DL_FUNC) &dELU);
  R_RegisterCCallable("rxode2", "d2ELU", (DL_FUNC) &d2ELU);
  R_RegisterCCallable("rxode2", "d2aELU", (DL_FUNC) &d2aELU);
  R_RegisterCCallable("rxode2", "dELUa", (DL_FUNC) &dELUa);
  R_RegisterCCallable("rxode2", "d2ELUa", (DL_FUNC) &d2ELUa);
  R_RegisterCCallable("rxode2", "softplus", (DL_FUNC) &softplus);
  R_RegisterCCallable("rxode2", "dsoftplus", (DL_FUNC) &dsoftplus);
  R_RegisterCCallable("rxode2", "d2softplus", (DL_FUNC) &d2softplus);
  R_RegisterCCallable("rxode2", "d3softplus", (DL_FUNC) &d3softplus);
  R_RegisterCCallable("rxode2", "d4softplus", (DL_FUNC) &d4softplus);
  R_RegisterCCallable("rxode2", "SELU", (DL_FUNC) &SELU);
  R_RegisterCCallable("rxode2", "dSELU", (DL_FUNC) &dSELU);
  R_RegisterCCallable("rxode2", "lReLU", (DL_FUNC) &lReLU);
  R_RegisterCCallable("rxode2", "dlReLU", (DL_FUNC) &dlReLU);
  R_RegisterCCallable("rxode2", "PReLU", (DL_FUNC) &PReLU);
  R_RegisterCCallable("rxode2", "dPReLU", (DL_FUNC) &dPReLU);
  R_RegisterCCallable("rxode2", "dPReLUa", (DL_FUNC) &dPReLUa);
  R_RegisterCCallable("rxode2", "dPReLUa1", (DL_FUNC) &dPReLUa1);
  R_RegisterCCallable("rxode2", "Swish", (DL_FUNC) &Swish);
  R_RegisterCCallable("rxode2", "dSwish", (DL_FUNC) &dSwish);
  R_RegisterCCallable("rxode2", "powerDi", (DL_FUNC) &powerDi);
  R_RegisterCCallable("rxode2", "powerD", (DL_FUNC) &powerD);
  R_RegisterCCallable("rxode2", "powerDD", (DL_FUNC) &powerDD);
  R_RegisterCCallable("rxode2", "powerDDD", (DL_FUNC) &powerDDD);
  R_RegisterCCallable("rxode2", "powerL", (DL_FUNC) &powerL);
  R_RegisterCCallable("rxode2", "powerDL", (DL_FUNC) &powerDL);
  R_RegisterCCallable("rxode2", "par_progress", (DL_FUNC) &par_progress);
  R_RegisterCCallable("rxode2", "isRstudio", (DL_FUNC) &isRstudio);
  R_RegisterCCallable("rxode2", "ind_solve", (DL_FUNC) &ind_solve);
  R_RegisterCCallable("rxode2", "par_solve", (DL_FUNC) &par_solve);
  R_RegisterCCallable("rxode2", "_update_par_ptr", (DL_FUNC) &_update_par_ptr);
  R_RegisterCCallable("rxode2", "_getParCov", (DL_FUNC) &_getParCov);
  R_RegisterCCallable("rxode2","rxRmModelLib", (DL_FUNC) &rxRmModelLib);
  R_RegisterCCallable("rxode2","rxGetModelLib", (DL_FUNC) &rxGetModelLib);

  R_RegisterCCallable("rxode2","rxode2_ode_free", (DL_FUNC) &rxode2_ode_free);

  //Functions
      R_RegisterCCallable("rxode2","rxode2_sum",                (DL_FUNC) &rxode2_sum);
  R_RegisterCCallable("rxode2","rxode2_prod",               (DL_FUNC) &rxode2_prod);

  R_RegisterCCallable("rxode2","rxode2_assign_fn_pointers", (DL_FUNC) &rxode2_assign_fn_pointers);

  R_RegisterCCallable("rxode2","_rxode2_rxAssignPtr",       (DL_FUNC) _rxode2_rxAssignPtr);
  R_RegisterCCallable("rxode2", "rxIsCurrentC", (DL_FUNC) rxIsCurrentC);
  R_RegisterCCallable("rxode2","rxode2_current_fn_pointer_id", (DL_FUNC) &rxode2_current_fn_pointer_id);
  R_RegisterCCallable("rxode2","getRxSolve_", (DL_FUNC) &getRxSolve_);

  R_RegisterCCallable("rxode2", "gammap", (DL_FUNC) &gamma_p);
  R_RegisterCCallable("rxode2", "gammaq", (DL_FUNC) &gamma_q);
  R_RegisterCCallable("rxode2", "lowergamma", (DL_FUNC) &tgamma_lower);
  R_RegisterCCallable("rxode2", "uppergamma", (DL_FUNC) &tgamma_upper);
  R_RegisterCCallable("rxode2", "gammapDer", (DL_FUNC) &gamma_p_derivative);
  R_RegisterCCallable("rxode2", "gammapInv", (DL_FUNC) &gamma_p_inv);
  R_RegisterCCallable("rxode2", "gammapInva", (DL_FUNC) &gamma_p_inva);
  R_RegisterCCallable("rxode2", "gammaqInv", (DL_FUNC) &gamma_q_inv);
  R_RegisterCCallable("rxode2", "gammaqInva", (DL_FUNC) &gamma_q_inva);
  R_RegisterCCallable("rxode2", "compareFactorVal", (DL_FUNC) &compareFactorVal);
  R_RegisterCCallable("rxode2", "handleTlast", (DL_FUNC) &handleTlast);
  R_RegisterCCallable("rxode2", "phi", (DL_FUNC) &phi);
  R_RegisterCCallable("rxode2", "ribeta", (DL_FUNC) &ribeta);
  R_RegisterCCallable("rxode2", "ribinom", (DL_FUNC) &ribinom);
  R_RegisterCCallable("rxode2", "ricauchy", (DL_FUNC) &ricauchy);
  R_RegisterCCallable("rxode2", "richisq", (DL_FUNC) &richisq);
  R_RegisterCCallable("rxode2", "riexp", (DL_FUNC) &riexp);
  R_RegisterCCallable("rxode2", "rif", (DL_FUNC) &rif);
  R_RegisterCCallable("rxode2", "rigamma", (DL_FUNC) &rigamma);
  R_RegisterCCallable("rxode2", "rigeom", (DL_FUNC) &rigeom);
  R_RegisterCCallable("rxode2", "rinbinom", (DL_FUNC) &rinbinom);
  R_RegisterCCallable("rxode2", "rinbinomMu", (DL_FUNC) &rinbinomMu);
  R_RegisterCCallable("rxode2", "rinorm", (DL_FUNC) &rinorm);
  R_RegisterCCallable("rxode2", "ripois", (DL_FUNC) &ripois);
  R_RegisterCCallable("rxode2", "rit_", (DL_FUNC) &rit_);
  R_RegisterCCallable("rxode2", "riunif", (DL_FUNC) &riunif);
  R_RegisterCCallable("rxode2", "riweibull", (DL_FUNC) &riweibull);
  R_RegisterCCallable("rxode2", "rxbeta", (DL_FUNC) &rxbeta);
  R_RegisterCCallable("rxode2", "rxbinom", (DL_FUNC) &rxbinom);
  R_RegisterCCallable("rxode2", "rxcauchy", (DL_FUNC) &rxcauchy);
  R_RegisterCCallable("rxode2", "rxchisq", (DL_FUNC) &rxchisq);
  R_RegisterCCallable("rxode2", "rxexp", (DL_FUNC) &rxexp);
  R_RegisterCCallable("rxode2", "rxf", (DL_FUNC) &rxf);
  R_RegisterCCallable("rxode2", "rxgamma", (DL_FUNC) &rxgamma);
  R_RegisterCCallable("rxode2", "rxgeom", (DL_FUNC) &rxgeom);
  R_RegisterCCallable("rxode2", "rxnbinom", (DL_FUNC) &rxnbinom);
  R_RegisterCCallable("rxode2", "rxnbinomMu", (DL_FUNC) &rxnbinomMu);
  R_RegisterCCallable("rxode2", "rxnorm", (DL_FUNC) &rxnorm);
  R_RegisterCCallable("rxode2", "rxpois", (DL_FUNC) &rxpois);
  R_RegisterCCallable("rxode2", "rxt_", (DL_FUNC) &rxt_);
  R_RegisterCCallable("rxode2", "rxunif", (DL_FUNC) &rxunif);
  R_RegisterCCallable("rxode2", "rxweibull", (DL_FUNC) &rxweibull);
  R_RegisterCCallable("rxode2", "simeps", (DL_FUNC) &simeps);
  R_RegisterCCallable("rxode2", "simeta", (DL_FUNC) &simeta);
  // log likelihoods used in calculations
  static const R_CMethodDef cMethods[] = {
    {"rxode2_sum",               (DL_FUNC) &rxode2_sum, 2, rxode2_Sum_t},
    {"rxode2_prod",              (DL_FUNC) &rxode2_prod, 2, rxode2_Sum_t},
    {NULL, NULL, 0, NULL}
  };

  R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  rxOptionsIni();
  initRxThreads();
  avoid_openmp_hang_within_fork();
  nullGlobals();
  _rxode2_RcppExport_registerCCallable();
  /* rxOptionsIniFocei(); */
}
void parseFree(int last);
void rxOptionsFree(void);
void gFree(void);
void rxFreeLast(void);
void R_unload_rxode2(DllInfo *info){
  gFree();
  rxOptionsFree();
  rxOptionsIni();
  parseFree(1);
  rxFreeLast();
}
