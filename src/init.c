#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include "../inst/include/rxode2.h"
#define __DOINIT__
#include "tran.h"
#include "rxthreefry.h"
#include "llik.h"
#include "cbindThetaOmega.h"
#include "seed.h"
#include "getTime.h"

SEXP _rxHasOpenMp();

SEXP _vecDF(SEXP cv, SEXP n_);
SEXP _rxode2_dropUnitsRxSolve(SEXP);
SEXP _rxode2_atolRtolFactor_(SEXP);
SEXP _rxode2_etRep_(SEXP, SEXP, SEXP, SEXP, SEXP,
		   SEXP, SEXP);
SEXP _rxode2_etSeq_(SEXP, SEXP, SEXP, SEXP, SEXP,
		   SEXP, SEXP, SEXP, SEXP, SEXP,
		   SEXP);
SEXP _rxode2_rxSolveSEXP(SEXP, SEXP, SEXP, SEXP, SEXP,
			SEXP, SEXP, SEXP);
SEXP _rxode2_rxStack(SEXP, SEXP);
SEXP _rxode2_etUpdate(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_et_(SEXP, SEXP);
SEXP _rxode2_etTrans(SEXP, SEXP, SEXP, SEXP, SEXP,
		    SEXP, SEXP, SEXP);
SEXP _rxode2_rxUpdateTrans_(SEXP, SEXP, SEXP);
double powerDi(double x, double lambda, int yj);
double powerD(double x, double lambda, int yj);
double powerDD(double x, double lambda, int yj);
double powerDDD(double x, double lambda, int yj);
double powerL(double x, double lambda, int yj);
double powerDL(double x, double lambda, int yj);

SEXP _rxProgress(SEXP num, SEXP core);
SEXP _rxTick();
SEXP _rxProgressStop(SEXP);
SEXP _rxProgressAbort(SEXP);
SEXP _rxode2_codeLoaded();

SEXP _rxode2_codegen(SEXP c_file, SEXP prefix, SEXP libname, SEXP pMd5, SEXP timeId, SEXP lastMv);
SEXP _rxode2_parseModel(SEXP type);
SEXP _rxode2_isLinCmt();
SEXP _rxode2_RcppExport_registerCCallable();
SEXP _rxode2_setRstudio(SEXP);
SEXP _rxode2_rxSolveFree();
SEXP _rxode2_linCmtEnv(SEXP rho);
SEXP _rxode2_rxInv(SEXP matrix);
SEXP _rxode2_removableDrive(SEXP letter);
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
SEXP _rxode2_forderForceBase(SEXP);
SEXP _rxode2_rxRmvn_(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxCholperm(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxGradpsi(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxNleq(SEXP, SEXP, SEXP);
SEXP _rxode2_rxMvnrnd(SEXP, SEXP, SEXP, SEXP, SEXP,
		     SEXP, SEXP);
SEXP _rxode2_rxMvrandn_(SEXP, SEXP, SEXP, SEXP, SEXP,
		       SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP _rxode2_rxSolveDollarNames(SEXP);
SEXP _rxode2_etDollarNames(SEXP);
SEXP _rxode2_rxExpandNesting(SEXP, SEXP, SEXP);
SEXP _rxode2_expandTheta_(SEXP, SEXP, SEXP, SEXP, SEXP,
			 SEXP);

SEXP _rxode2_rxRmvnSEXP(SEXP, SEXP, SEXP, SEXP, SEXP,
		       SEXP, SEXP, SEXP, SEXP, SEXP,
		       SEXP, SEXP);

SEXP _cbindOme(SEXP et_, SEXP mat_, SEXP n_);

SEXP _rxode2_nestingInfo_(SEXP omega, SEXP data);

SEXP _rxode2_isNullZero(SEXP in);

SEXP _rxode2_llikNormInternal(SEXP xSEXP, SEXP muSEXP, SEXP sigmaSEXP);
SEXP _rxode2_llikPoisInternal(SEXP xSEXP, SEXP lambdaSEXP);
SEXP _rxode2_llikBinomInternal(SEXP xSEXP, SEXP sizeSEXP, SEXP probSEXP);
SEXP _rxode2_llikBetaInternal(SEXP xSEXP, SEXP shape1SEXP, SEXP shape2SEXP);
SEXP _rxode2_llikTInternal(SEXP xSEXP, SEXP df1SEXP, SEXP meanSEXP, SEXP sdSEXP);
SEXP _rxode2_llikChisqInternal(SEXP xSEXP, SEXP dfSEXP);
SEXP _rxode2_llikExpInternal(SEXP xSEXP, SEXP rateSEXP);

SEXP rxode2_get_mv();
SEXP _rxode2_rxGetSeed();

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
SEXP _expit(SEXP, SEXP, SEXP);
SEXP _logit(SEXP, SEXP, SEXP);
SEXP _linCmtParse(SEXP vars, SEXP inStr, SEXP verbose);
SEXP _rxode2_linCmtGen(SEXP linCmt, SEXP vars, SEXP linCmtSens, SEXP verbose);

SEXP _rxode2_rxordSelect(SEXP, SEXP);

static R_NativePrimitiveArgType rxode2_Sum_t[] = {
  REALSXP, INTSXP
};

extern int rxode2_current_fn_pointer_id();
extern double rxode2_sum(double *input, int len);
extern double rxode2_prod(double *input, int len);

extern void rxode2_assign_fn_pointers(SEXP mv);


// Need to change to remove global variables
extern void rxode2_ode_free();

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

SEXP _rxode2_getRxFn(SEXP);
SEXP _rxode2_setProgSupported(SEXP);
SEXP _rxode2_getProgSupported();
SEXP _rxode2_rxSetIni0(SEXP);
SEXP _rxode2_rxSetSilentErr(SEXP silentSEXP);
SEXP _rxode2_rxUnloadAll_();
SEXP _rxode2_rxLock(SEXP);
SEXP _rxode2_rxUnlock(SEXP);
SEXP _rxode2_rxAllowUnload(SEXP);

SEXP _rxode2_rxExpandGrid_(SEXP, SEXP, SEXP);
SEXP _rxode2_rxExpandSens_(SEXP, SEXP);
SEXP _rxode2_rxExpandSens2_(SEXP, SEXP, SEXP);
SEXP _rxode2_rxExpandFEta_(SEXP, SEXP, SEXP);
SEXP _rxode2_rxRepR0_(SEXP);
SEXP _rxode2_rLKJ1(SEXP, SEXP, SEXP);
SEXP _rxode2_rLKJcv1(SEXP, SEXP);
SEXP _rxode2_rLKJcvLsd1(SEXP, SEXP, SEXP);
SEXP _rxode2_rcvC1(SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP _rxode2_rxSeedEng(SEXP);
SEXP _rxode2_rxnorm_(SEXP, SEXP, SEXP, SEXP);
SEXP _rxode2_rxnormV_(SEXP, SEXP, SEXP, SEXP);
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

SEXP _rxode2_rxQs(SEXP);

SEXP _rxode2_rxQr(SEXP);

SEXP _rxode2_rxEtTransAsDataFrame_(SEXP);

extern int rxIsCurrentC(SEXP obj);

rx_solve *getRxSolve_();

void avoid_openmp_hang_within_fork();
void initRxThreads();

// Remove these functions later...

void rxOptionsIni();
/* void rxOptionsIniFocei(); */

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

void _update_par_ptr(double t, unsigned int id, rx_solve *rx, int idx);
double _getParCov(unsigned int id, rx_solve *rx, int parNo, int idx);

int par_progress(int c, int n, int d, int cores, clock_t t0, int stop);
void ind_solve(rx_solve *rx, unsigned int cid, t_dydt_liblsoda dydt_lls, 
	       t_dydt_lsoda_dum dydt_lsoda, t_jdum_lsoda jdum,
	       t_dydt c_dydt, t_update_inis u_inis, int jt);
void par_solve(rx_solve *rx);
int isRstudio();

const char *rxGetId(int id);


double phi(double q);
SEXP _phi(SEXP q);
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

int getSilentErr();

int iniSubjectE(int solveid, int inLhs, rx_solving_options_ind *ind, rx_solving_options *op, rx_solve *rx,
		t_update_inis u_inis);

t_calc_lhs getRxLhs();
t_update_inis getUpdateInis();

void sortIds(rx_solve* rx, int ini);

void handleTlast(double *time, rx_solving_options_ind *ind);

SEXP _probit(SEXP xS, SEXP lowS, SEXP highS);
SEXP _probitInv(SEXP xS, SEXP lowS, SEXP highS);
SEXP _rxode2_rxrandnV(SEXP, SEXP);
SEXP _rxode2_rxErf(SEXP);

void simeps(int id);
void simeta(int id);

void transIniNull();
void nullGlobals();
void R_init_rxode2(DllInfo *info){
  R_CallMethodDef callMethods[]  = {
    {"_rxProgress", (DL_FUNC) &_rxProgress, 2},
    {"_rxTick", (DL_FUNC) &_rxTick, 0},
    {"_rxProgressStop", (DL_FUNC) &_rxProgressStop, 1},
    {"_rxProgressAbort", (DL_FUNC) &_rxProgressAbort, 1},
    {"_rxode2_trans", (DL_FUNC) &_rxode2_trans, 7},
    {"_rxode2_codegen", (DL_FUNC) &_rxode2_codegen, 6},
    {"_rxode2_codeLoaded", (DL_FUNC) &_rxode2_codeLoaded, 0},
    {"_rxode2_parseModel", (DL_FUNC) &_rxode2_parseModel, 1},
    {"_rxode2_isLinCmt", (DL_FUNC) &_rxode2_isLinCmt, 0},
    {"rxode2_get_mv", (DL_FUNC) &rxode2_get_mv, 0},
    {"_rxode2_rxGetSeed", (DL_FUNC) &_rxode2_rxGetSeed, 0},
    {"_rxode2_rxInv", (DL_FUNC) &_rxode2_rxInv, 1},
    {"_rxode2_removableDrive", (DL_FUNC) &_rxode2_removableDrive, 1},
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
    {"_rxode2_getRxFn", (DL_FUNC) &_rxode2_getRxFn, 1},
    {"_rxode2_setProgSupported", (DL_FUNC) &_rxode2_setProgSupported, 1},
    {"_rxode2_getProgSupported", (DL_FUNC) &_rxode2_getProgSupported, 0},
    {"_rxode2_rxUpdateTrans_", (DL_FUNC) &_rxode2_rxUpdateTrans_, 3},
    {"_rxode2_etTrans", (DL_FUNC) &_rxode2_etTrans, 8},
    {"_rxode2_et_", (DL_FUNC) &_rxode2_et_, 2},
    {"_rxode2_etUpdate", (DL_FUNC) &_rxode2_etUpdate, 4},
    {"_rxode2_rxStack", (DL_FUNC) &_rxode2_rxStack, 2},
    {"_rxode2_etSeq_", (DL_FUNC) &_rxode2_etSeq_, 11},
    {"_rxode2_etRep_", (DL_FUNC) &_rxode2_etRep_, 7},
    {"_rxode2_rxSolveSEXP", (DL_FUNC) &_rxode2_rxSolveSEXP, 8},
    {"_rxode2_dropUnitsRxSolve", (DL_FUNC) &_rxode2_dropUnitsRxSolve, 1},
    {"_rxode2_atolRtolFactor_", (DL_FUNC) &_rxode2_atolRtolFactor_, 1},
    {"_rxode2_rxExpandGrid_", (DL_FUNC) &_rxode2_rxExpandGrid_, 3},
    {"_rxode2_rxExpandSens_", (DL_FUNC) &_rxode2_rxExpandSens_, 2},
    {"_rxode2_rxExpandSens2_",(DL_FUNC) &_rxode2_rxExpandSens2_, 3},
    {"_rxode2_rxExpandFEta_", (DL_FUNC) &_rxode2_rxExpandFEta_, 3},
    {"_rxode2_rxRepR0_", (DL_FUNC) &_rxode2_rxRepR0_, 1},
    {"_rxode2_rxOptRep_", (DL_FUNC) &_rxode2_rxOptRep_, 1},
    {"_rxode2_rxSetIni0", (DL_FUNC) &_rxode2_rxSetIni0, 1},
    {"_rxode2_rxSetSilentErr", (DL_FUNC) &_rxode2_rxSetSilentErr, 1},
    {"_rxode2_rxIndLin_",(DL_FUNC) &_rxode2_rxIndLin_, 1},
    {"_rxode2_rxUnloadAll_", (DL_FUNC) &_rxode2_rxUnloadAll_, 0},
    {"_rxode2_rxLock", (DL_FUNC) &_rxode2_rxLock, 1},
    {"_rxode2_rxUnlock", (DL_FUNC) &_rxode2_rxUnlock, 1},
    {"_rxode2_rxAllowUnload", (DL_FUNC) &_rxode2_rxAllowUnload, 1},
    {"_rxParProgress", (DL_FUNC) &_rxParProgress, 1},
    {"_rxode2_forderForceBase", (DL_FUNC) &_rxode2_forderForceBase, 1},
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
    {"_rxode2_rxnormV_", (DL_FUNC) &_rxode2_rxnormV_, 4},
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
    {"_rxode2_rxSolveDollarNames", (DL_FUNC) _rxode2_rxSolveDollarNames, 1},
    {"_rxode2_etDollarNames", (DL_FUNC) _rxode2_etDollarNames, 1},
    {"_rxode2_rxExpandNesting", (DL_FUNC) _rxode2_rxExpandNesting, 3},
    {"_rxode2_rxRmvn0", (DL_FUNC) _rxode2_rxRmvn0, 11},
    {"_vecDF", (DL_FUNC) _vecDF, 2},
    {"_cbindOme", (DL_FUNC) _cbindOme, 3},
    {"_rxode2_rxRmvnSEXP", (DL_FUNC) _rxode2_rxRmvnSEXP, 12},
    {"_rxode2_expandPars_", (DL_FUNC) _rxode2_expandPars_, 4},
    {"_rxode2_convertId_", (DL_FUNC) _rxode2_convertId_, 1},
    {"_rxode2_nestingInfo_", (DL_FUNC) _rxode2_nestingInfo_, 2},
    {"_phi", (DL_FUNC) _phi, 1},
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
    {"_expit", (DL_FUNC) _expit, 3},
    {"_logit", (DL_FUNC) _logit, 3},
    {"_calcDerived", (DL_FUNC) _calcDerived, 4},
    {"_linCmtParse", (DL_FUNC) _linCmtParse, 3},
    {"_rxode2_linCmtGen", (DL_FUNC) _rxode2_linCmtGen, 4},
    {"_rxode2_rpp_", (DL_FUNC) _rxode2_rpp_, 7},
    {"_rxode2_rxSolve_", (DL_FUNC) _rxode2_rxSolve_, 8},
    {"getRxThreads_R", (DL_FUNC) getRxThreads_R, 1},
    {"setRxthreads", (DL_FUNC) setRxthreads, 3},
    {"_rxHasOpenMp", (DL_FUNC) _rxHasOpenMp, 0},
    {"_probit", (DL_FUNC) _probit, 3},
    {"_probitInv", (DL_FUNC) _probitInv, 3},
    {"_rxode2_rxrandnV", (DL_FUNC) _rxode2_rxrandnV, 2},
    {"_rxode2_rxQs", (DL_FUNC) _rxode2_rxQs, 1},
    {"_rxode2_rxQr", (DL_FUNC) _rxode2_rxQr, 1},
    {"_rxode2_rxEtTransAsDataFrame_", (DL_FUNC) _rxode2_rxEtTransAsDataFrame_, 1},
    {"_rxode2_isNullZero", (DL_FUNC) _rxode2_isNullZero, 1},
    {"_rxode2_invWR1d", (DL_FUNC) _rxode2_invWR1d, 3},
    {"_rxode2_rxSimThetaOmega", (DL_FUNC) _rxode2_rxSimThetaOmega, 28},
    {"_rxCbindStudyIndividual", (DL_FUNC) _rxCbindStudyIndividual, 2},
    {"_rxSetSeed", (DL_FUNC) _rxSetSeed, 1},
    {"_rxode2_rxordSelect", (DL_FUNC) _rxode2_rxordSelect, 2},
    {"_rxode2_rxErf", (DL_FUNC) &_rxode2_rxErf, 1},
    {"_rxode2_llikNormInternal", (DL_FUNC) &_rxode2_llikNormInternal, 3},
    {"_rxode2_llikPoisInternal", (DL_FUNC) &_rxode2_llikPoisInternal, 2},
    {"_rxode2_llikBinomInternal",(DL_FUNC) &_rxode2_llikBinomInternal, 3},
    {"_rxode2_llikBetaInternal", (DL_FUNC) &_rxode2_llikBetaInternal, 3},
    {"_rxode2_llikTInternal", (DL_FUNC) &_rxode2_llikTInternal, 4},
    {"_rxode2_llikChisqInternal", (DL_FUNC) &_rxode2_llikChisqInternal, 2},
    {"_rxode2_llikExpInternal", (DL_FUNC) &_rxode2_llikExpInternal, 2},
    {NULL, NULL, 0}
  };
  // C callable to assign environments.
  R_RegisterCCallable("rxode2","_rxode2_rxQr", (DL_FUNC) &_rxode2_rxQr);
  R_RegisterCCallable("rxode2", "simeps", (DL_FUNC) &simeps);
  R_RegisterCCallable("rxode2", "simeta", (DL_FUNC) &simeta);
  R_RegisterCCallable("rxode2", "getSilentErr", (DL_FUNC) &getSilentErr);
  R_RegisterCCallable("rxode2", "logit", (DL_FUNC) &logit);
  R_RegisterCCallable("rxode2", "expit", (DL_FUNC) &expit);
  R_RegisterCCallable("rxode2", "phi", (DL_FUNC) &phi);
  
  R_RegisterCCallable("rxode2", "rxnormV", (DL_FUNC) &rxnormV);
  R_RegisterCCallable("rxode2", "rinormV", (DL_FUNC) &rinormV);
  
  R_RegisterCCallable("rxode2", "rxgamma", (DL_FUNC) &rxgamma);
  R_RegisterCCallable("rxode2", "rxbeta", (DL_FUNC) &rxbeta);
  R_RegisterCCallable("rxode2", "rxbinom", (DL_FUNC) &rxbinom);
  R_RegisterCCallable("rxode2", "rxcauchy", (DL_FUNC) &rxcauchy);
  R_RegisterCCallable("rxode2", "rxchisq", (DL_FUNC) &rxchisq);
  R_RegisterCCallable("rxode2", "rxexp", (DL_FUNC) &rxexp);
  R_RegisterCCallable("rxode2", "rxf", (DL_FUNC) &rxf);
  R_RegisterCCallable("rxode2", "rxgeom", (DL_FUNC) &rxgeom);
  R_RegisterCCallable("rxode2", "rxnorm", (DL_FUNC) &rxnorm);
  R_RegisterCCallable("rxode2", "rxpois", (DL_FUNC) &rxpois);
  R_RegisterCCallable("rxode2", "rxt_", (DL_FUNC) &rxt_);
  R_RegisterCCallable("rxode2", "rxunif", (DL_FUNC) &rxunif);
  R_RegisterCCallable("rxode2", "rxweibull", (DL_FUNC) &rxweibull);

  R_RegisterCCallable("rxode2", "rigamma", (DL_FUNC) &rigamma);
  R_RegisterCCallable("rxode2", "ribeta", (DL_FUNC) &ribeta);
  R_RegisterCCallable("rxode2", "ribinom", (DL_FUNC) &ribinom);
  R_RegisterCCallable("rxode2", "ricauchy", (DL_FUNC) &ricauchy);
  R_RegisterCCallable("rxode2", "richisq", (DL_FUNC) &richisq);
  R_RegisterCCallable("rxode2", "riexp", (DL_FUNC) &riexp);
  R_RegisterCCallable("rxode2", "rif", (DL_FUNC) &rif);
  R_RegisterCCallable("rxode2", "rigeom", (DL_FUNC) &rigeom);
  R_RegisterCCallable("rxode2", "rinorm", (DL_FUNC) &rinorm);
  R_RegisterCCallable("rxode2", "ripois", (DL_FUNC) &ripois);
  R_RegisterCCallable("rxode2", "rit_", (DL_FUNC) &rit_);
  R_RegisterCCallable("rxode2", "riunif", (DL_FUNC) &riunif);
  R_RegisterCCallable("rxode2", "riweibull", (DL_FUNC) &riweibull);
  
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
  R_RegisterCCallable("rxode2", "linCmtA", (DL_FUNC) &linCmtA);
  R_RegisterCCallable("rxode2", "linCmtC", (DL_FUNC) &linCmtC);
  R_RegisterCCallable("rxode2", "linCmtB", (DL_FUNC) &linCmtB);
  R_RegisterCCallable("rxode2", "_update_par_ptr", (DL_FUNC) &_update_par_ptr);
  R_RegisterCCallable("rxode2", "_getParCov", (DL_FUNC) &_getParCov);
  R_RegisterCCallable("rxode2","rxRmModelLib", (DL_FUNC) &rxRmModelLib);
  R_RegisterCCallable("rxode2","rxGetModelLib", (DL_FUNC) &rxGetModelLib);
  
  R_RegisterCCallable("rxode2","rxode2_ode_free",           (DL_FUNC) &rxode2_ode_free);
  
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
  R_RegisterCCallable("rxode2", "iniSubjectE", (DL_FUNC) &iniSubjectE);
  R_RegisterCCallable("rxode2", "getRxLhs", (DL_FUNC) &getRxLhs);
  R_RegisterCCallable("rxode2", "getUpdateInis", (DL_FUNC) &getUpdateInis);
  R_RegisterCCallable("rxode2", "sortIds", (DL_FUNC) &sortIds);
  R_RegisterCCallable("rxode2", "handleTlast", (DL_FUNC) &handleTlast);
  R_RegisterCCallable("rxode2", "rxGetId", (DL_FUNC) &rxGetId);
  R_RegisterCCallable("rxode2", "getTime", (DL_FUNC) &getTime);

  // log likelihoods used in calculations
  R_RegisterCCallable("rxode2", "rxLlikNorm", (DL_FUNC) &rxLlikNorm);
  R_RegisterCCallable("rxode2", "rxLlikNormDmean", (DL_FUNC) &rxLlikNormDmean);
  R_RegisterCCallable("rxode2", "rxLlikNormDsd", (DL_FUNC) &rxLlikNormDsd);
  R_RegisterCCallable("rxode2", "rxLlikPois", (DL_FUNC) &rxLlikPois);
  R_RegisterCCallable("rxode2", "rxLlikPoisDlambda", (DL_FUNC) &rxLlikPoisDlambda);
  R_RegisterCCallable("rxode2", "rxLlikBinom", (DL_FUNC) &rxLlikBinom);
  R_RegisterCCallable("rxode2", "rxLlikBinomDprob", (DL_FUNC) &rxLlikBinomDprob);
  R_RegisterCCallable("rxode2", "rxLlikBeta", (DL_FUNC) &rxLlikBeta);
  R_RegisterCCallable("rxode2", "rxLlikBetaDshape1", (DL_FUNC) &rxLlikBetaDshape1);
  R_RegisterCCallable("rxode2", "rxLlikBetaDshape2", (DL_FUNC) &rxLlikBetaDshape2);
  R_RegisterCCallable("rxode2", "rxLlikT", (DL_FUNC) &rxLlikT);
  R_RegisterCCallable("rxode2", "rxLlikTDdf", (DL_FUNC) &rxLlikTDdf);
  R_RegisterCCallable("rxode2", "rxLlikTDmean", (DL_FUNC) &rxLlikTDmean);
  R_RegisterCCallable("rxode2", "rxLlikTDsd", (DL_FUNC) &rxLlikTDsd);
  R_RegisterCCallable("rxode2", "rxLlikChisq", (DL_FUNC) &rxLlikChisq);
  R_RegisterCCallable("rxode2", "rxLlikChisqDdf", (DL_FUNC) &rxLlikChisqDdf);

  R_RegisterCCallable("rxode2", "rxLlikExp", (DL_FUNC) &rxLlikExp);
  R_RegisterCCallable("rxode2", "rxLlikExpDrate", (DL_FUNC) &rxLlikExpDrate);

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
  transIniNull();
  nullGlobals();
  /* rxOptionsIniFocei(); */
}

void parseFree(int last);
void rxOptionsFree();
void gFree();
void rxFreeLast();
void R_unload_rxode2(DllInfo *info){
  gFree();
  rxOptionsFree();
  rxOptionsIni();
  parseFree(1);
  rxFreeLast();
}
