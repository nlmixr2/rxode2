#ifndef __rxode2_model_shared_H__
#define __rxode2_model_shared_H__
#include <rxode2.h>
#include <float.h>

#define _evid (&_solveData->subjects[_cSub])->evid[(&_solveData->subjects[_cSub])->ix[(&_solveData->subjects[_cSub])->idx]]
#define amt (isDose(_evid) ?  (&_solveData->subjects[_cSub])->dose[(&_solveData->subjects[_cSub])->ixds] : NA_REAL)
#define JAC_Rprintf Rprintf
#define _idx (&_solveData->subjects[_cSub])->idx
#define JAC0_Rprintf if ( (&_solveData->subjects[_cSub])->jac_counter == 0) Rprintf
#define ODE_Rprintf Rprintf
#define ODE0_Rprintf if ( (&_solveData->subjects[_cSub])->dadt_counter == 0) Rprintf
#define LHS_Rprintf Rprintf
#define _safe_log(a) (&_solveData->safeZero ? (((a) <= 0) ? log(DBL_EPSILON) : log(a)) : log(a))
#define safe_zero(a) (&_solveData->safeZero ? ((a) == 0 ? DBL_EPSILON : (a)) : (a))
#define _as_zero(a) (&_solveData->safeZero && fabs(a) < sqrt(DBL_EPSILON) ? 0.0 : a)
#define _as_dbleps(a) (&_solveData->safeZero && fabs(a) < sqrt(DBL_EPSILON) ? ((a) < 0 ? -sqrt(DBL_EPSILON)  : sqrt(DBL_EPSILON)) : a)
#define _as_dbleps2(a) (&_solveData->safeZero && fabs(a) < sqrt(DBL_EPSILON) ? sqrt(DBL_EPSILON) : a)
#define factorial(a) exp(lgamma1p(a))
#define sign_exp(sgn, x)(((sgn) > 0.0) ? exp(x) : (((sgn) < 0.0) ? -exp(x) : 0.0))
#define Rx_pow(a, b) R_pow(a, b)
#define Rx_pow_di(a, b) R_pow_di(a, b)
#define abs_log1p(x) (((x) + 1.0 > 0.0) ? log1p(x) : (((x) + 1.0 > 0.0) ? log1p(-x) : 0.0))
#define abs_log(x) ((&_solveData->safeZero && fabs(x) <= sqrt(DBL_EPSILON)) ? log(sqrt(DBL_EPSILON)) : (((x) > 0.0) ? log(x) ? (((x) == 0) ? 0.0 : log(-x))))
#define _IR (_solveData->subjects[_cSub].InfusionRate)
#define _ON (_solveData->subjects[_cSub].on)
#define _PP (_solveData->subjects[_cSub].par_ptr)
#define _PL (_solveData->subjects[_cSub].lhs)
#define _SR (INTEGER(stateRmS))
#define NEWIND (_solveData->subjects[_cSub]._newind)
#define newind (_solveData->subjects[_cSub]._newind)
#define rx_lambda_ _solveData->subjects[_cSub].lambda
#define rx_yj_ _solveData->subjects[_cSub].yj
#define rx_hi_ _solveData->subjects[_cSub].logitHi
#define rx_low_ _solveData->subjects[_cSub].logitLow
#define rxTBS(x, lm, yj, hi, low)  _powerD(x,  lm, (int)(yj), hi, low)
#define rxTBSi(x, lm, yj, hi, low) _powerDi(x,  lm, (int)(yj), hi, low)
#define rxTBSd(x, lm, yj, hi, low) _powerDD(x, lm, (int)(yj), hi, low)
#define rxTBSd2(x, lm, yj, hi, low) _powerDDD(x, lm, (int)(yj), hi, low)
#define normcdf(x) phi(x)
#define _getIndSim(id, val) (_solveData->subjects[_cSub].isIni == 1 ? \
			     (_solveData->subjects[_cSub].simIni[id] = (val)) : _solveData->subjects[_cSub].simIni[id])
#undef rbeta
#define rbeta(ind, x, y) rxbeta(ind, x, y)
#undef rnorm
#define rnorm(ind,x,y) rxnorm(ind, x,y)
#define rxnorm1(x) rxnorm(&_solveData->subjects[_cSub], x, 1.0)
#define rnorm1(x) rxnorm(&_solveData->subjects[_cSub],x, 1.0)
#define rxnormV1(x) rxnorm(&_solveData->subjects[_cSub], x, 1.0)
#define rinorm1(id, x) rinorm(&_solveData->subjects[_cSub], id, x, 1.0)
#define rinormV1(id, x) rinorm(&_solveData->subjects[_cSub], id, x, 1.0)

// FIXME: need to use same scheme here
#define rnormV(ind, x,y) rxnormV(ind,x,y)
#define rnormV1(ind, id, x) rxnormV(ind, id, x, 1.0)
  
#undef rcauchy 
#define rcauchy(ind, x, y) rxcauchy(ind,x,y)
#define rxcauchy1(x) rxcauchy(&_solveData->subjects[_cSub],x, 1.0)
#define ricauchy1(id, x) ricauchy(&_solveData->subjects[_cSub], id, x, 1.0)
#undef rchisq
#define rchisq(ind, x) rxchisq(ind, x)
#undef rexp
#define rexp(ind, x) rxexp(ind, x)
#undef rgamma
#define rgamma(ind, x,y) rxgamma(ind, x,y)
#define rgamma1(x) rxgamma(&_solveData->subjects[_cSub], x,1.0)
#define rxgamma1(x) rxgamma(&_solveData->subjects[_cSub], x,1.0)
#define rigamma1(id, x) rigamma(&_solveData->subjects[_cSub], id, x,1.0)
#undef rgeom
#define rgeom(ind,x) rxgeom(ind,x)
#undef rpois
#define rpois(ind,x) rxpois(ind,x)
#undef runif
#define runif(ind,x,y) rxunif(ind,x,y)
#define runif1(x) rxunif(&_solveData->subjects[_cSub],x,1.0)
#define rxunif1(x) rxunif(&_solveData->subjects[_cSub],x,1.0)
#define riunif1(id, x) riunif(&_solveData->subjects[_cSub],id, x,1.0)
#undef rweibull
#define rweibull(ind,x,y) rxweibull(ind,x,y)
#define rxweibull1(x) rxweibull(&_solveData->subjects[_cSub], x, 1.0)
#define riweibull1(id, x) riweibull(&_solveData->subjects[_cSub], id, x, 1.0)
#define rweibull1(x) rxweibull(&_solveData->subjects[_cSub], x, 1.0)

#define llikNorm(x, mu, sd) _llikNorm((&_solveData->subjects[_cSub])->llikSave, x, mu, sd)
#define llikNormDmean(x, mu, sd) _llikNormDmean((&_solveData->subjects[_cSub])->llikSave, x, mu, sd)
#define llikNormDsd(x, mu, sd) _llikNormDsd((&_solveData->subjects[_cSub])->llikSave, x, mu, sd)
#define llikPois(x, l) _llikPois((&_solveData->subjects[_cSub])->llikSave, x, l)
#define llikPoisDlambda(x, l) _llikPoisDlambda((&_solveData->subjects[_cSub])->llikSave, x, l)
#define llikBinom(x, size, prob) _llikBinom((&_solveData->subjects[_cSub])->llikSave, x, size, prob)
#define llikBinomDprob(x, size, prob) _llikBinomDprob((&_solveData->subjects[_cSub])->llikSave, x, size, prob)
#define llikBeta(x, shape1, shape2) _llikBeta((&_solveData->subjects[_cSub])->llikSave, x, shape1, shape2)
#define llikBetaDshape1(x, shape1, shape2) _llikBetaDshape1((&_solveData->subjects[_cSub])->llikSave, x, shape1, shape2)
#define llikBetaDshape2(x, shape1, shape2) _llikBetaDshape2((&_solveData->subjects[_cSub])->llikSave, x, shape1, shape2)
#define llikT(x, nu, mean, sd) _llikT((&_solveData->subjects[_cSub])->llikSave, x, nu, mean, sd)
#define llikTDdf(x, nu, mean, sd) _llikTDdf((&_solveData->subjects[_cSub])->llikSave, x, nu, mean, sd)
#define llikTDsd(x, nu, mean, sd) _llikTDsd((&_solveData->subjects[_cSub])->llikSave, x, nu, mean, sd)
#define llikTDmean(x, nu, mean, sd) _llikTDmean((&_solveData->subjects[_cSub])->llikSave, x, nu, mean, sd)
#define llikChisq(x, nu) _llikChisq((&_solveData->subjects[_cSub])->llikSave, x, nu)
#define llikChisqDdf(x, nu) _llikChisqDdf((&_solveData->subjects[_cSub])->llikSave, x, nu)
#define llikExp(x, rate) _llikExp((&_solveData->subjects[_cSub])->llikSave, x, rate)
#define llikExpDrate(x, rate) _llikExpDrate((&_solveData->subjects[_cSub])->llikSave, x, rate)
#define llikF(x, df1, df2) _llikF((&_solveData->subjects[_cSub])->llikSave, x, df1, df2)
#define llikFDdf1(x, df1, df2) _llikFDdf1((&_solveData->subjects[_cSub])->llikSave, x, df1, df2)
#define llikFDdf2(x, df1, df2) _llikFDdf2((&_solveData->subjects[_cSub])->llikSave, x, df1, df2)
#define llikGeom(x, p) _llikGeom((&_solveData->subjects[_cSub])->llikSave, x, p)
#define llikGeomDprob(x, p) _llikGeomDp((&_solveData->subjects[_cSub])->llikSave, x, p)
#define llikUnif(x, alpha, beta) _llikUnif((&_solveData->subjects[_cSub])->llikSave, x, alpha, beta)
#define llikUnifDalpha(x, alpha, beta) _llikUnifDalpha((&_solveData->subjects[_cSub])->llikSave, x, alpha, beta)
#define llikUnifDbeta(x, alpha, beta) _llikUnifDbeta((&_solveData->subjects[_cSub])->llikSave, x, alpha, beta)
#define llikWeibull(x, shape, scale) _llikWeibull((&_solveData->subjects[_cSub])->llikSave, x, shape, scale)
#define llikWeibullDshape(x, shape, scale) _llikWeibullDshape((&_solveData->subjects[_cSub])->llikSave, x, shape, scale)
#define llikWeibullDscale(x, shape, scale) _llikWeibullDscale((&_solveData->subjects[_cSub])->llikSave, x, shape, scale)
#define llikGamma(x, shape, rate) _llikGamma((&_solveData->subjects[_cSub])->llikSave, x, shape, rate)
#define llikGammaDshape(x, shape, rate) _llikGammaDshape((&_solveData->subjects[_cSub])->llikSave, x, shape, rate)
#define llikGammaDrate(x, shape, rate) _llikGammaDrate((&_solveData->subjects[_cSub])->llikSave, x, shape, rate)
#define _pnorm1(x) pnorm(x, 0.0, 1.0, 1, 0)
#define _pnorm2(x, mu) pnorm(x, mu, 1.0, 1, 0)
#define _pnorm3(x, mu, sd) pnorm(x, mu, sd, 1, 0)
#define _qnorm1(x) qnorm(x, 0.0, 1.0, 1, 0)
#define _qnorm2(x, mu) qnorm(x, mu, 1.0, 1, 0)
#define _qnorm3(x, mu, sd) qnorm(x, mu, sd, 1, 0)
#define norminv(x) qnorm(x, 0.0, 1.0, 1, 0)
#define probitt(x) qnorm(x, 0.0, 1.0, 1, 0)
#define _logit1(x) logit(x, 0.0, 1.0)
#define _logit2(x, y) logit(x, y, 1.0)
#define _expit1(x) expit(x, 0.0, 1.0)
#define _expit2(x, y) expit(x, y, 1.0)
#define _invLogit1(x) expit(x, 0.0, 1.0)
#define _invLogit2(x, y) expit(x, y, 1.0)
#define _logitInv1(x) expit(x, 0.0, 1.0)
#define _logitInv2(x, y) expit(x, y, 1.0)
#define _podo0() (_solveData->subjects[_cSub].curDose)
#define _podo1(x) (_solveData->subjects[_cSub].curDoseS[x])
#define _dose0() (_solveData->subjects[_cSub].curDose)
#define _dose1(x) (_solveData->subjects[_cSub].curDoseS[x])
#define _tad0() (t-_solveData->subjects[_cSub].tlast)
#define _tad1(x) (t-_solveData->subjects[_cSub].tlastS[x])
#define _tafd0()  (t-_solveData->subjects[_cSub].tfirst)
#define _tafd1(x) (t-_solveData->subjects[_cSub].tfirstS[x])
#define _tlast0() _solveData->subjects[_cSub].tlast
#define _tlast1(x) _solveData->subjects[_cSub].tlastS[x]
#define _tfirst0()  _solveData->subjects[_cSub].tfirst
#define _tfirst1(x) _solveData->subjects[_cSub].tfirstS[x]
#undef rf
#define rf(ind, x, y) rxf(ind, x, y)
// int compareFactorVal(int val, const char *valStr, const char *cmpValue)
// equality_str2 : identifier_r ('!=' | '==' ) string;
#define _cmp2(val, valStr, type, cmpStr) (type ? _compareFactorVal(val, valStr, cmpStr) : !_compareFactorVal(val, valStr, cmpStr))
// equality_str1 : string ('!=' | '==' ) identifier_r; //type=1 is equal, type=0 not equal
#define _cmp1(cmpStr, type, val, valStr) (type ? _compareFactorVal(val, valStr, cmpStr) : !_compareFactorVal(val, valStr, cmpStr))

// Types for par pointers.r
typedef int (*rxode2_compareFactorVal_fn)(int val, const char *factor, const char *value);
typedef double (*rxode2_fn) (double x);
typedef int (*rxode2_ifn) (double x);
typedef double (*rxode2_fn2) (double x, double y);
typedef double (*rxode2_fn3) (double x, double y, double z);
typedef double (*rxode2_fn3i) (double x, double y, int i);
typedef double (*rxode2_fn2i) (double x, int i);
typedef int (*rxode2_fn0i) ();
typedef double (*rxode2i_fn) (rx_solving_options_ind* ind, double x);
typedef int (*rxode2i_ifn) (rx_solving_options_ind* ind, double x);
typedef double (*rxode2i_fn2) (rx_solving_options_ind* ind, double x, double y);
typedef double (*rxode2i_fn3i) (rx_solving_options_ind* ind, double x, double y, int i);
typedef double (*rxode2i_fn2i) (rx_solving_options_ind* ind, double x, int i);

typedef int (*rxode2i2_fn0i) (rx_solving_options_ind* ind, int id);
typedef double (*rxode2i2_fn) (rx_solving_options_ind* ind, int id, double x);
typedef int (*rxode2i2_ifn) (rx_solving_options_ind* ind, int id, double x);
typedef double (*rxode2i2_fn2) (rx_solving_options_ind* ind, int id, double x, double y);
typedef double (*rxode2i2_fn3i) (rx_solving_options_ind* ind, int id, double x, double y, int i);
typedef double (*rxode2i2_fn2i) (rx_solving_options_ind* ind, int id, double x, int i);

typedef double (*rxode2_vec) (int val, rx_solve *rx, unsigned int id);
typedef double (*rxode2_val) (rx_solve *rx, unsigned int id);
typedef void (*rxode2_assign_ptr)(SEXP);
typedef void (*rxode2_ode_solver_old_c)(int *neq,double *theta,double *time,int *evid,int *ntime,double *inits,double *dose,double *ret,double *atol,double *rtol,int *stiff,int *transit_abs,int *nlhs,double *lhs,int *rc);

typedef void (*_rxRmModelLibType)(const char *inp);
typedef SEXP (*_rxGetModelLibType)(const char *s);
typedef  SEXP (*_rx_asgn) (SEXP objectSEXP);
typedef int(*_rxIsCurrentC_type)(SEXP);
typedef double(*_rxSumType)(double *, int, double *, int, int);

typedef void(*_simfun)(int id);

typedef double(*_rxProdType)(double*, double*, int, int);


typedef double (*linCmtA_p) (rx_solve *rx, unsigned int id, double t, int linCmt,
			     int ncmt, int trans, double d_ka,
			     double p1, double v1,
			     double p2, double p3,
			     double p4, double p5,
			     double d_tlag, double d_tlag2, double d_F, double d_F2,
			     double d_rate, double d_dur,
			     double d_rate2, double d_dur2);

typedef double (*linCmtB_p) (rx_solve *rx, unsigned int id, double t, int linCmt,
			     int i_cmt, int trans, int val,
			     double dd_p1, double dd_v1,
			     double dd_p2, double dd_p3,
			     double dd_p4, double dd_p5,
			     double dd_ka,
			     double dd_tlag, double dd_tlag2,
			     double dd_F, double dd_F2,
			     double dd_rate, double dd_dur,
			     double dd_rate2, double dd_dur2);


typedef void (*_update_par_ptr_p)(double t, unsigned int id, rx_solve *rx, int idx);

typedef double (*_getParCov_p)(unsigned int id, rx_solve *rx, int parNo, int idx);

typedef rx_solve *(*_getRxSolve_t)();

typedef int (*rxode2i_rxbinom) (rx_solving_options_ind* ind, int n, double prob);
typedef int (*rxode2i2_ribinom) (rx_solving_options_ind* ind, int id, int n, double prob);

typedef double (*rxode2_llikNormFun) (double *in, double x, double mean, double sd);
typedef double (*rxode2_llikPoisFun) (double *in, double x, double lambda);
typedef double (*rxode2_llikBinomFun) (double *in, double x, double size, double prob);
typedef double (*rxode2_llikBetaFun) (double *in, double x, double shape1, double shape2);
typedef double (*rxode2_llikTFun) (double *in, double x, double df, double mean, double sd);

typedef double (*rxode2_llikChisqFun) (double *in, double x, double df);
typedef double (*rxode2_llikExpFun) (double *in, double x, double rate);
typedef double (*rxode2_llikFFun) (double *in, double x, double df1, double df2);
typedef double (*rxode2_llikGeomFun) (double *in, double x, double p);
typedef double (*rxode2_llikUnifFun) (double *in, double x, double alpha, double beta);
typedef double (*rxode2_llikWeibullFun) (double *in, double x, double shape, double scale);
typedef double (*rxode2_llikGammaFun) (double *in, double x, double shape, double rate);

#endif // __rxode2_model_shared_H__
