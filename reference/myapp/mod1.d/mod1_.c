#define _getRxSolve_ _rxmod1_b6569420435f0f78681146effa346691144
#define simeps _rxmod1_b6569420435f0f78681146effa346691145
#define simeta _rxmod1_b6569420435f0f78681146effa346691146
#define _solveData _rxmod1_b6569420435f0f78681146effa346691147
#define _assign_ptr _rxmod1_b6569420435f0f78681146effa346691148
#define _rxRmModelLib _rxmod1_b6569420435f0f78681146effa346691149
#define _rxGetModelLib _rxmod1_b6569420435f0f78681146effa346691150
#define _old_c _rxmod1_b6569420435f0f78681146effa346691151
#define _ptrid _rxmod1_b6569420435f0f78681146effa346691152
#define _rxIsCurrentC _rxmod1_b6569420435f0f78681146effa346691153
#define _sumPS _rxmod1_b6569420435f0f78681146effa346691154
#define _prodPS _rxmod1_b6569420435f0f78681146effa346691155
#define _prodType _rxmod1_b6569420435f0f78681146effa346691156
#define _sumType _rxmod1_b6569420435f0f78681146effa346691157
#define _update_par_ptr _rxmod1_b6569420435f0f78681146effa346691158
#define _getParCov _rxmod1_b6569420435f0f78681146effa346691159
#define linCmtA _rxmod1_b6569420435f0f78681146effa346691160
#define linCmtC _rxmod1_b6569420435f0f78681146effa346691161
#define linCmtB _rxmod1_b6569420435f0f78681146effa346691162
#define _rxode2_rxAssignPtr _rxmod1_b6569420435f0f78681146effa346691163
#define _rxQr _rxmod1_b6569420435f0f78681146effa346691164
#define phi _rxmod1_b6569420435f0f78681146effa346691165
#define logit _rxmod1_b6569420435f0f78681146effa346691166
#define expit _rxmod1_b6569420435f0f78681146effa346691167
#define gammap _rxmod1_b6569420435f0f78681146effa346691168
#define gammaq _rxmod1_b6569420435f0f78681146effa346691169
#define lowergamma _rxmod1_b6569420435f0f78681146effa346691170
#define uppergamma _rxmod1_b6569420435f0f78681146effa346691171
#define gammapInv _rxmod1_b6569420435f0f78681146effa346691172
#define gammapDer _rxmod1_b6569420435f0f78681146effa346691173
#define gammapInva _rxmod1_b6569420435f0f78681146effa346691174
#define gammaqInv _rxmod1_b6569420435f0f78681146effa346691175
#define gammaqInva _rxmod1_b6569420435f0f78681146effa346691176
#define rxnorm _rxmod1_b6569420435f0f78681146effa346691177
#define rxnormV _rxmod1_b6569420435f0f78681146effa346691178
#define rxbinom _rxmod1_b6569420435f0f78681146effa346691179
#define rxcauchy _rxmod1_b6569420435f0f78681146effa346691180
#define rxchisq _rxmod1_b6569420435f0f78681146effa346691181
#define rxexp _rxmod1_b6569420435f0f78681146effa346691182
#define rxf _rxmod1_b6569420435f0f78681146effa346691183
#define rxgeom _rxmod1_b6569420435f0f78681146effa346691184
#define rxgamma _rxmod1_b6569420435f0f78681146effa346691185
#define rxbeta _rxmod1_b6569420435f0f78681146effa346691186
#define rxpois _rxmod1_b6569420435f0f78681146effa346691187
#define rxt_ _rxmod1_b6569420435f0f78681146effa346691188
#define rxunif _rxmod1_b6569420435f0f78681146effa346691189
#define rxweibull _rxmod1_b6569420435f0f78681146effa346691190
#define rinorm _rxmod1_b6569420435f0f78681146effa346691191
#define rinormV _rxmod1_b6569420435f0f78681146effa346691192
#define ribinom _rxmod1_b6569420435f0f78681146effa346691193
#define ricauchy _rxmod1_b6569420435f0f78681146effa346691194
#define richisq _rxmod1_b6569420435f0f78681146effa346691195
#define riexp _rxmod1_b6569420435f0f78681146effa346691196
#define rif _rxmod1_b6569420435f0f78681146effa346691197
#define rigeom _rxmod1_b6569420435f0f78681146effa346691198
#define rigamma _rxmod1_b6569420435f0f78681146effa346691199
#define ribeta _rxmod1_b6569420435f0f78681146effa346691200
#define ripois _rxmod1_b6569420435f0f78681146effa346691201
#define rit_ _rxmod1_b6569420435f0f78681146effa346691202
#define riunif _rxmod1_b6569420435f0f78681146effa346691203
#define riweibull _rxmod1_b6569420435f0f78681146effa346691204
#define _compareFactorVal _rxmod1_b6569420435f0f78681146effa346691205
#define _sum _rxmod1_b6569420435f0f78681146effa346691206
#define _sign _rxmod1_b6569420435f0f78681146effa346691207
#define _prod _rxmod1_b6569420435f0f78681146effa346691208
#define _max _rxmod1_b6569420435f0f78681146effa346691209
#define _min _rxmod1_b6569420435f0f78681146effa346691210
#define _transit4P _rxmod1_b6569420435f0f78681146effa346691211
#define _transit3P _rxmod1_b6569420435f0f78681146effa346691212
#define _assignFuns0 _rxmod1_b6569420435f0f78681146effa346691213
#define _assignFuns _rxmod1_b6569420435f0f78681146effa346691214
#define _rxord _rxmod1_b6569420435f0f78681146effa346691215
#include <rxode2_model_shared.h>
#define __MAX_PROD__ 0
#define _CMT CMT
#define _SYNC_simeps_ for (int _svari=_solveData->neps; _svari--;){  if (_solveData->svar[_svari] == 0) {V2 = _PP[0];};   if (_solveData->svar[_svari] == 1) {V3 = _PP[1];};   if (_solveData->svar[_svari] == 2) {KA = _PP[2];};   if (_solveData->svar[_svari] == 3) {CL = _PP[3];};   if (_solveData->svar[_svari] == 4) {Q = _PP[4];};   if (_solveData->svar[_svari] == 5) {Kin = _PP[5];};   if (_solveData->svar[_svari] == 6) {Kout = _PP[6];};   if (_solveData->svar[_svari] == 7) {EC50 = _PP[7];}; }
#define _SYNC_simeta_ for (int _ovari=_solveData->neta; _ovari--;){  if (_solveData->ovar[_ovari] == 0) {V2 = _PP[0];};   if (_solveData->ovar[_ovari] == 1) {V3 = _PP[1];};   if (_solveData->ovar[_ovari] == 2) {KA = _PP[2];};   if (_solveData->ovar[_ovari] == 3) {CL = _PP[3];};   if (_solveData->ovar[_ovari] == 4) {Q = _PP[4];};   if (_solveData->ovar[_ovari] == 5) {Kin = _PP[5];};   if (_solveData->ovar[_ovari] == 6) {Kout = _PP[6];};   if (_solveData->ovar[_ovari] == 7) {EC50 = _PP[7];}; }
#include "extraC.h"
_getRxSolve_t _getRxSolve_;
_simfun simeps;
_simfun simeta;
rx_solve *_solveData=NULL;
rxode2_assign_ptr _assign_ptr=NULL;
_rxRmModelLibType _rxRmModelLib=NULL;
_rxGetModelLibType _rxGetModelLib=NULL;
rxode2_ode_solver_old_c _old_c=NULL;
rxode2_fn0i _ptrid=NULL;
_rxIsCurrentC_type _rxIsCurrentC=NULL;
_rxSumType _sumPS=NULL;
_rxProdType _prodPS=NULL;
rxode2_fn0i _prodType=NULL;
rxode2_fn0i _sumType=NULL;
_update_par_ptr_p _update_par_ptr=NULL;
_getParCov_p _getParCov=NULL;
linCmtA_p linCmtA;
linCmtA_p linCmtC;
linCmtB_p linCmtB;
_rx_asgn _rxode2_rxAssignPtr=NULL;
_rx_asgn _rxQr=NULL;
rxode2_fn phi;
rxode2_fn3 logit;
rxode2_fn3 expit;
rxode2_fn2 gammap;
rxode2_fn2 gammaq;
rxode2_fn2 lowergamma;
rxode2_fn2 uppergamma;
rxode2_fn2 gammapInv;
rxode2_fn2 gammapDer;
rxode2_fn2 gammapInva;
rxode2_fn2 gammaqInv;
rxode2_fn2 gammaqInva;
rxode2i_fn2 rxnorm;
rxode2i_fn2 rxnormV;
rxode2i_rxbinom rxbinom;
rxode2i_fn2 rxcauchy;
rxode2i_fn rxchisq;
rxode2i_fn rxexp;
rxode2i_fn2 rxf;
rxode2i_ifn rxgeom;
rxode2i_fn2 rxgamma;
rxode2i_fn2 rxbeta;
rxode2i_ifn rxpois;
rxode2i_fn rxt_;
rxode2i_fn2 rxunif;
rxode2i_fn2 rxweibull;
rxode2i2_fn2 rinorm;
rxode2i2_fn2 rinormV;
rxode2i2_ribinom ribinom;
rxode2i2_fn2 ricauchy;
rxode2i2_fn richisq;
rxode2i2_fn riexp;
rxode2i2_fn2 rif;
rxode2i2_ifn rigeom;
rxode2i2_fn2 rigamma;
rxode2i2_fn2 ribeta;
rxode2i2_ifn ripois;
rxode2i2_fn rit_;
rxode2i2_fn2 riunif;
rxode2i2_fn2 riweibull;
rxode2_compareFactorVal_fn _compareFactorVal;
double _prod(double *input, double *p, int type, int n, ...){
  va_list valist;
  va_start(valist, n);
  for (unsigned int i = 0; i < n; i++){
    input[i] = va_arg(valist, double);
  }
  va_end(valist);
  return _prodPS(input, p, n, type);
}
double _sum(double *input, double *pld, int m, int type, int n, ...){
  va_list valist;
  va_start(valist, n);
  for (unsigned int i = 0; i < n; i++){
    input[i] = va_arg(valist, double);
  }
  va_end(valist);
  double ret = _sumPS(input, n, pld, m, type);
  if (type == 2 && m < 0){
    for (int i = -m; i--;){
      pld[i] = 0.0;
    }
  }
  return ret;
}
double _sign(unsigned int n, ...) {
  va_list valist;
  va_start(valist, n);
  double s = 1;
  for (unsigned int i = 0; i < n; i++) {
    s = sign(va_arg(valist, double))*s;
    if (s == 0){
      break;
    }
  }
  va_end(valist);
  return s;
}
double _rxord(int _cSub, unsigned int n,  ...) {
  va_list valist;
  va_start(valist, n);
  double ret = 0.0;
  double p = 0.0;
  double u = rxunif(&_solveData->subjects[_cSub], 0.0, 1.0);
  for (unsigned int i = 0; i < n; i++) {
    p += va_arg(valist, double);
    if (ret < 1e-6) {
      if (u < p) {
        ret = (double)(i+1);
      }
    }
  }
  if (p >= 1) ret = NA_REAL;
  else if (ret < 1e-6) ret = (double)(n+1);
  va_end(valist);
  return ret;
}
double _max(unsigned int n, ...) {
  va_list valist;
  va_start(valist, n);
  double mx = NA_REAL;
  double tmp = 0;
  if (n >= 1){
    mx = va_arg(valist, double);
    for (unsigned int i = 1; i < n; i++) {
      tmp = va_arg(valist, double);
      if (tmp>mx) mx=tmp;
    }
    va_end(valist);
  }
  return mx;
}
double _min(unsigned int n, ...){
  va_list valist;
  va_start(valist, n);
  double mn = NA_REAL;
  double tmp = 0;
  if (n >= 1){
    mn = va_arg(valist, double);
    for (unsigned int i = 1; i < n; i++){
      tmp = va_arg(valist, double);
      if (tmp<mn) mn=tmp;
    }
    va_end(valist);
  }
  return mn;
}
double _transit4P(int cmt, double t, unsigned int id, double n, double mtt, double bio){
  double nd = (double) n;
  double ktr = (nd+1)/mtt;
  double lktr = _safe_log(nd+1)-_safe_log(mtt);
  double tlast = _solveData->subjects[id].tlastS[cmt];
  if (ISNA(tlast)) tlast = 0.0;
  double tad = (t-tlast);
  return exp(_safe_log(bio*(_solveData->subjects[id].curDoseS[cmt]))+lktr+n*(lktr+_safe_log(tad))-ktr*(tad)-lgamma1p(nd));
}
double _transit3P(int cmt, double t, unsigned int id, double n, double mtt){
  double nd = (double) n;
  double ktr = (nd+1)/mtt;
  double lktr = _safe_log(nd+1)-_safe_log(mtt);
  double tlast = _solveData->subjects[id].tlastS[cmt];
  if (ISNA(tlast)) tlast = 0.0;
  double tad = t-tlast;
  double podo = _solveData->subjects[id].curDoseS[cmt];
  return exp(_safe_log(podo)+lktr+n*(lktr+_safe_log(tad))-ktr*(tad)-lgamma1p(nd));
}
void _assignFuns0() {
  _getRxSolve_ = (_getRxSolve_t) R_GetCCallable("rxode2","getRxSolve_");
  _assign_ptr=(rxode2_assign_ptr) R_GetCCallable("rxode2","rxode2_assign_fn_pointers");
  _rxRmModelLib=(_rxRmModelLibType) R_GetCCallable("rxode2","rxRmModelLib");
  _rxGetModelLib=(_rxGetModelLibType) R_GetCCallable("rxode2","rxGetModelLib");
  _rxode2_rxAssignPtr=(_rx_asgn)R_GetCCallable("rxode2","_rxode2_rxAssignPtr");
  _rxQr=(_rx_asgn)R_GetCCallable("rxode2","_rxode2_rxQr");
  _rxIsCurrentC = (_rxIsCurrentC_type)R_GetCCallable("rxode2","rxIsCurrentC");
  _sumPS  = (_rxSumType) R_GetCCallable("PreciseSums","PreciseSums_sum_r");
  _prodPS = (_rxProdType) R_GetCCallable("PreciseSums","PreciseSums_prod_r");
  _prodType=(rxode2_fn0i)R_GetCCallable("PreciseSums", "PreciseSums_prod_get");
  _sumType=(rxode2_fn0i)R_GetCCallable("PreciseSums", "PreciseSums_sum_get");
  _ptrid=(rxode2_fn0i)R_GetCCallable("rxode2", "rxode2_current_fn_pointer_id");
  linCmtA=(linCmtA_p)R_GetCCallable("rxode2", "linCmtA");
  linCmtB=(linCmtB_p)R_GetCCallable("rxode2", "linCmtB");
  linCmtC=(linCmtA_p)R_GetCCallable("rxode2", "linCmtC");
    
  rxnorm = (rxode2i_fn2)R_GetCCallable("rxode2", "rxnorm");
  rxnormV = (rxode2i_fn2)R_GetCCallable("rxode2", "rxnormV");
  rxbinom = (rxode2i_rxbinom)R_GetCCallable("rxode2","rxbinom") ;
  rxcauchy = (rxode2i_fn2)R_GetCCallable("rxode2","rxcauchy") ;
  rxchisq = (rxode2i_fn)R_GetCCallable("rxode2","rxchisq") ;
  rxexp = (rxode2i_fn)R_GetCCallable("rxode2","rxexp");
  rxf = (rxode2i_fn2)R_GetCCallable("rxode2","rxf") ;
  rxgeom = (rxode2i_ifn)R_GetCCallable("rxode2","rxgeom") ;
  rxgamma = (rxode2i_fn2)R_GetCCallable("rxode2","rxgamma") ;
  rxbeta = (rxode2i_fn2)R_GetCCallable("rxode2","rxbeta") ;
  rxpois = (rxode2i_ifn)R_GetCCallable("rxode2","rxpois") ;
  rxt_ = (rxode2i_fn)R_GetCCallable("rxode2","rxt_") ;
  rxunif = (rxode2i_fn2)R_GetCCallable("rxode2","rxunif") ;
  rxweibull = (rxode2i_fn2)R_GetCCallable("rxode2","rxweibull");
  rinorm = (rxode2i2_fn2)R_GetCCallable("rxode2", "rinorm");
  rinormV = (rxode2i2_fn2)R_GetCCallable("rxode2", "rinormV");
  ribinom = (rxode2i2_ribinom)R_GetCCallable("rxode2","ribinom") ;
  ricauchy = (rxode2i2_fn2)R_GetCCallable("rxode2","ricauchy") ;
  richisq = (rxode2i2_fn)R_GetCCallable("rxode2","richisq") ;
  riexp = (rxode2i2_fn)R_GetCCallable("rxode2","riexp");
  rif = (rxode2i2_fn2)R_GetCCallable("rxode2","rif") ;
  rigeom = (rxode2i2_ifn)R_GetCCallable("rxode2","rigeom") ;
  rigamma = (rxode2i2_fn2)R_GetCCallable("rxode2","rigamma") ;
  ribeta = (rxode2i2_fn2)R_GetCCallable("rxode2","ribeta") ;
  ripois = (rxode2i2_ifn)R_GetCCallable("rxode2","ripois") ;
  rit_ = (rxode2i2_fn)R_GetCCallable("rxode2","rit_") ;
  riunif = (rxode2i2_fn2)R_GetCCallable("rxode2","riunif") ;
  riweibull = (rxode2i2_fn2)R_GetCCallable("rxode2","riweibull");
    
  phi = (rxode2_fn)R_GetCCallable("rxode2","phi");
  gammap = (rxode2_fn2) R_GetCCallable("rxode2","gammap");
  gammaq = (rxode2_fn2) R_GetCCallable("rxode2","gammaq");
  gammapInv = (rxode2_fn2) R_GetCCallable("rxode2","gammapInv");
  gammapInva = (rxode2_fn2) R_GetCCallable("rxode2","gammapInva");
  gammaqInv = (rxode2_fn2) R_GetCCallable("rxode2","gammaqInv");
  gammaqInva = (rxode2_fn2) R_GetCCallable("rxode2","gammaqInva");
  uppergamma = (rxode2_fn2) R_GetCCallable("rxode2","uppergamma");
  lowergamma = (rxode2_fn2) R_GetCCallable("rxode2","lowergamma");
  gammapDer  = (rxode2_fn2) R_GetCCallable("rxode2","gammapDer");
  logit = (rxode2_fn3) R_GetCCallable("rxode2", "logit");
  expit = (rxode2_fn3) R_GetCCallable("rxode2", "expit");
  simeta =(_simfun) R_GetCCallable("rxode2", "simeta");
  simeps =(_simfun) R_GetCCallable("rxode2", "simeps");
  _compareFactorVal=(rxode2_compareFactorVal_fn) R_GetCCallable("rxode2", "compareFactorVal");
  _update_par_ptr = (_update_par_ptr_p) R_GetCCallable("rxode2","_update_par_ptr");
  _getParCov = (_getParCov_p) R_GetCCallable("rxode2","_getParCov");
  _solveData = _getRxSolve_();
}
void _assignFuns() {
  if (_assign_ptr == NULL){
    _assignFuns0();
  }
}
extern void  mod1__ode_solver_solvedata (rx_solve *solve){
  _solveData = solve;
}
extern rx_solve *mod1__ode_solver_get_solvedata(){
  return _solveData;
}
SEXP mod1__model_vars();


// prj-specific differential eqns
void mod1__dydt(int *_neq, double __t, double *__zzStateVar__, double *__DDtStateVar__)
{
  int _itwhile = 0;
  (void)_itwhile;
  int _cSub = _neq[1];
  double t = __t + _solveData->subjects[_neq[1]].curShift;
  (void)t;
    double C2;
  double centr;
  double V2;
  double C3;
  double peri;
  double V3;
  double depot;
  double KA;
  double CL;
  double Q;
  double eff;
  double Kin;
  double Kout;
  double EC50;

  (void)t;
  (void)C2;
  (void)centr;
  (void)V2;
  (void)C3;
  (void)peri;
  (void)V3;
  (void)depot;
  (void)KA;
  (void)CL;
  (void)Q;
  (void)eff;
  (void)Kin;
  (void)Kout;
  (void)EC50;

  C2 = _PL[0];
  C3 = _PL[1];

  _update_par_ptr(__t, _cSub, _solveData, _idx);
  V2 = _PP[0];
  V3 = _PP[1];
  KA = _PP[2];
  CL = _PP[3];
  Q = _PP[4];
  Kin = _PP[5];
  Kout = _PP[6];
  EC50 = _PP[7];

  depot = __zzStateVar__[0]*((double)(_ON[0]));
  centr = __zzStateVar__[1]*((double)(_ON[1]));
  peri = __zzStateVar__[2]*((double)(_ON[2]));
  eff = __zzStateVar__[3]*((double)(_ON[3]));

  C2=centr/safe_zero(V2);
  C3=peri/safe_zero(V3);
  __DDtStateVar__[0] = ((double)(_ON[0]))*(_IR[0] -KA*depot);
  __DDtStateVar__[1] = ((double)(_ON[1]))*(_IR[1] + KA*depot-CL*C2-Q*C2+Q*C3);
  __DDtStateVar__[2] = ((double)(_ON[2]))*(_IR[2] + Q*C2-Q*C3);
  __DDtStateVar__[3] = ((double)(_ON[3]))*(_IR[3] + Kin-Kout*(1-C2/safe_zero((EC50+C2)))*eff);
  (&_solveData->subjects[_cSub])->dadt_counter[0]++;
}

// Jacobian derived vars
void mod1__calc_jac(int *_neq, double __t, double *__zzStateVar__, double *__PDStateVar__, unsigned int __NROWPD__) {
  int _itwhile = 0;
  (void)_itwhile;
    int _cSub=_neq[1];
  double t = __t + _solveData->subjects[_neq[1]].curShift;
  (void)t;
    (&_solveData->subjects[_cSub])->jac_counter[0]++;
}
// Functional based initial conditions.
void mod1__inis(int _cSub, double *__zzStateVar__){
  int _itwhile = 0;
  (void)_itwhile;
  
}
// prj-specific derived vars
void mod1__calc_lhs(int _cSub, double __t, double *__zzStateVar__, double *_lhs) {
    int _itwhile = 0;
  (void)_itwhile;
  double t = __t + _solveData->subjects[_cSub].curShift;
  (void)t;
    double  __DDtStateVar_0__;
  double  __DDtStateVar_1__;
  double  __DDtStateVar_2__;
  double  __DDtStateVar_3__;
  double C2;
  double centr;
  double V2;
  double C3;
  double peri;
  double V3;
  double depot;
  double KA;
  double CL;
  double Q;
  double eff;
  double Kin;
  double Kout;
  double EC50;

  (void)t;
  (void)__DDtStateVar_0__;
  (void)__DDtStateVar_1__;
  (void)__DDtStateVar_2__;
  (void)__DDtStateVar_3__;
  (void)C2;
  (void)centr;
  (void)V2;
  (void)C3;
  (void)peri;
  (void)V3;
  (void)depot;
  (void)KA;
  (void)CL;
  (void)Q;
  (void)eff;
  (void)Kin;
  (void)Kout;
  (void)EC50;

  C2 = _PL[0];
  C3 = _PL[1];

  _update_par_ptr(__t, _cSub, _solveData, _idx);
  V2 = _PP[0];
  V3 = _PP[1];
  KA = _PP[2];
  CL = _PP[3];
  Q = _PP[4];
  Kin = _PP[5];
  Kout = _PP[6];
  EC50 = _PP[7];

  depot = __zzStateVar__[0]*((double)(_ON[0]));
  centr = __zzStateVar__[1]*((double)(_ON[1]));
  peri = __zzStateVar__[2]*((double)(_ON[2]));
  eff = __zzStateVar__[3]*((double)(_ON[3]));

  C2=centr/safe_zero(V2);
  C3=peri/safe_zero(V3);
  __DDtStateVar_0__ = ((double)(_ON[0]))*(_IR[0] -KA*depot);
  __DDtStateVar_1__ = ((double)(_ON[1]))*(_IR[1] + KA*depot-CL*C2-Q*C2+Q*C3);
  __DDtStateVar_2__ = ((double)(_ON[2]))*(_IR[2] + Q*C2-Q*C3);
  __DDtStateVar_3__ = ((double)(_ON[3]))*(_IR[3] + Kin-Kout*(1-C2/safe_zero((EC50+C2)))*eff);

  _lhs[0]=C2;
  _lhs[1]=C3;
}
// Functional based bioavailability
double mod1__F(int _cSub,  int _cmt, double _amt, double __t, double *__zzStateVar__){
 return _amt;
}
// Functional based absorption lag
double mod1__Lag(int _cSub,  int _cmt, double __t, double *__zzStateVar__){
 return __t;
}
// Modeled zero-order rate
double mod1__Rate(int _cSub,  int _cmt, double _amt, double __t, double *__zzStateVar__){
 return 0.0;
}
// Modeled zero-order duration
double mod1__Dur(int _cSub,  int _cmt, double _amt, double __t){
 return 0.0;
}
// Model Times
void mod1__mtime(int _cSub, double *_mtime){
}
// Matrix Exponential (0)
void mod1__ME(int _cSub, double _t, double __t, double *_mat, const double *__zzStateVar__){
  int _itwhile = 0;
  (void)_itwhile;
  double t = __t + _solveData->subjects[_cSub].curShift;
  (void)t;
  }
// Inductive linearization Matf
void mod1__IndF(int _cSub, double _t, double __t, double *_matf){
 int _itwhile = 0;
  (void)_itwhile;
  double t = __t + _solveData->subjects[_cSub].curShift;
  (void)t;
  }
extern SEXP mod1__model_vars(){
  int pro=0;
  SEXP _mv = PROTECT(_rxGetModelLib("mod1__model_vars"));pro++;
  if (!_rxIsCurrentC(_mv)){
    SEXP hash    = PROTECT(allocVector(STRSXP, 1));pro++;
#define __doBuf__  sprintf(buf, "un]\"BAAA@QRtHACAAAAAAA[9yAAAv7#aT)dXbvMA7(Eah\";*7t1)_UMyaYWBC9ebPaIYL+Bt[<oGfmIz>ZCzPm]{+7Vx7*A5ii96}X\"WpXT~CNLg^=H8v;$/~CFtiAnBYY6x$7rv@f{#Q96TkdX;_%%nuTxY,R2.m^)k_F4cn*Uek+oFt|VglCbW9b~i$;oWg>i,oT;sYY/qLQ/NXjrC)$08#d8*@~Vu<E+&7;VC{`:&>L,u|obL@UW`W>{gz34#k.r<yaU>P1nWNiYTQ34]9r\?xtZRMMui#+yALn2$jvPVZJKUR)cxjy5KU|3;sNQ6;nQZ+]{E~XC:>j4zRK5jJLEgJEig*4V5tFa*Mqp)K`Re<vOqm`UXepOt!F28hr02|vrv%%Y\?{.LgGox1|EXLI8Zq4.oQ,fH)M>`I5XJ\"y.);Zu#Igj]G:V>]\"(BGR_7De_|hK{73\?mZkgOb)2Nxq:*/x*UJ!yaC{.@s{$\?uE{vlq$=(33csf#QfO|U9.:b~_g{:3>_&xKh6K\"|86e0kFuK:kgGuLC3EEi|(AFo_0MyT\"$tkbqD;cnP+Vq)9grVo%%u>AmpCWZsc[:2EY0:c)+/.}v!G3~@o]u*}^%%Fev+niv5.@aj<`E\?#BuI[KO6/sa]4N:E#)ipz^#dWV>_r0Dllhy|Q5yViz~}.+n.wQXS(x##zQa`$8!)x\?qH}z>~z:k}[^.]E0D)9PseP)q*qdaYp$%%gLTlZTjS3$]/gXq:a6Wwl=S2bTXKLmlq&UTh1mM*4Bzd4x:>R7gjVU]j|,XvGk*fCFeGWO~XNr\"CE0/bLOX2o0;isuQfEn@i+(j#wuZ\?8Lu(dI9i~).rHg$<aJ%%]eZ]O#2\?wd2d<V_lQo9xn)&N!iBqI=[2<xV]w2]z[d_O~N7wo*ZTolqP3ZAT10hWXb\"xiH)#R{2SA~5=9[\?x#U>O`!96f]t1rZYB5LIm=_H}[;h_/HmeGMUOn>(*Md*ca0Je*_ZyKU|yyCH>ZsG[Z(L<2+Q\?h9GVDg4Y~#hsm$zM>YE~OCqzdOJ");
    char buf[1037];
    __doBuf__
#undef __doBuf__
    SET_STRING_ELT(hash, 0, mkChar(buf));
    SEXP lst      = PROTECT(_rxQr(hash));pro++;
    _assign_ptr(lst);
    UNPROTECT(pro);
    return lst;
  } else {
    UNPROTECT(pro);
    return _mv;
  }
}
extern void mod1__dydt_lsoda(int *neq, double *t, double *A, double *DADT)
{
  mod1__dydt(neq, *t, A, DADT);
}
extern int mod1__dydt_liblsoda(double __t, double *y, double *ydot, void *data)
{
  int *neq = (int*)(data);
  mod1__dydt(neq, __t, y, ydot);
  return(0);
}
extern void mod1__calc_jac_lsoda(int *neq, double *t, double *A,int *ml, int *mu, double *JAC, int *nrowpd){
  // Update all covariate parameters
  mod1__calc_jac(neq, *t, A, JAC, *nrowpd);
}

//Create function to call from R's main thread that assigns the required functions. Sometimes they don't get assigned.
extern void mod1__assignFuns(){
  _assignFuns();
}

//Initialize the dll to match rxode2's calls
void R_init0_mod1_(){
  // Get C callables on load; Otherwise it isn't thread safe
  R_RegisterCCallable("mod1_","mod1__assignFuns", (DL_FUNC) mod1__assignFuns);
  R_RegisterCCallable("mod1_","mod1__inis",(DL_FUNC) mod1__inis);
  R_RegisterCCallable("mod1_","mod1__dydt",(DL_FUNC) mod1__dydt);
  R_RegisterCCallable("mod1_","mod1__calc_lhs",(DL_FUNC) mod1__calc_lhs);
  R_RegisterCCallable("mod1_","mod1__calc_jac",(DL_FUNC) mod1__calc_jac);
  R_RegisterCCallable("mod1_","mod1__dydt_lsoda", (DL_FUNC) mod1__dydt_lsoda);
  R_RegisterCCallable("mod1_","mod1__calc_jac_lsoda", (DL_FUNC) mod1__calc_jac_lsoda);
  R_RegisterCCallable("mod1_","mod1__ode_solver_solvedata", (DL_FUNC) mod1__ode_solver_solvedata);
  R_RegisterCCallable("mod1_","mod1__ode_solver_get_solvedata", (DL_FUNC) mod1__ode_solver_get_solvedata);
  R_RegisterCCallable("mod1_","mod1__F", (DL_FUNC) mod1__F);
  R_RegisterCCallable("mod1_","mod1__Lag", (DL_FUNC) mod1__Lag);
  R_RegisterCCallable("mod1_","mod1__Rate", (DL_FUNC) mod1__Rate);
  R_RegisterCCallable("mod1_","mod1__Dur", (DL_FUNC) mod1__Dur);
  R_RegisterCCallable("mod1_","mod1__mtime", (DL_FUNC) mod1__mtime);
  R_RegisterCCallable("mod1_","mod1__ME", (DL_FUNC) mod1__ME);
  R_RegisterCCallable("mod1_","mod1__IndF", (DL_FUNC) mod1__IndF);
  R_RegisterCCallable("mod1_","mod1__dydt_liblsoda", (DL_FUNC) mod1__dydt_liblsoda);
}
//Initialize the dll to match rxode2's calls
void R_init_mod1_(DllInfo *info){
  // Get C callables on load; Otherwise it isn't thread safe
  R_init0_mod1_();
  static const R_CallMethodDef callMethods[]  = {
    {"mod1__model_vars", (DL_FUNC) &mod1__model_vars, 0},
    {NULL, NULL, 0}
  };

  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info,FALSE);
  _assignFuns0();

}

void R_unload_mod1_ (DllInfo *info){
  // Free resources required for single subject solve.
  SEXP _mv = PROTECT(_rxGetModelLib("mod1__model_vars"));
  if (!isNull(_mv)){
    _rxRmModelLib("mod1__model_vars");
  }
  UNPROTECT(1);
}