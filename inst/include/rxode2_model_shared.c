_getRxSolve_t _getRxSolve_;
_simfun simeps;
_simfun simeta;

rx_solve *_solveData = NULL;
rxode2_assign_ptr _assign_ptr = NULL;
_rxRmModelLibType _rxRmModelLib = NULL;
_rxGetModelLibType _rxGetModelLib = NULL;
rxode2_ode_solver_old_c _old_c = NULL;
rxode2_fn0i _ptrid=NULL;
_rxIsCurrentC_type _rxIsCurrentC=NULL;
_rxSumType _sumPS = NULL;
_rxProdType _prodPS = NULL;

rxode2_fn0i _prodType = NULL;
rxode2_fn0i _sumType = NULL;

_update_par_ptr_p _update_par_ptr=NULL;
_getParCov_p _getParCov=NULL;
linCmtA_p linCmtA;
linCmtA_p linCmtC;
linCmtB_p linCmtB;
_rx_asgn _rxode2_rxAssignPtr =NULL;
_rx_asgn _rxQr =NULL;

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

rxode2_llikNormFun _llikNorm;
rxode2_llikNormFun _llikNormDmean;
rxode2_llikNormFun _llikNormDsd;

rxode2_llikPoisFun _llikPois;
rxode2_llikPoisFun _llikPoisDlambda;

rxode2_llikBinomFun _llikBinom;
rxode2_llikBinomFun _llikBinomDprob;


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
  
  _llikNorm=(rxode2_llikNormFun) R_GetCCallable("rxode2","rxLlikNorm");
  _llikNormDmean=(rxode2_llikNormFun) R_GetCCallable("rxode2","rxLlikNormDmean");
  _llikNormDsd=(rxode2_llikNormFun) R_GetCCallable("rxode2","rxLlikNormDsd");
  
  _llikPois        = (rxode2_llikPoisFun) R_GetCCallable("rxode2","rxLlikPois");
  _llikPoisDlambda = (rxode2_llikPoisFun) R_GetCCallable("rxode2","rxLlikPoisDlambda");

  _llikBinom = (rxode2_llikBinomFun) R_GetCCallable("rxode2", "rxLlikBinom");
  _llikBinomDprob = (rxode2_llikBinomFun) R_GetCCallable("rxode2", "rxLlikBinomDprob");
  
  _solveData = _getRxSolve_();
}

void _assignFuns() {
  if (_assign_ptr == NULL){
    _assignFuns0();
  }
}
