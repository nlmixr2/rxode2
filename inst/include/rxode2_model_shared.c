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
rxode2i_rxbinom rxnbinom;
rxode2i_rxbinom rxnbinomMu;
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
rxode2i2_ribinom rinbinom;
rxode2i2_ribinom rinbinomMu;
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

rxode2_llikBinomFun _llikNbinom;
rxode2_llikBinomFun _llikNbinomDprob;

rxode2_llikBinomFun _llikNbinomMu;
rxode2_llikBinomFun _llikNbinomMuDmu;

rxode2_llikBetaFun _llikBeta;
rxode2_llikBetaFun _llikBetaDshape1;
rxode2_llikBetaFun _llikBetaDshape2;

rxode2_llikTFun _llikT;
rxode2_llikTFun _llikTDdf;
rxode2_llikTFun _llikTDmean;
rxode2_llikTFun _llikTDsd;

rxode2_llikChisqFun _llikChisq;
rxode2_llikChisqFun _llikChisqDdf;

rxode2_llikExpFun _llikExp;
rxode2_llikExpFun _llikExpDrate;

rxode2_llikFFun _llikF;
rxode2_llikFFun _llikFDdf1;
rxode2_llikFFun _llikFDdf2;

rxode2_llikGeomFun _llikGeom;
rxode2_llikGeomFun _llikGeomDp;

rxode2_llikUnifFun _llikUnif;
rxode2_llikUnifFun _llikUnifDalpha;
rxode2_llikUnifFun _llikUnifDbeta;

rxode2_llikWeibullFun _llikWeibull;
rxode2_llikWeibullFun _llikWeibullDshape;
rxode2_llikWeibullFun _llikWeibullDscale;

rxode2_llikGammaFun _llikGamma;
rxode2_llikGammaFun _llikGammaDshape;
rxode2_llikGammaFun _llikGammaDrate;

rxode2_llikCauchyFun _llikCauchy;
rxode2_llikCauchyFun _llikCauchyDlocation;
rxode2_llikCauchyFun _llikCauchyDscale;


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
  double ret = 1.0;
  double p = 0.0;
  double u = rxunif(&_solveData->subjects[_cSub], 0.0, 1.0);
  int found = 0;
  for (unsigned int i = 0; i < n; i++) {
    p += va_arg(valist, double);
    if (!found) {
      if (u < p) {
        ret = (double)(i+1);
        found = 1;
      }
    }
  }
  if (!found) ret =(double)(n+1);
  va_end(valist);
  return ret;
}

double _rxord2(int _cSub, unsigned int n0,  ...) {
  va_list valist;
  va_start(valist, n0);
  int n = (n0-1)/2;
  double ret = 1.0;
  double p = 0.0;
  double u = rxunif(&_solveData->subjects[_cSub], 0.0, 1.0);
  int found = 0;
  for (unsigned int i = 0; i < n; i++) {
    p += va_arg(valist, double);
    if (!found) {
      if (u < p) {
        ret = (double)(i+1);
        found = 1;
      }
    }
  }
  if (!found) ret =(double)(n+1);
  double retF, cur;
  for (unsigned int i = 0; i < n; i++) {
    cur = va_arg(valist, double);
    if ((double)(i+1) == ret) {
      retF = cur;
    }
  }
  cur = va_arg(valist, double);
  if (!found) retF = cur;
  va_end(valist);
  return retF;
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
  rxbinom = (rxode2i_rxbinom)R_GetCCallable("rxode2","rxbinom");
  rxnbinom = (rxode2i_rxbinom)R_GetCCallable("rxode2","rxnbinom");
  rxnbinomMu = (rxode2i_rxbinom)R_GetCCallable("rxode2","rxnbinomMu");
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
  ribinom = (rxode2i2_ribinom)R_GetCCallable("rxode2","ribinom");
  rinbinom = (rxode2i2_ribinom)R_GetCCallable("rxode2","rinbinom");
  rinbinomMu = (rxode2i2_ribinom)R_GetCCallable("rxode2","rinbinomMu");
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

  _llikNbinom = (rxode2_llikBinomFun) R_GetCCallable("rxode2", "rxLlikNbinom");
  _llikNbinomDprob = (rxode2_llikBinomFun) R_GetCCallable("rxode2", "rxLlikNbinomDprob");
  
  _llikNbinomMu = (rxode2_llikBinomFun) R_GetCCallable("rxode2", "rxLlikNbinomMu");
  _llikNbinomMuDmu = (rxode2_llikBinomFun) R_GetCCallable("rxode2", "rxLlikNbinomMuDmu");

  _llikBeta = (rxode2_llikBetaFun)   R_GetCCallable("rxode2", "rxLlikBeta");
  _llikBetaDshape1 = (rxode2_llikBetaFun) R_GetCCallable("rxode2", "rxLlikBetaDshape1");
  _llikBetaDshape2 = (rxode2_llikBetaFun) R_GetCCallable("rxode2", "rxLlikBetaDshape2");

  _llikT = (rxode2_llikTFun)   R_GetCCallable("rxode2", "rxLlikT");
  _llikTDdf = (rxode2_llikTFun) R_GetCCallable("rxode2", "rxLlikTDdf");
  _llikTDmean = (rxode2_llikTFun) R_GetCCallable("rxode2", "rxLlikTDmean");
  _llikTDsd = (rxode2_llikTFun) R_GetCCallable("rxode2", "rxLlikTDsd");
  _llikChisq    = (rxode2_llikChisqFun) R_GetCCallable("rxode2", "rxLlikChisq");
  _llikChisqDdf = (rxode2_llikChisqFun) R_GetCCallable("rxode2", "rxLlikChisqDdf");

  _llikExp    = (rxode2_llikExpFun) R_GetCCallable("rxode2", "rxLlikExp");
  _llikExpDrate = (rxode2_llikExpFun) R_GetCCallable("rxode2", "rxLlikExpDrate");

  _llikF    = (rxode2_llikFFun) R_GetCCallable("rxode2", "rxLlikF");
  _llikFDdf1 = (rxode2_llikFFun) R_GetCCallable("rxode2", "rxLlikFDdf1");
  _llikFDdf2 = (rxode2_llikFFun) R_GetCCallable("rxode2", "rxLlikFDdf2");

  _llikGeom    = (rxode2_llikGeomFun) R_GetCCallable("rxode2", "rxLlikGeom");
  _llikGeomDp  = (rxode2_llikGeomFun) R_GetCCallable("rxode2", "rxLlikGeomDp");

  _llikUnif        = (rxode2_llikUnifFun) R_GetCCallable("rxode2", "rxLlikUnif");
  _llikUnifDalpha  = (rxode2_llikUnifFun) R_GetCCallable("rxode2", "rxLlikUnifDalpha");
  _llikUnifDbeta   = (rxode2_llikUnifFun) R_GetCCallable("rxode2", "rxLlikUnifDbeta");

  _llikWeibull        = (rxode2_llikWeibullFun) R_GetCCallable("rxode2", "rxLlikWeibull");
  _llikWeibullDshape  = (rxode2_llikWeibullFun) R_GetCCallable("rxode2", "rxLlikWeibullDshape");
  _llikWeibullDscale   = (rxode2_llikWeibullFun) R_GetCCallable("rxode2", "rxLlikWeibullDscale");

  _llikGamma        = (rxode2_llikGammaFun) R_GetCCallable("rxode2", "rxLlikGamma");
  _llikGammaDshape  = (rxode2_llikGammaFun) R_GetCCallable("rxode2", "rxLlikGammaDshape");
  _llikGammaDrate   = (rxode2_llikGammaFun) R_GetCCallable("rxode2", "rxLlikGammaDrate");

  _llikCauchy        = (rxode2_llikCauchyFun) R_GetCCallable("rxode2", "rxLlikCauchy");
  _llikCauchyDlocation  = (rxode2_llikCauchyFun) R_GetCCallable("rxode2", "rxLlikCauchyDlocation");
  _llikCauchyDscale   = (rxode2_llikCauchyFun) R_GetCCallable("rxode2", "rxLlikCauchyDscale");


  _solveData = _getRxSolve_();
}

void _assignFuns() {
  if (_assign_ptr == NULL){
    _assignFuns0();
  }
}
