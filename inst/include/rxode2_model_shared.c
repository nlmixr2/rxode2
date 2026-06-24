_getRxSolve_t _getRxSolve_;
_simfun simeps;
_simfun simeta;

_udf_type _evalUdf = NULL;
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
_setThreadInd_t _setThreadInd=NULL;
_rxPushDose_t _rxPushDose=NULL;
linCmtA_p linCmtA;
linCmtB_p linCmtB;
_rx_asgn _rxode2_rxAssignPtr =NULL;
_rx_asgn _rxQr =NULL;

rxode2_fn phi;
rxode2_fn ReLU;
rxode2_fn dReLU;
rxode2_fn GELU;
rxode2_fn dGELU;
rxode2_fn d2GELU;
rxode2_fn d3GELU;
rxode2_fn d4GELU;
rxode2_fn softplus;
rxode2_fn dsoftplus;
rxode2_fn d2softplus;
rxode2_fn d3softplus;
rxode2_fn d4softplus;
rxode2_fn SELU;
rxode2_fn dSELU;
rxode2_fn lReLU;
rxode2_fn dlReLU;
rxode2_fn Swish;
rxode2_fn dSwish;

rxode2_fn2 PReLU;
rxode2_fn2 dPReLU;
rxode2_fn2 dPReLUa;
rxode2_fn2 dPReLUa1;

rxode2_fn2 ELU;
rxode2_fn2 dELU;
rxode2_fn2 d2ELU;
rxode2_fn2 d2aELU;
rxode2_fn2 dELUa;
rxode2_fn2 d2ELUa;

rxode2_fn3 logit;
rxode2_fn3 expit;
rxode2_fn3 probitInv;
rxode2_fn3 probit;
rxode2_fn2 gammap;
rxode2_fn2 gammaq;
rxode2_fn2 lowergamma;
rxode2_fn2 uppergamma;
rxode2_fn2 gammapInv;
rxode2_fn2 gammapDer;
rxode2_fn2 gammapInva;
rxode2_fn2 gammaqInv;
rxode2_fn2 gammaqInva;

rxode2_fn2 rxnorm;
rxode2i_rxbinom rxbinom;
rxode2i_rxbinom rxnbinom;
rxode2i_rxbinom rxnbinomMu;
rxode2_fn2 rxcauchy;
rxode2_fn rxchisq;
rxode2_fn rxexp;
rxode2_fn2 rxf;
rxode2_ifn rxgeom;
rxode2_fn2 rxgamma;
rxode2_fn2 rxbeta;
rxode2_ifn rxpois;
rxode2_fn rxt_;
rxode2_fn2 rxunif;
rxode2_fn2 rxweibull;

rxode2i2_fn2 rinorm;
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
rxode2_compareFactorInt_fn _compareFactorInt;

static double _prod(double *input, double *p, int type, int n, ...){
  va_list valist;
  va_start(valist, n);
  for (unsigned int i = 0; i < n; i++){
    input[i] = va_arg(valist, double);
  }
  va_end(valist);
  return _prodPS(input, p, n, type);
}

static double _udf(const char *funName, double *input, int n, ...) {
  if (n == -42) (Rf_error)("%s", "this has a ui user function that cannot be called directly");
  va_list valist;
  va_start(valist, n);
  for (unsigned int i = 0; i < n; i++){
    input[i] = va_arg(valist, double);
  }
  va_end(valist);
  return _evalUdf(funName, n, input);
}

static double _sum(double *input, double *pld, int m, int type, int n, ...){
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

static double _sign(unsigned int n, ...) {
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

static double _mix(int _cSub, unsigned int n,  ...) {
  rx_solving_options_ind* ind = &(_solveData->subjects[_cSub]);
  va_list valist;
  double ret = NA_REAL;
  double p = 0.0;
  // In the case of simulating, mixunif is sampled once per patient
  double u = ind->mixunif;
  if (u >= 1.0) {
    // In this case, the mixest is assigned
    ind->mixest = u;
    u = 0;
    int found = 0;
    double v = 0.0;
    va_start(valist, n);
    for (unsigned int i = 0; i < n-1; i+= 2) {
      v = va_arg(valist, double);
      p += va_arg(valist, double);
      if (found == 0) {
        if (trunc(i*0.5 + 1) == trunc(ind->mixest)) {
          // This is the mixture selected
          ret = v;
          found = 1;
          ind->mixunif = u + (p - u)*0.5;
        }
        u = p;
      }
    }
    v = va_arg(valist, double); // other possibility
    if (found == 0) {
      ind->mixunif = u + (1.0 - u)*0.5;
      ret = v;
    }
    va_end(valist);
    return ret;
  } else {
    int found = 0;
    double v = 0.0;
    va_start(valist, n);
    for (unsigned int i = 0; i < n-1; i+= 2) {
      v = va_arg(valist, double);
      p += va_arg(valist, double);
      if (found == 0) {
        if (u < p) {
          ret = v;
          ind->mixest = i*0.5 + 1; // 1-based index
          found = 1;
        }
      }
    }
    v = va_arg(valist, double); // other possibility
    if (found == 0) {
      ind->mixest = _solveData->mixnum;
      ret = v;
    }
    va_end(valist);
    return ret;
  }
}


double _rxord(int _cSub, unsigned int n,  ...) {
  rx_solving_options_ind* ind = &(_solveData->subjects[_cSub]);
  if (!ind->inLhs) {
    return 1.0;
  }
  va_list valist;
  va_start(valist, n);
  double ret = 1.0;
  double p = 0.0;
  double u = rxunif(0.0, 1.0);
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

static void _obs(int _cSub, double _curTime, unsigned int n,  ...) {
  rx_solving_options_ind* _ind = &(_solveData->subjects[_cSub]);
  if (_ind->inLhs) {
    return; // only push observations in ode solving
  }
  va_list valist;
  va_start(valist, n);
  double _t;
  for (unsigned int i = 0; i < n; i++) {
    _t = va_arg(valist, double);
    // push observations after time
    _rxPushDose(_ind, _curTime,
                _curTime + _t, 0, 0, 1, 0, 0, 0, 0, 0);
  }
  va_end(valist);
  return;
}

double _transit4P(int cmt, double t, unsigned int id, double n, double mtt, double bio){
  double nd = (double) n;
  double ktr = (nd+1)/mtt;
  double lktr = _safe_log(nd+1)-_safe_log(mtt);
  double tlast = _solveData->subjects[id].tlastS[cmt];
  double dose = _solveData->subjects[id].curDoseS[cmt];
  if (ISNA(dose)) dose = 0.0;
  if (ISNA(tlast)) tlast = 0.0;
  double tad = (t-tlast);
  return exp(_safe_log(bio*dose)+lktr+n*(lktr+_safe_log(tad))-ktr*(tad)-lgamma1p(nd));
}

double _transit3P(int cmt, double t, unsigned int id, double n, double mtt){
  double nd = (double) n;
  double ktr = (nd+1)/mtt;
  double lktr = _safe_log(nd+1)-_safe_log(mtt);
  double tlast = _solveData->subjects[id].tlastS[cmt];
  if (ISNA(tlast)) tlast = 0.0;
  double tad = t-tlast;
  double podo = _solveData->subjects[id].curDoseS[cmt];
  if (ISNA(podo)) podo = 0.0;
  return exp(_safe_log(podo)+lktr+n*(lktr+_safe_log(tad))-ktr*(tad)-lgamma1p(nd));
}

// delay(state, T): value of ODE state `i` at time (t - T), used for delay
// differential equations (Monolix delay() semantics).  Before the start of
// integration the constant initial-condition history is returned.  Otherwise
// the value is interpolated from the per-subject dense history recorded by the
// solver, using the same 8th-order Dormand-Prince interpolant as dop853's
// contd8(), so delayed states are obtained to the full accuracy of the solve.
double _rxDelay(rx_solving_options_ind *_ind, int i, double t, double T) {
  double td = t - T;
  // Learn the smallest delay so the solver can cap its step size and never
  // step over the delay (keeping the lagged time inside recorded history).
  if (T > 0.0 && T < _ind->delayMinT) _ind->delayMinT = T;
  if (!_ind->delayHistOn || _ind->delayHistN == 0 || td <= _ind->delayT0) {
    return _solveData->op->inits[i];   // constant initial history before t0
  }
  int n = _ind->delayHistNeq;
  int stride = _ind->delayHistStride;
  double *hist = _ind->delayHist;
  // History stores only the states delay() looks back on; map this state's ODE
  // index to its compact history column.
  int col = _solveData->op->delayCol[i];
  if (col < 0) {
    return _solveData->op->inits[i];   // constant initial history defensive fallback
  }
  // Records are sorted by increasing step start time (xold) and cover
  // contiguous intervals [xold, xold+h].  Find the largest xold <= td; that
  // record's dense polynomial interpolates td (extrapolating slightly when td
  // falls in the in-progress, not-yet-recorded step, i.e. a delay below the
  // current step size).
  // t_old, h and the per-record solver type are stored in the last three slots
  // of every record, regardless of solver, so the bracketing search and the
  // interpolation dispatch are uniform even when a solve mixes dop853 and ros4
  // steps (the dense AutoSwitch composite).
  int lo = 0, hi = _ind->delayHistN - 1;
  while (lo < hi) {
    int mid = (lo + hi + 1) / 2;
    if (hist[(size_t) mid * stride + (stride - 3)] <= td) lo = mid; else hi = mid - 1;
  }
  double *rec = hist + (size_t) lo * stride;
  double xold = rec[stride - 3];
  double h    = rec[stride - 2];
  double s  = (td - xold) / h;
  if (rec[stride - 1] == 1.0) {
    // ros4 dense output is a cubic; reconstruct it from the four samples
    // stored at s = 0, 1/3, 2/3, 1 via Lagrange interpolation.
    double L0 = -4.5 * (s - 1.0/3.0) * (s - 2.0/3.0) * (s - 1.0);
    double L1 = 13.5 * s * (s - 2.0/3.0) * (s - 1.0);
    double L2 = -13.5 * s * (s - 1.0/3.0) * (s - 1.0);
    double L3 = 4.5 * s * (s - 1.0/3.0) * (s - 2.0/3.0);
    return L0 * rec[0 * n + col] + L1 * rec[1 * n + col] +
           L2 * rec[2 * n + col] + L3 * rec[3 * n + col];
  }
  // dop853 8th-order dense interpolant (same polynomial as contd8())
  double s1 = 1.0 - s;
  return rec[0 * n + col] + s * (rec[1 * n + col] + s1 * (rec[2 * n + col] +
         s * (rec[3 * n + col] + s1 * (rec[4 * n + col] + s * (rec[5 * n + col] +
         s1 * (rec[6 * n + col] + s * rec[7 * n + col]))))));
}

// rxDelayD(state, T): time-derivative d/dt'[state(t')] at t' = t - T, i.e. the
// derivative of the dense history interpolant.  Used by the forward-sensitivity
// machinery for parameter-dependent delays (delay durations that depend on an
// estimated parameter).  Before the start of integration the history is the
// constant initial condition, so the derivative is 0.
double _rxDelayD(rx_solving_options_ind *_ind, int i, double t, double T) {
  double td = t - T;
  if (T > 0.0 && T < _ind->delayMinT) _ind->delayMinT = T;
  if (!_ind->delayHistOn || _ind->delayHistN == 0 || td <= _ind->delayT0) {
    return 0.0;   // constant initial history -> zero time-derivative
  }
  int n = _ind->delayHistNeq;
  int stride = _ind->delayHistStride;
  double *hist = _ind->delayHist;
  int col = _solveData->op->delayCol[i];
  if (col < 0) {
    return 0.0;   // constant initial history -> zero time-derivative defensive fallback
  }
  int lo = 0, hi = _ind->delayHistN - 1;
  while (lo < hi) {
    int mid = (lo + hi + 1) / 2;
    if (hist[(size_t) mid * stride + (stride - 3)] <= td) lo = mid; else hi = mid - 1;
  }
  double *rec = hist + (size_t) lo * stride;
  double xold = rec[stride - 3];
  double h    = rec[stride - 2];
  double s  = (td - xold) / h;
  if (rec[stride - 1] == 1.0) {
    // derivative of the ros4 cubic Lagrange interpolant (nodes 0,1/3,2/3,1)
    double a = s - 1.0/3.0, b = s - 2.0/3.0, c = s - 1.0;
    double L0d = -4.5 * (b*c + a*c + a*b);
    double L1d = 13.5 * (b*c + s*c + s*b);
    double L2d = -13.5 * (a*c + s*c + s*a);
    double L3d = 4.5 * (a*b + s*b + s*a);
    return (L0d * rec[0 * n + col] + L1d * rec[1 * n + col] +
            L2d * rec[2 * n + col] + L3d * rec[3 * n + col]) / h;
  }
  // derivative of the dop853 8th-order interpolant: dy/dt' = (1/h) dy/ds,
  // computed from the same nested (Horner) form as _rxDelay by carrying the
  // value and its s-derivative together through each level (s1 = 1 - s).
  double s1 = 1.0 - s;
  double c0 = rec[0*n+col], c1 = rec[1*n+col], c2 = rec[2*n+col], c3 = rec[3*n+col];
  double c4 = rec[4*n+col], c5 = rec[5*n+col], c6 = rec[6*n+col], c7 = rec[7*n+col];
  double a6 = c6 + s * c7,    a6d = c7;
  double a5 = c5 + s1 * a6,   a5d = -a6 + s1 * a6d;
  double a4 = c4 + s * a5,    a4d = a5 + s * a5d;
  double a3 = c3 + s1 * a4,   a3d = -a4 + s1 * a4d;
  double a2 = c2 + s * a3,    a2d = a3 + s * a3d;
  double a1 = c1 + s1 * a2,   a1d = -a2 + s1 * a2d;
  double yd = a1 + s * a1d;   // d/ds of (c0 + s*a1)
  return yd / h;
}

// rxDelayD2(state, T): second time-derivative d^2/dt'^2[state(t')] at t' = t - T,
// i.e. the second derivative of the dense history interpolant.  Used by the
// second-order forward sensitivities of parameter-dependent delays.  Constant
// initial history -> 0.
double _rxDelayD2(rx_solving_options_ind *_ind, int i, double t, double T) {
  double td = t - T;
  if (T > 0.0 && T < _ind->delayMinT) _ind->delayMinT = T;
  if (!_ind->delayHistOn || _ind->delayHistN == 0 || td <= _ind->delayT0) {
    return 0.0;
  }
  int n = _ind->delayHistNeq;
  int stride = _ind->delayHistStride;
  double *hist = _ind->delayHist;
  int col = _solveData->op->delayCol[i];
  if (col < 0) return 0.0;
  int lo = 0, hi = _ind->delayHistN - 1;
  while (lo < hi) {
    int mid = (lo + hi + 1) / 2;
    if (hist[(size_t) mid * stride + (stride - 3)] <= td) lo = mid; else hi = mid - 1;
  }
  double *rec = hist + (size_t) lo * stride;
  double xold = rec[stride - 3];
  double h    = rec[stride - 2];
  double s  = (td - xold) / h;
  if (rec[stride - 1] == 1.0) {
    // second derivative of the ros4 cubic Lagrange interpolant (linear in s):
    // for a cubic (s-a)(s-b)(s-c), d^2/ds^2 = 6s - 2(a+b+c).
    double L0dd = -4.5 * (6.0 * s - 4.0);            // nodes 1/3,2/3,1 -> sum 2
    double L1dd = 13.5 * (6.0 * s - 10.0 / 3.0);     // nodes 0,2/3,1   -> sum 5/3
    double L2dd = -13.5 * (6.0 * s - 8.0 / 3.0);     // nodes 0,1/3,1   -> sum 4/3
    double L3dd = 4.5 * (6.0 * s - 2.0);             // nodes 0,1/3,2/3 -> sum 1
    return (L0dd * rec[0 * n + col] + L1dd * rec[1 * n + col] +
            L2dd * rec[2 * n + col] + L3dd * rec[3 * n + col]) / (h * h);
  }
  // second derivative of the dop853 8th-order interpolant: d^2y/dt'^2 =
  // (1/h^2) d^2y/ds^2, carrying value, first and second s-derivatives through
  // the same nested Horner form (d^2/ds^2 of c+s*A = 2A'+s*A''; of c+s1*A =
  // -2A'+s1*A'').
  double s1 = 1.0 - s;
  double c0 = rec[0*n+col], c1 = rec[1*n+col], c2 = rec[2*n+col], c3 = rec[3*n+col];
  double c4 = rec[4*n+col], c5 = rec[5*n+col], c6 = rec[6*n+col], c7 = rec[7*n+col];
  double a6 = c6 + s * c7,    a6d = c7,            a6dd = 0.0;
  double a5 = c5 + s1 * a6,   a5d = -a6 + s1 * a6d, a5dd = -2.0 * a6d + s1 * a6dd;
  double a4 = c4 + s * a5,    a4d = a5 + s * a5d,   a4dd = 2.0 * a5d + s * a5dd;
  double a3 = c3 + s1 * a4,   a3d = -a4 + s1 * a4d, a3dd = -2.0 * a4d + s1 * a4dd;
  double a2 = c2 + s * a3,    a2d = a3 + s * a3d,   a2dd = 2.0 * a3d + s * a3dd;
  double a1 = c1 + s1 * a2,   a1d = -a2 + s1 * a2d, a1dd = -2.0 * a2d + s1 * a2dd;
  (void) a1;
  double ydd = 2.0 * a1d + s * a1dd;   // d^2/ds^2 of (c0 + s*a1)
  return ydd / (h * h);
}

// rxDelayD3(state, T): third time-derivative d^3/dt'^3[state(t')] at t' = t - T.
// Used by the third-order forward sensitivities of parameter-dependent delays
// (breaking-point jump terms).  Constant initial history -> 0.
double _rxDelayD3(rx_solving_options_ind *_ind, int i, double t, double T) {
  double td = t - T;
  if (T > 0.0 && T < _ind->delayMinT) _ind->delayMinT = T;
  if (!_ind->delayHistOn || _ind->delayHistN == 0 || td <= _ind->delayT0) {
    return 0.0;
  }
  int n = _ind->delayHistNeq;
  int stride = _ind->delayHistStride;
  double *hist = _ind->delayHist;
  int col = _solveData->op->delayCol[i];
  if (col < 0) return 0.0;
  int lo = 0, hi = _ind->delayHistN - 1;
  while (lo < hi) {
    int mid = (lo + hi + 1) / 2;
    if (hist[(size_t) mid * stride + (stride - 3)] <= td) lo = mid; else hi = mid - 1;
  }
  double *rec = hist + (size_t) lo * stride;
  double xold = rec[stride - 3];
  double h    = rec[stride - 2];
  double s  = (td - xold) / h;
  if (rec[stride - 1] == 1.0) {
    // third derivative of the ros4 cubic Lagrange interpolant is constant
    // (6 * leading coefficient); any higher derivative is 0.
    return (-27.0 * rec[0 * n + col] + 81.0 * rec[1 * n + col] +
            -81.0 * rec[2 * n + col] + 27.0 * rec[3 * n + col]) / (h * h * h);
  }
  // third derivative of the dop853 8th-order interpolant: d^3y/dt'^3 =
  // (1/h^3) d^3y/ds^3, carrying value + first/second/third s-derivatives
  // (d^3/ds^3 of c+s*A = 3A''+s*A'''; of c+s1*A = -3A''+s1*A''').
  double s1 = 1.0 - s;
  double c0 = rec[0*n+col], c1 = rec[1*n+col], c2 = rec[2*n+col], c3 = rec[3*n+col];
  double c4 = rec[4*n+col], c5 = rec[5*n+col], c6 = rec[6*n+col], c7 = rec[7*n+col];
  double a6 = c6 + s * c7,   a6d = c7,             a6dd = 0.0,                  a6ddd = 0.0;
  double a5 = c5 + s1 * a6,  a5d = -a6 + s1 * a6d, a5dd = -2.0*a6d + s1*a6dd,   a5ddd = -3.0*a6dd + s1*a6ddd;
  double a4 = c4 + s * a5,   a4d = a5 + s * a5d,   a4dd = 2.0*a5d + s*a5dd,     a4ddd = 3.0*a5dd + s*a5ddd;
  double a3 = c3 + s1 * a4,  a3d = -a4 + s1 * a4d, a3dd = -2.0*a4d + s1*a4dd,   a3ddd = -3.0*a4dd + s1*a4ddd;
  double a2 = c2 + s * a3,   a2d = a3 + s * a3d,   a2dd = 2.0*a3d + s*a3dd,     a2ddd = 3.0*a3dd + s*a3ddd;
  double a1 = c1 + s1 * a2,  a1d = -a2 + s1 * a2d, a1dd = -2.0*a2d + s1*a2dd,   a1ddd = -3.0*a2dd + s1*a2ddd;
  (void) a1; (void) a1d;
  double yddd = 3.0 * a1dd + s * a1ddd;   // d^3/ds^3 of (c0 + s*a1)
  return yddd / (h * h * h);
}

void _assignFuns0(void) {
  _evalUdf = (_udf_type) R_GetCCallable("rxode2", "_rxode2_evalUdf");
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
  _compareFactorVal=(rxode2_compareFactorVal_fn) R_GetCCallable("rxode2", "compareFactorVal");
  _compareFactorInt=(rxode2_compareFactorInt_fn) R_GetCCallable("rxode2", "compareFactorInt");
  _update_par_ptr = (_update_par_ptr_p) R_GetCCallable("rxode2","_update_par_ptr");
  _getParCov = (_getParCov_p) R_GetCCallable("rxode2","_getParCov");
  _setThreadInd = (_setThreadInd_t) R_GetCCallable("rxode2","_setThreadInd");
  _rxPushDose   = (_rxPushDose_t)   R_GetCCallable("rxode2","_rxPushDose");
  // dynamic start
  linCmtA=(linCmtA_p)R_GetCCallable("rxode2", "linCmtA");
  linCmtB=(linCmtB_p)R_GetCCallable("rxode2", "linCmtB");
  rxnorm = (rxode2i_fn2)R_GetCCallable("rxode2", "rxnorm");
  rxbinom = (rxode2i_rxbinom)R_GetCCallable("rxode2","rxbinom");
  rxnbinom = (rxode2i_rxbinom)R_GetCCallable("rxode2","rxnbinom");
  rxnbinomMu = (rxode2i_rxbinom)R_GetCCallable("rxode2","rxnbinomMu");
  rxcauchy = (rxode2_fn2)R_GetCCallable("rxode2","rxcauchy") ;
  rxchisq = (rxode2_fn)R_GetCCallable("rxode2","rxchisq") ;
  rxexp = (rxode2_fn)R_GetCCallable("rxode2","rxexp");
  rxf = (rxode2_fn2)R_GetCCallable("rxode2","rxf");
  rxgeom = (rxode2_ifn)R_GetCCallable("rxode2","rxgeom");
  rxgamma = (rxode2_fn2)R_GetCCallable("rxode2","rxgamma");
  rxbeta = (rxode2_fn2)R_GetCCallable("rxode2","rxbeta");
  rxpois = (rxode2_ifn)R_GetCCallable("rxode2","rxpois");
  rxt_ = (rxode2_fn)R_GetCCallable("rxode2","rxt_");
  rxunif = (rxode2_fn2)R_GetCCallable("rxode2","rxunif");
  rxweibull = (rxode2_fn2)R_GetCCallable("rxode2","rxweibull");
  rinorm = (rxode2i2_fn2)R_GetCCallable("rxode2", "rinorm");
  ribinom = (rxode2i2_ribinom)R_GetCCallable("rxode2","ribinom");
  rinbinom = (rxode2i2_ribinom)R_GetCCallable("rxode2","rinbinom");
  rinbinomMu = (rxode2i2_ribinom)R_GetCCallable("rxode2","rinbinomMu");
  ricauchy = (rxode2i2_fn2)R_GetCCallable("rxode2","ricauchy");
  richisq = (rxode2i2_fn)R_GetCCallable("rxode2","richisq");
  riexp = (rxode2i2_fn)R_GetCCallable("rxode2","riexp");
  rif = (rxode2i2_fn2)R_GetCCallable("rxode2","rif");
  rigeom = (rxode2i2_ifn)R_GetCCallable("rxode2","rigeom");
  rigamma = (rxode2i2_fn2)R_GetCCallable("rxode2","rigamma");
  ribeta = (rxode2i2_fn2)R_GetCCallable("rxode2","ribeta");
  ripois = (rxode2i2_ifn)R_GetCCallable("rxode2","ripois");
  rit_ = (rxode2i2_fn)R_GetCCallable("rxode2","rit_");
  riunif = (rxode2i2_fn2)R_GetCCallable("rxode2","riunif");
  riweibull = (rxode2i2_fn2)R_GetCCallable("rxode2","riweibull");
  ReLU = (rxode2_fn)R_GetCCallable("rxode2","ReLU");
  dReLU = (rxode2_fn)R_GetCCallable("rxode2","dReLU");
  GELU = (rxode2_fn)R_GetCCallable("rxode2","GELU");
  dGELU = (rxode2_fn)R_GetCCallable("rxode2","dGELU");
  d2GELU = (rxode2_fn)R_GetCCallable("rxode2","d2GELU");
  d3GELU = (rxode2_fn)R_GetCCallable("rxode2","d3GELU");
  d4GELU = (rxode2_fn)R_GetCCallable("rxode2","d4GELU");
  softplus = (rxode2_fn)R_GetCCallable("rxode2","softplus");
  dsoftplus = (rxode2_fn)R_GetCCallable("rxode2","dsoftplus");
  d2softplus = (rxode2_fn)R_GetCCallable("rxode2","d2softplus");
  d3softplus = (rxode2_fn)R_GetCCallable("rxode2","d3softplus");
  d4softplus = (rxode2_fn)R_GetCCallable("rxode2","d4softplus");
  SELU = (rxode2_fn)R_GetCCallable("rxode2","SELU");
  dSELU = (rxode2_fn)R_GetCCallable("rxode2","dSELU");
  lReLU = (rxode2_fn)R_GetCCallable("rxode2","lReLU");
  dlReLU = (rxode2_fn)R_GetCCallable("rxode2","dlReLU");
  Swish = (rxode2_fn)R_GetCCallable("rxode2","Swish");
  dSwish = (rxode2_fn)R_GetCCallable("rxode2","dSwish");
  PReLU = (rxode2_fn2)R_GetCCallable("rxode2","PReLU");
  dPReLU = (rxode2_fn2)R_GetCCallable("rxode2","dPReLU");
  dPReLUa = (rxode2_fn2)R_GetCCallable("rxode2","dPReLUa");
  dPReLUa1 = (rxode2_fn2)R_GetCCallable("rxode2","dPReLUa1");
  ELU = (rxode2_fn2)R_GetCCallable("rxode2","ELU");
  dELU = (rxode2_fn2)R_GetCCallable("rxode2","dELU");
  d2ELU = (rxode2_fn2)R_GetCCallable("rxode2","d2ELU");
  d2aELU = (rxode2_fn2)R_GetCCallable("rxode2","d2aELU");
  dELUa = (rxode2_fn2)R_GetCCallable("rxode2","dELUa");
  d2ELUa = (rxode2_fn2)R_GetCCallable("rxode2","d2ELUa");
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
  probit = (rxode2_fn3) R_GetCCallable("rxode2", "probit");
  expit = (rxode2_fn3) R_GetCCallable("rxode2", "expit");
  probitInv = (rxode2_fn3) R_GetCCallable("rxode2", "probitInv");
  simeta =(_simfun) R_GetCCallable("rxode2", "simeta");
  simeps =(_simfun) R_GetCCallable("rxode2", "simeps");

  _llikNorm=(rxode2_llikNormFun) R_GetCCallable("rxode2ll","rxLlikNorm");
  _llikNormDmean=(rxode2_llikNormFun) R_GetCCallable("rxode2ll","rxLlikNormDmean");
  _llikNormDsd=(rxode2_llikNormFun) R_GetCCallable("rxode2ll","rxLlikNormDsd");

  _llikPois        = (rxode2_llikPoisFun) R_GetCCallable("rxode2ll","rxLlikPois");
  _llikPoisDlambda = (rxode2_llikPoisFun) R_GetCCallable("rxode2ll","rxLlikPoisDlambda");

  _llikBinom = (rxode2_llikBinomFun) R_GetCCallable("rxode2ll", "rxLlikBinom");
  _llikBinomDprob = (rxode2_llikBinomFun) R_GetCCallable("rxode2ll", "rxLlikBinomDprob");

  _llikNbinom = (rxode2_llikBinomFun) R_GetCCallable("rxode2ll", "rxLlikNbinom");
  _llikNbinomDprob = (rxode2_llikBinomFun) R_GetCCallable("rxode2ll", "rxLlikNbinomDprob");

  _llikNbinomMu = (rxode2_llikBinomFun) R_GetCCallable("rxode2ll", "rxLlikNbinomMu");
  _llikNbinomMuDmu = (rxode2_llikBinomFun) R_GetCCallable("rxode2ll", "rxLlikNbinomMuDmu");

  _llikBeta = (rxode2_llikBetaFun)   R_GetCCallable("rxode2ll", "rxLlikBeta");
  _llikBetaDshape1 = (rxode2_llikBetaFun) R_GetCCallable("rxode2ll", "rxLlikBetaDshape1");
  _llikBetaDshape2 = (rxode2_llikBetaFun) R_GetCCallable("rxode2ll", "rxLlikBetaDshape2");

  _llikT = (rxode2_llikTFun)   R_GetCCallable("rxode2ll", "rxLlikT");
  _llikTDdf = (rxode2_llikTFun) R_GetCCallable("rxode2ll", "rxLlikTDdf");
  _llikTDmean = (rxode2_llikTFun) R_GetCCallable("rxode2ll", "rxLlikTDmean");
  _llikTDsd = (rxode2_llikTFun) R_GetCCallable("rxode2ll", "rxLlikTDsd");
  _llikChisq    = (rxode2_llikChisqFun) R_GetCCallable("rxode2ll", "rxLlikChisq");
  _llikChisqDdf = (rxode2_llikChisqFun) R_GetCCallable("rxode2ll", "rxLlikChisqDdf");

  _llikExp    = (rxode2_llikExpFun) R_GetCCallable("rxode2ll", "rxLlikExp");
  _llikExpDrate = (rxode2_llikExpFun) R_GetCCallable("rxode2ll", "rxLlikExpDrate");

  _llikF    = (rxode2_llikFFun) R_GetCCallable("rxode2ll", "rxLlikF");
  _llikFDdf1 = (rxode2_llikFFun) R_GetCCallable("rxode2ll", "rxLlikFDdf1");
  _llikFDdf2 = (rxode2_llikFFun) R_GetCCallable("rxode2ll", "rxLlikFDdf2");

  _llikGeom    = (rxode2_llikGeomFun) R_GetCCallable("rxode2ll", "rxLlikGeom");
  _llikGeomDp  = (rxode2_llikGeomFun) R_GetCCallable("rxode2ll", "rxLlikGeomDp");

  _llikUnif        = (rxode2_llikUnifFun) R_GetCCallable("rxode2ll", "rxLlikUnif");
  _llikUnifDalpha  = (rxode2_llikUnifFun) R_GetCCallable("rxode2ll", "rxLlikUnifDalpha");
  _llikUnifDbeta   = (rxode2_llikUnifFun) R_GetCCallable("rxode2ll", "rxLlikUnifDbeta");

  _llikWeibull        = (rxode2_llikWeibullFun) R_GetCCallable("rxode2ll", "rxLlikWeibull");
  _llikWeibullDshape  = (rxode2_llikWeibullFun) R_GetCCallable("rxode2ll", "rxLlikWeibullDshape");
  _llikWeibullDscale   = (rxode2_llikWeibullFun) R_GetCCallable("rxode2ll", "rxLlikWeibullDscale");

  _llikGamma        = (rxode2_llikGammaFun) R_GetCCallable("rxode2ll", "rxLlikGamma");
  _llikGammaDshape  = (rxode2_llikGammaFun) R_GetCCallable("rxode2ll", "rxLlikGammaDshape");
  _llikGammaDrate   = (rxode2_llikGammaFun) R_GetCCallable("rxode2ll", "rxLlikGammaDrate");

  _llikCauchy        = (rxode2_llikCauchyFun) R_GetCCallable("rxode2ll", "rxLlikCauchy");
  _llikCauchyDlocation  = (rxode2_llikCauchyFun) R_GetCCallable("rxode2ll", "rxLlikCauchyDlocation");
  _llikCauchyDscale   = (rxode2_llikCauchyFun) R_GetCCallable("rxode2ll", "rxLlikCauchyDscale");
  // dynamic stop
  _solveData = _getRxSolve_();
}

void _assignFuns(void) {
  // Re-initialize if rxode2 was reloaded: the registered function pointer will
  // differ from the one stored at last init, revealing a stale _solveData.
  if (_assign_ptr == NULL ||
      _assign_ptr != (rxode2_assign_ptr)R_GetCCallable("rxode2","rxode2_assign_fn_pointers")){
    _assignFuns0();
  }
}

void __assignFuns2(rx_solve rx,
                   rx_solving_options op,
                   t_F f,
                   t_LAG lag,
                   t_RATE rate,
                   t_DUR dur,
                   t_calc_mtime mtime,
                   t_ME me,
                   t_IndF indf,
                   t_getTime gettime,
                   t_locateTimeIndex timeindex,
                   t_handle_evidL handleEvid,
                   t_getDur getdur) {
  // assign start
  // Always look up via R_GetCCallable so that a reloaded rxode2 (new address)
  // is used rather than a stale static pointer from a previous load session.
  rxode2_assignFuns2_t rxode2parse_assignFuns2 = (rxode2_assignFuns2_t)(R_GetCCallable("rxode2", "_rxode2_assignFuns2"));
  rxode2parse_assignFuns2(rx, op, f, lag, rate, dur, mtime, me, indf, gettime, timeindex, handleEvid, getdur);
  // assign stop
}
