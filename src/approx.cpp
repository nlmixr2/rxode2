#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "rxomp.h"
#include <stdio.h>
#include <stdarg.h>
#include "../inst/include/rxode2.h"
#include "../inst/include/rxode2parseHandleEvid.h"
#include "../inst/include/rxode2parseGetTime.h"

#define safe_zero(a) ((a) == 0 ? DBL_EPSILON : (a))
#define _as_zero(a) (fabs(a) < sqrt(DBL_EPSILON) ? 0.0 : a)
#define _as_dbleps(a) (fabs(a) < sqrt(DBL_EPSILON) ? ((a) < 0 ? -sqrt(DBL_EPSILON)  : sqrt(DBL_EPSILON)) : a)

#define isSameTimeOp(xout, xp) (op->stiff == 0 ? isSameTimeDop(xout, xp) : isSameTime(xout, xp))

#define _(String) (String)
//#include "lincmtB2.h"
//#include "lincmtB3d.h"

extern "C" void handleTlast(double *time, rx_solving_options_ind *ind);

double rxunif(rx_solving_options_ind* ind, double low, double hi);

// From https://cran.r-project.org/web/packages/Rmpfr/vignettes/log1mexp-note.pdf
extern "C" double log1mex(double a){
  if (a < M_LN2) return log(-expm1(-a));
  return(log1p(-exp(-a)));
}

extern "C" int _locateTimeIndex(double obs_time,  rx_solving_options_ind *ind){
  // Uses bisection for slightly faster lookup of dose index.
  int i, j, ij;
  i = 0;
  j = ind->n_all_times - 1;
  if (obs_time < (ind->fns ? ind->fns->gettime(ind->ix[i], ind) : getTime(ind->ix[i], ind))){
    return i;
  }
  if (obs_time > (ind->fns ? ind->fns->gettime(ind->ix[j], ind) : getTime(ind->ix[j], ind))){
    return j;
  }
  while(i < j - 1) { /* x[i] <= obs_time <= x[j] */
    ij = (i + j)/2; /* i+1 <= ij <= j-1 */
    if(obs_time < (ind->fns ? ind->fns->gettime(ind->ix[ij], ind) : getTime(ind->ix[ij], ind)))
      j = ij;
    else
      i = ij;
  }
  /* if (i == 0) return 0; */
  while(i != 0 && isSameTime(obs_time, (ind->fns ? ind->fns->gettime(ind->ix[i], ind) : getTime(ind->ix[i], ind)))){
    i--;
  }
  if (i == 0){
    while(i < ind->ndoses-2 && fabs(obs_time  - (ind->fns ? ind->fns->gettime(ind->ix[i+1], ind) : getTime(ind->ix[i+1], ind)))<= sqrt(DBL_EPSILON)){
      i++;
    }
  }
  return i;
}

/* Authors: Robert Gentleman and Ross Ihaka and The R Core Team */
/* Taken directly from https://github.com/wch/r-source/blob/922777f2a0363fd6fe07e926971547dd8315fc24/src/library/stats/src/approx.c*/
/* Changed as follows:
   - Different Name
   - Use rxode2 structure
   - Use getTime(to allow model-based changes to dose timing
   - Use getValue to ignore NA values for time-varying covariates
*/
static inline double rxCovRec(double *y, int raw, int nOrig) {
  return (raw >= 0 && raw < nOrig) ? y[raw] : NA_REAL;
}

static inline bool rxPushedRec(const int *ix, int i, int nOrig) {
  return (ix == NULL ? i : ix[i]) >= nOrig;
}

extern "C" int _rxDvCov;

// getValue/rx_approxP index records through ix (NULL = record order, the
// resampled-covariate case) and report the lh = -2/2 index through *iOut.
// Records from nOrig on were pushed while solving and have no covariate value.
static inline double getValue(int idx, double *y, int is_locf, const int *ix, int n,
                              int nOrig, rx_solving_options *op, int lh, int *iOut = NULL){
#define _Y(i) rxCovRec(y, ix == NULL ? (i) : ix[i], nOrig)
  int i = idx;
  double ret = _Y(idx);
  if (ISNA(ret)) {
    // NA handling
    int backward = 1;
    if (is_locf == 1) {
      backward = 1; // for locf always get the previous value
    } else if (is_locf == 2) {
      backward = 0; // for nocb always get the previous value
    } else if (is_locf == 0 || is_locf == 3) {
      // linear & midpoint choose based on direction
      if (lh == -1 || lh == -2) {
        // get previous value for the left or centered value
        backward = 1;
      } else if (lh == 0) {
        backward = op->instant_backward;
      } else {
        // get next value for the right value for approx
        backward = 0;
      }
    }
    if (backward) {
      // Go backward.
      while (ISNA(ret) && i != 0) {
        i--; ret = _Y(i);
      }
      if (ISNA(ret)) {
        // Still not found go forward.
        i = idx;
        while (ISNA(ret) && i != n-1){
          i++; ret = _Y(i);
        }
      }
    } else {
      // Go forward
      while (ISNA(ret) && i != n-1) {
        i++; ret = _Y(i);
      }
      if (ISNA(ret)) {
        // Still not found go backward
        i = idx;
        while (ISNA(ret) && i != 0){
          i--; ret = _Y(i);
        }
      }
    }
  }
#undef _Y
  if (iOut != NULL) *iOut = i;
  return ret;
}

static inline double getValue(int idx, double *y, int is_locf,
                              rx_solving_options_ind *ind, rx_solving_options *op,
                              int lh){
  return getValue(idx, y, is_locf, ind->ix, ind->n_all_times, ind->n_all_times_orig, op, lh);
}

// A covariate's value on record idx.  DV is a measurement, not a covariate: a
// record without one (a dose) keeps NA instead of a neighbouring record's DV.
static inline double rxRecCov(int k, int idx, double *y, int is_locf,
                              rx_solving_options_ind *ind, rx_solving_options *op) {
  if (k == _rxDvCov) return rxCovRec(y, ind->ix == NULL ? idx : ind->ix[idx], ind->n_all_times_orig);
  return getValue(idx, y, is_locf, ind, op, 0);
}

// v is at record i's time; a pushed record has no value of its own, so
// interpolate across it instead
static inline bool rxAtDataRec(double v, double ti, const int *ix, int i, int nOrig) {
  return isSameTime(v, ti) && !rxPushedRec(ix, i, nOrig);
}

// Linear interpolation between records i and j, moved past NA values
template <typename TimeFn>
static inline double rxApproxLinear(double v, int i, int j, double *y, int n, int nOrig,
                                    const int *ix, rx_solving_options *Meth, TimeFn T) {
  int idxLow = i, idxHi = j;
  double vi = getValue(i, y, 0, ix, n, nOrig, Meth, -2, &idxLow);
  double vj = getValue(j, y, 0, ix, n, nOrig, Meth, 2, &idxHi);
  // only one side has a value (eg a trailing NA or pushed record)
  if (idxLow == idxHi) return vi;
  double ti = T(idxLow);
  double tj = T(idxHi);
  if (isSameTime(ti, tj)) return vi;
  return vi + (vj - vi) * ((v - ti)/(tj - ti));
}

#define V(i, lh) getValue(i, y, is_locf, ix, n, nOrig, Meth, lh)
template <typename TimeFn>
static inline double rx_approxP(double v, double *y, int is_locf, int n, int nOrig,
                                const int *ix, rx_solving_options *Meth, TimeFn T){
  /* Approximate  y(v),  given (x,y)[i], i = 0,..,n-1 */
  int i, j, ij;
  if(!n) return R_NaN;

  i = 0; j = n - 1;

  /* handle out-of-domain points */
  if(v < T(i)) return V(0, -1);
  if(v > T(j)) return V(n-1, 1);

  /* find the correct interval by bisection */
  while(i < j - 1) { /* T(i) <= v <= T(j) */
    ij = (i + j)/2; /* i+1 <= ij <= j-1 */
    if(v < T(ij)) j = ij; else i = ij;
    /* still i < j */
  }
  /* provably have i == j-1 */

  /* interpolation */

  double tj = T(j);
  double ti = T(i);
  if(rxAtDataRec(v, tj, ix, j, nOrig)) return V(j, 1);
  if(rxAtDataRec(v, ti, ix, i, nOrig)) return V(i, -1);
  /* impossible: if(T(j) == T(i)) return V(i); */

  switch (is_locf) {
  case 0: // linear
    return rxApproxLinear(v, i, j, y, n, nOrig, ix, Meth, T);
    break;
  case 1: // locf
    return V(i, -1);
    break;
  case 2: // nocb
    return V(j, 1);
    break;
  case 3: // midpoint
    return 0.5*(V(i, -1) + V(j, 1));
    break;
  }
  return NA_REAL; // nocov
}/* approx1() */

#undef V

// Covariate of this subject at time t, on its (possibly lagged) sorted times
static inline double rxApproxCov(double t, double *y, int is_locf,
                                 rx_solving_options *op, rx_solving_options_ind *id) {
  return rx_approxP(t, y, is_locf, id->n_all_times, id->n_all_times_orig, id->ix, op, [id](int i) {
    return id->fns ? id->fns->gettime(id->ix[i], id) : getTime(id->ix[i], id);
  });
}

// Covariate k of a subject at time t from its data records in record order and
// their data times, never ix/timeThread: the subject a resampled covariate is
// drawn from may be unsorted or solving on another thread, and a pushed record
// is not sorted into ix yet when its lag is evaluated.
static inline double rxApproxCovData(double t, int k, int is_locf,
                                     rx_solving_options *op, rx_solving_options_ind *indSample) {
  int n = indSample->n_all_times_orig;
  double *y = indSample->cov_ptr + n*k;
  double *at = indSample->all_times;
  int *evid = indSample->evid;
  // a modeled rate/duration stop's time is set while solving; use its start's
  return rx_approxP(t, y, is_locf, n, n, NULL, op, [at, evid](int i) {
    return (i > 0 && (isEvidModeledRateStop(evid[i]) ||
                      isEvidModeledDurationStop(evid[i]))) ? at[i-1] : at[i];
  });
}

/* End approx from R */

// getParCov first(parNo, idx=0) last(parNo, idx=ind->n_all_times-1)
extern "C" double _getParCov(unsigned int id, rx_solve *rx, int parNo, int idx0){
  rx_solving_options_ind *ind;
  ind = &(rx->subjects[id]);
  rx_solving_options *op = rx->op;
  int idx=0;
  if (idx0 == NA_INTEGER){
    idx=0;
    if (getEvid(ind, ind->ix[idx]) == 9) idx++;
  } else if (idx0 >= ind->n_all_times) {
    return NA_REAL;
  } else {
    idx=idx0;
  }
  if (idx < 0 || idx > ind->n_all_times) return NA_REAL;
  if (op->do_par_cov){
    for (int k = op->ncov; k--;){
      if (op->par_cov[k] == parNo+1){
        double *y = ind->cov_ptr + ind->n_all_times_orig*k;
        // a pushed record has no value; use the nearest data record before it
        // (or after it, when none is before)
        int i = idx;
        while (i > 0 && rxPushedRec(ind->ix, i, ind->n_all_times_orig)) i--;
        while (i < ind->n_all_times-1 && rxPushedRec(ind->ix, i, ind->n_all_times_orig)) i++;
        return rxCovRec(y, ind->ix[i], ind->n_all_times_orig);
      }
    }
  }
  return ind->par_ptr[parNo];
}

// The time PK-type statements read with rxControl(nonmem = TRUE)
// (rxode2#1429): the time of the first data record at or after `t`, starting
// from the record that ends the interval being integrated (ind->idx), since a
// solver may step past it.  Records that are not data records are skipped,
// like NONMEM's non-event doses: addl repeats and the records a dose expands
// to (ind->pkSkip, from etTrans()), doses pushed at run time and lagged doses.
extern "C" double _rxPkTime(double t, unsigned int id, rx_solve *rx) {
  if (!rx->nonmem) return t;
  rx_solving_options *op = rx->op;
  rx_solving_options_ind *ind = &(rx->subjects[id]);
  if (ind->idx < 0 || ind->timeThread == NULL) return t;
  double t0 = t - ind->curShift;
  for (int j = ind->idx; j < ind->n_all_times; ++j) {
    int raw = ind->ix[j];
    if (raw < 0 || raw >= ind->n_all_times_orig) continue;
    if (ind->pkSkip != NULL && ind->pkSkip[raw]) continue;
    double tj = ind->timeThread[raw];
    if (tj < t0 && !isSameTimeOp(tj, t0)) continue;
    if (isDose(getEvid(ind, raw)) && !isSameTime(tj, ind->all_times[raw])) continue;
    return tj + ind->curShift;
  }
  return t;
}

// getTime() decodes the evid of the record it looks up into
// ind->wh/cmt/wh100/whI/wh0.  Looking up neighbouring records while
// interpolating a covariate must not clobber the event being handled.
struct RxSaveWh {
  rx_solving_options_ind *ind;
  int wh, cmt, wh100, whI, wh0;
  RxSaveWh(rx_solving_options_ind *i) : ind(i), wh(i->wh), cmt(i->cmt),
                                         wh100(i->wh100), whI(i->whI), wh0(i->wh0) {}
  ~RxSaveWh() {
    ind->wh = wh; ind->cmt = cmt; ind->wh100 = wh100; ind->whI = whI; ind->wh0 = wh0;
  }
};

extern "C" void _update_par_ptr(double tt, unsigned int id, rx_solve *rx, int idxIn) {
  if (rx == NULL) (Rf_errorcall)(R_NilValue, _("solve data is not loaded"));
  rx_solving_options_ind *ind, *indSample;
  ind = &(rx->subjects[id]);
  double t = 0.0;
  if (!ISNA(ind->ssTime)) {
    t = ind->ssTime;
  } else {
    t = tt;
  }
  if (ind->_update_par_ptr_in) return;
  int idx = idxIn;
  rx_solving_options *op = rx->op;
  if (!op->do_par_cov) return;
  RxSaveWh _saveWh(ind);
  // handle extra dose, and out of bounds idx values
  if (idx < 0 && ind->extraDoseN[0] > 0) {
    if (-1-idx >= ind->extraDoseN[0]) {
      // Get the last dose index for the extra doses
      idx = -1-ind->extraDoseTimeIdx[ind->extraDoseN[0]-1];
    }
    // extra dose time, find the closest index
    double v = (ind->fns ? ind->fns->gettime(idxIn, ind) : getTime(idxIn, ind));
    int i, j, ij, n = ind->n_all_times;
    i = 0;
    j = n - 1;
    if (v < (ind->fns ? ind->fns->gettime(ind->ix[i], ind) : getTime(ind->ix[i], ind))) {
      idx = i;
    } else if (v > (ind->fns ? ind->fns->gettime(ind->ix[j], ind) : getTime(ind->ix[j], ind))) {
      idx = j;
    } else {
      /* find the correct interval by bisection */
      while(i < j - 1) { /* T(i) <= v <= T(j) */
        ij = (i + j)/2; /* i+1 <= ij <= j-1 */
        if (v < (ind->fns ? ind->fns->gettime(ind->ix[ij], ind) : getTime(ind->ix[ij], ind))) {
          j = ij;
        } else  {
          i = ij;
        }
      }
      // Pick best match
      if (isSameTimeOp(v, (ind->fns ? ind->fns->gettime(ind->ix[j], ind) : getTime(ind->ix[j], ind)))) {
        idx = j;
      } else if (isSameTimeOp(v, (ind->fns ? ind->fns->gettime(ind->ix[i], ind) : getTime(ind->ix[i], ind)))) {
        idx = i;
      } else if (op->instant_backward == 0) {
        // use instant_backward to change the idx too; it does not
        // change based on covariate
        // backward=0=locf
        // backward=1=nocb
        // nocb
        idx = j;
      }  else {
        // locf
        idx = i;
      }
    }
  }
  if (idx >= ind->n_all_times) {
    idx = ind->n_all_times-1;
  } else if (idx < 0) {
    idx = 0;
  }
  ind->_update_par_ptr_in = 1;
  if (ISNA(t)) {
    // functional lag, rate, duration, mtime
    // Update all covariate parameters
    int k, idxSample;
    int ncov = op->ncov;
    indSample = ind;
    if (op->do_par_cov) {
      for (k = ncov; k--;) {
        if (op->par_cov[k]) {
          int is_locf = op->par_cov_interp[k];
          if (is_locf == -1) is_locf = op->is_locf;
          if (rx->sample && rx->par_sample[op->par_cov[k]-1] == 1) {
            // Get or sample id from overall ids
            if (ind->cov_sample[k] == 0) {
              // Set inLhs to 1 to make sure uniform is going to be used
              int inLhs = ind->inLhs;
              ind->inLhs = 1;
              ind->cov_sample[k] = floor(rxunif(ind, 0, (double)rx->nsub * rx->nsim))+1;
              ind->inLhs = inLhs;
            }
            indSample = &(rx->subjects[ind->cov_sample[k]-1]);
            // the sampled subject at the data time of this subject's record idx
            ind->par_ptr[op->par_cov[k]-1] =
              rxApproxCovData(getAllTimes(ind, ind->ix[idx]), k, is_locf, op, indSample);
            ind->cacheME=0;
            continue;
          } else {
            indSample = ind;
            idxSample = idx;
          }
          // cov_ptr is laid out by the data records (n_all_times_orig)
          double *y = indSample->cov_ptr + indSample->n_all_times_orig*k;
          if (rxPushedRec(ind->ix, idx, ind->n_all_times_orig)) {
            ind->par_ptr[op->par_cov[k]-1] =
              rxApproxCovData(getAllTimes(ind, ind->ix[idx]), k, is_locf, op, ind);
            ind->cacheME=0;
            continue;
          }
          ind->par_ptr[op->par_cov[k]-1] = rxRecCov(k, idxSample, y, is_locf,
                                                    indSample, op);
          if (idx == 0){
            ind->cacheME=0;
          } else if (!isSameTimeOp(getValue(idxSample, y, is_locf,
                                            indSample, op, 0),
                                   getValue(idxSample-1, y, is_locf,
                                            indSample, op, 0))) {
            ind->cacheME=0;
          }
        }
      }
    }
  } else {
    // Update all covariate parameters
    int k, idxSample;
    int ncov = op->ncov;
    if (op->do_par_cov) {
      for (k = ncov; k--;) {
        if (op->par_cov[k]) {
          int is_locf = op->par_cov_interp[k];
          if (is_locf == -1) is_locf = op->is_locf;
          if (rx->sample && rx->par_sample[op->par_cov[k]-1] == 1) {
            // Get or sample id from overall ids
            if (ind->cov_sample[k] == 0) {
              // Set inLhs to 1 to make sure uniform is going to be used
              int inLhs = ind->inLhs;
              ind->inLhs = 1;
              ind->cov_sample[k] = floor(rxunif(ind, 0.0, (double)rx->nsub * rx->nsim))+1;
              ind->inLhs = inLhs;
            }
            indSample = &(rx->subjects[ind->cov_sample[k]-1]);
            // Use the same methodology as approxfun.  Don't need to reset ME
            // because solver doesn't use the times in-between.
            ind->par_ptr[op->par_cov[k]-1] = rxApproxCovData(t, k, is_locf, op, indSample);
            continue;
          } else {
            indSample = ind;
            idxSample = idx;
          }
          double *par_ptr = ind->par_ptr;
          //double *all_times = indSample->all_times;
          // cov_ptr is laid out by the data records (n_all_times_orig)
          double *y = indSample->cov_ptr + indSample->n_all_times_orig*k;
          // a pushed record has no value of its own; interpolate across it
          bool pushed = rxPushedRec(ind->ix, idxSample, ind->n_all_times_orig);
          if (!pushed && idxSample == 0 &&
              isSameTimeOp(t, (indSample->fns && indSample->fns->gettime ? indSample->fns->gettime(indSample->ix[idxSample], indSample) : getTime(indSample->ix[idxSample], indSample)))) {
            // y is in record order; a lagged dose can move record 0 off sorted slot 0
            par_ptr[op->par_cov[k]-1] = rxRecCov(k, 0, y, is_locf, indSample, op);
            ind->cacheME=0;
          } else if (!pushed && idxSample > 0 && idxSample < indSample->n_all_times &&
                     isSameTimeOp(t, (indSample->fns && indSample->fns->gettime ? indSample->fns->gettime(indSample->ix[idxSample], indSample) : getTime(indSample->ix[idxSample], indSample)))) {
            par_ptr[op->par_cov[k]-1] = rxRecCov(k, idxSample, y, is_locf,
                                                 indSample, op);
            if (!isSameTimeOp(getValue(idxSample, y, is_locf,
                                       indSample, op, 0),
                              getValue(idxSample-1, y, is_locf,
                                       indSample, op, 0))) {
              ind->cacheME=0;
            }
          } else {
            // Use the same methodology as approxfun.
            par_ptr[op->par_cov[k]-1] = rxApproxCov(t, y, is_locf, op, indSample);
            // Don't need to reset ME because solver doesn't use the
            // times in-between.
          }
        }
      }
    }
  }
  ind->_update_par_ptr_in = 0;
}

/* void doSort(rx_solving_options_ind *ind); */
extern "C" void sortInd(rx_solving_options_ind *ind);
