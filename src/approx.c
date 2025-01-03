#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <stdio.h>
#include <stdarg.h>
#include "../inst/include/rxode2.h"
#include "../inst/include/rxode2parseHandleEvid.h"
#include "../inst/include/rxode2parseGetTime.h"

#define safe_zero(a) ((a) == 0 ? DBL_EPSILON : (a))
#define _as_zero(a) (fabs(a) < sqrt(DBL_EPSILON) ? 0.0 : a)
#define _as_dbleps(a) (fabs(a) < sqrt(DBL_EPSILON) ? ((a) < 0 ? -sqrt(DBL_EPSILON)  : sqrt(DBL_EPSILON)) : a)

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif
//#include "lincmtB2.h"
//#include "lincmtB3d.h"

void handleTlast(double *time, rx_solving_options_ind *ind);

double rxunif(rx_solving_options_ind* ind, double low, double hi);

// From https://cran.r-project.org/web/packages/Rmpfr/vignettes/log1mexp-note.pdf
double log1mex(double a){
  if (a < M_LN2) return log(-expm1(-a));
  return(log1p(-exp(-a)));
}

extern int _locateTimeIndex(double obs_time,  rx_solving_options_ind *ind){
  // Uses bisection for slightly faster lookup of dose index.
  int i, j, ij;
  i = 0;
  j = ind->n_all_times - 1;
  if (obs_time < getTime(ind->ix[i], ind)){
    return i;
  }
  if (obs_time > getTime(ind->ix[j], ind)){
    return j;
  }
  while(i < j - 1) { /* x[i] <= obs_time <= x[j] */
    ij = (i + j)/2; /* i+1 <= ij <= j-1 */
    if(obs_time < getTime(ind->ix[ij], ind))
      j = ij;
    else
      i = ij;
  }
  /* if (i == 0) return 0; */
  while(i != 0 && obs_time == getTime(ind->ix[i], ind)){
    i--;
  }
  if (i == 0){
    while(i < ind->ndoses-2 && fabs(obs_time  - getTime(ind->ix[i+1], ind))<= sqrt(DBL_EPSILON)){
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
static inline double getValue(int idx, double *y, int is_locf,
                              rx_solving_options_ind *ind, rx_solving_options *op,
                              int lh){
  int i = idx;
  double ret = y[ind->ix[idx]];
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
        i--; ret = y[ind->ix[i]];
      }
      if (ISNA(ret)) {
        // Still not found go forward.
        i = idx;
        while (ISNA(ret) && i != ind->n_all_times-1){
          i++; ret = y[ind->ix[i]];
        }
      }
    } else {
      // Go forward
      while (ISNA(ret) && i != ind->n_all_times-1) {
        i++; ret = y[ind->ix[i]];
      }
      if (ISNA(ret)) {
        // Still not found go backward
        i = idx;
        while (ISNA(ret) && i != 0){
          i--; ret = y[ind->ix[i]];
        }
      }
    }
  }
  if (lh == -2) {
    ind->idxLow = i;
  } else if (lh == 2) {
    ind->idxHi = i;
  }
  return ret;
}
#define T(i) getTime(id->ix[i], id)
#define V(i, lh) getValue(i, y, is_locf, id, Meth, lh)
double rx_approxP(double v, double *y, int is_locf, int n,
                  rx_solving_options *Meth, rx_solving_options_ind *id){
  /* Approximate  y(v),  given (x,y)[i], i = 0,..,n-1 */
  int i, j, ij;

  if(!n) return R_NaN;

  i = 0;
  j = n - 1;

  /* handle out-of-domain points */
  if(v < T(i)) return id->ylow;
  if(v > T(j)) return id->yhigh;

  /* find the correct interval by bisection */
  while(i < j - 1) { /* T(i) <= v <= T(j) */
    ij = (i + j)/2; /* i+1 <= ij <= j-1 */
    if(v < T(ij)) j = ij; else i = ij;
    /* still i < j */
  }
  /* provably have i == j-1 */

  /* interpolation */

  if(v == T(j)) return V(j, 1);
  if(v == T(i)) return V(i, -1);
  /* impossible: if(T(j) == T(i)) return V(i); */

  switch (is_locf) {
  case 0: // linear
    {
      // in the case of linear the time needs to be adjusted based on any na handling rules
      // when i = -2 or i = 2 then the index of the na value adjustment is saved.
      double vi = V(i, -2);
      double vj = V(j, 2);
      // These saved values are then used for the adjusted times
      double ti = T(id->idxLow);
      double tj = T(id->idxHi);
      return vi + (vj - vi) * ((v - ti)/(tj - ti));
    }
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

#undef T
#undef V

/* End approx from R */

// getParCov first(parNo, idx=0) last(parNo, idx=ind->n_all_times-1)
double _getParCov(unsigned int id, rx_solve *rx, int parNo, int idx0){
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
        double *y = ind->cov_ptr + ind->n_all_times*k;
        return y[ind->ix[idx]];
      }
    }
  }
  return ind->par_ptr[parNo];
}

void _update_par_ptr(double t, unsigned int id, rx_solve *rx, int idxIn) {
  if (rx == NULL) Rf_errorcall(R_NilValue, _("solve data is not loaded"));
  rx_solving_options_ind *ind, *indSample;
  ind = &(rx->subjects[id]);
  if (ind->_update_par_ptr_in) return;
  int idx = idxIn;
  rx_solving_options *op = rx->op;
  // handle extra dose, and out of bounds idx values
  if (idx < 0 && ind->extraDoseN[0] > 0) {
    if (-1-idx >= ind->extraDoseN[0]) {
      // Get the last dose index for the extra doses
      idx = -1-ind->extraDoseTimeIdx[ind->extraDoseN[0]-1];
    }
    // extra dose time, find the closest index
    double v = getTime(idxIn, ind);
    int i, j, ij, n = ind->n_all_times;
    i = 0;
    j = n - 1;
    if (v < getTime(ind->ix[i], ind)) {
      idx = i;
    } else if (v > getTime(ind->ix[j], ind)) {
      idx = j;
    } else {
      /* find the correct interval by bisection */
      while(i < j - 1) { /* T(i) <= v <= T(j) */
        ij = (i + j)/2; /* i+1 <= ij <= j-1 */
        if (v < getTime(ind->ix[ij], ind)) {
          j = ij;
        } else  {
          i = ij;
        }
      }
      // Pick best match
      if (isSameTimeOp(v, getTime(ind->ix[j], ind))) {
        idx = j;
      } else if (isSameTimeOp(v, getTime(ind->ix[i], ind))) {
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
              ind->cov_sample[k] = round(rxunif(ind, 0.0, (double)(rx->nsub*rx->nsim)))+1;
            }
            indSample = &(rx->subjects[ind->cov_sample[k]-1]);
            idxSample = -1;
          } else {
            indSample = ind;
            idxSample = idx;
          }
          double *y = indSample->cov_ptr + indSample->n_all_times*k;
          ind->par_ptr[op->par_cov[k]-1] = getValue(idxSample, y, is_locf,
                                                    indSample, op, 0);
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
      for (k = ncov; k--;){
        if (op->par_cov[k]) {
          int is_locf = op->par_cov_interp[k];
          if (is_locf == -1) is_locf = op->is_locf;
          if (rx->sample && rx->par_sample[op->par_cov[k]-1] == 1) {
            // Get or sample id from overall ids
            if (ind->cov_sample[k] == 0) {
              ind->cov_sample[k] = (int)(rxunif(ind, (double)1, (double)(rx->nsub*rx->nsim+1)));
            }
            indSample = &(rx->subjects[ind->cov_sample[k]-1]);
            idxSample = -1;
          } else {
            indSample = ind;
            idxSample = idx;
          }
          double *par_ptr = ind->par_ptr;
          //double *all_times = indSample->all_times;
          double *y = indSample->cov_ptr + indSample->n_all_times*k;
          if (idxSample == 0 &&
              isSameTimeOp(t, getTime(ind->ix[idxSample], indSample))) {
            par_ptr[op->par_cov[k]-1] = y[0];
            ind->cacheME=0;
          } else if (idxSample > 0 && idxSample < indSample->n_all_times &&
                     isSameTimeOp(t, getTime(ind->ix[idxSample], indSample))) {
            par_ptr[op->par_cov[k]-1] = getValue(idxSample, y, is_locf,
                                                 indSample, op, 0);
            if (!isSameTimeOp(getValue(idxSample, y, is_locf,
                                       indSample, op, 0),
                              getValue(idxSample-1, y, is_locf,
                                       indSample, op, 0))) {
              ind->cacheME=0;
            }
          } else {
            // Use the same methodology as approxfun.
            indSample->ylow = getValue(0, y, is_locf,
                                       indSample, op, -1);/* cov_ptr[ind->n_all_times*k]; */
            indSample->yhigh = getValue(indSample->n_all_times-1, y, is_locf,
                                        indSample, op, 1);/* cov_ptr[ind->n_all_times*k+ind->n_all_times-1]; */
            par_ptr[op->par_cov[k]-1] = rx_approxP(t, y, is_locf,
                                                   indSample->n_all_times, op, indSample);
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
void sortInd(rx_solving_options_ind *ind);
