#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <stdio.h>
#include <stdarg.h>
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <R_ext/Rdynload.h>
#include "../inst/include/rxode2.h"
#include <rxode2parseHandleEvid.h>
#include <rxode2parseGetTime.h>
#include "seed.h"
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

// From https://cran.r-project.org/web/packages/Rmpfr/vignettes/log1mexp-note.pdf
double log1mex(double a){
  if (a < M_LN2) return log(-expm1(-a));
  return(log1p(-exp(-a)));
}

void getWh(int evid, int *wh, int *cmt, int *wh100, int *whI, int *wh0);

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
static inline double getValue(int idx, double *y, rx_solving_options_ind *ind, rx_solving_options *op){
  int i = idx;
  double ret = y[ind->ix[idx]];
  if (ISNA(ret)){
    if (op->f2 == 1.0 && op->f1 == 0.0) {
      // use nocb
      // Go forward
      while (ISNA(ret) && i != ind->n_all_times){
        i++; ret = y[ind->ix[i]];
      }
      if (ISNA(ret)){
        // Still not found go backward
        i = idx;
        while (ISNA(ret) && i != 0){
          i--; ret = y[ind->ix[i]];
        }
        if (ISNA(ret)){
          // All Covariates values for a single individual are NA.
          ind->allCovWarn=1;
        }
      }
    } else {
      // Go backward.
      while (ISNA(ret) && i != 0){
        i--; ret = y[ind->ix[i]];
      }
      if (ISNA(ret)){
        // Still not found go forward.
        i = idx;
        while (ISNA(ret) && i != ind->n_all_times){
          i++; ret = y[ind->ix[i]];
        }
        if (ISNA(ret)){
          // All Covariates values for a single individual are NA.
          ind->allCovWarn=1;
        }
      }

    }
  }
  return ret;
}
#define T(i) getTime(id->ix[i], id)
#define V(i) getValue(i, y, id, Meth)
double rx_approxP(double v, double *y, int n,
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

  if(v == T(j)) return V(j);
  if(v == T(i)) return V(i);
  /* impossible: if(T(j) == T(i)) return V(i); */

  if(Meth->kind == 1){ /* linear */
    return V(i) + (V(j) - V(i)) * ((v - T(i))/(T(j) - T(i)));
  } else { /* 2 : constant */
    return (Meth->f1 != 0.0 ? V(i) * Meth->f1 : 0.0)
      + (Meth->f2 != 0.0 ? V(j) * Meth->f2 : 0.0);
  }
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
    if (ind->evid[ind->ix[idx]] == 9) idx++;
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

void _update_par_ptr(double t, unsigned int id, rx_solve *rx, int idx) {
  if (rx == NULL) Rf_errorcall(R_NilValue, _("solve data is not loaded"));
  rx_solving_options_ind *ind, *indSample;
  ind = &(rx->subjects[id]);
  if (ind->_update_par_ptr_in) return;
  ind->_update_par_ptr_in = 1;
  if (ISNA(t)) {
    // functional lag, rate, duration, mtime
    rx_solving_options *op = rx->op;
    // Update all covariate parameters
    int k, idxSample;
    int ncov = op->ncov;
    if (op->do_par_cov){
      for (k = ncov; k--;){
        if (op->par_cov[k]){
          if (rx->sample && rx->par_sample[op->par_cov[k]-1] == 1) {
            // Get or sample id from overall ids
            if (ind->cov_sample[k] == 0) {
              ind->cov_sample[k] = (int)rxodeUnif(ind, (double)1, (double)(rx->nsub*rx->nsim+1));
            }
            indSample = &(rx->subjects[ind->cov_sample[k]-1]);
            idxSample = -1;
          } else {
            indSample = ind;
            idxSample = idx;
          }
          double *y = indSample->cov_ptr + indSample->n_all_times*k;
          ind->par_ptr[op->par_cov[k]-1] = getValue(idxSample, y, indSample, op);
          if (idx == 0){
            ind->cacheME=0;
          } else if (getValue(idxSample, y, indSample, op) != getValue(idxSample-1, y, indSample, op)) {
            ind->cacheME=0;
          }
        }
      }
    }
  } else {
    rx_solving_options *op = rx->op;
    // Update all covariate parameters
    int k, idxSample;
    int ncov = op->ncov;
    if (op->do_par_cov){
      for (k = ncov; k--;){
        if (op->par_cov[k]){
          if (rx->sample && rx->par_sample[op->par_cov[k]-1] == 1) {
            // Get or sample id from overall ids
            if (ind->cov_sample[k] == 0) {
              ind->cov_sample[k] = (int)rxodeUnif(ind, (double)1, (double)(rx->nsub*rx->nsim+1));
            }
            indSample = &(rx->subjects[ind->cov_sample[k]-1]);
            idxSample = -1;
          } else {
            indSample = ind;
            idxSample = idx;
          }
          double *par_ptr = ind->par_ptr;
          double *all_times = indSample->all_times;
          double *y = indSample->cov_ptr + indSample->n_all_times*k;
          if (idxSample == 0 && fabs(t- all_times[idxSample]) < DBL_EPSILON) {
            par_ptr[op->par_cov[k]-1] = y[0];
            ind->cacheME=0;
          } else if (idxSample > 0 && idxSample < indSample->n_all_times && fabs(t- all_times[idxSample]) < DBL_EPSILON) {
            par_ptr[op->par_cov[k]-1] = getValue(idxSample, y, indSample, op);
            if (getValue(idxSample, y, indSample, op) != getValue(idxSample-1, y, indSample, op)) {
              ind->cacheME=0;
            }
          } else {
            // Use the same methodology as approxfun.
            indSample->ylow = getValue(0, y, indSample, op);/* cov_ptr[ind->n_all_times*k]; */
            indSample->yhigh = getValue(indSample->n_all_times-1, y, indSample, op);/* cov_ptr[ind->n_all_times*k+ind->n_all_times-1]; */
            par_ptr[op->par_cov[k]-1] = rx_approxP(t, y, indSample->n_all_times, op, indSample);
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
void sortRadix(rx_solving_options_ind *ind);

