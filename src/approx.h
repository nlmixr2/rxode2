#ifndef __APPROX_H__
#define __APPROX_H__

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
      }

    }
  }
  return ret;
}
#endif 
