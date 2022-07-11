// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: t; -*-
#ifndef __PAR_SOLVE_H___
#define __PAR_SOLVE_H___

#if defined(__cplusplus)
extern "C" {
#endif


#include "../inst/include/rxode2.h"
#include "rxThreadData.h"

void sortRadix(rx_solving_options_ind *ind);

static inline int iniSubject(int solveid, int inLhs, rx_solving_options_ind *ind, rx_solving_options *op, rx_solve *rx,
                             t_update_inis u_inis) {
	setIndPointersByThread(ind);
	for (int i=9; i--;) {
		ind->llikSave[i] = 0.0;
	}
  ind->ixds = ind->idx = ind->_update_par_ptr_in = 0; // reset dosing
  ind->id=solveid;
  ind->cacheME=0;
  ind->curShift=0.0;
  // neq[0] = op->neq
	for (int j = (op->neq + op->extraCmt); j--;) {
		ind->InfusionRate[j] = 0;
		ind->on[j] = 1;
		ind->tlastS[j] = NA_REAL;
		ind->tfirstS[j] = NA_REAL;
		ind->curDoseS[j] = NA_REAL;
	}
  ind->inLhs = inLhs;
  if (rx->nMtime) calc_mtime(solveid, ind->mtime);
  for (int j = op->nlhs; j--;) ind->lhs[j] = NA_REAL;
  if ((inLhs == 0 && op->neq > 0) ||
      (inLhs == 1 && op->neq == 0 && (rx->nIndSim > 0 || (rx->simflg & 1) != 0 ))) {
    ind->isIni = 1;
    // Also can update individual random variables (if needed)
    if (inLhs == 0) memcpy(ind->solve, op->inits, op->neq*sizeof(double));
    u_inis(solveid, ind->solve); // Update initial conditions @ current time
    ind->isIni = 0;
  }
  ind->_newind = 1;
  ind->dosenum = 0;
  ind->tlast = NA_REAL;
  ind->tfirst = NA_REAL;
  ind->curDose = NA_REAL;
  if (inLhs == 0 || (inLhs == 1 && op->neq==0)) {
    ind->solved = -1;
    if (rx->needSort){
      sortRadix(ind);
      if (op->badSolve) return 0;
    }
  }
  ind->ixds=ind->idx=0;
  return 1;
}

#if defined(__cplusplus)
}
#endif

#endif
