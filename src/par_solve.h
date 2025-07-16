// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: nil; -*-
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#ifndef __PAR_SOLVE_H___
#define __PAR_SOLVE_H___

#define isSameTimeOp(xout, xp) (op->stiff == 0 ? isSameTimeDop(xout, xp) : isSameTime(xout, xp))

#if defined(__cplusplus)

extern "C" {
#endif


#include "../inst/include/rxode2.h"
#include "rxThreadData.h"

	void sortInd(rx_solving_options_ind *ind);

  void _setIndPointersByThread(rx_solving_options_ind *ind);

	static inline int iniSubject(int solveid, int inLhs, rx_solving_options_ind *ind, rx_solving_options *op, rx_solve *rx,
															 t_update_inis u_inis) {
		ind->_rxFlag=1;
    ind->linSS=0;
    ind->linSScmt=0;
    ind->linSSvar=0.0;
    ind->linSStau=0.0;
    ind->linSSbolusCmt=0;
    ind->linCmtAlast = NULL;
    ind->ssTime = NA_REAL;
    _setIndPointersByThread(ind);
		for (int i=rxLlikSaveSize*op->nLlik; i--;) {
			ind->llikSave[i] = 0.0;
		}
		ind->ixds = ind->idx = ind->_update_par_ptr_in = 0; // reset dosing
		ind->id=solveid;
		ind->cacheME=0;
		ind->curShift=0.0;
		ind->lastIsSs2 = false;
    ind->idxLow=0;
    ind->idxHi=0;
		// neq[0] = op->neq
    int ncmt = (op->neq + op->extraCmt);
		for (int j = ncmt; j--;) {
			ind->InfusionRate[j] = 0;
			ind->on[j] = 1;
			ind->tlastS[j] = NA_REAL;
			ind->tfirstS[j] = NA_REAL;
			ind->curDoseS[j] = NA_REAL;
		}
		ind->inLhs = inLhs;
		if (rx->nMtime) calc_mtime(solveid, ind->mtime);
		for (int j = op->nlhs; j--;) {
      if (op->lhs_str[j] == 1) {
        ind->lhs[j] = 1.0; // default is first string defined
      } else {
        ind->lhs[j] = NA_REAL;
      }
    }
		if ((inLhs == 0 && op->neq > 0) ||
				(inLhs == 1 && op->neq == 0 && (rx->nIndSim > 0 || (rx->simflg & 1) != 0 ))) {
      if (u_inis != NULL) {
        ind->isIni = 1;
        // Also can update individual random variables (if needed)
        if (inLhs == 0) memcpy(ind->solve, op->inits, op->neq*sizeof(double));
        u_inis(solveid, ind->solve); // Update initial conditions @ current time
        ind->isIni = 0;
      }
		}
		ind->_newind = 1;
		ind->dosenum = 0;
		ind->tlast = NA_REAL;
		ind->tfirst = NA_REAL;
		ind->curDose = NA_REAL;
		if (inLhs == 0 || (inLhs == 1 && op->neq==0)) {
			ind->solved = -1;
		}
    sortInd(ind);
    if (op->badSolve) return 0;
		ind->ixds=ind->idx=0;
    if (ncmt) ind->pendingDosesN[0] = 0;
		return 1;
	}
  static inline void handleEvid3(rx_solving_options_ind *ind, rx_solving_options *op, rx_solve *rx,
                                 int *neq, double *xp, double *xout,  double *yp,
                                 int *idid,
                                 t_update_inis u_inis) {
    ind->curShift -= rx->maxShift;
    for (unsigned int j = neq[0]; j--;) {
      ind->InfusionRate[j] = 0;
      ind->on[j] = 1;
      ind->cacheME=0;
    }
    cancelInfusionsThatHaveStarted(ind, neq[1], *xout);
    cancelPendingDoses(ind, neq[1]);
    memcpy(yp, op->inits, neq[0]*sizeof(double));
    u_inis(neq[1], yp); // Update initial conditions @ current time
    if (rx->istateReset) *idid = 1;
    *xout -= rx->maxShift;
    *xp = *xout;
    ind->linCmtAlast = yp ;
    ind->ixds++;
  }


#if defined(__cplusplus)
}

static inline int handleExtraDose(int *neq,
                                  int *BadDose,
                                  double *InfusionRate,
                                  double *dose,
                                  double *yp,
                                  double xout, double xp, int id,
                                  int *i, int nx,
                                  int *istate,
                                  rx_solving_options *op,
                                  rx_solving_options_ind *ind,
                                  t_update_inis u_inis,
                                  void *ctx) {
  if (ind->extraDoseN[0] > ind->idxExtra) {
    if (ind->extraSorted == 0) {
      // do sort
      SORT(ind->extraDoseTimeIdx + ind->idxExtra, ind->extraDoseTimeIdx + ind->extraDoseN[0],
           [ind](int a, int b){
             double timea = ind->extraDoseTime[a],
               timeb = ind->extraDoseTime[b];
             if (timea == timeb) {
               int evida = ind->extraDoseEvid[a],
                 evidb = ind->extraDoseEvid[b];
               if (evida == evidb){
                 return a < b;
               }
               return evida < evidb;
             }
             return timea < timeb;
           });
      ind->extraSorted=1;
      ind->idxExtra=0;
    }
    // Use "real" xout for handle_evid functions.
    int idx = ind->idx;
    int ixds = ind->ixds;
    int trueIdx = ind->extraDoseTimeIdx[ind->idxExtra];
    ind->idx = -1-trueIdx;
    double time = getAllTimes(ind, ind->idx);
    while (!isSameTimeOp(time, xp) && time < xp && ind->idxExtra < ind->extraDoseN[0]) {
      ind->idxExtra++;
      trueIdx = ind->extraDoseTimeIdx[ind->idxExtra];
      ind->idx = -1-trueIdx;
      time = getAllTimes(ind, ind->idx);
    }
    if ((isSameTimeOp(time, xp) || time > xp) &&
        (isSameTimeOp(time, xout) || time <= xout)) {
      bool ignore = true;
      while (ignore && time <= xout) {
        ignore=false;
        for (int i = 0; i < ind->ignoredDosesN[0]; ++i) {
          int curIdx = ind->ignoredDoses[i];
          if (curIdx < 0 && -1-curIdx == trueIdx) {
            ignore = true;
            break;
          }
        }
        if (ignore) {
          ind->idxExtra++;
          if (ind->idxExtra < ind->extraDoseN[0]) {
            trueIdx = ind->extraDoseTimeIdx[ind->idxExtra];
            ind->idx = -1-trueIdx;
            time = getAllTimes(ind, ind->idx);
          } else {
            ind->idxExtra--;
            break;
          }
        } else {
          break;
        }
      }
      if (ignore) {
        ind->idx = idx;
        ind->ixds = ixds;
        return 0;
      } else {
        ind->extraDoseNewXout = time;
        ind->idx = idx;
        ind->ixds = ixds;
        // REprintf("time: %f; xp: %f; xout: %f; handleExtra\n", time, xp, xout);
        return 1;
      }
    }
    ind->idx = idx;
    ind->ixds = ixds;
    return 0;
  }
  return 0;
}

static inline void preSolve(rx_solving_options *op, rx_solving_options_ind *ind,
                            double &xp, double &xout, double *yp) {
  // First set the last values of time and compartment values
  if (op->numLin > 0) {
    ind->linCmtAlast = yp + op->linOffset;
    ind->tprior = xp + ind->curShift; // Set the time to the time to solve to.
    ind->tout   = xout + ind->curShift;
  }
}


#endif

#endif
