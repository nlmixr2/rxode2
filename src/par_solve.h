// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: nil; -*-
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#ifndef __PAR_SOLVE_H___
#define __PAR_SOLVE_H___

#define isSameTimeOp(xout, xp) (op->stiff == 0 ? isSameTimeDop(xout, xp) : isSameTime(xout, xp))
#ifndef min2
#define min2( a , b )  ( (a) < (b) ? (a) : (b) )
#endif

#if defined(__cplusplus)

extern "C" {
#endif

#include "../inst/include/rxode2.h"
#include "rxThreadData.h"


	void sortInd(rx_solving_options_ind *ind);

  void _setIndPointersByThread(rx_solving_options_ind *ind);

  extern double maxAtolRtolFactor;


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
    // Apply this individual's sticky tolerance factor to the thread-local
    // tolerance arrays.  _setIndPointersByThread() has already reset
    // ind->atol2/rtol2/ssAtol/ssRtol to the per-thread global baseline.
    // Multiplying by tolFactor here means the ODE solver immediately sees
    // the correct loosened tolerances without any further call to
    // atolRtolFactor_().  For most individuals tolFactor == 1.0 (set by
    // setupRxInd()), so this loop costs only a few multiplications.
    if (ind->atol2 != NULL) {
      for (int _i = op->neq; _i--;) {
        ind->atol2[_i]  = min2(ind->atol2[_i]  * ind->tolFactor, maxAtolRtolFactor);
        ind->rtol2[_i]  = min2(ind->rtol2[_i]  * ind->tolFactor, maxAtolRtolFactor);
        ind->ssAtol[_i] = min2(ind->ssAtol[_i] * ind->tolFactor, maxAtolRtolFactor);
        ind->ssRtol[_i] = min2(ind->ssRtol[_i] * ind->tolFactor, maxAtolRtolFactor);
      }
    }
		for (int i=rxLlikSaveSize*op->nLlik; i--;) {
			ind->llikSave[i] = 0.0;
		}
		ind->ixds = ind->idx = ind->_update_par_ptr_in = 0; // reset dosing
    ind->
    ind->nPushedExtra = 0; // reset per-solve evid_() push counter
    ind->n_all_times = ind->n_all_times_orig; // reset n_all_times to original value (in case it was increased for extra doses in a previous solve)
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
      } else if (rx->nMtime && inLhs == 0 && op->neq > 0) {
        // No u_inis but state-dep mtime needs initial values in ind->solve
        memcpy(ind->solve, op->inits, op->neq*sizeof(double));
      }
		}
    // Compute model times using ind->solve (which has user-specified inits after u_inis).
    // ind->solve is always a valid calloc'd pointer, unlike op->inits which may be unset.
    if (rx->nMtime) {
      if (inLhs == 0 || op->neq == 0) {
        // ODE solve pass (inLhs==0) or LHS-only model (neq==0): initialise mtime.
        // Compute mtime with actual initial state → mtime_init[k].
        double *_initState = (inLhs == 0 && op->neq > 0) ? ind->solve : op->inits;
        calc_mtime(solveid, ind->mtime, _initState);

        // Compute mtime with zero state → base (state-independent) time mtime_base[k].
        // If base <= init, place event at base so the solver is forced to visit base,
        // then recomputeMtimeIfNeeded re-evaluates with actual state(base) and reschedules
        // to base + f(state(base)) >= base.  This is the correct semantics: state-dep offset
        // is evaluated at the trigger time, not at t=0.
        // If base > init (e.g. negative initial offset shifts event earlier), keep init
        // so the event fires at the correct earlier time (old behaviour preserved).
        double _baseMtime[90];
        if (op->neq > 0) {
          double *_zeroState = new double[op->neq]();  // zero-initialised
          calc_mtime(solveid, _baseMtime, _zeroState);
          delete[] _zeroState;
        } else {
          calc_mtime(solveid, _baseMtime, _initState);
        }
        std::fill_n(ind->mtime0, rx->nMtime, R_NegInf);
        for (int k = 0; k < rx->nMtime; k++) {
          if (_baseMtime[k] <= ind->mtime[k]) {
            // Event is at or after the base time: place at base so solver visits it.
            ind->mtime[k] = _baseMtime[k];
          }
          // else: event is before base (negative offset); keep mtime_init (old behaviour).
          ind->mtime0[k] = ind->mtime[k];  // trigger = initial placement
        }
      }
      // else: LHS pass (inLhs==1, neq>0) — preserve ind->mtime[k] set by the ODE
      // solve (including any recomputeMtimeIfNeeded updates).  getTime_ returns
      // ind->mtime[evid-10] so using the correct final time is essential for the
      // output dataframe to show the actual event time, not the trigger time.
    }
		ind->_newind = 1;
		ind->dosenum = 0;
		ind->tlast = NA_REAL;
		ind->tfirst = NA_REAL;
		ind->curDose = NA_REAL;
		if (inLhs == 0 || (inLhs == 1 && op->neq==0)) {
			ind->solved = -1;
		}
    if (inLhs == 0 || op->neq == 0) {
      // Sort for solving (inLhs==0), or for LHS-only models (neq==0) where no
      // ODE solver ran.  When inLhs==1 and neq>0 the ODE solve loop already ran
      // sortInd and may have re-sorted for state-dep lag; preserve that order so
      // getSolve(i) positions in rxode2_df agree with the solve loop.
      sortInd(ind);
      // Note: op->badSolve is NOT checked here — it is a shared global flag.
      // In parallel mode another thread's failed solve would prevent THIS
      // individual from initializing.  Bad-solve state is handled per-individual
      // via localBadSolve / *rc in the caller.
    }
    ind->mainSorted = 1;
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
  ind->_atEventTime = 1;
}


#endif

#endif
