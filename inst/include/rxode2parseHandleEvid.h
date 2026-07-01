// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: nil; -*-
#ifndef __RXODE2PARSEHANDLEVID_H___
#define __RXODE2PARSEHANDLEVID_H___

#include "rxode2parse.h"
#include "rxode2EventTranslate.h"
//#include "rxThreadData.h"
#include "rxode2dataErr.h"
#include "needSortDefines.h"

#if defined(__cplusplus)
#define FLOOR(x) std::floor(x)
extern "C" {
#else
#define FLOOR(x) floor(x)
#endif

#ifndef _isrxode2parse_

  int handle_evidL(int evid, double *yp, double xout, int id, rx_solving_options_ind *ind);
  void handleTlast(double *time, rx_solving_options_ind *ind);
  double _getDur(int l, rx_solving_options_ind *ind, int backward, unsigned int *p);

#else
  #define _getDur _rxode2parse_getDur
  extern rx_solving_options op_global;
  extern rx_solve rx_global;
#endif

#if defined(__cplusplus)
}
#endif

// EVID = 0; Observations
// EVID = 1; is illegal, but converted from NONMEM
// EVID = 2; Non-observation, possibly covariate
// EVID = 3; Reset ODE states to zero; Non-observation event
// EVID = 4; Reset and then dose event;  Illegal
// EVID = 9; Non-observation event to ini system at time zero; This is to set the INIs at the correct place.
// EVID = 10-99; mtime events (from ODE system)
// When EVID > 100
// EVID: ## # ## ##
//       c2 I c1 xx
// c2 = Compartment numbers over 100
//  I = Infusion Flag/ Special event flag
#define EVIDF_NORMAL 0

#define EVIDF_INF_RATE 1
#define EVIDF_INF_DUR  2

#define EVIDF_REPLACE  4
#define EVIDF_MULT     5

#define EVIDF_MODEL_DUR_ON   8
#define EVIDF_MODEL_DUR_OFF  6

#define EVIDF_MODEL_RATE_ON  9
#define EVIDF_MODEL_RATE_OFF 7
//      0 = no Infusion
//      1 = Infusion, AMT=rate (mg/hr for instance)
//      2 = Infusion, duration is fixed
//      4 = Replacement event
//      5 = Multiplication event
//      6 = Turn off modeled duration
//      7 = Turn off modeled rate compartment
//      8 = Duration is modeled, AMT=dose; Rate = AMT/(Modeled Duration) NONMEM RATE=-2
//      9 = Rate is modeled, AMT=dose; Duration = AMT/(Modeled Rate) NONMEM RATE=-1
// c1 = Compartment numbers below 99
// xx =  1, regular event (no lag time)
// xx =  2, An infusion/rate event that doesn't look for start/end of infusion AND does not apply lags
// xx =  8, possibly turn off steady state infusion with lag time (needed in case spans dur)
// xx =  9, steady state event SS=1 with lag time
// xx = 10, steady state event SS=1 (no lag)
// xx = 19, steady state event at dose time (SS=2) with lag
// xx = 20, steady state event + last observed info (not lagged)
// xx = 21, steady state event at dose time (with absorption lag) + last observed info
// xx = 30, Turn off compartment
// xx = 40, Steady state constant infusion
// xx = 50, Phantom event, used for transit compartments
// xx = 60, Dose that does not track as a dose turn on system
// Steady state events need a II data item > 0
#define EVID0_REGULAR  1
#define EVID0_RATEADJ 2
#define EVID0_INFRM 8
#define EVID0_SS0 9
#define EVID0_SS 10
#define EVID0_SS20 19
#define EVID0_SS2 20
#define EVID0_OFF 30
#define EVID0_SSINF 40
#define EVID0_PHANTOM 50
#define EVID0_ONDOSE 60

static inline double getDoseNumber(rx_solving_options_ind *ind, int i) {
  return getDose(ind, ind->idose[i]);
}

static inline double getDoseIndex(rx_solving_options_ind *ind, int i) {
  return (i < 0 ? getDose(ind, i) : getDose(ind, ind->ix[i]));
}

static inline double getDoseIndexPlus1(rx_solving_options_ind *ind, int i) {
  return getDoseP1(ind, ind->ix[i]);
}

static inline double getIiNumber(rx_solving_options_ind *ind, int i) {
  //return ind->ii[i];
  return getIi(ind,ind->idose[i]);
}

static inline void setDoseNumber(rx_solving_options_ind *ind, int i, int j, double value) {
  setDoseP1(ind, ind->idose[i] + j, value)
}

static inline void handleInfusionGetEndOfInfusionIndex(int idx, int *infEixds,
																											 rx_solve *rx,
                                                       rx_solving_options *op,
																											 rx_solving_options_ind *ind) {
	int curEvid = getEvid(ind, ind->idose[idx]);
	double curAmt = getDoseNumber(ind, idx);
	int lastKnownOff = 0;
	*infEixds = -1;
	for (int j = 0; j < ind->ndoses; j++) {
		if (curEvid == getEvid(ind, ind->idose[j]) &&
				curAmt == getDoseNumber(ind, j)) {
			// get the first dose combination
			if (lastKnownOff == 0) {
				lastKnownOff=j+1;
			} else {
				lastKnownOff++;
			}
			for (int k = lastKnownOff; k < ind->ndoses; k++) {
				if (curEvid == getEvid(ind, ind->idose[k]) &&
						curAmt == -getDoseNumber(ind, k)) {
					lastKnownOff = k;
					if (j == idx) {
						*infEixds = k;
						// dur = getTime_(ind->idose[infEixds], ind);
						// dur -= getTime_(ind->idose[ind->ixds+2], ind);
						// dur2 = getIiNumber(ind, ind->ixds) - dur;
					}
					k = ind->ndoses;
				}
			}
		}
		if (*infEixds != -1) break;
	}
}

static inline int handleTlastInlineUpateDosingInformation(rx_solving_options_ind *ind, double *curDose, double *tinf) {
  unsigned int p;
  switch (ind->whI) {
  case EVIDF_MODEL_RATE_ON: // modeled rate.
  case EVIDF_MODEL_DUR_ON: // modeled duration.
    // Rate already calculated and saved in the next dose record
    // InfusionRate[cmt] -= getDoseIndexPlus1(ind, ind->idx);
    *tinf = getAllTimesP1(ind, ind->idx) - getAllTimes(ind, ind->idx);
    return 1;
    break;
  case EVIDF_MODEL_RATE_OFF: // End modeled rate
  case EVIDF_MODEL_DUR_OFF: // end modeled duration
    return 0;
    break;
  case EVIDF_INF_DUR:
  case EVIDF_INF_RATE:
    if (curDose[0] <= 0) {
      return 0;
    } else {
      // The amt in rxode2 is the infusion rate, but we need the amt
      if (ind->fns->getdur) tinf[0] = ind->fns->getdur(ind->ixds, ind, 2, &p);
      else tinf[0] = _getDur(ind->ixds, ind, 2, &p);
      if (!ISNA(tinf[0])) {
        curDose[0] = tinf[0] * curDose[0];
        return 1;
      } else {
        return 0;
      }
    }
    break;
  }
  if (ind->wh0 == EVID0_ONDOSE) {
    // evid=2 can add zero dose to turn on a compartment;
    // therefore if the current dose is zero and the next evid = 2
    // don't treat it as a new dose.
    return 0;
  }
  return 1;
}

static inline void handleTlastInline(double *time, rx_solving_options_ind *ind) {
  rx_solving_options *op = &op_global;
  double _time = *time + ind->curShift;
  int evid = 0;
  if (ind->idx < 0) {
    evid = getEvid(ind, ind->idx);
  } else {
    evid = getEvid(ind, ind->ix[ind->idx]);
  }
  if (op->neq + op->extraCmt != 0 &&
      isDose(evid) &&
      ind->cmt >= 0 &&
      ind->cmt < op->neq + op->extraCmt &&
      ind->tlastS[ind->cmt] != _time) {
    double curDose = getDoseIndex(ind, ind->idx), tinf = NA_REAL;
    if (handleTlastInlineUpateDosingInformation(ind, &curDose, &tinf) == 0) return;
    ind->dosenum++;
    ind->tlast = _time;
    ind->curDose = curDose;
    ind->curDoseS[ind->cmt] = ind->curDose;
    if (ISNA(ind->tfirst)) ind->tfirst = _time;
    ind->tlastS[ind->cmt] = _time;
    if (ISNA(ind->tfirstS[ind->cmt])) ind->tfirstS[ind->cmt] = _time;
  }
}

static inline int getDoseNumberFromIndex(rx_solving_options_ind *ind, int idx) {
  // bisection https://en.wikipedia.org/wiki/Binary_search_algorithm
  int l = 0, r = ind->ndoses-1, m=0, idose = 0;
  while(l <= r){
    m = FLOOR((l+r)/2);
    idose= ind->idose[m];
    if (idose < idx) l = m+1;
    else if (idose > idx) r = m-1;
    else return m;
  }
  return -1;
}


static inline int syncIdx(rx_solving_options_ind *ind) {
  if (ind->idx < 0) return 1; // additional dose; technically the idx doesn't relate to idose/ix
  // ind->ixds can be advanced past the last dose: handle_evid() does an
  // unconditional ind->ixds++ after handling a dose, and handleEvid1() ignores
  // syncIdx()'s return value, so a failed re-sync can leave ixds == ndoses and
  // the next dose pushes it past the end.  idose holds ndoses entries (+1 guard
  // slot), so dereferencing idose[ixds] when ixds >= ndoses reads out of bounds
  // (ASAN: heap-buffer-overflow in syncIdx).  Treat an out-of-range ixds as
  // "needs re-sync" instead of reading past the array.
  if (ind->ixds < 0 || ind->ixds >= ind->ndoses ||
      ind->ix[ind->idx] != ind->idose[ind->ixds]) {
    // bisection https://en.wikipedia.org/wiki/Binary_search_algorithm
    int m = getDoseNumberFromIndex(ind, ind->ix[ind->idx]);
    if (m != -1) {
      // REprintf("sync to %d ind->ixds from %d to %d #1 (ind->idx: %d)\n", getEvid(ind, ind->idose[m]),
      //          ind->ixds, m, ind->idx);
      ind->ixds=m;
    } else {
      //262144
      if (!(ind->err & rxErrSync)){
        ind->err += rxErrSync;
      }
      return 0;
    }
    // Need to adjust ixdsr
    for(int j = ind->ixds; j--;){
      if (ind->ix[ind->idx] == ind->idose[j]){
        ind->ixds = j;
        break;
      }
    }
    if (ind->ix[ind->idx] != ind->idose[ind->ixds]){
      for(int j = ind->ixds+1; j< ind->ndoses; j++){
        if (ind->ix[ind->idx] == ind->idose[j]){
          ind->ixds = j;
          break;
        }
      }
    }
    if (ind->ix[ind->idx] != ind->idose[ind->ixds]){
      //524288
      if (!(ind->err & rxErrSync2)){
        ind->err += rxErrSync2;
      }
      return 0;
    }
  }
  return 1;
}

static inline double getAmt(rx_solving_options_ind *ind, int id, int cmt,
                            double dose, double t, double *y) {
  if (ind->fns->f == NULL) return dose;
  double ret = ind->fns->f(id, cmt, dose, t, y);
  if (ISNA(ret)){
    rx_solving_options *op = (ind->op ? ind->op : &op_global);
    int newBadSolve = 1;
#pragma omp atomic write
    op->badSolve = newBadSolve;
    int newNaTime = 5 + 10*cmt;
#pragma omp critical
    { if (op->naTime == 0) op->naTime = newNaTime; }
  }
  return ret;
}

static inline int isIgnoredDose(rx_solving_options_ind *ind, int ixds) {
  for (int i = 0; i < ind->ignoredDosesN[0]; ++i) {
    if (ind->idx < 0 ) {
      return 0;
    } else if (ind->idx >= 0 && ind->ignoredDoses[i] == ixds) {
      return 1;
    }
  }
  return 0;
}

static inline int pushIgnoredDose(int doseIdx, rx_solving_options_ind *ind) {
  int re = 0;
  for (int i = 0; i < ind->ignoredDosesN[0]; ++i) {
    if (ind->ignoredDoses[i] == doseIdx) return 0;
  }
  if (ind->ignoredDosesN[0]+1 >= ind->ignoredDosesAllocN[0]) {
    rx_solving_options *op = (ind->op ? ind->op : &op_global);
    int allocFailed = 0;
#pragma omp critical
    {
      int *tmpI = (int*)realloc(ind->ignoredDoses, (ind->ignoredDosesN[0]+1+EVID_EXTRA_SIZE)*sizeof(int));
      if (tmpI == NULL) {
        allocFailed = 1;
      } else {
        ind->ignoredDoses = tmpI;
        ind->ignoredDosesAllocN[0] = (ind->ignoredDosesN[0]+1+EVID_EXTRA_SIZE);
        re = 1;
      }
    }
    if (allocFailed) {
#pragma omp atomic write
      op->badSolve = 1;
      return 0;
    }
  }
  ind->ignoredDoses[ind->ignoredDosesN[0]] = doseIdx;
  ind->ignoredDosesN[0] = ind->ignoredDosesN[0]+1;
  return re;
}

static inline int pushPendingDose(int doseIdx, rx_solving_options_ind *ind) {
  int re = 0;
  if (ind->pendingDosesN[0]+1 >= ind->pendingDosesAllocN[0]) {
    rx_solving_options *op = (ind->op ? ind->op : &op_global);
    int allocFailed = 0;
#pragma omp critical
    {
      int *tmpI = (int*)realloc(ind->pendingDoses, (ind->pendingDosesN[0]+1+EVID_EXTRA_SIZE)*sizeof(int));
      if (tmpI == NULL) {
        allocFailed = 1;
      } else {
        ind->pendingDoses = tmpI;
        ind->pendingDosesAllocN[0] = (ind->pendingDosesN[0]+1+EVID_EXTRA_SIZE);
        re = 1;
      }
    }
    if (allocFailed) {
#pragma omp atomic write
      op->badSolve = 1;
      return 0;
    }
  }
  ind->pendingDoses[ind->pendingDosesN[0]] = doseIdx;
  ind->pendingDosesN[0] = ind->pendingDosesN[0]+1;
  return re;
}


static inline int pushDosingEvent(double time, double amt, int evid,
                                   rx_solving_options_ind *ind) {
  int re = 0;
  if (ind->extraDoseN[0]+1 >= ind->extraDoseAllocN[0]) {
    rx_solving_options *op = (ind->op ? ind->op : &op_global);
    int allocFailed = 0;  // 0=ok, 1=partial alloc, -1=first alloc failed
#pragma omp critical
    {
      int *tmpI = (int*)realloc(ind->extraDoseTimeIdx, (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE)*sizeof(int));
      if (tmpI == NULL) {
        allocFailed = -1;
      } else {
        ind->extraDoseTimeIdx = tmpI;

        tmpI = (int*)realloc(ind->extraDoseEvid, (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE)*sizeof(int));
        if (tmpI == NULL) {
          allocFailed = 1;
        } else {
          ind->extraDoseEvid = tmpI;
          double * tmpD = (double*)realloc(ind->extraDoseTime, (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE)*sizeof(double));
          if (tmpD == NULL) {
            allocFailed = 1;
          } else {
            ind->extraDoseTime = tmpD;

            tmpD = (double*)realloc(ind->extraDoseDose,  (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE)*sizeof(double));
            if (tmpD == NULL) {
              allocFailed = 1;
            } else {
              ind->extraDoseDose = tmpD;

              ind->extraDoseAllocN[0] = (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE);
              re = 1;
            }
          }
        }
      }
    }
    if (allocFailed != 0) {
      int newBadSolve = 1;
#pragma omp atomic write
      op->badSolve = newBadSolve;
      return (allocFailed == 1) ? 1 : 0;
    }
  }
  ind->extraDoseTimeIdx[ind->extraDoseN[0]] = ind->extraDoseN[0];
  ind->extraDoseTime[ind->extraDoseN[0]] = time;
  ind->extraDoseDose[ind->extraDoseN[0]] = amt;
  ind->extraDoseEvid[ind->extraDoseN[0]] = evid;
  pushPendingDose(-1-ind->extraDoseTimeIdx[ind->extraDoseN[0]], ind);
  ind->extraDoseN[0] = ind->extraDoseN[0]+1;
  ind->extraSorted = 0;
  return re;
}

static inline int pushUniqueDosingEvent(double time, double amt, int evid,
                                        rx_solving_options_ind *ind) {
  int re = 0;
  if (ind->extraDoseN[0]+1 >= ind->extraDoseAllocN[0]) {
    rx_solving_options *op = (ind->op ? ind->op : &op_global);
    int allocFailed = 0;  // 0=ok, 1=partial alloc, -1=first alloc failed
#pragma omp critical
    {
      int *tmpI = (int*)realloc(ind->extraDoseTimeIdx, (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE)*sizeof(int));
      if (tmpI == NULL) {
        allocFailed = -1;
      }  else {
        ind->extraDoseTimeIdx = tmpI;

        tmpI = (int*)realloc(ind->extraDoseEvid, (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE)*sizeof(int));
        if (tmpI == NULL) {
          allocFailed = 1;
        } else {
          ind->extraDoseEvid = tmpI;

          double * tmpD = (double*)realloc(ind->extraDoseTime, (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE)*sizeof(double));
          if (tmpD == NULL) {
            allocFailed = 1;
          } else {
            ind->extraDoseTime = tmpD;

            tmpD = (double*)realloc(ind->extraDoseDose,  (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE)*sizeof(double));
            if (tmpD == NULL) {
              allocFailed = 1;
            } else {
              ind->extraDoseDose = tmpD;

              ind->extraDoseAllocN[0] = (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE);
            }
          }
        }
      }
    }
    if (allocFailed != 0) {
      int newBadSolve = 1;
#pragma omp atomic write
      op->badSolve = newBadSolve;
      return (allocFailed == 1) ? 1 : 0;
    }
    re = 1;
  }
  for (int i = 0; i < ind->extraDoseN[0]; ++i) {
    if (ind->extraDoseTime[i] == time &&
        ind->extraDoseDose[i] == amt &&
        ind->extraDoseEvid[i] == evid) {
      // found return early
      return re;
    }
  }
  ind->extraDoseTimeIdx[ind->extraDoseN[0]] = ind->extraDoseN[0];
  ind->extraDoseTime[ind->extraDoseN[0]] = time;
  ind->extraDoseDose[ind->extraDoseN[0]] = amt;
  ind->extraDoseEvid[ind->extraDoseN[0]] = evid;
  pushPendingDose(-1-ind->extraDoseTimeIdx[ind->extraDoseN[0]], ind);
  ind->extraDoseN[0] = ind->extraDoseN[0]+1;
  ind->extraSorted = 0;
  return re;
}

// Forward declarations: defined in rxode2parseGetTime.h (included after this header)
static inline void updateRate(int idx, rx_solving_options_ind *ind, double *yp);
static inline void updateDur(int idx, rx_solving_options_ind *ind, double *yp);

// Event ("jump") sensitivities: physical Jacobian column J[.,cmt] and
// f_cmt = dydt(pre-event state)[cmt], needed by the dtau (event-time) jump
// rows in the paper's replace/additive/multiplicative tables.  Source
// depends on model type (see _rxEsUseCalcJac, set from mv$indLin):
//   - matExp()/indLin() models: dydt() is a no-op stub (matrix-exponential
//     primal solve, not RHS evaluation), so both come from calc_jac -- the
//     column directly, and f_cmt from the identity dX/dt = A*X (exact for
//     the reconstructed matExp system) via row `cmt` of the Jacobian dotted
//     with the current state.
//   - ordinary ODE models: calc_jac is normally an empty stub (only
//     populated by explicit user-written df/dy lines) but dydt is fully
//     functional, so the column comes from a central difference of dydt and
//     f_cmt from the average of the two perturbed evaluations already
//     computed for it (accurate to O(eps^2), avoids a third dydt call).
// `_esJcol` (size >= ns) and `*_esFc` are left untouched if neither source
// is available (caller must pre-zero/guard on the relevant function pointer).
static inline void _esJacColF(int id, double xout, double *yp, int cmt, int ns,
                              int neq, double *_esJcol, double *_esFc) {
  int _esNj[2]; _esNj[0] = neq; _esNj[1] = id;
  if (_rxEsUseCalcJac) {
    double *_esPD = (double*) calloc((size_t)ns * ns, sizeof(double));
    if (_esPD != NULL) {
      calc_jac(_esNj, xout, yp, _esPD, (unsigned int) ns);
      double _f = 0.0;
      for (int _k = 0; _k < ns; _k++) {
        _esJcol[_k] = _esPD[_k * ns + cmt];
        _f += _esPD[cmt * ns + _k] * yp[_k];
      }
      *_esFc = _f;
      free(_esPD);
    }
  } else if (dydtEs != NULL) {
    double *_esF0 = (double*) calloc((size_t)neq, sizeof(double));
    double *_esF1 = (double*) calloc((size_t)neq, sizeof(double));
    if (_esF0 != NULL && _esF1 != NULL) {
      double _esXc = yp[cmt];
      double _esAx = _esXc < 0 ? -_esXc : _esXc;
      double _esEps = 6e-6 * (_esAx > 1.0 ? _esAx : 1.0);
      yp[cmt] = _esXc + _esEps; dydtEs(_esNj, xout, yp, _esF1);
      yp[cmt] = _esXc - _esEps; dydtEs(_esNj, xout, yp, _esF0);
      yp[cmt] = _esXc; // restore pre-event state
      double _esInv = 1.0 / (2.0 * _esEps);
      for (int _k = 0; _k < ns; _k++) {
        _esJcol[_k] = (_esF1[_k] - _esF0[_k]) * _esInv;
      }
      *_esFc = 0.5 * (_esF0[cmt] + _esF1[cmt]);
    }
    if (_esF0 != NULL) free(_esF0);
    if (_esF1 != NULL) free(_esF1);
  }
}

// Is a physical Jacobian column source available for this model (see
// _esJacColF())?  Mirrors the guard the additive-bolus dtau row already used.
static inline int _esHaveJacCol(void) {
  return _rxEsUseCalcJac ? (calc_jac != NULL) : (dydtEs != NULL);
}

// Second-order infusion-boundary jump (Phase F/H1's remaining infusion gap):
// a fixed-rate/duration infusion whose START or STOP time is shifted by a
// modeled alag jumps InfusionRate[cmt] by a FIXED (p,q-independent) amount
// `dtmp` at the lag-shifted boundary time. The validated 1st-order formula
// (see EVIDF_INF_DUR/EVIDF_INF_RATE/EVIDF_MODEL_RATE_ON etc.) is
// `[S^p]_cmt(tau) = -dtmp*dLag_p[cmt]` -- ONLY the DOSED compartment jumps
// in VALUE (unlike the additive-bolus dtau row, a forcing-jump does not
// couple to other states through the physical Jacobian at the instant of
// the jump: S^p_k for k != cmt stays continuous, picking up only a KINK --
// a jump in its own time-derivative, not its value -- since
// dS^p_k/dt = sum_l J[k][l]*S^p_l depends on the now-discontinuous S^p_cmt).
//
// Applying the same "jump condition one level up" derivation used for the
// additive-bolus dtau row (treating S^p_cmt itself as the jumping quantity)
// gives, for the FIXED-forcing-jump case, restricted to k=cmt:
//   [S^{p,q}]_cmt(tau) = -dtmp*d2Lag[p][q][cmt]                (product rule)
//                        - dLag_q[cmt] * (g+_cmt(tau) - g-_cmt(tau))  (Leibniz)
// where g_k = dS^p_k/dt (the RHS of S^p_k's own sensitivity ODE).
//
// CRITICAL EXTRA PIECE (found by FD on a depot-infused/central-observed
// model, i.e. cmt coupled to OTHER states -- every earlier isolated test
// happened to observe the SAME compartment being infused, which hid this):
// even though S^p_k (k != cmt) has NO value jump at 1st order, its SECOND
// derivative wrt q at FIXED time DOES pick up a Leibniz-style value jump,
// by the same "kink whose location moves with q" mechanism applied to
// dS^p_k/dt's OWN jump (proportional to the cmt jump, via the k-th row of
// the Jacobian) instead of to S^p_cmt directly:
//   [S^{p,q}]_k(tau) += -dLag_q[cmt] * (g+_k(tau) - g-_k(tau))   for ALL k
// (no product-rule term for k != cmt: dtmp/Delta only ever affects cmt
// directly, so there is nothing else to differentiate for those rows).
//
// `dydtPre` must be captured by the caller BEFORE dtmp was applied to
// InfusionRate[cmt] and before the 1st-order jump was applied to yp; this
// function reads the CURRENT (already-updated) yp/InfusionRate for g+, so no
// scratch-copy is needed here (unlike the additive-bolus case, InfusionRate
// is not part of yp itself).  ODE models only (`!_rxEsUseCalcJac`) for now --
// matExp needs the same compartment-order fix the additive-bolus dtau row
// required; not yet extended here.
static inline void _esInfusionBoundary2ndOrder(int id, double xout, double *yp,
                                               int cmt, int ns, int neq,
                                               double dtmp, double *dydtPre) {
  if (_rxEsUseCalcJac || dydtEs == NULL || dydtPre == NULL) return;
  if (d2LagEs == NULL || dLagQEs == NULL) return;
  int np = _rxEsNParam, np2 = _rxEsNParam2;
  if (np2 <= 0) return;
  if (ns * (1 + np) + ns * np * np2 > neq) return;
  double *_d2LagB = (double*) calloc((size_t)ns * np * np2, sizeof(double));
  double *_dLagQB = (double*) calloc((size_t)ns * np2, sizeof(double));
  double *_dydtPost = (double*) calloc((size_t)neq, sizeof(double));
  if (_d2LagB != NULL && _dLagQB != NULL && _dydtPost != NULL) {
    d2LagEs(id, xout, yp, _d2LagB);
    dLagQEs(id, xout, yp, _dLagQB);
    int _esNj[2]; _esNj[0] = neq; _esNj[1] = id;
    dydtEs(_esNj, xout, yp, _dydtPost);
    for (int _p = 0; _p < np; _p++) {
      for (int _q = 0; _q < np2; _q++) {
        double _d2LagPQ = _d2LagB[cmt * (np * np2) + _p * np2 + _q];
        double _dLagQq = _dLagQB[cmt * np2 + _q];
        int _c2cmt = ns * (1 + np) + cmt + ns * (_p + _q * np);
        yp[_c2cmt] += -dtmp * _d2LagPQ;
        if (_dLagQq != 0.0) {
          for (int _esK = 0; _esK < ns; _esK++) {
            double _gpost = _dydtPost[ns + _p * ns + _esK];
            double _gpre = dydtPre[ns + _p * ns + _esK];
            int _c2 = ns * (1 + np) + _esK + ns * (_p + _q * np);
            yp[_c2] += -(_gpost - _gpre) * _dLagQq;
          }
        }
      }
    }
  }
  if (_d2LagB != NULL) free(_d2LagB);
  if (_dLagQB != NULL) free(_dLagQB);
  if (_dydtPost != NULL) free(_dydtPost);
}

// Captures dS^p_./dt (via dydtEs) BEFORE an infusion-boundary event mutates
// InfusionRate/yp, for `_esInfusionBoundary2ndOrder()`'s Leibniz term.  ODE
// models only, mirroring `_esInfusionBoundary2ndOrder()`'s own scoping.
static inline double *_esInfusionDydtPre(int id, double xout, double *yp, int neq) {
  if (_rxEsUseCalcJac || dydtEs == NULL) return NULL;
  double *_pre = (double*) calloc((size_t)neq, sizeof(double));
  if (_pre != NULL) {
    int _esNj[2]; _esNj[0] = neq; _esNj[1] = id;
    dydtEs(_esNj, xout, yp, _pre);
  }
  return _pre;
}

static inline int handle_evid(int evid, int neq,
                              int *BadDose,
                              double *InfusionRate,
                              double *dose,
                              double *yp,
                              double xout, int id,
                              rx_solving_options_ind *ind) {
  if (isObs(evid)) return 0;
  if (isIgnoredDose(ind, ind->ixds)) {
    // REprintf("ignored evid %d dose at time %f is value %f (ind->ixds: %d: ind->idx: %d)\n",
    //          evid, xout, getDoseIndex(ind, ind->idx), ind->ixds, ind->idx);
    ind->ixds++;
    ind->solved = ind->idx;
    return 0;
  } // else if (!ind->doSS) {
  //   rx_solving_options *op = &op_global;
  //   REprintf("handle evid[%d] %d dose at time %f is value %f (ind->ixds: %d; ind->idx: %d; id: %d)\n",
  //            op->numLin, evid, xout,
  //            getDoseIndex(ind, ind->idx), ind->ixds, ind->idx, ind->id);
  // }
  int cmt, foundBad, j;
  double tmp;
  getWh(evid, &(ind->wh), &(ind->cmt), &(ind->wh100), &(ind->whI), &(ind->wh0));
  handleTlastInline(&xout, ind);
  if (ind->wh0 == EVID0_SSINF) {
    ind->ixds++;
    ind->solved = ind->idx;
    return 1;
  }
  /* wh100 = ind->wh100; */
  cmt = ind->cmt;
  if (cmt<0) {
    if (!(ind->err & rxErrNegCmt)) {
      ind->err += rxErrNegCmt;
      /* Rprintf("Supplied an invalid EVID (EVID=%d; cmt %d)", evid, cmt); */
    }
    return 0;
  }
  if (cmt >= neq) {
    foundBad = 0;
    for (j = 0; j < ind->nBadDose; j++) {
      if (BadDose[j] == cmt+1) {
        foundBad=1;
        break;
      }
    }
    if (!foundBad) {
      BadDose[ind->nBadDose]=cmt+1;
      ind->nBadDose++;
    }
  } else {
    rx_solving_options *op = (ind->op ? ind->op : &op_global);
    //if (syncIdx(ind) == 0) return 0;
    if (ind->wh0 == EVID0_OFF) {
      yp[cmt]=op->inits[cmt];
      InfusionRate[cmt] = 0;
      ind->cacheME=0;
      ind->on[cmt] = 0;
      return 1;
    }
    if (!ind->doSS && (ind->wh0 == EVID0_SS2 || ind->wh0 == EVID0_SS20) &&
        cmt < op->neq) {
      // Save for adding at the end; Only for ODE systems
      memcpy(ind->solveSave, yp, op->neq*sizeof(double));
    }
    switch(ind->whI) {
    case EVIDF_MODEL_RATE_ON: // modeled rate.
    case EVIDF_MODEL_DUR_ON: // modeled duration.
      // Rate already calculated and saved in the next dose record
      if (ind->wh0 != EVID0_SS0 &&
          ind->wh0 != EVID0_SS20) {
        // Recompute with actual state for state-dependent rate/duration.
        // ind->idx is an ix-array index; updateRate/updateDur need the direct
        // all_times/dose index.  For extra-dose events (ind->idx < 0) the
        // negative value IS the direct index.
        {
          rx_solve *rx = (ind->rx ? ind->rx : &rx_global);
          int _directIdx = (ind->idx >= 0) ? ind->ix[ind->idx] : ind->idx;
          if (ind->whI == EVIDF_MODEL_RATE_ON && (rx->needSort & needSortRate)) {
            updateRate(_directIdx, ind, yp);
            ind->extraSorted = 0;
            if (ind->idx >= 0) ind->mainSorted = 0;  // flag main timeline re-sort
          } else if (ind->whI == EVIDF_MODEL_DUR_ON && (rx->needSort & needSortDur)) {
            updateDur(_directIdx, ind, yp);
            ind->extraSorted = 0;
            if (ind->idx >= 0) ind->mainSorted = 0;  // flag main timeline re-sort
          }
        }
        ind->on[cmt] = 1;
        ind->cacheME = 0;
        tmp = getDoseIndexPlus1(ind, ind->idx);
        if (ind->whI == EVIDF_MODEL_DUR_ON && tmp == 0.0) {
          yp[cmt] += getAmt(ind, id, cmt, getDoseIndex(ind, ind->idx), xout, yp);
          break;
        }
        double *_esDydtPreB1 = _esInfusionDydtPre(id, xout, yp, neq);
        InfusionRate[cmt] -= tmp;
        // Modeled rate/duration infusion moving boundary from a modeled lag.  The
        // start time tau1 = t0 + alag shifts by d(alag)/dp; the forcing change
        // here is -tmp, so the sensitivity jumps by [S] = -(-tmp)*d(alag)/dp =
        // tmp*d(alag)/dp.  (Applies to both MODEL_RATE_ON and MODEL_DUR_ON.)
        if (_rxEsActive && _rxEsNParam > 0 && cmt < _rxEsNState && _rxEsNState * (1 + _rxEsNParam) <= neq && dLagEs != NULL) {
          int _ns = _rxEsNState, _np = _rxEsNParam;
          double *_eA = (double*) calloc((size_t)_ns * _np, sizeof(double));
          if (_eA != NULL) {
            dLagEs(id, xout, yp, _eA);
            for (int _p = 0; _p < _np; _p++) {
              yp[_ns + _p * _ns + cmt] += tmp * _eA[cmt * _np + _p];
            }
            free(_eA);
          }
          // Second-order moving-boundary term (product rule + Leibniz): the
          // actual InfusionRate[cmt] CHANGE here is -tmp (see the assignment
          // just above), matching this case's own 1st-order sign convention.
          // (An earlier naive attempt reusing just the product-rule piece,
          // without the Leibniz term, was found incorrect by FD -- see the
          // additive-bolus dtau row's own note and
          // _esInfusionBoundary2ndOrder()'s derivation comment.)
          _esInfusionBoundary2ndOrder(id, xout, yp, cmt, _ns, neq, -tmp, _esDydtPreB1);
        }
        if (_esDydtPreB1 != NULL) free(_esDydtPreB1);
        // Event ("jump") sensitivities -- modeled rate continuous forcing.
        // The infusion rate is a solver-applied forcing (not a symbolic term in
        // f), so the symbolic sensitivity ODE misses d(rate)/dp.  Mirror the same
        // InfusionRate operation on each sensitivity compartment using d(rate)/dp
        // so the sens ODE picks up the forcing over [tau1, tau2].
        if (_rxEsActive && _rxEsNParam > 0 && cmt < _rxEsNState && _rxEsNState * (1 + _rxEsNParam) <= neq &&
            ind->whI == EVIDF_MODEL_RATE_ON && dRateEs != NULL) {
          int _ns = _rxEsNState, _np = _rxEsNParam;
          double *_esB = (double*) calloc((size_t)_ns * _np, sizeof(double));
          if (_esB != NULL) {
            dRateEs(id, xout, yp, _esB);
            // Physical `InfusionRate[cmt] -= tmp` starts the infusion (the stored
            // rate `tmp` is negative); the forcing the state gains is +rate, so
            // the sensitivity compartment gains +d(rate)/dp.
            for (int _p = 0; _p < _np; _p++) {
              InfusionRate[_ns + _p * _ns + cmt] += _esB[cmt * _np + _p];
            }
            free(_esB);
          }
          // NOTE (2026-06-30): a naive second-order extension
          // (InfusionRate[2nd-order-cmt] += d2Rate[p][q] over the window)
          // was tried and found INCORRECT by FD after the infusion ends --
          // matches to machine precision *during* [tau1,tau2] but diverges
          // sharply afterward. Root cause: differentiating a forcing
          // integrated over a parameter-dependent window [tau1,tau2(p)] a
          // SECOND time picks up a genuine Leibniz-rule boundary-delta term
          // (~dRate(tau2)*d(tau2)/dq) that the first-order construction
          // never needed (the well-known, FD-validated 1st-order asymmetry
          // "modeled RATE needs no explicit boundary term, unlike modeled
          // DUR" does not survive a second differentiation -- tau2 depends
          // on the parameter here too, via tau2=tau1+amt/rate(p)). Deferred
          // pending a proper derivation; reverted rather than left in as a
          // silently-wrong result valid only inside the infusion window.
        }
        // Modeled duration continuous forcing.  rate = F*amt/dur, so
        //   d(rate)/dp = (amt/dur)*dF/dp - (rate/dur)*d(dur)/dp
        //             = (amt*dF + tmp*dDur)/dur     (tmp = -rate = stored value).
        // The dF term covers a parameter-dependent bioavailability on the
        // modeled-duration infusion; dDur covers a parameter-dependent duration.
        //
        // NOTE (2026-06-30): the analogous SECOND-order piece is deliberately
        // NOT implemented here yet.  Unlike the boundary/modeled-rate terms
        // above (self-contained: tmp*d2Lag[p][q] / d2Rate[p][q] directly),
        // differentiating this quotient (rate=F*amt/dur) a second time wrt q
        // via the quotient rule needs d(dur)/dq and d(F)/dq -- i.e. the
        // FIRST-order dDur/dF value at q's index in the *first-order*
        // (calcSens) parameter list, which is a DIFFERENT index than q's
        // position in the calcSens2 list this runtime code has (nParam2-
        // indexed).  That cross-order index map (calcSens2 position ->
        // calcSens position, for the SAME shared parameter name) is not
        // currently threaded from R to the runtime; adding it is a small,
        // well-scoped follow-up (see the event-sensitivities plan / project
        // memory) rather than something to approximate silently wrong here.
        if (_rxEsActive && _rxEsNParam > 0 && cmt < _rxEsNState && _rxEsNState * (1 + _rxEsNParam) <= neq &&
            ind->whI == EVIDF_MODEL_DUR_ON && dDurEs != NULL && durEsFn != NULL) {
          int _ns = _rxEsNState, _np = _rxEsNParam;
          double _esAmt = getDoseIndex(ind, ind->idx);
          double _esDur = durEsFn(id, cmt, _esAmt, xout, yp);
          if (_esDur != 0.0) {
            double *_esB = (double*) calloc((size_t)_ns * _np, sizeof(double));
            double *_esFB = (double*) calloc((size_t)_ns * _np, sizeof(double));
            if (_esB != NULL && _esFB != NULL) {
              dDurEs(id, xout, yp, _esB);
              if (dF != NULL) dF(id, xout, yp, _esFB);
              for (int _p = 0; _p < _np; _p++) {
                InfusionRate[_ns + _p * _ns + cmt] +=
                  (_esAmt * _esFB[cmt * _np + _p] + tmp * _esB[cmt * _np + _p]) / _esDur;
              }
            }
            if (_esB != NULL) free(_esB);
            if (_esFB != NULL) free(_esFB);
          }
        }
        if (ind->wh0 == EVID0_SS2 &&
            getAmt(ind, id, cmt, getDoseIndex(ind, ind->idx), xout, yp) !=
            getDoseIndex(ind, ind->idx)) {
          if (!(ind->err & rxErrModeledFss2)){
            ind->err += rxErrModeledFss2;
          }
          return 0;
        }
      }
      break;
    case EVIDF_MODEL_RATE_OFF: // End modeled rate
    case EVIDF_MODEL_DUR_OFF: // end modeled duration
      // In this case re-sort is not going to be assessed
      // If cmt is off, don't remove rate....
      // Probably should throw an error if the infusion rate is on still.
      // ind->curDose and ind->curDoseS[cmt] are handled when the modeled item is turned on.
      tmp = getDoseIndex(ind, ind->idx);
      if (tmp == 0.0) break;
      {
      double *_esDydtPreB2 = _esInfusionDydtPre(id, xout, yp, neq);
      InfusionRate[cmt] += tmp;
      // Modeled rate/duration infusion moving boundary from a modeled lag at the
      // stop time tau2 (which shifts with alag too): forcing change +tmp, so
      // [S] = -tmp*d(alag)/dp.  (Applies to MODEL_RATE_OFF and MODEL_DUR_OFF.)
      if (_rxEsActive && _rxEsNParam > 0 && cmt < _rxEsNState && _rxEsNState * (1 + _rxEsNParam) <= neq && dLagEs != NULL) {
        int _ns = _rxEsNState, _np = _rxEsNParam;
        double *_eA = (double*) calloc((size_t)_ns * _np, sizeof(double));
        if (_eA != NULL) {
          dLagEs(id, xout, yp, _eA);
          for (int _p = 0; _p < _np; _p++) {
            yp[_ns + _p * _ns + cmt] += -tmp * _eA[cmt * _np + _p];
          }
          free(_eA);
        }
        // Second-order alag-boundary term (product rule + Leibniz); see
        // _esInfusionBoundary2ndOrder()'s derivation comment. The DUR-OFF
        // case's OWN separate d(dur)/dp-driven boundary term below (tau2 =
        // tau1+dur(p)) still has no 2nd-order counterpart -- that needs an
        // analogous dDurQ/d2Dur-based derivation, not yet built.
        _esInfusionBoundary2ndOrder(id, xout, yp, cmt, _ns, neq, tmp, _esDydtPreB2);
      }
      if (_esDydtPreB2 != NULL) free(_esDydtPreB2);
      }
      // Event ("jump") sensitivities -- modeled rate continuous forcing OFF.
      // Mirror of the MODEL_RATE_ON injection: remove d(rate)/dp from the
      // sensitivity compartments' forcing at the end of the infusion.
      if (_rxEsActive && _rxEsNParam > 0 && cmt < _rxEsNState && _rxEsNState * (1 + _rxEsNParam) <= neq &&
          ind->whI == EVIDF_MODEL_RATE_OFF && dRateEs != NULL) {
        int _ns = _rxEsNState, _np = _rxEsNParam;
        double *_esB = (double*) calloc((size_t)_ns * _np, sizeof(double));
        if (_esB != NULL) {
          dRateEs(id, xout, yp, _esB);
          // Mirror of the ON injection: remove +d(rate)/dp at infusion end.
          for (int _p = 0; _p < _np; _p++) {
            InfusionRate[_ns + _p * _ns + cmt] -= _esB[cmt * _np + _p];
          }
          free(_esB);
        }
      }
      // Modeled duration: remove the continuous forcing AND add the moving-
      // boundary jump.  The infusion end time tau2 = tau1 + dur(p) depends on the
      // parameter, so as p changes the infusion stops earlier/later: the state's
      // derivative jumps by [xdot] = -rate at tau2, giving the sensitivity a jump
      // [S](tau2) = -[xdot]*d(tau2)/dp = rate*d(dur)/dp.  (rate = -tmp.)  The
      // continuous d(rate)/dp forcing alone is NOT enough for modeled duration --
      // unlike modeled rate, whose boundary is captured by the solver's OFF event.
      if (_rxEsActive && _rxEsNParam > 0 && cmt < _rxEsNState && _rxEsNState * (1 + _rxEsNParam) <= neq &&
          ind->whI == EVIDF_MODEL_DUR_OFF && dDurEs != NULL && durEsFn != NULL) {
        int _ns = _rxEsNState, _np = _rxEsNParam;
        // At the OFF event getDoseIndex returns the rate, so recover the dose
        // amount from curDoseS[cmt] (set when the modeled item was turned on).
        double _esAmt = ind->curDoseS[cmt];
        double _esDur = durEsFn(id, cmt, _esAmt, xout, yp);
        if (_esDur != 0.0) {
          double *_esB = (double*) calloc((size_t)_ns * _np, sizeof(double));
          double *_esFB = (double*) calloc((size_t)_ns * _np, sizeof(double));
          if (_esB != NULL && _esFB != NULL) {
            dDurEs(id, xout, yp, _esB);
            if (dF != NULL) dF(id, xout, yp, _esFB);
            for (int _p = 0; _p < _np; _p++) {
              double _esDDur = _esB[cmt * _np + _p];
              InfusionRate[_ns + _p * _ns + cmt] -=
                (_esAmt * _esFB[cmt * _np + _p] + tmp * _esDDur) / _esDur; // forcing off
              // moving boundary: tau2 = tau1 + dur(p) moves only with the
              // duration's parameters (not F), so d(tau2)/dp = d(dur)/dp = dDur.
              yp[_ns + _p * _ns + cmt] += (-tmp) * _esDDur;
            }
          }
          if (_esB != NULL) free(_esB);
          if (_esFB != NULL) free(_esFB);
        }
        // Second-order boundary/forcing extensions deliberately NOT added
        // here either -- see the NOTE at MODEL_RATE_ON above.  (An earlier
        // attempt here, using d2Dur for the moving-boundary piece alone,
        // was also reverted: even a structurally "self-contained" boundary
        // term of this form was not validated correct by FD once the
        // Leibniz-rule issue was found for the sibling lag-boundary term,
        // so it should not be trusted without its own derivation either.)
      }
      ind->cacheME=0;
      if (ind->wh0 == EVID0_SS2 &&
          getAmt(ind, id, cmt, getDoseIndex(ind, ind->idx), xout, yp) !=
          getDoseIndex(ind, ind->idx)) {
        if (!(ind->err & rxErrModeledFss2n2)){
          ind->err += rxErrModeledFss2n2;
        }
        return 0;
      }
      break;
    case EVIDF_INF_DUR:
      // In this case bio-availability changes the rate, but the
      // duration remains constant.  rate = amt/dur
      ind->on[cmt] = 1;
      tmp = getDoseIndex(ind, ind->idx);
      if (tmp > 0) {
        ind->curDose = tmp;
        ind->curDoseS[cmt] = ind->curDose;
      }
      tmp = getAmt(ind, id, cmt, tmp, xout, yp);
      {
        double *_esDydtPreB = _esInfusionDydtPre(id, xout, yp, neq);
        InfusionRate[cmt] += tmp;
        // Event ("jump") sensitivities -- infusion moving boundary from a modeled
        // lag.  alag(cmt) shifts the whole infusion window [tau1, tau2] by
        // d(alag)/dp (tau1 = t0 + alag, tau2 = tau1 + dur).  The forcing jumps by
        // +tmp (this InfusionRate change) at the boundary, so the sensitivity jumps
        // by [S] = -tmp*d(alag)/dp.  Covers both the start (tmp > 0) and stop
        // (tmp < 0) records of a fixed-rate/duration infusion.
        if (_rxEsActive && _rxEsNParam > 0 && cmt < _rxEsNState && _rxEsNState * (1 + _rxEsNParam) <= neq && dLagEs != NULL) {
          int _ns = _rxEsNState, _np = _rxEsNParam;
          double *_eA = (double*) calloc((size_t)_ns * _np, sizeof(double));
          if (_eA != NULL) {
            dLagEs(id, xout, yp, _eA);
            for (int _p = 0; _p < _np; _p++) {
              yp[_ns + _p * _ns + cmt] += -tmp * _eA[cmt * _np + _p];
            }
            free(_eA);
          }
          // Second-order moving-boundary term (product rule + Leibniz), see
          // _esInfusionBoundary2ndOrder()'s own comment for the derivation.
          _esInfusionBoundary2ndOrder(id, xout, yp, cmt, _ns, neq, tmp, _esDydtPreB);
        }
        if (_esDydtPreB != NULL) free(_esDydtPreB);
      }
      ind->cacheME=0;
      if (ind->wh0 == EVID0_SS2 && tmp != getDoseIndex(ind, ind->idx)) {
        if (!(ind->err & rxErrModeledFss2n3)){
          ind->err += rxErrModeledFss2n3;
        }
        return 0;
      }
      break;
    case EVIDF_INF_RATE:
      // In this case bio-availability changes the duration, but the
      // rate remains constant.  rate = amt/dur
      ind->on[cmt] = 1;
      tmp = getDoseIndex(ind, ind->idx);
      if (tmp > 0) {
        ind->curDose = tmp;
        ind->curDoseS[cmt] = ind->curDose;
      }
      // if (!ind->doSS) {
      //   REprintf("infusion dose[cmt:%d] at %f is %f ind->ixds: %d\n",
      //            cmt, xout, tmp, ind->ixds);
      // }
      {
      double *_esDydtPreB = _esInfusionDydtPre(id, xout, yp, neq);
      InfusionRate[cmt] += tmp;
      // Infusion moving boundary from a modeled lag (see EVIDF_INF_DUR above):
      // [S] = -tmp*d(alag)/dp at each start/stop record.
      if (_rxEsActive && _rxEsNParam > 0 && cmt < _rxEsNState && _rxEsNState * (1 + _rxEsNParam) <= neq && dLagEs != NULL) {
        int _ns = _rxEsNState, _np = _rxEsNParam;
        double *_eA = (double*) calloc((size_t)_ns * _np, sizeof(double));
        if (_eA != NULL) {
          dLagEs(id, xout, yp, _eA);
          for (int _p = 0; _p < _np; _p++) {
            yp[_ns + _p * _ns + cmt] += -tmp * _eA[cmt * _np + _p];
          }
          free(_eA);
        }
        // Second-order moving-boundary term (product rule + Leibniz), see
        // _esInfusionBoundary2ndOrder()'s own comment for the derivation.
        _esInfusionBoundary2ndOrder(id, xout, yp, cmt, _ns, neq, tmp, _esDydtPreB);
      }
      if (_esDydtPreB != NULL) free(_esDydtPreB);
      }
      ind->cacheME=0;
      if (ind->wh0 == EVID0_SS2 && getDoseIndex(ind, ind->idx) > 0 &&
          getAmt(ind, id, cmt, getDoseIndex(ind, ind->idx), xout, yp) !=
          getDoseIndex(ind, ind->idx)) {
        if (!(ind->err & rxErrModeledFss2n3)){
          ind->err += rxErrModeledFss2n3;
        }
      }
      break;
    case EVIDF_REPLACE: // replace
      ind->on[cmt] = 1;
      // Event ("jump") sensitivities -- replacement (plan Section 0, Table 1).
      // The replaced state is set to a value that (for a constant replacement)
      // does not depend on the parameters, so its sensitivity wrt every
      // parameter is reset to 0.  Done BEFORE the physical replace so the
      // pre-event state is still available.  Sens compartment for (state k,
      // param p) = nState + p*nState + k.
      if (_rxEsActive && _rxEsNParam > 0 && cmt < _rxEsNState && _rxEsNState * (1 + _rxEsNParam) <= neq) {
        int _ns = _rxEsNState;
        int _np = _rxEsNParam;
        for (int _p = 0; _p < _np; _p++) {
          yp[_ns + _p * _ns + cmt] = 0.0;
        }
        // Second-order (Hessian path): a constant replacement value has an
        // identically zero second derivative wrt any parameter pair too (same
        // reasoning as the first-order row above) -- zero the 2nd-order
        // compartment for the replaced state, same rxExpandSens2_ layout as
        // the additive-bolus d2F row.
        if (d2FEs != NULL && _rxEsNParam2 > 0 &&
            _ns * (1 + _np) + _ns * _np * _rxEsNParam2 <= neq) {
          int _np2 = _rxEsNParam2;
          for (int _i2 = 0; _i2 < _np; _i2++) {
            for (int _i3 = 0; _i3 < _np2; _i3++) {
              int _c2 = _ns * (1 + _np) + cmt + _ns * (_i2 + _i3 * _np);
              yp[_c2] = 0.0;
            }
          }
          // Third-order (Phase H1): same reasoning, one level deeper -- zero
          // the 3rd-order compartment for the replaced state too.
          if (d3FEs != NULL && _rxEsNParam3 > 0 &&
              _ns * (1 + _np) + _ns * _np * _np2 + _ns * _np * _np2 * _rxEsNParam3 <= neq) {
            int _np3 = _rxEsNParam3;
            for (int _i2 = 0; _i2 < _np; _i2++) {
              for (int _i3 = 0; _i3 < _np2; _i3++) {
                for (int _i4 = 0; _i4 < _np3; _i4++) {
                  int _c3 = _ns * (1 + _np) + _ns * _np * _np2 + cmt +
                    _ns * (_i2 + _np * _i3 + _np * _np2 * _i4);
                  yp[_c3] = 0.0;
                }
              }
            }
          }
        }
        // dtau row (event time), only if the lag is modeled on this
        // compartment (raw event-table replace/multiply records, not the
        // in-model replace()/multiply() plugins, which route param-dependent
        // values through a different, already-correct captured-dosing path
        // -- see the plan's Phase B "B2" note): dxk/dtau = J[k,c]*(x1-xi),
        // with an extra -f_c term for k==c (paper Table 1).  x1 = pre-event
        // state (BEFORE the physical replace just below), xi = replacement
        // value.
        if (dLagEs != NULL && _esHaveJacCol()) {
          double *_esDLagB = (double*) calloc((size_t)_ns * _np, sizeof(double));
          double *_esJcol = (double*) calloc((size_t)_ns, sizeof(double));
          if (_esDLagB != NULL && _esJcol != NULL) {
            dLagEs(id, xout, yp, _esDLagB);
            double _esFc = 0.0;
            _esJacColF(id, xout, yp, cmt, _ns, neq, _esJcol, &_esFc);
            double _esX1 = yp[cmt];
            double _esXi = getAmt(ind, id, cmt, getDoseIndex(ind, ind->idx), xout, yp);
            for (int _p = 0; _p < _np; _p++) {
              double _esDLagP = _esDLagB[cmt * _np + _p];
              if (_esDLagP != 0.0) {
                for (int _esK = 0; _esK < _ns; _esK++) {
                  double _esTerm = _esJcol[_esK] * (_esX1 - _esXi);
                  if (_esK == cmt) _esTerm -= _esFc;
                  yp[_ns + _p * _ns + _esK] += _esTerm * _esDLagP;
                }
              }
            }
          }
          if (_esDLagB != NULL) free(_esDLagB);
          if (_esJcol != NULL) free(_esJcol);
        }
      }
      yp[cmt] = getAmt(ind, id, cmt, getDoseIndex(ind, ind->idx), xout, yp);     //dosing before obs
      break;
    case EVIDF_MULT: //multiply
      ind->on[cmt] = 1;
      {
        double _esAlpha = getAmt(ind, id, cmt, getDoseIndex(ind, ind->idx), xout, yp);
        // Event ("jump") sensitivities -- multiplicative (plan Section 0,
        // Table 3).  The state is scaled by alpha, so each of its parameter
        // sensitivities is scaled by the same alpha.  Done BEFORE the physical
        // multiply so the pre-event state/sens are still available.
        if (_rxEsActive && _rxEsNParam > 0 && cmt < _rxEsNState && _rxEsNState * (1 + _rxEsNParam) <= neq) {
          int _ns = _rxEsNState;
          int _np = _rxEsNParam;
          double _esX1 = yp[cmt]; // pre-event state, before the *= alpha below
          for (int _p = 0; _p < _np; _p++) {
            yp[_ns + _p * _ns + cmt] *= _esAlpha;
          }
          // Second-order (Hessian path): alpha is treated as parameter-fixed
          // for this row (same convention as first order -- a parameter-
          // dependent VALUE routes through the captured-dosing path instead,
          // plan Phase B "B3"), so the second-order compartment for the
          // multiplied state scales by the same alpha, same rxExpandSens2_
          // layout as the additive-bolus d2F row.
          if (d2FEs != NULL && _rxEsNParam2 > 0 &&
              _ns * (1 + _np) + _ns * _np * _rxEsNParam2 <= neq) {
            int _np2 = _rxEsNParam2;
            for (int _i2 = 0; _i2 < _np; _i2++) {
              for (int _i3 = 0; _i3 < _np2; _i3++) {
                int _c2 = _ns * (1 + _np) + cmt + _ns * (_i2 + _i3 * _np);
                yp[_c2] *= _esAlpha;
              }
            }
            // Third-order (Phase H1): same reasoning, one level deeper -- scale
            // the 3rd-order compartment for the multiplied state too.
            if (d3FEs != NULL && _rxEsNParam3 > 0 &&
                _ns * (1 + _np) + _ns * _np * _np2 + _ns * _np * _np2 * _rxEsNParam3 <= neq) {
              int _np3 = _rxEsNParam3;
              for (int _i2 = 0; _i2 < _np; _i2++) {
                for (int _i3 = 0; _i3 < _np2; _i3++) {
                  for (int _i4 = 0; _i4 < _np3; _i4++) {
                    int _c3 = _ns * (1 + _np) + _ns * _np * _np2 + cmt +
                      _ns * (_i2 + _np * _i3 + _np * _np2 * _i4);
                    yp[_c3] *= _esAlpha;
                  }
                }
              }
            }
          }
          // dtau row (event time), only if the lag is modeled (same B2 scope
          // note as EVIDF_REPLACE above): dxk/dtau = (1-alpha)*J[k,c]*x1,
          // with an extra -(1-alpha)*f_c term for k==c (paper Table 3).
          if (dLagEs != NULL && _esHaveJacCol()) {
            double *_esDLagB = (double*) calloc((size_t)_ns * _np, sizeof(double));
            double *_esJcol = (double*) calloc((size_t)_ns, sizeof(double));
            if (_esDLagB != NULL && _esJcol != NULL) {
              dLagEs(id, xout, yp, _esDLagB);
              double _esFc = 0.0;
              _esJacColF(id, xout, yp, cmt, _ns, neq, _esJcol, &_esFc);
              double _esOneMAlpha = 1.0 - _esAlpha;
              for (int _p = 0; _p < _np; _p++) {
                double _esDLagP = _esDLagB[cmt * _np + _p];
                if (_esDLagP != 0.0) {
                  for (int _esK = 0; _esK < _ns; _esK++) {
                    double _esTerm = _esOneMAlpha * (_esJcol[_esK] * _esX1 - (_esK == cmt ? _esFc : 0.0));
                    yp[_ns + _p * _ns + _esK] += _esTerm * _esDLagP;
                  }
                }
              }
            }
            if (_esDLagB != NULL) free(_esDLagB);
            if (_esJcol != NULL) free(_esJcol);
          }
        }
        yp[cmt] *= _esAlpha;     //dosing before obs
      }
      break;
    case EVIDF_NORMAL:
      ind->on[cmt] = 1;
      if (ind->wh0 != EVID0_PHANTOM) {
        // REprintf("handle_evid: EVIDF_NORMAL: %d; cmt: %d; dose: %f; xout: %f\n",
        //          evid, cmt, getDoseIndex(ind, ind->idx), xout);
        double _esRawAmt = getDoseIndex(ind, ind->idx);
        double _esDelta = getAmt(ind, id, cmt, _esRawAmt, xout, yp); // F*amt
        // Event ("jump") sensitivities -- additive bolus.  Inject the jump into
        // the sensitivity compartments BEFORE the physical state is dosed (so the
        // pre-event state is used).  Two contributions (plan Section 0):
        //   ddelta row: d(sens_xc_BY_p) += amt * dF[c][p]   (bioavailability)
        //   dtau   row: d(sens_xk_BY_p) += -J[k][c]*delta*dLag[c][p]  (lag time)
        // Sens compartment for (state k, param p) = nState + p*nState + k.  The
        // physical Jacobian column J[.][c] is taken by a central difference of
        // dydt (calc_jac is empty by default; this is a local df/dx, not the
        // event-parameter FD this method replaces).
        if (_rxEsActive && _rxEsNParam > 0 && cmt < _rxEsNState && _rxEsNState * (1 + _rxEsNParam) <= neq) {
          int _np = _rxEsNParam, _ns = _rxEsNState;
          // Phase H1-dtau Leibniz term prerequisite: dS^p_k/dt BEFORE any of
          // this event's jumps are applied (captured now, before the ddelta/
          // dtau blocks below mutate yp's sensitivity compartments in place).
          // ODE models only (dydtEs is a no-op stub for matExp/indLin).
          double *_esDydtPre = NULL;
          if (!_rxEsUseCalcJac && dydtEs != NULL) {
            _esDydtPre = (double*) calloc((size_t)neq, sizeof(double));
            if (_esDydtPre != NULL) {
              int _esNjPre[2]; _esNjPre[0] = neq; _esNjPre[1] = id;
              dydtEs(_esNjPre, xout, yp, _esDydtPre);
            }
          }
          // matExp()/indLin() models: dydtEs is a no-op stub, so g=dS^p_k/dt
          // can't be read from it directly. But indLin's whole premise is a
          // CONSTANT (time-invariant) Jacobian over the interval -- the
          // k_from_to rate constants are parameter-functions only, never
          // state-dependent -- so dS^p_k/dt = sum_l J[k][l]*S^p_l is LINEAR
          // in S^p, meaning g+ - g- = sum_l J[k][l]*(S^p_l|post - S^p_l|pre)
          // EXACTLY (no approximation, unlike the general nonlinear-ODE
          // case). Capture the pre-jump 1st-order sensitivity block now
          // (before ddelta/dtau mutate it) so the difference can be formed
          // later from the (by-then-mutated) yp; the physical ns x ns
          // Jacobian itself is already available via calc_jac (the same
          // source `_esJacColF` uses for the 1st-order dtau row).
          double *_esSensPre = NULL;
          if (_rxEsUseCalcJac) {
            _esSensPre = (double*) calloc((size_t)_ns * _np, sizeof(double));
            if (_esSensPre != NULL) {
              memcpy(_esSensPre, yp + _ns, (size_t)_ns * _np * sizeof(double));
            }
          }
          // ddelta row (bioavailability)
          if (dF != NULL) {
            double *_dFB = (double*) calloc((size_t)_ns * _np, sizeof(double));
            if (_dFB != NULL) {
              dF(id, xout, yp, _dFB);
              for (int _p = 0; _p < _np; _p++) {
                yp[_ns + _p * _ns + cmt] += _esRawAmt * _dFB[cmt * _np + _p];
              }
              free(_dFB);
            }
          }
          // Second-order ddelta row (Hessian path): the second-order
          // sensitivity S^{pq} of the dosed compartment jumps by amt*d2F[p][q].
          // 2nd-order compartment for (cmt, p=i2, q=i3) follows the
          // rxExpandSens2_ layout: nState*(1+np) + cmt + nState*(i2 + i3*np),
          // after the states and the first-order sens block.
          if (d2FEs != NULL && _rxEsNParam2 > 0 &&
              _ns * (1 + _np) + _ns * _np * _rxEsNParam2 <= neq) {
            int _np2 = _rxEsNParam2;
            double *_d2FB = (double*) calloc((size_t)_ns * _np * _np2, sizeof(double));
            if (_d2FB != NULL) {
              d2FEs(id, xout, yp, _d2FB);
              for (int _i2 = 0; _i2 < _np; _i2++) {
                for (int _i3 = 0; _i3 < _np2; _i3++) {
                  int _c2 = _ns * (1 + _np) + cmt + _ns * (_i2 + _i3 * _np);
                  yp[_c2] += _esRawAmt * _d2FB[cmt * (_np * _np2) + _i2 * _np2 + _i3];
                }
              }
              free(_d2FB);
            }
          }
          // Third-order ddelta row (Phase H1): the third-order sensitivity
          // S^{pqr} of the dosed compartment jumps by amt*d3F[p][q][r].
          // Additive-bolus F row only (H1 scope).  3rd-order compartment for
          // (cmt, p=i2, q=i3, r=i4) follows the rxExpandSens3_ layout:
          // nState*(1+np) + nState*np*np2 + cmt + nState*(i2 + np*i3 + np*np2*i4),
          // after the states, first-order, and second-order sens blocks.
          if (d3FEs != NULL && _rxEsNParam3 > 0 && d2FEs != NULL && _rxEsNParam2 > 0 &&
              _ns * (1 + _np) + _ns * _np * _rxEsNParam2 + _ns * _np * _rxEsNParam2 * _rxEsNParam3 <= neq) {
            int _np2 = _rxEsNParam2, _np3 = _rxEsNParam3;
            double *_d3FB = (double*) calloc((size_t)_ns * _np * _np2 * _np3, sizeof(double));
            if (_d3FB != NULL) {
              d3FEs(id, xout, yp, _d3FB);
              for (int _i2 = 0; _i2 < _np; _i2++) {
                for (int _i3 = 0; _i3 < _np2; _i3++) {
                  for (int _i4 = 0; _i4 < _np3; _i4++) {
                    int _c3 = _ns * (1 + _np) + _ns * _np * _np2 + cmt +
                      _ns * (_i2 + _np * _i3 + _np * _np2 * _i4);
                    yp[_c3] += _esRawAmt * _d3FB[cmt * (_np * _np2 * _np3) +
                                                  _i2 * (_np2 * _np3) + _i3 * _np3 + _i4];
                  }
                }
              }
              free(_d3FB);
            }
          }
          // dtau row (lag time), only if the lag is modeled.  Jacobian column
          // J[.,cmt] source depends on _rxEsUseCalcJac: matExp()/indLin()
          // models have no functional dydt() (the primal system is solved by
          // matrix-exponential propagation, not RHS evaluation, so a central
          // difference of dydt is always zero for them) -- for those, read
          // the column from calc_jac instead (populated by rxSensMatExp()'s
          // explicit df/dy lines, sized/strided to the physical state count
          // to match how those lines are emitted). Ordinary ODE models keep
          // the central-difference-of-dydt approach (calc_jac is normally an
          // empty stub for them -- only populated by user-written df/dy).
          if (dLagEs != NULL && _esHaveJacCol()) {
            double *_esDLagB = (double*) calloc((size_t)_ns * _np, sizeof(double));
            double *_esJcol = (double*) calloc((size_t)_ns, sizeof(double));
            if (_esDLagB != NULL && _esJcol != NULL) {
              dLagEs(id, xout, yp, _esDLagB);
              double _esFc; // unused here (additive-bolus dtau row has no f_c term)
              _esJacColF(id, xout, yp, cmt, _ns, neq, _esJcol, &_esFc);
              for (int _p = 0; _p < _np; _p++) {
                double _esDLagP = _esDLagB[cmt * _np + _p];
                if (_esDLagP != 0.0) {
                  for (int _esK = 0; _esK < _ns; _esK++) {
                    yp[_ns + _p * _ns + _esK] += -_esJcol[_esK] * _esDelta * _esDLagP;
                  }
                }
              }
            }
            if (_esDLagB != NULL) free(_esDLagB);
            if (_esJcol != NULL) free(_esJcol);
          }
          // Second-order dtau row (Phase H1's dtau/lag row): d/dq of the
          // 1st-order dtau jump above, by the product rule applied to
          // -J[k][c]*delta*dLag_p[c]:
          //   -dJdq[k][c]*delta*dLag_p[c]              (Jacobian coupling)
          //   -J[k][c]*(amt*dFQ[c][q])*dLag_p[c]        (delta = F*amt coupling)
          //   -J[k][c]*delta*d2Lag[p][q][c]             (dLag itself coupling)
          // dJdq[k][c] = d(J[k][c])/dq, the total derivative of the physical
          // Jacobian column (`.rxEventSensDerivs()`'s "lagJacQ" table,
          // computed symbolically from the model's own d/dt() RHS -- see the
          // plan's Phase H1-dtau note).
          //
          // PLUS a Leibniz/moving-boundary term whenever q ALSO drives this
          // same alag (dLag_q[c] != 0): the three terms above hold time FIXED
          // at xout (they only capture q's effect through the CURRENT state,
          // via the S^q_l total-derivative coupling already baked into each
          // factor) -- they do not capture q ALSO shifting the jump time
          // itself. Re-deriving the standard jump-condition formula one
          // order up (treating S^p_k as "the state" that jumps, mirroring
          // how the physical-state formula -J[k][c]*delta*dLag_p[c] was
          // itself derived from x_k's own jump condition) gives an
          // additional term `-[g+(tau) - g-(tau)] * dLag_q[c]`, where
          // g = dS^p_k/dt is the RHS of S^p_k's OWN sensitivity ODE
          // (`sum_l J[k][l]*S^p_l`). Sign verified against the
          // ALREADY-VALIDATED 1st-order formula's own derivation (matching
          // signs requires g- - g+, i.e. `-(g+ - g-)`), then confirmed
          // against FD (two-lag-parameter and diagonal models, ~1e-9).
          //
          // ODE models read g directly from the compiled dydt() output for
          // the sensitivity compartment (g- pre-jump via `_esDydtPre`, g+
          // post-jump via a scratch copy with the physical dose added -- yp's
          // sensitivity compartments are already post-jump by this point).
          // matExp()/indLin() models: dydtEs is a no-op stub, but indLin's
          // whole premise is a Jacobian CONSTANT over the interval, so
          // g+ - g- = sum_l J[k][l]*(S^p_l|post - S^p_l|pre) exactly, using
          // the SAME physical Jacobian already read via calc_jac
          // (`_esJacColF`'s source) and the `_esSensPre` snapshot captured
          // at the top of this case.
          //
          // Compartment layout DIFFERS by model type and is NOT the same
          // rxExpandSens2_ layout the additive-bolus d2F row uses:
          // rxSensMatExp() builds its own (p-outer, q-inner) compartment
          // list for matExp models (confirmed directly from generated code),
          // vs. rxExpandSens2_'s (q-outer, p-inner) for ordinary ODE models.
          // d2F/d3F's own (p,q) writes are insensitive to this because their
          // values are symmetric in (p,q) by construction (a literal double/
          // triple symbolic differentiation, always commutes) -- swapping
          // which named compartment receives which of two equal values is
          // invisible. This row's 3-term-only piece is NOT symmetric on its
          // own (gated by dLag_p != 0), which is what originally exposed the
          // mismatch as a matExp-vs-ODE discrepancy; with the Leibniz term
          // added the full (p,q) result IS symmetric again, so getting the
          // per-model-type index right (rather than relying on symmetry to
          // paper over it) is what makes both model types correct together.
          if (d2LagEs != NULL && dLagJacEs != NULL && _esHaveJacCol() &&
              _rxEsNParam2 > 0 &&
              _ns * (1 + _np) + _ns * _np * _rxEsNParam2 <= neq) {
            int _np2 = _rxEsNParam2;
            double *_esDLagB2 = (double*) calloc((size_t)_ns * _np, sizeof(double));
            double *_esJcol2 = (double*) calloc((size_t)_ns, sizeof(double));
            double *_esD2LagB = (double*) calloc((size_t)_ns * _np * _np2, sizeof(double));
            double *_esDFQB = (double*) calloc((size_t)_ns * _np2, sizeof(double));
            double *_esJacQB = (double*) calloc((size_t)_ns * _ns * _np2, sizeof(double));
            double *_esDLagQB = (double*) calloc((size_t)_ns * _np2, sizeof(double));
            // matExp()/indLin() only: the full physical ns x ns Jacobian
            // (row-major, J[k][l] = _esFullJac[k*ns+l]), needed for the
            // Leibniz term's `sum_l J[k][l]*(S^p_l|post - S^p_l|pre)`.
            double *_esFullJac = NULL;
            if (_rxEsUseCalcJac && calc_jac != NULL) {
              _esFullJac = (double*) calloc((size_t)_ns * _ns, sizeof(double));
              if (_esFullJac != NULL) {
                int _esNjF[2]; _esNjF[0] = neq; _esNjF[1] = id;
                calc_jac(_esNjF, xout, yp, _esFullJac, (unsigned int) _ns);
              }
            }
            if (_esDLagB2 != NULL && _esJcol2 != NULL && _esD2LagB != NULL &&
                _esDFQB != NULL && _esJacQB != NULL && _esDLagQB != NULL) {
              dLagEs(id, xout, yp, _esDLagB2);
              double _esFc2;
              _esJacColF(id, xout, yp, cmt, _ns, neq, _esJcol2, &_esFc2);
              d2LagEs(id, xout, yp, _esD2LagB);
              if (dFQEs != NULL) dFQEs(id, xout, yp, _esDFQB);
              dLagJacEs(id, xout, yp, _esJacQB);
              if (dLagQEs != NULL) dLagQEs(id, xout, yp, _esDLagQB);
              // Does ANY calcSens2 param also drive this event's alag? The
              // Leibniz term below needs dS^p_./dt regardless of whether p
              // ITSELF has a nonzero dLag_p (S^p may have been jumped purely
              // via the ddelta/F row, e.g. p=tf when alag depends only on a
              // different parameter) -- so the p-loop below cannot skip on
              // dLag_p alone once this is true (that skip previously hid the
              // Leibniz contribution to the "mirror" compartment entirely).
              int _esAnyLagQ = 0;
              for (int _qq = 0; _qq < _np2; _qq++) {
                if (_esDLagQB[cmt * _np2 + _qq] != 0.0) { _esAnyLagQ = 1; break; }
              }
              for (int _p = 0; _p < _np; _p++) {
                double _esDLagP2 = _esDLagB2[cmt * _np + _p];
                if (_esDLagP2 == 0.0 && !_esAnyLagQ) continue;
                // Leibniz term prerequisite: g+ - g- = dS^p_./dt|post -
                // dS^p_./dt|pre, computed ONCE per active p (independent of
                // q). ODE: two real dydt() evaluations (handles state-
                // dependent/nonlinear Jacobians correctly). matExp: the
                // Jacobian is constant over the interval, so g+-g- reduces
                // to J*(S^p|post - S^p|pre) exactly -- yp's sensitivity
                // compartments already hold the post-jump S^p values
                // (mutated in place by the ddelta/dtau blocks above);
                // `_esSensPre` holds the pre-jump snapshot.
                double *_esDydtPostP = NULL;
                double *_esGdiff = NULL;
                if (_esAnyLagQ) {
                  if (_esDydtPre != NULL) {
                    double *_esYTemp = (double*) malloc((size_t)neq * sizeof(double));
                    if (_esYTemp != NULL) {
                      memcpy(_esYTemp, yp, (size_t)neq * sizeof(double));
                      _esYTemp[cmt] += _esDelta;
                      _esDydtPostP = (double*) calloc((size_t)neq, sizeof(double));
                      if (_esDydtPostP != NULL) {
                        int _esNjPost[2]; _esNjPost[0] = neq; _esNjPost[1] = id;
                        dydtEs(_esNjPost, xout, _esYTemp, _esDydtPostP);
                      }
                      free(_esYTemp);
                    }
                  } else if (_esFullJac != NULL && _esSensPre != NULL) {
                    _esGdiff = (double*) calloc((size_t)_ns, sizeof(double));
                    if (_esGdiff != NULL) {
                      for (int _esK = 0; _esK < _ns; _esK++) {
                        double _esAcc = 0.0;
                        for (int _esL = 0; _esL < _ns; _esL++) {
                          double _esDS = yp[_ns + _p * _ns + _esL] - _esSensPre[_p * _ns + _esL];
                          _esAcc += _esFullJac[_esK * _ns + _esL] * _esDS;
                        }
                        _esGdiff[_esK] = _esAcc;
                      }
                    }
                  }
                }
                for (int _q = 0; _q < _np2; _q++) {
                  double _esD2LagPQ = _esD2LagB[cmt * (_np * _np2) + _p * _np2 + _q];
                  double _esDFQc = _esDFQB[cmt * _np2 + _q];
                  double _esDLagQq = _esDLagQB[cmt * _np2 + _q];
                  for (int _esK = 0; _esK < _ns; _esK++) {
                    double _esDJdq = _esJacQB[cmt * (_ns * _np2) + _esK * _np2 + _q];
                    // Compartment layout is model-type-specific -- see the
                    // note above (rxSensMatExp: p-outer/q-inner; ordinary
                    // ODE models via rxExpandSens2_: q-outer/p-inner).
                    int _c2 = _rxEsUseCalcJac ?
                      (_ns * (1 + _np) + _esK + _ns * (_p * _np2 + _q)) :
                      (_ns * (1 + _np) + _esK + _ns * (_p + _q * _np));
                    double _esTerm = -_esDJdq * _esDelta * _esDLagP2
                      - _esJcol2[_esK] * (_esRawAmt * _esDFQc) * _esDLagP2
                      - _esJcol2[_esK] * _esDelta * _esD2LagPQ;
                    if (_esDLagQq != 0.0) {
                      if (_esDydtPostP != NULL && _esDydtPre != NULL) {
                        double _esGpost = _esDydtPostP[_ns + _p * _ns + _esK];
                        double _esGpre = _esDydtPre[_ns + _p * _ns + _esK];
                        _esTerm += -(_esGpost - _esGpre) * _esDLagQq;
                      } else if (_esGdiff != NULL) {
                        _esTerm += -_esGdiff[_esK] * _esDLagQq;
                      }
                    }
                    yp[_c2] += _esTerm;
                  }
                }
                if (_esDydtPostP != NULL) free(_esDydtPostP);
                if (_esGdiff != NULL) free(_esGdiff);
              }
            }
            if (_esFullJac != NULL) free(_esFullJac);
            if (_esDLagB2 != NULL) free(_esDLagB2);
            if (_esJcol2 != NULL) free(_esJcol2);
            if (_esD2LagB != NULL) free(_esD2LagB);
            if (_esDFQB != NULL) free(_esDFQB);
            if (_esJacQB != NULL) free(_esJacQB);
            if (_esDLagQB != NULL) free(_esDLagQB);
          }
          if (_esDydtPre != NULL) free(_esDydtPre);
          if (_esSensPre != NULL) free(_esSensPre);
        }
        yp[cmt] += _esDelta;     //dosing before obs
      }
		}
		ind->ixds++;
		ind->solved = ind->idx;
    return 1;
	}
  return 0;
}

static inline int handleEvid1(int *i, rx_solve *rx, int *neq, double *yp, double *xout) {
  rx_solving_options_ind *ind = &(rx->subjects[neq[1]]);
  rx_solving_options *op = rx->op;
  ind->idx = *i;
  if (!isObs(getEvid(ind, ind->ix[ind->idx]))) {
    syncIdx(ind);
  }
  // For pushed events whose time was corrected to earlier than the integration
  // endpoint, use the event's own time so handleTlastInline records the correct
  // tlast (tad/podo would otherwise be off by the endpoint vs. event-time delta).
  double evTime = ind->timeThread[ind->ix[ind->idx]];
  double effectiveXout = (!isSameTime(evTime, *xout) && evTime < *xout) ? evTime : *xout;
  int he = handle_evid(getEvid(ind, ind->ix[ind->idx]), neq[0] + op->extraCmt,
                       ind->BadDose, ind->InfusionRate, ind->dose, yp,
                       effectiveXout, neq[1], ind);
  return he;
}

// time   amt rate          ii  addl evid            ss
static inline int getEvidFlag(int cmt, double amt, double rate, double ii, int evid, double ss) {
	// #define  30
	if (evid == 7) {
		if (cmt > 0) {
			return EVID0_PHANTOM;
		} else {
			return -1; // bad phantom
		}
	}
	if (ss == 1.0) {
		if (ii > 0.0) {
			if (cmt > 0) {
				return EVID0_SS;
			} else {
				return -2; // bad steady state 1
			}
		}
		if (ii == 0.0 && amt == 0.0) {
			if (cmt > 0) {
				return EVID0_SSINF;
			} else {
				return -3; // bad infinite steady state infusion
			}
		}
	} else if (ss == 2.0 && ii > 0.0) {
		if (cmt > 0) {
			return EVID0_SS2;
		} else {
			return -4;
		}
	}
	if (cmt < 0) {
		// turn off the compartment
		return EVID0_OFF;
	}
	if (ss == 0.0 || ISNA(ss)) return EVID0_REGULAR;
	return EVID0_REGULAR;
}


static inline int getEvidRateI(int cmt, double amt, double rate, double dur, double ii, int evid, double ss) {
	if (evid == 1) {
		if (dur == 0.0) {
			if (rate == -1.0) {
				return EVIDF_MODEL_RATE_ON;
				// #define EVIDF_MODEL_RATE_OFF 7
			} else if  (rate == -2.0) {
				return EVIDF_MODEL_DUR_ON;
			} else if (rate > 0.0) {
				return EVIDF_INF_RATE;
				// #define EVIDF_MODEL_DUR_OFF  6
			}
		} else if (rate == 0.0) {
			if (dur > 0.0 ) {
				return EVIDF_INF_DUR;
			}
		}
	} else if (evid == 5) {
		// replace
		return EVIDF_REPLACE;
	} else if (evid == 6) {
		return EVIDF_MULT;
	}
	return EVIDF_NORMAL;
}

static inline int getEvidClassic(int cmt, double amt, double rate, double dur, double ii, int evid, double ss) {
	if (isObs(evid)) return evid;
	int cmtP = cmt;
	int cmt100, cmt99, rateI, flg;
	if (cmtP < 0) cmtP = -cmtP;
	if (cmtP <= 99){
		cmt100=0;
		cmt99=cmtP;
	} else {
		cmt100=cmtP/100;
		cmt99=cmtP-cmt100*100;
	}
	rateI = getEvidRateI(cmt, amt, rate, dur, ii, evid, ss);
	flg = getEvidFlag(cmt, amt, rate, ii, evid, ss);
	return cmt100*100000+rateI*10000+cmt99*100+flg;
}

#endif
