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
// `preState` must be captured by the caller (`_esInfusionDydtPre()`) BEFORE
// dtmp was applied to InfusionRate[cmt] and before the 1st-order jump was
// applied to yp; this function reads the CURRENT (already-updated)
// yp/InfusionRate for the "post" side, so no scratch-copy is needed here
// (unlike the additive-bolus case, InfusionRate is not part of yp itself).
//
// matExp()/indLin() models: dydtEs is a no-op stub, but (same trick as the
// additive-bolus dtau row's own matExp fix) indLin's premise is a physical
// Jacobian CONSTANT over the interval, so g+_k - g-_k = sum_l
// J[k][l]*(S^p_l|post - S^p_l|pre) exactly -- computed from the SAME
// calc_jac source `_esJacColF` uses, dotted against `preState` (here the
// pre-jump 1st-order sensitivity BLOCK, not a dydt snapshot -- see
// `_esInfusionDydtPre()`) and the current (already post-1st-order-jump) yp.
// Compartment layout is ALSO model-type-specific here, same reason as the
// dtau row (rxSensMatExp: p-outer/q-inner; ordinary ODE models via
// rxExpandSens2_: q-outer/p-inner) -- this row's own 3-term-only piece is
// asymmetric in (p,q) (gated on dtmp/dLag_p, not symmetric like d2F), so
// the per-model-type index matters here too, not just after adding the
// Leibniz term (unlike d2F, which never needed this because its own values
// are symmetric in (p,q) regardless of which slot receives them).
//
// SAFETY GUARD (found by FD, 2026-07-01): this derivation implicitly treats
// `dtmp` (the InfusionRate CHANGE at this event) as p,q-INDEPENDENT, valid
// for a fixed-rate/duration infusion (EVIDF_INF_RATE/DUR) but FALSE at the
// MODEL_RATE_ON/OFF and MODEL_DUR_ON/OFF events, where `tmp` IS the modeled
// rate value itself (tmp = +-F*amt/dur) and so genuinely depends on q
// whenever F or dur is modeled with q-dependence. The missing piece is an
// additional `-[d(tmp)/dq]*dLag_p[cmt]` cross term (confirmed wrong by FD
// when omitted). ALSO, when p ITSELF drives F/dur, the g+/g- (dS^p_k/dt)
// computation here runs BEFORE this same event's continuous-forcing code
// (dRate/dDur additions to InfusionRate) has executed for THIS event, so
// g+ is missing that just-turned-on forcing contribution -- an ordering
// issue, not just a missing term (confirmed wrong by FD: an alag+modeled
// -duration-only model, no F, still gave S^{tlag,tinf} off by ~8% even
// after guarding q alone). Rather than derive the full multi-parameter
// cross terms and fix the evaluation order, this SKIPS (leaves 0) any
// (p,q) pair where EITHER p or q drives F or dur at this cmt (checked via
// dF/dDurEs for p, dFQEs/dDurQEs for q) -- i.e. exactly the combination
// where the "dtmp constant" / "forcing already applied" assumptions break
// down -- shipping a documented incompleteness rather than a silently
// wrong value.
static inline void _esInfusionBoundary2ndOrder(int id, double xout, double *yp,
                                               int cmt, int ns, int neq,
                                               double dtmp, double *preState) {
  if (preState == NULL) return;
  if (!_rxEsUseCalcJac && dydtEs == NULL) return;
  if (_rxEsUseCalcJac && calc_jac == NULL) return;
  if (d2LagEs == NULL || dLagQEs == NULL) return;
  int np = _rxEsNParam, np2 = _rxEsNParam2;
  if (np2 <= 0) return;
  if (ns * (1 + np) + ns * np * np2 > neq) return;
  double *_d2LagB = (double*) calloc((size_t)ns * np * np2, sizeof(double));
  double *_dLagQB = (double*) calloc((size_t)ns * np2, sizeof(double));
  double *_dydtPost = NULL;  // ODE source for g+
  double *_fullJac = NULL;   // matExp source for g+ - g- (constant Jacobian)
  // Safety-guard tables: does p (calcSens-indexed) or q (calcSens2-indexed)
  // drive F/dur at this cmt?
  double *_fPGuard = (double*) calloc((size_t)ns * np, sizeof(double));
  double *_durPGuard = (double*) calloc((size_t)ns * np, sizeof(double));
  double *_fqGuard = (double*) calloc((size_t)ns * np2, sizeof(double));
  double *_durQGuard = (double*) calloc((size_t)ns * np2, sizeof(double));
  if (dF != NULL && _fPGuard != NULL) dF(id, xout, yp, _fPGuard);
  if (dDurEs != NULL && _durPGuard != NULL) dDurEs(id, xout, yp, _durPGuard);
  if (dFQEs != NULL && _fqGuard != NULL) dFQEs(id, xout, yp, _fqGuard);
  if (dDurQEs != NULL && _durQGuard != NULL) dDurQEs(id, xout, yp, _durQGuard);
  if (_rxEsUseCalcJac) {
    _fullJac = (double*) calloc((size_t)ns * ns, sizeof(double));
    if (_fullJac != NULL) {
      int _esNjF[2]; _esNjF[0] = neq; _esNjF[1] = id;
      calc_jac(_esNjF, xout, yp, _fullJac, (unsigned int) ns);
    }
  } else {
    _dydtPost = (double*) calloc((size_t)neq, sizeof(double));
    if (_dydtPost != NULL) {
      int _esNj[2]; _esNj[0] = neq; _esNj[1] = id;
      dydtEs(_esNj, xout, yp, _dydtPost);
    }
  }
  int _haveG = _rxEsUseCalcJac ? (_fullJac != NULL) : (_dydtPost != NULL);
  if (_d2LagB != NULL && _dLagQB != NULL && _haveG) {
    d2LagEs(id, xout, yp, _d2LagB);
    dLagQEs(id, xout, yp, _dLagQB);
    for (int _p = 0; _p < np; _p++) {
      // Safety guard: skip if tmp might depend on p (see comment above).
      if ((_fPGuard != NULL && _fPGuard[cmt * np + _p] != 0.0) ||
          (_durPGuard != NULL && _durPGuard[cmt * np + _p] != 0.0)) {
        continue;
      }
      for (int _q = 0; _q < np2; _q++) {
        // Safety guard: skip if tmp might depend on q (see comment above).
        if ((_fqGuard != NULL && _fqGuard[cmt * np2 + _q] != 0.0) ||
            (_durQGuard != NULL && _durQGuard[cmt * np2 + _q] != 0.0)) {
          continue;
        }
        double _d2LagPQ = _d2LagB[cmt * (np * np2) + _p * np2 + _q];
        double _dLagQq = _dLagQB[cmt * np2 + _q];
        int _c2cmt = _rxEsUseCalcJac ?
          (ns * (1 + np) + cmt + ns * (_p * np2 + _q)) :
          (ns * (1 + np) + cmt + ns * (_p + _q * np));
        yp[_c2cmt] += -dtmp * _d2LagPQ;
        if (_dLagQq != 0.0) {
          for (int _esK = 0; _esK < ns; _esK++) {
            double _gdiff;
            if (_rxEsUseCalcJac) {
              double _acc = 0.0;
              for (int _esL = 0; _esL < ns; _esL++) {
                double _dS = yp[ns + _p * ns + _esL] - preState[_p * ns + _esL];
                _acc += _fullJac[_esK * ns + _esL] * _dS;
              }
              _gdiff = _acc;
            } else {
              _gdiff = _dydtPost[ns + _p * ns + _esK] - preState[ns + _p * ns + _esK];
            }
            int _c2 = _rxEsUseCalcJac ?
              (ns * (1 + np) + _esK + ns * (_p * np2 + _q)) :
              (ns * (1 + np) + _esK + ns * (_p + _q * np));
            yp[_c2] += -_gdiff * _dLagQq;
          }
        }
      }
    }
  }
  if (_d2LagB != NULL) free(_d2LagB);
  if (_dLagQB != NULL) free(_dLagQB);
  if (_dydtPost != NULL) free(_dydtPost);
  if (_fullJac != NULL) free(_fullJac);
  if (_fqGuard != NULL) free(_fqGuard);
  if (_durQGuard != NULL) free(_durQGuard);
  if (_fPGuard != NULL) free(_fPGuard);
  if (_durPGuard != NULL) free(_durPGuard);
}

// Captures the state needed for `_esInfusionBoundary2ndOrder()`'s Leibniz
// term, BEFORE an infusion-boundary event mutates InfusionRate/yp: for ODE
// models, dS^p_./dt via dydtEs(); for matExp models (dydtEs is a no-op
// stub), the pre-jump 1st-order sensitivity BLOCK itself (same "constant
// Jacobian" trick as the additive-bolus dtau row's own matExp fix).
static inline double *_esInfusionDydtPre(int id, double xout, double *yp, int neq) {
  // SAFETY GUARD (found by git-bisecting a real C stack overflow crash,
  // 2026-07-01): every call site invokes this UNCONDITIONALLY, before any
  // `_rxEsActive` check, so it ran even for models with eventSens entirely
  // inactive. For an ODE model this meant calling `dydtEs()` (an alias for
  // the model's own compiled `dydt()`) -- but `handle_evid()` is itself
  // invoked FROM WITHIN `dydt()` for modeled-rate/duration events, so this
  // created unbounded re-entrant recursion into `dydt()` whenever a
  // MODEL_RATE_ON/MODEL_DUR_ON event fired on ANY model with modeled
  // rate()/dur(), active eventSens or not -- confirmed via `git bisect` to
  // this function's introducing commit, reproduced as a genuine "C stack
  // overflow" on a plain (non-eventSens) model using the classic `lsode`
  // solver (recursion depth is solver/step-count dependent, explaining why
  // it only manifested for specific method+dataset combinations). The
  // `_rxEsUseCalcJac` branch had a parallel latent bug: it `memcpy`s
  // `_ns*_np` doubles out of `yp` using POSSIBLY-STALE `_rxEsNState`/
  // `_rxEsNParam` dims left over from an earlier, unrelated active solve,
  // which could read past `yp`'s actual (`neq`-sized) allocation for the
  // CURRENT model. Gating on `_rxEsActive` here fixes both: no eventSens
  // means no jump-sensitivity information is needed, so there is nothing
  // for this function's caller to consume regardless.
  if (!_rxEsActive) return NULL;
  if (_rxEsUseCalcJac) {
    int _ns = _rxEsNState, _np = _rxEsNParam;
    if (_np <= 0 || _ns <= 0) return NULL;
    double *_pre = (double*) calloc((size_t)_ns * _np, sizeof(double));
    if (_pre != NULL) {
      memcpy(_pre, yp + _ns, (size_t)_ns * _np * sizeof(double));
    }
    return _pre;
  }
  if (dydtEs == NULL) return NULL;
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
        // Second-order piece (2026-07-01): differentiating the quotient
        // rate=F*amt/dur a second time wrt q via the quotient rule needs
        // d(dur)/dq and d(F)/dq -- i.e. the FIRST-order dDur/dF value at q's
        // position in calcSens2, a DIFFERENT index space than the (cmt,p)
        // buffers above (calcSens-indexed). Rather than build a
        // calcSens2-position -> calcSens-position cross-index map, this
        // reuses the SAME "evaluate directly at calcSens2 params" trick as
        // the dtau row's "fq"/"lagQ" tables: `dFQEs`/`dDurQEs` give dF/dDur
        // AT q's own index space directly, so no remapping is needed. Full
        // derivation (rate=F*amt/dur, tmp=-rate):
        //   d2(rate)/dp/dq = amt*d2F[p][q]/dur
        //                    - amt*(dF_p*dDurQ_q + dFQ_q*dDur_p)/dur^2
        //                    + tmp*d2Dur[p][q]/dur
        //                    - 2*tmp*dDur_p*dDurQ_q/dur^2
        // Compartment layout is model-type-specific, same reason/formula as
        // the additive-bolus d2F row and the infusion moving-boundary term.
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
              // Second-order continuous-forcing piece.
              if (d2FEs != NULL && d2DurEs != NULL && dFQEs != NULL && dDurQEs != NULL &&
                  _rxEsNParam2 > 0 &&
                  _ns * (1 + _np) + _ns * _np * _rxEsNParam2 <= neq) {
                int _np2 = _rxEsNParam2;
                double *_esD2FB = (double*) calloc((size_t)_ns * _np * _np2, sizeof(double));
                double *_esD2DurB = (double*) calloc((size_t)_ns * _np * _np2, sizeof(double));
                double *_esFQB = (double*) calloc((size_t)_ns * _np2, sizeof(double));
                double *_esDurQB = (double*) calloc((size_t)_ns * _np2, sizeof(double));
                if (_esD2FB != NULL && _esD2DurB != NULL && _esFQB != NULL && _esDurQB != NULL) {
                  d2FEs(id, xout, yp, _esD2FB);
                  d2DurEs(id, xout, yp, _esD2DurB);
                  dFQEs(id, xout, yp, _esFQB);
                  dDurQEs(id, xout, yp, _esDurQB);
                  double _esDur2 = _esDur * _esDur;
                  for (int _p = 0; _p < _np; _p++) {
                    double _esDFp = _esFB[cmt * _np + _p];
                    double _esDDurp = _esB[cmt * _np + _p];
                    for (int _q = 0; _q < _np2; _q++) {
                      double _esD2Fpq = _esD2FB[cmt * (_np * _np2) + _p * _np2 + _q];
                      double _esD2Durpq = _esD2DurB[cmt * (_np * _np2) + _p * _np2 + _q];
                      double _esFQq = _esFQB[cmt * _np2 + _q];
                      double _esDurQq = _esDurQB[cmt * _np2 + _q];
                      double _esTerm = _esAmt * _esD2Fpq / _esDur
                        - _esAmt * (_esDFp * _esDurQq + _esFQq * _esDDurp) / _esDur2
                        + tmp * _esD2Durpq / _esDur
                        - 2.0 * tmp * _esDDurp * _esDurQq / _esDur2;
                      int _c2 = _rxEsUseCalcJac ?
                        (_ns * (1 + _np) + cmt + _ns * (_p * _np2 + _q)) :
                        (_ns * (1 + _np) + cmt + _ns * (_p + _q * _np));
                      InfusionRate[_c2] += _esTerm;
                    }
                  }
                }
                if (_esD2FB != NULL) free(_esD2FB);
                if (_esD2DurB != NULL) free(_esD2DurB);
                if (_esFQB != NULL) free(_esFQB);
                if (_esDurQB != NULL) free(_esDurQB);
              }
              // Second-order tau1-boundary cross term: tau1 = t0+alag(p)
              // shifts with alag-driving p, and the ON-event forcing jump
              // [xdot]_ON=-tmp=+rate depends on q whenever q drives F/dur
              // (dRateQq, same sign convention as the validated 1st-order
              // d(rate)/dp=(amt*dF+tmp*dDur)/dur formula). This is a
              // DIFFERENT term from the continuous-forcing piece above: it
              // comes from differentiating the general moving-boundary jump
              // condition "[S](tau1)=-[xdot]*d(tau1)/dp" a SECOND time wrt
              // q, picking up an extra -dRateQq*dLag_p product-rule term
              // (tau1 itself has NO dur dependence, so unlike the DUR-OFF
              // boundary tau2=tau1+dur(p) there is no Leibniz-side fix
              // needed here when q drives dur/F alone -- dtau1/dq=dLagQ_q
              // is unaffected by dur/F -- only this product-rule cross
              // term, previously entirely missing). Confirmed by FD: an
              // alag+modeled-dur-only model (no F) showed
              // S^{tlag,tinf}_depot analytically 0 throughout the WHOLE
              // infusion window while FD showed a decaying nonzero value
              // seeded right at tau1 (_esInfusionBoundary2ndOrder()'s own
              // guard skips this (p,q) pair entirely since q drives dur,
              // which is necessary for the DUR-OFF boundary but overly
              // conservative here since tau1 has no dur term to worry
              // about -- rather than loosen that shared guard, this adds
              // the missing piece directly); a full alag+dur+F model's
              // S^{tlag,doseAmt} (F-driven q, no dur) caught a further sign
              // bug in dRateQq's dur term that had accidentally cancelled
              // for the dur-only case above. Validated to ~1e-10 on both.
              if (dLagEs != NULL && dFQEs != NULL && dDurQEs != NULL &&
                  _rxEsNParam2 > 0 &&
                  _ns * (1 + _np) + _ns * _np * _rxEsNParam2 <= neq) {
                int _np2 = _rxEsNParam2;
                double *_esLagB3 = (double*) calloc((size_t)_ns * _np, sizeof(double));
                double *_esFQB3 = (double*) calloc((size_t)_ns * _np2, sizeof(double));
                double *_esDurQB3 = (double*) calloc((size_t)_ns * _np2, sizeof(double));
                if (_esLagB3 != NULL && _esFQB3 != NULL && _esDurQB3 != NULL) {
                  dLagEs(id, xout, yp, _esLagB3);
                  dFQEs(id, xout, yp, _esFQB3);
                  dDurQEs(id, xout, yp, _esDurQB3);
                  for (int _p = 0; _p < _np; _p++) {
                    double _esDLagP = _esLagB3[cmt * _np + _p];
                    if (_esDLagP == 0.0) continue;
                    for (int _q = 0; _q < _np2; _q++) {
                      double _esFQq3 = _esFQB3[cmt * _np2 + _q];
                      double _esDurQq3 = _esDurQB3[cmt * _np2 + _q];
                      double _esDRateQq3 = (_esAmt * _esFQq3 + tmp * _esDurQq3) / _esDur;
                      int _c2cmt3 = _rxEsUseCalcJac ?
                        (_ns * (1 + _np) + cmt + _ns * (_p * _np2 + _q)) :
                        (_ns * (1 + _np) + cmt + _ns * (_p + _q * _np));
                      yp[_c2cmt3] += -_esDRateQq3 * _esDLagP;
                    }
                  }
                }
                if (_esLagB3 != NULL) free(_esLagB3);
                if (_esFQB3 != NULL) free(_esFQB3);
                if (_esDurQB3 != NULL) free(_esDurQB3);
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
        // _esInfusionBoundary2ndOrder()'s derivation comment. For
        // MODEL_DUR_OFF on ODE models, tau2 = tau1(alag) + dur(F,dur-params)
        // can shift with BOTH alag- and dur-driving parameters
        // simultaneously, so the combined block below (using dtau2/dp =
        // dLag_p+dDur_p etc.) supersedes this call entirely for that case --
        // calling both would double-count the pure-alag-alag corner. The
        // combined block is ODE-only (needs dydtEs), so still fall back to
        // this generic (alag-only) helper for matExp/indLin MODEL_DUR_OFF
        // events -- better a documented alag-only-corner result than none at
        // all (matches prior behavior for those models; skipping here
        // regressed test-mexp-nonmem.R's S^{tlag,tlag} check). Also still
        // fire it for MODEL_RATE_OFF, whose own dur-analog
        // (tau2=tau1+amt/rate(p)) moving-boundary term is still deferred.
        if (ind->whI != EVIDF_MODEL_DUR_OFF || _rxEsUseCalcJac) {
          _esInfusionBoundary2ndOrder(id, xout, yp, cmt, _ns, neq, tmp, _esDydtPreB2);
        }
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
            // Second-order continuous-forcing REMOVAL: exact mirror of the
            // MODEL_DUR_ON case's own addition (same quotient-rule formula,
            // subtracted instead of added -- see that case's derivation
            // comment).
            if (d2FEs != NULL && d2DurEs != NULL && dFQEs != NULL && dDurQEs != NULL &&
                _rxEsNParam2 > 0 &&
                _ns * (1 + _np) + _ns * _np * _rxEsNParam2 <= neq) {
              int _np2 = _rxEsNParam2;
              double *_esD2FB = (double*) calloc((size_t)_ns * _np * _np2, sizeof(double));
              double *_esD2DurB = (double*) calloc((size_t)_ns * _np * _np2, sizeof(double));
              double *_esFQB = (double*) calloc((size_t)_ns * _np2, sizeof(double));
              double *_esDurQB = (double*) calloc((size_t)_ns * _np2, sizeof(double));
              if (_esD2FB != NULL && _esD2DurB != NULL && _esFQB != NULL && _esDurQB != NULL) {
                d2FEs(id, xout, yp, _esD2FB);
                d2DurEs(id, xout, yp, _esD2DurB);
                dFQEs(id, xout, yp, _esFQB);
                dDurQEs(id, xout, yp, _esDurQB);
                double _esDur2 = _esDur * _esDur;
                for (int _p = 0; _p < _np; _p++) {
                  double _esDFp = _esFB[cmt * _np + _p];
                  double _esDDurp = _esB[cmt * _np + _p];
                  for (int _q = 0; _q < _np2; _q++) {
                    double _esD2Fpq = _esD2FB[cmt * (_np * _np2) + _p * _np2 + _q];
                    double _esD2Durpq = _esD2DurB[cmt * (_np * _np2) + _p * _np2 + _q];
                    double _esFQq = _esFQB[cmt * _np2 + _q];
                    double _esDurQq = _esDurQB[cmt * _np2 + _q];
                    double _esTerm = _esAmt * _esD2Fpq / _esDur
                      - _esAmt * (_esDFp * _esDurQq + _esFQq * _esDDurp) / _esDur2
                      + tmp * _esD2Durpq / _esDur
                      - 2.0 * tmp * _esDDurp * _esDurQq / _esDur2;
                    int _c2 = _rxEsUseCalcJac ?
                      (_ns * (1 + _np) + cmt + _ns * (_p * _np2 + _q)) :
                      (_ns * (1 + _np) + cmt + _ns * (_p + _q * _np));
                    InfusionRate[_c2] -= _esTerm;
                  }
                }
              }
              if (_esD2FB != NULL) free(_esD2FB);
              if (_esD2DurB != NULL) free(_esD2DurB);
              if (_esFQB != NULL) free(_esFQB);
              if (_esDurQB != NULL) free(_esDurQB);
            }
          }
          if (_esB != NULL) free(_esB);
          if (_esFB != NULL) free(_esFB);
          // Second-order DUR-boundary moving-boundary term, COMBINED across
          // both mechanisms that can shift tau2 = tau1(alag) + dur(F,dur):
          // alag shifting tau1, and dur/F shifting dur directly. These are
          // NOT separable into independent per-mechanism Leibniz terms --
          // both parts of dtau2/dp = dLag_p + dDur_p (and dtau2/dq =
          // dLagQ_q + dDurQ_q, d2tau2/dpdq = d2Lag[p][q] + d2Dur[p][q]) must
          // be SUMMED before use in the "jump condition one level up"
          // derivation (an earlier attempt handling alag and dur separately
          // -- one via _esInfusionBoundary2ndOrder(), one via a dur-only
          // Leibniz term guarded to skip alag-driving p -- either
          // cross-contaminated g+/g- or silently dropped the genuine
          // dLag_p-driven contribution to the product-rule term; confirmed
          // wrong/incomplete by FD on an alag+modeled-dur-only model with no
          // F, S^{tlag,tinf}; a SECOND bug of the same kind was later found
          // via a full F+dur+alag model's S^{tlag,doseAmt}, tracked to
          // dRateQq's dur-term sign below not matching the already-
          // validated 1st-order d(rate)/dp=(amt*dF+tmp*dDur)/dur formula --
          // it happened to cancel out for pure-dur q but flipped the answer
          // for pure-F q). tmp (=-rate, i.e. rate=-tmp) is parameter-
          // dependent (rate=F*amt/dur), contributing an extra d(rate)/dq
          // term (dRateQq, matching the SAME sign convention as the
          // validated 1st-order formula) on top of the Leibniz term:
          //   dtau2dp = dLag_p[cmt] + dDur_p[cmt]
          //   dtau2dq = dLagQ_q[cmt] + dDurQ_q[cmt]
          //   dRateQq = (amt*dFQ_q + tmp*dDurQ_q)/dur     (dur/F only; rate
          //             has no direct alag dependence)
          //   [S^{p,q}]_cmt += dRateQq*dtau2dp - tmp*(d2Lag[p][q]+d2Dur[p][q])
          //   [S^{p,q}]_k   += -dtau2dq * (g+_k - g-_k)      for ALL k
          // Validated by FD to ~1e-10 on an alag+modeled-dur-only isolate
          // (both compartments, through and after the boundary) AND on the
          // full alag+dur+F IMAX model's S^{doseAmt,tinf}, S^{tinf,tinf},
          // S^{doseAmt,doseAmt}, S^{tlag,tinf}, S^{tlag,doseAmt} pairs.
          // (same "kink whose location moves with q" extension to coupled
          // compartments as _esInfusionBoundary2ndOrder()'s own k!=cmt
          // piece). g_k = dS^p_k/dt via dydtEs, sampled AFTER all of this
          // event's forcing/first-order-jump updates (so S^p's own jump,
          // e.g. from a p that drives alag, is correctly reflected in g+ --
          // that inclusion is NOT contamination, it's exactly what the
          // envelope-theorem derivation calls for). This block supersedes
          // _esInfusionBoundary2ndOrder() entirely for MODEL_DUR_OFF (see
          // the guard added at that call site) -- reduces to the pure-alag
          // formula when dDur_p=dDurQ_q=d2Dur[p][q]=0, so there is no
          // double-count. ODE models only for now.
          if (!_rxEsUseCalcJac && dydtEs != NULL && _esDydtPreB2 != NULL &&
              d2DurEs != NULL && dDurQEs != NULL && dFQEs != NULL &&
              _rxEsNParam2 > 0 &&
              _ns * (1 + _np) + _ns * _np * _rxEsNParam2 <= neq) {
            int _np2 = _rxEsNParam2;
            double *_esD2DurB2 = (double*) calloc((size_t)_ns * _np * _np2, sizeof(double));
            double *_esDurQB2 = (double*) calloc((size_t)_ns * _np2, sizeof(double));
            double *_esFQB2 = (double*) calloc((size_t)_ns * _np2, sizeof(double));
            double *_esDydtPostB2 = (double*) calloc((size_t)neq, sizeof(double));
            double *_esDurB2 = (double*) calloc((size_t)_ns * _np, sizeof(double));
            // alag contribution to dtau2/dp, dtau2/dq, d2tau2/dpdq -- NULL
            // (treated as all-zero) when the model has no alag() tables.
            double *_esLagB2 = (d2LagEs != NULL && dLagQEs != NULL && dLagEs != NULL) ?
              (double*) calloc((size_t)_ns * _np, sizeof(double)) : NULL;
            double *_esLagQB2 = (_esLagB2 != NULL) ?
              (double*) calloc((size_t)_ns * _np2, sizeof(double)) : NULL;
            double *_esD2LagB2 = (_esLagB2 != NULL) ?
              (double*) calloc((size_t)_ns * _np * _np2, sizeof(double)) : NULL;
            if (_esLagB2 != NULL && _esLagQB2 != NULL && _esD2LagB2 != NULL) {
              dLagEs(id, xout, yp, _esLagB2);
              dLagQEs(id, xout, yp, _esLagQB2);
              d2LagEs(id, xout, yp, _esD2LagB2);
            }
            if (_esD2DurB2 != NULL && _esDurQB2 != NULL && _esFQB2 != NULL &&
                _esDydtPostB2 != NULL && _esDurB2 != NULL) {
              d2DurEs(id, xout, yp, _esD2DurB2);
              dDurQEs(id, xout, yp, _esDurQB2);
              dFQEs(id, xout, yp, _esFQB2);
              dDurEs(id, xout, yp, _esDurB2);
              int _esNjD[2]; _esNjD[0] = neq; _esNjD[1] = id;
              dydtEs(_esNjD, xout, yp, _esDydtPostB2);
              for (int _p = 0; _p < _np; _p++) {
                double _esDDurP = _esDurB2[cmt * _np + _p];
                double _esDLagP = (_esLagB2 != NULL) ? _esLagB2[cmt * _np + _p] : 0.0;
                double _esDTau2dp = _esDLagP + _esDDurP;
                for (int _q = 0; _q < _np2; _q++) {
                  double _esD2DurPQ = _esD2DurB2[cmt * (_np * _np2) + _p * _np2 + _q];
                  double _esD2LagPQ = (_esD2LagB2 != NULL) ?
                    _esD2LagB2[cmt * (_np * _np2) + _p * _np2 + _q] : 0.0;
                  double _esDurQq = _esDurQB2[cmt * _np2 + _q];
                  double _esLagQq = (_esLagQB2 != NULL) ? _esLagQB2[cmt * _np2 + _q] : 0.0;
                  double _esDTau2dq = _esLagQq + _esDurQq;
                  double _esFQq = _esFQB2[cmt * _np2 + _q];
                  double _esDRateQq = (_esAmt * _esFQq + tmp * _esDurQq) / _esDur;
                  int _c2cmt = _ns * (1 + _np) + cmt + _ns * (_p + _q * _np);
                  yp[_c2cmt] += _esDRateQq * _esDTau2dp - tmp * (_esD2LagPQ + _esD2DurPQ);
                  if (_esDTau2dq != 0.0) {
                    for (int _esK = 0; _esK < _ns; _esK++) {
                      double _esGdiff = _esDydtPostB2[_ns + _p * _ns + _esK] -
                        _esDydtPreB2[_ns + _p * _ns + _esK];
                      int _c2 = _ns * (1 + _np) + _esK + _ns * (_p + _q * _np);
                      yp[_c2] += -_esGdiff * _esDTau2dq;
                    }
                  }
                }
              }
            }
            if (_esLagB2 != NULL) free(_esLagB2);
            if (_esLagQB2 != NULL) free(_esLagQB2);
            if (_esD2LagB2 != NULL) free(_esD2LagB2);
            if (_esD2DurB2 != NULL) free(_esD2DurB2);
            if (_esDurQB2 != NULL) free(_esDurQB2);
            if (_esFQB2 != NULL) free(_esFQB2);
            if (_esDydtPostB2 != NULL) free(_esDydtPostB2);
            if (_esDurB2 != NULL) free(_esDurB2);
          }
        }
      }
      if (_esDydtPreB2 != NULL) free(_esDydtPreB2);
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
          // after the states and the first-order sens block -- for ordinary
          // ODE models. matExp models use their OWN (p-outer, q-inner) layout
          // instead (rxSensMatExp() declares `cmt()`s nested `for p in
          // calcSens { for q in calcSens2 { for state ... } }`, see the
          // dtau-row comment earlier in this file). This unconditionally used
          // the ODE formula for BOTH model types until 2026-07-01: harmless
          // when calcSens2==calcSens (d2F[i2][i3] is symmetric, so the two
          // layouts are just a transpose of each other and every written
          // value still lands on a slot expecting that same value by
          // coincidence) but a REAL bug once calcSens2 is a PROPER SUBSET
          // with FEWER elements than calcSens (i2 ranges further than np2-1,
          // so `i2 + i3*np` is not even a valid transposed-pair index
          // anymore) -- confirmed by FD: S^{tf,tf} and S^{ka,tlag} came out
          // completely uncorrelated with the ODE reference (calcSens=
          // c(tlag,tf,ka), calcSens2=c(tlag,tf)) while pairs that happened to
          // still coincide (S^{tlag,tlag}, S^{ka,tf}) matched fine, and even
          // some genuinely-scrambled pairs (S^{tlag,tf}/S^{tf,tlag}) looked
          // right ONLY because d2F's own value is symmetric under exchanging
          // i2<->i3 too -- a doubly-coincidental false negative that a
          // calcSens2==calcSens test alone would never catch.
          if (d2FEs != NULL && _rxEsNParam2 > 0 &&
              _ns * (1 + _np) + _ns * _np * _rxEsNParam2 <= neq) {
            int _np2 = _rxEsNParam2;
            double *_d2FB = (double*) calloc((size_t)_ns * _np * _np2, sizeof(double));
            if (_d2FB != NULL) {
              d2FEs(id, xout, yp, _d2FB);
              for (int _i2 = 0; _i2 < _np; _i2++) {
                for (int _i3 = 0; _i3 < _np2; _i3++) {
                  int _c2 = _rxEsUseCalcJac ?
                    (_ns * (1 + _np) + cmt + _ns * (_i2 * _np2 + _i3)) :
                    (_ns * (1 + _np) + cmt + _ns * (_i2 + _i3 * _np));
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
          // after the states, first-order, and second-order sens blocks --
          // ODE models. matExp's own layout is p-outer/q-middle/r-inner
          // (i2*(np2*np3) + i3*np3 + i4), same subset-vs-equal caveat as the
          // 2nd-order fix above (rxSensMatExp() declares 3rd-order `cmt()`s
          // nested `for p { for q { for r { for state ... } } }`).
          if (d3FEs != NULL && _rxEsNParam3 > 0 && d2FEs != NULL && _rxEsNParam2 > 0 &&
              _ns * (1 + _np) + _ns * _np * _rxEsNParam2 + _ns * _np * _rxEsNParam2 * _rxEsNParam3 <= neq) {
            int _np2 = _rxEsNParam2, _np3 = _rxEsNParam3;
            double *_d3FB = (double*) calloc((size_t)_ns * _np * _np2 * _np3, sizeof(double));
            if (_d3FB != NULL) {
              d3FEs(id, xout, yp, _d3FB);
              for (int _i2 = 0; _i2 < _np; _i2++) {
                for (int _i3 = 0; _i3 < _np2; _i3++) {
                  for (int _i4 = 0; _i4 < _np3; _i4++) {
                    int _c3 = _rxEsUseCalcJac ?
                      (_ns * (1 + _np) + _ns * _np * _np2 + cmt +
                       _ns * (_i2 * (_np2 * _np3) + _i3 * _np3 + _i4)) :
                      (_ns * (1 + _np) + _ns * _np * _np2 + cmt +
                       _ns * (_i2 + _np * _i3 + _np * _np2 * _i4));
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
