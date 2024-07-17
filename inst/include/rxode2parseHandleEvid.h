// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: nil; -*-
#ifndef __RXODE2PARSEHANDLEVID_H___
#define __RXODE2PARSEHANDLEVID_H___

#include "rxode2parse.h"
//#include "rxThreadData.h"
#define rxErrCorruptETSort    1
#define rxErrRate0            2
#define rxErrModelRateAbsent  4
#define rxErrCorruptETSort2   8
#define rxErrDurNeg0          16
#define rxErrModelDurAbsent   32
#define rxErrModelData686     64
#define rxErrModelDataNeg6    128
#define rxErrModelDataErr8    256
#define rxErrModelDataErr886  512
#define rxErrModelDataErr797  1024
#define rxErrModelDataNeg7    2048
#define rxErrModelDataErr9    4096
#define rxErrModelDataErr997  8192
#define rxErrCorruptETSort3   16384
#define rxErrCorruptET        32768
#define rxErrNegCmt           65536
#define rxErrCorruptET2       131072
#define rxErrSync             262144
#define rxErrSync2            524288
#define rxErrModeledFss2      1048576
#define rxErrModeledFss2n2    2097152
#define rxErrModeledFss2n3    4194304
#define rxErrRate02           8388608

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

#define EVID_EXTRA_SIZE 10
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

static inline void getWh(int evid, int *wh, int *cmt, int *wh100, int *whI, int *wh0){
  *wh = evid;
  *cmt = 0;
  *wh100 = FLOOR(*wh/1e5L);
  *whI   = FLOOR(*wh/1e4L-*wh100*10);
  *wh    = *wh - *wh100*1e5 - (*whI-1)*1e4;
  *wh0 = FLOOR((*wh%10000)/100);
  *cmt = *wh0 - 1 + *wh100*100;
  *wh0 = evid - *wh100*1e5 - *whI*1e4 - *wh0*100;
  if (rx_global.linNcmt != 0) {
    if (rx_global.linKa) {
      switch (*cmt) {
      case 0:
        *cmt = op_global.neq;
        break;
      case 1:
        *cmt = op_global.neq+1;
        break;
      case 2:
        *cmt -= 2;
        break;
      }
    } else {
      if (*cmt == 0) {
        *cmt = op_global.neq;
      } else {
        *cmt -= 1;
      }
    }
  }
}

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
																											 rx_solve *rx, rx_solving_options *op,
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
						// dur = getTime_(ind->idose[infEixds], ind);// -
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
      tinf[0] = _getDur(ind->ixds, ind, 2, &p);
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
  if (op->neq + op->extraCmt != 0 && ind->tlast != _time &&
      isDose(evid) &&
      ind->cmt < op->neq + op->extraCmt) {
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
  if (ind->ix[ind->idx] != ind->idose[ind->ixds]) {
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


extern t_F AMT;


static inline double getAmt(rx_solving_options_ind *ind, int id, int cmt, double dose, double t, double *y) {
  double ret = AMT(id, cmt, dose, t, y);
  if (ISNA(ret)){
    rx_solving_options *op = &op_global;
    op->badSolve=1;
    op->naTime = 1;
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
    int *tmpI = (int*)realloc(ind->ignoredDoses, (ind->ignoredDosesN[0]+1+EVID_EXTRA_SIZE)*sizeof(int));
    if (tmpI == NULL) {
      rx_solving_options *op = &op_global;
      op->badSolve = 1;
      return 0;
    }
    ind->ignoredDoses = tmpI;
    ind->ignoredDosesAllocN[0] = (ind->ignoredDosesN[0]+1+EVID_EXTRA_SIZE);
    re = 1;
  }
  ind->ignoredDoses[ind->ignoredDosesN[0]] = doseIdx;
  ind->ignoredDosesN[0] = ind->ignoredDosesN[0]+1;
  return re;
}

static inline int pushPendingDose(int doseIdx, rx_solving_options_ind *ind) {
  int re = 0;
  if (ind->pendingDosesN[0]+1 >= ind->pendingDosesAllocN[0]) {
    int *tmpI = (int*)realloc(ind->pendingDoses, (ind->pendingDosesN[0]+1+EVID_EXTRA_SIZE)*sizeof(int));
    if (tmpI == NULL) {
      rx_solving_options *op = &op_global;
      op->badSolve = 1;
      return 0;
    }
    ind->pendingDoses = tmpI;
    ind->pendingDosesAllocN[0] = (ind->pendingDosesN[0]+1+EVID_EXTRA_SIZE);
    re = 1;
  }
  ind->pendingDoses[ind->pendingDosesN[0]] = doseIdx;
  ind->pendingDosesN[0] = ind->pendingDosesN[0]+1;
  return re;
}


static inline int pushDosingEvent(double time, double amt, int evid,
                                   rx_solving_options_ind *ind) {
  int re = 0;
  if (ind->extraDoseN[0]+1 >= ind->extraDoseAllocN[0]) {
    int *tmpI = (int*)realloc(ind->extraDoseTimeIdx, (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE)*sizeof(int));
    if (tmpI == NULL) {
      rx_solving_options *op = &op_global;
      op->badSolve = 1;
      return 0;
    }
    ind->extraDoseTimeIdx = tmpI;

    tmpI = (int*)realloc(ind->extraDoseEvid, (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE)*sizeof(int));
    if (tmpI == NULL) {
      rx_solving_options *op = &op_global;
      op->badSolve = 1;
      return 1;
    }
    ind->extraDoseEvid = tmpI;

    double * tmpD = (double*)realloc(ind->extraDoseTime, (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE)*sizeof(double));
    if (tmpD == NULL) {
      rx_solving_options *op = &op_global;
      op->badSolve = 1;
      return 1;
    }
    ind->extraDoseTime = tmpD;

    tmpD = (double*)realloc(ind->extraDoseDose,  (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE)*sizeof(double));
    if (tmpD == NULL) {
      rx_solving_options *op = &op_global;
      op->badSolve = 1;
      return 1;
    }
    ind->extraDoseDose = tmpD;

    ind->extraDoseAllocN[0] = (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE);
    re = 1;
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
    int *tmpI = (int*)realloc(ind->extraDoseTimeIdx, (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE)*sizeof(int));
    if (tmpI == NULL) {
      rx_solving_options *op = &op_global;
      op->badSolve = 1;
      return 0;
    }
    ind->extraDoseTimeIdx = tmpI;

    tmpI = (int*)realloc(ind->extraDoseEvid, (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE)*sizeof(int));
    if (tmpI == NULL) {
      rx_solving_options *op = &op_global;
      op->badSolve = 1;
      return 1;
    }
    ind->extraDoseEvid = tmpI;

    double * tmpD = (double*)realloc(ind->extraDoseTime, (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE)*sizeof(double));
    if (tmpD == NULL) {
      rx_solving_options *op = &op_global;
      op->badSolve = 1;
      return 1;
    }
    ind->extraDoseTime = tmpD;

    tmpD = (double*)realloc(ind->extraDoseDose,  (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE)*sizeof(double));
    if (tmpD == NULL) {
      rx_solving_options *op = &op_global;
      op->badSolve = 1;
      return 1;
    }
    ind->extraDoseDose = tmpD;

    ind->extraDoseAllocN[0] = (ind->extraDoseN[0]+1+EVID_EXTRA_SIZE);
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
  }//  else if (!ind->doSS) {
  //   REprintf("handle evid %d dose at time %f is value %f (ind->ixds: %d; ind->idx: %d)\n",
  //            evid, xout, getDoseIndex(ind, ind->idx), ind->ixds, ind->idx);
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
    rx_solving_options *op = &op_global;
    //if (syncIdx(ind) == 0) return 0;
    if (ind->wh0 == EVID0_OFF) {
      yp[cmt]=op_global.inits[cmt];
      InfusionRate[cmt] = 0;
      ind->cacheME=0;
      ind->on[cmt] = 0;
      return 1;
    }
    if (!ind->doSS && (ind->wh0 == EVID0_SS2 || ind->wh0 == EVID0_SS20) && cmt < op->neq) {
      // Save for adding at the end; Only for ODE systems
      memcpy(ind->solveSave, yp, op->neq*sizeof(double));
    }
    switch(ind->whI) {
    case EVIDF_MODEL_RATE_ON: // modeled rate.
    case EVIDF_MODEL_DUR_ON: // modeled duration.
      // Rate already calculated and saved in the next dose record
      if (ind->wh0 != EVID0_SS0 &&
          ind->wh0 != EVID0_SS20) {
        ind->on[cmt] = 1;
        ind->cacheME = 0;
        InfusionRate[cmt] -= getDoseIndexPlus1(ind, ind->idx);
        // if (ind->wh0 != EVID0_SS2 &&
        //     ind->wh0 != EVID0_SS) {
        //   int infEixds = ind->ixds;
        //   if (infEixds > 0) {
        //     pushPendingDose(infEixds+1, ind);
        //   } else {
        //     pushPendingDose(infEixds-1, ind);
        //   }
        // }
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
      InfusionRate[cmt] += getDoseIndex(ind, ind->idx);
      ind->cacheME=0;
      if (ind->wh0 == EVID0_SS2 &&
          getAmt(ind, id, cmt, getDoseIndex(ind, ind->idx), xout, yp) !=
          getDoseIndex(ind, ind->idx)) {
        if (!(ind->err & 2097152)){
          ind->err += 2097152;
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
        // if (ind->wh0 != EVID0_SS2 &&
        //     ind->wh0 != EVID0_SS) {
        //   int infEixds;
        //   handleInfusionGetEndOfInfusionIndex(ind->ixds, &infEixds, &rx_global, op, ind);
        //   pushPendingDose(infEixds, ind);
        // }
      }
      tmp = getAmt(ind, id, cmt, tmp, xout, yp);
      InfusionRate[cmt] += tmp;
      ind->cacheME=0;
      if (ind->wh0 == EVID0_SS2 && tmp != getDoseIndex(ind, ind->idx)) {
        if (!(ind->err & 4194304)){
          ind->err += 4194304;
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
        // if (ind->wh0 != EVID0_SS2 &&
        //     ind->wh0 != EVID0_SS) {
        //   int infEixds;
        //   handleInfusionGetEndOfInfusionIndex(ind->ixds, &infEixds, &rx_global, op, ind);
        //   pushPendingDose(infEixds, ind);
        // }
      }
      // if (!ind->doSS) {
      //   REprintf("infusion dose at %f is %f ind->ixds: %d\n", xout, tmp, ind->ixds);
      // }
      InfusionRate[cmt] += tmp;
      ind->cacheME=0;
      if (ind->wh0 == EVID0_SS2 && getDoseIndex(ind, ind->idx) > 0 &&
          getAmt(ind, id, cmt, getDoseIndex(ind, ind->idx), xout, yp) !=
          getDoseIndex(ind, ind->idx)) {
        if (!(ind->err & 4194304)){
          ind->err += 4194304;
        }
      }
      break;
    case EVIDF_REPLACE: // replace
      ind->on[cmt] = 1;
      yp[cmt] = getAmt(ind, id, cmt, getDoseIndex(ind, ind->idx), xout, yp);     //dosing before obs
      break;
    case EVIDF_MULT: //multiply
      ind->on[cmt] = 1;
      yp[cmt] *= getAmt(ind, id, cmt, getDoseIndex(ind, ind->idx), xout, yp);     //dosing before obs
      break;
    case EVIDF_NORMAL:
      ind->on[cmt] = 1;
      if (ind->wh0 != EVID0_PHANTOM) {
        yp[cmt] += getAmt(ind, id, cmt, getDoseIndex(ind, ind->idx), xout, yp);     //dosing before obs
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
  return handle_evid(getEvid(ind, ind->ix[ind->idx]), neq[0] + op->extraCmt,
										 ind->BadDose, ind->InfusionRate, ind->dose, yp,
										 *xout, neq[1], ind);
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
