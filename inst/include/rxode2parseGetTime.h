// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: nil; -*-
#ifndef __RXODE2PARSEGETTTIME_H__
#define __RXODE2PARSEGETTTIME_H__
#if defined(__cplusplus)

#include "rxode2parse.h"

extern t_F AMT;
extern t_LAG LAG;
extern t_RATE RATE;
extern t_DUR DUR;
extern t_calc_mtime calc_mtime;

#ifndef __DOINIT__

#define returnBadTime(time)                         \
  if (ISNA(time)) {                                 \
    switch (op->naTimeInput) {                      \
    case rxode2naTimeInputWarn:                     \
      op->naTimeInputWarn=1;                        \
    case rxode2naTimeInputIgnore:                   \
      return time;                                  \
      break;                                        \
    case rxode2naTimeInputError:                    \
      Rf_errorcall(R_NilValue, "supplied NA time"); \
      break;                                        \
    }                                               \
  }


static inline double getLag(rx_solving_options_ind *ind, int id, int cmt, double time) {
  rx_solving_options *op = &op_global;
  returnBadTime(time);
  if (ind->wh0 == EVID0_SS0 || ind->wh0 == EVID0_SS20) {
    return time;
  }
  double ret = LAG(id, cmt, time);
  if (ISNA(ret)) {
    op->badSolve=1;
    op->naTime = 1;
  }
  return ret;
}

static inline double getRate(rx_solving_options_ind *ind, int id, int cmt, double dose, double t){
  rx_solving_options *op = &op_global;
  returnBadTime(t);
  double ret = RATE(id, cmt, dose, t);
  if (ISNA(ret)){
    op->badSolve=1;
    op->naTime = 1;
  }
  return ret;
}

static inline double getDur(rx_solving_options_ind *ind, int id, int cmt, double dose, double t){
  rx_solving_options *op = &op_global;
  returnBadTime(t);
  if (ISNA(t)) return t;
  double ret = DUR(id, cmt, dose, t);
  if (ISNA(ret)){
    op->badSolve=1;
    op->naTime = 1;
  }
  return ret;
}


static inline int isEvidType(int evid, int type) {
  int wh, cmt, wh100, whI, wh0;
  getWh(evid, &wh, &cmt, &wh100, &whI, &wh0);
  return (whI == type);
}

#define isEvidModeledDurationStart(evid) isEvidType(evid, EVIDF_MODEL_DUR_ON)
#define isEvidModeledDurationStop(evid) isEvidType(evid, EVIDF_MODEL_DUR_OFF)
#define isEvidModeledRateStart(evid) isEvidType(evid, EVIDF_MODEL_RATE_ON)
#define isEvidModeledRateStop(evid) isEvidType(evid, EVIDF_MODEL_RATE_OFF)

static inline void updateDur(int idx, rx_solving_options_ind *ind, double *yp){
  double t = getAllTimes(ind, idx);
  double dur, amt;
  // The duration and f cannot depend on state values
  int oldIdx = ind->idx;
  ind->idx = idx;
  amt  = getAmt(ind, ind->id, ind->cmt, getDose(ind, idx), t, yp);
  dur  = getDur(ind, ind->id, ind->cmt, amt, t);
  ind->idx = oldIdx;
  if (dur > 0) {
    setDoseP1(ind, idx, -amt/dur);
    setAllTimesP1(ind, idx, t+dur);
  } else {
    rx_solve *rx = &rx_global;
    rx_solving_options *op = &op_global;
    if (ind->cmt < op->neq){
      if (rx->needSort & 4){
        if (!(ind->err & 16)){
          ind->err += 16;
        }
        return;
      } else {
        if (!(ind->err & 32)){
          ind->err += 32;
        }
        return;
      }
    }
  }
}

static inline void updateRate(int idx, rx_solving_options_ind *ind, double *yp) {
  double t = getAllTimes(ind, idx);
  int oldIdx = ind->idx;
  ind->idx=idx;
  double dur, rate, amt;
  amt  = getAmt(ind, ind->id, ind->cmt, getDose(ind,idx), t, yp);
  rate  = getRate(ind, ind->id, ind->cmt, amt, t);
  if (rate > 0){
    dur = amt/rate; // mg/hr
    setDoseP1(ind, idx, -rate);
    setAllTimesP1(ind, idx, t+dur);
    ind->idx=oldIdx;
  } else {
    rx_solve *rx;
    rx = &rx_global;
    rx_solving_options *op = &op_global;
    if (ind->cmt < op->neq){
      if (rx->needSort & 8){
        if (!(ind->err & 2)){
          ind->err += 2;
        }
        return;
      } else {
        // FIXME don't error out with linear compartmental model
        if (!(ind->err & 4)){
          ind->err += 4;
        }
        return;
      }
    }
  }
  ind->idx=oldIdx;
}

static inline void handleTurnOffModeledDuration(int idx, rx_solve *rx, rx_solving_options *op, rx_solving_options_ind *ind) {
  if (idx > 0){
    if (!isEvidModeledDurationStart(getEvidM1(ind, idx))) {
      if (!(ind->err & 64)){
        ind->err += 64;
      }
      return;
    }
  } else {
    if (!(ind->err & 128)){
      ind->err += 128;
    }
    return;
  }
}

static inline void handleTurnOnModeledDuration(int idx, rx_solve *rx, rx_solving_options *op, rx_solving_options_ind *ind) {
  // This calculates the rate and the duration and then assigns it to the next record
  if (idx >= ind->n_all_times){
    // error: Last record, can't be used.
    if (!(ind->err & 256)){
      ind->err += 256;
    }
    return;
  } else {
    if (!isEvidModeledDurationStop(getEvidP1(ind, idx))) {
      if (!(ind->err & 512)){
        ind->err += 512;
      }
      return;
    }
    updateDur(idx, ind, rx->ypNA);
  }
}

static inline void handleTurnOffModeledRate(int idx, rx_solve *rx, rx_solving_options *op, rx_solving_options_ind *ind) {
  if (idx > 0){
    if (!isEvidModeledRateStart(getEvidM1(ind, idx))) {
      if (!(ind->err & 1024)){
        ind->err += 1024;
      }
      return;
    }
  } else {
    if (!(ind->err & 2048)){
      ind->err += 2048;
    }
    return;
  }
}

static inline void handleTurnOnModeledRate(int idx, rx_solve *rx, rx_solving_options *op, rx_solving_options_ind *ind) {
  // This calculates the rate and the duration and then assigns it to the next record
  if (idx >= ind->n_all_times){
    // error: Last record, can't be used.
    if (!(ind->err & 4096)){
      ind->err += 4096;
    }
    /* Rf_errorcall(R_NilValue, "Data Error 9\n"); */
    return;
  } else {
    if (!isEvidModeledRateStop(getEvidP1(ind, idx))) {
      if (!(ind->err & 8192)){
        ind->err += 8192;
      }
      return;
    }
    setAllTimesP1(ind, idx, getAllTimes(ind, idx));
    updateRate(idx, ind, rx->ypNA);
  }
}

static inline int handleInfusionStartRm(int *startIdx, int *endIdx,
                                        double *amt, int *idx,
                                        rx_solve *rx, rx_solving_options *op,
                                        rx_solving_options_ind *ind) {
  if (ind->wh0 == EVID0_INFRM) {
    // This is a possible removal event.  Look at the next duration
    int curEvid = getEvid(ind, ind->idose[*endIdx+1]);
    *startIdx = *endIdx+1;
    for (*endIdx = *startIdx; *endIdx < ind->ndoses; ++(*endIdx)) {
      if (getEvid(ind, ind->idose[*startIdx]) == getEvid(ind, ind->idose[*endIdx])) break;
      if (*endIdx == ind->ndoses-1) {
        //REprintf("curEvid@infrm: %d\n", curEvid);
        if (!(ind->err & 32768)){
          ind->err += 32768;
        }
        return 1;
      }
    }
    return 1;
  }
  return 0;
}

static inline int handleInfusionStartDefault(int *startIdx, int *endIdx,
                                             double *amt, int *idx,
                                             rx_solve *rx, rx_solving_options *op,
                                             rx_solving_options_ind *ind) {
  // This finds the duration based on the end of infusion
  int curEvid = getEvid(ind, ind->idose[*endIdx]);
  int jj = 0;
  for (*startIdx = 0; *startIdx < ind->ndoses; (*startIdx)++) {
    if (getEvid(ind, ind->idose[*startIdx]) == curEvid &&
        getDose(ind, ind->idose[*startIdx]) == -(*amt)) {
      // This will look after the last known infusion stopping point
      if (jj == 0) {
        jj = *startIdx;
      } else {
        jj++;
      }
      for (; jj < ind->ndoses; jj++) {
        if (getEvid(ind, ind->idose[jj]) == curEvid &&
            getDose(ind, ind->idose[jj]) == *amt) {
          break;
        }
      }
      if (jj == *endIdx) break;
    }
  }
  if (*startIdx == ind->ndoses) {
    //REprintf("cant match: %d\n", curEvid);
    if (!(ind->err & 32768)){
      ind->err += 32768;
    }
    return 1;
  }
  return 1;
}

static inline void handleInfusionGetStartOfInfusionIndex(int *startIdx, int *endIdx,
                                                         double *amt, int *idx,
                                                         rx_solve *rx, rx_solving_options *op,
                                                         rx_solving_options_ind *ind) {
  if (*amt > 0) {
    *startIdx = -1;
    *endIdx = -1;
    return;
  }
  *endIdx = getDoseNumberFromIndex(ind, *idx);
  if (*endIdx == -1){
    if (!(ind->err & 16384)){
      ind->err += 16384;
    }
    return;
    /* Rf_errorcall(R_NilValue, "Corrupted event table during sort (1)."); */
  }
  handleInfusionStartRm(startIdx, endIdx, amt, idx, rx, op, ind) ||
    handleInfusionStartDefault(startIdx, endIdx, amt, idx, rx, op, ind);
}

static inline double handleInfusionItem(int idx, rx_solve *rx, rx_solving_options *op, rx_solving_options_ind *ind) {
  if (ind->wh0 == EVID0_RATEADJ) {
    return getAllTimes(ind, idx);
  }
  double amt = getDose(ind, idx);
  if (amt > 0) {
    return getLag(ind, ind->id, ind->cmt, getAllTimes(ind, idx));
  } else if (amt < 0) {
    int infEidx = getDoseNumberFromIndex(ind, idx);
    if (infEidx == -1){
      if (!(ind->err & 16384)){
        ind->err += 16384;
      }
      return 0.0;
      /* Rf_errorcall(R_NilValue, "Corrupted event table during sort (1)."); */
    }
    int infBidx;
    handleInfusionGetStartOfInfusionIndex(&infBidx, &infEidx, &amt, &idx, rx, op, ind);
    if (infBidx == -1) return 0.0;
    rx_solve *rx = &rx_global;
    double f = getAmt(ind, ind->id, ind->cmt, 1.0, getAllTimes(ind, ind->idose[infBidx]), rx->ypNA);
    if (ISNA(f)){
      rx_solving_options *op = &op_global;
      op->badSolve=1;
      op->naTime = 1;
    }
    double durOld = (getAllTimes(ind, ind->idose[infEidx]) -
                     getAllTimes(ind, ind->idose[infBidx]));
    double dur = f*durOld;
    // To the correct lag time, we need to make sure the steady state flags are not set...
    int wh0 = ind->wh0;
    int wh, cmt, wh100, whI;
    getWh(getEvid(ind, ind->idose[infBidx]), &wh, &cmt, &wh100, &whI, &(ind->wh0));
    double tB = getLag(ind, ind->id, ind->cmt, getAllTimes(ind, ind->idose[infBidx]));
    ind->wh0 = wh0;
    return tB + dur;
  } else {
    /* Rf_errorcall(R_NilValue, "Corrupted events."); */
    if (!(ind->err & 131072)){
      ind->err += 131072;
    }
    return 0.0;
  }
}

static inline double getTimeCalculateInfusionTimes(int idx, rx_solve *rx, rx_solving_options *op, rx_solving_options_ind *ind) {
  switch(ind->whI){
  case EVIDF_MODEL_DUR_OFF:
    handleTurnOffModeledDuration(idx, rx, op, ind);
    break;
  case EVIDF_MODEL_DUR_ON:
    handleTurnOnModeledDuration(idx, rx, op, ind);
    break;
  case EVIDF_MODEL_RATE_OFF:
    handleTurnOffModeledRate(idx, rx, op, ind);
    break;
  case EVIDF_MODEL_RATE_ON:
    handleTurnOnModeledRate(idx, rx, op, ind);
    break;
  case EVIDF_INF_RATE:
    return handleInfusionItem(idx, rx, op, ind);
    break;
  }
  return getLag(ind, ind->id, ind->cmt, getAllTimes(ind,idx));
}

static inline double getTime__(int idx, rx_solving_options_ind *ind, int update) {
  rx_solving_options *op = &op_global;
  rx_solve *rx = &rx_global;
  int evid = getEvid(ind, idx);
  if (evid == 9) return 0.0;
  if (evid >= 10 && evid <= 99) return ind->mtime[evid-10];
  if (isObs(evid)) return getAllTimes(ind, idx);
  getWh(evid, &(ind->wh), &(ind->cmt), &(ind->wh100), &(ind->whI), &(ind->wh0));
  if (ind->wh0 == EVID0_SSINF ||
      ind->wh0 == EVID0_SS0 ||
      ind->wh0 == EVID0_SS20){
  } else {
    // yp should be the current solve values
    //
    // Before solving the solve will be zero
    // After solving the yp will contain the solved values
    //
    if (update == 0) {
      if (ind->whI == EVIDF_INF_RATE) {
        return handleInfusionItem(idx, rx, op, ind);
      }
    } else {
      return getTimeCalculateInfusionTimes(idx, rx, op, ind);
    }
  }
  return getLag(ind, ind->id, ind->cmt, getAllTimes(ind, idx));
}

static inline double getTime_(int idx, rx_solving_options_ind *ind) {
  return getTime__(idx, ind, 0);
}

static inline int cancelPendingDoses(rx_solving_options_ind *ind, int id) {
  int re = 0;
  for (int i = 0; i < ind->pendingDosesN[0]; ++i) {
    re = pushIgnoredDose(ind->pendingDoses[i], ind) || re;
  }
  ind->pendingDosesN[0] = 0;
  return re;
}

static inline int cancelInfusionsThatHaveStarted(rx_solving_options_ind *ind, int id, double time) {
  int re = 0;
  int wh, cmt, wh100, whI, wh0, evid;
  double amt, curTime;
  for (int i = 0; i < ind->extraDoseN[0]; ++i) {
    int cur = -1-i;
    evid = getEvid(ind, cur);
    // REprintf("Extra dose information (%d):\n", cur);
    // REprintf("\ttime: %f (curTime %f)\n", getAllTimes(ind, cur), time);
    // REprintf("\tevid: %d ", getEvid(ind, cur));
    // REprintf("\tdose: %f \n", getDose(ind, cur));
    // REprintf("================================================================================\n");
    getWh(evid, &wh, &cmt, &wh100, &whI, &wh0);
    if (whI != 1 && whI != 2) {
      // not an infusion
      continue;
    }
    // infusions
    amt = getDose(ind, cur);
    curTime = getAllTimes(ind, cur);
    if (amt > 0) {
      // an infusion start dose
      bool ignore = curTime < time;
      // infusion starts after time;
      // These should still be pending
      if (ignore) {
        // REprintf("ignore!\n");
        re = pushIgnoredDose(cur, ind) || re;
      }
      i++;
      if (i >= ind->extraDoseN[0]) break;
      cur = -1-i;
      double nextAmt = getDose(ind, cur);
      evid = getEvid(ind, cur);
      int nextWhI =0;
      getWh(evid, &wh, &cmt, &wh100, &nextWhI, &wh0);
      // REprintf("pair (%d):\n", cur);
      // REprintf("\ttime: %f (curTime %f)\n", getAllTimes(ind, cur), time);
      // REprintf("\tevid: %d ", evid);
      // REprintf("\tdose: %f \n", nextAmt);
      // REprintf("================================================================================\n");
      if (amt == -nextAmt && whI == nextWhI) {
        if (ignore) {
          // REprintf("ignore!\n");
          re = pushIgnoredDose(cur, ind) || re;
        }
      } else {
        i--;
      }
      continue;
    }
    // turn infusion off. Not sure what to do here. For now keep.
  }
  return re;
}

#undef cancelOrPush
#undef returnBadTime


#endif

extern "C" {
#endif

  double getTime(int idx, rx_solving_options_ind *ind);

#if defined(__cplusplus)
}
#endif
#endif
