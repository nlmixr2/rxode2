// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: nil; -*-
#ifndef __RXODE2PARSEGETTTIME_H__
#define __RXODE2PARSEGETTTIME_H__
#include "needSortDefines.h"
#include "rxode2dataErr.h"
#if defined(__cplusplus)

#include "rxode2parse.h"

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
      (Rf_errorcall)(R_NilValue, "supplied NA time"); \
      break;                                        \
    }                                               \
  }

static inline int getSortedPosition(rx_solving_options_ind *ind, int raw);

// The sorted position of the dose record that the addl repeat at ind->idx
// repeats (ind->addlParent), or -1
static inline int getAddlParentPosition(rx_solving_options_ind *ind) {
  if (ind->addlParent == NULL || ind->idx < 0 || ind->idx >= ind->n_all_times) return -1;
  int raw = ind->ix[ind->idx];
  if (raw < 0 || raw >= ind->n_all_times_orig || ind->addlParent[raw] == 0) return -1;
  return getSortedPosition(ind, raw + ind->addlParent[raw]);
}

static inline double getLag(rx_solving_options_ind *ind, int id, int cmt, double time, double *y) {
  rx_solving_options *op = (ind->op ? ind->op : &op_global);
  returnBadTime(time);
  if (ind->wh0 == EVID0_SS0 || ind->wh0 == EVID0_SS20) {
    return time;
  }
  if (ind->fns == NULL || ind->fns->lag == NULL) return time;
  // an addl repeat that does not keep its covariates evaluates its alag()
  // with the nocb covariates of the dose record it repeats, as NONMEM's ALAG1
  // does
  ind->lagParent = getAddlParentPosition(ind) + 1;
  double ret = ind->fns->lag(id, cmt, time, y);
  ind->lagParent = 0;
  if (ISNA(ret)) {
    int newBadSolve = 1;
#pragma omp atomic write
    op->badSolve = newBadSolve;
    int newNaTime = 1 + 10*cmt;
#pragma omp critical
    { if (op->naTime == 0) op->naTime = newNaTime; }
  }
  return ret;
}

static inline double getRate(rx_solving_options_ind *ind, int id, int cmt, double dose, double t, double *y){
  rx_solving_options *op = (ind->op ? ind->op : &op_global);
  returnBadTime(t);
  if (ind->fns == NULL || ind->fns->rate == NULL) return 0.0;
  double ret = ind->fns->rate(id, cmt, dose, t, y);
  if (ISNA(ret)){
    int newBadSolve = 1;
#pragma omp atomic write
    op->badSolve = newBadSolve;
    int newNaTime = 2 + 10*cmt;
#pragma omp critical
    { if (op->naTime == 0) op->naTime = newNaTime; }
  }
  return ret;
}

static inline double getDur(rx_solving_options_ind *ind, int id, int cmt, double dose, double t, double *y){
  rx_solving_options *op = (ind->op ? ind->op : &op_global);
  returnBadTime(t);
  if (ISNA(t)) return t;
  if (ind->fns == NULL || ind->fns->dur == NULL) return 0.0;
  double ret = ind->fns->dur(id, cmt, dose, t, y);
  if (ISNA(ret)){
    int newBadSolve = 1;
#pragma omp atomic write
    op->badSolve = newBadSolve;
    int newNaTime = 3 + 10*cmt;
#pragma omp critical
    { if (op->naTime == 0) op->naTime = newNaTime; }
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

// Sorted position of data record `raw` (ind->ix is a permutation of the
// records); a covariate update reads ind->idx as a sorted position.  At sort
// time, and in a record order no lag changed, the record sits at its own slot.
static inline int getSortedPosition(rx_solving_options_ind *ind, int raw) {
  if (raw < 0) return raw; // an extra dose is addressed by its negative index
  if (raw < ind->n_all_times && ind->ix[raw] == raw) return raw;
  for (int j = 0; j < ind->n_all_times; ++j) {
    if (ind->ix[j] == raw) return j;
  }
  return raw;
}

// f() of the dose at record `raw` times `dose`, read as NONMEM reads it: at
// the record's data time, with the record's covariates, wherever a lag puts
// the record in the solve (a covariate the record has no value for is read at
// that time)
static inline double getAmtAtRecord(rx_solving_options_ind *ind, int raw,
                                    double dose, double *yp) {
  int oldRec = ind->recCovPos;
  if (raw >= 0) ind->recCovPos = getSortedPosition(ind, raw) + 1;
  double ret = getAmt(ind, ind->id, ind->cmt, dose, getAllTimes(ind, raw), yp);
  ind->recCovPos = oldRec;
  return ret;
}

static inline void updateDur(int idx, rx_solving_options_ind *ind, double *yp){
  double t = getAllTimes(ind, idx);
  double dur, amt;
  // The duration and f cannot depend on state values; f(), dur() and alag()
  // are read at the start's record
  int oldIdx = ind->idx;
  ind->idx = getSortedPosition(ind, idx);
  amt  = getAmtAtRecord(ind, idx, getDose(ind, idx), yp);
  dur  = getDur(ind, ind->id, ind->cmt, amt, t, yp);
  double laggedStart = getLag(ind, ind->id, ind->cmt, t, yp);
  if (ISNA(laggedStart)) laggedStart = t;
  ind->idx = oldIdx;
  if (dur > 0) {
    setDoseP1(ind, idx, -amt/dur);
    setAllTimesP1(ind, idx, laggedStart + dur);
  } else if (dur == 0 && ind->whI == EVIDF_MODEL_DUR_ON) {
    rx_solve *rx = (ind->rx ? ind->rx : &rx_global);
    rx_solving_options *op = (ind->op ? ind->op : &op_global);
    if (rx->needSort & needSortDur) {
      setDoseP1(ind, idx, 0.0);
      setAllTimesP1(ind, idx, laggedStart);
    } else {
      if (!(ind->err & rxErrModelDurAbsent)){
        ind->err += rxErrModelDurAbsent;
      }
      return;
    }
  } else {
    rx_solve *rx = (ind->rx ? ind->rx : &rx_global);
    rx_solving_options *op = (ind->op ? ind->op : &op_global);
    if (ind->cmt < op->neq) {
      if (rx->needSort & needSortDur) {
        if (!(ind->err & rxErrDurNeg0)){
          ind->err += rxErrDurNeg0;
        }
        return;
      } else {
        if (!(ind->err & rxErrModelDurAbsent)){
          ind->err += rxErrModelDurAbsent;
        }
        return;
      }
    }
  }
}

static inline void updateRate(int idx, rx_solving_options_ind *ind, double *yp) {
  double t = getAllTimes(ind, idx);
  // f(), rate() and alag() are read at the start's record
  int oldIdx = ind->idx;
  ind->idx = getSortedPosition(ind, idx);
  double dur, rate, amt;
  amt  = getAmtAtRecord(ind, idx, getDose(ind,idx), yp);
  rate  = getRate(ind, ind->id, ind->cmt, amt, t, yp);
  double laggedStart = getLag(ind, ind->id, ind->cmt, t, yp);
  if (ISNA(laggedStart)) laggedStart = t;
  // restore before the checks below so every return path leaves ind->idx alone
  // (mirrors updateDur() above)
  ind->idx=oldIdx;
  if (rate > 0){
    dur = amt/rate; // mg/hr
    setDoseP1(ind, idx, -rate);
    setAllTimesP1(ind, idx, laggedStart + dur);
  } else {
    rx_solve *rx = (ind->rx ? ind->rx : &rx_global);
    rx_solving_options *op = (ind->op ? ind->op : &op_global);
    if (ind->cmt < op->neq){
      if (rx->needSort & needSortRate) {
        if (!(ind->err & rxErrRate0)){
          ind->err += rxErrRate0;
        }
        return;
      } else {
        // FIXME don't error out with linear compartmental model
        if (!(ind->err & rxErrModelRateAbsent)){
          ind->err += rxErrModelRateAbsent;
        }
        return;
      }
    }
  }
}

static inline void handleTurnOffModeledDuration(int idx, rx_solve *rx, rx_solving_options *op, rx_solving_options_ind *ind) {
  if (idx > 0){
    if (!isEvidModeledDurationStart(getEvidM1(ind, idx))) {
      if (!(ind->err & rxErrModelData686)){
        ind->err += rxErrModelData686;
      }
      return;
    }
  } else {
    if (!(ind->err & rxErrModelDataNeg6)) {
      ind->err += rxErrModelDataNeg6;
    }
    return;
  }
}

static inline void handleTurnOnModeledDuration(int idx, rx_solve *rx, rx_solving_options *op, rx_solving_options_ind *ind) {
  // This calculates the rate and the duration and then assigns it to the next record
  // idx + 1 has to exist: the checks and updateDur() below read getEvidP1() and
  // write setDoseP1()/setAllTimesP1(), so the LAST record has no room for the
  // stop this start needs.  (A negative idx is an extra dose and indexes the
  // extraDose* pools instead; it never trips this.)
  if (idx + 1 >= ind->n_all_times){
    // error: Last record, can't be used.
    if (!(ind->err & rxErrModelDataErr8)){
      ind->err += rxErrModelDataErr8;
    }
    return;
  } else {
    if (!isEvidModeledDurationStop(getEvidP1(ind, idx))) {
      if (!(ind->err & rxErrModelDataErr886)){
        ind->err += rxErrModelDataErr886;
      }
      return;
    }
    updateDur(idx, ind, (op->neq > 0) ? ind->solve : op->inits);
  }
}

static inline void handleTurnOffModeledRate(int idx, rx_solve *rx, rx_solving_options *op, rx_solving_options_ind *ind) {
  if (idx > 0){
    if (!isEvidModeledRateStart(getEvidM1(ind, idx))) {
      if (!(ind->err & rxErrModelDataErr797)){
        ind->err += rxErrModelDataErr797;
      }
      return;
    }
  } else {
    if (!(ind->err & rxErrModelDataNeg7)){
      ind->err += rxErrModelDataNeg7;
    }
    return;
  }
}

static inline void handleTurnOnModeledRate(int idx, rx_solve *rx, rx_solving_options *op, rx_solving_options_ind *ind) {
  // This calculates the rate and the duration and then assigns it to the next record
  // idx + 1 has to exist; see handleTurnOnModeledDuration() above
  if (idx + 1 >= ind->n_all_times){
    // error: Last record, can't be used.
    if (!(ind->err & rxErrModelDataErr9)){
      ind->err += rxErrModelDataErr9;
    }
    /* Rf_errorcall(R_NilValue, "Data Error 9\n"); */
    return;
  } else {
    if (!isEvidModeledRateStop(getEvidP1(ind, idx))) {
      if (!(ind->err & rxErrModelDataErr997)){
        ind->err += rxErrModelDataErr997;
      }
      return;
    }
    setAllTimesP1(ind, idx, getAllTimes(ind, idx));
    updateRate(idx, ind, (op->neq > 0) ? ind->solve : op->inits);
  }
}

static inline int handleInfusionStartRm(int *startIdx, int *endIdx,
                                        double *amt, int *idx,
                                        rx_solve *rx, rx_solving_options *op,
                                        rx_solving_options_ind *ind) {
  if (ind->wh0 == EVID0_INFRM) {
    // This is a possible removal event.  Look at the next duration
    // int curEvid = getEvid(ind, ind->idose[*endIdx+1]);
    *startIdx = *endIdx+1;
    for (*endIdx = *startIdx; *endIdx < ind->ndoses; ++(*endIdx)) {
      if (getEvid(ind, ind->idose[*startIdx]) == getEvid(ind, ind->idose[*endIdx])) break;
      if (*endIdx == ind->ndoses-1) {
        //REprintf("curEvid@infrm: %d\n", curEvid);
        if (!(ind->err & rxErrCorruptET)){
          ind->err += rxErrCorruptET;
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
  int mate = getInfusionMateDoseNumber(ind, *endIdx);
  if (mate != -1 && mate < *endIdx) {
    *startIdx = mate;
    return 1;
  }
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
    if (!(ind->err & rxErrCorruptET)){
      ind->err += rxErrCorruptET;
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
    if (!(ind->err & rxErrCorruptETSort3)){
      ind->err += rxErrCorruptETSort3;
    }
    return;
    /* Rf_errorcall(R_NilValue, "Corrupted event table during sort (1)."); */
  }
  handleInfusionStartRm(startIdx, endIdx, amt, idx, rx, op, ind) ||
    handleInfusionStartDefault(startIdx, endIdx, amt, idx, rx, op, ind);
}

// F*amt of the fixed-duration infusion record at ind->idx, whose F scales the
// rate: the start's f() read at the start's record (getAmtAtRecord()), for
// the stop as for the start, so the stop removes the rate the start added.
// An extra dose, or a stop with no start found, keeps the time-based read.
static inline double getInfusionDurAmt(rx_solving_options_ind *ind, int id, int cmt,
                                       double amt, double xout, double *yp) {
  if (ind->idx >= 0) {
    int raw = ind->ix[ind->idx];
    if (amt > 0 && raw < ind->n_all_times_orig) {
      return getAmtAtRecord(ind, raw, amt, yp);
    }
    if (amt < 0 && raw < ind->n_all_times_orig) {
      rx_solve *rx = (ind->rx ? ind->rx : &rx_global);
      rx_solving_options *op = (ind->op ? ind->op : &op_global);
      int infBidx = -1, infEidx = -1;
      double a = amt;
      handleInfusionGetStartOfInfusionIndex(&infBidx, &infEidx, &a, &raw, rx, op, ind);
      if (infBidx != -1) return getAmtAtRecord(ind, ind->idose[infBidx], amt, yp);
    }
  }
  return getAmt(ind, id, cmt, amt, xout, yp);
}

// The lagged start time of the infusion that starts at dose number infBidx,
// alag() read at its record
static inline double getInfusionStartLag(int infBidx, rx_solve *rx, rx_solving_options *op,
                                         rx_solving_options_ind *ind) {
  int oIdx = ind->idx;
  ind->idx = getSortedPosition(ind, ind->idose[infBidx]);
  // To the correct lag time, we need to make sure the steady state flags are not set...
  int wh0 = ind->wh0;
  int wh, cmt, wh100, whI;
  getWh(getEvid(ind, ind->idose[infBidx]), &wh, &cmt, &wh100, &whI, &(ind->wh0));
  double tB = getLag(ind, ind->id, ind->cmt, getAllTimes(ind, ind->idose[infBidx]),
                     (op->neq > 0) ? ind->solve : rx->ypNA);
  ind->wh0 = wh0;
  ind->idx = oIdx;
  return tB;
}

// The lagged time of a fixed-duration infusion's record: a stop is its
// start's lagged time plus the duration, alag() read at the start's record.
// A start, or a stop whose start is not found, reads alag() at its own record.
static inline double getInfusionDurTime(int idx, rx_solve *rx, rx_solving_options *op,
                                        rx_solving_options_ind *ind) {
  double amt = getDose(ind, idx);
  if (amt < 0 && ind->wh0 != EVID0_RATEADJ) {
    int infEidx = getDoseNumberFromIndex(ind, idx), infBidx = -1, stop = idx;
    if (infEidx != -1) {
      handleInfusionGetStartOfInfusionIndex(&infBidx, &infEidx, &amt, &stop, rx, op, ind);
    }
    if (infBidx != -1) {
      return getInfusionStartLag(infBidx, rx, op, ind) +
        getAllTimes(ind, idx) - getAllTimes(ind, ind->idose[infBidx]);
    }
  }
  return getLag(ind, ind->id, ind->cmt, getAllTimes(ind, idx),
                (op->neq > 0) ? ind->solve : rx->ypNA);
}

static inline double handleInfusionItem(int idx, rx_solve *rx, rx_solving_options *op, rx_solving_options_ind *ind) {
  if (ind->wh0 == EVID0_RATEADJ) {
    return getAllTimes(ind, idx);
  }
  double amt = getDose(ind, idx);
  if (amt > 0) {
    return getLag(ind, ind->id, ind->cmt, getAllTimes(ind, idx),
                  (op->neq > 0) ? ind->solve : rx->ypNA);
  } else if (amt < 0) {
    int infEidx = getDoseNumberFromIndex(ind, idx);
    if (infEidx == -1){
      if (!(ind->err & rxErrCorruptETSort3)){
        ind->err += rxErrCorruptETSort3;
      }
      return 0.0;
      /* Rf_errorcall(R_NilValue, "Corrupted event table during sort (1)."); */
    }
    int infBidx;
    handleInfusionGetStartOfInfusionIndex(&infBidx, &infEidx, &amt, &idx, rx, op, ind);
    if (infBidx == -1) return 0.0;
    rx_solve *rxp = (ind->rx ? ind->rx : &rx_global);
    // the stop is the start's lagged time plus F*duration, with alag() and f()
    // both read at the start's record
    double tB = getInfusionStartLag(infBidx, rx, op, ind);
    int oIdx = ind->idx;
    ind->idx = getSortedPosition(ind, ind->idose[infBidx]);
    double f = getAmtAtRecord(ind, ind->idose[infBidx], 1.0, rxp->ypNA);
    ind->idx = oIdx;
    if (ISNA(f)){
      rx_solving_options *opp = ind->op;
      int newBadSolve = 1;
#pragma omp atomic write
      opp->badSolve = newBadSolve;
      int newNaTime = 4 + 10*ind->cmt;
#pragma omp critical
      { if (opp->naTime == 0) opp->naTime = newNaTime; }
    }
    double durOld = (getAllTimes(ind, ind->idose[infEidx]) -
                     getAllTimes(ind, ind->idose[infBidx]));
    double dur = f*durOld;
    return tB + dur;
  } else {
    /* Rf_errorcall(R_NilValue, "Corrupted events."); */
    if (!(ind->err & rxErrCorruptET2)){
      ind->err += rxErrCorruptET2;
    }
    return 0.0;
  }
}

static inline double getTimeCalculateInfusionTimes(int idx, rx_solve *rx, rx_solving_options *op, rx_solving_options_ind *ind) {
  switch(ind->whI){
  case EVIDF_MODEL_DUR_OFF:
    handleTurnOffModeledDuration(idx, rx, op, ind);
    return getAllTimes(ind, idx);  // absolute lagged stop time stored by updateDur
  case EVIDF_MODEL_DUR_ON:
    handleTurnOnModeledDuration(idx, rx, op, ind);
    break;
  case EVIDF_MODEL_RATE_OFF:
    handleTurnOffModeledRate(idx, rx, op, ind);
    return getAllTimes(ind, idx);  // absolute lagged stop time stored by updateRate
  case EVIDF_MODEL_RATE_ON:
    handleTurnOnModeledRate(idx, rx, op, ind);
    break;
  case EVIDF_INF_RATE:
    return handleInfusionItem(idx, rx, op, ind);
    break;
  case EVIDF_INF_DUR:
    return getInfusionDurTime(idx, rx, op, ind);
  }
  // Sort-time lag for start/bolus events: use per-subject initial state
  return getLag(ind, ind->id, ind->cmt, getAllTimes(ind,idx),
                (op->neq > 0) ? ind->solve : rx->ypNA);
}

static inline double getTime__(int idx, rx_solving_options_ind *ind, int update) {
  rx_solving_options *op = (ind->op ? ind->op : &op_global);
  rx_solve *rx = (ind->rx ? ind->rx : &rx_global);
  int evid = getEvid(ind, idx);
  if (evid == 9) return 0.0;
  if (evid == 3) return getAllTimes(ind, idx);
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
      if (ind->whI == EVIDF_INF_DUR) {
        return getInfusionDurTime(idx, rx, op, ind);
      }
      if (ind->whI == EVIDF_MODEL_RATE_OFF || ind->whI == EVIDF_MODEL_DUR_OFF) {
        return getAllTimes(ind, idx);  // absolute lagged stop time stored by updateRate/updateDur
      }
    } else {
      return getTimeCalculateInfusionTimes(idx, rx, op, ind);
    }
  }
  // Use per-subject initial state for sort-time lag; runtime uses timeThread[] directly
  return getLag(ind, ind->id, ind->cmt, getAllTimes(ind, idx),
                (op->neq > 0) ? ind->solve : rx->ypNA);
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
