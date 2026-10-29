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

// Sorted position of data record `raw`; a covariate update reads ind->idx as
// a sorted position.  At sort time, and in a record order no lag changed, the
// record sits at its own slot.  sortInd() fills ix as it times each record, so
// while it runs a later record (the start an ss-with-lag stop pairs with) has
// no slot yet and is at its own.
static inline int getSortedPosition(rx_solving_options_ind *ind, int raw) {
  if (raw < 0) return raw; // an extra dose is addressed by its negative index
  if (raw < ind->n_all_times && ind->ix[raw] == raw) return raw;
  for (int j = 0; j < ind->n_all_times; ++j) {
    if (ind->ix[j] == raw) return j;
  }
  // unreachable while ix is a permutation (sortInd() pre-fills it); fail the
  // subject's solve
  ind->err |= rxErrSortedPos; // nocov
  return 0; // nocov
}

// rxControl(doseAtRecord=TRUE): a dose's f(), alag(), rate() and dur(), and
// the covariates of the records it expands to, are read at its own data
// record, as NONMEM reads them.  Otherwise (rxode2's rule) each record reads
// its own time and covariates.
static inline int doseAtRecord(rx_solving_options_ind *ind) {
  rx_solve *rx = (ind->rx ? ind->rx : &rx_global);
  return rx->doseAtRecord;
}

// The ind->idx a read at dose record `raw` uses: its sorted position with
// doseAtRecord, otherwise `raw` itself
static inline int getDoseRecordIdx(rx_solving_options_ind *ind, int raw) {
  return doseAtRecord(ind) ? getSortedPosition(ind, raw) : raw;
}

// f() of the dose at record `raw` times `dose`, at the record's data time.
// With doseAtRecord it reads the record's covariates, as NONMEM does,
// wherever a lag puts the record in the solve: a missing covariate is filled
// from the record's neighbours in record order, and a record a dose row
// expands to reads that row (ind->doseRow).  Otherwise the covariates are
// read by time.
static inline double getAmtAtRecord(rx_solving_options_ind *ind, int raw,
                                    double dose, double *yp) {
  int oldRec = ind->recCovPos;
  if (raw >= 0 && doseAtRecord(ind)) ind->recCovPos = getSortedPosition(ind, raw) + 1;
  double ret = getAmt(ind, ind->id, ind->cmt, dose, getAllTimes(ind, raw), yp);
  ind->recCovPos = oldRec;
  return ret;
}

// The lagged start of a modeled infusion whose start is at data time t, with
// ind->idx at its record: alag() read there with doseAtRecord, otherwise at
// oldIdx; restores ind->idx to oldIdx
static inline double getLaggedStartAtRecord(rx_solving_options_ind *ind, int oldIdx,
                                            double t, double *yp) {
  if (!doseAtRecord(ind)) ind->idx = oldIdx;
  double laggedStart = getLag(ind, ind->id, ind->cmt, t, yp);
  ind->idx = oldIdx;
  return ISNA(laggedStart) ? t : laggedStart;
}

static inline void updateDur(int idx, rx_solving_options_ind *ind, double *yp){
  double t = getAllTimes(ind, idx);
  double dur, amt;
  // The duration and f cannot depend on state values; f() and dur() are read
  // at the start's record, and alag() too with doseAtRecord
  int oldIdx = ind->idx;
  ind->idx = getDoseRecordIdx(ind, idx);
  amt  = getAmtAtRecord(ind, idx, getDose(ind, idx), yp);
  dur  = getDur(ind, ind->id, ind->cmt, amt, t, yp);
  double laggedStart = getLaggedStartAtRecord(ind, oldIdx, t, yp);
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
  // f() and rate() are read at the start's record, and alag() too with
  // doseAtRecord
  int oldIdx = ind->idx;
  ind->idx = getDoseRecordIdx(ind, idx);
  double dur, rate, amt;
  amt  = getAmtAtRecord(ind, idx, getDose(ind,idx), yp);
  rate  = getRate(ind, ind->id, ind->cmt, amt, t, yp);
  // restores ind->idx before the checks below so every return path leaves it
  // alone (mirrors updateDur() above)
  double laggedStart = getLaggedStartAtRecord(ind, oldIdx, t, yp);
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

// whether raw record `raw` is a data record, one that ends an interval and
// re-reads modeled rates; skips the records _rxPkTime() skips
static inline int rereadDataRecord(rx_solving_options_ind *ind, int raw) {
  if (raw < 0 || raw >= ind->n_all_times_orig) return 0;
  if (ind->pkSkip != NULL && ind->pkSkip[raw]) return 0;
  int evid = getEvid(ind, raw);
  return !(isDose(evid) && (isEvidModeledRateStop(evid) || isEvidModeledDurationStop(evid) ||
                            !isSameTime(ind->timeThread[raw], ind->all_times[raw])));
}

// sorted position of the first data record after position i (the record that
// ends the next interval); -1 when there is none
static inline int rereadNextRecord(rx_solving_options_ind *ind, int i) {
  for (int j = i + 1; j < ind->n_all_times; ++j) {
    if (rereadDataRecord(ind, ind->ix[j])) return j;
  }
  return -1;
}

// whether the modeled rate started by record `on`
// (compartment cmt) is handled at or before sorted position i with no reset
// (evid = 3) or turn-off of cmt since
static inline int modeledRateStarted(rx_solving_options_ind *ind, int i, int on, int cmt) {
  double tOn = ind->timeThread[on];
  for (int p = i; p >= 0; --p) {
    int r = ind->ix[p];
    if (r == on) return 1;
    if (r < 0) continue;
    double tp = ind->timeThread[r];
    if (tp < tOn && !isSameTime(tp, tOn)) return 0;
    int evid = getEvid(ind, r);
    if (evid == 3) return 0;
    if (evid >= 100) {
      int wh, cmt2, wh100, whI, wh0;
      getWh(evid, &wh, &cmt2, &wh100, &whI, &wh0);
      if (wh0 == EVID0_OFF && cmt2 == cmt) return 0;
    }
  }
  return 0;
}

#define MODELED_RATE 1
#define MODELED_DUR 2

// MODELED_RATE when raw record `on` starts a modeled rate infusion paired with
// its stop at on + 1, MODELED_DUR for a modeled duration, 0 otherwise; sets
// *cmt
static inline int modeledInfusionKind(rx_solving_options_ind *ind, int on, int *cmt) {
  if (on < 0 || on + 1 >= ind->n_all_times_orig) return 0;
  int wh, wh100, whI, wh0;
  getWh(getEvid(ind, on), &wh, cmt, &wh100, &whI, &wh0);
  if (wh0 != EVID0_REGULAR) return 0;
  int stop = getEvid(ind, on + 1);
  if (whI == EVIDF_MODEL_RATE_ON) return isEvidModeledRateStop(stop) ? MODELED_RATE : 0;
  if (whI == EVIDF_MODEL_DUR_ON) return isEvidModeledDurationStop(stop) ? MODELED_DUR : 0;
  return 0;
}

// whether the stop of the modeled infusion started by raw record `on` is
// ignored, as a reset (ss = 1, evid = 4) that cancelled the infusion leaves it
static inline int modeledStopIgnored(rx_solving_options_ind *ind, int on) {
  int e = getDoseNumberFromIndexAny(ind, on + 1);
  if (e < 0) return 0;
  for (int k = 0; k < ind->ignoredDosesN[0]; ++k) {
    if (ind->ignoredDoses[k] == e) return 1;
  }
  return 0;
}

// the rate of the modeled infusion into cmt started by raw record `on` when it
// is running at sorted position i (time tNow): started at or before it, ends
// after it.  0 when not running.
static inline double modeledRunningRate(rx_solving_options_ind *ind, int i, int on,
                                        double tNow, int ncmt, int cmt) {
  if (cmt < 0 || cmt >= ncmt || !ind->on[cmt]) return 0.0;
  double tOn = ind->timeThread[on];
  if (tOn > tNow && !isSameTime(tOn, tNow)) return 0.0;
  double tOff = ind->all_times[on + 1];
  if (tOff < tNow || isSameTime(tOff, tNow)) return 0.0;
  double rate = -ind->dose[on + 1];
  if (!(rate > 0)) return 0.0;
  if (modeledStopIgnored(ind, on)) return 0.0;
  if (!modeledRateStarted(ind, i, on, cmt)) return 0.0;
  return rate;
}

// re-reads the rate of the infusion started by raw
// record `on` from the data record at sorted position j and moves its end so
// the remaining amount is kept.  Returns 1 when moved, 0 when unchanged, -1 on
// a non-positive rate.
static inline int modeledRereadRate(rx_solving_options_ind *ind, int j, int on, int cmt,
                                   double rOld, double tNow, double *yp,
                                   double *InfusionRate) {
  int oldIdx = ind->idx;
  ind->idx = j;
  double rNew = getRate(ind, ind->id, cmt, ind->dose[on],
                        ind->timeThread[ind->ix[j]], yp);
  ind->idx = oldIdx;
  if (!(rNew > 0)) {
    if (!(ind->err & rxErrRate0)) ind->err += rxErrRate0;
    return -1;
  }
  if (rNew == rOld) return 0;
  double tOff = ind->all_times[on + 1];
  InfusionRate[cmt] += rNew - rOld;
  ind->dose[on + 1] = -rNew;
  ind->all_times[on + 1] = tNow + rOld*(tOff - tNow)/rNew;
  return 1;
}

// re-reads the duration of the infusion started by raw record `on` from the
// data record at sorted position j: it now ends at its start plus the new
// duration, delivering what is left evenly, or stops now, delivering no more,
// when that end is not after tNow.  Returns like modeledRereadRate().
static inline int modeledRereadDur(rx_solving_options_ind *ind, int j, int on, int cmt,
                                   double rOld, double tNow, double *yp,
                                   double *InfusionRate) {
  int oldIdx = ind->idx;
  ind->idx = j;
  double dNew = getDur(ind, ind->id, cmt, ind->dose[on],
                       ind->timeThread[ind->ix[j]], yp);
  ind->idx = oldIdx;
  if (ISNA(dNew)) return -1;
  if (dNew < 0) {
    if (!(ind->err & rxErrDurNeg0)) ind->err += rxErrDurNeg0;
    return -1;
  }
  double tOff = ind->all_times[on + 1];
  double tEnd = ind->timeThread[on] + dNew;
  if (isSameTime(tEnd, tOff)) return 0;
  if (tEnd < tNow || isSameTime(tEnd, tNow)) {
    // the stop at tOff now removes nothing
    InfusionRate[cmt] -= rOld;
    ind->dose[on + 1] = 0.0;
    return 1;
  }
  double rNew = rOld*(tOff - tNow)/(tEnd - tNow);
  InfusionRate[cmt] += rNew - rOld;
  ind->dose[on + 1] = -rNew;
  ind->all_times[on + 1] = tEnd;
  return 1;
}

// re-reads the modeled infusion of the given kind from the data record at
// sorted position j (-1 when there is none)
static inline int modeledReread(rx_solving_options_ind *ind, int kind, int j, int on,
                                int cmt, double rOld, double tNow, double *yp,
                                double *InfusionRate) {
  if (j < 0) return -1;
  if (kind == MODELED_RATE) {
    return modeledRereadRate(ind, j, on, cmt, rOld, tNow, yp, InfusionRate);
  }
  return modeledRereadDur(ind, j, on, cmt, rOld, tNow, yp, InfusionRate);
}

// whether modeled infusions of the given kind are re-read
static inline int modeledRereadKind(rx_solve *rx, int kind) {
  if (kind == MODELED_RATE) return !rx->rateAtDoseTime && (rx->needSort & needSortRate);
  if (kind == MODELED_DUR) return !rx->durAtDoseTime && (rx->needSort & needSortDur);
  return 0;
}

// the solving options when modeled rates are re-read after the record at
// sorted position i; NULL when neither modeled rates nor durations are
// (rxControl(rateAtDoseTime =, durAtDoseTime =), or a model without them)
static inline rx_solving_options *modeledRereadOp(rx_solve *rx, rx_solving_options_ind *ind,
                                                 int i) {
  if (!modeledRereadKind(rx, MODELED_RATE) && !modeledRereadKind(rx, MODELED_DUR)) return NULL;
  if (ind->idose == NULL || ind->timeThread == NULL) return NULL;
  rx_solving_options *op = (ind->op ? ind->op : &op_global);
  if (_rxEsActive || op->adjoint) return NULL;
  int raw = ind->ix[i];
  if (raw < 0 || raw >= ind->n_all_times_orig) return NULL;
  return op;
}

// rxode2#1586: a modeled rate or duration is re-read at each data record
// until its amount is delivered; a new rate moves the end, a new duration
// moves the end to start + duration.  Called after the record at sorted
// position i is handled.  By default the value read at data record i holds on
// [t[i], t[i+1]) (LOCF); with rxControl(nonmem = TRUE) the value read with
// data record j holds on (t[j-1], t[j]], like NONMEM's R1, and any other
// record (an addl repeat, a lagged dose) re-reads the infusion it starts from
// the next data record.  Returns 1 when an infusion changed
// (the timeline needs a re-sort).  Event sensitivities and the adjoint keep
// the rate of the dose, which their d(rate)/dp forcing assumes.
static inline int updateModeledRates(rx_solve *rx, rx_solving_options_ind *ind,
                                           int i, double *yp, double *InfusionRate) {
  rx_solving_options *op = modeledRereadOp(rx, ind, i);
  if (op == NULL) return 0;
  int raw = ind->ix[i];
  double tNow = ind->timeThread[raw];
  int dataRec = rereadDataRecord(ind, raw);
  if (!dataRec && !rx->nonmem) return 0;
  int ncmt = op->neq + op->extraCmt;
  int j = -2, ret = 0;
  for (int d = 0; d < ind->ndoses; ++d) {
    int on = ind->idose[d], cmt = -1;
    if (!dataRec && on != raw) continue;
    int kind = modeledInfusionKind(ind, on, &cmt);
    if (!modeledRereadKind(rx, kind)) continue;
    double rOld = modeledRunningRate(ind, i, on, tNow, ncmt, cmt);
    if (rOld == 0.0) continue;
    if (j == -2) j = rx->nonmem ? rereadNextRecord(ind, i) : i;
    int moved = modeledReread(ind, kind, j, on, cmt, rOld, tNow, yp, InfusionRate);
    if (moved < 0) return ret;
    ret |= moved;
  }
  return ret;
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

// The lagged start time of the infusion that starts at dose number infBidx,
// alag() read at its record with doseAtRecord, otherwise with the covariates
// of the record being timed
static inline double getInfusionStartLag(int infBidx, rx_solve *rx, rx_solving_options *op,
                                         rx_solving_options_ind *ind) {
  int oIdx = ind->idx;
  if (rx->doseAtRecord) ind->idx = getSortedPosition(ind, ind->idose[infBidx]);
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

// Dose number of the start of the infusion whose stop is record `idx`, or -1
// for a start or when none is found.  The search's corrupt-table flags are
// not kept: such a record keeps the reads it had before it was paired.
static inline int getInfusionStartDose(int idx, rx_solve *rx, rx_solving_options *op,
                                       rx_solving_options_ind *ind) {
  double amt = getDose(ind, idx);
  if (!(amt < 0) || ind->wh0 == EVID0_RATEADJ) return -1;
  int infEidx = getDoseNumberFromIndex(ind, idx), infBidx = -1, stop = idx;
  if (infEidx == -1) return -1;
  int err = ind->err;
  handleInfusionGetStartOfInfusionIndex(&infBidx, &infEidx, &amt, &stop, rx, op, ind);
  ind->err = err;
  return (infBidx < 0 || infBidx >= ind->ndoses) ? -1 : infBidx;
}

// The lagged time of a fixed-duration infusion's record: with doseAtRecord a
// stop is its start's lagged time plus the duration, alag() read at the
// start's record.  Otherwise, and for a start or a stop whose start is not
// found, alag() is read at the record's own.
static inline double getInfusionDurTime(int idx, rx_solve *rx, rx_solving_options *op,
                                        rx_solving_options_ind *ind) {
  int infBidx = rx->doseAtRecord ? getInfusionStartDose(idx, rx, op, ind) : -1;
  if (infBidx != -1) {
    return getInfusionStartLag(infBidx, rx, op, ind) +
      getAllTimes(ind, idx) - getAllTimes(ind, ind->idose[infBidx]);
  }
  return getLag(ind, ind->id, ind->cmt, getAllTimes(ind, idx),
                (op->neq > 0) ? ind->solve : rx->ypNA);
}

// F*amt of the fixed-duration infusion record at ind->idx, whose F scales the
// rate: with doseAtRecord the start's f() read at the start's record
// (getAmtAtRecord()), for the stop as for the start, so the stop removes the
// rate the start added.  Otherwise, and for an extra dose or a stop with no
// start found, f() is read at the record's own time.
static inline double getInfusionDurAmt(rx_solving_options_ind *ind, int id, int cmt,
                                       double amt, double xout, double *yp) {
  if (ind->idx >= 0 && doseAtRecord(ind)) {
    int raw = ind->ix[ind->idx];
    if (amt > 0 && raw < ind->n_all_times_orig) {
      return getAmtAtRecord(ind, raw, amt, yp);
    }
    if (amt < 0 && raw < ind->n_all_times_orig) {
      rx_solve *rx = (ind->rx ? ind->rx : &rx_global);
      rx_solving_options *op = (ind->op ? ind->op : &op_global);
      int infBidx = getInfusionStartDose(raw, rx, op, ind);
      if (infBidx != -1) return getAmtAtRecord(ind, ind->idose[infBidx], amt, yp);
    }
  }
  return getAmt(ind, id, cmt, amt, xout, yp);
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
    // the stop is the start's lagged time plus F*duration, with f() read at
    // the start's record, and alag() too with doseAtRecord
    double tB = getInfusionStartLag(infBidx, rx, op, ind);
    int oIdx = ind->idx;
    ind->idx = getDoseRecordIdx(ind, ind->idose[infBidx]);
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
