#ifndef RXODE2_EVENT_TRANSLATE_H
#define RXODE2_EVENT_TRANSLATE_H

/* Result of translating one NONMEM/rxode2 event: 1 to 3 internal events.
 *
 * This header is shared by:
 *   src/etTran.cpp   - batch event-table translation
 *   inst/include/rxode2parseHandleEvid.h - runtime evid_() push
 */

/* Up to 5: an evid=4 reset plus a steady-state dose into a compartment with a
 * modeled alag(), which expands to four records (rxode2#1349). */
#define RX_TRANSLATED_EVENT_MAX 5

typedef struct {
  int    n;            /* number of output events, 1 to RX_TRANSLATED_EVENT_MAX:
                        * 1 bolus/obs/reset, 2 infusion start+stop, 3 evid=4
                        * reset + infusion start+stop, up to 5 for a reset plus
                        * a lagged steady-state infusion */
  int    evid[RX_TRANSLATED_EVENT_MAX];   /* internal rxode2 evid code(s) */
  double time[RX_TRANSLATED_EVENT_MAX];   /* event time(s) */
  double amt[RX_TRANSLATED_EVENT_MAX];    /* amounts (+amt/+rate, then -rate) */
  double ii[RX_TRANSLATED_EVENT_MAX];     /* ii values */
  int    isDose[RX_TRANSLATED_EVENT_MAX]; /* 1 if this event contributes an idose entry */
} rx_translated_event;

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

static inline void getWh(int evid, int *wh, int *cmt, int *wh100, int *whI, int *wh0) {
  *wh = evid;
  *cmt = 0;
  *wh100 = *wh / 100000;
  *whI   = *wh / 10000 - *wh100 * 10;
  *wh    = *wh - *wh100 * 100000 - (*whI - 1) * 10000;
  *wh0   = (*wh % 10000) / 100;
  *cmt   = *wh0 - 1 + *wh100 * 100;
  *wh0   = evid - *wh100 * 100000 - *whI * 10000 - *wh0 * 100;
}

static inline int _rxEncodeEventCmt(int evid, int cmt) {
  int wh, oldCmt, wh100, whI, wh0;
  getWh(evid, &wh, &oldCmt, &wh100, &whI, &wh0);
  int cmt0 = cmt - 1;
  int cmt100 = cmt0 / 100;
  int cmt01 = cmt0 % 100 + 1;
  return cmt100 * 100000 + whI * 10000 + cmt01 * 100 + wh0;
}

static inline int _rxShouldSplitTranslatedBolus(int evid, int cmt, double amt, int splitCmt) {
  int wh, eventCmt, wh100, whI, wh0;
  getWh(evid, &wh, &eventCmt, &wh100, &whI, &wh0);
  if (splitCmt <= 0 || cmt != splitCmt || eventCmt + 1 != splitCmt || evid < 100 || amt <= 0.0) return 0;
  return whI == 0 && (wh0 == 1 || wh0 == 9 || wh0 == 10 || wh0 == 19 || wh0 == 20);
}

/* The steady-state flag carried by a dose, from its ss/ii/amt.  Shared so the
 * event table (src/etTran.cpp) and the runtime evid_() push
 * (_rxPushDose(), src/par_solve.cpp) cannot derive it differently. */
static inline int _rxDeriveFlg(int ss, double ii_val, double amt) {
  if (ss == 1 && ii_val > 0)                 return EVID0_SS;
  if (ss == 2 && ii_val > 0)                 return EVID0_SS2;
  if (ss == 1 && ii_val == 0 && amt == 0.0)  return EVID0_SSINF;
  return EVID0_REGULAR;
}

/* Does 'addl' repeat this NONMEM evid?  An observation (0), an "other" record
 * (2) and a reset (3) are single events: etTran.cpp warns and ignores addl for
 * them (src/etTran.cpp, the 'addl' is ignored with ... warnings). */
static inline int _rxAddlApplies(int evid) {
  return !(evid == 0 || evid == 2 || evid == 3);
}

/* Number of occurrences a record expands to: itself plus its addl repeats. */
static inline int _rxAddlCount(double ii_val, int addl) {
  return (ii_val > 0.0 && addl > 0) ? addl + 1 : 1;
}

/* The flg and ii carried by occurrence 'rep' of an addl series (rep 0 is the
 * record itself, 1..addl its repeats), writing the ii to *iiOut.
 *
 * dropSs (etTrans()'s addlDropSs, always on for the push path): a repeat of a
 * steady-state record -- lagged (flg 9/19) or not (10/20) -- becomes a plain
 * dose, which also drops the lagged expansion since only flg 9/19 expands.  A
 * repeat never resets either: the caller translates evid 4 as its plain dose
 * for rep > 0, since the reset is emitted once, before the series.
 *
 * flg 40 is a constant infusion, which requires ii == 0 and so can never have
 * addl repeats; it is defined here only to keep the mapping total. */
static inline int _rxAddlOccurrence(int rep, int flg, double ii_val, int addl,
                                    int dropSs, double *iiOut) {
  int ssLike = (flg == EVID0_SS0 || flg == EVID0_SS20 || flg == EVID0_SS ||
                flg == EVID0_SS2 || flg == EVID0_SSINF);
  if (rep == 0) {
    *iiOut = (ii_val > 0.0 && addl > 0 && !ssLike) ? 0.0 : ii_val;
    return flg;
  }
  if (dropSs && ssLike && flg != EVID0_SSINF) {
    *iiOut = 0.0;
    return EVID0_REGULAR;
  }
  *iiOut = ssLike ? ii_val : 0.0;
  return flg;
}

/* Translate one NONMEM-style (evid 0-7) or classic rxode2 internal (evid>=100) event
 * into the rxode2 internal representation.
 *
 * Internal evid encoding (unchanged from existing rxode2 convention):
 *   internal_evid = cmt100*100000 + rateI*10000 + cmt99*100 + flg
 *   rateI: 0=bolus, 1=fixed rate, 2=fixed dur, 4=replace, 5=mult, 7=phantom, 8=model_dur, 9=model_rate
 *   flg:   1=regular, 10=SS1(ii>0), 20=SS2(ii>0), 40=SS1 const infusion
 *
 * For evid >= 100: passed through verbatim; isDose determined by flg = evid%100.
 * For evid == 0 or 2: observation row pushed, isDose=0.
 * For evid 1-7: translated from NONMEM semantics.
 */
/* Write the dose record into out->[k], plus its off record into out->[k+1] when
 * the dose needs one; returns the number of slots used (1 or 2).
 *
 * A fixed rate (rateI 1) or fixed duration (rateI 2) infusion stores the RATE as
 * its amount and is turned off by a -rate record at time + dur.  A modeled rate
 * (rateI 9) or modeled duration (rateI 8) stores the AMT instead and is turned
 * off by a companion record at the SAME time carrying the matching off rateI (7
 * and 6) and a regular flg -- updateRate()/updateDur() fill in that record's
 * rate and stop time once the model has been evaluated.  Either way the off
 * record has to sit immediately after the on record, which is what
 * handleTurnOnModeledRate()/handleTurnOnModeledDuration() and the
 * getDoseP1()/getAllTimesP1() macros require.
 *
 * flg 40 (a constant steady-state infusion) never turns off, modeled or not --
 * getTime__() skips the infusion-time calculation for that flg entirely, and
 * etTran.cpp emits no off record for it either.
 */
/* A steady-state dose into a compartment carrying a modeled alag() (flg 9 for
 * ss=1, 19 for ss=2).  The steady-state calculation itself must run UNLAGGED
 * -- getLag() (inst/include/rxode2parseGetTime.h) returns the unlagged time
 * for wh0 9/19 -- while the dose the subject actually receives is lagged, so
 * the record is expanded into an unlagged steady-state record plus the plain
 * flg-1 records that carry the lag.  Mirrors src/etTran.cpp's ssAtDoseTime
 * expansion exactly, in the same order (rxode2#1349):
 *
 *   bolus / replace / multiply : SS record, then the plain dose
 *   modeled rate or duration   : SS record, plain on, modeled off
 *   fixed rate or duration     : SS record, an INFRM (flg 8) off so the SS
 *                                record's own infusion is accounted for,
 *                                then the plain on/off pair
 *
 * The SS and INFRM records carry ii; the lagged plain records carry 0. */
static inline int
_rxTranslateSsLagDoseInto(rx_translated_event *out, int k, double time,
                          int cmt100, int cmt99, int rateI, int flg,
                          double amt, double useRate, double dur,
                          double ii_val) {
  int base = cmt100*100000 + cmt99*100;
  out->evid[k]   = base + rateI*10000 + flg;
  out->time[k]   = time;
  out->amt[k]    = (rateI == EVIDF_INF_RATE || rateI == EVIDF_INF_DUR) ? useRate : amt;
  out->ii[k]     = ii_val;
  out->isDose[k] = 1;
  if (rateI == EVIDF_INF_RATE || rateI == EVIDF_INF_DUR) {
    out->evid[k+1]   = base + rateI*10000 + EVID0_INFRM;
    out->time[k+1]   = time;
    out->amt[k+1]    = -useRate;
    out->ii[k+1]     = ii_val;
    out->isDose[k+1] = 1;
    out->evid[k+2]   = base + rateI*10000 + EVID0_REGULAR;
    out->time[k+2]   = time;
    out->amt[k+2]    = useRate;
    out->ii[k+2]     = 0.0;
    out->isDose[k+2] = 1;
    out->evid[k+3]   = base + rateI*10000 + EVID0_REGULAR;
    out->time[k+3]   = time + dur;
    out->amt[k+3]    = -useRate;
    out->ii[k+3]     = 0.0;
    out->isDose[k+3] = 1;
    return 4;
  }
  out->evid[k+1]   = base + rateI*10000 + EVID0_REGULAR;
  out->time[k+1]   = time;
  out->amt[k+1]    = amt;
  out->ii[k+1]     = 0.0;
  out->isDose[k+1] = 1;
  if (rateI == EVIDF_MODEL_RATE_ON || rateI == EVIDF_MODEL_DUR_ON) {
    int offI = (rateI == EVIDF_MODEL_RATE_ON) ? EVIDF_MODEL_RATE_OFF : EVIDF_MODEL_DUR_OFF;
    out->evid[k+2]   = base + offI*10000 + EVID0_REGULAR;
    out->time[k+2]   = time;
    out->amt[k+2]    = amt;
    out->ii[k+2]     = 0.0;
    out->isDose[k+2] = 1;
    return 3;
  }
  return 2;
}

static inline int
_rxTranslateDoseInto(rx_translated_event *out, int k, double time,
                     int cmt100, int cmt99, int rateI, int flg,
                     double amt, double useRate, double dur, double ii_val,
                     int isDur) {
  /* A steady-state constant infusion (flg 40) never turns off, so pairing it
   * with a duration -- modeled (rateI 8) or fixed (rateI 2) -- is meaningless;
   * reject it the way etTran.cpp already does for the event table
   * (rxode2#1350).  -1 signals the caller to abort instead of silently
   * emitting a rate-less record that steady-states the compartment to zero.
   *
   * rateI 9 (modeled RATE) is legitimate at flg 40 -- it is the supported
   * spelling for a modeled constant infusion -- UNLESS it was reached via
   * infuseDur()'s duration slot (isDur set, meaning the caller wrote -1 into
   * a *duration*, NONMEM's "this was meant to be a modeled rate but landed
   * in the DUR column" mistake).  etTran.cpp rejects that same column
   * mistake for the event table; infuseDur() is the only push-path caller
   * that can produce rateI 9 with isDur set, since evid_()/infuse() always
   * pass isDur 0. */
  if (flg == EVID0_SSINF &&
      (rateI == EVIDF_INF_DUR || rateI == EVIDF_MODEL_DUR_ON ||
       (rateI == EVIDF_MODEL_RATE_ON && isDur))) {
    return -1;
  }
  if (flg == EVID0_SS0 || flg == EVID0_SS20) {
    return _rxTranslateSsLagDoseInto(out, k, time, cmt100, cmt99, rateI, flg,
                                     amt, useRate, dur, ii_val);
  }
  out->evid[k]   = cmt100*100000 + rateI*10000 + cmt99*100 + flg;
  out->time[k]   = time;
  out->amt[k]    = (rateI == EVIDF_INF_RATE || rateI == EVIDF_INF_DUR) ? useRate : amt;
  out->ii[k]     = ii_val;
  out->isDose[k] = 1;
  if ((rateI == EVIDF_INF_RATE || rateI == EVIDF_INF_DUR) && flg != EVID0_SSINF) {
    out->evid[k+1]   = cmt100*100000 + rateI*10000 + cmt99*100 + flg;
    out->time[k+1]   = time + dur;
    out->amt[k+1]    = -useRate;
    out->ii[k+1]     = 0.0;
    out->isDose[k+1] = 1;
    return 2;
  }
  if ((rateI == EVIDF_MODEL_RATE_ON || rateI == EVIDF_MODEL_DUR_ON) &&
      flg != EVID0_SSINF) {
    int offI = (rateI == EVIDF_MODEL_RATE_ON) ? EVIDF_MODEL_RATE_OFF : EVIDF_MODEL_DUR_OFF;
    out->evid[k+1]   = cmt100*100000 + offI*10000 + cmt99*100 + EVID0_REGULAR;
    out->time[k+1]   = time;
    out->amt[k+1]    = amt;
    out->ii[k+1]     = 0.0;
    out->isDose[k+1] = 1;
    return 2;
  }
  return 1;
}

/* One way this translator does NOT yet agree with etTran.cpp, only reachable
 * through the runtime evid_() push:
 *
 *   - A steady-state dose into a compartment carrying a modeled alag().
 *     etTran.cpp's ssAtDoseTime handling rewrites flg 10 -> 9 (and 20 -> 19) for
 *     such a compartment and expands the dose into a four record sequence (the
 *     flg 9 steady state start, an INFRM flg 8 record, then the lagged flg 1
 *     start and its stop).  flg 9 is not just bookkeeping -- getLag() returns
 *     the unlagged time for it.  A pushed dose gets the plain flg 10/20 pair,
 *     so its trajectory differs from the same regimen in the event table.
 *
 * A steady-state constant infusion (flg 40) carrying a duration, either
 * modeled (rate=-2) or fixed (isDur, which makes useRate = amt/dur), is
 * rejected below with out.n = -1 the same way etTran.cpp refuses it for the
 * event table (rxode2#1350) -- flg 40 never turns off (getTime__() skips the
 * infusion-time calculation for it and etTran.cpp emits no off record), so a
 * duration is meaningless and previously produced a lone flg 40 record with
 * no usable rate, silently steady-stating the compartment to zero.
 */
static inline rx_translated_event
_rxTranslateOneEvent(double time, int evid, int cmt, double amt,
                     double ii_val, int ss, double rate, int isDur,
                     int hasAlag) {
  rx_translated_event out;
  out.n = 0;
  for (int _i = 0; _i < RX_TRANSLATED_EVENT_MAX; ++_i) {
    out.evid[_i] = 0; out.time[_i] = 0; out.amt[_i] = 0;
    out.ii[_i] = 0; out.isDose[_i] = 0;
  }

  /* Classic rxode2 internal evid (>= 100): pass through verbatim.  A caller
   * can hand-encode this format directly (evid_()'s "evid" documents it as a
   * supported form), so the flg-40-plus-duration guard has to apply here too
   * -- otherwise a hand-encoded evid reproduces the exact silent-zero bug
   * this file otherwise rejects (rxode2#1350). */
  if (evid >= 100) {
    int _wh, _wCmt, _wh100, _whI, _flg;
    getWh(evid, &_wh, &_wCmt, &_wh100, &_whI, &_flg);
    if (_flg == EVID0_SSINF && (_whI == EVIDF_INF_DUR || _whI == EVIDF_MODEL_DUR_ON)) {
      out.n = -1;
      return out;
    }
    out.n         = 1;
    out.evid[0]   = evid;
    out.time[0]   = time;
    out.amt[0]    = amt;
    out.ii[0]     = ii_val;
    out.isDose[0] = (_flg == 1 || _flg == 10 || _flg == 20 || _flg == 40) ? 1 : 0;
    return out;
  }

  /* Compartment encoding.  A negative compartment turns that compartment off
   * (flg 30), the same signal etTran.cpp reads from a negative CMT column;
   * steady state is meaningless there, and etTran.cpp rejects the
   * combination outright. */
  int turnOff = (cmt < 0);
  if (turnOff) cmt = -cmt;
  int cmt100 = cmt / 100;
  int cmt99  = cmt % 100;

  /* rateI encoding (mirrors etTran.cpp) */
  int rateI = 0;
  double dur = 0.0, useRate = 0.0;
  if (rate > 0.0) {
    /* If isDur=1 and rate>0, rate carries fixed duration. */
    if (isDur) {
      rateI = 2;
      dur = rate;
      useRate = amt / dur;
    } else {
      rateI = 1;
      useRate = rate;
      dur = amt / rate;
    }
  }
  else if (rate == -1.0) { rateI = 9; }
  else if (rate == -2.0) { rateI = 8; }

  /* flg encoding (SS handling, mirrors etTran.cpp) */
  int flg = _rxDeriveFlg(ss, ii_val, amt);
  /* A steady-state dose into a compartment with a modeled alag() is marked
   * unlagged (flg 9/19) and expanded; etTran.cpp does the same rewrite from
   * mv[RxMv_alag] under ssAtDoseTime (rxode2#1349).  flg 1 and 40 are not
   * rewritten, matching etTran.cpp. */
  if (hasAlag) {
    if (flg == EVID0_SS)       flg = EVID0_SS0;
    else if (flg == EVID0_SS2) flg = EVID0_SS20;
  }
  if (turnOff) {
    if (flg != EVID0_REGULAR) { out.n = -1; return out; }
    flg = EVID0_OFF;
    rateI = 0;
  }
  /* A FIXED rate/duration only makes an infusion out of a dose record;
   * etTran.cpp drops it to a bolus for every other evid (a modeled rate or
   * duration is kept, which is how a phantom infusion stays representable). */
  if (evid != 1 && evid != 4 &&
      (rateI == EVIDF_INF_RATE || rateI == EVIDF_INF_DUR)) {
    rateI = 0;
  }

  /* Switch on NONMEM evid */
  switch (evid) {
  case 0: case 2:
    /* Observation: one event, evid passes through */
    out.n         = 1;
    out.evid[0]   = evid;
    out.time[0]   = time;
    out.amt[0]    = amt;
    out.ii[0]     = ii_val;
    out.isDose[0] = 0;
    if (evid == 2 && !turnOff && cmt >= 1) {
      /* A zero dose turns the compartment back on, as etTran.cpp emits for an
       * EVID=2 row naming a real compartment.  Same encoding it uses
       * (cevid - flg + 60, where cevid carries flg 1), so the rate flag is
       * kept.  etTran.cpp also requires cmt <= baseSize; every compartment a
       * pushed evid_() can name is a declared state, so cmt >= 1 is the
       * reachable form of that test here. */
      out.evid[1]   = cmt100*100000 + rateI*10000 + cmt99*100 + 1 - flg +
        EVID0_ONDOSE;
      out.time[1]   = time;
      out.amt[1]    = 0.0;
      out.ii[1]     = 0.0;
      out.isDose[1] = 1;
      out.n         = 2;
    } else if (evid == 2 && turnOff) {
      /* a negative compartment turns it off instead */
      out.evid[0]   = cmt100*100000 + cmt99*100 + EVID0_OFF;
      out.amt[0]    = amt;
      out.isDose[0] = 1;
    }
    break;

  case 1:
    /* Dose: bolus or infusion */
    out.n = _rxTranslateDoseInto(&out, 0, time, cmt100, cmt99, rateI, flg,
                                 amt, useRate, dur, ii_val, isDur);
    break;

  case 7:
    /* Phantom/transit event.  A modeled rate/duration survives so the on/off
     * pair is emitted, matching etTran.cpp; a FIXED rate/duration is dropped
     * to a bolus, which is also what etTran.cpp does for any evid but 1/4. */
    {
      int phantomRateI = (rateI == EVIDF_MODEL_RATE_ON ||
                          rateI == EVIDF_MODEL_DUR_ON) ? rateI : 0;
      out.n = _rxTranslateDoseInto(&out, 0, time, cmt100, cmt99, phantomRateI,
                                   EVID0_PHANTOM, amt, useRate, dur, ii_val,
                                   isDur);
    }
    break;

  case 3:
    /* Reset */
    out.n         = 1;
    out.evid[0]   = 3;
    out.time[0]   = time;
    out.amt[0]    = 0.0;
    out.ii[0]     = 0.0;
    out.isDose[0] = 0;
    break;

  case 4:
    /* Reset + dose: the reset first, then the dose and any off record it needs */
    out.evid[0]   = 3;
    out.time[0]   = time;
    out.amt[0]    = 0.0;
    out.ii[0]     = 0.0;
    out.isDose[0] = 0;
    {
      int doseN = _rxTranslateDoseInto(&out, 1, time, cmt100, cmt99, rateI,
                                       flg, amt, useRate, dur, ii_val, isDur);
      out.n = (doseN < 0) ? -1 : 1 + doseN;
    }
    break;

  case 5:
    /* Replace.  Goes through the dose path so that a lagged steady state
     * (flg 9/19) expands the same way etTran.cpp expands it; for every other
     * flg this is the same single record as before. */
    out.n = _rxTranslateDoseInto(&out, 0, time, cmt100, cmt99, EVIDF_REPLACE,
                                 flg, amt, useRate, dur, ii_val, isDur);
    break;

  case 6:
    /* Multiply -- see case 5 */
    out.n = _rxTranslateDoseInto(&out, 0, time, cmt100, cmt99, EVIDF_MULT,
                                 flg, amt, useRate, dur, ii_val, isDur);
    break;

  default:
    break;
  }
  return out;
}

#endif /* RXODE2_EVENT_TRANSLATE_H */
