#ifndef RXODE2_EVENT_TRANSLATE_H
#define RXODE2_EVENT_TRANSLATE_H

/* Result of translating one NONMEM/rxode2 event: 1 or 2 internal events.
 *
 * This header is shared by:
 *   src/etTran.cpp   – batch event-table translation
 *   inst/include/rxode2parseHandleEvid.h – runtime evid_() push
 */

typedef struct {
  int    n;            /* number of output events: 1 (bolus/obs/reset) or 2 (infusion: start+stop) */
  int    evid[2];      /* internal rxode2 evid code(s) */
  double time[2];      /* event time(s): [0]=start, [1]=stop */
  double amt[2];       /* amounts: [0]=+amt (or +rate), [1]=-amt (or -rate) */
  double ii[2];        /* ii values */
  int    isDose[2];    /* 1 if this event contributes an idose entry */
} rx_translated_event;

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
 * For evid 1–7: translated from NONMEM semantics.
 */
static inline rx_translated_event
_rxTranslateOneEvent(double time, int evid, int cmt, double amt,
                     double ii_val, int ss, double rate) {
  rx_translated_event out;
  out.n = 0;
  out.evid[0] = 0; out.evid[1] = 0;
  out.time[0] = 0; out.time[1] = 0;
  out.amt[0]  = 0; out.amt[1]  = 0;
  out.ii[0]   = 0; out.ii[1]   = 0;
  out.isDose[0] = 0; out.isDose[1] = 0;

  /* Classic rxode2 internal evid (>= 100): pass through verbatim */
  if (evid >= 100) {
    out.n         = 1;
    out.evid[0]   = evid;
    out.time[0]   = time;
    out.amt[0]    = amt;
    out.ii[0]     = ii_val;
    int _flg      = evid % 100;
    out.isDose[0] = (_flg == 1 || _flg == 10 || _flg == 20 || _flg == 40) ? 1 : 0;
    return out;
  }

  /* Compartment encoding */
  int cmt100 = cmt / 100;
  int cmt99  = cmt % 100;

  /* rateI encoding (mirrors etTran.cpp) */
  int rateI = 0;
  double dur = 0.0, useRate = 0.0;
  if (rate > 0.0)        { rateI = 1; useRate = rate; dur = amt / rate; }
  else if (rate == -1.0) { rateI = 9; }
  else if (rate == -2.0) { rateI = 8; }

  /* flg encoding (SS handling, mirrors etTran.cpp) */
  int flg = 1;
  if      (ss == 1 && ii_val > 0)                  flg = 10;
  else if (ss == 2 && ii_val > 0)                  flg = 20;
  else if (ss == 1 && ii_val == 0 && amt == 0.0)  flg = 40;

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
    break;

  case 1: case 7:
    /* Dose: bolus, phantom or infusion */
    out.evid[0]   = cmt100*100000 + rateI*10000 + cmt99*100 + flg;
    out.time[0]   = time;
    out.amt[0]    = (rateI == 1) ? useRate : amt;
    out.ii[0]     = ii_val;
    out.isDose[0] = 1;
    out.n         = 1;
    if (rateI == 1 && flg != 40) {
      /* Infusion stop event */
      out.evid[1]   = cmt100*100000 + rateI*10000 + cmt99*100 + flg;
      out.time[1]   = time + dur;
      out.amt[1]    = -useRate;
      out.ii[1]     = 0.0;
      out.isDose[1] = 1;
      out.n         = 2;
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
    /* Reset + dose: two events (reset first, then dose) */
    out.n         = 2;
    out.evid[0]   = 3;
    out.time[0]   = time;
    out.amt[0]    = 0.0;
    out.ii[0]     = 0.0;
    out.isDose[0] = 0;
    out.evid[1]   = cmt100*100000 + rateI*10000 + cmt99*100 + flg;
    out.time[1]   = time;
    out.amt[1]    = (rateI == 1) ? useRate : amt;
    out.ii[1]     = ii_val;
    out.isDose[1] = 1;
    break;

  case 5:
    /* Replace */
    out.n         = 1;
    out.evid[0]   = cmt100*100000 + 4*10000 + cmt99*100 + flg;
    out.time[0]   = time;
    out.amt[0]    = amt;
    out.ii[0]     = ii_val;
    out.isDose[0] = 1;
    break;

  case 6:
    /* Multiply */
    out.n         = 1;
    out.evid[0]   = cmt100*100000 + 5*10000 + cmt99*100 + flg;
    out.time[0]   = time;
    out.amt[0]    = amt;
    out.ii[0]     = ii_val;
    out.isDose[0] = 1;
    break;

  default:
    break;
  }
  return out;
}

#endif /* RXODE2_EVENT_TRANSLATE_H */
