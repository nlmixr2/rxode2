// Exact discrete-adjoint driver for liblsoda (method="liblsodaadj", code 202):
// the exact reverse-mode transpose of liblsoda's Nordsieck multistep step map,
// filling the same rx__sens_<state>_BY_<param>__ columns as rk4s/dop853s/
// cvodesadj.  #included into par_solve.cpp (IN_PAR_SOLVE) so it sees calc_lhs/
// getSolve/rxEffNeq/the dydt globals and rk4s.cpp's luFactor/luSolveT.
//
// The transpose is a linear map in (J, F_p, h, el, nq), so per accepted step we
// record only (tn, h, nq, el[], converged y) and recompute J,F_p via calc_lhs;
// at an observation the costate is seeded by intdy^T on the bracketing step.
// Variable order/step and Adams<->BDF switches are handled automatically (el
// come from the elco table by order).  Interior event jumps chain Nordsieck
// segments via the re-init dual and the event jump dual dPhi/dy^T, with modeled
// f()/alag()/rate()/dur() dose-modifier duals accumulated into the parameter
// quadrature.  par_liblsodaadj runs subjects in parallel (thread_local
// buffers).
#ifdef IN_PAR_SOLVE

// recording hooks reuse liblsoda's common block (common.h, already #included by
// par_solve.cpp; no include guard, so do not re-include).

// One recorded accepted step: the linearisation point + step metadata + the
// post-step transition (order change + scaleh) that stoda applied before the next
// step / before an observation interpolated in this step's interval.
struct lsAdjStep {
  double tn, h;               // end-time and step size actually USED (hu)
  int nq;                     // order USED (nqu)
  double el[14];              // el[1..nq+1] for the USED order (from the elco table,
                              // NOT _rxC(el) which is already reset for the next step)
  std::vector<double> y;      // converged base states at tn (yh[1][1..nBase])
  // transition applied AFTER update, BEFORE step n+1 (and before any obs interp):
  //   dOrder = nq_next - nq  (+1 order increase adds a Nordsieck row = acor*rInc,
  //            -1 decrease drops the top row, 0 none); then scaleh: yh[j]*=rh^(j-1).
  int dOrder;                 // nq_next - nq  (in {-1,0,+1})
  double rh;                  // scaleh factor h_next/h_used (1 = no step change)
  double rInc;                // order-increase new-row factor el[nq+1]/(nq+1)
  int nqNext;                 // order after transition (= order liblsoda's intdy uses)
  double hNext;               // h after transition (= h liblsoda's intdy uses)
  int meth;                   // method USED this step (1 Adams / 2 BDF); Adams<->BDF
                              // switches change el only (recorded from elco), so the
                              // transpose needs no special handling -- kept for the
                              // self-check to confirm switches are actually exercised.
  int seg;                    // segment index (a fresh liblsoda init starts a segment)
};
// A segment is a maximal run of accepted steps between liblsoda re-initialisations.
// istateReset (default) resets ctx->state=1 after every interior dose/reset, so the
// Nordsieck history restarts at order 1 with a fresh (t0,y0,h0) at each event -- the
// trajectory is a chain of segments joined by state jumps.
// A state-jump event at a segment start, reconstructed from the event table:
//   0 additive bolus (dPhi/dy=I) | 3 reset (y:=inits, dPhi/dy=0) | 8 ss=1 (y:=Y_ss(p),
//   dPhi/dy=0, adds lambda^T dY_ss/dp) | 5 replace (y[cmt]:=val, row cmt 0) |
//   6 multiply (y[cmt]*=factor, F*amt) | 12 a fixed-dur or modeled infusion start
//   (where a state-dependent f() window's start term applies, in the forward order).  amt is the raw dose (modeled-F mu), amtF = F*amt,
//   the amount that lands (modeled-alag mu); rec = a bolus/replace/multiply's
//   record index (its rxAdjJumpCap), else -1.
struct lsAdjEv { int type, cmt; double factor, amt, amtF; int rec = -1; int push = -1; };   // push: its extra dose (rxAdjPushCap)

struct lsAdjSeg {
  double t0, h0;              // segment start time and its initial step h0
  std::vector<double> y0;     // base state at segment start (post-event, yh[1])
  size_t stepBegin, stepEnd;  // [begin,end) range into lsAdjSteps
  // the events at this segment's start time (the interior events that opened it)
  std::vector<lsAdjEv> ev;   // those events in forward order (empty for seg 0 without one)
  // Infusion duals, nBase x np (c*np+p, empty = none), summed over overlapping
  // windows, with dR/dp and dF/dp read at each window's start record:
  //   infG  in-window forcing mu += h*infG^T w per step (durMult*dR/dp, durMult 1
  //         for rate() or F*amt for dur(), plus raw amt/dur*dF/dp for dur() or a
  //         fixed dur);
  //   offG  this segment STARTS at an infusion OFF boundary: mu += offG^T lam
  //         (-F*amt/R*durMult*dR/dp, plus raw amt*dF/dp for rate() or a fixed rate).
  std::vector<double> infG, offG;
  std::vector<double> infRate;  // InfusionRate[0..nBase) at segment start
  std::vector<double> lagRate;  // rate change per cmt from a lagged infusion start/stop
                                // opening this segment -> mu += -dR*dlag/dp*lam[cmt].
};
// G[c*np+p] += x*row[p] on an nBase x np infusion term, allocated on first use
static inline void lsAdjAddG(std::vector<double> &G, int nBase, int np, int c, double x,
                             const std::vector<double> &row) {
  if (row.empty() || x == 0.0) return;
  if (G.empty()) G.assign((size_t)nBase * np, 0.0);
  for (int p = 0; p < np; ++p) G[(size_t)c * np + p] += x * row[p];
}

// Thread-local recording state (serial per subject; par is P6).
static thread_local bool                 lsAdjActive = false;
static thread_local int                  lsAdjNbase  = 0;
static thread_local std::vector<lsAdjStep> lsAdjSteps;
static thread_local std::vector<lsAdjSeg>  lsAdjSegs;
static thread_local rx_solving_options_ind *lsAdjInd = NULL;   // for the InfusionRate snapshot

extern "C" int lsAdjIsActive(void) { return lsAdjActive ? 1 : 0; }

// Called at every state==1 init (yh[1]=y0, yh[2]=h0*f(y0)) -- once at t0 and once
// after each interior event that reset the integrator -- opening a new segment.
extern "C" void lsAdjSetActive(int a) { lsAdjActive = (a != 0); }

extern "C" void lsAdjInitStep(struct lsoda_context_t *ctx) {
  if (!lsAdjActive) return;   // paused (e.g. during a steady-state pre-solve)
  if (!lsAdjSegs.empty()) lsAdjSegs.back().stepEnd = lsAdjSteps.size();  // close prev
  lsAdjSeg s;
  s.t0 = _rxC(tn);
  s.h0 = _rxC(h);
  s.y0.resize(lsAdjNbase);
  for (int i = 1; i <= lsAdjNbase; ++i) s.y0[i - 1] = _rxC(yh)[1][i];
  s.stepBegin = lsAdjSteps.size();
  s.stepEnd = lsAdjSteps.size();
  if (lsAdjInd != NULL) s.infRate.assign(lsAdjInd->InfusionRate, lsAdjInd->InfusionRate + lsAdjNbase);
  lsAdjSegs.push_back(std::move(s));
}

// Called after every accepted step.  At this point stoda has already run UPDATE +
// the post-step transition (methodswitch / orderswitch / scaleh), so _rxC(nq) /
// _rxC(h) / _rxC(el) describe the NEXT step -- we recover the USED order/step from
// _rxC(nqu) / _rxC(hu) and the USED el from the elco table (indexed by used order,
// which survives resetcoeff), and read the transition off (nqu,hu) -> (nq,h).
extern "C" void lsAdjPushStep(struct lsoda_context_t *ctx) {
  if (!lsAdjActive) return;   // paused (e.g. during a steady-state pre-solve)
  lsAdjStep s;
  s.tn = _rxC(tn);
  s.h  = _rxC(hu);
  s.nq = _rxC(nqu);
  // el for the USED order from the fixed elco table (el[i] = elco[nq][i]); _rxC(el)
  // has already been reset to the next step's order by resetcoeff.
  for (int j = 1; j <= s.nq + 1 && j < 14; ++j) s.el[j] = _rxC(elco)[s.nq][j];
  s.y.resize(lsAdjNbase);
  for (int i = 1; i <= lsAdjNbase; ++i) s.y[i - 1] = _rxC(yh)[1][i];
  s.nqNext = _rxC(nq);
  s.hNext  = _rxC(h);
  s.dOrder = s.nqNext - s.nq;
  s.rh     = s.hNext / s.h;
  s.rInc   = (s.dOrder == 1) ? s.el[s.nq + 1] / (double)(s.nq + 1) : 0.0;
  s.meth   = _rxC(mused);     // method used in this (just-accepted) step
  s.seg    = lsAdjSegs.empty() ? 0 : (int)(lsAdjSegs.size() - 1);
  lsAdjSteps.push_back(std::move(s));
}

// ---- backward transpose driver -------------------------------------------------

// Evaluate J (F_X) and F_p at (t, base state yBase) via calc_lhs, like cvodesadj.
// fx points to a row-major nBase x nBase block (fx[i*nBase+j] = df_i/dy_j); fp to
// nBase x np (fp[i*np+p] = df_i/dp_p).  Both alias into ind->lhs.
static inline void lsAdjEval(int cSub, double t, const double *yBase, int nBase,
                             int eff, int fxOff, int fpOff, double *A, double *lhs,
                             const double **fx, const double **fp) {
  for (int i = 0; i < eff; ++i) A[i] = 0.0;
  for (int i = 0; i < nBase; ++i) A[i] = yBase[i];
  calc_lhs(cSub, t, A, lhs);
  *fx = &lhs[fxOff];
  *fp = &lhs[fpOff];
}

// Infusion-window duals per segment start, for liblsodaadj's segments and abs's
// calls (Seg needs t0, y0, infRate and the lsAdjSeg weight vectors).  A window's
// dR/dp, dF/dp and F are read at its start record, as the forward solve read them.
// Returns whether the subject has an infusion.
// A fixed-dur window whose f() depends on the state, over segments [seg0, seg1):
// its rate rawRate*F(x_on) is forced with dF/dp at the start state x_on (dF, the
// cmt row, np), and the start state gains (integral of lam_cmt over the window)
// * row, row = rawRate*dF_cmt/dx at x_on.  A lagged start reads x_on later by the
// lag, which moves it by fPre (the field just before the start): mu += dlag/dp *
// (that integral) * (row . fPre), dlag the cmt's d(alag)/dp row (both empty
// without a modeled alag()).  A modeled window (dF empty: its dF/dp forcing is in
// infG) has row = amt/dur * dF_cmt/dx for dur(), and, for rate() (atOff), row =
// amt * dF_cmt/dx weighted by lam_cmt at its off boundary (segment seg1), since
// its stop t_on + F*amt/R moves with x_on.
struct rxAdjSfWin { size_t seg0, seg1; int cmt, rec; double raw; std::vector<double> dF, row, fPre, dlag;
                    bool atOff = false; };

// Records a fixed-dur window as an rxAdjSfWin when its f() depends on the state
// (start state y0 at t0, record rec); false otherwise, for the plain dF forcing.
static inline bool rxAdjSfWinAdd(rx_solving_options *op, rx_solving_options_ind *ind, int cSub, int nBase,
                                 int eff, double t0, const double *y0, size_t seg0, size_t seg1, int c,
                                 double raw, int rec, std::vector<rxAdjSfWin> *sfw,
                                 bool mdl = false, bool atOff = false) {
  if (sfw == NULL || op->adjDfxOff < 0 || op->adjDfOff < 0 || cSub < 0) return false;
  // the state the start record read F at (before later same-time events), else y0
  const rxAdjJumpCap *cap = rec >= 0 ? rxAdjJumpFind(rec) : NULL;
  if (cap != NULL && cap->kind == 7 && !cap->yPre.empty()) y0 = cap->yPre.data();
  std::vector<double> _dR, _dF;
  double f = rxAdjRecRows(op, ind, cSub, eff, rec, t0, y0, c, _dR, _dF);   // leaves ind->lhs at x_on
  bool dep = false;
  for (int j = 0; j < nBase; ++j) dep = dep || ind->lhs[op->adjDfxOff + c * nBase + j] != 0.0;
  if (!dep) return false;
  int np = op->adjNp;
  rxAdjSfWin W; W.seg0 = seg0; W.seg1 = seg1; W.cmt = c; W.rec = rec; W.raw = raw; W.atOff = atOff;
  if (!mdl) W.dF = _dF;
  W.row.resize(nBase);
  for (int j = 0; j < nBase; ++j) W.row[j] = raw * ind->lhs[op->adjDfxOff + c * nBase + j];
  // the capture's field is taken after the start set its rate (raw*F)
  if (!mdl && op->adjDlagOff >= 0 && cap != NULL && cap->kind == 7 && (int)cap->fPre.size() == nBase) {
    W.fPre = cap->fPre;
    W.fPre[c] -= raw * f;
    W.dlag.assign(ind->lhs + op->adjDlagOff + c * np, ind->lhs + op->adjDlagOff + (c + 1) * np);
  }
  sfw->push_back(W);
  return true;
}

// mu += dlag/dp * acc * (row . fPre): a lagged state-dependent window's start
// term, acc the window's integral of lam_cmt
static inline void rxAdjSfWinLag(const rxAdjSfWin &W, double acc, double *mu, int nBase, int np) {
  if (W.dlag.empty()) return;
  double g = 0.0;
  for (int j = 0; j < nBase; ++j) g += W.row[j] * W.fPre[j];
  for (int p = 0; p < np; ++p) mu[p] += W.dlag[p] * acc * g;
}

template <class Seg>
static bool rxAdjInfusionWindows(rx_solving_options *op, rx_solving_options_ind *ind, int cSub, int nBase, int eff,
                                 std::vector<Seg> &segs, std::vector<rxAdjSfWin> *sfw = NULL) {
  int np = op->adjNp;
  // An infusion start/stop opening a segment changes the rate into its cmt by the
  // jump in InfusionRate across the segment boundary.  A modeled alag() moves that
  // time; a modeled F scales a fixed-dur rate (forcing rawRate*dF over the window) or
  // stretches a fixed-rate window (amtRaw*dF at its off boundary).
  if (op->adjDlagOff >= 0 || op->adjDfOff >= 0) {
    // open fixed windows: cmt, start key, first segment, raw rate, fixed dur or
    // rate, start record, and F and its dF/dp row there
    struct lsFxWin { int cmt, key; size_t seg; double raw; bool isDur; int rec; double f; std::vector<double> dF; };
    std::vector<lsFxWin> fxOpen;
    for (size_t sg = 0; sg < segs.size(); ++sg) {
      Seg &Sg = segs[sg];
      if ((int)Sg.infRate.size() != nBase) continue;
      // a lagged ss dose's pushed infusion start/stop opening this segment
      if (op->adjDlagOff >= 0)
        for (size_t q = 0; q < _adjLagPush.size(); ++q) {
          int k = _adjLagPush[q];
          if (k >= ind->extraDoseN[0] || !rxAdjSameTime(ind->extraDoseTime[k], Sg.t0)) continue;
          int _wh, c, _wh100, _whI, _wh0; getWh(ind->extraDoseEvid[k], &_wh, &c, &_wh100, &_whI, &_wh0);
          if (c < 0 || c >= nBase) continue;
          double prev = (sg > 0 && (int)segs[sg - 1].infRate.size() == nBase) ? segs[sg - 1].infRate[c] : 0.0;
          if (Sg.infRate[c] == prev) continue;
          if (Sg.lagRate.empty()) Sg.lagRate.assign(nBase, 0.0);
          Sg.lagRate[c] = Sg.infRate[c] - prev;
        }
      for (int i = 0; i < ind->n_all_times; ++i) {
        if (!rxAdjSameTime(ind->timeThread[ind->ix[i]], Sg.t0)) continue;
        int ev = getEvid(ind, ind->ix[i]);
        int c = rk4sInfusionEventCmt(ev, nBase, true);
        if (c < 0) continue;
        double prev = (sg > 0 && (int)segs[sg - 1].infRate.size() == nBase) ? segs[sg - 1].infRate[c] : 0.0;
        double dR = Sg.infRate[c] - prev;
        int _wh, _c, _wh100, _whI, _wh0; getWh(ev, &_wh, &_c, &_wh100, &_whI, &_wh0);
        if (op->adjDlagOff >= 0 && _wh0 == EVID0_REGULAR && dR != 0.0) {
          if (Sg.lagRate.empty()) Sg.lagRate.assign(nBase, 0.0);
          Sg.lagRate[c] = dR;
        }
        if (op->adjDfOff < 0 || (_whI != EVIDF_INF_RATE && _whI != EVIDF_INF_DUR)) continue;
        double raw = getDose(ind, ind->ix[i]);   // the record's raw rate (negative at a stop)
        int key = rk4sInfusionStartKey(ind, ind->ix[i]);
        if (raw > 0.0) {
          std::vector<double> _dR, _dF;
          double f = rxAdjRecRows(op, ind, cSub, eff, i, Sg.t0, Sg.y0.data(), c, _dR, _dF);
          if (f != 0.0) fxOpen.push_back({c, key, sg, raw, _whI == EVIDF_INF_DUR, i, f, _dF});
          continue;
        }
        // the window this stop closes: its own start, else the most recent one
        int wi = -1;
        for (size_t w = fxOpen.size(); w-- > 0;) {
          if (fxOpen[w].cmt != c) continue;
          if (key >= 0 && fxOpen[w].key == key) { wi = (int)w; break; }
          if (wi < 0) wi = (int)w;
        }
        if (wi < 0) continue;
        const lsFxWin W = fxOpen[wi];
        fxOpen.erase(fxOpen.begin() + wi);
        if (_whI == EVIDF_INF_DUR) {
          if (!rxAdjSfWinAdd(op, ind, cSub, nBase, eff, segs[W.seg].t0, segs[W.seg].y0.data(), W.seg, sg, c, W.raw, W.rec, sfw))
            for (size_t s2 = W.seg; s2 < sg; ++s2) lsAdjAddG(segs[s2].infG, nBase, np, c, W.raw, W.dF);
        } else {
          lsAdjAddG(Sg.offG, nBase, np, c, W.raw * (Sg.t0 - segs[W.seg].t0) / W.f, W.dF);
        }
      }
    }
    // a fixed-dur window still open when the solve ends (its stop lies past the
    // last record solved) forces every remaining segment
    for (size_t w = 0; w < fxOpen.size(); ++w)
      if (fxOpen[w].isDur &&
          !rxAdjSfWinAdd(op, ind, cSub, nBase, eff, segs[fxOpen[w].seg].t0, segs[fxOpen[w].seg].y0.data(),
                         fxOpen[w].seg, segs.size(), fxOpen[w].cmt, fxOpen[w].raw, fxOpen[w].rec, sfw))
        for (size_t s2 = fxOpen[w].seg; s2 < segs.size(); ++s2)
          lsAdjAddG(segs[s2].infG, nBase, np, fxOpen[w].cmt, fxOpen[w].raw, fxOpen[w].dF);
  }

  // Reconstruct modeled rate()/dur() infusion windows for the dR/dp dual: pair each
  // ON event with its matching OFF and derive R = F*amt/(t_off-t_on).  Segments in
  // [t_on,t_off) get the in-window forcing; the segment at t_off gets the off-
  // boundary transversality.  durMult = 1 for rate(), F*amt for dur().  A modeled F
  // adds amtRaw/dur*dF to the dur() forcing and amtRaw*dF at the rate() off boundary
  // (t_off = t_on + F*amt/rate).
  bool hasInfusion = false;
  for (int i = 0; i < ind->n_all_times; ++i) {
    int ev = getEvid(ind, ind->ix[i]);
    if (isObs(ev) || !isDose(ev)) continue;
    int _wh, _cmt, _wh100, _whI, _wh0; getWh(ev, &_wh, &_cmt, &_wh100, &_whI, &_wh0);
    if (_whI == EVIDF_INF_RATE || _whI == EVIDF_INF_DUR ||
        _whI == EVIDF_MODEL_RATE_ON || _whI == EVIDF_MODEL_DUR_ON ||
        _whI == EVIDF_MODEL_RATE_OFF || _whI == EVIDF_MODEL_DUR_OFF) hasInfusion = true;
    if ((_whI == EVIDF_MODEL_RATE_ON || _whI == EVIDF_MODEL_DUR_ON) && _cmt >= 0 && _cmt < nBase && op->adjDrateOff >= 0) {
      double tOn = ind->timeThread[ind->ix[i]];
      double amtRaw = getDose(ind, ind->ix[i]);
      bool isDur = (_whI == EVIDF_MODEL_DUR_ON);
      // its OFF event: the record right after the start (as the forward solve pairs
      // them), else the next off time > tOn into the same cmt
      double tOff = R_PosInf;
      int ixOff = ind->ix[i] + 1;
      if (ixOff < ind->n_all_times) {
        int _wh2, _cmt2, _wh1002, _whI2, _wh02; getWh(getEvid(ind, ixOff), &_wh2, &_cmt2, &_wh1002, &_whI2, &_wh02);
        if ((_whI2 == EVIDF_MODEL_RATE_OFF || _whI2 == EVIDF_MODEL_DUR_OFF) && _cmt2 == _cmt) tOff = ind->timeThread[ixOff];
      }
      for (int j = 0; !R_FINITE(tOff) && j < ind->n_all_times; ++j) {
        int ej = getEvid(ind, ind->ix[j]); if (isObs(ej) || !isDose(ej)) continue;
        int _wh2, _cmt2, _wh1002, _whI2, _wh02; getWh(ej, &_wh2, &_cmt2, &_wh1002, &_whI2, &_wh02);
        if ((_whI2 == EVIDF_MODEL_RATE_OFF || _whI2 == EVIDF_MODEL_DUR_OFF) && _cmt2 == _cmt) {
          double te = ind->timeThread[ind->ix[j]]; if (te > tOn + rxAdjTimeTol(te, tOn)) tOff = te;
        }
      }
      if (!R_FINITE(tOff) || tOff - tOn < 1e-12) continue;
      std::vector<double> _y(eff, 0.0);             // the window's start state
      for (size_t sg = 0; sg < segs.size(); ++sg)
        if (rxAdjSameTime(segs[sg].t0, tOn)) { for (int c = 0; c < nBase; ++c) _y[c] = segs[sg].y0[c]; break; }
      std::vector<double> _dR, _dF;
      double amt = amtRaw * rxAdjRecRows(op, ind, cSub, eff, i, tOn, _y.data(), _cmt, _dR, _dF);
      double R = amt / (tOff - tOn);
      double durMult = isDur ? amt : 1.0;
      size_t sgOn = segs.size(), sgOff = segs.size();
      for (size_t sg = 0; sg < segs.size(); ++sg) {
        double t0s = segs[sg].t0;
        if (t0s >= tOn - rxAdjTimeTol(t0s, tOn) && t0s < tOff - rxAdjTimeTol(t0s, tOff)) {
          if (sgOn == segs.size()) sgOn = sg;
          lsAdjAddG(segs[sg].infG, nBase, np, _cmt, durMult, _dR);
          if (isDur) lsAdjAddG(segs[sg].infG, nBase, np, _cmt, amtRaw / (tOff - tOn), _dF);
        } else if (rxAdjSameTime(t0s, tOff)) {
          sgOff = sg;
          lsAdjAddG(segs[sg].offG, nBase, np, _cmt, -(amt / R) * durMult, _dR);
          if (!isDur) lsAdjAddG(segs[sg].offG, nBase, np, _cmt, amtRaw, _dF);
        }
      }
      if (sgOn < segs.size())   // a state-dependent f(): the window moves with x_on
        rxAdjSfWinAdd(op, ind, cSub, nBase, eff, tOn, _y.data(), sgOn, sgOff, _cmt,
                      isDur ? amtRaw / (tOff - tOn) : amtRaw, i, sfw, true, !isDur);
    }
  }
  return hasInfusion;
}

extern "C" void ind_liblsodaadj_0(rx_solve *rx, rx_solving_options *op, struct lsoda_opt_t opt,
                                  int solveid, t_dydt_liblsoda c_dydt_ll, t_dydt c_dydt,
                                  t_update_inis u_inis) {
  int cSub = solveid; // subject id, not an rx->ordId position
  rx_solving_options_ind *ind = &(rx->subjects[cSub]);
  int nBase = op->adjNbase;
  int np    = op->adjNp;
  int eff   = rxEffNeq(ind, op);
  int fxOff = op->adjFxOff, fpOff = op->adjFpOff, sensOff = op->adjSensOff;

  // ---- forward: run the ordinary liblsoda solve, with recording hooks on -------
  lsAdjSteps.clear();
  lsAdjSegs.clear();
  rxAdjJumpClear(); _adjLagPush.clear(); _adjLagPushFixed.clear(); _adjLagPushMdl.clear();
  _lsSsKind = 0; _lsSsBolusIi = 0.0; _lsSsCmt = -1; _lsSsT = 0.0; _lsSsLag = 0.0; _lsSsLagOn = 0;   // reset the sticky ss snapshot
  _lsSsY.clear();
  _lsSsFixed = 0; _lsSsFixW = 0.0; _lsSsFixW2 = 0.0;
  _lsSsDur = _lsSsDur2 = _lsSsRate = _lsSsRate2 = 0.0;
  lsAdjNbase = nBase;
  lsAdjActive = true;                 // ind_liblsoda0 installs the ctx hooks
  lsAdjInd = ind;
  ind_liblsoda0(rx, op, opt, solveid, c_dydt_ll, u_inis);
  lsAdjActive = false;
  lsAdjInd = NULL;

  // this subject's failure, not op->badSolve, which any subject's sets
  if (ind->rc[0] < 0 || ind->err != 0) return;
  if (lsAdjSteps.empty() || lsAdjSegs.empty()) {   // no step taken: still at the initial conditions
    rk4sIniOnlyFill(op, ind, cSub, nBase, np, sensOff);
    return;
  }
  lsAdjSegs.back().stepEnd = lsAdjSteps.size();     // close the final segment
  size_t nStep = lsAdjSteps.size();

  // The events at time t among the first `upTo` records, in the order the
  // forward solve applied them; y is the state at t, for a dose's F*amt.
  // Additive boluses (dPhi/dy=I) carry only their F/alag duals.
  auto lsEventsAt = [&](double t, int upTo, const double *y) {
    std::vector<lsAdjEv> evs;
    std::vector<double> _y(eff, 0.0);
    for (int c = 0; c < nBase; ++c) _y[c] = y[c];
    // a lagged ss bolus's earlier-interval doses pushed past the record (a lag
    // longer than ii), which the forward solve applies before the records here
    for (size_t q = 0; q < _adjLagPush.size(); ++q) {
      int k = _adjLagPush[q];
      if (k >= ind->extraDoseN[0] || !rxAdjSameTime(ind->extraDoseTime[k], t)) continue;
      int _wh, _cmt, _wh100, _whI, _wh0; getWh(ind->extraDoseEvid[k], &_wh, &_cmt, &_wh100, &_whI, &_wh0);
      if (_whI != 0 || _cmt < 0 || _cmt >= nBase) continue;
      double amt = ind->extraDoseDose[k];
      const rxAdjPushCap *pc = rxAdjPushFind(k);
      std::vector<double> _yp(eff, 0.0);   // F reads the pre-dose state
      for (int c = 0; c < nBase; ++c) _yp[c] = pc ? pc->yPre[c] : _y[c];
      lsAdjEv _e = {0, _cmt, 1.0, amt, getAmt(ind, ind->id, _cmt, amt, t, _yp.data())};
      _e.push = k;
      evs.push_back(_e);
    }
    for (int i = 0; i < upTo; ++i) {
      int ev = getEvid(ind, ind->ix[i]);
      if (isObs(ev)) continue;
      if (!rxAdjSameTime(ind->timeThread[ind->ix[i]], t)) continue;
      if (ev == 3) { evs.push_back({3, -1, 1.0, 0.0, 0.0}); continue; }
      if (!isDose(ev)) continue;
      int _wh, _cmt, _wh100, _whI, _wh0; getWh(ev, &_wh, &_cmt, &_wh100, &_whI, &_wh0);
      if (_cmt < 0 || _cmt >= nBase) continue;
      // an ss = 1 dose (a bolus, or an infusion's start, not its stop; lagged or
      // not) replaces the state with Y_ss(p)
      bool _ss1 = (_wh0 == EVID0_SS || _wh0 == EVID0_SS0 || _wh0 == EVID0_SSINF);
      if (_ss1 && (_whI == 0 || getDose(ind, ind->ix[i]) > 0)) {
        evs.push_back({8, _cmt, 1.0, 0.0, 0.0});
        continue;
      }
      if (_ss1) continue;
      if (op->adjDfxOff >= 0 && _wh0 == EVID0_REGULAR &&
          ((_whI == EVIDF_INF_DUR && getDose(ind, ind->ix[i]) > 0) ||
           _whI == EVIDF_MODEL_RATE_ON || _whI == EVIDF_MODEL_DUR_ON)) {
        evs.push_back({12, _cmt, 0.0, 0.0, 0.0, i});
        continue;
      }
      if (_whI == 0) {
        double amt = getDose(ind, ind->ix[i]);
        evs.push_back({0, _cmt, 1.0, amt, getAmt(ind, ind->id, _cmt, amt, t, _y.data()), i});
      } else if (_whI == EVIDF_REPLACE) {
        evs.push_back({5, _cmt, 0.0, 0.0, 0.0, i});
      } else if (_whI == EVIDF_MULT) {
        evs.push_back({6, _cmt, getAmt(ind, ind->id, _cmt, getDose(ind, ind->ix[i]), t, _y.data()), 0.0, 0.0, i});
      }
      // (modeled rate/dur infusion on/off open a segment but are not a state jump;
      //  their dR/dp dual is reconstructed as windows below, not here.)
    }
    return evs;
  };
  // the events at each segment's start time (seg 0 included, for a modeled-F/lag
  // dose or a jump at t0)
  for (size_t sg = 0; sg < lsAdjSegs.size(); ++sg)
    lsAdjSegs[sg].ev = lsEventsAt(lsAdjSegs[sg].t0, ind->n_all_times, lsAdjSegs[sg].y0.data());

  std::vector<rxAdjSfWin> sfWins;   // fixed-dur windows with a state-dependent f()
  bool hasInfusion = rxAdjInfusionWindows(op, ind, cSub, nBase, eff, lsAdjSegs, &sfWins);
  std::vector<double> sfAcc(sfWins.size(), 0.0);

  std::vector<double> A(eff, 0.0);
  std::vector<double> lamFlat, adjPred, adjAcor, w, Pmat;
  std::vector<int> piv;
  int maxRows = 2;
  for (size_t s = 0; s < nStep; ++s) maxRows = std::max(maxRows, std::max(lsAdjSteps[s].nq, lsAdjSteps[s].nqNext) + 1);
  lamFlat.assign((size_t)(maxRows + 2) * nBase, 0.0); // costate rows 1..maxRows (1-based)
  adjPred.assign((size_t)(maxRows + 2) * nBase, 0.0);
  adjAcor.assign(nBase, 0.0);
  std::vector<double> acorExtra(nBase, 0.0);
  w.assign(nBase, 0.0);
  Pmat.assign((size_t)nBase * nBase, 0.0);
  piv.assign(nBase, 0);
  std::vector<double> mu(np, 0.0);
  double *lhs = ind->lhs;

  // Transpose one recorded step n in place on lamFlat (TRANSITION^T -> STEP^T ->
  // BRIDGE^T), accumulating the parameter quadrature into mu.  hPrevEnd for the
  // rejection bridge is segment-aware: a segment's first step bridges from its
  // own init h0, not the previous segment's last step.
  auto stepTransposeN = [&](size_t n) {
    const lsAdjStep &st = lsAdjSteps[n];
    int nq = st.nq; double h = st.h, el1 = st.el[1];
    const double *fx, *fp;
    lsAdjEval(cSub, st.tn, st.y.data(), nBase, eff, fxOff, fpOff, A.data(), lhs, &fx, &fp);
    // TRANSITION^T: scaleh^T then order^T
    std::fill(acorExtra.begin(), acorExtra.end(), 0.0);
    { int nqF = st.nqNext; double rp = 1.0;
      for (int j = 1; j <= nqF + 1; ++j) { double *lamj = &lamFlat[(size_t)j * nBase]; for (int c = 0; c < nBase; ++c) lamj[c] *= rp; rp *= st.rh; }
      if (st.dOrder == 1) { double *top = &lamFlat[(size_t)(nq + 2) * nBase]; for (int c = 0; c < nBase; ++c) { acorExtra[c] += st.rInc * top[c]; top[c] = 0.0; } }
      else if (st.dOrder == -1) { double *drop = &lamFlat[(size_t)(nq + 1) * nBase]; for (int c = 0; c < nBase; ++c) drop[c] = 0.0; }
    }
    // STEP^T: UPDATE^T -> CORRECT^T -> PREDICT^T
    for (int c = 0; c < nBase; ++c) adjAcor[c] = acorExtra[c];
    for (int j = 1; j <= nq + 1; ++j) { const double *lamj = &lamFlat[(size_t)j * nBase]; double *pj = &adjPred[(size_t)j * nBase]; double ej = st.el[j]; for (int c = 0; c < nBase; ++c) { pj[c] = lamj[c]; adjAcor[c] += ej * lamj[c]; } }
    for (int a = 0; a < nBase; ++a) for (int b = 0; b < nBase; ++b) Pmat[(size_t)a * nBase + b] = (a == b ? 1.0 : 0.0) - h * el1 * fx[a * nBase + b];
    for (int c = 0; c < nBase; ++c) w[c] = adjAcor[c];
    if (nBase > 0) { luFactor(Pmat.data(), nBase, piv.data()); luSolveT(Pmat.data(), nBase, piv.data(), w.data()); }
    double *p1 = &adjPred[(size_t)1 * nBase]; double *p2 = &adjPred[(size_t)2 * nBase];
    for (int b = 0; b < nBase; ++b) { double s = 0; for (int a = 0; a < nBase; ++a) s += fx[a * nBase + b] * w[a]; p1[b] += h * s; }
    for (int c = 0; c < nBase; ++c) p2[c] += -w[c];
    for (int p = 0; p < np; ++p) { double s = 0; for (int a = 0; a < nBase; ++a) s += fp[a * np + p] * w[a]; mu[p] += h * s; }
    // in-window infusion forcing: inside the infusion the RHS gains +R, so F_p[cmt]
    // gains dR/dp (and a dur()'s or fixed dur's F term) -> mu += h*infG^T w.
    { const lsAdjSeg &Sw = lsAdjSegs[st.seg];
      if (!Sw.infG.empty())
        for (int ic = 0; ic < nBase; ++ic)
          for (int p = 0; p < np; ++p) mu[p] += h * Sw.infG[(size_t)ic * np + p] * w[ic];
      for (size_t k = 0; k < sfWins.size(); ++k) {   // a state-dependent f(): forced at x_on
        const rxAdjSfWin &W = sfWins[k];
        if (W.atOff || st.seg < W.seg0 || st.seg >= W.seg1) continue;
        if (!W.dF.empty()) for (int p = 0; p < np; ++p) mu[p] += h * W.raw * W.dF[p] * w[W.cmt];
        sfAcc[k] += h * w[W.cmt];
      } }
    for (int j = 1; j <= nq; ++j) for (int i1 = nq; i1 >= j; --i1) { double *dst = &adjPred[(size_t)(i1 + 1) * nBase]; const double *src = &adjPred[(size_t)i1 * nBase]; for (int c = 0; c < nBase; ++c) dst[c] += src[c]; }
    std::fill(lamFlat.begin(), lamFlat.end(), 0.0);
    for (int j = 1; j <= nq + 1; ++j) { double *lamj = &lamFlat[(size_t)j * nBase]; const double *pj = &adjPred[(size_t)j * nBase]; for (int c = 0; c < nBase; ++c) lamj[c] = pj[c]; }
    // BRIDGE^T (segment-aware)
    double hPrevEnd = (n == lsAdjSegs[st.seg].stepBegin) ? lsAdjSegs[st.seg].h0 : lsAdjSteps[n - 1].hNext;
    double rhB = h / hPrevEnd;
    if (rhB != 1.0) { double rp = 1.0; for (int j = 1; j <= nq + 1; ++j) { double *lamj = &lamFlat[(size_t)j * nBase]; for (int c = 0; c < nBase; ++c) lamj[c] *= rp; rp *= rhB; } }
  };
  // bracketing step in segment seg for time t: first step with tn >= t (clamp).
  auto bracketStep = [&](int seg, double t) -> size_t {
    size_t b = lsAdjSegs[seg].stepBegin, e = lsAdjSegs[seg].stepEnd, S = b;
    while (S < e && lsAdjSteps[S].tn < t - rxAdjTimeTol(lsAdjSteps[S].tn, t)) ++S;
    if (S >= e) S = e - 1;
    return S;
  };
  // seed costate at (step S, time t) by intdy^T with weight wSeed (state covector).
  auto seedCostate = [&](size_t S, double t, const std::vector<double> &wSeed) {
    int nqSeed = lsAdjSteps[S].nqNext;
    double sS = (t - lsAdjSteps[S].tn) / lsAdjSteps[S].hNext;
    std::fill(lamFlat.begin(), lamFlat.end(), 0.0);
    double sp = 1.0;
    for (int j = 0; j <= nqSeed; ++j) { double *lamj = &lamFlat[(size_t)(j + 1) * nBase]; for (int c = 0; c < nBase; ++c) lamj[c] += sp * wSeed[c]; sp *= sS; }
  };

  // Backward sweep of one costate seed at (segIdx, time, wSeed) to t0, into mu.
  // Segments join at an event with an exact dual; the re-init yh0=[y0, h0*f(y0)]
  // couples rows 1 and 2, so the costate handed back is lam0[1] + h0*J(y0)^T*lam0[2]
  // (then the event jump's dPhi/dy^T).
  std::vector<double> wSeed(nBase);
  std::vector<double> sfRow, sfDF, sfDl;   // a state-dependent f() event: its row and blocks
  // ss=1 steady-state IC term (built below, before any sweep runs)
  rk4sSsIc lsIc; bool lsMono = false; rk4s_rec lsSsRec; rk4s_ss1Ev lsLagE;
  // continuous-infusion steady state: dY_ss/dp = -J^{-1} df/dp at Y_ss
  bool ssCont = false; std::vector<double> ssFXc, ssFP; std::vector<int> ssPiv;
  // lam^T dY_ss/dp of the subject's ss=1 steady state
  auto applySsIc = [&](std::vector<double> &lam, std::vector<double> &muA) {
    if (ssCont) {
      std::vector<double> w(lam.begin(), lam.begin() + nBase);   // w = J^{-T} lam
      luSolveT(ssFXc.data(), nBase, ssPiv.data(), w.data());
      for (int p = 0; p < np; ++p) { double v = 0; for (int j = 0; j < nBase; ++j) v += w[j] * ssFP[j * np + p]; muA[p] -= v; }
    }
    if (lsMono) lsIc.applyWindowIC(lam, muA);   // bolus / periodic-infusion monodromy
  };
  std::vector<double> dx0V;                         // parameter-dependent initial conditions
  rk4sIniDx0(op, ind, cSub, lsAdjSegs[0].t0, eff, nBase, np, dx0V);
  const double *dx0P = dx0V.empty() ? NULL : dx0V.data();
  auto sweepToStart = [&](int segIdx, double time) {
    std::fill(sfAcc.begin(), sfAcc.end(), 0.0);
    for (int seg = segIdx; ; --seg) {
      const lsAdjSeg &Sg = lsAdjSegs[seg];
      size_t Sb = bracketStep(seg, time);
      seedCostate(Sb, time, wSeed);
      for (long n = (long)Sb; n >= (long)Sg.stepBegin; --n) stepTransposeN((size_t)n);
      // lamFlat is now the costate of the segment's initial Nordsieck [y0, h0*f(y0)].
      const double *fx0, *fp0;
      lsAdjEval(cSub, Sg.t0, Sg.y0.data(), nBase, eff, fxOff, fpOff, A.data(), lhs, &fx0, &fp0);
      const double *lam1 = &lamFlat[(size_t)1 * nBase];
      const double *lam2 = &lamFlat[(size_t)2 * nBase];
      // init quadrature: mu += h0 * lam2^T Fp(t0,y0)
      for (int p = 0; p < np; ++p) { double s = 0; for (int c = 0; c < nBase; ++c) s += fp0[c * np + p] * lam2[c]; mu[p] += Sg.h0 * s; }
      // costate of y0 (pre-Nordsieck-init): lam_y0 = lam1 + h0*J0^T lam2.
      for (int b = 0; b < nBase; ++b) { double s = 0; for (int a = 0; a < nBase; ++a) s += fx0[a * nBase + b] * lam2[a]; wSeed[b] = lam1[b] + Sg.h0 * s; }
      // infusion OFF-boundary transversality: a modeled off time t_on+amt/R shifts
      // with R, a fixed rate's with F -> mu += offG^T lam(t_off), lam(t_off) = lam_y0.
      // (lhs holds calc_lhs at this off segment's (t0,y0), for the lag term below.)
      if (!Sg.offG.empty())
        for (int oc = 0; oc < nBase; ++oc)
          for (int p = 0; p < np; ++p) mu[p] += Sg.offG[(size_t)oc * np + p] * wSeed[oc];
      for (size_t q = 0; q < sfWins.size(); ++q)   // a state-dependent rate() window's stop
        if (sfWins[q].atOff && sfWins[q].seg1 == (size_t)seg) sfAcc[q] += wSeed[sfWins[q].cmt];
      if (op->adjDlagOff >= 0 && !Sg.lagRate.empty()) { const double *dlag = &lhs[op->adjDlagOff];
        for (int c = 0; c < nBase; ++c) if (Sg.lagRate[c] != 0.0)
          for (int p = 0; p < np; ++p) mu[p] += -Sg.lagRate[c] * dlag[c * np + p] * wSeed[c]; }
      // The events at this segment's start, last first.  A dose adds its explicit-
      // parameter quadrature with the costate of its post-dose state (modeled F ->
      // mu += amt*dF/dp*lam[c]; alag -> mu += -F*amt*dlag/dp*(lam^T F_X[:,c])); a jump
      // maps the costate back through dPhi/dy^T (reset 0, replace[c] 0, multiply[c]
      // *factor).  A reset or an ss = 1 dose replaces the whole state, so the events
      // recorded before it do not reach later observations and the sweep stops
      // there.  At the window start the caller adds the ss=1 and initial-condition
      // terms with the costate left here.
      for (size_t k = Sg.ev.size(); k-- > 0;) {
        const lsAdjEv &E = Sg.ev[k];
        // a state-dependent f(): the event's blocks at its pre-event state and the
        // jump's extra row (then lhs/fx0 back at this segment's start)
        const rxAdjPushCap *pc = E.push >= 0 ? rxAdjPushFind(E.push) : NULL;
        bool sf = pc ? rxAdjStateFPushEvent(op, ind, cSub, pc, E.cmt, Sg.t0, E.amt, sfRow, sfDF, sfDl)
                     : ((E.type == 0 || E.type == 5 || E.type == 6) &&
                        rxAdjStateFEvent(op, ind, cSub, E.rec, E.cmt, Sg.t0, sfRow, sfDF, sfDl));
        if (sf) lsAdjEval(cSub, Sg.t0, Sg.y0.data(), nBase, eff, fxOff, fpOff, A.data(), lhs, &fx0, &fp0);
        const double *dFp = sf ? (sfDF.empty() ? NULL : sfDF.data()) : (op->adjDfOff >= 0 ? &lhs[op->adjDfOff] : NULL);
        const double *dlp = sf ? (sfDl.empty() ? NULL : sfDl.data()) : (op->adjDlagOff >= 0 ? &lhs[op->adjDlagOff] : NULL);
        const double *dfx = sf ? sfRow.data() : NULL;
        double wC = (E.cmt >= 0 && E.cmt < nBase) ? wSeed[E.cmt] : 0.0;
        if (E.type == 0) {
          int c = E.cmt;
          if (dFp) { for (int p = 0; p < np; ++p) mu[p] += E.amt * dFp[c * np + p] * wSeed[c]; }
          if (dlp && pc) rxAdjPushLagTerm(pc, c, wSeed.data(), mu.data(), nBase, np, dlp, dfx);
          else if (dlp && !rxAdjJumpTerm(E.rec, c, wSeed.data(), mu.data(), nBase, np, dlp, NULL, dfx)) {
            double sc = 0; for (int i = 0; i < nBase; ++i) sc += wSeed[i] * fx0[i * nBase + c];
            for (int p = 0; p < np; ++p) mu[p] += -E.amtF * dlp[c * np + p] * sc; }
          if (dfx) for (int j = 0; j < nBase; ++j) wSeed[j] += wC * dfx[j];
        } else if (E.type == 3) {             // reset restores the initial conditions
          if (seg > 0) {
            rk4sAddIni(dx0P, wSeed.data(), mu.data(), nBase, np);
            for (int b = 0; b < nBase; ++b) wSeed[b] = 0.0;
          }
          break;
        } else if (E.type == 8) {             // ss=1: the state becomes Y_ss(p)
          if (seg > 0) {
            applySsIc(wSeed, mu);
            for (int b = 0; b < nBase; ++b) wSeed[b] = 0.0;
          }
          break;
        } else if (E.type == 5 || E.type == 6) {
          rxAdjJumpTerm(E.rec, E.cmt, wSeed.data(), mu.data(), nBase, np, dlp, dFp, dfx);
          if (E.type == 5) wSeed[E.cmt] = 0.0; else wSeed[E.cmt] *= rxAdjJumpFactor(E.rec, E.factor);
          if (dfx) for (int j = 0; j < nBase; ++j) wSeed[j] += wC * dfx[j];
        } else if (E.type == 12) {            // a state-dependent f() window starts: its rate moved with the state
          for (size_t q = 0; q < sfWins.size(); ++q)
            if (sfWins[q].seg0 == (size_t)seg && sfWins[q].rec == E.rec) {
              rxAdjSfWinLag(sfWins[q], sfAcc[q], mu.data(), nBase, np);
              for (int b = 0; b < nBase; ++b) wSeed[b] += sfAcc[q] * sfWins[q].row[b];
              sfAcc[q] = 0.0;
            }
        }
      }
      if (seg == 0) break;
      time = Sg.t0;
    }
  };

  // The segment the ss=1 steady state opened: 0 when it starts the window; an
  // interior one (any ss=1 kind) replaces the state there with Y_ss(p).
  int lsSsSeg = -1;
  if (_lsSsKind != 0)
    for (size_t sg = 0; sg < lsAdjSegs.size(); ++sg)
      if (rxAdjSameTime(lsAdjSegs[sg].t0, _lsSsT)) { lsSsSeg = (int)sg; break; }
  bool ssAtStart = (lsSsSeg == 0);
  // the ss record (the last ss = 1 dose at its time), whose covariates the
  // steady state reads
  int lsSsRecI = -1;
  if (_lsSsKind != 0)
    for (int i = 0; i < ind->n_all_times; ++i) {
      int ev = getEvid(ind, ind->ix[i]);
      if (isObs(ev) || !isDose(ev) || !rxAdjSameTime(ind->timeThread[ind->ix[i]], _lsSsT)) continue;
      int _wh, _cmt, _wh100, _whI, _wh0; getWh(ev, &_wh, &_cmt, &_wh100, &_whI, &_wh0);
      if ((_wh0 == EVID0_SS || _wh0 == EVID0_SS0 || _wh0 == EVID0_SSINF) && (_whI == 0 || getDose(ind, ind->ix[i]) > 0))
        lsSsRecI = i;
    }
  // The steady state's starting state: the state right after the (last) ss dose,
  // before later same-time events; the snapshot needs an ss segment, or (an ss
  // dose at the last record time, after which no segment opens) an observation
  // there.
  std::vector<double> ssY0;
  bool ssAtLast = false;
  if (lsSsSeg >= 0) {
    ssY0 = lsAdjSegs[lsSsSeg].y0;
  } else if (_lsSsKind != 0 && _lsSsT > lsAdjSegs.back().t0 + rxAdjTimeTol(lsAdjSegs.back().t0, _lsSsT)) {
    for (int i = 0; i < ind->n_all_times; ++i) {
      if (!isObs(getEvid(ind, ind->ix[i])) || !rxAdjSameTime(ind->timeThread[ind->ix[i]], _lsSsT)) continue;
      const double *o = getSolve(i);
      ssY0.assign(o, o + nBase);
      ssAtLast = true;
      break;
    }
  }
  if (!ssY0.empty() && (int)_lsSsY.size() >= nBase) ssY0.assign(_lsSsY.begin(), _lsSsY.begin() + nBase);
  if (lsSsSeg > 0) {
    std::vector<lsAdjEv> &E = lsAdjSegs[lsSsSeg].ev;
    bool has8 = false;
    for (size_t k = 0; k < E.size(); ++k) has8 = has8 || E[k].type == 8;
    if (!has8) E.insert(E.begin(), lsAdjEv{8, -1, 1.0, 0.0, 0.0});
  }
  // The infusions of earlier periods still running at the ss time: for an ss = 1
  // fixed infusion longer than ii, the n before it, ending at t_ss + offTime +
  // (k-1)*ii; for a lagged one, those started s0 + m*ii before it (s0 = ii - lag,
  // or 0 for lsLag0), ending at t_ss + D - s0 - m*ii.  They end by pushed stops that are not
  // event-table records.  Each carries the dF dual of a fixed window: a fixed
  // dur's forcing until its end, or a fixed rate's off-boundary term (its end
  // moves with F).
  // A lag of a whole number of periods (lag modulo ii == 0) starts this period's
  // infusion at the ss time, inside handleSS; unless its start record lies there
  // too (a zero lag), no record opens its window or carries its lag dual.
  bool lsLag0 = false;
  if (lsSsSeg >= 0 && _lsSsKind == 1 && _lsSsLagOn && _lsSsLag == 0.0 &&
      _lsSsRate > _lsSsRate2 && _lsSsCmt >= 0 && _lsSsCmt < nBase) {
    lsLag0 = !rk4sInfusionStartAt(ind, nBase, _lsSsCmt, _lsSsT);
    if (lsLag0 && op->adjDlagOff >= 0) {
      std::vector<double> &L = lsAdjSegs[lsSsSeg].lagRate;
      if (L.empty()) L.assign(nBase, 0.0);
      L[_lsSsCmt] += _lsSsRate - _lsSsRate2;
    }
  }
  if (lsSsSeg >= 0 && _lsSsKind == 1 && _lsSsFixed != 0 && op->adjDfOff >= 0 &&
      _lsSsRate > _lsSsRate2 && _lsSsCmt >= 0 && _lsSsCmt < nBase) {
    int c = _lsSsCmt;
    double r1 = _lsSsRate - _lsSsRate2, ii = _lsSsDur + _lsSsDur2;
    int n = (int)floor(_lsSsRate2 / r1 + 0.5);
    double full = _lsSsDur + n * ii;
    std::vector<double> ends;
    if (_lsSsLagOn) {
      double s0 = lsLag0 ? 0.0 : ii - _lsSsLag;
      for (int m = 0; full - s0 - m * ii > rxAdjTimeTol(full, s0); ++m) ends.push_back(_lsSsT + full - s0 - m * ii);
    } else {
      for (int k = 1; k <= n; ++k) ends.push_back(_lsSsT + _lsSsDur + (k - 1) * ii);
    }
    std::vector<double> _dR, _dF;
    double f = rxAdjRecRows(op, ind, cSub, eff, lsSsRecI, _lsSsT, lsAdjSegs[lsSsSeg].y0.data(), c, _dR, _dF);
    if (f != 0.0)
      for (size_t k = 0; k < ends.size(); ++k) {
        double tEnd = ends[k];
        for (size_t sg = (size_t)lsSsSeg; sg < lsAdjSegs.size(); ++sg) {
          if (lsAdjSegs[sg].t0 >= tEnd - rxAdjTimeTol(lsAdjSegs[sg].t0, tEnd)) {
            if (_lsSsFixed == 1 && rxAdjSameTime(lsAdjSegs[sg].t0, tEnd))
              lsAdjAddG(lsAdjSegs[sg].offG, nBase, np, c, r1 * full / f, _dF);
            break;
          }
          if (_lsSsFixed == 2) lsAdjAddG(lsAdjSegs[sg].infG, nBase, np, c, r1 / f, _dF);
        }
      }
  }
  // Continuous-infusion steady state (kind 2): that segment's y0 is the constant
  // Y_ss with dY_ss/dp = -J^{-1} df/dp, so its contribution per observation is
  // -(J^{-T} lam)^T df/dp.
  ssCont = (_lsSsKind == 2) && !ssY0.empty() && nBase > 0;
  if (ssCont) {
    const double *fx0, *fp0;
    lsAdjEval(cSub, _lsSsT, ssY0.data(), nBase, eff, fxOff, fpOff, A.data(), lhs, &fx0, &fp0);
    ssFXc.assign(fx0, fx0 + (size_t)nBase * nBase);
    ssFP.assign(fp0, fp0 + (size_t)nBase * np);
    ssPiv.resize(nBase);
    if (!luFactor(ssFXc.data(), nBase, ssPiv.data())) ssCont = false;   // singular J
  }

  // ss==1 bolus / fixed-rate finite infusion: the missing IC is the monodromy
  // fixed point S_ss = (I-M)^{-1}B of one steady-state period.  Re-record that
  // period from y0 with a dop853 tableau and reuse the shared rk4sSsIc series.
  if (!ssCont && !ssY0.empty() && op->adjoint && fxOff >= 0 && nBase > 0 && _lsSsKind == 1) {
    int neqOde = op->neq - op->numLin - op->numLinSens;
    int neq2[2]; neq2[0] = eff; neq2[1] = cSub;
    rksTableau Tss = rksGetTableau(200);            // dop853 explicit tableau
    std::vector<double> y0(ssY0.begin(), ssY0.end()), ssScr;
    y0.resize(eff, 0.0);
    bool recorded = false, lsSsTrough = false; size_t lsSsOnSteps = 0;
    // the ss bolus record: its cmt, raw amount and whether it is lagged
    int lsSsBCmt = -1; double lsSsBAmt = 0.0;
    lsLagE.lagged = (_lsSsLagOn != 0); lsLagE.lag = _lsSsLag;
    lsLagE.recIdx = lsSsRecI; lsLagE.tRec = _lsSsT;
    if (_lsSsBolusIi > 0.0)
      for (int i = 0; i < ind->n_all_times; ++i) {
        int ev = getEvid(ind, ind->ix[i]);
        if (isObs(ev) || !isDose(ev) || !rxAdjSameTime(ind->timeThread[ind->ix[i]], _lsSsT)) continue;
        int _wh, _cmt, _wh100, _whI, _wh0; getWh(ev, &_wh, &_cmt, &_wh100, &_whI, &_wh0);
        if (_wh0 != EVID0_SS && _wh0 != EVID0_SS0 && _wh0 != EVID0_SSINF) continue;
        // the last ss = 1 dose at that time sets the steady state
        if (_whI == 0 && _cmt >= 0 && _cmt < nBase) { lsSsBCmt = _cmt; lsSsBAmt = getDose(ind, ind->ix[i]); }
        else if (_whI != 0 && getDose(ind, ind->ix[i]) > 0) lsSsBCmt = -1;   // a later ss infusion start
      }
    if (_lsSsBolusIi > 0.0 && lsLagE.lagged && lsSsBCmt >= 0) {   // lagged bolus: peak period from the trough
      rk4sRecordLagBolus(ind, op, c_dydt, neq2, Tss, neqOde, lsLagE, y0.data(), lsSsBCmt, lsSsBAmt, _lsSsBolusIi, &lsSsRec);
      recorded = true;
    } else if (_lsSsBolusIi > 0.0) {                 // bolus: flow from peak over ii
      rks_step_interval(ind, op, c_dydt, neq2, Tss, neqOde, nBase, y0.data(), 0.0, _lsSsBolusIi, &lsSsRec, ssScr);
      recorded = true; lsSsTrough = true;               // y0 now holds the trough
    } else if (lsLagE.lagged) {                      // lagged infusion: its period from the period start
      lsLagE.inf = true; lsLagE.infCmt = _lsSsCmt;
      lsLagE.dur = _lsSsDur; lsLagE.dur2 = _lsSsDur2; lsLagE.rate = _lsSsRate; lsLagE.rate2 = _lsSsRate2;
      rk4sRecordLagInfusion(ind, op, c_dydt, neq2, Tss, neqOde, lsLagE, y0.data(), &lsSsRec);
      lsSsOnSteps = lsLagE.onSteps;
      recorded = true;
    } else {                                         // periodic/large-dur infusion: two-phase from trough
      double savedR = ind->InfusionRate[_lsSsCmt], t = 0.0;
      if (_lsSsDur > 0.0) { ind->InfusionRate[_lsSsCmt] = _lsSsRate;
        rks_step_interval(ind, op, c_dydt, neq2, Tss, neqOde, nBase, y0.data(), t, t + _lsSsDur, &lsSsRec, ssScr); t += _lsSsDur; }
      lsSsOnSteps = lsSsRec.nStep();
      if (_lsSsDur2 > 0.0) { ind->InfusionRate[_lsSsCmt] = _lsSsRate2;
        rks_step_interval(ind, op, c_dydt, neq2, Tss, neqOde, nBase, y0.data(), t, t + _lsSsDur2, &lsSsRec, ssScr); }
      ind->InfusionRate[_lsSsCmt] = savedR;
      recorded = true;
    }
    if (recorded && lsSsRec.nStep() > 0) {
      std::vector<double> Asc(eff, 0.0);
      lsIc.build(cSub, ind, op, lsSsRec, fxOff, fpOff, eff, Asc,
                 &lsSsRec, NULL, NULL, NULL, NULL, NULL, NULL, Tss, nBase, np, 0);
      lsMono = lsIc.haveMono;
      // a fixed-rate/duration ss infusion with a modeled F: the period's dF terms
      if (lsMono && _lsSsBolusIi <= 0.0 && _lsSsFixed != 0) {
        rk4s_ss1Ev E; E.inf = true; E.infCmt = _lsSsCmt; E.onSteps = lsSsOnSteps;
        E.recIdx = lsSsRecI; E.tRec = _lsSsT;
        E.fixed = _lsSsFixed; E.fixW = _lsSsFixW; E.fixW2 = _lsSsFixW2;
        lsIc.haveWsFC = lsIc.makeForce(cSub, ind, op, lsSsRec, E, Asc, lsIc.wsFC, false);
      }
      // an ss bolus's bioavailability enters every period's dose (no separate
      // dose term here, so from the first)
      if (lsMono && _lsSsBolusIi > 0.0 && lsSsBCmt >= 0) {
        // a lagged bolus's trough is its recorded period's end
        const double *lsTr = lsSsTrough ? y0.data() : (!lsLagE.yEnd.empty() ? lsLagE.yEnd.data() : NULL);
        lsIc.loadDF(cSub, ind, op, _lsSsT, lsTr ? lsTr : ssY0.data(), Asc, lsSsRecI,
                    lsTr ? lsSsBCmt : -1, lsSsBAmt);
        if (!lsIc.dFv.empty()) { lsIc.fFrom = 0; lsIc.wsFCmt = lsSsBCmt; lsIc.wsFAmt = lsSsBAmt; }
      }
      if (lsMono && lsLagE.lagged && lsLagE.lagSteps > 0) lsIc.wsLag = &lsLagE;
    }
  }

  double tReset = R_NegInf;   // the latest reset recorded before the current record
  for (int i = 0; i < ind->n_all_times; ++i) {
    int evI = getEvid(ind, ind->ix[i]);
    if (evI == 3) { tReset = ind->timeThread[ind->ix[i]]; continue; }
    if (!isObs(evI)) continue;
    double ti = ind->timeThread[ind->ix[i]];
    int segObs = 0;
    for (int sgi = (int)lsAdjSegs.size() - 1; sgi >= 0; --sgi) {
      const lsAdjSeg &Sg = lsAdjSegs[sgi];
      if (Sg.t0 > ti + rxAdjTimeTol(Sg.t0, ti)) continue;
      // a segment opened at the observation's time only by records stored after
      // it (a dose the model pushed there) starts after the observation
      bool after = sgi > 0 && rxAdjSameTime(Sg.t0, ti) && !Sg.ev.empty();
      for (size_t k = 0; after && k < Sg.ev.size(); ++k) after = Sg.ev[k].rec > i;
      if (after) continue;
      segObs = sgi; break;
    }
    double *out = getSolve(i);
    // A reset recorded before this observation but after its segment opened
    // means liblsoda took no step since the reset (e.g. an all-zero state),
    // so no segment starts there: the observation still reads the reset state,
    // which is the initial condition.
    if (tReset > lsAdjSegs[segObs].t0 + rxAdjTimeTol(tReset, lsAdjSegs[segObs].t0) && tReset < ti - rxAdjTimeTol(tReset, ti)) {
      for (int q = 0; q < nBase * np; ++q) out[sensOff + q] = dx0P ? dx0P[q] : 0.0;
      continue;
    }
    // No segment opens after the events at the last record time, so an
    // observation there transposes the events recorded before it itself.
    std::vector<lsAdjEv> evAt;
    if (lsAdjSegs[segObs].t0 < ti - rxAdjTimeTol(lsAdjSegs[segObs].t0, ti)) {
      evAt = lsEventsAt(ti, i, out);
      bool has8 = false;
      for (size_t k = 0; k < evAt.size(); ++k) has8 = has8 || evAt[k].type == 8;
      if (ssAtLast && !has8 && rxAdjSameTime(ti, _lsSsT)) evAt.insert(evAt.begin(), lsAdjEv{8, -1, 1.0, 0.0, 0.0});
    }
    for (int k = 0; k < nBase; ++k) {
      std::fill(mu.begin(), mu.end(), 0.0);
      for (int c = 0; c < nBase; ++c) wSeed[c] = (c == k) ? 1.0 : 0.0;
      bool atIni = false;   // a reset or ss=1 dose at ti: the sweep stops there
      if (!evAt.empty()) {
        const double *fxT, *fpT;
        lsAdjEval(cSub, ti, out, nBase, eff, fxOff, fpOff, A.data(), lhs, &fxT, &fpT);
        for (size_t e = evAt.size(); e-- > 0 && !atIni;) {
          const lsAdjEv &E = evAt[e];
          const rxAdjPushCap *pc = E.push >= 0 ? rxAdjPushFind(E.push) : NULL;
          bool sf = pc ? rxAdjStateFPushEvent(op, ind, cSub, pc, E.cmt, ti, E.amt, sfRow, sfDF, sfDl)
                       : ((E.type == 0 || E.type == 5 || E.type == 6) &&
                          rxAdjStateFEvent(op, ind, cSub, E.rec, E.cmt, ti, sfRow, sfDF, sfDl));
          if (sf) lsAdjEval(cSub, ti, out, nBase, eff, fxOff, fpOff, A.data(), lhs, &fxT, &fpT);
          const double *dFp = sf ? (sfDF.empty() ? NULL : sfDF.data()) : (op->adjDfOff >= 0 ? &lhs[op->adjDfOff] : NULL);
          const double *dlp = sf ? (sfDl.empty() ? NULL : sfDl.data()) : (op->adjDlagOff >= 0 ? &lhs[op->adjDlagOff] : NULL);
          const double *dfx = sf ? sfRow.data() : NULL;
          double wC = (E.cmt >= 0 && E.cmt < nBase) ? wSeed[E.cmt] : 0.0;
          if (E.type == 0) {
            int c = E.cmt;
            if (dFp) { for (int p = 0; p < np; ++p) mu[p] += E.amt * dFp[c * np + p] * wSeed[c]; }
            if (dlp && pc) rxAdjPushLagTerm(pc, c, wSeed.data(), mu.data(), nBase, np, dlp, dfx);
            else if (dlp && !rxAdjJumpTerm(E.rec, c, wSeed.data(), mu.data(), nBase, np, dlp, NULL, dfx)) {
              double sc = 0; for (int b = 0; b < nBase; ++b) sc += wSeed[b] * fxT[b * nBase + c];
              for (int p = 0; p < np; ++p) mu[p] += -E.amtF * dlp[c * np + p] * sc; }
            if (dfx) for (int j = 0; j < nBase; ++j) wSeed[j] += wC * dfx[j];
          } else if (E.type == 3) {
            rk4sAddIni(dx0P, wSeed.data(), mu.data(), nBase, np);
            atIni = true;
          } else if (E.type == 8) {         // ss=1: the state is Y_ss(p)
            applySsIc(wSeed, mu);
            atIni = true;
          } else if (E.type == 5 || E.type == 6) {
            rxAdjJumpTerm(E.rec, E.cmt, wSeed.data(), mu.data(), nBase, np, dlp, dFp, dfx);
            if (E.type == 5) wSeed[E.cmt] = 0.0; else wSeed[E.cmt] *= rxAdjJumpFactor(E.rec, E.factor);
            if (dfx) for (int j = 0; j < nBase; ++j) wSeed[j] += wC * dfx[j];
          }
        }
      }
      if (atIni) {
        for (int p = 0; p < np; ++p) out[sensOff + k * np + p] = mu[p];
        continue;
      }
      sweepToStart(segObs, ti);
      if (ssAtStart) applySsIc(wSeed, mu);   // window-start steady-state IC term
      // the window start holds the initial conditions unless the ss dose is there
      if (!ssAtStart) rk4sAddIni(dx0P, wSeed.data(), mu.data(), nBase, np);
      for (int p = 0; p < np; ++p) out[sensOff + k * np + p] = mu[p];
    }
  }

  // ---- Self-check (RX_LSADJ_SELFCHECK=1): two replays on the same recorded
  // schedule -- adjoint vs forward-sensitivity replay (exact-transpose property)
  // and primal replay vs liblsoda's recorded y (step-map model).  Skipped for
  // infusion models (the replay dydt lacks the runtime infusion rate; validate
  // those vs dop853s).
  if (getenv("RX_LSADJ_SELFCHECK") && hasInfusion) {
    REprintf("[lsadj] self-check skipped (infusion forcing not modeled in the replay); validate vs dop853s\n");
  } else if (getenv("RX_LSADJ_SELFCHECK")) {
    int R = maxRows;
    std::vector<double> yhP((size_t)(R + 2) * nBase, 0.0);
    std::vector<double> YHS((size_t)(R + 2) * nBase * np, 0.0);
    std::vector<double> rhs((size_t)nBase * np), dac((size_t)nBase * np), P2(Pmat);
    std::vector<double> yPred(nBase), acor(nBase), fEval(eff, 0.0), Aw(eff, 0.0);
    std::vector<double> Sin((size_t)nBase * np, 0.0);   // incoming state sensitivity
    if (dx0P && !ssAtStart) Sin.assign(dx0P, dx0P + (size_t)nBase * np);   // the initial conditions
    auto SX = [&](int row, int c){ return (size_t)row * nBase + c; };
    auto IDX = [&](int row, int c, int p){ return ((size_t)row * nBase + c) * np + p; };
    int neqP[2]; neqP[0] = eff; neqP[1] = cSub;
    double worstSens = 0.0, worstPrimal = 0.0;
    // obs -> bracketing flat step index (as in the backward)
    std::vector<int> obsIdx; std::vector<size_t> obsBrk;
    for (int i = 0; i < ind->n_all_times; ++i) if (isObs(getEvid(ind, ind->ix[i]))) {
      double ti = ind->timeThread[ind->ix[i]];
      int sg = 0; for (int s = (int)lsAdjSegs.size()-1; s >= 0; --s) { if (lsAdjSegs[s].t0 <= ti + rxAdjTimeTol(lsAdjSegs[s].t0, ti)) { sg = s; break; } }
      size_t S = lsAdjSegs[sg].stepBegin, e = lsAdjSegs[sg].stepEnd;
      while (S < e && lsAdjSteps[S].tn < ti - rxAdjTimeTol(lsAdjSteps[S].tn, ti)) ++S; if (S >= e) S = e - 1;
      obsIdx.push_back(i); obsBrk.push_back(S);
    }
    // forward replay, segment by segment (re-init the Nordsieck at each; carry the
    // state sensitivity Sin across the interior-event boundary).
    for (size_t sg = 0; sg < lsAdjSegs.size(); ++sg) {
      const lsAdjSeg &Sg = lsAdjSegs[sg];
      std::fill(yhP.begin(), yhP.end(), 0.0);
      std::fill(YHS.begin(), YHS.end(), 0.0);
      const double *fx0, *fp0;
      lsAdjEval(cSub, Sg.t0, Sg.y0.data(), nBase, eff, fxOff, fpOff, A.data(), lhs, &fx0, &fp0);
      for (int c = 0; c < nBase; ++c) yhP[SX(1, c)] = Sg.y0[c];
      for (int c = 0; c < eff; ++c) Aw[c] = 0.0;
      for (int c = 0; c < nBase; ++c) Aw[c] = Sg.y0[c];
      dydt(neqP, Sg.t0, Aw.data(), fEval.data());
      for (int c = 0; c < nBase; ++c) yhP[SX(2, c)] = Sg.h0 * fEval[c];
      // yh0 sens: row1 = dPhi/dy * Sin (event jump), row2 = h0*(J*row1 + Fp)
      for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) YHS[IDX(1, c, p)] = Sin[(size_t)c * np + p];
      // the events at the segment start, in forward order (as the backward's
      // transpose): reset -> dX0/dp, replace[c] -> 0, multiply[c] *= factor; a
      // modeled-F dose adds amt*dF/dp; a modeled alag shifts the dose time,
      // S+ = S- + (f(y-)-f(y+))*dlag/dp (its linearization -F*amt*F_X[:,c]*dlag/dp
      // for a dose pushed at run time).
      for (size_t k = 0; k < Sg.ev.size(); ++k) {
        const lsAdjEv &E = Sg.ev[k];
        if (E.type == 3 && sg > 0) {
          for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) YHS[IDX(1, c, p)] = dx0P ? dx0P[c * np + p] : 0.0;
        } else if (E.type == 5 || E.type == 6) {
          for (int p = 0; p < np; ++p) YHS[IDX(1, E.cmt, p)] = (E.type == 5) ? 0.0 : YHS[IDX(1, E.cmt, p)] * E.factor;
          // a lagged jump: S+ = G S- + r*dlag/dp; a modeled F adds amt*dF/dp
          // (replace) or x-*amt*dF/dp (multiply)
          const rxAdjJumpCap *cap = rxAdjJumpFind(E.rec);
          if (op->adjDfOff >= 0 && cap != NULL) { const double *dF = &lhs[op->adjDfOff];
            double w = cap->raw * (E.type == 6 ? cap->xPre : 1.0);
            for (int p = 0; p < np; ++p) YHS[IDX(1, E.cmt, p)] += w * dF[E.cmt * np + p]; }
          if (op->adjDlagOff >= 0 && cap != NULL && !cap->r.empty()) { const double *dlag = &lhs[op->adjDlagOff];
            for (int i = 0; i < nBase; ++i) for (int p = 0; p < np; ++p) YHS[IDX(1, i, p)] += cap->r[i] * dlag[E.cmt * np + p]; }
        }
        else if (E.type == 0) {
          int c = E.cmt;
          if (op->adjDfOff >= 0) { const double *dF = &lhs[op->adjDfOff]; for (int p = 0; p < np; ++p) YHS[IDX(1, c, p)] += E.amt * dF[c * np + p]; }
          if (op->adjDlagOff >= 0) { const double *dlag = &lhs[op->adjDlagOff];
            const rxAdjJumpCap *cap = rxAdjJumpFind(E.rec);
            for (int i = 0; i < nBase; ++i) for (int p = 0; p < np; ++p)
              YHS[IDX(1, i, p)] += (cap != NULL && !cap->r.empty() ? cap->r[i] : -E.amtF * fx0[i * nBase + c]) * dlag[c * np + p]; }
        }
      }
      for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) {
        double jr = 0; for (int b = 0; b < nBase; ++b) jr += fx0[c * nBase + b] * YHS[IDX(1, b, p)];
        YHS[IDX(2, c, p)] = Sg.h0 * (jr + fp0[c * np + p]);
      }
      for (size_t n = Sg.stepBegin; n < Sg.stepEnd; ++n) {
        const lsAdjStep &st = lsAdjSteps[n]; int nq = st.nq; double h = st.h, el1 = st.el[1];
        const double *fx, *fp; lsAdjEval(cSub, st.tn, st.y.data(), nBase, eff, fxOff, fpOff, A.data(), lhs, &fx, &fp);
        // BRIDGE scaleh (segment-aware hPrevEnd)
        { double hPrevEnd = (n == Sg.stepBegin) ? Sg.h0 : lsAdjSteps[n - 1].hNext;
          double rhB = h / hPrevEnd;
          if (rhB != 1.0) { double rp = 1.0; for (int j = 1; j <= nq + 1; ++j) {
            for (int c = 0; c < nBase; ++c) yhP[SX(j, c)] *= rp;
            for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) YHS[IDX(j, c, p)] *= rp;
            rp *= rhB; } } }
        // PREDICT
        for (int j = nq; j >= 1; --j) for (int i1 = j; i1 <= nq; ++i1) {
          for (int c = 0; c < nBase; ++c) yhP[SX(i1, c)] += yhP[SX(i1 + 1, c)];
          for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) YHS[IDX(i1, c, p)] += YHS[IDX(i1 + 1, c, p)];
        }
        for (int a = 0; a < nBase; ++a) for (int b = 0; b < nBase; ++b) P2[(size_t)a*nBase+b] = (a==b?1.0:0.0) - h*el1*fx[a*nBase+b];
        luFactor(P2.data(), nBase, piv.data());
        // CORRECT primal
        for (int c = 0; c < nBase; ++c) acor[c] = 0.0;
        for (int it = 0; it < 6; ++it) {
          for (int c = 0; c < nBase; ++c) yPred[c] = yhP[SX(1, c)] + el1 * acor[c];
          for (int c = 0; c < eff; ++c) Aw[c] = 0.0;
          for (int c = 0; c < nBase; ++c) Aw[c] = yPred[c];
          dydt(neqP, st.tn, Aw.data(), fEval.data());
          std::vector<double> del(nBase);
          for (int c = 0; c < nBase; ++c) del[c] = h * fEval[c] - (yhP[SX(2, c)] + acor[c]);
          luSolve(P2.data(), nBase, piv.data(), del.data());
          double nd = 0; for (int c = 0; c < nBase; ++c) { acor[c] += del[c]; nd = std::max(nd, fabs(del[c])); }
          if (nd < 1e-14) break;
        }
        // CORRECT sensitivity
        for (int p = 0; p < np; ++p) for (int a = 0; a < nBase; ++a) {
          double jr = 0; for (int b = 0; b < nBase; ++b) jr += fx[a*nBase+b]*YHS[IDX(1,b,p)];
          rhs[(size_t)a*np+p] = h*jr - YHS[IDX(2,a,p)] + h*fp[a*np+p];
        }
        for (int p = 0; p < np; ++p) { std::vector<double> col(nBase); for (int a=0;a<nBase;++a) col[a]=rhs[(size_t)a*np+p]; luSolve(P2.data(), nBase, piv.data(), col.data()); for (int a=0;a<nBase;++a) dac[(size_t)a*np+p]=col[a]; }
        // UPDATE
        for (int j = 1; j <= nq + 1; ++j) {
          for (int c = 0; c < nBase; ++c) yhP[SX(j, c)] += st.el[j] * acor[c];
          for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) YHS[IDX(j, c, p)] += st.el[j] * dac[(size_t)c*np+p];
        }
        for (int c = 0; c < nBase; ++c) worstPrimal = std::max(worstPrimal, fabs(yhP[SX(1, c)] - st.y[c]));
        // TRANSITION (order change + scaleh)
        if (st.dOrder == 1) {
          for (int c = 0; c < nBase; ++c) yhP[SX(nq + 2, c)] = acor[c] * st.rInc;
          for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) YHS[IDX(nq + 2, c, p)] = dac[(size_t)c*np+p] * st.rInc;
        } else if (st.dOrder == -1) {
          for (int c = 0; c < nBase; ++c) yhP[SX(nq + 1, c)] = 0.0;
          for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) YHS[IDX(nq + 1, c, p)] = 0.0;
        }
        { double rp = 1.0; for (int j = 1; j <= st.nqNext + 1; ++j) {
            for (int c = 0; c < nBase; ++c) yhP[SX(j, c)] *= rp;
            for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) YHS[IDX(j, c, p)] *= rp;
            rp *= st.rh;
          } }
        // obs bracketed by this step: extract at post-transition Nordsieck
        for (size_t oi = 0; oi < obsIdx.size(); ++oi) if (obsBrk[oi] == n) {
          int i = obsIdx[oi]; double ti = ind->timeThread[ind->ix[i]]; double sS = (ti - st.tn)/st.hNext;
          double *outc = getSolve(i);
          for (int k = 0; k < nBase; ++k) for (int p = 0; p < np; ++p) {
            double Sval = 0, sp = 1.0; for (int j = 0; j <= st.nqNext; ++j) { Sval += sp * YHS[IDX(j+1,k,p)]; sp *= sS; }
            worstSens = std::max(worstSens, fabs(Sval - outc[sensOff + k*np + p]));
          }
        }
      }
      // carry the state sensitivity to the next segment's start time (identity bolus)
      if (sg + 1 < lsAdjSegs.size()) {
        const lsAdjStep &sl = lsAdjSteps[Sg.stepEnd - 1];
        double sS = (lsAdjSegs[sg + 1].t0 - sl.tn) / sl.hNext;
        for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) {
          double v = 0, sp = 1.0; for (int j = 0; j <= sl.nqNext; ++j) { v += sp * YHS[IDX(j + 1, c, p)]; sp *= sS; }
          Sin[(size_t)c * np + p] = v;
        }
      }
    }
    int nSwitch = 0, maxNq = 1;
    for (size_t n = 0; n < nStep; ++n) { if (n > 0 && lsAdjSteps[n].meth != lsAdjSteps[n-1].meth) ++nSwitch; maxNq = std::max(maxNq, lsAdjSteps[n].nq); }
    REprintf("[lsadj] max|adjoint - discrete_forward_sens| = %.3e | primal_replay_err = %.3e | nStep=%zu nSeg=%zu maxOrder=%d methodSwitches=%d\n",
             worstSens, worstPrimal, nStep, lsAdjSegs.size(), maxNq, nSwitch);
  }
}

extern "C" void ind_liblsodaadj(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis) {
  rx_solving_options *op = rx->op;
  struct lsoda_opt_t opt = {0};
  opt.ixpr = 0;
  opt.rtol = op->rtol2; opt.atol = op->atol2;
  opt.itask = 1;
  opt.mxstep = op->mxstep; opt.mxhnil = op->mxhnil;
  opt.mxordn = op->MXORDN; opt.mxords = op->MXORDS;
  opt.h0 = op->H0; opt.hmax = op->hmax2; opt.hmin = op->HMIN; opt.hmxi = op->hmxi;
  ind_liblsodaadj_0(rx, op, opt, solveid, dydt_liblsoda, c_dydt, u_inis);
}

extern "C" void par_liblsodaadj(rx_solve *rx) {
  // Parallel across subjects (P6).  The recording buffers (lsAdjSteps/lsAdjSegs) are
  // thread_local, and each subject uses its own ind->lhs / getSolve() storage + its
  // own per-thread liblsoda ctx pool, so distinct-subject solves do not alias.  The
  // forward is the exact base liblsoda (ind_liblsoda0), already thread safe.
  rx_solving_options *op = &op_global;
#ifdef _OPENMP
  int cores = op->cores;
#else
  int cores = 1;
#endif
  uint32_t nsub = rx->nsub, nsim = rx->nsim;
  int nsolve = (int)(nsim * nsub);
  int displayProgress = (op->nDisplayProgress <= nsolve);
  clock_t t0 = clock();
  struct lsoda_opt_t opt = {0};
  opt.ixpr = 0;
  opt.rtol = op->rtol2; opt.atol = op->atol2;
  opt.itask = 1;
  opt.mxstep = op->mxstep; opt.mxhnil = op->mxhnil;
  opt.mxordn = op->MXORDN; opt.mxords = op->MXORDS;
  opt.h0 = op->H0; opt.hmax = op->hmax2; opt.hmin = op->HMIN; opt.hmxi = op->hmxi;
  int curTick = 0, cur = 0, abort = 0;
  uint32_t seed0 = getRxSeed1(cores);
#ifdef _OPENMP
#pragma omp parallel for num_threads(op->cores) schedule(dynamic,1)
#endif
  for (int solveid = 0; solveid < nsolve; solveid++) {
    int localAbort;
#pragma omp atomic read
    localAbort = abort;
    if (localAbort == 0) {
      // rx->ordId walks positions (sortIds); the drivers take subject ids.
      int _id = rx->ordId[solveid] - 1;
      setSeedEng1(seed0 + _id);
      ind_liblsodaadj_0(rx, op, opt, _id, dydt_liblsoda, dydt, update_inis);
      if (displayProgress) {
#pragma omp critical
        cur++;
#ifdef _OPENMP
        if (omp_get_thread_num() == 0)
#endif
        {
          curTick = par_progress(cur, nsolve, curTick, cores, t0, 0);
          int localAbort2;
#pragma omp atomic read
          localAbort2 = abort;
          if (localAbort2 == 0 && checkInterrupt()) { int nA = 1;
#pragma omp atomic write
            abort = nA; }
        }
      }
    }
  }
  setRxSeedFinal(seed0 + (uint32_t)nsolve);
  if (abort == 1) { op->abort = 1; par_progress(cur, nsolve, curTick, cores, t0, 1); }
  else if (displayProgress && curTick < 50) par_progress(nsolve, nsolve, curTick, cores, t0, 0);
}

#endif // IN_PAR_SOLVE
