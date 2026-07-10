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
struct lsAdjSeg {
  double t0, h0;              // segment start time and its initial step h0
  std::vector<double> y0;     // base state at segment start (post-event, yh[1])
  size_t stepBegin, stepEnd;  // [begin,end) range into lsAdjSteps
  // interior event that OPENED this segment (reconstructed from the event table):
  //   0 additive bolus (dPhi/dy=I) | 3 reset (y:=inits, dPhi/dy=0) | 5 replace
  //   (y[cmt]:=val, dPhi/dy row cmt=0) | 6 multiply (y[cmt]*=fac) | -1 none (seg 0).
  int evType, evCmt;
  double evFactor;           // multiply factor (type 6)
  double evAmt;              // raw dose amount (type 0 bolus; for modeled F/alag mu)
  int infCmt;                // cmt of an active MODELED infusion window (-1 = none) -> the
                             // in-window forcing mu += h*durMult*dR/dp[cmt]*w[cmt] per step.
  double infDurMult;         // forcing weight (1 for rate(), amt for dur())
  int offCmt;                // this segment STARTS at a modeled-infusion OFF boundary (-1
                             // none) -> off-transversality mu += offFac*dR/dp[cmt]*lam[cmt].
  double offFac;             // -amt/R*durMult at the off boundary
};
// Thread-local recording state (serial per subject; par is P6).
static thread_local bool                 lsAdjActive = false;
static thread_local int                  lsAdjNbase  = 0;
static thread_local std::vector<lsAdjStep> lsAdjSteps;
static thread_local std::vector<lsAdjSeg>  lsAdjSegs;

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
  s.evType = -1; s.evCmt = -1; s.evFactor = 1.0; s.evAmt = 0.0; // from the event table
  s.infCmt = -1; s.infDurMult = 0.0; s.offCmt = -1; s.offFac = 0.0;
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

extern "C" void ind_liblsodaadj_0(rx_solve *rx, rx_solving_options *op, struct lsoda_opt_t opt,
                                  int solveid, t_dydt_liblsoda c_dydt_ll, t_dydt c_dydt,
                                  t_update_inis u_inis) {
  int cSub = rx->ordId[solveid] - 1;
  rx_solving_options_ind *ind = &(rx->subjects[cSub]);
  int nBase = op->adjNbase;
  int np    = op->adjNp;
  int eff   = rxEffNeq(ind, op);
  int fxOff = op->adjFxOff, fpOff = op->adjFpOff, sensOff = op->adjSensOff;

  // ---- forward: run the ordinary liblsoda solve, with recording hooks on -------
  lsAdjSteps.clear();
  lsAdjSegs.clear();
  _lsSsKind = 0; _lsSsBolusIi = 0.0; _lsSsCmt = -1;   // reset the sticky ss snapshot
  _lsSsDur = _lsSsDur2 = _lsSsRate = _lsSsRate2 = 0.0;
  lsAdjNbase = nBase;
  lsAdjActive = true;                 // ind_liblsoda0 installs the ctx hooks
  ind_liblsoda0(rx, op, opt, solveid, c_dydt_ll, u_inis);
  lsAdjActive = false;

  if (op->badSolve || ind->err != 0 || lsAdjSteps.empty() || lsAdjSegs.empty()) return;
  lsAdjSegs.back().stepEnd = lsAdjSteps.size();     // close the final segment
  size_t nStep = lsAdjSteps.size();

  // Reconstruct the event opening each segment by matching its start time (seg 0
  // included, for a modeled-F/lag dose at t0).  Additive boluses (dPhi/dy=I) are
  // the default; reset/replace/multiply get their exact dual.
  for (size_t sg = 0; sg < lsAdjSegs.size(); ++sg) {
    double t0 = lsAdjSegs[sg].t0;
    for (int i = 0; i < ind->n_all_times; ++i) {
      int ev = getEvid(ind, ind->ix[i]);
      if (isObs(ev)) continue;
      if (fabs(ind->timeThread[ind->ix[i]] - t0) > 1e-8) continue;
      if (ev == 3) { lsAdjSegs[sg].evType = 3; break; }
      if (isDose(ev)) {
        int _wh, _cmt, _wh100, _whI, _wh0; getWh(ev, &_wh, &_cmt, &_wh100, &_whI, &_wh0);
        if (_cmt >= 0 && _cmt < nBase) {
          if (_whI == 0)                  { lsAdjSegs[sg].evType = 0; lsAdjSegs[sg].evCmt = _cmt; lsAdjSegs[sg].evAmt = getDose(ind, ind->ix[i]); break; }
          else if (_whI == EVIDF_REPLACE) { lsAdjSegs[sg].evType = 5; lsAdjSegs[sg].evCmt = _cmt; break; }
          else if (_whI == EVIDF_MULT)    { lsAdjSegs[sg].evType = 6; lsAdjSegs[sg].evCmt = _cmt; lsAdjSegs[sg].evFactor = getDose(ind, ind->ix[i]); break; }
          // (modeled rate/dur infusion on/off open a segment but are not a state jump;
          //  their dR/dp dual is reconstructed as windows below, not here.)
        }
      }
    }
  }

  // Reconstruct modeled rate()/dur() infusion windows for the dR/dp dual: pair each
  // ON event with its matching OFF and derive R = amt/(t_off-t_on).  Segments in
  // [t_on,t_off) get the in-window forcing; the segment at t_off gets the off-
  // boundary transversality.  durMult = 1 for rate(), amt for dur().
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
      double amt = getDose(ind, ind->ix[i]);
      bool isDur = (_whI == EVIDF_MODEL_DUR_ON);
      // matching OFF event (same cmt, next off time > tOn)
      double tOff = R_PosInf;
      for (int j = 0; j < ind->n_all_times; ++j) {
        int ej = getEvid(ind, ind->ix[j]); if (isObs(ej) || !isDose(ej)) continue;
        int _wh2, _cmt2, _wh1002, _whI2, _wh02; getWh(ej, &_wh2, &_cmt2, &_wh1002, &_whI2, &_wh02);
        if ((_whI2 == EVIDF_MODEL_RATE_OFF || _whI2 == EVIDF_MODEL_DUR_OFF) && _cmt2 == _cmt) {
          double te = ind->timeThread[ind->ix[j]]; if (te > tOn + 1e-9 && te < tOff) tOff = te;
        }
      }
      if (!R_FINITE(tOff) || tOff - tOn < 1e-12) continue;
      double R = amt / (tOff - tOn);
      double durMult = isDur ? amt : 1.0;
      for (size_t sg = 0; sg < lsAdjSegs.size(); ++sg) {
        double t0s = lsAdjSegs[sg].t0;
        if (t0s >= tOn - 1e-8 && t0s < tOff - 1e-8) { lsAdjSegs[sg].infCmt = _cmt; lsAdjSegs[sg].infDurMult = durMult; }
        else if (fabs(t0s - tOff) < 1e-8) { lsAdjSegs[sg].offCmt = _cmt; lsAdjSegs[sg].offFac = -(amt / R) * durMult; }
      }
    }
  }

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
    // modeled rate()/dur() in-window forcing: inside the infusion the RHS gains +R, so
    // F_p[cmt] gains durMult*dR/dp -> mu += h*durMult*dR/dp[cmt]*w[cmt] (lhs holds the
    // calc_lhs at this step's y, incl. the rx__adjDrate block).
    if (op->adjDrateOff >= 0) { int ic = lsAdjSegs[st.seg].infCmt;
      if (ic >= 0) { const double *dR = &lhs[op->adjDrateOff]; double dm = lsAdjSegs[st.seg].infDurMult;
        for (int p = 0; p < np; ++p) mu[p] += h * dm * dR[ic * np + p] * w[ic]; } }
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
    while (S < e && lsAdjSteps[S].tn < t - 1e-9) ++S;
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
  auto sweepToStart = [&](int segIdx, double time) {
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
      // modeled rate()/dur() OFF-boundary transversality: the off time t_on+amt/R shifts
      // with R -> mu += (-amt/R*durMult)*dR/dp[cmt]*lam_cmt(t_off).  (lhs holds calc_lhs
      // at this off segment's (t0,y0); lam_cmt(t_off) = lam_y0[cmt].)
      if (op->adjDrateOff >= 0 && Sg.offCmt >= 0) { const double *dR = &lhs[op->adjDrateOff]; int oc = Sg.offCmt;
        for (int p = 0; p < np; ++p) mu[p] += Sg.offFac * dR[oc * np + p] * wSeed[oc]; }
      // event dPhi/dp explicit-parameter quadrature (every segment start): modeled
      // F -> mu += amt*dF/dp*lam_y0[c]; alag -> mu += -amt*dlag/dp*(lam_y0^T F_X[:,c]).
      if (Sg.evType == 0 && Sg.evCmt >= 0) {
        int c = Sg.evCmt; double amt = Sg.evAmt;
        if (op->adjDfOff >= 0) { const double *dF = &lhs[op->adjDfOff]; for (int p = 0; p < np; ++p) mu[p] += amt * dF[c * np + p] * wSeed[c]; }
        if (op->adjDlagOff >= 0) { const double *dlag = &lhs[op->adjDlagOff];
          double sc = 0; for (int i = 0; i < nBase; ++i) sc += wSeed[i] * fx0[i * nBase + c];
          for (int p = 0; p < np; ++p) mu[p] += -amt * dlag[c * np + p] * sc; }
      }
      if (seg == 0) break;
      // event jump dPhi/dy^T maps lam_y0 -> costate of y(tau-) handed to the previous
      // segment: bolus (I) unchanged | reset all 0 | replace[c]->0 | multiply[c]*=factor.
      if (Sg.evType == 3) { for (int b = 0; b < nBase; ++b) wSeed[b] = 0.0; }
      else if (Sg.evType == 5 && Sg.evCmt >= 0) { wSeed[Sg.evCmt] = 0.0; }
      else if (Sg.evType == 6 && Sg.evCmt >= 0) { wSeed[Sg.evCmt] *= Sg.evFactor; }
      time = Sg.t0;
    }
  };

  // Continuous-infusion steady state (kind 2): seg-0 y0 is the constant Y_ss with
  // dY_ss/dp = -J^{-1} df/dp, so the missing IC contribution per observation is
  // -(J^{-T} wSeed)^T df/dp.
  bool ssContLL = (_lsSsKind == 2) && nBase > 0 && !lsAdjSegs.empty();
  std::vector<double> ssFXc, ssFP; std::vector<int> ssPiv;
  if (ssContLL) {
    const double *fx0, *fp0;
    lsAdjEval(cSub, lsAdjSegs[0].t0, lsAdjSegs[0].y0.data(), nBase, eff, fxOff, fpOff, A.data(), lhs, &fx0, &fp0);
    ssFXc.assign(fx0, fx0 + (size_t)nBase * nBase);
    ssFP.assign(fp0, fp0 + (size_t)nBase * np);
    ssPiv.resize(nBase);
    if (!luFactor(ssFXc.data(), nBase, ssPiv.data())) ssContLL = false;   // singular J
  }

  // ss==1 bolus / fixed-rate finite infusion: the missing IC is the monodromy
  // fixed point S_ss = (I-M)^{-1}B of one steady-state period.  Re-record that
  // period from y0 with a dop853 tableau and reuse the shared rk4sSsIc series.
  rk4sSsIc lsIc; bool lsMono = false; rk4s_rec lsSsRec;
  if (!ssContLL && op->adjoint && fxOff >= 0 && nBase > 0 && !lsAdjSegs.empty() && _lsSsKind == 1) {
    int neqOde = op->neq - op->numLin - op->numLinSens;
    int neq2[2]; neq2[0] = eff; neq2[1] = cSub;
    rksTableau Tss = rksGetTableau(200);            // dop853 explicit tableau
    std::vector<double> y0(lsAdjSegs[0].y0.begin(), lsAdjSegs[0].y0.end()), ssScr;
    y0.resize(eff, 0.0);
    bool recorded = false;
    if (_lsSsBolusIi > 0.0) {                        // bolus: flow from peak over ii
      rks_step_interval(ind, op, c_dydt, neq2, Tss, neqOde, nBase, y0.data(), 0.0, _lsSsBolusIi, &lsSsRec, ssScr);
      recorded = true;
    } else {                                         // periodic/large-dur infusion: two-phase from trough
      double savedR = ind->InfusionRate[_lsSsCmt], t = 0.0;
      if (_lsSsDur > 0.0) { ind->InfusionRate[_lsSsCmt] = _lsSsRate;
        rks_step_interval(ind, op, c_dydt, neq2, Tss, neqOde, nBase, y0.data(), t, t + _lsSsDur, &lsSsRec, ssScr); t += _lsSsDur; }
      if (_lsSsDur2 > 0.0) { ind->InfusionRate[_lsSsCmt] = _lsSsRate2;
        rks_step_interval(ind, op, c_dydt, neq2, Tss, neqOde, nBase, y0.data(), t, t + _lsSsDur2, &lsSsRec, ssScr); }
      ind->InfusionRate[_lsSsCmt] = savedR;
      recorded = true;
    }
    if (recorded && lsSsRec.nStep() > 0) {
      std::vector<double> Asc(eff, 0.0);
      lsIc.build(cSub, ind, op, lsSsRec, fxOff, fpOff, eff, Asc,
                 &lsSsRec, NULL, NULL, NULL, NULL, NULL, NULL, NULL, Tss, nBase, np, 0);
      lsMono = lsIc.haveMono;
    }
  }

  for (int i = 0; i < ind->n_all_times; ++i) {
    if (!isObs(getEvid(ind, ind->ix[i]))) continue;
    double ti = ind->timeThread[ind->ix[i]];
    int segObs = 0;
    for (int sgi = (int)lsAdjSegs.size() - 1; sgi >= 0; --sgi) { if (lsAdjSegs[sgi].t0 <= ti + 1e-9) { segObs = sgi; break; } }
    double *out = getSolve(i);
    for (int k = 0; k < nBase; ++k) {
      std::fill(mu.begin(), mu.end(), 0.0);
      for (int c = 0; c < nBase; ++c) wSeed[c] = (c == k) ? 1.0 : 0.0;
      sweepToStart(segObs, ti);
      if (ssContLL) {                       // continuous-infusion linear-solve IC term
        std::vector<double> w(wSeed.begin(), wSeed.begin() + nBase);   // w = J^{-T} lam
        luSolveT(ssFXc.data(), nBase, ssPiv.data(), w.data());
        for (int p = 0; p < np; ++p) { double s = 0; for (int j = 0; j < nBase; ++j) s += w[j] * ssFP[j * np + p]; mu[p] -= s; }
      }
      if (lsMono) lsIc.applyWindowIC(wSeed, mu);   // bolus / periodic-infusion monodromy IC
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
    auto SX = [&](int row, int c){ return (size_t)row * nBase + c; };
    auto IDX = [&](int row, int c, int p){ return ((size_t)row * nBase + c) * np + p; };
    int neqP[2]; neqP[0] = eff; neqP[1] = cSub;
    double worstSens = 0.0, worstPrimal = 0.0;
    // obs -> bracketing flat step index (as in the backward)
    std::vector<int> obsIdx; std::vector<size_t> obsBrk;
    for (int i = 0; i < ind->n_all_times; ++i) if (isObs(getEvid(ind, ind->ix[i]))) {
      double ti = ind->timeThread[ind->ix[i]];
      int sg = 0; for (int s = (int)lsAdjSegs.size()-1; s >= 0; --s) { if (lsAdjSegs[s].t0 <= ti + 1e-9) { sg = s; break; } }
      size_t S = lsAdjSegs[sg].stepBegin, e = lsAdjSegs[sg].stepEnd;
      while (S < e && lsAdjSteps[S].tn < ti - 1e-9) ++S; if (S >= e) S = e - 1;
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
      if (Sg.evType == 3) { for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) YHS[IDX(1, c, p)] = 0.0; }
      else if (Sg.evType == 5 && Sg.evCmt >= 0) { for (int p = 0; p < np; ++p) YHS[IDX(1, Sg.evCmt, p)] = 0.0; }
      else if (Sg.evType == 6 && Sg.evCmt >= 0) { for (int p = 0; p < np; ++p) YHS[IDX(1, Sg.evCmt, p)] *= Sg.evFactor; }
      else if (Sg.evType == 0 && Sg.evCmt >= 0) {
        // modeled F: y0[c] += F*amt -> dy0[c]/dp += amt*dF/dp.  modeled alag: the dose
        // time shifts -> S+ = S- + (f(y-)-f(y+))*dlag/dp = S- - amt*F_X[:,c]*dlag/dp.
        int c = Sg.evCmt; double amt = Sg.evAmt;
        if (op->adjDfOff >= 0) { const double *dF = &lhs[op->adjDfOff]; for (int p = 0; p < np; ++p) YHS[IDX(1, c, p)] += amt * dF[c * np + p]; }
        if (op->adjDlagOff >= 0) { const double *dlag = &lhs[op->adjDlagOff];
          for (int i = 0; i < nBase; ++i) for (int p = 0; p < np; ++p) YHS[IDX(1, i, p)] += -amt * fx0[i * nBase + c] * dlag[c * np + p]; }
      }
      for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) {
        double jr = 0; for (int b = 0; b < nBase; ++b) jr += fx0[c * nBase + b] * Sin[(size_t)b * np + p];
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
      setSeedEng1(seed0 + rx->ordId[solveid] - 1);
      ind_liblsodaadj_0(rx, op, opt, solveid, dydt_liblsoda, dydt, update_inis);
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
