// Exact discrete-adjoint driver for liblsoda (method="liblsodaadj", code 202 =
// base liblsoda 2 + 200).  Fills the same rx__sens_<state>_BY_<param>__ columns
// as rk4s / dop853s / cvodesadj, but as the EXACT reverse-mode transpose of
// liblsoda's OWN Nordsieck multistep step map -- not a continuous-adjoint
// approximation.
//
// #included into par_solve.cpp (guarded by IN_PAR_SOLVE, like rk4s.cpp /
// cvodes_adjoint.cpp) so it sees calc_lhs / getSolve / rxEffNeq / the dydt
// globals AND rk4s.cpp's luFactor/luSolveT (defined earlier in the same TU).
//
// Math (full derivation in ~/src/rxode2-liblsoda-adjoint-plan.md).  Per ACCEPTED
// step (order nq, step h, coeffs el[], Nordsieck yh with (nq+1) rows):
//   PREDICT  Pascal triangle (linear):  for j=nq..1: for i1=j..nq: yh[i1]+=yh[i1+1]
//   CORRECT  acor = h*f(y) - yh_pred[2],  y = yh_pred[1] + el[1]*acor
//            solved implicitly via  Pmat = I - h*el[1]*J,  w-solve uses Pmat^{-T}
//   UPDATE   yh_new[j] = yh_pred[j] + el[j]*acor
// The transpose (costate Lambda is yh-shaped, (nq+1) rows x nBase):
//   UPDATE^T  adj_pred[j] = Lambda[j];  adj_acor = sum_j el[j]*Lambda[j]
//   CORRECT^T w = Pmat^{-T} adj_acor;  adj_pred[1] += h*J^T w;  adj_pred[2] += -w;
//             mu += h*F_p^T w              (parameter quadrature; F_p = rx__adjFP)
//   PREDICT^T reverse Pascal:  for j=1..nq: for i1=nq..j: adj[i1+1]+=adj[i1]
// KEY: the transpose is a LINEAR map parameterised only by (J, F_p, h, el, nq);
// the predicted/acor VALUES are never needed -- so per step we record only
// (tn, h, nq, el[], converged y=yh[1]) and recompute J,F_p from y via calc_lhs.
// At an observation t_i (which liblsoda reaches by intdy interpolation, NOT at a
// step boundary) the costate is seeded by intdy^T on the bracketing step; a final
// t0 term folds the p-dependence of the initial Nordsieck row yh0[2]=h0*f(t0,y0).
//
// STATUS: P1-P3 -- variable ORDER (general Pascal^T + Nordsieck row add/drop on an
// order change) and variable STEP (scaleh^T at the step boundary AND a "bridge"
// scaleh^T for the rejection rescaling that happens between accepted steps).  The
// transition after each accepted step is read off the recorded (nqu,hu)->(nq,h):
// dOrder in {-1,0,+1} + the scaleh factor rh; the used el come from the elco table
// (indexed by the used order), NOT _rxC(el) which is already reset for the next step.
// VALIDATED (RX_LSADJ_SELFCHECK) two ways: (a) the discrete adjoint equals the
// discrete FORWARD sensitivity of the identical recorded schedule to MACHINE
// precision (~1e-14) -- proving the transpose is exact; (b) an independent PRIMAL
// replay of the same step-map reproduces liblsoda's recorded y to the corrector's
// convergence level (O(tol)) -- proving the step-map model matches liblsoda.  A
// finite difference of a re-solved liblsoda is NOT the reference: it perturbs the
// adaptive order/step schedule, agreeing only for params that do not move it.
// Method switch (P4), interior event jumps (P5) and parallelism (P6) are follow-ups.
#ifdef IN_PAR_SOLVE

// --- recording hooks: need liblsoda's common block (ctx->common->...) ----------
// par_solve.cpp already #included common.h (struct lsoda_common_t + the _rxC macro)
// and <vector>/<algorithm> long before this TU, and it #undef'd min/max above the
// method .cpp includes -- so we reuse those here directly (common.h has no include
// guard, so re-including it would redefine the struct).

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
};
// Thread-local recording state (serial per subject in P1; par is P6).
static thread_local bool                 lsAdjActive = false;
static thread_local int                  lsAdjNbase  = 0;
static thread_local std::vector<lsAdjStep> lsAdjSteps;
static thread_local double               lsAdjT0 = 0.0, lsAdjH0 = 0.0;
static thread_local std::vector<double>  lsAdjY0;    // initial base state y0

extern "C" int lsAdjIsActive(void) { return lsAdjActive ? 1 : 0; }

// Called once at state==1 init (yh[1]=y0, yh[2]=h0*f(y0)); capture (t0,y0,h0).
extern "C" void lsAdjInitStep(struct lsoda_context_t *ctx) {
  lsAdjT0 = _rxC(tn);
  lsAdjH0 = _rxC(h);
  lsAdjY0.resize(lsAdjNbase);
  for (int i = 1; i <= lsAdjNbase; ++i) lsAdjY0[i - 1] = _rxC(yh)[1][i];
}

// Called after every accepted step.  At this point stoda has already run UPDATE +
// the post-step transition (methodswitch / orderswitch / scaleh), so _rxC(nq) /
// _rxC(h) / _rxC(el) describe the NEXT step -- we recover the USED order/step from
// _rxC(nqu) / _rxC(hu) and the USED el from the elco table (indexed by used order,
// which survives resetcoeff), and read the transition off (nqu,hu) -> (nq,h).
extern "C" void lsAdjPushStep(struct lsoda_context_t *ctx) {
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
                                  int solveid, t_dydt_liblsoda c_dydt_ll, t_update_inis u_inis) {
  int cSub = rx->ordId[solveid] - 1;
  rx_solving_options_ind *ind = &(rx->subjects[cSub]);
  int nBase = op->adjNbase;
  int np    = op->adjNp;
  int eff   = rxEffNeq(ind, op);
  int fxOff = op->adjFxOff, fpOff = op->adjFpOff, sensOff = op->adjSensOff;

  // ---- forward: run the ordinary liblsoda solve, with recording hooks on -------
  lsAdjSteps.clear();
  lsAdjY0.clear();
  lsAdjNbase = nBase;
  lsAdjActive = true;                 // ind_liblsoda0 installs the ctx hooks
  ind_liblsoda0(rx, op, opt, solveid, c_dydt_ll, u_inis);
  lsAdjActive = false;

  if (op->badSolve || ind->err != 0 || lsAdjSteps.empty()) return;
  size_t nStep = lsAdjSteps.size();

  // ---- backward sweep: for each obs t_i and base state k, an independent -------
  //      reset sweep seeded by intdy^T on the bracketing step.
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

  for (int i = 0; i < ind->n_all_times; ++i) {
    if (!isObs(getEvid(ind, ind->ix[i]))) continue;
    double ti = ind->timeThread[ind->ix[i]];
    // bracketing step S = first recorded step with tn_S >= ti (clamp to last).
    size_t S = 0;
    while (S < nStep && lsAdjSteps[S].tn < ti - 1e-9) ++S;
    if (S >= nStep) S = nStep - 1;
    double *out = getSolve(i);

    for (int k = 0; k < nBase; ++k) {
      // Seed Lambda at the POST-transition Nordsieck of step S -- that is what
      // liblsoda's intdy interpolates the obs with (order nqNext_S, step hNext_S):
      //   y_k(ti) = sum_{j=0..nqNext} s^j yh_final[j+1][k],  s = (ti-tn_S)/hNext_S.
      int nqSeed = lsAdjSteps[S].nqNext;
      double sS = (ti - lsAdjSteps[S].tn) / lsAdjSteps[S].hNext;
      std::fill(lamFlat.begin(), lamFlat.end(), 0.0);
      double sp = 1.0;
      for (int j = 0; j <= nqSeed; ++j) { lamFlat[(size_t)(j + 1) * nBase + k] += sp; sp *= sS; }
      std::fill(mu.begin(), mu.end(), 0.0);

      // transpose from step S down to step 0
      for (long n = (long)S; n >= 0; --n) {
        const lsAdjStep &st = lsAdjSteps[n];
        int nq = st.nq;
        double h = st.h, el1 = st.el[1];
        const double *fx, *fp;
        lsAdjEval(cSub, st.tn, st.y.data(), nBase, eff, fxOff, fpOff, A.data(), lhs, &fx, &fp);

        // ---- TRANSITION^T: Lambda(yh_final_n) -> Lambda(yh_postupdate_n) --------
        // Forward order was: [update: rows 1..nq+1] -> [order change] -> [scaleh].
        // Reverse: scaleh^T (diag rh^(j-1)) over the post-transition rows, then
        // order^T (drop/absorb the extra row into acorExtra).
        std::fill(acorExtra.begin(), acorExtra.end(), 0.0);
        {
          int nqF = st.nqNext;                       // post-transition order
          double rp = 1.0;
          for (int j = 1; j <= nqF + 1; ++j) {       // scaleh^T
            double *lamj = &lamFlat[(size_t)j * nBase];
            for (int c = 0; c < nBase; ++c) lamj[c] *= rp;
            rp *= st.rh;
          }
          if (st.dOrder == 1) {
            // top row (nq+2) was acor*rInc (then scaled): absorb into acorExtra, drop it.
            double *top = &lamFlat[(size_t)(nq + 2) * nBase];
            for (int c = 0; c < nBase; ++c) { acorExtra[c] += st.rInc * top[c]; top[c] = 0.0; }
          } else if (st.dOrder == -1) {
            // dropped row (nq+1) had no downstream effect -> its costate is 0.
            double *drop = &lamFlat[(size_t)(nq + 1) * nBase];
            for (int c = 0; c < nBase; ++c) drop[c] = 0.0;
          }
        }

        // ---- STEP^T: UPDATE^T -> CORRECT^T -> PREDICT^T (order nq) --------------
        // UPDATE^T: adjPred[j] = lam[j]; adjAcor = sum_j el[j]*lam[j] + acorExtra
        for (int c = 0; c < nBase; ++c) adjAcor[c] = acorExtra[c];
        for (int j = 1; j <= nq + 1; ++j) {
          const double *lamj = &lamFlat[(size_t)j * nBase];
          double *pj = &adjPred[(size_t)j * nBase];
          double ej = st.el[j];
          for (int c = 0; c < nBase; ++c) { pj[c] = lamj[c]; adjAcor[c] += ej * lamj[c]; }
        }
        // CORRECT^T: Pmat = I - h*el1*J (row-major); w = Pmat^{-T} adjAcor
        for (int a = 0; a < nBase; ++a)
          for (int b = 0; b < nBase; ++b)
            Pmat[(size_t)a * nBase + b] = (a == b ? 1.0 : 0.0) - h * el1 * fx[a * nBase + b];
        for (int c = 0; c < nBase; ++c) w[c] = adjAcor[c];
        if (nBase > 0) { luFactor(Pmat.data(), nBase, piv.data()); luSolveT(Pmat.data(), nBase, piv.data(), w.data()); }
        // adjPred[1] += h*J^T w ; adjPred[2] += -w ; mu += h*F_p^T w
        double *p1 = &adjPred[(size_t)1 * nBase];
        double *p2 = &adjPred[(size_t)2 * nBase];
        for (int b = 0; b < nBase; ++b) { double s = 0; for (int a = 0; a < nBase; ++a) s += fx[a * nBase + b] * w[a]; p1[b] += h * s; }
        for (int c = 0; c < nBase; ++c) p2[c] += -w[c];
        for (int p = 0; p < np; ++p) { double s = 0; for (int a = 0; a < nBase; ++a) s += fp[a * np + p] * w[a]; mu[p] += h * s; }
        // PREDICT^T (reverse Pascal): adjPred -> adj of yh at start of this step
        for (int j = 1; j <= nq; ++j)
          for (int i1 = nq; i1 >= j; --i1) {
            double *dst = &adjPred[(size_t)(i1 + 1) * nBase];
            const double *src = &adjPred[(size_t)i1 * nBase];
            for (int c = 0; c < nBase; ++c) dst[c] += src[c];
          }
        // Lambda_prev = adjPred (rows 1..nq+1)
        std::fill(lamFlat.begin(), lamFlat.end(), 0.0);
        for (int j = 1; j <= nq + 1; ++j) {
          double *lamj = &lamFlat[(size_t)j * nBase];
          const double *pj = &adjPred[(size_t)j * nBase];
          for (int c = 0; c < nBase; ++c) lamj[c] = pj[c];
        }
        // BRIDGE^T: transpose of the pre-predict rescale (rejection scaleh) that
        // brought yh from the previous accepted step's end-h to this step's used h.
        double hPrevEnd = (n == 0) ? lsAdjH0 : lsAdjSteps[n - 1].hNext;
        double rhB = h / hPrevEnd;
        if (rhB != 1.0) { double rp = 1.0; for (int j = 1; j <= nq + 1; ++j) {
          double *lamj = &lamFlat[(size_t)j * nBase];
          for (int c = 0; c < nBase; ++c) lamj[c] *= rp;
          rp *= rhB; } }
      }

      // t0 term: yh0[2] = h0*f(t0,y0) depends on p -> mu += h0*Lambda0[2]^T F_p(t0,y0).
      // (yh0[1]=y0 is p-independent for P1.)
      if (!lsAdjY0.empty()) {
        const double *fx0, *fp0;
        lsAdjEval(cSub, lsAdjT0, lsAdjY0.data(), nBase, eff, fxOff, fpOff, A.data(), lhs, &fx0, &fp0);
        const double *lam2 = &lamFlat[(size_t)2 * nBase];
        for (int p = 0; p < np; ++p) { double s = 0; for (int c = 0; c < nBase; ++c) s += fp0[c * np + p] * lam2[c]; mu[p] += lsAdjH0 * s; }
      }

      for (int p = 0; p < np; ++p) out[sensOff + k * np + p] = mu[p];
    }
  }

  // ---- Self-check (RX_LSADJ_SELFCHECK=1).  Two independent replays on the SAME
  // recorded schedule, reported as one message line:
  //   * adjoint vs FORWARD-sensitivity replay -> must match to MACHINE precision;
  //     that is the DEFINING property of a discrete adjoint (exact transpose of the
  //     identical numerical step map), and it holds regardless of whether the
  //     schedule is param-dependent (both replays use the same frozen steps).
  //   * PRIMAL replay vs liblsoda's recorded y -> must match to O(tol); that
  //     independently confirms the step-map model (predict/correct/update + order
  //     change + scaleh + rejection bridge) actually reproduces liblsoda.
  // A finite difference of a re-solved liblsoda is NOT used: it moves the adaptive
  // order/step schedule and only agrees for params that do not perturb it.
  if (getenv("RX_LSADJ_SELFCHECK")) {
    int R = maxRows;
    // PRIMAL replay yhP (R+2 rows x nBase) and sensitivity replay YHS (x np), both
    // stepped through the SAME recorded predict/correct/update + transition map.
    std::vector<double> yhP((size_t)(R + 2) * nBase, 0.0);
    std::vector<double> YHS((size_t)(R + 2) * nBase * np, 0.0);
    std::vector<double> rhs((size_t)nBase * np), dac((size_t)nBase * np), P2(Pmat);
    std::vector<double> yPred(nBase), acor(nBase), fEval(eff, 0.0), Aw(eff, 0.0);
    auto SX = [&](int row, int c){ return (size_t)row * nBase + c; };
    auto IDX = [&](int row, int c, int p){ return ((size_t)row * nBase + c) * np + p; };
    int neqP[2]; neqP[0] = eff; neqP[1] = cSub;
    // init Nordsieck at t0: yhP[1]=y0, yhP[2]=h0*f(y0); YHS row2 = h0*Fp(y0).
    if (!lsAdjY0.empty()) {
      for (int c = 0; c < nBase; ++c) yhP[SX(1, c)] = lsAdjY0[c];
      for (int c = 0; c < eff; ++c) Aw[c] = 0.0;
      for (int c = 0; c < nBase; ++c) Aw[c] = lsAdjY0[c];
      dydt(neqP, lsAdjT0, Aw.data(), fEval.data());
      for (int c = 0; c < nBase; ++c) yhP[SX(2, c)] = lsAdjH0 * fEval[c];
      const double *fx0, *fp0;
      lsAdjEval(cSub, lsAdjT0, lsAdjY0.data(), nBase, eff, fxOff, fpOff, A.data(), lhs, &fx0, &fp0);
      for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) YHS[IDX(2, c, p)] = lsAdjH0 * fp0[c * np + p];
    }
    double worstSens = 0.0, worstPrimal = 0.0;
    std::vector<int> obsIdx; std::vector<size_t> obsBrk;
    for (int i = 0; i < ind->n_all_times; ++i) if (isObs(getEvid(ind, ind->ix[i]))) {
      double ti = ind->timeThread[ind->ix[i]]; size_t S = 0; while (S < nStep && lsAdjSteps[S].tn < ti - 1e-9) ++S; if (S >= nStep) S = nStep - 1;
      obsIdx.push_back(i); obsBrk.push_back(S);
    }
    for (size_t n = 0; n < nStep; ++n) {
      const lsAdjStep &st = lsAdjSteps[n]; int nq = st.nq; double h = st.h, el1 = st.el[1];
      const double *fx, *fp; lsAdjEval(cSub, st.tn, st.y.data(), nBase, eff, fxOff, fpOff, A.data(), lhs, &fx, &fp);
      // BRIDGE scaleh: if the step actually USED an h != the previous accepted step's
      // end-h, stoda rejected the first attempt and rescaled h before this accepted
      // try (no hook fires on a rejection).  Rescale yh from hEnd_{n-1} to hu_n.
      { double hPrevEnd = (n == 0) ? lsAdjH0 : lsAdjSteps[n - 1].hNext;
        double rhB = h / hPrevEnd;
        if (rhB != 1.0) { double rp = 1.0; for (int j = 1; j <= nq + 1; ++j) {
          for (int c = 0; c < nBase; ++c) yhP[SX(j, c)] *= rp;
          for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) YHS[IDX(j, c, p)] *= rp;
          rp *= rhB; } } }
      // PREDICT (Pascal) on primal + sensitivity
      for (int j = nq; j >= 1; --j) for (int i1 = j; i1 <= nq; ++i1) {
        for (int c = 0; c < nBase; ++c) yhP[SX(i1, c)] += yhP[SX(i1 + 1, c)];
        for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) YHS[IDX(i1, c, p)] += YHS[IDX(i1 + 1, c, p)];
      }
      // factor P = I - h el1 J at (tn, recorded y)
      for (int a = 0; a < nBase; ++a) for (int b = 0; b < nBase; ++b) P2[(size_t)a*nBase+b] = (a==b?1.0:0.0) - h*el1*fx[a*nBase+b];
      luFactor(P2.data(), nBase, piv.data());
      // CORRECT primal: chord iteration acor = P^{-1}(h f(y) - (yh_pred[2]+acor))
      // (one solve is exact for a linear model; iterate a few for mild nonlinearity).
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
      // CORRECT sensitivity: (I-h el1 J) dac = h J yhs_pred[1] - yhs_pred[2] + h Fp
      for (int p = 0; p < np; ++p) for (int a = 0; a < nBase; ++a) {
        double jr = 0; for (int b = 0; b < nBase; ++b) jr += fx[a*nBase+b]*YHS[IDX(1,b,p)];
        rhs[(size_t)a*np+p] = h*jr - YHS[IDX(2,a,p)] + h*fp[a*np+p];
      }
      for (int p = 0; p < np; ++p) { std::vector<double> col(nBase); for (int a=0;a<nBase;++a) col[a]=rhs[(size_t)a*np+p]; luSolve(P2.data(), nBase, piv.data(), col.data()); for (int a=0;a<nBase;++a) dac[(size_t)a*np+p]=col[a]; }
      // UPDATE primal + sensitivity: yh[j] += el[j]*acor
      for (int j = 1; j <= nq + 1; ++j) {
        for (int c = 0; c < nBase; ++c) yhP[SX(j, c)] += st.el[j] * acor[c];
        for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) YHS[IDX(j, c, p)] += st.el[j] * dac[(size_t)c*np+p];
      }
      // primal replay check: replayed yh[1] must equal the recorded converged y
      // (to ~the corrector's convergence level, i.e. O(tol) -- liblsoda stops the
      // corrector at its own criterion while this replay solves the exact fixed
      // point; a small residual here is expected and confirms the step-map model).
      for (int c = 0; c < nBase; ++c) worstPrimal = std::max(worstPrimal, fabs(yhP[SX(1, c)] - st.y[c]));
      // TRANSITION (order change + scaleh), same as the forward step map.
      if (st.dOrder == 1) {
        for (int c = 0; c < nBase; ++c) yhP[SX(nq + 2, c)] = acor[c] * st.rInc;
        for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) YHS[IDX(nq + 2, c, p)] = dac[(size_t)c*np+p] * st.rInc;
      } else if (st.dOrder == -1) {
        for (int c = 0; c < nBase; ++c) yhP[SX(nq + 1, c)] = 0.0;
        for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) YHS[IDX(nq + 1, c, p)] = 0.0;
      }
      { double rp = 1.0; for (int j = 1; j <= st.nqNext + 1; ++j) {   // scaleh
          for (int c = 0; c < nBase; ++c) yhP[SX(j, c)] *= rp;
          for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) YHS[IDX(j, c, p)] *= rp;
          rp *= st.rh;
        } }
      // obs bracketed by step n: extract at the POST-transition Nordsieck (nqNext,hNext)
      for (size_t oi = 0; oi < obsIdx.size(); ++oi) if (obsBrk[oi] == n) {
        int i = obsIdx[oi]; double ti = ind->timeThread[ind->ix[i]]; double sS = (ti - st.tn)/st.hNext;
        double *outc = getSolve(i);
        for (int k = 0; k < nBase; ++k) for (int p = 0; p < np; ++p) {
          double Sval = 0, sp = 1.0; for (int j = 0; j <= st.nqNext; ++j) { Sval += sp * YHS[IDX(j+1,k,p)]; sp *= sS; }
          worstSens = std::max(worstSens, fabs(Sval - outc[sensOff + k*np + p]));
        }
      }
    }
    REprintf("[lsadj] max|adjoint - discrete_forward_sens| = %.3e | primal_replay_err = %.3e\n", worstSens, worstPrimal);
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
  ind_liblsodaadj_0(rx, op, opt, solveid, dydt_liblsoda, u_inis);
}

extern "C" void par_liblsodaadj(rx_solve *rx) {
  // Serial per-subject recording (thread-local buffers) for P1; parallel is P6.
  rx_solving_options *op = rx->op;
  int nsolve = (int)(rx->nsim * rx->nsub);
  struct lsoda_opt_t opt = {0};
  opt.ixpr = 0;
  opt.rtol = op->rtol2; opt.atol = op->atol2;
  opt.itask = 1;
  opt.mxstep = op->mxstep; opt.mxhnil = op->mxhnil;
  opt.mxordn = op->MXORDN; opt.mxords = op->MXORDS;
  opt.h0 = op->H0; opt.hmax = op->hmax2; opt.hmin = op->HMIN; opt.hmxi = op->hmxi;
  for (int solveid = 0; solveid < nsolve; ++solveid) {
    ind_liblsodaadj_0(rx, op, opt, solveid, dydt_liblsoda, update_inis);
    if (op->badSolve) break;
  }
}

#endif // IN_PAR_SOLVE
