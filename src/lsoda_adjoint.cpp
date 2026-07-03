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
// STATUS: P1 -- fixed order-1 / fixed-step chord.  VALIDATED by the defining
// property: the discrete adjoint equals the discrete FORWARD sensitivity of the
// identical recorded schedule to machine precision (~1e-13..1e-10 across 1/2/3-cmt
// coupled models; RX_LSADJ_SELFCHECK self-check below).  A finite difference of a
// re-solved liblsoda instead perturbs the adaptive step schedule, so it agrees
// only for parameters that do not move it -- which is why FD is NOT the reference.
// Variable order (P2), variable step / scaleh (P3), method switch (P4), interior
// event jumps (P5) and parallelism (P6) are follow-ups; the per-step transpose
// below is written for general nq so those phases extend the recording, not this
// core.
#ifdef IN_PAR_SOLVE

// --- recording hooks: need liblsoda's common block (ctx->common->...) ----------
// par_solve.cpp already #included common.h (struct lsoda_common_t + the _rxC macro)
// and <vector>/<algorithm> long before this TU, and it #undef'd min/max above the
// method .cpp includes -- so we reuse those here directly (common.h has no include
// guard, so re-including it would redefine the struct).

// One recorded accepted step: the linearisation point + step metadata.
struct lsAdjStep {
  double tn, h;               // end-time and step size actually used (hu)
  int nq;                     // order used (nqu)
  double el[14];              // el[1..nq+1] method coefficients
  std::vector<double> y;      // converged base states at tn (yh[1][1..nBase])
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

// Called after every accepted step; record (tn, hu, nqu, el, converged y).
extern "C" void lsAdjPushStep(struct lsoda_context_t *ctx) {
  lsAdjStep s;
  s.tn = _rxC(tn);
  s.h  = _rxC(hu);
  s.nq = _rxC(nqu);
  for (int j = 1; j <= s.nq + 1 && j < 14; ++j) s.el[j] = _rxC(el)[j];
  s.y.resize(lsAdjNbase);
  for (int i = 1; i <= lsAdjNbase; ++i) s.y[i - 1] = _rxC(yh)[1][i];
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
  int maxRows = 0;
  for (size_t s = 0; s < nStep; ++s) maxRows = std::max(maxRows, lsAdjSteps[s].nq + 1);
  lamFlat.assign((size_t)(maxRows + 1) * nBase, 0.0); // costate rows 1..nq+1 (1-based)
  adjPred.assign((size_t)(maxRows + 1) * nBase, 0.0);
  adjAcor.assign(nBase, 0.0);
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
      // seed Lambda at step S by intdy^T (k=0 interpolation, order nq_S):
      //   y_k(ti) = sum_{j=0..nq} s^j yh[j+1][k],  s=(ti-tn_S)/h_S
      int nqS = lsAdjSteps[S].nq;
      double sS = (ti - lsAdjSteps[S].tn) / lsAdjSteps[S].h;
      std::fill(lamFlat.begin(), lamFlat.end(), 0.0);
      double sp = 1.0;
      for (int j = 0; j <= nqS; ++j) { lamFlat[(size_t)(j + 1) * nBase + k] += sp; sp *= sS; }
      std::fill(mu.begin(), mu.end(), 0.0);

      // transpose from step S down to step 0
      for (long n = (long)S; n >= 0; --n) {
        const lsAdjStep &st = lsAdjSteps[n];
        int nq = st.nq;
        double h = st.h, el1 = st.el[1];
        const double *fx, *fp;
        lsAdjEval(cSub, st.tn, st.y.data(), nBase, eff, fxOff, fpOff, A.data(), lhs, &fx, &fp);
        // UPDATE^T: adjPred[j] = lam[j]; adjAcor = sum_j el[j]*lam[j]
        std::fill(adjAcor.begin(), adjAcor.end(), 0.0);
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

  // ---- Self-check: discrete FORWARD-sensitivity replay on the SAME recorded
  // schedule.  The DEFINING property of a discrete adjoint is that it equals the
  // discrete forward sensitivity of the identical numerical step map to machine
  // precision -- independent of whether the schedule is param-dependent (both use
  // the same frozen steps).  This is the correct correctness test for an adaptive
  // solver: a finite difference of a re-solved liblsoda instead moves the adaptive
  // step schedule and so only agrees for parameters that do not perturb it.  Opt
  // in with RX_LSADJ_SELFCHECK=1 (used by the P1 validation test).
  if (getenv("RX_LSADJ_SELFCHECK")) {
    int R = maxRows;                                   // Nordsieck rows (1..nq+1)
    // YHS: (R+1) rows x nBase x np (row 0 unused).  Init yh0 sens: row2 = h0*Fp(y0).
    std::vector<double> YHS((size_t)(R + 1) * nBase * np, 0.0), YP((size_t)(R + 1) * nBase * np, 0.0);
    std::vector<double> rhs((size_t)nBase * np), dac((size_t)nBase * np), P2(Pmat);
    auto IDX = [&](int row, int c, int p){ return ((size_t)row * nBase + c) * np + p; };
    if (!lsAdjY0.empty()) {
      const double *fx0, *fp0;
      lsAdjEval(cSub, lsAdjT0, lsAdjY0.data(), nBase, eff, fxOff, fpOff, A.data(), lhs, &fx0, &fp0);
      for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) YHS[IDX(2, c, p)] = lsAdjH0 * fp0[c * np + p];
    }
    double worst = 0.0;
    size_t nextObs = 0;
    std::vector<int> obsIdx; std::vector<size_t> obsBrk;
    for (int i = 0; i < ind->n_all_times; ++i) if (isObs(getEvid(ind, ind->ix[i]))) {
      double ti = ind->timeThread[ind->ix[i]]; size_t S = 0; while (S < nStep && lsAdjSteps[S].tn < ti - 1e-9) ++S; if (S >= nStep) S = nStep - 1;
      obsIdx.push_back(i); obsBrk.push_back(S);
    }
    for (size_t n = 0; n < nStep; ++n) {
      const lsAdjStep &st = lsAdjSteps[n]; int nq = st.nq; double h = st.h, el1 = st.el[1];
      const double *fx, *fp; lsAdjEval(cSub, st.tn, st.y.data(), nBase, eff, fxOff, fpOff, A.data(), lhs, &fx, &fp);
      // PREDICT: Pascal
      for (int j = nq; j >= 1; --j) for (int i1 = j; i1 <= nq; ++i1)
        for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) YHS[IDX(i1, c, p)] += YHS[IDX(i1 + 1, c, p)];
      // CORRECT: (I-h el1 J) dac = h J yhs_pred[1] - yhs_pred[2] + h Fp
      for (int a = 0; a < nBase; ++a) for (int b = 0; b < nBase; ++b) P2[(size_t)a*nBase+b] = (a==b?1.0:0.0) - h*el1*fx[a*nBase+b];
      luFactor(P2.data(), nBase, piv.data());
      for (int p = 0; p < np; ++p) {
        for (int a = 0; a < nBase; ++a) {
          double jr = 0; for (int b = 0; b < nBase; ++b) jr += fx[a*nBase+b]*YHS[IDX(1,b,p)];
          rhs[(size_t)a*np+p] = h*jr - YHS[IDX(2,a,p)] + h*fp[a*np+p];
        }
      }
      for (int p = 0; p < np; ++p) { std::vector<double> col(nBase); for (int a=0;a<nBase;++a) col[a]=rhs[(size_t)a*np+p]; luSolve(P2.data(), nBase, piv.data(), col.data()); for (int a=0;a<nBase;++a) dac[(size_t)a*np+p]=col[a]; }
      // UPDATE
      for (int j = 1; j <= nq + 1; ++j) for (int c = 0; c < nBase; ++c) for (int p = 0; p < np; ++p) YHS[IDX(j, c, p)] += st.el[j] * dac[(size_t)c*np+p];
      // any obs whose bracketing step == n: extract S via intdy and compare
      for (size_t oi = 0; oi < obsIdx.size(); ++oi) if (obsBrk[oi] == n) {
        int i = obsIdx[oi]; double ti = ind->timeThread[ind->ix[i]]; double sS = (ti - st.tn)/st.h;
        double *outc = getSolve(i);
        for (int k = 0; k < nBase; ++k) for (int p = 0; p < np; ++p) {
          double Sval = 0, sp = 1.0; for (int j = 0; j <= nq; ++j) { Sval += sp * YHS[IDX(j+1,k,p)]; sp *= sS; }
          double d = fabs(Sval - outc[sensOff + k*np + p]); if (d > worst) worst = d;
        }
      }
      (void)nextObs;
    }
    REprintf("[lsadj] max|adjoint - discrete_forward_sens| (frozen schedule) = %.3e\n", worst);
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
