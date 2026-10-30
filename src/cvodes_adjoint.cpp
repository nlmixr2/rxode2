// CVODE adjoint-sensitivity driver (method="cvodesadj"): self-managed dense
// primal + plain-CVODE backward.  Fills rx__sens_<state>_BY_<param>__ cols.
//
// #included into par_solve.cpp (guarded by IN_PAR_SOLVE) so it sees calc_lhs /
// iniSubject / getSolve / handle_evid / the dydt globals.  Uses plain CVODE
// (CV_BDF) for both sweeps and manages the adjoint ourselves (CVODES native ASA
// assumes one continuous forward integration, which rxode2's dose/reset events
// corrupt, so only plain CVODE is vendored).
//
//   FORWARD  : plain CVODE driven like the other rxode2 solvers (cloned from
//              dop5.cpp), stepping CV_ONE_STEP to record a dense per-segment
//              primal history (t, y, f).  A new segment begins at every
//              dose/reset so interpolation never blurs a jump.
//   BACKWARD : for each (observation t_i, base state k) integrate d(lambda)/dt =
//              -F_X^T lambda, d(mu)/dt = -F_p^T lambda from t_i down to t_0,
//              evaluating F_X/F_p on the interpolated primal.  mu(t_0) =
//              dy_k(t_i)/dp lands in the rx__sens_<k>_BY_<p>__ slots.
//
// For plain bolus / constant-rate infusion / reset the adjoint dose jump is the
// identity, so backward integration through dose times is correct; only F_X/F_p
// change.  Modeled-F/alag dose-parameter jumps are a later addition.
#ifdef IN_PAR_SOLVE

#include <cvode/cvode.h>
#include <cvode/cvode_ls.h>
#include <nvector/nvector_serial.h>
#include <sunmatrix/sunmatrix_dense.h>
#include <sunlinsol/sunlinsol_dense.h>

// ---- dense per-segment primal history (cubic-Hermite interpolation) ----
struct cvHist {
  int nBase = 0;
  std::vector<double> t;            // all recorded times, non-decreasing
  std::vector<double> y;            // row-major: entry*nBase + k
  std::vector<double> f;            // dydt at each entry, row-major
  std::vector<int> segStart;        // index in t[] where each smooth segment starts
  void newSeg() { segStart.push_back((int)t.size()); }
  void push(double tt, const double *yy, const double *ff) {
    t.push_back(tt);
    for (int k = 0; k < nBase; ++k) { y.push_back(yy[k]); f.push_back(ff[k]); }
  }
  // Interpolate the primal at tq into out[nBase].  Segment-aware: at a dose time
  // the later (post-jump) segment wins, matching rxode2's "state is post-dose".
  void interp(double tq, double *out) const {
    int ns = (int)segStart.size();
    if (ns == 0) { for (int k = 0; k < nBase; ++k) out[k] = 0.0; return; }
    int s = 0;
    for (int si = 0; si < ns; ++si) {
      if (t[segStart[si]] <= tq + 1e-12) s = si; else break;
    }
    int lo = segStart[s];
    int hi = (s + 1 < ns) ? segStart[s + 1] : (int)t.size();   // [lo, hi)
    if (hi - lo <= 1 || tq <= t[lo]) { for (int k = 0; k < nBase; ++k) out[k] = y[lo * nBase + k]; return; }
    if (tq >= t[hi - 1]) { for (int k = 0; k < nBase; ++k) out[k] = y[(hi - 1) * nBase + k]; return; }
    int a = lo, b = hi - 1;
    while (b - a > 1) { int m = (a + b) / 2; if (t[m] <= tq) a = m; else b = m; }
    double t0 = t[a], t1 = t[a + 1], h = t1 - t0;
    if (h <= 0) { for (int k = 0; k < nBase; ++k) out[k] = y[a * nBase + k]; return; }
    double th = (tq - t0) / h;
    double h00 = (2 * th * th * th) - (3 * th * th) + 1;
    double h10 = (th * th * th) - (2 * th * th) + th;
    double h01 = (-2 * th * th * th) + (3 * th * th);
    double h11 = (th * th * th) - (th * th);
    for (int k = 0; k < nBase; ++k)
      out[k] = h00 * y[a * nBase + k] + h10 * h * f[a * nBase + k] +
               h01 * y[(a + 1) * nBase + k] + h11 * h * f[(a + 1) * nBase + k];
  }
};

// ---- backward costate history for the delay() anticipating term ----
// One degree-7 record per backward step: 8 equispaced CVodeGetDky nodes of
// lambda on [lo, hi].  Records are appended in decreasing time.
struct cvLamHist {
  int n = 0;
  std::vector<double> lo, hi, v;    // v: (record*8 + node)*n + k
  void clear() { lo.clear(); hi.clear(); v.clear(); }
  // lambda at s into out[n]; false when no record covers s.
  bool eval(double s, double *out) const {
    int nr = (int)lo.size();
    if (nr == 0 || s > hi[0] || s < lo[nr - 1]) return false;
    int a = 0, b = nr - 1;            // lo[] is decreasing
    while (a < b) { int m = (a + b) / 2; if (lo[m] <= s) b = m; else a = m + 1; }
    double h = hi[a] - lo[a];
    const double *r = &v[(size_t)a * 8 * n];
    if (h <= 0.0) { for (int k = 0; k < n; ++k) out[k] = r[k]; return true; }
    double x = 7.0 * (s - lo[a]) / h, w[8];
    for (int m = 0; m < 8; ++m) {
      double l = 1.0;
      for (int q = 0; q < 8; ++q) if (q != m) l *= (x - q) / (double)(m - q);
      w[m] = l;
    }
    for (int k = 0; k < n; ++k) {
      double acc = 0.0;
      for (int m = 0; m < 8; ++m) acc += w[m] * r[m * n + k];
      out[k] = acc;
    }
    return true;
  }
};

struct cvFwdUser {   // forward RHS user data
  int *neq; t_dydt dydt; int nBase, eff;
  std::vector<double> A, DADT;
  rx_solving_options_ind *ind = NULL; rx_solving_options *op = NULL;
  N_Vector dky = NULL;              // delay() history samples (NULL: none)
  std::vector<double> nodes;        // 8 x eff
};
// A modeled-rate infusion window (time-based) for the cvodesadj rate dual.
struct cvInfus { double tOn, tOff; int cmt; double R, amt, durMult; int offSeq, offY; };  // durMult: 1 (rate) or amt (dur)

struct cvBwdUser {   // backward RHS user data
  const cvHist *hist; int cSub, nBase, np, eff, fxOff, fpOff, drateOff;
  const std::vector<cvInfus> *infus;   // for the in-window forcing quadrature
  std::vector<double> A, yq;
  double *lhs;
  // delay() anticipating term: -F_Xd(t+tau)^T lambda(t+tau) for each delay tau
  bool dde = false;
  int fxdOff = -1, tauOff = -1;
  std::vector<double> taus;         // distinct delays
  double ti = 0.0;                  // observation time of this sweep
  long int mxstep = 0;              // steps allowed per backward interval
  cvLamHist lam;
  std::vector<double> lamTop, lamS, ant;
};

// Rounding-level tolerance for comparing times near t.
static inline double cvB_teps(double t) { return 16.0 * DBL_EPSILON * (1.0 + fabs(t)); }

// lambda at s for the anticipating term: 0 past the observation, the sweep's
// starting costate at it, otherwise the recorded costate (nearest end if s
// falls just outside the records).
static void cvB_lamAt(cvBwdUser *u, double s, double *out) {
  int n = u->nBase;
  double eps = cvB_teps(u->ti);
  if (s > u->ti + eps) { for (int k = 0; k < n; ++k) out[k] = 0.0; return; }
  if (u->lam.eval(s, out)) return;
  int nr = (int)u->lam.lo.size();
  if (nr == 0 || s >= u->lam.hi[0]) { for (int k = 0; k < n; ++k) out[k] = u->lamTop[k]; return; }
  const double *r = &u->lam.v[((size_t)(nr - 1) * 8) * n];   // node 0 of the lowest record
  for (int k = 0; k < n; ++k) out[k] = r[k];
}

// Sum over the delays of F_Xd(t+tau)^T lambda(t+tau) into u->ant.
static void cvB_anticipate(cvBwdUser *u, double t) {
  int n = u->nBase;
  double *A = u->A.data(), *yq = u->yq.data(), *ls = u->lamS.data();
  for (int j = 0; j < n; ++j) u->ant[j] = 0.0;
  for (size_t d = 0; d < u->taus.size(); ++d) {
    double tau = u->taus[d], s = t + tau;
    if (s > u->ti + cvB_teps(u->ti)) continue;
    cvB_lamAt(u, s, ls);
    u->hist->interp(s, yq);
    for (int i = 0; i < u->eff; ++i) A[i] = 0.0;
    for (int i = 0; i < n; ++i) A[i] = yq[i];
    calc_lhs(u->cSub, s, A, u->lhs);
    const double *fxd = &u->lhs[u->fxdOff], *tl = &u->lhs[u->tauOff];
    for (int i = 0; i < n; ++i)
      for (int j = 0; j < n; ++j)
        if (fabs(tl[i * n + j] - tau) <= 1e-12 * (1.0 + tau))
          u->ant[j] += fxd[i * n + j] * ls[i];
  }
}

// Forward RHS: advance the nBase base states (the rx__sens_* compartments have
// d/dt=0); dydt reads/writes the full eff-wide state.
static int cvB_fwd(sunrealtype t, N_Vector y, N_Vector yd, void *ud) {
  cvFwdUser *u = (cvFwdUser *)ud;
  double *A = u->A.data(), *D = u->DADT.data();
  for (int i = 0; i < u->eff; ++i) A[i] = 0.0;
  for (int i = 0; i < u->nBase; ++i) A[i] = NV_Ith_S(y, i);
  u->dydt(u->neq, t, A, D);
  for (int i = 0; i < u->nBase; ++i) NV_Ith_S(yd, i) = D[i];
  return 0;
}

// Backward augmented RHS: B = [lambda(nBase), mu(np)];
//   d(lambda)/dt = -F_X^T lambda,   d(mu)/dt = -F_p^T lambda.
static int cvB_bwd(sunrealtype t, N_Vector B, N_Vector Bd, void *ud) {
  cvBwdUser *u = (cvBwdUser *)ud;
  int n = u->nBase, np = u->np;
  double *A = u->A.data(), *yq = u->yq.data();
  if (u->dde) cvB_anticipate(u, (double)t);
  u->hist->interp((double)t, yq);
  for (int i = 0; i < u->eff; ++i) A[i] = 0.0;
  for (int i = 0; i < n; ++i) A[i] = yq[i];
  calc_lhs(u->cSub, t, A, u->lhs);
  const double *fx = &u->lhs[u->fxOff];
  const double *fp = &u->lhs[u->fpOff];
  for (int j = 0; j < n; ++j) {
    double s = 0.0;
    for (int i = 0; i < n; ++i) s += fx[i * n + j] * NV_Ith_S(B, i);
    NV_Ith_S(Bd, j) = u->dde ? -s - u->ant[j] : -s;
  }
  for (int p = 0; p < np; ++p) {
    double s = 0.0;
    for (int i = 0; i < n; ++i) s += fp[i * np + p] * NV_Ith_S(B, i);
    NV_Ith_S(Bd, n + p) = -s;
  }
  // modeled-rate infusion forcing: inside a window the RHS gains +R, so F_p[c] +=
  // dR/dtheta -> d(mu_p)/dt += -dR/dtheta * lambda_c.
  if (u->drateOff >= 0 && u->infus) {
    const double *drate = &u->lhs[u->drateOff];
    for (size_t w = 0; w < u->infus->size(); ++w) {
      const cvInfus &F = (*u->infus)[w];
      if ((double)t >= F.tOn - 1e-12 && (double)t <= F.tOff + 1e-12) {
        double lc = NV_Ith_S(B, F.cmt);   // dR/dtheta = durMult * drate
        for (int p = 0; p < np; ++p) NV_Ith_S(Bd, n + p) += -F.durMult * drate[F.cmt * np + p] * lc;
      }
    }
  }
  return 0;
}

// f = dydt at (t, base states yp) into D (eff wide; may be u->DADT).
static void cvB_rhs(cvFwdUser *u, double t, const double *yp, double *D) {
  double *A = u->A.data();
  for (int i = 0; i < u->eff; ++i) A[i] = 0.0;
  for (int i = 0; i < u->nBase; ++i) A[i] = yp[i];
  u->dydt(u->neq, t, A, D);
}

// Record the current (t, base states) plus f=dydt into the history segment.
static void cvB_record(cvHist *hist, cvFwdUser *u, double t, const double *yp) {
  double *D = u->DADT.data();
  cvB_rhs(u, t, yp, D);
  hist->push(t, yp, D);
}

// Record the last forward step into the delay() history as a degree-7 record
// sampled with CVodeGetDky.
static void cvB_delayRecord(void *mem, cvFwdUser *u) {
  rx_solving_options_ind *ind = u->ind;
  if (!ind->delayHistOn) return;
  sunrealtype tn, hu;
  if (CVodeGetCurrentTime(mem, &tn) < 0 || CVodeGetLastStep(mem, &hu) < 0) return;
  const double *qp[8];
  for (int m = 0; m < 8; ++m) {
    double *q = &u->nodes[(size_t)m * u->eff];
    double tm = (m == 7) ? (double)tn : (double)tn - (double)hu + (double)hu * m / 7.0;
    CVodeGetDky(mem, (sunrealtype)tm, 0, u->dky);
    for (int k = 0; k < u->nBase; ++k) q[k] = NV_Ith_S(u->dky, k);
    qp[m] = q;
  }
  rxDelayHistPushNodes(ind, u->op, (double)tn - (double)hu, (double)hu, qp);
}

// Step the forward primal from the integrator's current time to xout using
// CV_ONE_STEP, recording each accepted step into the current history segment.
static void cvB_step(void *mem, cvFwdUser *u, N_Vector y, double *yp,
                     double xout, int *rc, cvHist *hist) {
  for (int k = 0; k < u->nBase; ++k) NV_Ith_S(y, k) = yp[k];
  CVodeSetStopTime(mem, (sunrealtype)xout);
  sunrealtype tcur = 0.0;
  int guard = 0, maxit = 10000000;
  bool retried = false;
  while (true) {
    int flag = CVode(mem, (sunrealtype)xout, y, &tcur, CV_ONE_STEP);
    if (flag < 0 && !retried && u->dky != NULL) {
      // a step from a delay breakpoint (a kink in the solution) fails on the
      // history from before it: restart there once
      sunrealtype tn;
      if (CVodeGetCurrentTime(mem, &tn) >= 0 && CVodeGetDky(mem, tn, 0, u->dky) >= 0) {
        for (int k = 0; k < u->nBase; ++k) NV_Ith_S(y, k) = NV_Ith_S(u->dky, k);
        if (CVodeReInit(mem, tn, y) >= 0 && CVodeSetStopTime(mem, (sunrealtype)xout) >= 0) {
          retried = true;
          continue;
        }
      }
    }
    if (flag < 0) { if (*rc == 0) *rc = -2019; return; }
    retried = false;
    for (int k = 0; k < u->nBase; ++k) yp[k] = NV_Ith_S(y, k);
    if (u->dky != NULL) cvB_delayRecord(mem, u);
    cvB_record(hist, u, (double)tcur, yp);
    if (flag == CV_TSTOP_RETURN || tcur >= xout) break;
    if (++guard > maxit) { if (*rc == 0) *rc = -2019; return; }
  }
}

// Integrate the backward system from tFrom down to tEnd.  For a delay() model
// step one at a time (the step is capped at the smallest delay), recording the
// costate for the anticipating term, and restart at each breakpoint in
// (tEnd, tFrom) (descending `bps`) where the anticipated costate jumps.
static int cvB_bwdTo(void *bmem, N_Vector B, N_Vector dkyB, cvBwdUser *u,
                     double tFrom, double tEnd, const std::vector<double> &bps) {
  sunrealtype tret;
  if (!u->dde) {
    CVodeSetStopTime(bmem, (sunrealtype)tEnd);
    return (CVode(bmem, (sunrealtype)tEnd, B, &tret, CV_NORMAL) < 0) ? -1 : 1;
  }
  int n = u->nBase;
  double tCur = tFrom;
  size_t b = 0;
  while (true) {
    while (b < bps.size() && bps[b] >= tCur - cvB_teps(tCur)) ++b;
    bool isBp = (b < bps.size() && bps[b] > tEnd + cvB_teps(tEnd));
    double tgt = isBp ? bps[b] : tEnd;
    CVodeSetStopTime(bmem, (sunrealtype)tgt);
    long int nstep = 0;
    while (true) {
      if (u->mxstep > 0 && ++nstep > u->mxstep) return -1;
      int flag = CVode(bmem, (sunrealtype)tgt, B, &tret, CV_ONE_STEP);
      if (flag < 0) return -1;
      sunrealtype tn, hu;
      if (CVodeGetCurrentTime(bmem, &tn) < 0 || CVodeGetLastStep(bmem, &hu) < 0) return -1;
      double lo = (double)tn, hi = (double)tn - (double)hu;
      if (hi < lo) std::swap(lo, hi);
      u->lam.lo.push_back(lo); u->lam.hi.push_back(hi);
      for (int m = 0; m < 8; ++m) {
        CVodeGetDky(bmem, (sunrealtype)(lo + (hi - lo) * m / 7.0), 0, dkyB);
        for (int k = 0; k < n; ++k) u->lam.v.push_back(NV_Ith_S(dkyB, k));
      }
      if (flag == CV_TSTOP_RETURN || (double)tret <= tgt) break;
    }
    tCur = tgt;
    if (!isBp) return 1;
    if (CVodeReInit(bmem, (sunrealtype)tgt, B) < 0) return -1;
    ++b;
  }
}

// Solve one subject: forward primal + record history, then backward per (obs, k).
static int cvodesAdjSolveSubject(rx_solve *rx, rx_solving_options *op,
                                 rx_solving_options_ind *ind, int *neq, int istate0,
                                 t_dydt c_dydt, t_update_inis u_inis, SUNContext sunctx) {
  int i;
  int istate = istate0;
  void *ctx = NULL;
  int localBadSolve = 0;
  int cSub = neq[1];
  int nBase = op->adjNbase, np = op->adjNp, sensOff = op->adjSensOff;
  int neqOde = op->neq - op->numLin - op->numLinSens;
  int eff = rxEffNeq(ind, op);
  double atol = op->ATOL > 0 ? op->ATOL : 1e-8, rtol = op->RTOL > 0 ? op->RTOL : 1e-6;

  cvFwdUser fu;
  fu.neq = neq; fu.dydt = c_dydt; fu.nBase = nBase; fu.eff = eff;
  fu.A.assign(eff, 0.0); fu.DADT.assign(eff, 0.0);
  fu.ind = ind; fu.op = op;

  // delay() history, as in ind_dop0_dense()
  ind->delayHistOn = op->hasDelay;
  ind->delayHistN  = 0;
  ind->delayT0     = getAllTimes(ind, 0);
  ind->delayMinT   = R_PosInf;
  ind->delayMaxT   = 0.0;
  ind->delayWarmed = 0;

  cvHist hist; hist.nBase = nBase;

  double xp = getAllTimes(ind, 0);
  double t0 = xp;
  double xout;

  // ---- forward integrator (plain BDF, no ASA) ----
  N_Vector y = N_VNew_Serial(nBase, sunctx);
  { double *y0 = getSolve(0); for (int k = 0; k < nBase; ++k) NV_Ith_S(y, k) = y0[k]; }
  void *mem = CVodeCreate(CV_BDF, sunctx);
  int ok = (mem != NULL);
  if (ok && CVodeInit(mem, cvB_fwd, (sunrealtype)xp, y) < 0) ok = 0;
  if (ok && CVodeSStolerances(mem, rtol, atol) < 0) ok = 0;
  if (ok && CVodeSetUserData(mem, (void *)&fu) < 0) ok = 0;
  SUNMatrix Amat = ok ? SUNDenseMatrix(nBase, nBase, sunctx) : NULL;
  SUNLinearSolver LS = ok ? SUNLinSol_Dense(y, Amat, sunctx) : NULL;
  if (ok && (!Amat || !LS || CVodeSetLinearSolver(mem, LS, Amat) < 0)) ok = 0;
  if (ok) CVodeSetMaxNumSteps(mem, (long int)(op->mxstep > 0 ? op->mxstep : 50000));
  if (ok && ind->delayHistOn) {
    // one RHS evaluation so delay() learns the smallest delay, which caps the
    // step so every lookup lands in recorded history
    cvB_rhs(&fu, xp, getSolve(0), fu.DADT.data());
    ind->delayWarmed = 1;
    if (R_FINITE(ind->delayMinT)) CVodeSetMaxStep(mem, (sunrealtype)rxDelayStepCap(ind));
    fu.dky = N_VNew_Serial(nBase, sunctx);
    fu.nodes.assign((size_t)8 * eff, 0.0);
    if (fu.dky == NULL) ok = 0;
  }
  if (!ok) {
    if (LS) SUNLinSolFree(LS);
    if (Amat) SUNMatDestroy(Amat);
    if (mem) CVodeFree(&mem);
    if (fu.dky) N_VDestroy(fu.dky);
    N_VDestroy(y);
    return 0;
  }

  // seq: forward processing order, which orders records sharing a time.
  int seq = 0;
  std::vector<double> obsT; std::vector<int> obsIx, obsSeq;
  std::vector<double> resetT;   // evid-3 reset times (costate jump: lambda := 0)
  std::vector<int> resetSeq;
  // replace (type 5: lambda[c]:=0) / multiply (type 6: lambda[c]*=factor) events;
  // the backward sweep stops at each and applies the transpose costate jump.
  // seq: forward processing order; y: offset in evY of the primal just after
  // this event's own jump (-1: none), so same-time events see their own state;
  // an alag bolus (type 0) follows it with its vector-field jump F(y+)-F(y-).
  struct cvEvent { double t; int cmt; int type; double factor; int seq; int y; };
  std::vector<double> evY;
  std::vector<cvEvent> evJumps;
  std::vector<cvInfus> infusRec;   // modeled-rate infusion windows (rate dual)
  // state jumps (bolus, replace, multiply) for the dose jump of a
  // parameter-dependent delay
  // lag: a bolus whose modeled alag() also moves its arrival
  struct cvJump { double t; int cmt; double dy; int seq; bool lag; };
  std::vector<cvJump> jumpRec;
  bool recJumps = op->hasDelay && op->adjDtauOff >= 0;
  std::vector<double> yPre(nBase);
  int _infusOnCmt = -1;
  double *yp;
  hist.newSeg();   // segment 0 begins at t0

  // ================= forward primal loop (cloned from dop5.cpp) =================
  for (i = 0; i < ind->n_all_times; i++) {
    ind->idx = i;
    ind->linSS = 0;
    if (ind->mainSorted == 0) {
      double *_rtime = ind->timeThread;
      for (int _j = i; _j < ind->n_all_times; _j++) {
        int _raw = ind->ix[_j];
        int _evid = getEvid(ind, _raw);
        if (_evid >= 10 && _evid <= 99) {
          _rtime[_raw] = ind->mtime[_evid - 10];
        } else if (!isObs(_evid)) {
          int _wh, _cmt, _wh100, _whI, _wh0;
          getWh(_evid, &_wh, &_cmt, &_wh100, &_whI, &_wh0);
          if (_whI == EVIDF_MODEL_RATE_OFF || _whI == EVIDF_MODEL_DUR_OFF) {
            _rtime[_raw] = getAllTimes(ind, _raw);
          }
        }
      }
      reSortMainTimeline(ind, i);
      ind->mainSorted = 1;
    }
    _growSolveIfNeeded(ind, op, i, 1);
    yp = getSolve(i);
    xout = ind->timeThread[ind->ix[i]];
    int didEvent = 0;

    if (getEvid(ind, ind->ix[i]) != 3 && !isSameTime(xout, xp)) {
      if (ind->err) {
        ind->rc[0] = -1000;
        badSolveExit(i);
        localBadSolve = 1;
      } else {
        if (handleExtraDose(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                            xp, ind->id, &i, ind->n_all_times, &istate, op, ind, u_inis, ctx)) {
          if (!localBadSolve && !isSameTime(ind->extraDoseNewXout, xp)) {
            preSolve(op, ind, xp, ind->extraDoseNewXout, yp);
            if (neqOde > 0) cvB_step(mem, &fu, y, yp, ind->extraDoseNewXout, ind->rc, &hist);
            copyLinCmt(neq, ind, op, yp);
            const char *err_msg = "cvodesadj failed";
            postSolve(neq, &istate, ind->rc, &i, yp, &err_msg, 10, true, ind, op, rx);
            if (*(ind->rc) < 0) localBadSolve = 1;
            xp = ind->extraDoseNewXout;
          }
          if (!localBadSolve) {
            int idx = ind->idx;
            int ixds = ind->ixds;
            int trueIdx = ind->extraDoseTimeIdx[ind->idxExtra];
            ind->idx = -1 - trueIdx;
            if (recJumps) std::copy(yp, yp + nBase, yPre.begin());
            handle_evid(ind->extraDoseEvid[trueIdx], neq[0],
                        ind->BadDose, ind->InfusionRate, ind->dose, yp, xout, neq[1], ind);
            if (recJumps) {
              ++seq;
              for (int c = 0; c < nBase; ++c)
                if (yp[c] != yPre[c]) jumpRec.push_back({ind->extraDoseNewXout, c, yp[c] - yPre[c], seq, false});
            }
            istate = 1;
            ind->ixds = ixds;
            ind->idx = idx;
            ind->idxExtra++;
            // state/rate jumped -> resume the forward from here and start a new
            // history segment.
            for (int k = 0; k < nBase; ++k) NV_Ith_S(y, k) = yp[k];
            CVodeReInit(mem, (sunrealtype)ind->extraDoseNewXout, y);
            hist.newSeg(); cvB_record(&hist, &fu, ind->extraDoseNewXout, yp);
            didEvent = 0;
            if (!isSameTime(xout, ind->extraDoseNewXout)) {
              preSolve(op, ind, ind->extraDoseNewXout, xout, yp);
              if (neqOde > 0) cvB_step(mem, &fu, y, yp, xout, ind->rc, &hist);
              copyLinCmt(neq, ind, op, yp);
              const char *err_msg = "cvodesadj failed";
              postSolve(neq, &istate, ind->rc, &idx, yp, &err_msg, 10, false, ind, op, rx);
              if (*(ind->rc) < 0) localBadSolve = 1;
              ind->extraDoseNewXout = xout;
            }
            xp = ind->extraDoseNewXout;
          }
        }
        if (!localBadSolve && !isSameTime(xout, xp)) {
          preSolve(op, ind, xp, xout, yp);
          if (neqOde > 0) cvB_step(mem, &fu, y, yp, xout, ind->rc, &hist);
          copyLinCmt(neq, ind, op, yp);
          const char *err_msg = "cvodesadj failed";
          postSolve(neq, &istate, ind->rc, &i, yp, &err_msg, 10, true, ind, op, rx);
          if (*(ind->rc) < 0) localBadSolve = 1;
        }
        xp = xout;
      }
    }
    ind->_newind = 2;
    if (!localBadSolve) {
      ind->idx = i;
      ++seq;
      int _yOff = -1;   // evY slot to fill with this record's post-jump primal
      int _dfOff = -1;  // evY slot for an alag bolus's F(y+)-F(y-)
      double _tEv = xout;
      int _evNow = getEvid(ind, ind->ix[i]);
      if (recJumps) std::copy(yp, yp + nBase, yPre.begin());
      // Record replace/multiply state jumps for the backward transpose jump.
      { int _ev = _evNow;
        if (isDose(_ev)) {
          int _wh, _cmt, _wh100, _whI, _wh0;
          getWh(_ev, &_wh, &_cmt, &_wh100, &_whI, &_wh0);
          if (_cmt >= 0 && _cmt < nBase) {
            if (_whI == EVIDF_REPLACE) {
              evJumps.push_back({xout, _cmt, 5, 0.0, seq, -1});
            } else if (_whI == EVIDF_MULT) {
              evJumps.push_back({xout, _cmt, 6,
                getAmt(ind, ind->id, _cmt, getDoseIndex(ind, ind->idx), xout, yp), seq, -1});
            } else if (_whI == 0 && op->adjDlagOff >= 0) {
              // additive bolus in a model with a modeled alag(): record the
              // (lagged) dose time so the backward can add the transversality
              // mu += -dlag_c/dtheta*(lambda^T (F(y+)-F(y-))).
              _yOff = (int)evY.size(); _dfOff = _yOff + nBase;
              evY.resize(evY.size() + 2 * nBase);
              cvB_rhs(&fu, xout, yp, fu.DADT.data());   // F(y-), differenced below
              std::copy(fu.DADT.begin(), fu.DADT.begin() + nBase, evY.begin() + _dfOff);
              evJumps.push_back({xout, _cmt, 0, getDose(ind, ind->ix[i]), seq, _yOff});
            } else if ((_whI == EVIDF_MODEL_RATE_ON || _whI == EVIDF_MODEL_DUR_ON) && op->adjDrateOff >= 0) {
              double _amt = getDose(ind, ind->ix[i]);
              infusRec.push_back({xout, R_PosInf, _cmt, 0.0, _amt,
                                  (_whI == EVIDF_MODEL_DUR_ON) ? _amt : 1.0, 0, -1});
              _infusOnCmt = _cmt;   // R captured after handleEvid1
            } else if ((_whI == EVIDF_MODEL_RATE_OFF || _whI == EVIDF_MODEL_DUR_OFF) && op->adjDrateOff >= 0) {
              for (size_t w = infusRec.size(); w-- > 0;)
                if (infusRec[w].cmt == _cmt && infusRec[w].tOff == R_PosInf) {
                  infusRec[w].tOff = xout; infusRec[w].offSeq = seq;
                  infusRec[w].offY = _yOff = (int)evY.size(); evY.resize(evY.size() + nBase);
                  break;
                }
            }
          }
        } else if (!isObs(_ev) && op->adjDrateOff >= 0) {
          int _wh, _cmt, _wh100, _whI, _wh0;
          getWh(_ev, &_wh, &_cmt, &_wh100, &_whI, &_wh0);
          if ((_whI == EVIDF_MODEL_RATE_OFF || _whI == EVIDF_MODEL_DUR_OFF) && _cmt >= 0 && _cmt < nBase)
            for (size_t w = infusRec.size(); w-- > 0;)
              if (infusRec[w].cmt == _cmt && infusRec[w].tOff == R_PosInf) {
                  infusRec[w].tOff = xout; infusRec[w].offSeq = seq;
                  infusRec[w].offY = _yOff = (int)evY.size(); evY.resize(evY.size() + nBase);
                  break;
                }
        } }
      if (getEvid(ind, ind->ix[i]) == 3) {
        // A reset wipes the state to (parameter-independent) inits, so dPhi/dy = 0
        // and the backward costate zeroes here.  Record before handleEvid3 mutates
        // xout.
        resetT.push_back(xout); resetSeq.push_back(seq);
        handleEvid3(ind, op, rx, neq, &xp, &xout, yp, &(istate), u_inis);
        didEvent = 1;
      } else if (handleEvid1(&i, rx, neq, yp, &xout)) {
        handleSSDelay(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                      xp, ind->id, &i, ind->n_all_times, &istate, op, ind, u_inis, ctx);
        if (ind->wh0 == EVID0_OFF) {
          ind->solve[ind->cmt] = op->inits[ind->cmt];
        }
        if (rx->istateReset) istate = 1;
        xp = xout;
        didEvent = 1;
      }
      // state jumps of a dose (not a steady state, which rebuilds the history)
      if (recJumps && didEvent && _evNow != 3 && isDose(_evNow) && !rxDelayIsSSDose(ind)) {
        int _wh, _cmt, _wh100, _whI, _wh0;
        getWh(_evNow, &_wh, &_cmt, &_wh100, &_whI, &_wh0);
        for (int c = 0; c < nBase; ++c)
          if (yp[c] != yPre[c])
            jumpRec.push_back({_tEv, c, yp[c] - yPre[c], seq,
                               _whI == 0 && c == _cmt && op->adjDlagOff >= 0});
      }
      // Capture the modeled infusion rate R now that handleEvid1 has set it.
      if (_infusOnCmt >= 0 && !infusRec.empty()) {
        infusRec.back().R = ind->InfusionRate[_infusOnCmt];
        _infusOnCmt = -1;
      }
      // Resume the forward from the (possibly jumped) state and open a new segment.
      if (_yOff >= 0) std::copy(yp, yp + nBase, evY.begin() + _yOff);
      if (_dfOff >= 0) {
        cvB_rhs(&fu, xout, yp, fu.DADT.data());
        for (int k = 0; k < nBase; ++k) evY[_dfOff + k] = fu.DADT[k] - evY[_dfOff + k];
      }
      if (didEvent) {
        for (int k = 0; k < nBase; ++k) NV_Ith_S(y, k) = yp[k];
        CVodeReInit(mem, (sunrealtype)xout, y);
        hist.newSeg(); cvB_record(&hist, &fu, xout, yp);
      }
      int _mtime_requeued = 0;
      if (rx->nMtime > 0) {
        if (recomputeMtimeIfNeeded(rx, ind, yp, i, xout)) {
          ind->mainSorted = 0;
          _mtime_requeued = 1;
        }
      }
      if (rx->needSort & needSortAlag) {
        if (refreshLagTimesIfNeeded(rx, ind, yp, i + 1, xout)) {
          ind->mainSorted = 0;
        }
      }
      updateSolve(ind, op, neq, xout, i, ind->n_all_times);
      if (isObs(getEvid(ind, ind->ix[i]))) { obsT.push_back(xout); obsIx.push_back(i); obsSeq.push_back(seq); }
      if (_mtime_requeued) i--;
    }
    ind->solvedIdx = i;
  }

  if (LS) SUNLinSolFree(LS);
  if (Amat) SUNMatDestroy(Amat);
  if (mem) CVodeFree(&mem);
  if (fu.dky) N_VDestroy(fu.dky);
  N_VDestroy(y);

  // ================= backward sweep -> rx__sens_* output slots =================
  if (localBadSolve) return 0;
  int nAug = nBase + np;
  cvBwdUser bu;
  bu.hist = &hist; bu.cSub = cSub; bu.nBase = nBase; bu.np = np; bu.eff = eff;
  bu.fxOff = op->adjFxOff; bu.fpOff = op->adjFpOff; bu.lhs = ind->lhs;
  bu.drateOff = op->adjDrateOff; bu.infus = &infusRec;
  bu.A.assign(eff, 0.0); bu.yq.assign(nBase, 0.0);
  // Off-boundary transversality events (type 7): stop at each infusion off-time.
  for (size_t w = 0; w < infusRec.size(); ++w)
    if (infusRec[w].tOff < R_PosInf && infusRec[w].R != 0.0)
      evJumps.push_back({infusRec[w].tOff, infusRec[w].cmt, 7,
                         -(infusRec[w].amt / infusRec[w].R) * infusRec[w].durMult,
                         infusRec[w].offSeq, infusRec[w].offY});

  N_Vector B = N_VNew_Serial(nAug, sunctx);
  void *bmem = CVodeCreate(CV_BDF, sunctx);
  int bok = (bmem != NULL);
  if (bok && CVodeInit(bmem, cvB_bwd, (sunrealtype)t0, B) < 0) bok = 0;  // re-init per solve below
  if (bok && CVodeSStolerances(bmem, rtol, atol) < 0) bok = 0;
  if (bok && CVodeSetUserData(bmem, (void *)&bu) < 0) bok = 0;
  SUNMatrix BM = bok ? SUNDenseMatrix(nAug, nAug, sunctx) : NULL;
  SUNLinearSolver BLS = bok ? SUNLinSol_Dense(B, BM, sunctx) : NULL;
  if (bok && (!BM || !BLS || CVodeSetLinearSolver(bmem, BLS, BM) < 0)) bok = 0;
  if (bok) CVodeSetMaxNumSteps(bmem, (long int)(op->mxstep > 0 ? op->mxstep : 50000));
  // delay(): the distinct delays (constant per subject) for the anticipating
  // term; the backward step is capped at the smallest so lambda(t+tau) is
  // already recorded.
  N_Vector dkyB = NULL;
  std::vector<double> tau0;         // delay matrix (i*nBase+j), 0: none
  if (bok && op->hasDelay && op->adjFxdOff >= 0 && op->adjTauOff >= 0) {
    // delays depend on parameters only; any state will do
    double *A = bu.A.data(), *y0 = getSolve(0);
    for (int q = 0; q < eff; ++q) A[q] = 0.0;
    for (int q = 0; q < nBase; ++q) A[q] = y0[q];
    calc_lhs(cSub, t0, A, ind->lhs);
    tau0.assign(ind->lhs + op->adjTauOff, ind->lhs + op->adjTauOff + nBase * nBase);
    for (size_t q = 0; q < tau0.size(); ++q) {
      double tq = tau0[q];
      if (!(tq > 0.0)) continue;
      bool seen = false;
      for (size_t d = 0; d < bu.taus.size(); ++d)
        if (fabs(bu.taus[d] - tq) <= 1e-12 * (1.0 + tq)) seen = true;
      if (!seen) bu.taus.push_back(tq);
    }
    if (!bu.taus.empty()) {
      bu.dde = true;
      bu.fxdOff = op->adjFxdOff; bu.tauOff = op->adjTauOff;
      bu.mxstep = (long int)(op->mxstep > 0 ? op->mxstep : 50000);
      bu.lam.n = nBase;
      bu.lamTop.assign(nBase, 0.0); bu.lamS.assign(nBase, 0.0); bu.ant.assign(nBase, 0.0);
      CVodeSetMaxStep(bmem, (sunrealtype)*std::min_element(bu.taus.begin(), bu.taus.end()));
      dkyB = N_VNew_Serial(nAug, sunctx);
      if (dkyB == NULL) bok = 0;
    }
  }

  for (int oi = (int)obsT.size() - 1; bok && oi >= 0; --oi) {
    double ti = obsT[oi]; int solveIdx = obsIx[oi], si = obsSeq[oi];
    double *out = getSolve(solveIdx);
    // A reset processed before the observation zeroes the costate (dy_k(ti)/dp
    // depends only on the post-reset window), so the sweep only needs to reach
    // the most recent such reset; below tReset the costate is 0 and mu is frozen.
    double tStop = t0; int stopSeq = 0, hasReset = 0;
    for (size_t r = 0; r < resetT.size(); ++r)
      if (resetSeq[r] < si && resetSeq[r] > stopSeq) {
        tStop = resetT[r]; stopSeq = resetSeq[r]; hasReset = 1;
      }
    if (hasReset && isSameTime(ti, tStop)) {
      for (int q = 0; q < nBase * np; ++q) out[sensOff + q] = 0.0;
      continue;
    }
    // events processed between the reset and the observation (at either end's
    // time too), in reverse processing order: the backward stops at each and
    // applies the transpose costate jump.
    std::vector<int> segEv;
    for (size_t e = 0; e < evJumps.size(); ++e)
      if (evJumps[e].seq < si && evJumps[e].seq > stopSeq)
        segEv.push_back((int)e);
    gfx::timsort(segEv.begin(), segEv.end(),
                 [&](int a, int b) { return evJumps[a].seq > evJumps[b].seq; });
    // delay() breakpoints: the anticipated costate jumps where t + tau reaches
    // the observation or a replace/multiply costate jump.
    std::vector<double> bps;
    if (bu.dde) {
      for (size_t d = 0; d < bu.taus.size(); ++d) {
        bps.push_back(ti - bu.taus[d]);
        for (size_t s = 0; s < segEv.size(); ++s) {
          int ty = evJumps[segEv[s]].type;
          if (ty == 5 || ty == 6) bps.push_back(evJumps[segEv[s]].t - bu.taus[d]);
        }
      }
      std::vector<double> keep;
      for (size_t q = 0; q < bps.size(); ++q)
        if (bps[q] > tStop && bps[q] < ti) keep.push_back(bps[q]);
      std::sort(keep.begin(), keep.end(), std::greater<double>());
      keep.erase(std::unique(keep.begin(), keep.end(),
                             [](double a, double b) { return fabs(a - b) <= cvB_teps(a); }),
                 keep.end());
      bps.swap(keep);
      bu.ti = ti;
    }
    for (int k = 0; bok && k < nBase; ++k) {
      for (int j = 0; j < nBase; ++j) NV_Ith_S(B, j) = (j == k) ? 1.0 : 0.0;
      for (int p = 0; p < np; ++p) NV_Ith_S(B, nBase + p) = 0.0;
      if (bu.dde) {
        bu.lam.clear();
        for (int j = 0; j < nBase; ++j) bu.lamTop[j] = (j == k) ? 1.0 : 0.0;
      }
      if (CVodeReInit(bmem, (sunrealtype)ti, B) < 0) { bok = 0; break; }
      // integrate ti -> each event (applying its costate jump) -> tStop; mu (the
      // quadrature) is a component of B, so it carries across the reinits.
      double tCur = ti;
      for (size_t s = 0; s < segEv.size() && bok; ++s) {
        double te = evJumps[segEv[s]].t;
        if (!isSameTime(te, tCur)) {   // events sharing a time jump in turn, without integrating
          if (cvB_bwdTo(bmem, B, dkyB, &bu, tCur, te, bps) < 0) { bok = 0; break; }
          tCur = te;
        }
        int c = evJumps[segEv[s]].cmt; int ty = evJumps[segEv[s]].type;
        if (ty == 5) NV_Ith_S(B, c) = 0.0;                             // replace
        else if (ty == 6) NV_Ith_S(B, c) *= evJumps[segEv[s]].factor;  // multiply
        else if (ty == 0) {                                            // additive-alag transversality
          // dlag at the primal just after this dose; lambda unchanged (dPhi/dy=I).
          double *yq = bu.yq.data(), *A = bu.A.data();
          const cvEvent &ev = evJumps[segEv[s]];
          if (ev.y >= 0) std::copy(evY.begin() + ev.y, evY.begin() + ev.y + nBase, yq);
          else hist.interp((double)te, yq);
          for (int ii = 0; ii < eff; ++ii) A[ii] = 0.0;
          for (int ii = 0; ii < nBase; ++ii) A[ii] = yq[ii];
          calc_lhs(cSub, (double)te, A, ind->lhs);
          const double *dlag = &ind->lhs[op->adjDlagOff];
          const double *dF = &evY[ev.y + nBase];
          double sc = 0.0;
          for (int ii = 0; ii < nBase; ++ii) sc += NV_Ith_S(B, ii) * dF[ii];
          for (int p = 0; p < np; ++p) NV_Ith_S(B, nBase + p) += -dlag[c * np + p] * sc;
        } else {                                                       // ty==7: infusion off-boundary
          // mu += -(amt/R) * dR/dtheta * lambda_c(t_off).  factor = -(amt/R).
          double *yq = bu.yq.data(), *A = bu.A.data();
          const cvEvent &ev = evJumps[segEv[s]];
          if (ev.y >= 0) std::copy(evY.begin() + ev.y, evY.begin() + ev.y + nBase, yq);
          else hist.interp((double)te, yq);
          for (int ii = 0; ii < eff; ++ii) A[ii] = 0.0;
          for (int ii = 0; ii < nBase; ++ii) A[ii] = yq[ii];
          calc_lhs(cSub, (double)te, A, ind->lhs);
          const double *drate = &ind->lhs[op->adjDrateOff];
          double fac = evJumps[segEv[s]].factor, lc = NV_Ith_S(B, c);
          for (int p = 0; p < np; ++p) NV_Ith_S(B, nBase + p) += fac * drate[c * np + p] * lc;
        }
        if (CVodeReInit(bmem, (sunrealtype)te, B) < 0) { bok = 0; break; }
      }
      if (!bok) break;
      if (!isSameTime(tStop, tCur)) {
        if (cvB_bwdTo(bmem, B, dkyB, &bu, tCur, tStop, bps) < 0) { bok = 0; break; }
      }
      for (int p = 0; p < np; ++p) out[sensOff + k * np + p] = NV_Ith_S(B, nBase + p);
      // Parameter-dependent delay: a jump [y_j] at t_e (bolus, replace,
      // multiply) jumps delay(y_j, tau_ij), and so f_i, at xi = t_e + tau_ij:
      //   mu += -lambda_i(xi) [f_i(xi+) - f_i(xi-)] dtau_ij/dp.
      // The jump of f_i is measured at t = xi by moving only the delay lookups
      // (delayTOff), with a stencil that cancels their smooth part, so it is
      // exact for a nonlinear f.  When several delayed states arrive at xi
      // together each keeps its linearized share F_Xd_ij [y_j] and the rest of
      // the jump is split by size (exact for a linear f).
      if (bu.dde && op->adjDtauOff >= 0) {
        double *A = bu.A.data(), *yq = bu.yq.data(), *ls = bu.lamS.data();
        struct cvArrival { int i, j; double xi, amt, tEv; bool lag; };
        std::vector<cvArrival> arr;
        for (size_t d = 0; d < jumpRec.size(); ++d) {
          const cvJump &bd = jumpRec[d];
          if (bd.seq >= si || bd.seq <= stopSeq) continue;
          for (int ii = 0; ii < nBase; ++ii) {
            double tau = tau0[ii * nBase + bd.cmt], xi = bd.t + tau;
            if (!(tau > 0.0) || xi > ti + cvB_teps(ti)) continue;
            bool merged = false;
            for (size_t q = 0; q < arr.size(); ++q)
              if (arr[q].i == ii && arr[q].j == bd.cmt && arr[q].lag == bd.lag &&
                  isSameTime(arr[q].xi, xi)) {
                arr[q].amt += bd.dy;
                merged = true;
              }
            if (!merged) arr.push_back({ii, bd.cmt, xi, bd.dy, bd.t, bd.lag});
          }
        }
        std::vector<char> used(arr.size(), 0);
        std::vector<double> fs(4 * (size_t)eff);
        for (size_t a = 0; a < arr.size(); ++a) {
          if (used[a]) continue;
          int ii = arr[a].i;
          // one-sided extrapolations err by O(dlt^2 f''): keep dlt only clear
          // of the roundoff in t
          double dlt = fmax(1e-8, 1e4 * DBL_EPSILON * fabs(arr[a].xi));
          // arrivals into f_i closer than the stencil are measured together
          std::vector<size_t> grp(1, a);
          used[a] = 1;
          for (size_t g = 0; g < grp.size(); ++g)
            for (size_t q = a + 1; q < arr.size(); ++q)
              if (!used[q] && arr[q].i == ii && fabs(arr[q].xi - arr[grp[g]].xi) <= 4.0 * dlt) {
                grp.push_back(q);
                used[q] = 1;
              }
          double xi = arr[a].xi, xhi = arr[a].xi;
          for (size_t g = 0; g < grp.size(); ++g) {
            xi = fmin(xi, arr[grp[g]].xi);
            xhi = fmax(xhi, arr[grp[g]].xi);
          }
          hist.interp(xi, yq);
          // f_i extrapolated from the left to the first arrival and from the
          // right to the last one, at t = xi with only the lookups moved
          const double off[4] = {-2.0 * dlt, -dlt, xhi - xi + dlt, xhi - xi + 2.0 * dlt};
          for (int m = 0; m < 4; ++m) {
            ind->delayTOff = off[m];
            cvB_rhs(&fu, xi, yq, &fs[(size_t)m * eff]);
          }
          ind->delayTOff = 0.0;
          double jump = 2.0 * fs[2 * (size_t)eff + ii] - fs[3 * (size_t)eff + ii] -
            2.0 * fs[(size_t)eff + ii] + fs[ii];
          for (int q = 0; q < eff; ++q) A[q] = 0.0;
          for (int q = 0; q < nBase; ++q) A[q] = yq[q];
          // linearized arrival F_Xd_ij [y_j], F_Xd averaged over both sides
          std::vector<double> lin(grp.size(), 0.0);
          for (int side = 0; side < 2; ++side) {
            ind->delayTOff = (side == 0) ? -dlt : xhi - xi + dlt;
            calc_lhs(cSub, xi, A, ind->lhs);
            for (size_t g = 0; g < grp.size(); ++g)
              lin[g] += 0.5 * ind->lhs[op->adjFxdOff + ii * nBase + arr[grp[g]].j] * arr[grp[g]].amt;
          }
          ind->delayTOff = 0.0;
          // a modeled dose lag moves the arrival too: d(xi)/dp = dtau/dp + dlag_j/dp
          std::vector<double> dlagv(grp.size() * (size_t)np, 0.0);
          for (size_t g = 0; g < grp.size(); ++g) {
            const cvArrival &ar = arr[grp[g]];
            if (!ar.lag) continue;
            hist.interp(ar.tEv, yq);
            for (int q = 0; q < nBase; ++q) A[q] = yq[q];
            calc_lhs(cSub, ar.tEv, A, ind->lhs);
            for (int p = 0; p < np; ++p) dlagv[g * np + p] = ind->lhs[op->adjDlagOff + ar.j * np + p];
          }
          hist.interp(xi, yq);
          for (int q = 0; q < nBase; ++q) A[q] = yq[q];
          calc_lhs(cSub, xi, A, ind->lhs);
          // each arrival keeps its linearized share; the nonlinear rest of the
          // measured jump is split by size (all of it for a single arrival)
          double lsum = 0.0, labs = 0.0;
          for (size_t g = 0; g < grp.size(); ++g) {
            lsum += lin[g];
            labs += fabs(lin[g]);
          }
          for (size_t g = 0; g < grp.size(); ++g) {
            const cvArrival &ar = arr[grp[g]];
            double w = (labs > 0.0) ? fabs(lin[g]) / labs : 1.0 / (double)grp.size();
            double share = lin[g] + (jump - lsum) * w;
            cvB_lamAt(&bu, ar.xi, ls);
            double li = ls[ii];
            const double *dt = &ind->lhs[op->adjDtauOff + (ii * nBase + ar.j) * np];
            for (int p = 0; p < np; ++p)
              out[sensOff + k * np + p] += -li * share * (dt[p] + dlagv[g * np + p]);
          }
        }
      }
    }
  }

  if (B) N_VDestroy(B);
  if (dkyB) N_VDestroy(dkyB);
  if (BLS) SUNLinSolFree(BLS);
  if (BM) SUNMatDestroy(BM);
  if (bmem) CVodeFree(&bmem);
  return bok;
}

extern "C" void ind_cvodesadj_0(rx_solve *rx, rx_solving_options *op, int solveid,
                                int *neq, t_dydt c_dydt, t_update_inis u_inis) {
  clock_t tclk = clock();
  neq[1] = solveid; // subject id, not an rx->ordId position
  rx_solving_options_ind *ind = &(rx->subjects[neq[1]]);
  neq[0] = rxEffNeq(ind, op);
  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) return;
  ind->solvedIdx = 0;
  SUNContext sunctx = NULL;
  if (SUNContext_Create(SUN_COMM_NULL, &sunctx) != 0) {
    if (ind->rc[0] == 0) ind->rc[0] = -2019;
    rxSolverFail(ind);
    return;
  }
  if (!cvodesAdjSolveSubject(rx, op, ind, neq, 1, c_dydt, u_inis, sunctx)) {
    if (ind->rc[0] == 0) ind->rc[0] = -2019;
    rxSolverFail(ind);
  }
  SUNContext_Free(&sunctx);
  ind->solveTime += ((double)(clock() - tclk)) / CLOCKS_PER_SEC;
}

extern "C" void ind_cvodesadj(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis) {
  rx_solving_options *op = rx->op;
  int neq[2]; neq[0] = op->neq; neq[1] = 0;
  ind_cvodesadj_0(rx, op, solveid, neq, c_dydt, u_inis);
}

extern "C" void par_cvodesadj(rx_solve *rx) {
  // CVODES manages its own per-subject SUNContext; run serially for now.
  rx_solving_options *op = rx->op;
  int nsolve = (int)(rx->nsim * rx->nsub);
  uint32_t seed0 = getRxSeed1(1);
  for (int solveid = 0; solveid < nsolve; ++solveid) {
    int neq[2]; neq[0] = op->neq; neq[1] = 0;
    // rx->ordId walks positions (sortIds); the drivers take subject ids.
    int _id = rx->ordId[solveid] - 1;
    // this loop seeded nothing at all, so its subjects inherited whatever
    // stream was current -- seed per subject like every other par_*() loop
    setSeedEng1(seed0 + _id);
    ind_cvodesadj_0(rx, op, _id, neq, dydt, update_inis);
  }
  // close the per-subject seed block: the loop consumed nsolve seeds, not
  // cores, and the next solve must not re-use any of them
  setRxSeedFinal(seed0 + (uint32_t)nsolve);
}

#endif // IN_PAR_SOLVE
