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
      if (t[segStart[si]] <= tq + rxAdjStepTol(t[segStart[si]], tq)) s = si; else break;
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

struct cvFwdUser {   // forward RHS user data
  int *neq; t_dydt dydt; int nBase, eff;
  std::vector<double> A, DADT;
};
// An infusion window (time-based) for the cvodesadj rate and F duals.  amt is
// F*amt; durMult is 1 (rate) or F*amt (dur); amtRaw is the dose before F.  A
// fixed-rate/duration window (fixed) carries only the dF dual, weighted by dfW: the
// raw rate over a fixed-dur window, the raw amount at a fixed-rate off boundary.
struct cvInfus { double tOn, tOff; int cmt; double R, amt, durMult, amtRaw; bool isDur;
                 bool fixed = false; double dfW = 0.0; int key = -1; };

// Index of the open window into cmt that a stop with start key `key` closes (see
// rk4sInfusionStartKey()): the window with that key, else the most recent open
// one of the same kind, or -1.
static inline int cvInfusCloseIdx(const std::vector<cvInfus> &infus, int cmt, bool fixed, bool isDur, int key) {
  int last = -1;
  for (size_t w = infus.size(); w-- > 0;) {
    const cvInfus &F = infus[w];
    if (F.fixed != fixed || F.cmt != cmt || F.tOff != R_PosInf || (fixed && F.isDur != isDur)) continue;
    if (key >= 0 && F.key == key) return (int)w;
    if (last < 0) last = (int)w;
  }
  return last;
}

struct cvBwdUser {   // backward RHS user data
  const cvHist *hist; int cSub, nBase, np, eff, fxOff, fpOff, drateOff, dfOff;
  const std::vector<cvInfus> *infus;   // for the in-window forcing quadrature
  std::vector<double> A, yq;
  double *lhs;
};

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
  u->hist->interp((double)t, yq);
  for (int i = 0; i < u->eff; ++i) A[i] = 0.0;
  for (int i = 0; i < n; ++i) A[i] = yq[i];
  calc_lhs(u->cSub, t, A, u->lhs);
  const double *fx = &u->lhs[u->fxOff];
  const double *fp = &u->lhs[u->fpOff];
  for (int j = 0; j < n; ++j) {
    double s = 0.0;
    for (int i = 0; i < n; ++i) s += fx[i * n + j] * NV_Ith_S(B, i);
    NV_Ith_S(Bd, j) = -s;
  }
  for (int p = 0; p < np; ++p) {
    double s = 0.0;
    for (int i = 0; i < n; ++i) s += fp[i * np + p] * NV_Ith_S(B, i);
    NV_Ith_S(Bd, n + p) = -s;
  }
  // infusion forcing: inside a window the RHS gains +R, so F_p[c] += dR/dtheta ->
  // d(mu_p)/dt += -dR/dtheta * lambda_c.
  if (u->infus) {
    for (size_t w = 0; w < u->infus->size(); ++w) {
      const cvInfus &F = (*u->infus)[w];
      if ((double)t >= F.tOn - rxAdjStepTol(t, F.tOn) && (double)t <= F.tOff + rxAdjStepTol(t, F.tOff)) {
        double lc = NV_Ith_S(B, F.cmt);
        if (u->drateOff >= 0 && !F.fixed) {   // dR/dtheta = durMult * drate
          const double *drate = &u->lhs[u->drateOff];
          for (int p = 0; p < np; ++p) NV_Ith_S(Bd, n + p) += -F.durMult * drate[F.cmt * np + p] * lc;
        }
        // a dur() or fixed-dur rate also moves with a modeled F
        if (u->dfOff >= 0 && F.isDur && F.tOff < R_PosInf && F.tOff > F.tOn) {
          const double *dF = &u->lhs[u->dfOff];
          double dfm = F.fixed ? F.dfW : F.amtRaw / (F.tOff - F.tOn);
          for (int p = 0; p < np; ++p) NV_Ith_S(Bd, n + p) += -dfm * dF[F.cmt * np + p] * lc;
        }
      }
    }
  }
  return 0;
}

// Record the current (t, base states) plus f=dydt into the history segment.
static void cvB_record(cvHist *hist, cvFwdUser *u, double t, const double *yp) {
  double *A = u->A.data(), *D = u->DADT.data();
  for (int i = 0; i < u->eff; ++i) A[i] = 0.0;
  for (int i = 0; i < u->nBase; ++i) A[i] = yp[i];
  u->dydt(u->neq, t, A, D);
  hist->push(t, yp, D);
}

// Step the forward primal from the integrator's current time to xout using
// CV_ONE_STEP, recording each accepted step into the current history segment.
static void cvB_step(void *mem, cvFwdUser *u, N_Vector y, double *yp,
                     double xout, int *rc, cvHist *hist, int mxstep) {
  for (int k = 0; k < u->nBase; ++k) NV_Ith_S(y, k) = yp[k];
  CVodeSetStopTime(mem, (sunrealtype)xout);
  sunrealtype tcur = 0.0;
  // one step per call (to record the history), so CVODE's own step limit never
  // applies: cap the steps per output interval at maxsteps like the other solvers
  int guard = 0, maxit = mxstep > 0 ? mxstep : 10000000;
  while (true) {
    int flag = CVode(mem, (sunrealtype)xout, y, &tcur, CV_ONE_STEP);
    if (flag < 0) { if (*rc == 0) *rc = -2019; return; }
    for (int k = 0; k < u->nBase; ++k) yp[k] = NV_Ith_S(y, k);
    cvB_record(hist, u, (double)tcur, yp);
    if (flag == CV_TSTOP_RETURN || tcur >= xout) break;
    if (++guard > maxit) { if (*rc == 0) *rc = -2019; return; }
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
  if (!ok) {
    if (LS) SUNLinSolFree(LS);
    if (Amat) SUNMatDestroy(Amat);
    if (mem) CVodeFree(&mem);
    N_VDestroy(y);
    return 0;
  }

  std::vector<double> obsT; std::vector<int> obsIx;
  std::vector<size_t> obsReset;   // resets processed before each observation was stored
  std::vector<size_t> obsEv;      // evJumps recorded before it (the events it follows)
  std::vector<size_t> resetEv;    // evJumps recorded before each reset
  std::vector<double> resetT;   // evid-3 reset times (costate jump: lambda := 0)
  // replace (type 5: lambda[c]:=0) / multiply (type 6: lambda[c]*=factor) events;
  // the backward sweep stops at each and applies the transpose costate jump.
  // type 0 (additive bolus): factor = F*amt (the landed amount), raw = amt
  struct cvEvent { double t; int cmt; int type; double factor; double raw = 0.0; };
  std::vector<cvEvent> evJumps;
  std::vector<cvInfus> infusRec;   // modeled-rate infusion windows (rate dual)
  int _infusOnCmt = -1; double _infusOnPre = 0.0;
  int _lagRateCmt = -1, _lagRateWhI = 0, _lagRateKey = -1; double _lagRatePre = 0.0;   // infusion start/stop
  double *yp;
  hist.newSeg();   // segment 0 begins at t0
  // Record boluses only when F or the lag time depends on a sensitivity
  // parameter (a non-zero dF or dlag block); otherwise they add no dual.
  bool bolusDuals = false;
  if (op->adjDfOff >= 0 || op->adjDlagOff >= 0) {
    std::vector<double> _a(eff, 0.0);
    calc_lhs(cSub, t0, _a.data(), ind->lhs);
    for (int q = 0; q < nBase * np && !bolusDuals; ++q) {
      if (op->adjDfOff >= 0 && ind->lhs[op->adjDfOff + q] != 0.0) bolusDuals = true;
      if (op->adjDlagOff >= 0 && ind->lhs[op->adjDlagOff + q] != 0.0) bolusDuals = true;
    }
  }

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
            if (neqOde > 0) cvB_step(mem, &fu, y, yp, ind->extraDoseNewXout, ind->rc, &hist, op->mxstep);
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
            handle_evid(ind->extraDoseEvid[trueIdx], neq[0],
                        ind->BadDose, ind->InfusionRate, ind->dose, yp, xout, neq[1], ind);
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
              if (neqOde > 0) cvB_step(mem, &fu, y, yp, xout, ind->rc, &hist, op->mxstep);
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
          if (neqOde > 0) cvB_step(mem, &fu, y, yp, xout, ind->rc, &hist, op->mxstep);
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
      // Record replace/multiply state jumps for the backward transpose jump.
      { int _ev = getEvid(ind, ind->ix[i]);
        if (op->adjDlagOff >= 0 || op->adjDfOff >= 0) {
          _lagRateCmt = rk4sInfusionEventCmt(_ev, nBase);
          if (_lagRateCmt >= 0) {
            int _wh, _c, _wh100, _wh0; getWh(_ev, &_wh, &_c, &_wh100, &_lagRateWhI, &_wh0);
            _lagRatePre = ind->InfusionRate[_lagRateCmt];
            _lagRateKey = rk4sInfusionStartKey(ind, ind->ix[i]);
          }
        }
        if (isDose(_ev)) {
          int _wh, _cmt, _wh100, _whI, _wh0;
          getWh(_ev, &_wh, &_cmt, &_wh100, &_whI, &_wh0);
          if (_cmt >= 0 && _cmt < nBase) {
            if (_whI == EVIDF_REPLACE) {
              evJumps.push_back({xout, _cmt, 5, 0.0});
            } else if (_whI == EVIDF_MULT) {
              evJumps.push_back({xout, _cmt, 6,
                getAmt(ind, ind->id, _cmt, getDoseIndex(ind, ind->idx), xout, yp)});
            } else if (_whI == 0 && bolusDuals) {
              // additive bolus: record the (lagged) dose time so the backward can
              // add the modeled-F dual mu += amt*dF/dtheta*lambda_c and the alag
              // transversality mu += -F*amt*dlag_c/dtheta*(lambda^T F_X[:,c]).
              double _raw = getDose(ind, ind->ix[i]);
              evJumps.push_back({xout, _cmt, 0, getAmt(ind, ind->id, _cmt, _raw, xout, yp), _raw});
            } else if ((_whI == EVIDF_MODEL_RATE_ON || _whI == EVIDF_MODEL_DUR_ON) && op->adjDrateOff >= 0) {
              double _raw = getDose(ind, ind->ix[i]);
              double _amt = getAmt(ind, ind->id, _cmt, _raw, xout, yp);
              infusRec.push_back({xout, R_PosInf, _cmt, 0.0, _amt,
                                  (_whI == EVIDF_MODEL_DUR_ON) ? _amt : 1.0, _raw,
                                  _whI == EVIDF_MODEL_DUR_ON});
              infusRec.back().key = rk4sInfusionStartKey(ind, ind->ix[i]);
              _infusOnCmt = _cmt;   // R captured after handleEvid1
              _infusOnPre = ind->InfusionRate[_cmt];
            } else if ((_whI == EVIDF_MODEL_RATE_OFF || _whI == EVIDF_MODEL_DUR_OFF) && op->adjDrateOff >= 0) {
              int _w = cvInfusCloseIdx(infusRec, _cmt, false, false, rk4sInfusionStartKey(ind, ind->ix[i]));
              if (_w >= 0) infusRec[_w].tOff = xout;
            }
          }
        } else if (!isObs(_ev) && op->adjDrateOff >= 0) {
          int _wh, _cmt, _wh100, _whI, _wh0;
          getWh(_ev, &_wh, &_cmt, &_wh100, &_whI, &_wh0);
          if ((_whI == EVIDF_MODEL_RATE_OFF || _whI == EVIDF_MODEL_DUR_OFF) && _cmt >= 0 && _cmt < nBase) {
            int _w = cvInfusCloseIdx(infusRec, _cmt, false, false, rk4sInfusionStartKey(ind, ind->ix[i]));
            if (_w >= 0) infusRec[_w].tOff = xout;
          }
        } }
      if (getEvid(ind, ind->ix[i]) == 3) {
        // A reset wipes the state to its initial conditions, so dPhi/dy = 0
        // and the backward costate zeroes here.  Record before handleEvid3 mutates
        // xout.
        resetT.push_back(xout); resetEv.push_back(evJumps.size());
        handleEvid3(ind, op, rx, neq, &xp, &xout, yp, &(istate), u_inis);
        didEvent = 1;
      } else if (handleEvid1(&i, rx, neq, yp, &xout)) {
        handleSS(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                 xp, ind->id, &i, ind->n_all_times, &istate, op, ind, u_inis, ctx);
        if (ind->wh0 == EVID0_OFF) {
          ind->solve[ind->cmt] = op->inits[ind->cmt];
        }
        if (rx->istateReset) istate = 1;
        xp = xout;
        didEvent = 1;
      }
      // Capture the modeled infusion rate R now that handleEvid1 has set it.
      if (_infusOnCmt >= 0 && !infusRec.empty()) {
        // this window's own rate: the increment, since an overlapping window
        // into the same cmt already contributes to InfusionRate
        infusRec.back().R = ind->InfusionRate[_infusOnCmt] - _infusOnPre;
        _infusOnCmt = -1;
      }
      if (_lagRateCmt >= 0) {   // type 9: the rate into cmt changes by factor at a lagged time
        double _dR = ind->InfusionRate[_lagRateCmt] - _lagRatePre;
        if (_dR != 0.0 && op->adjDlagOff >= 0) evJumps.push_back({xout, _lagRateCmt, 9, _dR});
        if (_dR != 0.0 && op->adjDfOff >= 0 && (_lagRateWhI == EVIDF_INF_RATE || _lagRateWhI == EVIDF_INF_DUR)) {
          // fixed rate/dur: F scales a fixed-dur rate and stretches a fixed-rate window
          bool _isDur = (_lagRateWhI == EVIDF_INF_DUR);
          double _f = getAmt(ind, ind->id, _lagRateCmt, 1.0, xout, yp);
          if (_f != 0.0 && _dR > 0.0) {
            cvInfus _w = {xout, R_PosInf, _lagRateCmt, _dR, 0.0, 0.0, 0.0, _isDur};
            _w.fixed = true; _w.dfW = _isDur ? _dR / _f : 0.0; _w.key = _lagRateKey;
            infusRec.push_back(_w);
          } else if (_f != 0.0) {
            int _wi = cvInfusCloseIdx(infusRec, _lagRateCmt, true, _isDur, _lagRateKey);
            if (_wi >= 0) {
              cvInfus &F = infusRec[_wi];
              F.tOff = xout;
              if (!_isDur) F.dfW = -_dR * (xout - F.tOn) / _f;
            }
          }
        }
        _lagRateCmt = -1;
      }
      // Resume the forward from the (possibly jumped) state and open a new segment.
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
      if (isObs(getEvid(ind, ind->ix[i]))) {
        obsT.push_back(xout); obsIx.push_back(i); obsReset.push_back(resetT.size()); obsEv.push_back(evJumps.size());
      }
      if (_mtime_requeued) i--;
    }
    ind->solvedIdx = i;
  }

  if (LS) SUNLinSolFree(LS);
  if (Amat) SUNMatDestroy(Amat);
  if (mem) CVodeFree(&mem);
  N_VDestroy(y);

  // ================= backward sweep -> rx__sens_* output slots =================
  if (localBadSolve) return 0;
  int nAug = nBase + np;
  cvBwdUser bu;
  bu.hist = &hist; bu.cSub = cSub; bu.nBase = nBase; bu.np = np; bu.eff = eff;
  bu.fxOff = op->adjFxOff; bu.fpOff = op->adjFpOff; bu.lhs = ind->lhs;
  bu.drateOff = op->adjDrateOff; bu.dfOff = op->adjDfOff; bu.infus = &infusRec;
  bu.A.assign(eff, 0.0); bu.yq.assign(nBase, 0.0);
  // Off-boundary transversality events (type 7): stop at each infusion off-time.
  for (size_t w = 0; w < infusRec.size(); ++w) {
    const cvInfus &F = infusRec[w];
    if (F.tOff == R_PosInf || F.R == 0.0 || (F.fixed && F.isDur)) continue;
    evJumps.push_back({F.tOff, F.cmt, 7, F.fixed ? 0.0 : -(F.amt / F.R) * F.durMult,
                       F.fixed ? F.dfW : (F.isDur ? 0.0 : F.amtRaw)});
  }

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

  std::vector<double> dx0V;                         // parameter-dependent initial conditions
  rk4sIniDx0(op, ind, cSub, t0, eff, nBase, np, dx0V);
  const double *dx0P = dx0V.empty() ? NULL : dx0V.data();
  // Duals of an additive bolus at te, with B holding the post-dose costate
  // (dPhi/dy = I): modeled F adds amt*dF/dtheta*lambda_c, and a modeled alag
  // adds -F*amt*dlag_c/dtheta*(lambda^T F_X[:,c]) at the post-dose primal.
  auto bolusDual = [&](const cvEvent &ev) {
    double *yq = bu.yq.data(), *A = bu.A.data();
    hist.interp(ev.t, yq);
    for (int ii = 0; ii < eff; ++ii) A[ii] = 0.0;
    for (int ii = 0; ii < nBase; ++ii) A[ii] = yq[ii];
    calc_lhs(cSub, ev.t, A, ind->lhs);
    int c = ev.cmt;
    if (op->adjDfOff >= 0) {
      const double *dF = &ind->lhs[op->adjDfOff];
      for (int p = 0; p < np; ++p) NV_Ith_S(B, nBase + p) += ev.raw * dF[c * np + p] * NV_Ith_S(B, c);
    }
    if (op->adjDlagOff >= 0) {
      const double *fx = &ind->lhs[bu.fxOff];
      const double *dlag = &ind->lhs[op->adjDlagOff];
      double sc = 0.0;
      for (int ii = 0; ii < nBase; ++ii) sc += NV_Ith_S(B, ii) * fx[ii * nBase + c];
      for (int p = 0; p < np; ++p) NV_Ith_S(B, nBase + p) += -ev.factor * dlag[c * np + p] * sc;
    }
  };
  // The events at time t among evJumps[lo, hi), last first: a bolus's F/alag dual
  // (with the costate of its post-dose state), then replace[c] -> 0 and
  // multiply[c] *= factor.  An observation at an event time reads the state after
  // the events recorded before it.
  auto applyAt = [&](double t, size_t lo, size_t hi) {
    for (size_t e = hi; e-- > lo;) {
      const cvEvent &E = evJumps[e];
      if (!isSameTime(E.t, t)) continue;
      if (E.type == 0) bolusDual(E);
      else if (E.type == 5) NV_Ith_S(B, E.cmt) = 0.0;
      else if (E.type == 6) NV_Ith_S(B, E.cmt) *= E.factor;
    }
  };
  for (int oi = (int)obsT.size() - 1; bok && oi >= 0; --oi) {
    double ti = obsT[oi]; int solveIdx = obsIx[oi];
    double *out = getSolve(solveIdx);
    // A reset zeroes the costate (dy_k(ti)/dp depends only on the post-reset
    // window), so the sweep only needs to reach the most recent reset processed
    // before this observation was stored -- including one at the observation's
    // own time, whose observation reads the post-reset state.
    double tStop = obsReset[oi] > 0 ? resetT[obsReset[oi] - 1] : t0;
    // only events after that reset reach the observation
    size_t evLo = obsReset[oi] > 0 ? resetEv[obsReset[oi] - 1] : 0, evHi = obsEv[oi];
    if (ti <= tStop || isSameTime(ti, tStop)) {   // the initial conditions and the events at ti
      for (int k = 0; k < nBase; ++k) {
        for (int j = 0; j < nBase; ++j) NV_Ith_S(B, j) = (j == k) ? 1.0 : 0.0;
        for (int p = 0; p < np; ++p) NV_Ith_S(B, nBase + p) = 0.0;
        applyAt(ti, evLo, evHi);
        for (int p = 0; p < np; ++p) {
          double ini = 0.0;
          if (dx0P) for (int q = 0; q < nBase; ++q) ini += NV_Ith_S(B, q) * dx0P[q * np + p];
          out[sensOff + k * np + p] = NV_Ith_S(B, nBase + p) + ini;
        }
      }
      continue;
    }
    // replace/multiply events strictly inside (tStop, ti), descending in time and,
    // at one time, in reverse of the order the forward solve applied them: the
    // backward stops at each and applies the transpose costate jump.
    std::vector<int> segEv;
    for (size_t e = 0; e < evJumps.size(); ++e)
      if (evJumps[e].t < ti - rxAdjTimeTol(evJumps[e].t, ti) && evJumps[e].t > tStop + rxAdjTimeTol(evJumps[e].t, tStop)) segEv.push_back((int)e);
    gfx::timsort(segEv.begin(), segEv.end(), [&](int a, int b) {
      return evJumps[a].t > evJumps[b].t || (evJumps[a].t == evJumps[b].t && a > b); });
    for (int k = 0; bok && k < nBase; ++k) {
      for (int j = 0; j < nBase; ++j) NV_Ith_S(B, j) = (j == k) ? 1.0 : 0.0;
      for (int p = 0; p < np; ++p) NV_Ith_S(B, nBase + p) = 0.0;
      applyAt(ti, evLo, evHi);   // the events at ti itself
      if (CVodeReInit(bmem, (sunrealtype)ti, B) < 0) { bok = 0; break; }
      sunrealtype tret;
      // integrate ti -> each event (applying its costate jump) -> tStop; mu (the
      // quadrature) is a component of B, so it carries across the reinits.
      double tCur = ti;
      for (size_t s = 0; s < segEv.size() && bok; ++s) {
        double te = evJumps[segEv[s]].t;
        if (!isSameTime(te, tCur)) {   // events sharing a time jump in turn, without integrating
          CVodeSetStopTime(bmem, (sunrealtype)te);
          if (CVode(bmem, (sunrealtype)te, B, &tret, CV_NORMAL) < 0) { bok = 0; break; }
          tCur = te;
        }
        int c = evJumps[segEv[s]].cmt; int ty = evJumps[segEv[s]].type;
        if (ty == 5) NV_Ith_S(B, c) = 0.0;                             // replace
        else if (ty == 6) NV_Ith_S(B, c) *= evJumps[segEv[s]].factor;  // multiply
        else if (ty == 0) {                                            // additive bolus duals
          bolusDual(evJumps[segEv[s]]);
        } else if (ty == 9) {                                          // lagged rate change
          // mu += -dR * dlag_c/dtheta * lambda_c (factor = dR)
          double *yq = bu.yq.data(), *A = bu.A.data();
          hist.interp((double)te, yq);
          for (int ii = 0; ii < eff; ++ii) A[ii] = 0.0;
          for (int ii = 0; ii < nBase; ++ii) A[ii] = yq[ii];
          calc_lhs(cSub, (double)te, A, ind->lhs);
          const double *dlag = &ind->lhs[op->adjDlagOff];
          double fac = evJumps[segEv[s]].factor, lc = NV_Ith_S(B, c);
          for (int p = 0; p < np; ++p) NV_Ith_S(B, nBase + p) += -fac * dlag[c * np + p] * lc;
        } else {                                                       // ty==7: infusion off-boundary
          // mu += -(F*amt/R)*durMult * dR/dtheta * lambda_c(t_off) (factor), plus
          // amtRaw*dF/dtheta*lambda_c for rate(), whose t_off = t_on + F*amt/R (raw).
          double *yq = bu.yq.data(), *A = bu.A.data();
          hist.interp((double)te, yq);
          for (int ii = 0; ii < eff; ++ii) A[ii] = 0.0;
          for (int ii = 0; ii < nBase; ++ii) A[ii] = yq[ii];
          calc_lhs(cSub, (double)te, A, ind->lhs);
          double fac = evJumps[segEv[s]].factor, lc = NV_Ith_S(B, c);
          if (op->adjDrateOff >= 0 && fac != 0.0) {
            const double *drate = &ind->lhs[op->adjDrateOff];
            for (int p = 0; p < np; ++p) NV_Ith_S(B, nBase + p) += fac * drate[c * np + p] * lc;
          }
          if (op->adjDfOff >= 0 && evJumps[segEv[s]].raw != 0.0) {
            const double *dF = &ind->lhs[op->adjDfOff]; double raw = evJumps[segEv[s]].raw;
            for (int p = 0; p < np; ++p) NV_Ith_S(B, nBase + p) += raw * dF[c * np + p] * lc;
          }
        }
        if (CVodeReInit(bmem, (sunrealtype)te, B) < 0) { bok = 0; break; }
      }
      if (!bok) break;
      CVodeSetStopTime(bmem, (sunrealtype)tStop);
      if (CVode(bmem, (sunrealtype)tStop, B, &tret, CV_NORMAL) < 0) { bok = 0; break; }
      applyAt(tStop, evLo, evHi);   // the events at t0, or right after the reset
      // tStop is t0 or a reset; either way the state there is the initial condition
      for (int p = 0; p < np; ++p) {
        double ini = 0.0;
        if (dx0P) for (int q = 0; q < nBase; ++q) ini += NV_Ith_S(B, q) * dx0P[q * np + p];
        out[sensOff + k * np + p] = NV_Ith_S(B, nBase + p) + ini;
      }
    }
  }

  if (B) N_VDestroy(B);
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
    // a failed backward sweep leaves the subject's states and sensitivities
    // unusable: return NA, as a failed forward solve does
    int _i = 0; badSolveExit(_i); (void)_i;
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
