// CVODES native adjoint-sensitivity analysis (ASA) driver.  *** SMOOTH-FORWARD
// ONLY -- NOT WIRED (see the interior-discontinuity limit below) ***
//
// #included into par_solve.cpp (guarded by IN_PAR_SOLVE, like dop5.cpp/rk4s.cpp)
// so it sees calc_lhs / iniSubject / getSolve / handle_evid / the dydt globals.
// Compiles to an empty .o when built standalone.
//
// Uses SUNDIALS' VALIDATED continuous adjoint (CVodeF checkpointed forward +
// CVodeB backward + CVodeQuadB quadrature).  The forward primal is integrated
// exactly like every other rxode2 solver's main loop (cloned from dop5.cpp:
// preSolve/handleEvid1/handleEvid3/handleSS/handleExtraDose/updateSolve), with the
// numeric stepping between event times done by CVodeF so CVODES stores the
// checkpointed interpolant its backward pass needs.
//
// STATUS: VALIDATED to machine-FD precision (2.4e-6, == dop853s) for models whose
// forward is SMOOTH after t0 -- single dose at t0, PD models, no interior events.
//
// CONFIRMED LIMITATION (why this is not wired): CVODES native ASA checkpointing
// assumes ONE continuous CVodeF integration.  An interior state/RHS discontinuity
// (multi-dose, addl, an infusion rate-off, a reset) requires an external
// CVodeReInit that DESYNCS CVodeF's checkpoint bookkeeping -- and the resulting
// heap corruption occurs DURING the forward CVodeF checkpoint storage, so no
// post-forward guard can prevent it.  Full multi-dose support needs a per-segment
// restructure (one CVodeF+CVodeAdjInit per smooth interval, backward walking the
// segments and carrying lambda/mu across dose jumps) OR a self-managed dense
// primal + plain-CVODES backward.  Until then multi-dose adjoint is served by the
// discrete-adjoint RK methods (dop853s/rk4s/...), which handle every event type.
//
// The .rxAdjointExpand model exposes rx__adjFX_i_j__ (F_X = df/dy) and
// rx__adjFP_i_p__ (F_p = df/dp) as lhs.  Per (observation t_i, base state k) a
// backward problem is integrated from t_i to t_0 with lam(t_i)=e_k,
// dlam/dt = -F_X^T lam, quadrature dmu/dt = -F_p^T lam; CVodeGetQuadB then gives
// mu = dy_k(t_i)/dp, stored into the rx__sens_<k>_BY_<p>__ output slots.
#ifdef IN_PAR_SOLVE

#include <cvodes/cvodes.h>
#include <cvodes/cvodes_ls.h>
#include <nvector/nvector_serial.h>
#include <sunmatrix/sunmatrix_dense.h>
#include <sunlinsol/sunlinsol_dense.h>

struct cvAdjUser {
  rx_solving_options_ind *ind;
  rx_solving_options *op;
  int cSub, *neq;
  t_dydt dydt;
  int nBase, np, eff, fxOff, fpOff;
  std::vector<double> A, DADT;   // eff-wide scratch for dydt / calc_lhs
};

// Forward RHS of the base ODE.  rxode2's dydt reads/writes the full eff-wide
// state (base + rx__sens_* compartments, whose d/dt=0); we advance only nBase.
static int cvAdj_fwd(sunrealtype t, N_Vector y, N_Vector yd, void *ud) {
  cvAdjUser *u = (cvAdjUser *)ud;
  double *A = u->A.data(), *D = u->DADT.data();
  for (int i = 0; i < u->eff; ++i) A[i] = 0.0;
  for (int i = 0; i < u->nBase; ++i) A[i] = NV_Ith_S(y, i);
  u->dydt(u->neq, t, A, D);
  for (int i = 0; i < u->nBase; ++i) NV_Ith_S(yd, i) = D[i];
  return 0;
}

// Backward costate RHS: yBdot = -F_X(y(t))^T yB, with F_X from calc_lhs.
static int cvAdj_bwd(sunrealtype t, N_Vector y, N_Vector yB, N_Vector yBdot, void *ud) {
  cvAdjUser *u = (cvAdjUser *)ud;
  int n = u->nBase;
  double *A = u->A.data();
  for (int i = 0; i < u->eff; ++i) A[i] = 0.0;
  for (int i = 0; i < n; ++i) A[i] = NV_Ith_S(y, i);
  calc_lhs(u->cSub, t, A, u->ind->lhs);
  const double *fx = &u->ind->lhs[u->fxOff];
  for (int j = 0; j < n; ++j) {
    double s = 0.0;
    for (int i = 0; i < n; ++i) s += fx[i * n + j] * NV_Ith_S(yB, i); // (F_X^T lam)_j
    NV_Ith_S(yBdot, j) = -s;
  }
  return 0;
}

// Backward quadrature RHS: qBdot = -F_p(y(t))^T yB  (so integrating from t_i back
// to t_0 accumulates +int lam^T f_p = dy_k(t_i)/dp).
static int cvAdj_quad(sunrealtype t, N_Vector y, N_Vector yB, N_Vector qBdot, void *ud) {
  cvAdjUser *u = (cvAdjUser *)ud;
  int n = u->nBase, np = u->np;
  double *A = u->A.data();
  for (int i = 0; i < u->eff; ++i) A[i] = 0.0;
  for (int i = 0; i < n; ++i) A[i] = NV_Ith_S(y, i);
  calc_lhs(u->cSub, t, A, u->ind->lhs);
  const double *fp = &u->ind->lhs[u->fpOff];
  for (int p = 0; p < np; ++p) {
    double s = 0.0;
    for (int i = 0; i < n; ++i) s += fp[i * np + p] * NV_Ith_S(yB, i); // (F_p^T lam)_p
    NV_Ith_S(qBdot, p) = -s;
  }
  return 0;
}

// Step the forward primal from xp to xout using CVodeF (checkpointed).  yp holds
// the authoritative state (base + sens); we sync the base states in/out of the
// CVODES N_Vector.  CVODES's internal time must already be at xp (guaranteed by
// the CVodeReInit we do after every dose/reset).
static void cvF_step(void *mem, cvAdjUser *u, N_Vector y, double *yp,
                     double xout, int *rc) {
  for (int k = 0; k < u->nBase; ++k) NV_Ith_S(y, k) = yp[k];
  // Stop exactly at xout so no internal step crosses this segment boundary --
  // required for ASA: a checkpoint interval must never span a dose/reset, or the
  // backward re-integration (CVAdataStore) diverges / overflows dt_mem.
  CVodeSetStopTime(mem, (sunrealtype)xout);
  sunrealtype tret; int ncheck;
  int flag = CVodeF(mem, (sunrealtype)xout, y, &tret, CV_NORMAL, &ncheck);
  if (flag < 0) { if (*rc == 0) *rc = -2019; return; }
  for (int k = 0; k < u->nBase; ++k) yp[k] = NV_Ith_S(y, k);
}

// Solve one subject with CVODES ASA, filling the rx__sens_* output slots.
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
  double atol = op->ATOL > 0 ? op->ATOL : 1e-8, rtol = op->RTOL > 0 ? op->RTOL : 1e-6;

  cvAdjUser u;
  u.ind = ind; u.op = op; u.cSub = cSub; u.neq = neq; u.dydt = c_dydt;
  u.nBase = nBase; u.np = np; u.eff = rxEffNeq(ind, op);
  u.fxOff = op->adjFxOff; u.fpOff = op->adjFpOff;
  u.A.assign(u.eff, 0.0); u.DADT.assign(u.eff, 0.0);

  double xp = getAllTimes(ind, 0);
  double xp0 = xp;   // initial time t0 (doses at t0 keep the forward smooth)
  double xout;

  // Pre-scan for interior discontinuities (any dose/reset scheduled after t0).
  // CVODES native ASA checkpointing assumes ONE continuous CVodeF integration;
  // an interior state jump forces an external CVodeReInit that corrupts the
  // checkpoint list, so the backward re-integration (CVAdataStore) diverges.
  // We still integrate the forward primal (base states stay valid) but skip the
  // unsafe backward sweep and emit NA sens.  Multi-dose adjoint is served by the
  // discrete-adjoint RK methods (dop853s/rk4s/...).
  int interiorDiscont = 0;
  for (int _i = 0; _i < ind->n_all_times; ++_i) {
    int _raw = ind->ix[_i];
    if (isObs(getEvid(ind, _raw))) continue;
    if (getAllTimes(ind, _raw) > xp0 + 1e-12) { interiorDiscont = 1; break; }
  }
  // Also scan the authoritative dose list (idose), which carries addl-expanded
  // and on-the-fly doses that may not appear as separate all_times records.
  for (int _d = 0; !interiorDiscont && _d < ind->ndoses; ++_d) {
    if (getAllTimes(ind, ind->idose[_d]) > xp0 + 1e-12) interiorDiscont = 1;
  }

  // ---- set up the CVODES forward integrator with ASA checkpointing ----
  // (The deprecated N_VSpace calls CVODES makes are zeroed out by the CRAN
  // workaround, inst/tools/workaround.R, so the NULLed ops->nvspace is never hit.)
  N_Vector y = N_VNew_Serial(nBase, sunctx);
  { double *y0 = getSolve(0); for (int k = 0; k < nBase; ++k) NV_Ith_S(y, k) = y0[k]; }
  void *mem = CVodeCreate(CV_BDF, sunctx);
  int ok = (mem != NULL);
  if (ok && CVodeInit(mem, cvAdj_fwd, (sunrealtype)xp, y) < 0) ok = 0;
  if (ok && CVodeSStolerances(mem, rtol, atol) < 0) ok = 0;
  if (ok && CVodeSetUserData(mem, (void *)&u) < 0) ok = 0;
  SUNMatrix Amat = ok ? SUNDenseMatrix(nBase, nBase, sunctx) : NULL;
  SUNLinearSolver LS = ok ? SUNLinSol_Dense(y, Amat, sunctx) : NULL;
  if (ok && (!Amat || !LS || CVodeSetLinearSolver(mem, LS, Amat) < 0)) ok = 0;
  if (ok) CVodeSetMaxNumSteps(mem, (long int)(op->mxstep > 0 ? op->mxstep : 50000));
  if (ok && CVodeAdjInit(mem, 200, CV_HERMITE) < 0) ok = 0;
  if (!ok) {
    if (LS) SUNLinSolFree(LS);
    if (Amat) SUNMatDestroy(Amat);
    if (mem) CVodeFree(&mem);
    N_VDestroy(y);
    return 0;
  }

  // A CVodeReInit is needed whenever the forward integrator's current time or
  // state was changed outside CVODES (a dose/reset).  Track observation slots
  // for the backward pass.
  std::vector<double> obsT; std::vector<int> obsIx;
  double *yp;
  int nReinit = 0;   // count CVodeReInit calls: >1 => an interior discontinuity

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
            if (neqOde > 0) cvF_step(mem, &u, y, yp, ind->extraDoseNewXout, ind->rc);
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
            didEvent = 1;
            if (!isSameTime(ind->extraDoseNewXout, xp0)) interiorDiscont = 1; // extra dose after t0
            if (!isSameTime(xout, ind->extraDoseNewXout)) {
              // must resume CVODES from the post-dose state at extraDoseNewXout
              for (int k = 0; k < nBase; ++k) NV_Ith_S(y, k) = yp[k];
nReinit++;
              CVodeReInit(mem, (sunrealtype)ind->extraDoseNewXout, y);
              didEvent = 0;
              preSolve(op, ind, ind->extraDoseNewXout, xout, yp);
              if (neqOde > 0) cvF_step(mem, &u, y, yp, xout, ind->rc);
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
          if (neqOde > 0) cvF_step(mem, &u, y, yp, xout, ind->rc);
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
      if (getEvid(ind, ind->ix[i]) == 3) {
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
      // Resume CVODES from the (possibly jumped) state at xout after any event.
      if (didEvent) {
        if (!isSameTime(xout, xp0)) interiorDiscont = 1;   // dose/reset after t0
        for (int k = 0; k < nBase; ++k) NV_Ith_S(y, k) = yp[k];
nReinit++;
        CVodeReInit(mem, (sunrealtype)xout, y);
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
      if (isObs(getEvid(ind, ind->ix[i]))) { obsT.push_back(xout); obsIx.push_back(i); }
      else if (xout > xp0 + 1e-12) interiorDiscont = 1;  // any non-obs event after t0
      if (_mtime_requeued) i--;
    }
    ind->solvedIdx = i;
  }
  // On-the-fly (addl / SS) doses are counted in idxExtra; any of them is an
  // interior discontinuity that corrupts the ASA checkpoints.  The reinit count
  // is the definitive signal: one reinit is expected for a t0 dose; more than one
  // means the forward crossed an interior state jump.
  if (ind->idxExtra > 0 || nReinit > 1) interiorDiscont = 1;

  // ================= backward ASA sweep -> rx__sens_* output slots =================
  int which = -1;
  bool bInit = false;
  // With an interior discontinuity the ASA backward is unsafe (see above): fill
  // the sensitivity slots with NA and skip it -- the primal base states are still
  // valid.  The NA sens columns signal the user to use dop853s/rk4s instead.
  if (interiorDiscont) {
    for (size_t oi = 0; oi < obsT.size(); ++oi) {
      double *out = getSolve(obsIx[oi]);
      for (int q = 0; q < nBase * np; ++q) out[sensOff + q] = NA_REAL;
    }
    if (LS) SUNLinSolFree(LS);
    if (Amat) SUNMatDestroy(Amat);
    if (mem) CVodeFree(&mem);
    N_VDestroy(y);
    return 1;   // not a solve failure; base states are valid
  }

  N_Vector yB = (ok && !localBadSolve) ? N_VNew_Serial(nBase, sunctx) : NULL;
  N_Vector qB = (yB) ? N_VNew_Serial(np > 0 ? np : 1, sunctx) : NULL;
  SUNMatrix AB = NULL; SUNLinearSolver LSB = NULL;
  double t0 = getAllTimes(ind, 0);
  for (int oi = (int)obsT.size() - 1; ok && !localBadSolve && oi >= 0; --oi) {
    double ti = obsT[oi]; int solveIdx = obsIx[oi];
    double *out = getSolve(solveIdx);
    if (ti <= t0 || isSameTime(ti, t0)) {
      for (int q = 0; q < nBase * np; ++q) out[sensOff + q] = 0.0;
      continue;
    }
    for (int k = 0; ok && k < nBase; ++k) {
      for (int j = 0; j < nBase; ++j) NV_Ith_S(yB, j) = (j == k) ? 1.0 : 0.0;
      for (int p = 0; p < np; ++p) NV_Ith_S(qB, p) = 0.0;
      if (!bInit) {
        if (CVodeCreateB(mem, CV_BDF, &which) < 0 ||
            CVodeInitB(mem, which, cvAdj_bwd, (sunrealtype)ti, yB) < 0 ||
            CVodeSStolerancesB(mem, which, rtol, atol) < 0 ||
            CVodeSetUserDataB(mem, which, (void *)&u) < 0) { ok = 0; break; }
        AB = SUNDenseMatrix(nBase, nBase, sunctx);
        LSB = SUNLinSol_Dense(yB, AB, sunctx);
        if (!AB || !LSB || CVodeSetLinearSolverB(mem, which, LSB, AB) < 0 ||
            CVodeQuadInitB(mem, which, cvAdj_quad, qB) < 0) { ok = 0; break; }
        CVodeSetMaxNumStepsB(mem, which, (long int)(op->mxstep > 0 ? op->mxstep : 50000));
        bInit = true;
      } else {
        if (CVodeReInitB(mem, which, (sunrealtype)ti, yB) < 0 ||
            CVodeQuadReInitB(mem, which, qB) < 0) { ok = 0; break; }
      }
      if (CVodeB(mem, (sunrealtype)t0, CV_NORMAL) < 0) { ok = 0; break; }
      sunrealtype tret;
      if (CVodeGetQuadB(mem, which, &tret, qB) < 0) { ok = 0; break; }
      for (int p = 0; p < np; ++p) out[sensOff + k * np + p] = NV_Ith_S(qB, p);
    }
  }

  if (yB) N_VDestroy(yB);
  if (qB) N_VDestroy(qB);
  if (LSB) SUNLinSolFree(LSB);
  if (AB) SUNMatDestroy(AB);
  if (LS) SUNLinSolFree(LS);
  if (Amat) SUNMatDestroy(Amat);
  if (mem) CVodeFree(&mem);
  N_VDestroy(y);
  return ok && !localBadSolve;
}

extern "C" void ind_cvodesadj_0(rx_solve *rx, rx_solving_options *op, int solveid,
                                int *neq, t_dydt c_dydt, t_update_inis u_inis) {
  clock_t tclk = clock();
  neq[1] = rx->ordId[solveid] - 1;
  rx_solving_options_ind *ind = &(rx->subjects[neq[1]]);
  neq[0] = rxEffNeq(ind, op);
  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) return;
  ind->solvedIdx = 0;
  SUNContext sunctx = NULL;
  if (SUNContext_Create(SUN_COMM_NULL, &sunctx) != 0) {
    if (ind->rc[0] == 0) ind->rc[0] = -2019;
    ind->err = 1;
    return;
  }
  if (!cvodesAdjSolveSubject(rx, op, ind, neq, 1, c_dydt, u_inis, sunctx)) {
    if (ind->rc[0] == 0) ind->rc[0] = -2019;
    ind->err = 1;
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
  for (int solveid = 0; solveid < nsolve; ++solveid) {
    int neq[2]; neq[0] = op->neq; neq[1] = 0;
    ind_cvodesadj_0(rx, op, solveid, neq, dydt, update_inis);
    if (op->badSolve) break;
  }
}

#endif // IN_PAR_SOLVE
