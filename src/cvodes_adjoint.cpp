// CVODES native adjoint-sensitivity analysis (ASA) driver.  *** WIP / NOT WIRED ***
//
// Step-1 DONE: this compiles + links inside par_solve.cpp (CVODES headers coexist;
// senswrapper resolved via PKG_CPPFLAGS -I).  RUNTIME TODO before enabling a live
// method: (1) apply dose/reset EVENTS in the forward -- currently only samples, so
// an undosed model integrates to 0; mirror rk4s ind_rk4s_0's handle_evid loop.
// (2) a segfault in the ASA path (CVodeAdjInit/CVodeF/backward) needs debugging.
// (3) FD-validate the quadrature sign; dose-parameter jumps.  The user-facing
// dispatch (par_solve.cpp case 221) + R registration + rxData check are reverted
// until these land, so no crashing method is exposed.
//
// #included into par_solve.cpp (guarded by IN_PAR_SOLVE, like rk4s.cpp) so it
// sees calc_lhs / iniSubject / getSolve / the dydt & update_inis globals.
// Compiles to an empty .o when built standalone.  Reuses the .rxAdjointExpand
// model: rx__adjFX_i_j__ (F_X = df/dy) and rx__adjFP_i_p__ (F_p = df/dp) lhs feed
// the backward costate RHS (dlam/dt = -F_X^T lam) and quadrature
// (dmu/dt = -F_p^T lam).  Forward = CVodeF (checkpointed) on the nBase base ODE;
// per (observation t_i, base state k) a backward problem is integrated from t_i
// to t_0 with lam(t_i)=e_k, and CVodeGetQuadB gives mu = dy_k(t_i)/dp, stored
// into the rx__sens_<k>_BY_<p>__ output slots.  Continuous adjoint (tolerance-
// controlled), using SUNDIALS' validated ASA.
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

// Solve one subject with CVODES ASA, filling the rx__sens_* output slots.
static int cvodesAdjSolveSubject(rx_solve *rx, rx_solving_options *op,
                                 rx_solving_options_ind *ind, int cSub,
                                 int *neq, t_dydt c_dydt) {
  int nBase = op->adjNbase, np = op->adjNp, eff = rxEffNeq(ind, op);
  int sensOff = op->adjSensOff;
  double atol = op->ATOL > 0 ? op->ATOL : 1e-8, rtol = op->RTOL > 0 ? op->RTOL : 1e-6;

  cvAdjUser u;
  u.ind = ind; u.op = op; u.cSub = cSub; u.neq = neq; u.dydt = c_dydt;
  u.nBase = nBase; u.np = np; u.eff = eff; u.fxOff = op->adjFxOff; u.fpOff = op->adjFpOff;
  u.A.assign(eff, 0.0); u.DADT.assign(eff, 0.0);

  SUNContext sunctx = NULL;
  if (SUNContext_Create(SUN_COMM_NULL, &sunctx) != 0) return 0;
  double t0 = getAllTimes(ind, 0);
  _growSolveIfNeeded(ind, op, 0, 1);
  double *y0 = getSolve(0);
  N_Vector y = N_VNew_Serial(nBase, sunctx);
  for (int i = 0; i < nBase; ++i) NV_Ith_S(y, i) = y0[i];
  void *mem = CVodeCreate(CV_BDF, sunctx);
  int ok = 1;
  if (!mem || CVodeInit(mem, cvAdj_fwd, (sunrealtype)t0, y) < 0 ||
      CVodeSStolerances(mem, rtol, atol) < 0 ||
      CVodeSetUserData(mem, (void *)&u) < 0) ok = 0;
  SUNMatrix Amat = ok ? SUNDenseMatrix(nBase, nBase, sunctx) : NULL;
  SUNLinearSolver LS = ok ? SUNLinSol_Dense(y, Amat, sunctx) : NULL;
  if (ok && (!Amat || !LS || CVodeSetLinearSolver(mem, LS, Amat) < 0)) ok = 0;
  if (ok) CVodeSetMaxNumSteps(mem, (long int)(op->mxstep > 0 ? op->mxstep : 50000));
  if (ok && CVodeAdjInit(mem, 200, CV_HERMITE) < 0) ok = 0;   // checkpointing

  // Forward with checkpointing: advance to each observation, storing base states.
  int nx = ind->n_all_times;
  std::vector<double> obsT; std::vector<int> obsIx;
  for (int i = 0; ok && i < nx; ++i) {
    int ix = ind->ix[i];
    if (!isObs(getEvid(ind, ix))) continue;
    double tout = getAllTimes(ind, ix);
    _growSolveIfNeeded(ind, op, i, 1);   // allocate this obs's solve buffer slot
    double *out = getSolve(i);
    if (tout > t0) {
      int ncheck; sunrealtype tret;
      if (CVodeF(mem, (sunrealtype)tout, y, &tret, CV_NORMAL, &ncheck) < 0) { ok = 0; break; }
    }
    for (int k = 0; k < nBase; ++k) out[k] = NV_Ith_S(y, k);
    obsT.push_back(tout); obsIx.push_back(i);
  }

  // Backward: one problem per (observation t_i, base state k).
  int which = -1;
  N_Vector yB = ok ? N_VNew_Serial(nBase, sunctx) : NULL;
  N_Vector qB = ok ? N_VNew_Serial(np > 0 ? np : 1, sunctx) : NULL;
  SUNMatrix AB = NULL; SUNLinearSolver LSB = NULL;
  bool bInit = false;
  for (int oi = (int)obsT.size() - 1; ok && oi >= 0; --oi) {
    double ti = obsT[oi]; int solveIdx = obsIx[oi];
    double *out = getSolve(solveIdx);
    if (ti <= t0) { for (int q = 0; q < nBase * np; ++q) out[sensOff + q] = 0.0; continue; }
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
      sunrealtype tret;
      if (CVodeB(mem, (sunrealtype)t0, CV_NORMAL) < 0) { ok = 0; break; }
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
  if (y) N_VDestroy(y);
  if (sunctx) SUNContext_Free(&sunctx);
  return ok;
}

extern "C" void ind_cvodesadj_0(rx_solve *rx, rx_solving_options *op, int solveid,
                                int *neq, t_dydt c_dydt, t_update_inis u_inis) {
  neq[1] = rx->ordId[solveid] - 1;
  rx_solving_options_ind *ind = &(rx->subjects[neq[1]]);
  neq[0] = rxEffNeq(ind, op);
  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) return;
  if (!cvodesAdjSolveSubject(rx, op, ind, neq[1], neq, c_dydt)) {
    if (ind->rc[0] == 0) ind->rc[0] = -2019;
    ind->err = 1;
  }
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
