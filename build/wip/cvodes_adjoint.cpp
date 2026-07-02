// CVODES native adjoint-sensitivity analysis (ASA) driver.
//
// #included into par_solve.cpp (like rk4s.cpp), so it sees calc_lhs / getSolve /
// the rx_solve structs.  Reuses the .rxAdjointExpand model: the rx__adjFX_i_j__
// (F_X = df/dy) and rx__adjFP_i_p__ (F_p = df/dp) lhs feed the backward RHS
// (dlam/dt = -F_X^T lam) and the quadrature (dmu/dt = -F_p^T lam).  The forward
// integrates the nBase base ODE with CVodeF (checkpointed); for each observation
// t_i and base state k a backward problem is integrated from t_i to t_0 with the
// terminal covector lam(t_i)=e_k, and CVodeGetQuadB gives mu = dy_k(t_i)/dp,
// which is stored into the rx__sens_<k>_BY_<p>__ output slots.  This is the
// CONTINUOUS adjoint (tolerance-controlled), using SUNDIALS' validated ASA.
//
// First-cut scope: single dosing at/*before* the first solved point plus
// sampling (the common PK case); the analytic dose-parameter jumps handled by the
// rk4s F-jump are a follow-up here.

// INTEGRATION NOTE: this file is designed to be #included into par_solve.cpp
// (like rk4s.cpp / cvode.cpp) so it sees iniSubject (a static-inline in
// par_solve.h that depends on par_solve.cpp-local symbols), calc_lhs, getSolve,
// getAllTimes and the dydt/update_inis globals.  It CANNOT be a standalone TU.
// The CVODES headers below then enter the par_solve.cpp TU -- validate that
// combination (senswrapper is already resolved via PKG_CPPFLAGS -I) before
// enabling.  Kept out of src/ (in build/) until that wiring + FD validation land.
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
  t_calc_lhs calclhs;
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
  u->calclhs(u->cSub, t, A, u->ind->lhs);
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
  u->calclhs(u->cSub, t, A, u->ind->lhs);
  const double *fp = &u->ind->lhs[u->fpOff];
  for (int p = 0; p < np; ++p) {
    double s = 0.0;
    for (int i = 0; i < n; ++i) s += fp[i * np + p] * NV_Ith_S(yB, i); // (F_p^T lam)_p
    NV_Ith_S(qBdot, p) = -s;
  }
  return 0;
}

// Solve one subject with CVODES ASA, filling the rx__sens_* output slots.
// Returns 1 on success, 0 on failure (caller falls back / flags error).
static int cvodesAdjSolveSubject(rx_solve *rx, rx_solving_options *op,
                                 rx_solving_options_ind *ind, int cSub,
                                 int *neq, t_dydt c_dydt, t_calc_lhs c_calclhs) {
  int nBase = op->adjNbase, np = op->adjNp, eff = rxEffNeq(ind, op);
  int sensOff = op->adjSensOff;
  double atol = op->ATOL > 0 ? op->ATOL : 1e-8, rtol = op->RTOL > 0 ? op->RTOL : 1e-6;

  cvAdjUser u;
  u.ind = ind; u.op = op; u.cSub = cSub; u.neq = neq; u.dydt = c_dydt; u.calclhs = c_calclhs;
  u.nBase = nBase; u.np = np; u.eff = eff; u.fxOff = op->adjFxOff; u.fpOff = op->adjFpOff;
  u.A.assign(eff, 0.0); u.DADT.assign(eff, 0.0);

  SUNContext sunctx = NULL;
  if (SUNContext_Create(0, &sunctx) != 0) return 0;
  double t0 = getAllTimes(ind, 0);
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
  // adjoint checkpointing (cubic-Hermite interpolation of the forward trajectory)
  if (ok && CVodeAdjInit(mem, 200, CV_HERMITE) < 0) ok = 0;

  // Forward pass with checkpointing: advance to each observation, storing the
  // base states (the primal output).  Assumes a monotone sampling timeline.
  int nx = ind->n_all_times;
  std::vector<double> obsT; std::vector<int> obsIx;
  for (int i = 0; ok && i < nx; ++i) {
    int ix = ind->ix[i];
    if (!isObs(getEvid(ind, ix))) continue;
    double tout = getAllTimes(ind, ix);
    double *out = getSolve(i);
    if (tout > t0) {
      int ncheck; sunrealtype tret;
      if (CVodeF(mem, (sunrealtype)tout, y, &tret, CV_NORMAL, &ncheck) < 0) { ok = 0; break; }
    }
    for (int k = 0; k < nBase; ++k) out[k] = NV_Ith_S(y, k);
    obsT.push_back(tout); obsIx.push_back(i);
  }

  // Backward: one problem per (observation t_i, base state k).  lam(t_i)=e_k,
  // qB(t_i)=0; integrate to t_0; CVodeGetQuadB -> mu = dy_k(t_i)/dp.
  int which = -1;
  N_Vector yB = ok ? N_VNew_Serial(nBase, sunctx) : NULL;
  N_Vector qB = ok ? N_VNew_Serial(np > 0 ? np : 1, sunctx) : NULL;
  SUNMatrix AB = NULL; SUNLinearSolver LSB = NULL;
  bool bInit = false;
  for (int oi = (int)obsT.size() - 1; ok && oi >= 0; --oi) {
    double ti = obsT[oi]; int solveIdx = obsIx[oi];
    if (ti <= t0) { double *out = getSolve(solveIdx); for (int q = 0; q < nBase * np; ++q) out[sensOff + q] = 0.0; continue; }
    double *out = getSolve(solveIdx);
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

// Entry point (separately compiled): the model's dydt + calc_lhs are passed in
// (calc_lhs is not exposed in a header, only inside par_solve.cpp).  The
// par_solve.cpp trampolines ind_cvodesadj/par_cvodesadj call this with the
// current model's function pointers.
extern "C" void ind_cvodesadj_0(rx_solve *rx, rx_solving_options *op, int solveid,
                                int *neq, t_dydt c_dydt, t_calc_lhs c_calclhs,
                                t_update_inis u_inis) {
  neq[1] = rx->ordId[solveid] - 1;
  rx_solving_options_ind *ind = &(rx->subjects[neq[1]]);
  neq[0] = rxEffNeq(ind, op);
  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) return;
  if (!cvodesAdjSolveSubject(rx, op, ind, neq[1], neq, c_dydt, c_calclhs)) {
    if (ind->rc[0] == 0) ind->rc[0] = -2019;
    ind->err = 1;
  }
}
