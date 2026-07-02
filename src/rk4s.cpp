#ifdef IN_PAR_SOLVE
#undef min
#undef max
#include "odeinter.h"
#include <vector>
#include <cstring>

// ===========================================================================
// rk4s and the fixed-step explicit-RK discrete-adjoint family.
//
// SEPARATE ode methods (rk4 etc. stay byte-identical).  Each is a fixed-step
// explicit Runge-Kutta method selected by a Butcher tableau (rksGetTableau):
// rk4s (206), eulers (239), midpoints (240), heuns (241).  The forward pass
// RECORDS each step's realized dt + its stage states, so the backward
// reverse-mode sweep is the EXACT (table-driven) transpose of the actual
// numerical map -- a machine-precision-consistent gradient, mirroring
// R/adjointDiscrete.R's .rxDiscreteAdjointGrad.
//
// The model is built by R's .rxAdjointExpand: base ODE states, then
// rx__sens_<state>_BY_<param>__ OUTPUT-storage compartments (d/dt=0), plus the
// state Jacobian F_X (rx__adjFX_i_j__) and parameter forcing F_p
// (rx__adjFP_i_p__) exposed as lhs.  op carries the layout (op->adjNbase, adjNp,
// adjFxOff, adjFpOff, adjSensOff), set by rxData.cpp for these method codes.
//
// Forward: step ONLY the nBase base states (the rx__sens_* slots stay 0 during
// the forward pass).  Backward: for each observation time and each base state k
// run an independent reset sweep with terminal covector e_k; the resulting
// mu (length np) = dy_k(t_obs)/dtheta is written into that obs row's
// rx__sens_* solve slots.  rk4s is the GRADIENT method (full-trajectory
// sensitivities); if no gradient is needed use plain rk4.  A scalar objective
// gradient is a downstream REDUCTION of these columns, not a mode of the solver.
// ===========================================================================

// An additive bolus recorded during the forward pass: which 0-based RK4 step
// its (F-scaled) state jump precedes (step = cumulative steps at dose time),
// the base-state compartment, and the RAW amt (bioavailability F is applied to
// the state in handle_evid; the adjoint needs mu += amt*dF/dtheta*lambda[cmt]).
struct rk4s_dose { size_t step; int cmt; double amt; };

// Butcher tableau for an explicit RK method (the discrete-adjoint transpose is
// table-driven, so adding a fixed-step OR adaptive explicit method = adding a
// tableau).  A is s x s row-major, lower-triangular (A[i*s+j], j<i).  For
// adaptive methods bhat is the embedded (error-estimate) weight row and errOrder
// is the lower order of the embedded pair; bhat/errOrder are used ONLY by the
// forward step controller -- the backward transpose uses b (and A/c) alone.
// Sized for s<=16 (dop853 has 12 stages).
struct rksTableau { int s; int adaptive; int errOrder;
                    double c[16]; double b[16]; double bhat[16]; double A[256]; };

static void rksTableauRk4(rksTableau &T) {
  T.s = 4;
  T.c[0] = 0; T.c[1] = 0.5; T.c[2] = 0.5; T.c[3] = 1.0;
  T.b[0] = 1.0/6; T.b[1] = 1.0/3; T.b[2] = 1.0/3; T.b[3] = 1.0/6;
  T.A[1*4+0] = 0.5; T.A[2*4+1] = 0.5; T.A[3*4+2] = 1.0;
}

// Dormand-Prince RK5(4), 7 stages (the ode45 / rxode2 "dop5" tableau).
static void rksTableauDop5(rksTableau &T) {
  const int s = 7; T.s = s; T.adaptive = 1; T.errOrder = 4;
  T.c[0]=0; T.c[1]=1.0/5; T.c[2]=3.0/10; T.c[3]=4.0/5; T.c[4]=8.0/9; T.c[5]=1.0; T.c[6]=1.0;
  T.A[1*s+0]=1.0/5;
  T.A[2*s+0]=3.0/40;      T.A[2*s+1]=9.0/40;
  T.A[3*s+0]=44.0/45;     T.A[3*s+1]=-56.0/15;    T.A[3*s+2]=32.0/9;
  T.A[4*s+0]=19372.0/6561;T.A[4*s+1]=-25360.0/2187;T.A[4*s+2]=64448.0/6561; T.A[4*s+3]=-212.0/729;
  T.A[5*s+0]=9017.0/3168; T.A[5*s+1]=-355.0/33;   T.A[5*s+2]=46732.0/5247; T.A[5*s+3]=49.0/176;    T.A[5*s+4]=-5103.0/18656;
  T.A[6*s+0]=35.0/384;    T.A[6*s+1]=0;           T.A[6*s+2]=500.0/1113;   T.A[6*s+3]=125.0/192;   T.A[6*s+4]=-2187.0/6784;  T.A[6*s+5]=11.0/84;
  T.b[0]=35.0/384; T.b[1]=0; T.b[2]=500.0/1113; T.b[3]=125.0/192; T.b[4]=-2187.0/6784; T.b[5]=11.0/84; T.b[6]=0;
  T.bhat[0]=5179.0/57600; T.bhat[1]=0; T.bhat[2]=7571.0/16695; T.bhat[3]=393.0/640;
  T.bhat[4]=-92097.0/339200; T.bhat[5]=187.0/2100; T.bhat[6]=1.0/40;
}

static rksTableau rksGetTableau(int method) {
  rksTableau T; std::memset(&T, 0, sizeof(T));
  switch (method) {
  case 206: rksTableauRk4(T); break;   // rk4s -- classical RK4
  case 239:                            // eulers -- forward Euler
    T.s = 1; T.c[0] = 0; T.b[0] = 1.0; break;
  case 240:                            // midpoints -- explicit midpoint (RK2)
    T.s = 2; T.c[0] = 0; T.c[1] = 0.5; T.b[0] = 0; T.b[1] = 1.0; T.A[1*2+0] = 0.5; break;
  case 241:                            // heuns -- Heun / explicit trapezoid (RK2)
    T.s = 2; T.c[0] = 0; T.c[1] = 1.0; T.b[0] = 0.5; T.b[1] = 0.5; T.A[1*2+0] = 1.0; break;
  case 210: rksTableauDop5(T); break;  // dop5s -- adaptive Dormand-Prince 5(4)
  default:  rksTableauRk4(T); break;
  }
  return T;
}

struct rk4s_rec {
  int s = 0;                 // stages per step
  int nq = 0;                // base states recorded per stage
  std::vector<double> h;     // realized dt per step
  std::vector<double> t0;    // step start time per step
  std::vector<double> a;     // stage states, flattened (nStep * s * nq); stage i
                             // of step n at &a[(n*s + i)*nq]
  void clear() { h.clear(); t0.clear(); a.clear(); }
  size_t nStep() const { return h.size(); }
};

// One explicit-RK step over [t, t+dt] for the tableau `T`.  dydt READS/WRITES
// the FULL state (all `nAll` = neqOde entries), so the stage/derivative buffers
// MUST be nAll wide; we only RECORD the first `nRec` (base) states of each stage.
// In adjoint mode the rx__sens_* compartments have d/dt=0, so advancing them is
// a no-op that keeps them at 0 -- exactly what the backward sweep expects.
// scratch layout: k[0..s-1] (each nAll) then atmp (nAll) -> (s+1)*nAll.
static inline void rk4s_step_record(t_dydt dydt, int *neq, const rksTableau &T,
                                    int nAll, int nRec, double t, double dt, double *y,
                                    double *scratch, rk4s_rec *rec) {
  int s = T.s;
  double *atmp = scratch + s * nAll;
  for (int i = 0; i < s; ++i) {
    // a_i = y + dt * sum_{j<i} A[i][j] * k_j
    for (int m = 0; m < nAll; ++m) atmp[m] = y[m];
    for (int j = 0; j < i; ++j) {
      double aij = T.A[i * s + j];
      if (aij != 0.0) { const double *kj = scratch + j * nAll; for (int m = 0; m < nAll; ++m) atmp[m] += dt * aij * kj[m]; }
    }
    if (rec) rec->a.insert(rec->a.end(), atmp, atmp + nRec);
    dydt(neq, t + T.c[i] * dt, atmp, scratch + i * nAll);
  }
  // y_next = y + dt * sum_i b_i k_i
  for (int i = 0; i < s; ++i) {
    double bi = T.b[i];
    if (bi != 0.0) { const double *ki = scratch + i * nAll; for (int m = 0; m < nAll; ++m) y[m] += dt * bi * ki[m]; }
  }
  if (rec) { rec->t0.push_back(t); rec->h.push_back(dt); }
}

// Stage-recording analogue of rk4_do_steps (src/rk4.cpp): same step-size logic;
// advances all `nAll` states, records the first `nRec` (base) states per stage.
static inline void rk4s_do_steps(rx_solving_options_ind *ind, rx_solving_options *op,
                                 t_dydt dydt, int *neq, const rksTableau &T,
                                 int nAll, int nRec, double *yp,
                                 double xp, double xout, rk4s_rec *rec,
                                 std::vector<double> &scratch) {
  double t = xp;
  double dt = op->HMIN > 0.0 ? op->HMIN : 0.01;
  if (dt <= 0.0) dt = 0.01;
  if (fabs(xout - xp) / dt >= op->mxstep) {
    dt = fabs(xout - xp) / (double)(op->mxstep - 10);
  }
  int sign = (xout > xp) ? 1 : -1;
  dt = sign * dt;

  // dydt writes nAll derivatives into each k buffer -> (s k-buffers + atmp).
  if ((int)scratch.size() < (T.s + 1) * nAll) scratch.resize((T.s + 1) * nAll);

  error_checker check(ind, ind->rc, op->mxstep);
  zero_copy_state chk(yp, nAll);

  while ((sign > 0 && t < xout) || (sign < 0 && t > xout)) {
    double current_dt = dt;
    if ((sign > 0 && t + dt > xout) || (sign < 0 && t + dt < xout)) {
      current_dt = xout - t;
    }
    try {
      rk4s_step_record(dydt, neq, T, nAll, nRec, t, current_dt, yp, scratch.data(), rec);
    } catch (const std::exception &e) {
      if (ind->rc[0] == 0) ind->rc[0] = -2019;
      ind->err = 1;
      break;
    }
    t += current_dt;
    check(chk, t);
    if (ind->err != 0) break;
  }
}

// One trial step of an adaptive embedded pair: compute all stages a_i (into
// `stages`) and k_i (into `kbuf`), the high-order (b) and low-order (bhat)
// solutions, and return the scaled error norm.  Does NOT advance y.
static inline double rks_try_step(t_dydt dydt, int *neq, const rksTableau &T, int nAll,
                                  double t, double dt, const double *y, double *stages,
                                  double *kbuf, double *yhigh, double *ylow,
                                  double atol, double rtol) {
  int s = T.s;
  for (int i = 0; i < s; ++i) {
    double *ai = stages + i * nAll;
    for (int m = 0; m < nAll; ++m) ai[m] = y[m];
    for (int j = 0; j < i; ++j) {
      double aij = T.A[i * s + j];
      if (aij != 0.0) { const double *kj = kbuf + j * nAll; for (int m = 0; m < nAll; ++m) ai[m] += dt * aij * kj[m]; }
    }
    dydt(neq, t + T.c[i] * dt, ai, kbuf + i * nAll);
  }
  for (int m = 0; m < nAll; ++m) { yhigh[m] = y[m]; ylow[m] = y[m]; }
  for (int i = 0; i < s; ++i) {
    double bi = T.b[i], bh = T.bhat[i]; const double *ki = kbuf + i * nAll;
    for (int m = 0; m < nAll; ++m) { yhigh[m] += dt * bi * ki[m]; ylow[m] += dt * bh * ki[m]; }
  }
  double err = 0.0;
  for (int m = 0; m < nAll; ++m) {
    double sc = atol + rtol * fmax(fabs(y[m]), fabs(yhigh[m]));
    double e = (yhigh[m] - ylow[m]) / sc; err += e * e;
  }
  return sqrt(err / (nAll > 0 ? nAll : 1));
}

// Adaptive embedded-RK driver: standard error-controlled step selection.  The
// realized (accepted) step sequence is RECORDED (h + stages) and FROZEN -- the
// discrete adjoint transposes that fixed sequence (step-size control is not
// differentiated), reusing the same table-driven backward transpose.
static inline void rks_do_steps_adaptive(rx_solving_options_ind *ind, rx_solving_options *op,
                                         t_dydt dydt, int *neq, const rksTableau &T,
                                         int nAll, int nRec, double *yp,
                                         double xp, double xout, rk4s_rec *rec,
                                         std::vector<double> &scratch) {
  int s = T.s;
  double atol = op->ATOL > 0 ? op->ATOL : 1e-8, rtol = op->RTOL > 0 ? op->RTOL : 1e-6;
  double t = xp; int sign = (xout > xp) ? 1 : -1;
  double span = fabs(xout - xp); if (span == 0.0) return;
  double dt = op->H0 > 0 ? op->H0 : span / 100.0; if (dt <= 0.0) dt = span / 100.0;
  dt = sign * dt;
  if ((int)scratch.size() < (2 * s + 2) * nAll) scratch.resize((2 * s + 2) * nAll);
  double *stages = scratch.data(), *kbuf = stages + s * nAll,
         *yhigh = kbuf + s * nAll, *ylow = yhigh + nAll;
  error_checker check(ind, ind->rc, op->mxstep);
  zero_copy_state chk(yp, nAll);
  const double SAFE = 0.9, FACMIN = 0.2, FACMAX = 5.0;
  int expo = T.errOrder + 1, nrej = 0;
  while ((sign > 0 && t < xout) || (sign < 0 && t > xout)) {
    if ((sign > 0 && t + dt > xout) || (sign < 0 && t + dt < xout)) dt = xout - t;
    double err;
    try { err = rks_try_step(dydt, neq, T, nAll, t, dt, yp, stages, kbuf, yhigh, ylow, atol, rtol); }
    catch (const std::exception &e) { if (ind->rc[0] == 0) ind->rc[0] = -2019; ind->err = 1; break; }
    double fac = (err > 0.0) ? SAFE * pow(err, -1.0 / expo) : FACMAX;
    if (fac < FACMIN) fac = FACMIN; if (fac > FACMAX) fac = FACMAX;
    if (err <= 1.0 || fabs(dt) <= 1e-13 * span) {
      for (int i = 0; i < s; ++i) rec->a.insert(rec->a.end(), stages + i * nAll, stages + i * nAll + nRec);
      rec->t0.push_back(t); rec->h.push_back(dt);
      for (int m = 0; m < nAll; ++m) yp[m] = yhigh[m];
      t += dt; check(chk, t); if (ind->err != 0) break;
      dt *= fac; nrej = 0;
    } else {
      dt *= fac;
      if (++nrej > 50) { ind->err = 1; if (ind->rc[0] == 0) ind->rc[0] = -2019; break; }
    }
  }
}

// Dispatch a solve interval to the fixed-step or adaptive driver by the tableau.
static inline void rks_step_interval(rx_solving_options_ind *ind, rx_solving_options *op,
                                     t_dydt dydt, int *neq, const rksTableau &T,
                                     int nAll, int nRec, double *yp, double xp, double xout,
                                     rk4s_rec *rec, std::vector<double> &scratch) {
  if (T.adaptive) rks_do_steps_adaptive(ind, op, dydt, neq, T, nAll, nRec, yp, xp, xout, rec, scratch);
  else            rk4s_do_steps(ind, op, dydt, neq, T, nAll, nRec, yp, xp, xout, rec, scratch);
}

// Evaluate F_X (nBase x nBase) and F_p (nBase x np) at base-state vector `a` and
// time `t` via calc_lhs, into fx[nBase*nBase] (row-major i*nBase+j) and
// fp[nBase*np] (i*np+p).  Ascratch is a full-length (eff) state buffer.
static inline void rk4s_eval_jac(int cSub, double t, const double *a, int nBase, int np,
                                 int fxOff, int fpOff, double *Ascratch, int eff,
                                 rx_solving_options_ind *ind, double *fx, double *fp) {
  for (int i = 0; i < eff; ++i) Ascratch[i] = 0.0;
  for (int i = 0; i < nBase; ++i) Ascratch[i] = a[i];
  calc_lhs(cSub, t, Ascratch, ind->lhs);
  for (int i = 0; i < nBase * nBase; ++i) fx[i] = ind->lhs[fxOff + i];
  for (int i = 0; i < nBase * np; ++i)  fp[i] = ind->lhs[fpOff + i];
}

// The shared explicit-RK backward transpose (Phase 1 DRY seed): fills the
// rx__sens_* output slots for every observation.  Precomputes each step's 4
// stage Jacobians ONCE (calc_lhs), then runs an independent reset sweep per
// (observation, base-state) pair.
static void rk4s_backward_fill(rx_solve *rx, rx_solving_options *op, rx_solving_options_ind *ind,
                               int cSub, rk4s_rec &rec, const rksTableau &T, int nBase, int np,
                               int fxOff, int fpOff, int dfOff, int sensOff,
                               const std::vector<size_t> &boundary,
                               const std::vector<rk4s_dose> &doses) {
  size_t nStep = rec.nStep();
  if (nStep == 0) return;
  int eff = rxEffNeq(ind, op);
  int nfx = nBase * nBase, nfp = nBase * np;
  int sN = T.s;

  // Precompute each step's per-stage Jacobians: FXs[i] holds stage i for all
  // steps (n*nfx + ...), FPs[i] likewise.  Stage i of step n is at
  // &rec.a[(n*sN + i)*nBase], time t0 + c_i*h.
  std::vector<std::vector<double> > FXs(sN), FPs(sN);
  for (int i = 0; i < sN; ++i) { FXs[i].resize(nStep * nfx); FPs[i].resize(nStep * nfp); }
  std::vector<double> Ascratch(eff, 0.0);
  for (size_t n = 0; n < nStep; ++n) {
    double h = rec.h[n], t0 = rec.t0[n];
    for (int i = 0; i < sN; ++i)
      rk4s_eval_jac(cSub, t0 + T.c[i] * h, &rec.a[(n * sN + i) * nBase], nBase, np,
                    fxOff, fpOff, Ascratch.data(), eff, ind,
                    &FXs[i][n * nfx], &FPs[i][n * nfp]);
  }

  // dF/dtheta block (param-only) read once; used for the additive-bolus
  // dose-parameter jump transpose.  dFdp[c*np + p] = dF_c/dtheta_p.
  std::vector<double> dFdp;
  bool haveDose = (dfOff >= 0) && !doses.empty();
  if (haveDose) {
    dFdp.resize(nBase * np);
    calc_lhs(cSub, rec.t0.empty() ? 0.0 : rec.t0[0], Ascratch.data(), ind->lhs);
    for (int i = 0; i < nBase * np; ++i) dFdp[i] = ind->lhs[dfOff + i];
  }

  std::vector<double> lam(nBase), Ybar(nBase), abar(nBase), kbar(sN * nBase), mu(np);

  // The shared table-driven explicit-RK reverse-mode transpose (DRY): reverse of
  // one forward step under tableau T.  Given the incoming costate lamR
  // (= adjoint of y_{n+1}), produces the outgoing costate (= adjoint of y_n) and
  // accumulates the quadrature muR; plus the additive-bolus dose-parameter jump.
  // Init kbar_i = h b_i lam; then for i = s-1..0: abar = FX_i^T kbar_i,
  // muR += FP_i^T kbar_i, Ybar += abar, kbar_j += h A[i][j] abar (j<i).
  auto stepTranspose = [&](size_t n, std::vector<double> &lamR, std::vector<double> &muR) {
    double h = rec.h[n];
    for (int j = 0; j < nBase; ++j) Ybar[j] = lamR[j];
    for (int i = 0; i < sN; ++i)
      for (int j = 0; j < nBase; ++j) kbar[i * nBase + j] = h * T.b[i] * lamR[j];
    for (int i = sN - 1; i >= 0; --i) {
      const double *fx = &FXs[i][n * nfx], *fp = &FPs[i][n * nfp];
      const double *kb = &kbar[i * nBase];
      for (int j = 0; j < nBase; ++j) { double s = 0; for (int ii = 0; ii < nBase; ++ii) s += fx[ii * nBase + j] * kb[ii]; abar[j] = s; }
      for (int p = 0; p < np; ++p) { double s = 0; for (int ii = 0; ii < nBase; ++ii) s += fp[ii * np + p] * kb[ii]; muR[p] += s; }
      for (int j = 0; j < nBase; ++j) Ybar[j] += abar[j];
      for (int jstage = 0; jstage < i; ++jstage) {
        double aij = T.A[i * sN + jstage];
        if (aij != 0.0) { double *kbj = &kbar[jstage * nBase]; for (int j = 0; j < nBase; ++j) kbj[j] += h * aij * abar[j]; }
      }
    }
    for (int j = 0; j < nBase; ++j) lamR[j] = Ybar[j];   // additive bolus: dD/dX = I
    if (haveDose) {
      for (size_t di = 0; di < doses.size(); ++di) {
        if (doses[di].step == n) {
          int c = doses[di].cmt; double a = doses[di].amt;
          for (int p = 0; p < np; ++p) muR[p] += a * dFdp[c * np + p] * lamR[c];
        }
      }
    }
  };

  // Full-trajectory: for each observation and each base state k, an independent
  // reset sweep boundary[i]->0 with terminal covector e_k.
  for (int i = 0; i < ind->n_all_times; ++i) {
    if (!isObs(getEvid(ind, ind->ix[i]))) continue;
    size_t fromStep = boundary[i];
    double *out = getSolve(i);
    for (int k = 0; k < nBase; ++k) {
      for (int j = 0; j < nBase; ++j) lam[j] = (j == k) ? 1.0 : 0.0;
      for (int p = 0; p < np; ++p) mu[p] = 0.0;
      for (size_t nn = fromStep; nn >= 1; --nn) stepTranspose(nn - 1, lam, mu);
      for (int p = 0; p < np; ++p) out[sensOff + k * np + p] = mu[p];
    }
  }
}

extern "C" void ind_rk4s_0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq,
                           t_dydt c_dydt, t_update_inis u_inis) {
  clock_t t0 = clock();
  int i;
  int istate = 1;
  void* ctx = NULL;

  neq[1] = rx->ordId[solveid]-1;
  rx_solving_options_ind *ind = &(rx->subjects[neq[1]]);
  int eff = rxEffNeq(ind, op);
  neq[0] = eff;

  double xout;
  int localBadSolve = 0;

  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) return;

  double xp = getAllTimes(ind, 0);
  ind->solvedIdx = 0;

  int neqOde = op->neq - op->numLin - op->numLinSens;
  // dydt reads/writes the FULL neqOde state, so we step all of it (the adjoint
  // rx__sens_* compartments have d/dt=0 -> they stay 0); we RECORD only the
  // nBase base-state stages the backward sweep needs.
  int adj = op->adjoint;
  int nAll = neqOde;                          // full state advanced each step
  int nRec = adj ? op->adjNbase : neqOde;     // base states recorded per stage
  double *yp;

  rksTableau T = rksGetTableau(op->stiff);    // explicit-RK tableau for this method
  rk4s_rec rec;
  rec.s = T.s;
  rec.nq = nRec;
  std::vector<double> scratch;
  std::vector<size_t> boundary(ind->n_all_times, 0);  // cumulative steps at each stored time
  std::vector<rk4s_dose> doseRec;                     // additive boluses for the F-jump transpose

  for(i = 0; i < ind->n_all_times; i++) {
    ind->idx=i;
    ind->linSS=0;
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
    yp   = getSolve(i);
    xout = ind->timeThread[ind->ix[i]];

    if (getEvid(ind, ind->ix[i]) != 3 && !isSameTime(xout, xp)) {
      if (ind->err){
        ind->rc[0] = -1000;
        badSolveExit(i);
        localBadSolve = 1;
      } else {
        if (handleExtraDose(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                            xp, ind->id, &i, ind->n_all_times, &istate, op, ind, u_inis, ctx)) {
            if (!localBadSolve && !isSameTime(ind->extraDoseNewXout, xp)) {
              preSolve(op, ind, xp, ind->extraDoseNewXout, yp);
              if (nAll > 0) {
                  rks_step_interval(ind, op, c_dydt, neq, T, nAll, nRec, yp, xp, ind->extraDoseNewXout, &rec, scratch);
              }
              copyLinCmt(neq, ind, op, yp);
              const char* err_msg = "rk4s failed";
              postSolve(neq, &istate, ind->rc, &i, yp, &err_msg, 7, true, ind, op, rx);
              if (*(ind->rc) < 0) localBadSolve = 1;
              xp = ind->extraDoseNewXout;
            }
            if (!localBadSolve) {
              int idx = ind->idx;
              int ixds = ind->ixds;
              int trueIdx = ind->extraDoseTimeIdx[ind->idxExtra];
              ind->idx = -1-trueIdx;
              handle_evid(ind->extraDoseEvid[trueIdx], neq[0],
                          ind->BadDose, ind->InfusionRate, ind->dose, yp, xout, neq[1], ind);
              istate = 1;
              ind->ixds = ixds;
              ind->idx = idx;
              ind->idxExtra++;
              if (!isSameTime(xout, ind->extraDoseNewXout)) {
                preSolve(op, ind, ind->extraDoseNewXout, xout, yp);
                if (nAll > 0) {
                    rks_step_interval(ind, op, c_dydt, neq, T, nAll, nRec, yp, ind->extraDoseNewXout, xout, &rec, scratch);
                }
                copyLinCmt(neq, ind, op, yp);
                const char* err_msg = "rk4s failed";
                postSolve(neq, &istate, ind->rc, &idx, yp, &err_msg, 9, false, ind, op, rx);
                if (*(ind->rc) < 0) localBadSolve = 1;
                ind->extraDoseNewXout = xout;
              }
              xp = ind->extraDoseNewXout;
            }
        }
        if (!localBadSolve && !isSameTime(xout, xp)) {
          preSolve(op, ind, xp, xout, yp);
          if (nAll > 0) {
              rks_step_interval(ind, op, c_dydt, neq, T, nAll, nRec, yp, xp, xout, &rec, scratch);
          }
          copyLinCmt(neq, ind, op, yp);
          const char* err_msg = "rk4s failed";
          postSolve(neq, &istate, ind->rc, &i, yp, &err_msg, 7, true, ind, op, rx);
          if (*(ind->rc) < 0) localBadSolve = 1;
        }
        xp = xout;
      }
    }
    ind->_newind = 2;
    if (!localBadSolve){
      ind->idx = i;
      // Record additive boluses for the adjoint F-jump: raw amt + 0-based cmt,
      // tagged with the cumulative step count (the F-scaled state jump precedes
      // the next RK4 step, whose 0-based index == rec.nStep() at this point).
      if (adj && op->adjDfOff >= 0) {
        int _evCur = getEvid(ind, ind->ix[i]);
        if (isDose(_evCur)) {
          int _wh, _cmtD, _wh100, _whI, _wh0;
          getWh(_evCur, &_wh, &_cmtD, &_wh100, &_whI, &_wh0);
          if (_whI == 0 && _cmtD >= 0 && _cmtD < op->adjNbase) {
            rk4s_dose _d; _d.step = rec.nStep(); _d.cmt = _cmtD;
            _d.amt = getDose(ind, ind->ix[i]);
            doseRec.push_back(_d);
          }
        }
      }
      if (getEvid(ind, ind->ix[i]) == 3) {
        handleEvid3(ind, op, rx, neq, &xp, &xout, yp, &(istate), u_inis);
      } else if (handleEvid1(&i, rx, neq, yp, &xout)){
        handleSS(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                 xp, ind->id, &i, ind->n_all_times, &istate, op, ind, u_inis, ctx);
        if (ind->wh0 == EVID0_OFF){
          ind->solve[ind->cmt] = op->inits[ind->cmt];
        }
        if (rx->istateReset) istate = 1;
        xp = xout;
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
      if (_mtime_requeued) i--;
    }
    if (adj && i >= 0 && i < ind->n_all_times) boundary[i] = rec.nStep();
    ind->solvedIdx = i;
  }

  // ---- backward reverse-mode sweep -> fill rx__sens_* output slots ----
  if (adj && !localBadSolve && ind->err == 0) {
    rk4s_backward_fill(rx, op, ind, neq[1], rec, T, op->adjNbase, op->adjNp,
                       op->adjFxOff, op->adjFpOff, op->adjDfOff, op->adjSensOff,
                       boundary, doseRec);
  }

  ind->solveTime += ((double)(clock() - t0))/CLOCKS_PER_SEC;
}

extern "C" void ind_rk4s(rx_solve *rx, int solveid,
                         t_dydt c_dydt, t_update_inis u_inis){
  rx_solving_options *op = rx->op;
  int neq[2];
  neq[0] = op->neq;
  neq[1] = 0;
  ind_rk4s_0(rx, op, solveid, neq, c_dydt, u_inis);
}

extern "C" void par_rk4s(rx_solve *rx){
  rx_solving_options *op = rx->op;
#ifdef _OPENMP
  int cores = op->cores;
#else
  int cores = 1;
#endif
  uint32_t nsub = rx->nsub, nsim = rx->nsim;
  int nsolve = (int)(nsim*nsub);

  uint32_t seed0 = getRxSeed1(cores);
  int abort = 0;

#ifdef _OPENMP
#pragma omp parallel for num_threads(cores)
#endif
  for (int solveid = 0; solveid < nsolve; solveid++){
    int neq[2];
    neq[0] = op->neq;
    neq[1] = 0;
    int localAbort;
#ifdef _OPENMP
#pragma omp atomic read
#endif
    localAbort = abort;
    if (localAbort == 0){
      setSeedEng1(seed0 + rx->ordId[solveid] - 1);
      ind_rk4s_0(rx, op, solveid, neq, dydt, update_inis);

      if (op->badSolve) {
#ifdef _OPENMP
#pragma omp atomic write
#endif
        abort = 1;
      }
    }
  }
}

#endif // IN_PAR_SOLVE
