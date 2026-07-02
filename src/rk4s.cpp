#ifdef IN_PAR_SOLVE
#undef min
#undef max
#include "odeinter.h"
#include <vector>

// ===========================================================================
// rk4s -- adjoint (discrete-adjoint) RK4.
//
// SEPARATE ode method from rk4 (rk4 stays byte-identical).  Forward pass is a
// classical fixed-step RK4 that RECORDS each step's realized dt + 4 internal
// stage states, so the backward reverse-mode sweep is the EXACT transpose of
// the actual numerical map (machine-precision-consistent gradient, mirroring
// R/adjointDiscrete.R's .rxDiscreteAdjointGrad).
//
// The model is built by R's .rxAdjointExpand: base ODE states, then
// rx__sens_<state>_BY_<param>__ OUTPUT-storage compartments (d/dt=0), plus the
// state Jacobian F_X (rx__adjFX_i_j__) and parameter forcing F_p
// (rx__adjFP_i_p__) exposed as lhs.  op carries the layout (op->adjNbase, adjNp,
// adjFxOff, adjFpOff, adjSensOff), set by rxData.cpp when method==206.
//
// Forward: step ONLY the nBase base states (the rx__sens_* slots stay 0 during
// the forward pass).  Backward: for each observation time and each base state k
// run an independent reset sweep with terminal covector e_k; the resulting
// mu (length np) = dy_k(t_obs)/dtheta is written into that obs row's
// rx__sens_* solve slots.
// ===========================================================================

struct rk4s_rec {
  int nq = 0;                          // base states stepped/recorded
  std::vector<double> h;               // realized dt per step
  std::vector<double> t0;              // step start time per step
  std::vector<double> a1, a2, a3, a4;  // stage states, flattened (nStep * nq)
  void clear() { h.clear(); t0.clear(); a1.clear(); a2.clear(); a3.clear(); a4.clear(); }
  size_t nStep() const { return h.size(); }
};

// One classical RK4 step over [t, t+dt], recording the 4 stage states.  Calls
// dydt directly (no boost) so the recorded stages ARE the forward map.
static inline void rk4s_step_record(t_dydt dydt, int *neq, int nq,
                                     double t, double dt, double *y,
                                     double *k1, double *k2, double *k3, double *k4,
                                     double *tmp, rk4s_rec *rec) {
  if (rec) { rec->t0.push_back(t); rec->a1.insert(rec->a1.end(), y, y + nq); }
  dydt(neq, t, y, k1);
  for (int i = 0; i < nq; ++i) tmp[i] = y[i] + 0.5 * dt * k1[i];
  if (rec) rec->a2.insert(rec->a2.end(), tmp, tmp + nq);
  dydt(neq, t + 0.5 * dt, tmp, k2);
  for (int i = 0; i < nq; ++i) tmp[i] = y[i] + 0.5 * dt * k2[i];
  if (rec) rec->a3.insert(rec->a3.end(), tmp, tmp + nq);
  dydt(neq, t + 0.5 * dt, tmp, k3);
  for (int i = 0; i < nq; ++i) tmp[i] = y[i] + dt * k3[i];
  if (rec) rec->a4.insert(rec->a4.end(), tmp, tmp + nq);
  dydt(neq, t + dt, tmp, k4);
  for (int i = 0; i < nq; ++i)
    y[i] += (dt / 6.0) * (k1[i] + 2.0 * k2[i] + 2.0 * k3[i] + k4[i]);
  if (rec) rec->h.push_back(dt);
}

// Stage-recording analogue of rk4_do_steps (src/rk4.cpp): same step-size logic,
// stepping only the first `nq` states (base states in adjoint mode).
static inline void rk4s_do_steps(rx_solving_options_ind *ind, rx_solving_options *op,
                                 t_dydt dydt, int *neq, int nq, double *yp,
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

  if ((int)scratch.size() < 5 * nq) scratch.resize(5 * nq);
  double *k1 = scratch.data(), *k2 = k1 + nq, *k3 = k2 + nq, *k4 = k3 + nq, *tmp = k4 + nq;

  error_checker check(ind, ind->rc, op->mxstep);
  zero_copy_state chk(yp, nq);

  while ((sign > 0 && t < xout) || (sign < 0 && t > xout)) {
    double current_dt = dt;
    if ((sign > 0 && t + dt > xout) || (sign < 0 && t + dt < xout)) {
      current_dt = xout - t;
    }
    try {
      rk4s_step_record(dydt, neq, nq, t, current_dt, yp, k1, k2, k3, k4, tmp, rec);
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
                               int cSub, rk4s_rec &rec, int nBase, int np,
                               int fxOff, int fpOff, int sensOff,
                               const std::vector<size_t> &boundary) {
  size_t nStep = rec.nStep();
  if (nStep == 0) return;
  int eff = rxEffNeq(ind, op);
  int nfx = nBase * nBase, nfp = nBase * np;

  // Precompute stage Jacobians for all steps: FXs[s][n*nfx + ...], FPs[s][...].
  std::vector<double> FXs[4], FPs[4];
  for (int s = 0; s < 4; ++s) { FXs[s].resize(nStep * nfx); FPs[s].resize(nStep * nfp); }
  std::vector<double> Ascratch(eff, 0.0);
  for (size_t n = 0; n < nStep; ++n) {
    double h = rec.h[n], t0 = rec.t0[n];
    const double *aS[4] = { &rec.a1[n * nBase], &rec.a2[n * nBase],
                            &rec.a3[n * nBase], &rec.a4[n * nBase] };
    double tS[4] = { t0, t0 + 0.5 * h, t0 + 0.5 * h, t0 + h };
    for (int s = 0; s < 4; ++s)
      rk4s_eval_jac(cSub, tS[s], aS[s], nBase, np, fxOff, fpOff, Ascratch.data(), eff,
                    ind, &FXs[s][n * nfx], &FPs[s][n * nfp]);
  }

  std::vector<double> lam(nBase), Xbar(nBase), k1b(nBase), k2b(nBase), k3b(nBase), k4b(nBase);
  std::vector<double> ab(nBase), mu(np);

  // For each observation time index, each base state k, sweep boundary[i]->0.
  for (int i = 0; i < ind->n_all_times; ++i) {
    if (!isObs(getEvid(ind, ind->ix[i]))) continue;
    size_t fromStep = boundary[i];
    double *out = getSolve(i);
    for (int k = 0; k < nBase; ++k) {
      for (int j = 0; j < nBase; ++j) lam[j] = (j == k) ? 1.0 : 0.0;
      for (int p = 0; p < np; ++p) mu[p] = 0.0;
      for (size_t nn = fromStep; nn >= 1; --nn) {
        size_t n = nn - 1;              // 0-based step
        double h = rec.h[n];
        const double *fx1 = &FXs[0][n * nfx], *fx2 = &FXs[1][n * nfx],
                     *fx3 = &FXs[2][n * nfx], *fx4 = &FXs[3][n * nfx];
        const double *fp1 = &FPs[0][n * nfp], *fp2 = &FPs[1][n * nfp],
                     *fp3 = &FPs[2][n * nfp], *fp4 = &FPs[3][n * nfp];
        for (int j = 0; j < nBase; ++j) Xbar[j] = lam[j];
        for (int j = 0; j < nBase; ++j) {
          k1b[j] = h / 6.0 * Xbar[j]; k2b[j] = h / 3.0 * Xbar[j];
          k3b[j] = h / 3.0 * Xbar[j]; k4b[j] = h / 6.0 * Xbar[j];
        }
        // stage 4:  ab = FX4^T k4b ; Xbar += ab ; k3b += h*ab ; mu += FP4^T k4b
        for (int j = 0; j < nBase; ++j) { double s = 0; for (int ii = 0; ii < nBase; ++ii) s += fx4[ii * nBase + j] * k4b[ii]; ab[j] = s; }
        for (int j = 0; j < nBase; ++j) { Xbar[j] += ab[j]; k3b[j] += h * ab[j]; }
        for (int p = 0; p < np; ++p) { double s = 0; for (int ii = 0; ii < nBase; ++ii) s += fp4[ii * np + p] * k4b[ii]; mu[p] += s; }
        // stage 3:  ab = FX3^T k3b ; Xbar += ab ; k2b += h/2*ab ; mu += FP3^T k3b
        for (int j = 0; j < nBase; ++j) { double s = 0; for (int ii = 0; ii < nBase; ++ii) s += fx3[ii * nBase + j] * k3b[ii]; ab[j] = s; }
        for (int j = 0; j < nBase; ++j) { Xbar[j] += ab[j]; k2b[j] += 0.5 * h * ab[j]; }
        for (int p = 0; p < np; ++p) { double s = 0; for (int ii = 0; ii < nBase; ++ii) s += fp3[ii * np + p] * k3b[ii]; mu[p] += s; }
        // stage 2:  ab = FX2^T k2b ; Xbar += ab ; k1b += h/2*ab ; mu += FP2^T k2b
        for (int j = 0; j < nBase; ++j) { double s = 0; for (int ii = 0; ii < nBase; ++ii) s += fx2[ii * nBase + j] * k2b[ii]; ab[j] = s; }
        for (int j = 0; j < nBase; ++j) { Xbar[j] += ab[j]; k1b[j] += 0.5 * h * ab[j]; }
        for (int p = 0; p < np; ++p) { double s = 0; for (int ii = 0; ii < nBase; ++ii) s += fp2[ii * np + p] * k2b[ii]; mu[p] += s; }
        // stage 1:  ab = FX1^T k1b ; Xbar += ab ; mu += FP1^T k1b
        for (int j = 0; j < nBase; ++j) { double s = 0; for (int ii = 0; ii < nBase; ++ii) s += fx1[ii * nBase + j] * k1b[ii]; ab[j] = s; }
        for (int j = 0; j < nBase; ++j) Xbar[j] += ab[j];
        for (int p = 0; p < np; ++p) { double s = 0; for (int ii = 0; ii < nBase; ++ii) s += fp1[ii * np + p] * k1b[ii]; mu[p] += s; }
        for (int j = 0; j < nBase; ++j) lam[j] = Xbar[j];   // additive bolus: dD/dX = I
      }
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
  // Adjoint mode: step ONLY the base states; the rx__sens_* slots stay 0 in the
  // forward pass and are written by the backward sweep.
  int adj = op->adjoint;
  int nStep = adj ? op->adjNbase : neqOde;   // states to step forward
  double *yp;

  rk4s_rec rec;
  rec.nq = nStep;
  std::vector<double> scratch;
  std::vector<size_t> boundary(ind->n_all_times, 0);  // cumulative steps at each stored time

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
              if (nStep > 0) {
                  rk4s_do_steps(ind, op, c_dydt, neq, nStep, yp, xp, ind->extraDoseNewXout, &rec, scratch);
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
                if (nStep > 0) {
                    rk4s_do_steps(ind, op, c_dydt, neq, nStep, yp, ind->extraDoseNewXout, xout, &rec, scratch);
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
          if (nStep > 0) {
              rk4s_do_steps(ind, op, c_dydt, neq, nStep, yp, xp, xout, &rec, scratch);
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
    rk4s_backward_fill(rx, op, ind, neq[1], rec, op->adjNbase, op->adjNp,
                       op->adjFxOff, op->adjFpOff, op->adjSensOff, boundary);
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
