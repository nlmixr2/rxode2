#ifdef IN_PAR_SOLVE
#undef min
#undef max
#include "odeinter.h"
#include <vector>

// ===========================================================================
// rk4s -- adjoint (discrete-adjoint) RK4.
//
// This is the FIRST in-engine discrete-adjoint solver (plan Phase 1).  It is a
// SEPARATE ode method from rk4: the forward pass is a classical fixed-step RK4
// that RECORDS each step's realized dt and its 4 internal stage states, so the
// backward reverse-mode sweep is the EXACT transpose of the actual numerical
// map (machine-precision-consistent gradient, mirroring R/adjointDiscrete.R).
//
// rk4 (src/rk4.cpp) is left byte-identical; rk4s does not touch the boost path.
//
// Phase 0 (this commit): the stage-recording forward driver + method wiring.
// The primal trajectory must match rk4 to numerical tolerance.  The backward
// sweep (Phase 1) consumes rk4s_rec and is added next; F_X/F_p come from a
// codegen change exposing the Jacobian / param-derivatives as readable outputs.
// ===========================================================================

// Per-subject recording buffer.  For each accepted forward step n we store the
// realized step size h[n] and the 4 RK4 stage states a1..a4 (each nq long,
// flattened).  a1 = y at step start, a2 = y + h/2*k1, a3 = y + h/2*k2,
// a4 = y + h*k3 -- the points where F_X / F_p are evaluated in the transpose.
struct rk4s_rec {
  int nq = 0;                 // number of ODE states stepped
  std::vector<double> h;      // realized dt per step
  std::vector<double> a1, a2, a3, a4;  // stage states, flattened (nStep * nq)
  void clear() { h.clear(); a1.clear(); a2.clear(); a3.clear(); a4.clear(); }
  size_t nStep() const { return h.size(); }
};

// One classical RK4 step, recording the 4 stage states into rec.  Advances y
// in place over [t, t+dt].  Calls dydt directly (no boost) so the recorded
// stages ARE the forward map that the adjoint transposes.
static inline void rk4s_step_record(t_dydt dydt, int *neq, int nq,
                                     double t, double dt, double *y,
                                     double *k1, double *k2, double *k3, double *k4,
                                     double *tmp, rk4s_rec *rec) {
  // a1 = y
  if (rec) rec->a1.insert(rec->a1.end(), y, y + nq);
  dydt(neq, t, y, k1);
  // a2 = y + dt/2 * k1
  for (int i = 0; i < nq; ++i) tmp[i] = y[i] + 0.5 * dt * k1[i];
  if (rec) rec->a2.insert(rec->a2.end(), tmp, tmp + nq);
  dydt(neq, t + 0.5 * dt, tmp, k2);
  // a3 = y + dt/2 * k2
  for (int i = 0; i < nq; ++i) tmp[i] = y[i] + 0.5 * dt * k2[i];
  if (rec) rec->a3.insert(rec->a3.end(), tmp, tmp + nq);
  dydt(neq, t + 0.5 * dt, tmp, k3);
  // a4 = y + dt * k3
  for (int i = 0; i < nq; ++i) tmp[i] = y[i] + dt * k3[i];
  if (rec) rec->a4.insert(rec->a4.end(), tmp, tmp + nq);
  dydt(neq, t + dt, tmp, k4);
  // y_next = y + dt/6 * (k1 + 2 k2 + 2 k3 + k4)
  for (int i = 0; i < nq; ++i)
    y[i] += (dt / 6.0) * (k1[i] + 2.0 * k2[i] + 2.0 * k3[i] + k4[i]);
  if (rec) rec->h.push_back(dt);
}

// Stage-recording analogue of rk4_do_steps (src/rk4.cpp): same step-size logic
// (dt = HMIN or 0.01, capped by mxstep, last sub-step shortened to hit xout),
// but stepping yp directly and recording into rec.
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

  // scratch: k1,k2,k3,k4,tmp -> 5*nq
  if ((int)scratch.size() < 5 * nq) scratch.resize(5 * nq);
  double *k1 = scratch.data(), *k2 = k1 + nq, *k3 = k2 + nq, *k4 = k3 + nq, *tmp = k4 + nq;

  error_checker check(ind, ind->rc, op->mxstep);
  zero_copy_state chk(yp, nq);  // only for the step-count/err guard

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
  double *yp;

  rk4s_rec rec;                 // per-subject forward recording (Phase 1 sweep)
  rec.nq = neqOde;
  std::vector<double> scratch;  // reused RK4 stage/derivative scratch

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
              if (neqOde > 0) {
                  rk4s_do_steps(ind, op, c_dydt, neq, neqOde, yp, xp, ind->extraDoseNewXout, &rec, scratch);
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
                if (neqOde > 0) {
                    rk4s_do_steps(ind, op, c_dydt, neq, neqOde, yp, ind->extraDoseNewXout, xout, &rec, scratch);
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
          if (neqOde > 0) {
              rk4s_do_steps(ind, op, c_dydt, neq, neqOde, yp, xp, xout, &rec, scratch);
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
    ind->solvedIdx = i;
  }

  // ---- Phase 1 (next commit): backward reverse-mode sweep over `rec` here. ----
  // Consumes rec.a1..a4 + rec.h with the model's F_X / F_p (codegen change) and
  // dydt to fill the rx__sens_* output columns via the shared explicit-RK
  // transpose.  For Phase 0 the recording is validated by primal identity only.
  (void)rec;

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
