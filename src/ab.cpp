#ifdef IN_PAR_SOLVE
#undef min
#undef max
#include "odeinter.h"

#include <array>

// AB coefficients [most-recent first]: c[0]*f_n + c[1]*f_{n-1} + ...
static const double ab_coef[8][8] = {
  // AB1 (Euler)
  {1.0, 0, 0, 0, 0, 0, 0, 0},
  // AB2
  {3.0/2, -1.0/2, 0, 0, 0, 0, 0, 0},
  // AB3
  {23.0/12, -4.0/3, 5.0/12, 0, 0, 0, 0, 0},
  // AB4
  {55.0/24, -59.0/24, 37.0/24, -3.0/8, 0, 0, 0, 0},
  // AB5
  {1901.0/720, -1387.0/360, 109.0/30, -637.0/360, 251.0/720, 0, 0, 0},
  // AB6
  {4277.0/1440, -2641.0/480, 4991.0/720, -3649.0/720, 959.0/480, -95.0/288, 0, 0},
  // AB7
  {198721.0/60480, -18637.0/2520, 235183.0/20160, -10754.0/945, 135713.0/20160, -5603.0/2520, 19087.0/60480, 0},
  // AB8
  {16083.0/4480, -1152169.0/120960, 242653.0/13440, -296053.0/13440, 2102243.0/120960, -115747.0/13440, 32863.0/13440, -5257.0/17280}
};

// -- discrete-adjoint recording (method="abs", code 208) --------------------------
// ab_do_steps re-initialises (RK4 startup + fresh f-history) on every call, so each
// call is a self-contained AB integration; calls chain only through y.  We record,
// per call, the ordered step list (y BEFORE each step, dt, and whether it was an
// RK4-startup step or an AB step) plus the call's start/end.  ab_adjoint.cpp reads
// these and runs the exact reverse-mode transpose.  Recompute J/F_p in the backward
// from the recorded y via calc_lhs (RK4 stages are recomputed, not recorded).
struct abRecStep { std::vector<double> y; double t, dt; int isRK4; };
struct abRecCall { size_t stepBegin, stepEnd; int order; double t0, t1; std::vector<double> yEnd; };
static thread_local bool                  abAdjActive = false;
static thread_local int                   abAdjNbase  = 0;
static thread_local std::vector<abRecStep> abAdjSteps;
static thread_local std::vector<abRecCall> abAdjCalls;

// Direct Adams-Bashforth implementation using manual history management.
// Boost's adams_bashforth<N> has accuracy issues with zero_copy_state aliasing.
static inline void ab_do_steps(rx_solving_options_ind *ind, rx_solving_options *op, rxode2_system& sys, zero_copy_state& state, double xp, double xout) {
  int neq = state.size();
  if (neq == 0) return;
  int order = op->MXORDN;
  if (order < 1) order = 1;
  if (order > 8) order = 8;

  double t = xp;
  double dt = op->HMIN > 0.0 ? op->HMIN : 0.0001;
  if (dt <= 0.0) dt = 0.0001;
  if (fabs(xout - xp) / dt >= op->mxstep) {
      dt = fabs(xout - xp) / (double)(op->mxstep - 10);
  }
  int sign = (xout > xp) ? 1 : -1;
  dt = sign * dt;
  const double fp_tol = std::numeric_limits<double>::epsilon() * op->mxstep *
                        std::max(std::abs(xp), std::abs(xout));

  const double* c = ab_coef[order - 1];

  // Derivative history: hist[0] = most recent f, hist[1] = f_{n-1}, ...
  std::vector<std::vector<double>> hist(order, std::vector<double>(neq, 0.0));
  std::vector<double> ytmp(neq), k1(neq), k2(neq), k3(neq), k4(neq), dxdt(neq);

  // RK4 helper: advance y by one step.
  // Use block scopes to ensure zero_copy_state objects are constructed fresh
  // each time -- assigning to a non-owned state overwrites the pointed-to data,
  // which would corrupt y.
  auto rk4_step = [&](std::vector<double>& y, double tc, double h) {
    { zero_copy_state xs(y.data(), neq), ds(k1.data(), neq); sys(xs, ds, tc); }
    for (int i = 0; i < neq; i++) ytmp[i] = y[i] + 0.5*h*k1[i];
    { zero_copy_state xs(ytmp.data(), neq), ds(k2.data(), neq); sys(xs, ds, tc + 0.5*h); }
    for (int i = 0; i < neq; i++) ytmp[i] = y[i] + 0.5*h*k2[i];
    { zero_copy_state xs(ytmp.data(), neq), ds(k3.data(), neq); sys(xs, ds, tc + 0.5*h); }
    for (int i = 0; i < neq; i++) ytmp[i] = y[i] + h*k3[i];
    { zero_copy_state xs(ytmp.data(), neq), ds(k4.data(), neq); sys(xs, ds, tc + h); }
    for (int i = 0; i < neq; i++) y[i] += h/6.0*(k1[i] + 2*k2[i] + 2*k3[i] + k4[i]);
  };

  // Copy current state into working vector
  std::vector<double> y(state.data_, state.data_ + neq);
  int initialized = 0;
  int steps_taken = 0;
  int max_steps = op->mxstep;

  // discrete-adjoint: open a call record (this whole ab_do_steps call = one segment).
  size_t _abCallBegin = 0;
  if (abAdjActive) _abCallBegin = abAdjSteps.size();

  while ((sign > 0 && t < xout) || (sign < 0 && t > xout)) {
    double current_dt = dt;
    if ((sign > 0 && t + dt > xout) || (sign < 0 && t + dt < xout)) {
      current_dt = xout - t;
      if (std::abs(current_dt) <= fp_tol) break;
    }

    if (ind->err != 0) { ind->rc[0] = -2019; break; }

    // record this step (y BEFORE it, dt, and its type) for the backward transpose.
    if (abAdjActive) {
      abRecStep _s; _s.t = t; _s.dt = current_dt; _s.isRK4 = (initialized < order - 1) ? 1 : 0;
      _s.y.assign(y.begin(), y.begin() + abAdjNbase);
      abAdjSteps.push_back(std::move(_s));
    }

    // Evaluate f at current state (block scope avoids assignment aliasing)
    { zero_copy_state xs(y.data(), neq), ds(dxdt.data(), neq); sys(xs, ds, t); }

    if (ind->err != 0) { ind->rc[0] = -2019; break; }

    if (initialized < order - 1) {
      // Initialization: use RK4 to advance and build history.
      // Store at reversed position so that after all init steps and the first
      // shift, hist[0]=newest .. hist[order-1]=oldest (most-recent-first order).
      hist[order - 2 - initialized].assign(dxdt.begin(), dxdt.end());
      initialized++;
      rk4_step(y, t, current_dt);
    } else {
      // Shift history right to make room for the newest entry at hist[0].
      for (int i = order - 1; i > 0; i--) hist[i] = hist[i-1];
      hist[0].assign(dxdt.begin(), dxdt.end());

      // AB step
      for (int i = 0; i < neq; i++) {
        double sum = 0.0;
        for (int j = 0; j < order; j++) sum += c[j] * hist[j][i];
        y[i] += current_dt * sum;
      }
    }

    t += current_dt;
    steps_taken++;
    if (steps_taken > max_steps) {
      ind->rc[0] = -2019;
      ind->err = 1;
      break;
    }
    if (ind->err != 0) { ind->rc[0] = -2019; break; }
  }

  // discrete-adjoint: close the call record (start/end time, order, step range, end y).
  if (abAdjActive && abAdjSteps.size() > _abCallBegin) {
    abRecCall _c; _c.stepBegin = _abCallBegin; _c.stepEnd = abAdjSteps.size();
    _c.order = order; _c.t0 = xp; _c.t1 = t;
    _c.yEnd.assign(y.begin(), y.begin() + abAdjNbase);
    abAdjCalls.push_back(std::move(_c));
  }

  // Write result back to state (yp)
  std::copy(y.begin(), y.end(), state.data_);
}

extern "C" void ind_ab_0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq,
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
  rxode2_system sys(c_dydt, neq, ind);
  double *yp;
  
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
                  zero_copy_state state(yp, neqOde);
                  ab_do_steps(ind, op, sys, state, xp, ind->extraDoseNewXout);
              }

              copyLinCmt(neq, ind, op, yp);
              const char* err_msg = "ab failed";
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
                    zero_copy_state state(yp, neqOde);
                    ab_do_steps(ind, op, sys, state, ind->extraDoseNewXout, xout);
                }

                copyLinCmt(neq, ind, op, yp);
                const char* err_msg = "ab failed";
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
              zero_copy_state state(yp, neqOde);
              ab_do_steps(ind, op, sys, state, xp, xout);
          }
          
          copyLinCmt(neq, ind, op, yp);
          const char* err_msg = "ab failed";
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
  ind->solveTime += ((double)(clock() - t0))/CLOCKS_PER_SEC;
}

extern "C" void ind_ab(rx_solve *rx, int solveid,
                          t_dydt c_dydt, t_update_inis u_inis){
  rx_solving_options *op = rx->op;
  int neq[2];
  neq[0] = op->neq;
  neq[1] = 0;
  ind_ab_0(rx, op, solveid, neq, c_dydt, u_inis);
}

extern "C" void par_ab(rx_solve *rx){
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
      ind_ab_0(rx, op, solveid, neq, dydt, update_inis);
      
      if (op->badSolve) {
#ifdef _OPENMP
#pragma omp atomic write
#endif
        abort = 1;
      }
    }
  }
}

extern "C" void ab_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind) {
  int eff = rxEffNeq(ind, op);
  int neqOde = eff - op->numLin - op->numLinSens;

  rxode2_system sys(dydt, neq, ind);

  if (neqOde > 0) {
      zero_copy_state state(yp, neqOde);
      ab_do_steps(ind, op, sys, state, *xp, xout);
      if (ind->rc[0] < 0) {
          *istate = -1;
          return;
      }
  }
  *xp = xout;
  *istate = 1;
}

#endif // IN_PAR_SOLVE
