#ifdef IN_PAR_SOLVE
#undef min
#undef max
#include "ode_dop54_bridge.h"

static inline void dop54_do_steps(RxDoPri54 *solver, rx_solving_options_ind *ind, rx_solving_options *op,
                                    t_dydt c_dydt, int *neq, double *yp,
                                    double xp, double xout) {
  double tint = xout - xp;
  if (tint == 0.0) return;
  double dt = op->HMIN > 0.0 ? op->HMIN : 0.01;
  if (dt <= 0.0) dt = 0.01;
  if (dt > std::fabs(tint)) dt = std::fabs(tint);
  if (tint < 0.0) dt = -dt;

  solver->reset_counters();
  solver->set_sol_external(yp);
  solver->set_t(xp);
try {
    solver->solve_adaptive(std::fabs(tint), std::fabs(dt), true);
  } catch (...) {
    if (ind->rc[0] == 0) ind->rc[0] = -2019;
    ind->err = 1;
  }
}

extern "C" void ind_dop54_0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq,
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
  RxDoPri54 *solver = NULL;
  if (neqOde > 0) {
    solver = new RxDoPri54(c_dydt, neq, neqOde, ind, getSolve(0), (long unsigned)op->mxstep);
      solver->set_quiet(true);
      if (op->atol2 != NULL) solver->set_abstol(op->ATOL);
      if (op->rtol2 != NULL) solver->set_reltol(op->RTOL);
  }
  double *yp;

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
    yp   = getSolve(i);
    xout = ind->timeThread[ind->ix[i]];

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

            if (neqOde > 0)
              dop54_do_steps(solver, ind, op, c_dydt, neq, yp, xp, ind->extraDoseNewXout);

            copyLinCmt(neq, ind, op, yp);
            const char* err_msg = "dop54 failed";
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

              if (neqOde > 0)
                dop54_do_steps(solver, ind, op, c_dydt, neq, yp, ind->extraDoseNewXout, xout);

              copyLinCmt(neq, ind, op, yp);
              const char* err_msg = "dop54 failed";
              postSolve(neq, &istate, ind->rc, &idx, yp, &err_msg, 9, false, ind, op, rx);
              if (*(ind->rc) < 0) localBadSolve = 1;
              ind->extraDoseNewXout = xout;
            }
            xp = ind->extraDoseNewXout;
          }
        }
        if (!localBadSolve && !isSameTime(xout, xp)) {
          preSolve(op, ind, xp, xout, yp);

          if (neqOde > 0)
            dop54_do_steps(solver, ind, op, c_dydt, neq, yp, xp, xout);

          copyLinCmt(neq, ind, op, yp);
          const char* err_msg = "dop54 failed";
          postSolve(neq, &istate, ind->rc, &i, yp, &err_msg, 7, true, ind, op, rx);
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
      } else if (handleEvid1(&i, rx, neq, yp, &xout)) {
        handleSS(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                 xp, ind->id, &i, ind->n_all_times, &istate, op, ind, u_inis, ctx);
        if (ind->wh0 == EVID0_OFF) {
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
  if (solver != NULL) delete solver;
  ind->solveTime += ((double)(clock() - t0))/CLOCKS_PER_SEC;
}

extern "C" void ind_dop54(rx_solve *rx, int solveid,
                            t_dydt c_dydt, t_update_inis u_inis) {
  rx_solving_options *op = rx->op;
  int neq[2];
  neq[0] = op->neq;
  neq[1] = 0;
  ind_dop54_0(rx, op, solveid, neq, c_dydt, u_inis);
}

extern "C" void par_dop54(rx_solve *rx) {
  rx_solving_options *op = rx->op;
#ifdef _OPENMP
  int cores = op->cores;
#else
  int cores = 1;
#endif
  uint32_t nsub = rx->nsub, nsim = rx->nsim;
  int nsolve = (int)(nsim * nsub);

  uint32_t seed0 = getRxSeed1(cores);
  int abort = 0;

#ifdef _OPENMP
#pragma omp parallel for num_threads(cores)
#endif
  for (int solveid = 0; solveid < nsolve; solveid++) {
    int neq[2];
    neq[0] = op->neq;
    neq[1] = 0;
    int localAbort;
#ifdef _OPENMP
#pragma omp atomic read
#endif
    localAbort = abort;
    if (localAbort == 0) {
      setSeedEng1(seed0 + rx->ordId[solveid] - 1);
      ind_dop54_0(rx, op, solveid, neq, dydt, update_inis);

      if (op->badSolve) {
#ifdef _OPENMP
#pragma omp atomic write
#endif
        abort = 1;
      }
    }
  }
}

extern "C" void dop54_solveWith1Pt(int *neq, double *yp, double *xp, double xout,
                                     int *istate, rx_solving_options *op,
                                     rx_solving_options_ind *ind) {
  int eff = rxEffNeq(ind, op);
  int neqOde = eff - op->numLin - op->numLinSens;

    if (neqOde > 0) {
    RxDoPri54 solver(dydt, neq, neqOde, ind, yp, (long unsigned)op->mxstep);
    solver.set_quiet(true);
    if (op->atol2 != NULL) solver.set_abstol(op->ATOL);
    if (op->rtol2 != NULL) solver.set_reltol(op->RTOL);
    dop54_do_steps(&solver, ind, op, dydt, neq, yp, *xp, xout);

    if (ind->rc[0] < 0) {
      *istate = -1;
      return;
    }
  }
  *xp = xout;
  *istate = 1;
}

#endif // IN_PAR_SOLVE
