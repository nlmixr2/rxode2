#ifdef IN_PAR_SOLVE

#include <vector>
#include "cvode_solver.h"

extern "C" int getCvodeLinearSolver();

// dydt_liblsoda is the global RHS function pointer defined in par_solve.cpp's TU.
// It is accessible here because cvode.cpp is #included inside par_solve.cpp.
static int cvode_rhs_trampoline(double t, double *y, double *ydot, void *data) {
  return dydt_liblsoda(t, y, ydot, data);
}

// Called from the handleSS dispatch (case 21).
extern "C" void cvode_solveWith1Pt(int *neq, double *yp, double *xp_ptr, double xout,
                                    int *istate, rx_solving_options *op,
                                    rx_solving_options_ind *ind, void *ctx_ptr) {
  if (!ctx_ptr) { *istate = -1; return; }
  cvode_ctx_t *ctx = (cvode_ctx_t *)ctx_ptr;
  int neqOde = neq[0] - op->numLin - op->numLinSens;
  if (neqOde <= 0) { *xp_ptr = xout; *istate = 1; return; }
  *istate = cvode_ctx_integrate(ctx, yp, *xp_ptr, xout);
  if (*istate >= 0) *xp_ptr = xout;
}

// Whether CVODE may carry on across record r (a position in ind->ix) without
// re-initialising: a plain observation (evid 0) or the time-0 placeholder
// (evid 9) at which no time-varying covariate changes value, or slope, so
// neither the state nor the right-hand side changes there.  An mtime() or evid
// 2 record can mark a model-time or right-hand-side discontinuity, so it
// re-initialises as a dose does.
static bool cvodeCarriesAcross(rx_solving_options *op, rx_solving_options_ind *ind, int r) {
  int raw = ind->ix[r];
  if (raw < 0 || raw >= ind->n_all_times_orig) return false;
  int evid = getEvid(ind, raw);
  if (evid != 0 && evid != 9) return false;
  if (!op->do_par_cov) return true;
  for (int k = 0; k < op->ncov; k++) {
    if (!op->par_cov[k]) continue;
    double *y = ind->cov_ptr + ind->n_all_times_orig * k;
    double v = y[raw];
    if (ISNAN(v)) return false;
    for (int s = r - 1; s <= r + 1; s += 2) {
      if (s < 0 || s >= ind->n_all_times) continue;
      int rs = ind->ix[s];
      if (rs < 0 || rs >= ind->n_all_times_orig || !(y[rs] == v)) return false;
    }
  }
  return true;
}

// The time CVODE must not step past on its way to record i: the first record
// from i on that it may not carry on across (or the last record), or the first
// pending extra dose after xp when that comes earlier.  *rStop gets the
// record.
static double cvodeStopTime(rx_solving_options *op, rx_solving_options_ind *ind,
                            int i, double xp, int *rStop) {
  int r = i;
  while (r < ind->n_all_times - 1 && cvodeCarriesAcross(op, ind, r)) r++;
  *rStop = r;
  double ts = ind->timeThread[ind->ix[r]];
  for (int e = ind->idxExtra; e < ind->extraDoseN[0]; e++) {
    double te = ind->extraDoseTime[ind->extraDoseTimeIdx[e]];
    if (te > xp && te < ts) ts = te;
  }
  return ts;
}

extern "C" void ind_cvode_0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq,
                             t_update_inis u_inis) {
  clock_t t0_clock = clock();
  int i;
  int istate = 1;

  neq[1] = solveid; // subject id, not an rx->ordId position
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

  double  rtol      = (ind->rtol2 != NULL) ? ind->rtol2[0]
                    : (op->rtol2  != NULL) ? op->rtol2[0]
                    : op->RTOL;
  double *atol_data = (ind->atol2 != NULL) ? ind->atol2
                    : (op->atol2  != NULL) ? op->atol2
                    : &(op->ATOL);

  cvode_ctx_t *cvode_ctx = NULL;
  int cvodeLinSolver = op->cvodeLinSolver > 0 ? op->cvodeLinSolver :
    getCvodeLinearSolver();

  // Eagerly initialize CVODE before the main loop so cvode_ctx is non-NULL
  // when handleSS is called for a dose at t=0 (xout==xp, so the lazy path
  // inside the loop is never reached before handleSS fires).
  if (neqOde > 0 && ind->n_all_times > 0) {
    std::vector<double> atol_buf;
    double *atol_ptr = atol_data;
    if (atol_data == &(op->ATOL)) {
      atol_buf.assign(neqOde, op->ATOL);
      atol_ptr = atol_buf.data();
    }
    _growSolveIfNeeded(ind, op, 0, 1);
    yp = getSolve(0);
    cvode_ctx = cvode_ctx_create(neqOde, yp, atol_ptr, rtol,
                                 xp, op->HMIN,
                                 op->hmax2 > 0.0 ? op->hmax2 : 0.0,
                                 op->mxstep, cvode_rhs_trampoline,
                                 (void *)neq, cvodeLinSolver);
    if (!cvode_ctx) {
      ind->rc[0] = -2019;
      rxSolverFail(ind);
      ind->solveTime += ((double)(clock() - t0_clock)) / CLOCKS_PER_SEC;
      return;
    }
  }

  // CVODE re-initialises only where the state or the right-hand side changed:
  // across an observation record with no covariate change it carries on, its
  // step, order and history intact, with a stop time at the next record (or
  // extra dose) it may not cross.  cvLive says it may carry on from xp;
  // cvStop/cvStopRec cache that stop time and its record.  A covariate sampled
  // from another subject, or a delay() history, always re-initialises.
  const bool cvMayCarry = !rx->sample && !op->hasDelay;
  bool cvLive = false;
  double cvStop = 0.0;
  int cvStopRec = -1;

  for (i = 0; i < ind->n_all_times; i++) {
    ind->idx  = i;
    ind->linSS = 0;
    // a record that pushes events or adds extra doses invalidates the stop time
    int cvPushed = ind->nPushedExtra, cvExtraN = ind->extraDoseN[0];
    if (ind->mainSorted == 0) {
      cvLive = false;
      double *_rtime = ind->timeThread;
      for (int _j = i; _j < ind->n_all_times; _j++) {
        int _raw  = ind->ix[_j];
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
        if (!localBadSolve) {
          if (handleExtraDose(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                              xp, ind->id, &i, ind->n_all_times, &istate, op, ind, u_inis,
                              cvode_ctx ? (void *)cvode_ctx : NULL)) {
            if (!localBadSolve && !isSameTime(ind->extraDoseNewXout, xp)) {
              preSolve(op, ind, xp, ind->extraDoseNewXout, yp);
              if (neqOde > 0 && cvode_ctx) {
                istate = cvode_ctx_integrate_stop(cvode_ctx, yp, xp, ind->extraDoseNewXout,
                                                  ind->extraDoseNewXout, !cvLive);
              }
              copyLinCmt(neq, ind, op, yp);
              postSolve(neq, &istate, ind->rc, &i, yp, NULL, 0, false, ind, op, rx);
              if (*(ind->rc) < 0) localBadSolve = 1;
              xp = ind->extraDoseNewXout;
            }
            if (!localBadSolve) {
              int idx  = ind->idx;
              int ixds = ind->ixds;
              int trueIdx = ind->extraDoseTimeIdx[ind->idxExtra];
              ind->idx  = -1 - trueIdx;
              handle_evid(ind->extraDoseEvid[trueIdx], neq[0],
                          ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                          neq[1], ind);
              istate    = 1;
              ind->ixds = ixds;
              ind->idx  = idx;
              ind->idxExtra++;
              cvLive = false;
              if (!isSameTime(xout, ind->extraDoseNewXout)) {
                preSolve(op, ind, ind->extraDoseNewXout, xout, yp);
                if (neqOde > 0 && cvode_ctx) {
                  cvStop = cvMayCarry
                    ? cvodeStopTime(op, ind, i, ind->extraDoseNewXout, &cvStopRec)
                    : xout;
                  istate = cvode_ctx_integrate_stop(cvode_ctx, yp,
                                                    ind->extraDoseNewXout, xout,
                                                    cvStop, 1);
                  cvLive = cvMayCarry && istate > 0 && cvStopRec > i;
                }
                copyLinCmt(neq, ind, op, yp);
                postSolve(neq, &istate, ind->rc, &i, yp, NULL, 0, false, ind, op, rx);
                if (*(ind->rc) < 0) localBadSolve = 1;
                ind->extraDoseNewXout = xout;
              }
              xp = ind->extraDoseNewXout;
            }
          }
        }

        if (!localBadSolve && !isSameTime(xout, xp)) {
          preSolve(op, ind, xp, xout, yp);
          if (neqOde > 0 && cvode_ctx) {
            if (!cvLive) {
              cvStop = cvMayCarry ? cvodeStopTime(op, ind, i, xp, &cvStopRec) : xout;
            }
            istate = cvode_ctx_integrate_stop(cvode_ctx, yp, xp, xout, cvStop, !cvLive);
            cvLive = cvMayCarry && istate > 0 && cvStopRec > i;
          }
          copyLinCmt(neq, ind, op, yp);
          postSolve(neq, &istate, ind->rc, &i, yp, NULL, 0, false, ind, op, rx);
          if (*(ind->rc) < 0) localBadSolve = 1;
        }
        xp = xout;
      }
    }

    ind->_newind = 2;
    if (!localBadSolve) {
      ind->idx = i;
      if (getEvid(ind, ind->ix[i]) == 3) {
        cvLive = false;
        handleEvid3(ind, op, rx, neq, &xp, &xout, yp, &istate, u_inis);
      } else if (handleEvid1(&i, rx, neq, yp, &xout)) {
        cvLive = false;
        handleSS(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                 xp, ind->id, &i, ind->n_all_times, &istate, op, ind, u_inis,
                 cvode_ctx ? (void *)cvode_ctx : NULL);
        if (ind->wh0 == EVID0_OFF) {
          ind->solve[ind->cmt] = op->inits[ind->cmt];
        }
        if (rx->istateReset) istate = 1;
        xp = xout;
      }
      int _mtime_requeued = 0;
      if (rx->nMtime > 0) {
        if (recomputeMtimeIfNeeded(rx, ind, yp, i, xout)) {
          ind->mainSorted  = 0;
          _mtime_requeued = 1;
        }
      }
      if (rx->needSort & needSortAlag) {
        if (refreshLagTimesIfNeeded(rx, ind, yp, i + 1, xout)) {
          ind->mainSorted = 0;
        }
      }
      updateSolve(ind, op, neq, xout, i, ind->n_all_times);
      if (_mtime_requeued || ind->mainSorted == 0 || !cvodeCarriesAcross(op, ind, i) ||
          ind->nPushedExtra != cvPushed || ind->extraDoseN[0] != cvExtraN) {
        cvLive = false;
      }
      if (_mtime_requeued) i--;
    }
    ind->solvedIdx = i;
  }

  if (cvode_ctx) cvode_ctx_destroy(cvode_ctx);
  ind->solveTime += ((double)(clock() - t0_clock)) / CLOCKS_PER_SEC;
}

extern "C" void ind_cvode_dense(rx_solve *rx, int solveid, t_update_inis u_inis);
extern "C" void par_cvode_dense(rx_solve *rx);

extern "C" void ind_cvode(rx_solve *rx, int solveid, t_update_inis u_inis) {
  rx_solving_options *op = rx->op;
  if (op->useDense) { ind_cvode_dense(rx, solveid, u_inis); return; }
  int neq[2];
  neq[0] = op->neq;
  neq[1] = 0;
  ind_cvode_0(rx, op, solveid, neq, u_inis);
}

extern "C" void par_cvode(rx_solve *rx) {
  rx_solving_options *op = rx->op;
  if (op->useDense) { par_cvode_dense(rx); return; }
#ifdef _OPENMP
  int cores = op->cores;
#else
  int cores = 1;
#endif
  uint32_t nsub   = rx->nsub;
  uint32_t nsim   = rx->nsim;
  int      nsolve = (int)(nsim * nsub);

  uint32_t seed0 = getRxSeed1(cores);
#ifdef _OPENMP
#pragma omp parallel for num_threads(cores)
#endif
  for (int solveid = 0; solveid < nsolve; solveid++) {
    int neq[2];
    neq[0] = op->neq;
    neq[1] = 0;
    // rx->ordId walks positions (sortIds); the drivers take subject ids.
    int _id = rx->ordId[solveid] - 1;
    setSeedEng1(seed0 + _id);
    ind_cvode_0(rx, op, _id, neq, update_inis);
  }
  // close the per-subject seed block: the loop consumed nsolve seeds, not
  // cores, and the next solve must not re-use any of them
  setRxSeedFinal(seed0 + (uint32_t)nsolve);
}

#endif // IN_PAR_SOLVE
