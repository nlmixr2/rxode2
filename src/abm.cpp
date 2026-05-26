#ifdef IN_PAR_SOLVE
#undef min
#undef max
#include "odeinter.h"

#include <boost/numeric/odeint/stepper/adams_bashforth_moulton.hpp>

template <size_t N>
static inline void abm_do_steps_N(rx_solving_options_ind *ind, rx_solving_options *op, rxode2_system& sys, zero_copy_state& state, double xp, double xout) {
  typedef boost::numeric::odeint::adams_bashforth_moulton<N, zero_copy_state> stepper_type;
  stepper_type stepper;
  boost::numeric::odeint::runge_kutta4<zero_copy_state> rk4_stepper;
  double t = xp;
  double dt = op->HMIN > 0.0 ? op->HMIN : 0.0001;
  if (dt <= 0.0) dt = 0.0001;
  if (fabs(xout - xp) / dt > op->mxstep) {
      dt = fabs(xout - xp) / (double)(op->mxstep - 10);
  }
  int sign = (xout > xp) ? 1 : -1;
  dt = sign * dt;

  error_checker check(ind, ind->rc, op->mxstep);

  while ( (sign > 0 && t < xout) || (sign < 0 && t > xout) ) {
    double current_dt = dt;
    bool is_fractional = false;
    // We use a small tolerance to avoid taking a tiny fractional step due to floating point error
    if ( (sign > 0 && t + dt > xout) || (sign < 0 && t + dt < xout) ) {
      current_dt = xout - t;
      is_fractional = true;
    }

    try {
      if (is_fractional) {
        rk4_stepper.do_step(sys, state, t, current_dt);
      } else {
        stepper.do_step(sys, state, t, current_dt);
      }
    } catch(const std::exception& e) {
      if (ind->rc[0] == 0) ind->rc[0] = -2019;
      ind->err = 1;
      break;
    }

    t += current_dt;
    check(state, t);
    if (ind->err != 0) break;
  }
}

static inline void abm_do_steps(rx_solving_options_ind *ind, rx_solving_options *op, rxode2_system& sys, zero_copy_state& state, double xp, double xout) {
  switch (op->MXORDN) {
    case 1: abm_do_steps_N<1>(ind, op, sys, state, xp, xout); break;
    case 2: abm_do_steps_N<2>(ind, op, sys, state, xp, xout); break;
    case 3: abm_do_steps_N<3>(ind, op, sys, state, xp, xout); break;
    case 4: abm_do_steps_N<4>(ind, op, sys, state, xp, xout); break;
    case 5: abm_do_steps_N<5>(ind, op, sys, state, xp, xout); break;
    case 6: abm_do_steps_N<6>(ind, op, sys, state, xp, xout); break;
    case 7: abm_do_steps_N<7>(ind, op, sys, state, xp, xout); break;
    case 8: abm_do_steps_N<8>(ind, op, sys, state, xp, xout); break;
    default: abm_do_steps_N<5>(ind, op, sys, state, xp, xout); break;
  }
}

extern "C" void ind_abm_0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq,
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
                  abm_do_steps(ind, op, sys, state, xp, ind->extraDoseNewXout);
              }

              copyLinCmt(neq, ind, op, yp);
              const char* err_msg = "abm failed";
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
                    abm_do_steps(ind, op, sys, state, ind->extraDoseNewXout, xout);
                }

                copyLinCmt(neq, ind, op, yp);
                const char* err_msg = "abm failed";
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
              abm_do_steps(ind, op, sys, state, xp, xout);
          }
          
          copyLinCmt(neq, ind, op, yp);
          const char* err_msg = "abm failed";
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

extern "C" void ind_abm(rx_solve *rx, int solveid,
                          t_dydt c_dydt, t_update_inis u_inis){
  rx_solving_options *op = rx->op;
  int neq[2];
  neq[0] = op->neq;
  neq[1] = 0;
  ind_abm_0(rx, op, solveid, neq, c_dydt, u_inis);
}

extern "C" void par_abm(rx_solve *rx){
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
      ind_abm_0(rx, op, solveid, neq, dydt, update_inis);
      
      if (op->badSolve) {
#ifdef _OPENMP
#pragma omp atomic write
#endif
        abort = 1;
      }
    }
  }
}

extern "C" void abm_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind) {
  int eff = rxEffNeq(ind, op);
  int neqOde = eff - op->numLin - op->numLinSens;
  
  rxode2_system sys(dydt, neq, ind);
  
  if (neqOde > 0) {
      zero_copy_state state(yp, neqOde);
      abm_do_steps(ind, op, sys, state, *xp, xout);
      if (ind->rc[0] < 0) {
          *istate = -1;
          return;
      }
  }
  *xp = xout;
  *istate = 1;
}

#endif // IN_PAR_SOLVE
