#ifdef IN_PAR_SOLVE
#undef min
#undef max
#include "odeinter.h"

#include <boost/numeric/odeint/stepper/rosenbrock4.hpp>
#include <boost/numeric/odeint/stepper/rosenbrock4_controller.hpp>
#include <boost/numeric/odeint/stepper/rosenbrock4_dense_output.hpp>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/numeric/ublas/matrix.hpp>


extern "C" void ind_ros4_0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq,
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

  // Delay differential equation history for the ros4 (stiff) dense path.
  ind->delayHistOn = op->hasDelay;
  ind->delayHistN  = 0;
  ind->delayT0     = xp;
  ind->delayMinT   = R_PosInf;
  ind->delayWarmed = 0;
  double _ros4MaxDt = 0.0; // 0 = no cap (default rosenbrock4_controller behavior)
  if (ind->delayHistOn && neqOde > 0) {
    // one RHS evaluation so delay() learns the minimum delay, which both caps
    // the step size and confirms whether any delay is present
    std::vector<double> _ddt((size_t)neqOde), _y0((size_t)neqOde);
    for (int _j = 0; _j < neqOde; _j++) _y0[_j] = op->inits[_j];
    c_dydt(neq, xp, _y0.data(), _ddt.data());
    ind->delayWarmed = 1;
    if (R_FINITE(ind->delayMinT)) _ros4MaxDt = ind->delayMinT;
  }

  typedef boost::numeric::ublas::vector<double> state_type;
  typedef boost::numeric::ublas::matrix<double> matrix_type;
  typedef boost::numeric::odeint::rosenbrock4<double> stepper_base_type;
  typedef boost::numeric::odeint::rosenbrock4_controller<stepper_base_type> error_stepper_type;
  error_stepper_type stepper(ind->atol2[0], ind->rtol2[0], _ros4MaxDt, stepper_base_type());


  typedef boost::numeric::odeint::rosenbrock4_dense_output<error_stepper_type> dense_stepper_type;
  dense_stepper_type dense_stepper(stepper);
  bool stepper_initialized = false;

  // scratch vectors for sampling ros4 dense output when recording delay history
  state_type _dq0(neqOde), _dq1(neqOde), _dq2(neqOde), _dq3(neqOde);
  auto recordDelayStep = [&](void) {
    if (!ind->delayHistOn || ind->err) return;
    double t_old = dense_stepper.previous_time();
    double t_cur = dense_stepper.current_time();
    double hh = t_cur - t_old;
    dense_stepper.calc_state(t_old,                _dq0);
    dense_stepper.calc_state(t_old + hh / 3.0,     _dq1);
    dense_stepper.calc_state(t_old + 2.0*hh / 3.0, _dq2);
    dense_stepper.calc_state(t_cur,                _dq3);
    rxDelayHistPushSamples(ind, neqOde, t_old, hh,
                           &_dq0[0], &_dq1[0], &_dq2[0], &_dq3[0]);
  };

  auto sys = make_rxode2_system_ros4(ind, c_dydt, calc_jac, neq);
  
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
        ind->rc[0] = -1100;
        badSolveExit(i);
        localBadSolve = 1;
      } else {
        if (handleExtraDose(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                            xp, ind->id, &i, ind->n_all_times, &istate, op, ind, u_inis, ctx)) {
            if (!localBadSolve && !isSameTime(ind->extraDoseNewXout, xp)) {
              preSolve(op, ind, xp, ind->extraDoseNewXout, yp);

              if (neqOde > 0) {
                  state_type state(neqOde);
                  double dt = (ind->autoHcur > 0.0) ? ind->autoHcur
                  : (op->HMIN > 0.0) ? op->HMIN : 1e-4;
                  if (ind->extraDoseNewXout < xp) dt = -dt;
                  sys.first.xout_ = ind->extraDoseNewXout;
                  sys.first.sign_ = (dt > 0) ? 1 : -1;
                  try {
                      
                      std::copy(yp, yp + neqOde, state.begin());
                      boost::numeric::odeint::integrate_adaptive(

                          stepper, sys, state, xp, ind->extraDoseNewXout, dt, error_checker(ind, ind->rc, op->mxstep));
                      stepper_initialized = false;
                      if (!ind->err) std::copy(state.begin(), state.end(), yp);
                  } catch(const std::exception& e) {
                      if (ind->rc[0] == 0) ind->rc[0] = -2019;
                  }
              }

              copyLinCmt(neq, ind, op, yp);
              const char* err_msg = "ros4 failed";
              postSolve(neq, &istate, ind->rc, &i, yp, &err_msg, 11, true, ind, op, rx);
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
                    state_type state(neqOde);
                    double dt = (ind->autoHcur > 0.0) ? ind->autoHcur
                  : (op->HMIN > 0.0) ? op->HMIN : 1e-4;
                    if (xout < ind->extraDoseNewXout) dt = -dt;
                    sys.first.xout_ = xout;
                    sys.first.sign_ = (dt > 0) ? 1 : -1;
                    try {
                        
                      std::copy(yp, yp + neqOde, state.begin());
                      boost::numeric::odeint::integrate_adaptive(

                            stepper, sys, state, ind->extraDoseNewXout, xout, dt, error_checker(ind, ind->rc, op->mxstep));
                        stepper_initialized = false;
                      if (!ind->err) std::copy(state.begin(), state.end(), yp);
                    } catch(const std::exception& e) {
                        if (ind->rc[0] == 0) ind->rc[0] = -2019;
                    }
                }

                copyLinCmt(neq, ind, op, yp);
                const char* err_msg = "ros4 failed";
                postSolve(neq, &istate, ind->rc, &idx, yp, &err_msg, 11, false, ind, op, rx);
                if (*(ind->rc) < 0) localBadSolve = 1;
                ind->extraDoseNewXout = xout;
              }
              xp = ind->extraDoseNewXout;
            }
        }
        if (!localBadSolve && !isSameTime(xout, xp)) {
          preSolve(op, ind, xp, xout, yp);
          
          if (neqOde > 0) {
              state_type state(neqOde);
              double dt = (ind->autoHcur > 0.0) ? ind->autoHcur
                  : (op->HMIN > 0.0) ? op->HMIN : 1e-4;
              if (xout < xp) dt = -dt;
              sys.first.xout_ = xout;
              sys.first.sign_ = (dt > 0) ? 1 : -1;
              try {
                  // delay models always use the dense stepper (even toward
                  // non-observation events) so every step is recorded into the
                  // delay history with no gaps.
                  if ((op->useDense && isObs(getEvid(ind, ind->ix[i]))) || ind->delayHistOn) {
                      if (!stepper_initialized) {

                          std::copy(yp, yp + neqOde, state.begin());
                          dense_stepper.initialize(state, xp, dt);

                          stepper_initialized = true;
                      }
                      if (xout > xp) {
                          while (dense_stepper.current_time() < xout) {
                              dense_stepper.do_step(sys);
                              if (ind->err) { if (ind->rc[0] == 0) ind->rc[0] = -2019; break; }
                              recordDelayStep();
                          }
                      } else {
                          while (dense_stepper.current_time() > xout) {
                              dense_stepper.do_step(sys);
                              if (ind->err) { if (ind->rc[0] == 0) ind->rc[0] = -2019; break; }
                              recordDelayStep();
                          }
                      }

                      if (!ind->err) {
                          dense_stepper.calc_state(xout, state);
                          std::copy(state.begin(), state.end(), yp);
                      }

                  } else {

                      std::copy(yp, yp + neqOde, state.begin());
                      boost::numeric::odeint::integrate_adaptive(

                          stepper, sys, state, xp, xout, dt, error_checker(ind, ind->rc, op->mxstep));
                      stepper_initialized = false;
                      if (!ind->err) std::copy(state.begin(), state.end(), yp);
                  }
              } catch(const std::exception& e) {
                  if (ind->rc[0] == 0) ind->rc[0] = -2019;
              }
          }

          copyLinCmt(neq, ind, op, yp);
          const char* err_msg = "ros4 failed";
          postSolve(neq, &istate, ind->rc, &i, yp, &err_msg, 11, true, ind, op, rx);
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
        stepper_initialized = false;
      } else if (handleEvid1(&i, rx, neq, yp, &xout)){
        handleSS(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                 xp, ind->id, &i, ind->n_all_times, &istate, op, ind, u_inis, ctx);
        if (ind->wh0 == EVID0_OFF){
          ind->solve[ind->cmt] = op->inits[ind->cmt];
        }
        if (rx->istateReset) istate = 1;
        xp = xout;
        stepper_initialized = false;
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
  // release this subject's delay history (only needed during its own solve)
  if (ind->delayHist != NULL) {
    free(ind->delayHist);
    ind->delayHist = NULL;
    ind->delayHistCap = 0;
    ind->delayHistStride = 0;
    ind->delayHistNeq = 0;
  }
  ind->delayHistN = 0;
  ind->delayHistOn = 0;
  ind->solveTime += ((double)(clock() - t0))/CLOCKS_PER_SEC;
}

extern "C" void ind_ros4(rx_solve *rx, int solveid,
                          t_dydt c_dydt, t_update_inis u_inis){
  rx_solving_options *op = rx->op;
  int neq[2];
  neq[0] = op->neq;
  neq[1] = 0;
  ind_ros4_0(rx, op, solveid, neq, c_dydt, u_inis);
}

extern "C" void par_ros4(rx_solve *rx){
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
      ind_ros4_0(rx, op, solveid, neq, dydt, update_inis);
      
      if (op->badSolve) {
#ifdef _OPENMP
#pragma omp atomic write
#endif
        abort = 1;
      }
    }
  }
}

extern "C" void ros4_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind) {
  int eff = rxEffNeq(ind, op);
  int neqOde = eff - op->numLin - op->numLinSens;

  

  typedef boost::numeric::ublas::vector<double> state_type;
  typedef boost::numeric::ublas::matrix<double> matrix_type;
  typedef boost::numeric::odeint::rosenbrock4<double> stepper_base_type;
  typedef boost::numeric::odeint::rosenbrock4_controller<stepper_base_type> error_stepper_type;
  error_stepper_type stepper(ind->atol2[0], ind->rtol2[0], stepper_base_type());
  
  auto sys = make_rxode2_system_ros4(ind, dydt, calc_jac, neq);
  
  
  if (neqOde > 0) {
      state_type state(neqOde);
      double dt = (ind->autoHcur > 0.0) ? ind->autoHcur
                  : (op->HMIN > 0.0) ? op->HMIN : 1e-4;
      if (xout < *xp) dt = -dt;
      sys.first.xout_ = xout;
      sys.first.sign_ = (dt > 0) ? 1 : -1;
      try {
          std::copy(yp, yp + neqOde, state.begin());
          boost::numeric::odeint::integrate_adaptive(
              stepper, sys, state, *xp, xout, dt, error_checker(ind, ind->rc, op->mxstep));
          if (!ind->err) std::copy(state.begin(), state.end(), yp);
      } catch(const std::exception& e) {
          if (ind->rc[0] == 0) ind->rc[0] = -2019;
          *istate = -1;
          return;
      }
      if (ind->rc[0] < 0 || ind->err) {
          *istate = -1;
          return;
      }
  }
  *xp = xout;
  *istate = 1;
}

#endif // IN_PAR_SOLVE
