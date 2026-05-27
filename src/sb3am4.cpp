#ifdef IN_PAR_SOLVE
#undef min
#undef max
#include "odeinter.h"

#include <boost/numeric/odeint/stepper/symplectic_rkn_sb3a_m4_mclachlan.hpp>
#include <vector>
#include <algorithm>
#include <cmath>

struct sb3am4_system {
    rx_solving_options_ind *ind_;
    t_dydt c_dydt_;
    int* neq_;
    int neqOde_;
    int N_;
    mutable double t_;
    mutable std::vector<double> y_temp_;
    mutable std::vector<double> dy_temp_;

    // We keep pointers to the current values of coordinate and momentum.
    mutable const double* current_q_;
    mutable const double* current_p_;

    sb3am4_system(rx_solving_options_ind *ind, t_dydt c_dydt, int* neq)
        : ind_(ind), c_dydt_(c_dydt), neq_(neq), neqOde_(0), N_(0), t_(0.0), current_q_(nullptr), current_p_(nullptr) {
        int eff = rxEffNeq(ind, ind->op);
        neqOde_ = eff - ind->op->numLin - ind->op->numLinSens;
        N_ = (neqOde_ + 1) / 2;
        y_temp_.resize(2 * N_, 0.0);
        dy_temp_.resize(2 * N_, 0.0);
    }

    struct coor_func_t {
        const sb3am4_system& parent_;
        coor_func_t(const sb3am4_system& parent) : parent_(parent) {}

        void operator()(const zero_copy_state& p, zero_copy_state& dqdt) const {
            parent_.current_p_ = p.data_;
            if (parent_.current_q_) {
                std::copy(parent_.current_q_, parent_.current_q_ + parent_.N_, parent_.y_temp_.begin());
            } else {
                std::fill_n(parent_.y_temp_.begin(), parent_.N_, 0.0);
            }
            std::copy(p.data_, p.data_ + parent_.N_, parent_.y_temp_.begin() + parent_.N_);

            for (int i = parent_.neqOde_; i < 2 * parent_.N_; ++i) {
                parent_.dy_temp_[i] = 0.0;
            }

            parent_.c_dydt_(parent_.neq_, parent_.t_, parent_.y_temp_.data(), parent_.dy_temp_.data());

            std::copy(parent_.dy_temp_.begin(), parent_.dy_temp_.begin() + parent_.N_, dqdt.data_);
        }
    };

    struct momentum_func_t {
        const sb3am4_system& parent_;
        momentum_func_t(const sb3am4_system& parent) : parent_(parent) {}

        void operator()(const zero_copy_state& q, zero_copy_state& dpdt) const {
            parent_.current_q_ = q.data_;
            std::copy(q.data_, q.data_ + parent_.N_, parent_.y_temp_.begin());
            if (parent_.current_p_) {
                std::copy(parent_.current_p_, parent_.current_p_ + parent_.N_, parent_.y_temp_.begin() + parent_.N_);
            } else {
                std::fill_n(parent_.y_temp_.begin() + parent_.N_, parent_.N_, 0.0);
            }

            for (int i = parent_.neqOde_; i < 2 * parent_.N_; ++i) {
                parent_.dy_temp_[i] = 0.0;
            }

            parent_.c_dydt_(parent_.neq_, parent_.t_, parent_.y_temp_.data(), parent_.dy_temp_.data());

            std::copy(parent_.dy_temp_.begin() + parent_.N_, parent_.dy_temp_.end(), dpdt.data_);
        }
    };

    coor_func_t get_coor_func() const { return coor_func_t(*this); }
    momentum_func_t get_momentum_func() const { return momentum_func_t(*this); }

    auto get_pair() const {
        return std::make_pair(get_coor_func(), get_momentum_func());
    }
};

static inline void sb3am4_do_steps(rx_solving_options_ind *ind, rx_solving_options *op, sb3am4_system& sys, std::pair<zero_copy_state, zero_copy_state>& state, double xp, double xout) {
  typedef boost::numeric::odeint::symplectic_rkn_sb3a_m4_mclachlan<zero_copy_state> stepper_type;
  stepper_type stepper;
  double t = xp;
  double dt = op->HMIN > 0.0 ? op->HMIN : 0.0001;
  if (dt <= 0.0) dt = 0.0001;
  if (std::abs(xout - xp) / dt > op->mxstep) {
      dt = std::abs(xout - xp) / (double)(op->mxstep - 10);
  }
  int sign = (xout > xp) ? 1 : -1;
  dt = sign * dt;

  error_checker check(ind, ind->rc, op->mxstep);

  while ( (sign > 0 && t < xout) || (sign < 0 && t > xout) ) {
    double current_dt = dt;
    if ( (sign > 0 && t + dt > xout) || (sign < 0 && t + dt < xout) ) {
      current_dt = xout - t;
    }

    try {
      sys.t_ = t;
      sys.current_q_ = state.first.data_;
      sys.current_p_ = state.second.data_;
      stepper.do_step(sys.get_pair(), state, t, current_dt);
    } catch(const std::exception& e) {
      if (ind->rc[0] == 0) ind->rc[0] = -2019;
      ind->err = 1;
      break;
    }

    t += current_dt;
    check(state.first, t);
    if (ind->err != 0) break;
  }
}

extern "C" void ind_sb3am4_0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq,
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
  sb3am4_system sys(ind, c_dydt, neq);

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
                  int N = (neqOde + 1) / 2;
                  zero_copy_state q(N);
                  zero_copy_state p(N);
                  std::copy(yp, yp + N, q.begin());
                  std::copy(yp + N, yp + neqOde, p.begin());
                  if (2 * N > neqOde) {
                      p[N - 1] = 0.0;
                  }
                  auto state = std::make_pair(q, p);
                  sb3am4_do_steps(ind, op, sys, state, xp, ind->extraDoseNewXout);
                  std::copy(state.first.begin(), state.first.end(), yp);
                  std::copy(state.second.begin(), state.second.begin() + (neqOde - N), yp + N);
              }

              copyLinCmt(neq, ind, op, yp);
              const char* err_msg = "sb3am4 failed";
              postSolve(neq, &istate, ind->rc, &i, yp, &err_msg, 17, true, ind, op, rx);
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
                    int N = (neqOde + 1) / 2;
                    zero_copy_state q(N);
                    zero_copy_state p(N);
                    std::copy(yp, yp + N, q.begin());
                    std::copy(yp + N, yp + neqOde, p.begin());
                    if (2 * N > neqOde) {
                        p[N - 1] = 0.0;
                    }
                    auto state = std::make_pair(q, p);
                    sb3am4_do_steps(ind, op, sys, state, ind->extraDoseNewXout, xout);
                    std::copy(state.first.begin(), state.first.end(), yp);
                    std::copy(state.second.begin(), state.second.begin() + (neqOde - N), yp + N);
                }

                copyLinCmt(neq, ind, op, yp);
                const char* err_msg = "sb3am4 failed";
                postSolve(neq, &istate, ind->rc, &idx, yp, &err_msg, 17, false, ind, op, rx);
                if (*(ind->rc) < 0) localBadSolve = 1;
                ind->extraDoseNewXout = xout;
              }
              xp = ind->extraDoseNewXout;
            }
        }
        if (!localBadSolve && !isSameTime(xout, xp)) {
          preSolve(op, ind, xp, xout, yp);

          if (neqOde > 0) {
              int N = (neqOde + 1) / 2;
              zero_copy_state q(N);
              zero_copy_state p(N);
              std::copy(yp, yp + N, q.begin());
              std::copy(yp + N, yp + neqOde, p.begin());
              if (2 * N > neqOde) {
                  p[N - 1] = 0.0;
              }
              auto state = std::make_pair(q, p);
              sb3am4_do_steps(ind, op, sys, state, xp, xout);
              std::copy(state.first.begin(), state.first.end(), yp);
              std::copy(state.second.begin(), state.second.begin() + (neqOde - N), yp + N);
          }

          copyLinCmt(neq, ind, op, yp);
          const char* err_msg = "sb3am4 failed";
          postSolve(neq, &istate, ind->rc, &i, yp, &err_msg, 17, true, ind, op, rx);
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

extern "C" void ind_sb3am4(rx_solve *rx, int solveid,
                          t_dydt c_dydt, t_update_inis u_inis){
  rx_solving_options *op = rx->op;
  int neq[2];
  neq[0] = op->neq;
  neq[1] = 0;
  ind_sb3am4_0(rx, op, solveid, neq, c_dydt, u_inis);
}

extern "C" void par_sb3am4(rx_solve *rx){
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
      ind_sb3am4_0(rx, op, solveid, neq, dydt, update_inis);

      if (op->badSolve) {
#ifdef _OPENMP
#pragma omp atomic write
#endif
        abort = 1;
      }
    }
  }
}

extern "C" void sb3am4_solveWith1Pt(int *neq, double *yp, double *xp, double xout,
 int *istate, rx_solving_options *op, rx_solving_options_ind *ind) {
  int eff = rxEffNeq(ind, op);
  int neqOde = eff - op->numLin - op->numLinSens;

  if (neqOde > 0) {
      int N = (neqOde + 1) / 2;
      zero_copy_state q(N);
      zero_copy_state p(N);
      std::copy(yp, yp + N, q.begin());
      std::copy(yp + N, yp + neqOde, p.begin());
      if (2 * N > neqOde) {
          p[N - 1] = 0.0;
      }
      auto state = std::make_pair(q, p);
      sb3am4_system sys(ind, dydt, neq);
      sb3am4_do_steps(ind, op, sys, state, *xp, xout);
      std::copy(state.first.begin(), state.first.end(), yp);
      std::copy(state.second.begin(), state.second.begin() + (neqOde - N), yp + N);
      if (ind->rc[0] < 0) {
          *istate = -1;
          return;
      }
  }
  *xp = xout;
  *istate = 1;
}

#endif // IN_PAR_SOLVE
