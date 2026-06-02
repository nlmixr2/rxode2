#ifndef ODE_IMPLICIT_BRIDGE_H_
#define ODE_IMPLICIT_BRIDGE_H_

// RxImplicit<OdeSolver>: generic rxode2 bridge for all libode implicit methods.
// Overrides ode_fun() and ode_jac() so the Newton/Rosenbrock machinery inside
// OdeSolver::step_() dispatches through rxode2's dydt/calc_jac interfaces.
// The rxode2 bridge threads stage-specific evaluation times through libode's
// autonomous APIs so time-dependent models remain correct.

#include <stdexcept>
#include <vector>
#include <cmath>

template <class OdeSolver>
class RxImplicit : public OdeSolver {
    t_dydt                  dydt_;
    t_calc_jac              calc_jac_;
    int                    *full_neq_;
    rx_solving_options_ind *ind_;
    long unsigned           max_steps_;
    std::vector<double>     jac_flat_;
    double                  ode_eval_time_;
    double                  jac_eval_time_;
    bool                    has_ode_eval_time_;
    bool                    has_jac_eval_time_;

public:
    RxImplicit(t_dydt dydt, t_calc_jac jac, int *full_neq, int neqOde,
               rx_solving_options_ind *ind, double *yp, long unsigned max_steps)
        : OdeSolver((unsigned long)neqOde),
          dydt_(dydt), calc_jac_(jac), full_neq_(full_neq), ind_(ind),
          max_steps_(max_steps),
          jac_flat_((size_t)neqOde * (size_t)neqOde, 0.0),
          ode_eval_time_(0.0), jac_eval_time_(0.0),
          has_ode_eval_time_(false), has_jac_eval_time_(false)
    {
        // Use copy semantics: Newton-based IRK methods cache sol_ at
        // construction time, so set_sol_external would leave stale pointers.
        this->set_sol(yp);
    }

    // Prime dt_ before the first step so Newton functions see a nonzero dt.
    // OdeBase::step() sets dt_ only AFTER step_() returns, but Newton reads
    // *dt_ = &integrator_->dt_ DURING step_() — so dt_=0 on step 1 without this.
    void prime_dt(double dt) { this->dt_ = dt; }

    // Override step_() to pre-set dt_ before the solver's step_() runs.
    // OdeBase::step() sets dt_ only AFTER step_() returns, but the Newton
    // bridge reads *dt_ during step_().  For adaptive methods (SDIRK43) dt
    // changes every step, so the lag would always give the wrong value.
    // Calling OdeSolver::step_() (non-virtual, scope-qualified) ensures the
    // concrete solver's step logic runs with dt_ correctly primed.
    void step_(double dt) override {
        this->dt_ = dt;
        OdeSolver::step_(dt);
    }

protected:
    void ode_fun(double *solin, double *fout) override {
        double eval_time = has_ode_eval_time_ ? ode_eval_time_ : this->t_;
        if (ind_->err != 0) {
            for (unsigned long i = 0; i < this->neq_; i++) fout[i] = 0.0;
            return;
        }
        dydt_(full_neq_, eval_time, solin, fout);
        for (unsigned long i = 0; i < this->neq_; i++) {
            if (!std::isfinite(fout[i])) {
                ind_->err = 1;
                for (unsigned long j = 0; j < this->neq_; j++) fout[j] = 0.0;
                return;
            }
        }
    }

    void ode_jac(double *solin, double **Jout) override {
        if (calc_jac_ == NULL) {
            OdeSolver::ode_jac(solin, Jout);
            return;
        }
        double eval_time = has_jac_eval_time_ ? jac_eval_time_ :
            (has_ode_eval_time_ ? ode_eval_time_ : this->t_);
        std::fill(jac_flat_.begin(), jac_flat_.end(), 0.0);
        unsigned int nrowpd = (unsigned int)this->neq_;
        calc_jac_(full_neq_, eval_time, solin, jac_flat_.data(), nrowpd);
        for (unsigned long r = 0; r < this->neq_; r++)
            for (unsigned long c = 0; c < this->neq_; c++)
                Jout[r][c] = jac_flat_[r * this->neq_ + c];
    }

    void after_step(double /*t*/) override {
        if (ind_->err != 0) throw std::runtime_error("implicit solver: ode error");
        if (this->nstep_ > max_steps_) {
            if (ind_->rc[0] == 0) ind_->rc[0] = -2019;
            ind_->err = 1;
            throw std::runtime_error("implicit solver: max steps exceeded");
        }
    }

    void set_ode_eval_time(double t) override {
        ode_eval_time_ = t;
        has_ode_eval_time_ = true;
    }

    void clear_ode_eval_time() override {
        has_ode_eval_time_ = false;
    }

    void set_jac_eval_time(double t) override {
        jac_eval_time_ = t;
        has_jac_eval_time_ = true;
    }

    void clear_jac_eval_time() override {
        has_jac_eval_time_ = false;
    }
};

// ── Template helpers ─────────────────────────────────────────────────────────

template <class OdeSolver>
static inline void implicit_do_steps(rx_solving_options_ind *ind, rx_solving_options *op,
                                      t_dydt c_dydt, t_calc_jac c_jac, int *neq, double *yp,
                                      double xp, double xout) {
    double tint = xout - xp;
    if (tint == 0.0) return;
    double dt = op->HMIN > 0.0 ? op->HMIN : 0.01;
    if (dt <= 0.0) dt = 0.01;
    if (dt > std::fabs(tint)) dt = std::fabs(tint);
    if (tint < 0.0) dt = -dt;

    int neqOde = neq[0] - op->numLin - op->numLinSens;

    // For models with in-ODE adaptive dosing (indOwnAlloc), some implicit methods
    // evaluate their first stage at t + c1*h (c1 > 0, e.g. SDIRK, Gauss, RadauIIA).
    // When _atEventTime=1 is consumed at that offset time, evid_()/bolus()/etc. would
    // push doses using the wrong t value.  Pre-evaluate dydt at the canonical start
    // time xp so that dose pushing happens at the correct event time before the solver
    // takes any stage that evaluates at xp + c1*h.
    if (op->indOwnAlloc && ind->_atEventTime) {
        std::vector<double> tmp_f((size_t)neqOde, 0.0);
        c_dydt(neq, xp, yp, tmp_f.data());   // consumes _atEventTime; pushes doses at xp
    }

    RxImplicit<OdeSolver> solver(c_dydt, c_jac, neq, neqOde, ind, yp, (long unsigned)op->mxstep);
    solver.set_quiet(true);
    solver.set_t(xp);
    solver.prime_dt(std::fabs(dt));
    solver.set_abstol(op->ATOL);
    solver.set_reltol(op->RTOL);
    try {
        solver.solve_adaptive(std::fabs(tint), std::fabs(dt), true);
    } catch (...) {
        if (ind->rc[0] == 0) ind->rc[0] = -2019;
        ind->err = 1;
    }
    // Copy final state back to yp (required because we used set_sol instead of
    // set_sol_external to avoid Newton bridge cache staleness).
    double *result = solver.get_sol();
    for (int i = 0; i < neqOde; i++) yp[i] = result[i];
}

template <class OdeSolver>
static void ind_implicit_0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq,
                             t_dydt c_dydt, t_update_inis u_inis, const char *err_msg) {
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
                            implicit_do_steps<OdeSolver>(ind, op, c_dydt, calc_jac, neq, yp, xp, ind->extraDoseNewXout);
                        copyLinCmt(neq, ind, op, yp);
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
                                implicit_do_steps<OdeSolver>(ind, op, c_dydt, calc_jac, neq, yp, ind->extraDoseNewXout, xout);
                            copyLinCmt(neq, ind, op, yp);
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
                        implicit_do_steps<OdeSolver>(ind, op, c_dydt, calc_jac, neq, yp, xp, xout);
                    copyLinCmt(neq, ind, op, yp);
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
                if (ind->wh0 == EVID0_OFF) ind->solve[ind->cmt] = op->inits[ind->cmt];
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
                if (refreshLagTimesIfNeeded(rx, ind, yp, i + 1, xout)) ind->mainSorted = 0;
            }
            updateSolve(ind, op, neq, xout, i, ind->n_all_times);
            if (_mtime_requeued) i--;
        }
        ind->solvedIdx = i;
    }
    ind->solveTime += ((double)(clock() - t0))/CLOCKS_PER_SEC;
}

#endif // ODE_IMPLICIT_BRIDGE_H_
