#ifndef ODE_TRAPZ_BRIDGE_H_
#define ODE_TRAPZ_BRIDGE_H_

// RxTrapz: bridges rxode2's t_dydt interface into libode's OdeTrapz.
//
// Zero-copy: set_sol_external(yp) makes OdeBase::sol_ point directly at the
// rxode2 yp buffer.  solve_fixed() modifies it in-place; no memcpy needed
// after the solve (mirrors zero_copy_state semantics from odeinter.h).
//
// Size: the solver is constructed with neqOde (= neq[0] - numLin - numLinSens),
// so only the true ODE state variables are advanced.  Linear compartments are
// handled separately by copyLinCmt(), exactly as in rk4 / ck54.
//
// Time injection: libode is autonomous (ode_fun has no t parameter), but
// rxode2's dydt is time-dependent.  We override step_() to set t_stage_
// before each ode_fun_ call so ode_fun() can forward the correct stage time.

#include "ode/ode_trapz.h"
// t_dydt and rx_solving_options_ind are already declared by the time this
// header is pulled in via trapz.cpp (which is #include'd by par_solve.cpp
// after all rxode2 headers).  Do not re-include rxode2parseStruct.h here
// since it has no include guard and would cause redefinition errors.

class RxTrapz : public ode::OdeTrapz {
    t_dydt                  dydt_;
    int                    *full_neq_;  // full neq array (full_neq_[0] = eff), for dydt_ signature
    rx_solving_options_ind *ind_;
    double                  t_stage_;

public:
    // full_neq — full neq array (neq[0] = eff), forwarded unchanged to dydt_
    // neqOde   — number of true ODE states (solver size, excludes linear cmts)
    // yp       — state buffer; solver works on yp[0..neqOde-1] in-place
    RxTrapz(t_dydt dydt, int *full_neq, int neqOde,
            rx_solving_options_ind *ind, double *yp)
        : ode::OdeTrapz((unsigned long)neqOde),
          dydt_(dydt), full_neq_(full_neq), ind_(ind), t_stage_(0.0)
    {
        set_sol_external(yp);
    }

protected:
    void ode_fun(double *solin, double *fout) override {
        if (ind_->err != 0) {
            for (unsigned long i = 0; i < neq_; i++) fout[i] = 0.0;
            return;
        }
        dydt_(full_neq_, t_stage_, solin, fout);
    }

    // Override step_() to inject the correct stage time into each ode_fun_ call.
    // OdeBase::t_ holds the time at the start of the step.  t_stage_ carries the
    // per-stage time to ode_fun() since libode's autonomous interface has no t arg.
    void step_(double dt) override {
        t_stage_ = t_;                              // stage 1: current time
        ode_fun_(sol_, k_[0]);

        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * a21 * k_[0][i];

        t_stage_ = t_ + c2 * dt;                   // stage 2: t + c2*dt
        ode_fun_(soltemp_, k_[1]);

        for (unsigned long i = 0; i < neq_; i++)
            sol_[i] += dt * (b1 * k_[0][i] + b2 * k_[1][i]);
    }
};

#endif // ODE_TRAPZ_BRIDGE_H_
