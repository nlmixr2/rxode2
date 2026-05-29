#ifndef ODE_RKF32_BRIDGE_H_
#define ODE_RKF32_BRIDGE_H_

// RxRkf32: bridges rxode2's t_dydt interface into libode's OdeRKF32.
//
// Unlike RxTrapz/RxSsp3 (fixed-step), OdeRKF32 is adaptive: each call uses
// solve_adaptive() so the solver picks step sizes guided by atol/rtol.
// after_step() enforces the rxode2 mxstep limit by throwing when exceeded,
// which is caught in rkf32_do_steps.

#include "ode/ode_rkf_32.h"

class RxRkf32 : public ode::OdeRKF32 {
    t_dydt                  dydt_;
    int                    *full_neq_;
    rx_solving_options_ind *ind_;
    double                  t_stage_;
    long unsigned           max_steps_;

public:
    RxRkf32(t_dydt dydt, int *full_neq, int neqOde,
            rx_solving_options_ind *ind, double *yp,
            long unsigned max_steps)
        : ode::OdeRKF32((unsigned long)neqOde),
          dydt_(dydt), full_neq_(full_neq), ind_(ind),
          t_stage_(0.0), max_steps_(max_steps)
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

    void step_(double dt) override {
        // Stage 1: k1 = f(t, y)
        t_stage_ = t_;
        ode_fun_(sol_, k_[0]);

        // Stage 2: y2 = y + dt*a21*k1
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * a21 * k_[0][i];
        t_stage_ = t_ + c2 * dt;
        ode_fun_(soltemp_, k_[1]);

        // Stage 3: y3 = y + dt*(a31*k1 + a32*k2)
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a31 * k_[0][i] + a32 * k_[1][i]);
        t_stage_ = t_ + c3 * dt;
        ode_fun_(soltemp_, k_[2]);

        // 2nd-order embedded (before sol_ is overwritten)
        for (unsigned long i = 0; i < neq_; i++)
            solemb_[i] = sol_[i] + dt * (d1 * k_[0][i] + d2 * k_[1][i] + d3 * k_[2][i]);

        // 3rd-order primary (b3 = 1 - b1 - b2)
        double b3 = 1.0 - b1 - b2;
        for (unsigned long i = 0; i < neq_; i++)
            sol_[i] += dt * (b1 * k_[0][i] + b2 * k_[1][i] + b3 * k_[2][i]);
    }

    // Enforce mxstep limit: throw to break out of solve_adaptive_().
    void after_step(double /*t*/) override {
        if (ind_->err != 0) throw std::runtime_error("rkf32: ode error");
        if (nstep_ > max_steps_) {
            if (ind_->rc[0] == 0) ind_->rc[0] = -2019;
            ind_->err = 1;
            throw std::runtime_error("rkf32: max steps exceeded");
        }
    }
};

#endif // ODE_RKF32_BRIDGE_H_
