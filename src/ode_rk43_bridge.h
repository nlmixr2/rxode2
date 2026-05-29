#ifndef ODE_RK43_BRIDGE_H_
#define ODE_RK43_BRIDGE_H_

// RxRk43: bridges rxode2's t_dydt interface into libode's OdeRK43.
// Design mirrors RxRkf32 exactly — see ode_rkf32_bridge.h for commentary.
// 5-stage 4(3) FSAL: stages 1-4 advance sol_ (4th order); stage 5 (FSAL,
// k5=f(t+dt,y_new)) feeds the 3rd-order embedded error estimate in solemb_.

#include "ode/ode_rk_43.h"

class RxRk43 : public ode::OdeRK43 {
    t_dydt                  dydt_;
    int                    *full_neq_;
    rx_solving_options_ind *ind_;
    double                  t_stage_;
    long unsigned           max_steps_;

public:
    RxRk43(t_dydt dydt, int *full_neq, int neqOde,
           rx_solving_options_ind *ind, double *yp,
           long unsigned max_steps)
        : ode::OdeRK43((unsigned long)neqOde),
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
        // Detect NaN/Inf in derivatives (e.g. from NA parameters).
        // Mirror rxode2's other solvers: mark the solve bad and return zeros
        // so the adaptive loop exits cleanly on the next after_step() check.
        for (unsigned long i = 0; i < neq_; i++) {
            if (!std::isfinite(fout[i])) {
                ind_->err = 1;
                for (unsigned long j = 0; j < neq_; j++) fout[j] = 0.0;
                return;
            }
        }
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

        // Stage 4: y4 = y + dt*(a41*k1 + a42*k2 + a43*k3)
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a41 * k_[0][i] + a42 * k_[1][i] + a43 * k_[2][i]);
        t_stage_ = t_ + c4 * dt;
        ode_fun_(soltemp_, k_[3]);

        // 4th-order primary update — sol_ is now y_new
        for (unsigned long i = 0; i < neq_; i++)
            sol_[i] += dt * (b1 * k_[0][i] + b2 * k_[1][i] + b3 * k_[2][i] + b4 * k_[3][i]);

        // Stage 5 (FSAL): k5 = f(t+dt, y_new); a5j = bj
        t_stage_ = t_ + c5 * dt;
        ode_fun_(sol_, k_[4]);

        // 3rd-order embedded using adjusted difference from y_new:
        // solemb_ = y_old + dt*(d1*k1+d2*k2+d3*k3+d5*k5)
        //         = y_new + dt*((d1-b1)*k1+(d2-b2)*k2+(d3-b3)*k3+(0-b4)*k4+d5*k5)
        for (unsigned long i = 0; i < neq_; i++)
            solemb_[i] = sol_[i] + dt * ((d1 - b1) * k_[0][i] + (d2 - b2) * k_[1][i]
                                         + (d3 - b3) * k_[2][i] + (     -b4) * k_[3][i]
                                         +        d5  * k_[4][i]);
    }

    void after_step(double /*t*/) override {
        if (ind_->err != 0) throw std::runtime_error("rk43: ode error");
        if (nstep_ > max_steps_) {
            if (ind_->rc[0] == 0) ind_->rc[0] = -2019;
            ind_->err = 1;
            throw std::runtime_error("rk43: max steps exceeded");
        }
    }
};

#endif // ODE_RK43_BRIDGE_H_
