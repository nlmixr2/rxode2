#ifndef ODE_SSP3_BRIDGE_H_
#define ODE_SSP3_BRIDGE_H_

// RxSsp3: bridges rxode2's t_dydt interface into libode's OdeSsp3.
// Design mirrors RxTrapz exactly — see ode_trapz_bridge.h for commentary.

#include "ode/ode_ssp_3.h"

class RxSsp3 : public ode::OdeSsp3 {
    t_dydt                  dydt_;
    int                    *full_neq_;
    rx_solving_options_ind *ind_;
    double                  t_stage_;

public:
    RxSsp3(t_dydt dydt, int *full_neq, int neqOde,
           rx_solving_options_ind *ind, double *yp)
        : ode::OdeSsp3((unsigned long)neqOde),
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

        // Update: y = y + dt*(b1*k1 + b2*k2 + b3*k3)
        for (unsigned long i = 0; i < neq_; i++)
            sol_[i] += dt * (b1 * k_[0][i] + b2 * k_[1][i] + b3 * k_[2][i]);
    }
};

#endif // ODE_SSP3_BRIDGE_H_
