#ifndef ODE_EULER_BRIDGE_H_
#define ODE_EULER_BRIDGE_H_

#include "ode/ode_euler.h"

class RxEuler : public ode::OdeEuler {
    t_dydt                  dydt_;
    int                    *full_neq_;
    rx_solving_options_ind *ind_;
    double                  t_stage_;

public:
    RxEuler(t_dydt dydt, int *full_neq, int neqOde,
            rx_solving_options_ind *ind, double *yp)
        : ode::OdeEuler((unsigned long)neqOde),
          dydt_(dydt), full_neq_(full_neq), ind_(ind), t_stage_(0.0)
    {
        set_sol_external(yp);
    }

protected:
    void ode_fun(double * __restrict__ solin, double * __restrict__ fout) override {
        if (ind_->err != 0) {
            for (unsigned long i = 0; i < neq_; i++) fout[i] = 0.0;
            return;
        }
        dydt_(full_neq_, t_stage_, solin, fout);
        for (unsigned long i = 0; i < neq_; i++) {
            if (!std::isfinite(fout[i])) {
                ind_->err = 1;
                for (unsigned long j = 0; j < neq_; j++) fout[j] = 0.0;
                return;
            }
        }
    }

    void step_(double dt) override {
        t_stage_ = t_;
        ode_fun_(sol_, k_[0]);
        for (unsigned long i = 0; i < neq_; i++)
            sol_[i] += dt * k_[0][i];
    }
};

#endif // ODE_EULER_BRIDGE_H_
