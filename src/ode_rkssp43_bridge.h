#ifndef ODE_RKSSP43_BRIDGE_H_
#define ODE_RKSSP43_BRIDGE_H_

#include "ode/ode_rkssp43.h"

class RxRkssp43 : public ode::OdeRkssp43 {
    t_dydt                  dydt_;
    int                    *full_neq_;
    rx_solving_options_ind *ind_;
    double                  t_stage_;
    long unsigned           max_steps_;

public:
    RxRkssp43(t_dydt dydt, int *full_neq, int neqOde,
            rx_solving_options_ind *ind, double *yp,
            long unsigned max_steps)
        : ode::OdeRkssp43((unsigned long)neqOde),
          dydt_(dydt), full_neq_(full_neq), ind_(ind),
          t_stage_(0.0), max_steps_(max_steps)
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
        // SSP 4-stage 3(2) - iterative pattern (Conde et al. 2018)
        unsigned long i;
        double half_dt = 0.5 * dt;
        t_stage_ = t_;
        ode_fun_(sol_, k_[0]);
        for (i = 0; i < neq_; i++) soltemp_[i] = sol_[i] + half_dt * k_[0][i];
        t_stage_ = t_ + half_dt;
        ode_fun_(soltemp_, k_[0]);
        for (i = 0; i < neq_; i++) soltemp_[i] = soltemp_[i] + half_dt * k_[0][i];
        t_stage_ = t_ + dt;
        ode_fun_(soltemp_, k_[0]);
        for (i = 0; i < neq_; i++) soltemp_[i] = soltemp_[i] + half_dt * k_[0][i];
        for (i = 0; i < neq_; i++) k_[1][i] = (sol_[i] + 2.0*soltemp_[i]) / 3.0;
        for (i = 0; i < neq_; i++) soltemp_[i] = (2.0*sol_[i] + soltemp_[i]) / 3.0;
        t_stage_ = t_ + half_dt;
        ode_fun_(soltemp_, k_[0]);
        for (i = 0; i < neq_; i++) sol_[i] = soltemp_[i] + half_dt * k_[0][i];
        for (i = 0; i < neq_; i++) solemb_[i] = k_[1][i];
    }

    void after_step(double) override {
        if (ind_->err != 0) throw std::runtime_error("rkssp43: ode error");
        if (nstep_ > max_steps_) {
            if (ind_->rc[0] == 0) ind_->rc[0] = -2019;
            ind_->err = 1;
            throw std::runtime_error("rkssp43: max steps exceeded");
        }
    }
};

#endif // ODE_RKSSP43_BRIDGE_H_
