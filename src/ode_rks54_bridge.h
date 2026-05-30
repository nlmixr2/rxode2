#ifndef ODE_RKS54_BRIDGE_H_
#define ODE_RKS54_BRIDGE_H_

#include "ode/ode_rks54.h"

class RxRks54 : public ode::OdeRks54 {
    t_dydt                  dydt_;
    int                    *full_neq_;
    rx_solving_options_ind *ind_;
    double                  t_stage_;
    long unsigned           max_steps_;

public:
    RxRks54(t_dydt dydt, int *full_neq, int neqOde,
            rx_solving_options_ind *ind, double *yp,
            long unsigned max_steps)
        : ode::OdeRks54((unsigned long)neqOde),
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
        for (unsigned long i = 0; i < neq_; i++) {
            if (!std::isfinite(fout[i])) {
                ind_->err = 1;
                for (unsigned long j = 0; j < neq_; j++) fout[j] = 0.0;
                return;
            }
        }
    }



    void step_(double dt) override {
        // Stage 1 (FSAL: k_[0] = f(t, y))
        t_stage_ = t_;
        ode_fun_(sol_, k_[0]);
        // Stage 2
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a21*k_[0][i]);
        t_stage_ = t_ + c2 * dt;
        ode_fun_(soltemp_, k_[1]);
        // Stage 3
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a31*k_[0][i]+a32*k_[1][i]);
        t_stage_ = t_ + c3 * dt;
        ode_fun_(soltemp_, k_[2]);
        // Stage 4
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a41*k_[0][i]+a42*k_[1][i]+a43*k_[2][i]);
        t_stage_ = t_ + c4 * dt;
        ode_fun_(soltemp_, k_[3]);
        // Stage 5
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a51*k_[0][i]+a52*k_[1][i]+a53*k_[2][i]+a54*k_[3][i]);
        t_stage_ = t_ + c5 * dt;
        ode_fun_(soltemp_, k_[4]);
        // Stage 6
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a61*k_[0][i]+a62*k_[1][i]+a63*k_[2][i]+a64*k_[3][i]+a65*k_[4][i]);
        t_stage_ = t_ + dt;
        ode_fun_(soltemp_, k_[5]);
        // Primary (higher-order) solution -> sol_
        for (unsigned long i = 0; i < neq_; i++)
            sol_[i] = sol_[i]+dt*(b1*k_[0][i]+b3*k_[2][i]+b4*k_[3][i]+b5*k_[4][i]+b6*k_[5][i]);
        // Stage 7 (FSAL: k_[6] = f(t+dt, y_new))
        t_stage_ = t_ + dt;
        ode_fun_(sol_, k_[6]);
        // Embedded (lower-order) solution -> solemb_
        for (unsigned long i = 0; i < neq_; i++)
            solemb_[i] = sol_[i] + dt * ((d1 - b1) * k_[0][i] + (d3 - b3) * k_[2][i] + (d4 - b4) * k_[3][i] + (d5 - b5) * k_[4][i] + (d6 - b6) * k_[5][i] + (d7 - b7) * k_[6][i]);
    }

    void after_step(double) override {
        if (ind_->err != 0) throw std::runtime_error("rks54: ode error");
        if (nstep_ > max_steps_) {
            if (ind_->rc[0] == 0) ind_->rc[0] = -2019;
            ind_->err = 1;
            throw std::runtime_error("rks54: max steps exceeded");
        }
    }
};

#endif // ODE_RKS54_BRIDGE_H_
