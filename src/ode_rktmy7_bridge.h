#ifndef ODE_RKTMY7_BRIDGE_H_
#define ODE_RKTMY7_BRIDGE_H_

#include "ode/ode_rktmy7.h"

class RxRktmy7 : public ode::OdeRktmy7 {
    t_dydt                  dydt_;
    int                    *full_neq_;
    rx_solving_options_ind *ind_;
    double                  t_stage_;
    long unsigned           max_steps_;

public:
    RxRktmy7(t_dydt dydt, int *full_neq, int neqOde,
            rx_solving_options_ind *ind, double *yp,
            long unsigned max_steps)
        : ode::OdeRktmy7((unsigned long)neqOde),
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
        const double c1 = 7.81664651011384679726388269101836941032920932418419855605785254026460056610810379655e-2;
        // Stage 1
        t_stage_ = t_;
        ode_fun_(sol_, k_[0]);
        // Stage 2
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a21*k_[0][i]);
        t_stage_ = t_ + c1 * dt;
        ode_fun_(soltemp_, k_[1]);
        // Stage 3
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a31*k_[0][i]+a32*k_[1][i]);
        t_stage_ = t_ + c2 * dt;
        ode_fun_(soltemp_, k_[2]);
        // Stage 4
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a41*k_[0][i]+a43*k_[2][i]);
        t_stage_ = t_ + c3 * dt;
        ode_fun_(soltemp_, k_[3]);
        // Stage 5
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a51*k_[0][i]+a53*k_[2][i]+a54*k_[3][i]);
        t_stage_ = t_ + c4 * dt;
        ode_fun_(soltemp_, k_[4]);
        // Stage 6
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a61*k_[0][i]+a63*k_[2][i]+a64*k_[3][i]+a65*k_[4][i]);
        t_stage_ = t_ + c5 * dt;
        ode_fun_(soltemp_, k_[5]);
        // Stage 7
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a71*k_[0][i]+a73*k_[2][i]+a74*k_[3][i]+a75*k_[4][i]+a76*k_[5][i]);
        t_stage_ = t_ + c6 * dt;
        ode_fun_(soltemp_, k_[6]);
        // Stage 8
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a81*k_[0][i]+a83*k_[2][i]+a84*k_[3][i]+a85*k_[4][i]+a86*k_[5][i]+a87*k_[6][i]);
        t_stage_ = t_ + c7 * dt;
        ode_fun_(soltemp_, k_[7]);
        // Stage 9
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a91*k_[0][i]+a93*k_[2][i]+a94*k_[3][i]+a95*k_[4][i]+a96*k_[5][i]+a97*k_[6][i]+a98*k_[7][i]);
        t_stage_ = t_ + dt;
        ode_fun_(soltemp_, k_[8]);
        // Stage 10
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a101*k_[0][i]+a103*k_[2][i]+a104*k_[3][i]+a105*k_[4][i]+a106*k_[5][i]+a107*k_[6][i]+a108*k_[7][i]);
        t_stage_ = t_ + dt;
        ode_fun_(soltemp_, k_[9]);
        // Primary (higher-order) solution -> sol_
        for (unsigned long i = 0; i < neq_; i++)
            sol_[i] = sol_[i]+dt*(b1*k_[0][i]+b4*k_[3][i]+b5*k_[4][i]+b6*k_[5][i]+b7*k_[6][i]+b8*k_[7][i]+b9*k_[8][i]);
        // Embedded (lower-order) solution -> solemb_
        for (unsigned long i = 0; i < neq_; i++)
            solemb_[i] = sol_[i] + dt * ((d1 - b1) * k_[0][i] + (d4 - b4) * k_[3][i] + (d5 - b5) * k_[4][i] + (d6 - b6) * k_[5][i] + (d7 - b7) * k_[6][i] + (d8 - b8) * k_[7][i] + (d9 - b9) * k_[8][i] + (d10 - b10) * k_[9][i]);
    }

    void after_step(double) override {
        if (ind_->err != 0) throw std::runtime_error("rktmy7: ode error");
        if (nstep_ > max_steps_) {
            if (ind_->rc[0] == 0) ind_->rc[0] = -2019;
            ind_->err = 1;
            throw std::runtime_error("rktmy7: max steps exceeded");
        }
    }
};

#endif // ODE_RKTMY7_BRIDGE_H_
