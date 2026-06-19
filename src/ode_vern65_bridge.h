#ifndef ODE_VERN65_BRIDGE_H_
#define ODE_VERN65_BRIDGE_H_

// RxVern65: bridges rxode2's t_dydt interface into libode's OdeVern65.
// Design mirrors RxRk43/RxDoPri54 -- see those headers for commentary.
//
// Key difference from dop54: in Vern65, solemb_ and sol_ are BOTH computed
// from the OLD sol_ in the final step, so they are written in two separate
// loops here (first solemb_, then sol_ +=).  The FSAL stage k_[8] feeds only
// solemb_ (via d9), not sol_ (b9=0); k_[8] must therefore be computed before
// either update loop runs.

#include "ode/ode_vern_65.h"

class RxVern65 : public ode::OdeVern65 {
    t_dydt                  dydt_;
    int                    *full_neq_;
    rx_solving_options_ind *ind_;
    double                  t_stage_;
    long unsigned           max_steps_;

public:
    RxVern65(t_dydt dydt, int *full_neq, int neqOde,
             rx_solving_options_ind *ind, double *yp,
             long unsigned max_steps)
        : ode::OdeVern65((unsigned long)neqOde),
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
        // Stage 1
        t_stage_ = t_;
        ode_fun_(sol_, k_[0]);

        // Stage 2
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * a21 * k_[0][i];
        t_stage_ = t_ + c2 * dt;
        ode_fun_(soltemp_, k_[1]);

        // Stage 3
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a31 * k_[0][i] + a32 * k_[1][i]);
        t_stage_ = t_ + c3 * dt;
        ode_fun_(soltemp_, k_[2]);

        // Stage 4 (a42=0)
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a41 * k_[0][i] + a43 * k_[2][i]);
        t_stage_ = t_ + c4 * dt;
        ode_fun_(soltemp_, k_[3]);

        // Stage 5 (a52=0)
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a51 * k_[0][i] + a53 * k_[2][i] + a54 * k_[3][i]);
        t_stage_ = t_ + c5 * dt;
        ode_fun_(soltemp_, k_[4]);

        // Stage 6 (a62=0)
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a61 * k_[0][i] + a63 * k_[2][i]
                                         + a64 * k_[3][i] + a65 * k_[4][i]);
        t_stage_ = t_ + c6 * dt;
        ode_fun_(soltemp_, k_[5]);

        // Stage 7 (a72=0)
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a71 * k_[0][i] + a73 * k_[2][i]
                                         + a74 * k_[3][i] + a75 * k_[4][i]
                                         + a76 * k_[5][i]);
        t_stage_ = t_ + c7 * dt;
        ode_fun_(soltemp_, k_[6]);

        // Stage 8 (a82=0)
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a81 * k_[0][i] + a83 * k_[2][i]
                                         + a84 * k_[3][i] + a85 * k_[4][i]
                                         + a86 * k_[5][i] + a87 * k_[6][i]);
        t_stage_ = t_ + c8 * dt;
        ode_fun_(soltemp_, k_[7]);

        // Stage 9 (FSAL, a92=a93=0): a9j=bj so state = 6th-order solution
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a91 * k_[0][i] + a94 * k_[3][i]
                                         + a95 * k_[4][i] + a96 * k_[5][i]
                                         + a97 * k_[6][i] + a98 * k_[7][i]);
        t_stage_ = t_ + c9 * dt;
        ode_fun_(soltemp_, k_[8]);

        // solemb_ from OLD sol_ (5th-order embedded, uses FSAL k_[8] via d9)
        for (unsigned long i = 0; i < neq_; i++)
            solemb_[i] = sol_[i] + dt * (d1 * k_[0][i] + d4 * k_[3][i]
                                        + d5 * k_[4][i] + d6 * k_[5][i]
                                        + d8 * k_[7][i] + d9 * k_[8][i]);

        // sol_ update to 6th-order solution (b9=0, no k_[8])
        for (unsigned long i = 0; i < neq_; i++)
            sol_[i] += dt * (b1 * k_[0][i] + b4 * k_[3][i]
                           + b5 * k_[4][i] + b6 * k_[5][i]
                           + b7 * k_[6][i] + b8 * k_[7][i]);
    }

    void after_step(double /*t*/) override {
        if (ind_->err != 0) throw std::runtime_error("vern65: ode error");
        if (nstep_ > max_steps_) {
            if (ind_->rc[0] == 0) ind_->rc[0] = -2019;
            ind_->err = 1;
            throw std::runtime_error("vern65: max steps exceeded");
        }
    }
};

#endif // ODE_VERN65_BRIDGE_H_
