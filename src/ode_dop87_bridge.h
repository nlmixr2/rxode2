#ifndef ODE_DOP87_BRIDGE_H_
#define ODE_DOP87_BRIDGE_H_

// RxDoPri87: bridges rxode2's t_dydt interface into libode's OdeDoPri87.
// Design mirrors RxVern65/RxVern76 — 13 stages, solemb_ and sol_ both from
// OLD sol_ in separate final loops.  NaN detection and mxstep guard included.

#include "ode/ode_dopri_87.h"

class RxDoPri87 : public ode::OdeDoPri87 {
    t_dydt                  dydt_;
    int                    *full_neq_;
    rx_solving_options_ind *ind_;
    double                  t_stage_;
    long unsigned           max_steps_;

public:
    RxDoPri87(t_dydt dydt, int *full_neq, int neqOde,
              rx_solving_options_ind *ind, double *yp,
              long unsigned max_steps)
        : ode::OdeDoPri87((unsigned long)neqOde),
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

        // Stage 6 (a62=a63=0)
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a61 * k_[0][i] + a64 * k_[3][i] + a65 * k_[4][i]);
        t_stage_ = t_ + c6 * dt;
        ode_fun_(soltemp_, k_[5]);

        // Stage 7 (a72=a73=0)
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a71 * k_[0][i] + a74 * k_[3][i]
                                         + a75 * k_[4][i] + a76 * k_[5][i]);
        t_stage_ = t_ + c7 * dt;
        ode_fun_(soltemp_, k_[6]);

        // Stage 8 (a82=a83=0)
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a81 * k_[0][i] + a84 * k_[3][i]
                                         + a85 * k_[4][i] + a86 * k_[5][i]
                                         + a87 * k_[6][i]);
        t_stage_ = t_ + c8 * dt;
        ode_fun_(soltemp_, k_[7]);

        // Stage 9 (a92=a93=0)
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a91 * k_[0][i] + a94 * k_[3][i]
                                         + a95 * k_[4][i] + a96 * k_[5][i]
                                         + a97 * k_[6][i] + a98 * k_[7][i]);
        t_stage_ = t_ + c9 * dt;
        ode_fun_(soltemp_, k_[8]);

        // Stage 10 (a102=a103=0)
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a101 * k_[0][i] + a104 * k_[3][i]
                                         + a105 * k_[4][i] + a106 * k_[5][i]
                                         + a107 * k_[6][i] + a108 * k_[7][i]
                                         + a109 * k_[8][i]);
        t_stage_ = t_ + c10 * dt;
        ode_fun_(soltemp_, k_[9]);

        // Stage 11 (a112=a113=0)
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a111 * k_[0][i] + a114 * k_[3][i]
                                         + a115 * k_[4][i] + a116 * k_[5][i]
                                         + a117 * k_[6][i] + a118 * k_[7][i]
                                         + a119 * k_[8][i] + a1110 * k_[9][i]);
        t_stage_ = t_ + c11 * dt;
        ode_fun_(soltemp_, k_[10]);

        // Stage 12 (a122=a123=0)
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a121 * k_[0][i] + a124 * k_[3][i]
                                         + a125 * k_[4][i] + a126 * k_[5][i]
                                         + a127 * k_[6][i] + a128 * k_[7][i]
                                         + a129 * k_[8][i] + a1210 * k_[9][i]
                                         + a1211 * k_[10][i]);
        t_stage_ = t_ + c12 * dt;
        ode_fun_(soltemp_, k_[11]);

        // Stage 13 (a132=a133=0)
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a131 * k_[0][i] + a134 * k_[3][i]
                                         + a135 * k_[4][i] + a136 * k_[5][i]
                                         + a137 * k_[6][i] + a138 * k_[7][i]
                                         + a139 * k_[8][i] + a1310 * k_[9][i]
                                         + a1311 * k_[10][i]);
        t_stage_ = t_ + c13 * dt;
        ode_fun_(soltemp_, k_[12]);

        // solemb_ from OLD sol_ (7th-order embedded, no k_[12])
        for (unsigned long i = 0; i < neq_; i++)
            solemb_[i] = sol_[i] + dt * (d1  * k_[0][i]  + d6  * k_[5][i]
                                        + d7  * k_[6][i]  + d8  * k_[7][i]
                                        + d9  * k_[8][i]  + d10 * k_[9][i]
                                        + d11 * k_[10][i] + d12 * k_[11][i]);

        // sol_ update to 8th-order (includes k_[12] via b13)
        for (unsigned long i = 0; i < neq_; i++)
            sol_[i] += dt * (b1  * k_[0][i]  + b6  * k_[5][i]
                           + b7  * k_[6][i]  + b8  * k_[7][i]
                           + b9  * k_[8][i]  + b10 * k_[9][i]
                           + b11 * k_[10][i] + b12 * k_[11][i]
                           + b13 * k_[12][i]);
    }

    void after_step(double /*t*/) override {
        if (ind_->err != 0) throw std::runtime_error("dop87: ode error");
        if (nstep_ > max_steps_) {
            if (ind_->rc[0] == 0) ind_->rc[0] = -2019;
            ind_->err = 1;
            throw std::runtime_error("dop87: max steps exceeded");
        }
    }
};

#endif // ODE_DOP87_BRIDGE_H_
