#ifndef ODE_RKS98_BRIDGE_H_
#define ODE_RKS98_BRIDGE_H_

#include "ode/ode_rks98.h"

class RxRks98 : public ode::OdeRks98 {
    t_dydt                  dydt_;
    int                    *full_neq_;
    rx_solving_options_ind *ind_;
    double                  t_stage_;
    long unsigned           max_steps_;

public:
    RxRks98(t_dydt dydt, int *full_neq, int neqOde,
            rx_solving_options_ind *ind, double *yp,
            long unsigned max_steps)
        : ode::OdeRks98((unsigned long)neqOde),
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
            soltemp_[i] = sol_[i] + dt * (a41*k_[0][i]+a43*k_[2][i]);
        t_stage_ = t_ + c4 * dt;
        ode_fun_(soltemp_, k_[3]);
        // Stage 5
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a51*k_[0][i]+a53*k_[2][i]+a54*k_[3][i]);
        t_stage_ = t_ + c5 * dt;
        ode_fun_(soltemp_, k_[4]);
        // Stage 6
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a61*k_[0][i]+a64*k_[3][i]+a65*k_[4][i]);
        t_stage_ = t_ + c6 * dt;
        ode_fun_(soltemp_, k_[5]);
        // Stage 7
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a71*k_[0][i]+a74*k_[3][i]+a75*k_[4][i]+a76*k_[5][i]);
        t_stage_ = t_ + c7 * dt;
        ode_fun_(soltemp_, k_[6]);
        // Stage 8
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a81*k_[0][i]+a86*k_[5][i]+a87*k_[6][i]);
        t_stage_ = t_ + c8 * dt;
        ode_fun_(soltemp_, k_[7]);
        // Stage 9
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a91*k_[0][i]+a96*k_[5][i]+a97*k_[6][i]+a98*k_[7][i]);
        t_stage_ = t_ + c9 * dt;
        ode_fun_(soltemp_, k_[8]);
        // Stage 10
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a101*k_[0][i]+a106*k_[5][i]+a107*k_[6][i]+a108*k_[7][i]+a109*k_[8][i]);
        t_stage_ = t_ + c10 * dt;
        ode_fun_(soltemp_, k_[9]);
        // Stage 11
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a111*k_[0][i]+a116*k_[5][i]+a117*k_[6][i]+a118*k_[7][i]+a119*k_[8][i]+ a1110*k_[9][i]);
        t_stage_ = t_ + c11 * dt;
        ode_fun_(soltemp_, k_[10]);
        // Stage 12
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a121*k_[0][i]+a126*k_[5][i]+a127*k_[6][i]+a128*k_[7][i]+a129*k_[8][i]+ a1210*k_[9][i]+a1211*k_[10][i]);
        t_stage_ = t_ + c12 * dt;
        ode_fun_(soltemp_, k_[11]);
        // Stage 13
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a131*k_[0][i]+a136*k_[5][i]+a137*k_[6][i]+a138*k_[7][i]+a139*k_[8][i]+ a1310*k_[9][i]+a1311*k_[10][i]+a1312*k_[11][i]);
        t_stage_ = t_ + c13 * dt;
        ode_fun_(soltemp_, k_[12]);
        // Stage 14
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a141*k_[0][i]+a146*k_[5][i]+a147*k_[6][i]+a148*k_[7][i]+a149*k_[8][i]+ a1410*k_[9][i]+a1411*k_[10][i]+a1412*k_[11][i]+a1413*k_[12][i]);
        t_stage_ = t_ + c14 * dt;
        ode_fun_(soltemp_, k_[13]);
        // Stage 15
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a151*k_[0][i]+a156*k_[5][i]+a157*k_[6][i]+a158*k_[7][i]+a159*k_[8][i]+ a1510*k_[9][i]+a1511*k_[10][i]+a1512*k_[11][i]+a1513*k_[12][i]+a1514*k_[13][i]);
        t_stage_ = t_ + dt;
        ode_fun_(soltemp_, k_[14]);
        // Stage 16
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a161*k_[0][i]+a166*k_[5][i]+a167*k_[6][i]+a168*k_[7][i]+a169*k_[8][i]+ a1610*k_[9][i]+a1611*k_[10][i]+a1612*k_[11][i]+a1613*k_[12][i]);
        t_stage_ = t_ + dt;
        ode_fun_(soltemp_, k_[15]);
        // Primary (higher-order) solution -> sol_
        for (unsigned long i = 0; i < neq_; i++)
            sol_[i] = sol_[i]+dt*(b1*k_[0][i]+b8*k_[7][i]+b9*k_[8][i]+b10*k_[9][i]+b11*k_[10][i]+b12*k_[11][i]+b13*k_[12][i]+b14*k_[13][i]+b15*k_[14][i]);
        // Embedded (lower-order) solution -> solemb_
        for (unsigned long i = 0; i < neq_; i++)
            solemb_[i] = sol_[i] + dt * ((d1 - b1) * k_[0][i] + (d8 - b8) * k_[7][i] + (d9 - b9) * k_[8][i] + (d10 - b10) * k_[9][i] + (d11 - b11) * k_[10][i] + (d12 - b12) * k_[11][i] + (d13 - b13) * k_[12][i] + (d14 - b14) * k_[13][i] + (d15 - b15) * k_[14][i] + (d16 - b16) * k_[15][i]);
    }

    void after_step(double) override {
        if (ind_->err != 0) throw std::runtime_error("rks98: ode error");
        if (nstep_ > max_steps_) {
            if (ind_->rc[0] == 0) ind_->rc[0] = -2019;
            ind_->err = 1;
            throw std::runtime_error("rks98: max steps exceeded");
        }
    }
};

#endif // ODE_RKS98_BRIDGE_H_
