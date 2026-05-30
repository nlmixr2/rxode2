#ifndef ODE_RKC108_BRIDGE_H_
#define ODE_RKC108_BRIDGE_H_

#include "ode/ode_rkc108.h"

class RxRkc108 : public ode::OdeRkc108 {
    t_dydt                  dydt_;
    int                    *full_neq_;
    rx_solving_options_ind *ind_;
    double                  t_stage_;
    long unsigned           max_steps_;

public:
    RxRkc108(t_dydt dydt, int *full_neq, int neqOde,
            rx_solving_options_ind *ind, double *yp,
            long unsigned max_steps)
        : ode::OdeRkc108((unsigned long)neqOde),
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
        t_stage_ = t_ + c6 * dt;
        ode_fun_(soltemp_, k_[5]);
        // Stage 7
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a71*k_[0][i]+a72*k_[1][i]+a73*k_[2][i]+a74*k_[3][i]+a75*k_[4][i]+a76*k_[5][i]);
        t_stage_ = t_ + c7 * dt;
        ode_fun_(soltemp_, k_[6]);
        // Stage 8
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a81*k_[0][i]+a82*k_[1][i]+a83*k_[2][i]+a84*k_[3][i]+a85*k_[4][i]+a86*k_[5][i]+a87*k_[6][i]);
        t_stage_ = t_ + c8 * dt;
        ode_fun_(soltemp_, k_[7]);
        // Stage 9
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a91*k_[0][i]+a92*k_[1][i]+a93*k_[2][i]+a94*k_[3][i]+a95*k_[4][i]+a96*k_[5][i]+a97*k_[6][i]+a98*k_[7][i]);
        t_stage_ = t_ + c9 * dt;
        ode_fun_(soltemp_, k_[8]);
        // Stage 10
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a101*k_[0][i]+a102*k_[1][i]+a103*k_[2][i]+a104*k_[3][i]+a105*k_[4][i]+a106*k_[5][i]+a107*k_[6][i]+ a108*k_[7][i]+a109*k_[8][i]);
        t_stage_ = t_ + c10 * dt;
        ode_fun_(soltemp_, k_[9]);
        // Stage 11
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a111*k_[0][i]+a112*k_[1][i]+a113*k_[2][i]+a114*k_[3][i]+a115*k_[4][i]+a116*k_[5][i]+a117*k_[6][i]+ a118*k_[7][i]+a119*k_[8][i]+a1110*k_[9][i]);
        t_stage_ = t_ + c11 * dt;
        ode_fun_(soltemp_, k_[10]);
        // Stage 12
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a121*k_[0][i]+a122*k_[1][i]+a123*k_[2][i]+a124*k_[3][i]+a125*k_[4][i]+a126*k_[5][i]+a127*k_[6][i]+ a128*k_[7][i]+a129*k_[8][i]+a1210*k_[9][i]+a1211*k_[10][i]);
        t_stage_ = t_ + c12 * dt;
        ode_fun_(soltemp_, k_[11]);
        // Stage 13
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a131*k_[0][i]+a132*k_[1][i]+a133*k_[2][i]+a134*k_[3][i]+a135*k_[4][i]+a136*k_[5][i]+a137*k_[6][i]+ a138*k_[7][i]+a139*k_[8][i]+a1310*k_[9][i]+a1311*k_[10][i]+a1312*k_[11][i]);
        t_stage_ = t_ + c13 * dt;
        ode_fun_(soltemp_, k_[12]);
        // Stage 14
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a141*k_[0][i]+a142*k_[1][i]+a143*k_[2][i]+a144*k_[3][i]+a145*k_[4][i]+a146*k_[5][i]+a147*k_[6][i]+ a148*k_[7][i]+a149*k_[8][i]+a1410*k_[9][i]+a1411*k_[10][i]+a1412*k_[11][i]+a1413*k_[12][i]);
        t_stage_ = t_ + c14 * dt;
        ode_fun_(soltemp_, k_[13]);
        // Stage 15
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a151*k_[0][i]+a152*k_[1][i]+a153*k_[2][i]+a154*k_[3][i]+a155*k_[4][i]+a156*k_[5][i]+a157*k_[6][i]+ a158*k_[7][i]+a159*k_[8][i]+a1510*k_[9][i]+a1511*k_[10][i]+a1512*k_[11][i]+a1513*k_[12][i]+ a1514*k_[13][i]);
        t_stage_ = t_ + c15 * dt;
        ode_fun_(soltemp_, k_[14]);
        // Stage 16
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a161*k_[0][i]+a162*k_[1][i]+a163*k_[2][i]+a164*k_[3][i]+a165*k_[4][i]+a166*k_[5][i]+a167*k_[6][i]+ a168*k_[7][i]+a169*k_[8][i]+a1610*k_[9][i]+a1611*k_[10][i]+a1612*k_[11][i]+a1613*k_[12][i]+ a1614*k_[13][i]+a1615*k_[14][i]);
        t_stage_ = t_ + c16 * dt;
        ode_fun_(soltemp_, k_[15]);
        // Stage 17
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a171*k_[0][i]+a172*k_[1][i]+a173*k_[2][i]+a174*k_[3][i]+a175*k_[4][i]+a176*k_[5][i]+a177*k_[6][i]+ a178*k_[7][i]+a179*k_[8][i]+a1710*k_[9][i]+a1711*k_[10][i]+a1712*k_[11][i]+a1713*k_[12][i]+ a1714*k_[13][i]+a1715*k_[14][i]+a1716*k_[15][i]);
        t_stage_ = t_ + c17 * dt;
        ode_fun_(soltemp_, k_[16]);
        // Stage 18
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a181*k_[0][i]+a182*k_[1][i]+a183*k_[2][i]+a184*k_[3][i]+a185*k_[4][i]+a186*k_[5][i]+a187*k_[6][i]+ a188*k_[7][i]+a189*k_[8][i]+a1810*k_[9][i]+a1811*k_[10][i]+a1812*k_[11][i]+a1813*k_[12][i]+ a1814*k_[13][i]+a1815*k_[14][i]+a1816*k_[15][i]+a1817*k_[16][i]);
        t_stage_ = t_ + c18 * dt;
        ode_fun_(soltemp_, k_[17]);
        // Stage 19
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a191*k_[0][i]+a192*k_[1][i]+a193*k_[2][i]+a194*k_[3][i]+a195*k_[4][i]+a196*k_[5][i]+a197*k_[6][i]+ a198*k_[7][i]+a199*k_[8][i]+a1910*k_[9][i]+a1911*k_[10][i]+a1912*k_[11][i]+a1913*k_[12][i]+ a1914*k_[13][i]+a1915*k_[14][i]+a1916*k_[15][i]+a1917*k_[16][i]+a1918*k_[17][i]);
        t_stage_ = t_ + c19 * dt;
        ode_fun_(soltemp_, k_[18]);
        // Stage 20
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a201*k_[0][i]+a202*k_[1][i]+a203*k_[2][i]+a204*k_[3][i]+a205*k_[4][i]+a206*k_[5][i]+a207*k_[6][i]+ a208*k_[7][i]+a209*k_[8][i]+a2010*k_[9][i]+a2011*k_[10][i]+a2012*k_[11][i]+a2013*k_[12][i]+ a2014*k_[13][i]+a2015*k_[14][i]+a2016*k_[15][i]+a2017*k_[16][i]+a2018*k_[17][i]+a2019*k_[18][i]);
        t_stage_ = t_ + c20 * dt;
        ode_fun_(soltemp_, k_[19]);
        // Stage 21
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i] + dt * (a211*k_[0][i]+a212*k_[1][i]+a213*k_[2][i]+a214*k_[3][i]+a215*k_[4][i]+a216*k_[5][i]+a217*k_[6][i]+ a218*k_[7][i]+a219*k_[8][i]+a2110*k_[9][i]+a2111*k_[10][i]+a2112*k_[11][i]+a2113*k_[12][i]+ a2114*k_[13][i]+a2115*k_[14][i]+a2116*k_[15][i]+a2117*k_[16][i]+a2118*k_[17][i]+a2119* k_[18][i]+a2120*k_[19][i]);
        t_stage_ = t_ + c21 * dt;
        ode_fun_(soltemp_, k_[20]);
        // Primary (higher-order) solution -> sol_
        for (unsigned long i = 0; i < neq_; i++)
            sol_[i] = sol_[i]+dt*(b1*k_[0][i]+b2*k_[1][i]+b3*k_[2][i]+b4*k_[3][i]+b5*k_[4][i]+b6*k_[5][i]+b7*k_[6][i]+b8*k_[7][i]+b9*k_[8][i]+b10*k_[9][i]+ b11*k_[10][i]+b12*k_[11][i]+b13*k_[12][i]+b14*k_[13][i]+b15*k_[14][i]+b16*k_[15][i]+b17*k_[16][i]+b18*k_[17][i]+ b19*k_[18][i]+b20*k_[19][i]+b21*k_[20][i]);
        // Embedded (lower-order) solution -> solemb_
        for (unsigned long i = 0; i < neq_; i++)
            solemb_[i] = sol_[i] + dt * ((d1 - b1) * k_[0][i] + (d2 - b2) * k_[1][i] + (d3 - b3) * k_[2][i] + (d4 - b4) * k_[3][i] + (d5 - b5) * k_[4][i] + (d6 - b6) * k_[5][i] + (d7 - b7) * k_[6][i] + (d8 - b8) * k_[7][i] + (d9 - b9) * k_[8][i] + (d10 - b10) * k_[9][i] + (d11 - b11) * k_[10][i] + (d12 - b12) * k_[11][i] + (d13 - b13) * k_[12][i] + (d14 - b14) * k_[13][i] + (d15 - b15) * k_[14][i] + (d16 - b16) * k_[15][i] + (d17 - b17) * k_[16][i] + (d18 - b18) * k_[17][i] + (d19 - b19) * k_[18][i] + (d20 - b20) * k_[19][i] + (d21 - b21) * k_[20][i]);
    }

    void after_step(double) override {
        if (ind_->err != 0) throw std::runtime_error("rkc108: ode error");
        if (nstep_ > max_steps_) {
            if (ind_->rc[0] == 0) ind_->rc[0] = -2019;
            ind_->err = 1;
            throw std::runtime_error("rkc108: max steps exceeded");
        }
    }
};

#endif // ODE_RKC108_BRIDGE_H_
