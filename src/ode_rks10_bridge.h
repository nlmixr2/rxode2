#ifndef ODE_RKS10_BRIDGE_H_
#define ODE_RKS10_BRIDGE_H_
#include "ode/ode_rks10.h"
class RxRks10 : public ode::OdeRks10 {
    t_dydt dydt_; int *full_neq_; rx_solving_options_ind *ind_; double t_stage_;
public:
    RxRks10(t_dydt dydt, int *full_neq, int neqOde, rx_solving_options_ind *ind, double *yp)
        : ode::OdeRks10((unsigned long)neqOde), dydt_(dydt), full_neq_(full_neq), ind_(ind), t_stage_(0.0)
    { set_sol_external(yp); }
protected:
    void ode_fun(double *solin, double *fout) override {
        if (ind_->err!=0){for(unsigned long i=0;i<neq_;i++) fout[i]=0.0; return;}
        dydt_(full_neq_, t_stage_, solin, fout);
        for(unsigned long i=0;i<neq_;i++){if(!std::isfinite(fout[i])){ind_->err=1;for(unsigned long j=0;j<neq_;j++) fout[j]=0.0;return;}}
    }
    void step_(double dt) override {
        t_stage_=t_; ode_fun_(sol_, k_[0]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b21*k_[0][i]);
        t_stage_=t_+a2*dt; ode_fun_(soltemp_, k_[1]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b32*k_[1][i]);
        t_stage_=t_+a3*dt; ode_fun_(soltemp_, k_[2]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b41*k_[0][i]+b43*k_[2][i]);
        t_stage_=t_+a4*dt; ode_fun_(soltemp_, k_[3]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b51*k_[0][i]+b53*k_[2][i]+b54*k_[3][i]);
        t_stage_=t_+a5*dt; ode_fun_(soltemp_, k_[4]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b61*k_[0][i]+b63*k_[2][i]+b64*k_[3][i]+b65*k_[4][i]);
        t_stage_=t_+a6*dt; ode_fun_(soltemp_, k_[5]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b71*k_[0][i]+b74*k_[3][i]+b75*k_[4][i]+b76*k_[5][i]);
        t_stage_=t_+a7*dt; ode_fun_(soltemp_, k_[6]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b81*k_[0][i]+b84*k_[3][i]+b85*k_[4][i]+b86*k_[5][i]+b87*k_[6][i]);
        t_stage_=t_+a8*dt; ode_fun_(soltemp_, k_[7]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b91*k_[0][i]+b94*k_[3][i]+b95*k_[4][i]+b96*k_[5][i]+b97*k_[6][i]+b98*k_[7][i]);
        t_stage_=t_+a9*dt; ode_fun_(soltemp_, k_[8]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b101*k_[0][i]+b104*k_[3][i]+b105*k_[4][i]+b106*k_[5][i]+b107*k_[6][i]+b108*k_[7][i]+b109*k_[8][i]);
        t_stage_=t_+a10*dt; ode_fun_(soltemp_, k_[9]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b111*k_[0][i]+b114*k_[3][i]+b115*k_[4][i]+b116*k_[5][i]+b117*k_[6][i]+b118*k_[7][i]+b119*k_[8][i]+b1110*k_[9][i]);
        t_stage_=t_+a11*dt; ode_fun_(soltemp_, k_[10]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b121*k_[0][i]+b124*k_[3][i]+b125*k_[4][i]+b126*k_[5][i]+b127*k_[6][i]+b128*k_[7][i]+b129*k_[8][i]+b1210*k_[9][i]+b1211*k_[10][i]);
        t_stage_=t_+a12*dt; ode_fun_(soltemp_, k_[11]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b131*k_[0][i]+b134*k_[3][i]+b135*k_[4][i]+b136*k_[5][i]+b137*k_[6][i]+b138*k_[7][i]+b139*k_[8][i]+b1310*k_[9][i]+b1311*k_[10][i]+b1312*k_[11][i]);
        t_stage_=t_+a13*dt; ode_fun_(soltemp_, k_[12]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b141*k_[0][i]+b144*k_[3][i]+b145*k_[4][i]+b146*k_[5][i]+b147*k_[6][i]+b148*k_[7][i]+b149*k_[8][i]+b1410*k_[9][i]+b1411*k_[10][i]+b1412*k_[11][i]+b1413*k_[12][i]);
        t_stage_=t_+a14*dt; ode_fun_(soltemp_, k_[13]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b151*k_[0][i]+b154*k_[3][i]+b155*k_[4][i]+b156*k_[5][i]+b157*k_[6][i]+b158*k_[7][i]+b159*k_[8][i]+b1510*k_[9][i]+b1511*k_[10][i]+b1512*k_[11][i]+b1513*k_[12][i]+b1514*k_[13][i]);
        t_stage_=t_+dt; ode_fun_(soltemp_, k_[14]);
        for(unsigned long i=0;i<neq_;i++) sol_[i]+=dt*(c1*k_[0][i]+c9*k_[8][i]+c10*k_[9][i]+c11*k_[10][i]+c12*k_[11][i]+c13*k_[12][i]+c14*k_[13][i]+c15*k_[14][i]);
    }
};
#endif
