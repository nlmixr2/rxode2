#ifndef ODE_RKCV8_BRIDGE_H_
#define ODE_RKCV8_BRIDGE_H_
#include "ode/ode_rkcv8.h"
class RxRkcv8 : public ode::OdeRkcv8 {
    t_dydt dydt_; int *full_neq_; rx_solving_options_ind *ind_; double t_stage_;
public:
    RxRkcv8(t_dydt dydt, int *full_neq, int neqOde, rx_solving_options_ind *ind, double *yp)
        : ode::OdeRkcv8((unsigned long)neqOde), dydt_(dydt), full_neq_(full_neq), ind_(ind), t_stage_(0.0)
    { set_sol_external(yp); }
protected:
    void ode_fun(double * __restrict__ solin, double * __restrict__ fout) override {
        if (ind_->err!=0){for(unsigned long i=0;i<neq_;i++) fout[i]=0.0; return;}
        dydt_(full_neq_, t_stage_, solin, fout);
        for(unsigned long i=0;i<neq_;i++){if(!std::isfinite(fout[i])){ind_->err=1;for(unsigned long j=0;j<neq_;j++) fout[j]=0.0;return;}}
    }
    void step_(double dt) override {
        t_stage_=t_; ode_fun_(sol_, k_[0]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b21*k_[0][i]);
        t_stage_=t_+a2*dt; ode_fun_(soltemp_, k_[1]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b31*k_[0][i]+b32*k_[1][i]);
        t_stage_=t_+a3*dt; ode_fun_(soltemp_, k_[2]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b41*k_[0][i]+b42*k_[1][i]+b43*k_[2][i]);
        t_stage_=t_+a4*dt; ode_fun_(soltemp_, k_[3]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b51*k_[0][i]+b53*k_[2][i]+b54*k_[3][i]);
        t_stage_=t_+a5*dt; ode_fun_(soltemp_, k_[4]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b61*k_[0][i]+b63*k_[2][i]+b64*k_[3][i]+b65*k_[4][i]);
        t_stage_=t_+a6*dt; ode_fun_(soltemp_, k_[5]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b71*k_[0][i]+b73*k_[2][i]+b74*k_[3][i]+b75*k_[4][i]+b76*k_[5][i]);
        t_stage_=t_+a7*dt; ode_fun_(soltemp_, k_[6]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b81*k_[0][i]+b85*k_[4][i]+b86*k_[5][i]+b87*k_[6][i]);
        t_stage_=t_+a8*dt; ode_fun_(soltemp_, k_[7]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b91*k_[0][i]+b95*k_[4][i]+b96*k_[5][i]+b97*k_[6][i]+b98*k_[7][i]);
        t_stage_=t_+a9*dt; ode_fun_(soltemp_, k_[8]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b101*k_[0][i]+b105*k_[4][i]+b106*k_[5][i]+b107*k_[6][i]+b108*k_[7][i]+b109*k_[8][i]);
        t_stage_=t_+a10*dt; ode_fun_(soltemp_, k_[9]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b115*k_[4][i]+b116*k_[5][i]+b117*k_[6][i]+b118*k_[7][i]+b119*k_[8][i]+b1110*k_[9][i]);
        t_stage_=t_+dt; ode_fun_(soltemp_, k_[10]);
        for(unsigned long i=0;i<neq_;i++) sol_[i]+=dt*(c1*k_[0][i]+c8*k_[7][i]+c9*k_[8][i]+c10*k_[9][i]+c11*k_[10][i]);
    }
};
#endif
