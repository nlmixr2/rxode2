#ifndef ODE_RK8_12_BRIDGE_H_
#define ODE_RK8_12_BRIDGE_H_
#include "ode/ode_rk8_12.h"
class RxRk8_12 : public ode::OdeRk8_12 {
    t_dydt dydt_; int *full_neq_; rx_solving_options_ind *ind_; double t_stage_;
public:
    RxRk8_12(t_dydt dydt, int *full_neq, int neqOde, rx_solving_options_ind *ind, double *yp)
        : ode::OdeRk8_12((unsigned long)neqOde), dydt_(dydt), full_neq_(full_neq), ind_(ind), t_stage_(0.0)
    { set_sol_external(yp); }
protected:
    void ode_fun(double *solin, double *fout) override {
        if (ind_->err!=0){for(unsigned long i=0;i<neq_;i++) fout[i]=0.0; return;}
        dydt_(full_neq_, t_stage_, solin, fout);
        for(unsigned long i=0;i<neq_;i++){if(!std::isfinite(fout[i])){ind_->err=1;for(unsigned long j=0;j<neq_;j++) fout[j]=0.0;return;}}
    }
    void step_(double dt) override {
        t_stage_=t_; ode_fun_(sol_, k_[0]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*aa1*k_[0][i];
        t_stage_=t_+a1*dt; ode_fun_(soltemp_, k_[1]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*aa2*(k_[0][i]+b21*k_[1][i]);
        t_stage_=t_+a2*dt; ode_fun_(soltemp_, k_[2]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*aa3*(k_[0][i]+b32*k_[2][i]);
        t_stage_=t_+a3*dt; ode_fun_(soltemp_, k_[3]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*aa4*(b40*k_[0][i]+b42*k_[2][i]+b43*k_[3][i]);
        t_stage_=t_+a4*dt; ode_fun_(soltemp_, k_[4]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*aa5*(b50*k_[0][i]+b53*k_[3][i]+b54*k_[4][i]);
        t_stage_=t_+a5*dt; ode_fun_(soltemp_, k_[5]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*aa6*(b60*k_[0][i]+b63*k_[3][i]+b64*k_[4][i]+b65*k_[5][i]);
        t_stage_=t_+a6*dt; ode_fun_(soltemp_, k_[6]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*aa7*(b70*k_[0][i]+b73*k_[3][i]+b74*k_[4][i]+b76*k_[6][i]);
        t_stage_=t_+a7*dt; ode_fun_(soltemp_, k_[7]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*aa8*(b80*k_[0][i]+b83*k_[3][i]+b84*k_[4][i]+b85*k_[5][i]+b86*k_[6][i]+b87*k_[7][i]);
        t_stage_=t_+a8*dt; ode_fun_(soltemp_, k_[8]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*aa9*(b90*k_[0][i]+b93*k_[3][i]+b94*k_[4][i]+b95*k_[5][i]+b96*k_[6][i]+b97*k_[7][i]+b98*k_[8][i]);
        t_stage_=t_+a9*dt; ode_fun_(soltemp_, k_[9]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*aa10*(b100*k_[0][i]+b103*k_[3][i]+b104*k_[4][i]+b105*k_[5][i]+b106*k_[6][i]+b109*k_[9][i]);
        t_stage_=t_+a10*dt; ode_fun_(soltemp_, k_[10]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*aa11*(b110*k_[0][i]+b113*k_[3][i]+b114*k_[4][i]+b115*k_[5][i]+b116*k_[6][i]+b117*k_[7][i]+b118*k_[8][i]+b119*k_[9][i]+b1110*k_[10][i]);
        t_stage_=t_+dt; ode_fun_(soltemp_, k_[11]);
        for(unsigned long i=0;i<neq_;i++) sol_[i]+=dt*cv*(c0*k_[0][i]+c5*k_[5][i]+c6*k_[6][i]+c7*k_[7][i]+c8*k_[8][i]+c9*k_[9][i]+c10*k_[10][i]+c11*k_[11][i]);
    }
};
#endif
