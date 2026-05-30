#ifndef ODE_RK7_BRIDGE_H_
#define ODE_RK7_BRIDGE_H_
#include "ode/ode_rk7.h"
class RxRk7 : public ode::OdeRk7 {
    t_dydt dydt_; int *full_neq_; rx_solving_options_ind *ind_; double t_stage_;
public:
    RxRk7(t_dydt dydt, int *full_neq, int neqOde, rx_solving_options_ind *ind, double *yp)
        : ode::OdeRk7((unsigned long)neqOde), dydt_(dydt), full_neq_(full_neq), ind_(ind), t_stage_(0.0)
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
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*aa5*(b50*k_[0][i]+b52*k_[2][i]+b53*k_[3][i]+b54*k_[4][i]);
        t_stage_=t_+a5*dt; ode_fun_(soltemp_, k_[5]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*aa6*(b60*k_[0][i]+b62*k_[2][i]+b63*k_[3][i]+b64*k_[4][i]+b65*k_[5][i]);
        t_stage_=t_+a6*dt; ode_fun_(soltemp_, k_[6]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*aa7*(b70*k_[0][i]+b72*k_[2][i]+b73*k_[3][i]+b74*k_[4][i]+b75*k_[5][i]+b76*k_[6][i]);
        t_stage_=t_+a7*dt; ode_fun_(soltemp_, k_[7]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*aa8*(b80*k_[0][i]+b82*k_[2][i]+b83*k_[3][i]+b84*k_[4][i]+b85*k_[5][i]+b86*k_[6][i]+b87*k_[7][i]);
        t_stage_=t_+dt; ode_fun_(soltemp_, k_[8]);
        for(unsigned long i=0;i<neq_;i++) sol_[i]+=dt*cv*(c0*k_[0][i]+c3*k_[3][i]+c4*k_[4][i]+c5*k_[5][i]+c6*k_[6][i]+c7*k_[7][i]+c8*k_[8][i]);
    }
};
#endif
