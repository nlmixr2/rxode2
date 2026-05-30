#ifndef ODE_RKB6_BRIDGE_H_
#define ODE_RKB6_BRIDGE_H_
#include "ode/ode_rkb6.h"
class RxRkb6 : public ode::OdeRkb6 {
    t_dydt dydt_; int *full_neq_; rx_solving_options_ind *ind_; double t_stage_;
public:
    RxRkb6(t_dydt dydt, int *full_neq, int neqOde, rx_solving_options_ind *ind, double *yp)
        : ode::OdeRkb6((unsigned long)neqOde), dydt_(dydt), full_neq_(full_neq), ind_(ind), t_stage_(0.0)
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
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b41*k_[0][i]+b42*k_[1][i]+b43*k_[2][i]);
        t_stage_=t_+a4*dt; ode_fun_(soltemp_, k_[3]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b51*k_[0][i]+b52*k_[1][i]+b53*k_[2][i]+b54*k_[3][i]);
        t_stage_=t_+a5*dt; ode_fun_(soltemp_, k_[4]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b62*k_[1][i]+b63*k_[2][i]+b64*k_[3][i]+b65*k_[4][i]);
        t_stage_=t_+a6*dt; ode_fun_(soltemp_, k_[5]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*(b71*k_[0][i]+b72*k_[1][i]+b73*k_[2][i]+b74*k_[3][i]+b76*k_[5][i]);
        t_stage_=t_+dt; ode_fun_(soltemp_, k_[6]);
        for(unsigned long i=0;i<neq_;i++) sol_[i]+=dt*(c1*k_[0][i]+c3*k_[2][i]+c4*k_[3][i]+c5*k_[4][i]+c6*k_[5][i]+c7*k_[6][i]);
    }
};
#endif
