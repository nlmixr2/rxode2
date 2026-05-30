#ifndef ODE_RKS5_BRIDGE_H_
#define ODE_RKS5_BRIDGE_H_
#include "ode/ode_rks5.h"
class RxRks5 : public ode::OdeRks5 {
    t_dydt dydt_; int *full_neq_; rx_solving_options_ind *ind_; double t_stage_;
public:
    RxRks5(t_dydt dydt, int *full_neq, int neqOde, rx_solving_options_ind *ind, double *yp)
        : ode::OdeRks5((unsigned long)neqOde), dydt_(dydt), full_neq_(full_neq), ind_(ind), t_stage_(0.0)
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
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*aa2*(b20*k_[0][i]+b21*k_[1][i]);
        t_stage_=t_+a2*dt; ode_fun_(soltemp_, k_[2]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*aa3*(b30*k_[0][i]+b31*k_[1][i]+b32*k_[2][i]);
        t_stage_=t_+a3*dt; ode_fun_(soltemp_, k_[3]);
        for(unsigned long i=0;i<neq_;i++) soltemp_[i]=sol_[i]+dt*aa4*(b40*k_[0][i]+b41*k_[1][i]+b42*k_[2][i]+b43*k_[3][i]);
        t_stage_=t_+dt; ode_fun_(soltemp_, k_[4]);
        for(unsigned long i=0;i<neq_;i++) sol_[i]+=dt*cv*(c0*k_[0][i]+c2*k_[2][i]+c3*k_[3][i]+c4*k_[4][i]);
    }
};
#endif
