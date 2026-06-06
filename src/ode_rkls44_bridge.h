#ifndef ODE_RKLS44_BRIDGE_H_
#define ODE_RKLS44_BRIDGE_H_
#include "ode/ode_rkls44.h"
class RxRkls44 : public ode::OdeRkls44 {
    t_dydt dydt_; int *full_neq_; rx_solving_options_ind *ind_; double t_stage_;
public:
    RxRkls44(t_dydt dydt, int *full_neq, int neqOde, rx_solving_options_ind *ind, double *yp)
        : ode::OdeRkls44((unsigned long)neqOde), dydt_(dydt), full_neq_(full_neq), ind_(ind), t_stage_(0.0)
    { set_sol_external(yp); }
protected:
    void ode_fun(double * __restrict__ solin, double * __restrict__ fout) override {
        if (ind_->err != 0) { for (unsigned long i=0;i<neq_;i++) fout[i]=0.0; return; }
        dydt_(full_neq_, t_stage_, solin, fout);
        for (unsigned long i=0;i<neq_;i++) { if (!std::isfinite(fout[i])) { ind_->err=1; for (unsigned long j=0;j<neq_;j++) fout[j]=0.0; return; } }
    }
    void step_(double dt) override {
        unsigned long i;
        for (i=0;i<neq_;i++) k_[0][i]=-4.0/3.0*sol_[i];
        t_stage_=t_; ode_fun_(sol_, k_[1]);
        for (i=0;i<neq_;i++) { soltemp_[i]=sol_[i]+dt*k_[1][i]/2.0; k_[0][i]+=soltemp_[i]/3.0; }
        t_stage_=t_+dt/2.0; ode_fun_(soltemp_, k_[1]);
        for (i=0;i<neq_;i++) { soltemp_[i]=sol_[i]+dt*k_[1][i]/2.0; k_[0][i]+=2.0*soltemp_[i]/3.0; }
        t_stage_=t_+dt/2.0; ode_fun_(soltemp_, k_[1]);
        for (i=0;i<neq_;i++) { soltemp_[i]=sol_[i]+dt*k_[1][i]; k_[0][i]+=soltemp_[i]/3.0; }
        t_stage_=t_+dt; ode_fun_(soltemp_, k_[1]);
        for (i=0;i<neq_;i++) sol_[i]=sol_[i]+dt*k_[1][i]/6.0+k_[0][i];
    }
};
#endif
