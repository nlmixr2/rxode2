#ifndef ODE_RKSSP54_BRIDGE_H_
#define ODE_RKSSP54_BRIDGE_H_
#include "ode/ode_rkssp54.h"
class RxRkssp54 : public ode::OdeRkssp54 {
    t_dydt dydt_; int *full_neq_; rx_solving_options_ind *ind_; double t_stage_;
public:
    RxRkssp54(t_dydt dydt, int *full_neq, int neqOde, rx_solving_options_ind *ind, double *yp)
        : ode::OdeRkssp54((unsigned long)neqOde), dydt_(dydt), full_neq_(full_neq), ind_(ind), t_stage_(0.0)
    { set_sol_external(yp); }
protected:
    void ode_fun(double * __restrict__ solin, double * __restrict__ fout) override {
        if (ind_->err != 0) { for (unsigned long i=0;i<neq_;i++) fout[i]=0.0; return; }
        dydt_(full_neq_, t_stage_, solin, fout);
        for (unsigned long i=0;i<neq_;i++) { if (!std::isfinite(fout[i])) { ind_->err=1; for (unsigned long j=0;j<neq_;j++) fout[j]=0.0; return; } }
    }
    void step_(double dt) override {
        unsigned long i;
        t_stage_=t_; ode_fun_(sol_, k_[3]);
        for (i=0;i<neq_;i++) k_[0][i]=sol_[i]+b10*dt*k_[3][i];
        t_stage_=t_+c1*dt; ode_fun_(k_[0], k_[3]);
        for (i=0;i<neq_;i++) k_[0][i]=a20*sol_[i]+a21*k_[0][i]+b21*dt*k_[3][i];
        t_stage_=t_+c2*dt; ode_fun_(k_[0], k_[3]);
        for (i=0;i<neq_;i++) k_[1][i]=a30*sol_[i]+a32*k_[0][i]+b32*dt*k_[3][i];
        t_stage_=t_+c3*dt; ode_fun_(k_[1], k_[2]);
        for (i=0;i<neq_;i++) soltemp_[i]=a40*sol_[i]+a43*k_[1][i]+b43*dt*k_[2][i];
        t_stage_=t_+c4*dt; ode_fun_(soltemp_, k_[3]);
        for (i=0;i<neq_;i++) sol_[i]=a52*k_[0][i]+a53*k_[1][i]+b53*dt*k_[2][i]+a54*soltemp_[i]+b54*dt*k_[3][i];
    }
};
#endif
