#ifndef ODE_RKLS54_BRIDGE_H_
#define ODE_RKLS54_BRIDGE_H_
#include "ode/ode_rkls54.h"
class RxRkls54 : public ode::OdeRkls54 {
    t_dydt dydt_; int *full_neq_; rx_solving_options_ind *ind_; double t_stage_;
public:
    RxRkls54(t_dydt dydt, int *full_neq, int neqOde, rx_solving_options_ind *ind, double *yp)
        : ode::OdeRkls54((unsigned long)neqOde), dydt_(dydt), full_neq_(full_neq), ind_(ind), t_stage_(0.0)
    { set_sol_external(yp); }
protected:
    void ode_fun(double *solin, double *fout) override {
        if (ind_->err != 0) { for (unsigned long i=0;i<neq_;i++) fout[i]=0.0; return; }
        dydt_(full_neq_, t_stage_, solin, fout);
        for (unsigned long i=0;i<neq_;i++) { if (!std::isfinite(fout[i])) { ind_->err=1; for (unsigned long j=0;j<neq_;j++) fout[j]=0.0; return; } }
    }
    void step_(double dt) override {
        unsigned long i;
        t_stage_=t_; ode_fun_(sol_, k_[1]);
        for (i=0;i<neq_;i++) { k_[0][i]=dt*k_[1][i]; sol_[i]+=b1*k_[0][i]; }
        t_stage_=t_+c2*dt; ode_fun_(sol_, k_[1]);
        for (i=0;i<neq_;i++) { k_[0][i]=a2*k_[0][i]+dt*k_[1][i]; sol_[i]+=b2*k_[0][i]; }
        t_stage_=t_+c3*dt; ode_fun_(sol_, k_[1]);
        for (i=0;i<neq_;i++) { k_[0][i]=a3*k_[0][i]+dt*k_[1][i]; sol_[i]+=b3*k_[0][i]; }
        t_stage_=t_+c4*dt; ode_fun_(sol_, k_[1]);
        for (i=0;i<neq_;i++) { k_[0][i]=a4*k_[0][i]+dt*k_[1][i]; sol_[i]+=b4*k_[0][i]; }
        t_stage_=t_+c5*dt; ode_fun_(sol_, k_[1]);
        for (i=0;i<neq_;i++) { k_[0][i]=a5*k_[0][i]+dt*k_[1][i]; sol_[i]+=b5*k_[0][i]; }
    }
};
#endif
