#ifndef ODE_GRK4A_BRIDGE_H_
#define ODE_GRK4A_BRIDGE_H_

// RxGRK4A: bridges rxode2's t_dydt / t_calc_jac interfaces into libode's OdeGRK4A.
// Coefficients and step_() logic match ~/src/libode/src/ode_grk4a.cc exactly.
// t_cur_ is updated before each ode_fun_() call to carry the stage time to dydt_.
// When calc_jac_ is NULL the base class finite-difference Jacobian is used.

#include "ode/ode_grk4a.h"

class RxGRK4A : public ode::OdeGRK4A {
    t_dydt                  dydt_;
    t_calc_jac              calc_jac_;
    int                    *full_neq_;
    rx_solving_options_ind *ind_;
    double                  t_cur_;
    long unsigned           max_steps_;

public:
    RxGRK4A(t_dydt dydt, t_calc_jac jac, int *full_neq, int neqOde,
             rx_solving_options_ind *ind, double *yp, long unsigned max_steps)
        : ode::OdeGRK4A((unsigned long)neqOde),
          dydt_(dydt), calc_jac_(jac), full_neq_(full_neq), ind_(ind),
          t_cur_(0.0), max_steps_(max_steps)
    {
        set_sol_external(yp);
    }

protected:
    void ode_fun(double *solin, double *fout) override {
        if (ind_->err != 0) {
            for (unsigned long i = 0; i < neq_; i++) fout[i] = 0.0;
            return;
        }
        dydt_(full_neq_, t_cur_, solin, fout);
        for (unsigned long i = 0; i < neq_; i++) {
            if (!std::isfinite(fout[i])) {
                ind_->err = 1;
                for (unsigned long j = 0; j < neq_; j++) fout[j] = 0.0;
                return;
            }
        }
    }

    void ode_jac(double *solin, double **Jout) override {
        if (calc_jac_ == NULL) {
            // finite-difference fallback from OdeBase::ode_jac
            OdeGRK4A::ode_jac(solin, Jout);
            return;
        }
        // calc_jac fills a flat row-major buffer: JAC[row*NROWPD + col] = df_row/dy_col
        std::vector<double> flat(neq_ * neq_, 0.0);
        unsigned int nrowpd = (unsigned int)neq_;
        calc_jac_(full_neq_, t_cur_, solin, flat.data(), nrowpd);
        for (unsigned long r = 0; r < neq_; r++)
            for (unsigned long c = 0; c < neq_; c++)
                Jout[r][c] = flat[r * neq_ + c];
    }

    // Mirror libode's step_() exactly, adding t_cur_ tracking before each
    // ode_fun_() call.  Stage 4 reuses the same soltemp_ as stage 3 (GRK4A).
    void step_(double dt) override {
        unsigned long i;

        t_cur_ = t_;
        ode_jac_(sol_, Jac_);
        prep_jac(Jac_, neq_, dt, p_);

        // Stage 1
        ode_fun_(sol_, k_[0]);
        for (i=0; i<neq_; i++) rhs_[i] = dt*k_[0][i];
        ode::ode_solve_LU(Jac_, p_, rhs_, neq_, k_[0]);

        // Stage 2
        for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + alp21*k_[0][i];
        t_cur_ = t_ + alp21*dt;
        ode_fun_(soltemp_, k_[1]);
        for (i=0; i<neq_; i++) rhs_[i] = dt*k_[1][i] + g21*k_[0][i];
        ode::ode_solve_LU(Jac_, p_, rhs_, neq_, k_[1]);
        for (i=0; i<neq_; i++) k_[1][i] -= g21*k_[0][i];

        // Stage 3
        for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + alp31*k_[0][i] + alp32*k_[1][i];
        t_cur_ = t_ + (alp31 + alp32)*dt;
        ode_fun_(soltemp_, k_[2]);
        for (i=0; i<neq_; i++) rhs_[i] = dt*k_[2][i] + g31*k_[0][i] + g32*k_[1][i];
        ode::ode_solve_LU(Jac_, p_, rhs_, neq_, k_[2]);
        for (i=0; i<neq_; i++) k_[2][i] -= g31*k_[0][i] + g32*k_[1][i];

        // Stage 4 — same soltemp_ as stage 3 (GRK4A property)
        for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + alp31*k_[0][i] + alp32*k_[1][i];
        // t_cur_ unchanged: f evaluated at same point as stage 3
        ode_fun_(soltemp_, k_[3]);
        for (i=0; i<neq_; i++) rhs_[i] = dt*k_[3][i] + g41*k_[0][i] + g42*k_[1][i] + g43*k_[2][i];
        ode::ode_solve_LU(Jac_, p_, rhs_, neq_, k_[3]);
        for (i=0; i<neq_; i++) k_[3][i] -= g41*k_[0][i] + g42*k_[1][i] + g43*k_[2][i];

        // Solution and embedded solution
        for (i=0; i<neq_; i++) {
            solemb_[i] = sol_[i] + (d1*k_[0][i] + d2*k_[1][i] + d3*k_[2][i]);
            sol_[i]    = sol_[i] + (b1*k_[0][i] + b2*k_[1][i] + b3*k_[2][i] + b4*k_[3][i]);
        }
    }

    void after_step(double /*t*/) override {
        if (ind_->err != 0) throw std::runtime_error("grk4a: ode error");
        if (nstep_ > max_steps_) {
            if (ind_->rc[0] == 0) ind_->rc[0] = -2019;
            ind_->err = 1;
            throw std::runtime_error("grk4a: max steps exceeded");
        }
    }
};

#endif // ODE_GRK4A_BRIDGE_H_
