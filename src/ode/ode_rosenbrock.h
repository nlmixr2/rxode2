#ifndef ODE_ROSENBROCK_H_
#define ODE_ROSENBROCK_H_

//! \file ode_rosenbrock.h

#include "ode_linalg.h"

namespace ode {

//!Base class for Rosenbrock methods
class OdeRosenbrock {

    public:
        //!constructs
        /*!
        \param[in] neq size of ODE sytem
        \param[in] nk number of stages
        */
        OdeRosenbrock (unsigned long neq, int nk);
        //!destructs
        ~OdeRosenbrock ();

    protected:
        //!parameter multipying Jacobian or single diagonal gamma
        double gam;
        //!permutation array for LU factorization
        int *p_;
        //!right hand side of matrix equations
        double *rhs_;
        //!temporary sol vector
        double *soltemp_;
        //!stage derivatives
        double **k_;
        //!do necessary arithmetic with the Jacobian then lu factor it
        /*!
        \param[in] n size of Jacobian
        \param[in] Jac Jacobian matrix then LU decomposed Jacobian with modifications for Rosembrock stages
        \param[in] dt time step size
        \param[out] p permutation array for LU decomposition
        */
        void prep_jac (double **Jac, unsigned long n, double dt, int *p);

    private:
        //number of stages
        int nk_;
};

} // namespace ode

#endif
