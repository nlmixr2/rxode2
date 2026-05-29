#ifndef ODE_GAUSS_6_H_
#define ODE_GAUSS_6_H_

//! \file ode_gauss_6.h

#include "ode_adaptive.h"
#include "ode_irk.h"
#include "ode_newton_bridge.h"

namespace ode {

//forward declaration to set up Newton class
class OdeGauss6;

//!Nonlinear system solver for OdeGauss6
class NewtonGauss6 : public OdeNewtonIRK<OdeGauss6> {
    public:
        //!constructs
        /*!
        \param[in] neq size of ODE system
        \param[in] nnew size of Newton system
        \param[in] integrator pointer to OdeGauss6 object
        */
        NewtonGauss6 (unsigned long neq, unsigned long nnew, OdeGauss6 *integrator) : OdeNewtonIRK (neq, nnew, integrator) {};
    private:
        void f_Newton (double *x, double *y);
        void J_Newton (double *x, double **J);
};

//!The sixth-order, A-stable, fully-implicit Gauss-Legendre method with 3 stages
/*!
    + https://en.wikipedia.org/wiki/Gauss%E2%80%93Legendre_method
*/
class OdeGauss6 : public OdeAdaptive, private OdeIRK {
    //friends!
    friend class OdeNewtonBridge<OdeGauss6>;
    friend class OdeNewtonIRK<OdeGauss6>;

    public:

        //!constructs
        /*!
        \param[in] neq size of ODE system
        */
        OdeGauss6 (unsigned long neq);

        //!destructs
        ~OdeGauss6 ();

        //!returns a pointer to the solver's Newton system object
        NewtonGauss6 *get_newton () { return(newton_); }

    private:
        double **a;
        double *b;
        NewtonGauss6 *newton_;
        void step_ (double dt);
};

} // namespace ode

#endif
