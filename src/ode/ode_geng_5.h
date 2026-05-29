#ifndef ODE_GENG_5_H_
#define ODE_GENG_5_H_

//! \file ode_geng_5.h

#include "ode_adaptive.h"
#include "ode_irk.h"
#include "ode_newton_bridge.h"


namespace ode {

//forward declaration to set up Newton class
class OdeGeng5;

//!Nonlinear system solver for OdeGeng5
class NewtonGeng5 : public OdeNewtonIRK<OdeGeng5> {
    public:
        //!constructs
        /*!
        \param[in] neq size of ODE system
        \param[in] nnew size of Newton system
        \param[in] integrator pointer to OdeGeng5 object
        */
        NewtonGeng5 (unsigned long neq, unsigned long nnew, OdeGeng5 *integrator) : OdeNewtonIRK (neq, nnew, integrator) {};
    private:
        void f_Newton (double *x, double *y);
        void J_Newton (double *x, double **J);
};

//!The fifth-order, symplectic, fully-implicit Geng integrator with 3 stages
class OdeGeng5 : public OdeAdaptive, private OdeIRK {
    //friends!
    friend class OdeNewtonBridge<OdeGeng5>;
    friend class OdeNewtonIRK<OdeGeng5>;

    public:
        //!constructs
        /*!
        \param[in] neq size of ODE system
        */
        OdeGeng5 (unsigned long neq);

        //!destructs
        ~OdeGeng5 ();

        //!returns a pointer to the solver's Newton system object
        NewtonGeng5 *get_newton () { return(newton_); }

    private:
        double **a;
        double *b;
        NewtonGeng5 *newton_;
        void step_ (double dt);
};

} // namespace ode

#endif
