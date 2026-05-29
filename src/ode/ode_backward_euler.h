#ifndef ODE_BACKWARD_EULER_H_
#define ODE_BACKWARD_EULER_H_

//! \file ode_backward_euler.h

#include "ode_adaptive.h"
#include "ode_irk.h"
#include "ode_newton_bridge.h"

namespace ode {

//forward declaration to set up Newton class
class OdeBackwardEuler;

//!Nonlinear system solver for OdeBackwardEuler
class NewtonBackwardEuler : public OdeNewtonIRK<OdeBackwardEuler> {
    public:
        //!constructs
        /*!
        \param[in] neq size of ODE system
        \param[in] nnew size of Newton system
        \param[in] integrator pointer to OdeBackwardEuler object
        */
        NewtonBackwardEuler (unsigned long neq, unsigned long nnew, OdeBackwardEuler *integrator) : OdeNewtonIRK (neq, nnew, integrator) {};
    private:
        void f_Newton (double *x, double *y);
        void J_Newton (double *x, double **J);
};

//!Backward Euler's method, unconditionally stable but relatively inaccurate
/*! + https://en.wikipedia.org/wiki/Backward_Euler_method */
class OdeBackwardEuler : public OdeAdaptive, private OdeIRK {
    //friends!
    friend class OdeNewtonBridge<OdeBackwardEuler>;
    friend class OdeNewtonIRK<OdeBackwardEuler>;

    public:
        //!constructs
        /*!
        \param[in] neq size of ODE sytem
        */
        OdeBackwardEuler (unsigned long neq);

        //!destructs
        ~OdeBackwardEuler ();

        //!returns a pointer to the solver's Newton system object
        NewtonBackwardEuler *get_newton () { return(newton_); }

    private:
        double **a;
        double *b;
        NewtonBackwardEuler *newton_;
        void step_ (double dt);
};

} // namespace ode

#endif
