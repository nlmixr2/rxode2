#ifndef ODE_LOBATTO_IIIC_6_H_
#define ODE_LOBATTO_IIIC_6_H_

//! \file ode_lobatto_iiic_6.h

#include "ode_adaptive.h"
#include "ode_irk.h"
#include "ode_newton_bridge.h"

namespace ode {

//forward declaration to set up Newton class
class OdeLobattoIIIC6;

//!Nonlinear system solver for OdeLobattoIIIC6
class NewtonLobattoIIIC6 : public OdeNewtonIRK<OdeLobattoIIIC6> {
    public:
        //!constructs
        /*!
        \param[in] neq size of ODE system
        \param[in] nnew size of Newton system
        \param[in] integrator pointer to OdeLobattoIIIC6 object
        */
        NewtonLobattoIIIC6 (unsigned long neq, unsigned long nnew, OdeLobattoIIIC6 *integrator) : OdeNewtonIRK (neq, nnew, integrator) {};
    private:
        void f_Newton (double *x, double *y);
        void J_Newton (double *x, double **J);
};

//!The sixth-order, L-stable, fully-implicit Lobatto IIIC method with 4 stages
/*!
    + Hairer, E. & Wanner, G. Solving Ordinary Differential Equations II: Stiff and Differential-Algebraic Problems. (Springer, 1996)
*/
class OdeLobattoIIIC6 : public OdeAdaptive, private OdeIRK {
    //friends!
    friend class OdeNewtonBridge<OdeLobattoIIIC6>;
    friend class OdeNewtonIRK<OdeLobattoIIIC6>;

    public:
        //!constructs
        /*!
        \param[in] neq size of ODE sytem
        */
        OdeLobattoIIIC6 (unsigned long neq);

        //!destructs
        ~OdeLobattoIIIC6 ();

        //!returns a pointer to the solver's Newton system object
        NewtonLobattoIIIC6 *get_newton () { return(newton_); }

    private:
        double **a;
        double *b;
        NewtonLobattoIIIC6 *newton_;
        void step_ (double dt);
};

} // namespace ode

#endif
