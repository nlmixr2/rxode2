#ifndef ODE_GRK4A_H_
#define ODE_GRK4A_H_

//! \file ode_grk4a.h

#include "ode_linalg.h"
#include "ode_embedded.h"
#include "ode_rosenbrock.h"

namespace ode {

//!Fourth-order, A-stable, adaptive Rosenbrock method from Kaps and Rentrop
/*!
    + Kaps, Peter, and Peter Rentrop. "Generalized Runge-Kutta methods of order four with stepsize control for stiff ordinary differential equations." Numerische Mathematik 33.1 (1979): 55-68.
*/
class OdeGRK4A : public OdeEmbedded, private OdeRosenbrock {

    public:
        //!constructs
        /*!
        \param[in] neq size of ODE system
        */
        OdeGRK4A (unsigned long neq);

    private:
        //function for taking a single step
        void step_ (double dt);
        //coefficients
        double gam21,
               gam31, gam32,
               gam41, gam42, gam43;
        double g21,
               g31, g32,
               g41, g42, g43;
        double alp21,
               alp31, alp32;
        double b1, b2, b3, b4,
               d1, d2, d3;

};

} // namespace ode

#endif
