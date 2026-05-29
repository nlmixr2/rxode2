#ifndef ODE_RK4_H_
#define ODE_RK4_H_

//! \file ode_rk_4.h

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//!The classic Runge-Kutta 4th order method
/*!
    + http://mathworld.wolfram.com/Runge-KuttaMethod.html
*/
class OdeRK4 : public OdeAdaptive, private OdeRK, private OdeERK {

    public:
        //!constructs
        /*!
        \param[in] neq size of ODE sytem
        */
        OdeRK4 (unsigned long neq);

    private:
        //take a time step
        void step_ (double dt);
        //coefficents of tableau
        double c2, a21,
               c3,      a32,
               c4,           a43,
                    b1,  b2,  b3,  b4;
};

} // namespace ode

#endif
