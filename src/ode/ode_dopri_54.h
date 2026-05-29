#ifndef ODE_DOPRI54_H_
#define ODE_DOPRI54_H_

//! \file ode_dopri_54.h

#include "ode_embedded.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//!Popular explicit 5/4 pair from Dormand & Prince
/*!
Famous 5/4 pair from Dormand & Prince, also supposedly implemented as [ode45](https://www.mathworks.com/help/matlab/ref/ode45.html) in MATLAB
    + Dormand, John R., and Peter J. Prince. "A family of embedded Runge-Kutta formulae." Journal of computational and applied mathematics 6.1 (1980): 19-26.
    + https://en.wikipedia.org/wiki/Dormand%E2%80%93Prince_method
*/
class OdeDoPri54 : public OdeEmbedded, private OdeRK, private OdeERK {

    public:
        //!constructs
        /*!
        \param[in] neq size of ODE system
        */
        OdeDoPri54 (unsigned long neq);

    private:
        //function for taking a single time step
        void step_ (double dt);
        //coefficents of tableau
        double c2, a21,
               c3, a31, a32,
               c4, a41, a42, a43,
               c5, a51, a52, a53, a54,
               c6, a61, a62, a63, a64, a65,
               c7, a71, a72, a73, a74, a75, a76,
                    b1,  b2,  b3,  b4,  b5,  b6,
                    d1,  d2,  d3,  d4,  d5,  d6, d7;
};

} // namespace ode

#endif
