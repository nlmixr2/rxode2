#ifndef ODE_RKCK_H_
#define ODE_RKCK_H_

//! \file ode_rkck.h

#include "ode_embedded.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//!Explicit 5/4 pair, also with 3rd, 2nd, and 1st order embedded methods, from Cash & Karp
/*!
This class implements a 5th order method developed by Cash and Karp (reference below) which includes embeddes solutions of order 1, 2, 3, and 4. It's a complete family of solutions up to order 5. The lower order solutions can be used to vary the solution's order and control the step size in sophisticated ways, canceling a step early if the error estimate is large. The fact that function evaluations are made fairly evenly through the interval of a time step is also advantageous for surveying potential trouble and controlling the step.

So far, the sophisticated time stepping is not implemented, so this solver simply uses the 4th and 5th order solutions for adaptation.

    + J. R. Cash, A. H. Karp. "A variable order Runge-Kutta method for initial value problems with rapidly varying right-hand sides", ACM Transactions on Mathematical Software 16: 201-222, 1990. doi:10.1145/79505.79507
    + https://en.wikipedia.org/wiki/Cash%E2%80%93Karp_method
*/
class OdeRKCK : public OdeEmbedded, private OdeRK, private OdeERK {

    public:
        //!constructs
        /*!
        \param[in] neq size of ODE sytem
        */
        OdeRKCK (unsigned long neq);

    private:
        //function for taking a single time step
        void step_ (double dt);
        //coefficents of tableau
        double c2, a21,
               c3, a31, a32,
               c4, a41, a42, a43,
               c5, a51, a52, a53, a54,
               c6, a61, a62, a63, a64, a65,
                    b1,       b3,  b4,      b6,
                    d1,       d3,  d4,  d5, d6,
                    e1,       e3,  e4,
                    f1, f2,
                    g1;
};

} // namespace ode

#endif
