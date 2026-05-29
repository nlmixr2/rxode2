#ifndef ODE_DOPRI87_H_
#define ODE_DOPRI87_H_

//! \file ode_dopri_87.h

#include "ode_embedded.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//!Explicit 8/7 pair from Dormand & Prince
/*!
This class implements the 7th and 8th order method of Dormand and Prince, as reprinted in the paper below. The tableau coefficients are terribly unweildy, but the solver is scary accurate.
    + E. Hairer, S. P. Nørsett, and G. Wanner. 1993. Solving Ordinary Differential Equations I (2nd Revised. Ed.): Nonstiff Problems. Springer-Verlag, Berlin, Heidelberg.
*/
class OdeDoPri87 : public OdeEmbedded, private OdeRK, private OdeERK {

    public:
        //!constructs
        /*!
        \param[in] neq size of ODE system
        */
        OdeDoPri87 (unsigned long neq);

    private:
        //function for taking a single time step
        void step_ (double dt);
        //coefficents of tableau
        double c2,   a21,
               c3,   a31,   a32,
               c4,   a41,         a43,
               c5,   a51,         a53,  a54,
               c6,   a61,               a64,  a65,
               c7,   a71,               a74,  a75,  a76,
               c8,   a81,               a84,  a85,  a86,  a87,
               c9,   a91,               a94,  a95,  a96,  a97,  a98,
               c10,  a101,             a104, a105, a106, a107, a108, a109,
               c11,  a111,             a114, a115, a116, a117, a118, a119, a1110,
               c12,  a121,             a124, a125, a126, a127, a128, a129, a1210, a1211,
               c13,  a131,             a134, a135, a136, a137, a138, a139, a1310, a1311,
                       b1,                           b6,   b7,   b8,   b9,   b10,   b11,   b12,   b13,
                       d1,                           d6,   d7,   d8,   d9,   d10,   d11,   d12;
};

} // namespace ode

#endif
