#ifndef ODE_VERN65_H_
#define ODE_VERN65_H_

//! \file ode_vern_65.h

#include "ode_embedded.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//!Jim Verner's "most efficient" 6/5 pair
/*!
    + http://people.math.sfu.ca/~jverner/
*/
class OdeVern65 : public OdeEmbedded, private OdeRK, private OdeERK {

    public:
        //!constructs
        /*!
        \param[in] neq size of ODE sytem
        */
        OdeVern65 (unsigned long neq);

    private:
        //function for taking a single time step
        void step_ (double dt);
        //coefficents of tableau
        double c2, a21,
               c3, a31, a32,
               c4, a41,      a43,
               c5, a51,      a53, a54,
               c6, a61,      a63, a64, a65,
               c7, a71,      a73, a74, a75, a76,
               c8, a81,      a83, a84, a85, a86, a87,
               c9, a91,           a94, a95, a96, a97, a98,
                    b1,            b4,  b5,  b6,  b7,  b8,
                    d1,            d4,  d5,  d6,       d8,  d9;
};

} // namespace ode

#endif
