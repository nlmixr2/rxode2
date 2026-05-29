#ifndef ODE_RK43_H_
#define ODE_RK43_H_

//! \file ode_rk_43.h

#include "ode_embedded.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//!This class implements a 3rd and 4th order method with the FSAL (first same as last) property
class OdeRK43 : public OdeEmbedded, private OdeRK, private OdeERK {

    public:
        //!constructs
        /*!
        \param[in] neq size of ODE sytem
        */
        OdeRK43 (unsigned long neq);

    private:
        //function for taking a single time step
        void step_ (double dt);
        //coefficents of tableau
        double c2, a21,
               c3, a31, a32,
               c4, a41, a42, a43,
               c5, a51, a52, a53, a54,
                    b1,  b2,  b3,  b4,
                    d1,  d2,  d3,      d5;
};

} // namespace ode

#endif
