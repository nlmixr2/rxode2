#ifndef ODE_SSP3_H_
#define ODE_SSP3_H_

//! \file ode_ssp_3.h

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//!Strong stability preserving method of order 3
/*!
    + C. W. Shu and S. Osher, Effcient implementation of essentially nonoscillatory shock-capturing schemes, J. Comput. Phys., 77, 1988, pp. 439-471.
*/
class OdeSsp3 : public OdeAdaptive, protected OdeRK, protected OdeERK {

    public:
        //!constructs
        /*!
        \param[in] neq size of ODE sytem
        */
        OdeSsp3 (unsigned long neq);

    protected:
        //take a time step
        virtual void step_ (double dt);
        //coefficents of tableau
        double c2, a21,
               c3, a31, a32,
                    b1,  b2, b3;
};

} // namespace ode

#endif
