#ifndef ODE_TRAPZ_H_
#define ODE_TRAPZ_H_

//! \file ode_trapz.h

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//!Second order, explicit trapezoidal rule
class OdeTrapz : public OdeAdaptive, protected OdeRK, protected OdeERK {

    public:
        //!constructs
        /*!
        \param[in] neq size of ODE sytem
        */
        OdeTrapz (unsigned long neq);

    protected:
        //function for taking a single time step
        virtual void step_ (double dt);
        //coefficents of tableau
        double c2, a21,
                    b1, b2;
};

} // namespace ode

#endif
