#ifndef ODE_EULER_H_
#define ODE_EULER_H_

//! \file ode_euler.h

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! Euler (1st order) integration method.
class OdeEuler : public OdeAdaptive, protected OdeRK, protected OdeERK {

    public:
        OdeEuler(unsigned long neq);

    protected:
        virtual void step_(double dt);
};

} // namespace ode

#endif
