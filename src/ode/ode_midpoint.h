#ifndef ODE_MIDPOINT_H_
#define ODE_MIDPOINT_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! Midpoint (2nd order) integration method.
class OdeMidpoint : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeMidpoint(unsigned long neq);
protected:
    virtual void step_(double dt);
    double c2, a21;
};

} // namespace ode

#endif
