#ifndef ODE_HEUN_H_
#define ODE_HEUN_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! Heun (2nd order) integration method.
class OdeHeun : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeHeun(unsigned long neq);
protected:
    virtual void step_(double dt);
    double c2, a21, b1, b2;
};

} // namespace ode

#endif
