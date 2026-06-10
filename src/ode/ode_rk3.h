#ifndef ODE_RK3_H_
#define ODE_RK3_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! 3rd order, 3-stage Runge-Kutta method.
class OdeRk3 : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeRk3(unsigned long neq);
protected:
    virtual void step_(double dt);
    double a1, a2, a3, b2, c21, c32;
};

} // namespace ode

#endif
