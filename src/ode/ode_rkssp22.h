#ifndef ODE_RKSSP22_H_
#define ODE_RKSSP22_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! 2-stage, 2nd order TVD SSP Runge-Kutta (Shu-Osher 1988).
class OdeRkssp22 : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeRkssp22(unsigned long neq);
protected:
    virtual void step_(double dt);
};

} // namespace ode

#endif
