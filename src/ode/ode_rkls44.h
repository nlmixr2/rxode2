#ifndef ODE_RKLS44_H_
#define ODE_RKLS44_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! 4-stage, 4th order low-storage non-TVD Runge-Kutta (Jiang-Shu).
class OdeRkls44 : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeRkls44(unsigned long neq);
protected:
    virtual void step_(double dt);
};

} // namespace ode

#endif
