#ifndef ODE_RKLS54_H_
#define ODE_RKLS54_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! 5-stage, 4th order low-storage Runge-Kutta (Carpenter-Kennedy 1994).
class OdeRkls54 : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeRkls54(unsigned long neq);
protected:
    virtual void step_(double dt);
    double a2, a3, a4, a5;
    double b1, b2, b3, b4, b5;
    double c2, c3, c4, c5;
};

} // namespace ode

#endif
