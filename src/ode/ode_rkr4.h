#ifndef ODE_RKR4_H_
#define ODE_RKR4_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"
#include <cmath>

namespace ode {

//! 4th order Runge-Kutta Ralston (minimum truncation error).
class OdeRkr4 : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeRkr4(unsigned long neq);
protected:
    virtual void step_(double dt);
    double a2, a3;
    double b21, b31, b32, b41, b42, b43;
    double c1, c2, c3, c4;
};

} // namespace ode

#endif
