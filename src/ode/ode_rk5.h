#ifndef ODE_RK5_H_
#define ODE_RK5_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! 5th order Runge's method (6 stages).
class OdeRk5 : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeRk5(unsigned long neq);
protected:
    virtual void step_(double dt);
    double a2, a3, a5, a6;
    double b21, b32, b41, b42, b43, b51, b52, b53, b54, b61, b62, b63, b64;
    double c1, c3, c4, c5, c6;
};

} // namespace ode

#endif
