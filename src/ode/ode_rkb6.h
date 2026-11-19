#ifndef ODE_RKB6_H_
#define ODE_RKB6_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! 6th order Butcher method (7 stages).
class OdeRkb6 : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeRkb6(unsigned long neq);
protected:
    virtual void step_(double dt);
    double a2, a3, a4, a5, a6;
    double b21, b32, b41, b42, b43, b51, b52, b53, b54, b62, b63, b64, b65, b71, b72, b73, b74, b76;
    double c1, c3, c4, c5, c6, c7;
};

} // namespace ode

#endif
