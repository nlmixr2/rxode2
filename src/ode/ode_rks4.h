#ifndef ODE_RKS4_H_
#define ODE_RKS4_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! 4th order Runge-Kutta Shanks (4 stages).
class OdeRks4 : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeRks4(unsigned long neq);
protected:
    virtual void step_(double dt);
    double a1, a2, cv, c0, c1, c2, c3;
    double aa1, aa2, aa3;
    double b20, b21, b30, b31, b32;
};

} // namespace ode

#endif
