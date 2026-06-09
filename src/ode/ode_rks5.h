#ifndef ODE_RKS5_H_
#define ODE_RKS5_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! 5th order Runge-Kutta Shanks (5 stages).
class OdeRks5 : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeRks5(unsigned long neq);
protected:
    virtual void step_(double dt);
    double a1, a2, a3, cv, c0, c2, c3, c4;
    double aa1, aa2, aa3, aa4;
    double b20, b21, b30, b31, b32, b40, b41, b42, b43;
};

} // namespace ode

#endif
