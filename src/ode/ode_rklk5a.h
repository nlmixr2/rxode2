#ifndef ODE_RKLK5A_H_
#define ODE_RKLK5A_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! 5th order Luther-Konen method 1 (6 stages).
class OdeRklk5a : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeRklk5a(unsigned long neq);
protected:
    virtual void step_(double dt);
    double a2, a3, a4, a5;
    double b21, b31, b32, b41, b42, b51, b52, b53, b54, b61, b62, b63, b64, b65;
    double c1, c3, c5, c6;
};

} // namespace ode

#endif
