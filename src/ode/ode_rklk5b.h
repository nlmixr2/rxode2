#ifndef ODE_RKLK5B_H_
#define ODE_RKLK5B_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! 5th order Luther-Konen method 2 (6 stages).
class OdeRklk5b : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeRklk5b(unsigned long neq);
protected:
    virtual void step_(double dt);
    double a2, a3, a5, a6;
    double b21, b31, b32, b41, b42, b43, b51, b52, b53, b54, b61, b62, b63, b64, b65;
    double c3, c5, c6;
};

} // namespace ode

#endif
