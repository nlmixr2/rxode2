#ifndef ODE_RKSSP54_H_
#define ODE_RKSSP54_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! 5-stage, 4th order SSP Runge-Kutta (Spiteri-Ruuth 2005).
class OdeRkssp54 : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeRkssp54(unsigned long neq);
protected:
    virtual void step_(double dt);
    double b10, a20, a21, b21, a30, a32, b32, a40, a43, b43;
    double a52, a53, b53, a54, b54;
    double c1, c2, c3, c4;
};

} // namespace ode

#endif
