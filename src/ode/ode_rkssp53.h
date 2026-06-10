#ifndef ODE_RKSSP53_H_
#define ODE_RKSSP53_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! 5-stage, 3rd order SSP Runge-Kutta (Spiteri-Ruuth 2005).
class OdeRkssp53 : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeRkssp53(unsigned long neq);
protected:
    virtual void step_(double dt);
    double a30, a32, a40, a43, a52, a54;
    double b10, b21, b32, b43, b54;
    double c1, c2, c3, c4;
};

} // namespace ode

#endif
