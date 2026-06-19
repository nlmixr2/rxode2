#ifndef ODE_RKF45_H_
#define ODE_RKF45_H_

#include "ode_embedded.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

class OdeRkf45 : public OdeEmbedded, protected OdeRK, protected OdeERK {
public:
    OdeRkf45(unsigned long neq);
protected:
    virtual void step_(double dt);
        double c2, c3, c4, c5, c6;
        double a21;
        double a31, a32;
        double a41, a42, a43;
        double a51, a52, a53, a54;
        double a61, a62, a63, a64, a65;
        double b1, b2, b3, b4, b5, b6;
        double d1, d2, d3, d4, d5, d6;
};

} // namespace ode

#endif // ODE_RKF45_H_
