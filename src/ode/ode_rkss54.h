#ifndef ODE_RKSS54_H_
#define ODE_RKSS54_H_

#include "ode_embedded.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

class OdeRkss54 : public OdeEmbedded, protected OdeRK, protected OdeERK {
public:
    OdeRkss54(unsigned long neq);
protected:
    virtual void step_(double dt);
        double c2, c3, c4, c5, c6, c7;
        double a21;
        double a31, a32;
        double a41, a42, a43;
        double a51, a52, a53, a54;
        double a61, a62, a63, a64, a65;
        double a71, a72, a73, a74, a75, a76;
        double b1, b2, b3, b4, b5, b6, b7;
        double d1, d2, d3, d4, d5, d6, d7;
};

} // namespace ode

#endif // ODE_RKSS54_H_
