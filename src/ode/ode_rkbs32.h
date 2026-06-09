#ifndef ODE_RKBS32_H_
#define ODE_RKBS32_H_

#include "ode_embedded.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

class OdeRkbs32 : public OdeEmbedded, protected OdeRK, protected OdeERK {
public:
    OdeRkbs32(unsigned long neq);
protected:
    virtual void step_(double dt);
        double c2, c3, c4;
        double a21;
        double a31, a32;
        double a41, a42, a43;
        double b1, b2, b3, b4;
        double d1, d2, d3, d4;
};

} // namespace ode

#endif // ODE_RKBS32_H_
