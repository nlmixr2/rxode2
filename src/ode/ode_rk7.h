#ifndef ODE_RK7_H_
#define ODE_RK7_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! 7th order Shanks method (9 stages).
class OdeRk7 : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeRk7(unsigned long neq);
protected:
    virtual void step_(double dt);
    double a1, a2, a3, a4, a5, a6, a7;
    double cv, c0, c3, c4, c5, c6, c7, c8;
    double aa1, aa2, aa3, aa4, aa5, aa6, aa7, aa8;
    double b21, b32, b40, b42, b43, b50, b52, b53, b54;
    double b60, b62, b63, b64, b65, b70, b72, b73, b74, b75, b76;
    double b80, b82, b83, b84, b85, b86, b87;
};

} // namespace ode

#endif
