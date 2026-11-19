#ifndef ODE_RK8_10_H_
#define ODE_RK8_10_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! 8th order Shanks method, 10 stages (Formula 8-10).
class OdeRk8_10 : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeRk8_10(unsigned long neq);
protected:
    virtual void step_(double dt);
    double a1, a2, a3, a4, a5, a6, a8;
    double cv, c0, c3, c4, c5, c6, c8, c9;
    double aa1, aa2, aa3, aa4, aa5, aa6, aa7, aa8, aa9;
    double b21, b32, b43, b50, b52, b53, b54, b60, b62, b63, b64, b65;
    double b70, b72, b73, b74, b75, b76, b80, b82, b83, b84, b85, b86, b87;
    double b90, b92, b93, b94, b95, b96, b97, b98;
};

} // namespace ode

#endif
