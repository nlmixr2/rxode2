#ifndef ODE_RK8_12_H_
#define ODE_RK8_12_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! 8th order Shanks method, 12 stages.
class OdeRk8_12 : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeRk8_12(unsigned long neq);
protected:
    virtual void step_(double dt);
    double a1, a2, a3, a4, a5, a6, a7, a8, a9, a10;
    double cv, c0, c5, c6, c7, c8, c9, c10, c11;
    double aa1, aa2, aa3, aa4, aa5, aa6, aa7, aa8, aa9, aa10, aa11;
    double b21, b32, b40, b42, b43, b50, b53, b54, b60, b63, b64, b65;
    double b70, b73, b74, b76, b80, b83, b84, b85, b86, b87;
    double b90, b93, b94, b95, b96, b97, b98;
    double b100, b103, b104, b105, b106, b109;
    double b110, b113, b114, b115, b116, b117, b118, b119, b1110;
};

} // namespace ode

#endif
