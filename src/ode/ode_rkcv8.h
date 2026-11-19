#ifndef ODE_RKCV8_H_
#define ODE_RKCV8_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! 8th order Cooper-Verner method (11 stages).
class OdeRkcv8 : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeRkcv8(unsigned long neq);
protected:
    virtual void step_(double dt);
    double a2, a3, a4, a5, a6, a7, a8, a9, a10;
    double b21, b31, b32, b41, b42, b43, b51, b53, b54, b61, b63, b64, b65;
    double b71, b73, b74, b75, b76, b81, b85, b86, b87;
    double b91, b95, b96, b97, b98;
    double b101, b105, b106, b107, b108, b109;
    double b115, b116, b117, b118, b119, b1110;
    double c1, c8, c9, c10, c11;
};

} // namespace ode

#endif
