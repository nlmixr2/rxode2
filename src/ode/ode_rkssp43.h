#ifndef ODE_RKSSP43_H_
#define ODE_RKSSP43_H_

#include "ode_embedded.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

class OdeRkssp43 : public OdeEmbedded, protected OdeRK, protected OdeERK {
public:
    OdeRkssp43(unsigned long neq);
protected:
    virtual void step_(double dt);
};

} // namespace ode

#endif // ODE_RKSSP43_H_
