#ifndef ODE_RKV76R_H_
#define ODE_RKV76R_H_

#include "ode_embedded.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

class OdeRkv76r : public OdeEmbedded, protected OdeRK, protected OdeERK {
public:
    OdeRkv76r(unsigned long neq);
protected:
    virtual void step_(double dt);
        double c2, c3, c4, c5, c6, c7, c8, c9, c10;
        double a21;
        double a31, a32;
        double a41, a42, a43;
        double a51, a52, a53, a54;
        double a61, a62, a63, a64, a65;
        double a71, a72, a73, a74, a75, a76;
        double a81, a82, a83, a84, a85, a86, a87;
        double a91, a92, a93, a94, a95, a96, a97, a98;
        double a101, a102, a103, a104, a105, a106, a107, a108, a109;
        double b1, b2, b3, b4, b5, b6, b7, b8, b9, b10;
        double d1, d2, d3, d4, d5, d6, d7, d8, d9, d10;
};

} // namespace ode

#endif // ODE_RKV76R_H_
