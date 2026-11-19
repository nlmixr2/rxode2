#ifndef ODE_RKTP86_H_
#define ODE_RKTP86_H_

#include "ode_embedded.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

class OdeRktp86 : public OdeEmbedded, protected OdeRK, protected OdeERK {
public:
    OdeRktp86(unsigned long neq);
protected:
    virtual void step_(double dt);
        double c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12;
        double a21;
        double a31, a32;
        double a41, a42, a43;
        double a51, a52, a53, a54;
        double a61, a62, a63, a64, a65;
        double a71, a72, a73, a74, a75, a76;
        double a81, a82, a83, a84, a85, a86, a87;
        double a91, a92, a93, a94, a95, a96, a97, a98;
        double a101, a102, a103, a104, a105, a106, a107, a108, a109;
        double a111, a112, a113, a114, a115, a116, a117, a118, a119, a1110;
        double a121, a122, a123, a124, a125, a126, a127, a128, a129, a1210, a1211;
        double b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12;
        double d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12;
};

} // namespace ode

#endif // ODE_RKTP86_H_
