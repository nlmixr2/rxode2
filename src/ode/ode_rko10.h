#ifndef ODE_RKO10_H_
#define ODE_RKO10_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! 10th order Ono method (17 stages).
class OdeRko10 : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeRko10(unsigned long neq);
protected:
    virtual void step_(double dt);
    double a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16;
    double b21, b31, b32, b41, b43, b51, b53, b54, b61, b64, b65;
    double b71, b74, b75, b76, b81, b85, b86, b87;
    double b91, b96, b97, b98;
    double b101, b106, b107, b108, b109;
    double b111, b116, b117, b118, b119, b1110;
    double b121, b126, b127, b128, b129, b1210, b1211;
    double b131, b134, b135, b136, b137, b138, b139, b1310, b1311, b1312;
    double b141, b144, b145, b146, b147, b148, b149, b1410, b1411, b1412, b1413;
    double b151, b152, b156, b157, b1513, b1514;
    double b161, b163, b1615;
    double b171, b172, b173, b176, b177, b178, b179, b1710, b1711, b1712, b1713, b1714, b1715, b1716;
    double c1, c2, c3, c6, c7, c9, c10, c11, c12, c13, c14, c15, c16, c17;
};

} // namespace ode

#endif
