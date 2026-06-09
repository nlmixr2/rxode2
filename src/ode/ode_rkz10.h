#ifndef ODE_RKZ10_H_
#define ODE_RKZ10_H_

#include "ode_adaptive.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//! 10th order Zhang method (16 stages).
class OdeRkz10 : public OdeAdaptive, protected OdeRK, protected OdeERK {
public:
    OdeRkz10(unsigned long neq);
protected:
    virtual void step_(double dt);
    // c-node abscissae (computed from row sums)
    double a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16;
    // Butcher tableau entries
    double b21, b31, b32, b41, b42, b43, b51, b52, b53, b54;
    double b61, b62, b63, b64, b65, b71, b72, b73, b74, b75, b76;
    double b81, b82, b83, b84, b85, b86, b87;
    double b91, b92, b93, b94, b95, b96, b97, b98;
    double b101, b102, b103, b104, b105, b106, b107, b108, b109;
    double b111, b112, b113, b114, b115, b116, b117, b118, b119, b1110;
    double b121, b122, b123, b124, b125, b126, b127, b128, b129, b1210, b1211;
    double b131, b132, b133, b134, b135, b136, b137, b138, b139, b1310, b1311, b1312;
    double b141, b142, b143, b144, b145, b146, b147, b148, b149, b1410, b1411, b1412, b1413;
    double b151, b152, b153, b154, b155, b156, b157, b158, b159, b1510, b1511, b1512, b1513, b1514;
    double b161, b162, b163, b164, b165, b166, b167, b168, b169, b1610, b1611, b1612, b1613, b1614, b1615;
    // weights
    double c1, c3, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16;
};

} // namespace ode

#endif
