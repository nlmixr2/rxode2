#include "ode/ode_rkf45.h"

namespace ode {

// -- OdeRkf45 --------------------------------------------------
// Fehlberg 4(5)
OdeRkf45::OdeRkf45(unsigned long neq)
    : OdeEmbedded(neq, false, 4),
      OdeRK(neq, 6),
      OdeERK(neq)
{
    method_ = "Rkf45";
    c2 = 1.0  / 4.0;
    c3 = 3.0  / 8.0;
    c4 = 12.0 / 13.0;
    c6 = 1.0  / 2.0;
    a21 = 1.0 / 4.0;
    a31 = 3.0 / 32.0;
    a32 = 9.0 / 32.0;
    a41 = 1932.0  / 2197.0;
    a42 = -7200.0 / 2197.0;
    a43 = 7296.0  / 2197.0;
    a51 = 439.0  / 216.0;
    a52 = -8.0;
    a53 = 3680.0 / 513.0;
    a54 = -845.0 / 4104.0;
    a61 = -8.0     / 27.0;
    a62 = 2.0;
    a63 = -3544.0  / 2565.0;
    a64 = 1859.0  / 4104.0;
    a65 = -11.0    / 40.0;
    b1 = 16.0    / 135.0;
    b3 = 6656.0  / 12825.0;
    b4 = 28561.0 / 56430.0;
    b5 = -9.0    / 50.0;
    b6 = 2.0     / 55.0;
    d1 = 25.0 / 216.0;
    d3 = 1408.0 / 2565.0;
    d4 = 2197.0 / 4104.0;
    d5 = -1.0  / 5.0;
    d6 = 0.0;

}

void OdeRkf45::step_(double dt) {
    // This stub satisfies the vtable; the bridge override is called at runtime.
    (void)dt;
}

} // namespace ode
