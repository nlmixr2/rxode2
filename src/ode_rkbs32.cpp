#include "ode/ode_rkbs32.h"

namespace ode {

// ── OdeRkbs32 ──────────────────────────────────────────────────
// Bogacki-Shampine 3(2) FSAL
OdeRkbs32::OdeRkbs32(unsigned long neq)
    : OdeEmbedded(neq, false, 2),
      OdeRK(neq, 4),
      OdeERK(neq)
{
    method_ = "Rkbs32";
    c2 = 1.0 / 2.0;
    c3 = 3.0 / 4.0;
    a21 = 1.0 / 2.0;
    a32 = 3.0 / 4.0;
    a41 = 2.0 / 9.0;
    a42 = 1.0 / 3.0;
    a43 = 4.0 / 9.0;
    b1 = 2.0 / 9.0;
    b2 = 1.0 / 3.0;
    b3 = 4.0 / 9.0;
    b4 = 0.0;
    d1 = 7.0 / 24.0;
    d2 = 1.0 / 4.0;
    d3 = 1.0 / 3.0;
    d4 = 1.0 / 8.0;
}

void OdeRkbs32::step_(double dt) {
    // This stub satisfies the vtable; the bridge override is called at runtime.
    (void)dt;
}

} // namespace ode
