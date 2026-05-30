#include "ode/ode_rkssp43.h"

namespace ode {

// ── OdeRkssp43 ──────────────────────────────────────────────────
// SSP 4-stage 3(2)
OdeRkssp43::OdeRkssp43(unsigned long neq)
    : OdeEmbedded(neq, false, 2),
      OdeRK(neq, 4),
      OdeERK(neq)
{
    method_ = "Rkssp43";

}

void OdeRkssp43::step_(double dt) {
    // This stub satisfies the vtable; the bridge override is called at runtime.
    (void)dt;
}

} // namespace ode
