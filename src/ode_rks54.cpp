#include "ode/ode_rks54.h"

namespace ode {

// -- OdeRks54 --------------------------------------------------
// Stepanov 5(4) FSAL
OdeRks54::OdeRks54(unsigned long neq)
    : OdeEmbedded(neq, false, 4),
      OdeRK(neq, 7),
      OdeERK(neq)
{
    method_ = "Rks54";
    c2 = 1.0 / 5.0;
    c3 = 21.0 / 65.0;
    c4 = 9.0 / 10.0;
    c5 = 39.0 / 40.0;
    a21 = 1.0 / 5.0;
    a31 = 21.0 / 338.0;
    a32 = 441.0 / 1690.0;
    a41 = 639.0 / 392.0;
    a42 = -729.0 / 140.0;
    a43 = 1755.0 / 392.0;
    a51 = 4878991.0 / 1693440.0;
    a52 = -16601.0 / 1792.0;
    a53 = 210067.0 / 28224.0;
    a54 = -1469.0 / 17280.0;
    a61 = 13759919.0 / 4230954.0;
    a62 = -2995.0 / 287.0;
    a63 = 507312091.0 / 61294590.0;
    a64 = -22.0 / 405.0;
    a65 = -7040.0 / 180687.0;
    b1 = 1441.0 / 14742.0;
    b3 = 114244.0 / 234927.0;
    b4 = 118.0 / 81.0;
    b5 = -12800.0 / 4407.0;
    b6 = 41.0 / 22.0;

    // d_N = b_N - e_N (embedded weights from error coefficients)
    d1 = b1 - (-1.0 / 273.0);  // d_N = b_N - e_N
    d3 = b3 - (2197.0 / 174020.0);  // d_N = b_N - e_N
    d4 = b4 - (-4.0 / 15.0);  // d_N = b_N - e_N
    d5 = b5 - (1280.0 / 1469.0);  // d_N = b_N - e_N
    d6 = b6 - (-33743.0 / 52712.0);  // d_N = b_N - e_N
    d7 = -(127.0 / 4792.0);  // d_N = -e_N (b_N = 0)
    b7 = 0.0;

}

void OdeRks54::step_(double dt) {
    // This stub satisfies the vtable; the bridge override is called at runtime.
    (void)dt;
}

} // namespace ode
