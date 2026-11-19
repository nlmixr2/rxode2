#ifndef ODE_RKS1110A_H_
#define ODE_RKS1110A_H_

#include "ode_embedded.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

class OdeRks1110a : public OdeEmbedded, protected OdeRK, protected OdeERK {
public:
    OdeRks1110a(unsigned long neq);
protected:
    virtual void step_(double dt);
        double c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, c23, c24, c25, c26;
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
        double a131, a132, a133, a134, a135, a136, a137, a138, a139, a1310, a1311, a1312;
        double a141, a142, a143, a144, a145, a146, a147, a148, a149, a1410, a1411, a1412, a1413;
        double a151, a152, a153, a154, a155, a156, a157, a158, a159, a1510, a1511, a1512, a1513, a1514;
        double a161, a162, a163, a164, a165, a166, a167, a168, a169, a1610, a1611, a1612, a1613, a1614, a1615;
        double a171, a172, a173, a174, a175, a176, a177, a178, a179, a1710, a1711, a1712, a1713, a1714, a1715, a1716;
        double a181, a182, a183, a184, a185, a186, a187, a188, a189, a1810, a1811, a1812, a1813, a1814, a1815, a1816, a1817;
        double a191, a192, a193, a194, a195, a196, a197, a198, a199, a1910, a1911, a1912, a1913, a1914, a1915, a1916, a1917, a1918;
        double a201, a202, a203, a204, a205, a206, a207, a208, a209, a2010, a2011, a2012, a2013, a2014, a2015, a2016, a2017, a2018, a2019;
        double a211, a212, a213, a214, a215, a216, a217, a218, a219, a2110, a2111, a2112, a2113, a2114, a2115, a2116, a2117, a2118, a2119, a2120;
        double a221, a222, a223, a224, a225, a226, a227, a228, a229, a2210, a2211, a2212, a2213, a2214, a2215, a2216, a2217, a2218, a2219, a2220, a2221;
        double a231, a232, a233, a234, a235, a236, a237, a238, a239, a2310, a2311, a2312, a2313, a2314, a2315, a2316, a2317, a2318, a2319, a2320, a2321, a2322;
        double a241, a242, a243, a244, a245, a246, a247, a248, a249, a2410, a2411, a2412, a2413, a2414, a2415, a2416, a2417, a2418, a2419, a2420, a2421, a2422, a2423;
        double a251, a252, a253, a254, a255, a256, a257, a258, a259, a2510, a2511, a2512, a2513, a2514, a2515, a2516, a2517, a2518, a2519, a2520, a2521, a2522, a2523, a2524;
        double a261, a262, a263, a264, a265, a266, a267, a268, a269, a2610, a2611, a2612, a2613, a2614, a2615, a2616, a2617, a2618, a2619, a2620, a2621, a2622, a2623, a2624, a2625;
        double b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20, b21, b22, b23, b24, b25, b26;
        double d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17, d18, d19, d20, d21, d22, d23, d24, d25, d26;
};

} // namespace ode

#endif // ODE_RKS1110A_H_
