#ifndef ODE_VERN98_H_
#define ODE_VERN98_H_

//! \file ode_vern_98.h

#include "ode_embedded.h"
#include "ode_rk.h"
#include "ode_erk.h"

namespace ode {

//!Jim Verner's "most efficient" 9/8 pair
/*!
    + http://people.math.sfu.ca/~jverner/
*/
class OdeVern98 : public OdeEmbedded, private OdeRK, private OdeERK {

    public:
        //!constructs
        /*!
        \param[in] neq size of ODE sytem
        */
        OdeVern98 (unsigned long neq);

    private:
        //function for taking a single time step
        void step_ (double dt);
        //coefficents of tableau
        double c2,   a21,
               c3,   a31, a32,
               c4,   a41,       a43,
               c5,   a51,       a53,  a54,
               c6,   a61,             a64,  a65,
               c7,   a71,             a74,  a75,   a76,
               c8,   a81,                          a86,  a87,
               c9,   a91,                          a96,  a97, a98,
               c10, a101,                         a106, a107, a108, a109,
               c11, a111,                         a116, a117, a118, a119, a1110,
               c12, a121,                         a126, a127, a128, a129, a1210, a1211,
               c13, a131,                         a136, a137, a138, a139, a1310, a1311, a1312,
               c14, a141,                         a146, a147, a148, a149, a1410, a1411, a1412, a1413,
               c15, a151,                         a156, a157, a158, a159, a1510, a1511, a1512, a1513, a1514,
               c16, a161,                         a166, a167, a168, a169, a1610, a1611, a1612, a1613,
                      b1,                                       b8,   b9,   b10,   b11,   b12,   b13,   b14,   b15,
                      d1,                                       d8,   d9,   d10,   d11,   d12,   d13,               d16;
};

} // namespace ode

#endif
