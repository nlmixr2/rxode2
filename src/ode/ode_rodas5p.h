#ifndef ODE_RODAS5P_H_
#define ODE_RODAS5P_H_

//! \file ode_rodas5p.h

#include "ode_linalg.h"
#include "ode_embedded.h"
#include "ode_rosenbrock.h"

namespace ode {

//!Fifth-order, L-stable, adaptive Rosenbrock method Rodas5P (8 stages,
//!embedded 4th order); rxode2's "ros54p"
/*!
    + Steinebach, Gerd. "Construction of Rosenbrock-Wanner method Rodas5P and
      numerical benchmarks within the Julia Differential Equations package."
      BIT Numerical Mathematics 63.2 (2023): 27.
*/
class OdeRODAS5P : public OdeEmbedded, protected OdeRosenbrock {

    public:
        //!constructs
        /*!
        \param[in] neq size of ODE system
        */
        OdeRODAS5P (unsigned long neq);

    protected:
        //function for taking a single step
        void step_ (double dt);
        //coefficients: A (stage states) and C (stage corrections) below the
        //diagonal by rows, and the stage times
        double A_[28], C_[28], c_[8];

};

} // namespace ode

#endif
