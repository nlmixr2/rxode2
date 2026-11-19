#ifndef ODE_RK_H_
#define ODE_RK_H_

namespace ode {

//! \file ode_rk.h

//!Provides space for stage slope values, an array of arrays for k values
class OdeRK {

    public:
        //!constructs
        /*!
        \param[in] neq size of ODE sytem
        \param[out] nk number of stages
        */
        OdeRK (unsigned long neq, int nk);
        //!destructs
        ~OdeRK ();

    protected:
        //!stage evaluations
        double **k_;
        //!number of stages, or k vectors
        int nk_;

};

} // namespace ode

#endif
