#ifndef ODE_IRK_H_
#define ODE_IRK_H_

namespace ode {

//! \file ode_irk.h

//!Provides a large vector containing the slope values of all stages with pointers to each of the individual stages
class OdeIRK {
    public:
        //!constructs
        /*!
        \param[in] neq size of ODE system
        \param[in] nk number of RK stages
        */
        OdeIRK (unsigned long neq, int nk);
        //!destructs
        ~OdeIRK ();
    protected:
        //!number of RK stages
        int nk_;
        //!pointer to single array storing all stage k values
        double *kall_;
        //!individual k arrays for each stage
        double **k_;
};

} // namespace ode

#endif
