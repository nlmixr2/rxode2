#ifndef ODE_ADAPTIVE_H_
#define ODE_ADAPTIVE_H_

//! \file ode_adaptive.h

#include <cstdio>
#include <cmath>
#include <vector>
#include <cstring>
#include <string>

#include "ode_io.h"
#include "ode_util.h"
#include "ode_base.h"

namespace ode {

//! Base class implementing solver functions with adaptive time steps
/*!
* The OdeAdaptive class is a base for any integrator that can modify its own time step as it integrates and reject steps. The class inherits from the deepest base class, ode_base. It implements three virtual functions to enable time step adaptation:
*  1. The adapt function is meant to execute whatever calculations are needed
*     to subsequently assess whether the current time step should be rejected
*     and what the next time step should be.
*  2. The is_rejected function simply furnishes a boolean answering the
*     question, "should the step just taken be rejected?"
*  3. The dt_adapt function furnishes the next time step, whether the step
*     current step was rejected or not.
*These functions are used with local error tolerances to integrate with an efficiently adaptive time step. Specifically, they are used in the step_adaptive_ method of this class, which is called in each of the solve_adaptive methods.
*/
class OdeAdaptive : public OdeBase {

    public:
        //!constructs
        /*!
        \param[in] neq number of equations in ODE system
        \param[in] need_jac flag signaling whether the Jacobian of the system is needed
        */
        OdeAdaptive (unsigned long neq, bool need_jac);
        //!destructs
        virtual ~OdeAdaptive ();

        //-------------------
        //getters and setters

        //!gets the count of rejected steps
        long unsigned get_nrej () { return(nrej_); }
        //!gets the absolute error tolerance
        double get_abstol () { return(abstol_); }
        //!gets the relative error tolerance
        double get_reltol () { return(reltol_); }
        //!gets the maximum allowable time step
        double get_dtmax () { return(dtmax_); }

        //!sets the absolute error tolerance
        void set_abstol (double tol) { abstol_ = tol; };
        //!sets the relative error tolerance
        void set_reltol (double tol) { reltol_ = tol; };
        //!sets the absolute and relative error tolerance to the same value
        void set_tol (double tol) { abstol_ = tol; reltol_ = tol; };
        //!sets the maximum allowable time step
        void set_dtmax (double dtmax) { dtmax_ = dtmax; }

        //---------------------------------------
        //integration with adaptive time stepping

        //!integrates for a specified duration of independent variable without output
        /*!
        \param[in] tint total integration time
        \param[in] dt0 initial time step size
        \param[in] extras whether to call all the extra functions (before_solve, after_step, ...)
        */
        void solve_adaptive (double tint, double dt0, bool extras=true);

        //!lots of output, solves and stores every "inter" point along the way
        /*!
        \param[in] tint total integration time
        \param[in] dt0 initial time step size
        \param[in] dirout output directory (must already exist)
        \param[in] inter interval of steps to store and output
        */
        void solve_adaptive (double tint, double dt0, const char *dirout, int inter);

        //!solves and writes evenly spaced snapshots
        /*!
        \param[in] tint total integration time
        \param[in] dt0 initial time step size
        \param[in] nsnap number of snapshots to output
        \param[in] dirout output directory (must already exist)
        */
        void solve_adaptive (double tint, double dt0, unsigned long nsnap, const char *dirout);

        //!solves and writes snapshots at times specified in the tsnap array
        /*!
        \param[in] dt0 initial time step size
        \param[in] tsnap array of desired snapshot times
        \param[in] nsnap number of snapshots (length of tsnap)
        \param[in] dirout output directory (must already exist)
        */
        void solve_adaptive (double dt0, double *tsnap, unsigned long nsnap, const char *dirout);

    protected:

        //!integrates without output or any counters, trackers, extra functions...
        /*!
        \param[in] tint total integration time
        \param[in] dt0 initial time step size
        \param[in] extra whether to call after_step()
        */
        void solve_adaptive_ (double tint, double dt0, bool extra=true);

        //------------------------
        //basic adaptive variables

        //!counter for rejected steps
        long unsigned nrej_;
        //!absolute error tolerance
        double abstol_;
        //!absolute error tolerance
        double reltol_;
        //!maximum allowable time step
        double dtmax_;

        //!executes whatever calculations need to be performed for adapting, including a determination of whether a step is rejected and a calculation of the next time step size
        virtual void adapt (double abstol, double reltol);
        //!retreives a bool determining whether a step is accepted/rejected, false by default
        virtual bool is_rejected ();
        //!retrieves the best time step for the next step
        virtual double dt_adapt ();

        //!determines whether an adaptive solve is finished
        bool solve_done_adaptive (double tend);

        //wrappers
        //!executes a single time and calls all necessary adapting functions
        bool step_adaptive_ (double dt, bool extra=true);
        //!wrapper around dt_adapt() to perform additional checks
        double dt_adapt_ (double tend);

    private:
        //!previous solution, in case a step is rejected
        double *solprev_;
};

} // namespace ode

#endif
