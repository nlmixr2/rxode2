#ifndef ODE_EMBEDDED_H_
#define ODE_EMBEDDED_H_

//! \file ode_embedded.h

#include <cmath>

#include "ode_io.h"
#include "ode_util.h"
#include "ode_adaptive.h"

namespace ode {

//!Base clase implementing methods for embedded Runge-Kutta error estimation
class OdeEmbedded : public OdeAdaptive {

    public:
        //!constructs
        /*!
        \param[in] neq size of ODE system
        \param[in] need_jac flag signaling whether the Jacobian of the system is needed
        \param[in] lowerord the order of the lower order embedded method (assumed to be one less than the higher order one)
        */
        OdeEmbedded (unsigned long neq, bool need_jac, int lowerord);
        //!destructs
        virtual ~OdeEmbedded ();

        //-------------------
        //getters and setters

        //!gets safety factor applied to time step selection
        double get_facsafe () { return(facsafe_); }
        //!gets minimum allowable fraction change in time step (a number <1)
        double get_facmin () { return(facmin_); }
        //!gets maximum allowable fraction change in time step (a number >1)
        double get_facmax () { return(facmax_); }

        //!sets safety factor applied to time step selection
        void set_facsafe (double facsafe) { facsafe_ = facsafe; }
        //!sets minimum allowable fraction change in time step (a number <1)
        void set_facmin (double facmin) {
            if ( facmin >= 1.0 )
                ode_print_exit("facmin must be a number less than 1 in set_facmin()");
            facmin_ = facmin;
        }
        //!sets maximum allowable fraction change in time step (a number >1)
        void set_facmax (double facmax) {
            if ( facmax <= 1.0 )
                ode_print_exit("facmax must be a number greater than 1 in set_facmax()");
            facmax_ = facmax;
        }

    protected:

        //!safety factor applied to time step selection
        double facsafe_;
        //!minimum allowable fraction change in time step
        double facmin_;
        //!maximum allowable fraction change in time step
        double facmax_;

        //!embedded solution array
        double *solemb_;
        //!calculates error estimate with lower and higher order solutions
        double error (double abstol, double reltol);
        //!calculates factor for "optimal" next time step
        double facopt (double err);

        //------------------------------------------------
        //implementation of inherited methods for adapting

        //!does the calculations to determine isrej and dtopt
        virtual void adapt (double abstol, double reltol);
        //!simply returns isrej
        virtual bool is_rejected ();
        //!simply returns dtopt
        virtual double dt_adapt ();

    private:
        //!order of the LOWER order solution used for error estimation
        int lowerord_;
        //!flag for rejecting a step
        bool isrej_;
        //!next time step
        double dtopt_;
};

} // namespace ode

#endif
