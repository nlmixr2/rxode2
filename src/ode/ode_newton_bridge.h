//! \file ode_newton_bridge.h
/*!
These classes provides some variables of convenience when connecting the
Newton base (which is left as a generic Newton solver for potential reuse
elsewhere) and the systems required for implicit methods.
*/

#ifndef ODE_NEWTON_BRIDGE_H_
#define ODE_NEWTON_BRIDGE_H_

#include "ode_newton.h"

namespace ode {

//!Templated base class connecting solver objects and OdeNewton objects
template <class Integrator>
class OdeNewtonBridge : public OdeNewton {

    public:
        //!constructs
        /*!
        \param[in] neq size of ODE system
        \param[in] nnew size of Newton system
        \param[in] integrator pointer to integrator object
        */
        OdeNewtonBridge (unsigned long neq, unsigned long nnew, Integrator *integrator)
            : OdeNewton (nnew) {

            //make the default Newton iteration limit quite high
            set_iter_Newton(10000);

            //store pointers to the ODE solver object and its members
            integrator_ = integrator;
            sol_ = integrator_->sol_;
            Jac_ = integrator_->Jac_;
            dt_ = &(integrator_->dt_);
            //store ODE system size
            neq_ = integrator_->neq_;
            //store Newton system size;
            nnew_ = nnew;

            //temporary values for evaluation of ODE fun within Newton fun
            ftemp_ = new double[neq];
            //temporary solution values
            soltemp_ = new double[neq];
        }
        //!destructs
        ~OdeNewtonBridge () {
            delete [] ftemp_;
            delete [] soltemp_;
        }

    protected:

        //!ODE system size
        unsigned long neq_;
        //!Newton system size
        unsigned long nnew_;
        //!storage of a pointer to the solver class
        Integrator *integrator_;
        //!pointer to the solver's solution vector
        double *sol_;
        //!pointer to the solver's Jacobian matrix
        double **Jac_;
        //!pointer to time step member
        double *dt_;

        //!temporary values for evaluation of Newton function
        double *ftemp_;
        //!temporary solution values
        double *soltemp_;

        //!wrapper around system evaluation function
        /*!
        \param[in] solin current value of independent variables
        \param[in] fout evaluated system of equations
        */
        void fun(double *solin, double *fout) { integrator_->ode_fun_(solin, fout); }
        //!wrapper around Jacobian evaluation function
        /*!
        \param[in] solin current value of independent variables
        \param[in] Jout Jacobian
        */
        void jac(double *solin, double **Jout) { integrator_->ode_jac_(solin, Jout); }
};

//!Extension of OdeNewtonBridge class for fully implicit methods
/*!
This class extends the bridge class by storing pointers to the integrator class's
k vectors and tableau/coefficients, which are needed for newton iterations
*/
template <class Integrator>
class OdeNewtonIRK : public OdeNewtonBridge<Integrator> {

    public:
        //!constructs
        /*!
        \param[in] neq size of system of ODEs
        \param[in] nk number of stages
        \param[in] integrator pointer to integrator object
        */
        OdeNewtonIRK (unsigned long neq, int nk, Integrator *integrator) :
            OdeNewtonBridge<Integrator> (neq, neq*nk, integrator) {

            //number of stages or k vectors
            nk_ = nk;
            //pointer to stage k values
            k_ = integrator->k_;
            //pointers to tableau coefficients
            a = integrator->a;
            b = integrator->b;
        }

    protected:

        //!number of stages or k vectors
        int nk_;
        //!pointer to the stage slopes of RK methods
        double **k_;
        //!pointer to tableau coefficients
        double **a;
        //!pointer to tableau coefficients
        double *b;
};

//!Extension of OdeNewtonBridge class for fully SDIRK methods
/*!
This class extends the Bridge class by storing pointers to the integrator class's
k vectors and tableau/coefficients, which are needed for newton iterations. It
also stores an ik_ integer indicating which stage of the SDIRK scheme is being
solved for and the gamma value of the scheme (the diagonal of the a matrix).
*/
template <class Integrator>
class OdeNewtonSDIRK : public OdeNewtonBridge<Integrator> {

    public:
        //!constructs
        /*!
        \param[in] neq size of system of ODEs
        \param[in] integrator pointer to integrator object
        */
        OdeNewtonSDIRK (unsigned long neq, Integrator *integrator) :
            OdeNewtonBridge<Integrator> (neq, neq, integrator) {

            //pointer to stage k values
            k_ = integrator->k_;
            //pointers to tableau coefficients
            gam = integrator->gam;
            a = integrator->a;
            b = integrator->b;
            //index of k vector being solved for
            ik_ = 0;
        }

        //!sets the index of the k vector being solved for
        void set_ik (int ik) { ik_ = ik; }

    protected:

        //!pointer to the stage slopes of RK methods
        double **k_;
        //!diagonal tableau coefficient
        double gam;
        //!pointer to tableau coefficients
        double **a;
        //!pointer to tableau coefficients
        double *b;
        //!the index of the k vector being solved for
        int ik_;
};

} // namespace ode

#endif
