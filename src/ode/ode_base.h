//! \file ode_base.h
/*!
\mainpage libode

`libode` is a library of C++ classes for solving systems of ordinary differential equations in autonomous form. All of the solvers are single-step, Runge-Kutta-like methods. There are explicit, adaptive solvers up to the ninth order. The repository also includes Rosenbrock methods, a singly-diagonal implicit Runge-Kutta (SDIRK) method, and several fully implicit Runge-Kutta methods. However, only a few of the implicit methods have solid adaptive time steppers at this point. With the current collection of solvers and features, `libode` is well suited to any non-stiff system and to stiff systems that are tightly coupled and have a known Jacobian (ones that don't require sparse or banded matrix routines). It's been useful for solving the same system a huge number of times with varying parameters, when the speed advantage of parallel C++ might be worth it.

The classes were originally styled after [Chris Rycroft](https://people.seas.harvard.edu/~chr/)'s [example classes](https://github.com/chr1shr/am225_examples/tree/master/1a_ode_solvers). Their structure makes it easy to build a templated integrator on top of an arbitrary solver class and switch the solver/method. Implicit methods can be given a function for the ODE system's Jacobian or, if none is provided, the Jacobian is estimated using finite differences.

Several of the solvers and much more detail on the methods can be found in these amazing books:
+ Hairer, E., Norsett, S. P. & Wanner, G. Solving Ordinary Differential Equations I: Nonstiff Problems. (Springer-Verlag, 1987).
+ Hairer, E. & Wanner, G. Solving Ordinary Differential Equations II: Stiff and Differential-Algebraic Problems. (Springer, 1996).

The table below lists all the solvers and gives some basic information about them. All of the solvers can be used with a custom time step selection function, but those with a built-in adaptive capability are indicated below. Papers and/or links to the derivation or original publication of the methods are often copied in the headers for the solver classes and included in the documentation. Some work still needs to be done to make the implicit methods genuinely useful, and a list of things to implement is in the `todo.txt` file.

<table>
<tr><th>Method <th> Class Name <th> Header File <th> (ex/im)plicit <th> built-in adaptive? <th> stages <th> order <th> stability
<tr> <td> Forward Euler <td> `OdeEuler` <td> `ode_euler.h` <td> explicit <td> no <td> 1 <td> 1 <td>
<tr> <td> Trapezoidal Rule <td> `OdeTrapz` <td> `ode_trapz.h` <td> explicit <td> no <td> 2 <td> 2 <td>
<tr> <td> Strong Stability-Preserving, Order 3 <td> `OdeSsp3` <td> `ode_ssp_3.h` <td> explicit <td> no <td> 3 <td> 3 <td>
    <tr> <td> Runge-Kutta-Fehlberg 3(2) <td> `OdeRKF32` <td> `ode_rkf_32.h` <td> explicit <td> yes <td> 3 <td> 3 <td>
<tr> <td> RK4 <td> `OdeRK4` <td> `ode_rk_4.h` <td> explicit <td> no <td> 4 <td> 4 <td>
<tr> <td> Runge-Kutta 4(3) <td> `OdeRK43` <td> `ode_rk_43.h` <td> explicit <td> yes <td> 5 <td> 4 <td>
<tr> <td> Cash-Karp <td> `OdeRKCK` <td> `ode_rkck.h` <td> explicit <td> yes <td> 6 <td> 5 <td>
<tr> <td> Dormand-Prince 5(4) <td> `OdeDoPri54` <td> `ode_dopri_54.h` <td> explicit <td> yes <td> 7 <td> 5 <td>
<tr> <td> Jim Verner's "most efficent" 6(5) <td> `OdeVern65` <td> `ode_vern_65.h` <td> explicit <td> yes <td> 9 <td> 6 <td>
<tr> <td> Jim Verner's "most efficent" 7(6) <td> `OdeVern76` <td> `ode_vern_76.h` <td> explicit <td> yes <td> 10 <td> 7 <td>
<tr> <td> Dormand-Prince 8(7) <td> `OdeDoPri87` <td> `ode_dopri_87.h` <td> explicit <td> yes <td> 13 <td> 8 <td>
<tr> <td> Jim Verner's "most efficent" 9(8) <td> `OdeVern98` <td> `ode_vern_98.h` <td> explicit <td> yes <td> 16 <td> 9 <td>
<tr> <td> Rosenbrock 4(3) <td> `OdeGRK4A` <td> `ode_grk4a.h` <td> implicit <td> yes <td> 4 <td> 4 <td> A
<tr> <td> Rosenbrock 6 <td> `OdeROW6A` <td> `ode_row6a.h` <td> implicit <td> no <td> 6 <td> 6 <td> A
<tr> <td> Backward Euler <td> `OdeBackwardEuler` <td> `ode_backward_euler.h` <td> implicit <td> no <td> 1 <td> 1 <td> L
<tr> <td> Gauss 6th Order <td> `OdeGauss6` <td> `ode_gauss_6.h` <td> implicit <td> not yet <td> 3 <td> 6 <td> A
<tr> <td> Lobatto IIIC 6th Order <td> `OdeLobattoIIIC6` <td> `ode_lobatto_iiic_6.h` <td> implicit <td> not yet <td> 4 <td> 6 <td> L
<tr> <td> Radau IIA 5th Order <td> `OdeRadauIIA5` <td> `ode_radau_iia_5.h` <td> implicit <td> not yet <td> 3 <td> 5 <td> L
<tr> <td> Geng's Symplectic 5th Order <td> `OdeGeng5` <td> `ode_geng_5.h` <td> implicit <td> no <td> 3 <td> 5 <td> A?
<tr> <td> SDIRK 4(3) <td> `OdeSDIRK43` <td> `ode_sdirk_43.h` <td> implicit <td> yes <td> 4 <td> 4 <td> L
</table>

\section sec_compiling Compiling

\subsection sec_compiling_short Short Instructions

1. Copy the `_config.mk` file to `config.mk`.
2. Edit any of the compiler settings in your new `config.mk` file as necessary (specify which compiler to use and any compiling flags you want).
3. Run `make` in the top directory where the Makefile is.
4. If anything weird happens, tell me.
5. Execute the `run_all_tests.sh` script to check that things are working (Python with numpy and matplotlib are needed for plotting). If you want, also execute `run_all_examples.sh` to run some example solvers.
6. Create derived classes and link to the library with `-I<path>/libode/src -L<path>/libode/bin -lode`, replacing `<path>` with the path to the directory above `libode` on your computer.

\subsection sec_compiling_long Longer Instructions

`libode` is meant to provide straightforward access to class-based ODE solvers without dependencies or specialized compiling processes. The library is free-standing and there is only one step to take before simply running the Makefile and being done with it. Consequently, the library is also slim on features and doesn't provide things like sparse matrices and dense output. For many systems of ODEs, though, `libode` should make it easy to build an integrator and enjoy the speed of C++ and [openmp](https://en.wikipedia.org/wiki/OpenMP) without the headaches of large, complex packages.

First, before any of the `libode` classes can be compiled, you must copy the `_config.mk` file to `config.mk` and edit that file to specify the compiler settings you'd like the Makefile to use. This shouldn't be complicated. If you are using a current version of the GNU C++ compiler (g++), the contents of the template config file can likely be used without modification. There are also commented lines for use with the Intel C++ compiler (icpc), if that is available. To compile all the classes, simply run `make` in the top directory.

The Makefile compiles all of the necessary code into the `obj` folder, then archives it in the `bin` directory as a file called `libode.a`. To use the solvers, you can link `libode.a` (in the `bin` directory) or the object files directly (in the `obj` directory) when compiling your derived class. You must also include the header files in the `src` directory, as there is not a single header file for the library. All of the classes have their header file name displayed in the documentation and in the table above. Linking the solver classes requires something like

`-I<path>/libode/src -L<path>/libode/bin -lode`

when compiling derived code, with `<path>` replaced by path elements leading to the libode directory. For some examples of how to link a derived class to `libode` and create a program to run integrations, see the examples folder.

Test programs are compiled with `make tests` and they can all be run in sequence with the `run_all_tests.sh` script (which uses Python to plot the test results).

\subsection sec_compiling_cmake Compiling with CMake

Whe library can be built with CMake as well.
You can start CMake with the following script
```bash
   .  run_cmake.sh
```
This script creates an folder `build`, compiles the library, installs it locally and creates a package.

Here is an example of an file `CMakeLists.txt` to show how to use the **ode** library:

\code{bash}
cmake_minimum_required(VERSION 3.15)
project("ODETest" VERSION 0.9 DESCRIPTION "A project with the libode library")

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# It is a local prefix.
# Comment it out if you want to use the default prefix.
set(CMAKE_PREFIX_PATH "$ENV{HOME}/install")

find_package(ode CONFIG REQUIRED)

add_executable(${PROJECT_NAME})
target_sources(${PROJECT_NAME}
    PRIVATE
        my_ode_test.cpp # These are the names of the source files.
)
target_link_libraries(${PROJECT_NAME} PRIVATE ode::ode)
\endcode

\section sec_usage Using the Solvers

\subsection subsec_classes Define a Class

To integrate a specific system of ODEs, a new class must be created to inherit from one of the solver classes. This new inheriting class must
1. Define the system of ODEs to be solved by implementing the `ode_fun()` function. This is a virtual function in the base classes. Once implemented, it's used by the stepping and solving functions.
2. Set initial conditions using the `set_sol()` function.
3. Optionally implement the `ode_jac()` function for implicit methods. This is also a virtual function in the base classes. If it's not overridden but is needed, a (crude) finite-difference estimate of the Jacobian is used.

For flexibility, the derived class could be a template, so that the solver/method can be chosen when the class is constructed. Other than defining the system of equations and setting initial conditions, the derived class can store whatever information and implement whatever other methods are necessary. This could be something simple like an extra function for setting initial conditions. It could, however, comprise any other system that needs to run on top of an ODE solver, like the spatial discretization of a big PDE solver.

\subsection subsec_integrate Call an Integration Function

Each solver has a `step` method that can be used to integrate a single step with a specified step size. Each solver class also has a `solve_fixed()` method and, if it's an adaptive class, a `solve_adaptive()` method. These functions return nothing and both have the same four call signatures:

1. `void solve_fixed (double tint, double dt)`

   Simply advances the solution for a specified length of the independent variable. The independent variable is assumed to be time, so `tint` is the integration time and `dt` is the time step to use (or the initial time step for adaptive solves).

2. `void solve_fixed (double tint, double dt, const char *dirout, int inter)`

   Integrates for a duration of `tint` using time step (or initial time step) `dt` and writes solution values after every `inter` steps to the directory `dirout`. For example, if `inter` is one, the solution at every step is written to file. If `inter` is two, every other step is written.

3. `void solve_fixed (double tint, double dt, unsigned long nsnap, const char *dirout)`

   Integrates and writes `nsnap` even spaced snapshots of the solution into the directory `dirout`.

4. `void solve_fixed (double dt, double *tsnap, unsigned long nsnap, const char *dirout)`

   Integrates and writes snapshots at the times specified in `tsnap` into the directory `dirout`.

If these functions aren't enough, you could always write your own loop calling the `step()` function directly.

\subsection subsec_padapt Flexibly Adapt the Time Step

Some of the solvers have built-in adaptive time steppers. They automatically choose time steps by comparing the solution for a single step with that of an embedded, lower order solution for the step and computing an error estimate. The algorithm for this is well described in the books referenced above. If, however, there is another way that the time step should be chosen for a system, a new selection algorithm can be used with **any** of the solvers. If the virtual function `dt_adapt()` is overridden, it will be used to select the time step in the `solve_adaptive()` functions.

Rejecting an adaptive step is easy. During an adaptive solve, the virtual `is_rejected()` function is called after every step. If it returns `true`, the step is rejected. If it returns `false`, the step is accepted. Either way, `dt_adapt()` computes the next time step size and the solver proceeds. So, at minimum, an adaptive solver with time step rejection needs to have its `dt_adapt()` and `is_rejected()` functions implemented. The embedded Runge-Kutta methods have these functions built in, but they can be overridden.

If it's easier to compute the next time step and determine whether the step is rejected all at once, the virtual `adapt()` function can be implemented. It should store the next time step and store a boolean for rejection. Then `dt_adapt()` and `is_rejected()` simply return those stored values. This is how the embedded Runge-Kutta methods are structured because the same information determines the next step size and rejection/acceptance of the current step.

The point is to make time step selection totally flexible if the embedded Runge-Kutta algorithm isn't suitable. For example, this flexibility has been used to set the time step based on stability thresholds of PDE discretizations like the CFL condition for advection or the von Neumann condition for simple diffusion schemes. Prescribing the adaptive time step based on these conditions, then using `solve_adaptive()`, can provide huge speed boosts.

\subsection extra "Extra" Functions During Solves

There is a straightforward way to supplement the built-in solver functions and execute extra code at different points during solves. There are five "extra" functions, which are empty virtual functions that can be overridden by the derived class:

1. `before_solve ()`

   Executed at the beginning of any `solve_fixed()`/`solve_adaptive()` call.

2. `after_step (double t)`

   Executed after every step while the solve function is running. The `t` input is the current "time" of the system, or the value of the independent variable. Although the system must be in autonomous form, the classes track the independent variable as a convenience.

3. `after_capture (double t)`

   Executed each time the state of the system is captured during the second version of `solve_fixed()`/`solve_adaptive()` above (the one with the `inter` argument). This function is similar to `after_step()`, but if the output interval is greater than one, it is only called when output is stored. The `t` input is the current "time" of the system, or the value of the independent variable. Although the system must be in autonomous form, the classes track the independent variable as a convenience.

4. `after_snap (std::string dirout, long isnap, double t)`

   Executed after every snapshot in the solver functions that use snapshot output (call signatures 3 and 4 above). `dirout` is the output directory and `isnap` is the snapshot count (starting from 0). The `t` input is the current "time" of the system, or the value of the independent variable.

5. `after_solve ()`

   Executed at the end of any `solve_fixed()`/`solve_adaptive()` call.

These functions are meant to be very general. They can be used to implement a customized output/storage procedure, print updates, set and reset system variables, or anything else. If they are not overridden, they will do nothing.
*/

#ifndef ODE_BASE_H_
#define ODE_BASE_H_

#include <string>
#include <vector>
#include <cstdio>
#include <cstdlib>
#include <cmath>

#include "ode_io.h"
#include "ode_util.h"

namespace ode {

//!Lowest base class for all solvers
/*!
This is the deepest base class, upon which all integrators and other base classes are built. It provides basic variables like the solution array, the independent variable, and a string defining the integrator's name. It implements the solve_fixed function for integrating with a fixed time step. When constructing, it will allocate space for a Jacobian matrix if the need_jac flag is true.
*/
class OdeBase {

    public:
        //!constructs
        /*!
        \param[in] neq number of equations in ODE system
        \param[in] need_jac flag signaling whether the Jacobian of the system is needed
        */
        OdeBase (unsigned long neq, bool need_jac);
        //!destructs
        virtual ~OdeBase ();

        //-------------------
        //getters and setters

        //!gets the name of the ODE system
        const char *get_name () { return(name_.c_str()); }
        //!gets the name of the solver/method
        const char *get_method () { return(method_.c_str()); }
        //!gets output directory string if one has been set
        const char *get_dirout () { return(dirout_.c_str()); }
        //!gets the boolean determining if updates are printed during solves
        bool get_quiet () { return(quiet_); }
        //!gets whether to skip writing the solution vector to file when snapping
        bool get_silent_snap () { return(silent_snap_); }
        //!gets the size of the ODE system
        unsigned long get_neq () { return(neq_); }
        //!gets the current value of the independent variable
        /*!
        The value of the independent variable is provided as a convenience and is used internally to track integration progress, but is <b>not precisely accurate during steps</b>. Libode solves systems in autonomous form. If the independent variable is needed during a step, an extra variable must be added to the system of ODEs to represent it. The corresponding extra ODE is always one. For example, if the independent variable is t for time, the extra ODE states dt/dt = 1.
        */
        double get_t () { return(t_); }
        //!gets the most recent or current time step size
        double get_dt () { return(dt_); }
        //!gets a pointer to the whole solution array
        double *get_sol () { return(sol_); }
        //!gets an element of the solution array
        /*!\param[in] i index of element to get (sol[i])*/
        double get_sol (unsigned long i) { return(sol_[i]); }
        //!gets the total number of steps taken
        long unsigned get_nstep () { return(nstep_); }
        //!gets the total number of ODE system evaluation
        long unsigned get_neval () { return(neval_); }
        //!gets the number of steps after which the solution is checked for integrity
        long unsigned get_icheck () { return(icheck_); }
        //!gets the total number of Jacobian evaluations performed
        long unsigned get_nJac () { return(nJac_); }
        //!resets step, evaluation, and Jacobian counters
        virtual void reset_counters () {
            nstep_ = 0;
            neval_ = 0;
            nJac_ = 0;
        }

        //!sets the "time," or independent variable used to track progress
        /*!Because libode solves system in autonomous form, the "time" `t` is just an internal tracker for the integrating functions. See `get_t()`. Nevertheless, this function will set the internal value of the integrator's `t` variable.*/
        void set_t (double t) { t_ = t; }
        //!sets an element of the solution array
        /*!
        \param[in] i index of solution element to set
        \param[in] x value to copy into sol[i]
        */
        void set_sol (unsigned long i, double x) { sol_[i] = x; }
        //!copies an array into the solution array
        /*!\param[in] sol an array of length `neq` to copy into the solution vector*/
        void set_sol (double *sol) { for(unsigned long i=0; i<neq_; i++) sol_[i] = sol[i]; }
        //!points the solution array at an external buffer (zero-copy)
        /*!The caller owns the buffer and must ensure it lives at least as long as
        this object. The destructor will NOT free it.
        \param[in] ext external buffer of length `neq`*/
        void set_sol_external (double *ext) {
            if (ext == NULL) ode_print_exit("Cannot set solution buffer to NULL.\n");
            if (!sol_external_) delete[] sol_;
            sol_ = ext;
            sol_external_ = true;
        }
        //!sets the name of the ODE system
        void set_name (std::string name) { name_ = name; }
        //!sets the name of the ODE system
        void set_name (const char *name) { name_ = name; }
        //!sets the boolean determining if updates are printed during solves
        void set_quiet (bool quiet) { quiet_ = quiet; }
        //!sets whether to skip writing the solution vector to file when snapping
        void set_silent_snap (bool silent_snap) { silent_snap_ = silent_snap; }
        //!sets the number of steps after which the solution is checked for integrity
        void set_icheck (unsigned long icheck) { icheck_ = icheck; }

        //----------------
        //solver functions

        //!increments the step counter and the time, checks the solution integrity if needed, stores the time step in the object, and executes after_step() if extra is true
        void step (double dt, bool extra=true);

        //!integrates for a specified duration of independent variable without output
        /*!
        \param[in] tint total integration time
        \param[in] dt time step size
        \param[in] extras whether to call all the extra functions (before_solve, after_step, ...)
        */
        void solve_fixed (double tint, double dt, bool extras=true);

        //!lots of output, solves and stores every "inter" point along the way
        /*!
        \param[in] tint total integration time
        \param[in] dt time step size
        \param[in] dirout output directory (must already exist)
        \param[in] inter interval of steps to store and output
        */
        void solve_fixed (double tint, double dt, const char *dirout, int inter=1);

        //!solves and writes evenly spaced snapshots
        /*!
        \param[in] tint total integration time
        \param[in] dt time step size
        \param[in] nsnap number of snapshots to output
        \param[in] dirout output directory (must already exist)
        */
        void solve_fixed (double tint, double dt, unsigned long nsnap, const char *dirout);

        //!solves and writes snapshots at times specified in the tsnap array
        /*!
        \param[in] dt time step size
        \param[in] tsnap array of desired snapshot times
        \param[in] nsnap number of snapshots (length of tsnap)
        \param[in] dirout output directory (must already exist)
        */
        void solve_fixed (double dt, double *tsnap, unsigned long nsnap, const char *dirout);

        //!reset to a specified time and initial condition array
        /*!
        \param[in] t new value of independent variable
        \param[in] sol array of new values for solution array
        */
        void reset (double t, double *sol);

    protected:

        //!integrates without output or any counters, trackers, extra functions...
        /*!
        \param[in] tint total integration time
        \param[in] dt time step size
        \param[in] extra whether to call after_step()
        */
        void solve_fixed_ (double tint, double dt, bool extra=true);

        //----------------------
        //basic solver variables

        //!the "name" of the system, which is used for output
        std::string name_;
        //!the "name" of the solver/method, as in "Euler" or "RK4"
        std::string method_;
        //!output directory if one is being used by a solver
        std::string dirout_;
        //!whether stuff should be printed during a solve
        bool quiet_;
        //!whether to skip writing the solution vector to file when snapping but still execute after_snap()
        bool silent_snap_;
        //!number of equations in the system of ODEs
        unsigned long neq_;
        //!time, initialized to zero
        double t_;
        //!time step is stored and updated during solves
        double dt_;
        //!array for the solution, changing over time
        double *sol_;
        //!if true, sol_ points to an external buffer -- destructor must not free it
        bool sol_external_;
        //!number of time steps
        long unsigned nstep_;
        //!function evaluation counter, must be incremented in step() when defined
        long unsigned neval_;
        //!interval of steps after which to check for nans and infs (zero to ignore)
        long unsigned icheck_;
        //!storage for the ODE system's Jacobian matrix, only allocated for the methods that need it
        double **Jac_;
        //!counter for jacobian evaluations
        long unsigned nJac_;
        //!absolute adjustment fraction for numerical Jacobian, if needed
        double absjacdel_;
        //!relative adjustment fraction for numerical Jacobian, if needed
        double reljacdel_;

        //---------------------------
        //essential virtual functions

        //!evaluates the system of ODEs in autonomous form and must be defined by a derived class
        /*!
        The incoming `solin` vector contains the current values of all solution variables and has length `neq`. The output vector should be filled with the time derivatives for each variable in `solin`. All elements of `fout` should be set, even if they're zero, because the `fout` array isn't cleared before it's reused.
            \param[in] solin current solution array
            \param[out] fout evaluation of system of ordinary differential equations
        */
        virtual void ode_fun (double *solin, double *fout) = 0;

        //!sets the independent-variable value used for the next ode_fun() call
        virtual void set_ode_eval_time (double /*t*/) {}
        //!clears any ode_fun() evaluation-time override
        virtual void clear_ode_eval_time () {}

        //!evaluates the system's Jacobian matrix, also in autonomous form, and can either be defined in a derived class or left to numerical approximation
        /*!
        The incoming `solin` vector contains the current values of all solution variables and has length `neq`. The output array `Jout` is a 2D array with size `neq` x `neq`. All elements of `Jout` should be set, even if they're zero, because the `Jout` array isn't cleared before it's reused. If the Jacobian is needed and there is no overriding definition of this function, a finite differences approximation is used.
            \param[in] solin current solution array
            \param[out] Jout Jacobian of ode_fun
        */
        virtual void ode_jac (double *solin, double **Jout);

        //!sets the independent-variable value used for the next ode_jac() call
        virtual void set_jac_eval_time (double /*t*/) {}
        //!clears any ode_jac() evaluation-time override
        virtual void clear_jac_eval_time () {}

        //!advances a single time step (without changing counters or the time) and must be defined in the derived class implementing the solver/method
        /*!
        This is a virtual function overridden by the class which implements the stepping algorightm. It should never be used outside of the wrapper step() function, and this wrapper should always be used instead.
            \param[in] dt time step size
        */
        virtual void step_ (double dt) = 0;

        //---------------------------------------
        //wrappers of essential virtual functions

        //!wrapper, calls ode_fun() and increases the neval counter by one
        /*!
        This function should never be called by a top-level integrating class
            \param[in] solin current solution array
            \param[out] fout Jacobian of ode_fun
        */
        void ode_fun_ (double *solin, double *fout);
        //!wrapper, calls ode_jac() and increments nJac;
        /*!
        This function should never be called by a top-level integrating class
            \param[in] solin current solution array
            \param[out] Jout Jacobian of ode_fun
        */
        void ode_jac_ (double *solin, double **Jout);

        //------
        //extras

        //!does any extra stuff before starting a solve
        virtual void before_solve ();
        //!does any extra stuff after each step
        /*!
        \param[in] t current value of ODE system's independent variable
        */
        virtual void after_step (double t);
        //!does any extra stuff only when a step is captured
        /*!
        \param[in] t current value of ODE system's independent variable
        */
        virtual void after_capture (double t);
        //!does any extra stuff after each snap
        /*!
        \param[in] dirout output directory (must already exist)
        \param[in] isnap index of snap being taken (from 0)
        \param[in] t current value of ODE system's independent variable
        */
        virtual void after_snap (std::string dirout, long isnap, double t);

        //!does any extra stuff after completing a solve
        virtual void after_solve ();

        //--------------
        //solver support

        //!writes the current value of the solution to a binary file
        /*!
        \param[in] dirout output directory (must already exist)
        \param[in] isnap the index of the snap
        \param[in] tsnap independent variable value for snapshot
        */
        void snap (std::string dirout, long isnap, double tsnap);

        //!checks if the solution is within a single time step of the end point
        /*!
        \param[in] dt time step duration
        \param[in] tend end time
        \return true if the solution is within a step of the end, otherwise false
        */
        bool solve_done (double dt, double tend);

        //!checks solution for nans and infs, exiting the program if they're found
        void check_sol_integrity ();

        //!checks that a solve can be performed with given tend and dt values
        /*!
        \param[in] tint duration of integration
        \param[in] dt time step size
        */
        void check_pre_solve (double tint, double dt);

        //!checks that snap times are monotonically increasing and > current time
        /*!
        \param[in] dt time step size
        \param[in] tsnap array of snapshot times during integration
        \param[in] nsnap number of snapshots (length of tsnap)
        */
        void check_pre_snaps (double dt, double *tsnap, unsigned long nsnap);

    private:

        //flag for whether the Jacobian is being used
        bool need_jac_;
        //arrays for evaluating numerical jacobian
        double *f_, *g_;
};

} // namespace ode

#endif
