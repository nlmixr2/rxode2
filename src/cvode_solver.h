// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: nil; -*-
// Opaque C interface to the CVODE solver -- no SUNDIALS headers exposed.
// This header is safe to include from any translation unit.
#ifndef CVODE_SOLVER_H_
#define CVODE_SOLVER_H_

#ifdef __cplusplus
extern "C" {
#endif

// Opaque context (defined fully only in cvode_solver.cpp).
typedef struct cvode_ctx_t cvode_ctx_t;

// RHS callback: same signature as t_dydt_liblsoda.
typedef int (*cvode_rhs_fn_t)(double t, double *y, double *ydot, void *data);

// Create a CVODE context.
// neq      -- number of ODE state variables
// yp       -- initial state (zero-copy; caller owns the buffer)
// atol     -- per-compartment absolute tolerances (length neq)
// rtol     -- relative tolerance
// t0       -- initial time
// hmin     -- minimum step size (0 = no limit)
// hmax     -- maximum step size (0 = no limit)
// mxstep   -- maximum number of internal steps (0 -> 5000)
// rhs      -- right-hand side callback (liblsoda-compatible signature)
// rhs_data -- opaque pointer forwarded to rhs as user_data
// lin_type -- linear solver: 1=dense, 2=band, 3=gmres, 4=bicgstab, 5=tfqmr
// Returns NULL on failure.
cvode_ctx_t *cvode_ctx_create(int neq,
                               double *yp,
                               double *atol,
                               double rtol,
                               double t0,
                               double hmin,
                               double hmax,
                               int    mxstep,
                               cvode_rhs_fn_t rhs,
                               void  *rhs_data,
                               int    lin_type);

// Free all CVODE resources allocated by cvode_ctx_create.
void cvode_ctx_destroy(cvode_ctx_t *ctx);

// Integrate from t0 to tout; updates yp in-place (zero-copy).
// Returns 1 on success, -1 on failure.
int cvode_ctx_integrate(cvode_ctx_t *ctx, double *yp, double t0, double tout);

// Dense output interface (mirrors the odeint dense-stepper API).
// Reinitialize at a new initial state; call after dose events.
void cvode_ctx_dense_reinit(cvode_ctx_t *ctx, double *yp, double t0);
// Return the time at the end of the last internal step.
double cvode_ctx_dense_current_time(const cvode_ctx_t *ctx);
// Take one internal CV_ONE_STEP toward tout_dir; yp receives the step result.
// Returns 1 on success, -1 on failure.
int cvode_ctx_dense_do_step(cvode_ctx_t *ctx, double *yp, double tout_dir);
// Interpolate the solution at t via CVodeGetDky and write into yp.
// t must lie within the last completed step interval.
// Returns 1 on success, -1 on failure.
int cvode_ctx_dense_calc_state(cvode_ctx_t *ctx, double t, double *yp);
// Stop the next CV_ONE_STEP calls at tstop (cleared once it is reached).
int cvode_ctx_set_stop_time(cvode_ctx_t *ctx, double tstop);
// Maximum step size (0 = no limit).
int cvode_ctx_set_max_step(cvode_ctx_t *ctx, double hmax);
// Time tn reached by the last internal step and its size h, so
// cvode_ctx_dense_calc_state is valid on [tn - h, tn].
int cvode_ctx_last_step(cvode_ctx_t *ctx, double *tn, double *h);
// Restart the integration at its current time and state (order 1, fresh
// step size); the stop time must be set again.  Returns 1 or -1.
int cvode_ctx_restart(cvode_ctx_t *ctx);

#ifdef __cplusplus
}
#endif

#endif /* CVODE_SOLVER_H_ */
