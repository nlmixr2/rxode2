#ifdef IN_PAR_SOLVE
#undef min
#undef max
#include "ode_implicit_bridge.h"
#include "ode/ode_row6a.h"
#include "ode/ode_backward_euler.h"
#include "ode/ode_gauss_6.h"
#include "ode/ode_lobatto_iiic_6.h"
#include "ode/ode_radau_iia_5.h"
#include "ode/ode_geng_5.h"
#include "ode/ode_sdirk_43.h"

// -- Shared parallel driver ----------------------------------------------------

template <class OdeSolver>
static void par_implicit_tmpl(rx_solve *rx, const char *err_msg) {
  rx_solving_options *op = rx->op;
#ifdef _OPENMP
  int cores = op->cores;
#else
  int cores = 1;
#endif
  uint32_t nsub = rx->nsub, nsim = rx->nsim;
  int nsolve = (int)(nsim * nsub);
  uint32_t seed0 = getRxSeed1(cores);
  int abort = 0;
#ifdef _OPENMP
#pragma omp parallel for num_threads(cores)
#endif
  for (int solveid = 0; solveid < nsolve; solveid++) {
    int neq[2] = { op->neq, 0 };
    int localAbort;
#ifdef _OPENMP
#pragma omp atomic read
#endif
    localAbort = abort;
    if (localAbort == 0) {
      setSeedEng1(seed0 + rx->ordId[solveid] - 1);
      ind_implicit_0<OdeSolver>(rx, op, solveid, neq, dydt, update_inis, err_msg);
      if (op->badSolve) {
#ifdef _OPENMP
#pragma omp atomic write
#endif
        abort = 1;
      }
    }
  }
}

// -- ros6 -- OdeROW6A (32) -----------------------------------------------------
extern "C" void ind_ros6_0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq, t_dydt c_dydt, t_update_inis u_inis) {
  ind_implicit_0<ode::OdeROW6A>(rx, op, solveid, neq, c_dydt, u_inis, "ros6 failed");
}
extern "C" void ind_ros6(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis) {
  rx_solving_options *op = rx->op; int neq[2] = { op->neq, 0 };
  ind_ros6_0(rx, op, solveid, neq, c_dydt, u_inis);
}
extern "C" void par_ros6(rx_solve *rx) { par_implicit_tmpl<ode::OdeROW6A>(rx, "ros6 failed"); }
extern "C" void ros6_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind) {
  int neqOde = rxEffNeq(ind, op) - op->numLin - op->numLinSens;
  if (neqOde > 0) { implicit_do_steps<ode::OdeROW6A>(ind, op, dydt, calc_jac, neq, yp, *xp, xout); if (ind->rc[0] < 0) { *istate = -1; return; } }
  *xp = xout; *istate = 1;
}

// -- backwardEuler -- OdeBackwardEuler (33) ---------------------------------------------
extern "C" void ind_backwardEuler_0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq, t_dydt c_dydt, t_update_inis u_inis) {
  ind_implicit_0<ode::OdeBackwardEuler>(rx, op, solveid, neq, c_dydt, u_inis, "backwardEuler failed");
}
extern "C" void ind_backwardEuler(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis) {
  rx_solving_options *op = rx->op; int neq[2] = { op->neq, 0 };
  ind_backwardEuler_0(rx, op, solveid, neq, c_dydt, u_inis);
}
extern "C" void par_backwardEuler(rx_solve *rx) { par_implicit_tmpl<ode::OdeBackwardEuler>(rx, "backwardEuler failed"); }
extern "C" void backwardEuler_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind) {
  int neqOde = rxEffNeq(ind, op) - op->numLin - op->numLinSens;
  if (neqOde > 0) { implicit_do_steps<ode::OdeBackwardEuler>(ind, op, dydt, calc_jac, neq, yp, *xp, xout); if (ind->rc[0] < 0) { *istate = -1; return; } }
  *xp = xout; *istate = 1;
}

// -- gauss6 -- OdeGauss6 (34) --------------------------------------------------
extern "C" void ind_gauss6_0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq, t_dydt c_dydt, t_update_inis u_inis) {
  ind_implicit_0<ode::OdeGauss6>(rx, op, solveid, neq, c_dydt, u_inis, "gauss6 failed");
}
extern "C" void ind_gauss6(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis) {
  rx_solving_options *op = rx->op; int neq[2] = { op->neq, 0 };
  ind_gauss6_0(rx, op, solveid, neq, c_dydt, u_inis);
}
extern "C" void par_gauss6(rx_solve *rx) { par_implicit_tmpl<ode::OdeGauss6>(rx, "gauss6 failed"); }
extern "C" void gauss6_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind) {
  int neqOde = rxEffNeq(ind, op) - op->numLin - op->numLinSens;
  if (neqOde > 0) { implicit_do_steps<ode::OdeGauss6>(ind, op, dydt, calc_jac, neq, yp, *xp, xout); if (ind->rc[0] < 0) { *istate = -1; return; } }
  *xp = xout; *istate = 1;
}

// -- iiic6 -- OdeLobattoIIIC6 (35) ---------------------------------------------
extern "C" void ind_iiic6_0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq, t_dydt c_dydt, t_update_inis u_inis) {
  ind_implicit_0<ode::OdeLobattoIIIC6>(rx, op, solveid, neq, c_dydt, u_inis, "iiic6 failed");
}
extern "C" void ind_iiic6(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis) {
  rx_solving_options *op = rx->op; int neq[2] = { op->neq, 0 };
  ind_iiic6_0(rx, op, solveid, neq, c_dydt, u_inis);
}
extern "C" void par_iiic6(rx_solve *rx) { par_implicit_tmpl<ode::OdeLobattoIIIC6>(rx, "iiic6 failed"); }
extern "C" void iiic6_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind) {
  int neqOde = rxEffNeq(ind, op) - op->numLin - op->numLinSens;
  if (neqOde > 0) { implicit_do_steps<ode::OdeLobattoIIIC6>(ind, op, dydt, calc_jac, neq, yp, *xp, xout); if (ind->rc[0] < 0) { *istate = -1; return; } }
  *xp = xout; *istate = 1;
}

// -- radauiia5 -- OdeRadauIIA5 (36) --------------------------------------------
extern "C" void ind_radauiia5_0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq, t_dydt c_dydt, t_update_inis u_inis) {
  ind_implicit_0<ode::OdeRadauIIA5>(rx, op, solveid, neq, c_dydt, u_inis, "radauiia5 failed");
}
extern "C" void ind_radauiia5(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis) {
  rx_solving_options *op = rx->op; int neq[2] = { op->neq, 0 };
  ind_radauiia5_0(rx, op, solveid, neq, c_dydt, u_inis);
}
extern "C" void par_radauiia5(rx_solve *rx) { par_implicit_tmpl<ode::OdeRadauIIA5>(rx, "radauiia5 failed"); }
extern "C" void radauiia5_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind) {
  int neqOde = rxEffNeq(ind, op) - op->numLin - op->numLinSens;
  if (neqOde > 0) { implicit_do_steps<ode::OdeRadauIIA5>(ind, op, dydt, calc_jac, neq, yp, *xp, xout); if (ind->rc[0] < 0) { *istate = -1; return; } }
  *xp = xout; *istate = 1;
}

// -- geng5 -- OdeGeng5 (37) ----------------------------------------------------
extern "C" void ind_geng5_0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq, t_dydt c_dydt, t_update_inis u_inis) {
  ind_implicit_0<ode::OdeGeng5>(rx, op, solveid, neq, c_dydt, u_inis, "geng5 failed");
}
extern "C" void ind_geng5(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis) {
  rx_solving_options *op = rx->op; int neq[2] = { op->neq, 0 };
  ind_geng5_0(rx, op, solveid, neq, c_dydt, u_inis);
}
extern "C" void par_geng5(rx_solve *rx) { par_implicit_tmpl<ode::OdeGeng5>(rx, "geng5 failed"); }
extern "C" void geng5_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind) {
  int neqOde = rxEffNeq(ind, op) - op->numLin - op->numLinSens;
  if (neqOde > 0) { implicit_do_steps<ode::OdeGeng5>(ind, op, dydt, calc_jac, neq, yp, *xp, xout); if (ind->rc[0] < 0) { *istate = -1; return; } }
  *xp = xout; *istate = 1;
}

// -- sdirk43 -- OdeSDIRK43 (38) ------------------------------------------------
extern "C" void ind_sdirk43_0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq, t_dydt c_dydt, t_update_inis u_inis) {
  ind_implicit_0<ode::OdeSDIRK43>(rx, op, solveid, neq, c_dydt, u_inis, "sdirk43 failed");
}
extern "C" void ind_sdirk43(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis) {
  rx_solving_options *op = rx->op; int neq[2] = { op->neq, 0 };
  ind_sdirk43_0(rx, op, solveid, neq, c_dydt, u_inis);
}
extern "C" void par_sdirk43(rx_solve *rx) { par_implicit_tmpl<ode::OdeSDIRK43>(rx, "sdirk43 failed"); }
extern "C" void sdirk43_solveWith1Pt(int *neq, double *yp, double *xp, double xout, int *istate, rx_solving_options *op, rx_solving_options_ind *ind) {
  int neqOde = rxEffNeq(ind, op) - op->numLin - op->numLinSens;
  if (neqOde > 0) { implicit_do_steps<ode::OdeSDIRK43>(ind, op, dydt, calc_jac, neq, yp, *xp, xout); if (ind->rc[0] < 0) { *istate = -1; return; } }
  *xp = xout; *istate = 1;
}

#endif // IN_PAR_SOLVE
