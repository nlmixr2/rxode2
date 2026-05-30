#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define STRICT_R_HEADERS
// Implementations of libode base classes needed by OdeTrapz, OdeSsp3, OdeRKF32, OdeRK43, OdeDoPri54, OdeVern65, OdeVern76, OdeDoPri87, OdeVern98, OdeRosenbrock, and OdeGRK4A.
// All file-I/O stubs are replaced with R-safe equivalents.

#include <cstdlib>
#include <cstring>
#include <cmath>
#include <string>
#include <sstream>
#include <iostream>

#include <R.h>
#include <Rinternals.h>

#include "ode/ode_util.h"
#include "ode/ode_io.h"
#include "ode/ode_base.h"
#include "ode/ode_adaptive.h"
#include "ode/ode_rk.h"
#include "ode/ode_erk.h"
#include "ode/ode_trapz.h"
#include "ode/ode_ssp_3.h"
#include "ode/ode_embedded.h"
#include "ode/ode_rkf_32.h"
#include "ode/ode_rk_43.h"
#include "ode/ode_dopri_54.h"
#include "ode/ode_vern_65.h"
#include "ode/ode_vern_76.h"
#include "ode/ode_dopri_87.h"
#include "ode/ode_vern_98.h"
#include "ode/ode_rosenbrock.h"
#include "ode/ode_grk4a.h"
#include "ode/ode_row6a.h"
#include "ode/ode_irk.h"
#include "ode/ode_newton.h"
#include "ode/ode_newton_bridge.h"
#include "ode/ode_backward_euler.h"
#include "ode/ode_gauss_6.h"
#include "ode/ode_lobatto_iiic_6.h"
#include "ode/ode_radau_iia_5.h"
#include "ode/ode_geng_5.h"
#include "ode/ode_sdirk_43.h"
#include "ode/ode_euler.h"
#include "ode/ode_midpoint.h"
#include "ode/ode_heun.h"
#include "ode/ode_rkssp22.h"
#include "ode/ode_rk3.h"
#include "ode/ode_rkssp53.h"
#include "ode/ode_rks4.h"
#include "ode/ode_rkr4.h"
#include "ode/ode_rkls44.h"
#include "ode/ode_rkls54.h"
#include "ode/ode_rkssp54.h"
#include "ode/ode_rks5.h"
#include "ode/ode_rk5.h"
#include "ode/ode_rkc5.h"
#include "ode/ode_rkl5.h"
#include "ode/ode_rklk5a.h"
#include "ode/ode_rklk5b.h"
#include "ode/ode_rkb6.h"
#include "ode/ode_rk7.h"
#include "ode/ode_rk8_10.h"
#include "ode/ode_rkcv8.h"
#include "ode/ode_rk8_12.h"
#include "ode/ode_rks10.h"
#include "ode/ode_rkz10.h"
#include "ode/ode_rko10.h"
#include "ode/ode_rkh10.h"

namespace ode {

static inline double ode_stage_sum(double **a, int row, int ncol) {
    double c = 0.0;
    for (int j = 0; j < ncol; ++j) c += a[row][j];
    return c;
}

// ── ode_util ─────────────────────────────────────────────────────────────────

double ode_max2 (double a, double b) { return (a > b) ? a : b; }
double ode_min2 (double a, double b) { return (a < b) ? a : b; }
bool   ode_is_close (double a, double b, double thresh) {
    return std::fabs(a - b) <= thresh;
}

// ── ode_io ────────────────────────────────────────────────────────────────────

void ode_check_write (const char * /*fn*/) {}

void ode_print_exit (const char *msg) {
    (Rf_error)("%s", msg);
}

std::string ode_int_to_string (long i) {
    std::ostringstream s;
    s << i;
    return s.str();
}

// ── OdeRK ────────────────────────────────────────────────────────────────────

OdeRK::OdeRK (unsigned long neq, int nk) : nk_(nk) {
    k_ = new double*[nk_];
    for (int i = 0; i < nk_; i++) {
        k_[i] = new double[neq]();
    }
}

OdeRK::~OdeRK () {
    for (int i = 0; i < nk_; i++) delete[] k_[i];
    delete[] k_;
}

// ── OdeERK ───────────────────────────────────────────────────────────────────

OdeERK::OdeERK (unsigned long neq) {
    soltemp_ = new double[neq]();
}

OdeERK::~OdeERK () {
    delete[] soltemp_;
}

// ── OdeBase ──────────────────────────────────────────────────────────────────

OdeBase::OdeBase (unsigned long neq, bool need_jac)
    : quiet_(true), silent_snap_(false),
      neq_(neq), t_(0.0), dt_(0.0),
      nstep_(0), neval_(0), icheck_(0), nJac_(0),
      absjacdel_(1e-7), reljacdel_(1e-7),
      need_jac_(need_jac)
{
    sol_external_ = false;
    sol_ = new double[neq_]();
    Jac_ = nullptr;
    f_   = nullptr;
    g_   = nullptr;
    if (need_jac_) {
        Jac_ = new double*[neq_];
        for (unsigned long i = 0; i < neq_; i++) Jac_[i] = new double[neq_]();
        f_ = new double[neq_]();
        g_ = new double[neq_]();
    }
}

OdeBase::~OdeBase () {
    if (!sol_external_) delete[] sol_;
    if (Jac_) {
        for (unsigned long i = 0; i < neq_; i++) delete[] Jac_[i];
        delete[] Jac_;
    }
    delete[] f_;
    delete[] g_;
}

void OdeBase::step (double dt, bool extra) {
    step_(dt);
    nstep_++;
    t_ += dt;
    dt_ = dt;
    if (icheck_ > 0 && nstep_ % icheck_ == 0) check_sol_integrity();
    if (extra) after_step(t_);
}

void OdeBase::solve_fixed_ (double tint, double dt, bool extra) {
    double tend = t_ + tint;
    while (!solve_done(dt, tend)) {
        if (std::fabs(tend - t_) < std::fabs(dt)) dt = tend - t_;
        step(dt, extra);
    }
}

void OdeBase::solve_fixed (double tint, double dt, bool extras) {
    check_pre_solve(tint, dt);
    if (extras) before_solve();
    solve_fixed_(tint, dt, extras);
    if (extras) after_solve();
}

void OdeBase::solve_fixed (double tint, double dt, const char * /*dirout*/, int /*inter*/) {
    solve_fixed(tint, dt, false);
}

void OdeBase::solve_fixed (double tint, double dt, unsigned long /*nsnap*/, const char * /*dirout*/) {
    solve_fixed(tint, dt, false);
}

void OdeBase::solve_fixed (double dt, double * /*tsnap*/, unsigned long /*nsnap*/, const char * /*dirout*/) {
    (void)dt;
}

void OdeBase::reset (double t, double *sol) {
    t_ = t;
    for (unsigned long i = 0; i < neq_; i++) sol_[i] = sol[i];
}

bool OdeBase::solve_done (double dt, double tend) {
    return (dt > 0.0) ? (t_ >= tend - std::fabs(dt) * 1e-12)
                      : (t_ <= tend + std::fabs(dt) * 1e-12);
}

void OdeBase::check_sol_integrity () {
    for (unsigned long i = 0; i < neq_; i++) {
        if (!std::isfinite(sol_[i])) {
            (Rf_error)("libode: non-finite value in solution at step %lu", nstep_);
        }
    }
}

void OdeBase::check_pre_solve (double tint, double dt) {
    if (tint <= 0.0) (Rf_error)("libode: tint must be positive");
    if (dt   <= 0.0) (Rf_error)("libode: dt must be positive");
    // dt > tint is allowed: solve_fixed_() clamps the step on the first
    // iteration, so a single oversized step integrates exactly to tint.
}

void OdeBase::check_pre_snaps (double /*dt*/, double * /*tsnap*/, unsigned long /*nsnap*/) {}

void OdeBase::snap (std::string /*dirout*/, long /*isnap*/, double /*tsnap*/) {}

void OdeBase::ode_fun_ (double *solin, double *fout) {
    ode_fun(solin, fout);
    neval_++;
}

void OdeBase::ode_jac_ (double *solin, double **Jout) {
    ode_jac(solin, Jout);
    nJac_++;
}

void OdeBase::ode_jac (double *solin, double **Jout) {
    // finite-difference Jacobian (fallback; not used by trapz)
    if (!f_ || !g_) return;
    ode_fun(solin, f_);
    for (unsigned long j = 0; j < neq_; j++) {
        double h = absjacdel_ + reljacdel_ * std::fabs(solin[j]);
        double tmp = solin[j];
        solin[j] += h;
        ode_fun(solin, g_);
        solin[j] = tmp;
        for (unsigned long i = 0; i < neq_; i++)
            Jout[i][j] = (g_[i] - f_[i]) / h;
    }
}

void OdeBase::before_solve () {}
void OdeBase::after_step   (double /*t*/) {}
void OdeBase::after_capture(double /*t*/) {}
void OdeBase::after_snap   (std::string /*dirout*/, long /*isnap*/, double /*t*/) {}
void OdeBase::after_solve  () {}

// ── OdeAdaptive ───────────────────────────────────────────────────────────────

OdeAdaptive::OdeAdaptive (unsigned long neq, bool need_jac)
    : OdeBase(neq, need_jac),
      nrej_(0), abstol_(1e-6), reltol_(1e-6), dtmax_(1e10)
{
    solprev_ = new double[neq]();
}

OdeAdaptive::~OdeAdaptive () {
    delete[] solprev_;
}

// solve_adaptive is not used by the rxode2 trapz path (fixed-step only),
// but must be defined to satisfy the vtable.
void OdeAdaptive::solve_adaptive (double tint, double dt0, bool extras) {
    check_pre_solve(tint, dt0);
    if (extras) before_solve();
    solve_adaptive_(tint, dt0, extras);
    if (extras) after_solve();
}

void OdeAdaptive::solve_adaptive (double tint, double dt0, const char * /*dirout*/, int /*inter*/) {
    solve_adaptive(tint, dt0, false);
}

void OdeAdaptive::solve_adaptive (double tint, double dt0, unsigned long /*nsnap*/, const char * /*dirout*/) {
    solve_adaptive(tint, dt0, false);
}

void OdeAdaptive::solve_adaptive (double dt0, double * /*tsnap*/, unsigned long /*nsnap*/, const char * /*dirout*/) {
    (void)dt0;
}

void OdeAdaptive::solve_adaptive_ (double tint, double dt0, bool extra) {
    double tend = t_ + tint;
    double dt = dt0;
    // Use solve_done(dt, tend) rather than solve_done_adaptive() which uses
    // dt_ (the stored step size, initialized to 0 before the first step).
    while (!solve_done(dt, tend)) {
        if (std::fabs(tend - t_) < std::fabs(dt)) dt = tend - t_;
        step_adaptive_(dt, extra);
        dt = dt_adapt_(tend);
    }
}

bool OdeAdaptive::step_adaptive_ (double dt, bool extra) {
    for (unsigned long i = 0; i < neq_; i++) solprev_[i] = sol_[i];
    step(dt, extra);
    adapt(abstol_, reltol_);
    if (is_rejected()) {
        for (unsigned long i = 0; i < neq_; i++) sol_[i] = solprev_[i];
        t_ -= dt;
        nrej_++;
        return false;
    }
    return true;
}

double OdeAdaptive::dt_adapt_ (double tend) {
    double dt = dt_adapt();
    if (dt > dtmax_) dt = dtmax_;
    if (t_ + dt > tend) dt = tend - t_;
    return dt;
}

bool   OdeAdaptive::solve_done_adaptive (double tend) { return solve_done(dt_, tend); }
void   OdeAdaptive::adapt     (double /*abstol*/, double /*reltol*/) {}
bool   OdeAdaptive::is_rejected () { return false; }
double OdeAdaptive::dt_adapt  () { return dt_; }

// ── OdeTrapz ──────────────────────────────────────────────────────────────────

OdeTrapz::OdeTrapz (unsigned long neq)
    : OdeAdaptive(neq, false),
      OdeRK(neq, 2),
      OdeERK(neq)
{
    method_ = "Trapz";
    c2  = 1.0;
    a21 = 1.0;
    b1  = 0.5;
    b2  = 0.5;
}

void OdeTrapz::step_ (double dt) {
    ode_fun_(sol_, k_[0]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * a21 * k_[0][i];
    ode_fun_(soltemp_, k_[1]);
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * (b1 * k_[0][i] + b2 * k_[1][i]);
}

// ── OdeSsp3 ───────────────────────────────────────────────────────────────────
// Shu-Osher SSP-RK3: C. W. Shu and S. Osher (1988).
// Tableau: c2=1, a21=1; c3=1/2, a31=1/4, a32=1/4; b1=1/6, b2=1/6, b3=2/3.

OdeSsp3::OdeSsp3 (unsigned long neq)
    : OdeAdaptive(neq, false),
      OdeRK(neq, 3),
      OdeERK(neq)
{
    method_ = "Ssp3";
    c2  = 1.0;   a21 = 1.0;
    c3  = 0.5;   a31 = 0.25; a32 = 0.25;
    b1  = 1.0/6; b2  = 1.0/6; b3  = 2.0/3;
}

void OdeSsp3::step_ (double dt) {
    // k1 = f(t, y)
    ode_fun_(sol_, k_[0]);
    // y2 = y + dt*a21*k1
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * a21 * k_[0][i];
    // k2 = f(t + c2*dt, y2)
    ode_fun_(soltemp_, k_[1]);
    // y3 = y + dt*(a31*k1 + a32*k2)
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (a31 * k_[0][i] + a32 * k_[1][i]);
    // k3 = f(t + c3*dt, y3)
    ode_fun_(soltemp_, k_[2]);
    // y = y + dt*(b1*k1 + b2*k2 + b3*k3)
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * (b1 * k_[0][i] + b2 * k_[1][i] + b3 * k_[2][i]);
}

// ── OdeEmbedded ───────────────────────────────────────────────────────────────
// Base class for embedded RK pairs (error estimation + adaptive step control).

OdeEmbedded::OdeEmbedded (unsigned long neq, bool need_jac, int lowerord)
    : OdeAdaptive(neq, need_jac),
      lowerord_(lowerord),
      isrej_(false),
      dtopt_(0.0)
{
    solemb_  = new double[neq]();
    facsafe_ = 0.9;
    facmin_  = 0.1;
    facmax_  = 5.0;
}

OdeEmbedded::~OdeEmbedded () {
    delete[] solemb_;
}

double OdeEmbedded::error (double abstol, double reltol) {
    double err = 0.0;
    for (unsigned long i = 0; i < neq_; i++) {
        double sc = abstol + reltol * ode_max2(std::fabs(sol_[i]), std::fabs(solemb_[i]));
        if (sc == 0.0) sc = abstol;
        double d = (sol_[i] - solemb_[i]) / sc;
        err += d * d;
    }
    return std::sqrt(err / (double)neq_);
}

double OdeEmbedded::facopt (double err) {
    if (err == 0.0) return facmax_;
    return facsafe_ * std::pow(err, -1.0 / (double)(lowerord_ + 1));
}

void OdeEmbedded::adapt (double abstol, double reltol) {
    double err = error(abstol, reltol);
    double fac = facopt(err);
    fac    = ode_min2(facmax_, ode_max2(facmin_, fac));
    dtopt_ = dt_ * fac;
    isrej_ = (err > 1.0);
}

bool   OdeEmbedded::is_rejected () { return isrej_; }
double OdeEmbedded::dt_adapt    () { return dtopt_; }

// ── OdeRKF32 ──────────────────────────────────────────────────────────────────
// Heun-Euler 3(2) pair — coefficients match libode exactly.
// solemb_ (2nd-order): b1=1/2, b2=1/2 (Heun).
// sol_    (3rd-order): d1=1/6, d2=1/6, d3=4/6.

OdeRKF32::OdeRKF32 (unsigned long neq)
    : OdeEmbedded(neq, false, 2),
      OdeRK(neq, 3),
      OdeERK(neq)
{
    method_ = "RKF32";
    c2  = 1.0;     a21 = 1.0;
    c3  = 1.0/2;   a31 = 1.0/4;  a32 = 1.0/4;
    b1  = 1.0/2;   b2  = 1.0/2;
    d1  = 1.0/6;   d2  = 1.0/6;  d3  = 4.0/6;
}

void OdeRKF32::step_ (double dt) {
    unsigned long i;
    ode_fun_(sol_, k_[0]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*a21*k_[0][i];
    ode_fun_(soltemp_, k_[1]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a31*k_[0][i] + a32*k_[1][i]);
    ode_fun_(soltemp_, k_[2]);

    for (i=0; i<neq_; i++) {
        solemb_[i] = sol_[i] + dt*(b1*k_[0][i] + b2*k_[1][i]);
        sol_[i]    = sol_[i] + dt*(d1*k_[0][i] + d2*k_[1][i] + d3*k_[2][i]);
    }
}

// ── OdeRK43 ───────────────────────────────────────────────────────────────────
// 4(3) pair — coefficients match libode exactly.
// solemb_ (3rd-order): d1=1/12, d2=1/2, d3=1/4, d5=1/6.
// sol_    (4th-order): b1=1/8,  b2=3/8, b3=3/8, b4=1/8.

OdeRK43::OdeRK43 (unsigned long neq)
    : OdeEmbedded(neq, false, 3),
      OdeRK(neq, 5),
      OdeERK(neq)
{
    method_ = "RK43";
    c2 = 1.0/3;  a21 =  1.0/3;
    c3 = 2.0/3;  a31 = -1.0/3;  a32 = 1.0;
    c4 = 1.0;    a41 =  1.0;    a42 = -1.0;   a43 = 1.0;
    c5 = 1.0;    a51 =  1.0/8;  a52 =  3.0/8; a53 = 3.0/8; a54 = 1.0/8;
    b1 = 1.0/8;  b2  =  3.0/8;  b3  =  3.0/8; b4  = 1.0/8;
    d1 = 1.0/12; d2  =  1.0/2;  d3  =  1.0/4; d5  = 1.0/6;
}

void OdeRK43::step_ (double dt) {
    unsigned long i;
    ode_fun_(sol_, k_[0]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*a21*k_[0][i];
    ode_fun_(soltemp_, k_[1]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a31*k_[0][i] + a32*k_[1][i]);
    ode_fun_(soltemp_, k_[2]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a41*k_[0][i] + a42*k_[1][i] + a43*k_[2][i]);
    ode_fun_(soltemp_, k_[3]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a51*k_[0][i] + a52*k_[1][i] + a53*k_[2][i] + a54*k_[3][i]);
    ode_fun_(soltemp_, k_[4]);

    for (i=0; i<neq_; i++) {
        solemb_[i] = sol_[i] + dt*(d1*k_[0][i] + d2*k_[1][i] + d3*k_[2][i] + d5*k_[4][i]);
        sol_[i]    = sol_[i] + dt*(b1*k_[0][i] + b2*k_[1][i] + b3*k_[2][i] + b4*k_[3][i]);
    }
}

// ── OdeDoPri54 ────────────────────────────────────────────────────────────────
// Dormand-Prince 5(4) FSAL pair (Dormand & Prince 1980).
// 7 stages; 5th-order primary (b7=0, FSAL); 4th-order embedded (d1..d7).
// FSAL: a7j = bj, so k7 = f(t+dt, y_new) = next step's k1.
// Coefficients from the original paper and Wikipedia.

OdeDoPri54::OdeDoPri54 (unsigned long neq)
    : OdeEmbedded(neq, false, 4),
      OdeRK(neq, 7),
      OdeERK(neq)
{
    method_ = "DoPri54";
    c2 = 1.0/5;          a21 = 1.0/5;
    c3 = 3.0/10;         a31 = 3.0/40;        a32 = 9.0/40;
    c4 = 4.0/5;          a41 = 44.0/45;       a42 = -56.0/15;      a43 = 32.0/9;
    c5 = 8.0/9;          a51 = 19372.0/6561;  a52 = -25360.0/2187; a53 = 64448.0/6561; a54 = -212.0/729;
    c6 = 1.0;            a61 = 9017.0/3168;   a62 = -355.0/33;     a63 = 46732.0/5247; a64 = 49.0/176;   a65 = -5103.0/18656;
    c7 = 1.0;            a71 = 35.0/384;      a72 = 0.0;           a73 = 500.0/1113;   a74 = 125.0/192;  a75 = -2187.0/6784;  a76 = 11.0/84;
    b1 = 35.0/384;       b2 = 0.0;            b3 = 500.0/1113;     b4 = 125.0/192;     b5 = -2187.0/6784; b6 = 11.0/84;
    d1 = 5179.0/57600;   d2 = 0.0;            d3 = 7571.0/16695;   d4 = 393.0/640;     d5 = -92097.0/339200; d6 = 187.0/2100; d7 = 1.0/40;
}

void OdeDoPri54::step_ (double dt) {
    // Stage 1: k1 = f(t, y)
    ode_fun_(sol_, k_[0]);

    // Stage 2
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * a21 * k_[0][i];
    ode_fun_(soltemp_, k_[1]);

    // Stage 3
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (a31 * k_[0][i] + a32 * k_[1][i]);
    ode_fun_(soltemp_, k_[2]);

    // Stage 4
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (a41 * k_[0][i] + a42 * k_[1][i] + a43 * k_[2][i]);
    ode_fun_(soltemp_, k_[3]);

    // Stage 5
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (a51 * k_[0][i] + a52 * k_[1][i] + a53 * k_[2][i] + a54 * k_[3][i]);
    ode_fun_(soltemp_, k_[4]);

    // Stage 6
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (a61 * k_[0][i] + a62 * k_[1][i] + a63 * k_[2][i]
                                    + a64 * k_[3][i] + a65 * k_[4][i]);
    ode_fun_(soltemp_, k_[5]);

    // 5th-order primary update (b7=0; sol_ becomes y_new before stage 7)
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * (b1 * k_[0][i] + b2 * k_[1][i] + b3 * k_[2][i]
                       + b4 * k_[3][i] + b5 * k_[4][i] + b6 * k_[5][i]);

    // Stage 7 (FSAL): k7 = f(t+dt, y_new); a7j = bj
    ode_fun_(sol_, k_[6]);

    // 4th-order embedded (expressed relative to y_new = sol_):
    // solemb_ = y_new + dt*((d-b)·k[0..5] + d7*k7)
    for (unsigned long i = 0; i < neq_; i++)
        solemb_[i] = sol_[i] + dt * ((d1 - b1) * k_[0][i] + (d2 - b2) * k_[1][i]
                                    + (d3 - b3) * k_[2][i] + (d4 - b4) * k_[3][i]
                                    + (d5 - b5) * k_[4][i] + (d6 - b6) * k_[5][i]
                                    + d7         * k_[6][i]);
}

// ── OdeVern65 ─────────────────────────────────────────────────────────────────
// Jim Verner's "most efficient" 6(5) FSAL pair, 9 stages.
// Coefficients from: http://people.math.sfu.ca/~jverner/
//   RKV65.IIIXb.Efficient.00000144617.081204.CoeffsOnlyFLOAT
// solemb_ and sol_ are both computed from the OLD sol_ in the final loop.

OdeVern65::OdeVern65 (unsigned long neq)
    : OdeEmbedded(neq, false, 5),
      OdeRK(neq, 9),
      OdeERK(neq)
{
    method_ = "Vern65";

    c2 = 0.06;
    c3 = 0.09593333333333333333333333333333333333333;
    c4 = 0.1439;
    c5 = 0.4973;
    c6 = 0.9725;
    c7 = 0.9995;
    c8 = 1.0;
    c9 = 1.0;

    a21 =  c2;

    a31 =  0.01923996296296296296296296296296296296296;
    a32 =  0.07669337037037037037037037037037037037037;

    a41 =  0.035975;
    a43 =  0.107925;

    a51 =  1.318683415233148260919747276431735612861;
    a53 = -5.042058063628562225427761634715637693344;
    a54 =  4.220674648395413964508014358283902080483;

    a61 = -41.87259166432751461803757780644346812905;
    a63 =  159.4325621631374917700365669070346830453;
    a64 = -122.1192135650100309202516203389242140663;
    a65 =  5.531743066200053768252631238332999150076;

    a71 = -54.43015693531650433250642051294142461271;
    a73 =  207.0672513650184644273657173866509835987;
    a74 = -158.6108137845899991828742424365058599469;
    a75 =  6.991816585950242321992597280791793907096;
    a76 = -0.01859723106220323397765171799549294623692;

    a81 = -54.66374178728197680241215648050386959351;
    a83 =  207.9528062553893734515824816699834244238;
    a84 = -159.2889574744995071508959805871426654216;
    a85 =  7.018743740796944434698170760964252490817;
    a86 = -0.01833878590504572306472782005141738268361;
    a87 = -0.0005119484997882099077875432497245168395840;

    a91 =  0.03438957868357036009278820124728322386520;
    a94 =  0.2582624555633503404659558098586120858767;
    a95 =  0.4209371189673537150642551514069801967032;
    a96 =  4.405396469669310170148836816197095664891;
    a97 = -176.4831190242986576151740942499002125029;
    a98 =  172.3641334014150730294022582711902413315;

    b1 =  0.03438957868357036009278820124728322386520;
    b4 =  0.2582624555633503404659558098586120858767;
    b5 =  0.4209371189673537150642551514069801967032;
    b6 =  4.405396469669310170148836816197095664891;
    b7 = -176.4831190242986576151740942499002125029;
    b8 =  172.3641334014150730294022582711902413315;

    d1 =  0.04909967648382489730906854927971225836479;
    d4 =  0.2251112229516524153401395320539875329485;
    d5 =  0.4694682253029562039431948525047387412553;
    d6 =  0.8065792249988867707634161808995217981443;
    d8 = -0.6071194891777959797672951465256217122488;
    d9 =  0.05686113944047569241147603178766138153594;
}

void OdeVern65::step_ (double dt) {
    unsigned long i;

    ode_fun_(sol_, k_[0]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*a21*k_[0][i];
    ode_fun_(soltemp_, k_[1]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a31*k_[0][i] + a32*k_[1][i]);
    ode_fun_(soltemp_, k_[2]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a41*k_[0][i] + a43*k_[2][i]);
    ode_fun_(soltemp_, k_[3]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a51*k_[0][i] + a53*k_[2][i] + a54*k_[3][i]);
    ode_fun_(soltemp_, k_[4]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a61*k_[0][i] + a63*k_[2][i] + a64*k_[3][i] + a65*k_[4][i]);
    ode_fun_(soltemp_, k_[5]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a71*k_[0][i] + a73*k_[2][i] + a74*k_[3][i] + a75*k_[4][i] + a76*k_[5][i]);
    ode_fun_(soltemp_, k_[6]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a81*k_[0][i] + a83*k_[2][i] + a84*k_[3][i] + a85*k_[4][i] + a86*k_[5][i] + a87*k_[6][i]);
    ode_fun_(soltemp_, k_[7]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a91*k_[0][i] + a94*k_[3][i] + a95*k_[4][i] + a96*k_[5][i] + a97*k_[6][i] + a98*k_[7][i]);
    ode_fun_(soltemp_, k_[8]);

    // solemb_ computed from OLD sol_ (before update); sol_ updated from OLD sol_
    for (i=0; i<neq_; i++) {
        solemb_[i] = sol_[i] + dt*(d1*k_[0][i] + d4*k_[3][i] + d5*k_[4][i] + d6*k_[5][i] + d8*k_[7][i] + d9*k_[8][i]);
        sol_[i]    = sol_[i] + dt*(b1*k_[0][i] + b4*k_[3][i] + b5*k_[4][i] + b6*k_[5][i] + b7*k_[6][i] + b8*k_[7][i]);
    }
}

// ── OdeVern76 ─────────────────────────────────────────────────────────────────
// Jim Verner's "most efficient" 7(6) pair, 10 stages.
// Coefficients from: http://people.math.sfu.ca/~jverner/
//   RKV76.IIa.Efficient.00001675585.081206.CoeffsOnlyFLOAT
// Same final-loop pattern as Vern65: solemb_ and sol_ both from OLD sol_.

OdeVern76::OdeVern76 (unsigned long neq)
    : OdeEmbedded(neq, false, 6),
      OdeRK(neq, 10),
      OdeERK(neq)
{
    method_ = "Vern76";

    c2  =  0.005;
    c3  =  0.1088888888888888888888888888888888888889;
    c4  =  0.1633333333333333333333333333333333333333;
    c5  =  0.4555000000000000000000000000000000000000;
    c6  =  0.6095094489978381317087004421486024949638;
    c7  =  0.884;
    c8  =  0.925;
    c9  =  1.0;
    c10 =  1.0;

    a21 =  c2;

    a31 = -1.076790123456790123456790123456790123457;
    a32 =  1.185679012345679012345679012345679012346;

    a41 =  0.04083333333333333333333333333333333333333;
    a43 =  0.1225;

    a51 =  0.6389139236255726780508121615993336109954;
    a53 = -2.455672638223656809662640566430653894211;
    a54 =  2.272258714598084131611828404831320283215;

    a61 = -2.661577375018757131119259297861818119279;
    a63 =  10.80451388645613769565396655365532838482;
    a64 = -8.353914657396199411968048547819291691541;
    a65 =  0.8204875949566569791420417341743839209619;

    a71 =  6.067741434696770992718360183877276714679;
    a73 = -24.71127363591108579734203485290746001803;
    a74 =  20.42751793078889394045773111748346612697;
    a75 = -1.906157978816647150624096784352757010879;
    a76 =  1.006172249242068014790040335899474187268;

    a81 =  12.05467007625320299509109452892778311648;
    a83 = -49.75478495046898932807257615331444758322;
    a84 =  41.14288863860467663259698416710157354209;
    a85 = -4.461760149974004185641911603484815375051;
    a86 =  2.042334822239174959821717077708608543738;
    a87 = -0.09834843665406107379530801693870224403537;

    a91 =  10.13814652288180787641845141981689030769;
    a93 = -42.64113603171750214622846006736635730625;
    a94 =  35.76384003992257007135021178023160054034;
    a95 = -4.348022840392907653340370296908245943710;
    a96 =  2.009862268377035895441943593011827554771;
    a97 =  0.3487490460338272405953822853053145879140;
    a98 = -0.2714390051048312842371587140910297407572;

    a101 = -45.03007203429867712435322405073769635151;
    a103 =  187.3272437654588840752418206154201997384;
    a104 = -154.0288236935018690596728621034510402582;
    a105 =  18.56465306347536233859492332958439136765;
    a106 = -7.141809679295078854925420496823551192821;
    a107 =  1.308808578161378625114762706007696696508;

    b1 =  0.04715561848627222170431765108838175679569;
    b4 =  0.2575056429843415189596436101037687580986;
    b5 =  0.2621665397741262047713863095764527711129;
    b6 =  0.1521609265673855740323133199165117535523;
    b7 =  0.4939969170032484246907175893227876844296;
    b8 = -0.2943031171403250441557244744092703429139;
    b9 =  0.08131747232495109999734599440136761892478;

    d1  =  0.04460860660634117628731817597479197781432;
    d4  =  0.2671640378571372680509102260943837899738;
    d5  =  0.2201018300177293019979715776650753096323;
    d6  =  0.2188431703143156830983120833512893824578;
    d7  =  0.2289871705411202883378173889763552365362;
    d10 =  0.02029518466335628222767054793810430358554;
}

void OdeVern76::step_ (double dt) {
    unsigned long i;

    ode_fun_(sol_, k_[0]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*a21*k_[0][i];
    ode_fun_(soltemp_, k_[1]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a31*k_[0][i] + a32*k_[1][i]);
    ode_fun_(soltemp_, k_[2]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a41*k_[0][i] + a43*k_[2][i]);
    ode_fun_(soltemp_, k_[3]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a51*k_[0][i] + a53*k_[2][i] + a54*k_[3][i]);
    ode_fun_(soltemp_, k_[4]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a61*k_[0][i] + a63*k_[2][i] + a64*k_[3][i] + a65*k_[4][i]);
    ode_fun_(soltemp_, k_[5]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a71*k_[0][i] + a73*k_[2][i] + a74*k_[3][i] + a75*k_[4][i] + a76*k_[5][i]);
    ode_fun_(soltemp_, k_[6]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a81*k_[0][i] + a83*k_[2][i] + a84*k_[3][i] + a85*k_[4][i] + a86*k_[5][i] + a87*k_[6][i]);
    ode_fun_(soltemp_, k_[7]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a91*k_[0][i] + a93*k_[2][i] + a94*k_[3][i] + a95*k_[4][i] + a96*k_[5][i] + a97*k_[6][i] + a98*k_[7][i]);
    ode_fun_(soltemp_, k_[8]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a101*k_[0][i] + a103*k_[2][i] + a104*k_[3][i] + a105*k_[4][i] + a106*k_[5][i] + a107*k_[6][i]);
    ode_fun_(soltemp_, k_[9]);

    for (i=0; i<neq_; i++) {
        solemb_[i] = sol_[i] + dt*(d1*k_[0][i] + d4*k_[3][i] + d5*k_[4][i] + d6*k_[5][i] + d7*k_[6][i] + d10*k_[9][i]);
        sol_[i]    = sol_[i] + dt*(b1*k_[0][i] + b4*k_[3][i] + b5*k_[4][i] + b6*k_[5][i] + b7*k_[6][i] + b8*k_[7][i] + b9*k_[8][i]);
    }
}

// ── OdeDoPri87 ────────────────────────────────────────────────────────────────
// Dormand-Prince 8(7) pair, 13 stages.
// Coefficients from: Hairer, Norsett, Wanner (1993) "Solving ODEs I" (2nd ed.)
// Same final-loop pattern as Vern65/Vern76: solemb_ and sol_ from OLD sol_.
// solemb_ = 7th order (d1, d6..d12); sol_ = 8th order (b1, b6..b13).

OdeDoPri87::OdeDoPri87 (unsigned long neq)
    : OdeEmbedded(neq, false, 7),
      OdeRK(neq, 13),
      OdeERK(neq)
{
    method_ = "DoPri87";

    c2 =  1.0/18;     a21 =  c2;
    c3 =  1.0/12;     a31 =  1.0/48;         a32 =  1.0/16;
    c4 =  1.0/8;      a41 =  1.0/32;                           a43 =  3.0/32;
    c5 =  5.0/16;     a51 =  5.0/16;                           a53 = -75.0/64;     a54 =  75.0/64;
    c6 =  3.0/8;      a61 =  3.0/80;                                               a64 =  3.0/16;   a65 =  3.0/20;
    c7 =  59.0/400;   a71 =  29443841.0/614563906;                                 a74 =  77736538.0/692538347;   a75 = -28693883.0/1125000000; a76 =  23124283.0/1800000000;
    c8 =  93.0/200;   a81 =  16016141.0/946692911;                                 a84 =  61564180.0/158732637;   a85 =  22789713.0/633445777;  a86 =  545815736.0/2771057229; a87 = -180193667.0/1043307555;
    c9 =  5490023248.0/9719169821;
    a91 = 39632708.0/573591083;               a94 = -433636366.0/683701615; a95 = -421739975.0/2616292301; a96 =  100302831.0/723423059; a97 =  790204164.0/839813087;  a98 =  800635310.0/3783071287;
    c10 = 13.0/20;
    a101 =  246121993.0/1340847787;           a104 = -37695042795.0/15268766246; a105 = -309121744.0/1061227803; a106 =  -12992083.0/490766935; a107 =  6005943493.0/2108947869; a108 =  393006217.0/1396673457; a109 =  123872331.0/1001029789;
    c11 = 1201146811.0/1299019798;
    a111 = -1028468189.0/846180014;           a114 =  8478235783.0/508512852; a115 =  1311729495.0/1432422823; a116 = -10304129995.0/1701304382; a117 = -48777925059.0/3047939560; a118 =  15336726248.0/1032824649; a119 = -45442868181.0/3398467696; a1110 =  3065993473.0/597172653;
    c12 = 1.0;
    a121 =  185892177.0/718116043;            a124 = -3185094517.0/667107341; a125 = -477755414.0/1098053517; a126 =  -703635378.0/230739211; a127 =  5731566787.0/1027545527; a128 =  5232866602.0/850066563; a129 =  -4093664535.0/808688257; a1210 =  3962137247.0/1805957418; a1211 =  65686358.0/487910083;
    c13 = 1.0;
    a131 =  403863854.0/491063109;            a134 = -5068492393.0/434740067; a135 =  -411421997.0/543043805; a136 =   652783627.0/914296604; a137 =  11173962825.0/925320556; a138 = -13158990841.0/6184727034; a139 =  3936647629.0/1978049680; a1310 =  -160528059.0/685178525; a1311 =  248638103.0/1413531060;

    b1  =  14005451.0/335480064;
    b6  = -59238493.0/1068277825;
    b7  =  181606767.0/758867731;
    b8  =  561292985.0/797845732;
    b9  = -1041891430.0/1371343529;
    b10 =  760417239.0/1151165299;
    b11 =  118820643.0/751138087;
    b12 = -528747749.0/2220607170;
    b13 =  1.0/4;

    d1  =  13451932.0/455176632;
    d6  = -808719846.0/976000145;
    d7  =  1757004468.0/5645159321;
    d8  =  656045339.0/265891186;
    d9  = -3867574721.0/1518517206;
    d10 =  465885868.0/322736535;
    d11 =  53011238.0/667516719;
    d12 =  2.0/45;
}

void OdeDoPri87::step_ (double dt) {
    unsigned long i;

    ode_fun_(sol_, k_[0]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*a21*k_[0][i];
    ode_fun_(soltemp_, k_[1]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a31*k_[0][i] + a32*k_[1][i]);
    ode_fun_(soltemp_, k_[2]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a41*k_[0][i] + a43*k_[2][i]);
    ode_fun_(soltemp_, k_[3]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a51*k_[0][i] + a53*k_[2][i] + a54*k_[3][i]);
    ode_fun_(soltemp_, k_[4]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a61*k_[0][i] + a64*k_[3][i] + a65*k_[4][i]);
    ode_fun_(soltemp_, k_[5]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a71*k_[0][i] + a74*k_[3][i] + a75*k_[4][i] + a76*k_[5][i]);
    ode_fun_(soltemp_, k_[6]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a81*k_[0][i] + a84*k_[3][i] + a85*k_[4][i] + a86*k_[5][i] + a87*k_[6][i]);
    ode_fun_(soltemp_, k_[7]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a91*k_[0][i] + a94*k_[3][i] + a95*k_[4][i] + a96*k_[5][i] + a97*k_[6][i] + a98*k_[7][i]);
    ode_fun_(soltemp_, k_[8]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a101*k_[0][i] + a104*k_[3][i] + a105*k_[4][i] + a106*k_[5][i] + a107*k_[6][i] + a108*k_[7][i] + a109*k_[8][i]);
    ode_fun_(soltemp_, k_[9]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a111*k_[0][i] + a114*k_[3][i] + a115*k_[4][i] + a116*k_[5][i] + a117*k_[6][i] + a118*k_[7][i] + a119*k_[8][i] + a1110*k_[9][i]);
    ode_fun_(soltemp_, k_[10]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a121*k_[0][i] + a124*k_[3][i] + a125*k_[4][i] + a126*k_[5][i] + a127*k_[6][i] + a128*k_[7][i] + a129*k_[8][i] + a1210*k_[9][i] + a1211*k_[10][i]);
    ode_fun_(soltemp_, k_[11]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a131*k_[0][i] + a134*k_[3][i] + a135*k_[4][i] + a136*k_[5][i] + a137*k_[6][i] + a138*k_[7][i] + a139*k_[8][i] + a1310*k_[9][i] + a1311*k_[10][i]);
    ode_fun_(soltemp_, k_[12]);

    for (i=0; i<neq_; i++) {
        solemb_[i] = sol_[i] + dt*(d1*k_[0][i]  + d6*k_[5][i]  + d7*k_[6][i]  + d8*k_[7][i]
                                 + d9*k_[8][i]  + d10*k_[9][i] + d11*k_[10][i] + d12*k_[11][i]);
        sol_[i]    = sol_[i] + dt*(b1*k_[0][i]  + b6*k_[5][i]  + b7*k_[6][i]  + b8*k_[7][i]
                                 + b9*k_[8][i]  + b10*k_[9][i] + b11*k_[10][i] + b12*k_[11][i] + b13*k_[12][i]);
    }
}

// ── OdeVern98 ─────────────────────────────────────────────────────────────────
// Jim Verner's "most efficient" 9(8) pair, 16 stages.
// Coefficients from: http://people.math.sfu.ca/~jverner/
//   RKV98.IIa.Efficient.000000349.081209.CoeffsOnlyFLOAT6040
// Same final-loop pattern as Vern65/76/DoPri87: solemb_ and sol_ from OLD sol_.

OdeVern98::OdeVern98 (unsigned long neq)
    : OdeEmbedded(neq, false, 8),
      OdeRK(neq, 16),
      OdeERK(neq)
{
    method_ = "Vern98";

    c2  = 0.03462;
    c3  = 0.09702435063878044594828361677100617517633;
    c4  = 0.1455365259581706689224254251565092627645;
    c5  = 0.561;
    c6  = 0.2290079115904850126662751771814700052182;
    c7  = 0.5449920884095149873337248228185299947818;
    c8  = 0.645;
    c9  = 0.48375;
    c10 = 0.06757;
    c11 = 0.2500;
    c12 = 0.6590650618730998549405331618649220295334;
    c13 = 0.8206;
    c14 = 0.9012;
    c15 = 1.0;
    c16 = 1.0;

    a21 = c2;

    a31 = -0.0389335438857287327017042687229284478532;
    a32 =  0.1359578945245091786499878854939346230295;

    a41 =  0.03638413148954266723060635628912731569111;
    a43 =  0.1091523944686280016918190688673819470733;

    a51 =  2.025763914393969636805657604282571047511;
    a53 = -7.638023836496292020387602153091964592952;
    a54 =  6.173259922102322383581944548809393545442;

    a61 =  0.05112275589406060872792270881648288397197;
    a64 =  0.1770823794555021537929910813839068684087;
    a65 =  0.00080277624092225014536138698108025283759;

    a71 =  0.1316006357975216279279871693164256985334;
    a74 = -0.2957276252669636417685183174672273730699;
    a75 =  0.0878137803564295237421124704053886667082;
    a76 =  0.62130529752252747743214350056394300261;

    a81 =  0.07166666666666666666666666666666666666667;
    a86 =  0.3305533578915319409260346730051472207728;
    a87 =  0.2427799754418013924072986603281861125606;

    a91 =  0.071806640625;
    a96 =  0.3294380283228177160744825466257672816401;
    a97 =  0.1165190029271822839255174533742327183599;
    a98 = -0.034013671875;

    a101 =  0.04836757646340646986611287718844085773549;
    a106 =  0.03928989925676163974333190042057047002852;
    a107 =  0.1054740945890344608263649267140088017604;
    a108 = -0.02143865284648312665982642293830533996214;
    a109 = -0.1041229174627194437759832813847147895623;

    a111 =  -0.02664561487201478635337289243849737340534;
    a116 =   0.03333333333333333333333333333333333333333;
    a117 =  -0.1631072244872467239162704487554706387141;
    a118 =   0.03396081684127761199487954930015522928244;
    a119 =   0.1572319413814626097110769806810024118077;
    a1110 =  0.2152267478031879552303534778794770376960;

    a121 =   0.03689009248708622334786359863227633989718;
    a126 =  -0.1465181576725542928653609891758501156785;
    a127 =   0.2242577768172024345345469822625833796001;
    a128 =   0.02294405717066072637090897902753790803034;
    a129 =  -0.0035850052905728761357394424889330334334;
    a1210 =  0.08669223316444385506869203619044453906053;
    a1211 =  0.4383840651968337846196219974168630120572;

    a131 =  -0.4866012215113340846662212357570395295088;
    a136 =  -6.304602650282852990657772792012007122988;
    a137 =  -0.281245618289472564778284183790118418111;
    a138 =  -2.679019236219849057687906597489223155566;
    a139 =   0.518815663924157511565311164615012522024;
    a1310 =  1.365353187603341710683633635235238678626;
    a1311 =  5.885091088503946585721274891680604830712;
    a1312 =  2.802808786272062889819965117517532194812;

    a141 =   0.4185367457753471441471025246471931649633;
    a146 =   6.724547581906459363100870806514855026676;
    a147 =  -0.425444280164611790606983409697113064616;
    a148 =   3.343279153001265577811816947557982637749;
    a149 =   0.617081663117537759528421117507709784737;
    a1410 = -0.929966123939932833937749523988800852013;
    a1411 = -6.099948804751010722472962837945508844846;
    a1412 = -3.002206187889399044804158084895173690015;
    a1413 =  0.2553202529443445472336424602988558373637;

    a151 =  -0.779374086122884664644623040843840506343;
    a156 =  -13.93734253810777678786523664804936051203;
    a157 =   1.252048853379357320949735183924200895136;
    a158 =  -14.69150040801686878191527989293072091588;
    a159 =  -0.494705058533141685655191992136962873577;
    a1510 =  2.242974909146236657906984549543692874755;
    a1511 =  13.36789380382864375813864978592679139881;
    a1512 =  14.39665048665068644512236935340272139005;
    a1513 = -0.7975813331776800379127866056663258667437;
    a1514 =  0.4409353709534277758753793068298041158235;

    a161 =   2.058051337466886442151242368989994043993;
    a166 =   22.35793772796803295519317565842520212899;
    a167 =   0.90949810997556332745009198137971890783;
    a168 =   35.89110098240264104710550686568482456493;
    a169 =  -3.442515027624453437985000403608480262211;
    a1610 = -4.865481358036368826566013387928704014496;
    a1611 = -18.90980381354342625688427480879773032857;
    a1612 = -34.26354448030451782929251177395134170515;
    a1613 =  1.264756521695642578827783499806516664686;

    b1  =  0.01461197685842315252051541915018784713459;
    b8  = -0.3915211862331339089410228267288242030810;
    b9  =  0.2310932500289506415909675644868993669908;
    b10 =  0.1274766769992852382560589467488989175618;
    b11 =  0.2246434176204157731566981937082069688984;
    b12 =  0.5684352689748512932705226972873692126743;
    b13 =  0.05825871557215827200814768021863420902155;
    b14 =  0.1364317403482215641609022744494239843327;
    b15 =  0.03057013983082797397721005067920369646664;

    d1  =  0.01996996514886773085518508418098868756464;
    d8  =  2.191499304949330054530747099310837524864;
    d9  =  0.08857071848208438030833722031786358862953;
    d10 =  0.1140560234865965622484956605091432032674;
    d11 =  0.2533163805345107065564577734569651977347;
    d12 = -2.056564386240941011158999594595981300493;
    d13 =  0.3408096799013119935160094894224543812830;
    d16 =  0.04834231373823958314376726739772871714902;
}

void OdeVern98::step_ (double dt) {
    unsigned long i;

    ode_fun_(sol_, k_[0]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*a21*k_[0][i];
    ode_fun_(soltemp_, k_[1]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a31*k_[0][i] + a32*k_[1][i]);
    ode_fun_(soltemp_, k_[2]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a41*k_[0][i] + a43*k_[2][i]);
    ode_fun_(soltemp_, k_[3]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a51*k_[0][i] + a53*k_[2][i] + a54*k_[3][i]);
    ode_fun_(soltemp_, k_[4]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a61*k_[0][i] + a64*k_[3][i] + a65*k_[4][i]);
    ode_fun_(soltemp_, k_[5]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a71*k_[0][i] + a74*k_[3][i] + a75*k_[4][i] + a76*k_[5][i]);
    ode_fun_(soltemp_, k_[6]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a81*k_[0][i] + a86*k_[5][i] + a87*k_[6][i]);
    ode_fun_(soltemp_, k_[7]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a91*k_[0][i] + a96*k_[5][i] + a97*k_[6][i] + a98*k_[7][i]);
    ode_fun_(soltemp_, k_[8]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a101*k_[0][i] + a106*k_[5][i] + a107*k_[6][i] + a108*k_[7][i] + a109*k_[8][i]);
    ode_fun_(soltemp_, k_[9]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a111*k_[0][i] + a116*k_[5][i] + a117*k_[6][i] + a118*k_[7][i] + a119*k_[8][i] + a1110*k_[9][i]);
    ode_fun_(soltemp_, k_[10]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a121*k_[0][i] + a126*k_[5][i] + a127*k_[6][i] + a128*k_[7][i] + a129*k_[8][i] + a1210*k_[9][i] + a1211*k_[10][i]);
    ode_fun_(soltemp_, k_[11]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a131*k_[0][i] + a136*k_[5][i] + a137*k_[6][i] + a138*k_[7][i] + a139*k_[8][i] + a1310*k_[9][i] + a1311*k_[10][i] + a1312*k_[11][i]);
    ode_fun_(soltemp_, k_[12]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a141*k_[0][i] + a146*k_[5][i] + a147*k_[6][i] + a148*k_[7][i] + a149*k_[8][i] + a1410*k_[9][i] + a1411*k_[10][i] + a1412*k_[11][i] + a1413*k_[12][i]);
    ode_fun_(soltemp_, k_[13]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a151*k_[0][i] + a156*k_[5][i] + a157*k_[6][i] + a158*k_[7][i] + a159*k_[8][i] + a1510*k_[9][i] + a1511*k_[10][i] + a1512*k_[11][i] + a1513*k_[12][i] + a1514*k_[13][i]);
    ode_fun_(soltemp_, k_[14]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*(a161*k_[0][i] + a166*k_[5][i] + a167*k_[6][i] + a168*k_[7][i] + a169*k_[8][i] + a1610*k_[9][i] + a1611*k_[10][i] + a1612*k_[11][i] + a1613*k_[12][i]);
    ode_fun_(soltemp_, k_[15]);

    for (i=0; i<neq_; i++) {
        solemb_[i] = sol_[i] + dt*(d1*k_[0][i]  + d8*k_[7][i]   + d9*k_[8][i]  + d10*k_[9][i]
                                 + d11*k_[10][i] + d12*k_[11][i] + d13*k_[12][i] + d16*k_[15][i]);
        sol_[i]    = sol_[i] + dt*(b1*k_[0][i]  + b8*k_[7][i]   + b9*k_[8][i]  + b10*k_[9][i]
                                 + b11*k_[10][i] + b12*k_[11][i] + b13*k_[12][i] + b14*k_[13][i] + b15*k_[14][i]);
    }
}

// ── ode_linalg ────────────────────────────────────────────────────────────────
// R-safe implementations (Rf_error instead of printf+exit).

void ode_crout_forw_sub (double **L, double *b, int *p, int n, double *out) {
    for (int i=0; i<n; i++) {
        out[i] = b[p[i]];
        for (int j=0; j<i; j++) out[i] -= L[i][j]*out[j];
    }
}

void ode_back_sub (double **U, double *b, int n, double *out) {
    for (int i=n-1; i>=0; i--) {
        out[i] = b[i];
        for (int j=i+1; j<n; j++) out[i] -= U[i][j]*out[j];
        out[i] /= U[i][i];
    }
}

void ode_crout_LU (double **A, int n, int *p) {
    int i, j, k, idx, ti;
    double m, td;
    for (i=0; i<n; i++) p[i] = i;
    for (i=0; i<n; i++) {
        m = 0.0; idx = i;
        for (j=i; j<n; j++) { td = fabs(A[j][i]); if (td > m) { m = td; idx = j; } }
        if (!(fabs(m) > 0.0))
            (Rf_error)("libode: singular matrix in Rosenbrock Jacobian factorization");
        if (idx != i) {
            for (j=0; j<n; j++) { td = A[i][j]; A[i][j] = A[idx][j]; A[idx][j] = td; }
            ti = p[i]; p[i] = p[idx]; p[idx] = ti;
        }
        for (j=i+1; j<n; j++) A[j][i] /= A[i][i];
        for (j=i+1; j<n; j++) for (k=i+1; k<n; k++) A[j][k] -= A[j][i]*A[i][k];
    }
}

void ode_solve_LU (double **LU, int *p, double *b, int n, double *out) {
    ode_crout_forw_sub(LU, b, p, n, out);
    ode_back_sub(LU, out, n, out);
}

// ── OdeRosenbrock ─────────────────────────────────────────────────────────────

OdeRosenbrock::OdeRosenbrock (unsigned long neq, int nk) {
    nk_ = nk;
    k_ = new double*[nk];
    for (int i=0; i<nk; i++) k_[i] = new double[neq];
    p_ = new int[neq];
    rhs_ = new double[neq];
    soltemp_ = new double[neq];
}

OdeRosenbrock::~OdeRosenbrock () {
    for (int i=0; i<nk_; i++) delete [] k_[i];
    delete [] k_;
    delete [] p_;
    delete [] rhs_;
    delete [] soltemp_;
}

void OdeRosenbrock::prep_jac (double **Jac, unsigned long n, double dt, int *p) {
    unsigned long i, j;
    for (i=0; i<n; i++) {
        for (j=0; j<n; j++) Jac[i][j] = -Jac[i][j]*gam*dt;
        Jac[i][i] += 1.0;
    }
    ode_crout_LU(Jac, n, p);
}

// ── OdeGRK4A ─────────────────────────────────────────────────────────────────

OdeGRK4A::OdeGRK4A (unsigned long neq) :
    OdeEmbedded (neq, true, 3),
    OdeRosenbrock (neq, 4) {

    method_ = "GRK4A";
    gam = 0.395;
    gam21 = -0.767672395484;
    gam31 = -0.851675323742; gam32 =  0.522967289188;
    gam41 =  0.288463109545; gam42 = 0.0880214273381; gam43 = -0.337389840627;
    g21 = gam21/gam;
    g31 = gam31/gam; g32 = gam32/gam;
    g41 = gam41/gam; g42 = gam42/gam; g43 = gam43/gam;
    alp21 = 0.438;
    alp31 = 0.796920457938; alp32 = 0.0730795420615;
    b1 = 0.199293275701; b2 = 0.482645235674; b3 = 0.0680614886256; b4 = 0.25;
    d1 = 0.346325833758; d2 = 0.285693175712; d3 = 0.367980990530;
}

void OdeGRK4A::step_ (double dt) {
    unsigned long i;

    ode_jac_(sol_, Jac_);
    prep_jac(Jac_, neq_, dt, p_);

    ode_fun_(sol_, k_[0]);
    for (i=0; i<neq_; i++) rhs_[i] = dt*k_[0][i];
    ode_solve_LU(Jac_, p_, rhs_, neq_, k_[0]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + alp21*k_[0][i];
    ode_fun_(soltemp_, k_[1]);
    for (i=0; i<neq_; i++) rhs_[i] = dt*k_[1][i] + g21*k_[0][i];
    ode_solve_LU(Jac_, p_, rhs_, neq_, k_[1]);
    for (i=0; i<neq_; i++) k_[1][i] -= g21*k_[0][i];

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + alp31*k_[0][i] + alp32*k_[1][i];
    ode_fun_(soltemp_, k_[2]);
    for (i=0; i<neq_; i++) rhs_[i] = dt*k_[2][i] + g31*k_[0][i] + g32*k_[1][i];
    ode_solve_LU(Jac_, p_, rhs_, neq_, k_[2]);
    for (i=0; i<neq_; i++) k_[2][i] -= g31*k_[0][i] + g32*k_[1][i];

    // Stage 4 uses the same soltemp as stage 3 (GRK4A property)
    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + alp31*k_[0][i] + alp32*k_[1][i];
    ode_fun_(soltemp_, k_[3]);
    for (i=0; i<neq_; i++) rhs_[i] = dt*k_[3][i] + g41*k_[0][i] + g42*k_[1][i] + g43*k_[2][i];
    ode_solve_LU(Jac_, p_, rhs_, neq_, k_[3]);
    for (i=0; i<neq_; i++) k_[3][i] -= g41*k_[0][i] + g42*k_[1][i] + g43*k_[2][i];

    for (i=0; i<neq_; i++) {
        solemb_[i] = sol_[i] + (d1*k_[0][i] + d2*k_[1][i] + d3*k_[2][i]);
        sol_[i]    = sol_[i] + (b1*k_[0][i] + b2*k_[1][i] + b3*k_[2][i] + b4*k_[3][i]);
    }
}

// ── OdeIRK ────────────────────────────────────────────────────────────────────

OdeIRK::OdeIRK (unsigned long neq, int nk) {
    nk_ = nk;
    kall_ = new double[neq*nk];
    k_ = new double*[nk];
    for (int i=0; i<nk; i++) k_[i] = kall_ + i*neq;
}

OdeIRK::~OdeIRK () {
    delete [] k_;
    delete [] kall_;
}

// ── OdeNewton ─────────────────────────────────────────────────────────────────

OdeNewton::OdeNewton (unsigned long n) {
    n_ = n;
    f_ = new double[n];
    J_ = new double*[n];
    for (unsigned long i=0; i<n; i++) J_[i] = new double[n];
    delx_ = new double[n];
    p_ = new int[n];
    tol_Newton_ = 1e-8;
    iter_Newton_ = 250;
    iJLU_ = 1;
    nJLU_ = 0;
    n_solve_LU_ = 0;
    icheck_ = 2;
    modified_ = false;
    ignore_JLU_ = false;
}

OdeNewton::~OdeNewton () {
    delete [] f_;
    for (unsigned long i=0; i<n_; i++) delete [] J_[i];
    delete [] J_;
    delete [] delx_;
    delete [] p_;
}

void OdeNewton::err (double *errx, double *errf) {
    double tx, tf, mx = 0, my = 0;
    for (unsigned long i=0; i<n_; i++) {
        if ( (tx = fabs(delx_[i])) > mx ) mx = tx;
        if ( (tf = fabs(f_[i])) > my ) my = tf;
    }
    *errx = mx;
    *errf = my;
}

void OdeNewton::JLU (double *x) {
    J_Newton(x, J_);
    ode_crout_LU(J_, n_, p_);
    nJLU_++;
}

void OdeNewton::solve_LU_(double **LU, int *p, double *b, int n, double *out) {
    ode_solve_LU(LU, p, b, n, out);
    n_solve_LU_++;
}

int OdeNewton::check_integrity (double *x) {
    for (unsigned long i=0; i<n_; i++) {
        if ( std::isnan(x[i]) ) return(2);
        if ( !std::isfinite(x[i]) ) return(3);
    }
    return(0);
}

int OdeNewton::solve_Newton (double *x) {
    unsigned long i, iter;
    int suc;
    double errx, errf;

    for (i=0; i<n_; i++) delx_[i] = INFINITY;
    err(&errx, &errf);

    if ( modified_ && (!ignore_JLU_) ) JLU(x);

    iter = 0;
    while ( (errx > tol_Newton_) || (errf > tol_Newton_) ) {
        if ( (!ignore_JLU_) && (!modified_) && (iter % iJLU_ == 0) ) JLU(x);
        f_Newton(x, f_);
        for (i=0; i<n_; i++) f_[i] = -f_[i];
        solve_LU_(J_, p_, f_, n_, delx_);
        for (i=0; i<n_; i++) x[i] += delx_[i];
        err(&errx, &errf);
        iter++;
        if ( iter > iter_Newton_ ) return(1);
        if ( iter % icheck_ == 0 ) {
            suc = check_integrity(x);
            if ( suc != 0 ) return(suc);
        }
    }
    return(0);
}

// ── OdeROW6A ──────────────────────────────────────────────────────────────────

OdeROW6A::OdeROW6A (unsigned long neq) :
    OdeAdaptive (neq, true),
    OdeRosenbrock (neq, 6) {

    method_ = "ROW6A";
    gam = 0.33414236706805043;

    a21 =  0.66828473413610087;
    a31 =   0.5852480389573658; a32 = -0.048594008221492802;
    a41 = -0.61719233202999775; a42 =  -0.83995264476522158; a43 =   0.62641917900148600;
    a51 =   3.5406887484552165; a52 =   0.65991497772646308; a53 =  -0.63661180895697222; a54 = -1.1945984675295562;
    a61 =  0.80783664328582613; a62 =   0.10194631616818569; a63 = -0.078396778850607012; a64 = -0.044341977375427388; a65 = 0.013074732797453325;

    c21 = -5.8308828523185086;
    c31 = -4.0175939515896193; c32 =  0.43970131925236112;
    c41 =  7.7228006257490299; c42 =   4.3368108251435758; c43 = -2.8219574578033366;
    c51 = -1.0516225114542007; c52 = -0.58853585181331353; c53 =  2.0433794587212771; c54 = 5.0098631723809151;
    c61 = -6.7357785372199458; c62 = -0.53593889506199845; c63 = 0.38622517020810987; c64 = 0.21066472713931598; c65 = -0.053546655670373728;

    m1 = 11.358660043232931; m2 = -6.9896898855829058; m3 = -4.5967580421042947; m4 = -3.7220984696531517; m5 = 0.96012685868421520; m6 = 12.953396234292936;
}

void OdeROW6A::step_ (double dt) {
    unsigned long i;
    double t0 = t_;

    set_jac_eval_time(t0);
    ode_jac_(sol_, Jac_);
    clear_jac_eval_time();
    prep_jac(Jac_, neq_, dt, p_);

    set_ode_eval_time(t0);
    ode_fun_(sol_, k_[0]);
    clear_ode_eval_time();
    for (i=0; i<neq_; i++) rhs_[i] = dt*k_[0][i];
    ode_solve_LU(Jac_, p_, rhs_, neq_, k_[0]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + a21*k_[0][i];
    set_ode_eval_time(t0 + dt*a21);
    ode_fun_(soltemp_, k_[1]);
    clear_ode_eval_time();
    for (i=0; i<neq_; i++) rhs_[i] = dt*k_[1][i] + c21*k_[0][i];
    ode_solve_LU(Jac_, p_, rhs_, neq_, k_[1]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + a31*k_[0][i] + a32*k_[1][i];
    set_ode_eval_time(t0 + dt*(a31 + a32));
    ode_fun_(soltemp_, k_[2]);
    clear_ode_eval_time();
    for (i=0; i<neq_; i++) rhs_[i] = dt*k_[2][i] + c31*k_[0][i] + c32*k_[1][i];
    ode_solve_LU(Jac_, p_, rhs_, neq_, k_[2]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + a41*k_[0][i] + a42*k_[1][i] + a43*k_[2][i];
    set_ode_eval_time(t0 + dt*(a41 + a42 + a43));
    ode_fun_(soltemp_, k_[3]);
    clear_ode_eval_time();
    for (i=0; i<neq_; i++) rhs_[i] = dt*k_[3][i] + c41*k_[0][i] + c42*k_[1][i] + c43*k_[2][i];
    ode_solve_LU(Jac_, p_, rhs_, neq_, k_[3]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + a51*k_[0][i] + a52*k_[1][i] + a53*k_[2][i] + a54*k_[3][i];
    set_ode_eval_time(t0 + dt*(a51 + a52 + a53 + a54));
    ode_fun_(soltemp_, k_[4]);
    clear_ode_eval_time();
    for (i=0; i<neq_; i++) rhs_[i] = dt*k_[4][i] + c51*k_[0][i] + c52*k_[1][i] + c53*k_[2][i] + c54*k_[3][i];
    ode_solve_LU(Jac_, p_, rhs_, neq_, k_[4]);

    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + a61*k_[0][i] + a62*k_[1][i] + a63*k_[2][i] + a64*k_[3][i] + a65*k_[4][i];
    set_ode_eval_time(t0 + dt*(a61 + a62 + a63 + a64 + a65));
    ode_fun_(soltemp_, k_[5]);
    clear_ode_eval_time();
    for (i=0; i<neq_; i++) rhs_[i] = dt*k_[5][i] + c61*k_[0][i] + c62*k_[1][i] + c63*k_[2][i] + c64*k_[3][i] + c65*k_[4][i];
    ode_solve_LU(Jac_, p_, rhs_, neq_, k_[5]);

    for (i=0; i<neq_; i++)
        sol_[i] = sol_[i] + (m1*k_[0][i] + m2*k_[1][i] + m3*k_[2][i] + m4*k_[3][i] + m5*k_[4][i] + m6*k_[5][i]);
}

// ── OdeBackwardEuler ──────────────────────────────────────────────────────────

void NewtonBackwardEuler::f_Newton (double *x, double *y) {
    (void)x;
    unsigned long i;
    double dt = *dt_;
    for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*k_[0][i];
    set_fun_time(integrator_->get_t() + dt);
    fun(soltemp_, ftemp_);
    clear_fun_time();
    for (i=0; i<neq_; i++) y[i] = k_[0][i] - ftemp_[i];
}

void NewtonBackwardEuler::J_Newton (double *x, double **J) {
    (void)x;
    unsigned long i, j;
    double dt = *dt_;
    if ( get_modified() ) {
        set_jac_time(integrator_->get_t());
        jac(sol_, Jac_);
        clear_jac_time();
    }
    if ( !get_modified() ) {
        for (i=0; i<neq_; i++) soltemp_[i] = sol_[i] + dt*k_[0][i];
        set_jac_time(integrator_->get_t() + dt);
        jac(soltemp_, Jac_);
        clear_jac_time();
    }
    for (i=0; i<neq_; i++) {
        for (j=0; j<neq_; j++) J[i][j] = -dt*Jac_[i][j];
        J[i][i] += 1.0;
    }
}

OdeBackwardEuler::OdeBackwardEuler (unsigned long neq) :
    OdeAdaptive (neq, true),
    OdeIRK (neq, 1) {
    method_ = "BackwardEuler";
    newton_ = new NewtonBackwardEuler(neq, 1, this);
    newton_->set_modified(true);
}

OdeBackwardEuler::~OdeBackwardEuler () {
    delete newton_;
}

void OdeBackwardEuler::step_ (double dt) {
    unsigned long i;
    for (i=0; i<neq_; i++) k_[0][i] = 0.0;
    newton_->solve_Newton(k_[0]);
    for (i=0; i<neq_; i++) sol_[i] += dt*k_[0][i];
}

// ── OdeGauss6 ─────────────────────────────────────────────────────────────────

void NewtonGauss6::f_Newton (double *x, double *y) {
    (void)x;
    unsigned long i;
    int j, n;
    double dt = *dt_;
    for (n=0; n<nk_; n++) {
        for (i=0; i<neq_; i++) {
            soltemp_[i] = sol_[i];
            for (j=0; j<nk_; j++) soltemp_[i] += dt*a[n][j]*k_[j][i];
        }
        set_fun_time(integrator_->get_t() + dt*ode_stage_sum(a, n, nk_));
        fun(soltemp_, ftemp_);
        clear_fun_time();
        for (i=0; i<neq_; i++) y[i+n*neq_] = k_[n][i] - ftemp_[i];
    }
}

void NewtonGauss6::J_Newton (double *x, double **J) {
    (void)x;
    unsigned long i, j;
    int m, n;
    double dt = *dt_;
    if ( get_modified() ) {
        set_jac_time(integrator_->get_t());
        jac(sol_, Jac_);
        clear_jac_time();
    }
    for (n=0; n<nk_; n++) {
        if ( !get_modified() ) {
            for (i=0; i<neq_; i++) {
                soltemp_[i] = sol_[i];
                for (m=0; m<nk_; m++) soltemp_[i] += dt*a[n][m]*k_[m][i];
            }
            set_jac_time(integrator_->get_t() + dt*ode_stage_sum(a, n, nk_));
            jac(soltemp_, Jac_);
            clear_jac_time();
        }
        for (i=0; i<neq_; i++) {
            for (j=0; j<neq_; j++)
                for (m=0; m<nk_; m++)
                    J[i+n*neq_][j+m*neq_] = -dt*a[n][m]*Jac_[i][j];
            J[i+n*neq_][i+n*neq_] += 1.0;
        }
    }
}

OdeGauss6::OdeGauss6 (unsigned long neq) :
    OdeAdaptive (neq, true),
    OdeIRK (neq, 3) {
    method_ = "Gauss6";
    int nk = 3;
    a = new double*[nk];
    for (int i=0; i<nk; i++) a[i] = new double[nk];
    b = new double[nk];
    double r = sqrt(15.0);
    a[0][0] =        5.0/36; a[0][1] = 2.0/9 - r/15; a[0][2] = 5.0/36 - r/30;
    a[1][0] = 5.0/36 + r/24; a[1][1] =        2.0/9; a[1][2] = 5.0/36 - r/24;
    a[2][0] = 5.0/36 + r/30; a[2][1] = 2.0/9 + r/15; a[2][2] =        5.0/36;
    b[0] = 5.0/18; b[1] = 4.0/9; b[2] = 5.0/18;
    newton_ = new NewtonGauss6(neq, nk, this);
    newton_->set_modified(true);
}

OdeGauss6::~OdeGauss6 () {
    for (int i=0; i<nk_; i++) delete [] a[i];
    delete [] a;
    delete [] b;
    delete newton_;
}

void OdeGauss6::step_ (double dt) {
    unsigned long i;
    for (i=0; i<neq_*nk_; i++) kall_[i] = 0.0;
    newton_->solve_Newton(kall_);
    for (i=0; i<neq_; i++) sol_[i] += dt*(b[0]*k_[0][i] + b[1]*k_[1][i] + b[2]*k_[2][i]);
}

// ── OdeLobattoIIIC6 ───────────────────────────────────────────────────────────

void NewtonLobattoIIIC6::f_Newton (double *x, double *y) {
    (void)x;
    unsigned long i;
    int j, n;
    double dt = *dt_;
    for (n=0; n<nk_; n++) {
        for (i=0; i<neq_; i++) {
            soltemp_[i] = sol_[i];
            for (j=0; j<nk_; j++) soltemp_[i] += dt*a[n][j]*k_[j][i];
        }
        set_fun_time(integrator_->get_t() + dt*ode_stage_sum(a, n, nk_));
        fun(soltemp_, ftemp_);
        clear_fun_time();
        for (i=0; i<neq_; i++) y[i+n*neq_] = k_[n][i] - ftemp_[i];
    }
}

void NewtonLobattoIIIC6::J_Newton (double *x, double **J) {
    (void)x;
    unsigned long i, j;
    int m, n;
    double dt = *dt_;
    if ( get_modified() ) {
        set_jac_time(integrator_->get_t());
        jac(sol_, Jac_);
        clear_jac_time();
    }
    for (n=0; n<nk_; n++) {
        if ( !get_modified() ) {
            for (i=0; i<neq_; i++) {
                soltemp_[i] = sol_[i];
                for (m=0; m<nk_; m++) soltemp_[i] += dt*a[n][m]*k_[m][i];
            }
            set_jac_time(integrator_->get_t() + dt*ode_stage_sum(a, n, nk_));
            jac(soltemp_, Jac_);
            clear_jac_time();
        }
        for (i=0; i<neq_; i++) {
            for (j=0; j<neq_; j++)
                for (m=0; m<nk_; m++)
                    J[i+n*neq_][j+m*neq_] = -dt*a[n][m]*Jac_[i][j];
            J[i+n*neq_][i+n*neq_] += 1.0;
        }
    }
}

OdeLobattoIIIC6::OdeLobattoIIIC6 (unsigned long neq) :
    OdeAdaptive (neq, true),
    OdeIRK (neq, 4) {
    method_ = "LobattoIIIC6";
    int nk = 4;
    a = new double*[nk];
    for (int i=0; i<nk; i++) a[i] = new double[nk];
    b = new double[nk];
    double r = sqrt(5.0);
    a[0][0] = 1.0/12; a[0][1] =         -r/12; a[0][2] =          r/12; a[0][3] =  -1.0/12;
    a[1][0] = 1.0/12; a[1][1] =         1.0/4; a[1][2] = (10 - 7*r)/60; a[1][3] =     r/60;
    a[2][0] = 1.0/12; a[2][1] = (10 + 7*r)/60; a[2][2] =         1.0/4; a[2][3] =    -r/60;
    a[3][0] = 1.0/12; a[3][1] =        5.0/12; a[3][2] =        5.0/12; a[3][3] =   1.0/12;
    b[0] = 1.0/12; b[1] = 5.0/12; b[2] = 5.0/12; b[3] = 1.0/12;
    newton_ = new NewtonLobattoIIIC6(neq, nk, this);
    newton_->set_modified(true);
}

OdeLobattoIIIC6::~OdeLobattoIIIC6 () {
    for (int i=0; i<nk_; i++) delete [] a[i];
    delete [] a;
    delete [] b;
    delete newton_;
}

void OdeLobattoIIIC6::step_ (double dt) {
    unsigned long i;
    for (i=0; i<neq_*nk_; i++) kall_[i] = 0.0;
    newton_->solve_Newton(kall_);
    for (i=0; i<neq_; i++) sol_[i] += dt*(b[0]*k_[0][i] + b[1]*k_[1][i] + b[2]*k_[2][i] + b[3]*k_[3][i]);
}

// ── OdeRadauIIA5 ──────────────────────────────────────────────────────────────

void NewtonRadauIIA5::f_Newton (double *x, double *y) {
    (void)x;
    unsigned long i;
    int j, n;
    double dt = *dt_;
    for (n=0; n<nk_; n++) {
        for (i=0; i<neq_; i++) {
            soltemp_[i] = sol_[i];
            for (j=0; j<nk_; j++) soltemp_[i] += dt*a[n][j]*k_[j][i];
        }
        set_fun_time(integrator_->get_t() + dt*ode_stage_sum(a, n, nk_));
        fun(soltemp_, ftemp_);
        clear_fun_time();
        for (i=0; i<neq_; i++) y[i+n*neq_] = k_[n][i] - ftemp_[i];
    }
}

void NewtonRadauIIA5::J_Newton (double *x, double **J) {
    (void)x;
    unsigned long i, j;
    int m, n;
    double dt = *dt_;
    if ( get_modified() ) {
        set_jac_time(integrator_->get_t());
        jac(sol_, Jac_);
        clear_jac_time();
    }
    for (n=0; n<nk_; n++) {
        if ( !get_modified() ) {
            for (i=0; i<neq_; i++) {
                soltemp_[i] = sol_[i];
                for (m=0; m<nk_; m++) soltemp_[i] += dt*a[n][m]*k_[m][i];
            }
            set_jac_time(integrator_->get_t() + dt*ode_stage_sum(a, n, nk_));
            jac(soltemp_, Jac_);
            clear_jac_time();
        }
        for (i=0; i<neq_; i++) {
            for (j=0; j<neq_; j++)
                for (m=0; m<nk_; m++)
                    J[i+n*neq_][j+m*neq_] = -dt*a[n][m]*Jac_[i][j];
            J[i+n*neq_][i+n*neq_] += 1.0;
        }
    }
}

OdeRadauIIA5::OdeRadauIIA5 (unsigned long neq) :
    OdeAdaptive (neq, true),
    OdeIRK (neq, 3) {
    method_ = "RadauIIA5";
    int nk = 3;
    a = new double*[nk];
    for (int i=0; i<nk; i++) a[i] = new double[nk];
    b = new double[nk];
    double r = sqrt(6.0);
    a[0][0] =     (88 - 7*r)/360; a[0][1] = (296 - 169*r)/1800; a[0][2] = (-2 + 3*r)/225;
    a[1][0] = (296 + 169*r)/1800; a[1][1] =     (88 + 7*r)/360; a[1][2] = (-2 - 3*r)/225;
    a[2][0] =        (16 - r)/36; a[2][1] =        (16 + r)/36; a[2][2] =          1.0/9;
    b[0] = (16 - r)/36; b[1] = (16 + r)/36; b[2] = 1.0/9;
    newton_ = new NewtonRadauIIA5(neq, nk, this);
    newton_->set_modified(true);
}

OdeRadauIIA5::~OdeRadauIIA5 () {
    for (int i=0; i<nk_; i++) delete [] a[i];
    delete [] a;
    delete [] b;
    delete newton_;
}

void OdeRadauIIA5::step_ (double dt) {
    unsigned long i;
    for (i=0; i<neq_*nk_; i++) kall_[i] = 0.0;
    newton_->solve_Newton(kall_);
    for (i=0; i<neq_; i++) sol_[i] += dt*(b[0]*k_[0][i] + b[1]*k_[1][i] + b[2]*k_[2][i]);
}

// ── OdeGeng5 ──────────────────────────────────────────────────────────────────

void NewtonGeng5::f_Newton (double *x, double *y) {
    (void)x;
    unsigned long i;
    int j, n;
    double dt = *dt_;
    for (n=0; n<nk_; n++) {
        for (i=0; i<neq_; i++) {
            soltemp_[i] = sol_[i];
            for (j=0; j<nk_; j++) soltemp_[i] += dt*a[n][j]*k_[j][i];
        }
        set_fun_time(integrator_->get_t() + dt*ode_stage_sum(a, n, nk_));
        fun(soltemp_, ftemp_);
        clear_fun_time();
        for (i=0; i<neq_; i++) y[i+n*neq_] = k_[n][i] - ftemp_[i];
    }
}

void NewtonGeng5::J_Newton (double *x, double **J) {
    (void)x;
    unsigned long i, j;
    int m, n;
    double dt = *dt_;
    if ( get_modified() ) {
        set_jac_time(integrator_->get_t());
        jac(sol_, Jac_);
        clear_jac_time();
    }
    for (n=0; n<nk_; n++) {
        if ( !get_modified() ) {
            for (i=0; i<neq_; i++) {
                soltemp_[i] = sol_[i];
                for (m=0; m<nk_; m++) soltemp_[i] += dt*a[n][m]*k_[m][i];
            }
            set_jac_time(integrator_->get_t() + dt*ode_stage_sum(a, n, nk_));
            jac(soltemp_, Jac_);
            clear_jac_time();
        }
        for (i=0; i<neq_; i++) {
            for (j=0; j<neq_; j++)
                for (m=0; m<nk_; m++)
                    J[i+n*neq_][j+m*neq_] = -dt*a[n][m]*Jac_[i][j];
            J[i+n*neq_][i+n*neq_] += 1.0;
        }
    }
}

OdeGeng5::OdeGeng5 (unsigned long neq) :
    OdeAdaptive (neq, true),
    OdeIRK (neq, 3) {
    method_ = "Geng5";
    int nk = 3;
    a = new double*[nk];
    for (int i=0; i<nk; i++) a[i] = new double[nk];
    b = new double[nk];
    double r = sqrt(6.0);
    a[0][0] =        (16 - r)/72; a[0][1] = (328 - 167*r)/1800; a[0][2] = (-2 + 3*r)/450;
    a[1][0] = (328 + 167*r)/1800; a[1][1] =        (16 + r)/72; a[1][2] = (-2 - 3*r)/450;
    a[2][0] =    (85 - 10*r)/180; a[2][1] =    (85 + 10*r)/180; a[2][2] =         1.0/18;
    b[0] = (16 - r)/36; b[1] = (16 + r)/36; b[2] = 1.0/9;
    newton_ = new NewtonGeng5(neq, nk, this);
    newton_->set_modified(true);
}

OdeGeng5::~OdeGeng5 () {
    for (int i=0; i<nk_; i++) delete [] a[i];
    delete [] a;
    delete [] b;
    delete newton_;
}

void OdeGeng5::step_ (double dt) {
    unsigned long i;
    for (i=0; i<neq_*nk_; i++) kall_[i] = 0.0;
    newton_->solve_Newton(kall_);
    for (i=0; i<neq_; i++) sol_[i] += dt*(b[0]*k_[0][i] + b[1]*k_[1][i] + b[2]*k_[2][i]);
}

// ── OdeSDIRK43 ────────────────────────────────────────────────────────────────

void NewtonSDIRK43::f_Newton (double *x, double *y) {
    (void)x;
    unsigned long i;
    int m;
    double dt = *dt_;
    for (i=0; i<neq_; i++) {
        soltemp_[i] = sol_[i] + dt*gam*k_[ik_][i];
        for (m=0; m<ik_; m++) soltemp_[i] += dt*a[ik_][m]*k_[m][i];
    }
    double stage_c = gam;
    for (m=0; m<ik_; m++) stage_c += a[ik_][m];
    set_fun_time(integrator_->get_t() + dt*stage_c);
    fun(soltemp_, ftemp_);
    clear_fun_time();
    for (i=0; i<neq_; i++) y[i] = k_[ik_][i] - ftemp_[i];
}

void NewtonSDIRK43::J_Newton (double *x, double **J) {
    (void)x;
    unsigned long i, j;
    int m;
    double dt = *dt_;
    if ( !get_modified() ) {
        for (i=0; i<neq_; i++) {
            soltemp_[i] = sol_[i] + dt*gam*k_[ik_][i];
            for (m=0; m<ik_; m++) soltemp_[i] += dt*a[ik_][m]*k_[m][i];
        }
        double stage_c = gam;
        for (m=0; m<ik_; m++) stage_c += a[ik_][m];
        set_jac_time(integrator_->get_t() + dt*stage_c);
        jac(soltemp_, Jac_);
        clear_jac_time();
    }
    for (i=0; i<neq_; i++) {
        for (j=0; j<neq_; j++) J[i][j] = -dt*gam*Jac_[i][j];
        J[i][i] += 1.0;
    }
}

OdeSDIRK43::OdeSDIRK43 (unsigned long neq) :
    OdeEmbedded (neq, true, 3),
    OdeRK (neq, 5) {
    method_ = "SDIRK43";
    int nk = 5;
    a = new double*[nk];
    for (int i=0; i<nk; i++) a[i] = new double[nk];
    b = new double[nk];
    d = new double[nk];
    gam = 1.0/4;
    a[1][0] =      1.0/2;
    a[2][0] =    17.0/50; a[2][1] =     -1.0/25;
    a[3][0] = 371.0/1360; a[3][1] = -137.0/2720; a[3][2] = 15.0/544;
    a[4][0] =    25.0/24; a[4][1] =    -49.0/48; a[4][2] = 125.0/16; a[4][3] = -85.0/12;
    b[0] = a[4][0]; b[1] = a[4][1]; b[2] = a[4][2]; b[3] = a[4][3]; b[4] = gam;
    d[0] =  59.0/48; d[1] = -17.0/96; d[2] = 225.0/32; d[3] = -85.0/12;
    newton_ = new NewtonSDIRK43(neq, this);
    newton_->set_modified(true);
}

OdeSDIRK43::~OdeSDIRK43 () {
    for (int i=0; i<nk_; i++) delete [] a[i];
    delete [] a;
    delete [] b;
    delete [] d;
    delete newton_;
}

void OdeSDIRK43::step_ (double dt) {
    unsigned long i;
    if ( newton_->get_modified() ) {
        set_jac_eval_time(t_);
        ode_jac_(sol_, Jac_);
        clear_jac_eval_time();
    }
    for (i=0; i<neq_; i++) k_[0][i] = 0.0;
    newton_->set_ignore_JLU(false);
    newton_->set_ik(0);
    newton_->solve_Newton(k_[0]);
    newton_->set_ignore_JLU(true);
    for (int j=1; j<nk_; j++) {
        for (i=0; i<neq_; i++) k_[j][i] = k_[j-1][i];
        newton_->set_ik(j);
        newton_->solve_Newton(k_[j]);
    }
    for (i=0; i<neq_; i++) {
        solemb_[i] = sol_[i] + dt*(d[0]*k_[0][i] + d[1]*k_[1][i] + d[2]*k_[2][i] + d[3]*k_[3][i]);
        sol_[i] += dt*(b[0]*k_[0][i] + b[1]*k_[1][i] + b[2]*k_[2][i] + b[3]*k_[3][i] + b[4]*k_[4][i]);
    }
}

// ── OdeEuler ──────────────────────────────────────────────────────────────────
OdeEuler::OdeEuler(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 1), OdeERK(neq)
{ method_ = "Euler"; }

void OdeEuler::step_(double dt) {
    ode_fun_(sol_, k_[0]);
    for (unsigned long i = 0; i < neq_; i++) sol_[i] += dt * k_[0][i];
}

// ── OdeMidpoint ───────────────────────────────────────────────────────────────
OdeMidpoint::OdeMidpoint(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 2), OdeERK(neq)
{
    method_ = "Midpoint";
    c2 = 0.5; a21 = 0.5;
}

void OdeMidpoint::step_(double dt) {
    ode_fun_(sol_, k_[0]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * a21 * k_[0][i];
    ode_fun_(soltemp_, k_[1]);
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * k_[1][i];
}

// ── OdeHeun ───────────────────────────────────────────────────────────────────
OdeHeun::OdeHeun(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 2), OdeERK(neq)
{
    method_ = "Heun";
    c2 = 1.0; a21 = 1.0; b1 = 0.5; b2 = 0.5;
}

void OdeHeun::step_(double dt) {
    ode_fun_(sol_, k_[0]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * a21 * k_[0][i];
    ode_fun_(soltemp_, k_[1]);
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * (b1 * k_[0][i] + b2 * k_[1][i]);
}

// ── OdeRkssp22 ────────────────────────────────────────────────────────────────
// xf = x + h*fs; then xf = (x + xf + h*f(t+h,xf)) / 2
OdeRkssp22::OdeRkssp22(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 1), OdeERK(neq)
{ method_ = "Rkssp22"; }

void OdeRkssp22::step_(double dt) {
    // k_[0] plays the role of fs
    ode_fun_(sol_, k_[0]);
    // soltemp_ = x + h*fs  (this is xf after first call)
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * k_[0][i];
    // re-use k_[0] for second f evaluation
    ode_fun_(soltemp_, k_[0]);
    // xf = (x + xf + h*fs2) / 2
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] = (sol_[i] + soltemp_[i] + dt * k_[0][i]) / 2.0;
}

// ── OdeRk3 ────────────────────────────────────────────────────────────────────
OdeRk3::OdeRk3(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 3), OdeERK(neq)
{
    method_ = "Rk3";
    a1 = 1.0/6.0; a2 = 2.0/3.0; a3 = 1.0/6.0;
    b2 = 0.5; c21 = 0.5; c32 = 2.0;
}

void OdeRk3::step_(double dt) {
    // f1
    ode_fun_(sol_, k_[0]);
    // f2: t + b2*h, x + h*c21*f1
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * c21 * k_[0][i];
    ode_fun_(soltemp_, k_[1]);
    // f3: t + h, x + h*(-f1 + c32*f2)
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (-k_[0][i] + c32 * k_[1][i]);
    ode_fun_(soltemp_, k_[2]);
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * (a1 * k_[0][i] + a2 * k_[1][i] + a3 * k_[2][i]);
}

// ── OdeRkssp53 ────────────────────────────────────────────────────────────────
OdeRkssp53::OdeRkssp53(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 2), OdeERK(neq)
{
    method_ = "Rkssp53";
    a30 = 0.355909775063327; a32 = 0.644090224936674;
    a40 = 0.367933791638137; a43 = 0.632066208361863;
    a52 = 0.237593836598569; a54 = 0.762406163401431;
    b10 = 0.377268915331368; b21 = 0.377268915331368;
    b32 = 0.242995220537396; b43 = 0.238458932846290;
    b54 = 0.287632146308408;
    c1  = 0.377268915331368; c2  = 0.754537830662736;
    c3  = 0.728985661612188; c4  = 0.699226135931670;
}

void OdeRkssp53::step_(double dt) {
    // k_[0] = xs, k_[1] = fs
    ode_fun_(sol_, k_[1]);
    // x1 = x + b10*h*fs -> store in k_[0] (xs)
    for (unsigned long i = 0; i < neq_; i++)
        k_[0][i] = sol_[i] + b10 * dt * k_[1][i];
    ode_fun_(k_[0], k_[1]);
    // x2 = xs + b21*h*fs -> soltemp_ (xf)
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = k_[0][i] + b21 * dt * k_[1][i];
    ode_fun_(soltemp_, k_[1]);
    // x3 = a30*x + a32*xf + b32*h*fs -> k_[0] (xs)
    for (unsigned long i = 0; i < neq_; i++)
        k_[0][i] = a30 * sol_[i] + a32 * soltemp_[i] + b32 * dt * k_[1][i];
    ode_fun_(k_[0], k_[1]);
    // x4 = a40*x + a43*xs + b43*h*fs -> k_[0] (xs)
    for (unsigned long i = 0; i < neq_; i++)
        k_[0][i] = a40 * sol_[i] + a43 * k_[0][i] + b43 * dt * k_[1][i];
    ode_fun_(k_[0], k_[1]);
    // xf = a52*xf + a54*xs + b54*h*fs
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] = a52 * soltemp_[i] + a54 * k_[0][i] + b54 * dt * k_[1][i];
}

// ── OdeRks4 ───────────────────────────────────────────────────────────────────
OdeRks4::OdeRks4(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 4), OdeERK(neq)
{
    method_ = "Rks4";
    a1  = 1.0/100.0; a2  = 3.0/5.0;
    cv  = 1.0/70092.0;
    c0  = -179124.0; c1  = 200000.0; c2  = 40425.0; c3  = 8791.0;
    aa1 = 1.0/100.0; aa2 = 1.0/245.0; aa3 = 1.0/8791.0;
    b20 = -4278.0; b21 = 4425.0;
    b30 = 524746.0; b31 = -532125.0; b32 = 16170.0;
}

void OdeRks4::step_(double dt) {
    // f0
    ode_fun_(sol_, k_[0]);
    // f1: t + a1*h, x + aa1*h*f0
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa1 * k_[0][i];
    ode_fun_(soltemp_, k_[1]);
    // f2: t + a2*h, x + aa2*h*(b20*f0 + b21*f1)
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa2 * (b20 * k_[0][i] + b21 * k_[1][i]);
    ode_fun_(soltemp_, k_[2]);
    // f3: t + h, x + aa3*h*(b30*f0 + b31*f1 + b32*f2)
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa3 * (b30 * k_[0][i] + b31 * k_[1][i] + b32 * k_[2][i]);
    ode_fun_(soltemp_, k_[3]);
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * cv * (c0 * k_[0][i] + c1 * k_[1][i] + c2 * k_[2][i] + c3 * k_[3][i]);
}

// ── OdeRkr4 ───────────────────────────────────────────────────────────────────
OdeRkr4::OdeRkr4(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 4), OdeERK(neq)
{
    method_ = "Rkr4";
    const double sqrt5 = std::sqrt(5.0);
    a2 = 4.0/10.0;
    a3 = (14.0 - 3.0*sqrt5) / 16.0;
    b21 = 4.0/10.0;
    b31 = (-2889.0 + 1428.0*sqrt5) / 1024.0;
    b32 = (3785.0 - 1620.0*sqrt5) / 1024.0;
    b41 = (-3365.0 + 2094.0*sqrt5) / 6040.0;
    b42 = (-975.0 - 3046.0*sqrt5) / 2552.0;
    b43 = (467040.0 + 203968.0*sqrt5) / 240845.0;
    c1 = (263.0 + 24.0*sqrt5) / 1812.0;
    c2 = (125.0 - 1000.0*sqrt5) / 3828.0;
    c3 = 1024.0*(3346.0 + 1623.0*sqrt5) / 5924787.0;
    c4 = (30.0 - 4.0*sqrt5) / 123.0;
}

void OdeRkr4::step_(double dt) {
    ode_fun_(sol_, k_[0]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b21 * k_[0][i]);
    ode_fun_(soltemp_, k_[1]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b31 * k_[0][i] + b32 * k_[1][i]);
    ode_fun_(soltemp_, k_[2]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b41 * k_[0][i] + b42 * k_[1][i] + b43 * k_[2][i]);
    ode_fun_(soltemp_, k_[3]);
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * (c1 * k_[0][i] + c2 * k_[1][i] + c3 * k_[2][i] + c4 * k_[3][i]);
}

// ── OdeRkls44 ─────────────────────────────────────────────────────────────────
// Low-storage 4-stage 4th-order (Jiang-Shu / Donnert et al.)
// k_[0]=xs (accumulator), k_[1]=fs (derivative buffer)
// soltemp_ holds xf (current stage state); sol_ stays as x_orig until final update
OdeRkls44::OdeRkls44(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 2), OdeERK(neq)
{ method_ = "Rkls44"; }

void OdeRkls44::step_(double dt) {
    unsigned long i;
    // xs = -4*x/3
    for (i = 0; i < neq_; i++) k_[0][i] = -4.0/3.0 * sol_[i];
    // Stage 1: f(t, x)
    ode_fun_(sol_, k_[1]);
    // xf = x + h*fs/2;  xs += xf/3
    for (i = 0; i < neq_; i++) {
        soltemp_[i] = sol_[i] + dt * k_[1][i] / 2.0;
        k_[0][i] += soltemp_[i] / 3.0;
    }
    // Stage 2: f(t + h/2, xf)
    ode_fun_(soltemp_, k_[1]);
    // xf = x + h*fs/2;  xs += 2*xf/3
    for (i = 0; i < neq_; i++) {
        soltemp_[i] = sol_[i] + dt * k_[1][i] / 2.0;
        k_[0][i] += 2.0 * soltemp_[i] / 3.0;
    }
    // Stage 3: f(t + h/2, xf)
    ode_fun_(soltemp_, k_[1]);
    // xf = x + h*fs;  xs += xf/3
    for (i = 0; i < neq_; i++) {
        soltemp_[i] = sol_[i] + dt * k_[1][i];
        k_[0][i] += soltemp_[i] / 3.0;
    }
    // Stage 4: f(t + h, xf)
    ode_fun_(soltemp_, k_[1]);
    // xf = x + h*fs/6 + xs
    for (i = 0; i < neq_; i++)
        sol_[i] = sol_[i] + dt * k_[1][i] / 6.0 + k_[0][i];
}

// ── OdeRkls54 ─────────────────────────────────────────────────────────────────
// 5-stage, 4th order low-storage Runge-Kutta (Carpenter-Kennedy 1994).
// 2N scheme: ds (k_[0]) and xf (sol_) registers, fs (k_[1]) for derivative.
OdeRkls54::OdeRkls54(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 2), OdeERK(neq)
{
    method_ = "Rkls54";
    a2 = -567301805773.0 / 1357537059087.0;
    a3 = -2404267990393.0 / 2016746695238.0;
    a4 = -3550918686646.0 / 2091501179385.0;
    a5 = -1275806237668.0 / 842570457699.0;
    b1 = 1432997174477.0 / 9575080441755.0;
    b2 = 5161836677717.0 / 13612068292357.0;
    b3 = 1720146321549.0 / 2090206949498.0;
    b4 = 3134564353537.0 / 4481467310338.0;
    b5 = 2277821191437.0 / 14882151754819.0;
    c2 = 1432997174477.0 / 9575080441755.0;
    c3 = 2526269341429.0 / 6820363962896.0;
    c4 = 2006345519317.0 / 3224310063776.0;
    c5 = 2802321613138.0 / 2924317926251.0;
}

void OdeRkls54::step_(double dt) {
    unsigned long i;
    // Stage 1: ds = h*fs; xf = x + b1*ds
    ode_fun_(sol_, k_[1]);
    for (i = 0; i < neq_; i++) {
        k_[0][i] = dt * k_[1][i];
        sol_[i] += b1 * k_[0][i];
    }
    // Stage 2
    ode_fun_(sol_, k_[1]);
    for (i = 0; i < neq_; i++) {
        k_[0][i] = a2 * k_[0][i] + dt * k_[1][i];
        sol_[i] += b2 * k_[0][i];
    }
    // Stage 3
    ode_fun_(sol_, k_[1]);
    for (i = 0; i < neq_; i++) {
        k_[0][i] = a3 * k_[0][i] + dt * k_[1][i];
        sol_[i] += b3 * k_[0][i];
    }
    // Stage 4
    ode_fun_(sol_, k_[1]);
    for (i = 0; i < neq_; i++) {
        k_[0][i] = a4 * k_[0][i] + dt * k_[1][i];
        sol_[i] += b4 * k_[0][i];
    }
    // Stage 5
    ode_fun_(sol_, k_[1]);
    for (i = 0; i < neq_; i++) {
        k_[0][i] = a5 * k_[0][i] + dt * k_[1][i];
        sol_[i] += b5 * k_[0][i];
    }
}

// ── OdeRkssp54 ────────────────────────────────────────────────────────────────
OdeRkssp54::OdeRkssp54(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 4), OdeERK(neq)
{
    method_ = "Rkssp54";
    b10 = 0.391752226571890;
    a20 = 0.444370493651235; a21 = 0.555629506348765; b21 = 0.368410593050371;
    a30 = 0.620101851488403; a32 = 0.379898148511597; b32 = 0.251891774271694;
    a40 = 0.178079954393132; a43 = 0.821920045606868; b43 = 0.544974750228521;
    a52 = 0.517231671970585; a53 = 0.096059710526147; b53 = 0.063692468666290;
    a54 = 0.386708617503269; b54 = 0.226007483236906;
    c1  = 0.391752226571890; c2  = 0.586079689311540;
    c3  = 0.474542363121400; c4  = 0.935010630967653;
}

void OdeRkssp54::step_(double dt) {
    // k_[0]=x2/x1, k_[1]=x3, k_[2]=f3, k_[3]=fs
    unsigned long i;
    ode_fun_(sol_, k_[3]);
    // x1 = x + b10*h*fs -> k_[0]
    for (i = 0; i < neq_; i++) k_[0][i] = sol_[i] + b10 * dt * k_[3][i];
    ode_fun_(k_[0], k_[3]);
    // x2 = a20*x + a21*x1 + b21*h*fs -> k_[0]
    for (i = 0; i < neq_; i++) k_[0][i] = a20*sol_[i] + a21*k_[0][i] + b21*dt*k_[3][i];
    ode_fun_(k_[0], k_[3]);
    // x3 = a30*x + a32*x2 + b32*h*fs -> k_[1]
    for (i = 0; i < neq_; i++) k_[1][i] = a30*sol_[i] + a32*k_[0][i] + b32*dt*k_[3][i];
    ode_fun_(k_[1], k_[2]);
    // x4 = a40*x + a43*x3 + b43*h*f3 -> soltemp_
    for (i = 0; i < neq_; i++) soltemp_[i] = a40*sol_[i] + a43*k_[1][i] + b43*dt*k_[2][i];
    ode_fun_(soltemp_, k_[3]);
    // xf = a52*x2 + a53*x3 + b53*h*f3 + a54*x4 + b54*h*fs
    for (i = 0; i < neq_; i++)
        sol_[i] = a52*k_[0][i] + a53*k_[1][i] + b53*dt*k_[2][i] + a54*soltemp_[i] + b54*dt*k_[3][i];
}

// ── OdeRks5 ───────────────────────────────────────────────────────────────────
OdeRks5::OdeRks5(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 5), OdeERK(neq)
{
    method_ = "Rks5";
    a1 = 1.0/9000.0; a2 = 3.0/10.0; a3 = 3.0/4.0;
    cv = 1.0/1134.0;
    c0 = 105.0; c2 = 500.0; c3 = 448.0; c4 = 81.0;
    aa1 = 1.0/9000.0; aa2 = 1.0/10.0; aa3 = 1.0/8.0; aa4 = 1.0/81.0;
    b20 = -4047.0; b21 = 4050.0;
    b30 = 20241.0; b31 = -20250.0; b32 = 15.0;
    b40 = -931041.0; b41 = 931500.0; b42 = -490.0; b43 = 112.0;
}

void OdeRks5::step_(double dt) {
    ode_fun_(sol_, k_[0]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa1 * k_[0][i];
    ode_fun_(soltemp_, k_[1]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa2 * (b20*k_[0][i] + b21*k_[1][i]);
    ode_fun_(soltemp_, k_[2]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa3 * (b30*k_[0][i] + b31*k_[1][i] + b32*k_[2][i]);
    ode_fun_(soltemp_, k_[3]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa4 * (b40*k_[0][i] + b41*k_[1][i] + b42*k_[2][i] + b43*k_[3][i]);
    ode_fun_(soltemp_, k_[4]);
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * cv * (c0*k_[0][i] + c2*k_[2][i] + c3*k_[3][i] + c4*k_[4][i]);
}

// ── OdeRk5 ────────────────────────────────────────────────────────────────────
OdeRk5::OdeRk5(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 6), OdeERK(neq)
{
    method_ = "Rk5";
    a2 = 1.0/5.0; a3 = 2.0/5.0; a5 = 3.0/5.0; a6 = 4.0/5.0;
    b21 = 1.0/5.0; b32 = 2.0/5.0;
    b41 = 9.0/4.0; b42 = -5.0; b43 = 15.0/4.0;
    b51 = -63.0/100.0; b52 = 9.0/5.0; b53 = -13.0/20.0; b54 = 2.0/25.0;
    b61 = -6.0/25.0; b62 = 4.0/5.0; b63 = 2.0/15.0; b64 = 8.0/75.0;
    c1 = 17.0/144.0; c3 = 25.0/36.0; c4 = 1.0/72.0; c5 = -25.0/72.0; c6 = 25.0/48.0;
}

void OdeRk5::step_(double dt) {
    ode_fun_(sol_, k_[0]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b21*k_[0][i]);
    ode_fun_(soltemp_, k_[1]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b32*k_[1][i]);
    ode_fun_(soltemp_, k_[2]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b41*k_[0][i] + b42*k_[1][i] + b43*k_[2][i]);
    ode_fun_(soltemp_, k_[3]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b51*k_[0][i] + b52*k_[1][i] + b53*k_[2][i] + b54*k_[3][i]);
    ode_fun_(soltemp_, k_[4]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b61*k_[0][i] + b62*k_[1][i] + b63*k_[2][i] + b64*k_[3][i]);
    ode_fun_(soltemp_, k_[5]);
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * (c1*k_[0][i] + c3*k_[2][i] + c4*k_[3][i] + c5*k_[4][i] + c6*k_[5][i]);
}

// ── OdeRkc5 ───────────────────────────────────────────────────────────────────
OdeRkc5::OdeRkc5(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 6), OdeERK(neq)
{
    method_ = "Rkc5";
    a2 = 1.0/7.0; a3 = 5.0/14.0; a4 = 9.0/14.0; a5 = 6.0/7.0;
    b21 = 1.0/7.0;
    b31 = -367.0/4088.0; b32 = 261.0/584.0;
    b41 = 41991.0/2044.0; b42 = -2493.0/73.0; b43 = 57.0/4.0;
    b51 = -108413.0/196224.0; b52 = 58865.0/65408.0; b53 = 5.0/16.0; b54 = 265.0/1344.0;
    b61 = -204419.0/58984.0; b62 = 143829.0/58984.0; b63 = 171.0/202.0; b64 = 2205.0/404.0; b65 = -432.0/101.0;
    c1 = 1.0/9.0; c2 = 7.0/2700.0; c3 = 413.0/810.0; c4 = 7.0/450.0; c5 = 28.0/75.0; c6 = -101.0/8100.0;
}

void OdeRkc5::step_(double dt) {
    ode_fun_(sol_, k_[0]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b21*k_[0][i]);
    ode_fun_(soltemp_, k_[1]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b31*k_[0][i] + b32*k_[1][i]);
    ode_fun_(soltemp_, k_[2]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b41*k_[0][i] + b42*k_[1][i] + b43*k_[2][i]);
    ode_fun_(soltemp_, k_[3]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b51*k_[0][i] + b52*k_[1][i] + b53*k_[2][i] + b54*k_[3][i]);
    ode_fun_(soltemp_, k_[4]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b61*k_[0][i] + b62*k_[1][i] + b63*k_[2][i] + b64*k_[3][i] + b65*k_[4][i]);
    ode_fun_(soltemp_, k_[5]);
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * (c1*k_[0][i] + c2*k_[1][i] + c3*k_[2][i] + c4*k_[3][i] + c5*k_[4][i] + c6*k_[5][i]);
}

// ── OdeRkl5 ───────────────────────────────────────────────────────────────────
OdeRkl5::OdeRkl5(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 6), OdeERK(neq)
{
    method_ = "Rkl5";
    a2 = 1.0/12.0; a3 = 1.0/4.0; a4 = 1.0/2.0; a5 = 3.0/4.0;
    b21 = 1.0/12.0;
    b31 = -1.0/8.0; b32 = 3.0/8.0;
    b41 = 3.0/5.0; b42 = -9.0/10.0; b43 = 4.0/5.0;
    b51 = 39.0/80.0; b52 = -9.0/20.0; b53 = 3.0/20.0; b54 = 9.0/16.0;
    b61 = -59.0/35.0; b62 = 66.0/35.0; b63 = 48.0/35.0; b64 = -12.0/7.0; b65 = 8.0/7.0;
    c1 = 7.0/90.0; c3 = 16.0/45.0; c4 = 2.0/15.0; c5 = 16.0/45.0; c6 = 7.0/90.0;
}

void OdeRkl5::step_(double dt) {
    ode_fun_(sol_, k_[0]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b21*k_[0][i]);
    ode_fun_(soltemp_, k_[1]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b31*k_[0][i] + b32*k_[1][i]);
    ode_fun_(soltemp_, k_[2]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b41*k_[0][i] + b42*k_[1][i] + b43*k_[2][i]);
    ode_fun_(soltemp_, k_[3]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b51*k_[0][i] + b52*k_[1][i] + b53*k_[2][i] + b54*k_[3][i]);
    ode_fun_(soltemp_, k_[4]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b61*k_[0][i] + b62*k_[1][i] + b63*k_[2][i] + b64*k_[3][i] + b65*k_[4][i]);
    ode_fun_(soltemp_, k_[5]);
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * (c1*k_[0][i] + c3*k_[2][i] + c4*k_[3][i] + c5*k_[4][i] + c6*k_[5][i]);
}

// ── OdeRklk5a ─────────────────────────────────────────────────────────────────
OdeRklk5a::OdeRklk5a(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 6), OdeERK(neq)
{
    method_ = "Rklk5a";
    const double sqrt5 = std::sqrt(5.0);
    a2 = 0.5; a3 = 0.5 - sqrt5/10.0; a4 = 0.5; a5 = 0.5 + sqrt5/10.0;
    b21 = 0.5;
    b31 = 0.2; b32 = 0.3 - sqrt5/10.0;
    b41 = 0.25; b42 = 0.25;
    b51 = 0.05 - sqrt5/20.0; b52 = -0.2; b53 = 0.25 + 3.0*sqrt5/20.0; b54 = 0.4;
    b61 = sqrt5/4.0 - 0.25; b62 = sqrt5/2.0 - 0.5; b63 = 1.25 - sqrt5/4.0; b64 = -2.0; b65 = 2.5 - sqrt5/2.0;
    c1 = 1.0/12.0; c3 = 5.0/12.0; c5 = 5.0/12.0; c6 = 1.0/12.0;
}

void OdeRklk5a::step_(double dt) {
    ode_fun_(sol_, k_[0]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b21*k_[0][i]);
    ode_fun_(soltemp_, k_[1]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b31*k_[0][i] + b32*k_[1][i]);
    ode_fun_(soltemp_, k_[2]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b41*k_[0][i] + b42*k_[1][i]);
    ode_fun_(soltemp_, k_[3]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b51*k_[0][i] + b52*k_[1][i] + b53*k_[2][i] + b54*k_[3][i]);
    ode_fun_(soltemp_, k_[4]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b61*k_[0][i] + b62*k_[1][i] + b63*k_[2][i] + b64*k_[3][i] + b65*k_[4][i]);
    ode_fun_(soltemp_, k_[5]);
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * (c1*k_[0][i] + c3*k_[2][i] + c5*k_[4][i] + c6*k_[5][i]);
}

// ── OdeRklk5b ─────────────────────────────────────────────────────────────────
OdeRklk5b::OdeRklk5b(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 6), OdeERK(neq)
{
    method_ = "Rklk5b";
    const double sqrt15 = std::sqrt(15.0);
    a2 = 2.0/5.0; a3 = 0.5; a5 = 0.5 - sqrt15/10.0; a6 = 0.5 + sqrt15/10.0;
    b21 = 2.0/5.0;
    b31 = 3.0/16.0; b32 = 5.0/16.0;
    b41 = 0.25; b42 = -1.25; b43 = 2.0;
    b51 = 3.0/20.0 - sqrt15/100.0; b52 = -0.25; b53 = 3.0/5.0 - 2.0*sqrt15/25.0; b54 = -sqrt15/100.0;
    b61 = -3.0/20.0 - sqrt15/20.0; b62 = -0.25; b63 = 3.0/5.0; b64 = 3.0/10.0 - sqrt15/20.0; b65 = sqrt15/5.0;
    c3 = 4.0/9.0; c5 = 5.0/18.0; c6 = 5.0/18.0;
}

void OdeRklk5b::step_(double dt) {
    ode_fun_(sol_, k_[0]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b21*k_[0][i]);
    ode_fun_(soltemp_, k_[1]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b31*k_[0][i] + b32*k_[1][i]);
    ode_fun_(soltemp_, k_[2]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b41*k_[0][i] + b42*k_[1][i] + b43*k_[2][i]);
    ode_fun_(soltemp_, k_[3]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b51*k_[0][i] + b52*k_[1][i] + b53*k_[2][i] + b54*k_[3][i]);
    ode_fun_(soltemp_, k_[4]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b61*k_[0][i] + b62*k_[1][i] + b63*k_[2][i] + b64*k_[3][i] + b65*k_[4][i]);
    ode_fun_(soltemp_, k_[5]);
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * (c3*k_[2][i] + c5*k_[4][i] + c6*k_[5][i]);
}

// ── OdeRkb6 ───────────────────────────────────────────────────────────────────
OdeRkb6::OdeRkb6(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 7), OdeERK(neq)
{
    method_ = "Rkb6";
    a2 = 1.0/3.0; a3 = 2.0/3.0; a4 = 1.0/3.0; a5 = 0.5; a6 = 0.5;
    b21 = 1.0/3.0;
    b32 = 2.0/3.0;
    b41 = 1.0/12.0; b42 = 1.0/3.0; b43 = -1.0/12.0;
    b51 = -1.0/16.0; b52 = 9.0/8.0; b53 = -3.0/16.0; b54 = -3.0/8.0;
    b62 = 9.0/8.0; b63 = -3.0/8.0; b64 = -3.0/4.0; b65 = 0.5;
    b71 = 9.0/44.0; b72 = -9.0/11.0; b73 = 63.0/44.0; b74 = 18.0/11.0; b76 = -16.0/11.0;
    c1 = 11.0/120.0; c3 = 27.0/40.0; c4 = 27.0/40.0; c5 = -4.0/15.0; c6 = -4.0/15.0; c7 = 11.0/120.0;
}

void OdeRkb6::step_(double dt) {
    ode_fun_(sol_, k_[0]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b21*k_[0][i]);
    ode_fun_(soltemp_, k_[1]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b32*k_[1][i]);
    ode_fun_(soltemp_, k_[2]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b41*k_[0][i] + b42*k_[1][i] + b43*k_[2][i]);
    ode_fun_(soltemp_, k_[3]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b51*k_[0][i] + b52*k_[1][i] + b53*k_[2][i] + b54*k_[3][i]);
    ode_fun_(soltemp_, k_[4]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b62*k_[1][i] + b63*k_[2][i] + b64*k_[3][i] + b65*k_[4][i]);
    ode_fun_(soltemp_, k_[5]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b71*k_[0][i] + b72*k_[1][i] + b73*k_[2][i] + b74*k_[3][i] + b76*k_[5][i]);
    ode_fun_(soltemp_, k_[6]);
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * (c1*k_[0][i] + c3*k_[2][i] + c4*k_[3][i] + c5*k_[4][i] + c6*k_[5][i] + c7*k_[6][i]);
}

// ── OdeRk7 ────────────────────────────────────────────────────────────────────
OdeRk7::OdeRk7(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 9), OdeERK(neq)
{
    method_ = "Rk7";
    a1 = 2.0/9.0; a2 = 1.0/3.0; a3 = 0.5; a4 = 1.0/6.0; a5 = 8.0/9.0; a6 = 1.0/9.0; a7 = 5.0/6.0;
    cv = 1.0/2140320.0;
    c0 = 110201.0; c3 = 767936.0; c4 = 635040.0; c5 = -59049.0; c6 = -59049.0; c7 = 635040.0; c8 = 110201.0;
    aa1 = 2.0/9.0; aa2 = 1.0/12.0; aa3 = 1.0/8.0; aa4 = 1.0/216.0; aa5 = 1.0/729.0; aa6 = 1.0/151632.0; aa7 = 1.0/1375920.0; aa8 = 1.0/251888.0;
    b21 = 3.0; b32 = 3.0;
    b40 = 23.0; b42 = 21.0; b43 = -8.0;
    b50 = -4136.0; b52 = -13584.0; b53 = 5264.0; b54 = 13104.0;
    b60 = 105131.0; b62 = 302016.0; b63 = -107744.0; b64 = -284256.0; b65 = 1701.0;
    b70 = -775229.0; b72 = -2770950.0; b73 = 1735136.0; b74 = 2547216.0; b75 = 81891.0; b76 = 328536.0;
    b80 = 23569.0; b82 = -122304.0; b83 = -20384.0; b84 = 695520.0; b85 = -99873.0; b86 = -466560.0; b87 = 241920.0;
}

void OdeRk7::step_(double dt) {
    // k_[0]=f0, k_[1]=f1, ..., k_[8]=f8
    ode_fun_(sol_, k_[0]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa1 * k_[0][i];
    ode_fun_(soltemp_, k_[1]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa2 * (k_[0][i] + b21*k_[1][i]);
    ode_fun_(soltemp_, k_[2]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa3 * (k_[0][i] + b32*k_[2][i]);
    ode_fun_(soltemp_, k_[3]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa4 * (b40*k_[0][i] + b42*k_[2][i] + b43*k_[3][i]);
    ode_fun_(soltemp_, k_[4]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa5 * (b50*k_[0][i] + b52*k_[2][i] + b53*k_[3][i] + b54*k_[4][i]);
    ode_fun_(soltemp_, k_[5]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa6 * (b60*k_[0][i] + b62*k_[2][i] + b63*k_[3][i] + b64*k_[4][i] + b65*k_[5][i]);
    ode_fun_(soltemp_, k_[6]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa7 * (b70*k_[0][i] + b72*k_[2][i] + b73*k_[3][i] + b74*k_[4][i] + b75*k_[5][i] + b76*k_[6][i]);
    ode_fun_(soltemp_, k_[7]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa8 * (b80*k_[0][i] + b82*k_[2][i] + b83*k_[3][i] + b84*k_[4][i] + b85*k_[5][i] + b86*k_[6][i] + b87*k_[7][i]);
    ode_fun_(soltemp_, k_[8]);
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * cv * (c0*k_[0][i] + c3*k_[3][i] + c4*k_[4][i] + c5*k_[5][i] + c6*k_[6][i] + c7*k_[7][i] + c8*k_[8][i]);
}

// ── OdeRk8_10 ─────────────────────────────────────────────────────────────────
OdeRk8_10::OdeRk8_10(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 10), OdeERK(neq)
{
    method_ = "Rk8_10";
    a1 = 4.0/27.0; a2 = 2.0/9.0; a3 = 1.0/3.0; a4 = 0.5; a5 = 2.0/3.0; a6 = 1.0/6.0; a8 = 5.0/6.0;
    cv = 1.0/840.0;
    c0 = 41.0; c3 = 27.0; c4 = 272.0; c5 = 27.0; c6 = 216.0; c8 = 216.0; c9 = 41.0;
    aa1 = 4.0/27.0; aa2 = 1.0/18.0; aa3 = 1.0/12.0; aa4 = 1.0/8.0; aa5 = 1.0/54.0;
    aa6 = 1.0/4320.0; aa7 = 1.0/20.0; aa8 = 1.0/288.0; aa9 = 1.0/820.0;
    b21 = 3.0; b32 = 3.0; b43 = 3.0;
    b50 = 13.0; b52 = -27.0; b53 = 42.0; b54 = 8.0;
    b60 = 389.0; b62 = -54.0; b63 = 966.0; b64 = -824.0; b65 = 243.0;
    b70 = -231.0; b72 = 81.0; b73 = -1164.0; b74 = 656.0; b75 = -122.0; b76 = 800.0;
    b80 = -127.0; b82 = 18.0; b83 = -678.0; b84 = 456.0; b85 = -9.0; b86 = 576.0; b87 = 4.0;
    b90 = 1481.0; b92 = -81.0; b93 = 7104.0; b94 = -3376.0; b95 = 72.0; b96 = -5040.0; b97 = -60.0; b98 = 720.0;
}

void OdeRk8_10::step_(double dt) {
    ode_fun_(sol_, k_[0]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa1 * k_[0][i];
    ode_fun_(soltemp_, k_[1]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa2 * (k_[0][i] + b21*k_[1][i]);
    ode_fun_(soltemp_, k_[2]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa3 * (k_[0][i] + b32*k_[2][i]);
    ode_fun_(soltemp_, k_[3]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa4 * (k_[0][i] + b43*k_[3][i]);
    ode_fun_(soltemp_, k_[4]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa5 * (b50*k_[0][i] + b52*k_[2][i] + b53*k_[3][i] + b54*k_[4][i]);
    ode_fun_(soltemp_, k_[5]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa6 * (b60*k_[0][i] + b62*k_[2][i] + b63*k_[3][i] + b64*k_[4][i] + b65*k_[5][i]);
    ode_fun_(soltemp_, k_[6]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa7 * (b70*k_[0][i] + b72*k_[2][i] + b73*k_[3][i] + b74*k_[4][i] + b75*k_[5][i] + b76*k_[6][i]);
    ode_fun_(soltemp_, k_[7]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa8 * (b80*k_[0][i] + b82*k_[2][i] + b83*k_[3][i] + b84*k_[4][i] + b85*k_[5][i] + b86*k_[6][i] + b87*k_[7][i]);
    ode_fun_(soltemp_, k_[8]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa9 * (b90*k_[0][i] + b92*k_[2][i] + b93*k_[3][i] + b94*k_[4][i] + b95*k_[5][i] + b96*k_[6][i] + b97*k_[7][i] + b98*k_[8][i]);
    ode_fun_(soltemp_, k_[9]);
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * cv * (c0*k_[0][i] + c3*k_[3][i] + c4*k_[4][i] + c5*k_[5][i] + c6*k_[6][i] + c8*k_[8][i] + c9*k_[9][i]);
}

// ── OdeRkcv8 ──────────────────────────────────────────────────────────────────
OdeRkcv8::OdeRkcv8(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 11), OdeERK(neq)
{
    method_ = "Rkcv8";
    const double s = std::sqrt(21.0);
    a2 = 0.5; a3 = 0.5; a4 = 0.5 - s/14.0; a5 = 0.5 - s/14.0; a6 = 0.5;
    a7 = 0.5 + s/14.0; a8 = 0.5 + s/14.0; a9 = 0.5; a10 = 0.5 - s/14.0;
    b21 = 0.5;
    b31 = 0.25; b32 = 0.25;
    b41 = 1.0/7.0; b42 = -1.0/14.0 + 3.0*s/98.0; b43 = 3.0/7.0 - 5.0*s/49.0;
    b51 = 11.0/84.0 - s/84.0; b53 = 2.0/7.0 - 4.0*s/63.0; b54 = 1.0/12.0 + s/252.0;
    b61 = 5.0/48.0 - s/48.0; b63 = 0.25 - s/36.0; b64 = -77.0/120.0 - 7.0*s/180.0; b65 = 63.0/80.0 + 7.0*s/80.0;
    b71 = 5.0/21.0 + s/42.0; b73 = -48.0/35.0 - 92.0*s/315.0; b74 = 211.0/30.0 + 29.0*s/18.0; b75 = -36.0/5.0 - 23.0*s/14.0; b76 = 9.0/5.0 + 13.0*s/35.0;
    b81 = 1.0/14.0; b85 = 1.0/9.0 + s/42.0; b86 = 13.0/63.0 + s/21.0; b87 = 1.0/9.0;
    b91 = 1.0/32.0; b95 = 91.0/576.0 + 7.0*s/192.0; b96 = 11.0/72.0; b97 = -385.0/1152.0 + 25.0*s/384.0; b98 = 63.0/128.0 - 13.0*s/128.0;
    b101 = 1.0/14.0; b105 = 1.0/9.0; b106 = -733.0/2205.0 + s/15.0; b107 = 515.0/504.0 - 37.0*s/168.0; b108 = -51.0/56.0 + 11.0*s/56.0; b109 = 132.0/245.0 - 4.0*s/35.0;
    b115 = -7.0/3.0 - 7.0*s/18.0; b116 = -2.0/5.0 - 28.0*s/45.0; b117 = -91.0/24.0 + 53.0*s/72.0; b118 = 301.0/72.0 - 53.0*s/72.0; b119 = 28.0/45.0 + 28.0*s/45.0; b1110 = 49.0/18.0 + 7.0*s/18.0;
    c1 = 1.0/20.0; c8 = 49.0/180.0; c9 = 16.0/45.0; c10 = 49.0/180.0; c11 = 1.0/20.0;
}

void OdeRkcv8::step_(double dt) {
    ode_fun_(sol_, k_[0]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b21*k_[0][i]);
    ode_fun_(soltemp_, k_[1]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b31*k_[0][i] + b32*k_[1][i]);
    ode_fun_(soltemp_, k_[2]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b41*k_[0][i] + b42*k_[1][i] + b43*k_[2][i]);
    ode_fun_(soltemp_, k_[3]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b51*k_[0][i] + b53*k_[2][i] + b54*k_[3][i]);
    ode_fun_(soltemp_, k_[4]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b61*k_[0][i] + b63*k_[2][i] + b64*k_[3][i] + b65*k_[4][i]);
    ode_fun_(soltemp_, k_[5]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b71*k_[0][i] + b73*k_[2][i] + b74*k_[3][i] + b75*k_[4][i] + b76*k_[5][i]);
    ode_fun_(soltemp_, k_[6]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b81*k_[0][i] + b85*k_[4][i] + b86*k_[5][i] + b87*k_[6][i]);
    ode_fun_(soltemp_, k_[7]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b91*k_[0][i] + b95*k_[4][i] + b96*k_[5][i] + b97*k_[6][i] + b98*k_[7][i]);
    ode_fun_(soltemp_, k_[8]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b101*k_[0][i] + b105*k_[4][i] + b106*k_[5][i] + b107*k_[6][i] + b108*k_[7][i] + b109*k_[8][i]);
    ode_fun_(soltemp_, k_[9]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b115*k_[4][i] + b116*k_[5][i] + b117*k_[6][i] + b118*k_[7][i] + b119*k_[8][i] + b1110*k_[9][i]);
    ode_fun_(soltemp_, k_[10]);
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * (c1*k_[0][i] + c8*k_[7][i] + c9*k_[8][i] + c10*k_[9][i] + c11*k_[10][i]);
}

// ── OdeRk8_12 ─────────────────────────────────────────────────────────────────
OdeRk8_12::OdeRk8_12(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 12), OdeERK(neq)
{
    method_ = "Rk8_12";
    a1 = 1.0/9.0; a2 = 1.0/6.0; a3 = 0.25; a4 = 1.0/10.0; a5 = 1.0/6.0; a6 = 0.5; a7 = 2.0/3.0; a8 = 1.0/3.0; a9 = 5.0/6.0; a10 = 5.0/6.0;
    cv = 1.0/840.0;
    c0 = 41.0; c5 = 216.0; c6 = 272.0; c7 = 27.0; c8 = 27.0; c9 = 36.0; c10 = 180.0; c11 = 41.0;
    aa1 = 1.0/9.0; aa2 = 1.0/24.0; aa3 = 1.0/16.0; aa4 = 1.0/500.0; aa5 = 1.0/972.0; aa6 = 1.0/36.0;
    aa7 = 1.0/243.0; aa8 = 1.0/324.0; aa9 = 1.0/324.0; aa10 = 1.0/1620.0; aa11 = 1.0/4428.0;
    b21 = 3.0; b32 = 3.0;
    b40 = 29.0; b42 = 33.0; b43 = -12.0;
    b50 = 33.0; b53 = 4.0; b54 = 125.0;
    b60 = -21.0; b63 = 76.0; b64 = 125.0; b65 = -162.0;
    b70 = -30.0; b73 = -32.0; b74 = 125.0; b76 = 99.0;
    b80 = 1175.0; b83 = -3456.0; b84 = -6250.0; b85 = 8424.0; b86 = 242.0; b87 = -27.0;
    b90 = 293.0; b93 = -852.0; b94 = -1375.0; b95 = 1836.0; b96 = -118.0; b97 = 162.0; b98 = 324.0;
    b100 = 1303.0; b103 = -4260.0; b104 = -6875.0; b105 = 9990.0; b106 = 1030.0; b109 = 162.0;
    b110 = -8595.0; b113 = 30720.0; b114 = 48750.0; b115 = -66096.0; b116 = 378.0; b117 = -729.0; b118 = -1944.0; b119 = -1296.0; b1110 = 3240.0;
}

void OdeRk8_12::step_(double dt) {
    ode_fun_(sol_, k_[0]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa1 * k_[0][i];
    ode_fun_(soltemp_, k_[1]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa2 * (k_[0][i] + b21*k_[1][i]);
    ode_fun_(soltemp_, k_[2]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa3 * (k_[0][i] + b32*k_[2][i]);
    ode_fun_(soltemp_, k_[3]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa4 * (b40*k_[0][i] + b42*k_[2][i] + b43*k_[3][i]);
    ode_fun_(soltemp_, k_[4]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa5 * (b50*k_[0][i] + b53*k_[3][i] + b54*k_[4][i]);
    ode_fun_(soltemp_, k_[5]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa6 * (b60*k_[0][i] + b63*k_[3][i] + b64*k_[4][i] + b65*k_[5][i]);
    ode_fun_(soltemp_, k_[6]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa7 * (b70*k_[0][i] + b73*k_[3][i] + b74*k_[4][i] + b76*k_[6][i]);
    ode_fun_(soltemp_, k_[7]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa8 * (b80*k_[0][i] + b83*k_[3][i] + b84*k_[4][i] + b85*k_[5][i] + b86*k_[6][i] + b87*k_[7][i]);
    ode_fun_(soltemp_, k_[8]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa9 * (b90*k_[0][i] + b93*k_[3][i] + b94*k_[4][i] + b95*k_[5][i] + b96*k_[6][i] + b97*k_[7][i] + b98*k_[8][i]);
    ode_fun_(soltemp_, k_[9]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa10 * (b100*k_[0][i] + b103*k_[3][i] + b104*k_[4][i] + b105*k_[5][i] + b106*k_[6][i] + b109*k_[9][i]);
    ode_fun_(soltemp_, k_[10]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * aa11 * (b110*k_[0][i] + b113*k_[3][i] + b114*k_[4][i] + b115*k_[5][i] + b116*k_[6][i] + b117*k_[7][i] + b118*k_[8][i] + b119*k_[9][i] + b1110*k_[10][i]);
    ode_fun_(soltemp_, k_[11]);
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * cv * (c0*k_[0][i] + c5*k_[5][i] + c6*k_[6][i] + c7*k_[7][i] + c8*k_[8][i] + c9*k_[9][i] + c10*k_[10][i] + c11*k_[11][i]);
}

// ── OdeRks10 ──────────────────────────────────────────────────────────────────
OdeRks10::OdeRks10(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 15), OdeERK(neq)
{
    method_ = "Rks10";
    a2 = 0.133333333333333333333333333333333333333333333333333333;
    a3 = 0.266666666666666666666666666666666666666666666666666667;
    a4 = 0.400000000000000000000000000000000000000000000000000000;
    a5 = 0.571428571428571428571428571428571428571428571428571429;
    a6 = 0.778740761536291800442363524550407562438954342313508171;
    a7 = 0.642615758240322548157075497020439535959501736363212696;
    a8 = 0.882527661964732346425501486979669075182867844268052120;
    a9 = 0.117472338035267653574498513020330924817132155731947880;
    a10 = 0.117472338035267653574498513020330924817132155731947880;
    a11 = 0.357384241759677451842924502979560464040498263636787304;
    a12 = 0.357384241759677451842924502979560464040498263636787304;
    a13 = 0.642615758240322548157075497020439535959501736363212696;
    a14 = 0.882527661964732346425501486979669075182867844268052120;
    b21 = 0.133333333333333333333333333333333333333333333333333333;
    b32 = 0.266666666666666666666666666666666666666666666666666667;
    b41 = 0.100000000000000000000000000000000000000000000000000000;
    b43 = 0.300000000000000000000000000000000000000000000000000000;
    b51 = 0.134110787172011661807580174927113702623906705539358601;
    b53 = 0.087463556851311953352769679300291545189504373177842566e0;
    b54 = 0.349854227405247813411078717201166180758017492711370262;
    b61 = 0.328658938997791240721282668696245180100949935428058492;
    b63 = -0.843123044711636120617820434267319755889422538822419940;
    b64 = 1.230383064721160870667126440845906263177038685065274298;
    b65 = 0.062821802528975809671774849275575875050388260642595320;
    b71 = 0.130186990844533481397415277676876710451288921648802896;
    b74 = 0.580738972917374107341609289970199905398640941037865281;
    b75 = -0.132061017042928219570339015811796150446818327131853650;
    b76 = 0.063750811521343178988389945185159070556390200808398170;
    b81 = 0.116969144664032355608536137983019767284447494556651904;
    b84 = 0.804791321585223079797049560718389631085997501079130322;
    b85 = -1.032442407303393452179074692383412947850773751656102435;
    b86 = 0.141237507577425159424271186625123531674337461239364982;
    b87 = 0.851972095441445203774719294036549092988859139049007347;
    b91 = 0.072301376815318053808169355285248577196274719038173037;
    b94 = 0.278449353793857950736187552433349927872809528174204140;
    b95 = -0.720355742529399485982635010432963793377590743017129915;
    b96 = 0.071478309723007569077180467110101963232268708471224068;
    b97 = 0.480546127100390254710165280839106235150089447220646222;
    b98 = -0.064947086867906688774569132214511985256719504155169671;
    b101 = -0.033604580075425596431640044865448312697754125457571034;
    b104 = -0.857669388668674924032812732229566068998144989807984563;
    b105 = 2.054105395119576054328336908587945831582620175191578897;
    b106 = -0.209909042661849185066865444237374444876978972036734389;
    b107 = -1.333972235951611409051125524672704502068159831145103714;
    b108 = 0.180806348384581339693160596923480075192005804935788562;
    b109 = 0.317715841888671374135444753513998346683544094051974121;
    b111 = 0.037810053861933727583877526864046322952305373764286755;
    b114 = 0.045591558641579644208161372472470642588279099631100287;
    b115 = 0.374858247294747357801811789348440678077393709386181865;
    b116 = -0.018981904165238088665273073099056486944960468054380558;
    b117 = -0.336038862637559379967733799098120208915305739878288301;
    b118 = 0.042207716375559153022580951807545725160812786625465880;
    b119 = 0.051900360983690832037790128319440699859433125187462258;
    b1110 = 0.160037071404964205821709606364793091262540376974959118;
    b121 = 0.025378474265434522073341785109062596527860323276820541;
    b124 = 0.362306327543474080870356990535692676714482207942118981;
    b125 = -2.130022066259949711216588139786172085385079053283141572;
    b126 = 0.167271594859437715452103295041620121357577378494618057;
    b127 = 1.582750941864663213591839501273807297171503649177669092;
    b128 = -0.204720185139081567587174987631558173178320360472369067;
    b129 = 0.319265328144806137168538521838050367024648119039965646;
    b1210 = -0.199485256254696927298131093258193563570518501610697727;
    b1211 = 0.434639082735589988788638629857251227378344501071803353;
    b131 = 0.021029555123492649231004219196592342935742304160151528;
    b134 = -0.208531388344693562066394580214099278549298495126493309;
    b135 = 0.326863498654767652665730419629359718148991455519984894;
    b136 = -0.040291595063466341977981502499810508092167265608412827;
    b137 = -0.062168570110085104851127006245165266954000899589268211;
    b138 = 0.019401511635634607470015991594882193791752892731789828;
    b139 = 0.033618934322149698701521634179531768990520248587976979;
    b1310 = 0.184057539066668989188409585677311173226149887887488900;
    b1311 = 0.126719042450063065291362679244566355420313628999369516;
    b1312 = 0.241917230505790894504534056457271037041497978800625397;
    b141 = 0.059015439601144226763876106824299822651056803159364027;
    b144 = 0.346753529400439458709721774328857076918185484739163575;
    b145 = -0.543520439250941361637466822737163641756559230304227659;
    b146 = 0.066998320513439386022349163118524383402374304344205128;
    b147 = -0.209105926273890705326433037391968985907198847287185344;
    b148 = 0.034196819215377393468216514309803928830340701888167709;
    b149 = 0.656808691640117064523673744875271638362693883629496565;
    b1410 = -0.530023086491271629685194176217211641242245756479495834;
    b1411 = 0.682370713621575520444713157488350028626121649514411954;
    b1412 = -0.524316794649154201841271672103049178047360868487598332;
    b1413 = 0.843350394637897194983316734483955643345459719551750331;
    b151 = -0.040440866328662678612786513864723972854343311900228087;
    b154 = -0.936174412160392166960880644803504126455580435000519336;
    b155 = 1.467410954959016212947954342116057777884286077126485057;
    b156 = -0.180883849778987051699697942242124022922323870654198892;
    b157 = 1.167589102274410575304328368479226265476383761610677844;
    b158 = -0.280113604810353377498686142206136702751291429624401159;
    b159 = -1.871668376348516420302371567652811494220817967866998660;
    b1510 = 2.241588116125051681252015179294157675223848655884912767;
    b1511 = -1.572572082691418559566908951416310106551328354276059432;
    b1512 = 2.151700399601058696148477230250852973515302452974155467;
    b1513 = -1.813340450902764402851969633637645578401662879405552526;
    b1514 = 0.666905070061557491840526275682961312057527301131726957;
    c1 = 0.033333333333333333333333333333333333333333333333333333;
    c9 = 0.135169627249231064398790288647151661598687390677589879;
    c10 = 0.054067850899692425759516115458860664639474956271035952;
    c11 = 0.215778257736022470617613537547175598111058915336253984;
    c12 = 0.061650930781720705890746725013478742317445404381786853;
    c13 = 0.277429188517743176508360262560654340428504319718040836;
    c14 = 0.189237478148923490158306404106012326238162346948625830;
    c15 = 0.033333333333333333333333333333333333333333333333333333;
}

void OdeRks10::step_(double dt) {
    ode_fun_(sol_, k_[0]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b21*k_[0][i]);
    ode_fun_(soltemp_, k_[1]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b32*k_[1][i]);
    ode_fun_(soltemp_, k_[2]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b41*k_[0][i] + b43*k_[2][i]);
    ode_fun_(soltemp_, k_[3]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b51*k_[0][i] + b53*k_[2][i] + b54*k_[3][i]);
    ode_fun_(soltemp_, k_[4]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b61*k_[0][i] + b63*k_[2][i] + b64*k_[3][i] + b65*k_[4][i]);
    ode_fun_(soltemp_, k_[5]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b71*k_[0][i] + b74*k_[3][i] + b75*k_[4][i] + b76*k_[5][i]);
    ode_fun_(soltemp_, k_[6]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b81*k_[0][i] + b84*k_[3][i] + b85*k_[4][i] + b86*k_[5][i] + b87*k_[6][i]);
    ode_fun_(soltemp_, k_[7]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b91*k_[0][i] + b94*k_[3][i] + b95*k_[4][i] + b96*k_[5][i] + b97*k_[6][i] + b98*k_[7][i]);
    ode_fun_(soltemp_, k_[8]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b101*k_[0][i] + b104*k_[3][i] + b105*k_[4][i] + b106*k_[5][i] + b107*k_[6][i] + b108*k_[7][i] + b109*k_[8][i]);
    ode_fun_(soltemp_, k_[9]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b111*k_[0][i] + b114*k_[3][i] + b115*k_[4][i] + b116*k_[5][i] + b117*k_[6][i] + b118*k_[7][i] + b119*k_[8][i] + b1110*k_[9][i]);
    ode_fun_(soltemp_, k_[10]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b121*k_[0][i] + b124*k_[3][i] + b125*k_[4][i] + b126*k_[5][i] + b127*k_[6][i] + b128*k_[7][i] + b129*k_[8][i] + b1210*k_[9][i] + b1211*k_[10][i]);
    ode_fun_(soltemp_, k_[11]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b131*k_[0][i] + b134*k_[3][i] + b135*k_[4][i] + b136*k_[5][i] + b137*k_[6][i] + b138*k_[7][i] + b139*k_[8][i] + b1310*k_[9][i] + b1311*k_[10][i] + b1312*k_[11][i]);
    ode_fun_(soltemp_, k_[12]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b141*k_[0][i] + b144*k_[3][i] + b145*k_[4][i] + b146*k_[5][i] + b147*k_[6][i] + b148*k_[7][i] + b149*k_[8][i] + b1410*k_[9][i] + b1411*k_[10][i] + b1412*k_[11][i] + b1413*k_[12][i]);
    ode_fun_(soltemp_, k_[13]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b151*k_[0][i] + b154*k_[3][i] + b155*k_[4][i] + b156*k_[5][i] + b157*k_[6][i] + b158*k_[7][i] + b159*k_[8][i] + b1510*k_[9][i] + b1511*k_[10][i] + b1512*k_[11][i] + b1513*k_[12][i] + b1514*k_[13][i]);
    ode_fun_(soltemp_, k_[14]);
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * (c1*k_[0][i] + c9*k_[8][i] + c10*k_[9][i] + c11*k_[10][i] + c12*k_[11][i] + c13*k_[12][i] + c14*k_[13][i] + c15*k_[14][i]);
}

// ── OdeRkz10 ──────────────────────────────────────────────────────────────────
// Helper: computes row sum of Butcher tableau row for rkz10 c-nodes
OdeRkz10::OdeRkz10(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 16), OdeERK(neq)
{
    method_ = "Rkz10";
    b21   = +0.06888096612188652230677098661632935381315159322698285980682460033484176156636;
    b31   = -0.83810520353364237535186366202804838694106968892100493081079119861616316483888;
    b32   = +1.25369004871695465344923436259290210937215845259642921936068038992469719487344;
    b41   = -0.00490948675932680175369634467679043271190185159760893197940412354439119879747;
    b42   = +0.08232173030768021571147603044143500716498050758632346872918243517328221749373;
    b43   = -0.00853127742646689165100869914831522063992706276173167694295371129404925712991;
    b51   = +1.04893195376435953811751460952804681519865641127045728585681123905425821150864;
    b52   = -0.75382817731759581241904280322911590179250916990960557195131331600659134298994;
    b53   = +0.80522815974064911780476652756884437914316713728791543021500322792680878839117;
    b54   = -0.38446322074238561063992159166768203951623890872336238198515458977800812023865;
    b61   = -0.23992383433329995270018501071487605020336614587127799309879281304981537313900;
    b62   = -0.06364261163929229571107164643776483490956349179570579724023318438638668084701;
    b63   = +0.19967543135197895864283978317448663244372194705250026204952108330132418721869;
    b64   = +0.61035379509547002158981202467825146906358429601618359949777530100370262806814;
    b65   = +0.37859947224999048426403753064329493462141566215033797838496558400426835557965;
    b71   = +0.01778833946316172550067667113299992644020408256145083580468064240809624146756;
    b72   = -0.01105021905501825472152093430479940021746699824126398377648788328289961044531;
    b73   = -0.00439342855052892929633342674635339618911779936473248023015437836908839819799;
    b74   = +0.10597527290509018731505658395393495306363828909501538659771103682791206047696;
    b75   = +0.00405089069638330732472929903714130352675532845602736909932582009874330236167;
    b76   = -0.00167929709134223461022001825411827908084682305838572957913124250043819202374;
    b81   = +0.23566046225418537925692690356824973135127585829120749799176100081366132717900;
    b82   = +0.08893362689701559656112930868794335701480724068673657440558996917556014206620;
    b83   = +0.04138834709876858545169518341465722051417932518424807026039922164100253591914;
    b84   = -0.85290303603263069912309614093599359240031750375681439893926580845622126359984;
    b85   = -0.02308087548113625563008379864870135277272977633339373075859414529485111031087;
    b86   = +0.00968592188591852514590807745954989979539211465004178878459403530669594442891;
    b87   = +0.80144977934196893864833794701047679699141990680205169668196460489197395944287;
    b91   = +0.09048693760705330681795027653195090842362561521235737211709949607524998063814;
    b92   = +0.03204427202479226242378001797390171191984804295279358947221505285617826137779;
    b93   = +0.12433768215891056280095053314827014992247446913700152962717366852615035330919;
    b94   = -0.30731521755038158104946232011583084977900951861660916480042444618524837737411;
    b95   = +0.16447843681160268810163161551627427448581444120771873237010859382161886567365;
    b96   = -0.04100681167344768439646557295106150922485658224571650562701715147123044716022;
    b97   = +0.40661989930773198456977153545461924102851176247811339291440854490078881632775;
    b98   = +0.18669987124631939180018777607871869034096434382893787663176153320303681978038;
    b101  = -0.12815849137721058028379122684114729957595452282110310092889040855742427673083;
    b102  = -0.09494292242532245543748881606192770346771760827967185000597507680447193729080;
    b103  = -0.14450344511826259894917045729900439186304081642314445476990786282061867102157;
    b104  = +0.92134940704406912363606156819205798706915260363096464763106069459083167107327;
    b105  = -0.13265301054949046320484467982507968172620145761107284691851092173475314294014;
    b106  = +0.01661485872631464348319012586897860991564771318706900087871886151957318825574;
    b107  = -0.64461773243991825568826538659502156175792556790721387489238296660149971176174;
    b108  = +0.47148463683422024098060807066053294761028203774917809473770514989820710158152;
    b109  = +0.15101154448891262356107150246546481622684638215041867281807172181868980886911;
    b111  = -0.35394262889926481874318272132638227200275604000107564690424492655250744148526;
    b112  = -0.12999250061102426001102206164524015480707041252098090235235458563228391897841;
    b113  = +0.07296717474130959358123820183761890201581557542115580687780801792533382974931;
    b114  = +1.23409288203437673148811178545199717478084359792767549031487434785241641765067;
    b115  = +0.24325479314414790350916184694938182359002523242070889454561579643424180130675;
    b116  = -0.04886415340347670065787549059652108014832442895629947389530738182689787996728;
    b117  = -0.51442371407978914516038594645191171303742863244306956194414424856852330710239;
    b118  = +0.07088500449199304304537010242765865457286791828303557337358385517356109210099;
    b119  = -0.20555333994396552677698205301698855743095009068049025897065388713681681672772;
    b1110 = +0.04716132770900545782293703693524094489806604422476436750471220364001025348790;
    b121  = -1.28883596401187309154174375754588391722337985378291577953064993833085325077145;
    b122  = -0.28020869761589692854933312826845810393637896090137224607343344880154309805585;
    b123  = +2.95502531060581466025398140893693575414694538631086569687435523799636000133369;
    b124  = +2.68729452804277601048934070442766694554427130448420986662047690713132366345266;
    b125  = +4.94062342991935573838476417426809163760371481941041460313006887242753416771859;
    b126  = -1.10669748003212502578007105340045335120159611347391447923604138591653946190627;
    b127  = -1.01499184284449721513261150169983810954496707583685170059698090784083909089787;
    b128  = +2.53750982609373480524394613209930567128221625331336116097749979112883387569908;
    b129  = -3.05085435525303312587778339284072228780737127406865428592625671405448243077510;
    b1210 = -4.48248369012964780236263511347668602256466957962418723733767273268597925464354;
    b1211 = -1.21257843188510861623323738621796488386816954981559478788233918659317859600661;
    b131  = -0.26440344286774078027313975052115341819194404740705505988980388884472351746613;
    b132  = -0.06681118811399606825394685412417487195704333031683770523718698572711060614338;
    b133  = +0.22983178806827308655694675660046084355033982772309698316158066683955449428729;
    b134  = +0.64074149645736181719485655167408177784648088775776468738158085006283347730738;
    b135  = +0.42942911081576828545053652299580217770451868606900245607955202491949872419495;
    b136  = -0.00967078876662303036856558223558993132212111489612543886857914521712390529304;
    b137  = +0.01613666672999681392501812041282804272240296713357286534780333216134004281864;
    b138  = -0.03596314318487727365094853269870982256844289210631058204493077377818835633949;
    b139  = -0.04902670091896965612083428201101205091198148478269054303875029017005640576667;
    b1310 = +0.01388537673498896790224810676002758506392838309962714417674884945696557987538;
    b1311 = -0.02329475109524907760636494276820464664967810607545806961371703044079964798003;
    b1312 = +0.00411071472206196739547606137318514638902786727009559929394539277815884070579;
    b141  = +1.28995927881082034516023610876593719136951515200841564716720963602983766539309;
    b142  = +0.13503265202676397192358271319486347590315093450300003507414087911915346743493;
    b143  = -1.57533735338626298015011591165511659112640458688783385930436332029909777715227;
    b144  = -1.13614144125767380945133294734170682018114470434355261134291967031467884444496;
    b145  = -2.60629976327340284860734294234508518773200013764798796731373995419345584010989;
    b146  = -0.14225514857221282929776869245957057127882456911938845124276329363250774479820;
    b147  = -1.30050279397530175137190231972314623324238865480815606289913301764254981931424;
    b148  = +2.70797163114645615543070920761013389431867941711011998785845670786144007733081;
    b149  = +2.43554296946750176014394493560037156798756615586273835416220476865949138763199;
    b1410 = -0.26458182516446999987007272710827096690076304619434648405874330108389046658365;
    b1411 = +0.28431747275350668428305940521170347967298804085514279367418757904848425107603;
    b1412 = -0.01567336501605444936187349334932741547111318613954509638698908027760861917463;
    b1413 = +0.60355253162364202926624736416406789911182794847681800316234125803391629274555;
    b151  = -0.82170145239485721248182614307298014363501501247665665859542963907661741852252;
    b152  = +1.25591221619821840505806060191670198289896021578755350597122093077684012752648;
    b153  = -0.01615284116525821921698589364201132298146948312403205059324494512949466638820;
    b154  = -0.02297038987201048533939731478067813642773684981394747854111883904855646952919;
    b155  = -0.02796034376707111482574969609615809805957457730932703561247060660045527963337;
    b156  = -0.75094298727080692288567852962945843784728392190426468013732662611274129611030;
    b157  = -0.00831029089278008603544533798969572897337060350821384521268253868513966100453;
    b158  = +0.02785523862077057819395231768788127899618354611871665730619924470585495216854;
    b159  = +0.04021421539333651503730432703988977024358721548426886753518601253708363296062;
    b1510 = +1.61713646360095425893804116572314412328254887955289755326352595104935489454125;
    b1511 = -1.39628377741243214317623603663032161218084680028714877542197062209713837494256;
    b1512 = -0.01424439159061064208304848726946155039179802539115130281665541763757472383980;
    b1513 = +0.75709054336643265169970374917936196757291644158102449813751290304179069353124;
    b1514 = -0.22405735763057330478532402187136037006601226103429496673285661641467238072308;
    b161  = +0.27726401853185531071095250844208662954769209843860654782270439586107555912462;
    b162  = +0.09453818370728229619312145290868518019954577268055790438905422269619151226603;
    b163  = +0.84172754295454286541980788975065897179825972249436132684274103676922879908590;
    b164  = -0.90665259832844475943298457713954683980649043330939335614048943465366310987303;
    b165  = -0.09334858033923263720339222938292231543826754868386677793091178055855225779084;
    b166  = +4.08887758914114010965574538791054120551785765841976165061624483740399180627400;
    b167  = +0.79539986499428990687079492545982314739495812782573436535546220822826278953650;
    b168  = -0.04859175145875636251218702854395529418032114353274620641007859711365783956136;
    b169  = +0.14819851457498430887143064575970769402240035004018486609649448109702272035773;
    b1610 = -1.77021148062189597056755123808303838576982978034562033343027660487016998355672;
    b1611 = +1.83821084492008174732030666893446362369060575712213328902105414298283276556307;
    b1612 = +0.04909344463430916722179909282844531508454281205967923461622573420987424382584;
    b1613 = -3.80378823067007538071243467410680278604792596795503781745130930797860484235966;
    b1614 = +0.33157239872339854149205035234217673029656296754907585248267975563293121409049;
    b1615 = -0.84228976076347914332745917708032287630959039280343054587959508970676337698255;
    // abscissae = row sums (computed from b values)
    a2 = b21;
    a3 = b31+b32; a4 = b41+b42+b43; a5 = b51+b52+b53+b54;
    a6 = b61+b62+b63+b64+b65; a7 = b71+b72+b73+b74+b75+b76;
    a8 = b81+b82+b83+b84+b85+b86+b87;
    a9 = b91+b92+b93+b94+b95+b96+b97+b98;
    a10 = b101+b102+b103+b104+b105+b106+b107+b108+b109;
    a11 = b111+b112+b113+b114+b115+b116+b117+b118+b119+b1110;
    a12 = b121+b122+b123+b124+b125+b126+b127+b128+b129+b1210+b1211;
    a13 = b131+b132+b133+b134+b135+b136+b137+b138+b139+b1310+b1311+b1312;
    a14 = b141+b142+b143+b144+b145+b146+b147+b148+b149+b1410+b1411+b1412+b1413;
    a15 = b151+b152+b153+b154+b155+b156+b157+b158+b159+b1510+b1511+b1512+b1513+b1514;
    a16 = b161+b162+b163+b164+b165+b166+b167+b168+b169+b1610+b1611+b1612+b1613+b1614+b1615;
    c1 = +0.03181927458023409759419419944088926839595967458804889313841865851348230504492;
    c3 = +0.04681369289018421954398607025172424191865836856120072277976273594277845007872;
    c6 = +1.37553536170545749013126339747598151094852489823041688723220548486290269295630;
    c7 = +0.17506567143964248943590740397548086595186840317795589108615338820052040464001;
    c8 = +0.14924798653008455234489054168544610975862154045444166485218499565185924479229;
    c9 = +0.27180992312662372142355988162582830282467844949919584507610421296773792690656;
    c10 = -0.17077940511361075387191294440611902706762757201779959741914461935949309724569;
    c11 = +0.30035519768848521819453255694344499143765502380409939822945376435190034136303;
    c12 = -0.01014254403202798636405231311418830930667910487160410011307114999518762983823;
    c13 = -1.19177815782093190956613115811061881440838744821819017576359936663422556606341;
    c14 = +0.03639139354170713653095246097302347593652682408895579802073390992985525665017;
    c15 = -0.04683316086510645556720041443823709643011325850704077997225219431585033242635;
    c16 = +0.03249476632925818017001031769734448004031420121031955285305017988372000314167;
}

void OdeRkz10::step_(double dt) {
    ode_fun_(sol_, k_[0]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b21*k_[0][i]);
    ode_fun_(soltemp_, k_[1]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b31*k_[0][i] + b32*k_[1][i]);
    ode_fun_(soltemp_, k_[2]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b41*k_[0][i] + b42*k_[1][i] + b43*k_[2][i]);
    ode_fun_(soltemp_, k_[3]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b51*k_[0][i] + b52*k_[1][i] + b53*k_[2][i] + b54*k_[3][i]);
    ode_fun_(soltemp_, k_[4]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b61*k_[0][i] + b62*k_[1][i] + b63*k_[2][i] + b64*k_[3][i] + b65*k_[4][i]);
    ode_fun_(soltemp_, k_[5]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b71*k_[0][i] + b72*k_[1][i] + b73*k_[2][i] + b74*k_[3][i] + b75*k_[4][i] + b76*k_[5][i]);
    ode_fun_(soltemp_, k_[6]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b81*k_[0][i] + b82*k_[1][i] + b83*k_[2][i] + b84*k_[3][i] + b85*k_[4][i] + b86*k_[5][i] + b87*k_[6][i]);
    ode_fun_(soltemp_, k_[7]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b91*k_[0][i] + b92*k_[1][i] + b93*k_[2][i] + b94*k_[3][i] + b95*k_[4][i] + b96*k_[5][i] + b97*k_[6][i] + b98*k_[7][i]);
    ode_fun_(soltemp_, k_[8]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b101*k_[0][i] + b102*k_[1][i] + b103*k_[2][i] + b104*k_[3][i] + b105*k_[4][i] + b106*k_[5][i] + b107*k_[6][i] + b108*k_[7][i] + b109*k_[8][i]);
    ode_fun_(soltemp_, k_[9]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b111*k_[0][i] + b112*k_[1][i] + b113*k_[2][i] + b114*k_[3][i] + b115*k_[4][i] + b116*k_[5][i] + b117*k_[6][i] + b118*k_[7][i] + b119*k_[8][i] + b1110*k_[9][i]);
    ode_fun_(soltemp_, k_[10]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b121*k_[0][i] + b122*k_[1][i] + b123*k_[2][i] + b124*k_[3][i] + b125*k_[4][i] + b126*k_[5][i] + b127*k_[6][i] + b128*k_[7][i] + b129*k_[8][i] + b1210*k_[9][i] + b1211*k_[10][i]);
    ode_fun_(soltemp_, k_[11]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b131*k_[0][i] + b132*k_[1][i] + b133*k_[2][i] + b134*k_[3][i] + b135*k_[4][i] + b136*k_[5][i] + b137*k_[6][i] + b138*k_[7][i] + b139*k_[8][i] + b1310*k_[9][i] + b1311*k_[10][i] + b1312*k_[11][i]);
    ode_fun_(soltemp_, k_[12]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b141*k_[0][i] + b142*k_[1][i] + b143*k_[2][i] + b144*k_[3][i] + b145*k_[4][i] + b146*k_[5][i] + b147*k_[6][i] + b148*k_[7][i] + b149*k_[8][i] + b1410*k_[9][i] + b1411*k_[10][i] + b1412*k_[11][i] + b1413*k_[12][i]);
    ode_fun_(soltemp_, k_[13]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b151*k_[0][i] + b152*k_[1][i] + b153*k_[2][i] + b154*k_[3][i] + b155*k_[4][i] + b156*k_[5][i] + b157*k_[6][i] + b158*k_[7][i] + b159*k_[8][i] + b1510*k_[9][i] + b1511*k_[10][i] + b1512*k_[11][i] + b1513*k_[12][i] + b1514*k_[13][i]);
    ode_fun_(soltemp_, k_[14]);
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (b161*k_[0][i] + b162*k_[1][i] + b163*k_[2][i] + b164*k_[3][i] + b165*k_[4][i] + b166*k_[5][i] + b167*k_[6][i] + b168*k_[7][i] + b169*k_[8][i] + b1610*k_[9][i] + b1611*k_[10][i] + b1612*k_[11][i] + b1613*k_[12][i] + b1614*k_[13][i] + b1615*k_[14][i]);
    ode_fun_(soltemp_, k_[15]);
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * (c1*k_[0][i] + c3*k_[2][i] + c6*k_[5][i] + c7*k_[6][i] + c8*k_[7][i] + c9*k_[8][i] + c10*k_[9][i] + c11*k_[10][i] + c12*k_[11][i] + c13*k_[12][i] + c14*k_[13][i] + c15*k_[14][i] + c16*k_[15][i]);
}

// ── Macro for 17-stage step bodies (rko10 and rkh10 share same sparsity) ──────
#define STEP17_BODY \
    unsigned long _i; \
    ode_fun_(sol_, k_[0]); \
    for (_i=0;_i<neq_;_i++) soltemp_[_i]=sol_[_i]+dt*(b21*k_[0][_i]); \
    ode_fun_(soltemp_, k_[1]); \
    for (_i=0;_i<neq_;_i++) soltemp_[_i]=sol_[_i]+dt*(b31*k_[0][_i]+b32*k_[1][_i]); \
    ode_fun_(soltemp_, k_[2]); \
    for (_i=0;_i<neq_;_i++) soltemp_[_i]=sol_[_i]+dt*(b41*k_[0][_i]+b43*k_[2][_i]); \
    ode_fun_(soltemp_, k_[3]); \
    for (_i=0;_i<neq_;_i++) soltemp_[_i]=sol_[_i]+dt*(b51*k_[0][_i]+b53*k_[2][_i]+b54*k_[3][_i]); \
    ode_fun_(soltemp_, k_[4]); \
    for (_i=0;_i<neq_;_i++) soltemp_[_i]=sol_[_i]+dt*(b61*k_[0][_i]+b64*k_[3][_i]+b65*k_[4][_i]); \
    ode_fun_(soltemp_, k_[5]); \
    for (_i=0;_i<neq_;_i++) soltemp_[_i]=sol_[_i]+dt*(b71*k_[0][_i]+b74*k_[3][_i]+b75*k_[4][_i]+b76*k_[5][_i]); \
    ode_fun_(soltemp_, k_[6]); \
    for (_i=0;_i<neq_;_i++) soltemp_[_i]=sol_[_i]+dt*(b81*k_[0][_i]+b85*k_[4][_i]+b86*k_[5][_i]+b87*k_[6][_i]); \
    ode_fun_(soltemp_, k_[7]); \
    for (_i=0;_i<neq_;_i++) soltemp_[_i]=sol_[_i]+dt*(b91*k_[0][_i]+b96*k_[5][_i]+b97*k_[6][_i]+b98*k_[7][_i]); \
    ode_fun_(soltemp_, k_[8]); \
    for (_i=0;_i<neq_;_i++) soltemp_[_i]=sol_[_i]+dt*(b101*k_[0][_i]+b106*k_[5][_i]+b107*k_[6][_i]+b108*k_[7][_i]+b109*k_[8][_i]); \
    ode_fun_(soltemp_, k_[9]); \
    for (_i=0;_i<neq_;_i++) soltemp_[_i]=sol_[_i]+dt*(b111*k_[0][_i]+b116*k_[5][_i]+b117*k_[6][_i]+b118*k_[7][_i]+b119*k_[8][_i]+b1110*k_[9][_i]); \
    ode_fun_(soltemp_, k_[10]); \
    for (_i=0;_i<neq_;_i++) soltemp_[_i]=sol_[_i]+dt*(b121*k_[0][_i]+b126*k_[5][_i]+b127*k_[6][_i]+b128*k_[7][_i]+b129*k_[8][_i]+b1210*k_[9][_i]+b1211*k_[10][_i]); \
    ode_fun_(soltemp_, k_[11]); \
    for (_i=0;_i<neq_;_i++) soltemp_[_i]=sol_[_i]+dt*(b131*k_[0][_i]+b134*k_[3][_i]+b135*k_[4][_i]+b136*k_[5][_i]+b137*k_[6][_i]+b138*k_[7][_i]+b139*k_[8][_i]+b1310*k_[9][_i]+b1311*k_[10][_i]+b1312*k_[11][_i]); \
    ode_fun_(soltemp_, k_[12]); \
    for (_i=0;_i<neq_;_i++) soltemp_[_i]=sol_[_i]+dt*(b141*k_[0][_i]+b144*k_[3][_i]+b145*k_[4][_i]+b146*k_[5][_i]+b147*k_[6][_i]+b148*k_[7][_i]+b149*k_[8][_i]+b1410*k_[9][_i]+b1411*k_[10][_i]+b1412*k_[11][_i]+b1413*k_[12][_i]); \
    ode_fun_(soltemp_, k_[13]); \
    for (_i=0;_i<neq_;_i++) soltemp_[_i]=sol_[_i]+dt*(b151*k_[0][_i]+b152*k_[1][_i]+b156*k_[5][_i]+b157*k_[6][_i]+b1513*k_[12][_i]+b1514*k_[13][_i]); \
    ode_fun_(soltemp_, k_[14]); \
    for (_i=0;_i<neq_;_i++) soltemp_[_i]=sol_[_i]+dt*(b161*k_[0][_i]+b163*k_[2][_i]+b1615*k_[14][_i]); \
    ode_fun_(soltemp_, k_[15]); \
    for (_i=0;_i<neq_;_i++) soltemp_[_i]=sol_[_i]+dt*(b171*k_[0][_i]+b172*k_[1][_i]+b173*k_[2][_i]+b176*k_[5][_i]+b177*k_[6][_i]+b178*k_[7][_i]+b179*k_[8][_i]+b1710*k_[9][_i]+b1711*k_[10][_i]+b1712*k_[11][_i]+b1713*k_[12][_i]+b1714*k_[13][_i]+b1715*k_[14][_i]+b1716*k_[15][_i]); \
    ode_fun_(soltemp_, k_[16]); \
    for (_i=0;_i<neq_;_i++) \
        sol_[_i] += dt*(c1*k_[0][_i]+c2*k_[1][_i]+c3*k_[2][_i]+c6*k_[5][_i]+c7*k_[6][_i]+c9*k_[8][_i]+c10*k_[9][_i]+c11*k_[10][_i]+c12*k_[11][_i]+c13*k_[12][_i]+c14*k_[13][_i]+c15*k_[14][_i]+c16*k_[15][_i]+c17*k_[16][_i]);
    unsigned long i;
// ── OdeRko10 ──────────────────────────────────────────────────────────────────
OdeRko10::OdeRko10(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 17), OdeERK(neq)
{
    method_ = "Rko10";
    a2 = .3357505083417036184129939488963472892525539577157696170900805997207182929518116563365;
    a3 = .5263563553500217821118595221339767434067222948292617539626688113669112965383197614021;
    a4 = .7895345330250326731677892832009651151100834422438926309440032170503669448074796421031;
    a5 = .1852155685265047076970656199437875208987680209449849941608024849050604421517806064228;
    a6 = .2895345330250326731677892832009651151100834422438926309440032170503669448074796421031;
    a7 = .7659879027055932401898717206953675811356898851157703878511505293452324618973555436361;
    a8 = .1080739009578824490100240661758266719014599665988034811870817465871751958605805741080;
    a9 = .3573842417596774518429245029795604640404982636367873040901247917361510345429002009092;
    a10 = .8825276619647323464255014869796690751828678442680521196637911779185276585194132570617;
    a11 = .6426157582403225481570754970204395359595017363632126959098752082638489654570997990908;
    a12 = .1174723380352676535744985130203309248171321557319478803362088220814723414805867429383;
    a13 = .7659879027055932401898717206953675811356898851157703878511505293452324618973555436361;
    a14 = .2895345330250326731677892832009651151100834422438926309440032170503669448074796421031;
    a15 = .5263563553500217821118595221339767434067222948292617539626688113669112965383197614021;
    a16 = .3357505083417036184129939488963472892525539577157696170900805997207182929518116563365;
    b21 = .3357505083417036184129939488963472892525539577157696170900805997207182929518116563365;
    b31 = .1137717040478783056449396184425018992536986324028992632761012508174661576282091865815;
    b32 = .4125846513021434764669199036914748441530236624263624906865675605494451389101105748206;
    b41 = .1973836332562581682919473208002412787775208605609731577360008042625917362018699105258;
    b43 = .5921508997687745048758419624007238363325625816829194732080024127877752086056097315774;
    b51 = .1360001717992528326665261557210531883399922632350163155393510462488093550801937826726;
    b53 = .0824720805202811222379079977574504818037996000363091041777905360271594623631069354284;
    b54 = -.0332566837930292472073685335347161492450238423263404255563390973709083752915201116782;
    b61 = .0654678519948111057975663033586887072416508858302450954893843449284860127842110887182;
    b64 = .0006858715620423033993966826640550281985093888681236135859756994188871385539662876479;
    b65 = .2233808094681792639708262971782213796699231675455239218686431727029937934693022657371;
    b71 = .2379439999135994223360182301765245366877534054337310606780367192080205084533052094979;
    b74 = .1285793626493546102687479560301629068933431333159680850577212924587597635005521433930;
    b75 = -.7303763776380165799305971651206183012312272113455920027358381669467410091843571268521;
    b76 = 1.129840917780655787515702699609298438785820557711663244851230684625193199127855317597;
    b81 = .0606278089644172041828851763289449356101383929213871935662188828124574009433854416578;
    b85 = .0788822224869202238639802956157355708195673413893998012378730299018568351728356274718;
    b86 = -.0321321350412593900510162678207448391177877251765465842094683934155351160999215185782;
    b87 = .0006960045478044110141748620518910045895419574645630705924582272883960758442810235566;
    b91 = .0310441586543872188908409753881999593035847986835220089359164492112482866700568653692;
    b96 = .1571656120644442171909172634188670081467914525944196307657031602142991040495820182979;
    b97 = .0001117638541384084302989687338271505642661663918417037071041752801553969715970722156;
    b98 = .1690627071867076073308672954386663460258558459670039606814010070304482468516642450265;
    b101 = .0143061420142267773294341145620270598158292988407553851922433281175939125604562870960;
    b106 = -.3914725335338579386439799990026912235591541327277623987271673700062264924050571982953;
    b107 = .2848400967322984698670378405298733357388045123736554424201702352859211869408079104816;
    b108 = .2559426777170804941167590501634194225203620020234561863154326261989141389268124787652;
    b109 = .7189112790349845437562504807270404806670261637579475044631123583223249124963937790142;
    b111 = .0100666845742663178817227082262022726090612102393137172374184106085273098952254964165;
    b116 = -.3683271809355001194608455001594963737510628435069995678571139196388186857259494651043;
    b117 = .0857550262402229990354693960925451370506815971267273889165067994976749715636339652327;
    b118 = .2633792404483482304677774849110341109669789276529274236259927084605103685123897043694;
    b119 = .6783120744401823352568978305425238981627472606453781308422134412090277276134222894570;
    b1110 = -.0265700865271972150239464225923695090789044157941343968551422318730727264016221912805;
    b121 = .0421257301570145802287543187145341282749104461888283907786317114955424946915941038413;
    b126 = -.0114073025739498662832358741501332045332754077505475889494504614085885658522072079316;
    b127 = -.0035773235838810416426398549103908438384770947522487791259947719745071791077708166736;
    b128 = .0847808506904718159588379566933344327419896519600476705478313620344711827543672159469;
    b129 = -.0000823689740384519053451672363846415534369879668146873991026059653843017075131871518;
    b1210 = .0007920174936649160445251535005941104644271462692404428580786438040911208546142377593;
    b1211 = .0048407348259857011736019804087769432609944017834424316262149407964171311570643418676;
    b131 = .1866168584194642618748985099397434698267694490527249444362505121514680479505442836574;
    b134 = .1285793626493546102687479560301629068933431333159680850577212924587597635005521433930;
    b135 = -.7303763776380165799305971651206183012312272113455920027358381669467410091843571268521;
    b136 = .3365620746651354218129539461265307620933082614096471567813938850533189571406833598679;
    b137 = .3439152195696137654283681672732838006806635738118480999635325742827372086353048385257;
    b138 = .6116809614283795957124860804122425718138169583157463948158033894717678649195939132136;
    b139 = .8555642651045021202243366091904917631763787495189342130253621820535903092162656518481;
    b1310 = -.0891392852486696004845640155766881857316512047949986128540746584443895055096019167947;
    b1311 = -.4263317531098201582091498458499961586420389075833366648914840378325891404712881232497;
    b1312 = -.4510834231343501965076085217297850477436729165851712257475164429026900343003414799732;
    b141 = .0813272217458117794807280979168314150245787313612597895474152882582268286833402023137;
    b144 = .0006858715620423033993966826640550281985093888681236135859756994188871385539662876479;
    b145 = .2233808094681792639708262971782213796699231675455239218686431727029937934693022657371;
    b146 = -.3543152669595614715438074770044728248424634682417120916726014150358797064660842419218;
    b147 = -.1614228337471357340842753214971350545154909424968569413717974406833347832344548170007;
    b148 = -1.267989518319081979614362221100086229540210555088918164010191288219362963604567702803;
    b149 = .2482505660965057784745955362600844051763121832196360142449926255422280563592831868088;
    b1410 = .0018940150722043298623043050882674956645435583508483109701033317083696286291966019611;
    b1411 = -.0236710850425040469729927587547751462365353620277346625237201541217354742435686516188;
    b1412 = 1.376373544047006195976245547649688122109906103559164922810806152088122419417100442009;
    b1413 = .1650212091015662542191305948002865244010106371945579174943772453918520072439660689694;
    b151 = .1137717040478783056449396184425018992536986324028992632761012508174661576282091865815;
    b152 = .4125846513021434764669199036914748441530236624263624906865675605494451389101105748206;
    b156 = -.5266141894222268880524004361760397955773267936583064281992495328485812564705031059898;
    b157 = .4270299995350464166415042303891280056028480962227556089080302280248036279775533169729;
    b1513 = -.4270299995350464166415042303891280056028480962227556089080302280248036279775533169729;
    b1514 = .5266141894222268880524004361760397955773267936583064281992495328485812564705031059898;
    b161 = .3357505083417036184129939488963472892525539577157696170900805997207182929518116563365;
    b163 = -.4368367083891407610190158064846822677554138099678168504498524386288907219928146135373;
    b1615 = .4368367083891407610190158064846822677554138099678168504498524386288907219928146135373;
    b171 = .0352840091453906575769537425002052698379204209477870663357404857513859839544537545996;
    b172 = -.4368588562339554617519938616027395568153523718226810275461527253101515442644071737416;
    b173 = -.5185253025751911700815354370658368214653404020923461413204927007907407344767758710502;
    b176 = .0835388214631834770882397990787144429369302489753300898028685113495020955686828548200;
    b177 = .3357324883823615793514438806416832609180686549969009829861766772233990660034579152038;
    b178 = -.1180346853290197678148440843338719378456952414935754110833043790640005049977678056530;
    b179 = -.2028121524999718341813208304021045923675061150408674489110338814798518734278169865166;
    b1710 = .4018734442526045674154192467995252408036818247069631361197899796078897150680466096292;
    b1711 = .6982808681238487516481594542254805552510541794271997632640921307356093400075120505480;
    b1712 = .2745953340127460413630142920026546525788337913278252157685398122819348957290706395825;
    b1713 = -.8071051158813288531951186779278104056732084407804403857002131168492985419909905100904;
    b1714 = .2986469883301853807480531774155235135599206769328769914173437804434298240853514778770;
    b1715 = .5185253025751911700815354370658368214653404020923461413204927007907407344767758710502;
    b1716 = .4368588562339554617519938616027395568153523718226810275461527253101515442644071737416;
    c1  = .0333333333333333333333333333333333333333333333333333333333333333333333333333333333333;
    c2  = -.0219224283305227655986509274873524451939291736930860033726812816188870151770657672850;
    c3  = -.0567107750472589792060491493383742911153119092627599243856332703213610586011342155010;
    c6  = -.0560471976401179941002949852507374631268436578171091445427728613569321533923303834808;
    c7  = .1789297658862876254180602006688963210702341137123745819397993311036789297658862876254;
    c9  = .2774291885177431765083602625606543404285043197180408363394722409866844803871713937960;
    c10 = .1892374781489234901583064041060123262381623469486258303271944256799821862794952728707;
    c11 = .2774291885177431765083602625606543404285043197180408363394722409866844803871713937960;
    c12 = .1892374781489234901583064041060123262381623469486258303271944256799821862794952728707;
    c13 = -.1789297658862876254180602006688963210702341137123745819397993311036789297658862876254;
    c14 = .0560471976401179941002949852507374631268436578171091445427728613569321533923303834808;
    c15 = .0567107750472589792060491493383742911153119092627599243856332703213610586011342155010;
    c16 = .0219224283305227655986509274873524451939291736930860033726812816188870151770657672850;
    c17 = .0333333333333333333333333333333333333333333333333333333333333333333333333333333333333;
}

void OdeRko10::step_(double dt) { STEP17_BODY }

// ── OdeRkh10 ──────────────────────────────────────────────────────────────────
OdeRkh10::OdeRkh10(unsigned long neq)
    : OdeAdaptive(neq, false), OdeRK(neq, 17), OdeERK(neq)
{
    method_ = "Rkh10";
    a2 = .5233584004620047139632937023215170497515953383496781610502942213792083195573343614542;
    a3 = .5265091001416125727329516734775408259523008085442588621240322448552569517961160776999;
    a4 = .7897636502124188590994275102163112389284512128163882931860483672828854276941741165498;
    a5 = .3939235701256720143227738119996521367488350094732736567444747389961242969755195414769;
    a6 = .7666539862535505911932668693560686601417881683220066628197494388337786524595845606945;
    a7 = .2897636502124188590994275102163112389284512128163882931860483672828854276941741165498;
    a8 = .1084776892195672933536461100396272126536495082872530661076180009074872219675135232227;
    a9 = .3573842417596774518429245029795604640404982636367873040901247917361510345429002009092;
    a10 = .8825276619647323464255014869796690751828678442680521196637911779185276585194132570617;
    a11 = .6426157582403225481570754970204395359595017363632126959098752082638489654570997990908;
    a12 = .1174723380352676535744985130203309248171321557319478803362088220814723414805867429383;
    a13 = .7666539862535505911932668693560686601417881683220066628197494388337786524595845606945;
    a14 = .2897636502124188590994275102163112389284512128163882931860483672828854276941741165498;
    a15 = .5265091001416125727329516734775408259523008085442588621240322448552569517961160776999;
    a16 = .5233584004620047139632937023215170497515953383496781610502942213792083195573343614542;
    b21 = .5233584004620047139632937023215170497515953383496781610502942213792083195573343614542;
    b31 = .2616697163778127283312402097548997641973627614039327706102102491953717704187699219879;
    b32 = .2648393837637998444017114637226410617549380471403260915138219956598851813773461557120;
    b41 = .1974409125531047147748568775540778097321128032040970732965120918207213569235435291375;
    b43 = .5923227376593141443245706326622334291963384096122912198895362754621640707706305874124;
    b51 = .1973205486287023067036649485978952117578825584337199991656132317825743833021058860612;
    b53 = .2950833340926721918228255598274359230145509784596048092593866299668586683096812321792;
    b54 = -.0984803125957024842037166964256789980235985274200511516805251227533087546362675767635;
    b61 = .1313134173444616536130177999345909470542768366990469474228776563495603985387834598888;
    b64 = .1101544395386396206773696967716892932905881833462590372574439152868807925960672597269;
    b65 = .5251861293704493169028793726497884197969231482767006781394278671973374613247338410788;
    b71 = .1342003418463226002727476951680931444018781918996634475042938727232827990267510411664;
    b74 = .6960887032881160802299824047678314828416281106463280160258778625630871045892118300249;
    b75 = .2504977215703398097125518092509800218006823479445679226772611578145189247821334145350;
    b76 = -.7910231164923596311158543989705934101157374376741710930213845258180034007039221691766;
    b81 = .0722182741896626194200508184551704977047411771786043553816725961719388531778723978952;
    b85 = -.0583363229364561071638060613893065187416111593185084019437153089695318415302860382616;
    b86 = .0030475576685745252201749500702940360925190825522870157830499824678571316292849362329;
    b87 = .0915481802977862558772264029034691975980004078748700968866107312372230786906422273562;
    b91 = .0312550081351661794705012052826347661215092881180084429562242445312997971185711894214;
    b96 = .0001091238215424128929294834955207716494619546474783251185719596191189550956073995219;
    b97 = .1567257586309938356246107465648794708917441337700138495832023584660674808897051732313;
    b98 = .1692943511719750238548830676365254553777828871012866864321262291196648014390164387346;
    b101 = .0119066044146686192421688425808092509950954606115616378176147857033813431604361732217;
    b106 = .2834370820246027860255992266982188665428702252125274101591962479489267620092644600260;
    b107 = -.4163121675706282353724276181356300212164192944619403444080980777942870933794472685216;
    b108 = .2646463339497663668210902091085361027053564926326924812994390366777834273576276339310;
    b109 = .7388498091463228097090708267277348761559649602732109347956391853827232193715322584046;
    b111 = .0234065736913319789147083837798400784250394685775684541636233986599966237705791696029;
    b116 = .0944931301894936540130025309560561432498251662377734609644880062513095401829593904774;
    b117 = -.2728720559019952606363092580665963250433705067252372208829562550632597611313198545757;
    b118 = .2240220461156057997944315522518131846261124699330640294272458923970695162204120486767;
    b119 = .6043814410751657569719347222576085340011863610739072982875214128849988434755301302413;
    b1110 = -.0308153769292793809006924341582820792992912227338633260500472468662657970610610853317;
    b121 = .0454437753101761631576538990815309649864589094199196117783663313790235366676063368709;
    b126 = -.0011879966718640285867652542192853563433762859901763864748911509292456603557421308602;
    b127 = .0120356549909226109796618821723436205851544669504769411623189591929644148009012557560;
    b128 = .0751269029876496682162752137156557214027531550065624051350452811259815018139344504867;
    b129 = -.0182209240988801240314118610597483889276157985919218207469682836908895976741907756718;
    b1210 = -.0002571528540841043468806376221771396205460354181513400383106090430345956162981031278;
    b1211 = .0045320783713474681859652709520115027343037443552384695206482940466727418443757094845;
    b131 = .1767137782592772030958798765711993346076326211800572275450227165783753236705910865492;
    b134 = .1101544395386396206773696967716892932905881833462590372574439152868807925960672597269;
    b135 = .5251861293704493169028793726497884197969231482767006781394278671973374613247338410788;
    b136 = -.4716207672801957948798217912152359376250630852495511063738116933651587031904328351457;
    b137 = .8990310498491875266368990071875152922763468480002185650326986125011485318362907529907;
    b138 = -.7467230306916289638599602008088168117750310724922743198498253813592425510843163068237;
    b139 = -1.017101516756146040853186972006065972987027196800421553809421717321497529906933631477;
    b1310 = .1263508715195988962951307827687648346421985369266969430473204298972536422365713122404;
    b1311 = .5660138272355064270682732249907470012763799581315503842554078250210353407723389384909;
    b1312 = .5986492052088624001098038724464832066388402270027708075754868643976463442046741430643;
    b141 = .1277534947480869822694777006880571541639616513225826576695303067404023367054772185702;
    b144 = .6960887032881160802299824047678314828416281106463280160258778625630871045892118300249;
    b145 = .2504977215703398097125518092509800218006823479445679226772611578145189247821334145350;
    b146 = -.7368246436028416867609246757454535374296880219263938462439002090823944915566264811824;
    b147 = -.2778578777108241826773273374900723250222301109862216853553157201018147214465526588169;
    b148 = -.5997526313598403501296884799197753021563938240370770948150479630779446286262003432092;
    b149 = .2024692338910704693500237585621903123505161701229471467587157451308903694383321235511;
    b1410 = .0054320369823638497806006846526344436014681899696786667750467188132249898834161048714;
    b1411 = -.0107447247415504792010120691989438133712544466427220502431479893641873325892076956337;
    b1412 = .6951688484570234004700591858164146072357628221597117426434839740273190245052113679250;
    b1413 = -.0624665113095250339443154711675518050860016757570131827064555161802161479910207640856;
    b151 = .2616697163778127283312402097548997641973627614039327706102102491953717704187699219879;
    b152 = .2648393837637998444017114637226410617549380471403260915138219956598851813773461557120;
    b156 = -.1998011270205324791079663580830885049848273745422651189682301346802905866051733476638;
    b157 = -.6510499873052827124921914489683813643155863882516440645794556633240216912803403931627;
    b1513 = .1998011270205324791079663580830885049848273745422651189682301346802905866051733476638;
    b1514 = .6510499873052827124921914489683813643155863882516440645794556633240216912803403931627;
    b161 = .5233584004620047139632937023215170497515953383496781610502942213792083195573343614542;
    b163 = -.5558812136754302060726143105309293455559184141943321053532734480099926250948077261183;
    b1615 = .5558812136754302060726143105309293455559184141943321053532734480099926250948077261183;
    b171 = .0573207954320655910311426170510398365649521650486746231028599442807856804316065443980;
    b172 = -.5499710763899945608115841896290187887481592249811405834035066676393750158953834290913;
    b173 = -.6499374174008749135116607420010890619711618624173024222960650740195874521599402439688;
    b176 = -1.061667370401756207240019539023157074172524666307437022389776456477183230723296269940;
    b177 = -.0404015668980635829426968223421218330826256202391248636522064257787040249155571106248;
    b178 = -.1828302366407607254710272774065261039379052622607190097473388370699414811305446343873;
    b179 = -.3336592706492786845666575661828162687906558601961826440714525336287466822150370633233;
    b1710 = .3956485423760567568801345107166015519577734440834727480004748180136901286634710478955;
    b1711 = .6950570494599735891002099282005158129027126868215679095299345058137097320818106877162;
    b1712 = .2714873764573748588377263058539220945263829691804714618529052530298982146739754552950;
    b1713 = .6071810560414041202873774349794680164722661545496003750296400378855628528787164400954;
    b1714 = .5918636248229842840838104081530739675596239893196764223449596939309288102548549028752;
    b1715 = .6499374174008749135116607420010890619711618624173024222960650740195874521599402439688;
    b1716 = .5499710763899945608115841896290187887481592249811405834035066676393750158953834290913;
    c1 = .0333333333333333333333333333333333333333333333333333333333333333333333333333333333333;
    c2 = -.0384615384615384615384615384615384615384615384615384615384615384615384615384615384615;
    c3 = -.0909090909090909090909090909090909090909090909090909090909090909090909090909090909091;
    c6 = -.1348314606741573033707865168539325842696629213483146067415730337078651685393258426966;
    c7 = -.1111111111111111111111111111111111111111111111111111111111111111111111111111111111111;
    c9 = .2774291885177431765083602625606543404285043197180408363394722409866844803871713937960;
    c10 = .1892374781489234901583064041060123262381623469486258303271944256799821862794952728707;
    c11 = .2774291885177431765083602625606543404285043197180408363394722409866844803871713937960;
    c12 = .1892374781489234901583064041060123262381623469486258303271944256799821862794952728707;
    c13 = .1348314606741573033707865168539325842696629213483146067415730337078651685393258426966;
    c14 = .1111111111111111111111111111111111111111111111111111111111111111111111111111111111111;
    c15 = .0909090909090909090909090909090909090909090909090909090909090909090909090909090909091;
    c16 = .0384615384615384615384615384615384615384615384615384615384615384615384615384615384615;
    c17 = .0333333333333333333333333333333333333333333333333333333333333333333333333333333333333;
}

void OdeRkh10::step_(double dt) { STEP17_BODY }

#undef STEP17_BODY

} // namespace ode


