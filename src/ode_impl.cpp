#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define STRICT_R_HEADERS
// Implementations of libode base classes needed by OdeTrapz, OdeSsp3, OdeRKF32, OdeRK43, OdeDoPri54, OdeVern65, OdeVern76, and OdeDoPri87.
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

namespace ode {

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
// Fehlberg's 3(2) pair.
// Primary (3rd-order): b1=1/6, b2=2/3, b3=1/6 (b3 = 1-b1-b2, not stored).
// Embedded (2nd-order): d1=1/2, d2=0, d3=1/2.

OdeRKF32::OdeRKF32 (unsigned long neq)
    : OdeEmbedded(neq, false, 2),
      OdeRK(neq, 3),
      OdeERK(neq)
{
    method_ = "RKF32";
    c2  = 0.5;    a21 = 0.5;
    c3  = 1.0;    a31 = -1.0;  a32 = 2.0;
    b1  = 1.0/6;  b2  = 2.0/3;
    d1  = 0.5;    d2  = 0.0;   d3  = 0.5;
}

void OdeRKF32::step_ (double dt) {
    // Stage 1: k1 = f(t, y)
    ode_fun_(sol_, k_[0]);

    // Stage 2: y2 = y + dt*a21*k1
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * a21 * k_[0][i];
    ode_fun_(soltemp_, k_[1]);

    // Stage 3: y3 = y + dt*(a31*k1 + a32*k2)
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (a31 * k_[0][i] + a32 * k_[1][i]);
    ode_fun_(soltemp_, k_[2]);

    // 2nd-order embedded (must be computed before sol_ is overwritten)
    for (unsigned long i = 0; i < neq_; i++)
        solemb_[i] = sol_[i] + dt * (d1 * k_[0][i] + d2 * k_[1][i] + d3 * k_[2][i]);

    // 3rd-order primary update (b3 = 1 - b1 - b2)
    double b3 = 1.0 - b1 - b2;
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * (b1 * k_[0][i] + b2 * k_[1][i] + b3 * k_[2][i]);
}

// ── OdeRK43 ───────────────────────────────────────────────────────────────────
// Classical RK4 primary with FSAL 3rd-order embedded pair.
// FSAL: a5j = bj, so k5 = f(t+dt, y_new) = next step's k1.
// Primary (4th): b1=1/6, b2=1/3, b3=1/3, b4=1/6 (classical RK4).
// Embedded (3rd): d1=1/6, d2=7/18, d3=5/18, d4=0 (not stored), d5=1/6.
// All order conditions verified analytically.

OdeRK43::OdeRK43 (unsigned long neq)
    : OdeEmbedded(neq, false, 3),
      OdeRK(neq, 5),
      OdeERK(neq)
{
    method_ = "RK43";
    c2  = 0.5;      a21 = 0.5;
    c3  = 0.5;      a31 = 0.0;       a32 = 0.5;
    c4  = 1.0;      a41 = 0.0;       a42 = 0.0;       a43 = 1.0;
    c5  = 1.0;      a51 = 1.0/6;     a52 = 1.0/3;     a53 = 1.0/3;  a54 = 1.0/6;
    b1  = 1.0/6;    b2  = 1.0/3;     b3  = 1.0/3;     b4  = 1.0/6;
    d1  = 1.0/6;    d2  = 7.0/18;    d3  = 5.0/18;    d5  = 1.0/6;
}

void OdeRK43::step_ (double dt) {
    // Stage 1: k1 = f(t, y)
    ode_fun_(sol_, k_[0]);

    // Stage 2: y2 = y + dt*a21*k1
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * a21 * k_[0][i];
    ode_fun_(soltemp_, k_[1]);

    // Stage 3: y3 = y + dt*(a31*k1 + a32*k2)
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (a31 * k_[0][i] + a32 * k_[1][i]);
    ode_fun_(soltemp_, k_[2]);

    // Stage 4: y4 = y + dt*(a41*k1 + a42*k2 + a43*k3)
    for (unsigned long i = 0; i < neq_; i++)
        soltemp_[i] = sol_[i] + dt * (a41 * k_[0][i] + a42 * k_[1][i] + a43 * k_[2][i]);
    ode_fun_(soltemp_, k_[3]);

    // 4th-order primary update (sol_ becomes y_new before stage 5)
    for (unsigned long i = 0; i < neq_; i++)
        sol_[i] += dt * (b1 * k_[0][i] + b2 * k_[1][i] + b3 * k_[2][i] + b4 * k_[3][i]);

    // Stage 5 (FSAL): k5 = f(t+dt, y_new); a5j = bj
    ode_fun_(sol_, k_[4]);

    // 3rd-order embedded (d4=0, d5=1/6)
    // solemb_ uses the OLD y (= sol_ - dt*update above), so back-compute
    // y_old = sol_ - dt*(b1*k1+b2*k2+b3*k3+b4*k4) and add d-weighted sum.
    // Equivalently: solemb_ = sol_ + dt*((d1-b1)*k1+(d2-b2)*k2+(d3-b3)*k3+(0-b4)*k4+d5*k5)
    for (unsigned long i = 0; i < neq_; i++)
        solemb_[i] = sol_[i] + dt * ((d1 - b1) * k_[0][i] + (d2 - b2) * k_[1][i]
                                    + (d3 - b3) * k_[2][i] + (   - b4) * k_[3][i]
                                    + d5         * k_[4][i]);
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

} // namespace ode
