#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define STRICT_R_HEADERS
// Implementations of libode base classes needed by OdeTrapz, OdeSsp3, and OdeRKF32.
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

} // namespace ode
