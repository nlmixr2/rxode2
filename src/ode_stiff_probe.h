#ifndef ODE_STIFF_PROBE_H_
#define ODE_STIFF_PROBE_H_
// Stiffness probe of an AutoSwitch composite's explicit primary (Hairer &
// Wanner IV.2): after each accepted step, h*|f(yb) - f(ya)|/|yb - ya| of two
// stages that both end at t + h, against the method's real-axis stability
// size times autoSwitchNonstifftol (autoSwitchStifftol once the subject has
// been persistently stiff).  The verdict rules and their per-subject state
// (ind->autoStiffIasti/Nonsti) are dop853's (dop853.c) and the device
// integrators' (rxode2gpu/rxg_stepper.h): 15 verdicts make a stiff report,
// 6 clean ones clear the count.
#include <cmath>

// Thrown from adapt() after a stiff report; the solution is the last
// accepted step's
struct RxStiffStop {};

struct RxStiffProbe {
  rx_solving_options_ind *ind = nullptr; // nullptr: off
  double lim = 0.0;

  void start(rx_solving_options *op, rx_solving_options_ind *i, double stab) {
    ind = i;
    double tol = (i->autoBackoff > 0) ? op->autoSwitchStifftol
                                      : op->autoSwitchNonstifftol;
    lim = stab * ((tol > 0.0) ? tol : 0.9);
  }

  void check(double h, const double *ka, const double *kb, const double *ya,
             const double *yb, unsigned long n) {
    if (ind == nullptr) return;
    double num = 0.0, den = 0.0;
    for (unsigned long i = 0; i < n; i++) {
      double d = kb[i] - ka[i];
      num += d*d;
      d = yb[i] - ya[i];
      den += d*d;
    }
    // a step that moved nothing carries no stiffness signal
    double hl = (den > 0.0) ? std::fabs(h) * std::sqrt(num / den) : 0.0;
    if (hl > lim) {
      ind->autoStiffNonsti = 0;
      if (++ind->autoStiffIasti >= 15) {
        ind->autoStiffIasti = 0;
        ind->autoStiffNonsti = 0;
        throw RxStiffStop();
      }
    } else if (++ind->autoStiffNonsti >= 6) {
      ind->autoStiffIasti = 0;
      ind->autoStiffNonsti = 0;
    }
  }
};

#endif // ODE_STIFF_PROBE_H_
