#ifndef __RXODE2_SOLVE_DIAG_H__
#define __RXODE2_SOLVE_DIAG_H__

#include <Rinternals.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Failed-solve diagnostics (rxode2#1805).  Only a failing subject records
   anything; the records live in a side table keyed by the solve id, so a
   successful solve touches nothing but one atomic read per subject.

   Setup: store the model's state names and drop every record (called from the
   rxSolve() setup, on the main thread).
   Clear: drop one subject's record (start of a subject's solve).
   Dydt: record a right-hand-side evaluation that returned a non-finite
   derivative: the time, the `n` states `y` and derivatives `f`.  Only the
   first one since the subject's solve started is kept.
   Capture: record the subject's last accepted state `yAcc` (at `tAcc`) and,
   when known, the state after the failed step `yFail` (NULL otherwise).
   `named` is 0 when the state layout is not the model's (an nlmixr2est
   neqOverride), so states are labeled by position.  Only the first capture is
   kept.
   Push: push the subject's diagnostics, if any, to the solve-warning
   aggregator as "solve error: ..." lines and drop its record. */
void rxSolveDiagSetup(SEXP stateNames);
void rxSolveDiagClear(int id);
void rxSolveDiagDydt(int id, int n, double t, const double *y, const double *f);
void rxSolveDiagCapture(int id, int n, int named, double tAcc,
                        const double *yAcc, const double *yFail);
void rxSolveDiagPush(int id);

#ifdef __cplusplus
}
#endif

#endif
