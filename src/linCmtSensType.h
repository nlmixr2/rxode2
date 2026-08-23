#ifndef __LINCMTSENSTYPE_H__
#define __LINCMTSENSTYPE_H__
#include "linCmtDiffConstant.h"
// Single source of truth for classifying the linCmt() sensitivity (Jacobian)
// method encoded in rx->sensType.  Shared by linCmt.cpp (thetaSens scaling),
// par_solve.cpp (setupLinH step-size skip) and rxData.cpp (thread decision) so
// the classification cannot drift between translation units.

// True for the automatic-differentiation Jacobian methods: forward-mode fvar
// (3/30), reverse-mode (31) and the auto default (100).  The finite-difference
// methods (1,2,4,5,6,7,10,20,40,50) return false.  linCmtB reads ind->linH only
// on the finite-difference methods, so these are exactly the AD methods that do
// not need finite-difference step-size estimation.
static inline int linCmtSensIsAD(int sensType) {
  switch (sensType) {
  case 1: case 2: case 4: case 5: case 6: case 7:
  case 10: case 20: case 40: case 50:
    return 0;
  default:
    return 1;
  }
}

// True for the AD Jacobian methods that can run across threads: 3/30
// (forward-mode fvar, stack-local), 31 (reverse mode -- rxode2 builds with
// -DSTAN_THREADS, so the Stan tape is thread_local and linCmtB() creates a
// worker's tape before its first var, see linCmtRevTapeInit()) and 100 (auto,
// which linCmtSensResolveAuto() turns into one of those two).  The
// finite-difference methods stay excluded: their first-subject
// scaling/step-size setup is shared, not per-thread.
static inline int linCmtSensAdThreadSafe(int sensType) {
  return (sensType == 3 || sensType == 30 || sensType == 31 || sensType == 100);
}

// Number of linCmtB() sensitivity directions a model requests: the bits of
// the parser's ndiff mask that belong to this model's parameters (numSens()
// in linCmt.h counts the same bits).  0 when the model reads no derivative.
static inline int linCmtSensNreq(int ndiff, int ncmt, int oral0) {
  int mask = diffP1 | diffV1;
  if (ncmt >= 2) mask |= diffP2 | diffP3;
  if (ncmt >= 3) mask |= diffP4 | diffP5;
  if (oral0) mask |= diffKa;
  int nreq = 0;
  for (int b = ndiff & mask; b != 0; b >>= 1) nreq += b & 1;
  return nreq;
}

// Resolve linCmtSensType="auto" (100) for a model.  Forward-mode fvar costs
// one pass per REQUESTED direction (the kernel honors the ndiff mask), reverse
// mode one adjoint sweep per compartment regardless, so forward (3) wins when
// the requested count is at most m = ncmt + oral0 and reverse (31) otherwise.
// No requested direction means no Jacobian is taken; forward is returned so
// the value-only solve stays on the cheaper stack-local path.  Every solve
// path must resolve through here (rxData.cpp at the control read, setupLinH()
// for ind_solve(), linCmtModelDouble() for the R-level kernel) so they agree.
static inline int linCmtSensResolveAuto(int sensType, int ndiff, int ncmt, int oral0) {
  if (sensType != 100) return sensType;
  return (linCmtSensNreq(ndiff, ncmt, oral0) <= ncmt + oral0) ? 3 : 31;
}

#endif // __LINCMTSENSTYPE_H__
