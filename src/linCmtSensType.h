#ifndef __LINCMTSENSTYPE_H__
#define __LINCMTSENSTYPE_H__
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
// which setupLinH() remaps to 3).  The finite-difference methods stay excluded:
// their first-subject scaling/step-size setup is shared, not per-thread.
static inline int linCmtSensAdThreadSafe(int sensType) {
  return (sensType == 3 || sensType == 30 || sensType == 31 || sensType == 100);
}

#endif // __LINCMTSENSTYPE_H__
