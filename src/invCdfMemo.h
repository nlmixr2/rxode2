#ifndef __INVCDFMEMO_H__
#define __INVCDFMEMO_H__
// The per-thread memo for the root-finding inverse CDFs (gammapInv, gammaqInv,
// gammapInva, gammaqInva, ibetaInv, studentTInv, and the chi-squared and
// student-t families that route through them).  Implemented in boost.cpp.
//
// These two entry points cross three translation units -- boost.cpp defines
// them, the PARSER sizes the table from the call sites it counts, and the
// memory report asks what that cost.  They used to be hand-declared separately
// in parseFuns.h and rxMemoryComponents.cpp, so a changed signature would not
// have been caught anywhere: C has no cross-file checking without a shared
// declaration, and this package has twice shipped a wrong measurement to a
// stale-binary problem of exactly that shape.  One header, one declaration.
//
// C-compatible on purpose: parseFuns.h is included from tran.c.
#ifdef __cplusplus
extern "C" {
#endif

  // Bytes the memo occupies across `cores` threads, for the memory report.
  double rxInvCdfMemoBytes(int cores);

  // Ask for at least `n` slots per thread.  MONOTONIC and rounded up to a power
  // of two, with a hard floor of 8 and a ceiling of 4096 -- several models are
  // live at once in a normal session and none may shrink the table under
  // another.  The parser calls this with 4 slots per inverse-CDF call site.
  void rxSetInvCdfMemoSize(int n);

#ifdef __cplusplus
}
#endif
#endif // __INVCDFMEMO_H__
