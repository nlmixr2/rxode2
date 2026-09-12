# rxode2 5.1.7

This is a feature and bug-fix release.

The headline changes are correlated inter-occasion variability (a `| occ`
block may now carry off-diagonal elements, and NONMEM's `$OMEGA BLOCK(n)
SAME` is written `same()`), several endpoints sharing one model variable,
per-compartment dose-time sensitivities for `linCmt()` models, and one
shared implementation of the NONMEM event semantics behind both the event
table and the in-model dose-pushing statements.  The rest is bug fixes; the
full list is in NEWS.md.

## Dependency

This version requires `lotri` (>= 1.0.5), which carries the `same()` block
and the prior column it is built on.  `lotri` 1.0.5 is being submitted
alongside this and should be installed first; rxode2 5.1.7 will not install
against `lotri` 1.0.4.

## Update frequency

The incoming check notes seven updates in the past six months.  Most of
those were the 5.1.5/5.1.6 sequence, which was driven by upstream changes
outside our control (the RcppParallel 6.2.0 TBB reversal) and by the
reverse-dependency failure that followed.  This release collects the work
since then in one upload rather than several, and we expect to return to a
slower cadence.

## Test environments

* local: Ubuntu 24.04, R 4.6.1, `R CMD check --as-cran`

## R CMD check results

Status: OK, with the following local-only NOTEs.

* `Compilation used the following non-portable flag(s): -mno-omit-leaf-frame-pointer`
  -- this flag comes from the Ubuntu distribution build of R itself
  (`R CMD config CFLAGS`), not from the package.

* `Skipping checking HTML validation: no command 'tidy' found` -- HTML Tidy is
  not installed on the check machine.

## Reverse dependencies

PENDING
