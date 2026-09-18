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

## Incoming checks

Both items raised on the previous submission appear to be properties of the
check configuration rather than of the package.

* `flang-23: warning: argument unused during compilation: '-Wall'`
  (r-devel-linux-x86_64-debian-gcc).  `-Wall` is a C warning flag.  It reaches
  the Fortran compiler from that configuration's own `FFLAGS`, which R appends
  to every Fortran compile as

      ALL_FFLAGS = $(PKG_FFLAGS) $(FPICFLAGS) $(SHLIB_FFLAGS) $(FFLAGS)

  rxode2 sets no `PKG_FFLAGS`, so it contributes nothing to that command line,
  and `flang` is right to report that it cannot act on a C warning flag.  The
  same line should appear for any package with Fortran sources built with that
  `FFLAGS` and a clang-derived Fortran driver, so the configuration looks like
  the right place to fix it.

  We have nonetheless added a workaround so this submission is clean, kept as
  narrow as we could make it: `configure` drops the flag from the Fortran
  compile line only when the Fortran compiler identifies itself as `flang` and
  `flang` itself reports the flag unusable.  Nothing is suppressed and no
  diagnostic is disabled -- the flag is simply not passed to a compiler that
  has said it cannot use it.  On every other toolchain, `gfortran` included,
  no such line is generated and the Fortran compile line is byte for byte what
  it was.  The generated line is target specific, since R reads `src/Makevars`
  before `Makeconf` and a plain assignment there would be overridden; it is
  emitted only on the platforms described above, which use GNU make, and the
  shipped sources contain no GNU make construct, so `SystemRequirements` is
  unchanged.  We are happy to drop the workaround if you would rather the
  configuration carried the fix.

* `lazy-load database '.../codetools/R/codetools.rdb' is corrupt` /
  `internal error 1 in R_decompress1 with libdeflate`, under "R code for
  possible problems".  This is the check machine's own `codetools`
  installation failing to decompress; no package code is involved and we could
  not reproduce it.

## Test environments

* local: Ubuntu 24.04, R 4.6.1, `R CMD check --as-cran`

## R CMD check results

Status: OK, with the following local-only NOTEs.

* `Compilation used the following non-portable flag(s): -mno-omit-leaf-frame-pointer`
  -- this flag comes from the Ubuntu distribution build of R itself
  (`R CMD config CFLAGS`), not from the package.

* `Skipping checking HTML validation: no command 'tidy' found` -- HTML Tidy is
  not installed on the check machine.
