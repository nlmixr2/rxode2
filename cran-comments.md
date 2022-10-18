# CRAN Comments

## Clang 15
Thanks, we see:

with clang15 still


Found the following significant warnings:
   dgefa.c:9:1: warning: a function definition without a prototype is
deprecated in all versions of C and is not supported in C2x
[-Wdeprecated-non-prototype]
   dgesl.c:8:1: warning: a function definition without a prototype is
deprecated in all versions of C and is not supported in C2x
[-Wdeprecated-non-prototype]

Fixed

## Authorship

Are those not based on LINPACK code?  We see no credit in the files (nor
others) nor the DESCRIPTION file.  LINPACK may be copyrighted in some
countries (and maybe not in the USA), but it has authors and R credits
those for the LINPACK code it includes (in doc/COPYRIGHTS).

Fixed as discussed below:

- All authors of LAPACK are listed as contributors and the inst/COPYRIGHTS
- Added Gilbert Stewart and Jim Bunch since they were the only authors
  missing in our DESCRIPTION file
- NOTE we also Jim Bunch who is not included in the R's (doc/COPYRIGHTS)

## M1 mac

On M1mac it fails miserably:

When this is installed:

 > library(nlmixr2)
Loading required package: nlmixr2data

 > library(nlmixr2est)
Error: package or namespace load failed for ‘nlmixr2est’:
  .onAttach failed in attachNamespace() for 'nlmixr2est', details:
   call: NULL
   error: nlmixr2 compiled against different version of rxode2, cannot
run nlmixr2
try `install.packages("nlmixr2", type = "source")` to recompile

This should say `install.packages("nlmixr2est", type="source")`

BUT this package is currently broken because of CRAN's request reduce
compile time for 'rxode2'.

This package will immediately be resubmitted once rxode2 is accepted.

Should I submit it on CRAN at the same time?

Matt
