# Get the rxode2 function pointers

This function is used to get the function pointers for rxode2. This is
used to allow rxode2 to have binary linkage to nlmixr2est.

## Usage

``` r
.rxode2ptrs()
```

## Value

a list of function pointers

## Author

Matthew L. Fidler

## Examples

``` r
.rxode2ptrs()
#> $rxode2rxRmvnSEXP
#> <pointer: 0x7f2b5dee9b60>
#> 
#> $rxode2rxParProgress
#> <pointer: 0x7f2b5e0e3c20>
#> 
#> $rxode2getRxSolve_
#> <pointer: 0x7f2b5e0e4e20>
#> 
#> $rxode2indSolve
#> <pointer: 0x7f2b5e0f5940>
#> 
#> $rxode2getTime
#> <pointer: 0x7f2b5e0e4e50>
#> 
#> $rxode2isRstudio
#> <pointer: 0x7f2b5e0ffaa0>
#> 
#> $rxode2iniSubjectE
#> <pointer: 0x7f2b5e0ec860>
#> 
#> $rxode2sortIds
#> <pointer: 0x7f2b5e10b690>
#> 
#> $getSolvingOptionsInd
#> <pointer: 0x7f2b5e0fcbd0>
#> 
#> $rxode2getUpdateInis
#> <pointer: 0x7f2b5e0fcbe0>
#> 
#> $rxode2_rxode2_rxModelVars_
#> <pointer: 0x7f2b5dee48a0>
#> 
#> $rxode2_par_solve
#> <pointer: 0x7f2b5e0f4700>
#> 
#> $rxode2rxGetId
#> <pointer: 0x7f2b5e0e2d20>
#> 
#> $rxode2getIndLambda
#> <pointer: 0x7f2b5e0fcc20>
#> 
#> $rxode2getIndLambdaYj
#> <pointer: 0x7f2b5e0fcc30>
#> 
#> $rxode2getIndLogitLow
#> <pointer: 0x7f2b5e0fcc40>
#> 
#> $rxode2getIndLogitHi
#> <pointer: 0x7f2b5e0fcc50>
#> 
#> $rxode2setIndParPtr
#> <pointer: 0x7f2b5e0fcc60>
#> 
#> $rxode2getIndParPtr
#> <pointer: 0x7f2b5e0fccc0>
#> 
#> $rxode2getIndNallTimes
#> <pointer: 0x7f2b5e0fcd10>
#> 
#> $rxode2setIndIdx
#> <pointer: 0x7f2b5e0fcd20>
#> 
#> $rxode2getIndIx
#> <pointer: 0x7f2b5e0fcd30>
#> 
#> $rxode2getIndEvid
#> <pointer: 0x7f2b5e0fcdc0>
#> 
#> $rxode2getIndLhs
#> <pointer: 0x7f2b5e0fcdf0>
#> 
#> $rxode2getIndNdoses
#> <pointer: 0x7f2b5e0fce00>
#> 
#> $rxode2getIndNevid2
#> <pointer: 0x7f2b5e0fce10>
#> 
#> $rxode2setIndSolve
#> <pointer: 0x7f2b5e0fce20>
#> 
#> $rxode2getIndSolve
#> <pointer: 0x7f2b5e0fce30>
#> 
#> $rxode2getIndDv
#> <pointer: 0x7f2b5e0fce40>
#> 
#> $rxode2getIndYj
#> <pointer: 0x7f2b5e0fce80>
#> 
#> $rxode2getIndLimit
#> <pointer: 0x7f2b5e0fce90>
#> 
#> $rxode2getIndCens
#> <pointer: 0x7f2b5e0fced0>
#> 
#> $rxode2getIndIdx
#> <pointer: 0x7f2b5e0fcf00>
#> 
#> $rxode2getOpNeq
#> <pointer: 0x7f2b5e0fcf10>
#> 
#> $rxode2setOpNeq
#> <pointer: 0x7f2b5e0fcf20>
#> 
#> $rxode2hasOpBadSolve
#> <pointer: 0x7f2b5e0fcf30>
#> 
#> $rxode2getOpNlin
#> <pointer: 0x7f2b5e0fcf40>
#> 
#> $rxode2getOpCores
#> <pointer: 0x7f2b5e0fcf50>
#> 
#> $rxode2getOpNlhs
#> <pointer: 0x7f2b5e0fcf60>
#> 
#> $rxode2getOpStiff
#> <pointer: 0x7f2b5e0fcf70>
#> 
#> $rxode2resetOpBadSolve
#> <pointer: 0x7f2b5e0fcf80>
#> 
#> $rxode2getRxNsub
#> <pointer: 0x7f2b5e0fcf90>
#> 
#> $rxode2hasRxLimit
#> <pointer: 0x7f2b5e0fcfa0>
#> 
#> $rxode2hasRxCens
#> <pointer: 0x7f2b5e0fcfb0>
#> 
#> $rxode2getRxNall
#> <pointer: 0x7f2b5e0fcfc0>
#> 
#> $rxode2getRxNobs
#> <pointer: 0x7f2b5e0fcfd0>
#> 
#> $rxode2getRxNobs2
#> <pointer: 0x7f2b5e0fcfe0>
#> 
#> $rxode2getOpIndSolve
#> <pointer: 0x7f2b5e0fd000>
#> 
#> $rxode2getRxNpars
#> <pointer: 0x7f2b5e0fcff0>
#> 
#> $rxode2getIndMixest
#> <pointer: 0x7f2b5e0fcd60>
#> 
#> $rxode2setIndMixest
#> <pointer: 0x7f2b5e0fcd70>
#> 
#> $rxode2mexpit
#> <pointer: 0x7f2b5e084b60>
#> 
#> $rxode2getRxMixnum
#> <pointer: 0x7f2b5e0fcd90>
#> 
#> $rxode2setRxMixnum
#> <pointer: 0x7f2b5e0fcda0>
#> 
```
