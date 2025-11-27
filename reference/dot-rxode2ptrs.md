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
#> <pointer: 0x7ff791ce9b60>
#> 
#> $rxode2rxParProgress
#> <pointer: 0x7ff791ee3c20>
#> 
#> $rxode2getRxSolve_
#> <pointer: 0x7ff791ee4e20>
#> 
#> $rxode2indSolve
#> <pointer: 0x7ff791ef5940>
#> 
#> $rxode2getTime
#> <pointer: 0x7ff791ee4e50>
#> 
#> $rxode2isRstudio
#> <pointer: 0x7ff791effaa0>
#> 
#> $rxode2iniSubjectE
#> <pointer: 0x7ff791eec860>
#> 
#> $rxode2sortIds
#> <pointer: 0x7ff791f0b690>
#> 
#> $getSolvingOptionsInd
#> <pointer: 0x7ff791efcbd0>
#> 
#> $rxode2getUpdateInis
#> <pointer: 0x7ff791efcbe0>
#> 
#> $rxode2_rxode2_rxModelVars_
#> <pointer: 0x7ff791ce48a0>
#> 
#> $rxode2_par_solve
#> <pointer: 0x7ff791ef4700>
#> 
#> $rxode2rxGetId
#> <pointer: 0x7ff791ee2d20>
#> 
#> $rxode2getIndLambda
#> <pointer: 0x7ff791efcc20>
#> 
#> $rxode2getIndLambdaYj
#> <pointer: 0x7ff791efcc30>
#> 
#> $rxode2getIndLogitLow
#> <pointer: 0x7ff791efcc40>
#> 
#> $rxode2getIndLogitHi
#> <pointer: 0x7ff791efcc50>
#> 
#> $rxode2setIndParPtr
#> <pointer: 0x7ff791efcc60>
#> 
#> $rxode2getIndParPtr
#> <pointer: 0x7ff791efccc0>
#> 
#> $rxode2getIndNallTimes
#> <pointer: 0x7ff791efcd10>
#> 
#> $rxode2setIndIdx
#> <pointer: 0x7ff791efcd20>
#> 
#> $rxode2getIndIx
#> <pointer: 0x7ff791efcd30>
#> 
#> $rxode2getIndEvid
#> <pointer: 0x7ff791efcdc0>
#> 
#> $rxode2getIndLhs
#> <pointer: 0x7ff791efcdf0>
#> 
#> $rxode2getIndNdoses
#> <pointer: 0x7ff791efce00>
#> 
#> $rxode2getIndNevid2
#> <pointer: 0x7ff791efce10>
#> 
#> $rxode2setIndSolve
#> <pointer: 0x7ff791efce20>
#> 
#> $rxode2getIndSolve
#> <pointer: 0x7ff791efce30>
#> 
#> $rxode2getIndDv
#> <pointer: 0x7ff791efce40>
#> 
#> $rxode2getIndYj
#> <pointer: 0x7ff791efce80>
#> 
#> $rxode2getIndLimit
#> <pointer: 0x7ff791efce90>
#> 
#> $rxode2getIndCens
#> <pointer: 0x7ff791efced0>
#> 
#> $rxode2getIndIdx
#> <pointer: 0x7ff791efcf00>
#> 
#> $rxode2getOpNeq
#> <pointer: 0x7ff791efcf10>
#> 
#> $rxode2setOpNeq
#> <pointer: 0x7ff791efcf20>
#> 
#> $rxode2hasOpBadSolve
#> <pointer: 0x7ff791efcf30>
#> 
#> $rxode2getOpNlin
#> <pointer: 0x7ff791efcf40>
#> 
#> $rxode2getOpCores
#> <pointer: 0x7ff791efcf50>
#> 
#> $rxode2getOpNlhs
#> <pointer: 0x7ff791efcf60>
#> 
#> $rxode2getOpStiff
#> <pointer: 0x7ff791efcf70>
#> 
#> $rxode2resetOpBadSolve
#> <pointer: 0x7ff791efcf80>
#> 
#> $rxode2getRxNsub
#> <pointer: 0x7ff791efcf90>
#> 
#> $rxode2hasRxLimit
#> <pointer: 0x7ff791efcfa0>
#> 
#> $rxode2hasRxCens
#> <pointer: 0x7ff791efcfb0>
#> 
#> $rxode2getRxNall
#> <pointer: 0x7ff791efcfc0>
#> 
#> $rxode2getRxNobs
#> <pointer: 0x7ff791efcfd0>
#> 
#> $rxode2getRxNobs2
#> <pointer: 0x7ff791efcfe0>
#> 
#> $rxode2getOpIndSolve
#> <pointer: 0x7ff791efd000>
#> 
#> $rxode2getRxNpars
#> <pointer: 0x7ff791efcff0>
#> 
#> $rxode2getIndMixest
#> <pointer: 0x7ff791efcd60>
#> 
#> $rxode2setIndMixest
#> <pointer: 0x7ff791efcd70>
#> 
#> $rxode2mexpit
#> <pointer: 0x7ff791e84b60>
#> 
#> $rxode2getRxMixnum
#> <pointer: 0x7ff791efcd90>
#> 
#> $rxode2setRxMixnum
#> <pointer: 0x7ff791efcda0>
#> 
```
