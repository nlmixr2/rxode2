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
#> <pointer: 0x7f27dfae9b60>
#> 
#> $rxode2rxParProgress
#> <pointer: 0x7f27dfce3c20>
#> 
#> $rxode2getRxSolve_
#> <pointer: 0x7f27dfce4e20>
#> 
#> $rxode2indSolve
#> <pointer: 0x7f27dfcf5940>
#> 
#> $rxode2getTime
#> <pointer: 0x7f27dfce4e50>
#> 
#> $rxode2isRstudio
#> <pointer: 0x7f27dfcffaa0>
#> 
#> $rxode2iniSubjectE
#> <pointer: 0x7f27dfcec860>
#> 
#> $rxode2sortIds
#> <pointer: 0x7f27dfd0b690>
#> 
#> $getSolvingOptionsInd
#> <pointer: 0x7f27dfcfcbd0>
#> 
#> $rxode2getUpdateInis
#> <pointer: 0x7f27dfcfcbe0>
#> 
#> $rxode2_rxode2_rxModelVars_
#> <pointer: 0x7f27dfae48a0>
#> 
#> $rxode2_par_solve
#> <pointer: 0x7f27dfcf4700>
#> 
#> $rxode2rxGetId
#> <pointer: 0x7f27dfce2d20>
#> 
#> $rxode2getIndLambda
#> <pointer: 0x7f27dfcfcc20>
#> 
#> $rxode2getIndLambdaYj
#> <pointer: 0x7f27dfcfcc30>
#> 
#> $rxode2getIndLogitLow
#> <pointer: 0x7f27dfcfcc40>
#> 
#> $rxode2getIndLogitHi
#> <pointer: 0x7f27dfcfcc50>
#> 
#> $rxode2setIndParPtr
#> <pointer: 0x7f27dfcfcc60>
#> 
#> $rxode2getIndParPtr
#> <pointer: 0x7f27dfcfccc0>
#> 
#> $rxode2getIndNallTimes
#> <pointer: 0x7f27dfcfcd10>
#> 
#> $rxode2setIndIdx
#> <pointer: 0x7f27dfcfcd20>
#> 
#> $rxode2getIndIx
#> <pointer: 0x7f27dfcfcd30>
#> 
#> $rxode2getIndEvid
#> <pointer: 0x7f27dfcfcdc0>
#> 
#> $rxode2getIndLhs
#> <pointer: 0x7f27dfcfcdf0>
#> 
#> $rxode2getIndNdoses
#> <pointer: 0x7f27dfcfce00>
#> 
#> $rxode2getIndNevid2
#> <pointer: 0x7f27dfcfce10>
#> 
#> $rxode2setIndSolve
#> <pointer: 0x7f27dfcfce20>
#> 
#> $rxode2getIndSolve
#> <pointer: 0x7f27dfcfce30>
#> 
#> $rxode2getIndDv
#> <pointer: 0x7f27dfcfce40>
#> 
#> $rxode2getIndYj
#> <pointer: 0x7f27dfcfce80>
#> 
#> $rxode2getIndLimit
#> <pointer: 0x7f27dfcfce90>
#> 
#> $rxode2getIndCens
#> <pointer: 0x7f27dfcfced0>
#> 
#> $rxode2getIndIdx
#> <pointer: 0x7f27dfcfcf00>
#> 
#> $rxode2getOpNeq
#> <pointer: 0x7f27dfcfcf10>
#> 
#> $rxode2setOpNeq
#> <pointer: 0x7f27dfcfcf20>
#> 
#> $rxode2hasOpBadSolve
#> <pointer: 0x7f27dfcfcf30>
#> 
#> $rxode2getOpNlin
#> <pointer: 0x7f27dfcfcf40>
#> 
#> $rxode2getOpCores
#> <pointer: 0x7f27dfcfcf50>
#> 
#> $rxode2getOpNlhs
#> <pointer: 0x7f27dfcfcf60>
#> 
#> $rxode2getOpStiff
#> <pointer: 0x7f27dfcfcf70>
#> 
#> $rxode2resetOpBadSolve
#> <pointer: 0x7f27dfcfcf80>
#> 
#> $rxode2getRxNsub
#> <pointer: 0x7f27dfcfcf90>
#> 
#> $rxode2hasRxLimit
#> <pointer: 0x7f27dfcfcfa0>
#> 
#> $rxode2hasRxCens
#> <pointer: 0x7f27dfcfcfb0>
#> 
#> $rxode2getRxNall
#> <pointer: 0x7f27dfcfcfc0>
#> 
#> $rxode2getRxNobs
#> <pointer: 0x7f27dfcfcfd0>
#> 
#> $rxode2getRxNobs2
#> <pointer: 0x7f27dfcfcfe0>
#> 
#> $rxode2getOpIndSolve
#> <pointer: 0x7f27dfcfd000>
#> 
#> $rxode2getRxNpars
#> <pointer: 0x7f27dfcfcff0>
#> 
#> $rxode2getIndMixest
#> <pointer: 0x7f27dfcfcd60>
#> 
#> $rxode2setIndMixest
#> <pointer: 0x7f27dfcfcd70>
#> 
#> $rxode2mexpit
#> <pointer: 0x7f27dfc84b60>
#> 
#> $rxode2getRxMixnum
#> <pointer: 0x7f27dfcfcd90>
#> 
#> $rxode2setRxMixnum
#> <pointer: 0x7f27dfcfcda0>
#> 
```
