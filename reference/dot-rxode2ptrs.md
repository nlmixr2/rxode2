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
#> <pointer: 0x7f34e68eb1a0>
#> 
#> $rxode2rxParProgress
#> <pointer: 0x7f34e6ae4f40>
#> 
#> $rxode2getRxSolve_
#> <pointer: 0x7f34e6ae6180>
#> 
#> $rxode2indSolve
#> <pointer: 0x7f34e6af6cc0>
#> 
#> $rxode2getTime
#> <pointer: 0x7f34e6ae61b0>
#> 
#> $rxode2isRstudio
#> <pointer: 0x7f34e6b009c0>
#> 
#> $rxode2iniSubjectE
#> <pointer: 0x7f34e6aedbe0>
#> 
#> $rxode2sortIds
#> <pointer: 0x7f34e6b0c5b0>
#> 
#> $getSolvingOptionsInd
#> <pointer: 0x7f34e6afdaa0>
#> 
#> $rxode2getUpdateInis
#> <pointer: 0x7f34e6afdab0>
#> 
#> $rxode2_rxode2_rxModelVars_
#> <pointer: 0x7f34e68e5b60>
#> 
#> $rxode2_par_solve
#> <pointer: 0x7f34e6af5a80>
#> 
#> $rxode2rxGetId
#> <pointer: 0x7f34e6ae4040>
#> 
#> $rxode2getIndLambda
#> <pointer: 0x7f34e6afdaf0>
#> 
#> $rxode2getIndLambdaYj
#> <pointer: 0x7f34e6afdb00>
#> 
#> $rxode2getIndLogitLow
#> <pointer: 0x7f34e6afdb10>
#> 
#> $rxode2getIndLogitHi
#> <pointer: 0x7f34e6afdb20>
#> 
#> $rxode2setIndParPtr
#> <pointer: 0x7f34e6afdb30>
#> 
#> $rxode2getIndParPtr
#> <pointer: 0x7f34e6afdb90>
#> 
#> $rxode2getIndNallTimes
#> <pointer: 0x7f34e6afdbe0>
#> 
#> $rxode2setIndIdx
#> <pointer: 0x7f34e6afdbf0>
#> 
#> $rxode2getIndIx
#> <pointer: 0x7f34e6afdc00>
#> 
#> $rxode2getIndEvid
#> <pointer: 0x7f34e6afdc90>
#> 
#> $rxode2getIndLhs
#> <pointer: 0x7f34e6afdcc0>
#> 
#> $rxode2getIndNdoses
#> <pointer: 0x7f34e6afdcd0>
#> 
#> $rxode2getIndNevid2
#> <pointer: 0x7f34e6afdce0>
#> 
#> $rxode2setIndSolve
#> <pointer: 0x7f34e6afdcf0>
#> 
#> $rxode2getIndSolve
#> <pointer: 0x7f34e6afdd00>
#> 
#> $rxode2getIndDv
#> <pointer: 0x7f34e6afdd10>
#> 
#> $rxode2getIndYj
#> <pointer: 0x7f34e6afdd50>
#> 
#> $rxode2getIndLimit
#> <pointer: 0x7f34e6afdd60>
#> 
#> $rxode2getIndCens
#> <pointer: 0x7f34e6afdda0>
#> 
#> $rxode2getIndIdx
#> <pointer: 0x7f34e6afddd0>
#> 
#> $rxode2getOpNeq
#> <pointer: 0x7f34e6afdde0>
#> 
#> $rxode2setOpNeq
#> <pointer: 0x7f34e6afddf0>
#> 
#> $rxode2hasOpBadSolve
#> <pointer: 0x7f34e6afde00>
#> 
#> $rxode2getOpNlin
#> <pointer: 0x7f34e6afde10>
#> 
#> $rxode2getOpCores
#> <pointer: 0x7f34e6afde20>
#> 
#> $rxode2getOpNlhs
#> <pointer: 0x7f34e6afde30>
#> 
#> $rxode2getOpStiff
#> <pointer: 0x7f34e6afde40>
#> 
#> $rxode2resetOpBadSolve
#> <pointer: 0x7f34e6afde50>
#> 
#> $rxode2getRxNsub
#> <pointer: 0x7f34e6afde60>
#> 
#> $rxode2hasRxLimit
#> <pointer: 0x7f34e6afde70>
#> 
#> $rxode2hasRxCens
#> <pointer: 0x7f34e6afde80>
#> 
#> $rxode2getRxNall
#> <pointer: 0x7f34e6afde90>
#> 
#> $rxode2getRxNobs
#> <pointer: 0x7f34e6afdea0>
#> 
#> $rxode2getRxNobs2
#> <pointer: 0x7f34e6afdeb0>
#> 
#> $rxode2getOpIndSolve
#> <pointer: 0x7f34e6afded0>
#> 
#> $rxode2getRxNpars
#> <pointer: 0x7f34e6afdec0>
#> 
#> $rxode2getIndMixest
#> <pointer: 0x7f34e6afdc30>
#> 
#> $rxode2setIndMixest
#> <pointer: 0x7f34e6afdc40>
#> 
#> $rxode2mexpit
#> <pointer: 0x7f34e6a85e80>
#> 
#> $rxode2getRxMixnum
#> <pointer: 0x7f34e6afdc60>
#> 
#> $rxode2setRxMixnum
#> <pointer: 0x7f34e6afdc70>
#> 
```
