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
#> <pointer: 0x7fc64d6eb1a0>
#> 
#> $rxode2rxParProgress
#> <pointer: 0x7fc64d8e4f40>
#> 
#> $rxode2getRxSolve_
#> <pointer: 0x7fc64d8e6180>
#> 
#> $rxode2indSolve
#> <pointer: 0x7fc64d8f6cc0>
#> 
#> $rxode2getTime
#> <pointer: 0x7fc64d8e61b0>
#> 
#> $rxode2isRstudio
#> <pointer: 0x7fc64d9009c0>
#> 
#> $rxode2iniSubjectE
#> <pointer: 0x7fc64d8edbe0>
#> 
#> $rxode2sortIds
#> <pointer: 0x7fc64d90c5b0>
#> 
#> $getSolvingOptionsInd
#> <pointer: 0x7fc64d8fdaa0>
#> 
#> $rxode2getUpdateInis
#> <pointer: 0x7fc64d8fdab0>
#> 
#> $rxode2_rxode2_rxModelVars_
#> <pointer: 0x7fc64d6e5b60>
#> 
#> $rxode2_par_solve
#> <pointer: 0x7fc64d8f5a80>
#> 
#> $rxode2rxGetId
#> <pointer: 0x7fc64d8e4040>
#> 
#> $rxode2getIndLambda
#> <pointer: 0x7fc64d8fdaf0>
#> 
#> $rxode2getIndLambdaYj
#> <pointer: 0x7fc64d8fdb00>
#> 
#> $rxode2getIndLogitLow
#> <pointer: 0x7fc64d8fdb10>
#> 
#> $rxode2getIndLogitHi
#> <pointer: 0x7fc64d8fdb20>
#> 
#> $rxode2setIndParPtr
#> <pointer: 0x7fc64d8fdb30>
#> 
#> $rxode2getIndParPtr
#> <pointer: 0x7fc64d8fdb90>
#> 
#> $rxode2getIndNallTimes
#> <pointer: 0x7fc64d8fdbe0>
#> 
#> $rxode2setIndIdx
#> <pointer: 0x7fc64d8fdbf0>
#> 
#> $rxode2getIndIx
#> <pointer: 0x7fc64d8fdc00>
#> 
#> $rxode2getIndEvid
#> <pointer: 0x7fc64d8fdc90>
#> 
#> $rxode2getIndLhs
#> <pointer: 0x7fc64d8fdcc0>
#> 
#> $rxode2getIndNdoses
#> <pointer: 0x7fc64d8fdcd0>
#> 
#> $rxode2getIndNevid2
#> <pointer: 0x7fc64d8fdce0>
#> 
#> $rxode2setIndSolve
#> <pointer: 0x7fc64d8fdcf0>
#> 
#> $rxode2getIndSolve
#> <pointer: 0x7fc64d8fdd00>
#> 
#> $rxode2getIndDv
#> <pointer: 0x7fc64d8fdd10>
#> 
#> $rxode2getIndYj
#> <pointer: 0x7fc64d8fdd50>
#> 
#> $rxode2getIndLimit
#> <pointer: 0x7fc64d8fdd60>
#> 
#> $rxode2getIndCens
#> <pointer: 0x7fc64d8fdda0>
#> 
#> $rxode2getIndIdx
#> <pointer: 0x7fc64d8fddd0>
#> 
#> $rxode2getOpNeq
#> <pointer: 0x7fc64d8fdde0>
#> 
#> $rxode2setOpNeq
#> <pointer: 0x7fc64d8fddf0>
#> 
#> $rxode2hasOpBadSolve
#> <pointer: 0x7fc64d8fde00>
#> 
#> $rxode2getOpNlin
#> <pointer: 0x7fc64d8fde10>
#> 
#> $rxode2getOpCores
#> <pointer: 0x7fc64d8fde20>
#> 
#> $rxode2getOpNlhs
#> <pointer: 0x7fc64d8fde30>
#> 
#> $rxode2getOpStiff
#> <pointer: 0x7fc64d8fde40>
#> 
#> $rxode2resetOpBadSolve
#> <pointer: 0x7fc64d8fde50>
#> 
#> $rxode2getRxNsub
#> <pointer: 0x7fc64d8fde60>
#> 
#> $rxode2hasRxLimit
#> <pointer: 0x7fc64d8fde70>
#> 
#> $rxode2hasRxCens
#> <pointer: 0x7fc64d8fde80>
#> 
#> $rxode2getRxNall
#> <pointer: 0x7fc64d8fde90>
#> 
#> $rxode2getRxNobs
#> <pointer: 0x7fc64d8fdea0>
#> 
#> $rxode2getRxNobs2
#> <pointer: 0x7fc64d8fdeb0>
#> 
#> $rxode2getOpIndSolve
#> <pointer: 0x7fc64d8fded0>
#> 
#> $rxode2getRxNpars
#> <pointer: 0x7fc64d8fdec0>
#> 
#> $rxode2getIndMixest
#> <pointer: 0x7fc64d8fdc30>
#> 
#> $rxode2setIndMixest
#> <pointer: 0x7fc64d8fdc40>
#> 
#> $rxode2mexpit
#> <pointer: 0x7fc64d885e80>
#> 
#> $rxode2getRxMixnum
#> <pointer: 0x7fc64d8fdc60>
#> 
#> $rxode2setRxMixnum
#> <pointer: 0x7fc64d8fdc70>
#> 
```
