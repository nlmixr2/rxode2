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
#> <pointer: 0x7f3ba26eb1a0>
#> 
#> $rxode2rxParProgress
#> <pointer: 0x7f3ba28e4f40>
#> 
#> $rxode2getRxSolve_
#> <pointer: 0x7f3ba28e6180>
#> 
#> $rxode2indSolve
#> <pointer: 0x7f3ba28f6cc0>
#> 
#> $rxode2getTime
#> <pointer: 0x7f3ba28e61b0>
#> 
#> $rxode2isRstudio
#> <pointer: 0x7f3ba29009c0>
#> 
#> $rxode2iniSubjectE
#> <pointer: 0x7f3ba28edbe0>
#> 
#> $rxode2sortIds
#> <pointer: 0x7f3ba290c5b0>
#> 
#> $getSolvingOptionsInd
#> <pointer: 0x7f3ba28fdaa0>
#> 
#> $rxode2getUpdateInis
#> <pointer: 0x7f3ba28fdab0>
#> 
#> $rxode2_rxode2_rxModelVars_
#> <pointer: 0x7f3ba26e5b60>
#> 
#> $rxode2_par_solve
#> <pointer: 0x7f3ba28f5a80>
#> 
#> $rxode2rxGetId
#> <pointer: 0x7f3ba28e4040>
#> 
#> $rxode2getIndLambda
#> <pointer: 0x7f3ba28fdaf0>
#> 
#> $rxode2getIndLambdaYj
#> <pointer: 0x7f3ba28fdb00>
#> 
#> $rxode2getIndLogitLow
#> <pointer: 0x7f3ba28fdb10>
#> 
#> $rxode2getIndLogitHi
#> <pointer: 0x7f3ba28fdb20>
#> 
#> $rxode2setIndParPtr
#> <pointer: 0x7f3ba28fdb30>
#> 
#> $rxode2getIndParPtr
#> <pointer: 0x7f3ba28fdb90>
#> 
#> $rxode2getIndNallTimes
#> <pointer: 0x7f3ba28fdbe0>
#> 
#> $rxode2setIndIdx
#> <pointer: 0x7f3ba28fdbf0>
#> 
#> $rxode2getIndIx
#> <pointer: 0x7f3ba28fdc00>
#> 
#> $rxode2getIndEvid
#> <pointer: 0x7f3ba28fdc90>
#> 
#> $rxode2getIndLhs
#> <pointer: 0x7f3ba28fdcc0>
#> 
#> $rxode2getIndNdoses
#> <pointer: 0x7f3ba28fdcd0>
#> 
#> $rxode2getIndNevid2
#> <pointer: 0x7f3ba28fdce0>
#> 
#> $rxode2setIndSolve
#> <pointer: 0x7f3ba28fdcf0>
#> 
#> $rxode2getIndSolve
#> <pointer: 0x7f3ba28fdd00>
#> 
#> $rxode2getIndDv
#> <pointer: 0x7f3ba28fdd10>
#> 
#> $rxode2getIndYj
#> <pointer: 0x7f3ba28fdd50>
#> 
#> $rxode2getIndLimit
#> <pointer: 0x7f3ba28fdd60>
#> 
#> $rxode2getIndCens
#> <pointer: 0x7f3ba28fdda0>
#> 
#> $rxode2getIndIdx
#> <pointer: 0x7f3ba28fddd0>
#> 
#> $rxode2getOpNeq
#> <pointer: 0x7f3ba28fdde0>
#> 
#> $rxode2setOpNeq
#> <pointer: 0x7f3ba28fddf0>
#> 
#> $rxode2hasOpBadSolve
#> <pointer: 0x7f3ba28fde00>
#> 
#> $rxode2getOpNlin
#> <pointer: 0x7f3ba28fde10>
#> 
#> $rxode2getOpCores
#> <pointer: 0x7f3ba28fde20>
#> 
#> $rxode2getOpNlhs
#> <pointer: 0x7f3ba28fde30>
#> 
#> $rxode2getOpStiff
#> <pointer: 0x7f3ba28fde40>
#> 
#> $rxode2resetOpBadSolve
#> <pointer: 0x7f3ba28fde50>
#> 
#> $rxode2getRxNsub
#> <pointer: 0x7f3ba28fde60>
#> 
#> $rxode2hasRxLimit
#> <pointer: 0x7f3ba28fde70>
#> 
#> $rxode2hasRxCens
#> <pointer: 0x7f3ba28fde80>
#> 
#> $rxode2getRxNall
#> <pointer: 0x7f3ba28fde90>
#> 
#> $rxode2getRxNobs
#> <pointer: 0x7f3ba28fdea0>
#> 
#> $rxode2getRxNobs2
#> <pointer: 0x7f3ba28fdeb0>
#> 
#> $rxode2getOpIndSolve
#> <pointer: 0x7f3ba28fded0>
#> 
#> $rxode2getRxNpars
#> <pointer: 0x7f3ba28fdec0>
#> 
#> $rxode2getIndMixest
#> <pointer: 0x7f3ba28fdc30>
#> 
#> $rxode2setIndMixest
#> <pointer: 0x7f3ba28fdc40>
#> 
#> $rxode2mexpit
#> <pointer: 0x7f3ba2885e80>
#> 
#> $rxode2getRxMixnum
#> <pointer: 0x7f3ba28fdc60>
#> 
#> $rxode2setRxMixnum
#> <pointer: 0x7f3ba28fdc70>
#> 
```
