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
#> <pointer: 0x7f52d8eeb1b0>
#> 
#> $rxode2rxParProgress
#> <pointer: 0x7f52d90e4f50>
#> 
#> $rxode2getRxSolve_
#> <pointer: 0x7f52d90e6190>
#> 
#> $rxode2indSolve
#> <pointer: 0x7f52d90f6cd0>
#> 
#> $rxode2getTime
#> <pointer: 0x7f52d90e61c0>
#> 
#> $rxode2isRstudio
#> <pointer: 0x7f52d9100a80>
#> 
#> $rxode2iniSubjectE
#> <pointer: 0x7f52d90edbf0>
#> 
#> $rxode2sortIds
#> <pointer: 0x7f52d910c670>
#> 
#> $getSolvingOptionsInd
#> <pointer: 0x7f52d90fdb60>
#> 
#> $rxode2getUpdateInis
#> <pointer: 0x7f52d90fdb70>
#> 
#> $rxode2_rxode2_rxModelVars_
#> <pointer: 0x7f52d8ee5b70>
#> 
#> $rxode2_par_solve
#> <pointer: 0x7f52d90f5a90>
#> 
#> $rxode2rxGetId
#> <pointer: 0x7f52d90e4050>
#> 
#> $rxode2getIndLambda
#> <pointer: 0x7f52d90fdbb0>
#> 
#> $rxode2getIndLambdaYj
#> <pointer: 0x7f52d90fdbc0>
#> 
#> $rxode2getIndLogitLow
#> <pointer: 0x7f52d90fdbd0>
#> 
#> $rxode2getIndLogitHi
#> <pointer: 0x7f52d90fdbe0>
#> 
#> $rxode2setIndParPtr
#> <pointer: 0x7f52d90fdbf0>
#> 
#> $rxode2getIndParPtr
#> <pointer: 0x7f52d90fdc50>
#> 
#> $rxode2getIndNallTimes
#> <pointer: 0x7f52d90fdca0>
#> 
#> $rxode2setIndIdx
#> <pointer: 0x7f52d90fdcb0>
#> 
#> $rxode2getIndIx
#> <pointer: 0x7f52d90fdcc0>
#> 
#> $rxode2getIndEvid
#> <pointer: 0x7f52d90fdd50>
#> 
#> $rxode2getIndLhs
#> <pointer: 0x7f52d90fdd80>
#> 
#> $rxode2getIndNdoses
#> <pointer: 0x7f52d90fdd90>
#> 
#> $rxode2getIndNevid2
#> <pointer: 0x7f52d90fdda0>
#> 
#> $rxode2setIndSolve
#> <pointer: 0x7f52d90fddb0>
#> 
#> $rxode2getIndSolve
#> <pointer: 0x7f52d90fddc0>
#> 
#> $rxode2getIndDv
#> <pointer: 0x7f52d90fddd0>
#> 
#> $rxode2getIndYj
#> <pointer: 0x7f52d90fde10>
#> 
#> $rxode2getIndLimit
#> <pointer: 0x7f52d90fde20>
#> 
#> $rxode2getIndCens
#> <pointer: 0x7f52d90fde60>
#> 
#> $rxode2getIndIdx
#> <pointer: 0x7f52d90fde90>
#> 
#> $rxode2getOpNeq
#> <pointer: 0x7f52d90fdea0>
#> 
#> $rxode2setOpNeq
#> <pointer: 0x7f52d90fdeb0>
#> 
#> $rxode2hasOpBadSolve
#> <pointer: 0x7f52d90fdec0>
#> 
#> $rxode2getOpNlin
#> <pointer: 0x7f52d90fded0>
#> 
#> $rxode2getOpCores
#> <pointer: 0x7f52d90fdee0>
#> 
#> $rxode2getOpNlhs
#> <pointer: 0x7f52d90fdef0>
#> 
#> $rxode2getOpStiff
#> <pointer: 0x7f52d90fdf00>
#> 
#> $rxode2resetOpBadSolve
#> <pointer: 0x7f52d90fdf10>
#> 
#> $rxode2getRxNsub
#> <pointer: 0x7f52d90fdf20>
#> 
#> $rxode2hasRxLimit
#> <pointer: 0x7f52d90fdf30>
#> 
#> $rxode2hasRxCens
#> <pointer: 0x7f52d90fdf40>
#> 
#> $rxode2getRxNall
#> <pointer: 0x7f52d90fdf50>
#> 
#> $rxode2getRxNobs
#> <pointer: 0x7f52d90fdf60>
#> 
#> $rxode2getRxNobs2
#> <pointer: 0x7f52d90fdf70>
#> 
#> $rxode2getOpIndSolve
#> <pointer: 0x7f52d90fdf90>
#> 
#> $rxode2getRxNpars
#> <pointer: 0x7f52d90fdf80>
#> 
#> $rxode2getIndMixest
#> <pointer: 0x7f52d90fdcf0>
#> 
#> $rxode2setIndMixest
#> <pointer: 0x7f52d90fdd00>
#> 
#> $rxode2mexpit
#> <pointer: 0x7f52d9085e90>
#> 
#> $rxode2getRxMixnum
#> <pointer: 0x7f52d90fdd20>
#> 
#> $rxode2setRxMixnum
#> <pointer: 0x7f52d90fdd30>
#> 
```
