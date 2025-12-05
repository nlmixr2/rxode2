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
#> <pointer: 0x7f4b2f0eb1b0>
#> 
#> $rxode2rxParProgress
#> <pointer: 0x7f4b2f2e4f50>
#> 
#> $rxode2getRxSolve_
#> <pointer: 0x7f4b2f2e6190>
#> 
#> $rxode2indSolve
#> <pointer: 0x7f4b2f2f6cd0>
#> 
#> $rxode2getTime
#> <pointer: 0x7f4b2f2e61c0>
#> 
#> $rxode2isRstudio
#> <pointer: 0x7f4b2f300a80>
#> 
#> $rxode2iniSubjectE
#> <pointer: 0x7f4b2f2edbf0>
#> 
#> $rxode2sortIds
#> <pointer: 0x7f4b2f30c670>
#> 
#> $getSolvingOptionsInd
#> <pointer: 0x7f4b2f2fdb60>
#> 
#> $rxode2getUpdateInis
#> <pointer: 0x7f4b2f2fdb70>
#> 
#> $rxode2_rxode2_rxModelVars_
#> <pointer: 0x7f4b2f0e5b70>
#> 
#> $rxode2_par_solve
#> <pointer: 0x7f4b2f2f5a90>
#> 
#> $rxode2rxGetId
#> <pointer: 0x7f4b2f2e4050>
#> 
#> $rxode2getIndLambda
#> <pointer: 0x7f4b2f2fdbb0>
#> 
#> $rxode2getIndLambdaYj
#> <pointer: 0x7f4b2f2fdbc0>
#> 
#> $rxode2getIndLogitLow
#> <pointer: 0x7f4b2f2fdbd0>
#> 
#> $rxode2getIndLogitHi
#> <pointer: 0x7f4b2f2fdbe0>
#> 
#> $rxode2setIndParPtr
#> <pointer: 0x7f4b2f2fdbf0>
#> 
#> $rxode2getIndParPtr
#> <pointer: 0x7f4b2f2fdc50>
#> 
#> $rxode2getIndNallTimes
#> <pointer: 0x7f4b2f2fdca0>
#> 
#> $rxode2setIndIdx
#> <pointer: 0x7f4b2f2fdcb0>
#> 
#> $rxode2getIndIx
#> <pointer: 0x7f4b2f2fdcc0>
#> 
#> $rxode2getIndEvid
#> <pointer: 0x7f4b2f2fdd50>
#> 
#> $rxode2getIndLhs
#> <pointer: 0x7f4b2f2fdd80>
#> 
#> $rxode2getIndNdoses
#> <pointer: 0x7f4b2f2fdd90>
#> 
#> $rxode2getIndNevid2
#> <pointer: 0x7f4b2f2fdda0>
#> 
#> $rxode2setIndSolve
#> <pointer: 0x7f4b2f2fddb0>
#> 
#> $rxode2getIndSolve
#> <pointer: 0x7f4b2f2fddc0>
#> 
#> $rxode2getIndDv
#> <pointer: 0x7f4b2f2fddd0>
#> 
#> $rxode2getIndYj
#> <pointer: 0x7f4b2f2fde10>
#> 
#> $rxode2getIndLimit
#> <pointer: 0x7f4b2f2fde20>
#> 
#> $rxode2getIndCens
#> <pointer: 0x7f4b2f2fde60>
#> 
#> $rxode2getIndIdx
#> <pointer: 0x7f4b2f2fde90>
#> 
#> $rxode2getOpNeq
#> <pointer: 0x7f4b2f2fdea0>
#> 
#> $rxode2setOpNeq
#> <pointer: 0x7f4b2f2fdeb0>
#> 
#> $rxode2hasOpBadSolve
#> <pointer: 0x7f4b2f2fdec0>
#> 
#> $rxode2getOpNlin
#> <pointer: 0x7f4b2f2fded0>
#> 
#> $rxode2getOpCores
#> <pointer: 0x7f4b2f2fdee0>
#> 
#> $rxode2getOpNlhs
#> <pointer: 0x7f4b2f2fdef0>
#> 
#> $rxode2getOpStiff
#> <pointer: 0x7f4b2f2fdf00>
#> 
#> $rxode2resetOpBadSolve
#> <pointer: 0x7f4b2f2fdf10>
#> 
#> $rxode2getRxNsub
#> <pointer: 0x7f4b2f2fdf20>
#> 
#> $rxode2hasRxLimit
#> <pointer: 0x7f4b2f2fdf30>
#> 
#> $rxode2hasRxCens
#> <pointer: 0x7f4b2f2fdf40>
#> 
#> $rxode2getRxNall
#> <pointer: 0x7f4b2f2fdf50>
#> 
#> $rxode2getRxNobs
#> <pointer: 0x7f4b2f2fdf60>
#> 
#> $rxode2getRxNobs2
#> <pointer: 0x7f4b2f2fdf70>
#> 
#> $rxode2getOpIndSolve
#> <pointer: 0x7f4b2f2fdf90>
#> 
#> $rxode2getRxNpars
#> <pointer: 0x7f4b2f2fdf80>
#> 
#> $rxode2getIndMixest
#> <pointer: 0x7f4b2f2fdcf0>
#> 
#> $rxode2setIndMixest
#> <pointer: 0x7f4b2f2fdd00>
#> 
#> $rxode2mexpit
#> <pointer: 0x7f4b2f285e90>
#> 
#> $rxode2getRxMixnum
#> <pointer: 0x7f4b2f2fdd20>
#> 
#> $rxode2setRxMixnum
#> <pointer: 0x7f4b2f2fdd30>
#> 
```
