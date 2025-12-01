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
#> <pointer: 0x7f0eb9aea020>
#> 
#> $rxode2rxParProgress
#> <pointer: 0x7f0eb9ce3d30>
#> 
#> $rxode2getRxSolve_
#> <pointer: 0x7f0eb9ce4f30>
#> 
#> $rxode2indSolve
#> <pointer: 0x7f0eb9cf5a50>
#> 
#> $rxode2getTime
#> <pointer: 0x7f0eb9ce4f60>
#> 
#> $rxode2isRstudio
#> <pointer: 0x7f0eb9cff600>
#> 
#> $rxode2iniSubjectE
#> <pointer: 0x7f0eb9cec970>
#> 
#> $rxode2sortIds
#> <pointer: 0x7f0eb9d0b1f0>
#> 
#> $getSolvingOptionsInd
#> <pointer: 0x7f0eb9cfc730>
#> 
#> $rxode2getUpdateInis
#> <pointer: 0x7f0eb9cfc740>
#> 
#> $rxode2_rxode2_rxModelVars_
#> <pointer: 0x7f0eb9ae49e0>
#> 
#> $rxode2_par_solve
#> <pointer: 0x7f0eb9cf4810>
#> 
#> $rxode2rxGetId
#> <pointer: 0x7f0eb9ce2e30>
#> 
#> $rxode2getIndLambda
#> <pointer: 0x7f0eb9cfc780>
#> 
#> $rxode2getIndLambdaYj
#> <pointer: 0x7f0eb9cfc790>
#> 
#> $rxode2getIndLogitLow
#> <pointer: 0x7f0eb9cfc7a0>
#> 
#> $rxode2getIndLogitHi
#> <pointer: 0x7f0eb9cfc7b0>
#> 
#> $rxode2setIndParPtr
#> <pointer: 0x7f0eb9cfc7c0>
#> 
#> $rxode2getIndParPtr
#> <pointer: 0x7f0eb9cfc820>
#> 
#> $rxode2getIndNallTimes
#> <pointer: 0x7f0eb9cfc870>
#> 
#> $rxode2setIndIdx
#> <pointer: 0x7f0eb9cfc880>
#> 
#> $rxode2getIndIx
#> <pointer: 0x7f0eb9cfc890>
#> 
#> $rxode2getIndEvid
#> <pointer: 0x7f0eb9cfc920>
#> 
#> $rxode2getIndLhs
#> <pointer: 0x7f0eb9cfc950>
#> 
#> $rxode2getIndNdoses
#> <pointer: 0x7f0eb9cfc960>
#> 
#> $rxode2getIndNevid2
#> <pointer: 0x7f0eb9cfc970>
#> 
#> $rxode2setIndSolve
#> <pointer: 0x7f0eb9cfc980>
#> 
#> $rxode2getIndSolve
#> <pointer: 0x7f0eb9cfc990>
#> 
#> $rxode2getIndDv
#> <pointer: 0x7f0eb9cfc9a0>
#> 
#> $rxode2getIndYj
#> <pointer: 0x7f0eb9cfc9e0>
#> 
#> $rxode2getIndLimit
#> <pointer: 0x7f0eb9cfc9f0>
#> 
#> $rxode2getIndCens
#> <pointer: 0x7f0eb9cfca30>
#> 
#> $rxode2getIndIdx
#> <pointer: 0x7f0eb9cfca60>
#> 
#> $rxode2getOpNeq
#> <pointer: 0x7f0eb9cfca70>
#> 
#> $rxode2setOpNeq
#> <pointer: 0x7f0eb9cfca80>
#> 
#> $rxode2hasOpBadSolve
#> <pointer: 0x7f0eb9cfca90>
#> 
#> $rxode2getOpNlin
#> <pointer: 0x7f0eb9cfcaa0>
#> 
#> $rxode2getOpCores
#> <pointer: 0x7f0eb9cfcab0>
#> 
#> $rxode2getOpNlhs
#> <pointer: 0x7f0eb9cfcac0>
#> 
#> $rxode2getOpStiff
#> <pointer: 0x7f0eb9cfcad0>
#> 
#> $rxode2resetOpBadSolve
#> <pointer: 0x7f0eb9cfcae0>
#> 
#> $rxode2getRxNsub
#> <pointer: 0x7f0eb9cfcaf0>
#> 
#> $rxode2hasRxLimit
#> <pointer: 0x7f0eb9cfcb00>
#> 
#> $rxode2hasRxCens
#> <pointer: 0x7f0eb9cfcb10>
#> 
#> $rxode2getRxNall
#> <pointer: 0x7f0eb9cfcb20>
#> 
#> $rxode2getRxNobs
#> <pointer: 0x7f0eb9cfcb30>
#> 
#> $rxode2getRxNobs2
#> <pointer: 0x7f0eb9cfcb40>
#> 
#> $rxode2getOpIndSolve
#> <pointer: 0x7f0eb9cfcb60>
#> 
#> $rxode2getRxNpars
#> <pointer: 0x7f0eb9cfcb50>
#> 
#> $rxode2getIndMixest
#> <pointer: 0x7f0eb9cfc8c0>
#> 
#> $rxode2setIndMixest
#> <pointer: 0x7f0eb9cfc8d0>
#> 
#> $rxode2mexpit
#> <pointer: 0x7f0eb9c84c70>
#> 
#> $rxode2getRxMixnum
#> <pointer: 0x7f0eb9cfc8f0>
#> 
#> $rxode2setRxMixnum
#> <pointer: 0x7f0eb9cfc900>
#> 
```
