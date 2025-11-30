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
#> <pointer: 0x7f7a7d6ea020>
#> 
#> $rxode2rxParProgress
#> <pointer: 0x7f7a7d8e3d30>
#> 
#> $rxode2getRxSolve_
#> <pointer: 0x7f7a7d8e4f30>
#> 
#> $rxode2indSolve
#> <pointer: 0x7f7a7d8f5a50>
#> 
#> $rxode2getTime
#> <pointer: 0x7f7a7d8e4f60>
#> 
#> $rxode2isRstudio
#> <pointer: 0x7f7a7d8ff600>
#> 
#> $rxode2iniSubjectE
#> <pointer: 0x7f7a7d8ec970>
#> 
#> $rxode2sortIds
#> <pointer: 0x7f7a7d90b1f0>
#> 
#> $getSolvingOptionsInd
#> <pointer: 0x7f7a7d8fc730>
#> 
#> $rxode2getUpdateInis
#> <pointer: 0x7f7a7d8fc740>
#> 
#> $rxode2_rxode2_rxModelVars_
#> <pointer: 0x7f7a7d6e49e0>
#> 
#> $rxode2_par_solve
#> <pointer: 0x7f7a7d8f4810>
#> 
#> $rxode2rxGetId
#> <pointer: 0x7f7a7d8e2e30>
#> 
#> $rxode2getIndLambda
#> <pointer: 0x7f7a7d8fc780>
#> 
#> $rxode2getIndLambdaYj
#> <pointer: 0x7f7a7d8fc790>
#> 
#> $rxode2getIndLogitLow
#> <pointer: 0x7f7a7d8fc7a0>
#> 
#> $rxode2getIndLogitHi
#> <pointer: 0x7f7a7d8fc7b0>
#> 
#> $rxode2setIndParPtr
#> <pointer: 0x7f7a7d8fc7c0>
#> 
#> $rxode2getIndParPtr
#> <pointer: 0x7f7a7d8fc820>
#> 
#> $rxode2getIndNallTimes
#> <pointer: 0x7f7a7d8fc870>
#> 
#> $rxode2setIndIdx
#> <pointer: 0x7f7a7d8fc880>
#> 
#> $rxode2getIndIx
#> <pointer: 0x7f7a7d8fc890>
#> 
#> $rxode2getIndEvid
#> <pointer: 0x7f7a7d8fc920>
#> 
#> $rxode2getIndLhs
#> <pointer: 0x7f7a7d8fc950>
#> 
#> $rxode2getIndNdoses
#> <pointer: 0x7f7a7d8fc960>
#> 
#> $rxode2getIndNevid2
#> <pointer: 0x7f7a7d8fc970>
#> 
#> $rxode2setIndSolve
#> <pointer: 0x7f7a7d8fc980>
#> 
#> $rxode2getIndSolve
#> <pointer: 0x7f7a7d8fc990>
#> 
#> $rxode2getIndDv
#> <pointer: 0x7f7a7d8fc9a0>
#> 
#> $rxode2getIndYj
#> <pointer: 0x7f7a7d8fc9e0>
#> 
#> $rxode2getIndLimit
#> <pointer: 0x7f7a7d8fc9f0>
#> 
#> $rxode2getIndCens
#> <pointer: 0x7f7a7d8fca30>
#> 
#> $rxode2getIndIdx
#> <pointer: 0x7f7a7d8fca60>
#> 
#> $rxode2getOpNeq
#> <pointer: 0x7f7a7d8fca70>
#> 
#> $rxode2setOpNeq
#> <pointer: 0x7f7a7d8fca80>
#> 
#> $rxode2hasOpBadSolve
#> <pointer: 0x7f7a7d8fca90>
#> 
#> $rxode2getOpNlin
#> <pointer: 0x7f7a7d8fcaa0>
#> 
#> $rxode2getOpCores
#> <pointer: 0x7f7a7d8fcab0>
#> 
#> $rxode2getOpNlhs
#> <pointer: 0x7f7a7d8fcac0>
#> 
#> $rxode2getOpStiff
#> <pointer: 0x7f7a7d8fcad0>
#> 
#> $rxode2resetOpBadSolve
#> <pointer: 0x7f7a7d8fcae0>
#> 
#> $rxode2getRxNsub
#> <pointer: 0x7f7a7d8fcaf0>
#> 
#> $rxode2hasRxLimit
#> <pointer: 0x7f7a7d8fcb00>
#> 
#> $rxode2hasRxCens
#> <pointer: 0x7f7a7d8fcb10>
#> 
#> $rxode2getRxNall
#> <pointer: 0x7f7a7d8fcb20>
#> 
#> $rxode2getRxNobs
#> <pointer: 0x7f7a7d8fcb30>
#> 
#> $rxode2getRxNobs2
#> <pointer: 0x7f7a7d8fcb40>
#> 
#> $rxode2getOpIndSolve
#> <pointer: 0x7f7a7d8fcb60>
#> 
#> $rxode2getRxNpars
#> <pointer: 0x7f7a7d8fcb50>
#> 
#> $rxode2getIndMixest
#> <pointer: 0x7f7a7d8fc8c0>
#> 
#> $rxode2setIndMixest
#> <pointer: 0x7f7a7d8fc8d0>
#> 
#> $rxode2mexpit
#> <pointer: 0x7f7a7d884c70>
#> 
#> $rxode2getRxMixnum
#> <pointer: 0x7f7a7d8fc8f0>
#> 
#> $rxode2setRxMixnum
#> <pointer: 0x7f7a7d8fc900>
#> 
```
