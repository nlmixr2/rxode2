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
#> <pointer: 0x7fe2432ea020>
#> 
#> $rxode2rxParProgress
#> <pointer: 0x7fe2434e3d30>
#> 
#> $rxode2getRxSolve_
#> <pointer: 0x7fe2434e4f30>
#> 
#> $rxode2indSolve
#> <pointer: 0x7fe2434f5a50>
#> 
#> $rxode2getTime
#> <pointer: 0x7fe2434e4f60>
#> 
#> $rxode2isRstudio
#> <pointer: 0x7fe2434ff600>
#> 
#> $rxode2iniSubjectE
#> <pointer: 0x7fe2434ec970>
#> 
#> $rxode2sortIds
#> <pointer: 0x7fe24350b1f0>
#> 
#> $getSolvingOptionsInd
#> <pointer: 0x7fe2434fc730>
#> 
#> $rxode2getUpdateInis
#> <pointer: 0x7fe2434fc740>
#> 
#> $rxode2_rxode2_rxModelVars_
#> <pointer: 0x7fe2432e49e0>
#> 
#> $rxode2_par_solve
#> <pointer: 0x7fe2434f4810>
#> 
#> $rxode2rxGetId
#> <pointer: 0x7fe2434e2e30>
#> 
#> $rxode2getIndLambda
#> <pointer: 0x7fe2434fc780>
#> 
#> $rxode2getIndLambdaYj
#> <pointer: 0x7fe2434fc790>
#> 
#> $rxode2getIndLogitLow
#> <pointer: 0x7fe2434fc7a0>
#> 
#> $rxode2getIndLogitHi
#> <pointer: 0x7fe2434fc7b0>
#> 
#> $rxode2setIndParPtr
#> <pointer: 0x7fe2434fc7c0>
#> 
#> $rxode2getIndParPtr
#> <pointer: 0x7fe2434fc820>
#> 
#> $rxode2getIndNallTimes
#> <pointer: 0x7fe2434fc870>
#> 
#> $rxode2setIndIdx
#> <pointer: 0x7fe2434fc880>
#> 
#> $rxode2getIndIx
#> <pointer: 0x7fe2434fc890>
#> 
#> $rxode2getIndEvid
#> <pointer: 0x7fe2434fc920>
#> 
#> $rxode2getIndLhs
#> <pointer: 0x7fe2434fc950>
#> 
#> $rxode2getIndNdoses
#> <pointer: 0x7fe2434fc960>
#> 
#> $rxode2getIndNevid2
#> <pointer: 0x7fe2434fc970>
#> 
#> $rxode2setIndSolve
#> <pointer: 0x7fe2434fc980>
#> 
#> $rxode2getIndSolve
#> <pointer: 0x7fe2434fc990>
#> 
#> $rxode2getIndDv
#> <pointer: 0x7fe2434fc9a0>
#> 
#> $rxode2getIndYj
#> <pointer: 0x7fe2434fc9e0>
#> 
#> $rxode2getIndLimit
#> <pointer: 0x7fe2434fc9f0>
#> 
#> $rxode2getIndCens
#> <pointer: 0x7fe2434fca30>
#> 
#> $rxode2getIndIdx
#> <pointer: 0x7fe2434fca60>
#> 
#> $rxode2getOpNeq
#> <pointer: 0x7fe2434fca70>
#> 
#> $rxode2setOpNeq
#> <pointer: 0x7fe2434fca80>
#> 
#> $rxode2hasOpBadSolve
#> <pointer: 0x7fe2434fca90>
#> 
#> $rxode2getOpNlin
#> <pointer: 0x7fe2434fcaa0>
#> 
#> $rxode2getOpCores
#> <pointer: 0x7fe2434fcab0>
#> 
#> $rxode2getOpNlhs
#> <pointer: 0x7fe2434fcac0>
#> 
#> $rxode2getOpStiff
#> <pointer: 0x7fe2434fcad0>
#> 
#> $rxode2resetOpBadSolve
#> <pointer: 0x7fe2434fcae0>
#> 
#> $rxode2getRxNsub
#> <pointer: 0x7fe2434fcaf0>
#> 
#> $rxode2hasRxLimit
#> <pointer: 0x7fe2434fcb00>
#> 
#> $rxode2hasRxCens
#> <pointer: 0x7fe2434fcb10>
#> 
#> $rxode2getRxNall
#> <pointer: 0x7fe2434fcb20>
#> 
#> $rxode2getRxNobs
#> <pointer: 0x7fe2434fcb30>
#> 
#> $rxode2getRxNobs2
#> <pointer: 0x7fe2434fcb40>
#> 
#> $rxode2getOpIndSolve
#> <pointer: 0x7fe2434fcb60>
#> 
#> $rxode2getRxNpars
#> <pointer: 0x7fe2434fcb50>
#> 
#> $rxode2getIndMixest
#> <pointer: 0x7fe2434fc8c0>
#> 
#> $rxode2setIndMixest
#> <pointer: 0x7fe2434fc8d0>
#> 
#> $rxode2mexpit
#> <pointer: 0x7fe243484c70>
#> 
#> $rxode2getRxMixnum
#> <pointer: 0x7fe2434fc8f0>
#> 
#> $rxode2setRxMixnum
#> <pointer: 0x7fe2434fc900>
#> 
```
