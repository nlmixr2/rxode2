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
#> <pointer: 0x7f041c8ea020>
#> 
#> $rxode2rxParProgress
#> <pointer: 0x7f041cae3d30>
#> 
#> $rxode2getRxSolve_
#> <pointer: 0x7f041cae4f30>
#> 
#> $rxode2indSolve
#> <pointer: 0x7f041caf5a50>
#> 
#> $rxode2getTime
#> <pointer: 0x7f041cae4f60>
#> 
#> $rxode2isRstudio
#> <pointer: 0x7f041caff600>
#> 
#> $rxode2iniSubjectE
#> <pointer: 0x7f041caec970>
#> 
#> $rxode2sortIds
#> <pointer: 0x7f041cb0b1f0>
#> 
#> $getSolvingOptionsInd
#> <pointer: 0x7f041cafc730>
#> 
#> $rxode2getUpdateInis
#> <pointer: 0x7f041cafc740>
#> 
#> $rxode2_rxode2_rxModelVars_
#> <pointer: 0x7f041c8e49e0>
#> 
#> $rxode2_par_solve
#> <pointer: 0x7f041caf4810>
#> 
#> $rxode2rxGetId
#> <pointer: 0x7f041cae2e30>
#> 
#> $rxode2getIndLambda
#> <pointer: 0x7f041cafc780>
#> 
#> $rxode2getIndLambdaYj
#> <pointer: 0x7f041cafc790>
#> 
#> $rxode2getIndLogitLow
#> <pointer: 0x7f041cafc7a0>
#> 
#> $rxode2getIndLogitHi
#> <pointer: 0x7f041cafc7b0>
#> 
#> $rxode2setIndParPtr
#> <pointer: 0x7f041cafc7c0>
#> 
#> $rxode2getIndParPtr
#> <pointer: 0x7f041cafc820>
#> 
#> $rxode2getIndNallTimes
#> <pointer: 0x7f041cafc870>
#> 
#> $rxode2setIndIdx
#> <pointer: 0x7f041cafc880>
#> 
#> $rxode2getIndIx
#> <pointer: 0x7f041cafc890>
#> 
#> $rxode2getIndEvid
#> <pointer: 0x7f041cafc920>
#> 
#> $rxode2getIndLhs
#> <pointer: 0x7f041cafc950>
#> 
#> $rxode2getIndNdoses
#> <pointer: 0x7f041cafc960>
#> 
#> $rxode2getIndNevid2
#> <pointer: 0x7f041cafc970>
#> 
#> $rxode2setIndSolve
#> <pointer: 0x7f041cafc980>
#> 
#> $rxode2getIndSolve
#> <pointer: 0x7f041cafc990>
#> 
#> $rxode2getIndDv
#> <pointer: 0x7f041cafc9a0>
#> 
#> $rxode2getIndYj
#> <pointer: 0x7f041cafc9e0>
#> 
#> $rxode2getIndLimit
#> <pointer: 0x7f041cafc9f0>
#> 
#> $rxode2getIndCens
#> <pointer: 0x7f041cafca30>
#> 
#> $rxode2getIndIdx
#> <pointer: 0x7f041cafca60>
#> 
#> $rxode2getOpNeq
#> <pointer: 0x7f041cafca70>
#> 
#> $rxode2setOpNeq
#> <pointer: 0x7f041cafca80>
#> 
#> $rxode2hasOpBadSolve
#> <pointer: 0x7f041cafca90>
#> 
#> $rxode2getOpNlin
#> <pointer: 0x7f041cafcaa0>
#> 
#> $rxode2getOpCores
#> <pointer: 0x7f041cafcab0>
#> 
#> $rxode2getOpNlhs
#> <pointer: 0x7f041cafcac0>
#> 
#> $rxode2getOpStiff
#> <pointer: 0x7f041cafcad0>
#> 
#> $rxode2resetOpBadSolve
#> <pointer: 0x7f041cafcae0>
#> 
#> $rxode2getRxNsub
#> <pointer: 0x7f041cafcaf0>
#> 
#> $rxode2hasRxLimit
#> <pointer: 0x7f041cafcb00>
#> 
#> $rxode2hasRxCens
#> <pointer: 0x7f041cafcb10>
#> 
#> $rxode2getRxNall
#> <pointer: 0x7f041cafcb20>
#> 
#> $rxode2getRxNobs
#> <pointer: 0x7f041cafcb30>
#> 
#> $rxode2getRxNobs2
#> <pointer: 0x7f041cafcb40>
#> 
#> $rxode2getOpIndSolve
#> <pointer: 0x7f041cafcb60>
#> 
#> $rxode2getRxNpars
#> <pointer: 0x7f041cafcb50>
#> 
#> $rxode2getIndMixest
#> <pointer: 0x7f041cafc8c0>
#> 
#> $rxode2setIndMixest
#> <pointer: 0x7f041cafc8d0>
#> 
#> $rxode2mexpit
#> <pointer: 0x7f041ca84c70>
#> 
#> $rxode2getRxMixnum
#> <pointer: 0x7f041cafc8f0>
#> 
#> $rxode2setRxMixnum
#> <pointer: 0x7f041cafc900>
#> 
```
