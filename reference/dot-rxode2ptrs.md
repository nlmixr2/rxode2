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
#> <pointer: 0x7fb9298ea020>
#> 
#> $rxode2rxParProgress
#> <pointer: 0x7fb929ae3d30>
#> 
#> $rxode2getRxSolve_
#> <pointer: 0x7fb929ae4f30>
#> 
#> $rxode2indSolve
#> <pointer: 0x7fb929af5a50>
#> 
#> $rxode2getTime
#> <pointer: 0x7fb929ae4f60>
#> 
#> $rxode2isRstudio
#> <pointer: 0x7fb929aff600>
#> 
#> $rxode2iniSubjectE
#> <pointer: 0x7fb929aec970>
#> 
#> $rxode2sortIds
#> <pointer: 0x7fb929b0b1f0>
#> 
#> $getSolvingOptionsInd
#> <pointer: 0x7fb929afc730>
#> 
#> $rxode2getUpdateInis
#> <pointer: 0x7fb929afc740>
#> 
#> $rxode2_rxode2_rxModelVars_
#> <pointer: 0x7fb9298e49e0>
#> 
#> $rxode2_par_solve
#> <pointer: 0x7fb929af4810>
#> 
#> $rxode2rxGetId
#> <pointer: 0x7fb929ae2e30>
#> 
#> $rxode2getIndLambda
#> <pointer: 0x7fb929afc780>
#> 
#> $rxode2getIndLambdaYj
#> <pointer: 0x7fb929afc790>
#> 
#> $rxode2getIndLogitLow
#> <pointer: 0x7fb929afc7a0>
#> 
#> $rxode2getIndLogitHi
#> <pointer: 0x7fb929afc7b0>
#> 
#> $rxode2setIndParPtr
#> <pointer: 0x7fb929afc7c0>
#> 
#> $rxode2getIndParPtr
#> <pointer: 0x7fb929afc820>
#> 
#> $rxode2getIndNallTimes
#> <pointer: 0x7fb929afc870>
#> 
#> $rxode2setIndIdx
#> <pointer: 0x7fb929afc880>
#> 
#> $rxode2getIndIx
#> <pointer: 0x7fb929afc890>
#> 
#> $rxode2getIndEvid
#> <pointer: 0x7fb929afc920>
#> 
#> $rxode2getIndLhs
#> <pointer: 0x7fb929afc950>
#> 
#> $rxode2getIndNdoses
#> <pointer: 0x7fb929afc960>
#> 
#> $rxode2getIndNevid2
#> <pointer: 0x7fb929afc970>
#> 
#> $rxode2setIndSolve
#> <pointer: 0x7fb929afc980>
#> 
#> $rxode2getIndSolve
#> <pointer: 0x7fb929afc990>
#> 
#> $rxode2getIndDv
#> <pointer: 0x7fb929afc9a0>
#> 
#> $rxode2getIndYj
#> <pointer: 0x7fb929afc9e0>
#> 
#> $rxode2getIndLimit
#> <pointer: 0x7fb929afc9f0>
#> 
#> $rxode2getIndCens
#> <pointer: 0x7fb929afca30>
#> 
#> $rxode2getIndIdx
#> <pointer: 0x7fb929afca60>
#> 
#> $rxode2getOpNeq
#> <pointer: 0x7fb929afca70>
#> 
#> $rxode2setOpNeq
#> <pointer: 0x7fb929afca80>
#> 
#> $rxode2hasOpBadSolve
#> <pointer: 0x7fb929afca90>
#> 
#> $rxode2getOpNlin
#> <pointer: 0x7fb929afcaa0>
#> 
#> $rxode2getOpCores
#> <pointer: 0x7fb929afcab0>
#> 
#> $rxode2getOpNlhs
#> <pointer: 0x7fb929afcac0>
#> 
#> $rxode2getOpStiff
#> <pointer: 0x7fb929afcad0>
#> 
#> $rxode2resetOpBadSolve
#> <pointer: 0x7fb929afcae0>
#> 
#> $rxode2getRxNsub
#> <pointer: 0x7fb929afcaf0>
#> 
#> $rxode2hasRxLimit
#> <pointer: 0x7fb929afcb00>
#> 
#> $rxode2hasRxCens
#> <pointer: 0x7fb929afcb10>
#> 
#> $rxode2getRxNall
#> <pointer: 0x7fb929afcb20>
#> 
#> $rxode2getRxNobs
#> <pointer: 0x7fb929afcb30>
#> 
#> $rxode2getRxNobs2
#> <pointer: 0x7fb929afcb40>
#> 
#> $rxode2getOpIndSolve
#> <pointer: 0x7fb929afcb60>
#> 
#> $rxode2getRxNpars
#> <pointer: 0x7fb929afcb50>
#> 
#> $rxode2getIndMixest
#> <pointer: 0x7fb929afc8c0>
#> 
#> $rxode2setIndMixest
#> <pointer: 0x7fb929afc8d0>
#> 
#> $rxode2mexpit
#> <pointer: 0x7fb929a84c70>
#> 
#> $rxode2getRxMixnum
#> <pointer: 0x7fb929afc8f0>
#> 
#> $rxode2setRxMixnum
#> <pointer: 0x7fb929afc900>
#> 
```
