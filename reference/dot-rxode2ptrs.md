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
#> <pointer: 0x7fecbecea020>
#> 
#> $rxode2rxParProgress
#> <pointer: 0x7fecbeee3d30>
#> 
#> $rxode2getRxSolve_
#> <pointer: 0x7fecbeee4f30>
#> 
#> $rxode2indSolve
#> <pointer: 0x7fecbeef5a50>
#> 
#> $rxode2getTime
#> <pointer: 0x7fecbeee4f60>
#> 
#> $rxode2isRstudio
#> <pointer: 0x7fecbeeff600>
#> 
#> $rxode2iniSubjectE
#> <pointer: 0x7fecbeeec970>
#> 
#> $rxode2sortIds
#> <pointer: 0x7fecbef0b1f0>
#> 
#> $getSolvingOptionsInd
#> <pointer: 0x7fecbeefc730>
#> 
#> $rxode2getUpdateInis
#> <pointer: 0x7fecbeefc740>
#> 
#> $rxode2_rxode2_rxModelVars_
#> <pointer: 0x7fecbece49e0>
#> 
#> $rxode2_par_solve
#> <pointer: 0x7fecbeef4810>
#> 
#> $rxode2rxGetId
#> <pointer: 0x7fecbeee2e30>
#> 
#> $rxode2getIndLambda
#> <pointer: 0x7fecbeefc780>
#> 
#> $rxode2getIndLambdaYj
#> <pointer: 0x7fecbeefc790>
#> 
#> $rxode2getIndLogitLow
#> <pointer: 0x7fecbeefc7a0>
#> 
#> $rxode2getIndLogitHi
#> <pointer: 0x7fecbeefc7b0>
#> 
#> $rxode2setIndParPtr
#> <pointer: 0x7fecbeefc7c0>
#> 
#> $rxode2getIndParPtr
#> <pointer: 0x7fecbeefc820>
#> 
#> $rxode2getIndNallTimes
#> <pointer: 0x7fecbeefc870>
#> 
#> $rxode2setIndIdx
#> <pointer: 0x7fecbeefc880>
#> 
#> $rxode2getIndIx
#> <pointer: 0x7fecbeefc890>
#> 
#> $rxode2getIndEvid
#> <pointer: 0x7fecbeefc920>
#> 
#> $rxode2getIndLhs
#> <pointer: 0x7fecbeefc950>
#> 
#> $rxode2getIndNdoses
#> <pointer: 0x7fecbeefc960>
#> 
#> $rxode2getIndNevid2
#> <pointer: 0x7fecbeefc970>
#> 
#> $rxode2setIndSolve
#> <pointer: 0x7fecbeefc980>
#> 
#> $rxode2getIndSolve
#> <pointer: 0x7fecbeefc990>
#> 
#> $rxode2getIndDv
#> <pointer: 0x7fecbeefc9a0>
#> 
#> $rxode2getIndYj
#> <pointer: 0x7fecbeefc9e0>
#> 
#> $rxode2getIndLimit
#> <pointer: 0x7fecbeefc9f0>
#> 
#> $rxode2getIndCens
#> <pointer: 0x7fecbeefca30>
#> 
#> $rxode2getIndIdx
#> <pointer: 0x7fecbeefca60>
#> 
#> $rxode2getOpNeq
#> <pointer: 0x7fecbeefca70>
#> 
#> $rxode2setOpNeq
#> <pointer: 0x7fecbeefca80>
#> 
#> $rxode2hasOpBadSolve
#> <pointer: 0x7fecbeefca90>
#> 
#> $rxode2getOpNlin
#> <pointer: 0x7fecbeefcaa0>
#> 
#> $rxode2getOpCores
#> <pointer: 0x7fecbeefcab0>
#> 
#> $rxode2getOpNlhs
#> <pointer: 0x7fecbeefcac0>
#> 
#> $rxode2getOpStiff
#> <pointer: 0x7fecbeefcad0>
#> 
#> $rxode2resetOpBadSolve
#> <pointer: 0x7fecbeefcae0>
#> 
#> $rxode2getRxNsub
#> <pointer: 0x7fecbeefcaf0>
#> 
#> $rxode2hasRxLimit
#> <pointer: 0x7fecbeefcb00>
#> 
#> $rxode2hasRxCens
#> <pointer: 0x7fecbeefcb10>
#> 
#> $rxode2getRxNall
#> <pointer: 0x7fecbeefcb20>
#> 
#> $rxode2getRxNobs
#> <pointer: 0x7fecbeefcb30>
#> 
#> $rxode2getRxNobs2
#> <pointer: 0x7fecbeefcb40>
#> 
#> $rxode2getOpIndSolve
#> <pointer: 0x7fecbeefcb60>
#> 
#> $rxode2getRxNpars
#> <pointer: 0x7fecbeefcb50>
#> 
#> $rxode2getIndMixest
#> <pointer: 0x7fecbeefc8c0>
#> 
#> $rxode2setIndMixest
#> <pointer: 0x7fecbeefc8d0>
#> 
#> $rxode2mexpit
#> <pointer: 0x7fecbee84c70>
#> 
#> $rxode2getRxMixnum
#> <pointer: 0x7fecbeefc8f0>
#> 
#> $rxode2setRxMixnum
#> <pointer: 0x7fecbeefc900>
#> 
```
