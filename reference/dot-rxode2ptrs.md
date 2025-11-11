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
#> <pointer: 0x7fb1bcaec880>
#> 
#> $rxode2rxParProgress
#> <pointer: 0x7fb1bcce5df0>
#> 
#> $rxode2getRxSolve_
#> <pointer: 0x7fb1bcce6ff0>
#> 
#> $rxode2indSolve
#> <pointer: 0x7fb1bccf7b10>
#> 
#> $rxode2getTime
#> <pointer: 0x7fb1bcce7020>
#> 
#> $rxode2isRstudio
#> <pointer: 0x7fb1bcd02830>
#> 
#> $rxode2iniSubjectE
#> <pointer: 0x7fb1bcceea30>
#> 
#> $rxode2sortIds
#> <pointer: 0x7fb1bcd0e7e0>
#> 
#> $getSolvingOptionsInd
#> <pointer: 0x7fb1bccff960>
#> 
#> $rxode2getUpdateInis
#> <pointer: 0x7fb1bccff970>
#> 
#> $rxode2_rxode2_rxModelVars_
#> <pointer: 0x7fb1bcae7560>
#> 
#> $rxode2_par_solve
#> <pointer: 0x7fb1bccf68d0>
#> 
#> $rxode2rxGetId
#> <pointer: 0x7fb1bcce4ef0>
#> 
#> $rxode2getIndLambda
#> <pointer: 0x7fb1bccff9b0>
#> 
#> $rxode2getIndLambdaYj
#> <pointer: 0x7fb1bccff9c0>
#> 
#> $rxode2getIndLogitLow
#> <pointer: 0x7fb1bccff9d0>
#> 
#> $rxode2getIndLogitHi
#> <pointer: 0x7fb1bccff9e0>
#> 
#> $rxode2setIndParPtr
#> <pointer: 0x7fb1bccff9f0>
#> 
#> $rxode2getIndParPtr
#> <pointer: 0x7fb1bccffa50>
#> 
#> $rxode2getIndNallTimes
#> <pointer: 0x7fb1bccffaa0>
#> 
#> $rxode2setIndIdx
#> <pointer: 0x7fb1bccffab0>
#> 
#> $rxode2getIndIx
#> <pointer: 0x7fb1bccffac0>
#> 
#> $rxode2getIndEvid
#> <pointer: 0x7fb1bccffb50>
#> 
#> $rxode2getIndLhs
#> <pointer: 0x7fb1bccffb80>
#> 
#> $rxode2getIndNdoses
#> <pointer: 0x7fb1bccffb90>
#> 
#> $rxode2getIndNevid2
#> <pointer: 0x7fb1bccffba0>
#> 
#> $rxode2setIndSolve
#> <pointer: 0x7fb1bccffbb0>
#> 
#> $rxode2getIndSolve
#> <pointer: 0x7fb1bccffbc0>
#> 
#> $rxode2getIndDv
#> <pointer: 0x7fb1bccffbd0>
#> 
#> $rxode2getIndYj
#> <pointer: 0x7fb1bccffc10>
#> 
#> $rxode2getIndLimit
#> <pointer: 0x7fb1bccffc20>
#> 
#> $rxode2getIndCens
#> <pointer: 0x7fb1bccffc60>
#> 
#> $rxode2getIndIdx
#> <pointer: 0x7fb1bccffc90>
#> 
#> $rxode2getOpNeq
#> <pointer: 0x7fb1bccffca0>
#> 
#> $rxode2setOpNeq
#> <pointer: 0x7fb1bccffcb0>
#> 
#> $rxode2hasOpBadSolve
#> <pointer: 0x7fb1bccffcc0>
#> 
#> $rxode2getOpNlin
#> <pointer: 0x7fb1bccffcd0>
#> 
#> $rxode2getOpCores
#> <pointer: 0x7fb1bccffce0>
#> 
#> $rxode2getOpNlhs
#> <pointer: 0x7fb1bccffcf0>
#> 
#> $rxode2getOpStiff
#> <pointer: 0x7fb1bccffd00>
#> 
#> $rxode2resetOpBadSolve
#> <pointer: 0x7fb1bccffd10>
#> 
#> $rxode2getRxNsub
#> <pointer: 0x7fb1bccffd20>
#> 
#> $rxode2hasRxLimit
#> <pointer: 0x7fb1bccffd30>
#> 
#> $rxode2hasRxCens
#> <pointer: 0x7fb1bccffd40>
#> 
#> $rxode2getRxNall
#> <pointer: 0x7fb1bccffd50>
#> 
#> $rxode2getRxNobs
#> <pointer: 0x7fb1bccffd60>
#> 
#> $rxode2getRxNobs2
#> <pointer: 0x7fb1bccffd70>
#> 
#> $rxode2getOpIndSolve
#> <pointer: 0x7fb1bccffd90>
#> 
#> $rxode2getRxNpars
#> <pointer: 0x7fb1bccffd80>
#> 
#> $rxode2getIndMixest
#> <pointer: 0x7fb1bccffaf0>
#> 
#> $rxode2setIndMixest
#> <pointer: 0x7fb1bccffb00>
#> 
#> $rxode2mexpit
#> <pointer: 0x7fb1bcc86d30>
#> 
#> $rxode2getRxMixnum
#> <pointer: 0x7fb1bccffb20>
#> 
#> $rxode2setRxMixnum
#> <pointer: 0x7fb1bccffb30>
#> 
```
