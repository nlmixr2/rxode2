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
#> <pointer: 0x7f93e5eeb1a0>
#> 
#> $rxode2rxParProgress
#> <pointer: 0x7f93e60e4f40>
#> 
#> $rxode2getRxSolve_
#> <pointer: 0x7f93e60e6180>
#> 
#> $rxode2indSolve
#> <pointer: 0x7f93e60f6cc0>
#> 
#> $rxode2getTime
#> <pointer: 0x7f93e60e61b0>
#> 
#> $rxode2isRstudio
#> <pointer: 0x7f93e61008c0>
#> 
#> $rxode2iniSubjectE
#> <pointer: 0x7f93e60edbe0>
#> 
#> $rxode2sortIds
#> <pointer: 0x7f93e610c4b0>
#> 
#> $getSolvingOptionsInd
#> <pointer: 0x7f93e60fd9a0>
#> 
#> $rxode2getUpdateInis
#> <pointer: 0x7f93e60fd9b0>
#> 
#> $rxode2_rxode2_rxModelVars_
#> <pointer: 0x7f93e5ee5b60>
#> 
#> $rxode2_par_solve
#> <pointer: 0x7f93e60f5a80>
#> 
#> $rxode2rxGetId
#> <pointer: 0x7f93e60e4040>
#> 
#> $rxode2getIndLambda
#> <pointer: 0x7f93e60fd9f0>
#> 
#> $rxode2getIndLambdaYj
#> <pointer: 0x7f93e60fda00>
#> 
#> $rxode2getIndLogitLow
#> <pointer: 0x7f93e60fda10>
#> 
#> $rxode2getIndLogitHi
#> <pointer: 0x7f93e60fda20>
#> 
#> $rxode2setIndParPtr
#> <pointer: 0x7f93e60fda30>
#> 
#> $rxode2getIndParPtr
#> <pointer: 0x7f93e60fda90>
#> 
#> $rxode2getIndNallTimes
#> <pointer: 0x7f93e60fdae0>
#> 
#> $rxode2setIndIdx
#> <pointer: 0x7f93e60fdaf0>
#> 
#> $rxode2getIndIx
#> <pointer: 0x7f93e60fdb00>
#> 
#> $rxode2getIndEvid
#> <pointer: 0x7f93e60fdb90>
#> 
#> $rxode2getIndLhs
#> <pointer: 0x7f93e60fdbc0>
#> 
#> $rxode2getIndNdoses
#> <pointer: 0x7f93e60fdbd0>
#> 
#> $rxode2getIndNevid2
#> <pointer: 0x7f93e60fdbe0>
#> 
#> $rxode2setIndSolve
#> <pointer: 0x7f93e60fdbf0>
#> 
#> $rxode2getIndSolve
#> <pointer: 0x7f93e60fdc00>
#> 
#> $rxode2getIndDv
#> <pointer: 0x7f93e60fdc10>
#> 
#> $rxode2getIndYj
#> <pointer: 0x7f93e60fdc50>
#> 
#> $rxode2getIndLimit
#> <pointer: 0x7f93e60fdc60>
#> 
#> $rxode2getIndCens
#> <pointer: 0x7f93e60fdca0>
#> 
#> $rxode2getIndIdx
#> <pointer: 0x7f93e60fdcd0>
#> 
#> $rxode2getOpNeq
#> <pointer: 0x7f93e60fdce0>
#> 
#> $rxode2setOpNeq
#> <pointer: 0x7f93e60fdcf0>
#> 
#> $rxode2hasOpBadSolve
#> <pointer: 0x7f93e60fdd00>
#> 
#> $rxode2getOpNlin
#> <pointer: 0x7f93e60fdd10>
#> 
#> $rxode2getOpCores
#> <pointer: 0x7f93e60fdd20>
#> 
#> $rxode2getOpNlhs
#> <pointer: 0x7f93e60fdd30>
#> 
#> $rxode2getOpStiff
#> <pointer: 0x7f93e60fdd40>
#> 
#> $rxode2resetOpBadSolve
#> <pointer: 0x7f93e60fdd50>
#> 
#> $rxode2getRxNsub
#> <pointer: 0x7f93e60fdd60>
#> 
#> $rxode2hasRxLimit
#> <pointer: 0x7f93e60fdd70>
#> 
#> $rxode2hasRxCens
#> <pointer: 0x7f93e60fdd80>
#> 
#> $rxode2getRxNall
#> <pointer: 0x7f93e60fdd90>
#> 
#> $rxode2getRxNobs
#> <pointer: 0x7f93e60fdda0>
#> 
#> $rxode2getRxNobs2
#> <pointer: 0x7f93e60fddb0>
#> 
#> $rxode2getOpIndSolve
#> <pointer: 0x7f93e60fddd0>
#> 
#> $rxode2getRxNpars
#> <pointer: 0x7f93e60fddc0>
#> 
#> $rxode2getIndMixest
#> <pointer: 0x7f93e60fdb30>
#> 
#> $rxode2setIndMixest
#> <pointer: 0x7f93e60fdb40>
#> 
#> $rxode2mexpit
#> <pointer: 0x7f93e6085e80>
#> 
#> $rxode2getRxMixnum
#> <pointer: 0x7f93e60fdb60>
#> 
#> $rxode2setRxMixnum
#> <pointer: 0x7f93e60fdb70>
#> 
```
