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
#> <pointer: 0x7f449f2eb1a0>
#> 
#> $rxode2rxParProgress
#> <pointer: 0x7f449f4e4f40>
#> 
#> $rxode2getRxSolve_
#> <pointer: 0x7f449f4e6180>
#> 
#> $rxode2indSolve
#> <pointer: 0x7f449f4f6cc0>
#> 
#> $rxode2getTime
#> <pointer: 0x7f449f4e61b0>
#> 
#> $rxode2isRstudio
#> <pointer: 0x7f449f5008c0>
#> 
#> $rxode2iniSubjectE
#> <pointer: 0x7f449f4edbe0>
#> 
#> $rxode2sortIds
#> <pointer: 0x7f449f50c4b0>
#> 
#> $getSolvingOptionsInd
#> <pointer: 0x7f449f4fd9a0>
#> 
#> $rxode2getUpdateInis
#> <pointer: 0x7f449f4fd9b0>
#> 
#> $rxode2_rxode2_rxModelVars_
#> <pointer: 0x7f449f2e5b60>
#> 
#> $rxode2_par_solve
#> <pointer: 0x7f449f4f5a80>
#> 
#> $rxode2rxGetId
#> <pointer: 0x7f449f4e4040>
#> 
#> $rxode2getIndLambda
#> <pointer: 0x7f449f4fd9f0>
#> 
#> $rxode2getIndLambdaYj
#> <pointer: 0x7f449f4fda00>
#> 
#> $rxode2getIndLogitLow
#> <pointer: 0x7f449f4fda10>
#> 
#> $rxode2getIndLogitHi
#> <pointer: 0x7f449f4fda20>
#> 
#> $rxode2setIndParPtr
#> <pointer: 0x7f449f4fda30>
#> 
#> $rxode2getIndParPtr
#> <pointer: 0x7f449f4fda90>
#> 
#> $rxode2getIndNallTimes
#> <pointer: 0x7f449f4fdae0>
#> 
#> $rxode2setIndIdx
#> <pointer: 0x7f449f4fdaf0>
#> 
#> $rxode2getIndIx
#> <pointer: 0x7f449f4fdb00>
#> 
#> $rxode2getIndEvid
#> <pointer: 0x7f449f4fdb90>
#> 
#> $rxode2getIndLhs
#> <pointer: 0x7f449f4fdbc0>
#> 
#> $rxode2getIndNdoses
#> <pointer: 0x7f449f4fdbd0>
#> 
#> $rxode2getIndNevid2
#> <pointer: 0x7f449f4fdbe0>
#> 
#> $rxode2setIndSolve
#> <pointer: 0x7f449f4fdbf0>
#> 
#> $rxode2getIndSolve
#> <pointer: 0x7f449f4fdc00>
#> 
#> $rxode2getIndDv
#> <pointer: 0x7f449f4fdc10>
#> 
#> $rxode2getIndYj
#> <pointer: 0x7f449f4fdc50>
#> 
#> $rxode2getIndLimit
#> <pointer: 0x7f449f4fdc60>
#> 
#> $rxode2getIndCens
#> <pointer: 0x7f449f4fdca0>
#> 
#> $rxode2getIndIdx
#> <pointer: 0x7f449f4fdcd0>
#> 
#> $rxode2getOpNeq
#> <pointer: 0x7f449f4fdce0>
#> 
#> $rxode2setOpNeq
#> <pointer: 0x7f449f4fdcf0>
#> 
#> $rxode2hasOpBadSolve
#> <pointer: 0x7f449f4fdd00>
#> 
#> $rxode2getOpNlin
#> <pointer: 0x7f449f4fdd10>
#> 
#> $rxode2getOpCores
#> <pointer: 0x7f449f4fdd20>
#> 
#> $rxode2getOpNlhs
#> <pointer: 0x7f449f4fdd30>
#> 
#> $rxode2getOpStiff
#> <pointer: 0x7f449f4fdd40>
#> 
#> $rxode2resetOpBadSolve
#> <pointer: 0x7f449f4fdd50>
#> 
#> $rxode2getRxNsub
#> <pointer: 0x7f449f4fdd60>
#> 
#> $rxode2hasRxLimit
#> <pointer: 0x7f449f4fdd70>
#> 
#> $rxode2hasRxCens
#> <pointer: 0x7f449f4fdd80>
#> 
#> $rxode2getRxNall
#> <pointer: 0x7f449f4fdd90>
#> 
#> $rxode2getRxNobs
#> <pointer: 0x7f449f4fdda0>
#> 
#> $rxode2getRxNobs2
#> <pointer: 0x7f449f4fddb0>
#> 
#> $rxode2getOpIndSolve
#> <pointer: 0x7f449f4fddd0>
#> 
#> $rxode2getRxNpars
#> <pointer: 0x7f449f4fddc0>
#> 
#> $rxode2getIndMixest
#> <pointer: 0x7f449f4fdb30>
#> 
#> $rxode2setIndMixest
#> <pointer: 0x7f449f4fdb40>
#> 
#> $rxode2mexpit
#> <pointer: 0x7f449f485e80>
#> 
#> $rxode2getRxMixnum
#> <pointer: 0x7f449f4fdb60>
#> 
#> $rxode2setRxMixnum
#> <pointer: 0x7f449f4fdb70>
#> 
```
