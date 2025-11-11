# Demote the error type

Demote the error type

## Usage

``` r
rxDemoteAddErr(errType)
```

## Arguments

- errType:

  Error type factor

## Value

Demoted Error Type

## Author

Matthew Fidler

## Examples

``` r
rxErrTypeCombine("add") %>%
  rxErrTypeCombine("prop")
#> $transform
#> [1] untransformed
#> 10 Levels: boxCox yeoJohnson untransformed lnorm logit ... probit + boxCox
#> 
#> $errType
#> [1] add + prop
#> Levels: add prop pow add + prop add + pow none
#> 
#> $errTypeF
#> [1] untransformed
#> Levels: untransformed transformed f none
#> 
#> $addProp
#> [1] default
#> Levels: combined1 combined2 default
#> 
#> attr(,"class")
#> [1] "rxCombinedErrorList"

# This removes the internal additive error
rxErrTypeCombine("add") %>%
  rxErrTypeCombine("prop") %>%
  rxDemoteAddErr()
#> $transform
#> [1] untransformed
#> 10 Levels: boxCox yeoJohnson untransformed lnorm logit ... probit + boxCox
#> 
#> $errType
#> [1] prop
#> Levels: add prop pow add + prop add + pow none
#> 
#> $errTypeF
#> [1] untransformed
#> Levels: untransformed transformed f none
#> 
#> $addProp
#> [1] default
#> Levels: combined1 combined2 default
#> 
#> attr(,"class")
#> [1] "rxCombinedErrorList"

# This is used for logitNorm(NA), the additive portion is stripped
```
