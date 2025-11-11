# Combine transformations and error structures

Combine error information to figure out what transformation is being
applied for the current endpoint

## Usage

``` r
rxErrTypeCombine(oldErrType, newErrType)
```

## Arguments

- oldErrType:

  This is the old transformation, by default is zero representing no
  prior transformation. This parameter is first to allow piping. When
  the parameter `addTransform` is missing and `oldErrType` is a
  character value, this functions swaps `oldErrType` and `addTransform`
  and assigns `oldErrType` to zero assuming that there is no prior
  distribution.

- newErrType:

  This is the new distribution that is being "added" to the current
  transformation. These assumes the inputs are in the preferred
  distribution name, as determined by
  [`rxPreferredDistributionName()`](https://nlmixr2.github.io/rxode2/reference/rxPreferredDistributionName.md)

## Value

The new transformation as a factor

## Author

Matthew Fidler

## Examples

``` r
rxErrTypeCombine("probitNorm")
#> $transform
#> [1] probit
#> 10 Levels: boxCox yeoJohnson untransformed lnorm logit ... probit + boxCox
#> 
#> $errType
#> [1] add
#> Levels: add prop pow add + prop add + pow none
#> 
#> $errTypeF
#> [1] none
#> Levels: untransformed transformed f none
#> 
#> $addProp
#> [1] default
#> Levels: combined1 combined2 default
#> 
#> attr(,"class")
#> [1] "rxCombinedErrorList"

rxErrTypeCombine("probitNorm") %>%
  rxErrTypeCombine("boxCox")
#> $transform
#> [1] probit + boxCox
#> 10 Levels: boxCox yeoJohnson untransformed lnorm logit ... probit + boxCox
#> 
#> $errType
#> [1] add
#> Levels: add prop pow add + prop add + pow none
#> 
#> $errTypeF
#> [1] none
#> Levels: untransformed transformed f none
#> 
#> $addProp
#> [1] default
#> Levels: combined1 combined2 default
#> 
#> attr(,"class")
#> [1] "rxCombinedErrorList"

```
