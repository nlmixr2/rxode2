# Preserved seed and possibly set the seed

Preserved seed and possibly set the seed

## Usage

``` r
rxWithSeed(
  seed,
  code,
  rxseed = rxGetSeed(),
  kind = "default",
  normal.kind = "default",
  sample.kind = "default"
)

rxWithPreserveSeed(code)
```

## Arguments

- seed:

  R seed to use for the session

- code:

  Is the code to evaluate

- rxseed:

  is the rxode2 seed that is being preserved

- kind:

  character or `NULL`. If `kind` is a character string, set R's RNG to
  the kind desired. Use `"default"` to return to the R default. See
  ‘Details’ for the interpretation of `NULL`.

- normal.kind:

  character string or `NULL`. If it is a character string, set the
  method of Normal generation. Use `"default"` to return to the R
  default. `NULL` makes no change.

- sample.kind:

  character string or `NULL`. If it is a character string, set the
  method of discrete uniform generation (used in
  [`sample`](https://rdrr.io/r/base/sample.html), for instance). Use
  `"default"` to return to the R default. `NULL` makes no change.

## Value

returns whatever the code is returning

## See also

rxGetSeed, rxSetSeed

## Examples

``` r
rxGetSeed()
#> [1] -1
rxWithSeed(1, {
   print(rxGetSeed())
   rxnorm()
   print(rxGetSeed())
   rxnorm()
}, rxseed=3)
#> [1] 3
#> [1] 5
#> [1] 0.03291887
```
