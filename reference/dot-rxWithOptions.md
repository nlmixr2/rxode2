# Temporarily set options then restore them while running code

Temporarily set options then restore them while running code

## Usage

``` r
.rxWithOptions(ops, code)
```

## Arguments

- ops:

  list of options that will be temporarily set for the `code`

- code:

  The code to run during the sink

## Value

value of code

## Examples

``` r
.rxWithOptions(list(digits = 21), {
  print(pi)
})
#> [1] 3.141592653589793116

print(pi)
#> [1] 3.141593
```
