# Plot rxode2 objects

Plot rxode2 objects

## Usage

``` r
# S3 method for class 'rxSolve'
plot(x, y, ..., log = "", xlab = "Time", ylab = "")

# S3 method for class 'rxSolveConfint1'
plot(x, y, ..., xlab = "Time", ylab = "", log = "")

# S3 method for class 'rxSolveConfint2'
plot(x, y, ..., xlab = "Time", ylab = "", log = "")
```

## Arguments

- x:

  rxode2 object to plot

- y:

  Compartments or left-hand-side values to plot either as a bare name or
  as a character vector

- ...:

  Ignored

- log:

  Should "" (neither x nor y), "x", "y", or "xy" (or "yx") be log-scale?

- xlab, ylab:

  The x and y axis labels

## Value

A ggplot2 object

## See also

Other rxode2 plotting:
[`rxTheme()`](https://nlmixr2.github.io/rxode2/reference/rxTheme.md)
