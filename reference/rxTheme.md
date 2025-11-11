# rxTheme is the ggplot2 theme for rxode2 plots

rxTheme is the ggplot2 theme for rxode2 plots

## Usage

``` r
rxTheme(
  base_size = 11,
  base_family = "",
  base_line_size = base_size/22,
  base_rect_size = base_size/22,
  grid = TRUE
)
```

## Arguments

- base_size:

  base font size, given in pts.

- base_family:

  base font family

- base_line_size:

  base size for line elements

- base_rect_size:

  base size for rect elements

- grid:

  a Boolean indicating if the grid is on (`TRUE`) or off (`FALSE`). This
  could also be a character indicating `x` or `y`.

## Value

ggplot2 theme used in rxode2

## See also

Other rxode2 plotting:
[`plot.rxSolve()`](https://nlmixr2.github.io/rxode2/reference/plot.rxSolve.md)
