# Linear model to replace in rxode2 ui model

Linear model to replace in rxode2 ui model

## Usage

``` r
linMod(
  variable,
  power,
  dv = "dv",
  intercept = TRUE,
  type = c("replace", "before", "after"),
  num = NULL,
  iniDf = NULL,
  data = FALSE,
  mv = FALSE
)

linMod0(..., intercept = FALSE)

linModB(..., type = "before")

linModB0(..., intercept = FALSE, type = "before")

linModA(..., type = "after")

linModA0(..., intercept = FALSE, type = "after")

linModD(..., intercept = TRUE, data = TRUE)

linModD0(..., intercept = FALSE, data = TRUE)

linModM(..., intercept = TRUE, mv = TRUE)

linModM0(..., intercept = FALSE, mv = TRUE)
```

## Arguments

- variable:

  The variable that the rxode2 will be made on.

- power:

  The power of the polynomial that will be generated.

- dv:

  the dependent variable to use to generate the initial estimates from
  the data. If `NULL` query using
  [`rxUdfUiData()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiData.md).

- intercept:

  Boolean that tells if the intercept be generated.

- type:

  the type of linear model replacement to be used.

- num:

  the number the particular model is being generated. If unspecified,
  query using
  [`rxUdfUiNum()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiNum.md).

- iniDf:

  the initialization `data.frame`, if `NULL` query using
  [`rxUdfUiIniDf()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiIniDf.md)

- data:

  logical that tells if the initial estimates of the linear model should
  be estimated from the data.

- mv:

  logical that tell if the model variables need to be used to generate
  model variables.

- ...:

  arguments that are passed to `linMod()` for the other abbreviations of
  `linMod()`

## Value

a list for use in when generating the `rxode2` ui model see
[`rxUdfUi()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUi.md) for
details.

## Functions

- `linMod0()`: linear model without intercept

- `linModB()`: linear model before where it occurs

- `linModB0()`: linear model before where the user function occurs

- `linModA()`: linear model after where the user function occurs

- `linModA0()`: liner model without an intercept placed after where the
  user function occurs

- `linModD()`: linear model where initial estimates are generated from
  the data

- `linModD0()`: linear model where initial estimates are generated from
  the data (no intercept)

- `linModM()`: linear model where the model variables are used to
  generate the model variables

- `linModM0()`: linear model where the model variables are used to
  generate the model variables (no intercept)

## See also

Other User functions:
[`rxUdfUiControl()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiControl.md),
[`rxUdfUiData()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiData.md),
[`rxUdfUiEst()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiEst.md),
[`rxUdfUiIniLhs()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiIniLhs.md),
[`rxUdfUiMv()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiMv.md),
[`rxUdfUiNum()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiNum.md),
[`rxUdfUiParsing()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiParsing.md)

## Author

Matthew L. Fidler

## Examples

``` r
linMod(x, 3)
#> $replace
#> [1] "rx.linMod.x1a+rx.linMod.x1b*x+rx.linMod.x1c*x^2+rx.linMod.x1d*x^3"
#> 
#> $iniDf
#> NULL
#> 
```
