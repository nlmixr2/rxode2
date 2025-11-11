# Assert properties of the rxUi models

Assert properties of the rxUi models

## Usage

``` r
assertRxUi(ui, extra = "", .var.name = .vname(ui))

assertRxUiPrediction(ui, extra = "", .var.name = .vname(ui))

assertRxUiIovNoCor(ui, extra = "", .var.name = .vname(ui))

assertRxUiNoMix(ui, extra = "", .var.name = .vname(ui))

assertRxUiSingleEndpoint(ui, extra = "", .var.name = .vname(ui))

assertRxUiTransformNormal(ui, extra = "", .var.name = .vname(ui))

assertRxUiNormal(ui, extra = "", .var.name = .vname(ui))

assertRxUiMuRefOnly(ui, extra = "", .var.name = .vname(ui))

assertRxUiEstimatedResiduals(ui, extra = "", .var.name = .vname(ui))

assertRxUiPopulationOnly(ui, extra = "", .var.name = .vname(ui))

assertRxUiMixedOnly(ui, extra = "", .var.name = .vname(ui))

assertRxUiRandomOnIdOnly(ui, extra = "", .var.name = .vname(ui))
```

## Arguments

- ui:

  Model to check

- extra:

  Extra text to append to the error message (like "for focei")

- .var.name:

  \[`character(1)`\]  
  Name of the checked object to print in assertions. Defaults to the
  heuristic implemented in
  [`vname`](https://mllg.github.io/checkmate/reference/vname.html).

## Value

the rxUi model

## Details

These functions have different types of assertions

- `assertRxUi` – Make sure this is a proper rxode2 model (if not throw
  error)

- `assertRxUiSingleEndpoint` – Make sure the rxode2 model is only a
  single endpoint model (if not throw error)

- `assertRxUiTransformNormal` – Make sure that the model residual
  distribution is normal or transformably normal

- `assertRxUiNormal` – Make sure that the model residual distribution is
  normal

- `assertRxUiEstimatedResiduals` – Make sure that the residual error
  parameters are estimated (not modeled).

- `assertRxUiPopulationOnly` – Make sure the model is the population
  only model (no mixed effects)

- `assertRxUiMixedOnly` – Make sure the model is a mixed effect model
  (not a population effect, only)

- `assertRxUiPrediction` – Make sure the model has predictions

- `assertRxUiMuRefOnly` – Make sure that all the parameters are
  mu-referenced

- `assertRxUiRandomOnIdOnly` – Make sure there are only random effects
  at the ID level

- `assertRxUiIovNoCor` – Make sure that the IOV model does not have any
  correlations

- `assertRxUiNoMix` – Make sure that the model does not have a mixture
  model inside it

## See also

Other Assertions:
[`assertCompartmentExists()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentExists.md),
[`assertCompartmentName()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentName.md),
[`assertCompartmentNew()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentNew.md),
[`assertVariableExists()`](https://nlmixr2.github.io/rxode2/reference/assertVariableExists.md),
[`assertVariableNew()`](https://nlmixr2.github.io/rxode2/reference/assertVariableNew.md),
[`testIniDf()`](https://nlmixr2.github.io/rxode2/reference/testIniDf.md),
[`testRxUnbounded()`](https://nlmixr2.github.io/rxode2/reference/testRxUnbounded.md)

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{
one.cmt <- function() {
 ini({
   tka <- 0.45; label("Ka")
   tcl <- log(c(0, 2.7, 100)); label("Cl")
   tv <- 3.45; label("V")
   eta.ka ~ 0.6
   eta.cl ~ 0.3
   eta.v ~ 0.1
   add.sd <- 0.7
 })
 model({
   ka <- exp(tka + eta.ka)
   cl <- exp(tcl + eta.cl)
   v <- exp(tv + eta.v)
   linCmt() ~ add(add.sd)
 })
}

assertRxUi(one.cmt)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
# assertRxUi(rnorm) # will fail

assertRxUiSingleEndpoint(one.cmt)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
# }
```
