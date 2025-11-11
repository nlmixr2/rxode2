# Handle the single error for normal or t distributions

Handle the single error for normal or t distributions

## Usage

``` r
.handleSingleErrTypeNormOrTFoceiBase(
  env,
  pred1,
  errNum = 1L,
  rxPredLlik = TRUE
)
```

## Arguments

- env:

  Environment for the parsed model

- pred1:

  The `data.frame` of the current error

- errNum:

  The number of the error specification in the nlmixr2 model

- rxPredLlik:

  A boolean indicating if the log likelihood should be calculated for
  non-normal distributions. By default `TRUE`.

## Value

A list of the lines added. The lines will contain

- `rx_yj_` which is an integer that corresponds to the transformation
  type.

- `rx_lambda_` is the transformation lambda

- `rx_low_` The lower boundary of the transformation

- `rx_hi_` The upper boundary of the transformation

- `rx_pred_f_` The prediction function

- `rx_pred_` The transformed prediction function

- `rx_r_` The transformed variance

## Author

Matthew Fidler
