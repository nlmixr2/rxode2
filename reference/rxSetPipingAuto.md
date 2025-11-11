# Set the variables for the model piping automatic covarite selection

Set the variables for the model piping automatic covarite selection

## Usage

``` r
rxSetPipingAuto(
  thetamodelVars = rex::rex(or("tv", "t", "pop", "POP", "Pop", "TV", "T", "cov", "err",
    "eff")),
  covariateExceptions = rex::rex(start, or("wt", "sex", "crcl", "kout"), end),
  etaParts = c("eta", "ETA", "Eta", "ppv", "PPV", "Ppv", "iiv", "Iiv", "bsv", "Bsv",
    "BSV", "bpv", "Bpv", "BPV", "psv", "PSV", "Psv")
)
```

## Arguments

- thetamodelVars:

  This is the prefixes for the theta model variables in a regular
  expression

- covariateExceptions:

  This is a regular expression of covariates that should always be
  covariates

- etaParts:

  This is the list of eta prefixes/post-fixes that identify a variable
  as a between subject variability

## Value

Nothing, called for side effects

## Details

This is called once at startup to set the defaults, though you can
change this if you wish so that piping can work differently for your
individual setup

## Author

Matthew L. Fidler
