# Event translation for rxode2

Event translation for rxode2

## Usage

``` r
etTrans(
  inData,
  obj,
  addCmt = FALSE,
  dropUnits = FALSE,
  allTimeVar = FALSE,
  keepDosingOnly = FALSE,
  combineDvid = NULL,
  keep = character(0),
  addlKeepsCov = FALSE,
  addlDropSs = TRUE,
  ssAtDoseTime = TRUE,
  iCov = NULL
)
```

## Arguments

- inData:

  Data frame to translate

- obj:

  object where model variables can be extracted from

- addCmt:

  Add compartment to data frame (default `FALSE`).

- dropUnits:

  Boolean to drop the units (default `FALSE`).

- allTimeVar:

  Treat all covariates as if they were time-varying

- keepDosingOnly:

  keep the individuals who only have dosing records and any trailing
  dosing records after the last observation.

- combineDvid:

  is a boolean indicating if rxode2 will use `DVID` on observation
  records to change the `cmt` value; Useful for multiple-endpoint nlmixr
  models. By default this is determined by
  `option("rxode2.combine.dvid")` and if the option has not been set,
  this is `TRUE`. This typically does not affect rxode2 simulations.

- keep:

  This is a named vector of items you want to keep in the final rxode2
  dataset. For added rxode2 event records (if seen), last observation
  carried forward will be used.

- addlKeepsCov:

  This determines if the additional dosing items repeats the dose only
  (`FALSE`) or keeps the covariates at the record of the dose (`TRUE`)

- addlDropSs:

  When there are steady state doses with an `addl` specification the
  steady state flag is dropped with repeated doses (when `TRUE`) or
  retained (when `FALSE`)

- ssAtDoseTime:

  Boolean that when `TRUE` back calculates the steady concentration at
  the actual time of dose, otherwise when `FALSE` the doses are shifted

- iCov:

  A data frame of individual non-time varying covariates to combine with
  the `events` dataset. The `iCov` dataset has one covariate per ID and
  should match the event table

## Value

Object for solving in rxode2
