# CRAN Comments

This is the exact same submission as before in hopes that the ubuntu server has update `rxode2parse`

This is submitted because `nlmixr2parse` is currently ABI linked t `rxode2`.

In addition:

- This version break the ABI requirement between `roxde2()` and
  `rxode2parse()` (so in theory this will not need to be resubmitted
  as often)

- Give a more meaningful error for 'rxode2' ui models with only error expressions 

- The new `rxode2parse` will fix the `sprintf` exclusion shown on CRAN.

