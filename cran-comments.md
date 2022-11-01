# CRAN Comments

This submission is meant to replace the `rxode2` `2.0.11` submission currently in the CRAN queue.

This is because it has some unintended suggested packages that were not declared, see:

https://github.com/nlmixr2/rxode2/issues/350

This submission is because `nlmixr2parse` is currently ABI linked to `rxode2`.

In addition:

- This version break the ABI requirement between `roxde2()` and
  `rxode2parse()` (so in theory this will not need to be resubmitted
  as often)

- Give a more meaningful error for 'rxode2' ui models with only error expressions 

- The new `rxode2parse` will fix the `sprintf` exclusion shown on CRAN.

