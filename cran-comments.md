
- Bug fix for `api`, the censoring function pointer has been updated
  (#801).

- Query `rxode2.verbose.pipe` at run time instead of requiring it to
  be set before loading `rxode2`.

- Have correct values at boundaries for `logit`, `expit`, `probit`,
  and `probitInv` (instead of `NA`). For most cases this does not
  break anything.

- Add a new style of user function that modifies the `ui` while
  parsing or just before using the function (in the presence of
  `data`).

- Used the new user function interface to allow all random functions
  in `rxode2` ui functions to be named.  For example, you can use
  `rxnorm(sd=3)` instead of having to use `rxnorm(0, 3)`, although
  `rxnorm()` still works.
