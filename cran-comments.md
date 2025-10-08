# rxode2 4.1.1

- Stacking for multiple-endpoint `ipredSim` now matches
  multiple-endpoint `sim`; Issue #929

- Fix occasional `$props` that threw an error with empty properties
  (when using properties like `tad0()`); Issue #924

- Allow mixture models `mix()` to be loaded with `rxS()` as a step to
  support mixtures in nlmixr2's focei; Issue #933.

- Identify the correct transformation type for `iov` variables (#936)

- Fix multiple compartment simulation edge cases where simulations
  were not being performed (#939)

- When referencing `cmt` in models, the variable is forced to be `CMT`
  (related to #939)
