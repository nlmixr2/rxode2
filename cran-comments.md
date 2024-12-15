# rxode2 3.0.3

- Add `logit`/`expit` named expressions, that is `logit(x, high=20)`
  becomes `logit(x, 0, 20)` in ui models.

- Updated random ui models like `rxnorm(sd=10)` to accept complex
  numeric expressions like `rxnorm(sd=10+1)`.

- Updated random ui models to accept complex non-numeric expressions
  like `rxnorm(sd=a+b)`

- Rework the `tad()` and related functions so they use the same
  interface as compartments (this way they do not depend on the order
  of compartments); See #815.  For mu-referencing, Also allow dummy
  variables to ignore state requirements (ie `podo(depot)` in a single
  line will not error when parsing mu-referenced equations).

- Add `getRxNpars` to api.  This allows the development version of
  `babelmixr2` to better check what model is loaded and unload/reload
  as necessary.

- Add `rxUdfUiControl()` to rxode2 user function to get control
  information from something like `nlmixr2`

- Bug fix for tracking time after dose when dosing to 2 compartments
  occur at the exact same time (#804, #819)

- Change `transit()` model so that it uses `tad0()`, `podo0()` and
  related functions for a bit more stable simulation and estimation

- Fix compile flags to work with BH 1.87 (#826)
