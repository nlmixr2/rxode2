# rxode2 4.0.3

- Adds options needed so that `nlmixr2est` can fix the ODR violation
  rule.  In this particular test it was caused by loading/unloading
  the same dll and/or unloading, removing and then reloading/compiling the same
  code/dll. Updating this is the first step needed for fixing `nlmixr2est`.
