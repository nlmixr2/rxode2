# rxode2 4.0.3

- Adds options needed so that `nlmixr2est` can fix the ODR violation
  rule.  In this particular test it was caused by loading/unloading
  the same dll and/or unloading, removing and then reloading/compiling the same
  code/dll. Updating this is the first step needed for fixing `nlmixr2est`.


With this fix, and the fix for nlmixr2est I have tested with rhub that
the `nlmixr2ext` package succeeds on the mac m1 platform:

https://github.com/nlmixr2/nlmixr2est/actions/runs/16487459462/job/46615031571
