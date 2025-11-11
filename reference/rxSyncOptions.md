# Sync options with rxode2 variables

Accessing rxode2 options via getOption slows down solving. This allows
the options to be synced with variables.

## Usage

``` r
rxSyncOptions(setDefaults = c("none", "permissive", "strict"))
```

## Arguments

- setDefaults:

  This will setup rxode2's default solving options with the following
  options:

  - `"none"` leave the options alone

  - `"permissive"` This is a permissive option set similar to R language
    specifications.

  - `"strict"` This is a strict option set similar to the original
    rxode2(). It requires semicolons at the end of lines and equals for
    assignment

## Value

nothing; called for side effects

## Author

Matthew L. Fidler
