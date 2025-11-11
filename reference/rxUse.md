# Use model object in your package

Use model object in your package

## Usage

``` r
rxUse(obj, overwrite = TRUE, compress = "bzip2", internal = FALSE)
```

## Arguments

- obj:

  model to save.

- overwrite:

  By default, `use_data()` will not overwrite existing files. If you
  really want to do so, set this to `TRUE`.

- compress:

  Choose the type of compression used by
  [`save()`](https://rdrr.io/r/base/save.html). Should be one of "gzip",
  "bzip2", or "xz".

- internal:

  If this is run internally. By default this is FALSE

## Value

Nothing; This is used for its side effects and shouldn't be called by a
user
