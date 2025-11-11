# rxode2 progress bar functions

`rxProgress` sets up the progress bar

## Usage

``` r
rxProgress(num, core = 0L)

rxTick()

rxProgressStop(clear = TRUE)

rxProgressAbort(error = "Aborted calculation")
```

## Arguments

- num:

  Tot number of operations to track

- core:

  Number of cores to show. If below 1, don't show number of cores

- clear:

  Boolean telling if you should clear the progress bar after completion
  (as if it wasn't displayed). By default this is TRUE

- error:

  With rxProgressAbort this is the error that is displayed

## Value

All return NULL invisibly.

## Details

`rxTick` is a progress bar tick

`rxProgressStop` stop progress bar

`rxProgressAbort` shows an abort if `rxProgressStop` wasn't called.

## Author

Matthew L. Fidler

## Examples

``` r
f <- function() {
  on.exit({
    rxProgressAbort()
  })
  rxProgress(100)
  for (i in 1:100) {
    rxTick()
    Sys.sleep(1 / 100)
  }
  rxProgressStop()
}
# \donttest{
f()
# }
```
