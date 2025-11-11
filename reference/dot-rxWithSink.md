# With one sink, then release

With one sink, then release

## Usage

``` r
.rxWithSink(file, code)

.rxWithSinkBoth(file, code)
```

## Arguments

- file:

  the path to the file sink while running the `code`

- code:

  The code to run during the sink

## Value

Will return the results of the `code` section

## Details

`.rxWithSink` captures output from `cat`

`.rxWithSinkBoth` captures output from `cat` and `message`

## Author

Matthew Fidler

## Examples

``` r
t <- tempfile()
.rxWithSink(t, cat("message\n"))
cat("cat2\n") # now you can see the cat2
#> cat2
lines <- readLines(t)
unlink(t)
```
