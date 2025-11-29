# Temporarily set options then restore them while running code

Temporarily set options then restore them while running code

## Usage

``` r
.rxWithWd(wd, code)
```

## Arguments

- wd:

  working directory to temporarily set the system to while evaluating
  the code

- code:

  The code to run during the sink

## Value

value of code

## Examples

``` r
.rxWithWd(tempdir(), {
  getwd()
})
#> [1] "/tmp/RtmprbpElK"

getwd()
#> [1] "/home/runner/work/rxode2/rxode2/docs/reference"
```
