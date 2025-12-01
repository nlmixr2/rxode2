# Convert event data to trial duration data A helper function to create a custom event table. The observation time will start from the first event time (baseline) and end at trial duration. The interval is the spacing between each observation.

Convert event data to trial duration data A helper function to create a
custom event table. The observation time will start from the first event
time (baseline) and end at trial duration. The interval is the spacing
between each observation.

## Usage

``` r
toTrialDuration(ev, trialEnd, interval, writeDir = NULL)
```

## Arguments

- ev:

  event data

- trialEnd:

  extend trial duration. Must be same time unit as event data

- interval:

  observation interval. Must be same time unit as event data

- writeDir:

  if not NULL, write the output to a csv file

## Author

Omar Elashkar

## Examples

``` r
 # Create event table with unique time for each ID
 ev = et(data.frame(id = rep(1:10, 3),  time = runif(min = 10, max = 20, n = 30)))

 # select the duration and spacing interval (assuming time is in years)
 toTrialDuration(ev, trialEnd = 1.5, interval = 0.2)
#> ── EventTable with 80 records ──
#> 0 dosing records (see value$get.dosing(); add with add.dosing or et)
#> 80 observation times (see value$get.sampling(); add with add.sampling or et)
#> ── First part of value: ──
#> # A tibble: 80 × 4
#>       id  time   amt evid         
#>    <int> <dbl> <dbl> <evid>       
#>  1     1  12.5    NA 0:Observation
#>  2     1  12.7    NA 0:Observation
#>  3     1  12.9    NA 0:Observation
#>  4     1  13.1    NA 0:Observation
#>  5     1  13.3    NA 0:Observation
#>  6     1  13.5    NA 0:Observation
#>  7     1  13.7    NA 0:Observation
#>  8     1  13.9    NA 0:Observation
#>  9     2  14.1    NA 0:Observation
#> 10     2  14.3    NA 0:Observation
#> # ℹ 70 more rows
```
