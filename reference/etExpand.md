# Expand additional doses

Expand additional doses

## Usage

``` r
etExpand(et)
```

## Arguments

- et:

  Event table to expand additional doses for.

## Value

New event table with `addl` doses expanded

## Author

Matthew Fidler

## Examples

``` r
ev <- et(amt = 3, ii = 24, until = 240)
print(ev)
#> ── EventTable with 1 records ──
#> 1 dosing records (see $get.dosing(); add with add.dosing or et)
#> 0 observation times (see $get.sampling(); add with add.sampling or et)
#> multiple doses in `addl` columns, expand with $expand(); or etExpand()
#> ── First part of : ──
#> # A tibble: 1 × 5
#>    time   amt    ii  addl evid        
#>   <dbl> <dbl> <dbl> <int> <evid>      
#> 1     0     3    24    10 1:Dose (Add)
etExpand(ev) # expands event table, but doesn't modify it
#> ── EventTable with 11 records ──
#> 11 dosing records (see value$get.dosing(); add with add.dosing or et)
#> 0 observation times (see value$get.sampling(); add with add.sampling or et)
#> ── First part of value: ──
#> # A tibble: 11 × 4
#>     time   amt    ii evid        
#>    <dbl> <dbl> <dbl> <evid>      
#>  1     0     3     0 1:Dose (Add)
#>  2    24     3     0 1:Dose (Add)
#>  3    48     3     0 1:Dose (Add)
#>  4    72     3     0 1:Dose (Add)
#>  5    96     3     0 1:Dose (Add)
#>  6   120     3     0 1:Dose (Add)
#>  7   144     3     0 1:Dose (Add)
#>  8   168     3     0 1:Dose (Add)
#>  9   192     3     0 1:Dose (Add)
#> 10   216     3     0 1:Dose (Add)
#> 11   240     3     0 1:Dose (Add)

print(ev)
#> ── EventTable with 1 records ──
#> 1 dosing records (see $get.dosing(); add with add.dosing or et)
#> 0 observation times (see $get.sampling(); add with add.sampling or et)
#> multiple doses in `addl` columns, expand with $expand(); or etExpand()
#> ── First part of : ──
#> # A tibble: 1 × 5
#>    time   amt    ii  addl evid        
#>   <dbl> <dbl> <dbl> <int> <evid>      
#> 1     0     3    24    10 1:Dose (Add)

ev$expand() ## Expands the current event table and saves it in ev
```
