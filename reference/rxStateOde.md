# Get the ODE states only

Get the ODE states only

## Usage

``` r
rxStateOde(obj)
```

## Arguments

- obj:

  rxode2 object

## Value

ODE states only

## Author

Matthew L. Fidler

## Examples

``` r
mod <- rxode2({
  Cp <- linCmt(Cl, V, Q2, V2, Q3, V3)
  ke0 <- log(2)/(50)
  d/dt(Ce) <- (Cp-Ce)*ke0
})
#>  
#>  

rxStateOde(mod)
#> [1] "Ce"

rxState(mod)
#> [1] "Ce"          "central"     "peripheral1" "peripheral2"

mod <- rxode2({
  Cp <- linCmt(Cl, V, Q2, V2, Q3, V3, ka)
  ke0 <- log(2)/(50)
  d/dt(Ce) <- (Cp-Ce)*ke0
}, linCmtSens="linCmtB")
#>  
#>  

rxStateOde(mod)
#> [1] "Ce"

rxState(mod)
#>  [1] "Ce"                         "depot"                     
#>  [3] "central"                    "peripheral1"               
#>  [5] "peripheral2"                "rx__sens_central_BY_p1"    
#>  [7] "rx__sens_central_BY_v1"     "rx__sens_central_BY_p2"    
#>  [9] "rx__sens_central_BY_p3"     "rx__sens_central_BY_p4"    
#> [11] "rx__sens_central_BY_p5"     "rx__sens_central_BY_ka"    
#> [13] "rx__sens_peripheral1_BY_p1" "rx__sens_peripheral1_BY_v1"
#> [15] "rx__sens_peripheral1_BY_p2" "rx__sens_peripheral1_BY_p3"
#> [17] "rx__sens_peripheral1_BY_p4" "rx__sens_peripheral1_BY_p5"
#> [19] "rx__sens_peripheral1_BY_ka" "rx__sens_peripheral2_BY_p1"
#> [21] "rx__sens_peripheral2_BY_v1" "rx__sens_peripheral2_BY_p2"
#> [23] "rx__sens_peripheral2_BY_p3" "rx__sens_peripheral2_BY_p4"
#> [25] "rx__sens_peripheral2_BY_p5" "rx__sens_peripheral2_BY_ka"
#> [27] "rx__sens_depot_BY_ka"      
```
