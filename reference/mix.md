# Specify a mixture model of variables

Specify a mixture model of variables

## Usage

``` r
mix(...)
```

## Arguments

- ...:

  Arguments to the mixture model.

  The first call to the mixture model function takes an odd number of
  arguments (at least 3).

  For example the model we could have:

  cl = mix(cl1, p1, cl2, p2, cl3)

  Here there is a mixture of three clearance variables, `cl1`, `cl2`,
  and `cl3`, at a probability of `p1`, `p2`, and the last one is assumed
  to be `1 - p1 - p2`.

  For simulations this is selected randomly. For estimations this is
  selected by the data for each individual.

  After the first call when the number of populations has been
  established, you can also call the mixture model with the number of
  populations, for example:

  v = mix(v1, v2, v3)

  The ui function will translate this to the following model:

  v = mix(v1, p1, v2, p2, v3)

  This is because the first call to `mix()` sets the probabilities. In
  rxode2/nlmixr2 these probabilities should be conserved between the
  models. These probabilities also have to be defined in the ini block
  directly.

## Value

The mixture model replacement for the underlying rxode2 model.

## Author

Matthew L. Fidler

## Examples

``` r
# This is an example of a mixture model
# Where there are 2 different clearance populations

one.cmt <- function() {
  ini({
    tka <- 0.45 # Log Ka
    tcl1 <- log(c(0, 2.7, 100)) # Log Cl
    tcl2 <- log(c(0, 0.1, 120)) # Log Cl
    tv <- 3.45; label("log V")
    p1 <- 0.3
    eta.ka ~ 0.6
    eta.cl ~ 0.3
    eta.v ~ 0.1
    add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka)
    # This is the example mixture model
    cl <- mix(exp(tcl1 + eta.cl), p1, exp(tcl2 + eta.cl))
    v <- exp(tv + eta.v)
    me <- mixest # This is the assigned mixture estimate
    mn <- mixnum # This is the number of mixture estimate in the model
    # This is the uniform mixture estimate used in simualtion to
    # determine the population
    mu <- mixunif
    linCmt() ~ add(add.sd)
  })
}

# \donttest{

s <- rxSolve(one.cmt, et(amt=320, ii=12, addl=2, cmt=1) %>%
                      et(seq(0, 72)) %>%
                      et(id=1:20))
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  
#>  

plot(s, ipredSim)


# }
```
