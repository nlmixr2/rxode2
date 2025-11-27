# rxode2 Jacobian specification and Stiff Systems

## Stiff ODEs with Jacobian Specification

Occasionally, you may come across a [**stiff** differential
equation](https://en.wikipedia.org/wiki/Stiff_equation), that is a
differential equation that is numerically unstable and small variations
in parameters cause different solutions to the ODEs. One way to tackle
this is to choose a stiff-solver, or hybrid stiff solver (like the
default LSODA). Typically this is enough. However exact Jacobian
solutions may increase the stability of the ODE. (Note the Jacobian is
the derivative of the ODE specification with respect to each variable).
In rxode2 you can specify the Jacobian with the
`df(state)/dy(variable)=` statement. A classic ODE that has stiff
properties under various conditions is the [Van der
Pol](http://www.ece.northwestern.edu/local-apps/matlabhelp/techdoc/math_anal/diffeq6.md)
differential equations.

In rxode2 these can be specified by the following:

``` r
library(rxode2)
```

    ## rxode2 5.0.0 using 2 threads (see ?getRxThreads)
    ##   no cache: create with `rxCreateCache()`

``` r
Vtpol2 <- function() {
  ini({
    mu <- 1 ## nonstiff; 10 moderately stiff; 1000 stiff
  })
  model({
    d/dt(y)       <- dy
    d/dt(dy)      <- mu*(1-y^2)*dy - y
    ## Jacobian
    df(y)/dy(dy)  <- 1
    df(dy)/dy(y)  <- -2*dy*mu*y - 1
    df(dy)/dy(dy) <- mu*(1-y^2)
    ## Initial conditions
    y(0)          <- 2
    dy(0)         <- 0
  })
}

et <- et(0, 10, length.out=200) %>%
  et(amt=0)

s1 <- Vtpol2 %>%  solve(et, method="lsoda")
```

    ## i parameter labels from comments are typically ignored in non-interactive mode

    ## i Need to run with the source intact to parse comments

``` r
print(s1)
```

    ## -- Solved rxode2 object --
    ## -- Parameters ($params): --
    ## mu 
    ##  1 
    ## -- Initial Conditions ($inits): --
    ##  y dy 
    ##  2  0 
    ## -- First part of data (object): --
    ## # A tibble: 200 x 3
    ##     time     y      dy
    ##    <dbl> <dbl>   <dbl>
    ## 1 0       2     0     
    ## 2 0.0503  2.00 -0.0933
    ## 3 0.101   1.99 -0.173 
    ## 4 0.151   1.98 -0.242 
    ## 5 0.201   1.97 -0.302 
    ## 6 0.251   1.95 -0.353 
    ## # i 194 more rows

While this is not stiff at mu=1, mu=1000 is a stiff system

``` r
s2 <- Vtpol2 %>%  solve(c(mu=1000), et)
```

    ## i parameter labels from comments are typically ignored in non-interactive mode

    ## i Need to run with the source intact to parse comments

``` r
print(s2)
```

    ## -- Solved rxode2 object --
    ## -- Parameters ($params): --
    ##   mu 
    ## 1000 
    ## -- Initial Conditions ($inits): --
    ##  y dy 
    ##  2  0 
    ## -- First part of data (object): --
    ## # A tibble: 200 x 3
    ##     time     y        dy
    ##    <dbl> <dbl>     <dbl>
    ## 1 0       2     0       
    ## 2 0.0503  2.00 -0.000667
    ## 3 0.101   2.00 -0.000667
    ## 4 0.151   2.00 -0.000667
    ## 5 0.201   2.00 -0.000667
    ## 6 0.251   2.00 -0.000667
    ## # i 194 more rows

While this is easy enough to do, it is a bit tedious. If you have rxode2
setup appropriately, you can use the computer algebra system sympy to
calculate the Jacobian automatically.

This is done by the rxode2 option `calcJac` option:

``` r
Vtpol <- function() {
  ini({
    mu <- 1 ## nonstiff; 10 moderately stiff; 1000 stiff
  })
  model({
    d/dt(y)       <- dy
    d/dt(dy)      <- mu*(1-y^2)*dy - y
    y(0)          <- 2
    dy(0)         <- 0
  })
}

Vtpol <- Vtpol()

# you can also use $symengineModelPrune if there is if/else blocks
# that need to be converted:
Vtpol <- rxode2(Vtpol$symengineModelNoPrune,  calcJac=TRUE)
```

    ## > pruning branches (`if`/`else`)...

    ## v done

    ## > loading into symengine environment...

    ## v done

    ## > calculate jacobian

    ## [====|====|====|====|====|====|====|====|====|====] 0:00:00

``` r
summary(Vtpol)
```

    ## rxode2 5.0.0 model named rx_2e62c6684900a51d5d8c18b69ffc68f1 model (ready). 
    ## DLL: /tmp/RtmpVFanKs/rxode2/rx_2e62c6684900a51d5d8c18b69ffc68f1__.rxd/rx_2e62c6684900a51d5d8c18b69ffc68f1_.so
    ## NULL
    ## -- rxode2 Model Syntax --
    ## rxode2({
    ##     cmt(y)
    ##     cmt(dy)
    ##     d/dt(y) = dy
    ##     d/dt(dy) = -y + mu * dy * (1 - Rx_pow_di(y, 2))
    ##     y(0) = 2
    ##     dy(0) = 0
    ##     df(y)/dy(y) = 0
    ##     df(dy)/dy(y) = -1 - 2 * y * mu * dy
    ##     df(y)/dy(dy) = 1
    ##     df(dy)/dy(dy) = mu * (1 - Rx_pow_di(y, 2))
    ##     df(y)/dy(mu) = 0
    ##     df(dy)/dy(mu) = dy * (1 - Rx_pow_di(y, 2))
    ## })
