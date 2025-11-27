# Add/Create C functions for use in rxode2

Add/Create C functions for use in rxode2

## Usage

``` r
rxFun(name, args, cCode)

rxRmFun(name)
```

## Arguments

- name:

  This can either give the name of the user function or be a simple R
  function that you wish to convert to C. If you have rxode2 convert the
  R function to C, the name of the function will match the function name
  provided and the number of arguments will match the R function
  provided. Hence, if you are providing an R function for conversion to
  C, the rest of the arguments are implied.

- args:

  This gives the arguments of the user function

- cCode:

  This is the C-code for the new function

## Examples

``` r
# \donttest{
# Right now rxode2 is not aware of the function fun
# Therefore it cannot translate it to symengine or
# Compile a model with it.

try(rxode2("a=fun(a,b,c)"))
#>  
#>  
#> Error : syntax errors (see above)

# Note for this approach to work, it cannot interfere with C
# function names or reserved rxode2 special terms.  Therefore
# f(x) would not work since f is an alias for bioavailability.

fun <- "
double fun(double a, double b, double c) {
  return a*a+b*a+c;
}
" # C-code for function

rxFun("fun", c("a", "b", "c"), fun) ## Added function

# Now rxode2 knows how to translate this function to symengine

rxToSE("fun(a,b,c)")
#> [1] "fun(a,b,c)"

# And will take a central difference when calculating derivatives

rxFromSE("Derivative(fun(a,b,c),a)")
#> [1] "(fun((a)+6.05545445239334e-06,b,c)-fun(a,b,c))/6.05545445239334e-06"

## Of course, you could specify the derivative table manually
rxD("fun", list(
  function(a, b, c) {
    paste0("2*", a, "+", b)
  },
  function(a, b, c) {
    return(a)
  },
  function(a, b, c) {
    return("0.0")
  }
))

rxFromSE("Derivative(fun(a,b,c),a)")
#> [1] "2*a+b"

# You can also remove the functions by `rxRmFun`

rxRmFun("fun")

# you can also use R functions directly in rxode2


gg <- function(x, y) {
  x + y
}

f <- rxode2({
 z = gg(x, y)
})
#>  
#>  


e <- et(1:10) %>% as.data.frame()

e$x <- 1:10
e$y <- 21:30

rxSolve(f, e)
#> Warning: not thread safe method, using 1 core
#> ── Solved rxode2 object ──
#> ── Parameters (value$params): ──
#> # A tibble: 1 × 0
#> ── Initial Conditions (value$inits): ──
#> named numeric(0)
#> ── First part of data (object): ──
#> # A tibble: 10 × 4
#>    time     z     x     y
#>   <dbl> <dbl> <dbl> <dbl>
#> 1     1    22     1    21
#> 2     2    24     2    22
#> 3     3    26     3    23
#> 4     4    28     4    24
#> 5     5    30     5    25
#> 6     6    32     6    26
#> # ℹ 4 more rows

# Note that since it touches R, it can only run single-threaded.
# There are also requirements for the function:
#
# 1. It accepts one value per argument (numeric)
#
# 2. It returns one numeric value

# If it is a simple function (like gg) you can also convert it to C
# using rxFun and load it into rxode2

rxFun(gg)
#> → finding duplicate expressions in d(gg)/d(x)...
#> → finding duplicate expressions in d(gg)/d(y)...
#> converted R function 'gg' to C (will now use in rxode2)
#> converted R function 'rx_gg_d_x' to C (will now use in rxode2)
#> converted R function 'rx_gg_d_y' to C (will now use in rxode2)
#> Added derivative table for 'gg'

rxSolve(f, e)
#> compiled with R user function 'gg'; now there is a clashing C user function
#> triggered a recompile to use the C user function (they are always preferred)
#>  
#>  
#> ── Solved rxode2 object ──
#> ── Parameters (value$params): ──
#> # A tibble: 1 × 0
#> ── Initial Conditions (value$inits): ──
#> named numeric(0)
#> ── First part of data (object): ──
#> # A tibble: 10 × 4
#>    time     z     x     y
#>   <dbl> <dbl> <dbl> <dbl>
#> 1     1    22     1    21
#> 2     2    24     2    22
#> 3     3    26     3    23
#> 4     4    28     4    24
#> 5     5    30     5    25
#> 6     6    32     6    26
#> # ℹ 4 more rows

# to stop the recompile simply reassign the function
f <- rxode2(f)
#>  
#>  

rxSolve(f, e)
#> ── Solved rxode2 object ──
#> ── Parameters (value$params): ──
#> # A tibble: 1 × 0
#> ── Initial Conditions (value$inits): ──
#> named numeric(0)
#> ── First part of data (object): ──
#> # A tibble: 10 × 4
#>    time     z     x     y
#>   <dbl> <dbl> <dbl> <dbl>
#> 1     1    22     1    21
#> 2     2    24     2    22
#> 3     3    26     3    23
#> 4     4    28     4    24
#> 5     5    30     5    25
#> 6     6    32     6    26
#> # ℹ 4 more rows

rxRmFun("gg")
rm(gg)
rm(f)


# You can also automatically convert a R function to R code (and
# calculate first derivatives)

fun <- function(a, b, c) {
  a^2+b*a+c
}

rxFun(fun)
#> → finding duplicate expressions in d(fun)/d(a)...
#> → finding duplicate expressions in d(fun)/d(b)...
#> → finding duplicate expressions in d(fun)/d(c)...
#> converted R function 'fun' to C (will now use in rxode2)
#> converted R function 'rx_fun_d_a' to C (will now use in rxode2)
#> converted R function 'rx_fun_d_b' to C (will now use in rxode2)
#> converted R function 'rx_fun_d_c' to C (will now use in rxode2)
#> Added derivative table for 'fun'

# You can see the R code if you want with rxC

message(rxC("fun"))
#> double fun(double  a, double  b, double  c) {
#>   double _lastValue=NA_REAL;
#>   _lastValue = R_pow_di(a,2)+b*a+c;
#>   return _lastValue;
#> }

# you can also remove both the function and the
# derivatives with rxRmFun("fun")

rxRmFun("fun")

# }
```
