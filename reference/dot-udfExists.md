# See if the UI function exists in given environment.

If other functions have been declared, make sure they exist too.

## Usage

``` r
.udfExists(fun, nargs, envir, doList = TRUE)
```

## Arguments

- fun:

  Function to check

- nargs:

  Number of args to check

- envir:

  Environment to check

- doList:

  A boolean to see if the functions in .udfEnv\$fun should be checked
  too. By default TRUE, but this is called recursively for each function
  (and set to FALSE)

## Value

logical declaring if the udf function exists in this environment

## Author

Matthew L. Fidler
