# Add user function to rxode2

This adds a user function to rxode2 that can be called. If needed, these
functions can be differentiated by numerical differences or by adding
the derivatives to rxode2's internal derivative table with rxode2's
`rxD` function

## Usage

``` r
rxFunParse(name, args, cCode)

rxRmFunParse(name)
```

## Arguments

- name:

  This gives the name of the user function

- args:

  This gives the arguments of the user function

- cCode:

  This is the C-code for the new function

## Value

nothing

## Author

Matthew L. Fidler
