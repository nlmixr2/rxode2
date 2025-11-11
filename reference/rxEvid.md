# EVID formatting for tibble and other places.

This is to make an EVID more readable by non pharmacometricians. It
displays what each means and allows it to be displayed in a tibble.

## Usage

``` r
rxEvid(x)

as.rxEvid(x)

# S3 method for class 'rxEvid'
c(x, ...)

# S3 method for class 'rxEvid'
x[...]

# S3 method for class 'rxEvid'
as.character(x, ...)

# S3 method for class 'rxEvid'
x[[...]]

# S3 method for class 'rxEvid'
units(x) <- value

# S3 method for class 'rxRateDur'
c(x, ...)

# S3 method for class 'rxEvid'
format(x, ...)

# S3 method for class 'rxRateDur'
format(x, ...)

# S3 method for class 'rxEvid'
print(x, ...)
```

## Arguments

- x:

  Item to be converted to a rxode2 EVID specification.

- ...:

  Other parameters

- value:

  It will be an error to set units for evid

## Value

rxEvid specification

## Examples

``` r
rxEvid(1:7)
#> 1:Dose (Add)                
#> 2:Other                     
#> 3:Reset                     
#> 4:Reset&Dose
#> 5:Replace                   
#> 6:Multiply                  
#> 7:Transit                    
```
