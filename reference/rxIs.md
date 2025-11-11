# Check the type of an object using Rcpp

Check the type of an object using Rcpp

## Usage

``` r
rxIs(obj, cls)
```

## Arguments

- obj:

  Object to check

- cls:

  Type of class. Only s3 classes for lists/environments and primitive
  classes are checked. For matrix types they are distinguished as
  `numeric.matrix`, `integer.matrix`, `logical.matrix`, and
  `character.matrix` as well as the traditional `matrix` class.
  Additionally checks for `event.data.frame` which is an `data.frame`
  object with `time`, `evid` and `amt`. (UPPER, lower or Title cases
  accepted)

## Value

A boolean indicating if the object is a member of the class.

## Author

Matthew L. Fidler
