# Swaps the matrix list with a cube

Swaps the matrix list with a cube

## Usage

``` r
swapMatListWithCube(matrixListOrCube)
```

## Arguments

- matrixListOrCube:

  Either a list of 2-dimensional matrices or a cube of matrices

## Value

A list or a cube (opposite format as input)

## Author

Matthew L. Fidler

## Examples

``` r
# Create matrix list
matLst <- cvPost(10, lotri::lotri(a+b~c(1, 0.25, 1)), 3)
print(matLst)
#> [[1]]
#>           a         b
#> a 0.9910437 0.4805643
#> b 0.4805643 1.1112637
#> 
#> [[2]]
#>            a          b
#> a  1.2580106 -0.8023558
#> b -0.8023558  2.1846885
#> 
#> [[3]]
#>            a          b
#> a  2.1956504 -0.5782201
#> b -0.5782201  1.2333343
#> 

# Convert to cube
matCube <- swapMatListWithCube(matLst)
print(matCube)
#> , , 1
#> 
#>           a         b
#> a 0.9910437 0.4805643
#> b 0.4805643 1.1112637
#> 
#> , , 2
#> 
#>            a          b
#> a  1.2580106 -0.8023558
#> b -0.8023558  2.1846885
#> 
#> , , 3
#> 
#>            a          b
#> a  2.1956504 -0.5782201
#> b -0.5782201  1.2333343
#> 

# Convert back to list
matLst2 <- swapMatListWithCube(matCube)
print(matLst2)
#> [[1]]
#>           a         b
#> a 0.9910437 0.4805643
#> b 0.4805643 1.1112637
#> 
#> [[2]]
#>            a          b
#> a  1.2580106 -0.8023558
#> b -0.8023558  2.1846885
#> 
#> [[3]]
#>            a          b
#> a  2.1956504 -0.5782201
#> b -0.5782201  1.2333343
#> 
```
