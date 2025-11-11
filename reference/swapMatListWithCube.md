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
#> a 2.4264748 0.7671728
#> b 0.7671728 0.8645639
#> 
#> [[2]]
#>           a          b
#> a  1.099929 -0.1181670
#> b -0.118167  0.6062742
#> 
#> [[3]]
#>           a         b
#> a 0.7727525 0.0775470
#> b 0.0775470 0.8274925
#> 

# Convert to cube
matCube <- swapMatListWithCube(matLst)
print(matCube)
#> , , 1
#> 
#>           a         b
#> a 2.4264748 0.7671728
#> b 0.7671728 0.8645639
#> 
#> , , 2
#> 
#>           a          b
#> a  1.099929 -0.1181670
#> b -0.118167  0.6062742
#> 
#> , , 3
#> 
#>           a         b
#> a 0.7727525 0.0775470
#> b 0.0775470 0.8274925
#> 

# Convert back to list
matLst2 <- swapMatListWithCube(matCube)
print(matLst2)
#> [[1]]
#>           a         b
#> a 2.4264748 0.7671728
#> b 0.7671728 0.8645639
#> 
#> [[2]]
#>           a          b
#> a  1.099929 -0.1181670
#> b -0.118167  0.6062742
#> 
#> [[3]]
#>           a         b
#> a 0.7727525 0.0775470
#> b 0.0775470 0.8274925
#> 
```
