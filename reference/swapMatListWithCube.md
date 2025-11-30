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
#> a 1.0961937 0.8784799
#> b 0.8784799 2.1915760
#> 
#> [[2]]
#>           a         b
#> a 0.8793426 0.3164880
#> b 0.3164880 0.7359173
#> 
#> [[3]]
#>           a          b
#> a  1.099929 -0.1181670
#> b -0.118167  0.6062742
#> 

# Convert to cube
matCube <- swapMatListWithCube(matLst)
print(matCube)
#> , , 1
#> 
#>           a         b
#> a 1.0961937 0.8784799
#> b 0.8784799 2.1915760
#> 
#> , , 2
#> 
#>           a         b
#> a 0.8793426 0.3164880
#> b 0.3164880 0.7359173
#> 
#> , , 3
#> 
#>           a          b
#> a  1.099929 -0.1181670
#> b -0.118167  0.6062742
#> 

# Convert back to list
matLst2 <- swapMatListWithCube(matCube)
print(matLst2)
#> [[1]]
#>           a         b
#> a 1.0961937 0.8784799
#> b 0.8784799 2.1915760
#> 
#> [[2]]
#>           a         b
#> a 0.8793426 0.3164880
#> b 0.3164880 0.7359173
#> 
#> [[3]]
#>           a          b
#> a  1.099929 -0.1181670
#> b -0.118167  0.6062742
#> 
```
