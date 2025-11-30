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
#>             a           b
#> a 1.765731465 0.005011756
#> b 0.005011756 2.082224576
#> 
#> [[2]]
#>           a         b
#> a 0.8416058 0.4161693
#> b 0.4161693 1.2559567
#> 
#> [[3]]
#>           a         b
#> a 1.2700422 0.1112434
#> b 0.1112434 0.7577530
#> 

# Convert to cube
matCube <- swapMatListWithCube(matLst)
print(matCube)
#> , , 1
#> 
#>             a           b
#> a 1.765731465 0.005011756
#> b 0.005011756 2.082224576
#> 
#> , , 2
#> 
#>           a         b
#> a 0.8416058 0.4161693
#> b 0.4161693 1.2559567
#> 
#> , , 3
#> 
#>           a         b
#> a 1.2700422 0.1112434
#> b 0.1112434 0.7577530
#> 

# Convert back to list
matLst2 <- swapMatListWithCube(matCube)
print(matLst2)
#> [[1]]
#>             a           b
#> a 1.765731465 0.005011756
#> b 0.005011756 2.082224576
#> 
#> [[2]]
#>           a         b
#> a 0.8416058 0.4161693
#> b 0.4161693 1.2559567
#> 
#> [[3]]
#>           a         b
#> a 1.2700422 0.1112434
#> b 0.1112434 0.7577530
#> 
```
