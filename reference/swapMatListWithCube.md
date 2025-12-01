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
#>            a          b
#> a 0.72028904 0.05119307
#> b 0.05119307 0.95463558
#> 
#> [[2]]
#>             a           b
#> a 1.765731465 0.005011756
#> b 0.005011756 2.082224576
#> 
#> [[3]]
#>           a         b
#> a 0.8416058 0.4161693
#> b 0.4161693 1.2559567
#> 

# Convert to cube
matCube <- swapMatListWithCube(matLst)
print(matCube)
#> , , 1
#> 
#>            a          b
#> a 0.72028904 0.05119307
#> b 0.05119307 0.95463558
#> 
#> , , 2
#> 
#>             a           b
#> a 1.765731465 0.005011756
#> b 0.005011756 2.082224576
#> 
#> , , 3
#> 
#>           a         b
#> a 0.8416058 0.4161693
#> b 0.4161693 1.2559567
#> 

# Convert back to list
matLst2 <- swapMatListWithCube(matCube)
print(matLst2)
#> [[1]]
#>            a          b
#> a 0.72028904 0.05119307
#> b 0.05119307 0.95463558
#> 
#> [[2]]
#>             a           b
#> a 1.765731465 0.005011756
#> b 0.005011756 2.082224576
#> 
#> [[3]]
#>           a         b
#> a 0.8416058 0.4161693
#> b 0.4161693 1.2559567
#> 
```
