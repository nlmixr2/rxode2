# Simulate from a (truncated) multivariate normal

This is simulated with the fast, thread-safe threefry simulator and can
use multiple cores to generate the random deviates.

## Usage

``` r
rxRmvn(
  n,
  mu = NULL,
  sigma,
  lower = -Inf,
  upper = Inf,
  ncores = 1,
  isChol = FALSE,
  keepNames = TRUE,
  a = 0.4,
  tol = 2.05,
  nlTol = 1e-10,
  nlMaxiter = 100L
)
```

## Arguments

- n:

  Number of random row vectors to be simulated OR the matrix to use for
  simulation (faster).

- mu:

  mean vector

- sigma:

  Covariance matrix for multivariate normal or a list of covariance
  matrices. If a list of covariance matrix, each matrix will simulate
  `n` matrices and combine them to a full matrix

- lower:

  is a vector of the lower bound for the truncated multivariate norm

- upper:

  is a vector of the upper bound for the truncated multivariate norm

- ncores:

  Number of cores used in the simulation

- isChol:

  A boolean indicating if `sigma` is a cholesky decomposition of the
  covariance matrix.

- keepNames:

  Keep the names from either the mean or covariance matrix.

- a:

  threshold for switching between methods; They can be tuned for maximum
  speed; There are three cases that are considered:

  case 1: a \< l \< u

  case 2: l \< u \< -a

  case 3: otherwise

  where l=lower and u = upper

- tol:

  When case 3 is used from the above possibilities, the tol value
  controls the acceptance rejection and inverse-transformation;

  When abs(u-l)\>tol, uses accept-reject from randn

- nlTol:

  Tolerance for newton line-search

- nlMaxiter:

  Maximum iterations for newton line-search

## Value

If `n==integer` (default) the output is an (n x d) matrix where the i-th
row is the i-th simulated vector.

If `is.matrix(n)` then the random vector are store in `n`, which is
provided by the user, and the function returns `NULL` invisibly.

## References

John K. Salmon, Mark A. Moraes, Ron O. Dror, and David E. Shaw (2011).
Parallel Random Numbers: As Easy as 1, 2, 3. D. E. Shaw Research, New
York, NY 10036, USA.

The thread safe multivariate normal was inspired from the `mvnfast`
package by Matteo Fasiolo <https://CRAN.R-project.org/package=mvnfast>

The concept of the truncated multivariate normal was taken from Zdravko
Botev Botev (2017)
[doi:10.1111/rssb.12162](https://doi.org/10.1111/rssb.12162) and Botev
and L'Ecuyer (2015)
[doi:10.1109/WSC.2015.7408180](https://doi.org/10.1109/WSC.2015.7408180)
and converted to thread safe simulation;

## Author

Matthew Fidler, Zdravko Botev and some from Matteo Fasiolo

## Examples

``` r
## From mvnfast
## Unlike mvnfast, uses threefry simulation

d <- 5
mu <- 1:d

# Creating covariance matrix
tmp <- matrix(rnorm(d^2), d, d)
mcov <- tcrossprod(tmp, tmp)


set.seed(414)
rxRmvn(4, 1:d, mcov)
#>            [,1]     [,2]     [,3]      [,4]     [,5]
#> [1,]  5.2707258 2.214025 8.168778 4.4070379 6.877293
#> [2,] -0.7423343 3.576793 6.292510 3.8644209 4.419573
#> [3,]  2.2540013 3.829004 3.153675 5.6056976 4.855277
#> [4,]  1.2524875 1.269743 3.569747 0.9088226 4.692126

set.seed(414)
rxRmvn(4, 1:d, mcov)
#>            [,1]     [,2]     [,3]      [,4]     [,5]
#> [1,]  5.2707258 2.214025 8.168778 4.4070379 6.877293
#> [2,] -0.7423343 3.576793 6.292510 3.8644209 4.419573
#> [3,]  2.2540013 3.829004 3.153675 5.6056976 4.855277
#> [4,]  1.2524875 1.269743 3.569747 0.9088226 4.692126

set.seed(414)
rxRmvn(4, 1:d, mcov, ncores = 2) # r.v. generated on the second core are different
#>            [,1]       [,2]     [,3]     [,4]     [,5]
#> [1,]  5.2707258  1.3074571 5.218499 3.900613 7.624714
#> [2,]  4.9851967 -0.5606283 9.972658 3.066770 8.264613
#> [3,] -0.7423343  2.7629370 4.680670 5.007808 5.204537
#> [4,]  0.5167397  3.5193389 2.134059 4.771087 3.984363

###### Here we create the matrix that will hold the simulated
#  random variables upfront.
A <- matrix(NA, 4, d)
class(A) <- "numeric" # This is important. We need the elements of A to be of class "numeric".

set.seed(414)
rxRmvn(A, 1:d, mcov, ncores = 2) # This returns NULL ...
A # ... but the result is here
#>            [,1]       [,2]     [,3]     [,4]     [,5]
#> [1,]  5.2707258  1.3074571 5.218499 3.900613 7.624714
#> [2,]  4.9851967 -0.5606283 9.972658 3.066770 8.264613
#> [3,] -0.7423343  2.7629370 4.680670 5.007808 5.204537
#> [4,]  0.5167397  3.5193389 2.134059 4.771087 3.984363

## You can also simulate from a truncated normal:

rxRmvn(10, 1:d, mcov, lower = 1:d - 1, upper = 1:d + 1)
#>            [,1]     [,2]     [,3]     [,4]     [,5]
#>  [1,] 1.0653597 2.133787 2.946986 4.287025 4.817676
#>  [2,] 1.2074339 2.603705 3.066479 4.373026 4.958052
#>  [3,] 0.6601250 2.163003 2.777882 3.298368 5.450638
#>  [4,] 1.1891312 1.803473 3.188579 4.272920 4.176795
#>  [5,] 0.8202332 1.982613 2.780687 3.742501 4.803707
#>  [6,] 0.7800441 1.544914 3.233315 4.346761 4.647580
#>  [7,] 0.9219472 1.580757 2.811885 4.969403 5.721241
#>  [8,] 1.2795017 2.621243 2.792514 3.260261 4.962780
#>  [9,] 1.0539189 1.885283 3.351143 4.736703 4.661864
#> [10,] 0.9404057 2.057468 3.130754 4.055279 4.661319


# You can also simulate from different matrices (if they match
# dimensions) by using a list of matrices.

matL <- lapply(1:4, function(...) {
  tmp <- matrix(rnorm(d^2), d, d)
  tcrossprod(tmp, tmp)
})


rxRmvn(4, setNames(1:d, paste0("a", 1:d)), matL)
#>               a1         a2         a3         a4       a5
#>  [1,]  3.8274046  2.1035743  0.5028009  4.5098402 5.189021
#>  [2,] -0.9871330  3.8835057  6.0910523  5.1594862 4.132973
#>  [3,]  3.4611290  6.9818905  2.4784421  5.1440327 8.310619
#>  [4,] -2.6283293 -5.0190437  2.8076771 -0.5884730 1.060942
#>  [5,]  0.3451536  0.5718889  3.7057607  4.3772555 3.829498
#>  [6,] -1.1689889  1.9051966  4.0675628  2.0180889 5.571646
#>  [7,] -1.7018538 -0.7786124  5.4574824  3.9037163 6.126236
#>  [8,] -3.9766942 -0.8582446  5.4820712 -0.5706566 4.047896
#>  [9,]  0.8420574 -0.4096991  3.9764994  4.7655857 4.167223
#> [10,]  1.3644751  5.2068446  1.3430378  3.6687705 8.175402
#> [11,]  1.5705584  3.1771677  1.9140454  6.9388794 5.757319
#> [12,]  0.6404211 11.0776336  5.3814729 10.6267879 6.349803
#> [13,]  0.2352059  0.8413157 -0.6291679  3.7003445 4.517157
#> [14,]  3.7622838 -1.0087429  0.1514493  0.7595107 4.988589
#> [15,]  0.4968318  3.1695539  6.2533925  3.9780854 5.781146
#> [16,] -3.7275917  2.3933661  1.3926983  6.3064293 2.905874
```
