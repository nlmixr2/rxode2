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
#>           [,1]     [,2]     [,3]     [,4]     [,5]
#> [1,]  5.239064 4.959084 4.784399 1.950413 7.508406
#> [2,] -0.729417 2.737759 3.105391 1.732354 5.136244
#> [3,]  2.244704 4.933124 2.527947 6.117024 5.025811
#> [4,]  1.250616 1.306642 3.442411 1.696451 3.083392

set.seed(414)
rxRmvn(4, 1:d, mcov)
#>           [,1]     [,2]     [,3]     [,4]     [,5]
#> [1,]  5.239064 4.959084 4.784399 1.950413 7.508406
#> [2,] -0.729417 2.737759 3.105391 1.732354 5.136244
#> [3,]  2.244704 4.933124 2.527947 6.117024 5.025811
#> [4,]  1.250616 1.306642 3.442411 1.696451 3.083392

set.seed(414)
rxRmvn(4, 1:d, mcov, ncores = 2) # r.v. generated on the second core are different
#>            [,1]     [,2]     [,3]      [,4]     [,5]
#> [1,]  5.2390635 3.899504 4.328324  3.397540 7.333269
#> [2,]  4.9556513 1.535007 6.226424 -1.660476 9.503750
#> [3,] -0.7294170 1.786539 2.970677  3.075479 6.426285
#> [4,]  0.5203225 3.469242 2.169031  5.685795 3.868330

###### Here we create the matrix that will hold the simulated
#  random variables upfront.
A <- matrix(NA, 4, d)
class(A) <- "numeric" # This is important. We need the elements of A to be of class "numeric".

set.seed(414)
rxRmvn(A, 1:d, mcov, ncores = 2) # This returns NULL ...
A # ... but the result is here
#>            [,1]     [,2]     [,3]      [,4]     [,5]
#> [1,]  5.2390635 3.899504 4.328324  3.397540 7.333269
#> [2,]  4.9556513 1.535007 6.226424 -1.660476 9.503750
#> [3,] -0.7294170 1.786539 2.970677  3.075479 6.426285
#> [4,]  0.5203225 3.469242 2.169031  5.685795 3.868330

## You can also simulate from a truncated normal:

rxRmvn(10, 1:d, mcov, lower = 1:d - 1, upper = 1:d + 1)
#>            [,1]     [,2]     [,3]     [,4]     [,5]
#>  [1,] 0.7779842 1.038996 2.759178 4.282005 5.301866
#>  [2,] 1.0658284 1.160767 2.936772 3.239619 5.139684
#>  [3,] 0.6575943 1.526952 2.765851 3.275490 5.682884
#>  [4,] 1.1905019 2.503944 3.203830 4.859300 4.448689
#>  [5,] 0.9396633 2.055747 2.986239 4.921664 4.416667
#>  [6,] 1.0256213 2.457655 2.886997 3.260261 5.012083
#>  [7,] 0.7407370 1.599032 2.808889 4.736703 5.839267
#>  [8,] 0.9956325 2.763909 2.824578 3.970260 4.563017
#>  [9,] 1.1153760 1.887343 2.801929 4.055279 4.981264
#> [10,] 0.8481447 2.220434 2.658241 4.772268 4.854159


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
