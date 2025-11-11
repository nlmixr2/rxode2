# Sample a covariance Matrix from the Posterior Inverse Wishart distribution.

Note this Inverse wishart rescaled to match the original scale of the
covariance matrix.

## Usage

``` r
cvPost(
  nu,
  omega,
  n = 1L,
  omegaIsChol = FALSE,
  returnChol = FALSE,
  type = c("invWishart", "lkj", "separation"),
  diagXformType = c("log", "identity", "variance", "nlmixrSqrt", "nlmixrLog",
    "nlmixrIdentity")
)
```

## Arguments

- nu:

  Degrees of Freedom (Number of Observations) for covariance matrix
  simulation.

- omega:

  Either the estimate of covariance matrix or the estimated standard
  deviations in matrix form each row forming the standard deviation
  simulated values

- n:

  Number of Matrices to sample. By default this is 1. This is only
  useful when `omega` is a matrix. Otherwise it is determined by the
  number of rows in the input `omega` matrix of standard deviations

- omegaIsChol:

  is an indicator of if the omega matrix is in the Cholesky
  decomposition. This is only used when `type="invWishart"`

- returnChol:

  Return the Cholesky decomposition of the covariance matrix sample.
  This is only used when `type="invWishart"`

- type:

  The type of covariance posterior that is being simulated. This can be:

  - `invWishart` The posterior is an inverse wishart; This allows for
    correlations between parameters to be modeled. All the uncertainty
    in the parameter is captured in the degrees of freedom parameter.

  - `lkj` The posterior separates the standard deviation estimates
    (modeled outside and provided in the `omega` argument) and the
    correlation estimates. The correlation estimate is simulated with
    the
    [`rLKJ1()`](https://nlmixr2.github.io/rxode2/reference/rLKJ1.md).
    This simulation uses the relationship `eta=(nu-1)/2`. This is
    relationship based on the proof of the relationship between the
    restricted LKJ-distribution and inverse wishart distribution
    (XXXXXX). Once the correlation posterior is calculated, the
    estimated standard deviations are then combined with the simulated
    correlation matrix to create the covariance matrix.

  - `separation` Like the `lkj` option, this separates out the
    estimation of the correlation and standard deviation. Instead of
    using the `LKJ` distribution to simulate the correlation, it
    simulates the inverse wishart of the identity matrix and converts
    the result to a correlation matrix. This correlation matrix is then
    used with the standard deviation to calculate the simulated
    covariance matrix.

- diagXformType:

  Diagonal transformation type. These could be:

  - `log` The standard deviations are log transformed, so the actual
    standard deviations are exp(omega)

  - `identity` The standard deviations are not transformed. The standard
    deviations are not transformed; They should be positive.

  - `variance` The variances are specified in the `omega` matrix; They
    are transformed into standard deviations.

  - `nlmixrSqrt` These standard deviations come from an nlmixr omega
    matrix where diag(chol(inv(omega))) = x^2

  - `nlmixrLog` These standard deviations come from a nlmixr omega
    matrix omega matrix where diag(chol(solve(omega))) = exp(x)

  - `nlmixrIdentity` These standard deviations come from a nlmixr omega
    matrix omega matrix where diag(chol(solve(omega))) = x

  The nlmixr transformations only make sense when there is no
  off-diagonal correlations modeled.

## Value

a matrix (n=1) or a list of matrices (n \> 1)

## Details

If your covariance matrix is a 1x1 matrix, this uses an scaled inverse
chi-squared which is equivalent to the Inverse Wishart distribution in
the uni-directional case.

In general, the separation strategy is preferred for diagonal matrices.
If the dimension of the matrix is below 10, `lkj` is numerically faster
than `separation` method. However, the `lkj` method has densities too
close to zero (XXXX) when the dimension is above 10. In that case,
though computationally more expensive `separation` method performs
better.

For matrices with modeled covariances, the easiest method to use is the
inverse Wishart which allows the simulation of correlation matrices
(XXXX). This method is more well suited for well behaved matrices, that
is the variance components are not too low or too high. When modeling
nonlinear mixed effects modeling matrices with too high or low variances
are considered sub-optimal in describing a system. With these rules in
mind, it is reasonable to use the inverse Wishart.

## References

Alvarez I, Niemi J and Simpson M. (2014) *Bayesian Inference for a
Covariance Matrix*. Conference on Applied Statistics in Agriculture.

Wang1 Z, Wu Y, and Chu H. (2018) *On Equivalence of the LKJ distribution
and the restricted Wishart distribution*.
\<doi:10.48550/arXiv.1809.047463

## Author

Matthew L.Fidler & Wenping Wang

## Examples

``` r
## Sample a single covariance.
draw1 <- cvPost(3, matrix(c(1, .3, .3, 1), 2, 2))

## Sample 3 covariances
set.seed(42)
draw3 <- cvPost(3, matrix(c(1, .3, .3, 1), 2, 2), n = 3)

## Sample 3 covariances, but return the cholesky decomposition
set.seed(42)
draw3c <- cvPost(3, matrix(c(1, .3, .3, 1), 2, 2), n = 3, returnChol = TRUE)

## Sample 3 covariances with lognormal standard deviations via LKJ
## correlation sample
cvPost(3, sapply(1:3, function(...) {
  rnorm(10)
}), type = "lkj")
#> [[1]]
#>           [,1]      [,2]      [,3]
#> [1,] 96.862339 0.7748630 -4.626203
#> [2,]  0.774863 0.0283645  0.119055
#> [3,] -4.626203 0.1190550  4.094623
#> 
#> [[2]]
#>             [,1]       [,2]       [,3]
#> [1,]  0.06218003 0.05815138 -0.5246680
#> [2,]  0.05815138 0.70904612  0.3915278
#> [3,] -0.52466798 0.39152780  7.9264641
#> 
#> [[3]]
#>            [,1]       [,2]       [,3]
#> [1,]  0.5725945 -0.7039115  0.3184467
#> [2,] -0.7039115 11.3514944 -0.9360070
#> [3,]  0.3184467 -0.9360070  0.2958648
#> 
#> [[4]]
#>            [,1]      [,2]       [,3]
#> [1,]  0.7659467 -2.803861  0.3011139
#> [2,] -2.8038607 44.273527 -9.4665546
#> [3,]  0.3011139 -9.466555  2.7453546
#> 
#> [[5]]
#>            [,1]        [,2]        [,3]
#> [1,] 3.56762745  0.07176028  0.13656513
#> [2,] 0.07176028  0.42276523 -0.02050365
#> [3,] 0.13656513 -0.02050365  0.03225709
#> 
#> [[6]]
#>           [,1]      [,2]      [,3]
#> [1,] 0.5663711 0.0340262 0.1454173
#> [2,] 0.0340262 0.5977762 0.3269646
#> [3,] 0.1454173 0.3269646 0.2082704
#> 
#> [[7]]
#>              [,1]         [,2]         [,3]
#> [1,]  0.004927562 -0.002843181  0.005520158
#> [2,] -0.002843181  0.029412775 -0.013463952
#> [3,]  0.005520158 -0.013463952  0.182352220
#> 
#> [[8]]
#>              [,1]        [,2]         [,3]
#> [1,]  0.007589923 -0.06216448  0.002859140
#> [2,] -0.062164482  2.50977902 -0.118417072
#> [3,]  0.002859140 -0.11841707  0.007999188
#> 
#> [[9]]
#>            [,1]       [,2]       [,3]
#> [1,] 14.0163806  1.2051675  0.8401261
#> [2,]  1.2051675  0.2780401 -0.1994978
#> [3,]  0.8401261 -0.1994978  1.0749189
#> 
#> [[10]]
#>            [,1]       [,2]       [,3]
#> [1,]  0.5415731 -0.7109454  0.4040513
#> [2,] -0.7109454  2.4865600 -0.4319643
#> [3,]  0.4040513 -0.4319643  1.5098302
#> 

## or return cholesky decomposition
cvPost(3, sapply(1:3, function(...) {
  rnorm(10)
}),
type = "lkj",
returnChol = TRUE
)
#> [[1]]
#>         [,1]      [,2]       [,3]
#> [1,] 1.86548 0.9644577 -1.1377549
#> [2,] 0.00000 0.5131372  0.7633548
#> [3,] 0.00000 0.0000000  1.3396488
#> 
#> [[2]]
#>           [,1]       [,2]      [,3]
#> [1,] 0.3853808 -0.2396288 0.2759888
#> [2,] 0.0000000  0.8531100 0.3904699
#> [3,] 0.0000000  0.0000000 3.9907672
#> 
#> [[3]]
#>           [,1]       [,2]        [,3]
#> [1,] 0.5811021 0.02348261 0.040235833
#> [2,] 0.0000000 0.30199556 0.006542798
#> [3,] 0.0000000 0.00000000 0.326766208
#> 
#> [[4]]
#>          [,1]       [,2]       [,3]
#> [1,] 1.787819 -0.5701632 0.02973627
#> [2,] 0.000000  1.7537550 0.16276115
#> [3,] 0.000000  0.0000000 0.38911063
#> 
#> [[5]]
#>          [,1]       [,2]        [,3]
#> [1,] 2.155836 -0.1976721 -0.27043101
#> [2,] 0.000000  0.7801646  0.06882305
#> [3,] 0.000000  0.0000000  0.16161284
#> 
#> [[6]]
#>          [,1]       [,2]        [,3]
#> [1,] 1.590053 0.07434835 -0.17317880
#> [2,] 0.000000 0.82964610  0.13403995
#> [3,] 0.000000 0.00000000  0.07785199
#> 
#> [[7]]
#>           [,1]      [,2]       [,3]
#> [1,] 0.4123939 0.2062854 -0.8475394
#> [2,] 0.0000000 2.5346241  0.4594744
#> [3,] 0.0000000 0.0000000  0.4939941
#> 
#> [[8]]
#>          [,1]       [,2]         [,3]
#> [1,] 0.332944 -0.6235242 -0.002281778
#> [2,] 0.000000  2.1873959  0.885067020
#> [3,] 0.000000  0.0000000  1.705736993
#> 
#> [[9]]
#>          [,1]      [,2]      [,3]
#> [1,] 4.539001 0.4690774 -2.300368
#> [2,] 0.000000 3.9959179 -1.030652
#> [3,] 0.000000 0.0000000  2.165767
#> 
#> [[10]]
#>          [,1]         [,2]      [,3]
#> [1,] 1.294237 2.545228e-05 -0.891757
#> [2,] 0.000000 6.211554e-01 -1.993399
#> [3,] 0.000000 0.000000e+00  1.819896
#> 

## Sample 3 covariances with lognormal standard deviations via separation
## strategy using inverse Wishart correlation sample
cvPost(3, sapply(1:3, function(...) {
  rnorm(10)
}), type = "separation")
#> [[1]]
#>            [,1]       [,2]        [,3]
#> [1,] 17.8091343 0.73461953 -1.39358522
#> [2,]  0.7346195 0.43703688  0.08246014
#> [3,] -1.3935852 0.08246014  0.39231482
#> 
#> [[2]]
#>             [,1]       [,2]        [,3]
#> [1,]  0.11144462  0.2079765 -0.08551474
#> [2,]  0.20797652  9.2698949 -0.16544961
#> [3,] -0.08551474 -0.1654496  0.08403645
#> 
#> [[3]]
#>            [,1]       [,2]       [,3]
#> [1,]  0.7908562 -0.5415399 -0.8590748
#> [2,] -0.5415399  0.3821333  0.5923616
#> [3,] -0.8590748  0.5923616  0.9845958
#> 
#> [[4]]
#>            [,1]        [,2]        [,3]
#> [1,] 11.0562602  0.33123520  1.27030624
#> [2,]  0.3312352  0.42048853 -0.01175715
#> [3,]  1.2703062 -0.01175715  0.20178261
#> 
#> [[5]]
#>           [,1]     [,2]      [,3]
#> [1,] 0.3908392 1.232200 0.3605851
#> [2,] 1.2321999 4.029834 1.1549621
#> [3,] 0.3605851 1.154962 0.3440444
#> 
#> [[6]]
#>           [,1]      [,2]       [,3]
#> [1,] 0.9003795 0.2147410  3.1855956
#> [2,] 0.2147410 0.1209066  0.7990146
#> [3,] 3.1855956 0.7990146 13.1359204
#> 
#> [[7]]
#>            [,1]       [,2]       [,3]
#> [1,]  0.8417985 -0.8216620 -0.7381474
#> [2,] -0.8216620  0.9218277  0.7890609
#> [3,] -0.7381474  0.7890609  0.7039473
#> 
#> [[8]]
#>             [,1]        [,2]        [,3]
#> [1,]  0.16942278  0.01244557 -0.01024524
#> [2,]  0.01244557  0.04491023 -0.07210998
#> [3,] -0.01024524 -0.07210998  0.11723618
#> 
#> [[9]]
#>            [,1]      [,2]       [,3]
#> [1,]  0.4109154 -1.842263 -0.7241764
#> [2,] -1.8422630 10.322635  3.4070393
#> [3,] -0.7241764  3.407039  1.3859887
#> 
#> [[10]]
#>            [,1]       [,2]       [,3]
#> [1,]  0.9428107 -0.7354081  0.6621306
#> [2,] -0.7354081  0.5785147 -0.5232715
#> [3,]  0.6621306 -0.5232715  0.4840937
#> 

## or returning the cholesky decomposition
cvPost(3, sapply(1:3, function(...) {
  rnorm(10)
}),
type = "separation",
returnChol = TRUE
)
#> [[1]]
#>           [,1]       [,2]       [,3]
#> [1,] 0.5361344 -0.5646490 -1.2157714
#> [2,] 0.0000000  0.2706145  0.7833376
#> [3,] 0.0000000  0.0000000  1.4143150
#> 
#> [[2]]
#>           [,1]       [,2]       [,3]
#> [1,] 0.4763222 -0.1686305 -0.2638639
#> [2,] 0.0000000  0.3953129  0.2370631
#> [3,] 0.0000000  0.0000000  0.2553825
#> 
#> [[3]]
#>          [,1]       [,2]        [,3]
#> [1,] 1.457362 -0.1565739 0.003957496
#> [2,] 0.000000  0.2969488 0.004259446
#> [3,] 0.000000  0.0000000 0.034623292
#> 
#> [[4]]
#>           [,1]      [,2]       [,3]
#> [1,] 0.2697727 0.1901255  0.1775689
#> [2,] 0.0000000 0.1863785 -0.4415488
#> [3,] 0.0000000 0.0000000  1.3256841
#> 
#> [[5]]
#>          [,1]       [,2]       [,3]
#> [1,] 1.081252 -0.2458460 0.05591077
#> [2,] 0.000000  0.4958417 0.11182166
#> [3,] 0.000000  0.0000000 0.07342814
#> 
#> [[6]]
#>           [,1]      [,2]       [,3]
#> [1,] 0.4513931 -1.555660  0.9059502
#> [2,] 0.0000000  1.430568 -0.3169197
#> [3,] 0.0000000  0.000000  0.7111382
#> 
#> [[7]]
#>          [,1]      [,2]       [,3]
#> [1,] 5.850312 0.3103629 -3.1410818
#> [2,] 0.000000 0.3115511  0.2156489
#> [3,] 0.000000 0.0000000  2.5678462
#> 
#> [[8]]
#>           [,1]        [,2]        [,3]
#> [1,] 0.3920281 -0.17091427  1.44657948
#> [2,] 0.0000000  0.01081568 -0.09688330
#> [3,] 0.0000000  0.00000000  0.08276372
#> 
#> [[9]]
#>          [,1]       [,2]       [,3]
#> [1,] 2.021659 -0.6427884  0.8126606
#> [2,] 0.000000  0.2158093 -0.3291584
#> [3,] 0.000000  0.0000000  0.3722985
#> 
#> [[10]]
#>           [,1]      [,2]      [,3]
#> [1,] 0.4696165 0.1467044 -1.908483
#> [2,] 0.0000000 0.4906213  3.952318
#> [3,] 0.0000000 0.0000000 10.894538
#> 
```
