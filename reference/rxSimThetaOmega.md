# Simulate Parameters from a Theta/Omega specification

Simulate Parameters from a Theta/Omega specification

## Usage

``` r
rxSimThetaOmega(
  params = NULL,
  omega = NULL,
  omegaDf = NULL,
  omegaLower = as.numeric(c(R_NegInf)),
  omegaUpper = as.numeric(c(R_PosInf)),
  omegaIsChol = FALSE,
  omegaSeparation = "auto",
  omegaXform = 1L,
  nSub = 1L,
  thetaMat = NULL,
  thetaLower = as.numeric(c(R_NegInf)),
  thetaUpper = as.numeric(c(R_PosInf)),
  thetaDf = NULL,
  thetaIsChol = FALSE,
  nStud = 1L,
  sigma = NULL,
  sigmaLower = as.numeric(c(R_NegInf)),
  sigmaUpper = as.numeric(c(R_PosInf)),
  sigmaDf = NULL,
  sigmaIsChol = FALSE,
  sigmaSeparation = "auto",
  sigmaXform = 1L,
  nCoresRV = 1L,
  nObs = 1L,
  dfSub = 0,
  dfObs = 0,
  simSubjects = TRUE,
  simVariability = as.logical(c(NA_LOGICAL))
)
```

## Arguments

- params:

  Named Vector of rxode2 model parameters

- omega:

  Estimate of Covariance matrix. When omega is a list, assume it is a
  block matrix and convert it to a full matrix for simulations. When
  `omega` is `NA` and you are using it with a `rxode2` ui model, the
  between subject variability described by the `omega` matrix are set to
  zero.

- omegaDf:

  The degrees of freedom of a t-distribution for simulation. By default
  this is `NULL` which is equivalent to `Inf` degrees, or to simulate
  from a normal distribution instead of a t-distribution.

- omegaLower:

  Lower bounds for simulated ETAs (by default -Inf)

- omegaUpper:

  Upper bounds for simulated ETAs (by default Inf)

- omegaIsChol:

  Indicates if the `omega` supplied is a Cholesky decomposed matrix
  instead of the traditional symmetric matrix.

- omegaSeparation:

  Omega separation strategy

  Tells the type of separation strategy when simulating covariance with
  parameter uncertainty with standard deviations modeled in the
  `thetaMat` matrix.

  - `"lkj"` simulates the correlation matrix from the `rLKJ1` matrix
    with the distribution parameter `eta` equal to the degrees of
    freedom `nu` by `(nu-1)/2`

  - `"separation"` simulates from the identity inverse Wishart
    covariance matrix with `nu` degrees of freedom. This is then
    converted to a covariance matrix and augmented with the modeled
    standard deviations. While computationally more complex than the
    `"lkj"` prior, it performs better when the covariance matrix size is
    greater or equal to 10

  - `"auto"` chooses `"lkj"` when the dimension of the matrix is less
    than 10 and `"separation"` when greater than equal to 10.

- omegaXform:

  When taking `omega` values from the `thetaMat` simulations (using the
  separation strategy for covariance simulation), how should the
  `thetaMat` values be turned int standard deviation values:

  - `identity` This is when standard deviation values are directly
    modeled by the `params` and `thetaMat` matrix

  - `variance` This is when the `params` and `thetaMat` simulates the
    variance that are directly modeled by the `thetaMat` matrix

  - `log` This is when the `params` and `thetaMat` simulates `log(sd)`

  - `nlmixrSqrt` This is when the `params` and `thetaMat` simulates the
    inverse cholesky decomposed matrix with the `x\^2` modeled along the
    diagonal. This only works with a diagonal matrix.

  - `nlmixrLog` This is when the `params` and `thetaMat` simulates the
    inverse cholesky decomposed matrix with the `exp(x\^2)` along the
    diagonal. This only works with a diagonal matrix.

  - `nlmixrIdentity` This is when the `params` and `thetaMat` simulates
    the inverse cholesky decomposed matrix. This only works with a
    diagonal matrix.

- nSub:

  Number between subject variabilities (`ETAs`) simulated for every
  realization of the parameters.

- thetaMat:

  Named theta matrix.

- thetaLower:

  Lower bounds for simulated population parameter variability (by
  default `-Inf`)

- thetaUpper:

  Upper bounds for simulated population unexplained variability (by
  default `Inf`)

- thetaDf:

  The degrees of freedom of a t-distribution for simulation. By default
  this is `NULL` which is equivalent to `Inf` degrees, or to simulate
  from a normal distribution instead of a `t`-distribution.

- thetaIsChol:

  Indicates if the `theta` supplied is a Cholesky decomposed matrix
  instead of the traditional symmetric matrix.

- nStud:

  Number virtual studies to characterize uncertainty in estimated
  parameters.

- sigma:

  Named sigma covariance or Cholesky decomposition of a covariance
  matrix. The names of the columns indicate parameters that are
  simulated. These are simulated for every observation in the solved
  system. When `sigma` is `NA` and you are using it with a `rxode2` ui
  model, the unexplained variability described by the `sigma` matrix are
  set to zero.

- sigmaLower:

  Lower bounds for simulated unexplained variability (by default -Inf)

- sigmaUpper:

  Upper bounds for simulated unexplained variability (by default Inf)

- sigmaDf:

  Degrees of freedom of the sigma t-distribution. By default it is
  equivalent to `Inf`, or a normal distribution.

- sigmaIsChol:

  Boolean indicating if the sigma is in the Cholesky decomposition
  instead of a symmetric covariance

- sigmaSeparation:

  separation strategy for sigma;

  Tells the type of separation strategy when simulating covariance with
  parameter uncertainty with standard deviations modeled in the
  `thetaMat` matrix.

  - `"lkj"` simulates the correlation matrix from the `rLKJ1` matrix
    with the distribution parameter `eta` equal to the degrees of
    freedom `nu` by `(nu-1)/2`

  - `"separation"` simulates from the identity inverse Wishart
    covariance matrix with `nu` degrees of freedom. This is then
    converted to a covariance matrix and augmented with the modeled
    standard deviations. While computationally more complex than the
    `"lkj"` prior, it performs better when the covariance matrix size is
    greater or equal to 10

  - `"auto"` chooses `"lkj"` when the dimension of the matrix is less
    than 10 and `"separation"` when greater than equal to 10.

- sigmaXform:

  When taking `sigma` values from the `thetaMat` simulations (using the
  separation strategy for covariance simulation), how should the
  `thetaMat` values be turned int standard deviation values:

  - `identity` This is when standard deviation values are directly
    modeled by the `params` and `thetaMat` matrix

  - `variance` This is when the `params` and `thetaMat` simulates the
    variance that are directly modeled by the `thetaMat` matrix

  - `log` This is when the `params` and `thetaMat` simulates `log(sd)`

  - `nlmixrSqrt` This is when the `params` and `thetaMat` simulates the
    inverse cholesky decomposed matrix with the `x\^2` modeled along the
    diagonal. This only works with a diagonal matrix.

  - `nlmixrLog` This is when the `params` and `thetaMat` simulates the
    inverse cholesky decomposed matrix with the `exp(x\^2)` along the
    diagonal. This only works with a diagonal matrix.

  - `nlmixrIdentity` This is when the `params` and `thetaMat` simulates
    the inverse cholesky decomposed matrix. This only works with a
    diagonal matrix.

- nCoresRV:

  Number of cores used for the simulation of the sigma variables. By
  default this is 1. To reproduce the results you need to run on the
  same platform with the same number of cores. This is the reason this
  is set to be one, regardless of what the number of cores are used in
  threaded ODE solving.

- nObs:

  Number of observations to simulate (with `sigma` matrix)

- dfSub:

  Degrees of freedom to sample the between subject variability matrix
  from the inverse Wishart distribution (scaled) or scaled inverse chi
  squared distribution.

- dfObs:

  Degrees of freedom to sample the unexplained variability matrix from
  the inverse Wishart distribution (scaled) or scaled inverse chi
  squared distribution.

- simSubjects:

  boolean indicated rxode2 should simulate subjects in studies (`TRUE`,
  default) or studies (`FALSE`)

- simVariability:

  determines if the variability is simulated. When `NA` (default) this
  is determined by the solver.

## Value

a data frame with the simulated subjects

## Author

Matthew L.Fidler
