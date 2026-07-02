## Package-level cache: function object digest → rxUi (list form).
## Populated by rxSolve.function; entries for uiUseData models are skipped.
.rxFunctionUiCache <- new.env(hash = TRUE, parent = emptyenv())

#' Options, Solving & Simulation of an ODE/solved system
#'
#' This uses rxode2 family of objects, file, or model specification to
#' solve a ODE system.  There are many options for a solved rxode2
#' model, the first are the required `object`, and `events` with the
#' some-times optional `params` and `inits`.
#'
#' The rest of the document focus on the different ODE solving
#' methods, followed by the core solving method's options, rxode2 event
#' handling options, rxode2's numerical stability options, rxode2's
#' output options, and finally internal rxode2 options or compatibility
#' options.
#'
#' @param object is a either a rxode2 family of objects, or a file-name
#'     with a rxode2 model specification, or a string with a rxode2
#'     model specification.
#'
#' @param params a numeric named vector with values for every
#'     parameter in the ODE system; the names must correspond to the
#'     parameter identifiers used in the ODE specification;
#'
#' @param events an `eventTable` object describing the input
#'     (e.g., doses) to the dynamic system and observation sampling
#'     time points (see [eventTable()]);
#'
#' @param inits a vector of initial values of the state variables
#'     (e.g., amounts in each compartment), and the order in this
#'     vector must be the same as the state variables (e.g., PK/PD
#'     compartments);
#'
#' @param sigdig Specifies the "significant digits" that the ode
#'   solving requests.  When specified this controls the relative and
#'   absolute tolerances of the ODE solvers.  By default the tolerance
#'   is `0.5*10^(-sigdig-2)` for regular ODEs. For the
#'   sensitivity equations the default is `0.5*10\^(-sigdig-1.5)`
#'   (sensitivity changes only applicable for liblsoda).  This also
#'   controls the `atol`/`rtol` of the steady state solutions. The
#'   `ssAtol`/`ssRtol` is `0.5*10\^(-sigdig)` and for the sensitivities
#'   `0.5*10\^(-sigdig+0.625)`.  By default
#'   this is unspecified (`NULL`) and uses the standard `atol`/`rtol`.
#'
#' @param atol a numeric absolute tolerance (1e-8 by default) used
#'     by the ODE solver to determine if a good solution has been
#'     achieved;  This is also used in the solved linear model to check
#'     if prior doses do not add anything to the solution.
#'
#' @param rtol a numeric relative tolerance (`1e-6` by default) used
#'     by the ODE solver to determine if a good solution has been
#'     achieved. This is also used in the solved linear model to check
#'     if prior doses do not add anything to the solution.
#'
#' @param atolSens Sensitivity atol, can be different than atol with
#'     liblsoda.  This allows a less accurate solve for gradients (if desired)
#'
#' @param rtolSens Sensitivity rtol, can be different than rtol with
#'     liblsoda.  This allows a less accurate solve for gradients (if desired)
#'
#' @param maxsteps maximum number of (internally defined) steps allowed
#'     during one call to the solver. (5000 by default)
#'
#' @param hmin The minimum absolute step size allowed. The default
#'     value is 0.
#'
#'     For the fixed-step Boost methods `"rk4"`, `"trapz"`, `"ssp3"`, `"ab"`,
#'     `"abm"`, `"sem"`, `"sb3a"`, `"sb3am4"`, `"vv"`, `"mm"`, `"em"`, `"ros6"`,
#'     `"backwardEuler"`, `"gauss6"`, `"iiic6"`, `"radauiia5"`, `"geng5"`, and the
#'     libode fixed-step family (`"euler"`, `"midpoint"`, `"heun"`, `"ssp22"`,
#'     `"rk3"`, `"ssp53"`, `"s4"`, `"r4"`, `"ls44"`, `"ls54"`,
#'     `"ssp54"`, `"s5"`, `"rk5"`, `"c5"`, `"l5"`, `"lk5a"`, `"lk5b"`,
#'     `"b6"`, `"s7"`, `"s8_10"`, `"cv8"`, `"s8_12"`, `"s10"`, `"z10"`,
#'     `"o10"`, `"h10"`), this specifies the fixed step size.
#'     If `hmin=0` (the default), it uses a default of `0.01` for `"rk4"`,
#'     `"trapz"`, `"ssp3"`, `"ros6"`, `"backwardEuler"`, `"gauss6"`, `"iiic6"`,
#'     `"radauiia5"`, `"geng5"`, and all libode fixed-step methods; `0.0001` for
#'     `"ab"`, `"abm"`, `"sem"`, `"sb3a"`, `"sb3am4"`, `"vv"`, `"mm"`, and `"em"`.
#'     If the requested step size would cause the number of steps to exceed
#'     `maxsteps`, the step size is automatically increased to ensure the
#'     integration completes within the `maxsteps` limit.  For `"trapz"`, the step
#'     is also silently clamped to the interval length when the inter-event
#'     interval is shorter than the nominal step size, so short intervals (e.g.,
#'     between closely spaced doses) are always handled correctly.
#'
#'     For the adaptive methods `"f78"`, `"ck54"`, `"dop5"`, `"bs"`, `"f32"`,
#'     `"rk43"`, `"dop54"`, `"vern65"`, `"vern76"`, `"dop87"`, `"vern98"`,
#'     `"ros43"`, `"sdirk43"`, and all libode adaptive methods (`"bs32"`,
#'     `"ssp43"`, `"f45"`, `"t54"`, `"s54"`, `"pp54"`, `"pp54b"`,
#'     `"bs54"`, `"ss54"`, `"dp65"`, `"c65"`, `"tp64"`, `"v65r"`,
#'     `"v65"`, `"dverk65"`, `"tf65"`, `"tp75"`, `"tmy7"`, `"tmy7s"`,
#'     `"v76r"`, `"ss76"`, `"v78"`, `"dverk78"`, `"dp85"`, `"tp86"`,
#'     `"v87e"`, `"v87r"`, `"ev87"`, `"k87"`, `"f89"`, `"v89"`,
#'     `"t98a"`, `"v98r"`, `"s98"`, `"f108"`, `"c108"`, `"b109"`,
#'     `"s1110a"`, `"f1210"`, `"o129"`, `"f1412"`, the libode aliases
#'     `"dp54"`, `"v65e"`, `"v76e"`, `"dp87"`, `"v98e"`,
#'     `"ssp33"`, and the deSolve-derived methods `"lsode"`, `"bdf"`),
#'     this specifies the initial step size (default `0.01` when
#'     `hmin=0`); subsequent steps are chosen adaptively using `atol`, `rtol`,
#'     and `maxsteps`.
#'
#' @param hmax The maximum absolute step size allowed.  When
#'   `hmax=NA` (default), uses the average difference +
#'   hmaxSd*sd in times and sampling events. The `hmaxSd` is a user
#'   specified parameter and which defaults to zero.  When
#'   `hmax=NULL` rxode2 uses the maximum difference in times in
#'   your sampling and events.  The value 0 is equivalent to infinite
#'   maximum absolute step size.
#'
#'   Note that for dense output methods (`"dop853"`, `"dop5"`, `"bs"`,
#'   `"ros4"`), `hmax` defaults to `NULL` to allow the solvers to
#'   determine the step size when `dense=TRUE`
#'
#'   For `method="indLin"`, `hmax` caps how long an interval is treated
#'   as having a CONSTANT Jacobian/matrix-exponential term before
#'   re-linearizing (previously silently ignored for this method). For a
#'   true (state-independent) `matExp()` model this makes no numerical
#'   difference; for an `indLin()`-forcing model with a state-dependent
#'   term (e.g. Michaelis-Menten elimination), the default `hmax` (tied to
#'   the spacing of your own sampling/dosing times) may be too coarse for
#'   the desired accuracy -- set an explicit, smaller `hmax` to force more
#'   frequent relinearization.
#'
#' @param hmaxSd The number of standard deviations of the time
#'     difference to add to hmax. The default is 0
#'
#' @param hini The step size to be attempted on the first step. The
#'     default value is determined by the solver (when `hini = 0`)
#'
#' @param maxordn The maximum order to be allowed for the nonstiff
#'     (Adams) method.  The default is 12.  It can be between 1 and
#'     12.
#'
#' @param order The order for the `"ab"`, `"abm"`, and `"mm"` methods.
#'     For `"ab"` and `"abm"`, the default is 5, and it can be between 1 and 8.
#'     For `"mm"` (Modified Midpoint), it represents the number of intermediate steps,
#'     the default is 5, and it must be a positive integer (>= 1).
#'
#' @param maxords The maximum order to be allowed for the stiff (BDF)
#'     method.  The default value is 5.  This can be between 1 and 5.
#'
#' @param mxhnil maximum number of messages printed (per problem)
#'     warning that `T + H = T` on a step (`H` = step size).  This must
#'     be positive to result in a non-default value.  The default
#'     value is 0 (or infinite).
#'
#' @param hmxi inverse of the maximum absolute value of `H` to are used.
#'     hmxi = 0.0 is allowed and corresponds to an infinite `hmax1
#'     (default).  `hmin` and `hmxi` may be changed at any time, but will
#'     not take effect until the next change of `H` is considered.
#'     This option is only considered with `method="liblsoda"`.
#'
#' @param istateReset When `TRUE`, reset the `ISTATE` variable to 1 for
#'     lsoda and liblsoda with doses, like `deSolve`; When `FALSE`, do
#'     not reset the `ISTATE` variable with doses.
#'
#' @param indLinMatExpType This is them matrix exponential type that
#'     is use for rxode2.  Currently the following are supported:
#'
#' * `Al-Mohy` Uses the exponential matrix method of Al-Mohy Higham (2009)
#'
#' * `arma` Use the exponential matrix from RcppArmadillo
#'
#' * `expokit` Use the exponential matrix from Roger B. Sidje (1998)
#'
#'
#' @param indLinMatExpOrder an integer, the order of approximation to
#'     be used, for the `Al-Mohy` and `expokit` values.
#'     The best value for this depends on machine precision (and
#'     slightly on the matrix). We use `6` as a default.
#'
#' @param indLinPhiTol the requested accuracy tolerance on
#'     exponential matrix.
#'
#' @param indLinPhiM  the maximum size for the Krylov basis
#'
#' @param minSS Minimum number of iterations for a steady-state dose
#'
#' @param maxSS Maximum number of iterations for a steady-state dose
#'
#' @param strictSS Boolean indicating if a strict steady-state is
#'     required. If a strict steady-state is (`TRUE`) required
#'     then at least `minSS` doses are administered and the
#'     total number of steady states doses will continue until
#'     `maxSS` is reached, or `atol` and `rtol` for
#'     every compartment have been reached.  However, if ODE solving
#'     problems occur after the `minSS` has been reached the
#'     whole subject is considered an invalid solve. If
#'     `strictSS` is `FALSE` then as long as `minSS`
#'     has been reached the last good solve before ODE solving
#'     problems occur is considered the steady state, even though
#'     either `atol`, `rtol` or `maxSS` have not
#'     been achieved.
#'
#' @param infSSstep Step size for determining if a constant infusion
#'     has reached steady state.  By default this is large value,
#'     12.
#'
#' @param ssAtol Steady state atol convergence factor.  Can be
#'     a vector based on each state.
#'
#' @param ssRtol Steady state rtol convergence factor.  Can be a
#'     vector based on each state.
#'
#' @param ssAtolSens Sensitivity absolute tolerance (atol) for
#'     calculating if steady state has been achieved for sensitivity compartments.
#'
#' @param ssRtolSens Sensitivity relative tolerance (rtol) for
#'     calculating if steady state has been achieved for sensitivity compartments.
#'
#' @param maxAtolRtolFactor The maximum `atol`/`rtol` that
#'     FOCEi and other routines may adjust to.  By default 0.1
#'
#' @param tolFactor A per-individual tolerance multiplier (>= 1.0, or
#'     `NULL` for no effect).  When supplied, each individual's
#'     `atol`, `rtol`, `ssAtol`, and `ssRtol` are multiplied by
#'     this factor before the ODE solver is called, loosening the
#'     tolerances for that individual.  This is useful when a small
#'     number of subjects are numerically stiff and would otherwise
#'     cause solve failures: pass a larger factor for the
#'     problematic subjects and leave the rest at 1.0 (or omit them).
#'     The effective tolerance is always capped at `maxAtolRtolFactor`.
#'
#'     `tolFactor` may be:
#'     * `NULL` (default) -- no adjustment applied.
#'     * A single numeric value -- the same factor is applied to
#'       the first `length(tolFactor)` subjects in order.
#'     * A numeric vector -- applied element-wise to subjects in
#'       the order they appear.
#'     * A **named** numeric vector -- names are matched to subject
#'       IDs; unmatched subjects retain `tolFactor = 1.0`.
#'
#'     The per-subject factors used (after matching) are stored back
#'     in the solved object and accessible via `$tolFactor`.
#'
#' @param stateTrim When amounts/concentrations in one of the states
#'     are above this value, trim them to be this value. By default
#'     Inf.  Also trims to -stateTrim for large negative
#'     amounts/concentrations.  If you want to trim between a range
#'     say `c(0, 2000000)` you may specify 2 values with a lower and
#'     upper range to make sure all state values are in the
#'     reasonable range.
#'
#' @param safeZero Use safe zero divide. By default
#'     this is turned on but you may turn it off if you wish.
#'
#' @param safePow Use safe powers.  When enabled if your power is
#'   negative and your base is zero, this will return the `machine
#'   epsilon^(negative power)`.  By default this is turned on.
#'
#' @param safeLog Use safe log.  When enabled if your value that you are taking log() of is negative or zero, this will return `log(machine epsilon)`.  By default this is turned on.
#'
#' @param sumType Sum type to use for `sum()` in
#'     rxode2 code blocks.
#'
#' `pairwise` uses the pairwise sum (fast, default)
#'
#' `fsum` uses the PreciseSum package's fsum function (most accurate)
#'
#' `kahan` uses Kahan correction
#'
#' `neumaier` uses Neumaier correction
#'
#' `c` uses no correction: default/native summing
#'
#' @param prodType Product to use for `prod()` in rxode2 blocks
#'
#' `long double` converts to long double, performs the
#' multiplication and then converts back.
#'
#' `double` uses the standard double scale for multiplication.
#'
#' @param maxwhile represents the maximum times a while loop is
#'   evaluated before exiting.  By default this is 100000
#'
#' @param iCov A data frame of individual non-time varying covariates
#'   to combine with the `events` dataset.  The `iCov` dataset has one
#'   covariate per ID and should match the event table
#'
#' @param covsInterpolation specifies the interpolation method for
#'     time-varying covariates. When solving ODEs it often samples
#'     times outside the sampling time specified in `events`.
#'     When this happens, the time varying covariates are
#'     interpolated.  Currently this can be:
#'
#' * `"linear"` interpolation, which interpolates the covariate
#'     by solving the line between the observed covariates and extrapolating the new
#'     covariate value.
#'
#' * `"locf"` -- Last observation carried forward (the default).
#'
#' * `"nocb"` -- Next Observation Carried Backward.  This is the same method
#'       that NONMEM uses.
#'
#' * `"midpoint"` Last observation carried forward to midpoint; Next observation
#'   carried backward to midpoint.
#'
#'   For time-varying covariates where a missing value is present, the
#'   interpolation method will use either "locf" or "nocb" throughout
#'   if they are the type of covariate interpolation that is selected.
#'
#'   When using the linear or midpoint interpolation, the lower point
#'   in the interpolation will use locf to interpolate missing
#'   covariates and the upper point will use the nocb to interpolate
#'   missing covariates.
#'
#' @param naInterpolation specifies the interpolation method for
#'   time-varying covariates when the instantaneous value is `NA` (not
#'   during an explicit interpolation) and the `covsInterpolation` is
#'   either `"midpoint"` or `"linear"`. This can be:
#'
#'   * `"locf"` -- last observation carried forward (default)
#'
#'   * `"nocb"` -- next observation carried backward.
#'
#'   This will look for the prior value (backwards/locf) when
#'   instantaneously missing, or the next value when instantaneously
#'   missing.  If all the covariates are missing and you find the
#'   end/beginning of the individual record, switch direction.  If all
#'   are really missing, then return missing.
#'
#' @param keepInterpolation specifies the interpolation method for
#'   variables in the `keep` column.  When `nlmixr2` creates `mtime`,
#'   `addl` doses etc, these items were not originally in the dataset.
#'   The interpolation methods you can choose are:
#'
#'   * `"locf"` -- last observation carried forward (default)
#'
#'   * `"nocb"` -- next observation carried backward.
#'
#'   * `"na"` -- no interpolation, simply put `NA` for the
#'   interpolated `keep` covariates.
#'
#'
#' @param addCov A boolean indicating if covariates should be added
#'     to the output matrix or data frame. By default this is
#'     disabled.
#'
#' @param seed an object specifying if and how the random number
#'    generator should be initialized
#'
#' @param nsim represents the number of simulations.  For rxode2, if
#'     you supply single subject event tables (created with
#'     `[eventTable()]`)
#'
#' @param thetaMat Named theta matrix.
#'
#' @param thetaLower Lower bounds for simulated population parameter
#'   variability (by default `-Inf`)
#'
#' @param thetaUpper Upper bounds for simulated population unexplained
#'   variability (by default `Inf`)
#'
#' @param thetaDf The degrees of freedom of a t-distribution for
#'     simulation.  By default this is `NULL` which is
#'     equivalent to `Inf` degrees, or to simulate from a normal
#'     distribution instead of a `t`-distribution.
#'
#' @param thetaIsChol Indicates if the `theta` supplied is a
#'     Cholesky decomposed matrix instead of the traditional
#'     symmetric matrix.
#'
#' @param nStud Number virtual studies to characterize uncertainty in estimated
#'        parameters.
#'
#' @param omega Estimate of Covariance matrix. When omega is a list,
#'   assume it is a block matrix and convert it to a full matrix for
#'   simulations.  When `omega` is `NA` and you are using it with a
#'   `rxode2` ui model, the between subject variability described by
#'   the `omega` matrix are set to zero.
#'
#' @param omegaIsChol Indicates if the `omega` supplied is a
#'     Cholesky decomposed matrix instead of the traditional
#'     symmetric matrix.
#'
#' @param omegaSeparation Omega separation strategy
#'
#' Tells the type of separation strategy when
#' simulating covariance with parameter uncertainty with standard
#' deviations modeled in the `thetaMat` matrix.
#'
#'  * `"lkj"` simulates the correlation matrix from the
#'    `rLKJ1` matrix with the distribution parameter `eta`
#'    equal to the degrees of freedom `nu` by `(nu-1)/2`
#'
#' *  `"separation"` simulates from the identity inverse Wishart
#'     covariance matrix with `nu` degrees of freedom.  This is then
#'     converted to a covariance matrix and augmented with the modeled
#'     standard deviations.  While computationally more complex than the
#'    `"lkj"` prior, it performs better when the covariance matrix
#'     size is greater or equal to 10
#'
#'  * `"auto"` chooses `"lkj"` when the dimension of the
#'     matrix is less than 10 and `"separation"` when greater
#'    than equal to 10.
#'
#' @param omegaXform When taking `omega` values from the `thetaMat`
#'   simulations (using the separation strategy for covariance
#'   simulation), how should the `thetaMat` values be turned int
#'   standard deviation values:
#'
#'   - `identity` This is when standard deviation values are
#'    directly modeled by the `params` and `thetaMat` matrix
#'
#'  - `variance` This is when the `params` and `thetaMat`
#'     simulates the variance that are directly modeled by the
#'     `thetaMat` matrix
#'
#'  - `log` This is when the `params` and `thetaMat`
#'     simulates `log(sd)`
#'
#'   - `nlmixrSqrt` This is when the `params` and
#'     `thetaMat` simulates the inverse cholesky decomposed matrix
#'     with the `x\^2` modeled along the diagonal.  This only works
#'      with a diagonal matrix.
#'
#'   - `nlmixrLog` This is when the `params` and
#'     `thetaMat` simulates the inverse cholesky decomposed matrix
#'      with the `exp(x\^2)` along the diagonal.  This only works
#'      with a diagonal matrix.
#'
#'   - `nlmixrIdentity` This is when the `params` and
#'      `thetaMat` simulates the inverse cholesky decomposed matrix.
#'      This only works with a diagonal matrix.
#'
#' @param omegaLower Lower bounds for simulated ETAs (by default -Inf)
#'
#' @param omegaUpper Upper bounds for simulated ETAs (by default Inf)
#'
#' @param omegaDf The degrees of freedom of a t-distribution for
#'     simulation.  By default this is `NULL` which is
#'     equivalent to `Inf` degrees, or to simulate from a normal
#'     distribution instead of a t-distribution.
#'
#' @param nSub Number between subject variabilities (`ETAs`) simulated for every
#'        realization of the parameters.
#'
#' @param dfSub Degrees of freedom to sample the between subject variability matrix from the
#'        inverse Wishart distribution (scaled) or scaled inverse chi squared distribution.
#'
#' @param sigma Named sigma covariance or Cholesky decomposition of a
#'     covariance matrix.  The names of the columns indicate
#'     parameters that are simulated.  These are simulated for every
#'     observation in the solved system. When `sigma` is `NA` and you are using it with a
#'    `rxode2` ui model, the unexplained variability described by
#'    the `sigma` matrix are set to zero.
#'
#' @param sigmaLower Lower bounds for simulated unexplained variability (by default -Inf)
#'
#' @param sigmaUpper Upper bounds for simulated unexplained variability (by default Inf)
#'
#' @param sigmaXform When taking `sigma` values from the `thetaMat`
#'   simulations (using the separation strategy for covariance
#'   simulation), how should the `thetaMat` values be turned int
#'   standard deviation values:
#'
#'  - `identity` This is when standard deviation values are
#'    directly modeled by the `params` and `thetaMat` matrix
#'
#'  - `variance` This is when the `params` and `thetaMat`
#'     simulates the variance that are directly modeled by the
#'     `thetaMat` matrix
#'
#'  - `log` This is when the `params` and `thetaMat`
#'     simulates `log(sd)`
#'
#'   - `nlmixrSqrt` This is when the `params` and
#'     `thetaMat` simulates the inverse cholesky decomposed matrix
#'     with the `x\^2` modeled along the diagonal.  This only works
#'      with a diagonal matrix.
#'
#'   - `nlmixrLog` This is when the `params` and
#'     `thetaMat` simulates the inverse cholesky decomposed matrix
#'      with the `exp(x\^2)` along the diagonal.  This only works
#'      with a diagonal matrix.
#'
#'   - `nlmixrIdentity` This is when the `params` and
#'      `thetaMat` simulates the inverse cholesky decomposed matrix.
#'      This only works with a diagonal matrix.
#'
#'
#' @param sigmaDf Degrees of freedom of the sigma t-distribution.  By
#'     default it is equivalent to `Inf`, or a normal distribution.
#'
#' @param sigmaIsChol Boolean indicating if the sigma is in the
#'     Cholesky decomposition instead of a symmetric covariance
#'
#' @param sigmaSeparation separation strategy for sigma;
#'
#' Tells the type of separation strategy when
#' simulating covariance with parameter uncertainty with standard
#' deviations modeled in the `thetaMat` matrix.
#'
#' * `"lkj"` simulates the correlation matrix from the
#'   `rLKJ1` matrix with the distribution parameter `eta`
#'   equal to the degrees of freedom `nu` by `(nu-1)/2`
#'
#' *  `"separation"` simulates from the identity inverse Wishart
#'    covariance matrix with `nu` degrees of freedom.  This is then
#'    converted to a covariance matrix and augmented with the modeled
#'    standard deviations.  While computationally more complex than the
#'    `"lkj"` prior, it performs better when the covariance matrix
#'    size is greater or equal to 10
#'
#' *  `"auto"` chooses `"lkj"` when the dimension of the
#'    matrix is less than 10 and `"separation"` when greater
#'    than equal to 10.
#'
#' @param dfObs Degrees of freedom to sample the unexplained variability matrix from the
#'        inverse Wishart distribution (scaled) or scaled inverse chi squared distribution.
#'
#' @param resample A character vector of model variables to resample
#'   from the input dataset; This sampling is done with replacement.
#'   When `NULL` or `FALSE` no resampling is done.  When
#'   `TRUE` resampling is done on all covariates in the input
#'   dataset
#'
#' @param resampleID boolean representing if the resampling should be
#'   done on an individual basis `TRUE` (ie. a whole patient is
#'   selected) or each covariate is resampled independent of the
#'   subject identifier `FALSE`.  When `resampleID=TRUE`
#'   correlations of parameters are retained, where as when
#'   `resampleID=FALSE` ignores patient covariate correaltions.
#'   Hence the default is `resampleID=TRUE`.
#'
#' @param returnType This tells what type of object is returned.  The
#'   currently supported types are:
#'
#' * `"rxSolve"` (default) will return a reactive data frame
#'      that can change easily change different pieces of the solve and
#'      update the data frame.  This is the currently standard solving
#'      method in rxode2,  is used for `rxSolve(object, ...)`, `solve(object,...)`,
#'
#' * `"data.frame"` -- returns a plain, non-reactive data
#'      frame; Currently very slightly faster than `returnType="matrix"`
#'
#' * `"matrix"` -- returns a plain matrix with column names attached
#'     to the solved object.  This is what is used `object$run` as well as `object$solve`
#'
#' * `"data.table"` -- returns a `data.table`; The `data.table` is
#'     created by reference (ie `setDt()`), which should be fast.
#'
#' * `"tbl"` or `"tibble"` returns a tibble format.
#'
#' @param addDosing Boolean indicating if the solve should add rxode2
#'     EVID and related columns.  This will also include dosing
#'     information and estimates at the doses.  Be default, rxode2
#'     only includes estimates at the observations. (default
#'     `FALSE`). When `addDosing` is `NULL`, only
#'     include `EVID=0` on solve and exclude any model-times or
#'     `EVID=2`. If `addDosing` is `NA` the classic
#'     `rxode2` EVID events are returned. When `addDosing` is `TRUE`
#'     add the event information in NONMEM-style format; If
#'     `subsetNonmem=FALSE` rxode2 will also include extra event types
#'     (`EVID`) for ending infusion and modeled times:
#'
#'
#' * `EVID=-1` when the modeled rate infusions are turned
#' off (matches `rate=-1`)
#'
#' * `EVID=-2` When the modeled duration infusions are
#' turned off (matches `rate=-2`)
#'
#' * `EVID=-10` When the specified `rate` infusions are
#' turned off (matches `rate>0`)
#'
#' * `EVID=-20` When the specified `dur` infusions are
#' turned off (matches `dur>0`)
#'
#' * `EVID=101,102,103,...` Modeled time where 101 is the
#' first model time, 102 is the second etc.
#'
#' @param keep Columns to keep from either the input dataset or the
#'     `iCov` dataset.  With the `iCov` dataset, the column
#'     is kept once per line.  For the input dataset, if any records
#'     are added to the data LOCF (Last Observation Carried forward)
#'     imputation is performed.
#'
#' @param drop Columns to drop from the output
#'
#' @param idFactor This boolean indicates if original ID values
#'     should be maintained. This changes the default sequentially
#'     ordered ID to a factor with the original ID values in the
#'     original dataset.  By default this is enabled.
#'
#' @param subsetNonmem subset to NONMEM compatible EVIDs only.  By
#'   default `TRUE`.
#'
#' @param scale a numeric named vector with scaling for ode
#'     parameters of the system.  The names must correspond to the
#'     parameter identifiers in the ODE specification. Each of the
#'     ODE variables will be divided by the scaling factor.  For
#'     example `scale=c(center=2)` will divide the center ODE
#'     variable by 2.
#'
#' @param amountUnits This supplies the dose units of a data frame
#'     supplied instead of an event table.  This is for importing the
#'     data as an rxode2 event table.
#'
#' @param timeUnits This supplies the time units of a data frame
#'     supplied instead of an event table.  This is for importing the
#'     data as an rxode2 event table.
#'
#' @param theta A vector of parameters that will be named `THETA\[#\]` and
#'     added to parameters
#'
#' @param eta A vector of parameters that will be named `ETA\[#\]` and
#'     added to parameters
#'
#' @param from When there is no observations in the event table,
#'     start observations at this value. By default this is zero.
#'
#' @param to When there is no observations in the event table, end
#'     observations at this value. By default this is 24 + maximum
#'     dose time.
#'
#' @param length.out The number of observations to create if there
#'     isn't any observations in the event table. By default this is 200.
#'
#' @param by When there are no observations in the event table, this
#'     is the amount to increment for the observations between `from`
#'     and `to`.
#'
#' @param warnIdSort Warn if the ID is not present and rxode2 assumes
#'     the order of the parameters/iCov are the same as the order of
#'     the parameters in the input dataset.
#'
#' @param warnDrop Warn if column(s) were supposed to be dropped, but
#'     were not present.
#'
#' @param nDisplayProgress An integer indicating the minimum number
#'     of c-based solves before a progress bar is shown.  By default
#'     this is 10,000.
#'
#' @param simVariability determines if the variability is simulated.
#'   When `NA` (default) this is determined by the solver.
#'
#' @param ... Other arguments including scaling factors for each
#'     compartment.  This includes \code{S# =} numeric will scale a compartment
#'     \code{#} by a dividing the compartment amount by the scale factor,
#'     like NONMEM.
#'
#' @param a when using `solve()`, this is equivalent to the
#'     `object` argument.  If you specify `object` later in
#'     the argument list it overwrites this parameter.
#'
#' @param b when using `solve()`, this is equivalent to the
#'     `params` argument.  If you specify `params` as a
#'     named argument, this overwrites the output
#'
#' @param updateObject This is an internally used flag to update the
#'     rxode2 solved object (when supplying an rxode2 solved object) as
#'     well as returning a new object.  You probably should not
#'     modify it's `FALSE` default unless you are willing to
#'     have unexpected results.
#'
#' @param cores Number of cores used in parallel ODE solving.  This
#'    is equivalent to calling [setRxThreads()]
#'
#' @param nCoresRV Number of cores used for the simulation of the
#'   sigma variables.  By default this is 1. To reproduce the results
#'   you need to run on the same platform with the same number of
#'   cores. This is the reason this is set to be one, regardless of
#'   what the number of cores are used in threaded ODE solving.
#'
#' @param nLlikAlloc The number of log likelihood endpoints that are
#'   used in the model.  This allows independent log likelihood per
#'   endpoint in focei for nlmixr2.  It likely shouldn't be set,
#'   though it won't hurt anything if you do (just may take up more
#'   memory for larger allocations).
#'
#' @inheritParams odeMethodToInt
#'
#' @inheritParams rxode2parse
#'
#' @param useStdPow This uses C's `pow` for exponentiation instead of
#'   R's `R_pow` or `R_pow_di`.  By default this is `FALSE`
#'
#' @param ss2cancelAllPending When `TRUE` the `SS=2` event type
#'   cancels all pending doses like `SS=1`.  When `FALSE` the pending
#'   doses not canceled with `SS=2` (the infusions started before
#'   `SS=2` occurred are canceled, though).
#'
#' @param addlKeepsCov This determines if the additional dosing items
#'   repeats the dose only (`FALSE`) or keeps the covariates at the
#'   record of the dose (`TRUE`)
#'
#' @param addlDropSs When there are steady state doses with an `addl`
#'   specification the steady state flag is dropped with repeated
#'   doses (when `TRUE`) or retained (when `FALSE`)
#'
#' @param ssAtDoseTime Boolean that when `TRUE` back calculates the
#'   steady concentration at the actual time of dose, otherwise when
#'   `FALSE` the doses are shifted
#'
#' @param naTimeHandle Determines what time of handling happens when
#'   the time becomes `NA`: current options are:
#'
#'  - `ignore` this ignores the `NA` time input and passes it through.
#'
#'  - `warn` (default) this will produce a warning at the end of the
#'     solve, but continues solving passing through the `NA` time
#'
#'  - `error` this will stop this solve if this is not a parallel
#'     solved ODE (otherwise stopping can crash R)
#'
#' @param ssSolved When `TRUE` this will return the solved steady
#'   state solutions for the linear compartment model.  When `FALSE`
#'   this will solve to steady state using the linear solutions
#'   instead.  This is only used when the method only has `linCmt()`
#'   and does not mix ODEs with the solution.  The default is `TRUE`.
#'
#' @param linCmtSensType The type of linear compartment
#'   sensitivity/gradients to use.  The current options are:
#'
#' - `auto` -- for one compartment models this will use the `AD`
#'   method, for 2 and 3 compartment model this will use `forwardG`.
#'
#' - `AD` -- automatic differentiation (using stan math).  This now
#'   uses forward-mode AD (`stan::math::fvar`), which differentiates the
#'   same analytic solution as the reverse-mode path but on the C++
#'   stack with no reverse autodiff tape, matching the reverse result to
#'   round-off while avoiding the per-call tape setup/teardown.
#'
#' - `ADr` -- reverse-mode automatic differentiation (the prior `AD`
#'   behavior, `stan::math::jacobian`).  Kept as an escape hatch for
#'   validation; `AD` (forward) is preferred.
#'
#' - `forward` -- forward sensitivity where the step size is
#'    determined by shi 2021 optimization (only once per problem)
#'
#' - `forwardG` -- forward sensitivity where the step size is determined by the
#'    Gill 1983 optimization for forward differences (only once per problem).
#'
#' - `central` -- central sensitivity where the step size is
#'    determined by shi 2021 optimization (only once per problem)
#'
#' - `forward3` -- three point central difference where step size is
#'   determined by shi 2021 optimization for central differences (only
#'   once per problem)
#'
#' - `endpoint5` -- five point endpoint difference where step size is
#'   determined by the shi 2021 optimization for central differences
#'   (only once per problem)
#'
#' - `fowardH` -- forward sensitivity where the step size is fixed
#'
#' - `centralH` -- central sensitivity where the step size is fixed
#'
#' - `forward3H` -- three point central difference where step size is fixed
#'
#' - `endpoint5H` -- five point endpoint difference where step size is fixed
#'
#' @param linCmtSensH The step size for the forward and central
#'   differences when using the option `centralH`, `forwardH`,
#'   `foward3H` or `endpoint5H` options.
#'#'
#' @param linCmtGillK The total number of possible steps to determine the
#'     optimal forward/central difference step size per parameter (by
#'     the Gill 1983 method).  If 0, no optimal step size is
#'     determined.  Otherwise this is the optimal step size
#'     determined.
#'
#' @param linCmtGillStep When looking for the optimal forward difference
#'     step size, this is This is the step size to increase the
#'     initial estimate by.  So each iteration the new step size =
#'     (prior step size)*gillStep
#'
#' @param linCmtGillFtol The gillFtol is the gradient error tolerance that
#'     is acceptable before issuing a warning/error about the gradient estimates.
#'
#' @param linCmtScale The scale of the linear compartment model.  This
#'   is applied to sensitivity approximation using numeric
#'   differences.  When `TRUE` or `NULL` use default scaling, when
#'   `FALSE` use no scaling.  If it is one elment numeric, the value
#'   is duplicated 7 times and applies to all the parameters.
#'   Otherwise this is a seven element numeric vector implying the
#'   scaling for each of the linear compartmental model parameters.
#'
#' @param linCmtHcmt This represents the compartments considered when
#'   optimizing the forward difference step size.  When a character
#'   vector it can be any of the following (multiple allowed):
#'
#'   - `"depot"` -- depot compartment
#'
#'   - `"central"` -- central compartment
#'
#'   - `"peripheral1"` -- peripheral compartment
#'
#'   - `"peripheral2"` -- second peripheral compartment
#'
#'   - `"concentration"` -- concentration value (i.e. central
#'      compartment/volume)
#'
#' @param linCmtHmeanI This represents the type of sum done for each
#'   time-point of the linear solved systems (as defined by `"linCmtHcmt"`).
#'
#'  - `"arithmetic"` -- gives the arithmetic mean
#'
#'  - `"geometric"` -- gives the geometric mean
#'
#'  - `"harmonic"` -- gives the harmonic mean
#'
#' @param linCmtHmeanO This represents the type of sum done for the
#'   overall problem of the linear solved systems (first each time
#'   point mean is calculated with `linCmtHmeanI`).
#'
#' - `"arithmetic"` -- gives the arithmetic mean
#'
#' - `"geometric"` -- gives the geometric mean
#'
#' - `"harmonic"` -- gives the harmonic mean
#'
#' @param linCmtSuspect The tolerance for gradients in linear
#'   compartment solutions to re-compute when gradients seem to be
#'   zero.
#'
#' @param linCmtGillRtol The relative tolerance used for Gill 1983
#'   optimal step size determination.
#'
#' @param linCmtShiErr Shi difference error
#'
#' @param linCmtShiMax The maximum number of steps for the
#'   optimization of the forward-difference step size in linear
#'   compartment numeric difference.
#'
#' @param linCmtForwardMax The maximum number of points in a forward
#'   difference to take while calculating the gradients.  This is an
#'   integer from 1 to 3.  There is at least 1 extra point taken for
#'   gradient calculation, if the gradient is suspect another is taken
#'   (if this value is 2), and finally a third is calculated if the
#'   gradient is still suspect.
#'
#' @param maxExtra Integer; maximum number of events (doses and
#'   observations) that `evid_()` may push per individual per solve.
#'   When an individual exceeds this limit the solve is aborted for
#'   that individual (output filled with `NA`) and an error is raised
#'   after the full parallel solve completes.  Set to `0L` to allow
#'   unlimited pushes (use with care -- cascading `evid_()` calls can
#'   grow without bound).  Default is `100L`.
#'
#' @param indOwnAlloc Logical; when `TRUE` each individual's `dose`,
#'   `ii`, `all_times`, and `solve` arrays are allocated independently
#'   via `malloc`/`calloc` rather than as pointers into a single
#'   global buffer.  This enables per-individual reallocation for dose
#'   and observation-pushing with `evid_()` and related
#'   functions. When `FALSE` these arrays are allocated as a single
#'   global buffer, which is slightly faster and slightly more memory
#'   efficient but does not allow for dynamic dosing/observations.  By
#'   default this is `NA` which automatically decides based on if
#'   there is any dosing or observation pushing in the model.
#'
#' @param serializeFile Controls serialization-driven solves. When
#'   this is a file path, `rxSolve()` writes the pre-integration
#'   serialized state to that file and returns the file path invisibly
#'   without running the solve. When this is `TRUE`, `rxSolve()`
#'   writes the serialized state to a temporary `.rxbin` file, solves
#'   by reloading from that file, returns the solve result, and then
#'   removes the temporary file (mostly for testing). When solving
#'   from an existing serialization file via `rxSolve(model,
#'   "state.rxbin")`, only the model and serialization file are
#'   allowed because the file already stores the solve inputs and
#'   controls.
#'
#' @param dense Logical; when `TRUE` and the method supports dense output,
#'   enables continuous interpolation so the solver can take large internal
#'   steps and reconstruct the solution cheaply at each observation time.
#'   Dense-capable single methods are `"dop853"`, `"dop5"`, `"bs"`, and
#'   `"ros4"`.  For composite AutoSwitch methods (e.g. `"dop5+ros4"`), dense
#'   output is enabled only when **both** the primary and stiff secondary
#'   support dense output; `"ros4"` is the only stiff method that does, so
#'   valid dense composites are `"dop853+ros4"`, `"dop5+ros4"`, and
#'   `"bs+ros4"`.  A warning is issued and `dense` is set to `FALSE` when a
#'   composite stiff secondary does not support dense output.  Silently
#'   ignored for non-dense single methods.  Not yet supported for `linCmt()`
#'   models (a warning is emitted and the standard path is used instead).
#'
#' @param cvodeLinSolver Character; selects the linear solver used by the CVODE
#'   integrator when `method = "cvode"`.  Ignored for all other methods.
#'   Available choices and when to use them:
#'
#'   * `"dense"` (default) -- Direct LU factorization of the full Jacobian.
#'     Best for small to medium systems (typically fewer than ~50 compartments).
#'     Requires `O(n^2)` storage and `O(n^3)` work per Newton iteration.
#'
#'   * `"band"` -- Currently aliases to `"dense"`.  Kept for compatibility
#'     until rxode2 can determine model bandwidth safely during solve setup.
#'
#'   * `"gmres"` -- Generalized Minimal Residual iterative Krylov solver.
#'     Avoids explicit Jacobian formation, making it practical for large stiff
#'     systems (tens to hundreds of compartments) where LU factorization would
#'     dominate runtime.  Generally the first iterative solver to try.
#'
#'   * `"bicgstab"` -- Bi-Conjugate Gradient Stabilized iterative Krylov
#'     solver.  Similar use case to `"gmres"` but can converge faster on some
#'     non-symmetric problems.  Try if `"gmres"` is slow or fails to converge.
#'
#'   * `"tfqmr"` -- Transpose-Free Quasi-Minimal Residual iterative Krylov
#'     solver.  Another alternative for large systems; avoids the matrix
#'     transpose required by classical QMR.
#'
#'   For most pharmacometric models with fewer than ~50 compartments the default
#'   `"dense"` solver is fastest.  Switch to an iterative solver (`"gmres"` is
#'   a good first choice) only for large QSP or PBPK models where Jacobian
#'   factorization becomes the bottleneck.
#'
#' @param autoSwitchNonstifftol Numeric in `(0, 1]`; stiffness ratio threshold
#'   used when the solver is in non-stiff mode.  If
#'   `rho * |dt| / S(primary) > autoSwitchNonstifftol`, the interval is
#'   considered stiff and the secondary solver is tried.  Default `9/10`.
#'
#' @param autoSwitchStifftol Numeric in `(0, 1]`; non-stiffness ratio threshold
#'   used when the solver is in stiff mode.  If
#'   `rho * |dt| / S(primary) < autoSwitchStifftol`, the interval is considered
#'   non-stiff and the switch-back counter is incremented.  Default `9/10`.
#'
#' @param autoSwitchDtfac Numeric `>= 1`; factor by which the suggested step
#'   size is multiplied when switching to the stiff solver, and divided when
#'   switching back.  Default `2.0`.
#'
#' @param autoSwitchMaxStiff Integer; number of consecutive stiff-detected
#'   intervals before permanently switching to the stiff solver.  Default `10L`.
#'
#' @param autoSwitchMaxNonstiff Integer; number of consecutive non-stiff
#'   intervals (while in stiff mode) before switching back to the fast
#'   non-stiff solver.  Default `3L`.
#'
#' @param autoSwitchStiffFirst Logical; when `TRUE`, start each subject solve
#'   with the stiff solver instead of the non-stiff primary.  Default `FALSE`.
#'
#' @param autoSwitchSwitchMax Non-negative integer; minimum number of
#'   integration intervals that must elapse after a switch before the solver
#'   is allowed to switch back in the opposite direction.  Acts as an
#'   oscillation guard: if the stiffness ratio fluctuates near a threshold,
#'   this prevents rapid back-and-forth between solvers.  Set to `0L` to
#'   disable the guard entirely.  Default `5L`.
#'
#' @param stiff2 Integer method code for the stiff secondary solver used in
#'   AutoSwitch composite methods.  Normally set automatically when `method` is
#'   a composite string of the form `"primary+stiff"` (e.g.
#'   `"dop853+ros4"`); the `"+"` notation is the preferred way to configure
#'   composite solving and `stiff2` need not be supplied directly.  When
#'   supplied as a raw integer it must be a stiff method code as returned by
#'   [odeMethodToInt()]; `0L` (the default) disables the stiff secondary and
#'   causes the solver to run as a plain non-composite method.  Dense output
#'   (`dense = TRUE`) is silently disabled when `stiff2` names a stiff method
#'   that does not support dense output; `"ros4"` (code `13L`) is the only
#'   stiff secondary that does support it.
#'
#' @param useLinCmt Logical; when `TRUE` and the model contains
#'   linear-compartment ODEs that can be solved analytically,
#'   automatically convert them to a `linCmt()` call before solving.
#'   The detection and conversion use [odeToLin()]; the converted
#'   model is cached so the compilation cost is paid only once.  Set
#'   to `FALSE` to keep the original ODE solver.  This flag is also
#'   stored in the returned [rxControl()] object so that downstream
#'   hooks (e.g. in nlmixr2) can read and apply it.  The default is to
#'   use the value of `rxode2.useLinCmt` option (which when specified
#'   is `TRUE` by default).
#'
#' @return An \dQuote{rxSolve} solve object that stores the solved
#'   value in a special data.frame or other type as determined by
#'   `returnType`. By default this has as many rows as there are
#'   sampled time points and as many columns as system variables (as
#'   defined by the ODEs and additional assignments in the rxode2 model
#'   code).  It also stores information about the call to allow
#'   dynamic updating of the solved object.
#'
#'   The operations for the object are similar to a data-frame, but
#'   expand the `$` and `[[""]]` access operators and assignment
#'   operators to resolve based on different parameter values, initial
#'   conditions, solver parameters, or events (by updating the `time`
#'   variable).
#'
#'   You can call the [eventTable()] methods on the solved object to
#'   update the event table and resolve the system of equations.
#'
#' @references
#'
#'  "New Scaling and Squaring Algorithm for the Matrix Exponential", by
#'  Awad H. Al-Mohy and Nicholas J. Higham, August 2009
#'
#' Roger B. Sidje (1998).  EXPOKIT: Software package for computing
#' matrix exponentials.  ACM - Transactions on Mathematical Software
#' *24*(1), 130-156.
#'
#' Hindmarsh, A. C.
#' *ODEPACK, A Systematized Collection of ODE Solvers*.
#' Scientific Computing, R. S. Stepleman et al. (Eds.),
#' North-Holland, Amsterdam, 1983, pp. 55-64.
#'
#' Petzold, L. R.
#' *Automatic Selection of Methods for Solving Stiff and Nonstiff
#' Systems of Ordinary Differential Equations*.
#' Siam J. Sci. Stat. Comput. 4 (1983), pp. 136-148.
#'
#' Hairer, E., Norsett, S. P., and Wanner, G.
#' *Solving ordinary differential equations I, nonstiff problems*.
#' 2nd edition, Springer Series in Computational Mathematics,
#' Springer-Verlag (1993).
#'
#' @seealso [rxode2()]
#' @author Matthew Fidler, Melissa Hallow and  Wenping Wang
#' @export
rxSolve <- function(object, params = NULL, events = NULL, inits = NULL,
                    scale = NULL, method = c("liblsoda", "lsoda", "dop853", "indLin", "f78", "rk4", "ck54", "ab", "abm", "dop5", "bs", "ros4", "iem", "sem", "sb3a", "sb3am4", "vv", "mm", "em", "cvode", "trapz", "ssp3", "f32", "rk43", "dop54", "vern65", "vern76", "dop87", "vern98", "ros43", "ros6", "backwardEuler", "gauss6", "iiic6", "radauiia5", "geng5", "sdirk43", "euler", "midpoint", "heun", "ssp22", "rk3", "ssp53", "s4", "r4", "ls44", "ls54", "ssp54", "s5", "rk5", "c5", "l5", "lk5a", "lk5b", "b6", "s7", "s8_10", "cv8", "s8_12", "s10", "z10", "o10", "h10", "dp54", "v65e", "v76e", "dp87", "v98e", "ssp33", "bs32", "ssp43", "f45", "t54", "s54", "pp54", "pp54b", "bs54", "ss54", "dp65", "c65", "tp64", "v65r", "v65", "dverk65", "tf65", "tp75", "tmy7", "tmy7s", "v76r", "ss76", "v78", "dverk78", "dp85", "tp86", "v87e", "v87r", "ev87", "k87", "f89", "v89", "t98a", "v98r", "s98", "f108", "c108", "b109", "s1110a", "f1210", "o129", "f1412", "lsode", "bdf", "rk4s", "eulers", "midpoints", "heuns", "dop5s", "dop853s", "ck54s", "bs32s", "vern65s", "vern76s", "dop87s", "f78s", "ros4s", "radauiia5s", "backwardEulers", "gauss6s", "sdirk43s", "iiic6s", "ros43s", "ros6s", "geng5s", "rk3s", "rk43s"),

                    sigdig=NULL,
                    atol = 1.0e-8, rtol = 1.0e-6,
                    maxsteps = 70000L, hmin = 0, hmax = NA_real_,
                    hmaxSd = 0, hini = 0, maxordn = 12L, maxords = 5L, order = 5L, ...,
                    cores,
                    covsInterpolation = c("locf", "linear", "nocb", "midpoint"),
                    naInterpolation = c("locf", "nocb"),
                    keepInterpolation=c("na", "locf", "nocb"),
                    addCov = TRUE, sigma = NULL, sigmaDf = NULL,
                    sigmaLower = -Inf, sigmaUpper = Inf,
                    nCoresRV = 1L, sigmaIsChol = FALSE,
                    sigmaSeparation = c("auto", "lkj", "separation"),
                    sigmaXform = c("identity", "variance", "log", "nlmixrSqrt", "nlmixrLog", "nlmixrIdentity"),
                    nDisplayProgress = 10000L,
                    amountUnits = NA_character_, timeUnits = "hours",
                    theta = NULL,
                    thetaLower = -Inf, thetaUpper = Inf,
                    eta = NULL, addDosing = FALSE,
                    stateTrim = Inf, updateObject = FALSE,
                    omega = NULL, omegaDf = NULL, omegaIsChol = FALSE,
                    omegaSeparation = c("auto", "lkj", "separation"),
                    omegaXform = c("variance", "identity", "log", "nlmixrSqrt", "nlmixrLog", "nlmixrIdentity"),
                    omegaLower = -Inf, omegaUpper = Inf,
                    nSub = 1L, thetaMat = NULL, thetaDf = NULL, thetaIsChol = FALSE,
                    nStud = 1L, dfSub = 0.0, dfObs = 0.0,
                    returnType = c("rxSolve", "matrix", "data.frame", "data.frame.TBS", "data.table", "tbl", "tibble"),
                    seed = NULL, nsim = NULL,
                    minSS = 10L, maxSS = 10000L,
                    infSSstep = 12,
                    strictSS = TRUE,
                    istateReset = TRUE,
                    subsetNonmem = TRUE,
                    maxAtolRtolFactor = 0.1,
                    from = NULL,
                    to = NULL,
                    by = NULL,
                    length.out = NULL,
                    iCov = NULL,
                    keep = NULL,
                    indLinPhiTol = 1e-7,
                    indLinPhiM = 0L,
                    indLinMatExpType = c("expokit", "Al-Mohy", "arma"),
                    indLinMatExpOrder = 6L,
                    drop = NULL,
                    idFactor = TRUE,
                    mxhnil = 0,
                    hmxi = 0.0,
                    warnIdSort = TRUE,
                    warnDrop = TRUE,
                    ssAtol = 1.0e-8,
                    ssRtol = 1.0e-6,
                    safeZero = TRUE,
                    safeLog = TRUE,
                    safePow = TRUE,
                    sumType = c("pairwise", "fsum", "kahan", "neumaier", "c"),
                    prodType = c("long double", "double", "logify"),
                    resample = NULL,
                    resampleID = TRUE,
                    maxwhile = 100000,
                    atolSens = 1.0e-8,
                    rtolSens = 1.0e-6,
                    ssAtolSens=1.0e-8,
                    ssRtolSens=1.0e-6,
                    simVariability=NA,
                    nLlikAlloc=NULL,
                    useStdPow=FALSE,
                    naTimeHandle=c("ignore", "warn", "error"),
                    addlKeepsCov=FALSE,
                    addlDropSs=TRUE,
                    ssAtDoseTime=TRUE,
                    ss2cancelAllPending=FALSE,
                    ssSolved=TRUE,
                    linCmtSensType=c("auto",
                                     "endpoint5", "endpoint5G",
                                     "forward3", "forward3G",
                                     "AD", "ADr", "central",
                                     "forward", "forwardG",
                                     "forwardH", "centralH", "forward3H",
                                     "endpointH5", "forwardG"),
                    linCmtSensH=0.0001,
                    linCmtGillFtol=0,
                    linCmtGillK=20L,
                    linCmtGillStep=4,
                    linCmtGillRtol=sqrt(.Machine$double.eps),
                    linCmtShiErr=sqrt(.Machine$double.eps),
                    linCmtShiMax=20L,
                    linCmtScale=FALSE,
                    linCmtHcmt=NULL,
                    linCmtHmeanI=c("geometric", "arithmetic", "harmonic"),
                    linCmtHmeanO=c("geometric", "arithmetic", "harmonic"),
                    linCmtSuspect=1e-6,
                    linCmtForwardMax=2L,
                    indOwnAlloc=NA,
                    maxExtra=1000L,
                    tolFactor=NULL,
                    serializeFile=NULL,
                    dense=FALSE,
                    cvodeLinSolver=c("dense", "band", "gmres", "bicgstab", "tfqmr"),
                    autoSwitchNonstifftol=9/10,
                    autoSwitchStifftol=9/10,
                    autoSwitchDtfac=2.0,
                    autoSwitchMaxStiff=10L,
                    autoSwitchMaxNonstiff=3L,
                    autoSwitchStiffFirst=FALSE,
                    autoSwitchSwitchMax=5L,
                    stiff2=0L,
                    useLinCmt=getOption("rxode2.useLinCmt", TRUE),
                    envir=parent.frame()) {
  .udfEnvSet(list(envir, parent.frame(1))) # nolint
  if (is.null(object)) {
    .xtra <- list(...)
    .nxtra <- names(.xtra)
    .w <- which(regexpr("^[Ss][0-9]+$", .nxtra) != -1)
    if (length(.w) > 0) {
      for (.arg in .w) {
        checkmate::assertNumeric(.xtra[[.arg]], lower=0,
                                 finite=TRUE, len=1, .var.name=.nxtra[.arg])
      }
      .bad <- .nxtra[-.w]
    } else {
      .bad <- .nxtra
    }
    .bad <- .bad[!(.bad %in% c(".setupOnly", "keepF", ".zeros"))]
    if (length(.bad) > 0) {
      if ("transitAbs" %in% .bad) {
        stop("'transitAbs' is no longer supported, use 'evid=7' instead",
             call.=FALSE)
      }
      stop("unused argument: ",
           paste(paste0("'", .bad, "'", sep=""), collapse=", "),
           call.=FALSE)
    }
    if (checkmate::testIntegerish(sigmaXform, len=1L, lower=1L,
                                  upper=6L, any.missing=FALSE)) {
      .sigmaXform <- as.integer(sigmaXform)
    } else {
      .sigmaXform <- c(
        "variance" = 6L, "log" = 5L, "identity" = 4L,
        "nlmixrSqrt" = 1L, "nlmixrLog" = 2L,
        "nlmixrIdentity" = 3L
      )[match.arg(sigmaXform)]
    }

    if (checkmate::testIntegerish(linCmtHmeanI, len=1L, lower=1L,
                                  upper=3L, any.missing=FALSE)) {
    } else if (checkmate::testCharacter(linCmtHmeanI, any.missing=FALSE)) {
      linCmtHmeanI <- c("arithmetic"=1L,
                        "geometric"=2L,
                        "harmonic"=3L)[match.arg(linCmtHmeanI)]
    } else {
      stop("linCmtHmeanI must be a character vector of 'arithmetic', 'geometric', or 'harmonic' or an integer between 1 and 3",
           call.=FALSE)
    }

    if (checkmate::testIntegerish(linCmtHmeanO, len=1L, lower=1L,
                                  upper=3L, any.missing=FALSE)) {
    } else if (checkmate::testCharacter(linCmtHmeanO, any.missing=FALSE)) {
      linCmtHmeanO <- c("arithmetic"=1L,
                        "geometric"=2L,
                        "harmonic"=3L)[match.arg(linCmtHmeanO)]
    } else {
      stop("linCmtHmeanO must be a character vector of 'arithmetic', 'geometric', or 'harmonic' or an integer between 1 and 3",
           call.=FALSE)
    }

    checkmate::assertNumeric(linCmtSuspect, lower=0, finite=TRUE, len=1)
    checkmate::assertIntegerish(linCmtForwardMax, lower=1, upper=3, any.missing=FALSE)

    if (is.null(linCmtHcmt)) {
      linCmtHcmt <- 1L
    } else if (checkmate::testIntegerish(linCmtHcmt, len=1L, lower=1L,
                                         upper=31L, any.missing=FALSE)) {
      # ok value
    } else if (checkmate::testCharacter(linCmtHcmt, any.missing=FALSE)) {
      .vars <- match.arg(linCmtHcmt,
                         c("depot", "central", "peripheral1", "peripheral2",
                           "concentration"), several.ok = TRUE)
      linCmtHcmt <- sum(vapply(.vars, function(x) {
        switch(x,
               depot         = 8L,
               central       = 1L,
               peripheral1   = 2L,
               peripheral2   = 4L,
               concentration = 16L)
      }, integer(1)))
    } else {
      stop("linCmtHcmt must be a character vector of 'depot', 'central', 'peripheral1', 'peripheral2', or 'concentration' or an integer between 1 and 31",
           call.=FALSE)
    }

    if (checkmate::testIntegerish(omegaXform, len=1L, lower=1L,
                                  upper=6L, any.missing=FALSE)) {
      .omegaXform <- as.integer(omegaXform)
    } else {
      .omegaXform <- c(
        "variance" = 6L, "log" = 5L,
        "identity" = 4L, "nlmixrSqrt" = 1L,
        "nlmixrLog" = 2L,
        "nlmixrIdentity" = 3L
      )[match.arg(omegaXform)]
    }
    if (!is.null(seed)) {
      # Depending on the kind of seed, seed may be a vector of
      # integers, though that use-case is likely not common
      checkmate::testIntegerish(seed, any.missing=FALSE, min.len=1)
      set.seed(seed)
    }
    if (checkmate::testIntegerish(nsim, len=1, lower=1, any.missing=FALSE)) {
      if (nSub == 1L && (rxIs(params, "rxEt") || rxIs(events, "rxEt") ||
                         rxIs(params, "eventTable") || rxIs(events, "eventTable"))) {
        nSub <- as.integer(nsim)
      } else if (nStud == 1L) {
        nStud <- as.integer(nsim)
      }
    }
    method <- odeMethodToInt(method)
    if (length(method) == 2L && !is.null(names(method)) && "primary" %in% names(method)) {
      stiff2 <- as.integer(unname(method["stiff"]))
      method <- as.integer(unname(method["primary"]))
    } else {
      stiff2 <- as.integer(stiff2)
    }
    checkmate::assertNumeric(as.numeric(autoSwitchNonstifftol), lower=0, upper=1, len=1, any.missing=FALSE)
    checkmate::assertNumeric(as.numeric(autoSwitchStifftol), lower=0, upper=1, len=1, any.missing=FALSE)
    checkmate::assertNumeric(as.numeric(autoSwitchDtfac), lower=1, len=1, any.missing=FALSE)
    checkmate::assertIntegerish(autoSwitchMaxStiff, lower=1L, len=1, any.missing=FALSE)
    checkmate::assertIntegerish(autoSwitchMaxNonstiff, lower=1L, len=1, any.missing=FALSE)
    checkmate::assertIntegerish(autoSwitchSwitchMax, lower=0L, len=1, any.missing=FALSE)
    if (is.logical(autoSwitchStiffFirst)) {
      checkmate::assertLogical(autoSwitchStiffFirst, len=1, any.missing=FALSE)
    } else {
      checkmate::assertIntegerish(autoSwitchStiffFirst, lower=0L, upper=1L, len=1, any.missing=FALSE)
    }
    if (checkmate::testIntegerish(cvodeLinSolver, len=1, lower=1L, upper=5L,
                                  any.missing=FALSE)) {
      cvodeLinSolver <- as.integer(cvodeLinSolver)
    } else {
      cvodeLinSolver <- c("dense"=1L, "band"=2L, "gmres"=3L,
                          "bicgstab"=4L, "tfqmr"=5L)[match.arg(cvodeLinSolver)]
    }
    if (checkmate::testIntegerish(returnType, len=1, lower=0,
                                  upper=5, any.missing=FALSE)) {
      returnType <- as.integer(returnType)
    } else {
      .matrixIdx <- c("rxSolve" = 0L, "matrix" = 1L,
                      "data.frame" = 2L, "data.frame.TBS" = 3L,
                      "data.table" = 4L, "tbl" = 5L, "tibble" = 5L)
      returnType <- .matrixIdx[match.arg(returnType)]
    }
    if (checkmate::testIntegerish(covsInterpolation, len=1, lower=0,
                                  upper=3, any.missing=FALSE)) {
      covsInterpolation <- as.integer(covsInterpolation)
    } else {
      covsInterpolation <- c("linear"=0L, "locf"=1L,
                             "nocb"=2L, "midpoint"=3L)[match.arg(covsInterpolation)]
    }
    if (checkmate::testIntegerish(naInterpolation, len=1, lower=0,
                                  upper=1, any.missing=FALSE)) {
      naInterpolation <- as.integer(naInterpolation)
    } else {
      naInterpolation <- c("locf"=1L, "nocb"=0L)[match.arg(naInterpolation)]
    }
    if (checkmate::testIntegerish(keepInterpolation, len=1, lower=0,
                                  upper=2, any.missing=FALSE)) {
      keepInterpolation <- as.integer(keepInterpolation)
    } else {
      keepInterpolation <- c("locf"=1L, "nocb"=0L, "na"=2L)[match.arg(keepInterpolation)]
    }
    if (missing(naTimeHandle) && !is.null(getOption("rxode2.naTimeHandle", NULL))) {
      naTimeHandle <- getOption("rxode2.naTimeHandle")
    }
    if (checkmate::testIntegerish(naTimeHandle, len=1, lower=1, upper=3, any.missing=FALSE)) {
      naTimeHandle <- as.integer(naTimeHandle)
    } else {
      naTimeHandle <- c("ignore"=1L, "warn"=2L, "error"=3L)[match.arg(naTimeHandle)]
    }
    if (any(names(.xtra) == "covs")) {
      stop("covariates can no longer be specified by 'covs' include them in the event dataset", .call = FALSE)
    }
    if (missing(cores)) {
      cores <- 0L
    } else if (!missing(cores)) {
      checkmate::assertIntegerish(cores, lower = 0L, len = 1)
      cores <- as.integer(cores)
    }
    if (inherits(sigma, "logical")) {
      .sigma <- sigma
    } else if (inherits(sigma, "character")) {
      .sigma <- sigma
      checkmate::assertNumeric(dfObs, lower=length(sigma), finite=TRUE,
                               any.missing=FALSE, len=1)
    } else {
      .sigma <- lotri(sigma)
    }
    if (inherits(omega, "logical")) {
      .omega <- omega
    }  else if (inherits(omega, "character")) {
      .omega <- omega
      checkmate::testNumeric(dfSub, lower=length(omega), finite=TRUE,
                             any.missing=FALSE, len=1)
    } else if (inherits(omega, "lotri")) {
      .omega <- omega
    } else {
      .omega <- lotri(omega)
    }
    if (checkmate::testIntegerish(indLinMatExpType, len=1, lower=1, upper=3, any.missing=FALSE)) {
      .indLinMatExpType <- as.integer(indLinMatExpType)
    } else {
      .indLinMatExpTypeIdx <- c("Al-Mohy" = 3L, "arma" = 1L, "expokit" = 2L)
      .indLinMatExpType <- .indLinMatExpTypeIdx[match.arg(indLinMatExpType)]
    }
    if (checkmate::testIntegerish(sumType, len=1, lower=1,
                                  upper=5, any.missing=FALSE)) {
      .sum <- as.integer(sumType)
    } else {
      .sum <- c("pairwise"=1L, "fsum"=2L, "kahan"=3L , "neumaier"=4L, "c"=5L)[match.arg(sumType)]
    }
    if (checkmate::testIntegerish(linCmtSensType)) {
      .linCmtSensType <- as.integer(linCmtSensType)
    } else {
      .linCmtSensType <- c("AD"=3L,        # forward-mode AD (fvar) -- default AD
                           "ADr"=31L,      # reverse-mode AD (escape hatch)
                           "forward"=1L,
                           "central"=2L,
                           "forward3"=4L,
                           "endpoint5"=5L,
                           # Gill differences
                           "forwardG"=6L,
                           "forward3G"=7L,
                           "endpoint5G"=8L,
                           # Fixed step sizes
                           "forward3H"=40L,
                           "endpoint5H"=50L,
                           "forwardH"=10L,
                           "centralH"=20L,
                           "auto"=100L)[match.arg(linCmtSensType)]
    }
    if (is.logical(linCmtScale)) {
      checkmate::assertLogical(linCmtScale, len=1, any.missing=FALSE)
      if (linCmtScale) {
        linCmtScale <- c(1, 1, 1, 1, 1, 1, 1)
      } else {
        linCmtScale <- c(0, 0, 0, 0, 0, 0, 0) # only first needs to be zero
      }
    }
    if (is.null(linCmtScale)) {
      linCmtScale <- c(1, 1, 1, 1, 1, 1, 1)
    }
    if (checkmate::testNumeric(linCmtScale, lower=0, finite=TRUE,
                               any.missing=FALSE, len=1)) {
      linCmtScale <- rep(linCmtScale, 7L)
    }
    checkmate::assertNumeric(linCmtScale, lower=0, finite=TRUE, any.missing=FALSE, len=7)
    checkmate::assertNumeric(linCmtSensH, lower=0, finite=TRUE, any.missing=FALSE, len=1)
    checkmate::assertNumeric(linCmtGillFtol, lower=0, finite=TRUE, any.missing=FALSE, len=1)
    checkmate::assertIntegerish(linCmtGillK, lower=0, any.missing=FALSE, len=1)
    checkmate::assertNumeric(linCmtGillStep, lower=0, finite=TRUE,
                             any.missing=FALSE, len=1)
    checkmate::assertNumeric(linCmtGillRtol, lower=0, finite=TRUE,
                             any.missing=FALSE, len=1)

    checkmate::assertNumeric(linCmtShiErr, lower=0, finite=TRUE,
                             any.missing=FALSE, len=1)
    checkmate::assertIntegerish(linCmtShiMax, lower=0, any.missing=FALSE, len=1)

    if (checkmate::testIntegerish(prodType, len=1, lower=1, upper=3, any.missing=FALSE)) {
      .prod <- as.integer(prodType)
    } else {
      .prod <- c("long double"=1L, "double"=1L, "logify"=1L)[match.arg(prodType)]
    }

    if (checkmate::testIntegerish(strictSS, len=1, lower=0, upper=1, any.missing=FALSE)) {
      strictSS <- as.integer(strictSS)
    } else {
      checkmate::assertLogical(strictSS, any.missing=FALSE, len=1)
      strictSS <- as.integer(strictSS)
    }
    checkmate::assertIntegerish(indLinMatExpOrder, len=1, lower=1, any.missing=FALSE)
    indLinMatExpOrder <- as.integer(indLinMatExpOrder)
    if (!checkmate::testIntegerish(safeZero, lower=0, upper=1,
                                   len=1, any.missing=FALSE)) {
      checkmate::assertLogical(safeZero, len=1, any.missing=FALSE)
    }
    safeZero <- as.integer(safeZero)
    if (!checkmate::testIntegerish(safeLog, lower=0, upper=1,
                                   len=1, any.missing=FALSE)) {
      checkmate::assertLogical(safeLog, len=1, any.missing=FALSE)
    }
    safeLog <- as.integer(safeLog)
    if (!checkmate::testIntegerish(safePow, lower=0, upper=1, len=1,
                                   any.missing=FALSE)) {
      checkmate::assertLogical(safePow, len=1, any.missing=FALSE)
    }
    if (!checkmate::testIntegerish(indOwnAlloc, lower=-1, upper=1,
                                   len=1, any.missing=FALSE)) {
      checkmate::assertLogical(indOwnAlloc, len=1, any.missing=TRUE)
      if (is.na(indOwnAlloc)) {
        indOwnAlloc <- -1L
      } else {
        indOwnAlloc <- as.integer(indOwnAlloc)
      }
    }
    safePow <- as.integer(safePow)
    if (is.null(scale)) {
    } else if (is.list(scale)) {
      checkmate::assertList(scale, types="double", any.missing=FALSE,names="strict")
      lapply(names(scale), function(n) {
        checkmate::assertNumeric(scale[[n]], lower=0, finite=TRUE,
                                 any.missing=FALSE, len=1, .var.name=n)
      })
    } else {
      checkmate::assertNumeric(scale, lower=0, finite=TRUE, any.missing=FALSE,names="strict")
    }
    if (!is.null(sigdig)) {
      checkmate::assertNumeric(sigdig, lower=2, finite=TRUE, any.missing=FALSE, len=1)
      if (missing(atol)) {
        atol <- 0.5 * 10^(-sigdig - 2)
      }
      if (missing(rtol)) {
        rtol <- 0.5 * 10^(-sigdig - 2)
      }
      if (missing(atolSens)) {
        atolSens <- 0.5 * 10^(-sigdig - 1.5)
      }
      if (missing(rtolSens)) {
        rtolSens <- 0.5 * 10^(-sigdig - 1.5)
      }
      if (missing(ssAtol)) {
        ssAtol <- 0.5 * 10^(-sigdig)
      }
      if (missing(ssRtol)) {
        ssRtol <- 0.5 * 10^(-sigdig)
      }
      if (missing(ssAtolSens)) {
        ssAtolSens <- 0.5 * 10^(-sigdig + 0.625)
      }
      if (missing(ssRtolSens)) {
        ssRtolSens <- 0.5 * 10^(-sigdig + 0.625)
      }
    }
    checkmate::assertNumeric(atol, lower=0, finite=TRUE, any.missing=FALSE, min.len=1)
    checkmate::assertNumeric(rtol, lower=0, finite=TRUE, any.missing=FALSE, min.len=1)
    checkmate::assertNumeric(atolSens, lower=0, finite=TRUE, any.missing=FALSE, len=1)
    checkmate::assertNumeric(rtolSens, lower=0, finite=TRUE, any.missing=FALSE, len=1)
    checkmate::assertNumeric(ssAtol, lower=0, finite=TRUE, any.missing=FALSE, min.len=1)
    checkmate::assertNumeric(ssRtol, lower=0, finite=TRUE, any.missing=FALSE, min.len=1)
    checkmate::assertNumeric(ssAtolSens, lower=0, finite=TRUE, any.missing=FALSE, len=1)
    checkmate::assertNumeric(ssRtolSens, lower=0, finite=TRUE, any.missing=FALSE, len=1)
    checkmate::assertIntegerish(maxsteps, lower=1, any.missing=FALSE, len=1)
    maxsteps <- as.integer(maxsteps)
    checkmate::assertNumeric(hmin, lower=0, finite=TRUE, any.missing=FALSE, len=1)
    checkmate::assertNumeric(hmax, lower=0, any.missing=TRUE, null.ok=TRUE,
                             finite=TRUE, len=1)
    checkmate::assertNumeric(hmaxSd, lower=0, any.missing=FALSE, null.ok=FALSE, finite=TRUE, len=1)
    checkmate::assertNumeric(hini, lower=0, any.missing=FALSE, null.ok=FALSE, finite=TRUE, len=1)
    checkmate::assertIntegerish(maxordn, lower=1, upper=12, any.missing=FALSE, len=1)
    maxordn <- as.integer(maxordn)
    if (method == 8L || method == 9L || method == 19L) {
      if (method == 19L) {
        checkmate::assertIntegerish(order, lower=1, any.missing=FALSE, len=1)
      } else {
        checkmate::assertIntegerish(order, lower=1, upper=8, any.missing=FALSE, len=1)
      }
      maxordn <- as.integer(order)
    }
    checkmate::assertIntegerish(maxords, lower=1, upper=5, any.missing=FALSE, len=1)
    maxods <- as.integer(maxords)
    checkmate::assertIntegerish(mxhnil, lower=0, any.missing=FALSE, len=1)
    mxhnil <- as.integer(mxhnil)
    checkmate::assertIntegerish(hmxi, lower=0, any.missing=FALSE, len=1)
    checkmate::assertLogical(istateReset, any.missing=TRUE, len=1)
    checkmate::assertLogical(simVariability, len=1)
    checkmate::assertLogical(dense, len=1, any.missing=FALSE)
    if (isTRUE(dense) && stiff2 > 0L && stiff2 != 13L) {
      warning("dense output is not supported for the stiff method of this composite; ignoring dense=TRUE",
              call.=FALSE)
      dense <- FALSE
    }
    if (isTRUE(dense) && method %in% c(0L, 10L, 11L, 13L) &&
        (stiff2 == 0L || stiff2 == 13L) && missing(hmax)) {
      ## .minfo("dense=TRUE: setting hmax=NULL so the solver can take steps larger than the observation spacing")
      hmax <- NULL
    }
    if (isTRUE(dense) && method == 7L) {
      warning("dense output is not supported for ck54. Ignoring dense=TRUE", call.=FALSE)
      dense <- FALSE
    }
    checkmate::assertNumeric(indLinPhiTol, lower=0, any.missing=FALSE, len=1)
    checkmate::assertIntegerish(indLinPhiM, lower=0L, any.missing=FALSE, len=1)
    indLinPhiM <- as.integer(indLinPhiM)
    checkmate::assertIntegerish(minSS, lower=5L, any.missing=FALSE, len=1)
    checkmate::assertIntegerish(maxSS, lower=7L, any.missing=FALSE, len=1)
    if (maxSS <= minSS) stop("'maxSS' must be larger than 'minSS'", call.=FALSE)
    checkmate::assertNumeric(infSSstep, lower=6, any.missing=FALSE, len=1)
    checkmate::assertNumeric(maxAtolRtolFactor, lower=0.01, any.missing=FALSE,
                             finite=TRUE, null.ok=FALSE, len=1)
    if (!is.null(tolFactor))
      checkmate::assertNumeric(tolFactor, lower=1.0, finite=TRUE, any.missing=FALSE, .var.name="tolFactor")
    checkmate::assertNumeric(from, null.ok=TRUE, finite=TRUE, any.missing=FALSE, len=1)
    checkmate::assertNumeric(to, null.ok=TRUE, finite=TRUE, any.missing=FALSE, len=1)
    checkmate::assertNumeric(by, null.ok=TRUE, finite=TRUE, any.missing=FALSE, len=1)
    checkmate::assertIntegerish(length.out, lower=0, any.missing=FALSE,
                                null.ok=TRUE, len=1)
    checkmate::assertLogical(addCov, len=1, any.missing=FALSE)
    checkmate::assertIntegerish(nCoresRV, len=1, lower=1)
    checkmate::assertLogical(sigmaIsChol,len=1, any.missing=FALSE)
    checkmate::assertIntegerish(nDisplayProgress, len=1, lower=100, any.missing=FALSE)
    checkmate::assertCharacter(amountUnits, any.missing=TRUE, len=1)
    checkmate::assertCharacter(timeUnits, any.missing=TRUE, len=1)
    checkmate::assertLogical(addDosing, any.missing=TRUE, null.ok=TRUE, len=1)
    checkmate::assertLogical(subsetNonmem, any.missing=FALSE, null.ok=FALSE, len=1)
    checkmate::assertNumeric(sigmaDf, any.missing=FALSE, lower=0, len=1, null.ok=TRUE)
    checkmate::assertNumeric(stateTrim, min.len=1, max.len=2)
    checkmate::assertLogical(updateObject, len=1, any.missing=FALSE)
    checkmate::assertNumeric(omegaDf, any.missing=FALSE, lower=0, len=1, null.ok=TRUE)
    checkmate::assertLogical(omegaIsChol,len=1, any.missing=FALSE)
    checkmate::assertIntegerish(nSub, len=1, lower=1)
    nSub <- as.integer(nSub)
    checkmate::assertMatrix(thetaMat, col.names="strict", null.ok=TRUE, mode="numeric")
    checkmate::assertNumeric(thetaDf, any.missing=FALSE, lower=0, len=1, null.ok=TRUE)
    checkmate::assertLogical(thetaIsChol,len=1, any.missing=FALSE,  null.ok=TRUE)
    checkmate::assertIntegerish(nStud, len=1, any.missing=FALSE, lower=1)
    checkmate::assertNumeric(dfSub, len=1, any.missing=FALSE, finite=TRUE, lower=0.0)
    checkmate::assertNumeric(dfObs, len=1, any.missing=FALSE, finite=TRUE, lower=0.0)
    # iCov = data.frame
    checkmate::assertDataFrame(iCov, null.ok=TRUE)
    .invalidKeep <- c("id", "sim.id", "resetno", "time")
    .invalidKeep <- intersect(tolower(keep), tolower(.invalidKeep))
    if (length(.invalidKeep) > 0) {
      .w <- which(tolower(keep) %in% .invalidKeep)
      keep <- keep[-.w]
      warning("'keep' contains ", paste(.invalidKeep, collapse=", "), "\nwhich are output when needed, ignoring these items", call.=FALSE)
    }
    .invalidKeep <- c("evid",  "ss", "amt", "rate", "dur", "ii")
    .invalidKeep <- intersect(tolower(keep), tolower(.invalidKeep))
    if (length(.invalidKeep) > 0) {
      stop("'keep' cannot contain ", paste(.invalidKeep, collapse=", "), "\nconsider using addDosing=TRUE or merging to original dataset", call.=FALSE)
    }
    .invalidKeep <- c ("rxLambda", "rxYj", "rxLow", "rxHi")
    .invalidKeep <- intersect(tolower(keep), tolower(.invalidKeep))
    if (length(.invalidKeep) > 0) {
      stop("'keep' cannot contain ", paste(.invalidKeep, collapse=", "), "\nconsider using returnType=\"data.frame.TBS\"", call.=FALSE)
    }
    checkmate::assertCharacter(drop, any.missing=FALSE, null.ok=TRUE)
    checkmate::assertLogical(warnDrop, len=1, any.missing=FALSE)
    checkmate::assertNumeric(omegaLower, any.missing=FALSE, null.ok=TRUE)
    checkmate::assertNumeric(omegaUpper, any.missing=FALSE, null.ok=TRUE)
    checkmate::assertNumeric(sigmaLower, any.missing=FALSE, null.ok=TRUE)
    checkmate::assertNumeric(sigmaUpper, any.missing=FALSE, null.ok=TRUE)
    checkmate::assertNumeric(thetaLower, any.missing=FALSE, null.ok=TRUE)
    checkmate::assertNumeric(thetaUpper, any.missing=FALSE, null.ok=TRUE)
    checkmate::assertLogical(idFactor, any.missing=FALSE)
    checkmate::assertLogical(warnIdSort, any.missing=FALSE)
    if (is.null(resample)) {
    } else if (checkmate::testLogical(resample)) {
      checkmate::assertLogical(resample, any.missing=FALSE, len=1)
    } else {
      checkmate::assertCharacter(resample, min.len=1, any.missing=FALSE, unique=TRUE)
    }
    checkmate::assertLogical(resampleID, null.ok=FALSE, any.missing=FALSE, len=1)
    checkmate::assertIntegerish(maxwhile, lower=20, len=1)
    checkmate::assertIntegerish(maxExtra, lower=0, len=1)
    if (!is.null(serializeFile)) {
      if (isTRUE(serializeFile)) {
      } else if (checkmate::testCharacter(serializeFile, len = 1, any.missing = FALSE)) {
      } else {
        stop("'serializeFile' must be TRUE or a single file path", call. = FALSE)
      }
    }
    if (!is.null(nLlikAlloc)) {
      checkmate::assertIntegerish(nLlikAlloc, lower=1, len=1, any.missing=FALSE)
    }
    if (checkmate::testLogical(useStdPow)) {
      checkmate::assertLogical(useStdPow, len=1, any.missing=FALSE)
    } else {
      checkmate::assertIntegerish(useStdPow, lower=0, upper=1, len=1, any.missing=FALSE)
    }
    checkmate::assertLogical(addlKeepsCov, any.missing=FALSE, null.ok=FALSE, len=1)
    checkmate::assertLogical(addlDropSs, any.missing=FALSE, null.ok=FALSE, len=1)
    checkmate::assertLogical(ssAtDoseTime, any.missing=FALSE, null.ok=FALSE, len=1)
    checkmate::assertLogical(ss2cancelAllPending, any.missing=FALSE, null.ok=FALSE, len=1)
    checkmate::assertLogical(ssSolved, any.missing=FALSE, null.ok=FALSE, len=1)
    useStdPow <- as.integer(useStdPow)
    maxwhile <- as.integer(maxwhile)
    maxExtra <- as.integer(maxExtra)
    .zeros <- .xtra$.zeros
    if (inherits(.omega, "matrix")) {
      .w <-which(diag(.omega) == 0.0)
      if (length(.w) > 0) {
        # name boundaries if they are not named
        .dimnames <- dimnames(.omega)[[2]]
        if (length(omegaLower) == length(.dimnames) && is.null(names(omegaLower))) {
          names(omegaLower) <- .dimnames
        }
        if (length(omegaUpper) == length(.dimnames) && is.null(names(omegaUpper))) {
          names(omegaUpper) <- .dimnames
        }
        .zeros <- c(.zeros, .dimnames[.w])
        if (length(.w) == length(.dimnames)) {
          .omega <- NULL
        } else {
          .omega <- .omega[-.w, -.w, drop=FALSE]
        }
      }
    }
    if (inherits(.sigma, "matrix")) {
      .w <-which(diag(.sigma) == 0.0)
      if (length(.w) > 0) {
        # name boundaries if they are not named
        .dimnames <- dimnames(.sigma)[[2]]
        if (length(sigmaLower) == length(.dimnames) && is.null(names(sigmaLower))) {
          names(sigmaLower) <- .dimnames
        }
        if (length(sigmaUpper) == length(.dimnames) && is.null(names(sigmaUpper))) {
          names(sigmaUpper) <- .dimnames
        }
        .zeros <- c(.zeros, .dimnames[.w])
        if (length(.w) == length(.dimnames)) {
          .sigma <- NULL
        } else {
          .sigma <- .sigma[-.w, -.w, drop=FALSE]
        }
      }
    }
    .ret <- list(
      scale = scale, #
      method = method, #
      atol = atol, #
      rtol = rtol, #
      maxsteps = maxsteps,#
      hmin = hmin, #
      hmax = hmax, #
      hini = hini, #
      maxordn = maxordn, #
      maxords = maxords, #
      covsInterpolation = covsInterpolation,#
      addCov = addCov,#
      returnType = returnType, #
      sigma = .sigma, #
      sigmaDf = sigmaDf, #
      nCoresRV = nCoresRV, #
      sigmaIsChol = sigmaIsChol, #
      sigmaSeparation = match.arg(sigmaSeparation),
      sigmaXform = .sigmaXform, #
      nDisplayProgress = nDisplayProgress, #
      amountUnits = amountUnits, #
      timeUnits = timeUnits, #
      addDosing = addDosing,#
      stateTrim = stateTrim, #
      updateObject = updateObject, #
      omega = .omega, #
      omegaDf = omegaDf, #
      omegaIsChol = omegaIsChol, #
      omegaSeparation = match.arg(omegaSeparation),
      omegaXform = .omegaXform,
      nSub = nSub, #
      thetaMat = thetaMat, #
      thetaDf = thetaDf, #
      thetaIsChol = thetaIsChol, #
      nStud = nStud,#
      dfSub = dfSub,#
      dfObs = dfObs,#
      seed = seed,#
      nsim = nsim, #
      minSS = minSS, maxSS = maxSS, #
      strictSS = strictSS, #
      infSSstep = as.double(infSSstep), #
      istateReset = istateReset, #
      subsetNonmem = subsetNonmem, #
      hmaxSd = hmaxSd, #
      maxAtolRtolFactor = maxAtolRtolFactor, #
      from = from, #
      to = to, #
      by = by, #
      length.out = length.out, #
      iCov = iCov, #
      keep = keep, #
      keepF = character(0),
      drop = drop, #
      warnDrop = warnDrop, #
      omegaLower = omegaLower, #
      omegaUpper = omegaUpper, #
      sigmaLower = sigmaLower, #
      sigmaUpper = sigmaUpper,#
      thetaLower = thetaLower, #
      thetaUpper = thetaUpper, #
      indLinPhiM = indLinPhiM, #
      indLinPhiTol = indLinPhiTol, #
      indLinMatExpType = .indLinMatExpType, #
      indLinMatExpOrder = indLinMatExpOrder, #
      idFactor = idFactor, #
      mxhnil = mxhnil, #
      hmxi = hmxi, #
      warnIdSort = warnIdSort, #
      ssAtol = ssAtol, #
      ssRtol = ssRtol, #
      safeZero = safeZero,
      sumType = .sum,
      prodType = .prod,
      resample = resample, #
      resampleID = resampleID,#
      maxwhile = maxwhile,
      cores = cores,
      atolSens = atolSens,
      rtolSens = rtolSens,
      ssAtolSens=ssAtolSens,
      ssRtolSens=ssRtolSens,
      simVariability=simVariability,
      nLlikAlloc=nLlikAlloc,
      useStdPow=useStdPow,
      naTimeHandle=naTimeHandle,
      addlKeepsCov=addlKeepsCov,
      addlDropSs=addlDropSs,
      ssAtDoseTime=ssAtDoseTime,
      ss2cancelAllPending=ss2cancelAllPending,
      naInterpolation=naInterpolation,
      keepInterpolation=keepInterpolation,
      safeLog=safeLog,
      safePow=safePow,
      ssSolved=ssSolved,
      linCmtSensType=.linCmtSensType,
      linCmtSensH=linCmtSensH,
      linCmtGillFtol=linCmtGillFtol,
      linCmtGillK=linCmtGillK,
      linCmtGillStep=linCmtGillStep,
      linCmtGillRtol=linCmtGillRtol,
      linCmtShiErr=linCmtShiErr,
      linCmtShiMax=linCmtShiMax,
      linCmtScale=linCmtScale,
      linCmtHcmt = linCmtHcmt,
      linCmtHmeanI=linCmtHmeanI,
      linCmtHmeanO=linCmtHmeanO,
      linCmtSuspect=linCmtSuspect,
      linCmtForwardMax=linCmtForwardMax,
      indOwnAlloc=as.integer(indOwnAlloc),
      maxExtra=maxExtra,
      tolFactor=tolFactor,
      serializeFile=serializeFile,
      dense=dense,
      cvodeLinSolver=cvodeLinSolver,
      stiff2=stiff2,
      autoSwitchMaxStiff=as.integer(autoSwitchMaxStiff),
      autoSwitchMaxNonstiff=as.integer(autoSwitchMaxNonstiff),
      autoSwitchStiffFirst=as.integer(autoSwitchStiffFirst),
      autoSwitchNonstifftol=as.double(autoSwitchNonstifftol),
      autoSwitchStifftol=as.double(autoSwitchStifftol),
      autoSwitchDtfac=as.double(autoSwitchDtfac),
      autoSwitchSwitchMax=as.integer(autoSwitchSwitchMax),
      useLinCmt=isTRUE(useLinCmt),
      .zeros=unique(.zeros)
    )
    class(.ret) <- "rxControl"
    return(.ret)
  }
  UseMethod("rxSolve")
}

#' @rdname rxSolve
#' @export
rxSolve.function <- function(object, params = NULL, events = NULL, inits = NULL, ...,
                             theta = NULL, eta = NULL, envir=parent.frame()) {
  if (.rxIsSerializedSolvePath(params)) {
    .xtra <- list(...)
    .rxAssertSerializedSolveArgs(eventsMissing = missing(events), events = events,
                                 initsMissing = missing(inits), inits = inits,
                                 dots = .xtra,
                                 thetaMissing = missing(theta),
                                 etaMissing = missing(eta),
                                 file = params)
    .object <- rxode2(object) # nolint
    return(rxSolve.default(.object, params = params, envir = envir))
  }
  rxUdfUiReset() # nolint
  .eventsChk <- .rxSolveUiEventData(events)
  .paramsChk <- .rxSolveUiEventData(params)
  if (is.data.frame(.eventsChk)) {
    rxUdfUiData(.eventsChk) # nolint
  } else if (is.data.frame(.paramsChk)) {
    rxUdfUiData(.paramsChk) # nolint
  } else {
    stop("Cannot detect an event data frame to use while re-parsing the model",
         call.=FALSE)
  }
  rxUdfUiEst("rxSolve") # nolint
  on.exit({
    rxUdfUiReset()
  })
  .udfEnvSet(list(envir, parent.frame(1))) # nolint
  ## Cache the rxUi (function → rxUi conversion) to avoid re-parsing on every
  ## call.  Key is digest of the whole function object (body + closure), so
  ## factory-pattern closures with different captured values get separate
  ## entries.  UDF models (uiUseData=TRUE) bypass the cache because their rxUi
  ## depends on data that can change between calls.
  .fkey <- digest::digest(object)
  .object <- .rxFunctionUiCache[[.fkey]]
  if (is.null(.object) || isTRUE(.object$uiUseData)) {
    .object <- rxode2(object) # nolint
    if (!isTRUE(.object$uiUseData)) {
      .rxFunctionUiCache[[.fkey]] <- .object
    }
  }
  do.call("rxSolve", c(list(object=.object, params = params, events = events,
                            inits = inits),
                       list(...),
                       list(theta = theta, eta = eta, envir=envir)))
}

.rxSolveUiEventData <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.rxEt(x)) {
    .preview <- .etPreviewData(.rxEtEnv(x))
    if (is.null(.preview)) {
      return(.rxEtSyncData(x))
    }
    .meta <- attr(.preview, "rxEtPreviewGroups", exact = TRUE)
    if (!is.null(.meta)) {
      .preview$id <- rep.int(
        vapply(.meta, function(.g) as.integer(.g$ids[[1L]]), integer(1)),
        vapply(.meta, function(.g) as.integer(.g$nRow), integer(1))
      )
    }
    .preview
  } else if (rxIs(x, "event.data.frame")) {
    as.data.frame(x)
  } else {
    NULL
  }
}

## Default rxControl object cached once; avoids rebuilding 125-field list each
## call.  Only used by .uiRxControl to probe field names and as a fast-path
## return when no overrides exist.
.rxControlDefault <- NULL
.rxControlNames   <- NULL

.uiRxControl <- function(ui, params = NULL, events = NULL, inits = NULL, ...,
                         theta = NULL, eta = NULL) {
  if (is.null(.rxControlDefault) ||
        !is.null(getOption("rxode2.naTimeHandle", NULL))) {
    .d <- rxControl()
    if (is.null(getOption("rxode2.naTimeHandle", NULL))) {
      assignInMyNamespace(".rxControlDefault", .d)
    }
    if (is.null(.rxControlNames)) {
      assignInMyNamespace(".rxControlNames", names(.d))
    }
  }
  .meta <- try(ui$meta, silent=TRUE)
  if (!is.environment(.meta))  {
    .meta <- new.env(parent=emptyenv())
  }
  .lst <- list(...)
  .nlst <- names(.lst)
  .w <- which(vapply(.rxControlNames, function(x) {
    !(x %in% .nlst) && exists(x, envir=.meta)
  }, logical(1), USE.NAMES=FALSE))
  .extra <- NULL
  if (length(.w) > 0) {
    .v <- .rxControlNames[.w]
    .minfo(paste0("rxControl items read from fun: '",
                  paste(.v, collapse="', '"), "'"))
    .extra <- setNames(lapply(.v, function(x) {
      get(x, envir=.meta)
    }), .v)
  }
  ## Fast path: no per-model overrides and no caller-supplied options — return
  ## the cached default control directly without a second rxSolve(NULL) round.
  if (!is.null(.rxControlDefault) &&
        is.null(.extra) && length(.lst) == 0 &&
        is.null(theta) && is.null(eta)) {
    return(.rxControlDefault)
  }
  do.call(rxSolve, c(list(NULL, params = NULL, events = NULL, inits = NULL),
                     .lst, .extra,
                     list(theta=theta, eta=eta)))
}

.rxIsSerializedSolvePath <- function(params) {
  is.character(params) && length(params) == 1L &&
    file.exists(params) && .rxIsSerializeFile(params)
}

.rxSerializedSolveArgNames <- function(eventsMissing = TRUE,
                                       events = NULL,
                                       initsMissing = TRUE,
                                       inits = NULL,
                                       dots = list(),
                                       allowedDots = character(0),
                                       thetaMissing = TRUE,
                                       etaMissing = TRUE,
                                       indOwnAllocMissing = TRUE,
                                       extras = NULL) {
  .bad <- character(0)
  if (!eventsMissing && !is.null(events)) {
    .bad <- c(.bad, "events")
  }
  if (!initsMissing && !is.null(inits)) {
    .bad <- c(.bad, "inits")
  }
  if (!thetaMissing) {
    .bad <- c(.bad, "theta")
  }
  if (!etaMissing) {
    .bad <- c(.bad, "eta")
  }
  if (!indOwnAllocMissing) {
    .bad <- c(.bad, "indOwnAlloc")
  }
  if (length(dots) > 0) {
    .dotNames <- names(dots)
    if (is.null(.dotNames)) {
      .dotNames <- rep.int("", length(dots))
    }
    .dotNames[.dotNames == ""] <- "<unnamed>"
    if (length(allowedDots) > 0L) {
      .dotNames <- .dotNames[!(.dotNames %in% allowedDots)]
    }
    .bad <- c(.bad, .dotNames)
  }
  if (!is.null(extras) && length(extras) > 0) {
    .bad <- c(.bad, extras)
  }
  unique(.bad)
}

.rxAssertSerializedSolveArgs <- function(..., file) {
  .bad <- .rxSerializedSolveArgNames(...)
  if (length(.bad) > 0) {
    stop(sprintf(
      "Serialized solve '%s' only accepts the model and serialization file; disallowed inputs: %s",
      file, paste(sprintf("'%s'", .bad), collapse = ", ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}

.rxAssertSerializeFileWritable <- function(file) {
  if (file.exists(file)) {
    stop(sprintf(
      "Serialization file '%s' already exists; either delete the serialization file or solve without the file specified",
      file
    ), call. = FALSE)
  }
  invisible(TRUE)
}

.rxSerializedSolvePipeArgs <- function() {
  .bad <- character(0)
  .pipeChecks <- c("ThetaMat", "Omega", "Sigma", "DfObs", "DfSub", "NSub", "NStud")
  for (.nm in .pipeChecks) {
    .fn <- get(paste0(".pipe", .nm), envir = asNamespace("rxode2"))
    if (!is.null(.fn(NA))) {
      .bad <- c(.bad, substring(.nm, 1, 1) |> tolower() |> paste0(substring(.nm, 2)))
    }
  }
  .bad
}

.rxSolveFromUi <- function(object, params = NULL, events = NULL, inits = NULL, ...,
                           theta = NULL, eta = NULL) {
  .rxControl <- .uiRxControl(object, params = params, events = events, inits = inits, ...,
                             theta = theta, eta=eta)
  if (rxIs(params, "rx.event")) {
    if (!is.null(events)) {
      .tmp <- events
      events <- params
      params <- .tmp
    } else {
      events <- params
      params <- NULL
    }
  }
  if (is.null(params)) {
    params <- object$theta
  } else if (inherits(params, "data.frame")) {
    .theta <- object$theta
    params <- as.data.frame(params)
    for (.t in names(.theta)) {
      if (!any(names(params) == .t)) {
        params[[.t]] <- .theta[.t]
      }
    }
  } else if (inherits(params, "numeric")) {
    .theta <- object$theta
    .n <- names(.theta)
    .theta <- .theta[!(.n %in% names(params))]
    params <- c(params, .theta)
  }

  if (is.null(.rxControl$thetaLower)) {
    .rxControl$thetaLower <- object$thetaLower
  }
  if (is.null(.rxControl$thetaUpper)) {
    .rxControl$thetaUpper <- object$thetaUpper
  }
  if (is.null(.rxControl$omega)) {
    .omega <- object$omega
    .rxControl$omega <- .omega
  } else if (is.logical(.rxControl$omega)) {
    if (is.na(.rxControl$omega)) {
      .omega <- object$omega
      params <- c(params, setNames(rep(0, dim(.omega)[1]), dimnames(.omega)[[2]]))
      .rxControl$omega <- NULL
    }
  }
  if (inherits(.rxControl$omega, "matrix")) {
    .omega <- .rxControl$omega
    .v <- vapply(dimnames(.omega)[[1]],
                 function(v) {
                   !(v %in% names(params))
                 }, logical(1), USE.NAMES = FALSE)
    if (length(.v) == 1L) {
      if (!.v) .rxControl$omega <- NULL
    } else {
      .omega <- .omega[.v, .v]
      if (all(dim(.omega) == c(0L, 0L))) {
        .rxControl$omega <- NULL
      } else {
        .rxControl$omega <- .omega
      }
    }

  }
  if (inherits(.rxControl$omega, "matrix") &&
        all(dim(.rxControl$omega) == c(0,0))) {
    .rxControl$omega <- NULL
  }
  if (is.null(.rxControl$sigma)) {
    .rxControl$sigma <- object$simulationSigma
  } else if (length(.rxControl$sigma) == 0) {
    .rxControl$sigma <- object$simulationSigma
  } else if (is.logical(.rxControl$sigma)) {
    if (is.na(.rxControl$sigma)) {
      .sigma <- object$simulationSigma
      params <- c(params, setNames(rep(0, dim(.sigma)[1]), dimnames(.sigma)[[2]]))
      .rxControl$sigma <- NULL
    }
  }
  if (inherits(.rxControl$sigma, "matrix")) {
    .sigma <- .rxControl$sigma
    .v <- vapply(dimnames(.sigma)[[1]],
                 function(v) {
                   !(v %in% names(params))
                 }, logical(1), USE.NAMES = FALSE)
    if (length(.v) == 1L) {
      if (!.v) .rxControl$sigma <- NULL
    } else {
      .sigma <- .sigma[.v, .v, drop = FALSE]
      if (all(dim(.sigma) == c(0L, 0L))) {
        .rxControl$sigma <- NULL
      } else {
        .rxControl$sigma <- .sigma
      }
    }

  }
  if (inherits(.rxControl$sigma, "matrix") &&
        all(dim(.rxControl$sigma) == c(0,0))) {
    .rxControl$sigma <- NULL
  }
  if (inherits(object, "rxode2tos")) {
    .rx <- object
  } else {
    .rx <- object$simulationModel
  }
  list(list(object=.rx, params = params, events = events, inits = inits),
                       .rxControl,
                       list(theta = theta, eta = eta))
}

#' @rdname rxSolve
#' @export
rxSolve.rxUi <- function(object, params = NULL, events = NULL, inits = NULL, ...,
                         useLinCmt = TRUE,
                         theta = NULL, eta = NULL, envir=parent.frame()) {
  if (.rxIsSerializedSolvePath(params)) {
    .xtra <- list(...)
    .rxAssertSerializedSolveArgs(eventsMissing = missing(events), events = events,
                                 initsMissing = missing(inits), inits = inits,
                                 dots = .xtra,
                                 thetaMissing = missing(theta),
                                 etaMissing = missing(eta),
                                 file = params)
    if (inherits(object, "rxUi")) {
      object <- rxUiDecompress(object)
    }
    return(rxSolve.default(object$simulationModel, params = params, envir = envir))
  }
  rxUdfUiReset()
  if (isTRUE(object$uiUseData)) {
    # this needs to be re-parsed
    .eventsChk2 <- .rxSolveUiEventData(events)
    .paramsChk2 <- .rxSolveUiEventData(params)
    if (is.data.frame(.eventsChk2)) {
      rxUdfUiData(.eventsChk2)
      rxUdfUiMv(rxModelVars(object))
    } else if (is.data.frame(.paramsChk2)) {
      rxUdfUiData(.paramsChk2)
      rxUdfUiMv(rxModelVars(object))
    } else {
      stop("Cannot detect an event data frame to use while re-parsing the model",
           call.=FALSE)
    }
    rxUdfUiEst("rxSolve")
    on.exit({
      rxUdfUiReset()
    })
    # Now re-parse
    object <- as.function(object)
    object <- suppressMessages(rxode2(object))
  }
  .udfEnvSet(list(object$meta, envir, parent.frame(1)))
  if (inherits(object, "rxUi")) {
    object <- rxUiDecompress(object)
  }
  if (isTRUE(useLinCmt)) {
    .linInfo <- .odeToLinDetect(object) # nolint
    if (!is.null(.linInfo)) {
      .cacheKey <- .odeToLinCacheKey(object) # nolint
      if (exists(.cacheKey, envir = .odeToLinCache, inherits = FALSE)) { # nolint
        object <- .odeToLinCache[[.cacheKey]] # nolint
      } else {
        .linExpr   <- .odeToLinBuildExpr(object$lstExpr, .linInfo) # nolint
        # fall back to the original ODE model
        .converted <- tryCatch(
          rxUiDecompress(suppressMessages(.rebuildRxUiFromExpr(object, .linExpr))), # nolint
          error = function(e) NULL)
        if (is.null(.converted)) {
          .converted <- object
        }
        assign(.cacheKey, .converted, envir = .odeToLinCache) # nolint
        object <- .converted
      }
    }
  }
  .lst <- .rxSolveFromUi(object, params = params, events = events, inits = inits, ..., useLinCmt = useLinCmt, theta = theta, eta = eta)
  .lst <- do.call("c", .lst)
  .pred <- FALSE
  .mv <- rxModelVars(object)
  .hasIpred <- FALSE
  if (any(.mv$lhs == "ipredSim")) {
    .hasIpred <- TRUE
  }
  .hasSim <- FALSE
  if (any(.mv$lhs == "sim")) {
    .hasSim <- TRUE
  }
  if (is.null(.lst$omega) && is.null(.lst$sigma)) {
    .pred <- TRUE
    if (!.hasIpred && any(rxModelVars(.lst[[1]])$lhs == "ipredSim")) {
      .lst$drop <- c(.lst$drop, "ipredSim")
    }
  }
  .ret <- do.call("rxSolve.default", .lst)
  if (.pred) {
    .e <- attr(class(.ret), ".rxode2.env")
    .w <- which(names(.ret) == "sim")
    if (!.hasSim && length(.w) == 1L) {
      names(.ret)[.w] <- "pred"
      # don't break rxSolve, though maybe it should...
      if (is.environment(.e)) {
        .n2 <- .e$.check.names
        .n2[.w] <- "pred"
        .e$.check.names <- .n2
      }
    }
  }
  .ret
}
#' @rdname rxSolve
#' @export
rxSolve.rxode2tos <- rxSolve.rxUi


#nlmixr2.nlmixr2FitData <- nlmixr2.nlmixr2FitCore
#' @rdname rxSolve
#' @export
rxSolve.nlmixr2FitData <- function(object, params = NULL, events = NULL, inits = NULL, ...,
                                   theta = NULL, eta = NULL, envir=parent.frame()) {
  if (.rxIsSerializedSolvePath(params)) {
    .xtra <- list(...)
    .rxAssertSerializedSolveArgs(eventsMissing = missing(events), events = events,
                                 initsMissing = missing(inits), inits = inits,
                                 dots = .xtra,
                                 thetaMissing = missing(theta),
                                 etaMissing = missing(eta),
                                 file = params)
    return(rxSolve.default(object$simulationModel, params = params, envir = envir))
  }
  rxUdfUiReset()
  .udfEnvSet(list(envir, parent.frame(1)))
  .lst <- .rxSolveFromUi(object, params = params, events = events, inits = inits, ..., theta = theta, eta = eta)
  .rxControl <- .lst[[2]]
  .env <- object$env
  # assign current control to object for expanded thetaMat
  if (exists("control", envir=.env)) {
    .oldControl <- get("control", envir=.env)
    assign("control", .rxControl, envir=.env)
    on.exit({
      rxUdfUiReset()
      assign("control", .oldControl, envir=.env)
    })
  } else {
    assign("control", .rxControl, envir=.env)
    on.exit({
      rxUdfUiReset()
      rm(list="control", envir=.env)
    })
  }
  .rxControl <- object$rxControlWithVar
  .lst[[2]] <- .rxControl
  .lst <- do.call("c", .lst)
  if (is.null(.lst$events)) {
    .lst$events <- object$origData
    .minfo("using original fit data for simulation")
  }
  do.call("rxSolve", .lst)
}

#' @rdname rxSolve
#' @export
rxSolve.nlmixr2FitCore <- rxSolve.nlmixr2FitData

rxSolveCacheEnv <- new.env(parent=emptyenv())
rxSolveCacheLimit <- 64L
rxSolveCacheEnv$.order <- character()

.rxSolveCacheTouch <- function(key) {
  .order <- rxSolveCacheEnv$.order
  rxSolveCacheEnv$.order <- c(key, .order[.order != key])
}

.rxSolveCacheGet <- function(key) {
  if (!exists(key, envir = rxSolveCacheEnv, inherits = FALSE)) return(NULL)
  .rxSolveCacheTouch(key)
  get(key, envir = rxSolveCacheEnv, inherits = FALSE)
}

.rxSolveCacheSet <- function(key, value) {
  assign(key, value, envir = rxSolveCacheEnv)
  .rxSolveCacheTouch(key)
  .order <- rxSolveCacheEnv$.order
  if (length(.order) > rxSolveCacheLimit) {
    .drop <- .order[-seq_len(rxSolveCacheLimit)]
    rm(list = .drop, envir = rxSolveCacheEnv)
    rxSolveCacheEnv$.order <- .order[seq_len(rxSolveCacheLimit)]
  }
  invisible(value)
}

#' @rdname rxSolve
#' @export
rxSolve.default <- function(object, params = NULL, events = NULL, inits = NULL, ...,
                            indOwnAlloc = TRUE,
                            theta = NULL, eta = NULL, envir=parent.frame()) {
  rxUdfUiReset()
  .udfEnvSet(list(envir, parent.frame(1)))
  on.exit({
    rxUdfUiReset()
    .clearPipe()
    .asFunctionEnv$rx <- NULL
  })
  .applyParams <- FALSE
  .rxParams <- NULL
  if (is.rxEt(object)) {
    if (!is.null(events)) {
      stop("events can be pipeline or solving arguments not both",
        call. = FALSE
      )
    }
    if (is.null(rxode2::.pipeRx(NA))) {
      stop("need an rxode2 compiled model as the start of the pipeline",
        call. = FALSE
      )
    } else {
      events <- object
      object <- rxode2::.pipeRx(NA)
    }
  } else if (inherits(object, "rxParams")) {
    .applyParams <- TRUE
    if (is.null(params) && !is.null(object$params)) {
      params <- object$params
    }
    if (is.null(rxode2::.pipeRx(NA))) {
      stop("need an rxode2 compiled model as the start of the pipeline",
        call. = FALSE
      )
    } else {
      .rxParams <- object
      object <- rxode2::.pipeRx(NA)
    }
    if (is.null(rxode2::.pipeEvents(NA))) {
      stop("need an rxode2 events as a part of the pipeline",
        call. = FALSE
      )
    } else {
      events <- rxode2::.pipeEvents(NA)
      rxode2::.pipeEvents(NULL)
    }
  }
  if (!is.null(rxode2::.pipeEvents(NA)) && is.null(events) && is.null(params)) {
    events <- rxode2::.pipeEvents(NA)
  } else if (!is.null(rxode2::.pipeEvents(NA)) && !is.null(events)) {
    stop("'events' in pipeline AND in solving arguments, please provide just one",
      call. = FALSE
    )
  } else if (!is.null(rxode2::.pipeEvents(NA)) && !is.null(params) &&
    rxIs(params, "event.data.frame")) {
    stop("'events' in pipeline AND in solving arguments, please provide just one",
      call. = FALSE
    )
  }

  if (!is.null(rxode2::.pipeParams(NA)) && is.null(params)) {
    params <- rxode2::.pipeParams(NA)
  } else if (!is.null(rxode2::.pipeParams(NA)) && !is.null(params)) {
    stop("'params' in pipeline AND in solving arguments, please provide just one",
      call. = FALSE
    )
  }

  if (!is.null(rxode2::.pipeInits(NA)) && is.null(inits)) {
    inits <- rxode2::.pipeInits(NA)
  } else if (!is.null(rxode2::.pipeInits(NA)) && !is.null(inits)) {
    stop("'inits' in pipeline AND in solving arguments, please provide just one",
      call. = FALSE
    )
  }

  if (.applyParams) {
    if (!is.null(.rxParams$inits)) {
      inits <- .rxParams$inits
    }
  }
  .xtra <- list(...)
  .serializeInput <- .rxIsSerializedSolvePath(params)
  .preloadedSerializedBundle <- NULL
  if (.serializeInput) {
    .rxAssertSerializedSolveArgs(eventsMissing = missing(events), events = events,
                                 initsMissing = missing(inits), inits = inits,
                                 dots = .xtra,
                                 allowedDots = c("iCov", "keep"),
                                 thetaMissing = missing(theta),
                                 etaMissing = missing(eta),
                                 indOwnAllocMissing = missing(indOwnAlloc),
                                 file = params)
  }
  if (any(duplicated(names(.xtra)))) {
    stop("duplicate arguments do not make sense",
      call. = FALSE
    )
  }
  if (any(names(.xtra) == "covs")) {
    stop("covariates can no longer be specified by 'covs'\n  include them in the event dataset\n\nindividual covariates: Can be specified by a 'iCov' dataset\n each each individual covariate has a value\n\ntime varying covariates: modify input event data-frame or\n  'eventTable' to include covariates(https://tinyurl.com/y52wfc2y)\n\nEach approach needs the covariates named to match the variable in the model",
      call. = FALSE
    )
  }
  .nms <- names(as.list(match.call())[-1])
  .lst <- list(...)
  .setupOnly <- 0L
  if (any(names(.lst) == ".setupOnly")) {
    .setupOnly <- .lst$.setupOnly
  }
  if (is.rxEt(params) && !is.rxEt(events)) {
    .tmp <- events
    events <- params
    params <- .tmp
  }
  if (inherits(inits, "rxControl")) {
    stop("'rxControl()' cannot be passed as 'inits'; pass control options as named arguments instead, e.g. rxSolve(object, params, events, method='dop853+ros4')",
         call. = FALSE)
  }
  .ctl <- rxControl(..., indOwnAlloc = indOwnAlloc, events = events, params = params)
  if (length(rxModelVars(object)$indLin) > 0L) {
    if (.ctl$method != 3L) {
      .ctl$method <- 3L
      .ctl <- do.call(rxControl, c(.ctl, list(events = events, params = params)))
    }
  } else if (.ctl$method == 3L) {
    if (length(rxModelVars(object)$state) > 0L) {
      .calcSens <- NULL
      if (rxIs(object, "rxode2")) {
        .e <- attr(class(object), ".rxode2.env")
        if (is.environment(.e)) {
          .calcSens <- .e$calcSens
        }
      }
      .mexpCode <- rxToIndLin(object, calcSens = .calcSens)
      object <- rxode2(.mexpCode)
    }
  }
  # Delay differential equations (models using delay()) require a dense ODE
  # solver so delay() can interpolate past states.  When delays are present,
  # default to the dense AutoSwitch composite "dop853+ros4" and turn on dense
  # output; a non-dense method requested explicitly is an error.
  .hasDelay <- isTRUE(rxModelVars(object)$flags[["hasDelay"]] == 1L)
  if (.hasDelay) {
    # Forward-sensitivity param-dependent delay: reproduce the dose-induced
    # breaking-point jump by mirroring each delayed-state dose onto its
    # sensitivity compartment (the alag()/f() model lines are already emitted by
    # .rxDelaySensAugment; here we add the doses that activate them).  Gated on the
    # model actually carrying rx__sens_* compartments, and a no-op unless a delay
    # duration depends on a sensitivity parameter (.rxDelaySensJump returns NULL).
    if (!is.null(events)) {
      # cheap gate: the jump alag() lines exist only for a param-dependent delay
      # forward-sens model, so skip the (symengine) rebuild for every other solve.
      .sensCmts <- grep("^rx__sens_", rxModelVars(object)$state, value = TRUE)
      if (length(.sensCmts) > 0L &&
            any(grepl("alag(rx__sens_", rxNorm(object), fixed = TRUE))) {
        .cs <- unique(sub("^rx__sens_.+?_BY_(.+)__$", "\\1", .sensCmts))
        .jm <- tryCatch(.rxDelaySensJump(object, .cs, events), error = function(e) NULL)
        if (!is.null(.jm) && !is.null(.jm$events)) events <- .jm$events
      }
    }
    # delay() history is recorded on the dense dop853 path (default, and the
    # dop853 leg of "dop853+ros4") and on the dense ros4 path (for stiff delay
    # models).  Other solvers cannot record dense history and are rejected.
    .stiff2 <- if (is.null(.ctl$stiff2)) 0L else as.integer(.ctl$stiff2)
    if (.ctl$method >= 200L) {
      # discrete-adjoint rk4s methods record their own cubic-Hermite dense
      # history per accepted step and carry the backward anticipating term
      # (delayed Jacobian) in every backward fill -- explicit, stiff (ros4s,
      # radauiia5s) and the dop853s+ros4s composite -- so all support delay().
    } else if (.ctl$method == 2L) {
      # liblsoda is the overall default; switch DDE models to dop853 + ros4
      .ctl$method <- 0L
      .ctl$stiff2 <- 13L
      .ctl$dense <- TRUE
      .ctl <- do.call(rxControl, c(.ctl, list(events = events, params = params)))
    } else if ((.ctl$method == 0L && (.stiff2 == 0L || .stiff2 == 13L)) ||
                 (.ctl$method == 13L && .stiff2 == 0L)) {
      # dop853 (optionally + ros4 secondary), or pure ros4 for stiff delays
      if (!isTRUE(.ctl$dense)) {
        .ctl$dense <- TRUE
        .ctl <- do.call(rxControl, c(.ctl, list(events = events, params = params)))
      }
    } else {
      stop("delay differential equations require a dense solver; use method='dop853+ros4' (the default for delay models), 'dop853', or 'ros4' (stiff)",
           call. = FALSE)
    }
  }
  # Generate the analytical Jacobian whenever the solve uses an implicit method
  # that needs one -- either as the primary method or as the stiff secondary of
  # an AutoSwitch composite ("primary+stiff").  rxIsImplicit() flags exactly the
  # methods the C solver consumes a Jacobian for (its `_jacAvailable` check):
  # ros4(13), iem(14), and ros43/ros6/backwardEuler/gauss6/iiic6/radauiia5/
  # geng5/sdirk43 (31-38).  Solvers that build their own Jacobian internally
  # (lsoda, liblsoda, cvode, bdf) are not flagged and need no generation here.
  .ddeNoJac <- .hasDelay && .ctl$method == 0L &&
    (is.null(.ctl$stiff2) || isTRUE(.ctl$stiff2 == 0L))
  if (!.ddeNoJac &&
      (rxIsImplicit(.ctl$method) ||
       (!is.null(.ctl$stiff2) && isTRUE(.ctl$stiff2 > 0L) && rxIsImplicit(.ctl$stiff2)))) {
    # A pure dop853 delay model (method 0, no stiff secondary) does not use a
    # Jacobian, so generation is skipped there.  The ros4 stiff path and the
    # dop853+ros4 dense composite (which switches to ros4 mid-solve) both need
    # the Jacobian; it is generated normally (delay() differentiates to zero).
    .mvCur <- rxModelVars(object)
    .jacType <- .mvCur$trans["jac"]
    if (.jacType != "fulluser") {
      .key <- paste0(.mvCur$md5["parsed_md5"], "_jac")
      .jacEnv <- new.env(parent = emptyenv())
      .jacEnv$errMsg <- NULL
      .filteredCode <- tryCatch({
        .cached <- .rxSolveCacheGet(.key)
        if (!is.null(.cached)) {
          .cached
        } else {
          .mv <- suppressMessages({
            rxModelVars(rxode2::rxode2(object, calcJac=TRUE))
          })
          .states <- .mvCur$state
          .normCode <- strsplit(rxNorm(.mv), "\n")[[1]]
          .origCode <- strsplit(rxNorm(.mvCur), "\n")[[1]]
          .fc <- .origCode

          for (.line in .normCode) {
            if (grepl("^df\\(", .line)) {
              .parts <- regmatches(.line, regexec("^df\\(([^)]+)\\)/dy\\(([^)]+)\\)", .line))[[1]]
              if (length(.parts) == 3) {
                if (.parts[2] %in% .states && .parts[3] %in% .states) {
                  if (!(.line %in% .origCode) && !(.line %in% .fc)) {
                    .fc <- c(.fc, .line)
                  }
                }
              }
            }
          }
          .fc <- paste(.fc, collapse="\n")
          .rxSolveCacheSet(.key, .fc)
          .fc
        }
      }, error = function(e) {
        assign("errMsg", conditionMessage(e), envir = .jacEnv)
        .rxSolveCacheSet(.key, NA_character_)
        NA_character_
      })
      if (!is.na(.filteredCode)) {
        .jacObject <- rxode2(.filteredCode)
        .jacMd5 <- rxModelVars(.jacObject)$md5["parsed_md5"]
        if (.jacMd5 != .mvCur$md5["parsed_md5"]) {
          # Model changed (Jacobian equations were added); recurse with new model.
          object <- .jacObject
          force(params)
          force(events)
          force(inits)
          force(indOwnAlloc)
          force(theta)
          force(eta)
          force(envir)
          return(rxSolve.default(object, params = params, events = events, inits = inits, ..., indOwnAlloc = indOwnAlloc, theta = theta, eta = eta, envir = envir))
        }
        # Model md5 unchanged: Jacobian equations were already present; calc_jac is now
        # loaded with the real Jacobian function. Fall through to solve directly.
      } else {
        .jacDetail <- if (!is.null(.jacEnv$errMsg)) .jacEnv$errMsg else
          "model previously failed Jacobian generation (cached)"
        warning("method requires an analytical Jacobian, but automatic ",
                "Jacobian generation failed for this model:\n  ", .jacDetail,
                "\n  Falling back to liblsoda.",
                call. = FALSE)
        .ctl$method <- 2L
        .ctl$stiff2 <- 0L
      }
    }
  }
  if (.ctl$addCov && length(.ctl$keep) > 0) {
    .mv <- rxModelVars(object)
    .both <- intersect(.mv$params, .ctl$keep)
    if (length(.both) > 0) {
      .keep <- .ctl$keep[!(.ctl$keep %in% .both)]
       if (length(.keep) == 0L) {
          .keep <- NULL
       }
      .w <- which(names(.ctl) == "keep")
      .ctl[[.w]] <- .keep
      .ctl <- do.call(rxControl,
                      c(.ctl, list(events = events, params = params)))

    }
  }
  .n1 <- setdiff(intersect(tolower(names(params)), tolower(names(.ctl$iCov))), "id")
  .n2 <- c(.n1, setdiff(intersect(tolower(names(events)), tolower(names(.ctl$iCov))), "id"))
  .n1 <- unique(c(.n1, .n2))
  if (length(.n1) > 0) {
    stop(sprintf(
      gettext("'iCov' has information contained in parameters/event data\nduplicate columns: '%s'"),
      paste(.n1, collapse = "', '")
    ), call = FALSE)
  }
  if (!is.null(rxode2::.pipeThetaMat(NA)) && is.null(.ctl$thetaMat)) {
    if (.serializeInput) {
      .rxAssertSerializedSolveArgs(extras = "thetaMat", file = params)
    }
    .ctl$thetaMat <- rxode2::.pipeThetaMat(NA)
  }
  if (!is.null(rxode2::.pipeOmega(NA)) && is.null(.ctl$omega)) {
    if (.serializeInput) {
      .rxAssertSerializedSolveArgs(extras = "omega", file = params)
    }
    .ctl$omega <- rxode2::.pipeOmega(NA)
  }
  if (!is.null(rxode2::.pipeSigma(NA)) && is.null(.ctl$sigma)) {
    if (.serializeInput) {
      .rxAssertSerializedSolveArgs(extras = "sigma", file = params)
    }
    .ctl$sigma <- rxode2::.pipeSigma(NA)
  }
  if (!is.null(rxode2::.pipeSigma(NA)) && is.null(.ctl$sigma)) {
    if (.serializeInput) {
      .rxAssertSerializedSolveArgs(extras = "sigma", file = params)
    }
    .ctl$sigma <- rxode2::.pipeSigma(NA)
  }
  if (!is.null(rxode2::.pipeDfObs(NA)) && .ctl$dfObs == 0) {
    if (.serializeInput) {
      .rxAssertSerializedSolveArgs(extras = "dfObs", file = params)
    }
    .ctl$dfObs <- rxode2::.pipeDfObs(NA)
  }
  if (!is.null(rxode2::.pipeDfSub(NA)) && .ctl$dfSub == 0) {
    if (.serializeInput) {
      .rxAssertSerializedSolveArgs(extras = "dfSub", file = params)
    }
    .ctl$dfSub <- rxode2::.pipeDfSub(NA)
  }
  if (!is.null(rxode2::.pipeNSub(NA)) && .ctl$nSub == 1) {
    if (.serializeInput) {
      .rxAssertSerializedSolveArgs(extras = "nSub", file = params)
    }
    .ctl$nSub <- rxode2::.pipeNSub(NA)
  }
  if (!is.null(rxode2::.pipeNStud(NA)) && .ctl$nStud == 1) {
    if (.serializeInput) {
      .rxAssertSerializedSolveArgs(extras = "nStud", file = params)
    }
    .ctl$nStud <- rxode2::.pipeNStud(NA)
  }
  if (!is.null(rxode2::.pipeKeep(NA)) && is.null(.ctl$keep)) {
    if (.serializeInput) {
      .rxAssertSerializedSolveArgs(extras = "keep", file = params)
    }
    .ctl$keep <- rxode2::.pipeKeep(NA)
  }
  if (.applyParams) {
    if (.serializeInput) {
      .rxAssertSerializedSolveArgs(extras = "rxParams", file = params)
    }
    if (!is.null(.rxParams$thetaMat) && is.null(.ctl$thetaMat)) {
      .ctl$thetaMat <- .rxParams$thetaMat
    }
    if (!is.null(.rxParams$omega) && is.null(.ctl$omega)) {
      .ctl$omega <- .rxParams$omega
    }
    if (!is.null(.rxParams$sigma) && is.null(.ctl$sigma)) {
      .ctl$sigma <- .rxParams$sigma
    }
    if (!is.null(.rxParams$dfSub)) {
      if (.ctl$dfSub == 0) {
        .ctl$dfSub <- .rxParams$dfSub
      }
    }
    if (!is.null(.rxParams$nSub)) {
      if (.ctl$nSub == 1) {
        .ctl$nSub <- .rxParams$nSub
      }
    }
    if (!is.null(.rxParams$nStud)) {
      if (.ctl$nStud == 1) {
        .ctl$nStud <- .rxParams$nStud
      }
    }
    if (!is.null(.rxParams$dfObs)) {
      if (.ctl$dfObs == 0) {
        .ctl$dfObs <- .rxParams$dfObs
      }
    }
    if (!is.null(.rxParams$keep)) {
      if (is.null(.ctl$keep)) {
        .ctl$keep <- .rxParams$keep
      }
    }
  }

  ## Prefers individual keep over keeping from the input data
  .keepF <- character(0)
  if (!is.null(.ctl$keep)) {
    .mv <- rxModelVars(object)
    .vars <- c(.mv$lhs, .mv$state)
    .keepF <- setdiff(.ctl$keep, .vars)
  }
  .ctl$keepF <- .keepF
  rxSolveFree()

  if (!is.null(theta) || !is.null(eta)) {
    .theta <- theta
    .eta <- eta
    if (!is.null(.theta)) {
      if (inherits(.theta, "data.frame")) {
        stop("'theta' cannot be a data.frame", call. = FALSE)
      } else if (inherits(.theta, "matrix")) {
        .dn <- dim(.theta)
        if (.dn[1] != 1) {
          stop("'theta' can only have 1 row", call. = FALSE)
        }
        .theta <- as.vector(theta)
      }
      if (is.null(attr(theta, "names"))) {
        .theta <- setNames(theta, paste0("THETA[", seq_along(theta), "]"))
      } else {
        warning("name specification for 'theta' is ignored", call. = FALSE)
        .theta <- setNames(theta, paste0("THETA[", seq_along(theta), "]"))
      }
    }
    if (!is.null(.eta)) {
      if (inherits(.eta, "data.frame")) {
        stop("'eta' cannot be a data.frame", call. = FALSE)
      } else if (inherits(.eta, "matrix")) {
        .dn <- dim(.eta)
        if (.dn[1] != 1) {
          stop("'eta' can only have 1 row", call. = FALSE)
        }
        .eta <- as.vector(eta)
      }
      if (is.null(attr(eta, "names"))) {
        .eta <- setNames(eta, paste0("ETA[", seq_along(eta), "]"))
      } else {
        warning("name specification for 'eta' is ignored", call. = FALSE)
        .eta <- setNames(eta, paste0("ETA[", seq_along(eta), "]"))
      }
    }
    if (is.null(params)) {
      params <- c(.theta, .eta)
    } else if (is.null(events)) {
      events <- c(.theta, .eta)
    } else {
      stop("cannot specify 'params' and 'theta'/'eta' at the same time",
        call. = FALSE
      )
    }
  }
  if (.serializeInput) {
    .rxAssertSerializedSolveArgs(eventsMissing = is.null(events), events = events,
                                 initsMissing = is.null(inits), inits = inits,
                                 extras = c(.rxSerializedSolvePipeArgs(),
                                            if (.applyParams) "rxParams"),
                                 file = params)
  }
  .origEvents <- events
  if (!is.null(.ctl$iCov)) {
    if (inherits(.ctl$iCov, "data.frame")) {
      .icovId <- which(tolower(names(.ctl$iCov)) == "id")
      .useEvents <- FALSE
      .replaceSolveInput <- TRUE
      .eventsChk3 <- events
      .paramsChk3 <- params
      if (rxIs(.eventsChk3, "event.data.frame")) {
        .events <- .eventsChk3
        .useEvents <- TRUE
      } else if (rxIs(.paramsChk3, "event.data.frame")) {
        .events <- .paramsChk3
      } else if (is.rxEt(.eventsChk3)) {
        .events <- .eventsChk3
        .useEvents <- TRUE
      } else if (is.rxEt(.paramsChk3)) {
        .events <- .paramsChk3
      } else if (.serializeInput) {
        if (is.null(.preloadedSerializedBundle)) {
          .preloadedSerializedBundle <- .rxReadStateBundle(params)
        }
        .events <- .preloadedSerializedBundle$events
        .replaceSolveInput <- FALSE
        if (is.null(.events) || !(is.data.frame(.events) || is.rxEt(.events))) {
          stop("Cannot detect an event data frame to merge 'iCov'")
        }
      } else {
        stop("Cannot detect an event data frame to merge 'iCov'")
      }
      if (is.rxEt(.events)) {
        .by <- "id"
        .id <- .etPresentIds(.rxEtEnv(.events))
      } else {
        if (!inherits(.events, "data.frame")) {
          .events <- as.data.frame(.events)
        }
        .eventId <- which(tolower(names(.events)) == "id")
        if (length(.eventId) != 1) {
          stop("to use 'iCov' you must have an id in your event table")
        }
        .by <- names(.events)[.eventId]
        .groups <- attr(.events, "rxHomGroups", exact = TRUE)
        .idLevels <- attr(.events, "rxHomIdLevels", exact = TRUE)
        if (!is.null(.idLevels) && length(.idLevels) > 0L) {
          .id <- .idLevels
        } else if (!is.null(.groups) && length(.groups) > 0L) {
          .id <- unlist(.groups, use.names = FALSE)
        } else {
          .id <- unique(.events[[.by]])
        }
      }
      if (length(.id) == 0L) {
        stop("to use 'iCov' you must have an id in your event table")
      }
      if (length(.icovId) == 0) {
        if (length(.ctl$iCov[, 1]) != length(.id)) {
          stop("'iCov' and 'id' mismatch")
        }
        .ctl$iCov$id <- .id
      } else if (length(.icovId) > 1) {
        stop("iCov has duplicate ids, cannot continue")
      }
      names(.ctl$iCov)[.icovId] <- .by
      if (.replaceSolveInput) {
        if (.useEvents) {
          events <- .events
        } else {
          params <- .events
        }
      }
    } else {
      stop("'iCov' must be an input dataset")
    }
  }
  if (is.rxEt(.origEvents) && !is.null(.ctl$iCov) && length(.etGroups(.rxEtEnv(.origEvents))) > 0L &&
      .ctl$nSub == 1L && .ctl$nStud == 1L) {
    .mv <- rxModelVars(object)
    .groupedSolve <- .etGroupedSolveDataICov(.origEvents, .ctl$iCov,
                                             keep = .ctl$keep,
                                             modelParams = .mv$params)
    if (!is.null(.groupedSolve)) {
      events <- .groupedSolve$events
      .ctl$iCov <- .groupedSolve$iCov
    }
  } else if (inherits(events, "data.frame") &&
             !is.null(attr(events, "rxHomGroups", exact = TRUE)) &&
             !is.null(.ctl$iCov)) {
    .mv <- rxModelVars(object)
    .groupedSolve <- .etGroupedSolveDataFrameICov(events, .ctl$iCov,
                                                  keep = .ctl$keep,
                                                  modelParams = .mv$params)
    if (!is.null(.groupedSolve)) {
      events <- .groupedSolve$events
      .ctl$iCov <- .groupedSolve$iCov
    }
  }
  if (getOption("rxode2.debug", FALSE)) {
    .rx <- rxNorm(object)
    qs2::qs_save(list(.rx, .ctl, .nms, .xtra, params, events, inits, .setupOnly), file.path(rxTempDir(), "last-rxode2.qs2"))
  }
  if (inherits(object, "function") ||
        inherits(object, "rxUi")) {
    .lst <- c(list(object, params = params, events = events, inits = inits),
              .ctl)

    return(do.call(rxSolve, .lst))
  }
  if (!any(class(object) %in% c("rxSolve", "rxode2", "character", "rxModelVars", "rxDll"))) {
    stop("Unsupported type of model trying to be solved")
  }
  .envReset <- new.env(parent=emptyenv())
  .envReset$ret <- NULL
  .envReset$reset <- TRUE
  .envReset$cacheReset <- FALSE
  .envReset$unload <- FALSE
  # take care of too many DLLs or not provided simulation errors
  .names <- NULL

  .extraNames <- character(0)

  if (inherits(.ctl$omega, "matrix")) {
    .mv <- rxModelVars(object)
    .col <- colnames(.ctl$omega)
    .w <- .col %in% .mv$params
    .ignore <- .col[!.w]
    if (length(.ignore)>0) {
      .minfo(paste0("omega has too many items, ignored: '", paste(.ignore, collapse="', '"), "'"))
    }
    .ctl$omega <-.ctl$omega[.w, .w, drop=FALSE]
    if (dim(.ctl$omega)[1] == 0) {
      .ctl$omega <- NULL
      .ctl <- do.call(rxControl, .ctl)
    }
    .names <- c(.names, .col[.w])
  } else if ( inherits(.ctl$omega, "character")) {
    .extraNames <- c(.extraNames, .ctl$omega)
    .mv <- rxModelVars(object)
    .col <- .ctl$omega
    .w <- .col %in% .mv$params
    .ignore <- .col[!.w]
    if (length(.ignore)>0) {
      .minfo(paste0("omega has too many items, ignored: '", paste(.ignore, collapse="', '"), "'"))
    }
    .names <- c(.names, .col[.w])
  }
  if (inherits(.ctl$sigma, "matrix")) {
    .mv <- rxModelVars(object)
    .col <- colnames(.ctl$sigma)
    .w <- .col %in% .mv$params
    .ignore <- .col[!.w]
    if (length(.ignore)>0) {
      .minfo(paste0("sigma has too many items, ignored: '", paste(.ignore, collapse="', '"), "'"))
    }
    .ctl$sigma <-.ctl$sigma[.w, .w, drop=FALSE]
    if (dim(.ctl$sigma)[1] == 0) {
      .ctl$sigma <- NULL
      .ctl <- do.call(rxControl, .ctl)
    }
    .names <- c(.names, .col[.w])
  } else if ( inherits(.ctl$sigma, "character")) {
    .extraNames <- c(.extraNames, .ctl$sigma)
    .mv <- rxModelVars(object)
    .col <- .ctl$sigma
    .w <- .col %in% .mv$params
    .ignore <- .col[!.w]
    if (length(.ignore)>0) {
      .minfo(paste0("sigma has too many items, ignored: '", paste(.ignore, collapse="', '"), "'"))
    }
    .names <- c(.names, .col[.w])
  }

  if (inherits(.ctl$thetaMat, "matrix")) {
    .mv <- rxModelVars(object)
    .col <- colnames(.ctl$thetaMat)
    .w <- .col %in% c(.mv$params, .extraNames)
    .ignore <- .col[!.w]
    if (length(.ignore)>0) {
      .minfo(paste0("thetaMat has too many items, ignored: '", paste(.ignore, collapse="', '"), "'"))
    }
    .ctl$thetaMat <-.ctl$thetaMat[.w, .w, drop=FALSE]
    if (dim(.ctl$thetaMat)[1] == 0) {
      .ctl$thetaMat <- NULL
      .ctl <- do.call(rxControl, .ctl)
    }
    .names <- c(.names, .col[.w])

    # now look for zero diagonals
    .col <- colnames(.ctl$thetaMat)
    .d <- diag(.ctl$thetaMat)
    .w <- which(.d == 0)
    if (length(.w) > 0) {
      .minfo(paste0("thetaMat has zero diagonal items, ignored: '", paste(.col[.w], collapse="', '"), "'"))
      .ctl$thetaMat <-.ctl$thetaMat[-.w, -.w, drop=FALSE]
      if (dim(.ctl$thetaMat)[1] == 0) {
        .ctl$thetaMat <- NULL
        .ctl <- do.call(rxControl, .ctl)
      }
      .names <- c(.names, .col[-.w])
    }
  }
  rxSetCovariateNamesForPiping(NULL)
  if (length(.ctl$.zeros) > 0) {
    if (rxIs(params, "rx.event")) {
      .tmp <- events
      events <- params
      params <- .tmp
    }
    if (inherits(params, "data.frame")) {
      for (v in .ctl$.zeros) {
        params[[v]] <- 0.0
      }
    } else if (inherits(params, "numeric") ||
                 inherits(params, "integer")) {
      params <- c(params, setNames(rep(0.0, length(.ctl$.zeros)), .ctl$.zeros))
    }
    .minfo(sprintf("omega/sigma items treated as zero: '%s'", paste(.ctl$.zeros, collapse="', '")))
  }
  .eventsForSolve <- if (is.rxEt(events) || inherits(events, "data.frame")) {
    .etPrepareSolveEvents(events, .ctl)
  } else {
    events
  }
  .replayFallbackParams <- params
  .replayFallbackEvents <- .eventsForSolve
  .replayFallbackInits <- inits
  .isSer <- .serializeInput
  .serializeMode <- if (isTRUE(.ctl$serializeFile)) {
    "temp"
  } else if (is.character(.ctl$serializeFile) && length(.ctl$serializeFile) == 1L) {
    "file"
  } else {
    "none"
  }
  .tempSerializedReplay <- FALSE
  if (.isSer && .serializeMode == "file") {
    .rxAssertSerializedSolveArgs(extras = "serializeFile", file = params)
  } else if (.isSer && .serializeMode == "temp") {
    .serializeMode <- "none"
    .ctl["serializeFile"] <- list(NULL)
  }

  .callSolve <- function() {
    ## Event ("jump") sensitivities: push the model's runtime dims to the solver
    ## (active flag + nState/nParam) so handle_evid can inject the dosing-parameter
    ## jumps.  No-op (active=0) for fd / models without jump info.
    .rxSetEventSensDims(object)
    if (.isSer) {
      .bundle <- if (is.null(.preloadedSerializedBundle)) {
        .rxReadStateBundle(params)
      } else {
        .preloadedSerializedBundle
      }
      if (!.tempSerializedReplay) {
        .rxValidateStateBundleModel(object, .bundle, params)
      }
      .rxRestoreStateBundle(.bundle)
      .bundleParams <- if (!is.null(.bundle$params)) .bundle$params else .replayFallbackParams
      .bundleEvents <- if (!is.null(.bundle$events)) .bundle$events else .replayFallbackEvents
      .bundleInits <- if (!is.null(.bundle$inits)) .bundle$inits else .replayFallbackInits
      if (inherits(.bundleEvents, "data.frame") &&
          !is.null(attr(.bundleEvents, "rxHomGroups", exact = TRUE)) &&
          !is.null(.ctl$iCov)) {
        .mv <- rxModelVars(object)
        .groupedSolve <- .etGroupedSolveDataFrameICov(.bundleEvents, .ctl$iCov,
                                                      keep = .ctl$keep,
                                                      modelParams = .mv$params)
        if (!is.null(.groupedSolve)) {
          .bundleEvents <- .groupedSolve$events
          .ctl$iCov <- .groupedSolve$iCov
        }
      }
      .bundleEventsForSolve <- if (is.rxEt(.bundleEvents) || inherits(.bundleEvents, "data.frame")) {
        .etPrepareSolveEvents(.bundleEvents, .ctl)
      } else {
        .bundleEvents
      }

      if (!is.null(.bundle$params) || !is.null(.bundle$events) || !is.null(.bundle$inits)) {
        rxSolveSEXP(object, .ctl, .nms, .xtra,
                    .bundleParams, .bundleEventsForSolve, .bundleInits,
                    setupOnlyS = .setupOnly)
      } else {
        rxSolveFromRaw_(object, .bundle$cState, .bundle$solveState, .ctl, .nms, .xtra,
                        .bundleParams, .bundleEventsForSolve, .bundleInits)
      }
    } else {
      rxSolveSEXP(object, .ctl, .nms, .xtra,
                  params, .eventsForSolve, inits,
                  setupOnlyS = .setupOnly)
    }
  }

  .callSerializeOnly <- function(file) {
    .rxAssertSerializeFileWritable(file)
    .saveCtl <- .ctl
    .saveCtl$serializeFile <- file
    on.exit(rxUnlock(object), add = TRUE)
    .collectWarnings(
      rxSolveSEXP(object, .saveCtl, .nms, .xtra,
                  params, .eventsForSolve, inits,
                  setupOnlyS = 1L)
    )
    invisible(file)
  }

  if (.serializeMode == "file") {
    return(.callSerializeOnly(.ctl$serializeFile))
  } else if (.serializeMode == "temp") {
    .tmpSerializeFile <- tempfile(fileext = ".rxbin")
    on.exit(unlink(.tmpSerializeFile), add = TRUE)
    .callSerializeOnly(.tmpSerializeFile)
    params <- .tmpSerializeFile
    .isSer <- TRUE
    .tempSerializedReplay <- TRUE
    .ctl["serializeFile"] <- list(NULL)
  }

  if (getOption("rxode2.debug", FALSE)) {
    .envReset$ret <- .collectWarnings(.callSolve(), lst = TRUE)
  } else {
    while (.envReset$reset) {
      .envReset$reset <- FALSE
      tryCatch({
        .envReset$ret <- .collectWarnings(.callSolve(), lst = TRUE)
      },
      error=function(e) {
        if (regexpr("not provided by package", e$message) != -1) {
          if (.envReset$cacheReset) {
            .malert("unsuccessful cache reset; try manual reset with 'rxClean()'")
            stop(e)
          } else {
            # reset
            gc()
            .minfo("try resetting cache")
            rxode2::rxClean()
            .envReset$cacheReset <- TRUE
            .envReset$reset <- TRUE
            .msuccess("done")
          }
        } else if (regexpr("maximal number of DLLs reached", e$message) != -1) {
          if (.envReset$unload) {
            .malert("Could not unload rxode2 models, try restarting R")
            stop(e)
          } else {
            # reset
            gc()
            .minfo("try resetting cache and unloading all rxode2 models")
            try(rxode2::rxUnloadAll(), silent=TRUE)
            rxode2::rxClean()
            .envReset$unload <- TRUE
            .envReset$reset <- TRUE
            .msuccess("done")
          }
        } else {
          stop(e)
        }
      })
    }

  }
  .ret <- .envReset$ret
  .ws <- .ret[[2]]
  if (length(.ws) > 0L &&
      inherits(.ctl$iCov, "data.frame") &&
      length(names(.ctl$iCov)) > 0L) {
    .iCovNames <- tolower(names(.ctl$iCov))
    .ws <- vapply(.ws, function(.w) {
      if (!startsWith(.w, "Cannot keep missing columns:")) {
        return(.w)
      }
      .miss <- sub("^Cannot keep missing columns:\\s*", "", .w)
      .miss <- strsplit(.miss, "[[:space:],]+")[[1]]
      .miss <- .miss[nzchar(.miss)]
      if (length(.miss) == 0L) {
        return(.w)
      }
      .missKeep <- .miss[!(tolower(.miss) %in% .iCovNames)]
      if (length(.missKeep) == 0L) {
        return(NA_character_)
      }
      paste("Cannot keep missing columns:", paste(.missKeep, collapse = " "))
    }, character(1), USE.NAMES = FALSE)
    .ws <- unique(.ws[!is.na(.ws)])
  }
  .rxModels$.ws <- .ws
  lapply(.ws, function(x) warning(x, call. = FALSE))
  .ret <- .ret[[1]]
  if (.ctl$returnType == 4L) {
    data.table::setDT(.ret)
  } else if (.ctl$returnType == 5L) {
    .ret <- tibble::as_tibble(.ret)
  }
  return(.ret)
}

#' @rdname rxSolve
#' @export
update.rxSolve <- function(object, ...) {
  rxSolve(object, ...)
}

#' @rdname rxSolve
#' @export
rxSolve.rxSolve <- function(object, params = NULL, events = NULL, inits = NULL, ...,
                            theta = NULL, eta = NULL, envir = parent.frame()) {
  if (is.rxEt(params) && !is.rxEt(events)) {
    .tmp <- events
    events <- params
    params <- .tmp
  }
  if (is.null(events)) {
    return(rxSolve.default(object, params = params, events = events, inits = inits, ...,
                           theta = theta, eta = eta, envir = envir))
  }
  .model <- object$model
  if (is.null(.model)) {
    return(rxSolve.default(object, params = params, events = events, inits = inits, ...,
                           theta = theta, eta = eta, envir = envir))
  }
  .dots <- list(...)
  if (isTRUE(.dots$updateObject)) {
    # Pass the rxSolve object directly so C++ updates the correct object
    # rather than the global rxCurObj, which may point to a different rxSolve.
    if (is.null(params)) params <- object$.params.single
    if (is.null(inits)) inits <- object$inits
    return(rxSolve.default(object, params = params, events = events, inits = inits, ...,
                           theta = theta, eta = eta, envir = envir))
  }
  .model <- as.character(.model)
  if (is.null(params)) params <- object$.params.single
  if (is.null(inits)) inits <- object$inits
  rxSolve(.model, params = params, events = events, inits = inits, ...,
          theta = theta, eta = eta, envir = envir)
}

#' @rdname rxSolve
#' @export
predict.rxode2 <- function(object, ...) {
  rxSolve(object, ...)
}

#' @rdname rxSolve
#' @export
predict.function <- function(object, ...) {
  rxSolve(object, ...)
}

#' @rdname rxSolve
#' @export
predict.rxUi <- function(object, ...) {
  .udfEnvSet(list(object$meta, parent.frame(1)))
  rxSolve(object, ...)
}

#' @rdname rxSolve
#' @export
predict.rxSolve <- predict.rxode2

#' @rdname rxSolve
#' @export
predict.rxEt <- predict.rxode2

#' @rdname rxSolve
#' @export
predict.rxParams <- predict.rxode2

#' @importFrom stats simulate

#' @rdname rxSolve
#' @export
simulate.rxode2 <- function(object, nsim = 1L, seed = NULL, ...) {
  rxSolve(object, ..., seed = seed, nsim = nsim)
}
#' @rdname rxSolve
#' @export
simulate.rxSolve <- simulate.rxode2


#' @rdname rxSolve
#' @export
simulate.rxParams <- simulate.rxode2

#' @rdname rxSolve
#' @export
solve.rxSolve <- function(a, b, ...) {
  lst <- as.list(match.call()[-1])
  n <- names(lst)
  if (!missing(a)) {
    n[n == "a"] <- ""
  }
  if (!missing(b)) {
    n[n == "b"] <- ""
  }
  names(lst) <- n
  do.call("rxSolve", lst, envir = parent.frame(1))
}

#' @rdname rxSolve
#' @export
solve.rxUi <- solve.rxSolve

#' @rdname rxSolve
#' @export
solve.function <- solve.rxSolve

#' @rdname rxSolve
#' @export
solve.rxode2 <- solve.rxSolve

#' @rdname rxSolve
#' @export
solve.rxParams <- solve.rxSolve

#' @rdname rxSolve
#' @export
solve.rxEt <- solve.rxSolve

#' @export
`$.rxSolveParams` <- function(obj, arg, exact = FALSE) {
  return(.Call(`_rxode2_rxSolveGet`, obj, arg, exact))
}


#' @export
`$.rxSolveCovs` <- function(obj, arg, exact = FALSE) {
  return(.Call(`_rxode2_rxSolveGet`, obj, arg, exact))
}

#' @export
`$.rxSolveSimType` <- function(obj, arg, exact = FALSE) {
  return(.Call(`_rxode2_rxSolveGet`, obj, arg, exact))
}

.rxSolveUpdateParNames <- function(parNames, .env) {
  .ret <- parNames
  for (.what in c(".nestTheta", ".nestEta")) {
    if (exists(.what, envir = .env, inherits = FALSE)) {
      .map <- get(.what, envir = .env, inherits = FALSE)
      .mapNames <- names(.map)
      if (!is.null(.mapNames)) {
        for (.i in seq_along(.ret)) {
          .w <- match(.ret[[.i]], .mapNames)
          if (!is.na(.w)) {
            .ret[[.i]] <- .map[[.w]]
          }
        }
      }
    }
  }
  .ret
}

.rxSolveMaterializeParams <- function(obj, .env) {
  if (exists(".params.dat", envir = .env, inherits = FALSE)) {
    return(invisible(TRUE))
  }
  .mv <- rxModelVars(obj)
  .mvIni <- .mv$ini
  .pars <- .mv$params
  .parso <- get(".args.params", envir = .env, inherits = FALSE)
  .ppos <- get(".par.pos", envir = .env, inherits = FALSE)
  .isIni <- isTRUE(get(".par.pos.ini", envir = .env, inherits = FALSE))
  .idLevels <- get(".idLevels", envir = .env, inherits = FALSE)
  if (is.null(.parso) ||
      ((is.numeric(.parso) || is.integer(.parso)) && is.null(dim(.parso)))) {
    .parNumeric <- if (is.null(.parso)) numeric(0) else as.numeric(.parso)
    .vals <- numeric(0)
    .nms <- character(0)
    for (.i in seq_along(.ppos)) {
      if (.ppos[[.i]] > 0) {
        .vals <- c(.vals, if (.isIni) .mvIni[[.ppos[[.i]]]] else .parNumeric[[.ppos[[.i]]]])
        .nms <- c(.nms, .pars[[.i]])
      } else if (.ppos[[.i]] < 0) {
        .vals <- c(.vals, .mvIni[[-.ppos[[.i]]]])
        .nms <- c(.nms, .pars[[.i]])
      }
    }
    .nms <- .rxSolveUpdateParNames(.nms, .env)
    .single <- stats::setNames(.vals, .nms)
    .dat <- as.data.frame(as.list(.single), check.names = FALSE)
    assign(".params.single", .single, envir = .env)
    assign(".params.dat", .dat, envir = .env)
  } else {
    .parsDf <- as.data.frame(.parso, check.names = FALSE)
    .nsub <- get(".nsub", envir = .env, inherits = FALSE)
    .nsim <- get(".nsim", envir = .env, inherits = FALSE)
    .dat <- as.data.frame(matrix(nrow = nrow(.parsDf), ncol = 0))
    if (.nsim > 1) {
      .dat[["sim.id"]] <- (seq_len(nrow(.parsDf)) - 1L) %/% .nsub + 1L
    }
    if (.nsub > 1) {
      .id <- (seq_len(nrow(.parsDf)) - 1L) %% .nsub + 1L
      if (length(.idLevels) > 0) {
        .id <- factor(.id, levels = seq_along(.idLevels), labels = .idLevels)
      }
      .dat[["id"]] <- .id
    }
    for (.i in seq_along(.ppos)) {
      if (.ppos[[.i]] > 0) {
        .dat[[.rxSolveUpdateParNames(.pars[[.i]], .env)]] <- .parsDf[[.ppos[[.i]]]]
      } else if (.ppos[[.i]] < 0) {
        .dat[[.rxSolveUpdateParNames(.pars[[.i]], .env)]] <- rep(.mvIni[[-.ppos[[.i]]]], nrow(.parsDf))
      }
    }
    assign(".params.dat", .dat, envir = .env)
    if (nrow(.dat) == 1L) {
      .single <- stats::setNames(as.numeric(.dat[1, , drop = TRUE]), names(.dat))
      assign(".params.single", .single, envir = .env)
    } else {
      assign(".params.single", NULL, envir = .env)
    }
  }
  assign("counts", data.frame(
    slvr = get(".slvr.counter", envir = .env, inherits = FALSE),
    dadt = get(".dadt.counter", envir = .env, inherits = FALSE),
    jac = get(".jac.counter", envir = .env, inherits = FALSE)
  ), envir = .env)
  invisible(TRUE)
}

.rxSolveGetInit <- function(.env, arg) {
  .ini <- get(".init.dat", envir = .env, inherits = FALSE)
  if (arg %in% c("inits", "init")) {
    return(.ini)
  }
  for (.nm in names(.ini)) {
    if (arg %in% c(paste0(.nm, "0"), paste0(.nm, ".0"), paste0(.nm, "_0"),
                   paste0(.nm, "(0)"), paste0(.nm, "[0]"), paste0(.nm, "{0}"))) {
      return(unname(.ini[[.nm]]))
    }
  }
  NULL
}

#' @export
`$.rxSolve` <- function(obj, arg, exact = FALSE) {
  if (arg == "rxModelVars") return(rxModelVars(obj))
  .cls <- attr(obj, "class")
  .env <- attr(.cls, ".rxode2.env")
  if (is.environment(.env) && exists(arg, envir = .env, inherits = FALSE)) {
    .val <- get(arg, envir = .env, inherits = FALSE)
    if (is.function(.val)) {
      rxSolveSetCurObj_(obj)
      return(.val)
    }
    return(.val)
  }
  if (is.environment(.env)) {
    if (arg %in% c("params", "par", "pars", "param")) {
      .rxSolveMaterializeParams(obj, .env)
      return(get(".params.dat", envir = .env, inherits = FALSE))
    }
    .init <- .rxSolveGetInit(.env, arg)
    if (!is.null(.init)) {
      return(.init)
    }
  }
  return(.Call(`_rxode2_rxSolveGet`, obj, arg, exact))
}

#' Check to see if this is an rxSolve object.
#'
#' @param x object to check to see if it is rxSolve
#'
#' If this is an rxSolve object that has expired strip all rxSolve
#' information.
#'
#' @return boolean indicating if this is a `rxSolve` object
#'
#' @author Matthew L.Fidler
#' @export
#' @keywords internal
is.rxSolve <- function(x) {
  .Call(`_rxode2_rxIs`, x, "rxSolve")
}

#' @export
`[.rxSolve` <- function(x, i, j, drop) {
  class(x) <- "data.frame"
  NextMethod("[")
}

#' @export
"[[.rxSolve" <- function(obj, arg, exact = TRUE) {
  return(.Call(`_rxode2_rxSolveGet`, obj, arg, exact))
}

#' @export
t.rxSolve <- function(x) {
  x <- as.matrix(x)
  NextMethod("t", x)
}

#' @export
dimnames.rxSolve <- function(x) {
  list(row.names(x), names(x))
}

#' @export
"dimnames<-.rxSolve" <- function(x, value) {
  class(x) <- "data.frame"
  "dimnames<-.data.frame"(x, value)
}

#' @export
"dimnames<-.rxSolve" <- function(x, value) {
  class(x) <- "data.frame"
  "dimnames<-.data.frame"(x, value)
}

#' @export
"[<-.rxSolve" <- function(x, i, j, value) {
  if (missing(i) && !missing(j)) {
    if (rxIs(j, "character")) {
      ret <- .Call(`_rxode2_rxSolveUpdate`, x, j, value)
      if (is.null(ret)) {
        class(x) <- "data.frame"
        return(`[<-.data.frame`(x, , j, value = value))
      } else {
        return(ret)
      }
    }
  }
  class(x) <- "data.frame"
  if (nargs() < 4) {
    if (missing(j)) {
      return(`[<-.data.frame`(x, i, value = value))
    } else {
      return(`[<-.data.frame`(x, , j, value = value))
    }
  } else {
    return(`[<-.data.frame`(x, i, j, value))
  }
  class(x) <- "data.frame"
  if (nargs() < 4) {
    if (missing(j)) {
      return(`[<-.data.frame`(x, i, value = value))
    } else {
      return(`[<-.data.frame`(x, , j, value = value))
    }
  } else {
    return(`[<-.data.frame`(x, i, j, value))
  }
}
#' @export
`$<-.rxSolve` <- function(x, name, value) {
  if (is.null(value)) {
    class(x) <- "data.frame"
    return(`$<-.data.frame`(x, name, value))
  }
  ret <- .Call(`_rxode2_rxSolveUpdate`, x, name, value)
  if (is.null(ret)) {
    class(x) <- "data.frame"
    return(`$<-.data.frame`(x, name, value))
  } else {
    return(ret)
  }
}
#' @export
"[[<-.rxSolve" <- function(x, i, j, value) {
  if (is.null(value)) {
    class(x) <- "data.frame"
    if (missing(j)) {
      return("[[<-.data.frame"(x, i, value = value))
    } else {
      return("[[<-.data.frame"(x, i, j, value))
    }
  }
  if (missing(j) && rxIs(i, "character")) {
    ret <- .Call(`_rxode2_rxSolveUpdate`, x, i, value)
    if (!is.null(ret)) {
      return(ret)
    } else {
      class(x) <- "data.frame"
      if (missing(j)) {
        return("[[<-.data.frame"(x, i, value = value))
      } else {
        return("[[<-.data.frame"(x, i, j, value))
      }
    }
  } else {
    class(x) <- "data.frame"
    if (missing(j)) {
      return("[[<-.data.frame"(x, i, value = value))
    } else {
      return("[[<-.data.frame"(x, i, j, value))
    }
  }
}

#' Update Solved object with '+'
#'
#' @param solved Solved object
#' @param new New information added to the table.
#' @return new solved object
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
`+.rxSolve` <- function(solved, new) {
  if (rxIs(new, "rx.event")) {
    return(update(solved, events = new))
  } else {
    return(as.data.frame(solved) + new)
  }
}

drop_units.rxSolve <- function(x) {
  dropUnitsRxSolve(x)
}

## dim (gets you nrow and ncol), t, dimnames
##
## [1] $<-           [             [[<-          [<-           all.equal
## [6] anyDuplicated as.data.frame as.data.table as.list       as.matrix
## [11] coerce        coerce<-      dcast         dim           dimnames
## [16] dimnames<-    duplicated    format        head          initialize
## [21] is.na         melt          merge         na.omit       names<-
## [26] Ops           print         show          slotsFromS3   split
## [31] subset        tail          transform     unique        within

#' @rdname rxSolve
#' @export
rxControl <- function(..., params = NULL, events = NULL, inits = NULL, envir=parent.frame()) {
  rxSolve(object = NULL, params = params, events = events, inits = inits, ...,
          envir=envir)
}

#' @export
rxEtDispatchSolve.rxode2et <- function(x, ...) {
  .lst <- x
  class(.lst) <- NULL
  do.call(rxSolve, .lst)
}

#' Conversion between character and integer ODE integration methods for rxode2
#'
#' If \code{NULL} is given as the method, all choices are returned as a named
#' vector.
#'
#' @param method The method for solving ODEs.  Currently this supports:
#'
#' * `"liblsoda"` thread safe lsoda.  This supports parallel
#'            thread-based solving, and ignores user Jacobian specification.
#'
#' * `"lsoda"` -- LSODA solver.  Does not support parallel thread-based
#'       solving, but allows user Jacobian specification.
#'
#' * `"dop853"` -- DOP853 solver.  Does not support parallel thread-based
#'         solving nor user Jacobian specification
#'
#' * `"indLin"` -- Solving through inductive linearization.  The rxode2 dll
#'         must be setup specially to use this solving routine.
#'
#' * `"f78"` -- Runge-Kutta Fehlberg 78 solver using Boost's odeint library.
#'
#' * `"rk4"` -- Runge-Kutta 4 solver using Boost's odeint library.
#'
#' * `"ck54"` -- Cash-Karp 5(4) solver using Boost's odeint library.
#'
#' * `"ab"` -- Adams-Bashforth multi-step solver with a direct implementation
#'   (order 1-8, default 5). Uses RK4 to initialize the derivative history and
#'   then applies the Adams-Bashforth extrapolation formula. Is a fixed-step
#'   method (step size controlled by `hmin`).
#'
#' * `"abm"` -- Adams-Bashforth-Moulton solver using Boost's odeint library.
#'
#' * `"dop5"` -- DOPRI5 solver using Boost's odeint library (supports dense output).
#'
#' * `"bs"` -- Bulirsch-Stoer solver using Boost's odeint library (supports dense output).
#'
#' * `"ros4"` **(implicit)** -- Rosenbrock 4 solver using Boost's odeint library (supports dense output).
#'   Requires an analytical Jacobian (auto-computed from the model when not provided);
#'   falls back to `liblsoda` if Jacobian generation fails.
#'
#' * `"ros43"` **(implicit)** -- Kaps-Rentrop 4th-order A-stable Rosenbrock method (GRK4A) from libode.
#'   Adaptive step size via embedded 3rd-order error estimate.
#'   Requires an analytical Jacobian (auto-computed from the model when not provided);
#'   falls back to `liblsoda` if Jacobian generation fails.
#'
#' * `"ros6"` **(implicit)** -- Kaps-Wanner 6th-order A-stable Rosenbrock method (ROW6A) from libode.
#'   Fixed step size (set with `hmin`). Requires Jacobian; falls back to `liblsoda` if unavailable.
#'
#' * `"backwardEuler"` **(implicit)** -- Backward Euler (BDF-1), 1st-order L-stable fully implicit method from libode.
#'   Fixed step size (set with `hmin`). Requires Jacobian; falls back to `liblsoda` if unavailable.
#'
#' * `"gauss6"` **(implicit)** -- Gauss-Legendre 6th-order A-stable fully-implicit method (3 stages) from libode.
#'   Fixed step size. Requires Jacobian; falls back to `liblsoda` if unavailable.
#'
#' * `"iiic6"` **(implicit)** -- Lobatto IIIC 6th-order L-stable fully-implicit method (4 stages) from libode.
#'   Fixed step size. Requires Jacobian; falls back to `liblsoda` if unavailable.
#'
#' * `"radauiia5"` **(implicit)** -- Radau IIA 5th-order L-stable fully-implicit method (3 stages) from libode.
#'   Fixed step size (set with `hmin`). Requires Jacobian; falls back to `liblsoda` if unavailable.
#'
#' * `"geng5"` **(implicit)** -- Geng 5th-order symplectic fully-implicit method (3 stages) from libode.
#'   Fixed step size. Requires Jacobian; falls back to `liblsoda` if unavailable.
#'
#' * `"sdirk43"` **(implicit)** -- L-stable SDIRK 4(3) pair from libode.
#'   Adaptive step size via embedded 3rd-order error estimate.
#'   Requires Jacobian; falls back to `liblsoda` if unavailable.
#'
#' * `"iem"` **(implicit)** -- Implicit Euler solver using Boost's odeint library.
#'   Requires an explicit Jacobian (auto-generated via `calcJac=TRUE` when not provided).
#'   Uses Boost.uBLAS vectors and is a fixed-step method (step size controlled by `hmin`).
#'
#' * `"sem"` -- Symplectic Euler solver using Boost's odeint library.
#'   Requires splitting the state into coordinate-momentum pairs (and
#'   automatically pads odd-dimensional systems).  Is a fixed-step
#'   method (step size controlled by `hmin`).
#'
#' * `"sb3a"` -- Symplectic Runge-Kutta-Nystrom SB3A McLachlan stepper
#'   using Boost's odeint library.  Requires splitting the state into
#'   coordinate-momentum pairs (and automatically pads odd-dimensional
#'   systems).  Is a fixed-step method (step size controlled by
#'   `hmin`).
#'
#' * `"sb3am4"` -- Symplectic Runge-Kutta-Nystrom SB3A M4 McLachlan
#'   stepper using Boost's odeint library.  Requires splitting the
#'   state into coordinate-momentum pairs (and automatically pads
#'   odd-dimensional systems).  Is a fixed-step method (step size
#'   controlled by `hmin`).
#'
#' * `"vv"` -- Velocity Verlet stepper using Boost's odeint library.
#'   Requires splitting the state into coordinate-momentum pairs (and
#'   automatically pads odd-dimensional systems).  Is a fixed-step
#'   method (step size controlled by `hmin`).
#'
#' * `"mm"` -- Modified Midpoint stepper using Boost's odeint library.
#'   Is a fixed-step method (step size controlled by `hmin`) and uses
#'   the `order` parameter to configure the number of intermediate
#'   steps.
#'
#' * `"em"` -- Explicit Euler stepper using Boost's odeint library.
#'   Is a fixed-step method (step size controlled by `hmin`).
#'
#' * `"cvode"` -- CVODE BDF stiff solver from the SUNDIALS library (vendored
#'    from the sundialr package sources).  Supports thread-parallel solving and
#'    per-compartment absolute tolerances.
#'
#' * `"trapz"` -- Explicit trapezoidal rule (Heun's method), a 2nd-order
#'   fixed-step Runge-Kutta method implemented via the libode library.
#'   Each inter-event interval is integrated with fixed steps of size
#'   `hmin` (default `0.01` when `hmin=0`).  If `hmin` would require
#'   more than `maxsteps` steps to span the interval, the step size is
#'   automatically enlarged to stay within that limit.  When an
#'   inter-event interval is shorter than the nominal step size (e.g.,
#'   two doses very close together), the step is silently clamped to the
#'   interval length so that short intervals are always handled correctly.
#'   Supports parallel thread-based solving and steady-state (`ss=1`)
#'   dosing.  The method does not use `atol` or `rtol` (fixed-step; no
#'   error control).  Steady-state convergence is governed by `ssAtol`,
#'   `ssRtol`, `minSS`, `maxSS`, and `strictSS`.
#'
#' * `"ssp3"` -- Strong Stability-Preserving Runge-Kutta of order 3 (SSP-RK3,
#'   Shu-Osher method), implemented via the libode library.  A 3-stage,
#'   3rd-order explicit fixed-step method with superior non-oscillatory
#'   properties for problems with discontinuities or sharp fronts.  Uses
#'   the Butcher tableau c2=1, a21=1; c3=1/2, a31=1/4, a32=1/4;
#'   b1=1/6, b2=1/6, b3=2/3.  The step-size behavior, clamping, and
#'   steady-state options (`hmin`, `maxsteps`, `ssAtol`, `ssRtol`,
#'   `minSS`, `maxSS`, `strictSS`) are identical to `"trapz"`.  Does
#'   not use `atol` or `rtol` (fixed-step; no error control).  Supports
#'   parallel thread-based solving and steady-state (`ss=1`) dosing.
#'
#' * `"f32"` -- Fehlberg 3(2) embedded pair from libode (class `OdeRKF32`).
#'   A 3-stage, 3rd-order adaptive method with a built-in 2nd-order error
#'   estimate for automatic step-size control.  Tableau: c2=1, a21=1;
#'   c3=1/2, a31=1/4, a32=1/4; primary (3rd-order) weights d1=1/6, d2=1/6,
#'   d3=4/6; embedded (2nd-order) weights b1=1/2, b2=1/2.  Uses `atol` and `rtol`
#'   for error control.  The `hmin`
#'   parameter sets the initial step size (default `0.01`); subsequent steps
#'   are chosen adaptively.  The total number of accepted and rejected steps
#'   is bounded by `maxsteps`.  Supports parallel thread-based solving and
#'   steady-state (`ss=1`) dosing with convergence governed by `ssAtol`,
#'   `ssRtol`, `minSS`, `maxSS`, and `strictSS`.
#'
#' * `"rk43"` -- Runge-Kutta 4(3) pair from libode.  A 5-stage, 4th-order
#'   adaptive method with a built-in 3rd-order embedded error estimate for
#'   automatic step-size control.  Tableau: c2=1/3, a21=1/3; c3=2/3,
#'   a31=-1/3, a32=1; c4=1, a41=1, a42=-1, a43=1; a5j=bj (FSAL).
#'   Primary (4th-order) weights b1=b4=1/8, b2=b3=3/8; embedded (3rd-order)
#'   weights d1=1/12, d2=1/2, d3=1/4, d5=1/6.  Since a5j=bj the 5th stage
#'   evaluation is reused as the 1st stage of the next step (FSAL).
#'   Uses `atol` and `rtol` for error control.  The `hmin` parameter sets
#'   the initial step size (default `0.01`); subsequent steps are chosen
#'   adaptively.  The
#'   total number of steps is bounded by `maxsteps`.  Supports parallel
#'   thread-based solving and steady-state (`ss=1`) dosing with convergence
#'   governed by `ssAtol`, `ssRtol`, `minSS`, `maxSS`, and `strictSS`.
#'
#' * `"dop54"` -- Dormand-Prince 5(4) FSAL method (Dormand & Prince 1980),
#'   implemented via the libode library.  The same algorithm as MATLAB's
#'   `ode45`.  A 7-stage, 5th-order adaptive method with an embedded
#'   4th-order error estimate for automatic step-size control (FSAL: the
#'   7th stage evaluation is reused as the 1st stage of the next step,
#'   giving effectively 6 function evaluations per accepted step).  Uses
#'   `atol` and `rtol` for error control.  The `hmin` parameter sets the
#'   initial step size (default `0.01`); subsequent steps are chosen
#'   adaptively.  The total number of steps is bounded by `maxsteps`.
#'   Supports parallel thread-based solving and steady-state (`ss=1`)
#'   dosing with convergence governed by `ssAtol`, `ssRtol`, `minSS`,
#'   `maxSS`, and `strictSS`.  NaN/Inf in derivatives (e.g. from NA
#'   parameters) is detected immediately and the solve exits with NA
#'   output for the affected subject.
#'
#' * `"vern65"` -- Jim Verner's "most efficient" 6(5) FSAL pair (9 stages),
#'   implemented via the libode library using coefficients from Verner's own
#'   website (RKV65.IIIXb.Efficient).  A 6th-order adaptive method with an
#'   embedded 5th-order error estimate.  The sparse tableau (many zero
#'   a-coefficients) reduces function evaluations; the FSAL property means the
#'   9th stage evaluation is reused as the 1st stage of the next step, giving
#'   effectively 8 function evaluations per accepted step.  Uses `atol` and
#'   `rtol` for error control.  The `hmin` parameter sets the initial step
#'   size (default `0.01`); subsequent steps are chosen adaptively.  The total
#'   number of steps is bounded by `maxsteps`.  Supports parallel thread-based
#'   solving and steady-state (`ss=1`) dosing with convergence governed by
#'   `ssAtol`, `ssRtol`, `minSS`, `maxSS`, and `strictSS`.  NaN/Inf in
#'   derivatives is detected immediately and the solve exits with NA output.
#'
#' * `"vern76"` -- Jim Verner's "most efficient" 7(6) pair (10 stages),
#'   implemented via the libode library using coefficients from Verner's own
#'   website (RKV76.IIa.Efficient).  A 7th-order adaptive method with an
#'   embedded 6th-order error estimate.  The sparse tableau reduces function
#'   evaluations per step.  Uses `atol` and `rtol` for error control.  The
#'   `hmin` parameter sets the initial step size (default `0.01`); subsequent
#'   steps are chosen adaptively.  The total number of steps is bounded by
#'   `maxsteps`.  Supports parallel thread-based solving and steady-state
#'   (`ss=1`) dosing with convergence governed by `ssAtol`, `ssRtol`,
#'   `minSS`, `maxSS`, and `strictSS`.  NaN/Inf in derivatives is detected
#'   immediately and the solve exits with NA output.
#'
#' * `"dop87"` -- Dormand-Prince 8(7) pair (13 stages), implemented via the
#'   libode library using coefficients from Hairer, Norsett and Wanner (1993)
#'   "Solving ODEs I" (2nd ed.).  An 8th-order adaptive method with an
#'   embedded 7th-order error estimate.  Both solutions are computed from the
#'   original state in the final step loop.  Uses `atol` and `rtol` for error
#'   control.  The `hmin` parameter sets the initial step size (default
#'   `0.01`); subsequent steps are chosen adaptively.  The total number of
#'   steps is bounded by `maxsteps`.  Supports parallel thread-based solving
#'   and steady-state (`ss=1`) dosing with convergence governed by `ssAtol`,
#'   `ssRtol`, `minSS`, `maxSS`, and `strictSS`.  NaN/Inf in derivatives is
#'   detected immediately and the solve exits with NA output.
#'
#' * `"vern98"` -- Jim Verner's "most efficient" 9(8) pair (16 stages),
#'   implemented via the libode library using coefficients from Verner's own
#'   website (RKV98.IIa.Efficient).  A 9th-order adaptive method with an
#'   embedded 8th-order error estimate.  The highly sparse tableau (stages
#'   8-16 use only \eqn{k_{0}}{x_0} and \eqn{k_{5..}}{k_5..}) minimizes function evaluations per
#'   step.  Both solutions are computed from the original state in the final
#'   step loop.  Uses `atol` and `rtol` for error control.  The `hmin`
#'   parameter sets the initial step size (default `0.01`); subsequent steps
#'   are chosen adaptively.  The total number of steps is bounded by
#'   `maxsteps`.  Supports parallel thread-based solving and steady-state
#'   (`ss=1`) dosing with convergence governed by `ssAtol`, `ssRtol`,
#'   `minSS`, `maxSS`, and `strictSS`.  NaN/Inf in derivatives is detected
#'   immediately and the solve exits with NA output.
#'
#' **Aliases for existing methods** (no additional C++ code; `hmin`, `atol`,
#' `rtol`, and `maxsteps` follow the aliased method):
#'
#' * `"dp54"` -- alias for `"dop54"` (Dormand-Prince 5(4) FSAL, libode).
#'
#' * `"v65e"` -- alias for `"vern65"` (Verner 6(5) efficient, libode).
#'
#' * `"v76e"` -- alias for `"vern76"` (Verner 7(6) efficient, libode).
#'
#' * `"dp87"` -- alias for `"dop87"` (Dormand-Prince 8(7), libode).
#'
#' * `"v98e"` -- alias for `"vern98"` (Verner 9(8) efficient, libode).
#'
#' * `"ssp33"` -- alias for `"ssp3"` (Strong Stability-Preserving RK3, libode).
#'
#' **libode fixed-step explicit methods**.  All use `hmin` as the fixed step
#' size (default `0.01` when `hmin=0`); `atol` and `rtol` are ignored (no
#' error control); `maxsteps` bounds the total number of steps; NaN/Inf in
#' derivatives sets `ind$err` and the subject exits with NA output.  All
#' support parallel thread-based solving.
#'
#' * `"euler"` -- Forward (explicit) Euler, 1st-order, 1 stage.  Requires a
#'   very small step size for accuracy (e.g., `hmin=1e-4`); intended for
#'   pedagogical use or method-comparison baselines.
#'
#' * `"midpoint"` -- Explicit midpoint rule, 2nd-order, 2 stages.
#'
#' * `"heun"` -- Heun's method (explicit trapezoid), 2nd-order, 2 stages.
#'   Identical in structure to `"trapz"` but uses the libode fixed-step driver.
#'
#' * `"ssp22"` -- Strong Stability-Preserving 2-stage 2nd-order method
#'   (SSP-RK22), 2 stages.  Superior non-oscillatory properties near
#'   discontinuities.
#'
#' * `"rk3"` -- Classical 3rd-order Runge-Kutta (Kutta 1901), 3 stages.
#'
#' * `"ssp53"` -- Strong Stability-Preserving 5-stage 3rd-order method
#'   (SSP-RK53), 5 stages.  High SSP coefficient for hyperbolic PDEs or
#'   event-heavy ODE systems.
#'
#' * `"s4"` -- Shanks 4th-order method, 4 stages.
#'
#' * `"r4"` -- Ralston's 4th-order method, 4 stages.  Minimizes local
#'   truncation error among classical 4-stage 4th-order methods.
#'
#' * `"ls44"` -- Low-storage 4th-order method, 4 stages.  Uses a 2-register
#'   update scheme that minimizes memory bandwidth at the cost of a less
#'   general tableau structure.
#'
#' * `"ls54"` -- Low-storage 4th-order method, 5 stages.  Five-stage
#'   variant of the 2-register low-storage scheme.
#'
#' * `"ssp54"` -- Strong Stability-Preserving 5-stage 4th-order method
#'   (SSP-RK54), 5 stages.
#'
#' * `"s5"` -- Shanks 5th-order method, 5 stages.
#'
#' * `"rk5"` -- Classical 5th-order Runge-Kutta, 6 stages.
#'
#' * `"c5"` -- Cassity 5th-order method, 6 stages.
#'
#' * `"l5"` -- Lawson 5th-order method, 6 stages.
#'
#' * `"lk5a"` -- Luther-Konen 5th-order method, variant A, 6 stages.
#'
#' * `"lk5b"` -- Luther-Konen 5th-order method, variant B, 6 stages.
#'
#' * `"b6"` -- Butcher 6th-order method, 7 stages.
#'
#' * `"s7"` -- Shanks 7th-order method, 9 stages.
#'
#' * `"s8_10"` -- Shanks 8th-order method, 10 stages.
#'
#' * `"cv8"` -- Cooper-Verner 8th-order method, 11 stages.
#'
#' * `"s8_12"` -- Shanks 8th-order method, 12 stages.
#'
#' * `"s10"` -- Stepanov 10th-order method, 15 stages.  Requires a moderate
#'   step size (e.g., `hmin=1.0`) because a single step is accurate to very
#'   high order.
#'
#' * `"z10"` -- Zhang 10th-order method, 16 stages.
#'
#' * `"o10"` -- Ono 10th-order method, 17 stages.
#'
#' * `"h10"` -- Hairer 10th-order method, 17 stages.
#'
#' **libode adaptive (variable-step) explicit methods**.  All use `atol` and
#' `rtol` for error control;
#' `hmin` sets the initial step size (default `0.01` when `hmin=0`); `hmax`
#' sets the maximum step size; `maxsteps` bounds total steps; NaN/Inf in
#' derivatives sets `ind$err` and the subject exits with NA output.  All
#' support parallel thread-based solving and steady-state (`ss=1`) dosing
#' (convergence governed by `ssAtol`, `ssRtol`, `minSS`, `maxSS`, `strictSS`).
#'
#' * `"bs32"` -- Bogacki-Shampine 3(2) FSAL pair, 4 stages (Bogacki &
#'   Shampine 1989).  3rd-order primary with 2nd-order embedded error estimate.
#'   FSAL: the 4th-stage evaluation is reused as the 1st stage of the next
#'   step.  The same algorithm as Julia `BS3()` and MATLAB `ode23`.
#'
#' * `"ssp43"` -- Strong Stability-Preserving 4(3) pair, 4 stages.
#'   Adaptive SSP method with a 3rd-order embedded error estimate.
#'
#' * `"f45"` -- Fehlberg 4(5) pair, 6 stages (Fehlberg 1970).  4th-order
#'   primary solution with a 5th-order embedded estimate used for error
#'   control.
#'
#' * `"t54"` -- Tsitouras 5(4) FSAL pair, 7 stages (Tsitouras 2011).
#'   5th-order primary with 4th-order embedded error estimate.  FSAL: the
#'   7th stage is reused as the 1st stage of the next step.  The same
#'   Butcher tableau as Julia's `Tsit5()`.
#'
#' * `"s54"` -- Stepanov 5(4) FSAL pair, 7 stages.  5th-order primary
#'   with 4th-order embedded error estimate.
#'
#' * `"pp54"` -- Papakostas-Papageorgiou 5(4) FSAL pair, 7 stages.
#'
#' * `"pp54b"` -- Papakostas-Papageorgiou 5(4) variant B FSAL pair,
#'   7 stages.
#'
#' * `"bs54"` -- Bogacki-Shampine 5(4) pair, 8 stages.  5th-order primary
#'   with 4th-order embedded error estimate (non-FSAL).
#'
#' * `"ss54"` -- Sharp-Smart 5(4) pair, 7 stages.
#'
#' * `"dp65"` -- Dormand-Prince 6(5) pair, 8 stages.  6th-order primary
#'   with 5th-order embedded error estimate.
#'
#' * `"c65"` -- Calvo 6(5) pair, 9 stages.
#'
#' * `"tp64"` -- Tsitouras-Papakostas 6(4) pair, 7 stages.  6th-order
#'   primary with 4th-order embedded error estimate.
#'
#' * `"v65r"` -- Verner "robust" 6(5) FSAL pair, 9 stages.  Robust
#'   variant of Verner's 6(5) family with wider stability region.
#'
#' * `"v65"` -- Verner 6(5) pair, 8 stages (non-FSAL).
#'
#' * `"dverk65"` -- Verner DVERK 6(5) pair, 8 stages.  Coefficients from
#'   the classic DVERK Fortran code distributed by Hull and Enright.
#'
#' * `"tf65"` -- Tsitouras-Famelis 6(5) FSAL pair, 9 stages.
#'
#' * `"tp75"` -- Tsitouras-Papakostas 7(5) pair, 9 stages.  7th-order
#'   primary with 5th-order embedded error estimate.
#'
#' * `"tmy7"` -- Tanaka-Muramatsu-Yamashita 7th-order pair, 10 stages.
#'   The same family as Julia's `TanYam7()`.
#'
#' * `"tmy7s"` -- Tanaka-Muramatsu-Yamashita 7th-order stable variant,
#'   10 stages.  Alternative coefficient set with wider stability region.
#'
#' * `"v76r"` -- Verner "robust" 7(6) pair, 10 stages.
#'
#' * `"ss76"` -- Sharp-Smart 7(6) pair, 11 stages.
#'
#' * `"v78"` -- Verner 7(8) pair, 13 stages.  7th-order primary with
#'   8th-order embedded error estimate.  Closest rxode2 analog to Julia
#'   `Vern8()`.
#'
#' * `"dverk78"` -- Verner DVERK 7(8) pair, 13 stages.  Coefficients from
#'   the classic DVERK Fortran code; companion to `"dverk65"`.  Also close
#'   to Julia `Vern8()`.
#'
#' * `"dp85"` -- Dormand-Prince 8(5) pair, 12 stages.  8th-order primary
#'   with 5th-order embedded error estimate.
#'
#' * `"tp86"` -- Tsitouras-Papakostas 8(6) pair, 12 stages.  The same
#'   family as Julia's `TsitPap8()`.
#'
#' * `"v87e"` -- Verner "efficient" 8(7) pair, 13 stages.
#'
#' * `"v87r"` -- Verner "robust" 8(7) pair, 13 stages.
#'
#' * `"ev87"` -- Enright-Verner 8(7) pair, 13 stages.
#'
#' * `"k87"` -- Kovalnogov-Fedorov-Karpukhina-Simos 8(7) pair, 13 stages.
#'
#' * `"f89"` -- Fehlberg 8(9) pair, 17 stages.  8th-order primary with
#'   9th-order embedded estimate.  Same family as MATLAB `ode89`.
#'
#' * `"v89"` -- Verner 8(9) pair, 16 stages.  Alternative to `"f89"`
#'   in the same order bracket.  Also close to MATLAB `ode89`.
#'
#' * `"t98a"` -- Tsitouras 9(8) variant A pair, 16 stages.
#'
#' * `"v98r"` -- Verner "robust" 9(8) pair, 16 stages.
#'
#' * `"s98"` -- Sharp 9(8) pair, 16 stages.
#'
#' * `"f108"` -- Feagin 10(8) pair, 17 stages (Feagin 2007).  10th-order
#'   primary with 8th-order embedded error estimate.  The same method as
#'   Julia's `Feagin10()`.  The large number of stages carries elevated
#'   transcription-error risk; verified against Williams' libode canonical order test.
#'
#' * `"c108"` -- Curtis 10(8) pair, 21 stages.
#'
#' * `"b109"` -- Baker 10(9) pair, 21 stages.
#'
#' * `"s1110a"` -- Stone 11(10) variant A pair, 26 stages.  11th-order
#'   primary; very few published references.
#'
#' * `"f1210"` -- Feagin 12(10) pair, 25 stages (Feagin 2007).  12th-order
#'   primary with 10th-order embedded error estimate.  The same method as
#'   Julia's `Feagin12()`.  Elevated transcription-error risk due to many
#'   stages; verified against Williams' libode canonical order test.
#'
#' * `"o129"` -- Ono 12(9) pair, 29 stages.
#'
#' * `"f1412"` -- Feagin 14(12) pair, 35 stages (Feagin 2007).  14th-order
#'   primary with 12th-order embedded error estimate.  The same method as
#'   Julia's `Feagin14()`.  The highest-order method in rxode2; elevated
#'   transcription-error risk due to 35 stages; verified against
#'   Williams' libode canonical order test.
#'
#' **deSolve-derived Fortran solvers** (from the deSolve R package).  Both
#' use non-reentrant Fortran COMMON blocks and therefore run single-threaded
#' only, regardless of the `cores` setting.  They are BDF (Backward
#' Differentiation Formula) stiff solvers and are most useful when you want
#' behavior comparable to deSolve's `lsode()` or `bdf()`/`vode()` functions.
#'
#' * `"lsode"` -- DLSODE (Hindmarsh 1983, ODEPACK) in Adams (non-stiff) mode,
#'   MF=10.  Variable-order Adams method, order 1-12; no Jacobian evaluation.
#'   Corresponds to deSolve's `lsode(method="adams")`.  Adaptive step-size;
#'   error controlled by `atol` and `rtol`.
#'   **Not thread-safe** -- always runs on a single core.
#'
#' * `"bdf"` -- DLSODE (Hindmarsh 1983, ODEPACK) in BDF (stiff) mode, MF=22.
#'   Variable-order BDF method, order 1-5; internally generated dense Jacobian.
#'   Corresponds to deSolve's `bdf()` / `vode(method="bdf")`.  Adaptive
#'   step-size; error controlled by `atol` and `rtol`.
#'   **Not thread-safe** -- always runs on a single core.
#'
#' @keywords Internal
#'
#' @return An integer for the method (unless the input is NULL, in which case,
#'   see the details)
#'
#' @export
odeMethodToInt <- function(method = c("liblsoda", "lsoda", "dop853", "indLin", "f78", "rk4", "ck54", "ab", "abm", "dop5", "bs", "ros4", "iem", "sem", "sb3a", "sb3am4", "vv", "mm", "em", "cvode", "trapz", "ssp3", "f32", "rk43", "dop54", "vern65", "vern76", "dop87", "vern98", "ros43", "ros6", "backwardEuler", "gauss6", "iiic6", "radauiia5", "geng5", "sdirk43", "euler", "midpoint", "heun", "ssp22", "rk3", "ssp53", "s4", "r4", "ls44", "ls54", "ssp54", "s5", "rk5", "c5", "l5", "lk5a", "lk5b", "b6", "s7", "s8_10", "cv8", "s8_12", "s10", "z10", "o10", "h10", "dp54", "v65e", "v76e", "dp87", "v98e", "ssp33", "bs32", "ssp43", "f45", "t54", "s54", "pp54", "pp54b", "bs54", "ss54", "dp65", "c65", "tp64", "v65r", "v65", "dverk65", "tf65", "tp75", "tmy7", "tmy7s", "v76r", "ss76", "v78", "dverk78", "dp85", "tp86", "v87e", "v87r", "ev87", "k87", "f89", "v89", "t98a", "v98r", "s98", "f108", "c108", "b109", "s1110a", "f1210", "o129", "f1412", "lsode", "bdf", "rk4s", "eulers", "midpoints", "heuns", "dop5s", "dop853s", "ck54s", "bs32s", "vern65s", "vern76s", "dop87s", "f78s", "ros4s", "radauiia5s", "backwardEulers", "gauss6s", "sdirk43s", "iiic6s", "ros43s", "ros6s", "geng5s", "rk3s", "rk43s")) {
  .methodIdx <- c("lsoda" = 1L, "dop853" = 0L, "liblsoda" = 2L, "indLin" = 3L, "f78" = 5L, "rk4" = 6L, "ck54" = 7L, "ab" = 8L, "abm" = 9L, "dop5" = 10L, "bs" = 11L, "ros4" = 13L, "iem" = 14L, "sem" = 15L, "sb3a" = 16L, "sb3am4" = 17L, "vv" = 18L, "mm" = 19L, "em" = 20L, "cvode" = 21L, "trapz" = 22L, "ssp3" = 23L, "f32" = 24L, "rk43" = 25L, "dop54" = 26L, "vern65" = 27L, "vern76" = 28L, "dop87" = 29L, "vern98" = 30L, "ros43" = 31L, "ros6" = 32L, "backwardEuler" = 33L, "gauss6" = 34L, "iiic6" = 35L, "radauiia5" = 36L, "geng5" = 37L, "sdirk43" = 38L,
                  "euler" = 39L, "midpoint" = 40L, "heun" = 41L, "ssp22" = 42L,
                  "rk3" = 43L, "ssp53" = 44L, "s4" = 45L, "r4" = 46L,
                  "ls44" = 47L, "ls54" = 48L, "ssp54" = 49L,
                  "s5" = 50L, "rk5" = 51L, "c5" = 52L, "l5" = 53L,
                  "lk5a" = 54L, "lk5b" = 55L, "b6" = 56L, "s7" = 57L,
                  "s8_10" = 58L, "cv8" = 59L, "s8_12" = 60L, "s10" = 61L,
                  "z10" = 62L, "o10" = 63L, "h10" = 64L,
                  "dp54" = 26L, "v65e" = 27L,
                  "v76e" = 28L, "dp87" = 29L, "v98e" = 30L, "ssp33" = 23L,
                  "bs32" = 65L, "ssp43" = 66L, "f45" = 67L,
                  "t54" = 68L, "s54" = 69L, "pp54" = 70L, "pp54b" = 71L,
                  "bs54" = 72L, "ss54" = 73L, "dp65" = 74L, "c65" = 75L,
                  "tp64" = 76L, "v65r" = 77L, "v65" = 78L, "dverk65" = 79L,
                  "tf65" = 80L, "tp75" = 81L, "tmy7" = 82L, "tmy7s" = 83L,
                  "v76r" = 84L, "ss76" = 85L, "v78" = 86L, "dverk78" = 87L,
                  "dp85" = 88L, "tp86" = 89L, "v87e" = 90L, "v87r" = 91L,
                  "ev87" = 92L, "k87" = 93L, "f89" = 94L, "v89" = 95L,
                  "t98a" = 96L, "v98r" = 97L, "s98" = 98L, "f108" = 99L,
                  "c108" = 100L, "b109" = 101L, "s1110a" = 102L,
                  "f1210" = 103L, "o129" = 104L, "f1412" = 105L,
                  "lsode" = 106L, "bdf" = 107L, "rk4s" = 206L, "eulers" = 239L, "midpoints" = 240L, "heuns" = 241L, "dop5s" = 210L, "dop853s" = 200L, "ck54s" = 207L, "bs32s" = 265L, "vern65s" = 227L, "vern76s" = 228L, "dop87s" = 229L, "f78s" = 205L, "ros4s" = 213L, "radauiia5s" = 236L, "backwardEulers" = 233L, "gauss6s" = 234L, "sdirk43s" = 238L, "iiic6s" = 235L, "ros43s" = 231L, "ros6s" = 232L, "geng5s" = 237L, "rk3s" = 243L, "rk43s" = 225L)

  if (missing(method) && grepl("SunOS", Sys.info()["sysname"])) {
    method <- 1L
  } else if (is.null(method)) {
    method <- .methodIdx
  } else if (checkmate::testIntegerish(method)) {
    method <- as.integer(method)
  } else if (is.character(method) && length(method) == 1L && grepl("+", method, fixed = TRUE)) {
    method <- .parseAutoSwitchMethod(method)
  } else {
    method <- .methodIdx[match.arg(method)]
  }
  method
}

#' Check whether an ODE solving method requires a Jacobian (is implicit)
#'
#' Implicit ODE solvers linearise the system at each step and therefore
#' require the Jacobian df/dy.  rxode2 auto-computes a symbolic Jacobian
#' when one of these methods is requested and falls back to `"liblsoda"` if
#' symbolic differentiation fails.  Use this function to test whether a
#' method string or integer code corresponds to an implicit solver.
#'
#' Implicit methods are:
#' `"ros4"` (13), `"iem"` (14), `"ros43"` (31), `"ros6"` (32),
#' `"backwardEuler"` (33), `"gauss6"` (34), `"iiic6"` (35), `"radauiia5"` (36),
#' `"geng5"` (37), `"sdirk43"` (38).
#'
#' @param method A character vector of method names or an integerish vector of
#'   method codes (as returned by [odeMethodToInt()]).  Vectorised.
#'
#' @return A logical vector the same length as `method`.  `TRUE` if the
#'   corresponding method is implicit and requires a Jacobian.
#'
#' @examples
#' rxIsImplicit("ros4")              # TRUE
#' rxIsImplicit("liblsoda")          # FALSE
#' rxIsImplicit(c("ros43", "dop853", "iem"))  # TRUE FALSE TRUE
#' rxIsImplicit(13L)                 # TRUE  (ros4)
#' rxIsImplicit(0L)                  # FALSE (dop853)
#'
#' @seealso [odeMethodToInt()]
#' @export
rxIsImplicit <- function(method) {
  .implicitCodes <- c(13L, 14L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L)
  .methodIdx <- c(
    "lsoda" = 1L, "dop853" = 0L, "liblsoda" = 2L, "indLin" = 3L,
    "f78" = 5L, "rk4" = 6L, "ck54" = 7L, "ab" = 8L, "abm" = 9L,
    "dop5" = 10L, "bs" = 11L, "ros4" = 13L, "iem" = 14L,
    "sem" = 15L, "sb3a" = 16L, "sb3am4" = 17L, "vv" = 18L,
    "mm" = 19L, "em" = 20L, "cvode" = 21L, "trapz" = 22L,
    "ssp3" = 23L, "f32" = 24L, "rk43" = 25L, "dop54" = 26L,
    "vern65" = 27L, "vern76" = 28L, "dop87" = 29L, "vern98" = 30L,
    "ros43" = 31L, "ros6" = 32L, "backwardEuler" = 33L, "gauss6" = 34L,
    "iiic6" = 35L, "radauiia5" = 36L, "geng5" = 37L, "sdirk43" = 38L,
    "euler" = 39L, "midpoint" = 40L, "heun" = 41L, "ssp22" = 42L,
    "rk3" = 43L, "ssp53" = 44L, "s4" = 45L, "r4" = 46L,
    "ls44" = 47L, "ls54" = 48L, "ssp54" = 49L,
    "s5" = 50L, "rk5" = 51L, "c5" = 52L, "l5" = 53L,
    "lk5a" = 54L, "lk5b" = 55L, "b6" = 56L, "s7" = 57L,
    "s8_10" = 58L, "cv8" = 59L, "s8_12" = 60L, "s10" = 61L,
    "z10" = 62L, "o10" = 63L, "h10" = 64L,
    "dp54" = 26L, "v65e" = 27L,
    "v76e" = 28L, "dp87" = 29L, "v98e" = 30L, "ssp33" = 23L,
    "bs32" = 65L, "ssp43" = 66L, "f45" = 67L,
    "t54" = 68L, "s54" = 69L, "pp54" = 70L, "pp54b" = 71L,
    "bs54" = 72L, "ss54" = 73L, "dp65" = 74L, "c65" = 75L,
    "tp64" = 76L, "v65r" = 77L, "v65" = 78L, "dverk65" = 79L,
    "tf65" = 80L, "tp75" = 81L, "tmy7" = 82L, "tmy7s" = 83L,
    "v76r" = 84L, "ss76" = 85L, "v78" = 86L, "dverk78" = 87L,
    "dp85" = 88L, "tp86" = 89L, "v87e" = 90L, "v87r" = 91L,
    "ev87" = 92L, "k87" = 93L, "f89" = 94L, "v89" = 95L,
    "t98a" = 96L, "v98r" = 97L, "s98" = 98L, "f108" = 99L,
    "c108" = 100L, "b109" = 101L, "s1110a" = 102L,
    "f1210" = 103L, "o129" = 104L, "f1412" = 105L,
    "lsode" = 106L, "bdf" = 107L, "rk4s" = 206L, "eulers" = 239L, "midpoints" = 240L, "heuns" = 241L, "dop5s" = 210L, "dop853s" = 200L, "ck54s" = 207L, "bs32s" = 265L, "vern65s" = 227L, "vern76s" = 228L, "dop87s" = 229L, "f78s" = 205L, "ros4s" = 213L, "radauiia5s" = 236L, "backwardEulers" = 233L, "gauss6s" = 234L, "sdirk43s" = 238L, "iiic6s" = 235L, "ros43s" = 231L, "ros6s" = 232L, "geng5s" = 237L, "rk3s" = 243L, "rk43s" = 225L
  )
  if (is.character(method)) {
    .composite <- rxIsAutoSwitch(method)
    .codes <- ifelse(.composite, NA_integer_, .methodIdx[method])
    .unknown <- is.na(.codes) & !.composite
    if (any(.unknown)) {
      stop("unknown method(s): ",
           paste(method[.unknown], collapse = ", "),
           call. = FALSE)
    }
    return(ifelse(.composite, FALSE, .codes %in% .implicitCodes))
  } else {
    method <- as.integer(method)
  }
  method %in% .implicitCodes
}

#' Check whether an ODE solving method is a purely stiff solver
#'
#' Returns `TRUE` for methods that are designed exclusively for stiff systems
#' and will never switch to a non-stiff algorithm.  Solvers like `"lsoda"` and
#' `"liblsoda"` that automatically switch between stiff and non-stiff algorithms
#' return `FALSE`.
#'
#' Stiff-only methods are:
#' `"ros4"` (13), `"iem"` (14), `"cvode"` (21), `"ros43"` (31), `"ros6"` (32),
#' `"backwardEuler"` (33), `"gauss6"` (34), `"iiic6"` (35), `"radauiia5"` (36),
#' `"geng5"` (37), `"sdirk43"` (38), `"bdf"` (107).
#'
#' @param method A character vector of method names or an integerish vector of
#'   method codes (as returned by [odeMethodToInt()]).  Vectorised.
#'
#' @return A logical vector the same length as `method`.  `TRUE` if the
#'   corresponding method is a purely stiff solver.
#'
#' @examples
#' rxIsStiff("bdf")                           # TRUE
#' rxIsStiff("lsoda")                         # FALSE (switches)
#' rxIsStiff("liblsoda")                      # FALSE (switches)
#' rxIsStiff(c("cvode", "dop853", "ros43"))   # TRUE FALSE TRUE
#'
#' @seealso [rxIsNonStiff()], [rxIsImplicit()], [odeMethodToInt()]
#' @export
rxIsStiff <- function(method) {
  .stiffCodes <- c(13L, 14L, 21L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 107L, 213L, 236L, 233L, 234L, 238L, 235L, 231L, 232L, 237L)
  .methodIdx <- c(
    "lsoda" = 1L, "dop853" = 0L, "liblsoda" = 2L, "indLin" = 3L,
    "f78" = 5L, "rk4" = 6L, "ck54" = 7L, "ab" = 8L, "abm" = 9L,
    "dop5" = 10L, "bs" = 11L, "ros4" = 13L, "iem" = 14L,
    "sem" = 15L, "sb3a" = 16L, "sb3am4" = 17L, "vv" = 18L,
    "mm" = 19L, "em" = 20L, "cvode" = 21L, "trapz" = 22L,
    "ssp3" = 23L, "f32" = 24L, "rk43" = 25L, "dop54" = 26L,
    "vern65" = 27L, "vern76" = 28L, "dop87" = 29L, "vern98" = 30L,
    "ros43" = 31L, "ros6" = 32L, "backwardEuler" = 33L, "gauss6" = 34L,
    "iiic6" = 35L, "radauiia5" = 36L, "geng5" = 37L, "sdirk43" = 38L,
    "euler" = 39L, "midpoint" = 40L, "heun" = 41L, "ssp22" = 42L,
    "rk3" = 43L, "ssp53" = 44L, "s4" = 45L, "r4" = 46L,
    "ls44" = 47L, "ls54" = 48L, "ssp54" = 49L,
    "s5" = 50L, "rk5" = 51L, "c5" = 52L, "l5" = 53L,
    "lk5a" = 54L, "lk5b" = 55L, "b6" = 56L, "s7" = 57L,
    "s8_10" = 58L, "cv8" = 59L, "s8_12" = 60L, "s10" = 61L,
    "z10" = 62L, "o10" = 63L, "h10" = 64L,
    "dp54" = 26L, "v65e" = 27L,
    "v76e" = 28L, "dp87" = 29L, "v98e" = 30L, "ssp33" = 23L,
    "bs32" = 65L, "ssp43" = 66L, "f45" = 67L,
    "t54" = 68L, "s54" = 69L, "pp54" = 70L, "pp54b" = 71L,
    "bs54" = 72L, "ss54" = 73L, "dp65" = 74L, "c65" = 75L,
    "tp64" = 76L, "v65r" = 77L, "v65" = 78L, "dverk65" = 79L,
    "tf65" = 80L, "tp75" = 81L, "tmy7" = 82L, "tmy7s" = 83L,
    "v76r" = 84L, "ss76" = 85L, "v78" = 86L, "dverk78" = 87L,
    "dp85" = 88L, "tp86" = 89L, "v87e" = 90L, "v87r" = 91L,
    "ev87" = 92L, "k87" = 93L, "f89" = 94L, "v89" = 95L,
    "t98a" = 96L, "v98r" = 97L, "s98" = 98L, "f108" = 99L,
    "c108" = 100L, "b109" = 101L, "s1110a" = 102L,
    "f1210" = 103L, "o129" = 104L, "f1412" = 105L,
    "lsode" = 106L, "bdf" = 107L, "rk4s" = 206L, "eulers" = 239L, "midpoints" = 240L, "heuns" = 241L, "dop5s" = 210L, "dop853s" = 200L, "ck54s" = 207L, "bs32s" = 265L, "vern65s" = 227L, "vern76s" = 228L, "dop87s" = 229L, "f78s" = 205L, "ros4s" = 213L, "radauiia5s" = 236L, "backwardEulers" = 233L, "gauss6s" = 234L, "sdirk43s" = 238L, "iiic6s" = 235L, "ros43s" = 231L, "ros6s" = 232L, "geng5s" = 237L, "rk3s" = 243L, "rk43s" = 225L
  )
  if (is.character(method)) {
    .composite <- rxIsAutoSwitch(method)
    .codes <- ifelse(.composite, NA_integer_, .methodIdx[method])
    .unknown <- is.na(.codes) & !.composite
    if (any(.unknown)) {
      stop("unknown method(s): ", paste(method[.unknown], collapse = ", "), call. = FALSE)
    }
    return(ifelse(.composite, FALSE, .codes %in% .stiffCodes))
  } else {
    method <- as.integer(method)
  }
  method %in% .stiffCodes
}

#' Check whether an ODE solving method is a purely non-stiff solver
#'
#' Returns `TRUE` for methods that are designed exclusively for non-stiff
#' systems.  Solvers like `"lsoda"` and `"liblsoda"` that automatically switch
#' between stiff and non-stiff algorithms return `FALSE`, as do all stiff-only
#' methods.
#'
#' Switchers (`"lsoda"` = 1, `"liblsoda"` = 2) and the inductive linearisation
#' solver (`"indLin"` = 3) are excluded from both [rxIsStiff()] and
#' `rxIsNonStiff()`.
#'
#' @param method A character vector of method names or an integerish vector of
#'   method codes (as returned by [odeMethodToInt()]).  Vectorised.
#'
#' @return A logical vector the same length as `method`.  `TRUE` if the
#'   corresponding method is a purely non-stiff solver.
#'
#' @examples
#' rxIsNonStiff("dop853")                        # TRUE
#' rxIsNonStiff("lsoda")                         # FALSE (switches)
#' rxIsNonStiff("bdf")                           # FALSE (stiff-only)
#' rxIsNonStiff(c("lsode", "cvode", "bs32"))     # TRUE FALSE TRUE
#'
#' @seealso [rxIsStiff()], [rxIsImplicit()], [odeMethodToInt()]
#' @export
rxIsNonStiff <- function(method) {
  .switcherCodes <- c(1L, 2L, 3L)
  .stiffCodes    <- c(13L, 14L, 21L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 107L, 213L, 236L, 233L, 234L, 238L, 235L, 231L, 232L, 237L)
  .methodIdx <- c(
    "lsoda" = 1L, "dop853" = 0L, "liblsoda" = 2L, "indLin" = 3L,
    "f78" = 5L, "rk4" = 6L, "ck54" = 7L, "ab" = 8L, "abm" = 9L,
    "dop5" = 10L, "bs" = 11L, "ros4" = 13L, "iem" = 14L,
    "sem" = 15L, "sb3a" = 16L, "sb3am4" = 17L, "vv" = 18L,
    "mm" = 19L, "em" = 20L, "cvode" = 21L, "trapz" = 22L,
    "ssp3" = 23L, "f32" = 24L, "rk43" = 25L, "dop54" = 26L,
    "vern65" = 27L, "vern76" = 28L, "dop87" = 29L, "vern98" = 30L,
    "ros43" = 31L, "ros6" = 32L, "backwardEuler" = 33L, "gauss6" = 34L,
    "iiic6" = 35L, "radauiia5" = 36L, "geng5" = 37L, "sdirk43" = 38L,
    "euler" = 39L, "midpoint" = 40L, "heun" = 41L, "ssp22" = 42L,
    "rk3" = 43L, "ssp53" = 44L, "s4" = 45L, "r4" = 46L,
    "ls44" = 47L, "ls54" = 48L, "ssp54" = 49L,
    "s5" = 50L, "rk5" = 51L, "c5" = 52L, "l5" = 53L,
    "lk5a" = 54L, "lk5b" = 55L, "b6" = 56L, "s7" = 57L,
    "s8_10" = 58L, "cv8" = 59L, "s8_12" = 60L, "s10" = 61L,
    "z10" = 62L, "o10" = 63L, "h10" = 64L,
    "dp54" = 26L, "v65e" = 27L,
    "v76e" = 28L, "dp87" = 29L, "v98e" = 30L, "ssp33" = 23L,
    "bs32" = 65L, "ssp43" = 66L, "f45" = 67L,
    "t54" = 68L, "s54" = 69L, "pp54" = 70L, "pp54b" = 71L,
    "bs54" = 72L, "ss54" = 73L, "dp65" = 74L, "c65" = 75L,
    "tp64" = 76L, "v65r" = 77L, "v65" = 78L, "dverk65" = 79L,
    "tf65" = 80L, "tp75" = 81L, "tmy7" = 82L, "tmy7s" = 83L,
    "v76r" = 84L, "ss76" = 85L, "v78" = 86L, "dverk78" = 87L,
    "dp85" = 88L, "tp86" = 89L, "v87e" = 90L, "v87r" = 91L,
    "ev87" = 92L, "k87" = 93L, "f89" = 94L, "v89" = 95L,
    "t98a" = 96L, "v98r" = 97L, "s98" = 98L, "f108" = 99L,
    "c108" = 100L, "b109" = 101L, "s1110a" = 102L,
    "f1210" = 103L, "o129" = 104L, "f1412" = 105L,
    "lsode" = 106L, "bdf" = 107L, "rk4s" = 206L, "eulers" = 239L, "midpoints" = 240L, "heuns" = 241L, "dop5s" = 210L, "dop853s" = 200L, "ck54s" = 207L, "bs32s" = 265L, "vern65s" = 227L, "vern76s" = 228L, "dop87s" = 229L, "f78s" = 205L, "ros4s" = 213L, "radauiia5s" = 236L, "backwardEulers" = 233L, "gauss6s" = 234L, "sdirk43s" = 238L, "iiic6s" = 235L, "ros43s" = 231L, "ros6s" = 232L, "geng5s" = 237L, "rk3s" = 243L, "rk43s" = 225L
  )
  if (is.character(method)) {
    .composite <- rxIsAutoSwitch(method)
    .codes <- ifelse(.composite, NA_integer_, .methodIdx[method])
    .unknown <- is.na(.codes) & !.composite
    if (any(.unknown)) {
      stop("unknown method(s): ", paste(method[.unknown], collapse = ", "), call. = FALSE)
    }
    return(ifelse(.composite, FALSE, !.codes %in% c(.switcherCodes, .stiffCodes)))
  } else {
    method <- as.integer(method)
  }
  !method %in% c(.switcherCodes, .stiffCodes)
}

#' Check whether an ODE solving method supports dense output
#'
#' Dense output (continuous interpolation) allows the solver to reconstruct
#' the solution at any point within a completed step without extra function
#' evaluations.  This lets the solver take large internal steps that span many
#' requested output times, then interpolate cheaply -- improving both speed and
#' accuracy on densely sampled grids.
#'
#' Dense-capable single methods are:
#' `"dop853"` (0), `"dop5"` (10), `"bs"` (11), `"ros4"` (13).
#'
#' For composite AutoSwitch methods (e.g. `"dop5+ros4"`), `TRUE` is returned
#' only when **both** the primary and stiff secondary are dense-capable.
#' `"ros4"` is the only stiff method with dense support, so the valid dense
#' composites are `"dop853+ros4"`, `"dop5+ros4"`, and `"bs+ros4"`.
#'
#' @param method A character vector of method names or an integerish vector of
#'   method codes (as returned by [odeMethodToInt()]).  Vectorised.
#'
#' @return A logical vector the same length as `method`.  `TRUE` if the
#'   corresponding method supports dense output.
#'
#' @examples
#' rxIsDense("dop853")                       # TRUE
#' rxIsDense("ros4")                         # TRUE
#' rxIsDense("liblsoda")                     # FALSE
#' rxIsDense(c("dop5", "bs", "cvode"))       # TRUE TRUE FALSE
#' rxIsDense("dop5+ros4")                    # TRUE  (both dense)
#' rxIsDense("dop5+ros43")                   # FALSE (ros43 not dense)
#'
#' @seealso [rxIsStiff()], [rxIsNonStiff()], [odeMethodToInt()]
#' @export
rxIsDense <- function(method) {
  .denseCodes <- c(0L, 10L, 11L, 13L)
  .methodIdx <- c(
    "lsoda" = 1L, "dop853" = 0L, "liblsoda" = 2L, "indLin" = 3L,
    "f78" = 5L, "rk4" = 6L, "ck54" = 7L, "ab" = 8L, "abm" = 9L,
    "dop5" = 10L, "bs" = 11L, "ros4" = 13L, "iem" = 14L,
    "sem" = 15L, "sb3a" = 16L, "sb3am4" = 17L, "vv" = 18L,
    "mm" = 19L, "em" = 20L, "cvode" = 21L, "trapz" = 22L,
    "ssp3" = 23L, "f32" = 24L, "rk43" = 25L, "dop54" = 26L,
    "vern65" = 27L, "vern76" = 28L, "dop87" = 29L, "vern98" = 30L,
    "ros43" = 31L, "ros6" = 32L, "backwardEuler" = 33L, "gauss6" = 34L,
    "iiic6" = 35L, "radauiia5" = 36L, "geng5" = 37L, "sdirk43" = 38L,
    "euler" = 39L, "midpoint" = 40L, "heun" = 41L, "ssp22" = 42L,
    "rk3" = 43L, "ssp53" = 44L, "s4" = 45L, "r4" = 46L,
    "ls44" = 47L, "ls54" = 48L, "ssp54" = 49L,
    "s5" = 50L, "rk5" = 51L, "c5" = 52L, "l5" = 53L,
    "lk5a" = 54L, "lk5b" = 55L, "b6" = 56L, "s7" = 57L,
    "s8_10" = 58L, "cv8" = 59L, "s8_12" = 60L, "s10" = 61L,
    "z10" = 62L, "o10" = 63L, "h10" = 64L,
    "dp54" = 26L, "v65e" = 27L,
    "v76e" = 28L, "dp87" = 29L, "v98e" = 30L, "ssp33" = 23L,
    "bs32" = 65L, "ssp43" = 66L, "f45" = 67L,
    "t54" = 68L, "s54" = 69L, "pp54" = 70L, "pp54b" = 71L,
    "bs54" = 72L, "ss54" = 73L, "dp65" = 74L, "c65" = 75L,
    "tp64" = 76L, "v65r" = 77L, "v65" = 78L, "dverk65" = 79L,
    "tf65" = 80L, "tp75" = 81L, "tmy7" = 82L, "tmy7s" = 83L,
    "v76r" = 84L, "ss76" = 85L, "v78" = 86L, "dverk78" = 87L,
    "dp85" = 88L, "tp86" = 89L, "v87e" = 90L, "v87r" = 91L,
    "ev87" = 92L, "k87" = 93L, "f89" = 94L, "v89" = 95L,
    "t98a" = 96L, "v98r" = 97L, "s98" = 98L, "f108" = 99L,
    "c108" = 100L, "b109" = 101L, "s1110a" = 102L,
    "f1210" = 103L, "o129" = 104L, "f1412" = 105L,
    "lsode" = 106L, "bdf" = 107L, "rk4s" = 206L, "eulers" = 239L, "midpoints" = 240L, "heuns" = 241L, "dop5s" = 210L, "dop853s" = 200L, "ck54s" = 207L, "bs32s" = 265L, "vern65s" = 227L, "vern76s" = 228L, "dop87s" = 229L, "f78s" = 205L, "ros4s" = 213L, "radauiia5s" = 236L, "backwardEulers" = 233L, "gauss6s" = 234L, "sdirk43s" = 238L, "iiic6s" = 235L, "ros43s" = 231L, "ros6s" = 232L, "geng5s" = 237L, "rk3s" = 243L, "rk43s" = 225L
  )
  if (is.character(method)) {
    .composite <- rxIsAutoSwitch(method)
    .codes <- ifelse(.composite, NA_integer_, .methodIdx[method])
    .unknown <- is.na(.codes) & !.composite
    if (any(.unknown)) {
      stop("unknown method(s): ", paste(method[.unknown], collapse = ", "), call. = FALSE)
    }
    .isDense <- ifelse(.composite, FALSE, .codes %in% .denseCodes)
    if (any(.composite)) {
      .isDense[.composite] <- vapply(method[.composite], function(.m) {
        .parts <- .parseAutoSwitchMethod(.m)
        !is.null(.parts) &&
          as.integer(.parts["primary"]) %in% .denseCodes &&
          as.integer(.parts["stiff"]) %in% .denseCodes
      }, logical(1))
    }
    return(.isDense)
  } else {
    method <- as.integer(method)
  }
  method %in% .denseCodes
}

#' Check whether an ODE method code is thread-safe for parallel solving
#'
#' @param code Integer method code as returned by [odeMethodToInt()].
#' @return `TRUE` if safe to use in parallel OpenMP loops.
#' @noRd
.rxIsThreadSafeMethod <- function(code) {
  code <- as.integer(code)
  !(code %in% c(1L, 2L, 106L, 107L))
}

#' Parse a composite AutoSwitch method string of the form "primary+stiff"
#'
#' @param method Character scalar like `"dop853+ros43"`.
#' @return Named integer vector `c(primary=..., stiff=...)` or `NULL` if not composite.
#' @noRd
.parseAutoSwitchMethod <- function(method) {
  if (!grepl("+", method, fixed = TRUE)) return(NULL)
  .parts <- trimws(strsplit(method, "+", fixed = TRUE)[[1]])
  if (length(.parts) != 2L) {
    stop("AutoSwitch method must be 'primary+stiff', got: '", method, "'", call. = FALSE)
  }
  .code1 <- tryCatch(odeMethodToInt(.parts[1]), error = function(e) {
    stop("AutoSwitch: unknown primary method '", .parts[1], "'", call. = FALSE)
  })
  .code2 <- tryCatch(odeMethodToInt(.parts[2]), error = function(e) {
    stop("AutoSwitch: unknown stiff method '", .parts[2], "'", call. = FALSE)
  })
  if (!rxIsNonStiff(.code1)) {
    stop("AutoSwitch primary '", .parts[1], "' must be a non-stiff method", call. = FALSE)
  }
  if (!.rxIsThreadSafeMethod(.code1)) {
    stop("AutoSwitch primary '", .parts[1], "' is not thread-safe", call. = FALSE)
  }
  if (!rxIsStiff(.code2)) {
    stop("AutoSwitch stiff secondary '", .parts[2], "' must be a stiff method", call. = FALSE)
  }
  if (!.rxIsThreadSafeMethod(.code2)) {
    stop("AutoSwitch stiff secondary '", .parts[2], "' is not thread-safe", call. = FALSE)
  }
  if (.code2 == 21L) {
    stop("AutoSwitch: CVODE ('cvode') as stiff secondary is not yet supported in composite methods",
         call. = FALSE)
  }
  c(primary = as.integer(.code1), stiff = as.integer(.code2))
}

#' Check whether an ODE method string is an AutoSwitch composite pair
#'
#' Returns `TRUE` for strings containing `"+"`, such as `"dop853+ros43"`.
#'
#' @param method Character vector of method names.
#' @return Logical vector the same length as `method`.
#'
#' @examples
#' rxIsAutoSwitch("dop853+ros43")  # TRUE
#' rxIsAutoSwitch("dop853")        # FALSE
#' rxIsAutoSwitch(c("dop853+ros43", "liblsoda"))  # TRUE FALSE
#'
#' @seealso [odeMethodToInt()], [rxIsNonStiff()], [rxIsStiff()]
#' @export
rxIsAutoSwitch <- function(method) {
  if (is.character(method)) return(grepl("+", method, fixed = TRUE))
  rep(FALSE, length(method))
}

#' This updates the tolerances based on the sensitivity equations
#'
#' This assumes the normal ODE equations are the first equations and
#' the ODE is expanded by the forward sensitivities or other type of
#' sensitivity (like adjoint)
#'
#' @param rxControl Input list or rxControl type of list
#' @param sensCmt Number of sensitivity compartments
#' @param ncmt Number of compartments
#' @return Updated rxControl where `$atol`, `$rtol`, `$ssAtol`
#'   `$ssRtol` are updated with different sensitivities for the normal
#'   ODEs (first) and a different sensitivity for the larger
#'   compartments (sensitivities).
#' @author Matthew L. Fidler
#' @export
#' @examples
#'
#' tmp <- rxControl()
#'
#' tmp2 <- rxControlUpdateSens(tmp, 3, 6)
#'
#' tmp2$atol
#' tmp2$rtol
#' tmp2$ssAtol
#' tmp2$ssRtol
rxControlUpdateSens <- function(rxControl, sensCmt=NULL, ncmt=NULL) {
  checkmate::assertIntegerish(sensCmt, lower=1, len=1)
  checkmate::assertIntegerish(ncmt, lower=2, len=1)
  if (sensCmt >= ncmt) {
    stop("'sensCmt' must be lower than the number of compartments 'ncmt'",
         call.=FALSE)
  }
  if (is.list(rxControl) && !inherits(rxControl, "rxControl")) {
    rxControl <- do.call(rxode2::rxControl, rxControl)
  }
  if (!inherits(rxControl, "rxControl")) {
    stop("'rxControl' must be a rxode2 control options list",
         call.=FALSE)
  }
  rxControl$atol <- c(rep(rxControl$atol[1], ncmt - sensCmt), rep(rxControl$atolSens, sensCmt))
  rxControl$rtol <- c(rep(rxControl$rtol[1], ncmt - sensCmt), rep(rxControl$rtolSens, sensCmt))
  rxControl$ssAtol <- c(rep(rxControl$ssAtol[1], ncmt - sensCmt), rep(rxControl$ssAtolSens, sensCmt))
  rxControl$ssRtol <- c(rep(rxControl$ssRtol[1], ncmt - sensCmt), rep(rxControl$ssRtolSens, sensCmt))
  rxControl
}


#' rxUiDeparse.rxControl(rxControl(covsInterpolation="linear", method="dop853",
#'  naInterpolation="nocb", keepInterpolation="nocb", sigmaXform="variance",
#'  omegaXform="variance", returnType="data.frame", sumType="fsum", prodType="logify"),
#' "ctl")

#' @rdname rxUiDeparse
#' @export
rxUiDeparse.rxControl <- function(object, var) {
  .ret <- rxControl()

  .w <- which(vapply(names(.ret), function(x) {
    if (is.integer(.ret[[x]]) && is.integer(object[[x]])) {
      .ret[[x]] != object[[x]]
    } else {
      !identical(.ret[[x]], object[[x]])
    }
  }, logical(1)))

  .retD <- vapply(names(.ret)[.w], function(x) {
    if (x == "covsInterpolation") {
      .covsInterpolation <- c("linear"=0L, "locf"=1L, "nocb"=2L, "midpoint"=3L)
      paste0(x, " =", deparse1(names(.covsInterpolation)[which(object[[x]] == .covsInterpolation)]))
    } else if (x == "method")  {
      .methodIdx <- c("lsoda" = 1L, "dop853" = 0L, "liblsoda" = 2L, "indLin" = 3L, "f78" = 5L, "rk4" = 6L, "ros4" = 13L, "iem" = 14L, "trapz" = 22L, "ssp3" = 23L, "f32" = 24L, "rk43" = 25L, "dop54" = 26L, "vern65" = 27L, "vern76" = 28L, "dop87" = 29L, "vern98" = 30L, "ros43" = 31L, "ros6" = 32L, "backwardEuler" = 33L, "gauss6" = 34L, "iiic6" = 35L, "radauiia5" = 36L, "geng5" = 37L, "sdirk43" = 38L)
      paste0(x, " =", deparse1(names(.methodIdx)[which(object[[x]] == .methodIdx)]))
    } else if (x == "naInterpolation") {
      .naInterpolation <- c("locf"=1L, "nocb"=0L)
      paste0(x, " =", deparse1(names(.naInterpolation)[which(object[[x]] == .naInterpolation)]))
    } else if (x == "keepInterpolation") {
      .keepInterpolation <- c("locf"=1L, "nocb"=0L, "na"=2L)
      paste0(x, " =", deparse1(names(.keepInterpolation)[which(object[[x]] == .keepInterpolation)]))
    } else if (x %in% c("sigmaXform", "omegaXform")) {
      .sigmaXform <- c(
        "variance" = 6L, "log" = 5L, "identity" = 4L,
        "nlmixrSqrt" = 1L, "nlmixrLog" = 2L,
        "nlmixrIdentity" = 3L)
      paste0(x, " =", deparse1(names(.sigmaXform)[which(object[[x]] == .sigmaXform)]))
    } else if (x == "returnType") {
      .matrixIdx <- c(
        "rxSolve" = 0L, "matrix" = 1L, "data.frame" = 2L, "data.frame.TBS" = 3L, "data.table" = 4L,
        "tbl" = 5L, "tibble" = 5L)
      paste0(x, " =", deparse1(names(.matrixIdx)[which(object[[x]] == .matrixIdx)]))
    } else if (x == "sumType") {
      .sum <- c("pairwise"=1L, "fsum"=2L, "kahan"=3L , "neumaier"=4L, "c"=5L)
      paste0(x, " = ", deparse1(names(.sum)[which(object[[x]] == .sum)]))
    } else if (x == "prodType") {
      .prod <- c("long double"=1L, "double"=1L, "logify"=1L)
      paste0(x, " = ", deparse1(names(.prod)[which(object[[x]] == .prod)]))
    } else if (x == "naTimeHandle") {
      .naTimeHandle <- c("ignore"=1L, "warn"=2L, "error"=3L)
      paste0(x, " = ", deparse1(names(.naTimeHandle)[which(object[[x]] == .naTimeHandle)]))
    }  else {
      paste0(x, "=", deparse1(object[[x]]))
    }
  }, character(1), USE.NAMES=FALSE)
  str2lang(paste(var, " <- rxControl(", paste(.retD, collapse=","),")"))
}
