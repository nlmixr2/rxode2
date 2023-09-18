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
#' @param hmax The maximum absolute step size allowed.  When
#'   `hmax=NA` (default), uses the average difference +
#'   hmaxSd*sd in times and sampling events. The `hmaxSd` is a user
#'   specified parameter and which defaults to zero.  When
#'   `hmax=NULL` rxode2 uses the maximum difference in times in
#'   your sampling and events.  The value 0 is equivalent to infinite
#'   maximum absolute step size.
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
#' @param stateTrim When amounts/concentrations in one of the states
#'     are above this value, trim them to be this value. By default
#'     Inf.  Also trims to -stateTrim for large negative
#'     amounts/concentrations.  If you want to trim between a range
#'     say `c(0, 2000000)` you may specify 2 values with a lower and
#'     upper range to make sure all state values are in the
#'     reasonable range.
#'
#' @param safeZero Use safe zero divide and log routines.  By default
#'     this is turned on but you may turn it off if you wish.
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
#' @param sensType Sensitivity type for `linCmt()` model:
#'
#' `advan` Use the direct advan solutions
#'
#' `autodiff` Use the autodiff advan solutions
#'
#' `forward` Use forward difference solutions
#'
#' `central` Use central differences
#'
#' @param linDiff This gives the linear difference amount for all the
#'   types of linear compartment model parameters where sensitivities
#'   are not calculated. The named components of this numeric vector are:
#'
#' * `"lag"` Central compartment lag
#' * `"f"` Central compartment bioavailability
#' * `"rate"` Central compartment modeled rate
#' * `"dur"` Central compartment modeled duration
#' * `"lag2"` Depot compartment lag
#' * `"f2"` Depot compartment bioavailability
#' * `"rate2"` Depot compartment modeled rate
#' * `"dur2"` Depot compartment modeled duration
#'
#' @param linDiffCentral This gives the which parameters use central
#'   differences for the linear compartment model parameters.  The
#'   are the same components as `linDiff`
#'
#' @param iCov A data frame of individual non-time varying covariates
#'     to combine with the `events` dataset by merge.
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
#' * `"constant"` -- Last observation carried forward (the default).
#'
#' * `"NOCB"` -- Next Observation Carried Backward.  This is the same method
#'       that NONMEM uses.
#'
#' * `"midpoint"` Last observation carried forward to midpoint; Next observation
#'   carried backward to midpoint.
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
#'     compartment.  This includes S# = numeric will scale a compartment
#'     # by a dividing the compartment amount by the scale factor,
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
                    scale = NULL, method = c("liblsoda", "lsoda", "dop853", "indLin"),
                    sigdig=NULL,
                    atol = 1.0e-8, rtol = 1.0e-6,
                    maxsteps = 70000L, hmin = 0, hmax = NA_real_,
                    hmaxSd = 0, hini = 0, maxordn = 12L, maxords = 5L, ...,
                    cores,
                    covsInterpolation = c("locf", "linear", "nocb", "midpoint"),
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
                    nStud = 1L, dfSub = 0.0, dfObs = 0.0, returnType = c("rxSolve", "matrix", "data.frame", "data.frame.TBS", "data.table", "tbl", "tibble"),
                    seed = NULL, nsim = NULL,
                    minSS = 10L, maxSS = 1000L,
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
                    sumType = c("pairwise", "fsum", "kahan", "neumaier", "c"),
                    prodType = c("long double", "double", "logify"),
                    sensType = c("advan", "autodiff", "forward", "central"),
                    linDiff = c(tlag = 1.5e-5, f = 1.5e-5, rate = 1.5e-5, dur = 1.5e-5, tlag2 = 1.5e-5, f2 = 1.5e-5, rate2 = 1.5e-5, dur2 = 1.5e-5),
                    linDiffCentral = c(tlag = TRUE, f = TRUE, rate = TRUE, dur = TRUE, tlag2 = TRUE, f2 = TRUE, rate2 = TRUE, dur2 = TRUE),
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
                    ss2cancelAllPending=FALSE) {
  if (is.null(object)) {
    .xtra <- list(...)
    .nxtra <- names(.xtra)
    .w <- which(regexpr("^[Ss][0-9]+$", .nxtra) != -1)
    if (length(.w) > 0) {
      for (.arg in .w) {
        checkmate::assertNumeric(.xtra[[.arg]], lower=0, finite=TRUE, len=1, .var.name=.nxtra[.arg])
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
    if (checkmate::testIntegerish(sigmaXform, len=1L, lower=1L, upper=6L, any.missing=FALSE)) {
      .sigmaXform <- as.integer(sigmaXform)
    } else {
      .sigmaXform <- c(
        "variance" = 6L, "log" = 5L, "identity" = 4L,
        "nlmixrSqrt" = 1L, "nlmixrLog" = 2L,
        "nlmixrIdentity" = 3L
      )[match.arg(sigmaXform)]
    }
    if (checkmate::testIntegerish(omegaXform, len=1L, lower=1L, upper=6L, any.missing=FALSE)) {
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
      if (rxIs(params, "eventTable") || rxIs(events, "eventTable") && nSub == 1L) {
        nSub <- as.integer(nsim)
      } else if (nStud == 1L) {
        nStud <- as.integer(nsim)
      }
    }
    method <- odeMethodToInt(method)
    if (checkmate::testIntegerish(returnType, len=1, lower=0, upper=5, any.missing=FALSE)) {
      returnType <- as.integer(returnType)
    } else {
      .matrixIdx <- c(
        "rxSolve" = 0L, "matrix" = 1L, "data.frame" = 2L, "data.frame.TBS" = 3L, "data.table" = 4L,
        "tbl" = 5L, "tibble" = 5L)
      returnType <- .matrixIdx[match.arg(returnType)]
    }
    if (checkmate::testIntegerish(covsInterpolation, len=1, lower=0, upper=3, any.missing=FALSE)) {
      covsInterpolation <- as.integer(covsInterpolation)
    } else {
      covsInterpolation <- c("linear"=0L, "locf"=1L, "nocb"=2L, "midpoint"=3L)[match.arg(covsInterpolation)]
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
      stop("covariates can no longer be specified by 'covs' include them in the event dataset",
           .call = FALSE
           )
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
      checkmate::assertNumeric(dfObs, lower=length(sigma), finite=TRUE, any.missing=FALSE, len=1)
    } else {
      .sigma <- lotri(sigma)
    }
    if (inherits(omega, "logical")) {
      .omega <- omega
    }  else if (inherits(omega, "character")) {
      .omega <- omega
      checkmate::testNumeric(dfSub, lower=length(omega), finite=TRUE, any.missing=FALSE, len=1)
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
    if (checkmate::testIntegerish(sumType, len=1, lower=1, upper=5, any.missing=FALSE)) {
      .sum <- as.integer(sumType)
    } else {
      .sum <- c("pairwise"=1L, "fsum"=2L, "kahan"=3L , "neumaier"=4L, "c"=5L)[match.arg(sumType)]
    }
    if (checkmate::testIntegerish(prodType, len=1, lower=1, upper=3, any.missing=FALSE)) {
      .prod <- as.integer(prodType)
    } else {
      .prod <- c("long double"=1L, "double"=1L, "logify"=1L)[match.arg(prodType)]
    }

    if (checkmate::testIntegerish(sensType, len=1, lower=1, upper=4, any.missing=FALSE)) {
      .sensType <- as.integer(sensType)
    } else {
      .sensType <- c("autodiff"=1L, "forward"=2L, "central"=3L, "advan"=4L)[match.arg(sensType)]
    }
    if (checkmate::testIntegerish(strictSS, len=1, lower=0, upper=1, any.missing=FALSE)) {
      strictSS <- as.integer(strictSS)
    } else {
      checkmate::assertLogical(strictSS, any.missing=FALSE, len=1)
      strictSS <- as.integer(strictSS)
    }
    checkmate::assertIntegerish(indLinMatExpOrder, len=1, lower=1, any.missing=FALSE)
    indLinMatExpOrder <- as.integer(indLinMatExpOrder)
    if (!checkmate::testIntegerish(safeZero, lower=0, upper=1, len=1, any.missing=FALSE)) {
      checkmate::assertLogical(safeZero, len=1, any.missing=FALSE)
    }
    safeZero <- as.integer(safeZero)
    if (is.null(scale)) {
    } else if (is.list(scale)) {
      checkmate::assertList(scale, types="double", any.missing=FALSE,names="strict")
      lapply(names(scale), function(n) {
        checkmate::assertNumeric(scale[[n]], lower=0, finite=TRUE, any.missing=FALSE, len=1, .var.name=n)
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
    checkmate::assertNumeric(hmax, lower=0, any.missing=TRUE, null.ok=TRUE, finite=TRUE, len=1)
    checkmate::assertNumeric(hmaxSd, lower=0, any.missing=FALSE, null.ok=FALSE, finite=TRUE, len=1)
    checkmate::assertNumeric(hini, lower=0, any.missing=FALSE, null.ok=FALSE, finite=TRUE, len=1)
    checkmate::assertIntegerish(maxordn, lower=1, upper=12, any.missing=FALSE, len=1)
    maxordn <- as.integer(maxordn)
    checkmate::assertIntegerish(maxords, lower=1, upper=5, any.missing=FALSE, len=1)
    maxods <- as.integer(maxords)
    checkmate::assertIntegerish(mxhnil, lower=0, any.missing=FALSE, len=1)
    mxhnil <- as.integer(mxhnil)
    checkmate::assertIntegerish(hmxi, lower=0, any.missing=FALSE, len=1)
    checkmate::assertLogical(istateReset, any.missing=TRUE, len=1)
    checkmate::assertLogical(simVariability, len=1)
    checkmate::assertNumeric(indLinPhiTol, lower=0, any.missing=FALSE, len=1)
    checkmate::assertIntegerish(indLinPhiM, lower=0L, any.missing=FALSE, len=1)
    indLinPhiM <- as.integer(indLinPhiM)
    checkmate::assertIntegerish(minSS, lower=5L, any.missing=FALSE, len=1)
    checkmate::assertIntegerish(maxSS, lower=7L, any.missing=FALSE, len=1)
    if (maxSS <= minSS) stop("'maxSS' must be larger than 'minSS'", call.=FALSE)
    checkmate::assertNumeric(infSSstep, lower=6, any.missing=FALSE, len=1)
    checkmate::assertNumeric(maxAtolRtolFactor, lower=0.01, any.missing=FALSE, finite=TRUE, null.ok=FALSE, len=1)
    checkmate::assertNumeric(from, null.ok=TRUE, finite=TRUE, any.missing=FALSE, len=1)
    checkmate::assertNumeric(to, null.ok=TRUE, finite=TRUE, any.missing=FALSE, len=1)
    checkmate::assertNumeric(by, null.ok=TRUE, finite=TRUE, any.missing=FALSE, len=1)
    checkmate::assertIntegerish(length.out, lower=0, any.missing=FALSE, null.ok=TRUE, len=1)
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
    checkmate::assertCharacter(keep, any.missing=FALSE, null.ok=TRUE)
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
    checkmate::assertNumeric(linDiff, names="strict", len=8)
    checkmate::assertLogical(linDiffCentral, names="strict", len=8, any.missing=FALSE)
    if (is.null(resample)) {
    } else if (checkmate::testLogical(resample)) {
      checkmate::assertLogical(resample, any.missing=FALSE, len=1)
    } else {
      checkmate::assertCharacter(resample, min.len=1, any.missing=FALSE, unique=TRUE)
    }
    checkmate::assertLogical(resampleID, null.ok=FALSE, any.missing=FALSE, len=1)
    checkmate::assertIntegerish(maxwhile, lower=20, len=1)
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
    useStdPow <- as.integer(useStdPow)
    maxwhile <- as.integer(maxwhile)
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
      sensType = .sensType,
      linDiff = linDiff, #
      linDiffCentral = linDiffCentral,#
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
                             theta = NULL, eta = NULL) {
  .object <- rxode2(object)
  do.call("rxSolve", c(list(object=.object, params = params, events = events, inits = inits),
                       list(...),
                       list(theta = theta, eta = eta)))
}

.rxSolveFromUi <- function(object, params = NULL, events = NULL, inits = NULL, ...,
                           theta = NULL, eta = NULL) {
  .rxControl <- rxSolve(NULL, params = params, events = events, inits = inits, ...,
                        theta = theta, eta = eta)
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
                         theta = NULL, eta = NULL) {
  if (inherits(object, "rxUi")) {
    object <- rxUiDecompress(object)
  }
  .lst <- .rxSolveFromUi(object, params = params, events = events, inits = inits, ..., theta = theta, eta = eta)
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
                                   theta = NULL, eta = NULL) {
  .lst <- .rxSolveFromUi(object, params = params, events = events, inits = inits, ..., theta = theta, eta = eta)
  .rxControl <- .lst[[2]]
  .env <- object$env
  # assign current control to object for expanded thetaMat
  if (exists("control", envir=.env)) {
    .oldControl <- get("control", envir=.env)
    assign("control", .rxControl, envir=.env)
    on.exit({
      assign("control", .oldControl, envir=.env)
    })
  } else {
    assign("control", .rxControl, envir=.env)
    on.exit({
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

#' @rdname rxSolve
#' @export
rxSolve.default <- function(object, params = NULL, events = NULL, inits = NULL, ...,
                            theta = NULL, eta = NULL) {
  if (inherits(object, "rxSolve") && is.null(.v$env$.ev)) {
    stop("cannot solve from more than one rxSolve object bound together", call.=FALSE)
  }
  on.exit({
    .clearPipe()
    .asFunctionEnv$rx <- NULL
  })
  .applyParams <- FALSE
  .rxParams <- NULL
  if (rxIs(object, "rxEt")) {
    if (!is.null(events)) {
      stop("events can be pipeline or solving arguments not both",
           call. = FALSE
           )
    }
    if (is.null(rxode2et::.pipeRx(NA))) {
      stop("need an rxode2 compiled model as the start of the pipeline",
           call. = FALSE
           )
    } else {
      events <- object
      object <- rxode2et::.pipeRx(NA)
    }
  } else if (rxIs(object, "rxParams")) {
    .applyParams <- TRUE
    if (is.null(params) && !is.null(object$params)) {
      params <- object$params
    }
    if (is.null(rxode2et::.pipeRx(NA))) {
      stop("need an rxode2 compiled model as the start of the pipeline",
           call. = FALSE
           )
    } else {
      .rxParams <- object
      object <- rxode2et::.pipeRx(NA)
    }
    if (is.null(rxode2et::.pipeEvents(NA))) {
      stop("need an rxode2 events as a part of the pipeline",
           call. = FALSE
           )
    } else {
      events <- rxode2et::.pipeEvents(NA)
      rxode2et::.pipeEvents(NULL)
    }
  }
  if (!is.null(rxode2et::.pipeEvents(NA)) && is.null(events) && is.null(params)) {
    events <- rxode2et::.pipeEvents(NA)
  } else if (!is.null(rxode2et::.pipeEvents(NA)) && !is.null(events)) {
    stop("'events' in pipeline AND in solving arguments, please provide just one",
         call. = FALSE
         )
  } else if (!is.null(rxode2et::.pipeEvents(NA)) && !is.null(params) &&
               rxIs(params, "event.data.frame")) {
    stop("'events' in pipeline AND in solving arguments, please provide just one",
         call. = FALSE
         )
  }

  if (!is.null(rxode2et::.pipeParams(NA)) && is.null(params)) {
    params <- rxode2et::.pipeParams(NA)
  } else if (!is.null(rxode2et::.pipeParams(NA)) && !is.null(params)) {
    stop("'params' in pipeline AND in solving arguments, please provide just one",
         call. = FALSE
         )
  }

  if (!is.null(rxode2et::.pipeInits(NA)) && is.null(inits)) {
    inits <- rxode2et::.pipeInits(NA)
  } else if (!is.null(rxode2et::.pipeInits(NA)) && !is.null(inits)) {
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
  .ctl <- rxControl(..., events = events, params = params)
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
    ), call. = FALSE)
  }
  if (!is.null(rxode2et::.pipeThetaMat(NA)) && is.null(.ctl$thetaMat)) {
    .ctl$thetaMat <- rxode2et::.pipeThetaMat(NA)
  }
  if (!is.null(rxode2et::.pipeOmega(NA)) && is.null(.ctl$omega)) {
    .ctl$omega <- rxode2et::.pipeOmega(NA)
  }
  if (!is.null(rxode2et::.pipeSigma(NA)) && is.null(.ctl$sigma)) {
    .ctl$sigma <- rxode2et::.pipeSigma(NA)
  }
  if (!is.null(rxode2et::.pipeSigma(NA)) && is.null(.ctl$sigma)) {
    .ctl$sigma <- rxode2et::.pipeSigma(NA)
  }
  if (!is.null(rxode2et::.pipeDfObs(NA)) && .ctl$dfObs == 0) {
    .ctl$dfObs <- rxode2et::.pipeDfObs(NA)
  }
  if (!is.null(rxode2et::.pipeDfSub(NA)) && .ctl$dfSub == 0) {
    .ctl$dfSub <- rxode2et::.pipeDfSub(NA)
  }
  if (!is.null(rxode2et::.pipeNSub(NA)) && .ctl$nSub == 1) {
    .ctl$nSub <- rxode2et::.pipeNSub(NA)
  }
  if (!is.null(rxode2et::.pipeNStud(NA)) && .ctl$nStud == 1) {
    .ctl$nStud <- rxode2et::.pipeNStud(NA)
  }
  if (!is.null(rxode2et::.pipeKeep(NA)) && is.null(.ctl$keep)) {
    .ctl$keep <- rxode2et::.pipeKeep(NA)
  }
  if (.applyParams) {
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
  if (!is.null(.ctl$iCov)) {
    if (inherits(.ctl$iCov, "data.frame")) {
      .icovId <- which(tolower(names(.ctl$iCov)) == "id")
      .useEvents <- FALSE
      if (rxIs(events, "event.data.frame")) {
        .events <- events
        .useEvents <- TRUE
      } else if (rxIs(params, "event.data.frame")) {
        .events <- params
      } else {
        stop("Cannot detect an event data frame to merge 'iCov'")
      }
      .events <- as.data.frame(.events)
      .eventId <- which(tolower(names(.events)) == "id")
      if (length(.eventId) != 1) {
        stop("to use 'iCov' you must have an id in your event table")
      }
      .by <- names(.events)[.eventId]
      if (length(.icovId) == 0) {
        .id <- unique(events[[.by]])
        if (length(.ctl$iCov[, 1]) != length(.id)) {
          stop("'iCov' and 'id' mismatch")
        }
        .ctl$iCov$id <- .id
      } else if (length(.icovId) > 1) {
        stop("iCov has duplicate IDs, cannot continue")
      }
      names(.ctl$iCov)[.icovId] <- .by
      .lEvents <- length(.events[, 1])
      .events <- merge(.events, .ctl$iCov, by = .by)
      if (.lEvents != length(.events[, 1])) {
        warning("combining iCov and events dropped some event information")
      }
      if (length(unique(.events[[.by]])) != length(.ctl$iCov[, 1])) {
        warning("combining iCov and events dropped some iCov information")
      }
      if (.useEvents) {
        events <- .events
      } else {
        params <- .events
      }
    } else {
      stop("'iCov' must be an input dataset")
    }
  }
  if (rxode2.debug) {
    .rx <- rxNorm(object)
    qs::qsave(list(.rx, .ctl, .nms, .xtra, params, events, inits, .setupOnly), file.path(rxTempDir(), "last-rxode2.qs"))
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
  if (inherits(.ctl$thetaMat, "matrix")) {
    .mv <- rxModelVars(object)
    .col <- colnames(.ctl$thetaMat)
    .w <- .col %in% .mv$params
    .ignore <- .col[!.w]
    if (length(.ignore)>0) {
      .minfo(paste0("thetaMat has too many items, ignored: '", paste(.ignore, collapse="', '"), "'"))
    }
    .names <- c(.names, .col[.w])
  }
  if (inherits(.ctl$omega, "matrix")) {
    .mv <- rxModelVars(object)
    .col <- colnames(.ctl$omega)
    .w <- .col %in% .mv$params
    .ignore <- .col[!.w]
    if (length(.ignore)>0) {
      .minfo(paste0("omega has too many items, ignored: '", paste(.ignore, collapse="', '"), "'"))
    }
    .names <- c(.names, .col[.w])
  } else if ( inherits(.ctl$omega, "character")) {
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
    .names <- c(.names, .col[.w])
  } else if ( inherits(.ctl$sigma, "character")) {
    .mv <- rxModelVars(object)
    .col <- .ctl$sigma
    .w <- .col %in% .mv$params
    .ignore <- .col[!.w]
    if (length(.ignore)>0) {
      .minfo(paste0("sigma has too many items, ignored: '", paste(.ignore, collapse="', '"), "'"))
    }
    .names <- c(.names, .col[.w])
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
  if (rxode2.debug) {
    .envReset$ret <- .collectWarnings(rxSolveSEXP(object, .ctl, .nms, .xtra,
                                                  params, events, inits,
                                                  setupOnlyS = .setupOnly
                                                  ), lst = TRUE)
  } else {
    while (.envReset$reset) {
      .envReset$reset <- FALSE
      tryCatch({
        .envReset$ret <- .collectWarnings(rxSolveSEXP(object, .ctl, .nms, .xtra,
                                                      params, events, inits,
                                                      setupOnlyS = .setupOnly
                                                      ), lst = TRUE)
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
            try(rxode2::rxUnloadAll())
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

#' @export
`$.rxSolve` <- function(obj, arg, exact = FALSE) {
  if (arg == "rxModelVars") return(rxModelVars(obj))
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
rxControl <- function(..., params = NULL, events = NULL, inits = NULL) {
  rxSolve(object = NULL, params = params, events = events, inits = inits, ...)
}

#' @export
rxEtDispatchSolve.rxode2et <- function(x, ...) {
  .lst <- x
  class(.lst) <- NULL
  do.call(rxSolve, .lst)
}

.getEtRxSolve <- function(x) {
  .Call(`_rxode2_getEtRxsolve`, x)
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
#' * `"lsoda"` -- LSODA solver.  Does not support parallel thread-based
#'       solving, but allows user Jacobian specification.
#' * `"dop853"` -- DOP853 solver.  Does not support parallel thread-based
#'         solving nor user Jacobian specification
#' * `"indLin"` -- Solving through inductive linearization.  The rxode2 dll
#'         must be setup specially to use this solving routine.
#' @keywords Internal
#' @return An integer for the method (unless the input is NULL, in which case,
#'   see the details)
#' @export
odeMethodToInt <- function(method = c("liblsoda", "lsoda", "dop853", "indLin")) {
  .methodIdx <- c("lsoda" = 1L, "dop853" = 0L, "liblsoda" = 2L, "indLin" = 3L)
  if (missing(method) && grepl("SunOS", Sys.info()["sysname"])) {
    method <- 1L
  } else if (is.null(method)) {
    method <- .methodIdx
  } else if (checkmate::testIntegerish(method)) {
    method <- as.integer(method)
  } else {
    method <- .methodIdx[match.arg(method)]
  }
  method
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
