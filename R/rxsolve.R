#' Solving & Simulation of a ODE/solved system (and solving options) equation
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
#' @param method The method for solving ODEs.  Currently this supports:
#'
#' * `"liblsoda"` thread safe lsoda.  This supports parallel
#'            thread-based solving, and ignores user Jacobian specification.
#' * `"lsoda"` -- LSODA solver.  Does not support parallel thread-based
#'       solving, but allows user Jacobian specification.
#' * `"dop853"` -- DOP853 solver.  Does not support parallel thread-based
#'         solving nor user Jacobain specification
#' * `"indLin"` -- Solving through inductive linearization.  The rxode2 dll
#'         must be setup specially to use this solving routine.
#'
#' @param stiff a logical (`TRUE` by default) indicating whether
#'     the ODE system is stiff or not.
#'
#'     For stiff ODE systems (`stiff = TRUE`), `rxode2` uses the
#'     LSODA (Livermore Solver for Ordinary Differential Equations)
#'     Fortran package, which implements an automatic method switching
#'     for stiff and non-stiff problems along the integration
#'     interval, authored by Hindmarsh and Petzold (2003).
#'
#'     For non-stiff systems (`stiff = FALSE`), `rxode2` uses
#'     DOP853, an explicit Runge-Kutta method of order 8(5, 3) of
#'     Dormand and Prince as implemented in C by Hairer and Wanner
#'     (1993).
#'
#'     If stiff is not specified, the `method` argument is used instead.
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
#'     420.
#'
#' @param ssAtol Steady state atol convergence factor.  Can be
#'     a vector based on each state.
#'
#' @param ssRtol Steady state rtol convergence factor.  Can be a
#'     vector based on each state.
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
#' @param transitAbs boolean indicating if this is a transit
#'     compartment absorption
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
#'     assume it is a block matrix and convert it to a full matrix
#'     for simulations.
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
#'     with the `x^2` modeled along the diagonal.  This only works
#'      with a diagonal matrix.
#'
#'   - `nlmixrLog` This is when the `params` and
#'     `thetaMat` simulates the inverse cholesky decomposed matrix
#'      with the `exp(x^2)` along the diagonal.  This only works
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
#'     observation in the solved system.
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
#'     with the `x^2` modeled along the diagonal.  This only works
#'      with a diagonal matrix.
#'
#'   - `nlmixrLog` This is when the `params` and
#'     `thetaMat` simulates the inverse cholesky decomposed matrix
#'      with the `exp(x^2)` along the diagonal.  This only works
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
#' @param matrix A boolean indicating if a matrix should be returned
#'     instead of the rxode2's solved object.
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
                    transitAbs = NULL, atol = 1.0e-8, rtol = 1.0e-6,
                    maxsteps = 70000L, hmin = 0, hmax = NA_real_,
                    hmaxSd = 0, hini = 0, maxordn = 12L, maxords = 5L, ...,
                    cores,
                    covsInterpolation = c("locf", "linear", "nocb", "midpoint"),
                    addCov = FALSE, matrix = FALSE, sigma = NULL, sigmaDf = NULL,
                    sigmaLower = -Inf, sigmaUpper = Inf,
                    nCoresRV = 1L, sigmaIsChol = FALSE,
                    sigmaSeparation = c("auto", "lkj", "separation"),
                    sigmaXform = c("identity", "variance", "log", "nlmixrSqrt", "nlmixrLog", "nlmixrIdentity"),
                    nDisplayProgress = 10000L,
                    amountUnits = NA_character_, timeUnits = "hours", stiff,
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
                    maxwhile = 100000) {
  if (is.null(object)) {
    .xtra <- list(...)
    if (inherits(sigmaXform, "numeric") || inherits(sigmaXform, "integer")) {
      .sigmaXform <- as.integer(sigmaXform)
    } else {
      .sigmaXform <- as.vector(c(
        "variance" = 6, "log" = 5, "identity" = 4,
        "nlmixrSqrt" = 1, "nlmixrLog" = 2,
        "nlmixrIdentity" = 3
      )[match.arg(sigmaXform)])
    }
    if (inherits(omegaXform, "numeric") || inherits(omegaXform, "integer")) {
      .omegaXform <- as.integer(omegaXform)
    } else {
      .omegaXform <- as.vector(c(
        "variance" = 6, "log" = 5,
        "identity" = 4, "nlmixrSqrt" = 1,
        "nlmixrLog" = 2,
        "nlmixrIdentity" = 3
      )[match.arg(omegaXform)])
    }

    if (is.null(transitAbs) && !is.null(.xtra$transit_abs)) {
      transitAbs <- .xtra$transit_abs
    }
    if (missing(updateObject) && !is.null(.xtra$update.object)) {
      updateObject <- .xtra$update.object
    }
    if (missing(covsInterpolation) && !is.null(.xtra$covs_interpolation)) {
      covsInterpolation <- .xtra$covs_interpolation
    }
    if (missing(addCov) && !is.null(.xtra$add.cov)) {
      addCov <- .xtra$add.cov
    }
    if (!is.null(seed)) {
      set.seed(seed)
    }
    if (!is.null(nsim)) {
      if (rxIs(params, "eventTable") || rxIs(events, "eventTable") && nSub == 1L) {
        nSub <- nsim
      } else if (nStud == 1L) {
        nStud <- nsim
      }
    }
    ## stiff = TRUE, transitAbs = NULL,
    ## atol = 1.0e-8, rtol = 1.0e-6, maxsteps = 5000, hmin = 0, hmax = NULL, hini = 0, maxordn = 12,
    ## maxords = 5, ..., covsInterpolation = c("linear", "constant", "NOCB", "midpoint"),
    ## theta=numeric(), eta=numeric(), matrix=TRUE,addCov=FALSE,
    ## inC=FALSE, counts=NULL, doSolve=TRUE
    if (!missing(stiff) && missing(method)) {
      if (rxIs(stiff, "logical")) {
        if (stiff) {
          method <- "lsoda"
          .Deprecated("method = \"lsoda\"", old = "stiff=TRUE")
        } else {
          method <- "dop853"
          .Deprecated("method = \"dop853\"", old = "stiff=FALSE")
        }
      }
    } else if (missing(method) && grepl("SunOS", Sys.info()["sysname"])) {
      method <- 1L
    } else {
      if (inherits(method, "numeric")) {
        method <- as.integer(method)
      }
      if (!rxIs(method, "integer")) {
        method <- match.arg(method)
      }
    }
    .matrixIdx <- c(
      "rxSolve" = 0, "matrix" = 1, "data.frame" = 2, "data.frame.TBS" = 3, "data.table" = 4,
      "tbl" = 5, "tibble" = 5
    )
    if (!missing(returnType)) {
      matrix <- .matrixIdx[match.arg(returnType)]
    } else if (!is.null(.xtra$return.type)) {
      matrix <- .matrixIdx[.xtra$return.type]
    } else {
      matrix <- as.integer(matrix)
    }
    if (!rxIs(method, "integer")) {
      .methodIdx <- c("lsoda" = 1L, "dop853" = 0L, "liblsoda" = 2L, "indLin" = 3L)
      method <- .methodIdx[method]
    }
    if (length(covsInterpolation) > 1) covsInterpolation <- covsInterpolation[1]
    if (!rxIs(covsInterpolation, "integer")) {
      covsInterpolation <- tolower(match.arg(
        covsInterpolation,
        c(
          "linear", "locf", "LOCF", "constant",
          "nocb", "NOCB", "midpoint"
        )
      ))
      if (covsInterpolation == "constant") covsInterpolation <- "locf"
      covsInterpolation <- as.integer(which(covsInterpolation ==
        c("linear", "locf", "nocb", "midpoint")) - 1)
    }
    if (any(duplicated(names(.xtra)))) {
      stop("duplicate arguments do not make sense", .call = FALSE)
    }
    if (any(names(.xtra) == "covs")) {
      stop("covariates can no longer be specified by 'covs' include them in the event dataset",
        .call = FALSE
      )
    }
    if (missing(cores)) {
      cores <- 0L
    } else if (!missing(cores)) {
      checkmate::assert_integerish(cores, lower = 0L, len = 1)
      cores <- as.integer(cores)
    }
    if (inherits(sigma, "character")) {
      .sigma <- sigma
    } else {
      .sigma <- lotri(sigma)
    }
    if (inherits(omega, "character")) {
      .omega <- omega
    } else if (inherits(omega, "lotri")) {
      .omega <- omega
    } else {
      .omega <- lotri(omega)
    }
    if (inherits(indLinMatExpType, "numeric") ||
      inherits(indLinMatExpType, "integer")) {
      .indLinMatExpType <- as.integer(indLinMatExpType)
    } else {
      .indLinMatExpTypeIdx <- c("Al-Mohy" = 3, "arma" = 1, "expokit" = 2)
      .indLinMatExpType <- match.arg(indLinMatExpType)
      .indLinMatExpType <- as.integer(.indLinMatExpTypeIdx[match.arg(indLinMatExpType)])
    }
    if (inherits(sumType, "numeric") ||
      inherits(sumType, "integer")) {
      .sum <- as.integer(sumType)
    } else {
      .sum <- which(match.arg(sumType) == c("pairwise", "fsum", "kahan", "neumaier", "c"))
    }
    if (inherits(prodType, "numeric") ||
      inherits(prodType, "integer")) {
      .prod <- as.integer(prodType)
    } else {
      .prod <- which(match.arg(prodType) == c("long double", "double", "logify"))
    }

    if (inherits(sensType, "numeric") ||
      inherits(sensType, "integer")) {
      .sensType <- as.integer(sensType)
    } else {
      .sensType <- as.integer(which(match.arg(sensType) == c("autodiff", "forward", "central", "advan")))
    }
    .ret <- list(
      scale = scale,
      method = method,
      transitAbs = transitAbs,
      atol = atol,
      rtol = rtol,
      maxsteps = maxsteps,
      hmin = hmin,
      hmax = hmax,
      hini = hini,
      maxordn = maxordn,
      maxords = maxords,
      covsInterpolation = covsInterpolation,
      addCov = addCov,
      matrix = matrix,
      sigma = .sigma,
      sigmaDf = sigmaDf,
      nCoresRV = nCoresRV,
      sigmaIsChol = sigmaIsChol,
      sigmaSeparation = match.arg(sigmaSeparation),
      sigmaXform = .sigmaXform,
      nDisplayProgress = nDisplayProgress,
      amountUnits = amountUnits,
      timeUnits = timeUnits,
      addDosing = addDosing,
      stateTrim = stateTrim,
      updateObject = updateObject,
      omega = .omega,
      omegaDf = omegaDf,
      omegaIsChol = omegaIsChol,
      omegaSeparation = match.arg(omegaSeparation),
      omegaXform = .omegaXform,
      nSub = nSub,
      thetaMat = thetaMat,
      thetaDf = thetaDf,
      thetaIsChol = thetaIsChol,
      nStud = nStud,
      dfSub = dfSub,
      dfObs = dfObs,
      seed = seed,
      nsim = nsim,
      minSS = minSS, maxSS = maxSS,
      strictSS = as.integer(strictSS),
      infSSstep = as.double(infSSstep),
      istateReset = istateReset,
      subsetNonmem = subsetNonmem,
      hmaxSd = hmaxSd,
      maxAtolRtolFactor = maxAtolRtolFactor,
      from = from,
      to = to,
      by = by,
      length.out = length.out,
      iCov = iCov,
      keep = keep, keepF = character(0), keepI = character(0),
      drop = drop,
      warnDrop = warnDrop,
      omegaLower = omegaLower, omegaUpper = omegaUpper,
      sigmaLower = sigmaLower, sigmaUpper = sigmaUpper,
      thetaLower = thetaLower, thetaUpper = thetaUpper,
      indLinPhiM = indLinPhiM,
      indLinPhiTol = indLinPhiTol,
      indLinMatExpType = .indLinMatExpType,
      indLinMatExpOrder = as.integer(indLinMatExpOrder),
      idFactor = idFactor,
      mxhnil = mxhnil, hmxi = hmxi, warnIdSort = warnIdSort,
      ssAtol = ssAtol, ssRtol = ssRtol, safeZero = as.integer(safeZero),
      sumType = as.integer(.sum),
      prodType = as.integer(.prod),
      sensType = as.integer(.sensType),
      linDiff = linDiff,
      linDiffCentral = linDiffCentral,
      resample = resample,
      resampleID = resampleID,
      maxwhile = maxwhile,
      cores = cores
    )
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
#' @rdname rxSolve
#' @export
rxSolve.rxUi <- function(object, params = NULL, events = NULL, inits = NULL, ...,
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
  }
  if (is.null(.rxControl$thetaLower)) {
    .rxControl$thetaLower <- object$thetaLower
  }
  if (is.null(.rxControl$thetaUpper)) {
    .rxControl$thetaUpper <- object$thetaUpper
  }
  if (is.null(.rxControl$omega)) {
    .rxControl$omega <- object$omega
  }
  if (is.null(.rxControl$sigma)) {
    .rxControl$sigma <- object$simulationSigma
  }
  .rx <- object$simulationModel
  do.call("rxSolve", c(list(object=.rx, params = params, events = events, inits = inits),
                       .rxControl,
                       list(theta = theta, eta = eta)))
}

#' @rdname rxSolve
#' @export
rxSolve.default <- function(object, params = NULL, events = NULL, inits = NULL, ...,
                            theta = NULL, eta = NULL) {
  on.exit({
    .clearPipe()
  })
  .applyParams <- FALSE
  .rxParams <- NULL
  if (rxIs(object, "rxEt")) {
    if (!is.null(events)) {
      stop("events can be pipeline or solving arguments not both",
        call. = FALSE
      )
    }
    if (is.null(.pipelineRx)) {
      stop("need an rxode2 compiled model as the start of the pipeline",
        call. = FALSE
      )
    } else {
      events <- object
      object <- .pipelineRx
    }
  } else if (rxIs(object, "rxParams")) {
    .applyParams <- TRUE
    if (is.null(params) && !is.null(object$params)) {
      params <- object$params
    }
    if (is.null(.pipelineRx)) {
      stop("need an rxode2 compiled model as the start of the pipeline",
        call. = FALSE
      )
    } else {
      .rxParams <- object
      object <- .pipelineRx
    }
    if (is.null(.pipelineEvents)) {
      stop("need an rxode2 events as a part of the pipeline",
        call. = FALSE
      )
    } else {
      events <- .pipelineEvents
      assignInMyNamespace(".pipelineEvents", NULL)
    }
  }
  if (!is.null(.pipelineEvents) && is.null(events) && is.null(params)) {
    events <- .pipelineEvents
  } else if (!is.null(.pipelineEvents) && !is.null(events)) {
    stop("'events' in pipeline AND in solving arguments, please provide just one",
      call. = FALSE
    )
  } else if (!is.null(.pipelineEvents) && !is.null(params) &&
    rxIs(params, "event.data.frame")) {
    stop("'events' in pipeline AND in solving arguments, please provide just one",
      call. = FALSE
    )
  }

  if (!is.null(.pipelineParams) && is.null(params)) {
    params <- .pipelineParams
  } else if (!is.null(.pipelineParams) && !is.null(params)) {
    stop("'params' in pipeline AND in solving arguments, please provide just one",
      call. = FALSE
    )
  }

  if (!is.null(.pipelineInits) && is.null(inits)) {
    inits <- .pipelineInits
  } else if (!is.null(.pipelineInits) && !is.null(inits)) {
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
  .n1 <- setdiff(intersect(tolower(names(params)), tolower(names(.ctl$iCov))), "id")
  .n2 <- c(.n1, setdiff(intersect(tolower(names(events)), tolower(names(.ctl$iCov))), "id"))
  .n1 <- unique(c(.n1, .n2))
  if (length(.n1) > 0) {
    stop(sprintf(
      gettext("'iCov' has information contained in parameters/event data\nduplicate columns: '%s'"),
      paste(.n1, collapse = "', '")
    ), call. = FALSE)
  }
  if (!is.null(.pipelineThetaMat) && is.null(.ctl$thetaMat)) {
    .ctl$thetaMat <- .pipelineThetaMat
  }
  if (!is.null(.pipelineOmega) && is.null(.ctl$omega)) {
    .ctl$omega <- .pipelineOmega
  }
  if (!is.null(.pipelineSigma) && is.null(.ctl$sigma)) {
    .ctl$sigma <- .pipelineSigma
  }
  if (!is.null(.pipelineSigma) && is.null(.ctl$sigma)) {
    .ctl$sigma <- .pipelineSigma
  }
  if (!is.null(.pipelineDfObs) && .ctl$dfObs == 0) {
    .ctl$dfObs <- .pipelineDfObs
  }
  if (!is.null(.pipelineDfSub) && .ctl$dfSub == 0) {
    .ctl$dfSub <- .pipelineDfSub
  }
  if (!is.null(.pipelineNSub) && .ctl$nSub == 1) {
    .ctl$nSub <- .pipelineNSub
  }
  if (!is.null(.pipelineNStud) && .ctl$nStud == 1) {
    .ctl$nStud <- .pipelineNStud
  }
  if (!is.null(.pipelineICov) && is.null(.ctl$iCov)) {
    .ctl$iCov <- .pipelineICov
  }
  if (!is.null(.pipelineKeep) && is.null(.ctl$keep)) {
    .ctl$keep <- .pipelineKeep
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
    if (!is.null(.rxParams$iCov)) {
      if (is.null(.ctl$iCov)) {
        .ctl$iCov <- .rxParams$iCov
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



  if (!is.null(theta) | !is.null(eta)) {
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
    qs::qsave(list(.rx, .ctl, .nms, .xtra, params, events, inits, .setupOnly), "last-rxode2.qs")
  }
  if (!any(class(object) %in% c("rxSolve", "rxode2", "character", "rxModelVars", "rxDll"))) {
    stop("Unsupported type of model trying to be solved")
  }
  .ret <- .collectWarnings(rxSolveSEXP(object, .ctl, .nms, .xtra,
    params, events, inits,
    setupOnlyS = .setupOnly
  ), lst = TRUE)
  .ws <- .ret[[2]]
  .rxModels$.ws <- .ws
  lapply(.ws, function(x) warning(x, call. = FALSE))
  .ret <- .ret[[1]]
  if (.ctl$matrix == 4L) {
    data.table::setDT(.ret)
  } else if (.ctl$matrix == 5L) {
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
