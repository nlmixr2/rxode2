#' Internal messaging statements
#'
#' @param text Text
#' @param ... Other arguments
#' @param .envir Environment to evaluate in
#' @return Nothing, called for side effects
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.minfo <- function(text, ..., .envir = parent.frame()) {
  cli::cli_alert_info(gettext(text), ..., .envir = .envir)
}
#' @rdname dot-minfo
#' @export
.malert <- function(text, ..., .envir = parent.frame()) {
  cli::cli_alert(gettext(text), ..., .envir = .envir)
}
#' @rdname dot-minfo
#' @export
.mwarn <- function(text, ..., .envir = parent.frame()) {
  cli::cli_alert_warning(gettext(text), ..., .envir = .envir)
}
#' @rdname dot-minfo
#' @export
.msuccess <- function(text, ..., .envir = parent.frame()) {
  cli::cli_alert_success(gettext(text), ..., .envir = .envir)
}
#' Internal function to tell if the linCmt() is the model variables
#'
#'
#' @return 0 or 1
#' @author Matthew L. Fidler
#' @keywords internal
#' @export
.rxIsLinCmt <- function() {
  .Call(`_rxode2_isLinCmt`)
}
#'  Internal function to generate the model variables for a linCmt() model
#'
#'
#' @param lenState Length of the state
#' @param vars Variables in the model
#' @return Model variables of expanded linCmt model
#' @author Matthew L. Fidler
#' @export
.rxLinCmtGen <- function(lenState, vars) {
  rxGetModel(.Call(
    `_rxode2_linCmtGen`,
    lenState, vars, 1L, FALSE))
}

.normalizePath <- function(path, ...) {
  ifelse(.Platform$OS.type == "windows",
    suppressWarnings(utils::shortPathName(normalizePath(path, ...))),
    ifelse(regexpr("^[/~]", path) != -1,
      suppressWarnings(normalizePath(path, ...)),
      suppressWarnings(normalizePath(file.path(getwd(), path), ...))
    )
  )
}

#' Use cat when rxode2.verbose is TRUE
#'
#' @param ... Parameters sent to cat
#' @author Matthew L. Fidler
#' @keywords internal
#' @return nothing
#' @export
rxCat <- function(a, ...) {
  ## nocov start
  if (rxode2.verbose) {
    if (is(a, "rxode2")) {
      message(rxode2::rxNorm(a), appendLF = FALSE)
    } else {
      message(a, ..., appendLF = FALSE)
    }
  }
  ## nocov end
}

#' Cleanup anonymous DLLs by unloading them
#'
#' This cleans up any rxode2 loaded DLLs
#'
#' @param wd What directory should be cleaned; (DEPRECIATED), this no
#'     longer does anything.
#'
#' This unloads all rxode2 anonymous dlls.
#'
#' @return TRUE if successful
#'
#' @author Matthew L. Fidler
#' @export
rxClean <- function(wd) {
  if (!missing(wd)) warning("'wd' is depreciated")
  rxUnloadAll()
  unlink(rxTempDir(), recursive = TRUE, force = TRUE)
  suppressMessages(.mkCache(rxTempDir()))
}

#' Defunct setting of sum
#'
#' @param type used to be type of product
#'
#' @return nothing
#'
#' @export
rxSetSum <- function(type = c("pairwise", "fsum", "kahan", "neumaier", "c")) {
  stop("'rxSetSum' has been moved to rxSolve(...,sum=)", call. = FALSE)
}

#' Defunct setting of product
#'
#' @param type used to be type of product
#' @return nothing
#'
#' @export
rxSetProd <- function(type = c("long double", "double", "logify")) {
  stop("'rxSetProd' has been moved to rxSolve(...,sum=)", call. = FALSE)
}

#' Set timing for progress bar
#'
#' @param seconds This sets the number of seconds that need to elapse
#'     before drawing the next segment of the progress bar.  When
#'     this is zero or below this turns off the progress bar.
#'
#' @return nothing, used for side effects
#'
#' @export
#' @author Matthew Fidler
rxSetProgressBar <- function(seconds = 1.0) {
  invisible(.Call(`_rxParProgress`, as.double(seconds)))
}



#' Error function
#'
#'
#' @param x vector or real values
#' @return erf of x
#' @author Matthew L. Fidler
#' @examples
#' erf(1.0)
#' @export
erf <- function(x) {
  checkmate::assertNumeric(x)
  .Call(`_rxode2_rxErf`, x, PACKAGE = "rxode2")
}
#' Gammap: normalized lower incomplete gamma function
#'
#' This is the gamma_p from the boost library
#'
#' @param a The numeric 'a' parameter in the normalized lower
#'   incomplete gamma
#'
#' @param z The numeric 'z' parameter in the normalized lower
#'   incomplete gamma
#'
#' @details
#'
#' The gamma p function is given by:
#'
#' gammap = lowergamma(a, z)/gamma(a)
#'
#' @return gammap results
#' @author Matthew L. Fidler
#' @examples
#'
#' gammap(1, 3)
#' gammap(1:3, 3)
#' gammap(1, 1:3)
#' @export
gammap <- function(a, z) {
  .Call(`_gammap`, a, z, PACKAGE = "rxode2")
}

#' Gammaq: normalized upper incomplete gamma function
#'
#' This is the gamma_q from the boost library
#'
#' @param a The numeric 'a' parameter in the normalized upper
#'   incomplete gamma
#'
#' @param z The numeric 'z' parameter in the normalized upper
#'   incomplete gamma
#'
#' @details
#'
#' The gamma q function is given by:
#'
#' gammaq = uppergamma(a, z)/gamma(a)
#'
#' @return gammaq results
#' @author Matthew L. Fidler
#' @examples
#'
#' gammaq(1, 3)
#' gammaq(1:3, 3)
#' gammaq(1, 1:3)
#' @export
gammaq <- function(a, z) {
  .Call(`_gammaq`, a, z, PACKAGE = "rxode2")
}
#' uppergamma:  upper incomplete gamma function
#'
#' This is the tgamma from the boost library
#'
#' @param a The numeric 'a' parameter in the upper
#'   incomplete gamma
#'
#' @param z The numeric 'z' parameter in the upper
#'   incomplete gamma
#'
#' @details
#'
#' The uppergamma function is given by:
#'
#' \eqn{uppergamma(a, z) = \int_{z}^{\infty}t^{a-1}\cdot e^{-t} dt}
#'
#' @return uppergamma results
#'
#' @author Matthew L. Fidler
#'
#' @examples
#'
#' uppergamma(1, 3)
#'
#' uppergamma(1:3, 3)
#'
#' uppergamma(1, 1:3)
#' @export
uppergamma <- function(a, z) {
  .Call(`_uppergamma`, a, z, PACKAGE = "rxode2")
}

#' lowergamma:  upper incomplete gamma function
#'
#' This is the tgamma_lower from the boost library
#'
#' @param a The numeric 'a' parameter in the upper
#'   incomplete gamma
#'
#' @param z The numeric 'z' parameter in the upper
#'   incomplete gamma
#'
#' @details
#'
#' The lowergamma function is given by:
#'
#' \deqn{lowergamma(a, z) = \int_{0}^{z}t^{a-1}\cdot e^{-t} dt}
#'
#' @return lowergamma results
#'
#' @author Matthew L. Fidler
#'
#' @examples
#'
#' lowergamma(1, 3)
#'
#' lowergamma(1:3, 3)
#'
#' lowergamma(1, 1:3)
#' @export
lowergamma <- function(a, z) {
  .Call(`_lowergamma`, a, z, PACKAGE = "rxode2")
}

#' gammapDer:  derivative of gammap
#'
#' This is the gamma_p_derivative from the boost library
#'
#' @param a The numeric 'a' parameter in the upper
#'   incomplete gamma
#'
#' @param z The numeric 'z' parameter in the upper
#'   incomplete gamma
#'
#' @return lowergamma results
#'
#' @author Matthew L. Fidler
#'
#' @examples
#'
#' gammapDer(1:3, 3)
#'
#' gammapDer(1, 1:3)
#' @export
gammapDer <- function(a, z) {
  .Call(`_gammapDer`, a, z, PACKAGE = "rxode2")
}

#' gammaqInv and gammaqInva:  Inverses of normalized gammaq function
#'
#' @param a The numeric 'a' parameter in the upper
#'   incomplete gamma
#'
#' @param x The numeric 'x' parameter in the upper incomplete gamma
#'
#' @param q The numeric 'q' parameter in the upper
#'   incomplete gamma
#'
#' @details
#'
#' With the equation:
#'
#' q = gammaq(a, x)
#'
#' The 'gammaqInv' function returns a value 'x' that satisfies the
#' equation above
#'
#' The 'gammaqInva' function returns a value 'a' that satisfies the
#' equation above
#'
#' NOTE: gammaqInva is slow
#'
#' @return inverse gammaq results
#'
#' @author Matthew L. Fidler
#'
#' @examples
#'
#' gammaqInv(1:3, 0.5)
#'
#' gammaqInv(1, 1:3 / 3)
#'
#' gammaqInv(1:3, 1:3 / 3.1)
#'
#' gammaqInva(1:3, 1:3 / 3.1)
#' @export
gammaqInv <- function(a, q) {
  .Call(`_gammaqInv`, a, q, PACKAGE = "rxode2")
}

#' @rdname gammaqInv
#' @export
gammaqInva <- function(x, q) {
  .Call(`_gammaqInva`, x, q, PACKAGE = "rxode2")
}


#' gammapInv and gammapInva:  Inverses of normalized gammap function
#'
#' @param a The numeric 'a' parameter in the upper
#'   incomplete gamma
#'
#' @param x The numeric 'x' parameter in the upper incomplete gamma
#'
#' @param p The numeric 'p' parameter in the upper incomplete gamma
#'
#' @details
#'
#' With the equation:
#'
#' p = gammap(a, x)
#'
#' The 'gammapInv' function returns a value 'x' that satisfies the
#' equation above
#'
#' The 'gammapInva' function returns a value 'q' that satisfies the
#' equation above
#'
#' NOTE: gammapInva is slow
#'
#' @return inverse gammap results
#'
#' @author Matthew L. Fidler
#'
#' @examples
#'
#' gammapInv(1:3, 0.5)
#'
#' gammapInv(1, 1:3 / 3.1)
#'
#' gammapInv(1:3, 1:3 / 3.1)
#'
#' gammapInva(1:3, 1:3 / 3.1)
#' @export
gammapInv <- function(a, p) {
  .Call(`_gammapInv`, a, p, PACKAGE = "rxode2")
}

#' @rdname gammapInv
#' @export
gammapInva <- function(x, p) {
  .Call(`_gammapInva`, x, p, PACKAGE = "rxode2")
}

#' logit and inverse logit (expit) functions
#'
#' @param x Input value(s) in range \[low,high\] to translate -Inf to
#'   Inf
#'
#' @param alpha Infinite value(s) to translate to range of \[low,
#'   high\]
#'
#' @param low Lowest value in the range
#'
#' @param high Highest value in the range
#'
#' @param mean logit-scale mean
#'
#' @param sd logit-scale standard deviation
#'
#' @inheritParams stats::integrate
#'
#' @param ... other parameters passed to `integrate()`
#'
#' @return  values from logit and expit
#'
#' @details
#'
#' logit is given by:
#'
#' logit(p) = -log(1/p-1)
#'
#' where:
#'
#' p = x-low/high-low
#'
#'  expit is given by:
#'
#' expit(p, low, high) = (high-low)/(1+exp(-alpha)) + low
#'
#' The `logitNormInfo()` gives the mean, variance and coefficient of
#' variability on the untransformed scale.
#'
#' @examples
#'
#' logit(0.25)
#'
#' expit(-1.09)
#'
#' logitNormInfo(logit(0.25), sd = 0.1)
#'
#' logitNormInfo(logit(1, 0, 10), sd = 1, low = 0, high = 10)
#' @export
logit <- function(x, low = 0, high = 1) {
  .Call(`_logit`, x, low, high, PACKAGE = "rxode2")
}
#' @rdname logit
#' @export
expit <- function(alpha, low = 0, high = 1) {
  .Call(`_expit`, alpha, low, high, PACKAGE = "rxode2")
}

#' @rdname logit
#' @export
logitNormInfo <- function(mean = 0, sd = 1, low = 0, high = 1, abs.tol = 1e-6, ...) {
  .fM1 <- function(x) .Call(`_expit`, x, low, high, PACKAGE = "rxode2") * dnorm(x, mean = mean, sd = sd)
  .m <- integrate(.fM1, -Inf, Inf, abs.tol = abs.tol, ...)$value
  .fV <- function(x) (.Call(`_expit`, x, low, high, PACKAGE = "rxode2") - .m)^2 * dnorm(x, mean = mean, sd = sd)
  .v <- integrate(.fV, -Inf, Inf, abs.tol = abs.tol, ...)$value
  c(mean = .m, var = .v, cv = sqrt(.v) / .m)
}

#' probit and inverse probit functions
#'
#' @inheritParams logit
#' @return values from probit, probitInv and probitNormInfo
#' @examples
#'
#' probit(0.25)
#'
#' probitInv(-0.674)
#'
#' probitNormInfo(probit(0.25), sd = 0.1)
#'
#' probitNormInfo(probit(1, 0, 10), sd = 1, low = 0, high = 10)
#' @export
probit <- function(x, low = 0, high = 1) {
  .Call(`_probit`, x, low, high, PACKAGE = "rxode2")
}

#' @rdname probit
#' @export
probitInv <- function(x, low = 0, high = 1) {
  .Call(`_probitInv`, x, low, high, PACKAGE = "rxode2")
}


#' @rdname logit
#' @export
probitNormInfo <- function(mean = 0, sd = 1, low = 0, high = 1, abs.tol = 1e-6, ...) {
  .fM1 <- function(x) .Call(`_probitInv`, x, low, high, PACKAGE = "rxode2") * dnorm(x, mean = mean, sd = sd)
  .m <- integrate(.fM1, -Inf, Inf, abs.tol = abs.tol, ...)$value
  .fV <- function(x) (.Call(`_probitInv`, x, low, high, PACKAGE = "rxode2") - .m)^2 * dnorm(x, mean = mean, sd = sd)
  .v <- integrate(.fV, -Inf, Inf, abs.tol = abs.tol, ...)$value
  c(mean = .m, var = .v, cv = sqrt(.v) / .m)
}

#' Get/Set the number of threads that rxode2 uses
#'
#' @param threads NULL (default) rereads environment variables. 0
#'   means to use all logical CPUs available. Otherwise a number >= 1
#'
#' @param percent If provided it should be a number between 2 and
#'   100; the percentage of logical CPUs to use. By default on
#'   startup, 50 percent.
#'
#' @param throttle 2 (default) means that, roughly speaking, a
#'   single thread will be used when number subjects solved for is <=2, 2 threads when
#'   the number of all points is <=4, etc. The throttle is to speed up small data
#'   tasks (especially when repeated many times) by not incurring the
#'   overhead of managing multiple threads.
#'
#'   The throttle will also suppress sorting which ID will be solved first
#'   when there are (nsubject solved)*throttle <= nthreads.  In
#'   `rxode2` this sorting occurs to minimize the time for waiting for
#'   another thread to finish. If the last item solved is has a long
#'   solving time, all the other solving have to wait for that last
#'   costly solving to occur. If the items which are likely to take
#'   more time are solved first, this wait is less likely to have an
#'   impact on the overall solving time.
#'
#'   In rxode2 the IDs are sorted by the individual number of solving
#'   points (largest first). It also has a C interface that allows
#'   these IDs to be resorted by total time spent solving the
#'   equation.  This allows packages like nlmixr to sort by solving
#'   time if needed.
#'
#'   Overall the the number of threads is throttled (restricted) for
#'   small tasks and sorting for IDs are suppressed.
#'
#' @param verbose Display the value of relevant OpenMP settings
#' @return number of threads that rxode2 uses
#' @export
getRxThreads <- function(verbose = FALSE) {
  .Call(`getRxThreads_R`, verbose)
}

#' @rdname getRxThreads
#' @export
setRxThreads <- function(threads = NULL, percent = NULL, throttle = NULL) {
  if (!missing(percent)) {
    if (!missing(threads)) stop("provide either threads= or percent= but not both")
    if (length(percent) != 1) stop("percent= is provided but is length ", length(percent))
    percent <- as.integer(percent)
    if (is.na(percent) || percent < 2L || percent > 100L) stop("percent==", percent, " but should be a number between 2 and 100")
    invisible(.Call(`setRxthreads`, percent, TRUE, as.integer(throttle)))
  } else {
    invisible(.Call(`setRxthreads`, as.integer(threads), FALSE, as.integer(throttle)))
  }
}

#' @rdname getRxThreads
#' @export
rxCores <- getRxThreads

#' Unloads all rxode2 compiled DLLs
#'
#' @return List of rxode2 dlls still loaded
#'
#' @return boolean of if all rxode2 dlls have been unloaded
#'
#' @examples
#'
#' print(rxUnloadAll())
#' @export
rxUnloadAll <- function() {
  try(rxUnloadAll_(), silent = TRUE)
}
#' With one sink, then release
#'
#' @param file the path to the file sink while running the `code`
#'
#' @param code The code to run during the sink
#'
#' @return Will return the results of the `code` section
#'
#' @details
#'
#' `.rxWithSink` captures output from `cat`
#'
#' `.rxWithSinkBoth` captures output from `cat` and `message`
#'
#' @export
#'
#' @keywords internal
#'
#' @author Matthew Fidler
#'
#' @examples
#'
#' t <- tempfile()
#' .rxWithSink(t, cat("message\n"))
#' cat("cat2\n") # now you can see the cat2
#' lines <- readLines(t)
#' unlink(t)
.rxWithSink <- function(file, code) {
  sink(file) # nolint
  on.exit(sink()) # nolint
  force(code)
}

#' @rdname dot-rxWithSink
#' @export
.rxWithSinkBoth <- function(file, code) {
  zz <- file(file, open = "wt")
  sink(zz) # nolint
  sink(zz, type = "message") # nolint
  on.exit({
    sink() # nolint
    sink(type = "message") # nolint
    close(zz)
  })
  force(code)
}


#' Temporarily set options then restore them while running code
#'
#' @param ops list of options that will be temporarily set for the
#'   `code`
#'
#' @inheritParams .rxWithSink
#'
#' @return value of code
#'
#' @export
#' @examples
#'
#' .rxWithOptions(list(digits = 21), {
#'   print(pi)
#' })
#'
#' print(pi)
.rxWithOptions <- function(ops, code) {
  .old <- options() # nolint
  rxSyncOptions()
  do.call(options, as.list(ops)) # nolint
  on.exit({
    options(.old) # nolint
    rxSyncOptions()
  })
  force(code)
}


#' Temporarily set options then restore them while running code
#'
#' @param wd working directory to temporarily set the system to while
#'   evaluating the code
#'
#' @return value of code
#'
#' @inheritParams .rxWithSink
#'
#' @export
#' @examples
#'
#' .rxWithWd(tempdir(), {
#'   getwd()
#' })
#'
#' getwd()
.rxWithWd <- function(wd, code) {
  .old <- getwd() # nolint
  on.exit({
    setwd(.old) # nolint
  })
  setwd(wd) # nolint
  force(code)
}

.qassert <- function(x, rules, .var.name = checkmate::vname(x)) {
  .val <- try(checkmate::qassert(x, rules, .var.name = .var.name), silent = TRUE)
  if (inherits(.val, "try-error")) {
    return(attr(.val, "condition")$message)
  }
  return("")
}



use.utf <- function() {
  opt <- getOption("cli.unicode", NULL)
  if (!is.null(opt)) {
    isTRUE(opt)
  } else {
    l10n_info()$`UTF-8` && !is.latex()
  }
}

is.latex <- function() {
  if (!("knitr" %in% loadedNamespaces())) {
    return(FALSE)
  }
  get("is_latex_output", asNamespace("knitr"))()
}


.nsToLoad <- function() {
  vapply(rxode2parseGetPackagesToLoad(),
         function(pkg) {
           requireNamespace(pkg, quietly = TRUE)
         }, logical(1))
}

#' Check if a language object matches a template language object
#'
#' \itemize{
#'   \item{If \code{template == str2lang(".")}, it will match anything.}
#'   \item{If \code{template == str2lang(".name")}, it will match any name.}
#'   \item{If \code{template == str2lang(".call()")}, it will match any call.}
#' }
#'
#' @param x The object to check
#' @param template The template object it should match
#' @return TRUE if it matches, FALSE, otherwise
#' @keywords Internal
#' @examples
#' .matchesLangTemplate(str2lang("d/dt(foo)"), str2lang("d/dt(.name)"))
#' .matchesLangTemplate(str2lang("d/dt(foo)"), str2lang("d/foo(.name)"))
#' .matchesLangTemplate(str2lang("d/dt(foo)"), str2lang("d/."))
#' @export
.matchesLangTemplate <- function(x, template) {
  if (identical(template, as.name("."))) {
    ret <- TRUE
  } else if (is.name(x) && identical(template, as.name(".name"))) {
    ret <- TRUE
  } else if (is.call(x) && identical(template, str2lang(".call()"))) {
    ret <- TRUE
  } else {
    # A more specific match is needed
    ret <- all(class(x) == class(template))
    if (ret) {
      if (length(x) == length(template)) {
        if (length(x) > 1) {
          for (idx in seq_along(x)) {
            ret <- ret && .matchesLangTemplate(x[[idx]], template[[idx]])
          }
        } else if (is.name(x)) {
          # Check for a value if the name is not ".name"
          ret <- x == template
        } else {
          # Require identical for one-length calls (e.g. `linCmt()`), numeric,
          # character, etc.
          ret <- identical(x, template)
        }
      } else {
        ret <- FALSE
      }
    }
  }
  ret
}
#' Print out a table in the documentation
#'
#' @param table data frame
#' @param caption a character vector representing the caption for the latex table
#' @return based on the `knitr` context:
#' - output a `kableExtra::kbl` for `latex` output
#' - output a `DT::datatable` for html output
#' - otherwise output a `knitr::kable`
#' @keywords internal
#' @export
#' @author Matthew L. Fidler
#' @examples
#' .rxDocTable(rxReservedKeywords)
.rxDocTable <- function(table, caption="none") {
  rxReq("knitr")
  if (knitr::is_latex_output()) {
    rxReq("kableExtra")
    kableExtra::kbl(table, longtable=TRUE, booktabs=TRUE, caption=caption) %>%
      kableExtra::kable_styling(latex_options=c("repeat_header", "striped", "hold_position"))
  } else if (knitr::is_html_output(excludes = "gfm")) {
    rxReq("DT")
    DT::datatable(table, rownames = FALSE, filter="top",  options=list(pageLength = 5, scrollX=TRUE))
  } else {
    knitr::kable(table)
  }
}

#' Calculate expected confidence bands or prediction intreval with normal or t sampling distribution
#'
#' The generic function `meanProbs` produces expected confidence bands
#' under either the t distribution or the normal sampling
#' distribution. This uses `qnorm()` or `qt()` with the mean and
#' standard deviation.
#'
#' For a single probability, p, it uses either:
#'
#' mean + qt(p, df=n)*sd/sqrt(n)
#'
#' or
#'
#' mean + qnorm(p)*sd/sqrt(n)
#'
#' The smallest observation corresponds to a probability of 0 and the
#' largest to a probability of 1 and the mean corresponds to 0.5.
#'
#' The mean and standard deviation of the sample is calculated based
#' on Welford's method for a single pass.
#'
#' This is meant to perform in the same way as `quantile()` so it can
#' be a drop in replacement for code using `quantile()` but using
#' distributional assumptions.
#'
#' @param x numeric vector whose mean and probability based confidence
#'   values are wanted, NA and NaN values are not allowed in numeric
#'   vectors unless ‘na.rm’ is ‘TRUE’.
#' @param probs numeric vector of probabilities with values in the
#'   interval from 0 to 1 .
#' @param na.rm logical; if true, any NA and NaN's are removed from
#'   `x` before the quantiles are computed.
#' @param names logical; if true, the result has a names attribute.
#' @param useT logical; if true, use the t-distribution to calculate
#'   the confidence-based estimates. If false use the normal
#'   distribution to calculate the confidence based estimates.
#' @param onlyProbs logical; if true, only return the probability
#'   based confidence interval estimates, otherwise return
#' @param pred logical; if true use the prediction interval instead of
#'   the confidence interval
#' @param n integer/integerish; this is the n used to calculate the
#'   prediction or confidence interval.  When `n=0` (default) use the
#'   number of non-`NA` observations.
#' @param ... Arguments passed to default method, allows many
#'   different methods to be applied.
#' @return By default the return has the probabilities as names (if
#'   named) with the points where the expected distribution are
#'   located given the sampling mean and standard deviation. If
#'   `onlyProbs=FALSE` then it would prepend mean, variance, standard
#'   deviation, minimum, maximum and number of non-NA observations.
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' quantile(x<- rnorm(1001))
#' meanProbs(x)
#'
#' # Can get some extra statistics if you request onlyProbs=FALSE
#' meanProbs(x, onlyProbs=FALSE)
#'
#' x[2] <- NA_real_
#'
#' meanProbs(x, onlyProbs=FALSE)
#'
#' quantile(x<- rnorm(42))
#'
#' meanProbs(x)
#'
#' meanProbs(x, useT=FALSE)
#'
meanProbs <- function(x, ...) {
  UseMethod("meanProbs")
}

#' @rdname meanProbs
#' @export
meanProbs.default <- function(x, probs=seq(0, 1, 0.25), na.rm=FALSE,
                              names=TRUE, useT=TRUE, onlyProbs=TRUE, pred=FALSE,
                              n=0L, ...) {
  checkmate::assertNumeric(x)
  checkmate::assertNumeric(probs, min.len=1, any.missing = FALSE, lower=0.0, upper=1.0)
  checkmate::assertLogical(na.rm, any.missing=FALSE, len=1)
  checkmate::assertLogical(names, any.missing=FALSE, len=1)
  checkmate::assertLogical(useT, any.missing=FALSE, len=1)
  checkmate::assertLogical(onlyProbs, any.missing=FALSE, len=1)
  checkmate::assertLogical(pred, any.missing=FALSE, len=1)
  checkmate::assertIntegerish(n, min.len=1, max.len=1, any.missing=FALSE, lower=0)
  n <- as.integer(n)
  .ret <- .Call(`_rxode2_meanProbs_`, x, probs, na.rm, useT, pred, n)
  .names <- NULL
  if (names) {
    .names <- paste0(probs*100, "%")
  }
  if (onlyProbs) {
    .ret <- .ret[-1L:-6L]
    if (names) {
      names(.ret) <- .names
    }
  } else if (names) {
    names(.ret) <- c("mean","var", "sd", "min", "max", "n", .names)
  }
  .ret
}

#' Calculate expected confidence bands with binomial sampling distribution
#'
#' This is meant to perform in the same way as `quantile()` so it can
#' be a drop in replacement for code using `quantile()` but using
#' distributional assumptions.
#'
#' It is used for confidence intervals with rxode2 solved objects using
#' `confint(mean="binom")`
#'
#' @param x numeric vector whose mean and probability based confidence
#'   values are wanted, NA and NaN values are not allowed in numeric
#'   vectors unless `na.rm` is `TRUE`.
#'
#' @param probs numeric vector of probabilities with values in the
#'   interval 0 to 1, inclusive. When 0, it represents the maximum
#'   observed, when 1, it represents the maximum observed. When 0.5 it
#'   represents the expected probability (mean).
#'
#' @param na.rm logical; if true, any NA and NaN's are removed from
#'   `x` before the quantiles are computed.
#'
#' @param names logical; if true, the result has a names attribute.
#'
#' @param onlyProbs logical; if true, only return the probability
#'   based confidence interval/prediction interval estimates,
#'   otherwise return extra statistics.
#'
#' @param n integer/integerish; this is the n used to calculate the
#'   prediction or confidence interval.  When `n=0` (default) use the
#'   number of non-`NA` observations.  When calculating the prediction
#'   interval, this represents the number of observations used in the
#'   input ("true") distribution.
#'
#' @param pred Use a prediction interval instead of a confidence
#'   interval.  By default this is `FALSE`.
#'
#' @param m integer.  When using the prediction interval this
#'   represents the number of samples that will be observed in the
#'   future for the prediction interval.
#'
#' @param piMethod gives the prediction interval method (currently only lim) from Lu 2020
#'
#' @param M number of simulations to run for the LIM PI.
#'
#' @param tol tolerance of root finding in the LIM prediction interval
#'
#' @param ciMethod gives the method for calculating the confidence
#'   interval.
#'
#'  Can be:
#'
#'  - "argestiCoull" or "ac" -- Agresti-Coull method. For a 95\% confidence
#'     interval, this method does not use the concept   of "adding 2
#'     successes and 2 failures," but rather uses the formulas explicitly
#'     described in the following link:
#'
#' https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Agresti-Coull_Interval.
#'
#'   - "wilson" -- Wilson Method
#'
#'   - "wilsonCorrect" or "wc" -- Wilson method with continuity correction
#'
#'   - "wald" -- Wald confidence interval or standard z approximation.
#'
#' @param ... Arguments passed to default method, allows many
#'   different methods to be applied.
#'
#' @return By default the return has the probabilities as names (if
#'   named) with the points where the expected distribution are
#'   located given the sampling mean and standard deviation. If
#'   `onlyProbs=FALSE` then it would prepend mean, variance, standard
#'   deviation, minimum, maximum and number of non-NA observations.
#'
#' @export
#' @author Matthew L. Fidler
#' @references
#'
#' - Newcombe, R. G. (1998). "Two-sided confidence intervals for the single
#'   proportion: comparison of seven methods". Statistics
#'   in Medicine. 17 (8):
#'   857–872. doi:10.1002/(SICI)1097-0258(19980430)17:8<857::AID-SIM777>3.0.CO;2-E. PMID
#'   9595616.
#'
#' - Hezhi Lu, Hua Jin,
#'   A new prediction interval for binomial random variable based on inferential models,
#'   Journal of Statistical Planning and Inference,
#'   Volume 205,
#'   2020,
#'   Pages 156-174,
#'   ISSN 0378-3758,
#'   https://doi.org/10.1016/j.jspi.2019.07.001.
#' @examples
#'
#' x<- rbinom(7001, p=0.375, size=1)
#' binomProbs(x)
#'
#' # you can also use the prediction interval
#' \donttest{
#' binomProbs(x, pred=TRUE)
#' }
#'
#' # Can get some extra statistics if you request onlyProbs=FALSE
#' binomProbs(x, onlyProbs=FALSE)
#'
#' x[2] <- NA_real_
#'
#' binomProbs(x, onlyProbs=FALSE)
#'
#' binomProbs(x, na.rm=TRUE)
#'
binomProbs <- function(x, ...) {
  UseMethod("binomProbs")
}

#' @rdname binomProbs
#' @export
binomProbs.default <- function(x, probs=c(0.025, 0.05, 0.5, 0.95, 0.975), na.rm=FALSE,
                               names=TRUE, onlyProbs=TRUE, n=0L, m=0L,
                               pred=FALSE,
                               piMethod=c("lim"), M=500000,
                               tol=.Machine$double.eps^0.25,
                               ciMethod=c("wilson", "wilsonCorrect", "agrestiCoull", "wald", "wc", "ac"), ...) {
  checkmate::assertNumeric(x, min.len=1, lower=0.0, upper=1.0)
  x <- as.double(x)
  checkmate::assertIntegerish(n, min.len=1, lower=0, any.missing=FALSE)
  n <- as.integer(n)
  checkmate::assertIntegerish(m, min.len=1, lower=0, any.missing=FALSE)
  m <- as.integer(m)
  checkmate::assertNumeric(probs, min.len=1, any.missing = FALSE, lower=0.0, upper=1.0)
  checkmate::assertLogical(na.rm, any.missing=FALSE, len=1)
  checkmate::assertLogical(names, any.missing=FALSE, len=1)
  checkmate::assertLogical(onlyProbs, any.missing=FALSE, len=1)
  if (pred) {
    .m <- mean(x, na.rm=na.rm)
    if (is.na(.m)) {
      .ret <- stats::quantile(NULL,probs=probs)
      if (!onlyProbs) {
        .ret <- c("mean"=NA_real_,"var"=NA_real_, "sd"=NA_real_, "n"=NA_real_,
                  .ret)
      }
    } else {
      .nC <- sum(!is.na(x))
      if (n == 0L) n <- as.integer(.nC)
      if (m == 0L) m <- as.integer(.nC)
      .Y <- round(.nC * .m) # number of successes
      .ret <- stats::quantile(.Call(`_rxode2_binomProbsPredVec_`, n, m, .Y, M, TRUE, tol),
                       probs=probs)
      if (!onlyProbs) {
        .ret <- c("mean"=.m,"var"=.m * (1.0 - .m), "sd"=sqrt(.m * (1.0 - .m)), "n"=.nC,
                  .ret)
      }
    }
    if (!names) {
      names(.ret) <- NULL
    }
    return(.ret)
  } else {
    ciMethod <- match.arg(ciMethod)
    ciMethod <- setNames(c("wilson"=1L, "wilsonCorrect"=0L, "agrestiCoull"=3L, "wald"=2L, "ac"=3L, "wc"=0L)[ciMethod], NULL)
    .ret <- .Call(`_rxode2_binomProbs_`, x, probs, na.rm, n, ciMethod)
    .names <- NULL
    if (names) {
      .names <- paste0(probs*100, "%")
    }
    if (onlyProbs) {
      .ret <- .ret[-1L:-4L]
      if (names) {
        names(.ret) <- .names
      }
    } else if (names) {
      names(.ret) <- c("mean","var", "sd", "n", .names)
    }
    .ret
  }
}



#' Convert a factor/char to an id
#'
#' @param a value to convert to an id
#' @return id factor
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
#' @examples
#' .convertId("a")
.convertId <- function(a) {
  .Call(`_rxode2_convertId_`, a)
}

#' Get the internal breakdown of an evid
#'
#' @param i evid to breakdown
#' @return named evid integer vector
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
#' @examples
#'
#' .getWh(1001)
#' .getWh(10401)
#'
.getWh <- function(i) {
  checkmate::assertIntegerish(i,len=1, any.missing=FALSE)
  .Call(`_rxode2_getWh`, as.integer(i))
}

#' This converts NONMEM-style EVIDs to classic RxODE events
#'
#' @param cmt compartment flag
#' @param amt dose amount
#' @param rate dose rate
#' @param dur dose duration
#' @param ii inter-dose interval
#' @param evid event id
#' @param ss steady state
#' @return classic evids, excluding evids that are added (you need to
#'   add them manually) or simply use etTran.  This is mostly for
#'   testing and really shouldn't be used directly.
#' @export
#' @author Matthew L. Fidler
#' @examples
#' .toClassicEvid(cmt=10, amt=3, evid=1)
#' .toClassicEvid(cmt=10, amt=3, rate=2, evid=1)
#' .toClassicEvid(cmt=10, amt=3, rate=-1, evid=1)
#' .toClassicEvid(cmt=10, amt=3, rate=-2, evid=1)
#' .toClassicEvid(cmt=10, amt=3, dur=2, evid=1)
#' .toClassicEvid(cmt=304, amt=3, dur=2, evid=1)
#' .toClassicEvid(cmt=7, amt=0, rate=2, evid=1, ss=1)
#' .toClassicEvid(cmt=-10, amt=3, evid=1)
#' .toClassicEvid(cmt=10, amt=3, evid=5)
#' .toClassicEvid(cmt=6, amt=3, evid=6)
#' .toClassicEvid(cmt=6, amt=3, evid=7)
#' .toClassicEvid(evid=2)
#' .toClassicEvid(evid=4)
.toClassicEvid <- function(cmt=1L, amt=0.0, rate=0.0, dur=0.0, ii=0.0, evid=0L, ss=0.0) {
  .w <- which(is.na(cmt))
  if (length(.w) > 0) cmt[.w] <- 1
  checkmate::assertIntegerish(cmt)
  checkmate::assertIntegerish(evid, any.missing=FALSE)
  checkmate::assertNumeric(amt)
  checkmate::assertNumeric(dur, any.missing=FALSE)
  checkmate::assertNumeric(ii)
  checkmate::assertNumeric(ss)
  .df <- data.frame(cmt=as.integer(cmt), evid=as.integer(evid), amt=as.double(amt),
                    rate=as.double(rate), dur=as.double(dur),
                    ii=as.double(ii),
                    ss=as.double(ss))
  .Call(`_rxode2_getClassicEvid`,
        .df$cmt, .df$amt, .df$rate, .df$dur,
        .df$ii, .df$evid, .df$ss)
}

.rxDerivedReg <- rex::rex(
  start,
  or(
    group(or("V", "Q", "VP", "VT", "CLD"), number),
    "KA", "VP", "VT", "CLD", "V", "VC", "CL", "VSS", "K", "KE", "KEL",
    "Q", "VT", group("K", number, number), "AOB", "ALPHA", "BETA", "GAMMA",
    "A", "B", "C"
  ),
  end
)


#' Calculate derived parameters for the 1-, 2-, and 3- compartment
#' linear models.
#'
#' This calculates the derived parameters based on what is provided
#' in a data frame or arguments
#'
#' @param ... The input can be:
#'
#'
#'  * A data frame with PK parameters in it; This should ideally
#'  be a data frame with one pk parameter per row since it will
#'  output a data frame with one PK parameter per row.
#'
#'  * PK parameters as either a vector or a scalar
#'
#'
#' @param verbose boolean that when TRUE provides a message about the detected pk parameters
#'   and the detected compartmental model.  By default this is `FALSE`.
#'
#' @param digits represents the number of significant digits for the
#'   output; If the number is zero or below (default), do not round.
#'
#' @return Return a data.frame of derived PK parameters for a 1-, 2-,
#'   or 3-compartment linear model given provided clearances and
#'   volumes based on the inferred model type.
#'
#' The model parameters that will be provided in the data frame are:
#'
#' * `vc`: Central Volume (for 1-, 2- and 3-
#'   compartment models)
#'
#' * `kel`: First-order elimination rate (for 1-, 2-, and
#'   3-compartment models)
#'
#' * `k12`: First-order rate of transfer from central to
#'   first peripheral compartment; (for 2- and 3-compartment models)
#'
#' * `k21`: First-order rate of transfer from first
#'   peripheral to central compartment, (for 2- and 3-compartment
#'   models)
#'
#' * `k13`: First-order rate of transfer from central to
#'   second peripheral compartment; (3-compartment model)
#'
#' * `k31`: First-order rate of transfer from second
#'   peripheral to central compartment (3-compartment model)
#'
#' * `vp`: Peripheral Volume (for 2- and 3- compartment models)
#'
#' * `vp2`: Peripheral Volume for 3rd compartment (3- compartment model)
#'
#' * `vss`: Volume of distribution at steady state; (1-, 2-, and 3-compartment models)
#'
#' * `t12alpha`: \eqn{t_{1/2,\alpha}}; (1-, 2-, and 3-compartment models)
#'
#' * `t12beta`: \eqn{t_{1/2,\beta}}; (2- and 3-compartment models)
#'
#' * `t12gamma`: \eqn{t_{1/2,\gamma}}; (3-compartment model)
#'
#' * `alpha`: \eqn{\alpha}; (1-, 2-, and 3-compartment models)
#'
#' * `beta`: \eqn{\beta}; (2- and 3-compartment models)
#'
#' * `gamma`: \eqn{\beta}; (3-compartment model)
#'
#' * `A`: true `A`; (1-, 2-, and 3-compartment models)
#'
#' * `B`: true `B`; (2- and 3-compartment models)
#'
#' * `C`: true `C`; (3-compartment model)
#'
#' * `fracA`: fractional A; (1-, 2-, and 3-compartment models)
#'
#' * `fracB`: fractional B; (2- and 3-compartment models)
#'
#' * `fracC`: fractional C; (3-compartment model)
#'
#' @author Matthew Fidler and documentation from Justin Wilkins, \email{justin.wilkins@@occams.com}
#'
#' @references Shafer S. L. `CONVERT.XLS`
#'
#' @references Rowland M, Tozer TN. Clinical Pharmacokinetics and Pharmacodynamics: Concepts and Applications (4th). Clipping Williams & Wilkins, Philadelphia, 2010.
#'
#' @examples
#'
#' ## Note that rxode2 parses the names to figure out the best PK parameter
#'
#' params <- rxDerived(cl = 29.4, v = 23.4, Vp = 114, vp2 = 4614, q = 270, q2 = 73)
#'
#' ## That is why this gives the same results as the value before
#'
#' params <- rxDerived(CL = 29.4, V1 = 23.4, V2 = 114, V3 = 4614, Q2 = 270, Q3 = 73)
#'
#' ## You may also use micro-constants alpha/beta etc.
#'
#' params <- rxDerived(k12 = 0.1, k21 = 0.2, k13 = 0.3, k31 = 0.4, kel = 10, v = 10)
#'
#' ## or you can mix vectors and scalars
#'
#' params <- rxDerived(CL = 29.4, V = 1:3)
#'
#' ## If you want, you can round to a number of significant digits
#' ## with the `digits` argument:
#'
#' params <- rxDerived(CL = 29.4, V = 1:3, digits = 2)
#' @export
rxDerived <- function(..., verbose = FALSE, digits = 0) {
  .lst <- list(...)
  if (inherits(.lst[[1]], "data.frame")) {
    .lst <- .lst[[1]]
  }
  .namesU <- toupper(names(.lst))
  .w <- which(regexpr(.rxDerivedReg, .namesU) != -1)
  if (length(.w) > 1L) {
    if (verbose) {
      message("parameters: ", paste(names(.lst)[.w], collapse = ","))
    }
    .linCmt <- .Call(
      `_linCmtParse`, names(.lst)[.w],
      c(
        "with(.lst,.Call(`_calcDerived`, ", "list(", "0, 0, 0, 0, ",
        ", 0, 0, 0, 0),digits))"
      ),
      verbose
    )$str
    .env <- environment()
    return(eval(parse(text = .linCmt), envir = .env))
  } else {
    stop("cannot figure out PK parameters to convert", call. = FALSE)
  }
}

#' Get the information about the rxode2 derived parameter transformation
#'
#'
#' @param ... Parameters translated, should be unquoted and not assigned to anything.
#' @return Translation information; This list contains:
#'
#' - `$str` A named string of the parameters as seen in the underlying C/C++
#'   code. The parameters that are NA are not used in the linear
#'   compartment model calculations.
#'
#' - `$ncmt` the number of compartments in the model
#'
#' - `$trans` the rxode2 translation number of the parameterization
#'
#' This contains the linCmt()
#'   translation number, the number of compartments and the parameters
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
#' @examples
#'
#' .rxTransInfo(cl, v , Vp, vp2, q, q2)
#'
#' .rxTransInfo(k12, k21, k13, k31, kel, v)
#'
#' .rxTransInfo(k12, k21, k13, k31, kel, v, ka)
#'
#' .rxTransInfo(CL, V)
#'
.rxTransInfo <- function(...) {
  .args <- as.list(match.call(expand.dots = TRUE))[-1]
  .args <- as.character(.args)
  .namesU <- toupper(as.character(.args))
  .w <- which(regexpr(.rxDerivedReg, .namesU) != -1)
  if (length(.w) > 1L) {
    .linCmt <- .Call(
      `_linCmtParse`, .args[.w],
      c(
        "", "", "tlag, F, rate1, dur1, ",
        ", tlag2, F2, rate2, dur2"
      ),
      FALSE
    )
    .str <- .linCmt$str
    .str <- strsplit(.str, ", +")[[1]]
    .str <- .str[-(1:2)]
    .str <- .str[c(1:6, 11)]
    .str <- vapply(seq_along(.str), function(i) {
      .num <- suppressWarnings(as.numeric(.str[i]))
      if (is.na(.num)) return(.str[i])
      NA_character_
    }, character(1), USE.NAMES=FALSE)
    names(.str) <- c("p1", "v1", "p2", "p3","p4", "p5", "ka")
    .linCmt$str <- .str
    .linCmt
  } else {
    stop("cannot figure out PK parameters to use", call. = FALSE)
  }
}

## nocov start
.dummy <- function() {
  #dummy import to make check() and CRAN happy
  .r <- rex::rex(start, end)
  .d <- data.table::data.table(a=1)
}
#' Is the linear systems with gradients built-in
#'
#' @return logical (TRUE) if the solved systems with gradients are
#'   built-in. (FALSE) if the solves systems with gradients are absent
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
#' @examples
#' .linCmtSensB()
.linCmtSensB <- function() {
  as.logical(.Call(`_rxode2parse_linCmtB`))
}
## nocov end
