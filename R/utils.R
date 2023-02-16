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
  vapply(rxode2parse::rxode2parseGetPackagesToLoad(),
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
#' @noRd
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

