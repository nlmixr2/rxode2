#' Match a dnorm/pnorm/qnorm call against the R function's formals
#'
#' @param fun the R language expression that is parsed
#' @param f the stats function to match against
#' @return list of the formals with the arguments replaced by the values in
#'   the call; the attribute "supplied" lists the arguments in the call
#' @noRd
#' @author Matthew L. Fidler
.normMatchCall <- function(fun, f) {
  .args <- as.list(formals(f))
  .call <- as.list(match.call(f, fun, expand.dots = FALSE))[-1L]
  .args[names(.call)] <- .call
  attr(.args, "supplied") <- names(.call)
  .args
}

#' Build a positional dnorm/pnorm/qnorm call for the rxode2 parser
#'
#' `mean` and `sd` are only written up to the last one the user supplied so
#' positional calls are kept as written.
#'
#' @param name function name
#' @param x first argument expression
#' @param mean mean expression
#' @param sd sd expression
#' @param supplied the argument names supplied in the original call
#' @return language object
#' @noRd
#' @author Matthew L. Fidler
.normCall <- function(name, x, mean, sd, supplied) {
  if ("sd" %in% supplied) {
    as.call(list(as.name(name), x, mean, sd))
  } else if ("mean" %in% supplied) {
    as.call(list(as.name(name), x, mean))
  } else {
    as.call(list(as.name(name), x))
  }
}

#' Wrap a call in parentheses when it is an arithmetic operation
#'
#' @param x language object
#' @return x, parenthesized when needed
#' @noRd
#' @author Matthew L. Fidler
.normParen <- function(x) {
  if (is.call(x) && length(x) %in% c(2L, 3L) && as.character(x[[1]]) %in% c("+", "-", "*", "/", "^")) {
    return(call("(", x))
  }
  x
}

#' Standardized normal value `(x-mean)/sd` dropping default mean and sd
#'
#' @param x x expression
#' @param mean mean expression
#' @param sd sd expression
#' @param env evaluation environment
#' @return language object
#' @noRd
#' @author Matthew L. Fidler
.normZLang <- function(x, mean, sd, env = baseenv()) {
  .ret <- x
  if (!rxUdfUiIsValue(mean, 0, env = env)) {
    .ret <- call("-", .normParen(x), .normParen(mean))
  }
  if (!rxUdfUiIsValue(sd, 1, env = env)) {
    .ret <- call("/", .normParen(.ret), .normParen(sd))
  }
  .ret
}

#' Negate a mean expression, leaving a default zero mean alone
#'
#' @param mean mean expression
#' @param env evaluation environment
#' @return language object
#' @noRd
#' @author Matthew L. Fidler
.normNeg <- function(mean, env = baseenv()) {
  if (rxUdfUiIsValue(mean, 0, env = env)) {
    return(mean)
  }
  call("-", .normParen(mean))
}

# The dnorm() ui translation; log=TRUE is written as the log density
#' @export
rxUdfUi.dnorm <- function(fun) {
  .args <- .normMatchCall(fun, stats::dnorm)
  if (is.name(.args$x) && identical(as.character(.args$x), "")) {
    return(list(replace = fun))
  }
  # baseenv() so only literal constants evaluate, never caller variables
  .env <- baseenv()
  .x <- .args$x
  .mean <- .args$mean
  .sd <- .args$sd
  .log <- rxUdfUiFlag(.args$log, arg = "log", funName = "dnorm", env = .env)
  if (!.log) {
    return(list(replace = .normCall("dnorm", .x, .mean, .sd, attr(.args, "supplied"))))
  }
  # -z^2/2 - log(sd) - log(2*pi)/2
  .ret <- call(
    "-",
    call("*", -0.5, call("^", .normParen(.normZLang(.x, .mean, .sd, env = .env)), 2)),
    quote(0.5 * log(2 * pi))
  )
  if (!rxUdfUiIsValue(.sd, 1, env = .env)) {
    .ret <- call("-", .ret, call("log", .sd))
  }
  list(replace = .ret)
}

# The pnorm() ui translation; the upper tail uses symmetry,
# pnorm(q, mean, sd, lower.tail=FALSE) = pnorm(-q, -mean, sd)
#' @export
rxUdfUi.pnorm <- function(fun) {
  .args <- .normMatchCall(fun, stats::pnorm)
  if (is.name(.args$q) && identical(as.character(.args$q), "")) {
    return(list(replace = fun))
  }
  # baseenv() so only literal constants evaluate, never caller variables
  .env <- baseenv()
  .q <- .args$q
  .mean <- .args$mean
  .sd <- .args$sd
  .lowerTail <- rxUdfUiFlag(.args$lower.tail, arg = "lower.tail", funName = "pnorm", env = .env)
  .logP <- rxUdfUiFlag(.args$log.p, arg = "log.p", funName = "pnorm", env = .env)
  .supplied <- attr(.args, "supplied")
  if (.lowerTail) {
    .ret <- .normCall("pnorm", .q, .mean, .sd, .supplied)
  } else {
    .ret <- .normCall("pnorm", call("-", .normParen(.q)), .normNeg(.mean, .env), .sd, .supplied)
  }
  if (.logP) {
    .ret <- call("log", .ret)
  }
  list(replace = .ret)
}

# The qnorm() ui translation; the upper tail uses symmetry,
# mean - sd*qnorm(p) = -qnorm(p, -mean, sd)
#' @export
rxUdfUi.qnorm <- function(fun) {
  .args <- .normMatchCall(fun, stats::qnorm)
  if (is.name(.args$p) && identical(as.character(.args$p), "")) {
    return(list(replace = fun))
  }
  # baseenv() so only literal constants evaluate, never caller variables
  .env <- baseenv()
  .p <- .args$p
  .mean <- .args$mean
  .sd <- .args$sd
  .lowerTail <- rxUdfUiFlag(.args$lower.tail, arg = "lower.tail", funName = "qnorm", env = .env)
  .logP <- rxUdfUiFlag(.args$log.p, arg = "log.p", funName = "qnorm", env = .env)
  if (.logP) {
    .p <- call("exp", .p)
  }
  .supplied <- attr(.args, "supplied")
  if (.lowerTail) {
    .ret <- .normCall("qnorm", .p, .mean, .sd, .supplied)
  } else {
    .ret <- call("-", .normCall("qnorm", .p, .normNeg(.mean, .env), .sd, .supplied))
  }
  list(replace = .ret)
}
