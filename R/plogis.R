#' Logistic distribution function via rxode2 expit expressions
#'
#' This mirrors the base R `plogis()` interface while using rxode2's
#' `expit()` building block for the common positive-scale path.
#'
#' @param q vector of quantiles.
#' @param location location parameter.
#' @param scale scale parameter.
#' @param lower.tail logical; if `TRUE` (default), probabilities are `P[X <= x]`,
#'   otherwise `P[X > x]`.
#' @param log.p logical; if `TRUE`, probabilities are returned on the log scale.
#'
#' @return numeric probabilities.
#'
#' @rdname logit
#' @export
plogis <- function(q, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE) {
  .lowerTail <- try(as.logical(lower.tail), silent = TRUE)
  .logP <- try(as.logical(log.p), silent = TRUE)
  if (inherits(.lowerTail, "try-error") ||
      inherits(.logP, "try-error") ||
      length(.lowerTail) != 1L ||
      length(.logP) != 1L ||
      is.na(.lowerTail) ||
      is.na(.logP) ||
      any(!is.na(scale) & scale <= 0)) {
    return(stats::plogis(q,
                         location = location,
                         scale = scale,
                         lower.tail = lower.tail,
                         log.p = log.p))
  }
  .z <- (q - location) / scale
  .ret <- if (.lowerTail) expit(.z) else expit(-.z)
  if (.logP) {
    return(log(.ret))
  }
  .ret
}

.plogisMatchCall <- function(fun) {
  .args <- as.list(formals(stats::plogis))
  .call <- as.list(match.call(stats::plogis, fun, expand.dots = FALSE))[-1L]
  .args[names(.call)] <- .call
  .args
}

.plogisIsValue <- function(expr, value, env = baseenv()) {
  .val <- try(eval(expr, envir = env), silent = TRUE)
  if (inherits(.val, "try-error") || length(.val) != 1L || is.na(.val)) {
    return(FALSE)
  }
  identical(as.numeric(.val), as.numeric(value))
}

.plogisUiExpr <- function(expr, env = parent.frame()) {
  .val <- suppressWarnings(try(eval(expr, envir = env), silent = TRUE))
  if (!inherits(.val, "try-error") &&
      length(.val) == 1L &&
      (is.numeric(.val) || is.character(.val))) {
    return(str2lang(as.character(.val)))
  }
  expr
}

.plogisFlag <- function(expr, arg, env = baseenv()) {
  .val <- suppressWarnings(try(eval(expr, envir = env), silent = TRUE))
  if (inherits(.val, "try-error") ||
      length(.val) != 1L ||
      is.na(.val) ||
      (!is.logical(.val) && !is.numeric(.val))) {
    stop(sprintf("'plogis' requires '%s' to be a scalar TRUE/FALSE value in rxode2 model syntax", arg),
         call. = FALSE)
  }
  as.logical(.val)
}

.plogisZLang <- function(q, location, scale, env = baseenv()) {
  .ret <- q
  if (!.plogisIsValue(location, 0, env = env)) {
    .ret <- call("-", .ret, location)
  }
  if (!.plogisIsValue(scale, 1, env = env)) {
    .ret <- call("/", .ret, scale)
  }
  .ret
}

.plogisRxLang <- function(q, location, scale, lower.tail, log.p, env = baseenv()) {
  .z <- .plogisZLang(q, location, scale, env = env)
  if (isTRUE(lower.tail)) {
    .ret <- call("expit", .z)
  } else {
    .ret <- call("expit", call("-", .z))
  }
  if (isTRUE(log.p)) {
    .ret <- call("log", .ret)
  }
  .ret
}

#' @export
rxUdfUi.plogis <- function(fun) {
  .args <- .plogisMatchCall(fun)
  .env <- parent.frame()
  .q <- .plogisUiExpr(.args$q, env = .env)
  .location <- .plogisUiExpr(.args$location, env = .env)
  .scale <- .plogisUiExpr(.args$scale, env = .env)
  .lowerTail <- .plogisFlag(.args$lower.tail, "lower.tail", env = .env)
  .logP <- .plogisFlag(.args$log.p, "log.p", env = .env)
  list(replace = .plogisRxLang(.q, .location, .scale, .lowerTail, .logP, env = .env))
}
