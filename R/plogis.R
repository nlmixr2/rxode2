.plogisMatchCall <- function(fun) {
  .args <- as.list(formals(stats::plogis))
  .call <- as.list(match.call(stats::plogis, fun, expand.dots = FALSE))[-1L]
  .args[names(.call)] <- .call
  .args
}

.plogisIsValue <- function(expr, value, env = baseenv()) {
  .val <- try(eval(expr, envir = env), silent = TRUE)
  if (inherits(.val, "try-error") ||
      length(.val) != 1L ||
      (!is.numeric(.val) && !is.logical(.val)) ||
      is.na(.val)) {
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
    if (is.call(.ret) && identical(.ret[[1]], quote(`-`)) && length(.ret) == 3L) {
      .ret <- str2lang(paste0("(", deparse1(.ret), ")"))
    }
    .ret <- call("/", .ret, scale)
  }
  .ret
}

.plogisNegZLang <- function(q, location, scale, env = baseenv()) {
  if (.plogisIsValue(location, 0, env = env)) {
    .ret <- call("-", q)
  } else {
    .ret <- call("-", location, q)
  }
  if (!.plogisIsValue(scale, 1, env = env)) {
    if (is.call(.ret) && identical(.ret[[1]], quote(`-`)) && length(.ret) == 3L) {
      .ret <- str2lang(paste0("(", deparse1(.ret), ")"))
    }
    .ret <- call("/", .ret, scale)
  }
  .ret
}

.plogisRxLang <- function(q, location, scale, lower.tail, log.p, env = baseenv()) {
  if (isTRUE(lower.tail)) {
    .ret <- call("expit", .plogisZLang(q, location, scale, env = env))
  } else {
    .ret <- call("expit", .plogisNegZLang(q, location, scale, env = env))
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
