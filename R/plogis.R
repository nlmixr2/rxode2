#' This function matches the call to 'plogis'
#'
#' @param fun the R language expression for plogis that is parsed
#' @return list of the formals for plogis with the arguments replaced
#'   by the values in the call
#' @noRd
#' @author Matthew L. Fidler
.plogisMatchCall <- function(fun) {
  .args <- as.list(formals(stats::plogis))
  .call <- as.list(match.call(stats::plogis, fun, expand.dots = FALSE))[-1L]
  .args[names(.call)] <- .call
  .args
}
#' This is the expression
#'
#' @param q the plogis `q` expression
#' @param location the `plogis` location expression
#' @param scale the `scale` expression
#' @param env the environment that evaluations are performed in.
#' @return the plogis()->expit() translation when lower.tail=TRUE
#' @noRd
#' @author Matthew L. Fidler
.plogisZLang <- function(q, location, scale, env = baseenv()) {
  .ret <- q
  if (!rxUdfUiIsValue(location, 0, env = env)) {
    .ret <- call("-", .ret, location) # q-location
  }
  if (!rxUdfUiIsValue(scale, 1, env = env)) {
    if (is.call(.ret) && identical(.ret[[1]], quote(`-`)) && length(.ret) == 3L) {
      .ret <- str2lang(paste0("(", deparse1(.ret), ")")) # (q-location)
    }
    .ret <- call("/", .ret, scale) # (q-location)/scale or q/scale
  }
  .ret
}
#' Plogis for lower.tail=FALSE
#'
#'
#' @param q the plogis `q` expression
#' @param location the `plogis` location expression
#' @param scale the `scale` expression
#' @param env the environment that evaluations are performed in.
#' @return the plogis()->expit() translation when lower.tail=FALSE
#' @noRd
#' @author Matthew L. Fidler
.plogisNegZLang <- function(q, location, scale, env = baseenv()) {
  if (rxUdfUiIsValue(location, 0, env = env)) {
    .ret <- call("-", q) # -q
  } else {
    .ret <- call("-", location, q) # location-q
  }
  if (!rxUdfUiIsValue(scale, 1, env = env)) {
    if (is.call(.ret) &&
          identical(.ret[[1]], quote(`-`)) &&
          length(.ret) == 3L) {
      .ret <- str2lang(paste0("(", deparse1(.ret), ")")) # -(location-q) or -(-q)
    }
    .ret <- call("/", .ret, scale) # -(location-q)/scale or -(-q)/scale
  }
  .ret
}
#' Change plogis to expit for RxUdfUi
#'
#' @param q the plogis `q` expression
#' @param location the `plogis` location expression
#' @param scale the `scale` expression
#' @param lower.tail the `lower.tail` expression
#' @param log.p the `log.p` expression
#' @param env the environment that evaluations are performed in.
#' @return the plogis()->expit() translation
#' @noRd
#' @author Matthew L. Fidler
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

# The plogis() ui translation to expit()
#' @export
rxUdfUi.plogis <- function(fun) {
  .args <- .plogisMatchCall(fun)
  .env <- parent.frame()
  .q <- rxUdfUiExpr(.args$q, env = .env)
  .location <- rxUdfUiExpr(.args$location, env = .env)
  .scale <- rxUdfUiExpr(.args$scale, env = .env)
  .lowerTail <- rxUdfUiFlag(.args$lower.tail, arg="lower.tail", funName="plogis", env = .env)
  .logP <- rxUdfUiFlag(.args$log.p, arg="log.p", funName="plogis", env = .env)
  list(replace = .plogisRxLang(.q, .location, .scale, .lowerTail, .logP, env = .env))
}
