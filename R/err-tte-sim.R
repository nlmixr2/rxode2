# tte() simulation model lines: the hidden cumulative-hazard states, the
# endpoint lines and the tteTime()/tteCount() endpoint-name rewrite.

#' Which tte endpoint (1-based, in $predDf order) is this endpoint?
#'
#' @param predDf the ui's `$predDf`
#' @param cond endpoint condition
#' @return integer tte index, or NA when `cond` is not a tte endpoint
#' @author Matthew L. Fidler
#' @noRd
.rxTteIndex <- function(predDf, cond) {
  .tte <- predDf$cond[predDf$distribution == "tte"]
  match(cond, .tte)
}

#' Name of the hidden cumulative-hazard state of a tte endpoint
#'
#' @param var endpoint variable
#' @return state name
#' @noRd
.rxTteStateName <- function(var) {
  # dot-free: symengine cannot parse a dotted name
  paste0("rx_tteH_", gsub("[^A-Za-z0-9_]", "_", var))
}

#' Hidden cumulative-hazard ODE lines for every tte endpoint
#'
#' @param ui rxode2 ui
#' @return list of `d/dt(rx_tteH_<var>) <- <hazard>` lines (NULL when the model
#'   has no tte endpoint)
#' @author Matthew L. Fidler
#' @noRd
.rxTteStateLines <- function(ui) {
  .predDf <- ui$predDf
  if (is.null(.predDf)) {
    return(NULL)
  }
  .w <- which(.predDf$distribution == "tte")
  if (length(.w) == 0L) {
    return(NULL)
  }
  lapply(.w, function(.i) {
    .o <- .rxTteOptions(ui, .predDf[.i, ])
    .h <- .rxTteRewriteLines(list(.o$hazard), .predDf, ui)[[1]]
    bquote(d / dt(.(str2lang(.rxTteStateName(.predDf$var[.i])))) <- .(.h))
  })
}

#' @rdname rxGetDistributionSimulationLines
#' @export
rxGetDistributionSimulationLines.tte <- function(line) {
  .env <- line[[1]]
  .pred1 <- line[[2]]
  .k <- as.numeric(.rxTteIndex(.env$predDf, .pred1$cond))
  .h <- str2lang(.rxTteStateName(.pred1$var))
  list(
    bquote(ipredSim <- exp(-(.(.h) - rxTteH0(.(.k))))),
    bquote(sim <- rxTteSim(.(.k)))
  )
}

#' Codes passed to `tteTime()`/`tteCount()` in C for each tte endpoint
#'
#' The code is `k + 100*cmt` (k the 1-based tte index, cmt the endpoint's
#' compartment), negative for `origin = TRUE`; a simulation solve reads k, an
#' estimation solve looks the endpoint up in the data by cmt.
#'
#' @param predDf the ui's `$predDf`
#' @param env parsed ui environment (for the `origin` option), or NULL
#' @return numeric codes named by the endpoint conditions
#' @author Matthew L. Fidler
#' @noRd
.rxTteCodes <- function(predDf, env = NULL) {
  if (is.null(predDf)) {
    return(numeric(0))
  }
  .w <- which(predDf$distribution == "tte")
  .code <- vapply(seq_along(.w), function(.k) {
    .pred1 <- predDf[.w[.k], ]
    .origin <- !is.null(env) && isTRUE(.rxTteOptions(env, .pred1)$origin)
    # no cmt yet while parsing (the model variables only need a number)
    .cmt <- if (length(.pred1$cmt) == 1L && !is.na(.pred1$cmt)) as.numeric(.pred1$cmt) else 0
    (if (.origin) -1 else 1) * (.k + 100 * .cmt)
  }, numeric(1))
  stats::setNames(.code, predDf$cond[.w])
}

#' Rewrite `tteTime(ev)`/`tteCount(ev)` to the endpoint's code
#'
#' @param expr quoted model expression
#' @param tte codes of the model's tte endpoints, from `.rxTteCodes()`
#' @return the rewritten expression
#' @author Matthew L. Fidler
#' @noRd
.rxTteRewrite <- function(expr, tte) {
  if (!is.call(expr)) {
    return(expr)
  }
  if ((identical(expr[[1]], quote(`tteTime`)) || identical(expr[[1]], quote(`tteCount`)))) {
    .f <- as.character(expr[[1]])
    if (length(expr) != 2L || !is.name(expr[[2]])) {
      stop("'", .f, "()' takes one tte() endpoint name, e.g. ", .f, "(ev)", call. = FALSE)
    }
    .k <- match(as.character(expr[[2]]), names(tte))
    if (is.na(.k)) {
      stop("'", .f, "(", as.character(expr[[2]]), ")' does not name a tte() endpoint", call. = FALSE)
    }
    expr[[2]] <- tte[[.k]]
    return(expr)
  }
  as.call(lapply(as.list(expr), .rxTteRewrite, tte = tte))
}

#' Rewrite the tteTime()/tteCount() calls of a list of model lines
#'
#' @param lines list of quoted lines, or character lines
#' @param predDf the ui's `$predDf`
#' @param env parsed ui environment (for the `origin` option), or NULL
#' @return the rewritten lines (same type)
#' @noRd
.rxTteRewriteLines <- function(lines, predDf, env = NULL) {
  .tte <- .rxTteCodes(predDf, env)
  if (is.character(lines)) {
    .w <- grepl("tte(Time|Count)", lines)
    if (!any(.w)) {
      return(lines)
    }
    lines[.w] <- vapply(
      lines[.w],
      function(.l) deparse1(.rxTteRewrite(str2lang(.l), .tte)),
      character(1),
      USE.NAMES = FALSE
    )
    return(lines)
  }
  lapply(lines, .rxTteRewrite, tte = .tte)
}
