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
  paste0("rx.tteH.", var)
}

#' Hidden cumulative-hazard ODE lines for every tte endpoint
#'
#' @param ui rxode2 ui
#' @return list of `d/dt(rx.tteH.<var>) <- <hazard>` lines (NULL when the model
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
    .h <- .rxTteRewriteLines(list(.o$hazard), .predDf)[[1]]
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

#' Rewrite `tteTime(ev)`/`tteCount(ev)` to the endpoint's tte index
#'
#' @param expr quoted model expression
#' @param tte names of the model's tte endpoints
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
    .k <- match(as.character(expr[[2]]), tte)
    if (is.na(.k)) {
      stop("'", .f, "(", as.character(expr[[2]]), ")' does not name a tte() endpoint", call. = FALSE)
    }
    expr[[2]] <- as.numeric(.k)
    return(expr)
  }
  as.call(lapply(as.list(expr), .rxTteRewrite, tte = tte))
}

#' Rewrite the tteTime()/tteCount() calls of a list of model lines
#'
#' @param lines list of quoted lines, or character lines
#' @param predDf the ui's `$predDf`
#' @return the rewritten lines (same type)
#' @noRd
.rxTteRewriteLines <- function(lines, predDf) {
  .tte <- if (is.null(predDf)) character(0) else predDf$cond[predDf$distribution == "tte"]
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
