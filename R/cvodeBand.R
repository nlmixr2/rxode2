#' Right-hand sides of the d/dt() lines of a model, by state
#'
#' A state with several d/dt() lines (in if/else branches) gets their union.
#'
#' @param model anything `rxNorm()` accepts.
#' @return named character vector of rhs texts.
#' @noRd
.rxDdtRhs <- function(model) {
  .ddt <- character(0)
  .collect <- function(x) {
    if (!is.call(x)) {
      return(invisible(NULL))
    }
    if (
      (identical(x[[1L]], quote(`=`)) || identical(x[[1L]], quote(`<-`))) &&
        .rxIsDdtLhs(x[[2L]])
    ) {
      .nm <- as.character(x[[2L]][[3L]][[2L]])
      .rhs <- deparse1(x[[3L]])
      .ddt[[.nm]] <<- if (.nm %in% names(.ddt)) paste0("(", .ddt[[.nm]], ")+(", .rhs, ")") else .rhs
    } else if (identical(x[[1L]], quote(`if`)) || identical(x[[1L]], quote(`{`))) {
      for (.i in seq_along(x)[-1L]) {
        .collect(x[[.i]])
      }
    }
    invisible(NULL)
  }
  for (.st in .rxNormStatements(model)) {
    .collect(.st)
  }
  .ddt
}

#' Is this the lhs `d/dt(state)`?
#' @noRd
.rxIsDdtLhs <- function(lhs) {
  is.call(lhs) &&
    identical(lhs[[1L]], quote(`/`)) &&
    identical(lhs[[2L]], quote(d)) &&
    is.call(lhs[[3L]]) &&
    identical(lhs[[3L]][[1L]], quote(dt)) &&
    is.name(lhs[[3L]][[2L]])
}

#' Lower and upper bandwidth of a model's ODE Jacobian
#'
#' Each state's d/dt() depends on the states it reaches through the model's
#' assignments; the bandwidths are the largest distances, in state order,
#' below and above the diagonal.
#'
#' @param model rxode2 model.
#' @return integer vector `c(ml, mu)`.
#' @noRd
.rxStateBandwidth <- function(model) {
  .state <- rxModelVars(model)$state
  .ddt <- .rxDdtRhs(model)
  .defs <- .rxModelDefs(model)
  .ml <- 0L
  .mu <- 0L
  for (.i in seq_along(.state)) {
    .rhs <- .ddt[.state[.i]]
    if (is.na(.rhs)) {
      next
    }
    .j <- match(.rxResolveRootVars(.rhs, .defs), .state)
    .j <- .j[!is.na(.j)]
    if (length(.j) == 0L) {
      next
    }
    .ml <- max(.ml, .i - min(.j))
    .mu <- max(.mu, max(.j) - .i)
  }
  c(.ml, .mu)
}

.rxCvodeBandCache <- new.env(parent = emptyenv())

#' Set the CVODE band solver's bandwidth for this solve
#'
#' Only when `cvodeLinSolver = "band"`; the bandwidth is cached by model.
#'
#' @param object rxode2 model.
#' @param ctl solve control list.
#' @return the previous bandwidth, or `NULL` when none was set.
#' @noRd
.rxSetCvodeBand <- function(object, ctl) {
  if (!identical(as.integer(ctl$cvodeLinSolver), 2L)) {
    return(NULL)
  }
  .mv <- rxModelVars(object)
  .key <- paste(.mv$md5, collapse = "")
  .band <- .rxCvodeBandCache[[.key]]
  if (is.null(.band)) {
    # a model whose dependencies cannot be read solves with the dense solver
    .band <- tryCatch(.rxStateBandwidth(object), error = function(e) c(-1L, -1L))
    assign(.key, .band, envir = .rxCvodeBandCache)
  }
  .Call(`_rxode2_setCvodeBand`, .band[1L], .band[2L])
}

#' Restore the CVODE band solver's bandwidth
#'
#' @param band previous bandwidth from `.rxSetCvodeBand()`.
#' @noRd
.rxRestoreCvodeBand <- function(band) {
  .Call(`_rxode2_setCvodeBand`, band[1L], band[2L])
  invisible(NULL)
}
