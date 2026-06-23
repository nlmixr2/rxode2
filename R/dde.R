## Delay differential equation (DDE) support helpers for forward sensitivities.
##
## Forward sensitivities of a model containing delay(state, T) require the
## "variational" delayed term  d f_i / d[delay(y_j, T)] * delay(S_j, T)  added to
## each sensitivity ODE (see ~/src/rxode2-dde-sensitivity-plan.md).  These helpers
## catalog the delayed terms in a model and resolve whether a delay duration
## depends on the parameters sensitivities are taken with respect to.

#' Catalog the delay(state, T) terms in a model
#'
#' Walks the normalized-model AST and returns one row per distinct delayed term
#' (state argument + delay-duration expression text), with a surrogate symbol
#' name used when generating sensitivities.  The surrogate lets symengine treat a
#' delayed value as an independent variable so the delayed Jacobian
#' d f / d[delay(state, T)] can be differentiated normally.
#'
#' @param model anything `rxNorm()` accepts (rxode2 model, ui, model vars).
#' @return `NULL` when the model has no delay() terms, otherwise a data.frame
#'   with columns `state`, `tau` (duration expression text) and `surrogate`.
#' @author Matthew L. Fidler
#' @keywords internal
#' @noRd
.rxDelayTerms <- function(model) {
  .norm <- rxNorm(model)
  .e <- parse(text = .norm)
  .found <- list()
  .walk <- function(x) {
    if (is.call(x)) {
      if (identical(x[[1]], quote(delay)) && length(x) == 3L) {
        .found[[length(.found) + 1L]] <<- list(
          state = deparse1(x[[2]]),
          tau = deparse1(x[[3]])
        )
      }
      for (.i in seq_along(x)) .walk(x[[.i]])
    }
  }
  for (.i in seq_along(.e)) .walk(.e[[.i]])
  if (length(.found) == 0L) {
    return(NULL)
  }
  .df <- do.call(rbind, lapply(.found, function(z) {
    data.frame(state = z$state, tau = z$tau, stringsAsFactors = FALSE)
  }))
  .df <- unique(.df)
  rownames(.df) <- NULL
  .df$surrogate <- paste0("rx__dly_", .df$state, "_", seq_len(nrow(.df)), "__")
  .df
}

#' Named list of explicit assignments (lhs = rhs) in a model
#'
#' Skips ODE (`d/dt(...)`) and compartment-property lines; used to resolve a
#' delay-duration expression down to its root symbols.
#'
#' @param model anything `rxNorm()` accepts.
#' @return named character vector mapping each assigned lhs to its rhs text.
#' @noRd
.rxModelDefs <- function(model) {
  .norm <- rxNorm(model)
  .e <- parse(text = .norm)
  .defs <- character(0)
  for (.i in seq_along(.e)) {
    .st <- .e[[.i]]
    if (is.call(.st) && (identical(.st[[1]], quote(`=`)) ||
                           identical(.st[[1]], quote(`<-`)))) {
      .lhs <- .st[[2]]
      ## only simple `name = rhs` assignments (skip d/dt(x), f(x), etc.)
      if (is.name(.lhs)) {
        .defs[[as.character(.lhs)]] <- deparse1(.st[[3]])
      }
    }
  }
  .defs
}

#' Root symbols an expression depends on, resolving through model definitions
#'
#' Transitively expands the free variables of `exprText` through the model's
#' explicit assignments so, e.g., `tau` defined as `exp(eta_tau)` resolves to
#' `eta_tau`.
#'
#' @param exprText expression text (e.g. a delay duration).
#' @param defs named vector from [.rxModelDefs()].
#' @return character vector of root symbol names.
#' @noRd
.rxResolveRootVars <- function(exprText, defs) {
  .seen <- character(0)
  .roots <- character(0)
  .stack <- all.vars(parse(text = exprText))
  while (length(.stack) > 0L) {
    .v <- .stack[[1L]]
    .stack <- .stack[-1L]
    if (.v %in% .seen) next
    .seen <- c(.seen, .v)
    if (.v %in% names(defs)) {
      .stack <- c(.stack, all.vars(parse(text = defs[[.v]])))
    } else {
      .roots <- c(.roots, .v)
    }
  }
  unique(.roots)
}

#' Validate that delay durations do not depend on the sensitivity parameters
#'
#' v1 forward sensitivities require `d tau / d p == 0` for every sensitivity
#' parameter `p` (a parameter- or eta-dependent delay needs the delayed-RHS term,
#' deferred to v2).  Errors with a clear message naming the offending term.
#'
#' @param model anything `rxNorm()` accepts.
#' @param params character vector of parameters sensitivities are taken w.r.t.
#' @param terms optional pre-computed [.rxDelayTerms()] result.
#' @return invisibly `TRUE` when valid; otherwise stops.
#' @noRd
.rxDelayValidateTau <- function(model, params, terms = NULL) {
  if (is.null(terms)) terms <- .rxDelayTerms(model)
  if (is.null(terms)) {
    return(invisible(TRUE))
  }
  .defs <- .rxModelDefs(model)
  for (.i in seq_len(nrow(terms))) {
    .roots <- .rxResolveRootVars(terms$tau[.i], .defs)
    .bad <- intersect(.roots, params)
    if (length(.bad) > 0L) {
      stop("delay duration 'delay(", terms$state[.i], ", ", terms$tau[.i],
           ")' depends on the sensitivity parameter(s) ",
           paste(.bad, collapse = ", "),
           "; parameter-dependent delays are not yet supported for sensitivities",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}
