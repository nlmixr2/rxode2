#' Remove all the d/dt(cmts) in modelLines
#'
#' @param modelLines list of model lines to modify
#' @param cmts compartment names to remove
#' @return modelLines with compartments removed
#' @noRd
#' @author Matthew L. Fidler
.rmDdt <- function(modelLines, cmts) {
  .w <- which(vapply(
    seq_along(modelLines),
    function(i) {
      .cur <- modelLines[[i]]
      any(vapply(
        cmts,
        function(cmt) {
          .ddtCentral1 <- str2lang(paste0("d/dt(", cmt, ") <- ."))
          .ddtCentral2 <- str2lang(paste0("d/dt(", cmt, ") = ."))
          .matchesLangTemplate(.cur, .ddtCentral1) ||
            .matchesLangTemplate(.cur, .ddtCentral2)
        },
        logical(1),
        USE.NAMES = FALSE
      ))
    },
    logical(1),
    USE.NAMES = FALSE
  ))
  lapply(seq_along(modelLines)[-.w], function(i) {
    modelLines[[i]]
  })
}

#' Remove all the compartment property lines in modelLines
#'
#' Drops \code{f(cmt)}, \code{lag(cmt)}, \code{alag(cmt)},
#' \code{dur(cmt)}, \code{rate(cmt)} and \code{cmt(0)} assignments for
#' the given compartments.  These become syntax errors once the
#' compartment's \code{d/dt()} is removed, so dropping a compartment
#' has to drop its properties too.
#'
#' @param modelLines list of model lines to modify
#' @param cmts compartment names whose property lines are removed
#' @return modelLines with the compartment property lines removed
#' @noRd
.rmCmtPropLines <- function(modelLines, cmts) {
  # rxode2 accepts F() as an alias of f() and lstExpr keeps the
  # spelling, so both must be enumerated
  .props <- c("f", "F", "lag", "alag", "dur", "rate")
  .exprs <- unlist(
    lapply(cmts, function(cmt) {
      c(
        lapply(.props, function(p) {
          str2lang(paste0(p, "(", cmt, ") <- ."))
        }),
        lapply(.props, function(p) {
          str2lang(paste0(p, "(", cmt, ") = ."))
        }),
        list(str2lang(paste0(cmt, "(0) <- .")), str2lang(paste0(cmt, "(0) = .")))
      )
    }),
    recursive = FALSE
  )
  .w <- which(vapply(
    seq_along(modelLines),
    function(i) {
      .cur <- modelLines[[i]]
      any(vapply(
        .exprs,
        function(e) {
          .matchesLangTemplate(.cur, e)
        },
        logical(1),
        USE.NAMES = FALSE
      ))
    },
    logical(1),
    USE.NAMES = FALSE
  ))
  if (length(.w) == 0L) {
    return(modelLines)
  }
  lapply(seq_along(modelLines)[-.w], function(i) {
    modelLines[[i]]
  })
}
