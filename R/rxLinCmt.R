#' Find the assignments in R expression
#'
#' @param x R expression
#' @return list of assigned parameters
#' @author Hadley Wickham and Matthew L. Fidler
#' @keywords internal
#' @export
findLhs <- function(x) {
  ## Modified from http://adv-r.had.co.nz/Expressions.html find_assign4
  if (is.atomic(x) || is.name(x)) {
    character()
  } else if (is.call(x)) {
    if ((identical(x[[1]], quote(`<-`)) ||
      identical(x[[1]], quote(`=`)) ||
      identical(x[[1]], quote(`~`))) &&
      is.name(x[[2]])) {
      .lhs <- as.character(x[[2]])
    } else {
      .lhs <- character()
    }
    unique(c(.lhs, unlist(lapply(x, rxode2::findLhs))))
  } else if (is.pairlist(x)) {
    unique(unlist(lapply(x, rxode2::findLhs)))
  } else {
    stop(sprintf("do not know how to handle type '%s'", typeof(x)),
      call. = FALSE
    )
  }
}

#' Get the linear compartment model true function
#'
#' @inheritParams rxode2
#' @return model with linCmt() replaced with linCmtA()
#' @author Matthew Fidler
#' @export
rxGetLin <- function(model, linCmtSens = c("linCmtA", "linCmtB", "linCmtC"), verbose = FALSE) {
  .mv <- rxGetModel(model)
  if (.Call(`_rxode2_isLinCmt`) == 1L) {
    .vars <- c(.mv$params, .mv$lhs, .mv$slhs)
    return(.Call(
      `_rxode2_linCmtGen`,
      length(.mv$state),
      .vars,
      setNames(
        c(
          "linCmtA" = 1L, "linCmtB" = 2L,
          "linCmtC" = 3L
        )[match.arg(linCmtSens)],
        NULL
      ), verbose
    ))
  } else {
    return(model)
  }
}
