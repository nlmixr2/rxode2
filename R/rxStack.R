#' Return if the object can be stacked
#'
#' @param object object to test if it can be stacked
#' @return boolean to tell if an object can be stacked using rxode2
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' is.rxStackData(NULL)
is.rxStackData <- function(object) {
  if (!inherits(object, "data.frame")) return(FALSE)
  .mv <- try(.Call(`_rxode2_rxModelVarsStack`, object), silent=TRUE)
  if (!inherits(.mv, "rxModelVars")) return(FALSE)
  .mv <- try(object$rxModelVars, silent=TRUE)
  if (!inherits(.mv, "rxModelVars")) return(FALSE)
  TRUE
}

#' Stack a solved object for things like default ggplot2 plot
#'
#' @param data is a rxode2 object to be stacked.
#'
#' @param vars Variables to include in stacked data; By default this
#'   is all the variables when vars is NULL.
#'
#' When vars is `sim` and comes from a `rxode2` ui simulation with
#' multiple endpoints (ie it has a `CMT` in the simulation), it will
#' rework the data as if it was stacked based the value based on the
#' compartments in the multiple endpoint model.
#'
#' When the vars is `sim.endpoint1` it will subset the stack to
#' endpoint1, you can also have `c("sim.endpoint1", "sim.endpoint2")
#' and the "stack" will subset to endpoint1 and endpoint2.
#'
#' When you specify the `sim` type variables they have to be all
#' prefixed with `sim` otherwise, the stack will not treat them
#' differently.
#'
#' @param doSim boolean that determines if the "sim" variable in a
#'   `rxSolve` dataset is actually "stacking" based on the endpoint
#'   (`TRUE`) or simply treating `sim` as a variable.
#'
#' @return Stacked data with \code{value} and \code{trt}, where value is the values
#'   and \code{trt} is the state and \code{lhs} variables.
#'
#' @author Matthew Fidler
#' @export
rxStack <- function(data, vars = NULL, doSim=TRUE) {
  if (!is.rxStackData(data)) stop("this data cannot be used with `rxStack`", call.=FALSE)
  .nd <- names(data)
  checkmate::assertCharacter(vars, pattern="^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$", null.ok=TRUE)
  if (doSim) {
    .doSim <- all(vapply(vars, function(x) {
      if (x == "sim") return(TRUE)
      if (substr(x, 0, 4) == "sim.") return(TRUE)
      FALSE
    }, logical(1), USE.NAMES=FALSE))
  } else {
    .doSim <- FALSE
  }
  if (length(vars) == 1L && .doSim &&
        any(.nd == "CMT")) {
    .vars <- vapply(vars, function(x) {
      if (x == "sim") return(NA_character_)
      substr(x, 5, nchar(x))
    }, character(1), USE.NAMES=FALSE)
    .vars <- .vars[!is.na(.vars)]
    .mv <- data$rxModelVars
    if (!is.null(.mv)) {
      .ret <- NULL
      if (any(.nd == "sim.id")) {
        .ret <- list(sim.id=data[["sim.id"]])
      }
      if (any(.nd == "id")) {
        .ret <- c(.ret, list(id=data[["id"]]))
      }
      if (any(.nd == "resetno")) {
        .ret <- c(.ret, list(resetno=data[["resetno"]]))
      }
      if (any(.nd == "evid")) {
        .ret <- c(.ret, list(evid=data[["evid"]]))
      }
      if (any(.nd == "amt")) {
        .ret <- c(.ret, list(amt=data[["amt"]]))
      }
      .cmt <- as.integer(data[["CMT"]])
      attr(.cmt, "levels") <- c(.mv$state, .mv$stateExtra)
      attr(.cmt, "class") <- "factor"
      .ret <- c(.ret, list(time=data[["time"]], value=data[["sim"]], trt=.cmt))
      attr(.ret, "row.names") <- c(NA_integer_, length(.cmt))
      attr(.ret, "class") <- "data.frame"
      if (length(.vars) > 0L) {
        .ret <- .ret[.ret$trt %in% .vars,]
      }
      return(.ret)
    }
  }
  rxStack_(data, vars)
}
