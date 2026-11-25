#' Figures out what line d/dt(central) is on
#'
#' @param modelLines modelLines expression list
#' @param central name of central compartment
#' @param start is the start of the expression, by default it is "d/dt("
#' @param end is the end of the expression, by default it is ")"
#' @return which item in modelLines is the central compartment (or
#'   error if there is multiple lines)
#' @noRd
#' @author Matthew L. Fidler
.whichDdt <- function(modelLines, central, start = "d/dt(", end = ")") {
  .dd1 <- start
  .dd2 <- end
  .ddtCentral1 <- str2lang(paste0(.dd1, central, .dd2, " <- ."))
  .ddtCentral2 <- str2lang(paste0(.dd1, central, .dd2, " = ."))
  .w <- which(vapply(
    seq_along(modelLines),
    function(i) {
      .cur <- modelLines[[i]]
      .matchesLangTemplate(.cur, .ddtCentral1) ||
        .matchesLangTemplate(.cur, .ddtCentral2)
    },
    logical(1),
    USE.NAMES = FALSE
  ))
  # Modify ODE for central compartment
  if (length(.w) != 1) {
    stop("'", .dd1, central, .dd2, "' not found or duplicated in model", call. = FALSE)
  }
  .w
}
