#' Fake blank Cc for creating PD only models
#'
#' @param fun function to that requires Cc
#' @param ... arguments sent PD function
#' @param cc character name of the concentration in the central
#'   compartment that will be faked to allow models that require Cc to
#'   change to models with Cc as a covariate
#' @return Model where Cc is a covariate
#' @family Pharmacodynamic building blocks
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' fakeCc(addDirectLin) |> convertEmaxHill()
#'
fakeCc <- function(fun, ..., cc = "Cc") {
  checkmate::assertCharacter(cc, any.missing = FALSE, min.len = 1)
  if (length(cc) > 0) {
    cc <- cc[length(cc)]
  }
  fun <- as.character(substitute(fun))
  .fun <- try(get(fun, mode = "function"), silent = TRUE)
  if (inherits(.fun, "try-error")) {
    stop(paste0(fun, " is not a function"))
  }
  .f <- paste0(cc, " <- NA\n")
  .f <- suppressMessages(as.rxUi(rxModelVars(.f)))
  .cc <- .fun(ui = .f, ..., cc = cc)
  .modelLines <- .cc$lstExpr
  .w <- .whichDdt(.modelLines, cc, start = "", end = "")
  .f <- rxode2(.f)
  .tmp <- .extractModelLinesAtW(.modelLines, .w)
  model(.cc) <- c(.tmp$pre, .tmp$post)
  .cc
}
