#' @export
rxUdfUi.mix <- function(fun) {
  eval(.fun)
}

.mixUi <- new.env(parent=emptyenv())
.mixUi$narg <-
#' This standardizes the mixture calls to what is needed in lower level rxode2
#'
#' @param ... parameters for the mixture model.
#'
#' @details
#'
#' The structure of the mix() model call within rxode2 ui models can be as follows:
#'
#'  `f = mix(f1, p1, f2, p2, f3)`
#'
#' Here `f` can be f1 (with probability f1), f2 (with probability f2),
#' or f3 (with probability 1-p1-p2).
#'
#' For all subsequent calls to `mix()` the number of populations and
#' probabilities need to match because only one mixture can be applied
#' per problem.  In this case you can call:
#'
#' `cl = mix(cl1, p1, cl2, p2, cl3)`
#'
#' or
#'
#' `cl = mix(cl1, cl2, cl3)`
#'
#'
#' These are handled with this `mix()` function and the code for the
#' lower-level `rxode2` will be used.
#'
#' In this case each `mix()` call will have an even number of arguments and the
#' probabilities will match.
#'
#' In the rxode2 ui, the probabilities have to be in the initial
#' estimates block and not anywhere else in the model.
#'
#' @return lower level rxode2 `mix()` function.
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' mix(f1, p1, f2, p2, f3)
#'
mix <- function(...) {
  .lst <- list(...)
  # For the first call to mix, we need an odd number of arguments
  if (identical(rxUdfUiNum(), 1L) &&
        is.null(rxUdfUiMv())) {
    # If this is the first call of mix()
    if (length(.lst) %% 2 == 0) {
      stop("mix() must have an odd number of arguments on the first call", call. = FALSE)
    } else {
      .mixUi$narg <- length(.lst)
      .mixUi$narg2 <- (.mixUi$narg + 1L) / 2L
    }
  } else {
    if (length(.lst) == .mixUi$narg) {
      # return as is
    } else if (length(.lst) == .mixUi$narg2) {
      # Here enhance list with extra probability arguments

    } else {
      stop("mix() must have ", .mixUi$narg, " or ", .mixUi$narg2, " arguments after first call",
           call. = FALSE)
    } else {

    }
  }
}
