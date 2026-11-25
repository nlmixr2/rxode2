#' Add a population parameter through rxode2's piping interface
#'
#' `model()` leaves a parameter that the new model lines reference,
#' but that the `ini()` block does not yet define, as a covariate.
#' `ini()` then promotes it to a population parameter and sets its
#' estimate, bounds and label.
#'
#' Going through those two interfaces -- rather than building an `iniDf` row
#' by hand and `rbind()`ing it onto `ui$iniDf` -- keeps this package clear of
#' the initial-estimate data frame's column set.  That column set belongs to
#' lotri, not to us, and it changes: lotri 1.0.5 added a `prior` column, which
#' is exactly the kind of drift that breaks a hand-built row.  It also avoids
#' inheriting `backTransform`, `condition` or `prior` from whatever unrelated
#' parameter happened to be the row the template was copied from.
#'
#' The model must already reference `name`, so call this after the
#' `model()` update that introduces it.
#'
#' @param ui rxode2 ui object whose model already references `name`
#' @param name parameter name to promote and set
#' @param est initial estimate.  `NULL` leaves whatever estimate rxode2
#'   assigned when the parameter was created, and leaves the bounds alone.
#' @param lower,upper bounds for the estimate.  Left at `-Inf`/`Inf` the
#'   bounds are not mentioned at all, so a bound rxode2 set itself (a
#'   residual error parameter's `lower = 0`, for instance) survives.
#' @param label parameter label; `NA_character_` leaves it unset
#' @return `ui` with `name` promoted to a population parameter
#' @noRd
.iniAddTheta <- function(ui, name, est = 0.1, lower = -Inf, upper = Inf, label = NA_character_) {
  checkmate::assertString(name, min.chars = 1L)
  # a label can arrive carrying the name of the vector element it came from
  # (ifelse() keeps names); label() wants a bare string
  label <- unname(label)
  checkmate::assertCharacter(label, len = 1L)
  .exprs <- list()
  if (!is.null(est)) {
    if (is.infinite(lower) && lower < 0 && is.infinite(upper) && upper > 0) {
      .exprs <- c(.exprs, list(str2lang(paste0(name, " <- ", deparse1(est)))))
    } else {
      .exprs <- c(
        .exprs,
        list(str2lang(paste0(
          name,
          " <- c(",
          deparse1(lower),
          ", ",
          deparse1(est),
          ", ",
          deparse1(upper),
          ")"
        )))
      )
    }
  }
  if (!is.na(label)) {
    .exprs <- c(
      .exprs,
      list(str2lang(paste0(
        name,
        " <- label(",
        deparse1(label),
        ")"
      )))
    )
  }
  if (length(.exprs) == 0L) {
    return(ui)
  }
  # suppressMessages(): ini() announces "promote <x> to population parameter"
  # and "change initial estimate of <x>" for every parameter it takes. The
  # caller here is a function whose whole job is to add that parameter, so the
  # announcement is not news to anyone -- but it is two lines per parameter,
  # and it made the test suite's output several times longer than the suite's
  # own report. The iniDf rbind() this replaced was silent.
  suppressMessages(do.call(ini, c(list(ui), .exprs)))
}

#' Append model lines through rxode2's piping interface
#'
#' `model(ui) <- lines` replaces the whole model block and rejects an
#' endpoint whose residual parameter is not already estimated.  Appending the
#' endpoint line on its own instead lets rxode2 create that parameter, with
#' the `condition`, `err` and `lower = 0` it decides on.
#'
#' @param ui rxode2 ui object
#' @param lines list of model lines (language objects) to append
#' @return `ui` with `lines` appended to the model block
#' @noRd
.modelAppend <- function(ui, lines) {
  for (.line in lines) {
    # quiet for the same reason as .iniAddTheta(): appending an endpoint
    # announces the residual parameter it creates, which the caller asked for
    ui <- suppressMessages(do.call(model, list(ui, .line, append = TRUE)))
  }
  ui
}
