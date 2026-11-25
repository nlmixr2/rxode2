#' Add residual error to a model
#'
#' @details For \code{reserr}, the parameter will be named with the dependent
#'   variable from the model as a prefix.  For example, if the dependent
#'   variable in the model is \code{Cc}, the parameter name for \code{propSd}
#'   will become \code{CcpropSd}.
#'
#' @param ui The model as a function
#' @param reserr The type or types of residual error (currently
#'   \code{"addSd"}, \code{"propSd"}, and \code{"lnormSd"} are
#'   accepted)
#' @param endpoint the endpoint to apply the error; will default to
#'   the first error in the model
#' @param model Deprecated alias for \code{ui}. Supplying \code{model}
#'   instead of \code{ui} still works but emits a deprecation warning.
#' @return The model with residual error modified
#' @examples
#' pk1cmt <- function() {
#'   ini({
#'     lka <- 0.45; label("Absorption rate (Ka)")
#'     lcl <- 1; label("Clearance (CL)")
#'     lvc <- 3.45; label("Central volume of distribution (V)")
#'     propSd <- 0.5; label("Proportional residual error (fraction)")
#'   })
#'   model({
#'     ka <- exp(lka)
#'     cl <- exp(lcl)
#'     vc <- exp(lvc)
#'     Cc <- linCmt()
#'     Cc ~ prop(propSd)
#'   })
#' }
#'
#' library(rxode2)
#' pk1cmt |> addResErr("addSd")
#' pk1cmt |> addResErr("lnormSd")
#' pk1cmt |> addResErr(c("addSd", "propSd"))
#' @family Parameter and naming building blocks
#' @export
addResErr <- function(ui, reserr, endpoint, model) {
  .useModelAsUi()
  modelUi <- mod <- assertRxUi(ui)
  assertRxUiPrediction(ui) # needs to have a prediction
  if (missing(endpoint)) {
    paramErr <- modelUi$predDf$cond
    if (length(paramErr) > 1) {
      stop("multiple endpoints detected, choose one: ", paste(paramErr, collapse = ", "))
    }
  } else {
    assertVariableName(endpoint)
    if (endpoint %in% modelUi$predDf$cond) {
      paramErr <- endpoint
    } else {
      stop(
        "requested to add/change residual error for '",
        endpoint,
        "' but not defined as a modeled endpoint",
        call. = FALSE
      )
    }
  }
  if ("rxLinCmt" %in% paramErr) {
    paramErr[paramErr %in% "rxLinCmt"] <- "linCmt()"
  }
  checkmate::assert_character(paramErr, len = 1, min.chars = 1)
  errFunMap <-
    c(
      addSd = "add(%s)",
      propSd = "prop(%s)",
      lnormSd = "lnorm(%s)"
    )
  defaultIniEst <- c(addSd = 1, propSd = 0.5, lnormSd = 0.5)
  # Confirm that the code is up to date
  stopifnot(length(errFunMap) == length(defaultIniEst))
  stopifnot(all(names(errFunMap) %in% names(defaultIniEst)))
  if (is.character(reserr)) {
    checkmate::assert_subset(reserr, choices = names(defaultIniEst), empty.ok = FALSE)
    reserr <- defaultIniEst[reserr]
  } else if (!is.numeric(reserr)) {
    cli::cli_abort("reserr must be a character string or a named numeric vector")
  }
  checkmate::assert_numeric(reserr, min.len = 1, lower = 0, finite = TRUE, any.missing = FALSE)
  checkmate::assert_names(names(reserr), subset.of = names(errFunMap))

  newErrLineRhs <-
    paste(
      sprintf(errFunMap[names(reserr)], combinePaste2(paramErr, names(reserr))),
      collapse = " + "
    )
  newErrLine <- sprintf("%s ~ %s", paramErr, newErrLineRhs)
  newIniEst <- reserr

  names(newIniEst) <- combinePaste2(paramErr, names(reserr))

  # Update the model with the new residual error line and the new initial
  # estimates
  # rxode2::model: the deprecated `model` argument shadows the function here
  modelUi <- do.call(rxode2::model, list(modelUi, str2lang(newErrLine)))
  modelUi <- ini(modelUi, newIniEst)
  # Return the model function or ui with props
  rxode2(mod) <- modelUi$fun
  mod
}
