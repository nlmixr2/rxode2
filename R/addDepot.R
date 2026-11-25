#' To convert from infusion/intravenous administration to first-order oral
#' absorption
#'
#' @param ui The model as a function (or something convertible to an rxUi
#'   object)
#' @param central central compartment name
#' @param depot depot compartment name
#' @param ka absorption rate parameter name
#' @param model Deprecated alias for \code{ui}. Supplying \code{model}
#'   instead of \code{ui} still works but emits a deprecation warning.
#' @return a model with the depot added
#' @family Absorption building blocks
#' @export
#' @examples
#' pk2cmtNoDepot <- function() {
#'   ini({
#'     lcl <- 1; label("Clearance (CL)")
#'     lvc <- 3; label("Central volume of distribution (V)")
#'     lvp <- 5; label("Peripheral volume of distribution (Vp)")
#'     lq <- 0.1; label("Intercompartmental clearance (Q)")
#'     propSd <- 0.5; label("Proportional residual error (fraction)")
#'   })
#'   model({
#'     cl <- exp(lcl)
#'     vc <- exp(lvc)
#'     vp <- exp(lvp)
#'     q <- exp(lq)
#'     kel <- cl / vc
#'     k12 <- q / vc
#'     k21 <- q / vp
#'     d/dt(central) <- -kel * central - k12 * central + k21 * peripheral1
#'     d/dt(peripheral1) <- k12 * central - k21 * peripheral1
#'     Cc <- central / vc
#'     Cc ~ prop(propSd)
#'   })
#' }
#'
#' # most of the examples in the model library already have a depot
#' # the PK_2cmt_no_depot is an exception
#' pk2cmtNoDepot |> addDepot()
addDepot <- function(ui, central = "central", depot = "depot", ka = "ka", model) {
  .useModelAsUi()
  .ui <- assertRxUi(ui)
  assertCompartmentName(depot)
  assertCompartmentExists(.ui, central)
  assertVariableName(ka)
  .mv <- rxModelVars(.ui)
  # Insert the new lines immediately before d/dt(central) and modify
  # d/dt(central) in place so every pre-existing line keeps its original
  # position (and any interleaved residual-error ~ lines continue to
  # capture their source-order semantics).
  .modelLines <- .ui$lstExpr
  .w <- .whichDdt(.modelLines, central)
  .modLine <- str2lang(paste0(deparse1(.modelLines[[.w]]), "+", ka, "*", depot))
  .newLines <- list(
    str2lang(paste0(ka, " <- exp(l", ka, ")")),
    str2lang(paste0("d/dt(", depot, ") <- -", ka, "*", depot))
  )
  .before <- if (.w > 1L) .modelLines[seq_len(.w - 1L)] else list()
  .after <- if (.w < length(.modelLines)) .modelLines[(.w + 1L):length(.modelLines)] else list()
  .modelLines <- c(.before, .newLines, list(.modLine), .after)

  .ui <- rxUiDecompress(.ui)
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- .modelLines
  .ui <- .iniAddTheta(.ui, paste0("l", ka), label = paste0("First order absorption rate (", ka, ")"))
  rxUiCompress(as.rxUi(.ui))
}


#' To convert from first order oral absorption to IV/Intravenous
#'
#' Any compartment property lines for the depot (\code{f()},
#' \code{lag()}, \code{dur()}, \code{rate()} or an initial condition)
#' are removed with its differential equation; the variables that
#' defined them are left in place (use [removeLinesAndInis()] to drop
#' them as well).
#'
#' @inheritParams addDepot
#' @return Returns a model with the depot from a first order absorption model removed
#' @family Absorption building blocks
#' @export
#' @examples
#' pk1cmtDes <- function() {
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
#'     kel <- cl / vc
#'     d/dt(depot) <- -ka * depot
#'     d/dt(central) <- ka * depot - kel * central
#'     Cc <- central / vc
#'     Cc ~ prop(propSd)
#'   })
#' }
#'
#' pk1cmtDes |> removeDepot()
removeDepot <- function(ui, central = "central", depot = "depot", ka = "ka", model) {
  .useModelAsUi()
  .ui <- assertRxUi(ui)
  assertCompartmentExists(.ui, central)
  assertCompartmentExists(.ui, depot)
  assertVariableName(ka)
  .modelLines <- .rmDdt(.ui$lstExpr, depot)
  .modelLines <- .rmCmtPropLines(.modelLines, depot)
  .w <- .whichDdt(.modelLines, central)
  .tmp <- .extractModelLinesAtW(.modelLines, .w)
  .tmp$w <- .dropDotAddExpr(.replaceMult(.tmp$w, ka, depot, "."))
  .modelLines <- c(.tmp$pre, .tmp$w, .tmp$post)
  .tmp <- .getEtaTheta(.ui)
  .iniDf <- .tmp$iniDf
  .eta <- .tmp$eta
  .theta <- .tmp$theta
  .theta <- .dropTheta(.theta, ka)
  .eta <- .dropEta(.eta, ka)
  .tmp <- .dropLines(.ui, .modelLines, .theta, .eta, ka)
  .modelLines <- .tmp$modelLines
  .theta <- .tmp$theta
  .eta <- .tmp$eta
  .ui <- rxUiDecompress(.ui)
  .ui$iniDf <- rbind(.theta, .eta)
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- .modelLines
  rxUiCompress(.ui)
}
