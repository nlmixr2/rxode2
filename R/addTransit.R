#' To add transit compartments to the model
#'
#' @param ui The model as a function
#' @param ntransit the number of transit compartments to be added
#' @param transit the transit compartment prefix
#' @param ktr the parameter name for the transit compartment rate
#' @param model Deprecated alias for \code{ui}. Supplying \code{model}
#'   instead of \code{ui} still works but emits a deprecation warning.
#' @inheritParams addDepot
#' @return a model with transit compartment added
#'
#' This matches
#'
#' `dose->a0->a1->abs cmt->central`
#'
#' But `a0` is depot so dosing records labeled depot do not need to be
#' changed
#'
#' The abs cmt becomes the last "transit" compartment
#'
#' This is simply for convenience
#'
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
#' pk1cmtDes |> addTransit(3)
addTransit <- function(
  ui,
  ntransit,
  central = "central",
  depot = "depot",
  transit = "transit",
  ktr = "ktr",
  ka = "ka",
  model
) {
  .useModelAsUi()
  checkmate::assertIntegerish(ntransit, lower = 1)
  assertCompartmentName(transit)
  .ui <- assertRxUi(ui)
  assertCompartmentExists(.ui, central)
  .mv <- rxModelVars(.ui)
  if (!testCompartmentExists(.ui, depot)) {
    .ui <- addDepot(.ui, central = central, depot = depot, ka = ka)
    .mv <- rxModelVars(.ui)
    warning("'", depot, "' added to model for transit model", call. = FALSE)
  } else if (testCompartmentExists(.ui, paste0(transit, "1"))) {
    .ui <- removeTransit(ui, central = central, depot = depot, transit = transit, ktr = ktr, ka = ka)
  }
  assertCompartmentExists(.ui, depot)

  # Extract model and central ODE
  .tmp <- .getEtaTheta(.ui)
  .iniDf <- .tmp$iniDf
  .theta <- .tmp$theta
  .eta <- .tmp$eta
  .modelLines <- .ui$lstExpr
  # Get the central ODE and modify the depot expression to a transit
  # expression
  .w <- .whichDdt(.modelLines, central)
  .tmp <- .extractModelLinesAtW(.modelLines, .w)
  .pre <- .tmp$pre
  .central <- .replaceMult(.tmp$w, v1 = depot, v2 = ka, ret = paste0(ka, "*", transit, ntransit))
  .post <- .tmp$post
  .v <- seq_len(ntransit)
  # ODEs for the transit compartment (except the one from the depot)
  .transMid <- lapply(.v, function(i) {
    if (i == 1) {
      str2lang(paste0(
        "d/dt(",
        transit,
        i,
        ")<- ",
        ktr,
        "*",
        depot,
        "-",
        ifelse(ntransit == 1, ka, ktr),
        "*",
        transit,
        i
      ))
    } else if (i == ntransit) {
      str2lang(paste0("d/dt(", transit, i, ")<- ", ktr, "*", transit, i - 1, "-", ka, "*", transit, i))
    } else {
      str2lang(paste0("d/dt(", transit, i, ")<- ", ktr, "*", transit, i - 1, "-", ktr, "*", transit, i))
    }
  })
  # combine the lines for now
  .modelLines <- c(.pre, .transMid, .central, .post)

  # Insert ktr <- exp(lktr) immediately before d/dt(depot) and modify
  # d/dt(depot) in place so every pre-existing line (including any
  # interleaved residual-error ~ specs) keeps its original position.
  .w <- .whichDdt(.modelLines, depot)
  .modRep <- .replaceMult(
    .modelLines[[.w]],
    v1 = ka,
    v2 = depot,
    ret = paste0(ktr, "*", depot)
  )
  .before <- if (.w > 1L) .modelLines[seq_len(.w - 1L)] else list()
  .after <- if (.w < length(.modelLines)) .modelLines[(.w + 1L):length(.modelLines)] else list()
  .modelLines <- c(
    .before,
    list(str2lang(paste0(ktr, " <- exp(l", ktr, ")"))),
    .modRep,
    .after
  )
  .ui <- rxUiDecompress(.ui)
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }

  # modify model block
  model(.ui) <- .modelLines
  .iniAddTheta(.ui, paste0("l", ktr), label = paste0("First order transition rate (", ktr, ")"))
}

#' To remove transit compartments from the model
#' @param ui The model as a function
#' @param transit The number of transit compartments to remove
#' @inheritParams addDepot
#' @inheritParams addTransit
#' @return rxode2 model with transit compartment removed
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
#'
#' # In this example the transit is added and then a few are removed
#'
#' pk1cmtDes |> addTransit(4) |> removeTransit(3)
#'
#' pk1cmtDes |> addTransit(4) |> removeTransit()
removeTransit <- function(
  ui,
  ntransit,
  central = "central",
  depot = "depot",
  transit = "transit",
  ktr = "ktr",
  ka = "ka",
  model
) {
  .useModelAsUi()
  if (!missing(ntransit)) {
    checkmate::assertIntegerish(ntransit, lower = 1, any.missing = FALSE)
  }
  .ui <- assertRxUi(ui)
  assertCompartmentExists(.ui, central)
  assertCompartmentExists(.ui, depot)
  assertCompartmentExists(.ui, paste0(transit, "1"))
  assertVariableName(ktr)
  .mv <- rxModelVars(.ui)
  .transitCmts <- .mv$state
  # numbered compartments only: a second chain with a letter-tagged
  # prefix (transitB1..N) must not be read as part of this one
  .transitCmts <- .transitCmts[grepl(paste0("^", transit, "[0-9]+$"), .transitCmts)]
  .nc <- nchar(transit) + 1
  .totTransit <- max(vapply(
    .transitCmts,
    function(n) {
      as.integer(substr(n, .nc, nchar(n)))
    },
    integer(1),
    USE.NAMES = FALSE
  ))
  if (!missing(ntransit)) {
    checkmate::assertIntegerish(ntransit, lower = 1, any.missing = FALSE, len = 1)
  } else {
    ntransit <- .totTransit
  }
  if (ntransit > .totTransit) {
    warning("reset ntransit to ", .totTransit, call. = FALSE)
    ntransit <- .totTransit
  }
  .ui <- rxUiDecompress(.ui)
  if (ntransit == .totTransit) {
    # remove all
    .tmp <- .getEtaTheta(.ui)
    .iniDf <- .tmp$iniDf
    .eta <- .tmp$eta
    .theta <- .tmp$theta
    .theta <- .dropTheta(.theta, ktr)
    .eta <- .dropEta(.eta, ktr)

    .rm <- seq_len(.totTransit)
    .transit <- paste0(transit, .rm)
    .modelLines <- .rmDdt(.ui$lstExpr, .transit)
    .w <- .whichDdt(.modelLines, central)
    .tmp <- .extractModelLinesAtW(.modelLines, .w)
    .tmp$w <- .replaceMult(.tmp$w, v1 = paste0(transit, .totTransit), v2 = ka, ret = paste0(ka, "*", depot))
    .tmp$pre <- .replaceMult(.tmp$pre, v1 = depot, v2 = ktr, ret = paste0(ka, "*", depot))
    .modelLines <- c(.tmp$pre, .tmp$w, .tmp$post)
    .tmp <- .dropLines(.ui, .modelLines, .theta, .eta, ktr)
    .modelLines <- .tmp$modelLines
    .theta <- .tmp$theta
    .eta <- .tmp$eta
    .ui$iniDf <- rbind(.theta, .eta)
  } else {
    # remove some, but not all
    .ftransit <- .totTransit - ntransit
    .transit <- paste0(transit, seq_len(.totTransit)[-seq_len(.ftransit)])
    .modelLines <- .rmDdt(.ui$lstExpr, .transit)
    .w <- .whichDdt(.modelLines, central)
    .tmp <- .extractModelLinesAtW(.modelLines, .w)
    .tmp$w <- .replaceMult(
      .tmp$w,
      v1 = paste0(transit, .totTransit),
      v2 = ka,
      ret = paste0(ka, "*", transit, .ftransit)
    )
    .tmp$pre <- .replaceMult(
      .tmp$pre,
      v1 = paste0(transit, ntransit),
      v2 = ktr,
      ret = paste0(ka, "*", transit, .ftransit)
    )
    .modelLines <- c(.tmp$pre, .tmp$w, .tmp$post)
  }
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- .modelLines
  rxUiCompress(.ui)
}
