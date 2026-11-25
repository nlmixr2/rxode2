#' Add a property to a compartment
#'
#' @param ui rxode2 ui object
#' @param prop property to add to a compartment:
#'
#'  - \code{F}: bioavailability
#'
#'  - \code{lag}: absorption lag time
#'
#' - \code{dur}: modeled duration of infusion
#'
#' - \code{rate}: modeled infusion rate
#'
#' - \code{ini}: initial value of the compartment
#'
#' @param cmt compartment to apply the property to
#'
#' @return rxode2 ui object with property applied
#' @family Distribution and elimination building blocks
#' @export
#' @author Matthew L. Fidler
#' @examples
#' pk3cmtDes <- function() {
#'   ini({
#'     lka <- 0.45; label("Absorption rate (Ka)")
#'     lcl <- 1; label("Clearance (CL)")
#'     lvc <- 3; label("Central volume of distribution (V)")
#'     lvp <- 5; label("Peripheral volume of distribution (Vp)")
#'     lvp2 <- 8; label("Second peripheral volume of distribution (Vp2)")
#'     lq <- 0.1; label("Intercompartmental clearance (Q)")
#'     lq2 <- 0.5; label("Second intercompartmental clearance (Q2)")
#'     propSd <- 0.5; label("Proportional residual error (fraction)")
#'   })
#'   model({
#'     ka <- exp(lka)
#'     cl <- exp(lcl)
#'     vc <- exp(lvc)
#'     vp <- exp(lvp)
#'     vp2 <- exp(lvp2)
#'     q <- exp(lq)
#'     q2 <- exp(lq2)
#'     kel <- cl / vc
#'     k12 <- q / vc
#'     k21 <- q / vp
#'     k13 <- q2 / vc
#'     k31 <- q2 / vp2
#'     d/dt(depot) <- -ka * depot
#'     d/dt(central) <- ka * depot - kel * central - k12 * central +
#'       k21 * peripheral1 - k13 * central + k31 * peripheral2
#'     d/dt(peripheral1) <- k12 * central - k21 * peripheral1
#'     d/dt(peripheral2) <- k13 * central - k31 * peripheral2
#'     Cc <- central / vc
#'     Cc ~ prop(propSd)
#'   })
#' }
#'
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
#' pk3cmtDes |> addCmtProp("f", "depot")
#'
#' pk3cmtDes |> addBioavailability(depot)
#'
#' pk3cmtDes |> addLag(depot)
#'
#' pk3cmtDes |> addDur(depot)
#'
#' pk3cmtDes |> addRate(depot)
#'
#' pk3cmtDes |> addIni(depot)
#'
#' # bioavailability bounded to (0,1) with a logit parameterization
#' pk1cmtDes |> addBioavailability(depot, scale = "logit")
#'
#' # dose split between two absorption paths, fraction named for depot
#' pk1cmtDes |>
#'   addDepot(depot = "depot2", ka = "ka2") |>
#'   addBioavailability(depot, depot2, scale = "logit")
#'
addCmtProp <- function(ui, prop = c("f", "lag", "dur", "rate", "ini"), cmt) {
  .ui <- assertRxUi(ui)
  .cmt <- as.character(substitute(cmt))
  cmt <- try(force(cmt), silent = TRUE)
  if (inherits(cmt, "try-error")) {
    cmt <- .cmt
  }
  prop <- match.arg(prop)
  if (testCompartmentExists(.ui, cmt)) {
    .cmt <- assertCompartmentExists(.ui, cmt)
    .modelLines <- .ui$lstExpr
    .var <- defaultCombine(prop, .cmt)
    .ui <- addLogEstimates(.ui, .var)
    .modelLines <- .ui$lstExpr
    .w <- .whichDdt(.modelLines, .cmt)
    .tmp <- .extractModelLinesAtW(.modelLines, .w)

    .ui <- rxUiDecompress(.ui)
    if (exists("description", envir = .ui$meta)) {
      rm("description", envir = .ui$meta)
    }
    if (prop == "ini") {
      model(.ui) <- c(.tmp$pre, .tmp$w, str2lang(paste0(.cmt, "(0) <- ", .var)), .tmp$post)
    } else {
      model(.ui) <- c(.tmp$pre, .tmp$w, str2lang(paste0(prop, "(", .cmt, ") <- ", .var)), .tmp$post)
    }
    .ui
  } else {
    stop("Compartment ", cmt, " does not exist")
  }
}

#' @describeIn addCmtProp Adds the bioavailability to a compartment in the model
#'
#' @param scale parameterization of the bioavailability: \code{"log"}
#'   (default) estimates it unboundedly on the log scale, while
#'   \code{"logit"} constrains it to (0,1) with a logit/expit
#'   parameterization (Monolix-style \code{F}, oral or subcutaneous)
#' @param cmt2 optional second compartment for the logit dose-split
#'   form (only with `scale = "logit"`)
#' @param f initial bioavailability fraction, in (0,1), for the logit
#'   form, or NULL to leave the initial estimate unset
#' @export
addBioavailability <- function(ui, cmt, scale = c("log", "logit"), cmt2 = NULL, f = 0.8) {
  scale <- match.arg(scale)
  # resolve both compartment arguments in this frame: the inner
  # function receives them as the symbols cmt/cmt2, so forwarding
  # them unevaluated would lose the caller's spelling; values
  # (quoted or computed) pass through force() unchanged.  Note that
  # force(cmt2) on an unbound compartment symbol raises (which the
  # try() converts into the spelling) while is.null(cmt2) on the
  # same symbol raises too, so the spelling fallback must come
  # first
  .cmt <- as.character(substitute(cmt))
  cmt <- try(force(cmt), silent = TRUE)
  if (inherits(cmt, "try-error")) {
    cmt <- .cmt
  }
  .cmt2 <- as.character(substitute(cmt2))
  .cmt2v <- try(force(cmt2), silent = TRUE)
  if (inherits(.cmt2v, "try-error")) {
    cmt2 <- .cmt2
  } else if (!is.null(.cmt2v)) {
    cmt2 <- .cmt2v
  }
  if (scale == "logit") {
    return(addBioavailabilityLogit(ui, cmt, cmt2, f))
  }
  if (!is.null(cmt2)) {
    stop(
      "a dose split (cmt2) needs scale = \"logit\": on the log scale f is ",
      "unbounded above, so the complement 1 - f given to cmt2 is not ",
      "confined to (0, 1) and can go negative",
      call. = FALSE
    )
  }
  addBioavailabilityLog(ui, cmt, f)
}

#' Add a log-parameterized bioavailability to a compartment
#'
#' The implementation behind `addBioavailability(scale = "log")`.  The
#' fraction is estimated unboundedly as `f<Cmt> <- exp(lf<Cmt>)`, so it
#' can leave (0,1); `addBioavailability(scale = "logit")` is the bounded form.
#'
#' @param ui rxode2 ui object
#' @param cmt compartment to apply the bioavailability to
#' @param f initial bioavailability fraction, or NULL to leave the
#'   initial estimate at the package default
#' @return rxode2 ui object with the log bioavailability applied
#' @noRd
addBioavailabilityLog <- function(ui, cmt, f = 0.8) {
  # assertNumeric's bounds are inclusive and log() is undefined at 0, so
  # the open lower bound needs its own check.  There is no upper bound:
  # the whole point of the log scale is that f may exceed 1.
  checkmate::assertNumeric(f, len = 1L, any.missing = FALSE, null.ok = TRUE)
  if (!is.null(f) && f <= 0) {
    stop("f must be > 0", call. = FALSE)
  }
  # addCmtProp() first, so a missing compartment keeps reporting itself in
  # its own words rather than through the assertion below
  .ui <- addCmtProp(ui, prop = "f", cmt = cmt)
  if (is.null(f)) {
    return(.ui)
  }
  .cmt <- assertCompartmentExists(assertRxUi(ui), cmt)
  .iniAddTheta(.ui, paste0("l", defaultCombine("f", .cmt)), est = log(f))
}

#' Assert compartments do not already have a bioavailability
#'
#' Applying a bioavailability twice leaves two definitions of the same
#' `f<Depot>` variable in the model block, and the solved model then
#' silently reflects neither intended parameterization.
#'
#' @param ui rxode2 ui object
#' @param cmt compartments to check
#' @return invisible NULL; raises an error when `f` is already present
#' @noRd
.assertNoBioavailability <- function(ui, cmt) {
  .cp <- ui$props$cmtProp
  if (is.null(.cp) || nrow(.cp) == 0) {
    return(invisible(NULL))
  }
  .w <- which(.cp$Compartment %in% cmt & .cp$Property == "f")
  if (length(.w) > 0) {
    stop(
      "bioavailability already present for compartment '",
      paste(unique(.cp$Compartment[.w]), collapse = "', '"),
      "'",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Add a logit-parameterized bioavailability to a compartment
#'
#' Adds a bioavailability fraction constrained to (0,1) with a
#' logit/expit parameterization, matching Monolix-style \code{F}
#' (oral, subcutaneous) and the \code{F1}/\code{Fd} dose-apportionment
#' fractions.
#'
#' Single-compartment form (no \code{cmt2}): the fraction is estimated
#' for \code{cmt} as \code{f(cmt) <- f<Depot>} with \code{f<Depot> <-
#'   expit(logitf<Depot>)}.
#'
#' Dose-split form (two compartments): the fraction for \code{cmt} is
#' \code{f(cmt) <- f<Depot>} with \code{f<Depot> <-
#'   expit(logitf<Depot>)}, while \code{cmt2} receives the remainder
#' \code{f(cmt2) <- 1 - f<Depot>}.  The fraction is named for (and
#' estimated on) the compartment named \emph{first}, matching the
#' double-absorption \code{F1} convention and the \code{PK_double_sim}
#' library seeds, which estimate \code{fdepot1}/\code{lgfdepot1} for
#' the first path.
#'
#' Unlike `addBioavailability(scale = "log")`, which log-parameterizes \code{F}
#' unboundedly, the logit form keeps \code{F} in (0,1): the initial
#' estimate is stored on the logit scale and the model block defines
#' the \code{expit()} line, which keeps the mu-referenced bounds.
#'
#' @param ui rxode2 ui object
#' @param cmt compartment to apply the bioavailability to; in the
#'   dose-split form this compartment receives the estimated fraction
#'   \code{f<Depot>} while \code{cmt2} receives the remainder
#' @param cmt2 optional second compartment; when supplied it receives
#'   the remainder \code{1 - f<Depot>} (dose split)
#' @param f initial bioavailability fraction, in (0,1), or NULL to
#'   leave the initial estimate unset
#' @return rxode2 ui object with the logit bioavailability applied
#' @author Matthew L. Fidler
#' @noRd
addBioavailabilityLogit <- function(ui, cmt, cmt2 = NULL, f = 0.8) {
  # the logit is undefined at the 0/1 boundaries, so only the open
  # interval is accepted (assertNumeric's bounds are inclusive)
  checkmate::assertNumeric(f, len = 1L, any.missing = FALSE, null.ok = TRUE)
  if (!is.null(f) && (f <= 0 || f >= 1)) {
    stop("f must be in (0, 1)", call. = FALSE)
  }
  # when called through addBioavailability(..., scale = "logit") the
  # arguments already arrive resolved (see there); only resolve here
  # for direct calls, detected by cmt still being an unbound symbol.
  # force(cmt2) on an unbound symbol raises, while is.null(cmt2) on
  # the same symbol raises too, so the spelling fallback comes first
  .cmt <- as.character(substitute(cmt))
  .unbound <- try(force(cmt), silent = TRUE)
  if (inherits(.unbound, "try-error")) {
    cmt <- .cmt
  }
  .cmt2 <- as.character(substitute(cmt2))
  .unbound2 <- try(force(cmt2), silent = TRUE)
  if (inherits(.unbound2, "try-error")) {
    cmt2 <- .cmt2
  } else if (!is.null(.unbound2)) {
    cmt2 <- .unbound2
  }
  .ui <- assertRxUi(ui)
  .cmt1 <- assertCompartmentExists(.ui, cmt)
  .split <- !is.null(cmt2)
  if (.split) {
    .cmt2 <- assertCompartmentExists(.ui, cmt2)
    if (identical(.cmt1, .cmt2)) {
      stop("cmt and cmt2 must be different compartments", call. = FALSE)
    }
  }
  .assertNoBioavailability(.ui, if (.split) c(.cmt1, .cmt2) else .cmt1)
  .var <- defaultCombine("f", .cmt1)
  .lvar <- paste0("logit", .var)
  .label <- if (.split) {
    paste0("Fraction of dose absorbed from ", .cmt1, " (", .var, ")")
  } else {
    paste0("Bioavailability fraction (", .var, ")")
  }
  .modelLines <- .ui$lstExpr
  .expitLine <- str2lang(paste0(.var, " <- expit(", .lvar, ")"))
  .w1 <- .whichDdt(.modelLines, .cmt1)
  .f1 <- if (.split) {
    str2lang(paste0("f(", .cmt1, ") <- ", .var))
  } else {
    str2lang(paste0("f(", .cmt1, ") <- ", .var))
  }
  if (.split) {
    .w2 <- .whichDdt(.modelLines, .cmt2)
    .f2 <- str2lang(paste0("f(", .cmt2, ") <- 1-", .var))
  }
  .out <- list(.expitLine)
  for (.i in seq_along(.modelLines)) {
    .out[[length(.out) + 1L]] <- .modelLines[[.i]]
    if (.i == .w1) {
      .out[[length(.out) + 1L]] <- .f1
    }
    if (.split && .i == .w2) {
      .out[[length(.out) + 1L]] <- .f2
    }
  }
  .ui <- rxUiDecompress(.ui)
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- .out
  .ui <- rxUiCompress(.ui)
  if (is.null(f)) {
    return(.ui)
  }
  # ini() can only set an estimate for a variable the model defines, so
  # the expit() line above must be in place before this call; it appends
  # the theta when missing and overwrites it when present
  # the estimate and the label go in through the same ini() call rather
  # than by writing into iniDf: that data frame's columns belong to
  # lotri and do change (see R/iniPiping.R).  bquote() (not do.call())
  # because ini() needs the compartment-derived name as an unevaluated
  # `<-` expression whose right-hand side is itself an unevaluated
  # call -- which also keeps the block reading `logit(0.8)`, the
  # spelling the PK_double_sim seeds use, rather than its value
  .iniCall <- bquote(
    ini(.ui,
      .(.name) <- logit(.(f2)),
      .(.name) <- label(.(lbl))
    ),
    list(.name = as.name(.lvar), f2 = f, lbl = .label)
  )
  suppressMessages(eval(.iniCall, envir = environment()))
}


#' @describeIn addCmtProp Adds the lag-time to a compartment in the model
#' @export
addLag <- function(ui, cmt) {
  .cmt <- as.character(substitute(cmt))
  cmt <- try(force(cmt), silent = TRUE)
  if (inherits(cmt, "try-error")) {
    cmt <- .cmt
  }
  addCmtProp(ui, prop = "lag", cmt = cmt)
}

#' @describeIn addCmtProp Adds the modeled duration to a compartment in the model
#' @export
addDur <- function(ui, cmt) {
  .cmt <- as.character(substitute(cmt))
  cmt <- try(force(cmt), silent = TRUE)
  if (inherits(cmt, "try-error")) {
    cmt <- .cmt
  }
  addCmtProp(ui, prop = "dur", cmt = cmt)
}
#' @describeIn addCmtProp Adds the modeled rate to a compartment in the model
#' @export
addRate <- function(ui, cmt) {
  .cmt <- as.character(substitute(cmt))
  cmt <- try(force(cmt), silent = TRUE)
  if (inherits(cmt, "try-error")) {
    cmt <- .cmt
  }
  addCmtProp(ui, prop = "rate", cmt = cmt)
}

#' @describeIn addCmtProp Adds the initial value to the compartment
#' @export
addIni <- function(ui, cmt) {
  .cmt <- as.character(substitute(cmt))
  cmt <- try(force(cmt), silent = TRUE)
  if (inherits(cmt, "try-error")) {
    cmt <- .cmt
  }
  addCmtProp(ui, prop = "ini", cmt = cmt)
}
