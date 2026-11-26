#' Add a second absorption path for double-absorption models
#'
#' Adds a parallel absorption path to a model that already has
#' first-order absorption through `depot`, for Monolix-style
#' double-absorption models: one dose record is split at translation
#' time into two parallel inputs with a logit-parameterized `F1`
#' apportionment.  The second path can itself be zero-order
#' (`type = "zero"`, Monolix's `Tk02` modeled duration directly on the
#' central compartment), first-order (`type = "first"`, `ka2`, which
#' adds `depot2`), or a first-order path behind a transit chain
#' (`delay = "transit"`), with an optional lag.  When the first path is
#' zero-order (`addZeroOrderAbs()`), only a first-order second path is
#' available.
#'
#' Only one second absorption path is supported per model; applying
#' this twice raises an error.
#'
#' @inheritParams addDepot
#' @param type second-path input type: `"zero"` for a zero-order
#'   input (Monolix's `Tk02`: a modeled duration `dur(central)` on the
#'   central compartment) or `"first"` for a first-order (`ka2`)
#'   input.  A zero-order second path is only available when the first
#'   path is first-order (two modeled durations cannot share the
#'   central compartment).
#' @param delay second-path delay: `"none"`, `"lag"` (an `alag` on the
#'   second path), or `"transit"` (a transit chain feeding the second
#'   path).  `"transit"` needs a first-order second path.
#' @param n number of transit compartments when `delay = "transit"`
#' @param depot2 name of the second depot compartment
#' @param ka2 name of the second first-order absorption rate
#' @param tk0 name of the zero-order duration on the second path
#' @param f1 initial fraction of the dose entering the first path, in
#'   (0,1)
#' @return a model with two parallel absorption paths
#' @family Absorption building blocks
#' @export
#' @author Matthew L. Fidler
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
#' # simultaneous first-order + first-order (Monolix oral1_oral1)
#' pk1cmtDes |>
#'   addSecondAbsorption(type = "first", delay = "lag", f1 = 0.7)
#'
#' # simultaneous zero-order + first-order (Monolix oral0_oral1:
#' # Tk02 as dur(central), F1 on the first-order depot)
#' pk1cmtDes |>
#'   addSecondAbsorption(type = "zero", f1 = 0.5)
#'
addSecondAbsorption <- function(
  ui,
  type = c("first", "zero"),
  delay = c("none", "lag", "transit"),
  n = NULL,
  central = "central",
  depot = "depot",
  depot2 = "depot2",
  ka = "ka",
  ka2 = "ka2",
  tk0 = "tk0",
  f1 = 0.7
) {
  .useModelAsUi()
  type <- match.arg(type)
  delay <- match.arg(delay)
  .ui <- assertRxUi(ui)
  .cmt <- as.character(substitute(depot2))
  depot2 <- try(force(depot2), silent = TRUE)
  if (inherits(depot2, "try-error")) {
    depot2 <- .cmt
  }
  assertCompartmentName(depot2)
  .ka2 <- as.character(substitute(ka2))
  ka2 <- try(force(ka2), silent = TRUE)
  if (inherits(ka2, "try-error")) {
    ka2 <- .ka2
  }
  assertVariableName(ka2)
  .cp <- .ui$props$cmtProp
  if (!is.null(.cp) && any(.cp$Compartment == depot2)) {
    stop("a second absorption path ('", depot2, "') is already present", call. = FALSE)
  }
  # a split directive means a second path is already present even when
  # it addresses differently-named compartments
  .hasSplit <- any(vapply(
    .ui$lstExpr,
    function(l) {
      .d <- deparse1(l)
      grepl("splitInfusionBolus(", .d, fixed = TRUE) ||
        grepl("splitBolusInfusion(", .d, fixed = TRUE) ||
        grepl("splitBolus(", .d, fixed = TRUE) ||
        grepl("splitInfusion(", .d, fixed = TRUE)
    },
    logical(1),
    USE.NAMES = FALSE
  ))
  if (.hasSplit) {
    stop("a second absorption path is already present (model has a split directive)", call. = FALSE)
  }
  .path <- .secondAbsorptionPath(.ui, type, delay, n, central, depot, depot2, ka2, tk0)
  .ui <- .path$ui
  # logit F1 split with the estimated fraction on the first path
  # (Monolix F1 convention); for a zero-order first path there is no
  # depot compartment, so the split is between central (which keeps
  # the modeled-duration input) and the second path
  .ui <- addBioavailability(.ui, .path$src, .path$secondCmt, scale = "logit", f = f1)
  # one dose record on the dosing compartment feeds both paths at
  # translation time; splitInfusionBolus() gives the FIRST target the
  # modeled-duration infusion and the other target a bolus copy, so a
  # zero-order second path puts central first (its dur() promotes it)
  .src <- .path$src
  .splitTargets <- .path$splitTargets
  .modelLines <- .ui$lstExpr
  .w <- .whichDdt(.modelLines, .src)
  .tmp <- .extractModelLinesAtW(.modelLines, .w)
  .ui <- rxUiDecompress(.ui)
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- c(
    .tmp$pre,
    list(str2lang(paste0("splitInfusionBolus(", .src, ", ", .splitTargets, ")"))),
    .tmp$w,
    .tmp$post
  )
  rxUiCompress(.ui)
}

#' Force the second absorption delay longer than the first
#'
#' Reparameterizes a double-absorption model so the second path's lag
#' is estimated as a positive increment over the first path's lag
#' (`Tlag2 = Tlag1 + diffTlag2`), matching Monolix's "force delay2
#' longer than delay1" models.  Both paths must have lag times and the
#' model must not already be sequential.
#'
#' @inheritParams addDepot
#' @param depot2 name of the second depot compartment
#' @return a model with the second lag tied above the first
#' @family Absorption building blocks
#' @export
#' @author Matthew L. Fidler
convertAbsForceLongerDelay <- function(ui, central = "central", depot = "depot", depot2 = "depot2") {
  .useModelAsUi()
  .ui <- assertRxUi(ui)
  central <- assertCompartmentExists(.ui, central)
  .modelLines <- .ui$lstExpr
  .lag1 <- sprintf("lag(%s) <- ", depot)
  .lag2 <- sprintf("lag(%s) <- ", depot2)
  .hasLag <- function(pat) {
    any(vapply(.modelLines, function(l) grepl(pat, deparse1(l), fixed = TRUE), logical(1), USE.NAMES = FALSE))
  }
  # a zero-order (Tk02) second path has no depot2: its lag is on central
  .cmt2 <- depot2
  if (!.hasLag(.lag2) && !testCompartmentExists(.ui, depot2)) {
    .cmt2 <- central
    .lag2 <- sprintf("lag(%s) <- ", central)
  }
  if (!.hasLag(.lag1) || !.hasLag(.lag2)) {
    stop("forcing a longer second delay requires lag times on both absorption paths", call. = FALSE)
  }
  .w2 <- which(vapply(.modelLines, function(l) grepl(.lag2, deparse1(l), fixed = TRUE), logical(1), USE.NAMES = FALSE))
  .rhs2 <- .modelLines[[.w2[1L]]][[3L]]
  .lagVar2 <- if (is.name(.rhs2)) as.character(.rhs2) else NULL
  # lag(depot2) <- <first-lag-var> + diffTlag2, estimating the
  # increment; the first path's lag variable is read from its own
  # lag() line (it need not follow the defaultCombine() convention)
  .w1 <- which(vapply(.modelLines, function(l) grepl(.lag1, deparse1(l), fixed = TRUE), logical(1), USE.NAMES = FALSE))
  .rhs1 <- .modelLines[[.w1[1L]]][[3L]]
  if (!is.name(.rhs1)) {
    stop("the first absorption path's lag time is not a plain variable", call. = FALSE)
  }
  .lagVar1 <- as.character(.rhs1)
  .modelLines[[.w2[1L]]] <- str2lang(paste0("lag(", .cmt2, ") <- ", .lagVar1, " + diffTlag2"))
  .ui <- rxUiDecompress(.ui)
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- .modelLines
  .ui <- rxUiCompress(.ui)
  .ui <- .iniAddTheta(.ui, "diffTlag2", label = "Second lag increment over the first (diffTlag2)")
  if (!is.null(.lagVar2) && testVariableExists(.ui, .lagVar2)) {
    .ui <- rxUiCompress(removeLinesAndInis(.ui, .lagVar2))
  }
  .ui
}

.secondAbsorptionCheckZero <- function(ui, type, delay, zoFirst, central) {
  if (type != "zero") {
    return(invisible(NULL))
  }
  # dur(central) has to be written into the ODE block; linCmt() models
  # have no d/dt(central) line to attach it to
  .hasLinCmt <- any(vapply(
    ui$lstExpr,
    function(l) grepl("linCmt(", deparse1(l), fixed = TRUE),
    logical(1),
    USE.NAMES = FALSE
  ))
  if (.hasLinCmt) {
    stop(
      "a zero-order second path needs an ODE model (the model uses linCmt())",
      call. = FALSE
    )
  }
  .cp <- ui$props$cmtProp
  .hasDur <- !is.null(.cp) && any(.cp$Compartment == central & .cp$Property == "dur")
  if (zoFirst && .hasDur) {
    stop(
      "two zero-order absorption routes are not supported: each modeled ",
      "duration is a property of one compartment, so the second route ",
      "would need its own compartment and rate",
      call. = FALSE
    )
  }
  if (zoFirst) {
    stop(
      "a zero-order second path needs a first-order first path; ",
      "use addZeroOrderAbs() for a zero-order route",
      call. = FALSE
    )
  }
  if (delay == "transit") {
    stop(
      "a zero-order second path with a transit-compartment delay is not supported; ",
      "use delay = \"lag\" (Monolix Tk02 + Tlag2) or a first-order second path",
      call. = FALSE
    )
  }
  assertCompartmentExists(ui, central)
  if (.hasDur) {
    stop("modeled duration already present for compartment '", central, "'", call. = FALSE)
  }
  invisible(NULL)
}

.secondAbsorptionPath <- function(ui, type, delay, n, central, depot, depot2, ka2, tk0) {
  .zoFirst <- !testCompartmentExists(ui, depot)
  if (delay == "transit") {
    checkmate::assertIntegerish(n, lower = 1L, len = 1L, any.missing = FALSE)
  }
  # Monolix's zero-order absorption route (Tk02) is a modeled duration
  # on the central compartment; it does not add a depot or a rate.  It
  # is only expressible when the first path keeps a depot: when the
  # first path is zero-order the duration is already on central, and
  # the second route then has to be first-order (the depot2 path).
  .secondAbsorptionCheckZero(ui, type, delay, .zoFirst, central)
  .zeroDirect <- type == "zero"
  if (type == "zero") {
    ui <- addLogEstimates(
      ui,
      stats::setNames(
        "Zero-order absorption duration on the second path (Tk0)",
        tk0
      )
    )
  }
  if (.zeroDirect) {
    ui <- .addSecondAbsorptionDur(ui, central, tk0)
  } else {
    ui <- addDepot(ui, central = central, depot = depot2, ka = ka2)
  }
  .src <- if (.zoFirst) central else depot
  .lagCmt <- if (.zeroDirect) central else depot2
  if (delay == "lag") {
    ui <- addLag(ui, .lagCmt)
  } else if (delay == "transit") {
    # the second chain gets a letter-tagged prefix and rate (transitB1..N,
    # ktrB) so it can never collide with the first path's numbered chain
    # (transit1..N, ktr) -- a numeric prefix would clash once a chain
    # reaches 10+ compartments
    ui <- addTransit(ui, n, central = central, depot = depot2, transit = "transitB", ktr = "ktrB", ka = ka2)
  }
  list(
    ui = ui,
    src = .src,
    secondCmt = if (.zeroDirect) central else depot2,
    splitTargets = if (.zeroDirect) paste0(central, ", ", depot) else paste0(.src, ", ", depot2)
  )
}

.addSecondAbsorptionDur <- function(ui, cmt, durVar) {
  .modelLines <- ui$lstExpr
  .w <- .whichDdt(.modelLines, cmt)
  .tmp <- .extractModelLinesAtW(.modelLines, .w)
  .ui <- rxUiDecompress(ui)
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- c(
    .tmp$pre,
    .tmp$w,
    str2lang(paste0("dur(", cmt, ") <- ", durVar)),
    .tmp$post
  )
  rxUiCompress(.ui)
}
