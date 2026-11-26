#' Removes lines and inis from a model
#'
#'
#' @param ui A rxode2 model
#' @param vars A character vector of variables to remove
#' @return model with rxode2 lines and any estimates associate with lines removed
#' @family Parameter and naming building blocks
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
#' pk3cmtDes |> removeLinesAndInis(c("kel", "k12", "k21"))
removeLinesAndInis <- function(ui, vars) {
  .ui <- assertRxUi(ui)
  .modelLines <- .ui$lstExpr

  # Get a list of the var <- or var = lines
  .exprs <- c(
    lapply(vars, function(x) {
      str2lang(paste0(x, "<- ."))
    }),
    lapply(vars, function(x) {
      str2lang(paste0(x, "= ."))
    })
  )
  # Find the lines that match the vars
  .w <- which(vapply(
    seq_along(.modelLines),
    function(i) {
      .cur <- .modelLines[[i]]
      any(vapply(
        seq_along(.exprs),
        function(j) {
          .matchesLangTemplate(.cur, .exprs[[j]])
        },
        logical(1)
      ))
    },
    logical(1)
  ))

  # Get the model variables that will be removed from initial estimates
  .txt <- rxModelVars(
    paste(
      vapply(
        .w,
        function(i) {
          deparse1(.modelLines[[i]])
        },
        character(1)
      ),
      collapse = "\n"
    )
  )
  .vars <- c(.txt$lhs, .txt$params)

  # Remove the lines from the model
  .modelLines <- lapply(seq_along(.modelLines)[-.w], function(i) {
    .modelLines[[i]]
  })
  # Remove the inis from the model
  .tmp <- .getEtaTheta(.ui)
  .iniDf <- .tmp$iniDf
  .eta <- .tmp$eta
  .theta <- .tmp$theta
  for (.v in .vars) {
    .tmp <- .dropLines(.ui, .modelLines, .theta, .eta, .v)
    .modelLines <- .tmp$modelLines
    .theta <- .tmp$theta
    .eta <- .tmp$eta
  }
  .ui <- rxUiDecompress(.ui)
  .ui$iniDf <- rbind(.theta, .eta)
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- .modelLines
  .ui
}
#' Add log estimates to a model
#'
#'
#' @param ui rxode2 model
#' @param vars estimates to add they will be parameterized as:
#'
#' \code{var <- exp(lvar)}
#'
#' where \code{var} is the variable name in the model and \code{lvar}
#' is the log transformed variable that will be estimated
#'
#' @param extraLines this is a list of additional lines to add to the
#'   model just after the variables are defined.  It must be
#'   \code{NULL} or a list of \code{language} objects.
#'
#' @param beforeCmt if the model is compartmental you can specify the
#'   preferred names where the estimates and extra lines are added before
#'
#' @return rxode2 model with log estimates added (and possibly extra lines)
#' @family Parameter and naming building blocks
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
#'
#' # Change the transformation of the PK model from cl to k
#'
#' pk3cmtDes |>
#'   removeLinesAndInis(c("kel", "k12", "k21", "k13", "k31", "vc")) |>
#'   addLogEstimates(c("kel", "k12", "k21", "k13", "k31", "vc"))
#'
#' # You can also label the parameters by using a named character
#' # vector with the names of the parameters representing the
#' # variables and the values representing the labels:
#'
#' pk3cmtDes |>
#'   removeLinesAndInis(c("kel", "k12", "k21", "k13", "k31", "vc")) |>
#'   addLogEstimates(c(kel = "elimination", k12 = "k12 constant",
#'     k21 = "k21 constant",
#'     k13 = "k13 constant",
#'     k31 = "k31 constant",
#'     vc = "volume of central compartment"))
#'
addLogEstimates <- function(ui, vars, extraLines = NULL, beforeCmt = NULL) {
  .ui <- assertRxUi(ui)
  .before <- NULL
  if (!is.null(beforeCmt)) {
    .before <- assertCompartmentExists(.ui, beforeCmt)
  }
  .tmp <- .getEtaTheta(.ui)
  .iniDf <- .tmp$iniDf
  .theta <- .tmp$theta
  .eta <- .tmp$eta
  if (length(.theta$ntheta) == 0) {
    .ntheta <- 0
  } else {
    .ntheta <- max(.theta$ntheta)
  }
  if (is.null(names(vars))) {
    .label <- rep(NA_character_, length(vars))
    .vars <- vars
  } else {
    .vars <- names(vars)
    .label <- ifelse(vars == "", NA_character_, vars)
  }
  .extra <- list()
  for (.i in seq_along(.vars)) {
    .v <- .vars[.i]
    .extra <- c(.extra, list(str2lang(paste0(.v, " <- exp(l", .v, ")"))))
  }
  .ui <- rxUiDecompress(.ui)
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  if (is.null(.before)) {
    model(.ui) <- c(
      .extra,
      extraLines,
      .ui$lstExpr
    )
  } else {
    .modelLines <- .ui$lstExpr
    .w <- .whichDdt(.modelLines, .before)
    .tmp <- .extractModelLinesAtW(.modelLines, .w)
    model(.ui) <- c(
      .tmp$pre,
      .extra,
      extraLines,
      .tmp$w,
      .tmp$post
    )
  }
  for (.i in seq_along(.vars)) {
    .ui <- .iniAddTheta(.ui, paste0("l", .vars[.i]), label = .label[.i])
  }
  .ui
}
#' Change the transformation type for PK models
#'
#' @param ui A model in terms of Clearance
#'
#' @param type the type of PK transformation to make:
#'
#'  - \code{"k"}: Change to rate constants (kel, k12, k21, k13, k31)
#'
#'  - \code{"vss"}: Change to volume of distribution at steady state (cl, vc, q, vss)
#'
#'  - \code{"aob"}: Change to A/B ratio (aob, alpha, beta, vc)
#'
#'  - \code{"k21"}: Change to k21 constant (k21, alpha, beta, vc) or (k21, k31, alpha, beta, gam, vc)
#'
#' - \code{"alpha"}: Change to macro constants  (alpha, beta, gam, A, B, C, vc)
#'
#' @param k13 name of rate constant from central to periph2
#' @param k31 name of rate constant from periph2 to central
#' @param k12 name of rate constant from central to periph1
#' @param k21 name of rate constant from periph1 to central
#' @param kel name of elimination rate constant
#' @param vc name of central compartment volume
#' @param cl name of clearance
#' @param vp name of volume of periph1
#' @param q name of intercompartmental clearance between central and
#'   periph1
#' @param vp2 name of volume of periph2
#' @param q2 name of intercompartmental clearance between central and
#'   periph2
#' @param vss name of volume of distribution at steady state
#' @param aob A/B ratio
#' @param alpha macro constant name for first exponential decay term
#' @param beta macro constant name for second exponential decay term
#' @param gam macro constant name for third exponential decay term
#' @param A macro coefficient for the first exponential decay term
#'   (corresponds with alpha)
#' @param B macro coefficient for the second exponential decay term
#'   (corresponds with beta)
#' @param C macro coefficient for the third exponential decay term
#'   (corresponds with gam)
#' @param s sum constant name for the k12 three compartment
#' @param p product constant name for the k12 three compartment
#' @param tmp name of temporary variable for the three compartment
#'   with `A`, `B`, `C`, `alpha`, `beta` and `gam`.
#' @param beforeCmt if the model is compartmental you can specify the
#'   preferred names where the estimates and extra lines are added
#'   before
#' @return ui with no PK parameters estimated
#' @family Parameter and naming building blocks
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
#' pk2cmtDes <- function() {
#'   ini({
#'     lka <- 0.45; label("Absorption rate (Ka)")
#'     lcl <- 1; label("Clearance (CL)")
#'     lvc <- 3; label("Central volume of distribution (V)")
#'     lvp <- 5; label("Peripheral volume of distribution (Vp)")
#'     lq <- 0.1; label("Intercompartmental clearance (Q)")
#'     propSd <- 0.5; label("Proportional residual error (fraction)")
#'   })
#'   model({
#'     ka <- exp(lka)
#'     cl <- exp(lcl)
#'     vc <- exp(lvc)
#'     vp <- exp(lvp)
#'     q <- exp(lq)
#'     kel <- cl / vc
#'     k12 <- q / vc
#'     k21 <- q / vp
#'     d/dt(depot) <- -ka * depot
#'     d/dt(central) <- ka * depot - kel * central - k12 * central + k21 * peripheral1
#'     d/dt(peripheral1) <- k12 * central - k21 * peripheral1
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
#' \donttest{
#' # Three compartment model translations
#'
#' pk3cmtDes |>
#'   pkTrans("k")
#'
#' pk3cmtDes |>
#'   pkTrans("k21")
#'
#' pk3cmtDes |>
#'   pkTrans("alpha")
#'
#' # The most types of transformations are
#' # available for 2 compartment models
#'
#' pk2cmtDes |>
#'   pkTrans("k")
#'
#' pk2cmtDes |>
#'   pkTrans("vss")
#'
#' pk2cmtDes |>
#'   pkTrans("aob")
#'
#' pk2cmtDes |>
#'   pkTrans("k21")
#'
#' pk2cmtDes |>
#'   pkTrans("alpha")
#'
#' # One compartment transformations are also available:
#'
#' pk1cmtDes |>
#'   pkTrans("k")
#'
#' pk1cmtDes |>
#'   pkTrans("alpha")
#'
#' # also works without depot:
#'
#' pk3cmtDes |>
#'   removeDepot() |>
#'   pkTrans("k")
#' }
pkTrans <- function(
  ui,
  type = c("k", "k21", "vss", "aob", "alpha"),
  k13 = "k13",
  k31 = "k31",
  k12 = "k12",
  k21 = "k21",
  kel = "kel",
  vc = "vc",
  cl = "cl",
  vp = "vp",
  q = "q",
  vp2 = "vp2",
  q2 = "q2",
  vss = "vss",
  aob = "aob",
  alpha = "alpha",
  beta = "beta",
  gam = "gam",
  A = "A",
  B = "B",
  C = "C",
  s = "s",
  p = "p",
  tmp = "tmp",
  beforeCmt = c("depot", "central")
) {
  typ <- match.arg(type)
  .ui <- assertRxUi(ui)
  .cmt <- 1L
  .rm <- c(assertVariableExists(.ui, kel), assertVariableExists(.ui, vc))
  if (testVariableExists(.ui, k12)) {
    .cmt <- 2L
    .rm <- c(.rm, assertVariableExists(.ui, k12), assertVariableExists(.ui, k21))
  }
  if (testVariableExists(.ui, k13)) {
    .cmt <- 3L
    .rm <- c(.rm, assertVariableExists(.ui, k13), assertVariableExists(.ui, k31))
  }
  .ui <- removeLinesAndInis(.ui, .rm)
  if (type == "k") {
    # These would be transformations in terms of rate constants alone
    if (.cmt == 1L) {
      .est <- stats::setNames(
        c(
          paste0("Elimination from central (", kel, ")"),
          paste0("Central compartment volume (", vc, ")")
        ),
        c(kel, vc)
      )
      .ui <- addLogEstimates(.ui, .est, beforeCmt = beforeCmt)
      return(.ui)
    } else if (.cmt == 2L) {
      .est <- stats::setNames(
        c(
          paste0("Central->Periph1 constant (", k12, ")"),
          paste0("Periph1->Central constant (", k21, ")"),
          paste0("Elimination from central (", kel, ")"),
          paste0("Central compartment volume (", vc, ")")
        ),
        c(k12, k21, kel, vc)
      )
      .ui <- addLogEstimates(.ui, .est, beforeCmt = beforeCmt)
      return(.ui)
    } else if (.cmt == 3L) {
      .est <- stats::setNames(
        c(
          paste0("Central->Periph1 constant (", k12, ")"),
          paste0("Periph1->Central constant (", k21, ")"),
          paste0("Central->Periph2 constant (", k13, ")"),
          paste0("Periph2->Central constant (", k31, ")"),
          paste0("Elimination from central (", kel, ")"),
          paste0("Central compartment volume (", vc, ")")
        ),
        c(k12, k21, k13, k31, kel, vc)
      )
      .ui <- addLogEstimates(.ui, .est, beforeCmt = beforeCmt)
      return(.ui)
    }
  } else if (type == "vss") {
    if (.cmt != 2L) {
      stop("vss transformation only works for 2 compartment models", call. = FALSE)
    }
    .est <- stats::setNames(
      c(
        paste0("Clearance (", cl, ")"),
        paste0("Central compartment volume (", vc, ")"),
        paste0("Periph1<->Central inter-compartmental clearance (", q, ")"),
        paste0("Volume of distribution at steady state (", vss, ")")
      ),
      c(cl, vc, q, vss)
    )
    .ui <- addLogEstimates(
      .ui,
      .est,
      beforeCmt = beforeCmt,
      extraLines = list(
        str2lang(paste0(kel, "<-", cl, "/", vc)),
        str2lang(paste0(k12, "<-", q, "/", vc)),
        str2lang(paste0(k21, "<-", q, "/(", vss, "-", vc, ")"))
      )
    )
    return(.ui)
  } else if (type == "aob") {
    if (.cmt != 2) {
      stop("aob transformation only works for 2 compartment models", call. = FALSE)
    }
    .est <- stats::setNames(
      c(
        paste0("A/B (", aob, ")"),
        paste0("alpha macro constant (", alpha, ")"),
        paste0("beta macro constant (", beta, ")"),
        paste0("Volume of central compartment (", vc, ")")
      ),
      c(aob, alpha, beta, vc)
    )
    .ui <- addLogEstimates(
      .ui,
      .est,
      beforeCmt = beforeCmt,
      extraLines = list(
        str2lang(paste0(k21, "<-(", aob, "*", beta, "+", alpha, ")/(", aob, "+1)")),
        str2lang(paste0(kel, "<-(", alpha, "*", beta, ")/", k21)),
        str2lang(paste0(k12, "<-", alpha, "+", beta, "-", k21, "-", kel))
      )
    )
    return(.ui)
  } else if (type == "k21") {
    if (.cmt == 1L) {
      stop("k21 transformation only works for 2 and 3 compartment models", call. = FALSE)
    }
    if (.cmt == 2L) {
      .est <- stats::setNames(
        c(
          paste0("Periph1->Central constant (", k21, ")"),
          paste0("alpha macro constant (", alpha, ")"),
          paste0("beta macro constant (", beta, ")"),
          paste0("Volume of central compartment (", vc, ")")
        ),
        c(k21, alpha, beta, vc)
      )
      .ui <- addLogEstimates(
        .ui,
        .est,
        beforeCmt = beforeCmt,
        extraLines = list(
          str2lang(paste0(kel, "<-", alpha, "*", beta, "/", k21)),
          str2lang(paste0(k12, "<-", alpha, "+", beta, "-", k21, "-", kel))
        )
      )
      return(.ui)
    } else {
      assertVariableNew(.ui, p)
      assertVariableNew(.ui, s)
      .est <- stats::setNames(
        c(
          paste0("Periph1->Central constant (", k21, ")"),
          paste0("Periph2->Central constant (", k31, ")"),
          paste0("alpha macro constant (", alpha, ")"),
          paste0("beta macro constant (", beta, ")"),
          paste0("gam macro constant (", gam, ")"),
          paste0("Volume of central compartment (", vc, ")")
        ),
        c(k21, k31, alpha, beta, gam, vc)
      )
      .ui <- addLogEstimates(
        .ui,
        .est,
        beforeCmt = beforeCmt,
        extraLines = list(
          str2lang(paste0(kel, "<-", alpha, "*", beta, "*", gam, "/(", k21, "*", k31, ")")),
          str2lang(paste0(s, "<-", alpha, "+", beta, "+", gam)),
          str2lang(paste0(p, "<-", alpha, "*", beta, "+", alpha, "*", gam, "+", beta, "*", gam)),
          str2lang(paste0(
            k13,
            "<- (",
            p,
            "+",
            k31,
            "*",
            k31,
            "-",
            k31,
            "*",
            s,
            "-",
            kel,
            "*",
            k21,
            ")/(",
            k21,
            "-",
            k31,
            ")"
          )),
          str2lang(paste0(k12, "<-", s, "-", kel, "-", k13, "-", k21, "-", k31))
        )
      )
      return(.ui)
    }
  } else if (type == "alpha") {
    if (.cmt == 3L) {
      # trans 10
      assertVariableNew(.ui, tmp)
      .est <- stats::setNames(
        c(
          paste0("alpha macro constant (", alpha, ")"),
          paste0("beta macro constant (", beta, ")"),
          paste0("gam macro constant (", gam, ")"),
          paste0("A coefficient (", A, ")"),
          paste0("B coefficient (", B, ")"),
          paste0("C coefficent (", C, ")")
        ),
        c(alpha, beta, gam, A, B, C)
      )
      assertVariableNew(.ui, s)
      assertVariableNew(.ui, p)
      .ui <- addLogEstimates(
        .ui,
        .est,
        beforeCmt = beforeCmt,
        extraLines = list(
          str2lang(paste0(vc, "<- 1/(", A, "+", B, "+", C, ")")),
          str2lang(paste0(
            s,
            "<- -(",
            alpha,
            "*",
            C,
            "+",
            alpha,
            "*",
            B,
            "+",
            gam,
            "*",
            A,
            "+",
            gam,
            "*",
            B,
            "+",
            beta,
            "*",
            A,
            "+",
            beta,
            "*",
            C,
            ")*",
            vc
          )),
          str2lang(paste0(
            p,
            "<- (",
            alpha,
            "*",
            beta,
            "*",
            C,
            "+",
            alpha,
            "*",
            gam,
            "*",
            B,
            "+",
            beta,
            "*",
            gam,
            "*",
            A,
            ")*",
            vc
          )),
          str2lang(paste0(tmp, "<- sqrt(", p, "*", p, "-4*", s, ")")),
          str2lang(paste0(k21, "<- 0.5*(-", p, "+", tmp, ")")),
          str2lang(paste0(k31, "<- 0.5*(-", p, "-", tmp, ")")),
          str2lang(paste0(kel, "<-", alpha, "*", beta, "*", gam, "/(", k21, "*", k31, ")")),
          str2lang(paste0(
            k12,
            "<- ((",
            beta,
            "*",
            gam,
            "+",
            alpha,
            "*",
            beta,
            "+",
            alpha,
            "*",
            gam,
            ") - ",
            k21,
            "*(",
            alpha,
            "+",
            beta,
            "+",
            gam,
            ")-",
            kel,
            "*",
            k31,
            "+",
            k21,
            "*",
            k21,
            ")/(",
            k31,
            "-",
            k21,
            ")"
          )),
          str2lang(paste0(k13, "<-", alpha, "+", beta, "+", gam, "-(", kel, "+", k12, "+", k21, "+", k31, ")"))
        )
      )
      return(.ui)
    } else if (.cmt == 2L) {
      .est <- stats::setNames(
        c(
          paste0("alpha macro constant (", alpha, ")"),
          paste0("beta macro constant (", beta, ")"),
          paste0("A coefficient (", A, ")"),
          paste0("B coefficient (", B, ")")
        ),
        c(alpha, beta, A, B)
      )
      .ui <- addLogEstimates(
        .ui,
        .est,
        beforeCmt = beforeCmt,
        extraLines = list(
          str2lang(paste0(vc, "<-1/(", A, "+", B, ")")),
          str2lang(paste0(k21, "<-(", A, "*", beta, "+", B, "*", alpha, ")*", vc)),
          str2lang(paste0(kel, "<-", alpha, "*", beta, "/", k21)),
          str2lang(paste0(k12, "<-", alpha, "+", beta, "-", k21, "-", kel))
        )
      )
      return(.ui)
    } else if (.cmt == 1L) {
      .est <- stats::setNames(
        c(
          paste0("alpha macro constant (", alpha, ")"),
          paste0("A coefficient (", A, ")")
        ),
        c(alpha, A)
      )
      .ui <- addLogEstimates(
        .ui,
        .est,
        beforeCmt = beforeCmt,
        extraLines = list(str2lang(paste0(kel, "<-", alpha)), str2lang(paste0(vc, "<- 1/", A)))
      )
      return(.ui)
    }
  }
  stop("should not get here", call. = FALSE) # nocov
}
