#' Remove model lines
#'
#' @param modelLines model line expression
#' @param lhs left handed variable
#' @returns model with lhs assignments dropped without checking
#'
#' @noRd
#' @author Matthew L. Fidler
.removeLines <- function(modelLines, lhs) {
  # Look for assignment of elimination in model
  .w <- vapply(
    seq_along(modelLines),
    function(i) {
      .matchesLangTemplate(modelLines[[i]], str2lang(paste0(lhs, " <- ."))) ||
        .matchesLangTemplate(modelLines[[i]], str2lang(paste0(lhs, "=.")))
    },
    logical(1),
    USE.NAMES = FALSE
  )
  .w <- which(!.w)
  .seq <- seq_along(modelLines)[.w]
  lapply(.seq, function(i) modelLines[[i]])
}

#' Replace multiplication expressions
#'
#' @param x expression to modify
#' @param v1 expression for the first multiplication term
#' @param v2 expression for the second multiplication term
#' @param ret return expression that v1*v2 will be replaced
#' @returns expression with multiplications replaced with ret
#'
#' @noRd
#' @author Matthew L. Fidler
.replaceMultC <- function(x, v1, v2, ret) {
  if (is.call(x)) {
    if (
      length(x) == 3 &&
        identical(x[[1]], quote(`*`))
    ) {
      .neg <- FALSE
      .x20 <- x[[2]]
      if (
        length(x[[2]]) == 2 &&
          identical(x[[2]][[1]], quote(`+`))
      ) {
        x[[2]] <- x[[2]][[2]]
      }
      if (
        length(x[[2]]) == 2 &&
          identical(x[[2]][[1]], quote(`-`))
      ) {
        .neg <- TRUE
        x[[2]] <- x[[2]][[2]]
      }
      .matchForward <- identical(x[[2]], v1) && identical(x[[3]], v2)
      .matchReverse <- identical(x[[3]], v1) && identical(x[[2]], v2)
      if (.matchForward || .matchReverse) {
        if (.neg) {
          return(str2lang(paste0("-", deparse1(ret))))
        } else {
          return(ret)
        }
      }
      if (.neg) {
        x[[2]] <- .x20
      }
      as.call(lapply(x, .replaceMultC, v1 = v1, v2 = v2, ret = ret))
    } else {
      as.call(lapply(x, .replaceMultC, v1 = v1, v2 = v2, ret = ret))
    }
  } else {
    x
  }
}

#' Replace multiplication expressions
#'
#' @param modelLines model lines to replace multiplication expressions
#' @param v1 variable one to replace
#' @param v2 variable two to replace
#' @param ret new expression with multiplication replaced
#' @returns modelLines with multiplication expressions removedes
#'
#' @noRd
#' @author Matthew L. Fidler
.replaceMult <- function(modelLines, v1, v2, ret) {
  if (!is.list(modelLines)) {
    modelLines <- list(modelLines)
  }
  .v1 <- str2lang(v1)
  .v2 <- str2lang(v2)
  .ret <- str2lang(ret)
  lapply(seq_along(modelLines), function(i) {
    as.call(.replaceMultC(modelLines[[i]], v1 = .v1, v2 = .v2, ret = .ret))
  })
}
#' Drop thetas in the theta section of an iniDf
#'
#' @param theta theta iniDf data.frame
#' @param pars  parameters to drop
#' @returns theta data frame
#'
#' @noRd
#' @author Matthew L. Fidler
.dropTheta <- function(theta, pars) {
  .theta <- theta
  .w <- which(.theta$name %in% pars)
  if (length(.w) > 0) {
    # These are directly estimated, drop
    .theta <- .theta[-.w, , drop = FALSE]
    .theta$ntheta <- seq_along(.theta$name)
  }
  .theta
}

#' Drop the etas in the eta iniDf data frame
#'
#' @param eta eta iniDf data.frame
#' @param pars parameters
#' @returns eta data frame
#'
#' @noRd
#' @author Matthew L. Fidler
.dropEta <- function(eta, pars) {
  .eta <- eta
  .w <- which(.eta$name %in% pars)
  if (length(.w) > 0) {
    # Convert to matrix and drop columns
    .e <- vapply(
      .w,
      function(i) {
        .eta$neta1[i]
      },
      double(1),
      USE.NAMES = FALSE
    )
    .eta <- .eta[!(.eta$neta1 %in% .e), , drop = FALSE]
    .eta <- .eta[!(.eta$neta2 %in% .e), , drop = FALSE]
    if (length(.eta$name) > 0) {
      .eta <- .eta[order(.eta$neta1, .eta$neta2), ]
      .eta$neta1 <- as.integer(factor(.eta$neta1))
      .eta$neta2 <- as.integer(factor(.eta$neta2))
    }
  }
  .eta
}

#' Drops a single line from a model and recursively removes items
#'
#' @param ui original ui
#' @param modelLines model lines
#' @param theta iniDf theta name
#' @param eta iniDf eta name
#' @param par1 a single parameter to remove from the model
#' @return list of modelLines, theta, and eta
#'
#' @noRd
#' @author Matthew L. Fidler
.dropLine1 <- function(ui, modelLines, theta, eta, par1) {
  .line <- modelExtract(ui, par1)
  if (length(.line) == 0) {
    return(list(modelLines = modelLines, theta = theta, eta = eta))
  }
  .modelLines <- .removeLines(modelLines, par1)
  .vars <- rxModelVars(.line)$params
  .theta <- .dropTheta(theta, .vars)
  .eta <- .dropEta(eta, .vars)
  for (.v in .vars) {
    .ret <- .dropLine1(ui, .modelLines, .theta, .eta, .v)
    .theta <- .ret$theta
    .eta <- .ret$eta
    .modelLines <- .ret$modelLines
  }
  list(modelLines = .modelLines, theta = .theta, eta = .eta)
}
#' Drop the lines from the model
#'
#'
#' @param ui original ui
#' @param modelLines model lines where this will be dropped
#' @param theta theta section of iniDf
#' @param eta eta section of iniDf
#' @param vars variables to drop
#' @return list of modelLines, theta, and eta
#' @noRd
#' @author Matthew L. Fidler
.dropLines <- function(ui, modelLines, theta, eta, vars) {
  .modelLines <- modelLines
  .theta <- theta
  .eta <- eta
  for (.v in vars) {
    .ret <- .dropLine1(ui, .modelLines, .theta, .eta, .v)
    .theta <- .ret$theta
    .eta <- .ret$eta
    .modelLines <- .ret$modelLines
  }
  list(modelLines = .modelLines, theta = .theta, eta = .eta)
}
#' Split an iniDf into its population and between-subject halves
#'
#' @param ui rxode2 ui function
#' @return A list of the whole `iniDf`, its `theta` rows and its `eta` rows.
#'   Every element is a row subset of `ui$iniDf`, so callers that recombine
#'   them with `rbind()` make no assumption about its columns -- which belong
#'   to lotri and do change.  New parameters are added with `.iniAddTheta()`
#'   rather than by building a row here.
#' @noRd
#' @author Matthew L. Fidler
.getEtaTheta <- function(ui) {
  .iniDf <- ui$iniDf
  list(
    iniDf = .iniDf,
    theta = .iniDf[is.na(.iniDf$neta1), , drop = FALSE],
    eta = .iniDf[!is.na(.iniDf$neta1), , drop = FALSE]
  )
}

#' Convert models from linear elimination to Michaelis-Menten elimination
#'
#' @param ui model to convert
#' @param central the central compartment where the elimination is present
#' @param elimination variable for the elimination constant in the
#'   model
#' @param vm variable name for Vmax in the model
#' @param km variable name for Km in the model
#' @param vc variable name for Vc in the model
#' @returns new model changing linear elimination to Michaelis-Menten elimination
#'
#' @family Distribution and elimination building blocks
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
#' pk1cmtDes |> convertMM()
#'
#' pk2cmtDes |> convertMM()
#'
#' pk3cmtDes |> convertMM()
#'
#' pk3cmtDes |> removeDepot() |> convertMM()
convertMM <- function(ui, central = "central", elimination = "kel", vm = "vm", km = "km", vc = "vc") {
  assertVariableName(elimination)
  assertVariableName(vm)
  assertVariableName(km)
  assertVariableName(vc)
  .ui <- assertRxUi(ui)
  assertCompartmentExists(.ui, central)
  .tmp <- .getEtaTheta(.ui)
  .iniDf <- .tmp$iniDf
  .theta <- .tmp$theta
  .eta <- .tmp$eta
  .line <- modelExtract(.ui, elimination)
  .modelLines <- .ui$lstExpr
  if (identical(.line, character(0))) {
    # line not in model; kel estimated directly
    .theta <- .dropTheta(.theta, elimination)
    .eta <- .dropEta(.eta, elimination)
  } else {
    .modelLines <- .removeLines(.modelLines, elimination)
    .vars <- rxModelVars(.line)$params
    .vars <- .vars[!(.vars %in% c(vm, km, vc))]
    .theta <- .dropTheta(.theta, .vars)
    .eta <- .dropEta(.eta, .vars)
    .ret <- .dropLines(.ui, .modelLines, .theta, .eta, .vars)
    .modelLines <- .ret$modelLines
    .theta <- .ret$theta
    .eta <- .ret$eta
  }
  .ui <- rxUiDecompress(.ui)
  # .theta/.eta are subsets of this model's own iniDf (the elimination
  # parameters dropped), so binding them assumes nothing about its columns;
  # the new parameters go in via ini().
  .ui$iniDf <- rbind(.theta, .eta)
  .model <- c(
    list(str2lang(paste0(vm, " <- exp(l", vm, ")")), str2lang(paste0(km, " <- exp(l", km, ")"))),
    .replaceMult(
      .modelLines,
      elimination,
      central,
      paste0("(", vm, "*", central, "/", vc, ")/(", km, "+", central, "/", vc, ")")
    )
  )
  if (exists("description", envir = .ui$meta)) {
    rm("description", envir = .ui$meta)
  }
  model(.ui) <- .model
  .ui <- .iniAddTheta(.ui, paste0("l", vm))
  .iniAddTheta(.ui, paste0("l", km))
}
