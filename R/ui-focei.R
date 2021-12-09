#' Get the THETA/ETA lines from rxode2 UI
#'
#' @param rxui This is the rxode2 ui object
#' @return The theta/eta lines
#' @author Matthew L. Fidler
#' @noRd
.uiGetThetaEta <- function(rxui) {
  .iniDf <- rxui$iniDf
  .w <- which(!is.na(.iniDf$ntheta))
  .thetas <- lapply(.w, function(i) {
    eval(parse(text=paste0("quote(", .iniDf$name[i], " <- THETA[", .iniDf$ntheta[i],"])")))
  })
  .etas <- NULL
  .i2 <- .iniDf[-.w, ]
  if (length(.i2$name) > 0) {
    .i2 <- .i2[.i2$neta1 == .i2$neta2, ]
    .etas <- lapply(seq_along(.i2$name), function(i) {
      eval(parse(text=paste0("quote(", .i2$name[i], " <- ETA[", .i2$neta1[i], "])")))
    })
  }
  c(.thetas, .etas)
}
#' Get the THETA/ETA params from the rxode2 UI
#'
#' @param rxui This is the rxode2 ui object
#' @return The params rxode2 UI
#' @author Matthew L. Fidler
#' @noRd
.uiGetThetaEtaParams <- function(rxui) {
  .iniDf <- rxui$iniDf
  .w <- which(!is.na(.iniDf$ntheta))
  .thetas <- vapply(.w, function(i) {
    paste0("THETA[", .iniDf$ntheta[i],"]")
  }, character(1), USE.NAMES=FALSE)
  .etas <- NULL
  .i2 <- .iniDf[-.w, ]
  if (length(.i2$name) > 0) {
    .i2 <- .i2[.i2$neta1 == .i2$neta2, ]
    .etas <- vapply(seq_along(.i2$name), function(i) {
      paste0("ETA[", .i2$neta1[i],"]")
    }, character(1), USE.NAMES=FALSE)
  }
  eval(parse(text=paste0("quote(params(", paste(c(.thetas, .etas), collapse=", "), "))")))
}

# This handles the errors for focei
.createFoceiLineObject <- function(x, line) {
  .predDf <- get("predDf", x)
  if (line > nrow(.predDf)) {
    return(NULL)
  }
  .predLine <- .predDf[line, ]
  .ret <- list(x, .predLine)
  class(.ret) <- c(paste(.predLine$distribution), "rxGetDistributionFoceiLines")
  .ret
}

#' This is a S3 method for getting the distribution lines for a base rxode2 focei problem
#'
#' @param line Parsed rxode2 model environment
#' @return Lines for the simulation of `ipred` and `dv`. This is based
#'   on the idea that the focei parameters are defined
#' @author Matthew Fidler
#' @keywords internal
#' @export
rxGetDistributionFoceiLines <- function(line) {
  UseMethod("rxGetDistributionFoceiLines")
}

#' @rdname rxGetDistributionFoceiLines
#' @export
rxGetDistributionFoceiLines.norm <- function(line) {
  env <- line[[1]]
  pred1 <- line[[2]]
  .handleSingleErrTypeNormOrTFoceiBase(env, pred1)
}

#' @rdname rxGetDistributionFoceiLines
#' @export
rxGetDistributionFoceiLines.t <- function(line) {
  stop("t isn't supported yet")
}

#' @rdname rxGetDistributionFoceiLines
#' @export
rxGetDistributionFoceiLines.default  <- function(line) {
  stop("Distribution not supported")
}

#' @rdname rxGetDistributionFoceiLines
#' @export
rxGetDistributionFoceiLines.rxUi <- function(line) {
  .predDf <- get("predDf", line)
  lapply(seq_along(.predDf$cond), function(c){
    .mod <- .createFoceiLineObject(line, c)
    rxGetDistributionFoceiLines(.mod)
  })
}

#' @rdname rxUiGet
#' @export
rxUiGet.foceiModel0 <- function(x, ...) {
  .f <- x[[1]]
  rxCombineErrorLines(.f, errLines=rxGetDistributionFoceiLines(.f),
                      prefixLines=.uiGetThetaEta(.f),
                      paramsLine=NA, #.uiGetThetaEtaParams(.f),
                      modelVars=TRUE,
                      cmtLines=FALSE,
                      dvidLine=FALSE)
}
attr(rxUiGet.foceiModel0, "desc") <- "FOCEi model base"


..foceiPrune <- function(x, fullModel=TRUE) {
  .x <- x[[1]]
  .x <- .x$foceiModel0[[-1]]
  .env <- new.env(parent = emptyenv())
  .env$.if <- NULL
  .env$.def1 <- NULL
  if (fullModel) {
    .malert("pruning branches ({.code if}/{.code else}) of full model...")
  } else {
    .malert("pruning branches ({.code if}/{.code else})...")
  }
  .ret <- .rxPrune(.x, envir = .env)
  .mv <- rxModelVars(.ret)
  ## Need to convert to a function
  if (.Call(`_rxode2_isLinCmt`) == 1L) {
    .vars <- c(.mv$params, .mv$lhs, .mv$slhs)
    .mv <- rxGetModel(.Call(
      `_rxode2_linCmtGen`,
      length(.mv$state),
      .vars, 1L, FALSE))
  }
  .msuccess("done")
  rxNorm(.mv)
}

..loadSymengine <- function(newmod, promoteLinSens = TRUE, fullModel = FALSE) {
  if (fullModel) {
    .malert("loading full model into {.pkg symengine} environment...")
  } else {
    .malert("loading into {.pkg symengine} environment...")
  }
  rxS(newmod, TRUE, promoteLinSens = promoteLinSens)
}

#' @rdname rxUiGet
#' @export
rxUiGet.loadPruneSens <- function(x, ...) {
  #..foceiPrune(x)
  ..loadSymengine(..foceiPrune(x), promoteLinSens = TRUE)
}
attr(rxUiGet.loadPruneSens, "desc") <- "load sensitivity with linCmt() promoted"


#' @rdname rxUiGet
#' @export
rxUiGet.loadPrune <- function(x, ...) {
  ..loadSymengine(..foceiPrune(x), promoteLinSens = FALSE)
}
attr(rxUiGet.loadPrune, "desc") <- "load sensitivity without linCmt() promoted"
