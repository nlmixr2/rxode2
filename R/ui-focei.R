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
    eval(parse(text=paste0("quote(THETA[", .iniDf$ntheta[i],"] <- ", .iniDf$name[i], ")")))
  })
  .etas <- NULL
  .i2 <- .iniDf[-.w, ]
  if (length(.i2$name) > 0) {
    .i2 <- .i2[.i2$neta1 == .i2$neta2, ]
    .etas <- lapply(seq_along(.i2$name), function(i) {
      eval(parse(text=paste0("quote(ETA[", .i2$neta1[i],"] <- ", .i2$name[i], ")")))
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
                      paramsLine=.uiGetThetaEtaParams(.f),
                      modelVars=TRUE)
}
attr(rxUiGet.foceiModel0, "desc") <- "FOCEi model base"
