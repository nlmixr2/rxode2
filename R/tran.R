.rxModelVarsLast <- NULL

#' Internal translation to get model variables list
#'
#'
#' @param model Model (either file name or string)
#' @param linear boolean indicating if linear compartment model should
#'   be generated from `linCmt()` (default FALSE)
#' @param linCmtSens Linear compartment model sensitivity type
#' @param verbose is a boolean indicating the type of model detected
#'   with `linCmt()` parsing
#' @param code is a file name where the c code is written to (for
#'   testing purposes mostly, it needs `rxode2` to do anything fancy)
#' @param envir is the environment to look for R user functions
#'   (defaults to parent environment)
#' @return A rxModelVars object that has the model variables of a
#'   rxode2 syntax expression
#' @export
#' @importFrom Rcpp evalCpp
#' @importFrom dparser dparse
#' @importFrom utils capture.output
#' @importFrom stats setNames
#' @importFrom methods is
#' @importFrom utils assignInMyNamespace
#' @importFrom compiler cmpfun
#' @examples
#' rxode2parse("a=3")
rxode2parse <- function(
  model,
  linear = FALSE,
  linCmtSens = c("linCmtA", "linCmtB"),
  verbose = FALSE,
  code = NULL,
  envir = parent.frame()
) {
  rxParseSuppressMsg()
  .udfEnvSet(envir)
  checkmate::assertCharacter(model, len = 1, any.missing = FALSE)
  if (file.exists(model)) {
    .isStr <- 0L
  } else {
    .isStr <- 1L
  }
  modelPrefix <- ""
  fullPrint <- FALSE
  md5 <- digest::digest(model)
  .ret <- .Call(
    `_rxode2_trans`,
    model,
    modelPrefix,
    md5,
    .isStr,
    as.integer(crayon::has_color()),
    "",
    .rxSupportedFuns(),
    fullPrint
  )
  if (linear && .isLinCmt()) {
    .vars <- c(.ret$params, .ret$lhs, .ret$slhs)
    .ret <- .Call(
      `_rxode2_linCmtGen`,
      length(.ret$state),
      .vars,
      setNames(
        c(
          "linCmtA" = 1L,
          "linCmtB" = 2L
        )[match.arg(linCmtSens)],
        NULL
      ),
      verbose
    )
    md5 <- digest::digest(.ret)
    .ret <- .Call(
      `_rxode2_trans`,
      .ret,
      modelPrefix,
      md5,
      .isStr,
      as.integer(crayon::has_color()),
      "",
      .rxSupportedFuns(),
      fullPrint
    )
  }
  md5 <- c(
    file_md5 = md5,
    parsed_md5 = digest::digest(c(
      .ret$model,
      .ret$ini,
      .ret$state,
      .ret$params,
      .ret$lhs,
      .ret$alag
    ))
  )
  .ret$timeId <- -1L
  .ret$md5 <- md5
  if (.isStr == 1L) {
    ## Now update trans.
    .prefix <- paste0("rx_", md5["parsed_md5"], "_", .Platform$r_arch, "_")
    .libName <- substr(.prefix, 0, nchar(.prefix) - 1)
    .ret <- .Call(`_rxode2_rxUpdateTrans_`, .ret, .prefix, .libName)
  }
  ## dparser::dpReload();
  ## rxReload()
  if (is.character(code)) {
    .libname <- gsub("[.]c$", "", code)
    .libname <- c(.libname, .libname)
    .ret[[17]] <- list()
    .Call(
      `_rxode2_codegen`,
      code,
      .prefix,
      .libname,
      md5["parsed_md5"],
      "-1",
      .ret,
      .rxSupportedFuns(),
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      ""
    )
  }
  .ret
}
