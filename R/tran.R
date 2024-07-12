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
#' @importFrom qs qsave
#' @importFrom dparser dparse
#' @importFrom utils capture.output
#' @importFrom stats setNames
#' @importFrom methods is
#' @importFrom utils assignInMyNamespace
#' @examples
#' rxode2parse("a=3")
rxode2parse <- function(model, linear=FALSE, linCmtSens = c("linCmtA", "linCmtB", "linCmtC"), verbose=FALSE,
                        code=NULL, envir=parent.frame()) {
  rxParseSuppressMsg()
  .udfEnvSet(envir)
  checkmate::assertCharacter(model, len=1, any.missing=FALSE)
  if (file.exists(model)) {
    .isStr <- 0L
  } else {
    .isStr <- 1L
  }
  modelPrefix <- ""
  fullPrint <- FALSE
  md5 <-   digest::digest(model)
  .ret <- .Call(
    `_rxode2_trans`, model, modelPrefix, md5, .isStr,
    as.integer(crayon::has_color()),
    "", .parseEnv$.parseFuns,
    fullPrint
  )
  if (linear && .isLinCmt()) {
    .vars <- c(.ret$params, .ret$lhs, .ret$slhs)
    .ret <- .Call(`_rxode2parse_linCmtGen`,length(.ret$state), .vars,
                  setNames(
                    c(
                      "linCmtA" = 1L, "linCmtB" = 2L,
                      "linCmtC" = 3L
                    )[match.arg(linCmtSens)],
                    NULL
                  ), verbose)
    md5 <- digest::digest(.ret)
    .ret <- .Call(`_rxode2parse_trans`, .ret, modelPrefix, md5, .isStr,
                  as.integer(crayon::has_color()),
                  "", .parseEnv$.parseFuns,
                  fullPrint)
  }
  md5 <- c(file_md5 = md5, parsed_md5 = digest::digest(c(
    .ret$model,
    .ret$ini,
    .ret$state,
    .ret$params,
    .ret$lhs,
    .ret$alag
  )))
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
      `_rxode2parse_codegen`, code, .prefix, .libname,
            md5["parsed_md5"], "-1",
            .ret, .parseEnv$.parseFuns)
  }
  .ret
}

rxode2parseFuns <- function() {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("this requires devtools", call.=FALSE)
  }
  message("rebuild parseFuns.R from rxode2")
  try(source(devtools::package_file("build/refresh.R")))
  message("done")
  ""
}

#' This assigns the c level linkages for a roxde2 model
#'
#' @param df data frame containing the character column names rxFun,
#'   fun, type, package, packageFun and the integer column names
#'   argMin and argMax
#' @return Nothing called for side effects
#' @author Matthew L. Fidler
#' @export
#' @examples
#'
#' rxode2parseAssignTranslation(rxode2parseGetTranslation())
#'
rxode2parseAssignTranslation <- function(df) {
  .char <- c("rxFun", "fun", "type", "package", "packageFun")
  .int <- c("argMin", "argMax", "threadSafe")
  .df <- df[,c(.char, .int)]
  for (.c in .char) {
    .df[[.c]] <- as.character(.df[[.c]])
  }
  for (.i in .int) {
    .df[[.i]] <- as.integer(.df[[.i]])
  }
  assign(".rxode2parseDf", .df, envir=.parseEnv)
  invisible(.df)
}

#' This function gets the currently assigned translations
#'
#' @return The currently assigned translations
#' @author Matthew L. Fidler
#' @export
#' @examples
#' rxode2parseGetTranslation()
rxode2parseGetTranslation <- function() {
  .parseEnv$.rxode2parseDf
}

rxode2parseGetTranslationBuiltin <- function() {
  data.frame(n=names(.parseEnv$.parseNum), i=as.integer(setNames(.parseEnv$.parseNum, NULL)))
}


.parseEnv$.packagesToLoad <- c("rxode2ll", "lotri")

#'@rdname rxode2parseAssignPackagesToLoad
#'@export
rxode2parseGetPackagesToLoad <- function() {
  .parseEnv$.packagesToLoad
}

#' Control the packages that are loaded when a `rxode2` model dll is loaded
#'
#' @param pkgs The packages to make sure are loaded every time you load an rxode2 model.
#' @return List of packages to load
#' @author Matthew Fidler
#' @examples
#'
#' rxode2parseGetPackagesToLoad()
#'
#' rxode2parseAssignPackagesToLoad(rxode2parseGetPackagesToLoad())
#' @export
rxode2parseAssignPackagesToLoad <- function(pkgs=rxode2parseGetPackagesToLoad()) {
  assign(".packagesToLoad", pkgs, envir=.parseEnv)
  pkgs
}


.parseEnv$.rxode2parsePointerAssignment <- "rxode2"

#' This function gets the currently assigned function pointer assignments
#'
#' @return The currently assigned pointer assignments
#' @author Matthew L. Fidler
#' @export
#' @examples
#' rxode2parseGetTranslation()
rxode2parseGetPointerAssignment <- function() {
  .parseEnv$.rxode2parsePointerAssignment
}


#' This sets function gets the currently assigned function pointer assignments
#'
#' @param var List of packages where pointer assignment will be called.
#'
#' @return Nothing, called for side effects
#' @author Matthew L. Fidler
#' @keywords internal
#' @export
#' @examples
#' rxode2parseAssignPointerTranslation("rxode2parse")
rxode2parseAssignPointerTranslation <- function(var) {
  checkmate::assertCharacter(var)
  assign(".rxode2parsePointerAssignment", var, envir=.parseEnv)
  invisible()
}
