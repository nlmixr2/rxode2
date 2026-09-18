rxode2parseFuns <- function() {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("this requires devtools", call. = FALSE)
  }
  message("rebuild parseFuns.R from rxode2")
  try(source(devtools::package_file("build/refresh.R")), silent = TRUE)
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
  .df <- df[, c(.char, .int)]
  for (.c in .char) {
    .df[[.c]] <- as.character(.df[[.c]])
  }
  for (.i in .int) {
    .df[[.i]] <- as.integer(.df[[.i]])
  }
  assign(".rxode2parseDf", .df, envir = .parseEnv)
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
  data.frame(n = names(.parseEnv$.parseNum), i = as.integer(setNames(.parseEnv$.parseNum, NULL)))
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
rxode2parseAssignPackagesToLoad <- function(pkgs = rxode2parseGetPackagesToLoad()) {
  assign(".packagesToLoad", pkgs, envir = .parseEnv)
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
#' rxode2parseAssignPointerTranslation("rxode2")
rxode2parseAssignPointerTranslation <- function(var) {
  checkmate::assertCharacter(var)
  assign(".rxode2parsePointerAssignment", var, envir = .parseEnv)
  invisible()
}
