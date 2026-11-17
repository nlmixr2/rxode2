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
#'   argMin, argMax and threadSafe.  An optional integer gpuSafe column
#'   marks functions with a device (OpenCL) version.  Without it the
#'   functions are not gpu safe, except a row identical (rxFun, fun,
#'   package and packageFun) to one already registered, which keeps its
#'   value
#' @return Nothing called for side effects
#' @author Matthew L. Fidler
#' @family Developer API
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
  .gpu <- df$gpuSafe
  if (is.null(.gpu)) {
    ## a new or changed C function is not gpu safe; rows passed back
    ## unchanged (eg rbind() onto rxode2parseGetTranslation()) keep theirs
    .cur <- .parseEnv$.rxode2parseDf
    .key <- function(d) paste(d$rxFun, d$fun, d$package, d$packageFun, sep = "\r")
    .gpu <- if (is.null(.cur$gpuSafe)) 0L else .cur$gpuSafe[match(.key(.df), .key(.cur))]
  }
  .gpu <- rep_len(as.integer(.gpu), nrow(.df))
  .gpu[is.na(.gpu)] <- 0L
  .full <- .df
  .full$gpuSafe <- .gpu
  assign(".rxode2parseDf", .full, envir = .parseEnv)
  invisible(.df)
}

#' This function gets the currently assigned translations
#'
#' @return The currently assigned translations
#' @author Matthew L. Fidler
#' @family Developer API
#' @export
#' @examples
#' rxode2parseGetTranslation()
rxode2parseGetTranslation <- function() {
  ## the released columns: reverse dependencies rbind() rows onto this
  .df <- .parseEnv$.rxode2parseDf
  .df[, setdiff(names(.df), "gpuSafe"), drop = FALSE]
}

#' The translation table with the gpuSafe column (read by the parser)
#'
#' @return data frame
#' @noRd
#' @author Matthew L. Fidler
.rxode2parseGetTranslationFull <- function() {
  .parseEnv$.rxode2parseDf
}

## Builtin functions with a device (OpenCL) version: OpenCL C math or
## inst/include/rxode2gpu/.  Not here: the Bessel functions (R's versions
## allocate), transit(), linCmt(), the dosing/lag history functions and llik*().
.rxGpuSafeBuiltin <- c(
  "round",
  "beta",
  "digamma",
  "trigamma",
  "logspace_add",
  "logspace_sub",
  "fmax2",
  "fmin2",
  "fsign",
  "fprec",
  "fround",
  "ftrunc",
  "erfinv",
  "dabs",
  "dabs2",
  "abs1",
  "dabs1",
  "phiU",
  "probit",
  "probitInv",
  "rxTBS",
  "rxTBSi",
  "rxTBSd",
  "rxTBSd2",
  "rxTBSdL",
  "rxTBSdL2",
  "rxTBSdLx",
  "gammap",
  "gammaq",
  "gammapDer",
  "gammapInv",
  "gammapInva",
  "gammaqInv",
  "gammaqInva",
  "lowergamma",
  "uppergamma",
  "gammapDera",
  "ibeta",
  "ibetaDer",
  "ibetaInv",
  "ibetaDera",
  "ibetaDerb",
  "studentTDen",
  "studentTCdf",
  "studentTCdfDnu",
  "studentTInv",
  "lgamma",
  "abs",
  "acos",
  "acosh",
  "asin",
  "asinh",
  "atan",
  "atan2",
  "atanh",
  "cos",
  "cosh",
  "erf",
  "erfc",
  "exp",
  "gamma",
  "log",
  "sin",
  "sinh",
  "sqrt",
  "tan",
  "tanh",
  "floor",
  "ceil",
  "ceiling",
  "trunc",
  "sign",
  "max",
  "min",
  "logit",
  "expit",
  "gammafn",
  "lgammafn",
  "loggamma",
  "log1p",
  "expm1",
  "factorial",
  "lfactorial",
  "lgamma1p",
  "log10",
  "log2",
  "phi",
  "normcdf",
  "fabs",
  "pow",
  "R_pow",
  "R_pow_di",
  "Rx_pow_di",
  "Rx_pow",
  "is.nan",
  "is.na",
  "is.finite",
  "is.infinite",
  "!",
  "==",
  "!=",
  ">=",
  "<=",
  "<",
  ">",
  "&&",
  "||",
  "&",
  "|"
)

rxode2parseGetTranslationBuiltin <- function() {
  .n <- names(.parseEnv$.parseNum)
  data.frame(
    n = .n,
    i = as.integer(setNames(.parseEnv$.parseNum, NULL)),
    gpuSafe = as.integer(.n %in% .rxGpuSafeBuiltin)
  )
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
#' @family Developer API
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
#' @family Developer API
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
