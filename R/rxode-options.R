.ggplot2Fix <- function() {
  .ggplot2 <- loadNamespace("ggplot2")
  if (any(ls(.ggplot2) == "guide_none")) {
    assignInMyNamespace("guide_none", .ggplot2$guide_none)
  }
  if (compareVersion(as.character(packageVersion("ggplot2")), "3.3.6.9000") < 0) {
    assignInMyNamespace("GeomAmt",
                        ggplot2::ggproto("GeomAmt", ggplot2::GeomSegment,
                                         required_aes = c("x", "y", "xend", "yend"),
                                         default_aes = ggplot2::aes(colour = "black", linetype = "dotted", size = 0.5, alpha = 1, fill = "black")))
  } else {
    assignInMyNamespace("GeomAmt",
                        ggplot2::ggproto("GeomAmt", ggplot2::GeomSegment,
                                         required_aes = c("x", "y", "xend", "yend"),
                                         default_aes = ggplot2::aes(colour = "black", linetype = "dotted", linewidth = 0.5, alpha = 1, fill = "black")))
  }
}
.hasUnits <- FALSE
#' Get the rxode2 function pointers
#'
#' This function is used to get the function pointers for rxode2.  This is
#' used to allow rxode2 to have binary linkage to nlmixr2est.
#'
#' @return a list of function pointers
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' .rxode2ptrs()
.rxode2ptrs <- function() {
  .Call(`_rxode2_rxode2Ptr`, PACKAGE = "rxode2")
}

## nocov start
.onLoad <- function(libname, pkgname) {
  requireNamespace("data.table", quietly=TRUE)
  if (requireNamespace("pillar", quietly = TRUE)) {
    .s3register("pillar::type_sum", "rxEvid")
    .s3register("pillar::type_sum", "rxRateDur")
    .s3register("pillar::pillar_shaft", "rxEvid")
    .s3register("pillar::pillar_shaft", "rxRateDur")
  }
  if (requireNamespace("tibble", quietly = TRUE)) {
    .s3register("tibble::as_tibble", "rxEt")
  }
  if (requireNamespace("data.table", quietly = TRUE)) {
    .s3register("data.table::as.data.table", "rxEt")
  }
  if (requireNamespace("dplyr", quietly=TRUE)) {
    .s3register("dplyr::rename", "rxUi")
    .s3register("dplyr::rename", "function")
  }

  if (requireNamespace("nlme", quietly=TRUE)) {
    .s3register("nlme::fixef", "rxUi")
    .s3register("nlme::fixef", "function")
  }
  if (requireNamespace("units", quietly = TRUE)) {
    .s3register("units::set_units", "rxEt")
    .s3register("units::set_units", "rxRateDur")
    .s3register("units::drop_units", "rxEt")
    .s3register("units::units<-", "rxEvid")
    .s3register("units::drop_units", "rxSolve")
    assignInMyNamespace(".hasUnits", TRUE)
  } else {
    assignInMyNamespace(".hasUnits", FALSE)
  }
  backports::import(pkgname)
  ## Setup rxode2.prefer.tbl
  .Call(`_rxode2_setRstudio`, Sys.getenv("RSTUDIO") == "1")
  rxSyncOptions("permissive")
  rxTempDir()
  if (!interactive()) {
    setProgSupported(0)
  }
  .ggplot2Fix()
  .linkAll()
  forderForceBase(FALSE)
} ## nocov end

.iniLotriPtrs <- function() {
  .Call(`_iniLotriPtr`, lotri::.lotriPointers())
}

.iniPreciseSumsPtr <- function() {
  .Call(`_iniPreciseSumsPtr`, PreciseSums::.preciseSumsPtr())
}

.iniDparserPtr <- function() {
  .Call(`_rxode2_iniDparserPtr`, dparser::.dparsePtr())
}

.linkAll <- function() {
  .iniLotriPtrs()
  .iniPreciseSumsPtr()
  .iniDparserPtr()
}


.onAttach <- function(libname, pkgname) {
  ## For some strange reason, mvnfast needs to be loaded before rxode2 to work correctly
  .Call(`_rxode2_setRstudio`, Sys.getenv("RSTUDIO") == "1")
  rxSyncOptions("permissive")
  if (!interactive()) {
    setProgSupported(0)
  }
  .linkAll()

  rxTempDir()
  .ggplot2Fix()
  v <- utils::packageVersion("rxode2")
  packageStartupMessage(
    "rxode2 ", v, " using ", getRxThreads(verbose = FALSE),
    " threads (see ?getRxThreads)",
    ifelse(.cacheIsTemp, "\n  no cache: create with `rxCreateCache()`", "")
  )
  if (!.Call(`_rxHasOpenMp`)) {
    packageStartupMessage(
      "========================================\n",
      "rxode2 has not detected OpenMP support and will run in single-threaded mode\n",
      if (Sys.info()["sysname"] == "Darwin") {
        "This is a Mac. Please read https://mac.r-project.org/openmp/"
      } else {
        paste0("The system is ", Sys.info()["sysname"], "; To get best performance enable OpenMP")
      },
      "\n========================================\n"
    )
  }
  forderForceBase(FALSE)
}

.onUnload <- function(libpath) {
  ## nocov start
  rxUnloadAll()
  gc() # Force garbage collection finalization
  library.dynam.unload("rxode2", libpath)
  ## nocov end
}

.mkCache <- function(.tmp) {
  if (!file.exists(.tmp)) {
    dir.create(.tmp, recursive = TRUE, showWarnings = FALSE)
  } else if (!file.exists(file.path(.tmp, paste0(rxode2.md5, ".md5")))) {
    if (!.cacheIsTemp) packageStartupMessage("detected new version of rxode2, cleaning cache")
    unlink(.tmp, recursive = TRUE, force = TRUE)
    dir.create(.tmp, recursive = TRUE, showWarnings = FALSE)
    writeLines("rxode2", file.path(.tmp, paste0(rxode2.md5, ".md5")))
  }
}

.cacheIsTemp <- TRUE
.rxTempDir0 <- NULL
.cacheDefault <- NULL
#' Get the rxode2 temporary directory
#'
#' @return rxode2 temporary directory.
#' @export
rxTempDir <- function() {
  if (is.null(getFromNamespace(".rxTempDir0", "rxode2"))) {
    .tmp <- Sys.getenv("rxTempDir")
    .rxUserDir <- R_user_dir("rxode2", "cache")
    assignInMyNamespace(".cacheIsTemp", FALSE)
    if (!file.exists(.rxUserDir)) {
      .rxUserDir <- file.path(tempdir(), "rxode2")
      assignInMyNamespace(".cacheIsTemp", TRUE)
    }
    if (.tmp == "") {
      if (is.null(.cacheDefault)) {
        assignInMyNamespace(".cacheDefault", .rxUserDir)
      }
      if (getOption("rxode2.cache.directory", .cacheDefault) != ".") {
        .tmp <- getOption("rxode2.cache.directory", .cacheDefault)
      } else {
        .tmp <- .rxUserDir
      }
    }
    .mkCache(.tmp)
    .tmp <- .normalizePath(.tmp)
    Sys.setenv(rxTempDir = .tmp)
    utils::assignInMyNamespace(".rxTempDir0", .tmp)
    utils::assignInMyNamespace("rxode2.cache.directory", .tmp)
    return(.tmp)
  } else {
    .tmp <- getFromNamespace(".rxTempDir0", "rxode2")
    .mkCache(.tmp)
    utils::assignInMyNamespace("rxode2.cache.directory", .tmp)
    return(.tmp)
  }
}
#' This will create the cache directory for rxode2 to save between sessions
#'
#' When run, if the `R_user_dir` for rxode2's cache isn't present,
#' create the cache
#'
#' @return nothing
#'
#' @author Matthew Fidler
#'
#' @export
rxCreateCache <- function() {
  .tmp <- R_user_dir("rxode2", "cache")
  assignInMyNamespace(".cacheDefault", R_user_dir("rxode2", "cache"))
  .mkCache(.tmp)
  .tmp <- .normalizePath(.tmp)
  Sys.setenv(rxTempDir = .tmp)
  utils::assignInMyNamespace(".rxTempDir0", .tmp)
  utils::assignInMyNamespace("rxode2.cache.directory", .tmp)
  invisible()
}

#' Clear memoise cache for rxode2
#'
#' @author Matthew L. Fidler
#' @return nothing; called for side effects
#' @keywords internal
#' @export
rxForget <- function() {
  for (fn in ls(envir = getNamespace("rxode2"))) {
    if (memoise::is.memoised(getFromNamespace(fn, "rxode2"))) {
      memoise::forget(getFromNamespace(fn, "rxode2"))
    }
  }
}

## strict/permissive
rxOpt <- list(
  rxode2.prefer.tbl = c(FALSE, FALSE),
  rxode2.warn.on.assign = c(TRUE, TRUE),
  rxode2.syntax.allow.ini = c(FALSE, TRUE),
  rxode2.calculate.jacobian = c(FALSE, FALSE),
  rxode2.calculate.sensitivity = c(FALSE, FALSE),
  rxode2.verbose = c(TRUE, TRUE),
  rxode2.suppress.syntax.info = c(FALSE, FALSE),
  rxode2.sympy.engine = c("", ""),
  rxode2.cache.directory = c(.cacheDefault, .cacheDefault),
  rxode2.tempfiles = c(TRUE, TRUE),
  rxode2.sympy.run.internal = c(FALSE, FALSE),
  rxode2.syntax.require.ode.first = c(FALSE, FALSE),
  rxode2.compile.O = c("3", "3"),
  rxode2.unload.unused = c(FALSE, FALSE),
  rxode2.debug=c(FALSE, FALSE)
)

rxode2.prefer.tbl <- NULL
rxode2.warn.on.assign <- NULL
rxode2.syntax.allow.ini <- NULL
rxode2.calculate.jacobian <- NULL
rxode2.calculate.sensitivity <- NULL
rxode2.verbose <- NULL
rxode2.suppress.syntax.info <- NULL
rxode2.sympy.engine <- NULL
rxode2.cache.directory <- NULL
rxode2.delete.unnamed <- NULL
rxode2.tempfiles <- NULL
rxode2.sympy.run.internal <- NULL
rxode2.syntax.require.ode.first <- NULL
rxode2.compile.O <- NULL
rxode2.unload.unused <- NULL
rxode2.debug <- NULL

.isTestthat <- function() {
  return(regexpr("/tests/testthat/", getwd(), fixed = TRUE) != -1) # nolint
}
#' Respect suppress messages
#'
#' This turns on the silent REprintf in C when `suppressMessages()` is
#' turned on. This makes the `REprintf` act like `messages` in R,
#' they can be suppressed with `suppressMessages()`
#'
#' @return Nothing
#' @author Matthew Fidler
#' @export
#' @examples
#'
#' # rxSupressMsg() is called with rxode2()
#'
#' # Note the errors are output to the console
#'
#' try(rxode2("d/dt(matt)=/3"), silent = TRUE)
#'
#' # When using suppressMessages, the output is suppressed
#'
#' suppressMessages(try(rxode2("d/dt(matt)=/3"), silent = TRUE))
#'
#' # In rxode2, we use REprintf so that interrupted threads do not crash R
#' # if there is a user interrupt. This isn't captured by R's messages, but
#' # This interface allows the `suppressMessages()` to suppress the C printing
#' # as well
#'
#' # If you  want to suppress messages from rxode2 in other packages, you can use
#' # this function
rxSuppressMsg <- function() {
  if (requireNamespace("knitr", quietly = TRUE)) {
    if (!is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))) {
      return(invisible(NULL))
    } else {
      rxSetSilentErr(as.integer(length(capture.output(message(" "), type = "message")) == 0L))
    }
  } else {
    rxSetSilentErr(as.integer(length(capture.output(message(" "), type = "message")) == 0L))
  }
  invisible(NULL)
}

#' Sync options with rxode2 variables
#'
#' Accessing rxode2 options via getOption slows down solving.  This
#' allows the options to be synced with variables.
#'
#' @param setDefaults This will setup rxode2's default solving options with the following options:
#'
#' - `"none"` leave the options alone
#' - `"permissive"` This is a permissive option set similar to R language specifications.
#' - `"strict"` This is a strict option set similar to the original
#'    rxode2(). It requires semicolons at the end of lines and equals for
#'    assignment
#'
#' @author Matthew L. Fidler
#' @return nothing; called for side effects
#' @export
rxSyncOptions <- function(setDefaults = c("none", "permissive", "strict")) {
  x <- c(
    "none" = 0L, "permissive" = 2L,
    "strict" = 1L
  )[match.arg(setDefaults)]
  if (x > 0) {
    op.rx <- list()
    for (v in names(rxOpt)) {
      op.rx[[v]] <- getOption(v, rxOpt[[v]][x])
    }
    options(op.rx) # nolint
  }
  for (var in names(rxOpt)) {
    assignInMyNamespace(var, getOption(var, rxOpt[[var]][1]))
  }
}
