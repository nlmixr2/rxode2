#' Returns a list of physical drives that have been or currently are
#' mounted to the computer.
#'
#' This excludes network drives.  See
#' <https://www.forensicmag.com/article/2012/06/windows-7-registry-forensics-part-5>
#'
#' @param duplicates Return drives with duplicate entries in
#'     `SYSTEM\\MountedDevices`; These are likely removable media.  By default this is `FALSE`
#' @return Drives with letters
#' @author Matthew L. Fidler
#' @keywords internal
#' @export
rxPhysicalDrives <- memoise::memoise(function(duplicates = FALSE) {
  if (.Platform$OS.type == "unix") {
    return(NULL)
  } else {
    ## This lists all the drive letters (and volume
    ## information) of drives mounted to your computer.
    n <- names(utils::readRegistry("SYSTEM\\MountedDevices"))
    reg <- rex::rex(start, "\\DosDevices\\", capture(or("A":"Z", "a":"z"), ":"), end)
    ns <- n[regexpr(reg, n) != -1]
    if (length(n) > 0) {
      ns <- toupper(gsub(reg, "\\1", ns))
      dups <- unique(ns[duplicated(ns)])
      if (length(dups) > 1) {
        if (duplicates) {
          ## Duplicate drive names are more likely to be removable media letters (like usb/cd/etc.)
          d <- paste0(sort(unique(dups)), "\\")
          w <- which(!sapply(d, removableDrive))
          if (length(d) >= 1) {
            return(d)
          } else {
            return("C:\\")
          }
        } else {
          d <- paste0(sort(unique(ns[!(ns %in% dups)])), "\\")
          w <- which(!sapply(d, removableDrive))
          d <- d[w]
          if (length(d) >= 1) {
            return(d)
          } else {
            return("C:\\")
          }
        }
      } else {
        d <- paste0(sort(ns), "\\")
        w <- which(!sapply(d, removableDrive))
        d <- d[w]
        if (length(d) >= 1) {
          return(d)
        } else {
          return("C:\\")
        }
      }
      ret <- ns
    } else {
      ret <- "C:\\"
    }
    return(ret)
  }
})

.rxNlmixr <- memoise::memoise(function() {
  .keys <- NULL
  .keys <- try(utils::readRegistry(sprintf("SOFTWARE\\nlmixr%s", ifelse(.Platform$r_arch == "i386", "32", "")),
    hive = "HCU", maxdepth = 2
  ), silent = TRUE)
  if (!inherits(.keys, "try-error")) {
    .ca1 <- commandArgs()[[1]]
    if (.ca1 != "RStudio") {
      .ca1 <- .normalizePath(.ca1)
      if (regexpr(rex::rex(substring(.normalizePath(.keys[[1]]), 2)), .ca1) == -1) {
        return(list())
      }
    }
    .lst <- list(
      rtoolsBase = .normalizePath(file.path(.keys[[1]], "rtools"), winslash = "/", mustWork = FALSE)
    )
    if (dir.exists(.lst$rtoolsBase)) {
      return(.lst)
    }
  }
  return(list())
})

.rxRtoolsBaseWin <- memoise::memoise(function(retry = FALSE) {
  if (.Platform$OS.type == "unix" || getOption("rxode2.rtools.setup", FALSE)) {
    return("")
  } else if (file.exists(R.home("rtools"))) {
    return(R.home("rtools"))
  } else {
    return(NULL)
  }
})
#' Setup Rtools path
#'
#' @inheritParams rxWinSetup
#'
#' @param retry Should you retry to find Rtools?  If NA, don't throw
#'     an error if it isn't found.
#' @author Matthew L. Fidler
#' @noRd
.rxWinRtoolsPath <- function(rm.rtools = TRUE, retry = FALSE) {
  ## Note that devtools seems to assume that rtools/bin is setup
  ## appropriately, and figures out the c compiler from there.
  if (.Platform$OS.type == "unix" || getOption("rxode2.rtools.setup", FALSE)) {
    return(TRUE)
  } else if (Sys.which("make") != "") {
    return(TRUE)
  } else if (as.integer(R.version$major) >= 4) {
    if (file.exists(Sys.getenv("RTOOLS40_HOME"))) {
    } else if (file.exists(file.path(Sys.getenv("R_HOME"), "rtools40"))) {
      Sys.setenv("RTOOLS40_HOME" = .normalizePath(file.path(Sys.getenv("R_HOME"), "rtools40")))
    } else {
      return(FALSE)
    }
    Sys.setenv(PATH = paste0(
      .normalizePath(file.path(Sys.getenv("RTOOLS40_HOME"), "usr", "bin")),
      ";", Sys.getenv("PATH")
    ))
    return(TRUE)
  } else {
    .path <- unique(sapply(
      sub(rex::rex('"', end), "", sub(
        rex::rex(start, '"'), "",
        gsub("/", "\\\\", strsplit(Sys.getenv("PATH"), ";")[[1]])
      )),
      function(x) {
        if (file.exists(x)) {
          return(.normalizePath(x))
        } else {
          return("")
        }
      }
    ))
    .path <- .path[.path != ""]
    if (!inherits(rm.rtools, "logical")) rm.rtools <- FALSE
    if (rm.rtools) {
      .path <- .path[regexpr(rex::rex(or("Rtools", "RTOOLS", "rtools")), .path) == -1]
    }
    .rPath <- .normalizePath(file.path(Sys.getenv("R_HOME"), paste0("bin", Sys.getenv("R_ARCH"))))
    .path <- c(.rPath, .path)

    ## Look in the registry...
    ## This is taken from devtools and adapted.
    .rtoolsBase <- .rxRtoolsBaseWin(retry = retry)
    if (is.null(.rtoolsBase)) {
      return("")
    }
    .x <- file.path(.rtoolsBase, ifelse(.Platform$r_arch == "i386", "mingw_32/bin", "mingw_64/bin"))
    if (file.exists(.x)) {
      Sys.setenv(rxBINPREF = gsub("([^/])$", "\\1/", gsub("\\\\", "/", .normalizePath(.x))))
    }
    .exists <- try(file.exists(.rtoolsBase), silent = TRUE)
    if (inherits(.exists, "try-error")) .exists <- FALSE
    if (.exists) {
      .gcc <- list.files(.rtoolsBase, "gcc", full.names = TRUE)[1]
      if (is.na(.gcc)) {
        .gcc <- ""
      }
      ## This allows both toolchains to be present, but rxode2 should still work...
      for (.x in rev(c(
        file.path(.rtoolsBase, "bin"),
        file.path(.rtoolsBase, "pandoc"),
        file.path(.rtoolsBase, "qpdf/bin"),
        ## file.path(.rtoolsBase, "mingw_32/bin") ## Rtools sets up the mingw_32/bin first (even if x64)
        file.path(.rtoolsBase, ifelse(.Platform$r_arch == "i386", "mingw_32/bin", "mingw_64/bin")),
        file.path(.rtoolsBase, ifelse(.Platform$r_arch == "i386", "mingw_32/bin", "mingw_64/bin")),
        file.path(.rtoolsBase, ifelse(.Platform$r_arch == "i386", "mingw_32/opt/bin", "mingw_64/opt/bin"))
        ## ifelse(.gcc == "", "", file.path(.gcc, "bin")),
        ## ifelse(.gcc == "", "", ifelse(.Platform$r_arch == "i386",file.path(.gcc, "bin32"), file.path(.gcc, "bin64"))
        ## )
      ))) {
        if (file.exists(.x)) {
          .path <- c(.normalizePath(.x), .path)
        }
      }
      ## java <- as.vector(Sys.which("java"));
      ## if (java != ""){
      ##     java <- sub(rex::rex(one_of("/", "\\"), except_any_of("/", "\\", "\n"), end), "", java)
      ## }
      .keys <- NULL
      ## Is there a 64 bit aspell that should be checked for...?
      .keys <- try(utils::readRegistry("SOFTWARE\\Aspell", hive = "HLM", view = "32-bit", maxdepth = 3), silent = TRUE)
      ## Add aspell for cran checks...
      if (!is.null(.keys)) {
        if (any(names(.keys) == "Path")) {
          if (file.exists(.keys$Path)) {
            .path <- c(.normalizePath(.keys$Path), .path)
          }
        }
      }
      ## Last CRAN check for Rtools is qpdf
      .qpdf <- c(paste0(.rtoolsBase, "/qpdf/bin"), paste0(rxPhysicalDrives(), "/qpdf/bin"))
      for (.p in .qpdf) {
        if (file.exists(.p)) {
          .path <- c(.normalizePath(.p), .path)
          break
        }
      }
      .path <- .path[.path != ""]
      .path <- paste(unique(.path), collapse = ";")
      Sys.setenv(PATH = .path)
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}
#' Setup Windows components for rxode2
#'
#' @param rm.rtools Remove the Rtools from the current path specs.
#'
#' @author Matthew L. Fidler
#' @return nothing, used for its side effects
#' @export
rxWinSetup <- function(rm.rtools = TRUE) {
  if (!.rxWinRtoolsPath(rm.rtools = rm.rtools)) {
    message("rxode2 requires 'rtools' for custom models\nPlease download from http://cran.r-project.org/bin/windows/Rtools/,\ninstall and restart your R session before proceeding\n")
  }
}
