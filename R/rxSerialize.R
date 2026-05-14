##' Save the pre-integration rxode2 solver state to a binary file
##'
##' This is called automatically when \code{rxControl(serializeFile="path")} is
##' supplied to \code{rxSolve()}.  The file can be reloaded later by passing it
##' as the \code{params} argument of \code{rxSolve()}.
##'
##' @param file Character(1).  Path of the file to write.
##' @return Invisibly \code{TRUE}.
##' @seealso \code{\link{rxLoadState}}
##' @export
rxSaveState <- function(file) {
  checkmate::assertCharacter(file, len = 1L, any.missing = FALSE)
  .cState <- rxSaveState_()
  .rxSaveStateBundle(file, .cState)
  invisible(TRUE)
}

#' Internal function to bundle the C state with R metadata and write to disk
#' @param file The file path
#' @param cState The raw vector from C++
#' @noRd
.rxSaveStateBundle <- function(file, cState) {
  .model <- rxModels_()
  .bundle <- list(
    cState = cState,
    keepFcov = .model$keepFcov,
    keepFcovType = .model$keepFcovType,
    idLevels = .model$idLevels
  )
  .rawBundle <- serialize(.bundle, NULL)
  .compressedBundle <- memCompress(.rawBundle, type = "xz")
  writeBin(.compressedBundle, file)
}

#' Check if a file is an rxode2 serialize file
#' @param file The file path
#' @return TRUE if it is a valid serialize file, FALSE otherwise
#' @noRd
.rxIsSerializeFile <- function(file) {
  if (!file.exists(file)) {
    return(FALSE)
  }
  .res <- tryCatch(
    {
      .sz <- file.info(file)$size
      .raw_dat <- readBin(file, "raw", .sz)
      .decompressed <- memDecompress(.raw_dat, type = "xz")
      .bundle <- unserialize(.decompressed)
      if (is.list(.bundle) && !is.null(.bundle$cState)) {
        rxIsSerializeFile_(.bundle$cState)
      } else {
        FALSE
      }
    },
    error = function(e) {
      FALSE
    }
  )
  .res
}

##' Restore a pre-integration rxode2 solver state from a binary file
##'
##' Reads a file written by \code{\link{rxSaveState}} (or via
##' \code{rxControl(serializeFile=...)}) and reconstructs the live solver state
##' so that ODE integration can proceed from the saved pre-integration
##' checkpoint.
##'
##' This function is called automatically when \code{rxSolve(mod, "state.rxbin")}
##' detects a serialization file by its magic bytes.
##'
##' @param file Character(1).  Path of a file written by \code{\link{rxSaveState}}.
##' @return Invisibly \code{TRUE}.
##' @seealso \code{\link{rxSaveState}}
##' @export
rxLoadState <- function(file) {
  checkmate::assertCharacter(file, len = 1L, any.missing = FALSE)
  if (!file.exists(file)) stop("File does not exist")
  .sz <- file.info(file)$size
  .rawDat <- readBin(file, "raw", .sz)
  .decompressed <- memDecompress(.rawDat, type = "xz")
  .bundle <- unserialize(.decompressed)

  .rxm <- rxModels_()
  if (!is.null(.bundle$keepFcov)) {
    assign("keepFcov", .bundle$keepFcov, envir = .rxm)
  } else {
    assign("keepFcov", NULL, envir = .rxm)
  }
  if (!is.null(.bundle$keepFcovType)) {
    assign("keepFcovType", .bundle$keepFcovType, envir = .rxm)
  } else {
    assign("keepFcovType", NULL, envir = .rxm)
  }
  if (!is.null(.bundle$idLevels)) {
    assign("idLevels", .bundle$idLevels, envir = .rxm)
  } else {
    assign("idLevels", NULL, envir = .rxm)
  }

  rxRestoreState_(.bundle$cState)
  invisible(TRUE)
}
