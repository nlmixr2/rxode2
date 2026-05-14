##' Save the pre-integration rxode2 solver state to a binary file
##'
##' This is called automatically when \code{rxControl(serializeFile="path")} is
##' supplied to \code{rxSolve()}.  The file can be reloaded later by passing it
##' as the \code{params} argument of \code{rxSolve()} together with the matching
##' model object.
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
#' @param object Optional rxode2 model object used to persist model identity
#' @noRd
.rxSaveStateBundle <- function(file, cState, object = NULL) {
  .model <- rxModels_()
  .bundle <- list(
    cState = cState,
    keepFcov = .model$keepFcov,
    keepFcovType = .model$keepFcovType,
    idLevels = .model$idLevels,
    modelId = .rxSerializeModelId(object)
  )
  .rawBundle <- serialize(.bundle, NULL)
  .compressedBundle <- memCompress(.rawBundle, type = "xz")
  writeBin(.compressedBundle, file)
}

#' Internal helper to return the parsed model md5 for serialization validation
#' @param object Optional rxode2 model object
#' @return Character scalar or NULL when model identity is unavailable
#' @noRd
.rxSerializeModelId <- function(object = NULL) {
  if (is.null(object)) {
    return(NULL)
  }
  .mv <- rxModelVars(object)
  .md5 <- .mv$md5["parsed_md5"]
  if (length(.md5) != 1L || is.na(.md5) || identical(unname(.md5), "")) {
    return(NULL)
  }
  unname(.md5)
}

#' Internal helper to read a serialized rxode2 state bundle
#' @param file Serialization file path
#' @return Bundled serialized state
#' @noRd
.rxReadStateBundle <- function(file) {
  checkmate::assertCharacter(file, len = 1L, any.missing = FALSE)
  if (!file.exists(file)) {
    stop("File does not exist", call. = FALSE)
  }
  .sz <- file.info(file)$size
  .rawDat <- readBin(file, "raw", .sz)
  .decompressed <- memDecompress(.rawDat, type = "xz")
  unserialize(.decompressed)
}

#' Internal helper to restore R-side metadata from a serialized bundle
#' @param bundle Deserialized bundle
#' @return Invisibly TRUE
#' @noRd
.rxRestoreStateBundle <- function(bundle) {
  .rxm <- rxModels_()
  if (!is.null(bundle$keepFcov)) {
    assign("keepFcov", bundle$keepFcov, envir = .rxm)
  } else {
    assign("keepFcov", NULL, envir = .rxm)
  }
  if (!is.null(bundle$keepFcovType)) {
    assign("keepFcovType", bundle$keepFcovType, envir = .rxm)
  } else {
    assign("keepFcovType", NULL, envir = .rxm)
  }
  if (!is.null(bundle$idLevels)) {
    assign("idLevels", bundle$idLevels, envir = .rxm)
  } else {
    assign("idLevels", NULL, envir = .rxm)
  }
  invisible(TRUE)
}

#' Internal helper to ensure a serialized bundle matches the supplied model
#' @param object rxode2 model object
#' @param bundle Deserialized bundle
#' @param file Serialization file path
#' @return Invisibly TRUE
#' @noRd
.rxValidateStateBundleModel <- function(object, bundle, file) {
  .modelId <- bundle$modelId
  if (is.null(.modelId) || length(.modelId) != 1L || is.na(.modelId) ||
      identical(.modelId, "")) {
    stop(sprintf("Serialization file '%s' is missing model identity", file),
         call. = FALSE)
  }
  .currentId <- .rxSerializeModelId(object)
  if (is.null(.currentId)) {
    stop("Unable to determine model identity for serialized solve", call. = FALSE)
  }
  if (!identical(.currentId, .modelId)) {
    stop(sprintf("Serialization file '%s' does not match the supplied model", file),
         call. = FALSE)
  }
  invisible(TRUE)
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
      .bundle <- .rxReadStateBundle(file)
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
##' detects a serialization file by its magic bytes and validates that the file
##' matches the supplied model.
##'
##' @param file Character(1).  Path of a file written by \code{\link{rxSaveState}}.
##' @return Invisibly \code{TRUE}.
##' @seealso \code{\link{rxSaveState}}
##' @export
rxLoadState <- function(file) {
  checkmate::assertCharacter(file, len = 1L, any.missing = FALSE)
  .bundle <- .rxReadStateBundle(file)
  .rxRestoreStateBundle(.bundle)
  rxRestoreState_(.bundle$cState)
  invisible(TRUE)
}
