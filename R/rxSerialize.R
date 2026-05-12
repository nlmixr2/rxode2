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
  invisible(rxSaveState_(file))
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
  invisible(rxRestoreState_(file))
}
