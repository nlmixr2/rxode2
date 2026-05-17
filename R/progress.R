#' rxode2 progress bar functions
#'
#' `rxProgress` sets up the progress bar
#'
#' `rxTick` is a progress bar tick
#'
#' `rxProgressStop` stop progress bar
#'
#' `rxProgressAbort` shows an abort if `rxProgressStop`
#' wasn't called.
#'
#' @param num Tot number of operations to track
#' @param core Number of cores to show.  If below 1, don't show
#'     number of cores
#' @param clear Boolean telling if you should clear the progress bar
#'     after completion (as if it wasn't displayed).  By default this is TRUE
#' @param error With rxProgressAbort this is the error that is displayed
#' @return All return NULL invisibly.
#' @author Matthew L. Fidler
#' @examples
#' 
#' f <- function() {
#'   on.exit({
#'     rxProgressAbort()
#'   })
#'   rxProgress(100)
#'   for (i in 1:100) {
#'     rxTick()
#'     Sys.sleep(1 / 100)
#'   }
#'   rxProgressStop()
#' }
#' \donttest{
#' f()
#' }
#'
#' @export
rxProgress <- function(num, core = 0L) {
  invisible(.Call(`_rxProgress`, as.integer(num), as.integer(core)))
}

#' @rdname rxProgress
#' @export
rxTick <- function() {
  invisible(.Call(`_rxTick`))
}

#' @rdname rxProgress
#' @export
rxProgressStop <- function(clear = TRUE) {
  invisible(.Call(`_rxProgressStop`, as.integer(clear)))
}

#' @rdname rxProgress
#' @export
rxProgressAbort <- function(error = "Aborted calculation") {
  invisible(.Call(`_rxProgressAbort`, error))
}
