#' Split bolus doses across compartments inside an rxode2 model
#'
#' @description
#' `splitBolus()` is a model-only directive that rewrites bolus doses
#' aimed at `cmt` into parallel bolus doses to the target compartments.
#' Each target compartment receives the full original amount. The
#' source-compartment bolus is replaced, not retained.
#'
#' This rewrite applies both to event tables translated by [etTrans()]
#' and to future doses scheduled during solving with [evid_()].
#'
#' The source compartment may also appear in the target list, but the
#' target compartments in `...` must be unique.
#'
#' @param cmt Source compartment name.
#' @param ... Target compartment names. Provide at least two target
#'   compartments.
#'
#' @return This function is only meaningful inside an rxode2 model; it
#'   errors when called directly from R.
#'
#' @export
splitBolus <- function(cmt, ...) {
  stop("'splitBolus()' can only be used inside an rxode2 model block", call. = FALSE)
}

#' @export
#' @keywords internal
#' @rdname splitBolus
rxUdfUi.splitBolus <- function(fun) {
  .args <- vapply(as.list(fun)[-1], deparse1, character(1), USE.NAMES = FALSE)
  list(replace = paste0("splitBolus(", paste(.args, collapse = ","), ")"))
}
