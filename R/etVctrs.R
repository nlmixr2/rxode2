#' rxEt vctrs helpers
#'
#' `rxEt` behaves like a data-frame subclass for vctrs/dplyr operations, but it
#' stores its mutable state in an attached environment. These helpers round-trip
#' through a canonical materialized data set and rebuild a valid env-backed
#' object on restore instead of copying stale attributes onto a plain frame.
#'
#' @keywords internal
#' @importFrom vctrs vec_cast vec_ptype2 vec_proxy vec_restore
NULL

#' Get the meta data for an `rxEt` object
#'
#' @param x `rxEt` object
#' @return A list showing the units, show settings, randomType, and
#'   canResize properties of the `rxEt` object
#' @noRd
#' @author Matthew L. Fidler
.rxEtMeta <- function(x) {
  .meta <- list(
    units = c(dosing = NA_character_, time = NA_character_),
    show = .etDefaultShow(),
    randomType = NA_integer_,
    canResize = TRUE
  )
  .env <- tryCatch(.rxEtEnv(x), error = function(e) NULL)
  if (is.environment(.env)) {
    .meta$units <- .env$units
    .meta$show <- .env$show
    .meta$randomType <- .env$randomType
    .meta$canResize <- .env$canResize
  }
  .lst <- attr(attr(x, "class"), ".rxode2.lst", exact = TRUE)
  if (is.na(.meta$randomType) && !is.null(.lst$randomType)) {
    .meta$randomType <- as.integer(.lst$randomType)
  }
  .meta
}
#' Combine the units of two `rxEt` objects, checking for consistency
#'
#' This is used when combining two `rxEt` objects to ensure that their
#' dosing and time units are compatible. If one of the objects has
#' missing or NA units, it will take the units from the other
#' object. If both objects have non-missing units that are not
#' identical, an error will be thrown.
#'
#' @param x The first `rxEt` object (or its meta data)
#' @param y The second `rxEt` object (or its meta data)
#' @param what A string indicating whether we are combining "dose" or
#'   "time" units, used for error messages
#' @return The combined units, which will be the non-missing units if one is
#'  missing, or the common units if both are non-missing and identical
#' @noRd
#' @author Matthew L. Fidler
.rxEtMetaCombineUnits <- function(x, y, what) {
  if (is.null(x) || length(x) == 0L || isTRUE(is.na(x)) || identical(x, "")) {
    return(y)
  }
  if (is.null(y) || length(y) == 0L || isTRUE(is.na(y)) || identical(y, "")) {
    return(x)
  }
  if (!identical(unname(x), unname(y))) {
    stop(sprintf("cannot combine rxEt objects with different %s units ('%s' vs '%s')",
                 what, x, y),
         call. = FALSE)
  }
  x
}
#' Combine the meta data of two `rxEt` objects
#'
#'
#' @param x The first `rxEt` object (or its meta data)
#' @param y The second `rxEt` object (or its meta data)
#' @return A list containing the combined meta data, where the units
#'   are combined using `.rxEtMetaCombineUnits`, the show settings are
#'   combined using a logical OR, the randomType is the maximum of the
#'   two (ignoring NA), and canResize is TRUE only if both are TRUE.
#' @noRd
#' @author Matthew L. Fidler
#' @examples
.rxEtMetaCombine <- function(x, y) {
  .x <- .rxEtMeta(x)
  .y <- .rxEtMeta(y)
  .show <- .etDefaultShow() # nolint
  .show[names(.x$show)] <- as.logical(.x$show)
  .show[names(.y$show)] <- .show[names(.y$show)] | as.logical(.y$show)
  list(
    units = c(
      dosing = unname(.rxEtMetaCombineUnits(.x$units["dosing"], .y$units["dosing"], "dose")),
      time = unname(.rxEtMetaCombineUnits(.x$units["time"], .y$units["time"], "time"))
    ),
    show = .show,
    randomType = suppressWarnings(max(c(.x$randomType, .y$randomType), na.rm = TRUE)),
    canResize = isTRUE(.x$canResize) && isTRUE(.y$canResize)
  )
}

#' Make sure the meta data list has all the expected fields and types, filling in
#' defaults where necessary.
#'
#' @param meta A list containing meta data fields such as units, show,
#'   randomType, and canResize.
#' @return normalized meta data list with all expected fields and types
#' @noRd
#' @author Matthew L. Fidler
.rxEtNormalizeMeta <- function(meta) {
  .ret <- list(
    units = c(dosing = NA_character_, time = NA_character_),
    show = .etDefaultShow(), # nolint
    randomType = NA_integer_,
    canResize = TRUE
  )
  if (is.null(meta)) return(.ret)
  if (!is.null(meta$units)) .ret$units[names(meta$units)] <- meta$units
  if (!is.null(meta$show)) {
    .ret$show[names(meta$show)] <- as.logical(meta$show)
  }
  if (!is.null(meta$randomType)) .ret$randomType <- as.integer(meta$randomType)
  if (!is.null(meta$canResize)) .ret$canResize <- isTRUE(meta$canResize)
  if (is.infinite(.ret$randomType)) .ret$randomType <- NA_integer_
  .ret
}

.rxEtPrototype <- function(.meta = NULL) {
  .meta <- .rxEtNormalizeMeta(.meta)
  .et <- .newRxEt(amountUnits = .meta$units["dosing"], timeUnits = .meta$units["time"])
  .env <- .rxEtEnv(.et)
  .env$show[names(.meta$show)] <- .meta$show
  .env$randomType <- .meta$randomType
  .env$canResize <- .meta$canResize
  .proxy <- .etEmptyDf()
  attr(.proxy, "class") <- c("rxEt", "data.frame")
  attr(.proxy, ".rxEtEnv") <- .env
  .proxy
}

.rxEtAsFullDataFrame <- function(x) {
  if (is.rxEt(x)) return(as.data.frame(x, all = TRUE))
  as.data.frame(x, stringsAsFactors = FALSE)
}

.rxEtRebuild <- function(x, to) {
  .meta <- .rxEtNormalizeMeta(.rxEtMeta(to))
  .df <- .rxEtAsFullDataFrame(x)
  .et <- .newRxEt(amountUnits = .meta$units["dosing"], timeUnits = .meta$units["time"])
  .env <- .rxEtEnv(.et)
  if (nrow(.df) > 0L) {
    .env$methods$import.EventTable(.df)
  }
  .env$show[names(.meta$show)] <- .env$show[names(.meta$show)] | .meta$show
  if (!is.na(.meta$randomType)) .env$randomType <- .meta$randomType
  .env$canResize <- .meta$canResize
  .proxy <- .etMaterialize(.et)
  attr(.proxy, "class") <- c("rxEt", "data.frame")
  attr(.proxy, ".rxEtEnv") <- .env
  .proxy
}

.rxEtAsDataTable <- function(x) {
  data.table::as.data.table(.rxEtAsFullDataFrame(x))
}

.rxEtAsTibble <- function(x) {
  rxReq("tibble")
  tibble::as_tibble(.rxEtAsFullDataFrame(x))
}

#' @export
vec_proxy.rxEt <- function(x, ...) {
  .df <- .etMaterialize(x)
  attr(.df, ".rxEtMeta") <- .rxEtMeta(x)
  .df
}

#' @export
vec_restore.rxEt <- function(x, to, ...) {
  .rxEtRebuild(x, to)
}

#' @export
vec_ptype2.rxEt.rxEt <- function(x, y, ...) {
  vec_ptype2(.rxEtAsFullDataFrame(x), .rxEtAsFullDataFrame(y), ...)
}

#' @export
vec_ptype2.rxEt.data.frame <- function(x, y, ...) {
  vec_ptype2(.rxEtAsFullDataFrame(x), y, ...)
}

#' @export
vec_ptype2.data.frame.rxEt <- function(x, y, ...) {
  vec_ptype2(x, .rxEtAsFullDataFrame(y), ...)
}

#' @export
vec_ptype2.rxEt.data.table <- function(x, y, ...) {
  vec_ptype2(.rxEtAsDataTable(x), y, ...)
}

#' @export
vec_ptype2.data.table.rxEt <- function(x, y, ...) {
  vec_ptype2(x, .rxEtAsDataTable(y), ...)
}

#' @export
vec_ptype2.rxEt.tbl_df <- function(x, y, ...) {
  vec_ptype2(.rxEtAsTibble(x), y, ...)
}

#' @export
vec_ptype2.tbl_df.rxEt <- function(x, y, ...) {
  vec_ptype2(x, .rxEtAsTibble(y), ...)
}

#' @export
vec_ptype2.rxEt.tibble <- function(x, y, ...) {
  vec_ptype2.rxEt.tbl_df(x, y, ...)
}

#' @export
vec_ptype2.tibble.rxEt <- function(x, y, ...) {
  vec_ptype2.tbl_df.rxEt(x, y, ...)
}

#' @export
vec_cast.rxEt.rxEt <- function(x, to, ...) {
  .rxEtRebuild(.rxEtAsFullDataFrame(x), to)
}

#' @export
vec_cast.rxEt.data.frame <- function(x, to, ...) {
  .rxEtRebuild(as.data.frame(x, stringsAsFactors = FALSE), to)
}

#' @export
vec_cast.data.frame.rxEt <- function(x, to, ...) {
  vec_cast(.rxEtAsFullDataFrame(x), to, ...)
}

#' @export
vec_cast.rxEt.data.table <- function(x, to, ...) {
  .rxEtRebuild(as.data.frame(x), to)
}

#' @export
vec_cast.data.table.rxEt <- function(x, to, ...) {
  vec_cast(.rxEtAsDataTable(x), to, ...)
}

#' @export
vec_cast.rxEt.tbl_df <- function(x, to, ...) {
  .rxEtRebuild(as.data.frame(x), to)
}

#' @export
vec_cast.tbl_df.rxEt <- function(x, to, ...) {
  vec_cast(.rxEtAsTibble(x), to, ...)
}

#' @export
vec_cast.rxEt.tibble <- function(x, to, ...) {
  vec_cast.rxEt.tbl_df(x, to, ...)
}

#' @export
vec_cast.tibble.rxEt <- function(x, to, ...) {
  vec_cast.tbl_df.rxEt(x, to, ...)
}

dplyr_reconstruct.rxEt <- function(data, template) {
  if (inherits(data, "rxEt")) {
    class(data) <- setdiff(class(data), "rxEt")
  }
  data
}
