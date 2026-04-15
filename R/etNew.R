# Default show flags (controls which columns print/as.data.frame expose)
.etDefaultShow <- function() {
  c(id = FALSE, low = FALSE, time = TRUE, high = FALSE, cmt = FALSE,
    amt = FALSE, rate = FALSE, ii = FALSE, addl = FALSE, evid = TRUE,
    ss = FALSE, dur = FALSE)
}

#' Create a new empty rxEt object
#' @param amountUnits character dose unit, e.g. "mg"
#' @param timeUnits character time unit, e.g. "hours"
#' @return rxEt object
#' @noRd
.newRxEt <- function(amountUnits = NA_character_, timeUnits = NA_character_) {
  .env <- new.env(parent = emptyenv())
  .env$chunks    <- list()
  .env$units     <- c(dosing = amountUnits, time = timeUnits)
  .env$show      <- .etDefaultShow()
  .env$IDs       <- 1L
  .env$nobs      <- 0L
  .env$ndose     <- 0L
  .env$randomType <- NA_integer_
  .env$canResize  <- TRUE
  structure(list(.env = .env), class = "rxEt")
}

# Canonical column order matching C++ etEmpty()
.etColOrder <- c("id", "low", "time", "high", "cmt", "amt", "rate", "ii", "addl", "evid", "ss", "dur")

#' Empty data.frame skeleton matching materialized format
#' @noRd
.etEmptyDf <- function() {
  data.frame(
    id    = integer(0),
    low   = numeric(0),
    time  = numeric(0),
    high  = numeric(0),
    cmt   = character(0),
    amt   = numeric(0),
    rate  = numeric(0),
    ii    = numeric(0),
    addl  = integer(0),
    evid  = integer(0),
    ss    = integer(0),
    dur   = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' Materialize all chunks into a sorted data.frame
#' @param et rxEt object
#' @return data.frame with canonical columns, sorted by (id, time, evid-adjusted)
#' @noRd
.etMaterialize <- function(et) {
  .env <- .subset2(et, ".env")
  .chunks <- .env$chunks

  # Empty case — return skeleton
  if (length(.chunks) == 0L) {
    return(.etEmptyDf())
  }

  # Bind: data.table::rbindlist is ~10x faster than do.call(rbind, ...)
  # fill=TRUE handles sparse chunks (obs chunks lack amt/rate/ii/addl/ss/dur)
  .dt <- data.table::rbindlist(.chunks, fill = TRUE, use.names = TRUE)

  # ---- Fill column defaults ----
  if (is.null(.dt[["id"]]))   data.table::set(.dt, j = "id",   value = 1L)
  if (is.null(.dt[["evid"]])) data.table::set(.dt, j = "evid", value = 0L)
  if (is.null(.dt[["low"]]))  data.table::set(.dt, j = "low",  value = NA_real_)
  if (is.null(.dt[["high"]])) data.table::set(.dt, j = "high", value = NA_real_)
  if (is.null(.dt[["cmt"]]))  data.table::set(.dt, j = "cmt",  value = "(default)")

  # Dose-specific defaults — NA for obs records, 0 for dose records
  .isDose <- .dt$evid != 0L
  if (is.null(.dt[["amt"]]))  data.table::set(.dt, j = "amt",  value = NA_real_)
  if (is.null(.dt[["rate"]])) data.table::set(.dt, j = "rate", value = ifelse(.isDose, 0.0, NA_real_))
  if (is.null(.dt[["ii"]]))   data.table::set(.dt, j = "ii",   value = ifelse(.isDose, 0.0, NA_real_))
  if (is.null(.dt[["addl"]])) data.table::set(.dt, j = "addl", value = ifelse(.isDose, 0L,  NA_integer_))
  if (is.null(.dt[["ss"]]))   data.table::set(.dt, j = "ss",   value = ifelse(.isDose, 0L,  NA_integer_))
  if (is.null(.dt[["dur"]]))  data.table::set(.dt, j = "dur",  value = ifelse(.isDose, 0.0, NA_real_))

  # ---- Sort: id ASC, time ASC, evid=3 before others at same time ----
  # Map evid=3 -> -1 so it sorts first within same (id, time)
  .evidSort <- ifelse(.dt$evid == 3L, -1L, as.integer(.dt$evid))
  .ord <- .order3(.dt$id, .dt$time, .evidSort)
  .dt <- .dt[.ord, ]

  # ---- Ensure canonical column order ----
  .missing <- setdiff(.etColOrder, names(.dt))
  for (.col in .missing) data.table::set(.dt, j = .col, value = NA)
  data.table::setcolorder(.dt, .etColOrder)

  as.data.frame(.dt)
}

#' Check if object is an rxEt event table
#' @param x object to test
#' @return logical
#' @export
is.rxEt <- function(x) {
  if (!inherits(x, "rxEt")) return(FALSE)
  # New-style: list with .env environment (pure-R rewrite)
  .env <- .subset2(x, ".env")
  if (is.environment(.env)) return(TRUE)
  # Old-style: data.frame-based rxEt (C++ et_() output, has .rxode2.lst in class attr)
  !is.null(attr(class(x), ".rxode2.lst"))
}
