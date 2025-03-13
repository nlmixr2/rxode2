.isIntegerish <- function(v) {
  checkmate::testIntegerish(v)
}
#' This gives the iCov index when generating keep from iCov
#'
#'
#' @param fullData Full data index information
#'
#'  This dataset has an "id" from the input event table and idx0 a
#'  0-based index of the full dataset
#'
#' @param iCovData This dataset has an "id" from the input `iCov`
#'   dataset and "idxi" which is a zero based `iCov` index
#'
#' @return An C-based index of where each item from the `iCov`
#'   data-frame to create the keep data-est as if the `iCov` came from
#'   the first dataset
#' @author Matthew L. Fidler
#' @noRd
.getIcovIdx <- function(fullData, iCovData) {
  .m <- merge(fullData, iCovData, by="id", all.x=TRUE)
  .m <- .m[order(.m$idx0),]
  .m$idxi
}

.convertExtra <- function(dat) {
  d <- as.data.frame(dat)
  .colNames0 <- colnames(d)
  .colNames <- toupper(.colNames0)
  ## Handle DATE TIME; DAT1 TIME; DAT2 TIME and DAT3 TIME

  ## Note NONMEM handles dates of the format DAY-MONTH and DAY as
  ## well for the DATE class of objects.

  ## It is too complex to handle, and not very common so it will
  ## throw an error

  .doDate <- FALSE
  .dupDate <- gettext("dates can only be specified by one of: 'DATE', 'DAT1', 'DAT2', 'DAT3' / 'TIME'")
  .checkBad <- function(d) {
    d <- paste(d)
    if (any(unlist(lapply(strsplit(d, "[^0-9]+"), length)) != 3)) {
      stop("dates formatted as MONTH-DAY or DAY alone are not supported in this conversion",
           call. = FALSE
           )
    }
    return(d)
  }
  if (any(.colNames == "DATE")) {
    ##  Month Day Year
    .datReg2 <- rex::rex(start, any_spaces, capture(numbers), non_numbers, capture(numbers), non_numbers, capture(number, number), any_spaces, end)
    .datReg4 <- rex::rex(start, any_spaces, capture(numbers), non_numbers, capture(numbers), non_numbers, capture(number, number, number, number), any_spaces, end)
    dt <- .checkBad(d$DATE)
    d$DATE.TIME <- as.POSIXct(NA)
    w <- which(regexpr(.datReg2, dt) != -1)
    if (length(w) > 0) {
      d$DATE.TIME[w] <- as.POSIXct(paste(gsub(.datReg2, "\\1-\\2-\\3", dt[w]), d$TIME[w]), format = "%m-%d-%y %H:%M")
    }
    w <- which(regexpr(.datReg4, dt) != -1)
    if (length(w) > 0) {
      d$DATE.TIME[w] <- as.POSIXct(paste(gsub(.datReg4, "\\1-\\2-\\3", dt[w]), d$TIME[w]), format = "%m-%d-%Y %H:%M")
    }
    d <- d[, -which(names(d) == "DATE")]
    .doDate <- TRUE
  }
  if (any(.colNames == "DAT1")) {
    if (.doDate) {
      stop(.dupDate, call. = FALSE)
    }
    ## DAT1   day month year
    .datReg2 <- rex::rex(start, any_spaces, capture(numbers), non_numbers, capture(numbers), non_numbers, capture(number, number), any_spaces, end)
    .datReg4 <- rex::rex(start, any_spaces, capture(numbers), non_numbers, capture(numbers), non_numbers, capture(number, number, number, number), any_spaces, end)
    dt <- .checkBad(d$DAT1)
    d$DATE.TIME <- as.POSIXct(NA)
    w <- which(regexpr(.datReg2, dt) != -1)
    if (length(w) > 0) {
      d$DATE.TIME[w] <- as.POSIXct(paste(gsub(.datReg2, "\\1-\\2-\\3", dt[w]), d$TIME[w]), format = "%d-%m-%y %H:%M")
    }
    w <- which(regexpr(.datReg4, dt) != -1)
    if (length(w) > 0) {
      d$DATE.TIME[w] <- as.POSIXct(paste(gsub(.datReg4, "\\1-\\2-\\3", dt[w]), d$TIME[w]), format = "%d-%m-%Y %H:%M")
    }
    d <- d[, -which(names(d) == "DAT1")]
    .doDate <- TRUE
  }
  if (any(.colNames == "DAT2")) {
    ## DAT2   year month day
    if (.doDate) {
      stop(.dupDate, call. = FALSE)
    }
    .datReg2 <- rex::rex(start, any_spaces, capture(number, number), non_numbers, capture(numbers), non_numbers, capture(numbers), any_spaces, end)
    .datReg4 <- rex::rex(start, any_spaces, capture(number, number, number, number), non_numbers, capture(numbers), non_numbers, capture(numbers), any_spaces, end)
    dt <- .checkBad(d$DAT2)
    d$DATE.TIME <- as.POSIXct(NA)
    w <- which(regexpr(.datReg2, dt) != -1)
    if (length(w) > 0) {
      d$DATE.TIME[w] <- as.POSIXct(paste(gsub(.datReg2, "\\1-\\2-\\3", dt[w]), d$TIME[w]), format = "%y-%m-%d %H:%M")
    }
    w <- which(regexpr(.datReg4, dt) != -1)
    if (length(w) > 0) {
      d$DATE.TIME[w] <- as.POSIXct(paste(gsub(.datReg4, "\\1-\\2-\\3", dt[w]), d$TIME[w]), format = "%Y-%m-%d %H:%M")
    }
    d <- d[, -which(names(d) == "DAT2")]
    .doDate <- TRUE
  }
  if (any(.colNames == "DAT3")) {
    ## DAT3   year day month
    if (.doDate) {
      stop(.dupDate, call. = FALSE)
    }
    .datReg2 <- rex::rex(start, any_spaces, capture(number, number), non_numbers, capture(numbers), non_numbers, capture(numbers), any_spaces, end)
    .datReg4 <- rex::rex(start, any_spaces, capture(number, number, number, number), non_numbers, capture(numbers), non_numbers, capture(numbers), any_spaces, end)
    dt <- .checkBad(d$DAT3)
    d$DATE.TIME <- as.POSIXct(NA)
    w <- which(regexpr(.datReg2, dt) != -1)
    if (length(w) > 0) {
      d$DATE.TIME[w] <- as.POSIXct(paste(gsub(.datReg2, "\\1-\\2-\\3", dt[w]), d$TIME[w]), format = "%y-%d-%m %H:%M")
    }
    w <- which(regexpr(.datReg4, dt) != -1)
    if (length(w) > 0) {
      d$DATE.TIME[w] <- as.POSIXct(paste(gsub(.datReg4, "\\1-\\2-\\3", dt[w]), d$TIME[w]), format = "%Y-%d-%m %H:%M")
    }
    d <- d[, -which(names(d) == "DAT3")]
    .doDate <- TRUE
  }
  if (.doDate) {
    if (any(is.na(d$DATE.TIME))) {
      stop("date/time format was not correctly specified", call. = FALSE)
    }
  }
  if (.doDate) {
    ## Sort by date/time (though this should have been done already...)
    if (!any(names(d) == "ID")) {
      d$ID <- 1L
    }
    if (!any(names(d) == "EVID")) {
      d$EVID <- 0L
    }
    d <- d[order(d$ID, d$DATE.TIME, -d$EVID), ]
    d$TIME <- as.vector(unlist(sapply(unique(d$ID), function(id) {
      d0 <- d[d$ID == id, ]
      return(as.numeric(difftime(d0$DATE.TIME,
                                 d0$DATE.TIME[1],
                                 units = "hours"
                                 )))
    })))
    d <- d[, -which(names(d) == "DATE.TIME")]
  }
  if (is(d$TIME, "numeric") || is(d$TIME, "integer")) {
    return(d)
  }
  stop("cannot figure out numeric time", call. = FALSE)
}

.lastIdLvl <- NULL

.setLastIdLvl <- function(idLvl) {
  assignInMyNamespace(".lastIdLvl", idLvl)
}
#' Get the last `idLvl`
#'
#' @return Last `idLvl`
#' @author Matthew L. Fidler
#' @keywords internal
#' @examples
#' .getLastIdLvl()
#' @export
.getLastIdLvl <- function() {
  .lastIdLvl
}

#' @export
as.data.frame.rxEtTran <- function(x, row.names = NULL, optional = FALSE, ...) {
  .Call(`_rxode2_rxEtTransAsDataFrame_`, x)
}

#' Get the compartment numbers based on a model
#'
#' @param mv  object where model variables can be extracted
#' @param cmt compartment numbers to translate; default is 1...nState
#' @return Translated (and named) compartment numbers
#' @noRd
#' @author Matthew L. Fidler
.getCmtNum <- function(mv, cmt) {
  .mv <- rxModelVars(mv)
  if (missing(cmt)) {
    cmt <- seq_along(.mv$state)
  }
  .cmt <- getCmtNum_(cmt, .mv)
  setNames(.cmt, vapply(.cmt, function(x) {
    .mv$state[x]
  }, character(1), USE.NAMES = FALSE))
}
