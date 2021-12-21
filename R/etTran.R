.expandPars <- function(object, params, events, control) {
  .Call(`_rxode2_expandPars_`, object, params, events, control,
    PACKAGE = "rxode2"
  )
}

.warnIdSort0 <- TRUE
#' Turn on/off warnings for ID sorting.
#'
#' @param warnIdSort Boolean for if the sorting warning is turned on
#'     or off.
#' @return Nothing
#' @author Matthew Fidler
#' @export
.setWarnIdSort <- function(warnIdSort = TRUE) {
  assignInMyNamespace(".warnIdSort0", warnIdSort)
  invisible()
}

.DTEnv <- NULL
.getDTEnv <- function() {
  if (is.null(.DTEnv)) {
    if (requireNamespace("data.table", quietly = TRUE)) {
      .env <- loadNamespace("data.table")
      if (utils::compareVersion(
        as.character(
          utils::packageVersion("data.table")
        ),
        "1.12.4"
      ) >= 0) {
        assignInMyNamespace(".DTEnv", .env)
        return(.env)
      }
    }
    .env <- new.env(parent = emptyenv())
    assignInMyNamespace(".DTEnv", .env)
    return(.env)
  } else {
    return(.DTEnv)
  }
}

#' This function sorts the parameter or iCov data based on the event
#' table data
#'
#' @param idData This is the individual parameter data or iCov data
#'
#' @param goodLvl These are the "good" levels based on the event
#'     table
#'
#' @param type Type can be "iCov" or "parameter"
#'
#' @param warnIdSort When `TRUE` warnings about merging the
#'     parameter/id with rxode2 event tables are issued.
#'
#' @return A sorted parameter table that can be used directly in the
#'     C-based routines.
#'
#' @author Matthew Fidler
#'
#' @noRd
.sortId <- function(idData, goodLvl, type = "parameter",
                    warnIdSort, skipStop = TRUE) {
  ## print(str(idData))
  .n <- tolower(names(idData))
  .w <- which(.n == "id")
  .nid <- length(goodLvl)
  if (length(.w) == 1) {
    if (.nid == 1 && length(idData[[.w]]) == 1) {
      .idData <- as.data.frame(idData)
      goodLvl <- paste(.idData[[.w]])
    } else {
      .idData <- as.data.frame(idData)
    }
    .oId <- .idData[[.w]]
    if (inherits(.idData[[.w]], "numeric")) {
      .idData[[.w]] <- factor(.idData[[.w]], levels = as.numeric(goodLvl), labels = goodLvl)
    } else {
      .idData[[.w]] <- factor(.idData[[.w]], levels = goodLvl, labels = goodLvl)
    }
    .wrn <- ""
    if (any(is.na(.idData[[.w]]))) {
      .w2 <- which(is.na(.idData[[.w]]))
      .oId <- unique(.oId[.w2])
      .wrn <- sprintf("Some IDs are in the %s dataset that are not in the event dataset.\nParameter information for these IDs were dropped (%s)", type, paste(.oId, collapse = ", "))
      .idData <- .idData[!is.na(.idData[[.w]]), ]
    }
    .idData <- .idData[order(.idData[[.w]]), ]
    .idData <- .idData[, -.w, drop = FALSE]
    if (length(.idData[, 1]) == 0) {
      if (skipStop) {
        return(.idData)
      } else {
        stop(sprintf(gettext("there are no individuals left to solve in %s data"), type),
          call. = FALSE
        )
      }
    }
    if (.wrn != "") warning(.wrn, call. = FALSE)
    return(.idData)
  } else if (length(.w) == 0L) {
    if (length(idData[, 1]) > 1) {
      if (warnIdSort && .warnIdSort0 && .nid > 1) {
        warning(sprintf(gettext("'ID' missing in '%s' dataset\nindividual parameters are assumed to have the same order as the event dataset"), type), call. = FALSE)
      }
    }
    return(as.data.frame(idData))
  } else {
    if (length(idData[, 1]) > 1) {
      warning(sprintf("unable to detect 'ID' correctly in '%s' dataset\nindividual parameters are assumed to have the same order as the dataset", type), call. = FALSE)
    }
    .idData <- idData[, -.w, drop = FALSE]
    return(as.data.frame(.idData))
  }
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
