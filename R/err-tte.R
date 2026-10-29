# Time-to-event (tte) endpoints: `ev ~ tte(hazard, eventType=, maxEventNumber=,
# rightCensoringTime=, intervalLength=, dropout=)`.  Like ordinal endpoints the
# options are not stored in $predDf/$iniDf; they are re-read from the endpoint's
# expression in `lstExpr` with `.rxTteOptions()`.

.rxTteOptionNames <- c(
  "eventType",
  "maxEventNumber",
  "rightCensoringTime",
  "intervalLength",
  "dropout"
)

.rxTteEventTypes <- c("exact", "intervalCensored")

#' Parse and validate a tte() call
#'
#' @param expr the `tte(...)` call
#' @return list with `hazard` (quoted), the options (NA when unspecified,
#'   `eventType`/`maxEventNumber`/`dropout` filled with their defaults) and
#'   `err`, a character vector of problems (empty when valid)
#' @author Matthew L. Fidler
#' @noRd
.rxTteParseCall <- function(expr) {
  .ret <- list(
    hazard = NULL,
    eventType = "exact",
    maxEventNumber = Inf,
    rightCensoringTime = NA_real_,
    intervalLength = NA_real_,
    dropout = FALSE,
    err = character(0)
  )
  .args <- as.list(expr)[-1]
  .n <- names(.args)
  if (is.null(.n)) {
    .n <- rep("", length(.args))
  }
  .pos <- which(.n == "")
  if (length(.pos) == 0L) {
    .ret$err <- "'tte()' requires the hazard as its first argument (i.e. ev ~ tte(haz))"
    return(.ret)
  }
  if (length(.pos) > 1L || .pos[1] != 1L) {
    .ret$err <- "'tte()' takes the hazard as its only unnamed (first) argument; options must be named"
    return(.ret)
  }
  .ret$hazard <- .args[[1]]
  .bad <- setdiff(.n[-1], .rxTteOptionNames)
  if (length(.bad) > 0L) {
    .ret$err <- c(
      .ret$err,
      paste0(
        "unknown 'tte()' option(s): ",
        paste(.bad, collapse = ", "),
        "; supported: ",
        paste(.rxTteOptionNames, collapse = ", ")
      )
    )
  }
  if (anyDuplicated(.n[-1])) {
    .ret$err <- c(.ret$err, "duplicated 'tte()' option(s)")
  }
  .num <- function(.name, .allowInf = FALSE) {
    .v <- .args[[.name]]
    if (is.call(.v) && identical(.v[[1]], quote(`-`)) && length(.v) == 2L && is.numeric(.v[[2]])) {
      .v <- -.v[[2]]
    }
    if (is.name(.v) && identical(.v, quote(Inf))) {
      .v <- Inf
    }
    if (!is.numeric(.v) || length(.v) != 1L || is.na(.v) || (!.allowInf && !is.finite(.v)) || .v <= 0) {
      .ret$err <<- c(.ret$err, paste0("'tte()' option '", .name, "' must be a positive number"))
      return(NA_real_)
    }
    as.numeric(.v)
  }
  if (!is.null(.args$eventType)) {
    .v <- .args$eventType
    if (is.name(.v)) .v <- as.character(.v)
    if (!is.character(.v) || length(.v) != 1L || !(.v %in% .rxTteEventTypes)) {
      .ret$err <- c(
        .ret$err,
        paste0("'tte()' option 'eventType' must be one of: ", paste0("\"", .rxTteEventTypes, "\"", collapse = ", "))
      )
    } else {
      .ret$eventType <- .v
    }
  }
  if (!is.null(.args$maxEventNumber)) {
    .v <- .num("maxEventNumber", .allowInf = TRUE)
    if (!is.na(.v) && is.finite(.v) && .v != round(.v)) {
      .ret$err <- c(.ret$err, "'tte()' option 'maxEventNumber' must be a whole number (or Inf)")
    } else {
      .ret$maxEventNumber <- .v
    }
  }
  if (!is.null(.args$rightCensoringTime)) {
    .ret$rightCensoringTime <- .num("rightCensoringTime")
  }
  if (!is.null(.args$intervalLength)) {
    .ret$intervalLength <- .num("intervalLength")
  }
  if (!is.null(.args$dropout)) {
    .v <- .args$dropout
    if (is.name(.v)) .v <- c(T = TRUE, F = FALSE)[as.character(.v)]
    if (!is.logical(.v) || length(.v) != 1L || is.na(.v)) {
      .ret$err <- c(.ret$err, "'tte()' option 'dropout' must be TRUE or FALSE")
    } else {
      .ret$dropout <- .v
    }
  }
  if (isTRUE(.ret$dropout) && !identical(.ret$maxEventNumber, 1)) {
    .ret$err <- c(.ret$err, "'tte()' option 'dropout=TRUE' requires 'maxEventNumber=1'")
  }
  .ret
}

#' Handle the tte() error structure while parsing the model
#'
#' @param expression the `tte(...)` call
#' @param env parsing environment
#' @return nothing, called for side effects
#' @author Matthew L. Fidler
#' @noRd
.errHandleTte <- function(expression, env) {
  env$distribution <- "tte"
  .p <- .rxTteParseCall(expression)
  if (length(.p$err) > 0L) {
    env$err <- c(env$err, .p$err)
  }
  env$errTypeInfo <- rxErrTypeCombine(env$errTypeInfo, "tte")
  env$needsToBeAnErrorExpression <- TRUE
  invisible()
}

#' Get the tte() options of an endpoint
#'
#' @param env parsed ui environment (holds `lstExpr`)
#' @param pred1 single `$predDf` row of a tte endpoint
#' @return the list from `.rxTteParseCall()`
#' @author Matthew L. Fidler
#' @noRd
.rxTteOptions <- function(env, pred1) {
  .rhs <- env$lstExpr[[pred1$line[1]]][[3]]
  if (is.call(.rhs) && identical(.rhs[[1]], quote(`|`))) {
    .rhs <- .rhs[[2]]
  }
  .rxTteParseCall(.rhs)
}
