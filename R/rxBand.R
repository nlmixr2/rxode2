## Structural bandwidth of the ODE Jacobian, from the normalized model text.
## Every dependency is a union over all assignments (ignoring order and taking
## if/while conditions as dependencies), so the band can only be too wide,
## never too narrow.

## lhs forms that do not change d/dt(): dosing modifiers and initial
## conditions (`f(x)`, `x(0)`, ...), and user Jacobian lines
.rxBandSkipLhs <- c("f", "F", "alag", "lag", "rate", "dur", "df")

## Name of the pseudo-variable holding d/dt(state)
.rxBandDdt <- function(state) {
  paste0("\001ddt\001", state)
}

## state named by a `d/dt(state)` call, or NULL
.rxBandDdtState <- function(e) {
  if (
    is.call(e) &&
      identical(e[[1]], quote(`/`)) &&
      length(e) == 3L &&
      identical(e[[2]], quote(d)) &&
      is.call(e[[3]]) &&
      identical(e[[3]][[1]], quote(dt)) &&
      length(e[[3]]) == 2L &&
      is.name(e[[3]][[2]])
  ) {
    return(as.character(e[[3]][[2]]))
  }
  NULL
}

## variables an expression reads; d/dt(x) reads the pseudo-variable for x
.rxBandVars <- function(e) {
  .s <- .rxBandDdtState(e)
  if (!is.null(.s)) {
    return(.rxBandDdt(.s))
  }
  if (is.name(e)) {
    return(as.character(e))
  }
  if (!is.call(e)) {
    return(character(0))
  }
  unique(unlist(lapply(as.list(e)[-1], .rxBandVars), use.names = FALSE))
}

## Collect the assignments of a statement into `.env$lhs`/`.env$rhs`; `ctl` are
## the variables of the enclosing conditions.  Returns FALSE on a form it does
## not understand.
.rxBandCollect <- function(e, ctl, .env) {
  if (!is.call(e)) {
    return(TRUE)
  }
  .f <- e[[1]]
  if (identical(.f, quote(`{`))) {
    for (.e in as.list(e)[-1]) {
      if (!.rxBandCollect(.e, ctl, .env)) {
        return(FALSE)
      }
    }
    return(TRUE)
  }
  if (identical(.f, quote(`if`)) || identical(.f, quote(`while`))) {
    .ctl <- unique(c(ctl, .rxBandVars(e[[2]])))
    for (.e in as.list(e)[-(1:2)]) {
      if (!.rxBandCollect(.e, .ctl, .env)) {
        return(FALSE)
      }
    }
    return(TRUE)
  }
  if (
    identical(.f, quote(`=`)) ||
      identical(.f, quote(`<-`)) ||
      identical(.f, quote(`~`))
  ) {
    if (length(e) != 3L) {
      return(TRUE)
    }
    .l <- e[[2]]
    .key <- .rxBandDdtState(.l)
    if (!is.null(.key)) {
      .key <- .rxBandDdt(.key)
    } else if (is.name(.l)) {
      .key <- as.character(.l)
    } else if (is.call(.l)) {
      .fn <- .l[[1]]
      if (is.call(.fn) || identical(.fn, quote(`/`))) {
        .l2 <- .l[[2]]
        if (is.call(.l2) && identical(.l2[[1]], quote(df))) {
          return(TRUE)
        }
        return(FALSE)
      }
      .fn <- as.character(.fn)
      if (.fn %in% .rxBandSkipLhs) {
        return(TRUE)
      }
      if (
        length(.l) == 2L &&
          (identical(.l[[2]], 0) || identical(.l[[2]], 0L))
      ) {
        return(TRUE) # x(0) = initial condition
      }
      if (.fn == "mtime" && length(.l) == 2L && is.name(.l[[2]])) {
        .key <- as.character(.l[[2]])
      } else {
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
    .env$lhs <- c(.env$lhs, .key)
    .env$rhs <- c(.env$rhs, list(unique(c(ctl, .rxBandVars(e[[3]])))))
    return(TRUE)
  }
  TRUE
}

#' Structural bandwidth of a model's ODE Jacobian
#'
#' Works out from the model equations which states each `d/dt()` depends on
#' (directly or through intermediate variables and `if()` conditions) and
#' returns the lower and upper bandwidth of the Jacobian in the model's state
#' order.  This is what `rxControl(band = "auto")` passes to the stiff solver.
#'
#' @param model rxode2 model (or anything [rxModelVars()] accepts)
#' @return named integer vector `c(ml = , mu = , n = )` with the lower and
#'   upper bandwidth and the number of states, or `NULL` when the structure
#'   cannot be determined (for example a `linCmt()` model)
#' @family Solver options and performance
#' @author Matthew L. Fidler
#' @export
#' @examples
#' \donttest{
#' m <- function() {
#'   model({
#'     d/dt(a) <- -k * a
#'     d/dt(b) <- k * a - k * b
#'     d/dt(c) <- k * b
#'   })
#' }
#' rxBandwidth(m)
#' }
rxBandwidth <- function(model) {
  .mv <- rxModelVars(model)
  .rxBandwidthMv(.mv)
}

.rxBandCache <- new.env(parent = emptyenv())

.rxBandwidthMv <- function(mv) {
  .md5 <- mv$md5["parsed_md5"]
  if (!is.na(.md5) && exists(.md5, envir = .rxBandCache, inherits = FALSE)) {
    return(get(.md5, envir = .rxBandCache, inherits = FALSE))
  }
  .ret <- tryCatch(.rxBandwidthMv0(mv), error = function(e) NULL)
  if (!is.na(.md5)) {
    assign(.md5, .ret, envir = .rxBandCache)
  }
  .ret
}

.rxBandwidthMv0 <- function(mv) {
  .states <- mv$state
  .n <- length(.states)
  if (.n == 0L) {
    return(NULL)
  }
  .txt <- mv$model["normModel"]
  if (regexpr("linCmt", .txt) != -1) {
    return(NULL)
  }
  .e <- parse(text = paste0("{\n", .txt, "\n}"), keep.source = FALSE)[[1]]
  .env <- new.env(parent = emptyenv())
  .env$lhs <- character(0)
  .env$rhs <- list()
  if (!.rxBandCollect(.e, character(0), .env)) {
    return(NULL)
  }
  .keys <- unique(.env$lhs)
  ## direct reads of each key, as a union over its assignments
  .reads <- lapply(.keys, function(k) {
    unique(unlist(.env$rhs[.env$lhs == k], use.names = FALSE))
  })
  names(.reads) <- .keys
  .dep <- lapply(.reads, function(r) intersect(r, .states))
  .via <- lapply(.reads, function(r) intersect(setdiff(r, .states), .keys))
  repeat {
    .changed <- FALSE
    for (.k in .keys) {
      .v <- .via[[.k]]
      if (length(.v) == 0L) {
        next
      }
      .new <- unique(c(.dep[[.k]], unlist(.dep[.v], use.names = FALSE)))
      if (length(.new) != length(.dep[[.k]])) {
        .dep[[.k]] <- .new
        .changed <- TRUE
      }
    }
    if (!.changed) break
  }
  .ml <- 0L
  .mu <- 0L
  for (.i in seq_len(.n)) {
    .d <- .dep[[.rxBandDdt(.states[.i])]]
    if (length(.d) == 0L) {
      next
    }
    .j <- match(.d, .states)
    .ml <- max(.ml, .i - .j)
    .mu <- max(.mu, .j - .i)
  }
  c(ml = as.integer(.ml), mu = as.integer(.mu), n = as.integer(.n))
}

## Solver methods that use rxControl(band=)
.rxBandMethods <- c(1L)

## Resolve rxControl(band=) for the model being solved into integer c(ml, mu),
## or integer(0) for a dense Jacobian.
.rxBandResolve <- function(ctl, object) {
  .band <- ctl$band
  if (is.null(.band) || length(.band) == 0L) {
    return(integer(0))
  }
  if (is.numeric(.band)) {
    return(as.integer(.band))
  }
  if (isFALSE(.band) || !(ctl$method %in% .rxBandMethods)) {
    return(integer(0))
  }
  .mv <- rxModelVars(object)
  if (identical(unname(.mv$trans["jac"]), "fulluser")) {
    return(integer(0))
  }
  .bw <- .rxBandwidthMv(.mv)
  if (is.null(.bw)) {
    return(integer(0))
  }
  .n <- .bw[["n"]]
  .w <- .bw[["ml"]] + .bw[["mu"]] + 1L
  if (isTRUE(.band)) {
    if (.w >= .n) {
      return(integer(0))
    }
  } else if (.n < 20L || 4L * .w > .n) {
    return(integer(0))
  }
  unname(.bw[c("ml", "mu")])
}

## Validate rxControl(band=)
.rxBandControl <- function(band) {
  if (is.character(band)) {
    checkmate::assertChoice(band, "auto", .var.name = "band")
    return(band)
  }
  if (is.logical(band)) {
    checkmate::assertLogical(band, len = 1L, any.missing = FALSE, .var.name = "band")
    return(band)
  }
  if (length(band) == 0L) {
    return(integer(0))
  }
  checkmate::assertIntegerish(band, len = 2L, lower = 0L, any.missing = FALSE, .var.name = "band")
  as.integer(band)
}
