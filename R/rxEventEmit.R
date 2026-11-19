## Event delivery: normalize a call payload and notify the listeners.

#' @rdname rxEventListen
#' @export
.rxEventCall <- function(call, fun = NULL) {
  if (!is.call(call)) {
    return(call)
  }
  if (!is.null(fun)) {
    call[[1]] <- as.name(fun)
  } else if (
    !is.name(call[[1]]) &&
      !(is.call(call[[1]]) && as.character(call[[1]][[1]]) %in% c("::", ":::"))
  ) {
    call[[1]] <- as.name("<fun>")
  }
  if (length(call) > 1L) {
    .idx <- seq.int(2L, length(call))
    .isVal <- vapply(.idx, function(i) !is.language(call[[i]]), logical(1))
    if (sum(.isVal) > 5L) {
      ## many values mean the call was built by do.call() (e.g. a spread
      ## control list): drop them all, leaving one marker
      .keep <- c(1L, .idx[!.isVal])
      call <- as.call(c(as.list(call)[.keep], list(as.name("<...>"))))
    } else {
      ## a few typed constants (nSub = 10) are kept; larger values are not
      for (.j in which(.isVal)) {
        .i <- .idx[.j]
        .a <- call[[.i]]
        if (!is.null(.a) && !(is.atomic(.a) && length(.a) <= 1L)) {
          call[[.i]] <- as.name("<value>")
        }
      }
    }
  }
  call
}

#' @rdname rxEventListen
#' @export
rxEventEmit <- function(event, ..., fun = NULL) {
  if (.rxEventEnv$depth != 0L || length(.rxEventEnv$listeners) == 0L) {
    return(invisible(NULL))
  }
  checkmate::assertString(event, min.chars = 1)
  .payload <- list(...)
  if (!is.null(.payload$call)) {
    .payload$call <- .rxEventCall(.payload$call, fun)
  }
  .rxEventEnv$seq <- .rxEventEnv$seq + 1L
  ## listeners run inside a scope, so anything they trigger is dropped
  .rxEventEnter()
  on.exit(.rxEventExit(), add = TRUE)
  for (.id in names(.rxEventEnv$listeners)) {
    .f <- .rxEventEnv$listeners[[.id]]
    tryCatch(
      do.call(.f, c(list(event), .payload), quote = TRUE),
      error = function(e) {
        warning(
          sprintf("rxode2 event listener '%s' failed on '%s': %s", .id, event, conditionMessage(e)),
          call. = FALSE
        )
      }
    )
  }
  invisible(NULL)
}

#' Leave the rxSolve() scope and emit solveComplete for a solved result
#' @noRd
.rxEventExitSolve <- function(result, object, call) {
  if (inherits(result, "rxSolve")) {
    .rxEventExit("solveComplete", result = result, object = object, call = call, kind = "rxSolve", fun = "rxSolve")
  } else {
    .rxEventExit()
  }
}
