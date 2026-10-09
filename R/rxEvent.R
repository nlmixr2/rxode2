## A small event bus so other packages (e.g. nlmixr2log) can be told when a
## top-level fit, simulation or result is finished.
##
## Only the outermost user-facing operation emits: every emitting function
## enters one shared "operation scope", and an event is delivered only when
## no scope is active (depth 0).  Nested work -- the rxSolve() calls inside a
## fit, the fits inside a bootstrap -- is therefore silent.  The depth is
## mirrored into the RXODE2_EVENT_DEPTH environment variable so worker
## processes spawned inside a scope start silent too.

.rxEventEnv <- new.env(parent = emptyenv())
.rxEventEnv$listeners <- list()
.rxEventEnv$depth <- 0L
.rxEventEnv$seq <- 0L

#' Set the operation depth (and mirror it for child processes)
#' @noRd
.rxEventSetDepth <- function(depth) {
  depth <- max(0L, as.integer(depth))
  .rxEventEnv$depth <- depth
  if (depth == 0L) {
    Sys.unsetenv("RXODE2_EVENT_DEPTH")
  } else {
    Sys.setenv(RXODE2_EVENT_DEPTH = as.character(depth))
  }
  invisible(depth)
}

#' Initialize the depth from the environment (called from .onLoad)
#' @noRd
.rxEventInitDepth <- function() {
  .d <- suppressWarnings(as.integer(Sys.getenv("RXODE2_EVENT_DEPTH", "0")))
  .rxEventEnv$depth <- if (is.na(.d) || .d < 0L) 0L else .d
  invisible()
}

#' rxode2 event bus
#'
#' A minimal publish/subscribe mechanism.  Packages that finish a top-level
#' fit, simulation or result call `rxEventEmit()`; listeners registered with
#' `rxEventListen()` are told about it.  Events are only delivered from the
#' outermost operation: work done inside `rxEventScope()` (or between
#' `.rxEventEnter()` and `.rxEventExit()`) emits nothing.
#'
#' Listeners are called as `fun(event, ...)` with the event name and the
#' payload fields as named arguments.  A listener that errors gives a warning
#' and never stops the other listeners or the caller.  Events emitted while a
#' listener runs are dropped.
#'
#' @param id A listener id; registering the same id again replaces it.
#' @param fun For `rxEventListen()`, the listener, a function
#'   `function(event, ...)`.  For `rxEventEmit()`, `.rxEventExit()` and
#'   `.rxEventCall()`, the name of the emitting function, used to normalize a
#'   `call` payload field (see Details).
#' @param event The event name, e.g. `"fitComplete"` or `"solveComplete"`.
#' @param ... Payload fields (named).
#' @param expr Code to evaluate inside an operation scope.
#' @param call A call to normalize.
#'
#' @details A `call` payload field is passed through `.rxEventCall()`: its head
#'   becomes `fun` (when given; otherwise a function object in the head becomes
#'   `` `<fun>` ``).  Arguments that are not language (values inlined by
#'   `do.call()`) become `` `<value>` ``, except single constants; when there
#'   are more than five such values they are all dropped and replaced by one
#'   `` `<...>` `` marker.  A recorded call therefore never embeds a large
#'   object.
#'
#' @return `rxEventListen()`, `rxEventUnlisten()` and `rxEventEmit()` return
#'   `NULL` invisibly; `rxEventListeners()` returns the listener ids;
#'   `rxEventDepth()` the current depth; `rxEventSeq()` the number of events
#'   delivered in this process; `rxEventScope()` the value of `expr`.
#' @export
#' @author Matthew L. Fidler
#' @examples
#' got <- NULL
#' rxEventListen("example", function(event, ...) got <<- event)
#' rxEventEmit("myEvent", value = 1)
#' got
#' rxEventScope(rxEventEmit("ignored"))
#' rxEventUnlisten("example")
rxEventListen <- function(id, fun) {
  checkmate::assertString(id, min.chars = 1)
  checkmate::assertFunction(fun)
  .l <- .rxEventEnv$listeners
  .l[[id]] <- fun
  .rxEventEnv$listeners <- .l
  invisible(NULL)
}

#' @rdname rxEventListen
#' @export
rxEventUnlisten <- function(id) {
  checkmate::assertString(id, min.chars = 1)
  .l <- .rxEventEnv$listeners
  .l[[id]] <- NULL
  .rxEventEnv$listeners <- .l
  invisible(NULL)
}

#' @rdname rxEventListen
#' @export
rxEventListeners <- function() {
  .n <- names(.rxEventEnv$listeners)
  if (is.null(.n)) character(0) else .n
}

#' @rdname rxEventListen
#' @export
rxEventDepth <- function() {
  .rxEventEnv$depth
}

#' @rdname rxEventListen
#' @export
rxEventSeq <- function() {
  .rxEventEnv$seq
}

#' @rdname rxEventListen
#' @export
rxEventScope <- function(expr) {
  .rxEventEnter()
  on.exit(.rxEventExit(), add = TRUE)
  force(expr)
}

#' @rdname rxEventListen
#' @export
.rxEventEnter <- function() {
  .rxEventSetDepth(.rxEventEnv$depth + 1L)
}

#' @rdname rxEventListen
#' @export
.rxEventExit <- function(event = NULL, ..., fun = NULL) {
  .rxEventSetDepth(.rxEventEnv$depth - 1L)
  if (!is.null(event)) {
    rxEventEmit(event, ..., fun = fun)
  }
  invisible(NULL)
}

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
