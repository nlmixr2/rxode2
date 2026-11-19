#' Use a model's own user functions for the duration of the calling function
#'
#' In a strong scope the functions are looked up before any other environment,
#' and those translated to C are registered only until the scope ends, so they
#' never become global.  A weak scope registers nothing and is searched only
#' after every other environment, so it can never shadow another function.
#'
#' @param env environment of the model's user functions (a ui's `meta`, or the
#'   result of `.udfModelFuns()`); ignored unless it holds a function
#' @param frame frame whose exit ends the scope
#' @param weak start a weak scope
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.udfModelLocal <- function(env, frame = parent.frame(), weak = FALSE) {
  if (.udfModelPush(env, frame, weak)) {
    # the function itself, since a caller's frame may not see rxode2's names
    do.call(base::on.exit, list(as.call(list(.udfModelUnlocal, frame, weak)), add = TRUE), envir = frame)
  }
  invisible()
}

#' Start a model user function scope; the caller ends it
#'
#' @param env environment of the model's user functions
#' @param frame frame whose exit ends the scope
#' @param weak start a weak scope (see `.udfModelLocal()`)
#' @return `TRUE` when a scope was started (not when `env` already has one
#'   of this kind for `frame`)
#' @noRd
#' @author Matthew L. Fidler
.udfModelPush <- function(env, frame, weak = FALSE) {
  if (
    !is.environment(env) ||
      length(env) == 0L ||
      !any(unlist(eapply(env, is.function), use.names = FALSE))
  ) {
    return(FALSE)
  }
  .udfModelPrune()
  for (.s in .udfEnv$modelStack) {
    if (identical(.s$env, env) && identical(.s$frame, frame) && .s$weak == weak) {
      return(FALSE)
    }
  }
  .regs <- if (weak) list() else .udfModelRegs(env)
  .saved <- .udfRegActivate(.regs)
  .udfEnv$modelStack <- c(
    .udfEnv$modelStack,
    list(list(env = env, frame = frame, weak = weak, saved = .saved, names = names(.regs)))
  )
  TRUE
}

#' Names the model user function scopes currently register
#'
#' @return character vector
#' @noRd
#' @author Matthew L. Fidler
.udfModelActiveNames <- function() {
  # every function a scope registers, including one whose identical C code
  # was already registered (so it did not need replacing)
  unique(unlist(lapply(.udfEnv$modelStack, function(.s) .s$names), use.names = FALSE))
}

#' C code of the model user functions currently registered
#'
#' Kept by a compiled model so it can be compiled again outside the scope.
#'
#' @return named character vector
#' @noRd
#' @author Matthew L. Fidler
.udfModelActiveC <- function() {
  .n <- intersect(.udfModelActiveNames(), names(.udfEnv$rxCcode))
  .udfEnv$rxCcode[.n]
}

#' End a `.udfModelLocal()` scope
#'
#' Strong scopes end in reverse order, but a weak scope (kept for a caller)
#' can outlive strong ones started after it, so the scope is found by its
#' frame.
#'
#' @param frame frame of the scope
#' @param weak whether it is a weak scope
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.udfModelUnlocal <- function(frame, weak = FALSE) {
  .stack <- .udfEnv$modelStack
  for (.i in rev(seq_along(.stack))) {
    .s <- .stack[[.i]]
    if (.s$weak == weak && identical(.s$frame, frame)) {
      .udfEnv$modelStack <- .stack[-.i]
      .udfRegRestore(.s$saved)
      break
    }
  }
  invisible()
}

#' Discard model user function scopes whose frame is gone
#'
#' This happens when their restore was dropped by a later `on.exit()` without
#' `add = TRUE`.  Strong scopes above the first dead one are ended and started
#' again, so registrations are always restored in reverse order.
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.udfModelPrune <- function() {
  .stack <- .udfEnv$modelStack
  if (length(.stack) == 0L) {
    return(invisible())
  }
  .frames <- sys.frames()
  .alive <- vapply(
    .stack,
    function(.s) {
      any(vapply(.frames, identical, logical(1), .s$frame))
    },
    logical(1)
  )
  if (all(.alive)) {
    return(invisible())
  }
  .first <- which(!.alive)[1]
  .udfEnv$modelStack <- .stack[seq_len(.first - 1L)]
  for (.i in rev(seq(.first, length(.stack)))) {
    .udfRegRestore(.stack[[.i]]$saved)
  }
  for (.i in seq(.first, length(.stack))) {
    if (.alive[.i]) {
      .udfModelPush(.stack[[.i]]$env, .stack[[.i]]$frame, .stack[[.i]]$weak)
    }
  }
  invisible()
}
