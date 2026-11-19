#' Model user function environment of the current solve
#'
#' Set by `.udfEnvSetUdf()` from the compiled model, for solves that have no
#' ui to take it from.
#'
#' @return environment or NULL
#' @noRd
#' @author Matthew L. Fidler
.udfModelSolveEnv <- function() {
  if (length(.udfEnv$modelSolve) == 0L) {
    return(NULL)
  }
  .udfEnv$modelSolve[[1]]
}

#' Record that the current parse used a model user function environment
#'
#' @param env environment
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.udfModelUsed <- function(env) {
  for (.e in .udfEnv$modelUsed) {
    if (identical(.e, env)) {
      return(invisible())
    }
  }
  .udfEnv$modelUsed <- c(.udfEnv$modelUsed, list(env))
  invisible()
}

#' Keep a model user function environment for the compiled model
#'
#' Kept, like `.udfEnv$envList`, for as long as the session lasts: a compiled
#' model has no other way back to its functions, and a same-named function
#' elsewhere must never stand in for them.  Only models with functions left
#' in R are kept.
#'
#' @param env environment
#' @return key recorded in the model variables
#' @noRd
#' @author Matthew L. Fidler
.udfModelKeep <- function(env) {
  .key <- paste0("model:", data.table::address(env))
  .udfEnv$modelList[[.key]] <- env
  .key
}

#' Use the model user function environments a compiled model recorded
#'
#' @param keys keys from `.udfModelKeep()`
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.udfModelSolve <- function(keys) {
  .udfEnv$modelSolve <- Filter(is.environment, lapply(keys, function(.k) .udfEnv$modelList[[.k]]))
  invisible()
}

#' The `meta` environment of a ui, without going through `$`
#'
#' @param ui rxUi (environment or compressed list)
#' @return environment or NULL
#' @noRd
#' @author Matthew L. Fidler
.udfModelMeta <- function(ui) {
  if (is.environment(ui)) {
    .meta <- get0("meta", envir = ui, inherits = FALSE)
  } else if (is.list(ui)) {
    .meta <- .subset2(ui, "meta")
  } else {
    .meta <- NULL
  }
  if (is.environment(.meta)) {
    .meta
  } else {
    NULL
  }
}

#' Model user function environment that defines a function
#'
#' Strong scopes are searched innermost first, then the environments of the
#' current solve; weak scopes are searched on their own.  Scopes whose frame is
#' gone are discarded first.
#'
#' @param fun function name
#' @param weak search the weak scopes instead
#' @return environment or NULL
#' @noRd
#' @author Matthew L. Fidler
.udfModelEnvFor <- function(fun, weak = FALSE) {
  .has <- function(.e) {
    is.environment(.e) && exists(fun, envir = .e, mode = "function", inherits = FALSE)
  }
  .udfModelPrune()
  for (.s in rev(.udfEnv$modelStack)) {
    if (.s$weak == weak && .has(.s$env)) {
      return(.s$env)
    }
  }
  if (!weak) {
    for (.e in .udfEnv$modelSolve) {
      if (.has(.e)) {
        return(.e)
      }
    }
  }
  NULL
}
