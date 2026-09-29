#' Environments lexically enclosing a model function
#'
#' Walks the parents of `env` up to the first named environment (the global
#' environment, a namespace or a package), so only the function's own
#' closures and `local()` blocks are included.
#'
#' @param env environment to start from
#' @return list of environments, innermost first
#' @noRd
#' @author Matthew L. Fidler
.udfModelClosure <- function(env) {
  .ret <- list()
  while (
    is.environment(env) &&
      !identical(env, emptyenv()) &&
      environmentName(env) == ""
  ) {
    .ret <- c(.ret, list(env))
    env <- parent.env(env)
  }
  .ret
}

#' User functions a model function defines for its model
#'
#' A function counts when a name used in the model block (or, transitively, in
#' one of these functions) is bound to a function in the model function's own
#' frame or in an environment lexically enclosing it (#1416).
#'
#' @param expr model block expression
#' @param envir frame of the model function
#' @param closure environment enclosing the model function; defaults to
#'   `parent.env(envir)`
#' @return environment holding the functions, by name
#' @noRd
#' @author Matthew L. Fidler
.udfModelFuns <- function(expr, envir, closure = parent.env(envir)) {
  .ret <- new.env(parent = emptyenv())
  .envs <- .udfModelClosure(closure)
  if (is.environment(envir) && environmentName(envir) == "") {
    .envs <- c(list(envir), .envs)
  }
  if (length(.envs) == 0L) {
    return(.ret)
  }
  # functions registered as C user functions (maybe by an earlier build of
  # this model) are still candidates
  .skip <- setdiff(rxSupportedFuns(), names(.udfEnv$rxSEeqUsr))
  .todo <- list(list(names = all.names(expr), envs = .envs))
  while (length(.todo) > 0L) {
    .cur <- .todo[[1]]
    .todo <- .todo[-1]
    .nms <- setdiff(unique(.cur$names), c(.skip, ls(.ret, all.names = TRUE)))
    for (.n in .nms) {
      for (.e in .cur$envs) {
        .f <- get0(.n, envir = .e, mode = "function", inherits = FALSE)
        if (is.function(.f)) {
          if (!is.primitive(.f)) {
            assign(.n, .f, envir = .ret)
            .todo <- c(.todo, list(list(
              names = all.names(body(.f)),
              envs = .udfModelClosure(environment(.f))
            )))
          }
          break
        }
      }
    }
  }
  .ret
}

#' Names a function body reads but neither receives nor assigns
#'
#' @param fun function
#' @return character vector of free variable names
#' @noRd
#' @author Matthew L. Fidler
.udfModelFreeVars <- function(fun) {
  .env <- new.env(parent = emptyenv())
  .env$used <- character(0)
  .env$assigned <- character(0)
  .walk <- function(x) {
    if (is.name(x)) {
      .env$used <- c(.env$used, as.character(x))
    } else if (is.call(x)) {
      .isAssign <- identical(x[[1]], quote(`<-`)) || identical(x[[1]], quote(`=`))
      if (!is.name(x[[1]])) {
        .walk(x[[1]])
      }
      for (.i in seq_along(x)[-1]) {
        if (.isAssign && .i == 2L && is.name(x[[2]])) {
          .env$assigned <- c(.env$assigned, as.character(x[[2]]))
        } else if (is.call(x[[.i]]) || (is.name(x[[.i]]) && nzchar(as.character(x[[.i]])))) {
          .walk(x[[.i]])
        }
      }
    }
  }
  .walk(body(fun))
  setdiff(unique(.env$used), c(names(formals(fun)), .env$assigned))
}

#' Convert one model user function to C, with its derivatives
#'
#' Registers it the way `rxFun()` does.  A function that reads a variable it
#' does not define, or that the translator cannot handle, stays an R user
#' function.
#'
#' @param name function name
#' @param fun function
#' @return list recording the function and whether it was converted
#' @noRd
#' @author Matthew L. Fidler
.udfModelFunToC <- function(name, fun) {
  .ret <- list(fun = fun, ok = FALSE, cCode = NULL)
  if (length(formals(fun)) == 0L || length(.udfModelFreeVars(fun)) > 0L) {
    return(.ret)
  }
  .lst <- try(suppressMessages(rxFun2c(fun, name = name)), silent = TRUE)
  if (inherits(.lst, "try-error")) {
    return(.ret)
  }
  .d <- list()
  for (.cur in .lst) {
    suppressWarnings(rxRmFunParse(.cur$name))
    rxFunParse(.cur$name, .cur$args, .cur$cCode)
    if (length(.cur) == 4L) {
      .d <- c(.d, list(.cur[[4]]))
    }
  }
  if (length(.d) > 0L) {
    suppressWarnings(rxD(name, .d))
  }
  message("converted model user function '", name, "' to C")
  .ret$ok <- TRUE
  .ret$cCode <- .lst[[1]]$cCode
  .ret
}

#' Convert a model's own user functions to C where possible
#'
#' Functions are only translated again when their definition changes or their
#' registration was removed.  A function calling another model user function
#' is tried after the one it calls.
#'
#' @param env environment of model user functions (see `.udfModelFuns()`)
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.udfModelToC <- function(env) {
  if (!is.environment(env)) {
    return(invisible())
  }
  .todo <- character(0)
  for (.n in ls(env)) {
    .f <- get0(.n, envir = env, mode = "function", inherits = FALSE)
    if (!is.function(.f) || is.primitive(.f)) {
      next
    }
    .c <- .udfEnv$modelC[[.n]]
    if (
      !is.null(.c) &&
        identical(.c$fun, .f, ignore.environment = TRUE, ignore.bytecode = TRUE, ignore.srcref = TRUE) &&
        (!.c$ok || identical(unname(.udfEnv$rxCcode[.n]), .c$cCode))
    ) {
      next
    }
    .todo <- c(.todo, .n)
  }
  repeat {
    .left <- character(0)
    for (.n in .todo) {
      .c <- .udfModelFunToC(.n, get(.n, envir = env))
      .udfEnv$modelC[[.n]] <- .c
      if (!.c$ok) {
        .left <- c(.left, .n)
      }
    }
    if (length(.left) == length(.todo)) {
      break
    }
    .todo <- .left
  }
  invisible()
}

#' Use a model's own user functions for the duration of the calling function
#'
#' These are looked up before any other environment, so a model uses the
#' functions it was defined with.
#'
#' @param env environment of the model's user functions (a ui's `meta`, or the
#'   result of `.udfModelFuns()`); ignored unless it holds something
#' @param frame frame whose exit ends the scope
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.udfModelLocal <- function(env, frame = parent.frame()) {
  if (!is.environment(env) || length(env) == 0L) {
    return(invisible())
  }
  .udfEnv$modelStack <- c(.udfEnv$modelStack, list(list(env = env, frame = frame)))
  do.call(base::on.exit, list(quote(.udfModelUnlocal()), add = TRUE), envir = frame)
  invisible()
}

#' End the innermost `.udfModelLocal()` scope
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.udfModelUnlocal <- function() {
  .n <- length(.udfEnv$modelStack)
  if (.n > 0L) {
    .udfEnv$modelStack <- .udfEnv$modelStack[-.n]
  }
  invisible()
}

#' Environment of the model user functions in scope
#'
#' Scopes whose frame is gone (their restore dropped by a later `on.exit()`
#' without `add = TRUE`) are discarded.
#'
#' @return environment or NULL
#' @noRd
#' @author Matthew L. Fidler
.udfModelEnvGet <- function() {
  .stack <- .udfEnv$modelStack
  if (length(.stack) == 0L) {
    return(.udfModelSolveEnv())
  }
  .frames <- sys.frames()
  .alive <- vapply(
    .stack,
    function(.s) {
      any(vapply(.frames, identical, logical(1), .s$frame))
    },
    logical(1)
  )
  if (!all(.alive)) {
    .stack <- .stack[.alive]
    .udfEnv$modelStack <- .stack
  }
  if (length(.stack) == 0L) {
    return(.udfModelSolveEnv())
  }
  .stack[[length(.stack)]]$env
}

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
#' The most recent `rxode2.udfSearchLimit` of them are kept.
#'
#' @param env environment
#' @return key recorded in the model variables
#' @noRd
#' @author Matthew L. Fidler
.udfModelKeep <- function(env) {
  .key <- paste0("model:", data.table::address(env))
  .lst <- .udfEnv$modelList
  .lst[[.key]] <- NULL
  .lst[[.key]] <- env
  .max <- .udfSearchListMax()
  if (length(.lst) > .max) {
    .lst <- .lst[seq(length(.lst) - .max + 1L, length(.lst))]
  }
  .udfEnv$modelList <- .lst
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

#' Model user functions of a ui, converted to C where possible
#'
#' Lets a ui made in another session convert its own functions again.
#'
#' @param ui rxUi
#' @return the ui's `meta` environment, or NULL
#' @noRd
#' @author Matthew L. Fidler
.udfModelUi <- function(ui) {
  .meta <- .udfModelMeta(ui)
  if (is.null(.meta) || length(.meta) == 0L) {
    return(.meta)
  }
  .lstExpr <- if (is.environment(ui)) get0("lstExpr", envir = ui, inherits = FALSE) else .subset2(ui, "lstExpr")
  if (length(.lstExpr) > 0L) {
    .udfModelToC(.udfModelFuns(as.call(c(list(quote(`{`)), .lstExpr)), .meta, emptyenv()))
  }
  .meta
}
