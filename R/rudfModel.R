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

#' Current C user function registration of a name
#'
#' @param name function name
#' @return list with the argument count, C code, symengine function and
#'   derivative table entry (each NULL when absent)
#' @noRd
#' @author Matthew L. Fidler
.udfRegGet <- function(name) {
  .eq <- .udfEnv$rxSEeqUsr
  .code <- .udfEnv$rxCcode
  list(
    eq = if (any(names(.eq) == name)) .eq[[name]] else NULL,
    code = if (any(names(.code) == name)) .code[[name]] else NULL,
    sfs = get0(name, envir = .udfEnv$symengineFs, inherits = FALSE),
    d = get0(name, envir = rxode2parseD(), inherits = FALSE)
  )
}

#' Set (or remove) the C user function registration of a name
#'
#' @param name function name
#' @param reg registration, as from `.udfRegGet()`
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.udfRegSet <- function(name, reg) {
  .eq <- .udfEnv$rxSEeqUsr
  .eq <- .eq[names(.eq) != name]
  if (!is.null(reg$eq)) {
    .eq <- c(.eq, setNames(reg$eq, name))
  }
  .udfEnv$rxSEeqUsr <- .eq
  .code <- .udfEnv$rxCcode
  .code <- .code[names(.code) != name]
  if (!is.null(reg$code)) {
    .code <- c(.code, setNames(reg$code, name))
  }
  .udfEnv$rxCcode <- .code
  if (is.null(reg$sfs)) {
    if (exists(name, envir = .udfEnv$symengineFs, inherits = FALSE)) {
      rm(list = name, envir = .udfEnv$symengineFs)
    }
  } else {
    assign(name, reg$sfs, envir = .udfEnv$symengineFs)
  }
  .rxD <- rxode2parseD()
  if (is.null(reg$d)) {
    if (exists(name, envir = .rxD, inherits = FALSE)) {
      rm(list = name, envir = .rxD)
    }
  } else {
    assign(name, reg$d, envir = .rxD)
  }
  .rxSEstate$dTemplates <- NULL
  invisible()
}

#' Register C user functions, returning what they replaced
#'
#' @param regs named list of registrations
#' @return named list of the registrations replaced, for `.udfRegRestore()`
#' @noRd
#' @author Matthew L. Fidler
.udfRegActivate <- function(regs) {
  .saved <- list()
  for (.n in names(regs)) {
    .cur <- .udfRegGet(.n)
    .reg <- regs[[.n]]
    if (identical(.cur$code, .reg$code) && identical(.cur$eq, .reg$eq)) {
      next
    }
    .saved[[.n]] <- .cur
    .udfRegSet(.n, .reg)
  }
  .saved
}

#' Restore the registrations `.udfRegActivate()` replaced
#'
#' @param saved named list returned by `.udfRegActivate()`
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.udfRegRestore <- function(saved) {
  for (.n in rev(names(saved))) {
    .udfRegSet(.n, saved[[.n]])
  }
  invisible()
}

#' Translate one model user function to C, with its derivatives
#'
#' A function that reads a variable it does not define, or that the
#' translator cannot handle, stays an R user function.
#'
#' @param name function name
#' @param fun function
#' @return list with the function, whether it was translated and the
#'   registrations that make it (and its derivatives) available
#' @noRd
#' @author Matthew L. Fidler
.udfModelConvert <- function(name, fun) {
  .ret <- list(fun = fun, ok = FALSE, regs = list())
  if (length(formals(fun)) == 0L || length(.udfModelFreeVars(fun)) > 0L) {
    return(.ret)
  }
  .lst <- try(suppressMessages(rxFun2c(fun, name = name)), silent = TRUE)
  if (inherits(.lst, "try-error")) {
    return(.ret)
  }
  .d <- list()
  for (.cur in .lst) {
    .ret$regs[[.cur$name]] <- list(
      eq = length(.cur$args),
      code = .cur$cCode,
      sfs = symengine::Function(.cur$name),
      d = NULL
    )
    if (length(.cur) == 4L) {
      .d <- c(.d, list(.cur[[4]]))
    }
  }
  if (length(.d) > 0L) {
    .ret$regs[[name]]$d <- .d
  }
  .ret$ok <- TRUE
  .ret
}

#' Translation of a model user function already tried this session
#'
#' @param name function name
#' @param fun function
#' @return list from `.udfModelConvert()`, or NULL
#' @noRd
#' @author Matthew L. Fidler
.udfModelCached <- function(name, fun) {
  for (.v in .udfEnv$modelC[[name]]) {
    if (identical(.v$fun, fun, ignore.environment = TRUE, ignore.bytecode = TRUE, ignore.srcref = TRUE)) {
      return(.v)
    }
  }
  NULL
}

#' Model user functions translated to C, ready to register
#'
#' Each distinct definition is translated once per session.  A function
#' calling another model user function is translated after it, with it
#' registered.
#'
#' @param env environment holding the model's user functions
#' @return named list of registrations for the functions that translated
#' @noRd
#' @author Matthew L. Fidler
.udfModelRegs <- function(env) {
  .nms <- ls(env)
  .funs <- list()
  for (.n in .nms) {
    .f <- get0(.n, envir = env, mode = "function", inherits = FALSE)
    if (is.function(.f) && !is.primitive(.f)) {
      .funs[[.n]] <- .f
    }
  }
  .regs <- list()
  .todo <- character(0)
  for (.n in names(.funs)) {
    .v <- .udfModelCached(.n, .funs[[.n]])
    if (is.null(.v)) {
      .todo <- c(.todo, .n)
    } else if (.v$ok) {
      .regs <- c(.regs, .v$regs)
    }
  }
  if (length(.todo) == 0L) {
    return(.regs)
  }
  .saved <- .udfRegActivate(.regs)
  on.exit(.udfRegRestore(.saved))
  repeat {
    .left <- character(0)
    for (.n in .todo) {
      .v <- .udfModelConvert(.n, .funs[[.n]])
      if (.v$ok) {
        .udfEnv$modelC[[.n]] <- c(.udfEnv$modelC[[.n]], list(.v))
        .regs <- c(.regs, .v$regs)
        .saved <- c(.saved, .udfRegActivate(.v$regs))
        message("converted model user function '", .n, "' to C")
      } else {
        .left <- c(.left, .n)
      }
    }
    if (length(.left) == 0L || length(.left) == length(.todo)) {
      break
    }
    .todo <- .left
  }
  for (.n in .left) {
    .udfEnv$modelC[[.n]] <- c(.udfEnv$modelC[[.n]], list(list(fun = .funs[[.n]], ok = FALSE, regs = list())))
  }
  .regs
}

#' Use a model's own user functions for the duration of the calling function
#'
#' The functions are looked up before any other environment, and those
#' translated to C are registered only until the scope ends, so they never
#' become global.
#'
#' @param env environment of the model's user functions (a ui's `meta`, or the
#'   result of `.udfModelFuns()`); ignored unless it holds a function
#' @param frame frame whose exit ends the scope
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.udfModelLocal <- function(env, frame = parent.frame()) {
  if (.udfModelPush(env, frame)) {
    do.call(base::on.exit, list(quote(.udfModelUnlocal()), add = TRUE), envir = frame)
  }
  invisible()
}

#' Start a model user function scope; the caller ends it
#'
#' @param env environment of the model's user functions
#' @param frame frame whose exit ends the scope
#' @return `TRUE` when a scope was started (not when `env` already has one
#'   for `frame`)
#' @noRd
#' @author Matthew L. Fidler
.udfModelPush <- function(env, frame) {
  if (
    !is.environment(env) ||
      length(env) == 0L ||
      !any(unlist(eapply(env, is.function), use.names = FALSE))
  ) {
    return(FALSE)
  }
  .udfModelPrune()
  for (.s in .udfEnv$modelStack) {
    if (identical(.s$env, env) && identical(.s$frame, frame)) {
      return(FALSE)
    }
  }
  .saved <- .udfRegActivate(.udfModelRegs(env))
  .udfEnv$modelStack <- c(
    .udfEnv$modelStack,
    list(list(env = env, frame = frame, saved = .saved))
  )
  TRUE
}

#' Names the model user function scopes currently register
#'
#' @return character vector
#' @noRd
#' @author Matthew L. Fidler
.udfModelActiveNames <- function() {
  unique(unlist(lapply(.udfEnv$modelStack, function(.s) names(.s$saved)), use.names = FALSE))
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

#' End the innermost `.udfModelLocal()` scope
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.udfModelUnlocal <- function() {
  .n <- length(.udfEnv$modelStack)
  if (.n > 0L) {
    .s <- .udfEnv$modelStack[[.n]]
    .udfEnv$modelStack <- .udfEnv$modelStack[-.n]
    .udfRegRestore(.s$saved)
  }
  invisible()
}

#' Discard model user function scopes whose frame is gone
#'
#' This happens when their restore was dropped by a later `on.exit()` without
#' `add = TRUE`.  Scopes above the first dead one are ended and started again,
#' so registrations are always restored in reverse order.
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
  for (.i in rev(seq(.first, length(.stack)))) {
    .udfModelUnlocal()
  }
  for (.i in seq(.first, length(.stack))) {
    if (.alive[.i]) {
      .udfModelPush(.stack[[.i]]$env, .stack[[.i]]$frame)
    }
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
  .udfModelPrune()
  .stack <- .udfEnv$modelStack
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

#' Model user function environment that defines a function
#'
#' The scope in effect comes first, then the environments of the current
#' solve.
#'
#' @param fun function name
#' @return environment or NULL
#' @noRd
#' @author Matthew L. Fidler
.udfModelEnvFor <- function(fun) {
  .has <- function(.e) {
    is.environment(.e) && exists(fun, envir = .e, mode = "function", inherits = FALSE)
  }
  .e <- .udfModelEnvGet()
  if (.has(.e)) {
    return(.e)
  }
  for (.e in .udfEnv$modelSolve) {
    if (.has(.e)) {
      return(.e)
    }
  }
  NULL
}

#' Frame a model user function scope started by `$` ends with
#'
#' The caller's frame, unless that is the global environment (which never
#' exits); then the `$` call itself.
#'
#' @param frame caller's frame
#' @return frame
#' @noRd
#' @author Matthew L. Fidler
.udfModelFrame <- function(frame) {
  if (identical(frame, globalenv())) {
    parent.frame()
  } else {
    frame
  }
}
