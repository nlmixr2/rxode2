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
