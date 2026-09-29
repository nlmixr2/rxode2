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
#' A function counts when a function the model block calls (or, transitively,
#' one of these functions calls) is bound to a function in the model function's own
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
  .todo <- list(list(names = .udfModelCallNames(expr), envs = .envs))
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
            .todo <- c(
              .todo,
              list(list(
                names = .udfModelCallNames(body(.f)),
                envs = .udfModelClosure(environment(.f))
              ))
            )
          }
          break
        }
      }
    }
  }
  .ret
}

#' Names of the functions an expression calls
#'
#' @param expr expression
#' @return character vector
#' @noRd
#' @author Matthew L. Fidler
.udfModelCallNames <- function(expr) {
  if (!is.call(expr)) {
    return(character(0))
  }
  .ret <- if (is.name(expr[[1]])) as.character(expr[[1]]) else .udfModelCallNames(expr[[1]])
  for (.i in seq_along(expr)[-1]) {
    if (is.call(expr[[.i]])) {
      .ret <- c(.ret, .udfModelCallNames(expr[[.i]]))
    }
  }
  .ret
}

#' Names a function body reads before assigning them itself
#'
#' The body is walked in evaluation order (the value of an assignment before
#' its target), so `a <- a + x` reads `a` from outside the function.
#'
#' @param fun function
#' @return character vector of free variable names
#' @noRd
#' @author Matthew L. Fidler
.udfModelFreeVars <- function(fun) {
  .env <- new.env(parent = emptyenv())
  .env$free <- character(0)
  .env$assigned <- names(formals(fun))
  .walk <- function(x) {
    if (is.name(x)) {
      .n <- as.character(x)
      if (nzchar(.n) && !(.n %in% .env$assigned)) {
        .env$free <- c(.env$free, .n)
      }
    } else if (is.call(x)) {
      if (!is.name(x[[1]])) {
        .walk(x[[1]])
      }
      .isAssign <- (identical(x[[1]], quote(`<-`)) || identical(x[[1]], quote(`=`))) &&
        length(x) == 3L &&
        is.name(x[[2]])
      if (.isAssign) {
        .walk(x[[3]])
        .env$assigned <- c(.env$assigned, as.character(x[[2]]))
      } else {
        for (.i in seq_along(x)[-1]) {
          if (is.call(x[[.i]]) || is.name(x[[.i]])) {
            .walk(x[[.i]])
          }
        }
      }
    }
  }
  .walk(body(fun))
  unique(.env$free)
}
