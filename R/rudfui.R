.udfUiEnv <- new.env(parent = emptyenv())

#' Look up a ui user-function S3 method, memoised for one model parse
#'
#' `.errProcessExpression()` asks this once per call node of every line, so
#' within a parse the answer is cached per function name in
#' `.udfUiEnv$methodCache` (NULL outside a parse, which disables the cache).
#'
#' @param generic "rxUdfUi" or "rxUdfUiLhs"
#' @param fun function name (character)
#' @return the method, or NULL when there is none
#' @noRd
#' @author Matthew L. Fidler
.rxUdfUiMethod <- function(generic, fun) {
  if (length(fun) != 1L) {
    .ret <- try(utils::getS3method(generic, fun), silent = TRUE)
    if (inherits(.ret, "try-error")) {
      return(NULL)
    }
    return(.ret)
  }
  .cache <- .udfUiEnv$methodCache
  if (is.null(.cache)) {
    return(utils::getS3method(generic, fun, optional = TRUE))
  }
  .key <- paste0(generic, ".", fun)
  .ret <- .cache[[.key]]
  if (is.null(.ret)) {
    .ret <- utils::getS3method(generic, fun, optional = TRUE)
    if (is.null(.ret)) {
      .ret <- FALSE
    }
    assign(.key, .ret, envir = .cache)
  }
  if (isFALSE(.ret)) {
    return(NULL)
  }
  .ret
}

#' Reset the rxode2 ui environment variables
#'
#' @return NULL silently
#' @export
#'
#' @keywords internal
#' @author Matthew L. Fidler
#' @examples
#' rxUdfUiReset()
rxUdfUiReset <- function() {
  .udfUiEnv$num <- 1L
  .udfUiEnv$iniDf <- NULL
  .udfUiEnv$lhs <- NULL
  .udfUiEnv$data <- NULL
  .udfUiEnv$est <- NULL
  .udfUiEnv$control <- NULL
  .udfUiEnv$parsing <- FALSE
  .udfUiEnv$mv <- NULL
  .udfUiEnv$probs <- NULL
  invisible(NULL)
}

rxUdfUiReset()


#' This gives the current number in the ui of the particular function being called.
#'
#' If this is called outside of function parsing or the input is
#' unexpected this returns 1L. This is useful when writing replacement
#' UI functions
#'
#' @return integer greater than 1L
#' @family User functions
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' rxUdfUiNum()
rxUdfUiNum <- function() {
  if (checkmate::testIntegerish(.udfUiEnv$num, lower = 1L, len = 1L, any.missing = FALSE)) {
    as.integer(.udfUiEnv$num)
  } else {
    1L
  }
}

#' Is the expression actually a flag that can be used in the rxUdfUi
#' functions?
#'
#' This is useful when writing replacement
#' UI functions
#'
#' @param expr expression to evaluate
#'
#' @param arg argument name for error messages for the argument name
#'   in the `rxUdfUi` extension when the expression is not logical
#'
#' @param funName function name for the error message for the argument
#'   name in the `rxUdfUi` extension when the expression is not
#'   logical.
#'
#' @param env the environment in which to evaluate the expression (in case it is numeric)
#'
#' @return logical value of the expression if it can be evaluated to a
#'   scalar `TRUE`/`FALSE` value, otherwise an error is thrown.
#'
#' @export
#'
#' @author Matthew L. Fidler
#'
rxUdfUiFlag <- function(expr, arg = "arg", funName = "fun", env = baseenv()) {
  checkmate::assertCharacter(arg, len = 1L, any.missing = FALSE)
  checkmate::assertCharacter(funName, len = 1L, any.missing = FALSE)
  .val <- suppressWarnings(try(eval(expr, envir = env), silent = TRUE))
  if (
    inherits(.val, "try-error") ||
      !(checkmate::testLogical(.val, len = 1L, any.missing = FALSE) ||
        checkmate::testIntegerish(.val, len = 1L, any.missing = FALSE, min = 0, max = 1))
  ) {
    stop(
      sprintf("'%s' requires '%s' to be a scalar TRUE/FALSE value in rxode2 model syntax", funName, arg),
      call. = FALSE
    )
  }
  as.logical(.val)
}

#' Get the rxode2 iniDf of the current UI being processed (or return NULL)
#'
#' @return Initial `data.frame` being processed or `NULL` for nothing.
#'
#' @export
#'
#' @author Matthew L. Fidler
#'
#' @examples
#'
#' rxUdfUiIniDf()
#'
rxUdfUiIniDf <- function() {
  if (testIniDf(.udfUiEnv$iniDf)) {
    .udfUiEnv$iniDf
  } else {
    NULL
  }
}
#' Return the lhs parsed language expression
#'
#' @return lhs language expression or NULL
#' @family User functions
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' rxUdfUiIniLhs()
#'
rxUdfUiIniLhs <- function() {
  if (is.language(.udfUiEnv$lhs)) {
    .udfUiEnv$lhs
  } else {
    NULL
  }
}
#' Is the expression actually equal to a value?
#'
#' This is used to determine if the location and scale parameters are
#' actually equal to their default values, which allows for more
#' efficient translation to expit expressions when possible.
#'
#' @param expr the R language expression to evaluate
#' @param value the value to compare against
#' @param env the environment in which to evaluate the expression
#' @return TRUE if the expression evaluates to a scalar value equal to
#'   the specified value, FALSE otherwise
#'
#' @export
#'
#' @author Matthew L. Fidler
rxUdfUiIsValue <- function(expr, value, env = baseenv()) {
  .val <- try(eval(expr, envir = env), silent = TRUE)
  if (
    inherits(.val, "try-error") ||
      length(.val) != 1L ||
      (!is.numeric(.val) && !is.logical(.val)) ||
      is.na(.val)
  ) {
    return(FALSE)
  }
  identical(as.numeric(.val), as.numeric(value))
}
#' Return the model variables that is being processed or setup model
#' variables for processing
#'
#'
#' @param value when specified, this assigns the model variables to be
#'   processed, or resets it by assigning it to be `NULL`.
#'
#' @return value of the `modelVariables` being processed or `NULL`.
#'
#' @family User functions
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' rxUdfUiMv()
#'
rxUdfUiMv <- function(value) {
  if (missing(value)) {
    .udfUiEnv$mv
  } else if (inherits(value, "rxModelVars")) {
    .udfUiEnv$mv <- value
  } else if (is.null(value)) {
    .udfUiEnv$mv <- value
  } else {
    stop("rxUdfUiMt must be called with model variables, NULL, or without any arguments", call. = FALSE)
  }
}
#' Return the data.frame that is being processed or setup data.frame for processing
#'
#'
#' @param value when specified, this assigns the data.frame to be processed, or resets it by assigning it to be `NULL`.
#'
#' @return value of the `data.frame` being processed or `NULL`.
#'
#' @export
#' @family User functions
#' @author Matthew L. Fidler
#' @examples
#'
#' rxUdfUiData()
#'
rxUdfUiData <- function(value) {
  if (missing(value)) {
    .udfUiEnv$data
  } else if (is.data.frame(value)) {
    .udfUiEnv$data <- value
  } else if (is.null(value)) {
    .udfUiEnv$data <- value
  } else {
    stop("rxUdfUiData must be called with a data.frame, NULL, or without any arguments", call. = FALSE)
  }
}


#' Give the expression as a compressed model or expression
#'
#' Here it means that it evaluates to a variable or a number.
#'
#' @param expr the R language expression to evaluate
#'
#' @param env the environment in which to evaluate the expression
#'
#' @return expression
#'
#' @export
#'
#' @author Matthew L. Fidler
#'
rxUdfUiExpr <- function(expr, env = parent.frame()) {
  .val <- suppressWarnings(try(eval(expr, envir = env), silent = TRUE))
  if (
    !inherits(.val, "try-error") &&
      length(.val) == 1L &&
      (is.numeric(.val) || is.character(.val))
  ) {
    return(str2lang(as.character(.val)))
  }
  expr
}

#' Return the control that is being processed or setup control for processing
#'
#' @param value when specified, this assigns the control to be
#'   processed, or resets it by assigning it to be `NULL`.
#'
#' @return value of the `data.frame` being processed or `NULL`.
#'
#' @export
#' @family User functions
#' @author Matthew L. Fidler
#' @examples
#'
#' rxUdfUiControl()
#'
rxUdfUiControl <- function(value) {
  if (missing(value)) {
    .udfUiEnv$control
  } else if (is.list(value)) {
    .udfUiEnv$control <- value
  } else if (is.null(value)) {
    .udfUiEnv$control <- value
  } else {
    stop("rxUdfUiControl must be called with a list, NULL, or without any arguments", call. = FALSE)
  }
  invisible(.udfUiEnv$control)
}
#' Return the current estimation method for the UI processing
#'
#' @param value when specified, this assigns the character value of
#'   the estimation method or NULL if there is nothing being estimated
#' @return value of the estimation method being processed or NULL
#' @family User functions
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' rxUdfUiEst()
#'
rxUdfUiEst <- function(value) {
  if (missing(value)) {
    .udfUiEnv$est
  } else if (checkmate::testCharacter(value, min.chars = 1L, any.missing = FALSE, len = 1L)) {
    .udfUiEnv$est <- value
  } else if (is.null(value)) {
    .udfUiEnv$est <- value
  } else {
    stop("rxUdfUiEst must be called with a character, NULL, or without any arguments", call. = FALSE)
  }
}
#' Returns if the current ui function is being parsed
#'
#' @return logical if the current ui function is being parsed
#' @family User functions
#' @export
#' @author Matthew L. Fidler
#' @examples
#' rxUdfUiParsing()
rxUdfUiParsing <- function() {
  .udfUiEnv$parsing
}


#' Handle User-Defined Functions in UI
#'
#' This function processes expressions to handle user-defined
#' functions in the UI.  It will see if there is any registered `s3`
#' generic in `rxUdfUi` and call that with the parsed function.  The
#' s3 generic is responsible for returning a list in the correct form
#' so that the parsed UI will be updated.
#'
#' @param expr The expression to be processed.
#' @param env The environment in which to evaluate the expression.
#' @return The processed expression.
#' @noRd
.handleUdfUi <- function(expr, env) {
  if (is.call(expr)) {
    if (length(expr) == 1L) {
      return(expr)
    }
    .c <- as.character(expr[[1]])
    .fun <- .rxUdfUiMethod("rxUdfUi", .c)
    if (is.null(.fun)) {
      as.call(c(expr[[1]], lapply(expr[-1], .handleUdfUi, env = env)))
    } else {
      if (!exists(.c, envir = env$rxUdfUiCount)) {
        assign(.c, 0L, envir = env$rxUdfUiCount)
      }
      .num <- get(.c, envir = env$rxUdfUiCount) + 1L
      assign(.c, .num, envir = env$rxUdfUiCount)
      .udfUiEnv$num <- .num
      .udfUiEnv$iniDf <- env$df
      .udfUiEnv$lhs <- env$lhs
      .e <- .fun(expr)
      if (is.language(.e$replace)) {
        if (!identical(expr, .e$replace)) {
          env$redo <- TRUE
        }
        expr <- .e$replace
      } else if (
        length(.e$replace) == 1 &&
          inherits(.e$replace, "character")
      ) {
        .t <- try(str2lang(.e$replace), silent = TRUE)
        if (inherits(.t, "try-error")) {
          stop(
            "rxode2 ui user function '",
            .c,
            "' failed to produce code that could be parsed '",
            .e$replace,
            "'",
            call. = FALSE
          )
        }
        if (!identical(expr, .t)) {
          env$redo <- TRUE
        }
        expr <- .t
      } else {
        stop("rxode2 ui user function '", .c, "' failed to produce code that could be parsed", call. = FALSE)
      }
      .handleUdifUiBeforeOrAfter("before", .e, env, .c)
      .handleUdifUiBeforeOrAfter("after", .e, env, .c)
      if (inherits(.e$iniDf, "data.frame")) {
        env$df <- .e$iniDf
      }
      if (
        is.null(.udfUiEnv$data) &&
          checkmate::testLogical(.e$uiUseData, len = 1L, any.missing = FALSE)
      ) {
        env$uiUseData <- .e$uiUseData
      }
      if (
        is.null(.udfUiEnv$mv) &&
          checkmate::testLogical(.e$uiUseMv, len = 1L, any.missing = FALSE)
      ) {
        env$uiUseMv <- .e$uiUseMv
      }
      if (!is.call(expr)) {
        return(expr)
      }
      expr <- as.call(c(expr[[1]], lapply(expr[-1], .handleUdfUi, env = env)))
      if (
        is.call(expr) &&
          length(expr) >= 2L &&
          (identical(expr[[1]], quote(`+`)) ||
            identical(expr[[1]], quote(`-`)) ||
            identical(expr[[1]], quote(`^`)) ||
            identical(expr[[1]], quote(`/`)) ||
            identical(expr[[1]], quote(`*`)))
      ) {
        expr <- str2lang(paste0("(", deparse1(expr), ")"))
      }
      expr
    }
  } else {
    expr
  }
}

#' Expand the ui user functions in model lines that are pure rewrites
#'
#' A call whose `rxUdfUi` method returns only `replace` (like `plogis()` ->
#' `expit()`) is replaced; one that also changes `iniDf` or adds lines (like
#' `linMod()`) is left as written.  Used so stored model lines that were piped
#' in unexpanded can be read by the C parser.
#'
#' @param lines list of model lines
#' @param iniDf the ui's initialization data frame
#' @return lines with pure-rewrite user functions expanded
#' @noRd
#' @author Matthew L. Fidler
.rxUdfUiExpandPure <- function(lines, iniDf) {
  .fields <- c("num", "iniDf", "lhs", "parsing", "probs", "np", "na")
  .had <- vapply(.fields, exists, logical(1), envir = .udfUiEnv, inherits = FALSE)
  .old <- mget(.fields[.had], envir = .udfUiEnv)
  on.exit({
    .new <- setdiff(.fields[!.had], names(.old))
    rm(list = intersect(.new, ls(.udfUiEnv, all.names = TRUE)), envir = .udfUiEnv)
    list2env(.old, envir = .udfUiEnv)
  })
  .args <- function(expr, depth) {
    as.call(c(expr[[1]], lapply(as.list(expr[-1]), .expand, depth = depth)))
  }
  .expand <- function(expr, depth = 0L) {
    if (!is.call(expr) || length(expr) == 1L) {
      return(expr)
    }
    if (
      length(expr) == 3L &&
        (identical(expr[[1]], quote(`<-`)) || identical(expr[[1]], quote(`=`)))
    ) {
      .udfUiEnv$lhs <- expr[[2]]
      expr[[3]] <- .expand(expr[[3]], depth)
      return(expr)
    }
    .fun <- if (is.name(expr[[1]])) .rxUdfUiMethod("rxUdfUi", as.character(expr[[1]])) else NULL
    if (is.null(.fun) || depth > 20L) {
      return(.args(expr, depth))
    }
    .udfUiEnv$num <- 1L
    .udfUiEnv$iniDf <- iniDf
    .udfUiEnv$parsing <- TRUE
    .e <- try(suppressWarnings(suppressMessages(.fun(expr))), silent = TRUE)
    if (
      is.list(.e) &&
        setequal(names(.e), "replace") &&
        (is.language(.e$replace) || is.character(.e$replace) && length(.e$replace) == 1L)
    ) {
      .r <- .e$replace
      if (is.character(.r)) {
        .r <- try(str2lang(.r), silent = TRUE)
      }
      if (is.language(.r) && !is.expression(.r) && !identical(.r, expr)) {
        return(.expand(.r, depth + 1L))
      }
    }
    # not a pure rewrite (or already in parser form): keep the call, but its
    # arguments may still hold one
    .args(expr, depth)
  }
  lapply(lines, .expand)
}

#' Is this model line's LHS a registered user function?
#'
#' `.handleUdfUi()` only ever walks the RIGHT-hand side of a line, and a `~`
#' line never reaches it at all (`.errHandleTilde()` claims those first).  So a
#' construct like
#'
#'     dist(eta.cl) ~ dgamma(shape = a, rate = b)
#'
#' has nowhere to be handled: `dist(eta.cl)` is not rxode2 grammar, and `~` is
#' already overloaded for endpoints and for hidden variables.  This is the hook
#' for it -- a user-function dispatch on the LHS, which is an R-level rule that
#' applies only to functions someone has registered an `rxUdfUiLhs` method for,
#' and never changes how rxode2 itself parses anything.
#'
#' @param expr one model line
#' @return the method name when the line's LHS is a call to a function with an
#'   `rxUdfUiLhs` method, otherwise NULL
#' @noRd
#' @author Matthew L. Fidler
.rxUdfUiLhsName <- function(expr) {
  if (!is.call(expr) || length(expr) != 3L) {
    return(NULL)
  }
  .op <- expr[[1]]
  if (!(identical(.op, quote(`~`)) || identical(.op, quote(`<-`)) || identical(.op, quote(`=`)))) {
    return(NULL)
  }
  .lhs <- expr[[2]]
  if (!is.call(.lhs) || length(.lhs) < 1L) {
    return(NULL)
  }
  .nm <- .lhs[[1]]
  if (!is.name(.nm)) {
    return(NULL)
  }
  .c <- as.character(.nm)
  if (is.null(.rxUdfUiMethod("rxUdfUiLhs", .c))) {
    return(NULL)
  }
  .c
}

#' Handle a user function on the LEFT-hand side of a model line
#'
#' The LHS analogue of [rxUdfUi()].  Dispatched by `.rxUdfUiLhsName()` on the
#' function called on the left of `~`, `<-` or `=`, and handed both sides.
#'
#' @param fun the LHS call, as a language object -- eg `dist(eta.cl)`
#' @param rhs the right-hand side, as a language object
#'
#' @return a list with the same fields [rxUdfUi()] returns (`iniDf`, `before`,
#'   `after`, `replace`), with one addition: `replace = NULL` DROPS the line,
#'   which is what a declaration that only modifies `iniDf` wants.
#'
#' @export
#' @keywords internal
#' @author Matthew L. Fidler
rxUdfUiLhs <- function(fun, rhs) {
  UseMethod("rxUdfUiLhs")
}

#' Run one LHS user function and fold its result into the parse environment
#'
#' @param expr the model line
#' @param env parse environment
#' @param what the method name from `.rxUdfUiLhsName()`
#' @return the replacement expression, or NULL to drop the line
#' @noRd
#' @author Matthew L. Fidler
.handleUdfUiLhs <- function(expr, env, what) {
  .fun <- utils::getS3method("rxUdfUiLhs", what)
  .udfUiEnv$iniDf <- env$df
  .udfUiEnv$lhs <- expr[[2]]
  ## Route the method's own error through the parser's error collection rather
  ## than letting it propagate.  A raw stop() here is swallowed by as.rxUi()'s
  ## handler and the user is told only "cannot convert to rxUi object", which
  ## says nothing about which declaration was wrong or why.  errGlobal is
  ## raised verbatim after the walk finishes.
  .e <- tryCatch(.fun(expr[[2]], expr[[3]]), error = function(e) {
    ## ALSO emitted as a message, because as.rxUi() replaces any
    ## error raised in here -- the parser's own included -- with
    ## a bare "cannot convert to rxUi object", and a declaration
    ## rejected without saying which one or why is not a usable
    ## diagnostic.
    message(conditionMessage(e))
    assign("errGlobal", c(env$errGlobal, conditionMessage(e)), envir = env)
    NULL
  })
  if (is.null(.e)) {
    return(NULL)
  }
  if (!is.list(.e)) {
    assign("errGlobal", c(env$errGlobal, paste0("rxode2 ui lhs function '", what, "' must return a list")), envir = env)
    return(NULL)
  }
  .handleUdifUiBeforeOrAfter("before", .e, env, what)
  .handleUdifUiBeforeOrAfter("after", .e, env, what)
  if (inherits(.e$iniDf, "data.frame")) {
    env$df <- .e$iniDf
  }
  .r <- .e$replace
  if (is.null(.r)) {
    return(NULL)
  }
  if (is.language(.r)) {
    return(.r)
  }
  if (length(.r) == 1L && inherits(.r, "character")) {
    .t <- try(str2lang(.r), silent = TRUE)
    if (inherits(.t, "try-error")) {
      stop("rxode2 ui lhs function '", what, "' failed to produce code that could be parsed '", .r, "'", call. = FALSE)
    }
    return(.t)
  }
  stop("rxode2 ui lhs function '", what, "' failed to produce code that could be parsed", call. = FALSE)
}

#' This function is called when processing rxode2 user functions from
#' the models
#'
#'
#' @param fun this is the function that needs to be parsed and
#'   changed.  This is a R language expression
#'
#' @return This needs to return a list with the following elements:
#'
#' - `iniDf` -- the modified initial estimate data.frame
#'
#' - `before` -- any model code that needs to be added before the current line
#'
#' - `after` -- any model code that needs to be added after the current line
#'
#' - `replace` -- replacement code for this user function
#'
#' @export
#' @keywords internal
#' @author Matthew L. Fidler
rxUdfUi <- function(fun) {
  UseMethod("rxUdfUi")
}

#' Get the number of arguments for user defined functions for ui
#' replacement
#'
#' @param fun The rxode2 ui function to replace
#' @return The number of arguments needed for this function
#' @noRd
#' @author Matthew L. Fidler
.rxUdfUiNarg <- function(fun) {
  .cls <- try(utils::getS3method("rxUdfUi", fun), silent = TRUE)
  if (inherits(.cls, "try-error")) {
    return(NA_integer_)
  }
  .nargs <- attr(.cls, "nargs")
  if (is.null(.nargs)) {
    return(NA_integer_)
  }
  as.integer(.nargs)
}

#' Convert a positive integer to a letter series
#'
#' @param x integer to convert
#' @param base can be 2 to 26
#' @return a sequence of letters representing the number(s) input
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' rxIntToLetter(1:100)
#'
rxIntToLetter <- function(x, base = 26L) {
  checkmate::testIntegerish(x, lower = 0L, any.missing = FALSE)
  checkmate::testIntegerish(base, lower = 2L, upper = 26L, any.missing = FALSE, len = 1L)
  .Call(`_rxode2_itoletter`, as.integer(x), as.integer(base), PACKAGE = "rxode2")
}

#' Convert a positive  base
#'
#' @param x integer to convert
#' @param base can be 2 to 36
#' @return a sequence of letters and representing the number(s) input
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' rxIntToBase(1:100)
#'
rxIntToBase <- function(x, base = 36L) {
  checkmate::testIntegerish(x, lower = 0L, any.missing = FALSE)
  checkmate::testIntegerish(base, lower = 2L, upper = 36L, any.missing = FALSE, len = 1L)
  .Call(`_rxode2_itostr`, as.integer(x), as.integer(base), PACKAGE = "rxode2")
}

.handleUdifUiBeforeOrAfter <- function(type = "before", e, env, fun) {
  .cur <- e[[type]]
  if (is.null(.cur)) {
    return(invisible())
  }
  if (is.list(.cur)) {
    .ret <- lapply(seq_along(.cur), function(i) {
      if (is.language(.cur[[i]])) {
        .cur[[i]]
      } else if (
        length(.cur[[i]]) == 1L &&
          inherits(.cur[[i]], "character")
      ) {
        .ret <- try(str2lang(.cur[[i]]), silent = TRUE)
        if (inherits(.ret, "try-error")) {
          stop(
            "rxode2 ui user function '",
            fun,
            "' failed to produce code that could be parsed '",
            .cur[[i]],
            "' in $",
            type,
            call. = FALSE
          )
        }
        .ret
      } else {
        stop(
          "rxode2 ui user function '",
          fun,
          "' failed to produce code that could be parsed in $",
          type,
          call. = FALSE
        )
      }
    })
    assign(type, c(get(type, env), .ret), envir = env)
  } else if (is.language(.cur)) {
    assign(type, c(get(type, env), list(.cur)), envir = env)
  } else if (inherits(.cur, "character")) {
    .ret <- lapply(seq_along(.cur), function(i) {
      .ret <- try(str2lang(.cur[[i]]), silent = TRUE)
      if (inherits(.ret, "try-error")) {
        stop(
          "rxode2 ui user function '",
          fun,
          "' failed to produce code that could be parsed '",
          .cur[[i]],
          "' in $",
          type,
          call. = FALSE
        )
      }
      .ret
    })
    assign(type, c(get(type, env), .ret), envir = env)
  } else {
    stop("rxode2 ui user function '", fun, "' failed to produce code that could be parsed in $", type, call. = FALSE)
  }
}
