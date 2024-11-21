.udfUiEnv <- new.env(parent=emptyenv())

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
  if (checkmate::testIntegerish(.udfUiEnv$num, lower=1L, len=1L, any.missing=FALSE))  {
    as.integer(.udfUiEnv$num)
  } else {
    1L
  }
}

#' Get the rxode2 iniDf of the current UI being processed (or return NULL)
#'
#' @return Initial `data.frame` being processed or `NULL` for nothing.
#'
#' @export
#' @author Matthew L. Fidler
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
    stop("rxUdfUiMt must be called with model variables, NULL, or without any arguments",
         call.=FALSE)
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
    stop("rxUdfUiData must be called with a data.frame, NULL, or without any arguments",
         call.=FALSE)
  }
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
    stop("rxUdfUiControl must be called with a list, NULL, or without any arguments",
         call.=FALSE)
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
  } else if (checkmate::testCharacter(value, min.chars=1L, any.missing=FALSE, len=1L)) {
    .udfUiEnv$est <- value
  } else if (is.null(value)) {
    .udfUiEnv$est <- value
  } else {
    stop("rxUdfUiEst must be called with a character, NULL, or without any arguments",
         call.=FALSE)
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
    .fun <- try(utils::getS3method("rxUdfUi", .c), silent=TRUE)
    if (inherits(.fun, "try-error")) {
      as.call(c(expr[[1]], lapply(expr[-1], .handleUdfUi, env=env)))
    } else {
      if (!exists(.c, envir=env$rxUdfUiCount)) {
        assign(.c, 0L, envir=env$rxUdfUiCount)
      }
      .num <- get(.c, envir=env$rxUdfUiCount) + 1L
      assign(.c, .num, envir=env$rxUdfUiCount)
      .udfUiEnv$num <- .num
      .udfUiEnv$iniDf <- env$df
      .udfUiEnv$lhs <- env$lhs
      .e <- .fun(expr)
      if (is.language(.e$replace)) {
        if (!identical(expr, .e$replace)) {
          env$redo <- TRUE
        }
        expr <- .e$replace
      } else if (length(.e$replace) == 1 &&
                   inherits(.e$replace, "character")) {
        .t <- try(str2lang(.e$replace), silent=TRUE)
        if (inherits(.t, "try-error")) {
          stop("rxode2 ui user function '", .c, "' failed to produce code that could be parsed '", .e$replace, "'",
               call.=FALSE)
        }
        if (!identical(expr, .t)) {
          env$redo <- TRUE
        }
        expr <- .t
      } else {
        stop("rxode2 ui user function '", .c, "' failed to produce code that could be parsed in the",
             call.=FALSE)
      }
      .handleUdifUiBeforeOrAfter("before", .e, env, .c)
      .handleUdifUiBeforeOrAfter("after", .e, env, .c)
      if (inherits(.e$iniDf, "data.frame")) {
        env$df <- .e$iniDf
      }
      if (is.null(.udfUiEnv$data) &&
            checkmate::testLogical(.e$uiUseData, len=1L, any.missing=FALSE)) {
        env$uiUseData <- .e$uiUseData
      }
      if (is.null(.udfUiEnv$mv) &&
            checkmate::testLogical(.e$uiUseMv, len=1L, any.missing=FALSE)) {
        env$uiUseMv <- .e$uiUseMv
      }
      if (!is.call(expr)) return(expr)
      expr <- as.call(c(expr[[1]], lapply(expr[-1], .handleUdfUi, env=env)))
      if (is.call(expr) && length(expr) >= 2L &&
            (identical(expr[[1]], quote(`+`)) ||
               identical(expr[[1]], quote(`-`)) ||
               identical(expr[[1]], quote(`^`)) ||
               identical(expr[[1]], quote(`/`)) ||
               identical(expr[[1]], quote(`*`)))) {
        expr <- str2lang(paste0("(", deparse1(expr), ")"))
      }
      expr
    }
  } else {
    expr
  }
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
  .cls <- try(utils::getS3method("rxUdfUi", fun), silent=TRUE)
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
rxIntToLetter <- function(x, base=26L) {
  checkmate::testIntegerish(x, lower=0L, any.missing=FALSE)
  checkmate::testIntegerish(base, lower=2L, upper=26L, any.missing=FALSE, len=1L)
  .Call(`_rxode2_itoletter`, as.integer(x), as.integer(base), PACKAGE="rxode2")
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
rxIntToBase <- function(x, base=36L) {
  checkmate::testIntegerish(x, lower=0L, any.missing=FALSE)
  checkmate::testIntegerish(base, lower=2L, upper=36L, any.missing=FALSE, len=1L)
  .Call(`_rxode2_itostr`, as.integer(x), as.integer(base), PACKAGE="rxode2")
}

.handleUdifUiBeforeOrAfter <- function(type="before", e, env, fun) {
  .cur <- e[[type]]
  if (is.null(.cur)) return(invisible())
  if (is.list(.cur)) {
    .ret <- lapply(seq_along(.cur),
                   function(i) {
                     if (is.language(.cur[[i]])) {
                       .cur[[i]]
                     } else if (length(.cur[[i]]) == 1L &&
                                  inherits(.cur[[i]], "character")) {
                       .ret <- try(str2lang(.cur[[i]]), silent=TRUE)
                       if (inherits(.ret, "try-error")) {
                         stop("rxode2 ui user function '", fun, "' failed to produce code that could be parsed '", .cur[[i]],
                              "' in $",
                              type,
                              call.=FALSE)
                       }
                       .ret
                     } else {
                       stop("rxode2 ui user function '", fun, "' failed to produce code that could be parsed in $", type,
                            call.=FALSE)
                     }
                   })
    assign(type, c(get(type, env), .ret), envir=env)
  } else if (is.language(.cur)) {
    assign(type, c(get(type, env), list(.cur)), envir=env)
  } else if (inherits(.cur, "character")) {
    .ret <- lapply(seq_along(.cur),
                   function(i) {
                     .ret <- try(str2lang(.cur[[i]]), silent=TRUE)
                     if (inherits(.ret, "try-error")) {
                       stop("rxode2 ui user function '", fun, "' failed to produce code that could be parsed '", .cur[[i]],
                            "' in $",
                            type,
                            call.=FALSE)
                     }
                     .ret
                   })
    assign(type, c(get(type, env), .ret), envir=env)
  } else {
    stop("rxode2 ui user function '", fun, "' failed to produce code that could be parsed in $", type,
         call.=FALSE)
  }
}
