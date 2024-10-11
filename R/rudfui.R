.udfUiEnv <- new.env(parent=emptyenv())
.udfUiEnv$num <- 1L
.udfUiEnv$iniDf <- NULL
.udfUiEnv$lhs <- NULL

#' This gives the current number in the ui of the particular function being called.
#'
#' If this is called outside of function parsing or the input is
#' unexpected this returns 1L. This is useful when writing replacement
#' UI functions
#'
#' @return integer greater than 1L
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' rxUdfUiNum()
#'
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
      expr <- str2lang(paste0("(", deparse1(expr), ")"))
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
#' @param num This represents the user function number in the model
#'
#' @param fun this is the function that needs to be parsed and
#'   changed.  This is a R language expression
#'
#' @param iniDf initial values `data.frame`
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

#' @export
rxUdfUi.linMod <- function(fun) {
  eval(fun)
}
attr(rxUdfUi.linMod, "nargs") <- 2L

#' @export
rxUdfUi.linMod0 <- function(fun) {
  eval(fun)
}
attr(rxUdfUi.linMod, "nargs") <- 2L

#' @export
rxUdfUi.linModB <- function(fun) {
  eval(fun)
}
attr(rxUdfUi.linMod, "nargs") <- 2L

#' @export
rxUdfUi.linModB0 <- function(fun) {
  eval(fun)
}
attr(rxUdfUi.linMod, "nargs") <- 2L

#' @export
rxUdfUi.linModA <- function(fun) {
  eval(fun)
}
attr(rxUdfUi.linMod, "nargs") <- 2L

#' @export
rxUdfUi.linModA0 <- function(fun) {
  eval(fun)
}
attr(rxUdfUi.linMod, "nargs") <- 2L

#' @export
rxUdfUi.default <- function(fun) {
  stop("rxode2 user defined function '", fun, "' not supported", call.=FALSE) # nocov
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
#' Linear model to replace in rxode2 ui model
#'
#' @param variable The variable that the rxode2 will be made on
#' @param power The power of the polynomial that will be generated
#' @param intercept Boolean that tells if the intercept be generated
#' @param type the type of linear model replacement to be used.
#' @param num the number the particular model is being generated. If
#'   unspecified, query using `rxUdfUiNum()`.
#' @param iniDf the initialization `data.frame`, if `NULL` query using
#'   `rxUdfUiIniDf()`
#' @return a list for use in when generating the `rxode2` ui model see
#'   `rxUdfUi()` for details.
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' linMod(x, 3)
linMod <- function(variable, power, intercept=TRUE,type=c("replace", "before", "after"),
                   num=NULL, iniDf=NULL) {
  .var <- as.character(substitute(variable))
  .tmp <- try(force(variable), silent=TRUE)
  if (!inherits(.tmp, "try-error")) {
    if (is.character(.tmp)) {
      .var <- variable
    }
  }
  checkmate::assertCharacter(.var, len=1L, any.missing=FALSE, pattern = "^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$", min.chars=1L,
                             .var.name="variable")
  checkmate::assertLogical(intercept, len=1L, any.missing=FALSE)
  checkmate::assertIntegerish(power, lower=ifelse(intercept, 0L, 1L), len=1L)
  if (is.null(num)) {
    num <- rxUdfUiNum()
  }
  checkmate::assertIntegerish(num, lower=1, any.missing=FALSE, len=1)
  if (is.null(iniDf)) {
    iniDf <- rxUdfUiIniDf()
  }
  assertIniDf(iniDf, null.ok=TRUE)
  type <- match.arg(type)
  .pre <- paste0("rx.linMod.", .var, num, rxIntToLetter(seq_len(power+ifelse(intercept, 1L, 0L))-1L))
  if (!is.null(iniDf)) {
    .theta <- iniDf[!is.na(iniDf$ntheta),,drop=FALSE]
    if (length(.theta$ntheta) > 0L) {
      .maxTheta <- max(.theta$ntheta)
      .theta1 <- .theta[1,]
    } else {
      .maxTheta <- 0L
      .theta1 <- .rxBlankIni("theta")
    }
    .theta1$lower <- -Inf
    .theta1$upper <- Inf
    .theta1$est <- 0
    .theta1$fix <- FALSE
    .theta1$label <- NA_character_
    .theta1$backTransform <- NA_character_
    .theta1$condition <- NA_character_
    .theta1$err <- NA_character_
    .cur <- c(list(.theta),
              lapply(seq_along(.pre), function(i) {
                .cur <- .theta1
                .cur$name <- .pre[i]
                .cur$ntheta <- .maxTheta+i
                .cur
              }))
    .theta <- do.call(`rbind`, .cur)
    .eta <- iniDf[is.na(iniDf$neta),,drop=FALSE]
    .iniDf <- rbind(.theta, .eta)
  } else {
    .iniDf <- NULL
  }
  .linMod <- paste(vapply(seq_along(.pre),
                          function(i) {
                            if (intercept) {
                              if (i == 1) return(.pre[i])
                              if (i == 2) return(paste0(.pre[i], "*", .var))
                              paste0(.pre[i], "*", paste0(.var,"^", i-1L))
                            } else {
                              if (i == 1) return(paste0(.pre[i], "*", .var))
                              paste0(.pre[i], "*", paste0(.var,"^", i))
                            }
                          }, character(1)), collapse="+")
  if (type == "replace") {
    list(replace=.linMod,
         iniDf=.iniDf )
  } else if (type == "before") {
    .replace <- paste0("rx.linMod.", .var, ".f", num)
    list(before=paste0(.replace, " <- ", .linMod),
         replace=.replace,
         iniDf=.iniDf)
  } else if (type == "after") {
    .replace <- paste0("rx.linMod.", .var, ".f", num)
    list(after=paste0(.replace, " <- ", .linMod),
         replace="0",
         iniDf=.iniDf)
  }
}
#' @describeIn linMod linear model without intercept
#' @export
linMod0 <- function(...,intercept=FALSE) {
  linMod(..., intercept=intercept)
}

#' @describeIn linMod linear model before where it occurs
#' @export
linModB <- function(..., type="before") {
  linMod(..., type=type)
}

#' @describeIn linMod linear model before where the user function occurs
#' @export
linModB0 <- function(..., intercept=FALSE, type="before") {
  linMod(..., intercept=intercept, type=type)
}
#' @describeIn linMod linear model after where the user function occurs
linModA <- function(..., type="after") {
  linMod(..., type=type)
}

#' @describeIn linMod liner model without an intercept placed after where the user function occurs
#' @export
linModA0 <- function(..., intercept=FALSE, type="after") {
  linMod(..., intercept=intercept, type=type)
}

.handleUdifUiBeforeOrAfter <- function(type="before", e, env, fun) {
  .cur <- e[[type]]
  if (is.null(.cur)) return(invisible())
  if (is.list(.cur)) {
    .ret <- lapply(seq_along(.cur),
                   function(i) {
                     if (is.language(.cur[[i]])) {
                       .cur[[i]]
                     } else if (length(cur[[i]]) == 1L &&
                                  -                                  inheritsc(cur[[i]], "character")) {
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
