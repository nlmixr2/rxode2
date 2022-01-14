.regIdentifier1 <- rex::rex(
  one_of("a":"z", "A":"Z"),
  any_of("_", "a":"z", "A":"Z", "0":"9", ".")
)
.regIdentifier2 <- rex::rex(
  at_least(".", 1),
  one_of("_", "a":"z", "A":"Z"),
  any_of("_", "a":"z", "A":"Z", "0":"9", ".")
)
.regIdentifier <- rex::rex(or(.regIdentifier1, .regIdentifier2))
regSens <- rex::rex(
  "rx__sens_", capture(.regIdentifier), "_BY_",
  capture(.regIdentifier), "__"
)
regSensEtaTheta <- rex::rex(
  "rx__sens_", capture(.regIdentifier),
  "_BY_", capture(.regIdentifier),
  "_BY_", capture(.regIdentifier), "__"
)
regToSens1 <- rex::rex(
  capture(.regIdentifier), or("_", ".", ":"),
  capture(.regIdentifier)
)
regToSens2 <- rex::rex(
  "d/dt(d(", capture(.regIdentifier), ")/d(",
  capture(.regIdentifier), "))"
)
regFloat1 <- rex::rex(
  or(
    group(some_of("0":"9"), ".", any_of("0":"9")),
    group(any_of("0":"9"), ".", some_of("0":"9"))
  ),
  maybe(group(
    one_of("E", "e"),
    maybe(one_of("+", "-")),
    some_of("0":"9")
  ))
)
regFloat2 <- rex::rex(
  some_of("0":"9"), one_of("E", "e"),
  maybe(one_of("-", "+")), some_of("0":"9")
)
regDecimalint <- rex::rex(or("0", group("1":"9", any_of("0":"9"))))
regNum <- rex::rex(maybe("-"), or(
  regDecimalint, regFloat1,
  regFloat2
))
regDDt <- rex::rex(
  start, "rx__d_dt_",
  capture(anything), "__", end
)
regDfDy <- rex::rex(
  start, "rx__df_", capture(anything),
  "_dy_", capture(anything), "__", end
)
regThEt <- rex::rex(
  capture(or("TH", ""), "ETA"), "_",
  capture("1":"9", any_of("0":"9")), "_"
)
regDfDyTh <- rex::rex(
  start, "rx__df_", capture(anything),
  "_dy_", regThEt, "__", end
)
regEta <- rex::rex(
  start, "ETA[",
  capture("1":"9", any_of("0":"9")),
  "]"
)
regTheta <- rex::rex(
  start, "THETA[",
  capture("1":"9", any_of("0":"9")),
  "]"
)
regJac <- rex::rex(
  "df(", capture(.regIdentifier), ")/dy(",
  capture(or(
    .regIdentifier,
    group(
      or("THETA[", "ETA["), "1":"9",
      any_of("0":"9"), "]"
    )
  )),
  ")"
)
.regRate <- rex::rex(start, "rx_rate_", capture(anything), "_")
.regDur <- rex::rex(start, "rx_dur_", capture(anything), "_")
.regLag <- rex::rex(start, "rx_lag_", capture(anything), "_")
.regF <- rex::rex(start, "rx_f_", capture(anything), "_")
.knownPrint <- c(
  "printf", "Rprintf", "print",
  "jac_printf", "jac_Rprintf", "jac_print",
  "ode_printf", "ode_Rprintf", "ode_print",
  "jac0_printf", "jac0_Rprintf", "jac0_print",
  "ode_printf", "ode_Rprintf", "ode_print",
  "ode0_printf", "ode0_Rprintf", "ode0_print",
  "lhs_printf", "lhs_Rprintf", "lhs_print"
)

regPrint <- rex::rex(
  start, or(.knownPrint),
  or(
    group(
      "(", anything, ")",
      any_spaces, at_most(";", 1), any_spaces
    ),
    group(any_spaces, at_most(";", 1), any_spaces)
  ),
  end
)

regIni0 <- rex::rex(
  start, "rx_", capture(anything),
  "_ini_0__", end
)
regIni <- rex::rex(or(
  group(one_of("_."), "0"),
  "0", "(0)", "[0]", "{0}"
), end)
#' Expand if/else clauses into multiple different types of lines.
#'
#'
#' @param model Model can be a character, or a rxode2 model.  It needs
#'     to have normalized syntax, that is `if (...){}` has to be
#'     on the same line.  The `else` statement must be on its
#'     own line with the closing bracket of the `if` statement
#'     on the previous line.  This `else` statment must also
#'     contain the opening bracket, like the code `else {}`
#' @param removeInis A boolean indicating if parameter
#'     initializations should be removed from the model.
#' @param removePrint A boolean indicating if printing statements
#'     should be removed from the model.
#' @return A named character vector. The names of the vector are the
#'     logical conditions, the values are the lines that satisfy the
#'     logical conditions.
#' @author Matthew L. Fidler
#' @keywords internal
#' @export
rxExpandIfElse <- function(model, removeInis = TRUE, removePrint = TRUE) {
  ## expand if/else blocks into a list with lines for conditions that are true
  x <- strsplit(rxNorm(model, FALSE), "\n")[[1]]
  if (removeInis) {
    x <- .rxRmIni(x)
  }
  if (removePrint) {
    x <- .rxRmPrint(x)
  }
  model <- x
  w1 <- which(regexpr(regIfOrElse, model) != -1)
  w2 <- which(regexpr(regEnd, model) != -1)
  if (length(w1) > 0) {
    currExpr <- ""
    lst <- list()
    last <- ""
    known <- list()
    for (i in seq_along(model)) {
      if (any(i == w1)) {
        if (regexpr(regElse, model[i]) != -1) {
          currExpr[length(currExpr) + 1] <- sprintf("!(%s)", last)
        } else {
          currExpr[length(currExpr) + 1] <- gsub(regIf, "!(\\1)", model[i])
          known[[length(known) + 1]] <- c(
            paste(paste0("(", currExpr[-1], ")"),
              collapse = " && "
            ),
            currExpr[-1]
          )
          currExpr[length(currExpr)] <- gsub(regIf, "\\1", model[i])
          known[[length(known) + 1]] <- c(
            paste(paste0("(", currExpr[-1], ")"),
              collapse = " && "
            ),
            currExpr[-1]
          )
        }
        lst[[i]] <- "control"
      } else if (any(i == w2)) {
        last <- currExpr[length(currExpr)]
        currExpr <- currExpr[seq(1, length(currExpr) - 1)]
        lst[[i]] <- "control"
      } else {
        lst[[i]] <- currExpr
      }
    }
    ret <- list()
    rm <- NULL
    for (i in seq_along(known)) {
      mod <- NULL
      for (j in seq_along(model)) {
        if (identical(lst[[j]], "")) {
          mod[length(mod) + 1] <- model[j]
        } else {
          i1 <- lst[[j]][-1]
          i2 <- known[[i]][-1]
          i3 <- i2[seq(1, min(length(i1), length(i2)))]
          if (!identical(i2, i3)) {
            ## Find the expression to remove
            for (k in seq_along(known)) {
              i4 <- known[[k]][-1]
              if (identical(i4, i3)) {
                rm <- c(rm, known[[k]][1])
              }
            }
          }
          if (identical(i1, i3)) {
            mod[length(mod) + 1] <- model[j]
          }
        }
      }
      ret[[known[[i]][1]]] <- paste(mod, collapse = "\n")
    }
    ret <- unlist(ret)
    ret <- ret[!(names(ret) %in% rm)]
    return(ret)
  } else {
    return(paste(model, collapse = "\n"))
  }
}

#' Remove INIs
#' @param x rxode2 list of lines to remove
#' @return rxode2 lines with inis removed.
#' @author Matthew L. Fidler
#' @keywords internal
#' @noRd
.rxRmIni <- function(x) {
  x <- x[regexpr(rex::rex(
    start, any_spaces,
    or(names(rxInits(x))), any_spaces,
    or("=", "~")
  ), x) == -1]
  x <- x[regexpr(rex::rex(
    start, any_spaces,
    or(names(rxInits(x))), "(0)",
    any_spaces, or("=", "~")
  ), x) == -1]
  return(x)
}

#' Remove print statements
#' @param x rxode2 lines to remove
#' @return rxode2 with print lines removed.
#' @author Matthew L. Fidler
#' @noRd
.rxRmPrint <- function(x) {
  return(x[regexpr(getFromNamespace("regPrint", "rxode2"), x) == -1])
}

#' Add a return statment to a function.
#'
#' @param fn Function to deparse
#' @param ret boolean stating if a return statement will be added.
#' @return Function with parens removed and add a return statment.
#' @author Matthew L. Fidler
#' @noRd
rxAddReturn <- function(fn, ret = TRUE) {
  txt <- deparse(body(fn))
  if (txt[1] == "{") {
    txt <- txt[-c(1, length(txt))]
  }
  .v <- try(parse(text = txt[length(txt)]), silent = TRUE)
  while (inherits(.v, "try-error")) {
    .v0 <- txt[length(txt)]
    txt <- txt[-length(txt)]
    txt[length(txt)] <- paste(txt[length(txt)], .v0)
    .v <- try(parse(text = txt[length(txt)]), silent = TRUE)
  }
  ## FIXME, naieve assumption about functions.
  if (ret) {
    if (regexpr(rex::rex(
      or(boundary, start),
      "return(", anything, ")"
    ),
    txt[length(txt)],
    perl = TRUE
    ) == -1) {
      ## Add return statement
      if (regexpr(
        rex::rex(or("=", "~", "<-", "}")),
        txt[length(txt)]
      ) == -1) {
        txt[length(txt)] <- gsub(
          rex::rex(
            start, any_spaces, capture(anything),
            or(";", ""), any_spaces, end
          ),
          "return(\\1);", txt[length(txt)]
        )
      }
    }
  }
  return(paste(txt, collapse = "\n"))
}



## Start DSL based on http://adv-r.had.co.nz/dsl.html
## These operators are called to create the language and are
## not called in tests.
unaryOp <- function(left, right) {
  force(left)
  force(right)
  function(e1) {
    paste0(left, e1, right)
  }
}

binaryOp <- function(sep) {
  force(sep)
  function(e1, e2) {
    if (missing(e2)) {
      paste0(gsub(" ", "", sep), .rxRepRxQ(e1))
    } else {
      paste0(.rxRepRxQ(e1), sep, .rxRepRxQ(e2))
    }
  }
}

binaryOp2 <- function(sep) {
  force(sep)
  function(e1, e2) {
    paste0("(", .rxRepRxQ(e1), ")", sep, "(", .rxRepRxQ(e2), ")")
  }
}

divOp <- function() {
  function(e1, e2) {
    if (e1 == "d" && grepl(rex::rex(start, "__dt__"), e2)) {
      paste0("rx__d_dt_", gsub(rex::rex(start, "__dt__"), "", e2))
    } else if (grepl(rex::rex(start, "__df_", anything, "_", end), e1) &&
      grepl(rex::rex(start, "_dy_", anything, "__", end), e2)) {
      paste0("rx", substring(e1, 0, nchar(e1) - 1), e2)
    } else if (is(e1, "numeric")) {
      paste0("S(", e1, ")/", e2)
    } else {
      paste0(e1, "/", e2)
    }
  }
}

functionOp <- function(fn) {
  force(fn)
  function(...) {
    paste0(fn, "(", paste(unlist(list(...)), collapse = ", "), ")")
  }
}

functionOp2 <- function(fn, end) {
  force(fn)
  force(end)
  function(...) {
    paste0(fn, paste(unlist(list(...)), collapse = ", "), end)
  }
}

.dslStripParen <- function(x) {
  .stripIt <- function(x) {
    if (is.call(x)) {
      if (length(x) == 1) {
        return(x)
      } else if (identical(x[[1]], quote(`(`))) {
        return(.stripIt(x[[2]]))
      } else {
        return(x)
      }
    } else {
      return(x)
    }
  }
  ret <- eval(parse(text = sprintf(".stripIt(quote(%s))", x)))
  ret <- paste(deparse1(ret), collapse = " ")
  return(ret)
}

.dslToPow <- function(a, b) {
  a <- .dslStripParen(a)
  b <- .dslStripParen(b)
  num <- suppressWarnings({
    as.numeric(b)
  })
  if (is.na(num)) {
    return(sprintf("Rx_pow(%s, %s)", a, b))
  } else if (abs(num - round(num)) < .Machine$double.eps^0.5) {
    return(sprintf("Rx_pow_di(%s, %s)", a, b))
  } else if (abs(num - 0.5) < .Machine$double.eps^0.5) {
    return(sprintf("sqrt(%s)", a))
  } else {
    return(sprintf("Rx_pow(%s, %s)", a, b))
  }
}

## Add sympy->C mini DSL for omega parsing

symengineC <- new.env(parent = emptyenv())
symengineC[["**"]] <- .dslToPow
symengineC[["^"]] <- .dslToPow

symengineC[["S"]] <- function(x) {
  sprintf("%s", x)
}

for (f in c(
  "acos", "acosh", "asin", "atan", "atan2", "atanh", "beta",
  "cos", "cosh", "digamma", "erf", "erfc", "exp", "factorial",
  "gamma", "sin", "sinh", "sqrt", "tan",
  "tanh", "trigamma", "rxTBS", "rxTBSd"
)) {
  symengineC[[f]] <- functionOp(f)
}

symengineC[["("]] <- unaryOp("(", ")")

for (op in c("+", "-", "*")) {
  symengineC[[op]] <- binaryOp(paste0(" ", op, " "))
}

symengineC[["/"]] <- function(e1, e2) {
  sprintf("%s /( (%s == 0) ? %s : %s)", e1, e2, .Machine$double.eps, e2)
}


unknownCsymengine <- function(op) {
  force(op)
  function(...) {
    stop(sprintf(gettext("rxode2 doesn't support '%s' translation for 'omega' translation"), op), call. = FALSE)
  }
}

symengineCEnv <- function(expr) {
  ## Known functions
  calls <- allCalls(expr)
  callList <- setNames(lapply(calls, unknownCsymengine), calls)
  callEnv <- list2env(callList)
  rxSymPyFEnv <- cloneEnv(symengineC, callEnv)
  names <- allNames(expr)
  ## Replace time with t.
  n1 <- names
  n2 <- names
  n2 <- gsub(rex::rex("t", capture(numbers)), "REAL(theta)[\\1]", n2)
  n2 <- gsub(rex::rex("pi"), "M_PI", n2)
  n2 <- gsub(rex::rex("rx_SymPy_Res_"), "", n2)
  n2 <- gsub("None", "NA_REAL", n2)
  w <- n2[n2 == "t"]
  symbolList <- setNames(as.list(n2), n1)
  symbolEnv <- list2env(symbolList, parent = symengineC)
  return(symbolEnv)
}

seC <- function(x) {
  expr <- eval(parse(text = sprintf("quote(%s)", as.character(x))))
  .ret <- eval(expr, symengineCEnv(expr))
}

sympyTransit4 <- function(t, n, mtt, bio, podo = "podo", tlast = "tlast") {
  ktr <- paste0("((", n, " + 1)/(", mtt, "))")
  lktr <- paste0("(log((", n, ") + 1) - log(", mtt, "))")
  tc <- paste0("((", t, ")-(", tlast, "))")
  paste0(
    "exp(log((", bio, ") * (", podo, ")) + ", lktr, " + (",
    n, ") * ", "(", lktr, " + log(", t, ")) - ",
    ktr, " * (", t, ") - log(gamma(1 + (", n, "))))"
  )
}

allStrs <- function(x) {
  if (is.atomic(x)) {
    if (is.character(x)) {
      return(x)
    }
    return(character())
  } else if (is.name(x)) {
    return(as.character())
  } else if (is.call(x) || is.pairlist(x)) {
    children <- lapply(x[-1], allStrs)
    unique(unlist(children))
  } else {
    stop(sprintf(gettext("do not know how to handle type '%s'"), typeof(x)),
      call. = FALSE
    )
  }
}

allNames <- function(x) {
  if (is.atomic(x)) {
    character()
  } else if (is.name(x)) {
    as.character(x)
  } else if (is.call(x) || is.pairlist(x)) {
    children <- lapply(x[-1], allNames)
    unique(unlist(children))
  } else {
    stop(sprintf(gettext("do not know how to handle type '%s'"), typeof(x)),
      call. = FALSE
    )
  }
}

allCalls <- function(x) {
  if (is.atomic(x) || is.name(x)) {
    character()
  } else if (is.call(x)) {
    fname <- as.character(x[[1]])
    children <- lapply(x[-1], allCalls)
    unique(c(fname, unlist(children)))
  } else if (is.pairlist(x)) {
    unique(unlist(lapply(x[-1], allCalls), use.names = FALSE))
  } else {
    stop(sprintf(gettext("do not know how to handle type '%s'"), typeof(x)),
      call. = FALSE
    )
  }
}

cloneEnv <- function(env, parent = parent.env(env)) {
  list2env(as.list(env), parent = parent)
}

.exists2 <- function(x, where) {
  .nc <- try(nchar(x) < 1000, silent = TRUE)
  if (inherits(.nc, "try-error")) .nc <- FALSE
  if (rxIs(.nc, "logical")) .nc <- FALSE
  if (.nc) {
    return(exists(x, where))
  } else {
    return(FALSE)
  }
}


## Start error function DSL
sumProdEnv <- new.env(parent = emptyenv())

rxSumProdSum <- FALSE
rxSumProdProd <- FALSE


sumProdEnv[["^"]] <- binaryOp("^")
sumProdEnv[["**"]] <- binaryOp("^")

sumProdEnv[["*"]] <- function(a, b) {
  if (rxSumProdProd) {
    a <- .dslStripParen(a)
    b <- .dslStripParen(b)
    ## log transformed
    sprintf("prod(%s, %s)", sub(rex::rex(start, "prod(", capture(anything), ")", end), "\\1", a), b)
  } else {
    sprintf("%s * %s", a, b)
  }
}

sumProdEnv[["/"]] <- function(a, b) {
  if (rxSumProdProd) {
    a <- .dslStripParen(a)
    b <- .dslStripParen(b)
    sprintf("prod(%s, 1/%s)", sub(rex::rex(start, "prod(", capture(anything), ")", end), "\\1", a), b)
  } else {
    sprintf("%s / %s", a, b)
  }
}

sumProdEnv[["+"]] <- function(a, b) {
  if (rxSumProdSum) {
    a <- .dslStripParen(a)
    if (!missing(b)) {
      b <- .dslStripParen(b)
      return(sprintf("sum(%s, %s)", sub(rex::rex(start, "sum(", capture(anything), ")", end), "\\1", a), b))
    } else {
      return(a)
    }
  } else {
    if (!missing(b)) {
      b <- .dslStripParen(b)
      return(sprintf("%s + %s", a, b))
    } else {
      return(a)
    }
  }
}

sumProdEnv[["-"]] <- function(a, b) {
  if (rxSumProdSum) {
    a <- .dslStripParen(a)
    if (!missing(b)) {
      b <- .dslStripParen(b)
      sprintf(
        "sum(%s, -%s)",
        sub(
          rex::rex(start, "sum(", capture(anything), ")", end),
          "\\1", a
        ), b
      )
    } else {
      paste0("-", a)
    }
  } else {
    if (!missing(b)) {
      b <- .dslStripParen(b)
      return(sprintf("%s - %s", a, b))
    } else {
      return(paste0("-", a))
    }
  }
}

sumProdEnv[["("]] <- function(a) {
  return(sprintf("%s", a))
}

sumProdEnv[["["]] <- function(name, val) {
  n <- toupper(name)
  err <- gettext("rxode2 only supports THETA[#] and ETA[#] numbers")
  if (any(n == c("THETA", "ETA")) && is.numeric(val)) {
    if (round(val) == val && val > 0) {
      return(sprintf("%s[%s]", n, val))
    } else {
      stop(err, call. = FALSE)
    }
  } else {
    stop(err, call. = FALSE)
  }
}

sumProdRxEnv <- function(expr) {
  ## Known functions
  calls <- allCalls(expr)
  callList <- setNames(lapply(calls, functionOp), calls)
  callEnv <- list2env(callList)
  currEnv <- cloneEnv(sumProdEnv, callEnv)
  names <- allNames(expr)
  ## Replace time with t.
  n1 <- names
  n2 <- names
  symbolList <- setNames(as.list(n2), n1)
  symbolEnv <- list2env(symbolList, parent = currEnv)
  return(symbolEnv)
}

rxSumProd <- function(x) {
  return(eval(x, sumProdRxEnv(x)))
}
#' Recast model in terms of sum/prod
#'
#' @param model rxode2 model
#' @param expand Boolean indicating if the expression is expanded.
#' @param sum Use sum(...)
#' @param prod Use prod(...)
#' @return model string with prod(.) and sum(.) for all these
#'     operations.
#' @author Matthew L. Fidler
#' @export
rxSumProdModel <- function(model, expand = FALSE, sum = TRUE, prod = TRUE) {
  ## Sum for pairwise is equivalent to regular sum under 8 elements.
  rxReq("symengine")
  assignInMyNamespace("rxSumProdSum", sum)
  assignInMyNamespace("rxSumProdProd", prod)
  lines <- strsplit(rxNorm(model), "\n")[[1]]
  for (i in seq_along(lines)) {
    if (regexpr("[=~]", lines[i]) != -1) {
      type <- sub(".*([=~]).*", "\\1", lines[i])
      l0 <- strsplit(lines[i], "[=~]")[[1]]
      l2 <- substr(l0[2], 1, nchar(l0[2]) - 1)
      if (expand) {
        l2 <- rxToSE(l2)
        l2 <- symengine::S(l2)
        l2 <- rxFromSE(l2)
      }
      l0[2] <- eval(parse(text = sprintf("rxSumProd(quote(%s))", l2)))
      lines[i] <- paste0(paste0(l0[1], type, l0[2]))
    }
  }
  mod <- paste(lines, collapse = "\n")
  return(mod)
}

rm(f)
rm(op)
