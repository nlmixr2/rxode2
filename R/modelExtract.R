#' Extract model lines from a rxui model
#'
#' @param x model to extract lines from
#'
#' @param ... variables to extract. When it is missing, it will
#'   extract the entire model (conditioned on the endpoint option
#'   below)
#'
#' @param expression return expressions (if `TRUE`) or strings (if
#'   `FALSE`)
#'
#' @param endpoint include endpoint.  This can be:
#'
#'  - `NA`    -- Missing means include both the endpoint and non-endpoint lines
#'
#'  - `TRUE`  -- Only include endpoint lines
#'
#'  - `FALSE` -- Only include non-endpoint lines
#'
#' @param lines is a boolean.  When `TRUE` this will add the lines as
#'   an attribute to the output value ie `attr(, "lines")`
#'
#' @param envir Environment for evaluating variables
#'
#' @return expressions or strings of extracted lines. Note if there is
#'   a duplicated lhs expression in the line, it will return both
#'   lines
#'
#' @export
#'
#' @author Matthew L. Fidler
#'
#' @examples
#'
#' one.compartment <- function() {
#'   ini({
#'     tka <- 0.45 # Log Ka
#'     tcl <- 1 # Log Cl
#'     tv <- 3.45    # Log V
#'     eta.ka ~ 0.6
#'     eta.cl ~ 0.3
#'     eta.v ~ 0.1
#'     add.sd <- 0.7
#'   })
#'   model({
#'     ka <- exp(tka + eta.ka)
#'     cl <- exp(tcl + eta.cl)
#'     v  <- exp(tv + eta.v)
#'     d/dt(depot)  <- -ka * depot
#'     d/dt(center) <-  ka * depot - cl / v * center
#'     cp <- center / v
#'     cp ~ add(add.sd)
#'   })
#'  }
#'
#'  f <- one.compartment()
#'
#'  modelExtract(f, cp)
#'
#'  modelExtract(one.compartment, d/dt(depot))
#'
#'  # from variable
#'  var <- "d/dt(depot)"
#'
#'  modelExtract(one.compartment, var)
#'
#'  modelExtract(f, endpoint=NA, lines=TRUE, expression=TRUE)
#'
modelExtract <- function(x, ..., expression=FALSE, endpoint=FALSE, lines=FALSE, envir=parent.frame()) {
  checkmate::assertLogical(expression, any.missing=FALSE, len=1)
  checkmate::assertLogical(lines, any.missing=FALSE, len=1)
  checkmate::assertLogical(endpoint, any.missing=TRUE, len=1)
  UseMethod("modelExtract")
}
#' Common extract model lines
#'
#' @param modelLines Model lines, in this case it is the variables
#' @param rxui rxode2 parsed ui
#' @param expression Should an expression list be returned
#' @param endpoint Should this be an endpoint (yes: TRUE, no: FALSE, both: TRUE)
#' @noRd
#' @author Matthew L. Fidler
.modelExtractCommon <- function(modelLines, rxui, expression=FALSE, endpoint=FALSE, lines=FALSE) {
  .lstExpr <- rxui$lstExpr
  .isNull <- length(modelLines) == 0L ||
    all(vapply(seq_along(modelLines),
               function(i) {
                 is.null(modelLines[[i]])
               }, logical(1)))
  if (.isNull) {
    .ret <- seq_along(.lstExpr)
  } else {
    .ret <- do.call(`c`, lapply(seq_along(modelLines),
                                function(i) {
                                  .w <- .getModelLineFromExpression(modelLines[[i]],
                                                                    rxui, errorLine=FALSE,
                                                                    returnAllLines=TRUE)
                                  .w <- .w[.w>0]
                                  .w
                                }))

  }
  .ret <- sort(unique(.ret))
  .endPointLines <- rxui$predDf
  if (!is.null(.endPointLines)) {
    .endPointLines <- .endPointLines$line
    if (is.na(endpoint)) {
      # do both
    } else if (endpoint) {
      .ret <- .ret[.in(.ret, .endPointLines)]
    } else {
      .ret <- .ret[!.in(.ret, .endPointLines)]
    }
  }
  .lines <- .ret
  .ret <- lapply(.ret,
                 function(i) {
                   .lstExpr[[i]]
                 })
  if (!expression) {
    .ret <- vapply(seq_along(.ret),
                   function(i) {
                     deparse1(.ret[[i]])
                   }, character(1))
  }
  if (lines) {
    attr(.ret, "lines") <- .lines
  }
  .ret
}
#' Quote call info for extraction variables
#'
#' @param callInfo Call information
#' @return list of expressions
#' @noRd
#' @author Matthew L. Fidler
.quoteCallVars <- function(callInfo, ..., envir=parent.frame()) {
  if (length(callInfo) == 0L) return(NULL)
  .env <- new.env(parent=emptyenv())
  .env$alag <- list()
  .env$lag <- list()
  c(lapply(seq_along(callInfo),
         function(i) {
           .name <- names(callInfo)[i]
           .cur <- callInfo[[i]]
           if (is.name(.cur)) {
             .curChar <- as.character(.cur)
             if (exists(.curChar, envir=envir)) {
               .cur <- get(.curChar, envir=envir)
             }
           }
           if (is.list(.cur)) {
             .tmp <- eval(.cur)
             .tmp <- as.vector(.cur)
             .tmp <- setNames(unlist(.tmp), NULL)
             .cur <- .tmp
           }
           if (is.call(.cur) &&
                 identical(.cur[[1]], quote(`$`))) {
             .list <- list(...)
             .tmp <- .list[[i]]
             .cur <- .tmp
           }
           if (inherits(.cur, "character")) {
             .cur <- str2lang(.cur)
           }
           if (is.null(.name)) {
           } else if (.in(.name,
                          c("expression",  "endpoint", "envir", "lines"))) {
             return(NULL)
           }
           if (is.name(.cur)) {
             return(str2lang(paste0("-",deparse1(.cur))))
           } else if (is.call(.cur) &&
                        (.matchesLangTemplate(.cur, str2lang("d/dt(.name)")) ||
                           .matchesLangTemplate(.cur, str2lang("f(.name)")) ||
                           .matchesLangTemplate(.cur, str2lang(".name(0)")) ||
                           .matchesLangTemplate(.cur, str2lang("rate(.name)")) ||
                           .matchesLangTemplate(.cur, str2lang("dur(.name)")))) {
             return(str2lang(paste0("-", deparse1(.cur))))
           } else if (is.call(.cur) &&
                        .matchesLangTemplate(.cur, str2lang("alag(.name)"))) {
             .env$lag <- c(.env$lag,
                           list(str2lang(paste0("-", sub("alag", "lag", deparse1(.cur))))))
              return(str2lang(paste0("-", deparse1(.cur))))
           } else if (is.call(.cur) &&
                        .matchesLangTemplate(.cur, str2lang("lag(.name)"))) {
             .env$alag <- c(.env$alag,
                            list(str2lang(paste0("-", sub("lag", "alag", deparse1(.cur))))))
             return(str2lang(paste0("-", deparse1(.cur))))
           }
           stop("unknown variable expression: ", deparse1(.cur),
                call.=FALSE)
         }),
    .env$alag,
    .env$lag)
}

#' @export
#' @rdname modelExtract
modelExtract.function <- function(x, ..., expression=FALSE, endpoint=FALSE, lines=FALSE, envir=parent.frame()) {
  .modelLines <- .quoteCallVars(match.call(expand.dots = TRUE)[-(1:2)], ..., envir=envir)
  .ret <- rxode2(x)
  .modelExtractCommon(.modelLines, .ret, expression=expression, endpoint=endpoint, lines=lines)
}
#' @export
#' @rdname modelExtract
modelExtract.rxUi <- function(x, ..., expression=FALSE, endpoint=FALSE, lines=FALSE, envir=parent.frame()) {
  .modelLines <- .quoteCallVars(match.call(expand.dots = TRUE)[-(1:2)], ..., envir=envir)
  .modelExtractCommon(.modelLines, x, expression=expression, endpoint=endpoint, lines=lines)
}
#' @export
#' @rdname modelExtract
modelExtract.rxode2 <- function(x, ..., expression=FALSE, endpoint=FALSE, lines=FALSE, envir=parent.frame()) {
  .modelLines <- .quoteCallVars(match.call(expand.dots = TRUE)[-(1:2)], ..., envir=envir)
  x <- as.function(x)
  .ret <- rxode2(x)
  .modelExtractCommon(.modelLines, .ret, expression=expression, endpoint=endpoint, lines=lines)
}
#' @export
#' @rdname modelExtract
modelExtract.rxModelVars <- modelExtract.rxode2

#' @export
#' @rdname modelExtract
modelExtract.default <- function(x, ..., expression=FALSE, endpoint=FALSE, lines=FALSE, envir=parent.frame()) {
  stop("rxode2 does not know how to handle this modelExtract object",
       call.=FALSE)
}
