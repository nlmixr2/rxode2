#' Extract model lines from a rxui model
#'
#' @param x model to extract lines from
#'
#' @param ... variables to extract
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
#' @return expressions or strings of extracted lines
#'
#' @export
#' 
#' @author Matthew L. Fidler
#' 
#' @examples
#'
#' one.compartment <- function() {
#'    ini({
#'      tka <- 0.45 # Log Ka
#'      tcl <- 1 # Log Cl
#'      tv <- 3.45    # Log V
#'      eta.ka ~ 0.6
#'      eta.cl ~ 0.3
#'      eta.v ~ 0.1
#'      add.sd <- 0.7
#'    })
#'    model({
#'      ka <- exp(tka + eta.ka)
#'      cl <- exp(tcl + eta.cl)
#'      v <- exp(tv + eta.v)
#'      d/dt(depot) = -ka * depot
#'      d/dt(center) = ka * depot - cl / v * center
#'      cp = center / v
#'      cp ~ add(add.sd)
#'    })
#'  }
#'
#'  f <- one.compartment()
#'
#'  modelExtract(f, cp)
#'
#'  modelExtract(one.compartment, d/dt(depot))
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
  .ret <- do.call(`c`, lapply(seq_along(modelLines),
                 function(i) {
                   .w <- .getModelLineFromExpression(modelLines[[i]], rxui, errorLine=FALSE,
                                                     returnAllLines=TRUE)
                   .w <- .w[.w>0]
                   .w
                 }))
  .ret <- sort(unique(.ret))
  .endPointLines <- rxui$predDf
  if (!is.null(.endPointLines)) {
    .endPointLines <- .endPointLines$line
    if (is.na(endpoint)) {
      # do both
    } else if (endpoint) {
      .ret <- .ret[.ret %in% .endPointLines]
    } else {
      .ret <- .ret[!(.ret %in% .endPointLines)]
    }
  }
  .lines <- .ret
  .lstExpr <- rxui$lstExpr
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
.quoteCallVars <- function(callInfo) {
  lapply(seq_along(callInfo),
         function(i) {
           .name <- names(callInfo)[i]
           .cur <- callInfo[[i]]
           if (inherits(.cur, "character")) {
             .cur <- str2lang(.cur)
           }
           if (is.null(.name)) {
           } else if (.name %in% c("expression",  "endpoint", "envir", "lines")) {
             return(NULL)
           }
           if (is.name(.cur)) {
             return(str2lang(paste0("-",deparse1(.cur))))
           } else if (is.call(.cur) &&
                        .matchesLangTemplate(.cur, str2lang("d/dt(.name)"))) {
             return(str2lang(paste0("-", deparse1(.cur))))
           }
           stop("unknown variable expression: ", deparse1(.cur),
                call.=FALSE)
         })
}

#' @export
#' @rdname modelExtract
modelExtract.function <- function(x, ..., expression=FALSE, endpoint=FALSE, lines=FALSE, envir=parent.frame()) {
  .modelLines <- .quoteCallVars(match.call(expand.dots = TRUE)[-(1:2)])
  .ret <- rxode2(x)
  .modelExtractCommon(.modelLines, .ret, expression=expression, endpoint=endpoint, lines=lines)
}
#' @export
#' @rdname model
modelExtract.rxUi <- function(x, ..., expression=FALSE, endpoint=FALSE, lines=FALSE, envir=parent.frame()) {
  .modelLines <- .quoteCallVars(match.call(expand.dots = TRUE)[-(1:2)])
  .modelExtractCommon(.modelLines, x, expression=expression, endpoint=endpoint, lines=lines)
}
#' @export
#' @rdname model
modelExtract.rxode2 <- function(x, ..., expression=FALSE, endpoint=FALSE, lines=FALSE, envir=parent.frame()) {
  .modelLines <- .quoteCallVars(match.call(expand.dots = TRUE)[-(1:2)])
  x <- as.function(x)
  .ret <- rxode2(x)
  .modelExtractCommon(.modelLines, .ret, expression=expression, endpoint=endpoint, lines=lines)
}
#' @export
#' @rdname model
modelExtract.rxModelVars <- modelExtract.rxode2

modelExtract.default <- function(x, ..., expression=FALSE, endpoint=FALSE, lines=FALSE, envir=parent.frame()) {
  stop("rxode2 does not know how to handle this modelExtract object",
       call.=FALSE)
}
