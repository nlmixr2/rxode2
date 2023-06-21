#' Extract model lines from a rxui model
#'
#' @param x model to extract lines from
#' @param ... 
#' @param expression
#' @param endpoint 
#' @return 
#' @export 
#' @author Matthew L. Fidler
#' @examples 
modelExtract <- function(x, ..., expression=FALSE, endpoint=FALSE, envir=parent.frame()) {
  UseMethod("modelExtract")
}

.modelExtractCommon <- function(modelLines, rxui, expression=FALSE, endpoint=FALSE,
                                returnAllLines=TRUE) {
  .ret <- do.call(`c`, lapply(seq_along(modelLines),
                 function(i) {
                   .w <- .getModelLineFromExpression(modelLines[[i]], rxui, errorLine=endpoint,
                                                     returnAllLines=returnAllLines)
                   .w <- .w[.w>0]
                   .w
                 }))
  .ret <- sort(unique(.ret))
  .lstExpr <- rxui$lstExpr
  .ret <- lapply(.ret,
                 function(i) {
                   .lstExpr[[i]]
                 })
  if (expression) {
    .ret <- vapply(seq_along(.ret),
                   function(i) {
                     deparse1(.ret[[i]])
                   }, )
  }
  .ret
}
#' Quote call info for extraction variables  
#'  
#' @param callInfo Call information
#' @return list of expressions
#' @export 
#' @author Matthew L. Fidler
#' @examples 
.quoteCallVars <- function(callInfo) {
  lapply(seq_along(callInfo),
         function(i) {
           .name <- names(callInfo)[i]
           .cur <- callInfo[[i]]
           if (inherits(.cur, "character")) {
             .cur <- str2lang(.cur)
           }
           if (is.null(.name)) {
           } else if (.name %in% c("expression",  "endpoint", "envir")) {
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
modelExtract.function <- function(x, ..., expression=FALSE, endpoint=FALSE, envir=parent.frame()) {
  .modelLines <- .quoteCallVars(match.call(expand.dots = TRUE)[-(1:2)])
  .ret <- rxode2(x)
  .modelExtractCommon(.modelLines, .ret, expression=expression, endpoint=endpoint)
}
#' @export
#' @rdname model
modelExtract.rxUi <- function(x, ..., expression=FALSE, endpoint=FALSE, envir=parent.frame()) {
  .modelLines <- .quoteCallVars(match.call(expand.dots = TRUE)[-(1:2)])
  .modelExtractCommon(.modelLines, x, expression=expression, endpoint=endpoint)
}
#' @export
#' @rdname model
modelExtract.rxode2 <- function(x, ..., expression=FALSE, endpoint=FALSE, envir=parent.frame()) {
  .modelLines <- .quoteCallVars(match.call(expand.dots = TRUE)[-(1:2)])
  x <- as.function(x)
  .ret <- rxode2(x)
  .modelExtractCommon(.modelLines, .ret, expression=expression, endpoint=endpoint)
}
#' @export
#' @rdname model
modelExtract.rxModelVars <- modelExtract.rxode2

modelExtract.default <- function(x, ..., expression=FALSE, endpoint=FALSE, envir=parent.frame()) {
  stop("rxode2 does not know how to handle this modelExtract object",
       call.=FALSE)
}
