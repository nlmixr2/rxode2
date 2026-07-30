#' Coerce a stored `modelName` to a single character string
#'
#' Guarantees the `$modelName` contract for values rxode2 did not create
#' itself (objects saved by older versions, or names assigned by downstream
#' packages).  A multi-element value keeps its first element, which is the
#' head of the call for the `as.character(<call>)` values that used to be
#' stored.
#'
#' @param x stored `modelName` value
#' @return single character string, or `NULL` when there is no usable name
#' @noRd
#' @author Bill Denney
.rxModelNameScalar <- function(x) {
  # a try-error is a character vector; without this its message would become
  # the model name
  if (is.null(x) || inherits(x, "try-error")) return(NULL)
  .ret <- try(as.character(x), silent=TRUE)
  if (inherits(.ret, "try-error")) return(NULL)
  .ret <- .ret[!is.na(.ret) & nzchar(.ret)]
  if (length(.ret) == 0L) return(NULL)
  .ret[[1L]]
}

#' Widest model name kept before it is truncated
#' @noRd
.rxModelNameMaxWidth <- 60L

#' Tidy the first deparsed line of an expression into a name
#'
#' `deparse()` breaks a multi-line construct after its opening brace, so the
#' first line of a function definition is `function() {`; dropping the brace
#' leaves `function()`.  Anything still wider than `.rxModelNameMaxWidth` is
#' truncated.
#'
#' @param x deparsed expression
#' @return single character string
#' @noRd
#' @author Bill Denney
.rxModelNameTrim <- function(x) {
  .ret <- trimws(sub("\\{[[:space:]]*$", "", x[1L]))
  if (!is.na(.ret) && nchar(.ret) > .rxModelNameMaxWidth) {
    .ret <- paste0(substr(.ret, 1L, .rxModelNameMaxWidth - 3L), "...")
  }
  .ret
}

#' Drop the `(` wrappers around an expression
#'
#' Calling an anonymous function requires them (`(function() {})()`), and they
#' are not part of any name.
#'
#' @param expr expression to unwrap
#' @return `expr` without its outer `(` calls
#' @noRd
#' @author Bill Denney
.rxModelNameUnwrapParens <- function(expr) {
  while (is.call(expr) && length(expr) == 2L &&
           identical(expr[[1L]], quote(`(`))) {
    expr <- expr[[2L]]
  }
  expr
}

#' Convert an expression naming a model into a single model name
#'
#' `as.character()` on a call returns one element per call component, so
#' `rxode2(readModelDb("PK_1cmt"))` used to give a length two `modelName`.  The
#' name is the tidied first deparsed line of the expression instead: a symbol
#' keeps its name, a call becomes its own text, and an anonymous function --
#' whose deparse breaks after the opening brace -- becomes `function()` rather
#' than its body.
#'
#' @param expr expression naming the model, usually from `substitute()` or
#'   `match.call()[[1]]`
#' @return single character string naming the model, or `NULL` when there is no
#'   expression to name (a missing argument)
#' @noRd
#' @author Bill Denney
.rxModelNameFromExpr <- function(expr) {
  if (missing(expr)) return(NULL)
  # A call is never the empty symbol, so it is safe to hand to another closure;
  # `substitute()` of a missing argument is, and passing that on would raise
  # "argument is missing" -- it stays in this frame and its name is "", which
  # .rxModelNameScalar() turns into NULL.
  if (is.call(expr)) expr <- .rxModelNameUnwrapParens(expr)
  if (is.symbol(expr) || is.character(expr)) {
    # same answer as deparsing, and the only form that reaches the empty symbol
    return(.rxModelNameScalar(try(as.character(expr), silent=TRUE)))
  }
  # nlines keeps this cheap no matter how large the deparsed object would be
  .ret <- try(deparse(expr, width.cutoff=500L, nlines=1L), silent=TRUE)
  if (inherits(.ret, "try-error")) return(NULL)
  .rxModelNameScalar(.rxModelNameTrim(.ret))
}

#' @export
#' @rdname rxUiGet
rxUiGet.modelName <- function(x, ...) {
  .ui <- x[[1]]
  if (exists("modelName", envir=.ui)) {
    return(.rxModelNameScalar(get("modelName", envir=.ui)))
  }
  if (!exists("meta", envir=.ui)) return(NULL)
  .meta <- get("meta", envir=.ui)
  if (!is.environment(.meta) || !exists("modelName", envir=.meta)) return(NULL)
  .rxModelNameScalar(get("modelName", envir=.meta))
}
attr(rxUiGet.modelName, "desc") <- "Name of the model"
attr(rxUiGet.modelName, "rstudio") <- NA # passthrough
