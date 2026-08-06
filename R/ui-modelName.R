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

#' Is this expression an anonymous model function?
#'
#' A function definition and a function value both name nothing.
#'
#' @param expr expression to test
#' @return logical
#' @noRd
#' @author Matthew L. Fidler
.rxModelNameIsFunctionExpr <- function(expr) {
  is.function(expr) ||
    (is.call(expr) && identical(expr[[1L]], quote(`function`)))
}

#' Environment holding the name supplied by an assignment operator
#' @noRd
.rxModelNameEnv <- new.env(parent=emptyenv())
.rxModelNameEnv$lhs <- NULL

#' Name a model from the left hand side of an assignment
#'
#' Registers the name an assignment operator like `nlmixr2save`'s `:=` is
#' assigning to, which rxode2 uses when the model expression itself names
#' nothing (an anonymous model function) and when the expression is a call
#' without a `rxModelName()` method.  It is not consumed when it is used, so
#' one assignment can name every model it builds; the operator that set it
#' clears it with `rxModelNameLhs(NULL)`, which is what keeps the name from
#' leaking into an unrelated later model.
#'
#' @param value when specified, a single non-empty character naming the model,
#'   or `NULL` to clear the name.  When missing the current name is returned.
#' @return the registered name, or `NULL` when there is none
#' @family Model names
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' # an assignment operator registers the name it is assigning to before it
#' # forces the model, and clears it afterward:
#' #
#' # `:=` <- function(x, value) {
#' #   rxode2::rxModelNameLhs(as.character(substitute(x)))
#' #   on.exit(rxode2::rxModelNameLhs(NULL))
#' #   assign(as.character(substitute(x)), force(value), envir=parent.frame())
#' # }
#'
#' rxModelNameLhs()
#'
#' rxModelNameLhs("mod")
#'
#' rxModelNameLhs()
#'
#' rxModelNameLhs(NULL)
#'
rxModelNameLhs <- function(value) {
  if (missing(value)) {
    return(.rxModelNameEnv$lhs)
  }
  if (is.null(value)) {
    .rxModelNameEnv$lhs <- NULL
    return(invisible(NULL))
  }
  if (!checkmate::testCharacter(value, len=1L, any.missing=FALSE, min.chars=1L)) {
    stop("'rxModelNameLhs()' must be called with a single non-empty character, NULL, or without any arguments",
         call.=FALSE)
  }
  .rxModelNameEnv$lhs <- value
  invisible(value)
}

#' Name a model from the function that created it
#'
#' When a model comes from a call, like
#' `rxode2(nlmixr2lib::readModelDb("PK_1cmt"))`, the text of the call is a poor
#' model name; the function that produced the model knows a better one.  This
#' is dispatched on the name of the called function, so a
#' `rxModelName.readModelDb()` method names every model `readModelDb()`
#' produces.  Without a method the call is named by its own (deparsed) text.
#'
#' A method is given the call as `x` and the call's arguments in `...`, matched
#' to the argument names of the function being called whenever that function
#' can be found.  The arguments are unevaluated: one a method does not look at
#' is never evaluated, and one it does look at is evaluated again, in the frame
#' the call came from, so a method should only read arguments that are cheap to
#' evaluate.  A method that cannot name the model should return `NULL`;
#' anything that is not a single non-empty string is ignored the same way.
#'
#' @param x the call that creates the model.  Its class is the name of the
#'   called function (without any `pkg::` qualifier) followed by
#'   `"rxModelNameCall"`.
#' @param ... the arguments of that call, unevaluated.
#' @return single character string naming the model, or `NULL` when the model
#'   cannot be named this way
#' @family Model names
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' # a function that creates models from a database of them:
#' readMyModelDb <- function(name) {
#'   # ...look `name` up and return the model function...
#'   function() {
#'     ini({a <- 1})
#'     model({b <- a})
#'   }
#' }
#'
#' # names the model for the database entry it came from
#' rxModelName.readMyModelDb <- function(x, ...) {
#'   list(...)$name
#' }
#'
#' registerS3method("rxModelName", "readMyModelDb", rxModelName.readMyModelDb)
#'
#' rxode2(readMyModelDb("one.cmt"))$modelName
#'
rxModelName <- function(x, ...) {
  UseMethod("rxModelName")
}

#' @rdname rxModelName
#' @export
rxModelName.default <- function(x, ...) {
  # `...` is never forced here; the name comes from the call itself
  .ret <- try(deparse(x, width.cutoff=500L, nlines=1L), silent=TRUE)
  if (inherits(.ret, "try-error")) return(NULL)
  .rxModelNameScalar(.rxModelNameTrim(.ret))
}

#' Name a call with its `rxModelName()` method, if it has one
#'
#' The method is called with the call itself (classed by the called function's
#' name) and the call's arguments, which stay unevaluated until the method
#' looks at them.  The call is shielded in `quote()`; handing it over bare
#' would make the method's `x` re-run the very function that created the model.
#'
#' @param expr the call creating the model
#' @param envir environment the call came from, where its arguments are
#'   evaluated
#' @return single character string, or `NULL` when there is no method for this
#'   function or it did not name the model
#' @noRd
#' @author Matthew L. Fidler
.rxModelNameDispatch <- function(expr, envir) {
  .head <- expr[[1L]]
  .cls <- try(sub("^.*:::?", "", deparse1(.head)), silent=TRUE)
  if (inherits(.cls, "try-error") ||
        !checkmate::testCharacter(.cls, len=1L, any.missing=FALSE, min.chars=1L)) {
    return(NULL)
  }
  .method <- try(utils::getS3method("rxModelName", .cls, optional=TRUE), silent=TRUE)
  if (inherits(.method, "try-error") || is.null(.method)) return(NULL)
  .args <- as.list(expr)[-1L]
  # name-match the arguments when the function itself can be found, so a method
  # can read them by name however the user wrote the call
  .fun <- try(eval(.head, envir=envir), silent=TRUE)
  if (is.function(.fun) && !is.primitive(.fun)) {
    .matched <- try(match.call(.fun, expr), silent=TRUE)
    if (!inherits(.matched, "try-error")) .args <- as.list(.matched)[-1L]
  }
  .x <- expr
  class(.x) <- c(.cls, "rxModelNameCall")
  # the generic itself, not `rxode2::rxModelName`, so this follows the loaded
  # rxode2 rather than an installed one
  .call <- as.call(c(list(rxModelName, bquote(quote(.(.x)))), .args))
  .ret <- try(eval(.call, envir=envir), silent=TRUE)
  if (inherits(.ret, "try-error") ||
        !checkmate::testCharacter(.ret, len=1L, any.missing=FALSE, min.chars=1L)) {
    return(NULL)
  }
  .ret
}

#' Convert an expression naming a model into a single model name
#'
#' `as.character()` on a call returns one element per call component, so
#' `rxode2(readModelDb("PK_1cmt"))` used to give a length two `modelName`.  A
#' symbol keeps its name; a call is named by its `rxModelName()` method, and
#' without one by the name being assigned to (see [rxModelNameLhs()]) or
#' failing that its own text; an anonymous model function has no name of its
#' own, so it is named by the name being assigned to or not at all.
#'
#' @param expr expression naming the model, usually from `substitute()` or
#'   `match.call()[[1]]`
#' @param envir environment the expression came from, used to dispatch and
#'   evaluate its arguments
#' @return single character string naming the model, or `NULL` when it cannot
#'   be named
#' @noRd
#' @author Bill Denney and Matthew L. Fidler
.rxModelNameFromExpr <- function(expr, envir=parent.frame()) {
  if (missing(expr)) return(rxModelNameLhs())
  # A call is never the empty symbol, so it is safe to hand to another closure;
  # `substitute()` of a missing argument is, and passing that on would raise
  # "argument is missing" -- it stays in this frame and its name is "", which
  # .rxModelNameScalar() turns into NULL.
  if (is.call(expr)) expr <- .rxModelNameUnwrapParens(expr)
  if (is.symbol(expr) || is.character(expr)) {
    # same answer as deparsing, and the only form that reaches the empty symbol
    .ret <- .rxModelNameScalar(try(as.character(expr), silent=TRUE))
    if (!is.null(.ret)) return(.ret)
    return(rxModelNameLhs())
  }
  # an anonymous model function names nothing, whether it is a definition or
  # the function value itself
  if (.rxModelNameIsFunctionExpr(expr)) return(rxModelNameLhs())
  if (is.call(expr)) {
    .ret <- .rxModelNameDispatch(expr, envir)
    if (!is.null(.ret)) return(.ret)
    .lhs <- rxModelNameLhs()
    if (!is.null(.lhs)) return(.lhs)
  }
  # nlines keeps this cheap no matter how large the deparsed object would be
  .ret <- try(deparse(expr, width.cutoff=500L, nlines=1L), silent=TRUE)
  if (inherits(.ret, "try-error")) return(NULL)
  .rxModelNameScalar(.rxModelNameTrim(.ret))
}

#' Name a model from the expression that produced it
#'
#' The naming rxode2 itself uses, exported so a package that captures a model
#' expression with `substitute()` names models the same way rxode2 does.  A
#' symbol keeps its name, a call is named by its [rxModelName()] method, and an
#' expression that names nothing falls back to [rxModelNameLhs()].
#'
#' @param expr expression naming the model, usually from `substitute()`
#' @param envir environment the expression came from, used to dispatch and
#'   evaluate its arguments
#' @return single character string naming the model, or `NULL` when it cannot
#'   be named
#' @family Model names
#' @export
#' @keywords internal
#' @author Matthew L. Fidler
#' @examples
#'
#' rxModelNameFromExpr(quote(one.cmt))
#'
#' rxModelNameFromExpr(quote(readModelDb("PK_1cmt")))
#'
rxModelNameFromExpr <- function(expr, envir=parent.frame()) {
  if (missing(expr)) return(rxModelNameLhs())
  .rxModelNameFromExpr(expr, envir=envir)
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
