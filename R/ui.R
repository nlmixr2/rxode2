#' Replace comments with label()
#'
#' @param src source function
#' @return character vector with labels("") replaced in function
#' @author Matthew Fidler
#' @noRd
.rxReplaceCommentWithLabel <- function(src) {
  .env <- new.env(parent=emptyenv())
  .env$inIni <- FALSE
  .env$convertLabel <- FALSE
  .regIni <- rex::rex(boundary, or("ini", "lotri"), "(", any_spaces, "{")
  .regOther <- rex::rex(boundary, or(group("}", any_spaces, ")")), group(or("model"), "(", any_spaces, "{"))
  .regCommentOnBlankLine <- "^ *#+ *(.*) *$"
  .regLabel <- "^( *[^\n\"]+) *#+ *(.*) *$"
  .ret <- vapply(src,
                 function(line) {
                   if (regexpr(.regIni, line, perl=TRUE) != -1) {
                     .env$inIni <- TRUE
                   } else if (regexpr(.regOther, line, perl=TRUE) != -1) {
                     .env$inIni <- FALSE
                   } else if (.env$inIni) {
                     if (regexpr(.regCommentOnBlankLine, line) != -1) {
                     } else if (regexpr(.regLabel, line) != -1) {
                       .env$convertLabel <- TRUE
                       .label <- deparse1(sub(.regLabel, "\\2", line))
                       return(sub(.regLabel, paste0("\\1; label(", .label, ")"), line))
                     }
                   }
                   line
                 }, character(1), USE.NAMES = FALSE)
  if (.env$convertLabel) {
    cli::cli_alert_info("parameter labels from comments will be replaced by 'label()'")
  }
  .ret <- deparse(eval(parse(text=paste(.ret, collapse="\n"), keep.source=FALSE)))
  .ret
}

#' Convert rxode2/nlmixr model function to a string
#'
#' @param fun function name for parsing
#' @return Modified function
#' @author Matthew Fidler
#' @noRd
.rxFunction2string <- function(fun) {
  .srcRef <- attr(fun, "srcref")
  if (is.null(.srcRef)) {
    cli::cli_alert_info("parameter labels from comments are typically ignored in non-interactive mode")
    cli::cli_alert_info("Need to run with the source intact to parse comments")
    .ret <- deparse(fun)
  } else {
    .ret <- .rxReplaceCommentWithLabel(as.character(.srcRef, useSource = TRUE))
  }
  .ret
}

.rxFunction2ui <- function(fun) {
  .fun <- eval(parse(text=paste(.rxFunction2string(fun), collapse="\n")))
  .ret <- .fun()
  # Save $model like nlmixr UI used to...
  assign("model", fun, envir=.ret)
  .ret
}

.lastIni <- NULL
.lastIniQ <- NULL
#' Ini block for rxode2/nlmixr models
#'
#' @param x expression
#' @param ... Other expressions for `ini()` function
#' @param envir the `environment` in which unevaluated model
#'   expressions is to be evaluated.  May also be `NULL`, a list, a
#'   data frame, a pairlist or an integer as specified to `sys.call`.
#' @return Ini block
#' @author Matthew Fidler
#' @export
ini <- function(x, ..., envir = parent.frame()) {
  if (is(substitute(x), "{")) {
    .ini <- eval(bquote(lotri(.(substitute(x)))), envir=envir)
    assignInMyNamespace(".lastIni", .ini)
    assignInMyNamespace(".lastIniQ", bquote(.(substitute(x))))
    return(invisible(.ini))
  }
  UseMethod("ini")
}

#' @export
#' @rdname ini
ini.default <- function(x, ...) {

}

#' Model block for rxode2/nlmixr models
#'
#' @param x model expression
#' @param ... Other arguments
#' @param envir the `environment` in which unevaluated model
#'   expressions is to be evaluated.  May also be `NULL`, a list, a
#'   data frame, a pairlist or an integer as specified to `sys.call`.
#' @return Model block with ini information included.  `ini` must be
#'   called before `model` block
#' @author Matthew Fidler
#' @export
model <- function(x, ..., envir=parent.frame()) {
  if (is(substitute(x), "{")) {
    .ini <- .lastIni
    .iniQ <- .lastIniQ
    if (is.null(.ini)) {
      stop("ini({}) block must be called before the model block",
           call.=FALSE)
    }
    assignInMyNamespace(".lastIni", NULL)
    assignInMyNamespace(".lastIniQ", NULL)
    .mod <- .rxMuRef(eval(bquote(.errProcessExpression(quote(.(substitute(x))), .ini))))
    .meta <- new.env(parent=emptyenv())
    if (!identical(envir, globalenv())) {
      for (.i in ls(envir, all=TRUE)) {
        assign(.i, get(.i, envir), .meta)
      }
    }
    .mod$meta <- .meta
    .w <- which(!is.na(.mod$iniDf$err) & !is.na(.mod$iniDf$neta1))
    if (length(.w) > 0) {
      stop("the parameter(s) '", paste(.mod$iniDf$name[.w], collapse="', '"), "' cannot be an error and between subject variability",
           call.=FALSE)
    }
    class(.mod) <- "rxUi"
    return(.mod)
  }
  UseMethod("model")
}

#' @export
#' @rdname model
model.default <- function(x, ...) {

}

#' @export
print.rxUi <-function(x, ...) {
  .md <- x$modelDesc
  cat(cli::cli_format_method({
        cli::cli_h1("{.md}")
    }), "\n")
  cat(cli::cli_format_method({
    cli::cli_h2("Initalization:")
  }), "\n")
  cat(paste0(crayon::bold("Fixed Effects"), " (", crayon::bold$blue("$theta"), "):"), "\n")
  print(x$theta)
  .omega <- x$omega
  if (dim(.omega)[1] > 0) {
    cat(paste0("\n", crayon::bold("Omega"), " (", crayon::bold$blue("$omega"), "):"), "\n")
    print(.omega)
  }

  # Multiple Endpoint
  .me <- x$multipleEndpoint
  .hasHux <- requireNamespace("huxtable", quietly = TRUE)
  if (!is.null(.me)) {
    .met <- crayon::bold("Multiple Endpoint Model")
    .med <- crayon::bold$blue("$multipleEndpoint")
    cat(cli::cli_format_method({
      cli::cli_h2("{.met} ({.med}):")
    }), "\n")
    if (.hasHux) {
      .me %>%
        huxtable::print_screen(colnames = FALSE)
    } else {
      print(.mu)
    }
    cat("\n")
  }

  # muRefTable
  .mu <- x$muRefTable
  if (!is.null(.mu)) {
    .muU <- crayon::bold(paste0(ifelse(use.utf(), "\u03bc", "mu"), "-referencing"))
    .muR <- crayon::bold$blue("$muRefTable")
    cat(cli::cli_format_method({
      cli::cli_h2("{.muU} ({.muR}):")
    }), "\n")
    if (.hasHux) {
      .mu %>%
        huxtable::print_screen(colnames = FALSE)
    } else {
      print(.mu)
    }
    cat("\n")
  }
  cat(cli::cli_format_method({
    cli::cli_h2("Model (Normalized Syntax):")
  }))
  cat("\nfunction() ")
  print(as.call(x$funPrint))
  return(invisible(x))
}
