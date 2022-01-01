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
  .regIni <- rex::rex(boundary, or("ini", "lotri"), "(", any_spaces, "{", any_spaces)
  .regOther1 <- rex::rex(any_spaces, "}", any_spaces, ")", any_spaces)
  .regOther2 <- rex::rex(any_spaces, "model", any_spaces, "(", any_spaces, "{", any_spaces)
  .regCommentOnBlankLine <- "^ *#+ *(.*) *$"
  .regLabel <- "^( *[^\n\"]+) *#+ *(.*) *$"
  .ret <- vapply(src,
                 function(line) {
                   if (regexpr(.regIni, line, perl=TRUE) != -1) {
                     print(line)
                     assign("inIni", TRUE, envir=.env)
                   } else if (regexpr(.regOther1, line, perl=TRUE) != -1) {
                     print(line)
                     assign("inIni", FALSE, envir=.env)
                   } else if (regexpr(.regOther2, line, perl=TRUE) != -1) {
                     print(line)
                     assign("inIni", FALSE, envir=.env)
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
#' The ini block controls initial conditions for 'theta' (fixed effects),
#' 'omega' (random effects), and 'sigma' (residual error) elements of the model.
#'
#' 'theta' and 'sigma' can be set using either \code{<-} or \code{=} such as
#' \code{tvCL <- 1} or equivalently \code{tvCL = 1}.  'omega' can be set with a
#' \code{~}.
#'
#' Parameters can be named or unnamed (though named parameters are preferred).
#' A named parameter is set using the name on the left of the assignment while
#' unnamed parameters are set without an assignment operator.  \code{tvCL <- 1}
#' would set a named parameter of \code{tvCL} to \code{1}.  Unnamed parameters
#' are set using just the value, such as \code{1}.
#'
#' For some estimation methods, lower and upper bounds can be set for 'theta'
#' and 'sigma' values.  To set a lower and/or upper bound, use a vector of
#' values.  The vector is \code{c(lower, estimate, upper)}.  The vector may be
#' given with just the estimate (\code{c(estimate)}), the lower bound and
#' estimate (\code{c(lower, estimate)}), or all three (\code{c(lower, estimate,
#' upper)}).  To set an estimate and upper bound without a lower bound, set the
#' lower bound to \code{-Inf}, \code{c(-Inf, estimate, upper)}.  When an
#' estimation method does not support bounds, the bounds will be ignored with a
#' warning.
#'
#' 'omega' values can be set as a single value or as the values of a
#' lower-triangular matrix.  The values may be set as either a
#' variance-covariance matrix (the default) or as a correlation matrix for the
#' off-diagonals with the standard deviations on the diagonals.  Names may be
#' set on the left side of the \code{~}.  To set a variance-covariance matrix
#' with variance values of 2 and 3 and a covariance of -2.5 use \code{~c(2, 2.5,
#' 3)}.  To set the same matrix with names of \code{iivKa} and \code{iivCL}, use
#' \code{iivKa + iivCL~c(2, 2.5, 3)}.  To set a correlation matrix with standard
#' deviations on the diagonal, use \code{cor()} like \code{iivKa + iivCL~cor(2,
#' -0.5, 3)}.
#'
#' Values may be fixed (and therefore not estimated) using either the name
#' \code{fixed} at the end of the assignment or by calling \code{fixed()} as a
#' function for the value to fix.  For 'theta' and 'sigma', either the estimate
#' or the full definition (including lower and upper bounds) may be included in
#' the fixed setting.  For example, the following are all effectively equivalent
#' to set a 'theta' or 'sigma' to a fixed value (because the lower and upper
#' bounds are ignored for a fixed value): \code{tvCL <- fixed(1)}, \code{tvCL <-
#' fixed(0, 1)}, \code{tvCL <- fixed(0, 1, 2)}, \code{tvCL <- c(0, fixed(1),
#' 2)}, or \code{tvCL <- c(0, 1, fixed)}.  For 'omega' assignment, the full
#' block or none of the block must be set as \code{fixed}.  Examples of setting
#' an 'omega' value as fixed are: \code{iivKa~fixed(1)}, \code{iivKa +
#' iivCL~fixed(1, 2, 3)}, or \code{iivKa + iivCL~c(1, 2, 3, fixed)}.  Anywhere
#' that \code{fixed} is used, \code{FIX}, \code{FIXED}, or \code{fix} may be
#' used equivalently.
#'
#' For any value, standard mathematical operators or functions may be used to
#' define the value.  For example, \code{exp(2)} and \code{24*30} may be used to
#' define a value anywhere that a number can be used (e.g. lower bound,
#' estimate, upper bound, variance, etc.).
#'
#' Values may be labeled using the \code{label()} function after the assignment.
#' Labels are are used to make reporting easier by giving a human-readable
#' description of the parameter, but the labels do not have any effect on
#' estimation.  The typical way to set a label so that the parameter \code{tvCL}
#' has a label of "Typical Value of Clearance (L/hr)" is \code{tvCL <- 1;
#' label("Typical Value of Clearance (L/hr)")}.
#'
#' \code{rxode2}/\code{nlmixr2} will attempt to determine some back-transformations for the
#' user.  For example, \code{CL <- exp(tvCL)} will detect that \code{tvCL} must
#' be back-transformed by \code{exp()} for easier interpretation.  When you want
#' to control the back-transformation, you can specify the back-transformation
#' using \code{backTransform()} after the assignment.  For example, to set the
#' back-transformation to \code{exp()}, you can use \code{tvCL <- 1;
#' backTransform(exp())}.
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
  stop("cannot figure out what to do with the ini({}) function", call.=FALSE)
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
      for (.i in ls(envir, all.names=TRUE)) {
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
  if (!is.null(dim(.omega))) {
    if (dim(.omega)[1] > 0) {
      cat(paste0("\n", crayon::bold("Omega"), " (", crayon::bold$blue("$omega"), "):"), "\n")
      print(.omega)
    }
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
