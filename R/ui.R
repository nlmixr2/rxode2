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
                     assign("inIni", TRUE, envir=.env)
                   } else if (regexpr(.regOther1, line, perl=TRUE) != -1) {
                     assign("inIni", FALSE, envir=.env)
                   } else if (regexpr(.regOther2, line, perl=TRUE) != -1) {
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
#' Rearrange function
#'
#' @param fun Function to rearrange so that `model` is at the end (if needed)
#' @return A more normalized function
#' @author Matthew L. Fidler
#' @noRd
.rxFunctionRearrange <- function(fun) {
  .lst <- as.list(body(fun)[-1])
  .idx <- seq_along(.lst)
  .w <- which(vapply(.idx, function(x) {
    identical(.lst[[x]][[1]], quote(`ini`))
  }, logical(1), USE.NAMES=TRUE))
  if (length(.w) == 0) {
  } else if (length(.w) != 1) {
    stop("rxode2 model function can only have one 'ini({})' block",
         call.=FALSE)
  }
  if (identical(.lst[[length(.lst)]][[1]], quote(`model`))) {
    return(fun)
  }
  .w <- which(vapply(.idx, function(x) {
    identical(.lst[[x]][[1]], quote(`model`))
  }, logical(1), USE.NAMES=TRUE))
  if (length(.w) != 1) {
    stop("rxode2 model function requires one 'model({})' block",
         call.=FALSE)
  }
  warning("'model({})' is not on the last line of the function, rearranging; function cannot be called directly to produce model object",
          call.=FALSE)
  .fun2 <- function() {
  }
  body(.fun2) <- as.call(lapply(c(-1L, .idx[-.w], .w), function(i) {
    if (i == -1L) return(quote(`{`))
    .lst[[i]]
  }))
  .fun2
}

.rxFunction2ui <- function(fun) {
  .fun <- .rxFunctionRearrange(eval(parse(text=paste(.rxFunction2string(fun), collapse="\n"))))
  .ret <- .fun()
  # Save $model like nlmixr UI used to...
  .ret <- rxUiDecompress(.ret)
  assign("model", fun, envir=.ret)
  rxUiCompress(.ret)
}

.lastIni <- NULL
.lastIniQ <- NULL

#' Ini block for rxode2/nlmixr models
#'
#' The ini block controls initial conditions for 'theta' (fixed effects),
#' 'omega' (random effects), and 'sigma' (residual error) elements of the model.
#'
#' The \code{ini()} function is used in two different ways.  The main way that
#' it is used is to set the initial conditions and associated attributes
#' (described below) in a model.  The other way that it is used is for updating
#' the initial conditions in a model, often using the pipe operator.
#'
#' 'theta' and 'sigma' can be set using either \code{<-} or \code{=} such as
#' \code{tvCL <- 1} or equivalently \code{tvCL = 1}.  'omega' can be set with a
#' \code{~} such as \code{etaCL ~ 0.1}.
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
#' given with just the estimate (\code{estimate}), the lower bound and
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
#' define the value.  For example, \code{log(2)} and \code{24*30} may be used to
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
#' \code{rxode2}/\code{nlmixr2} will attempt to determine some
#' back-transformations for the user.  For example, \code{CL <- exp(tvCL)} will
#' detect that \code{tvCL} must be back-transformed by \code{exp()} for easier
#' interpretation.  When you want to control the back-transformation, you can
#' specify the back-transformation using \code{backTransform()} after the
#' assignment.  For example, to set the
#' back-transformation to \code{exp()}, you can use \code{tvCL <- 1;
#' backTransform(exp())}.
#'
#' @param x expression
#' @param ... Other expressions for `ini()` function
#' @param envir the `environment` in which unevaluated model
#'   expressions is to be evaluated.  May also be `NULL`, a list, a
#'   data frame, a pairlist or an integer as specified to `sys.call`.
#' @inheritParams .iniHandleAppend
#' @return ini block
#' @author Matthew Fidler
#' @family Initial conditions
#' @examples
#' # Set the ini() block in a model
#' one.compartment <- function() {
#'   ini({
#'     tka <- log(1.57); label("Ka")
#'     tcl <- log(2.72); label("Cl")
#'     tv <- log(31.5); label("V")
#'     eta.ka ~ 0.6
#'     eta.cl ~ 0.3
#'     eta.v ~ 0.1
#'     add.sd <- 0.7
#'   })
#'   model({
#'     ka <- exp(tka + eta.ka)
#'     cl <- exp(tcl + eta.cl)
#'     v <- exp(tv + eta.v)
#'     d/dt(depot) = -ka * depot
#'     d/dt(center) = ka * depot - cl / v * center
#'     cp = center / v
#'     cp ~ add(add.sd)
#'   })
#' }
#'
#' # Use piping to update initial conditions
#' one.compartment %>% ini(tka <- log(2))
#' one.compartment %>% ini(tka <- label("Absorption rate, Ka (1/hr)"))
#' # Move the tka parameter to be just below the tv parameter (affects parameter
#' # summary table, only)
#' one.compartment %>% ini(tka <- label("Absorption rate, Ka (1/hr)"), append = "tv")
#' # When programming with rxode2/nlmixr2, it may be easier to pass strings in
#' # to modify the ini
#' one.compartment %>% ini("tka <- log(2)")
#' @export
ini <- function(x, ..., envir = parent.frame(), append = NULL) {
  if (is(substitute(x), "{")) {
    .ini <- eval(bquote(lotri(.(substitute(x)))), envir=envir)
    assignInMyNamespace(".lastIni", .ini)
    assignInMyNamespace(".lastIniQ", bquote(.(substitute(x))))
    return(invisible(.ini))
  }
  UseMethod("ini")
}

#' Model block for rxode2/nlmixr models
#'
#' @param x model expression
#'
#' @param ... Other arguments
#'
#' @param append This is a boolean to determine if the lines are
#'   appended in piping.  The possible values for this is:
#'
#'  - `TRUE` which is when the lines are appended to the model instead of replaced
#'  - `FALSE` when the lines are replaced in the model (default)
#'  - `NA` is when the lines are pre-pended to the model instead of replaced
#'  - `lhs expression`, which will append the lines after the last observed line of the expression `lhs`
#'
#' @param auto This boolean tells if piping automatically selects the
#'   parameters should be characterized as a population parameter,
#'   between subject variability, or a covariate.  When `TRUE` this
#'   automatic selection occurs.  When `FALSE` this automatic
#'   selection is turned off and everything is added as a covariate
#'   (which can be promoted to a parameter with the `ini` statement).
#'   By default this is `TRUE`, but it can be changed by
#'   `options(rxode2.autoVarPiping=FALSE)`.
#'
#'
#' @param cov is a character vector of variables that should be
#'   assumed to be covariates.  This will override automatic promotion
#'   to a population parameter estimate (or an eta)
#'
#' @param envir the `environment` in which unevaluated model
#'   expressions is to be evaluated.  May also be `NULL`, a list, a
#'   data frame, a pairlist or an integer as specified to `sys.call`.
#'
#' @return Model block with ini information included.  `ini` must be
#'   called before `model` block
#'
#' @author Matthew Fidler
#'
#' @export
model <- function(x, ..., append=FALSE, auto=getOption("rxode2.autoVarPiping", TRUE),
                  cov=NULL, envir=parent.frame()) {
  if (is(substitute(x), "{")) {
    .funName <- try(as.character(as.list(with(envir, match.call()))[[1]]), silent=TRUE)
    if (inherits(.funName, "try-error")) {
      .funName <- NULL
    } else if (length(.funName) == 1L && exists(.funName, envir=parent.env(envir))) {
      .udfEnvSet(parent.env(envir))
    }
    .ini <- .lastIni
    .iniQ <- .lastIniQ
    if (is.null(.ini)) {
      .ini <- data.frame(ntheta=integer(0),
                         neta1=numeric(0),
                         neta2=numeric(0),
                         name=character(0),
                         lower=numeric(0),
                         est=numeric(0),
                         upper=numeric(0),
                         fix=logical(0),
                         label=character(0),
                         backTransform=character(0),
                         condition=character(0),
                         err=character(0))
      .iniQ <- NULL
      ## stop("ini({}) block must be called before the model block",
      ##      call.=FALSE)
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
    .mod$sticky <- character(0)
    .w <- which(!is.na(.mod$iniDf$err) & !is.na(.mod$iniDf$neta1))
    if (length(.w) > 0) {
      stop("the parameter(s) '", paste(.mod$iniDf$name[.w], collapse="', '"), "' cannot be an error and between subject variability",
           call.=FALSE)
    }
    assign("modelName", .funName, envir=.mod)
    class(.mod) <- "rxUi"
    return(rxUiCompress(.mod))
  }
  on.exit({.varSelect$cov <- NULL})
  UseMethod("model")
}

#' @export
#' @rdname model
model.default <- function(x, ..., append=FALSE, cov=NULL, envir=parent.frame()) {
  stop("rxode2 does not know how to handle this model statement")
}

#' @export
print.rxUi <-function(x, ...) {
  .md <- x$modelDesc
  cat(cli::cli_format_method({
    cli::cli_h1("{.md}")
  }), "\n")
  .theta <- x$theta
  .omega <- x$omega
  if (length(x$iniDf$cond) > 0) {
    cat(cli::cli_format_method({
      cli::cli_h2("Initalization:")
    }), "\n")
  }
  if (length(.theta) > 0) {
    cat(paste0(crayon::bold("Fixed Effects"), " (", crayon::bold$blue("$theta"), "):"), "\n")
    print(.theta)
  }
  if (!is.null(dim(.omega))) {
    if (dim(.omega)[1] > 0) {
      cat(paste0("\n", crayon::bold("Omega"), " (", crayon::bold$blue("$omega"), "):"), "\n")
      print(.omega)
    }
  }
  if (length(x$state) > 0) {
    cat(paste0(crayon::bold("\nStates"), " (", crayon::bold$blue("$state"), " or ", crayon::bold$blue("$stateDf"), "):"), "\n")
    print(rxUiGet.stateDf(list(x,TRUE)))
  }
  # Multiple Endpoint
  .me <- x$multipleEndpoint
  if (!is.null(.me)) {
    .met <- crayon::bold("Multiple Endpoint Model")
    .med <- crayon::bold$blue("$multipleEndpoint")
    cat(cli::cli_format_method({
      cli::cli_h2("{.met} ({.med}):")
    }), "\n")
    print(.me)
    if (getOption("rxode2.combine.dvid", TRUE)) {
      cat("  * If dvids are outside this range, all dvids are re-numered sequentially, ie 1,7, 10 becomes 1,2,3 etc\n")
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
    print(.mu)
    cat("\n")
  }
  cat(cli::cli_format_method({
    cli::cli_h2("Model (Normalized Syntax):")
  }))
  cat("\nfunction() ")
  print(as.call(x$funPrint))
  return(invisible(x))
}
#' Compress/Decompress `rxode2` ui
#'
#'
#' @param ui rxode2 ui object
#' @inheritParams rxUiCompressDefault
#' @return A compressed or decompressed rxui object
#' @author Matthew L. Fidler
#' @export
#' @examples
#'
#' one.cmt <- function() {
#'   ini({
#'     ## You may label each parameter with a comment
#'     tka <- 0.45 # Log Ka
#'     tcl <- log(c(0, 2.7, 100)) # Log Cl
#'     ## This works with interactive models
#'     ## You may also label the preceding line with label("label text")
#'     tv <- 3.45; label("log V")
#'     ## the label("Label name") works with all models
#'     eta.ka ~ 0.6
#'     eta.cl ~ 0.3
#'     eta.v ~ 0.1
#'     add.sd <- 0.7
#'   })
#'   model({
#'     ka <- exp(tka + eta.ka)
#'     cl <- exp(tcl + eta.cl)
#'     v <- exp(tv + eta.v)
#'     linCmt() ~ add(add.sd) | tmp
#'   })
#' }
#'
#' f <- rxode2(one.cmt)
#' print(class(f))
#' print(is.environment(f))
#'
#' f  <- rxUiDecompress(f)
#' print(class(f))
#' print(is.environment(f))
#'
#' f  <- rxUiCompress(f)
#' print(class(f))
#' print(is.environment(f))
#'
rxUiDecompress <- function(ui) {
  if (!inherits(ui, "rxUi")) return(ui)
  if (is.environment(ui))  return(ui)
  if (is.raw(ui)) {
    .ret <- try(qs::qdeserialize(ui), silent=TRUE)
    if (inherits(.ret, "try-error")) {
      stop("cannot decompress ui object, possibly corrupt",
           call.=FALSE)
    }
    class(.ret) <- "rxUi"
    return(.ret)
  }
  if (is.list(ui)) {
    .meta <- new.env(parent=emptyenv())
    lapply(names(ui$meta), function(x) {
      assign(x, ui$meta[[x]], .meta)
    })
    .ret <- new.env(parent=emptyenv())
    lapply(names(ui), function(x) {
      if (x == "meta") {
        assign(x, .meta, .ret)
      }
      assign(x, ui[[x]], .ret)
    })
    class(.ret) <- "rxUi"
    return(.ret)
  }
  .ret <- qs::qdeserialize(ui)
  class(.ret) <- "rxUi"
  .ret
}

.rxUiCompressDefault <- new.env(parent=emptyenv())
.rxUiCompressDefault$type <-"list"

#' Set the default compression type for rxode2 ui objects
#'
#' @param type type can either be:
#'
#'  - \code{"list"} (default) for a list object, which changes the environment to a list object
#' - \code{"qs"} for a raw object, which uses the qs package to serialize the object
#' @return nothing, called for side effect of setting the default compression type
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' rxUiCompressDefault("list")
#'
rxUiCompressDefault <- function(type=c("list","qs")) {
  if (missing(type)) {
    type <- "list"
  } else  {
    type <- match.arg(type)
  }
  .rxUiCompressDefault$type <- type
}

#' @rdname rxUiDecompress
#' @export
rxUiCompress <- function(ui, type=c("list","qs")) {
  if (missing(type)) {
    type <- "list"
  } else  {
    type <- match.arg(type)
  }
  if (!inherits(ui, "rxUi")) return(ui)
  if (is.environment(ui) && type=="qs") {
    .ret <- try(qs::qserialize(ui, preset="fast"), silent=TRUE)
    if (inherits(.ret, "try-error")) .ret <- qs::qserialize(ui, preset="archive")
    class(.ret) <- c("rxUi", "raw")
    return(.ret)
  }
  if (is.environment(ui) && type=="list") {
    .m <- get("meta", envir=ui)
    .n <- ls(get("meta", envir=ui), all=TRUE)
    .meta <- lapply(.n, function(x) {
      get(x, .m)
    })
    names(.meta) <- .n
    .n <- ls(ui, all=TRUE)
    .ret <- lapply(.n, function(x) {
      if (x == "meta") return(.meta)
      get(x, ui)
    })
    names(.ret) <- .n
    class(.ret) <- c("rxUi", "list")
    return(.ret)
  }
  ui
}
