# backward-compatible list
.rxUiBackward <- c(
  "model.desc"="modelDesc",
  "fun.txt"="funTxt",
  "all.covs"="allCovs"
)

#' Convert rxode2 UI object to object for `rxUiGet`
#'
#' @param obj rxode2 ui object
#' @param arg argument that you are trying to get from rxui
#' @param exact exact argument
#' @return
#' @author Matthew L. Fidler
#' @noRd
.uiToRxUiGet <- function(obj, arg, exact=TRUE) {
  .lst <- list(obj, exact)
  .arg <- .rxUiBackward[arg]
  if (is.na(.arg)) .arg <- arg
  class(.lst) <- c(.arg, "rxUiGet")
  .lst
}

#' @export
`$.rxUi` <- function(obj, arg, exact = TRUE) {
  rxUiGet(.uiToRxUiGet(obj=obj, arg=arg, exact=exact))
}

#' S3 for getting information from UI model
#'
#' @param x list of (UIenvironment, exact).  UI environment is the
#'   parsed function for rxode2.  `exact` is a boolean that says if an
#'   exact match is required.
#' @param ... Other arguments
#' @family Query model information
#' @return value that was requested from the UI object
#' @author Matthew Fidler
#' @export
rxUiGet <- function(x, ...) {
  if (!inherits(x, "rxUiGet")) {
    stop("object is wrong type for `rxUiGet`")
  }
  UseMethod("rxUiGet")
}
#' @export
#' @rdname rxUiGet
rxUiGet.theta <- function(x, ...) {
  .x <- x[[1]]
  .ini <- .x$iniDf
  .w <- !is.na(.ini$ntheta)
  setNames(.ini$est[.w], .ini$name[.w])
}
attr(rxUiGet.theta, "desc") <- "Initial Population/Fixed Effects estimates, theta"

#' @export
#' @rdname rxUiGet
rxUiGet.omega <- function(x, ...) {
  .x <- x[[1]]
  .lotri <- lotri::as.lotri(.x$iniDf)
  if (inherits(.lotri, "matrix")) {
    attr(.lotri, "lotriEst") <- NULL
    class(.lotri) <- NULL
  } else {
    attr(.lotri, "lotriEst") <- NULL
    class(.lotri) <- class(.lotri)[-1]
  }
  .lotri
}
attr(rxUiGet.omega, "desc") <- "Initial Random Effects variability matrix, omega"

#' @export
#' @rdname rxUiGet
rxUiGet.funTxt <- function(x, ...) {
  .x <- x[[1]]
  paste(get("lstChr", envir=.x), collapse="\n")
}
attr(rxUiGet.funTxt, "desc") <- "Get function text for the model({}) block"

#' @export
#' @rdname rxUiGet
rxUiGet.allCovs <- function(x, ...) {
  get("covariates", envir=x[[1]])
}
attr(rxUiGet.allCovs, "desc") <- "Get all covariates defined in the model"

#' @export
#' @rdname rxUiGet
rxUiGet.muRefTable <- function(x, ...) {
  .x <- x[[1]]
  .exact <- x[[2]]
  .muRef <- get("muRefDataFrame", .x)
  if (length(.muRef$theta) == 0) return(NULL)
  .muRefCov <- get("muRefCovariateDataFrame", .x)
  if (length(.muRefCov$theta) > 0) {
    .env <- new.env(parent=emptyenv())
    lapply(seq_along(.muRefCov$theta), function(i){
      .theta <- .muRefCov$theta[i]
      .cov <- paste0(.muRefCov$covariate[i], "*", .muRefCov$covariateParameter[i])
      if (exists(.theta, .env)) {
        assign(.theta, c(get(.theta, .env), .cov), .env)
      } else {
        assign(.theta, .cov, .env)
      }
    })
    .muRef$covariates <- vapply(.muRef$theta, function(theta) {
      if (exists(theta, .env)) {
        return(paste(get(theta, .env), collapse=" + "))
      }
      return("")
    }, character(1), USE.NAMES=FALSE)
  }
  if (requireNamespace("huxtable", quietly = TRUE)) {
    .muRef <- huxtable::hux(.muRef) %>%
      ## huxtable::add_colnames() %>%
      huxtable::set_bold(row = 1, col = huxtable::everywhere, value = TRUE) %>%
      huxtable::set_position("center") %>%
      huxtable::set_all_borders(TRUE)
  }
  .muRef
}
attr(rxUiGet.muRefTable, "desc") <- "table/huxtable of mu-referenced items in a model"

#' @rdname rxUiGet
#' @export
rxUiGet.multipleEndpoint <- function(x, ...) {
  .x <- x[[1]]
  .exact <- x[[2]]
  .info <- get("predDf", .x)
  if (is.null(.info)) {
    return(invisible())
  }
  if (length(.info$cond) == 1) return(NULL)
  if (getOption("rxode2.combine.dvid", TRUE)) {
    .info <- .info[order(.info$dvid), ]
  }
  .info <- with(.info, data.frame(
    variable = paste(var, "~", ifelse(use.utf(), "\u2026", "...")),
    cmt = paste0("cmt='", cond, "' or cmt=", cmt),
    "dvid*" = ifelse(is.na(dvid), "",
                     paste0("dvid='", cond, "' or dvid=", dvid)),
    check.names = FALSE,
    stringsAsFactors=FALSE))
  if (!getOption("rxode2.combine.dvid", TRUE)) {
    .info <- .info[, names(.info) != "dvid*"]
  }
  if (requireNamespace("huxtable", quietly = TRUE)) {
    .hux <- huxtable::hux(.info) %>%
      huxtable::add_colnames() %>%
      huxtable::set_bold(row = 1, col = huxtable::everywhere, value = TRUE) %>%
      huxtable::set_position("center") %>%
      huxtable::set_all_borders(TRUE)
    if (getOption("rxode2.combine.dvid", TRUE)) {
      .hux <- .hux %>%
        huxtable::add_footnote("* If dvids are outside this range, all dvids are re-numered sequentially, ie 1,7, 10 becomes 1,2,3 etc")
    }
  } else {
    .hux <- .info
  }
  .hux
}
attr(rxUiGet.multipleEndpoint, "desc") <- "table/huxtable of multiple endpoint translations"

#' @rdname rxUiGet
#' @export
rxUiGet.funPrint <- function(x, ...) {
  .x <- x[[1]]
  .ls <- ls(.x$meta, all.names=TRUE)
  .ret <- vector("list", length(.ls) + 3)
  .ret[[1]] <- quote(`{`)
  for (.i in seq_along(.ls)) {
    .ret[[.i + 1]] <- eval(parse(text=paste("quote(", .ls[.i], "<-", deparse1(.x$meta[[.ls[.i]]]), ")")))
  }
  .len <- length(.ls)
  .ret[[.len + 2]] <- .x$iniFun
  .ret[[.len + 3]] <- .x$modelFun
  .ret
}
attr(rxUiGet.funPrint, "desc") <- "Normalized, quoted model function (for printing)"

#' @export
#' @rdname rxUiGet
rxUiGet.fun <- function(x, ...) {
  .ret <- rxUiGet.funPrint(x, ...)
  .ret2 <- function(){}
  body(.ret2) <- as.call(.ret)
  .ret2
}
attr(rxUiGet.fun, "desc") <- "Normalized model function"

#' @export
#' @rdname rxUiGet
rxUiGet.md5 <- function(x, ...) {
  digest::digest(rxUiGet.funPrint(x, ...))
}

#' @export
#' @rdname rxUiGet
rxUiGet.ini <- function(x, ...) {
  get("iniDf", x[[1]])
}
attr(rxUiGet.ini, "desc") <- "Model initilizations/bounds object"

#'@export
#' @rdname rxUiGet
rxUiGet.iniFun <- function(x, ...) {
  .x <- x[[1]]
  .arg <- class(x)[1]
  lotri::lotriDataFrameToLotriExpression(.x$iniDf, useIni=TRUE)
}
attr(rxUiGet.iniFun, "desc") <- "normalized, quoted `ini()` block"

#' @export
#' @rdname rxUiGet
rxUiGet.modelFun <- function(x, ...) {
  .x <- x[[1]]
  bquote(model(.(as.call(c(quote(`{`),.x$lstExpr)))))
}
attr(rxUiGet.modelFun, "desc") <- "normalized, quoted `model()` block"

#' @export
#' @rdname rxUiGet
rxUiGet.modelDesc <- function(x, ...) {
  .mv <- get("mv0", x[[1]])
  .mvL <- get("mvL", x[[1]])
  if (!is.null(.mvL)){
    return(sprintf(
      "rxode2-based solved PK %s-compartment model%s%s", .mvL$flags["ncmt"],
      ifelse(.mv$extraCmt == 2, " with first-order absorption", ""),
      ifelse(length(.mvL$state) == 0L, "",
             sprintf(" mixed with free from %d-cmt ODE model",
                     length(.mvL$state)))
    ))
  } else if (length(.mv$state) > 0) {
    return(sprintf("rxode2-based free-form %d-cmt ODE model", length(.mv$state)))
  } else {
    return("rxode2-based Pred model")
  }
}
attr(rxUiGet.modelDesc, "desc") <- "Model description (ie linear compartment, pred, ode etc)"

#' @export
#' @rdname rxUiGet
rxUiGet.thetaLower <- function(x, ...) {
  .x <- x[[1]]
  .ini <- .x$iniDf
  .w <- !is.na(.ini$ntheta)
  setNames(.ini$lower[.w], .ini$name[.w])
}
attr(rxUiGet.thetaLower, "desc") <- "thetaLower"

#' @export
#' @rdname rxUiGet
rxUiGet.thetaUpper <- function(x, ...) {
  .x <- x[[1]]
  .ini <- .x$iniDf
  .w <- !is.na(.ini$ntheta)
  setNames(.ini$upper[.w], .ini$name[.w])
}
attr(rxUiGet.thetaUpper, "desc") -> "thetaUpper"

#' @export
#' @rdname rxUiGet
rxUiGet.default <- function(x, ...) {
  .arg <- class(x)[1]
  if (!exists(.arg, envir=x[[1]])) return(NULL)
  get(.arg, x[[1]])
}

.rxUiGetEnvInfo <- c("model"="Original Model (with comments if available)")

.rxUiGetSupportedDollars <- function() {
  .v <- as.character(utils::methods("rxUiGet"))
  .v <- .v[.v != "rxUiGet.default"]
  .cls <- vapply(.v, function(methodStr){
    substr(methodStr,9,nchar(methodStr))
  }, character(1), USE.NAMES=FALSE)
  .v <- vapply(.cls, function(cls){
    .desc <- attr(utils::getS3method("rxUiGet", cls), "desc")
    if (is.null(.desc)) .desc <- ""
    .desc
  }, character(1), USE.NAMES=TRUE)
  # Take out any "hidden methods"
  .w <- which(.v != "")
  .v <- c(.v[.w], .rxUiGetEnvInfo)
  .v
}

#' @export
str.rxUi <- function(object, ...) {
  cat("rxode2 model function\n")
  .s <- .rxUiGetSupportedDollars()
  cat(paste(strtrim(paste(vapply(names(.s), function(x){
    .nchar <- nchar(x)
    if (.nchar >= 10) {
      return(paste0(" $ ", x, ": "))
    } else {
      return(paste0(" $ ",x, paste(rep(" ", 10 - .nchar), collapse=""), ": "))
    }
  }, character(1), USE.NAMES=FALSE), .s), 128), collapse="\n"))
  cat("\n")
  invisible()
}

#' @export
.DollarNames.rxUi <- function(x, pattern) {
  .cmp <- names(.rxUiGetSupportedDollars())
  grep(pattern, .cmp, value = TRUE)
}
