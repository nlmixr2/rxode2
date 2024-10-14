#' @export
rxUdfUi.linMod <- function(fun) {
  eval(fun)
}
attr(rxUdfUi.linMod, "nargs") <- 2L

#' @export
rxUdfUi.linMod0 <- function(fun) {
  eval(fun)
}
attr(rxUdfUi.linMod0, "nargs") <- 2L

#' @export
rxUdfUi.linModB <- function(fun) {
  eval(fun)
}
attr(rxUdfUi.linModB, "nargs") <- 2L

#' @export
rxUdfUi.linModB0 <- function(fun) {
  eval(fun)
}
attr(rxUdfUi.linModB0, "nargs") <- 2L

#' @export
rxUdfUi.linModA <- function(fun) {
  eval(fun)
}
attr(rxUdfUi.linModA, "nargs") <- 2L

#' @export
rxUdfUi.linModA0 <- function(fun) {
  eval(fun)
}
attr(rxUdfUi.linModA0, "nargs") <- 2L

#' @export
rxUdfUi.linModD <- function(fun) {
  eval(fun)
}
attr(rxUdfUi.linModD, "nargs") <- 2L

#' @export
rxUdfUi.linModD0 <- function(fun) {
  eval(fun)
}
attr(rxUdfUi.linModD0, "nargs") <- 2L

#' @export
rxUdfUi.default <- function(fun) {
  stop("rxode2 user defined function '", fun, "' not supported", call.=FALSE) # nocov
}

#' Linear model to replace in rxode2 ui model
#'
#' @param variable The variable that the rxode2 will be made on
#' @param power The power of the polynomial that will be generated
#' @param intercept Boolean that tells if the intercept be generated
#' @param type the type of linear model replacement to be used.
#' @param num the number the particular model is being generated. If
#'   unspecified, query using `rxUdfUiNum()`.
#' @param iniDf the initialization `data.frame`, if `NULL` query using
#'   `rxUdfUiIniDf()`
#' @param data logical that tells if the initial estimates of the
#'   linear model should be estimated from the data.
#' @param ... arguments that are passed to `linMod()` for the other
#'   abbreviations of `linMod()`
#' @return a list for use in when generating the `rxode2` ui model see
#'   `rxUdfUi()` for details.
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' linMod(x, 3)
linMod <- function(variable, power, intercept=TRUE,type=c("replace", "before", "after"),
                   num=NULL, iniDf=NULL, data=FALSE) {
  .var <- as.character(substitute(variable))
  .tmp <- try(force(variable), silent=TRUE)
  if (!inherits(.tmp, "try-error")) {
    if (is.character(.tmp)) {
      .var <- variable
    }
  }
  checkmate::assertCharacter(.var, len=1L, any.missing=FALSE, pattern = "^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$", min.chars=1L,
                             .var.name="variable")
  checkmate::assertLogical(intercept, len=1L, any.missing=FALSE)
  checkmate::assertIntegerish(power, lower=ifelse(intercept, 0L, 1L), len=1L)
  if (is.null(num)) {
    num <- rxUdfUiNum()
  }
  checkmate::assertIntegerish(num, lower=1, any.missing=FALSE, len=1)
  if (data && !rxUdfUiData()) {
    if (intercept) {
      return(list(replace=paste0("linModD(", .var, ", ", power, ")"),
                  uiUseData=TRUE))
    } else {
      return(list(replace=paste0("linModD0(", .var, ", ", power, ")"),
                  uiUseData=TRUE))
    }
  }
  if (is.null(iniDf)) {
    iniDf <- rxUdfUiIniDf()
  }
  assertIniDf(iniDf, null.ok=TRUE)
  type <- match.arg(type)
  .pre <- paste0("rx.linMod.", .var, num, rxIntToLetter(seq_len(power+ifelse(intercept, 1L, 0L))-1L))
  if (!is.null(iniDf)) {
    .theta <- iniDf[!is.na(iniDf$ntheta),,drop=FALSE]
    if (length(.theta$ntheta) > 0L) {
      .maxTheta <- max(.theta$ntheta)
      .theta1 <- .theta[1,]
    } else {
      .maxTheta <- 0L
      .theta1 <- .rxBlankIni("theta")
    }
    .theta1$lower <- -Inf
    .theta1$upper <- Inf
    .theta1$fix <- FALSE
    .theta1$label <- NA_character_
    .theta1$backTransform <- NA_character_
    .theta1$condition <- NA_character_
    .theta1$err <- NA_character_
    .est <- rep(0, length(.pre))
    if (data) {
      .dat <- rxUdfUiData()
      .dv <- which(tolower(names(.dat)) == "dv")
      if (lenght(.dv) == 0L) {
        warning("No dependent variable found in data, so no initial estimates will be set to zero")
      } else {
        names(.dat)[.dv] <- "dv"
        .model <- stats::lm(as.formula(paste0("dv ~ stats::poly(", .var, ",",
                                              power, ")",
                                              ifelse(intercept, "", "+0"))),
                            data=rxUdfUiData())
        .est <- coef(.model)
      }
    }
    .cur <- c(list(.theta),
              lapply(seq_along(.pre), function(i) {
                .cur <- .theta1
                .cur$name <- .pre[i]
                .cur$est <- .est[i]
                .cur$ntheta <- .maxTheta+i
                .cur
              }))
    .theta <- do.call(`rbind`, .cur)
    .eta <- iniDf[is.na(iniDf$neta),,drop=FALSE]
    .iniDf <- rbind(.theta, .eta)
  } else {
    .iniDf <- NULL
  }
  .linMod <- paste(vapply(seq_along(.pre),
                          function(i) {
                            if (intercept) {
                              if (i == 1) return(.pre[i])
                              if (i == 2) return(paste0(.pre[i], "*", .var))
                              paste0(.pre[i], "*", paste0(.var,"^", i-1L))
                            } else {
                              if (i == 1) return(paste0(.pre[i], "*", .var))
                              paste0(.pre[i], "*", paste0(.var,"^", i))
                            }
                          }, character(1)), collapse="+")
  if (type == "replace") {
    list(replace=.linMod,
         iniDf=.iniDf )
  } else if (type == "before") {
    .replace <- paste0("rx.linMod.", .var, ".f", num)
    list(before=paste0(.replace, " <- ", .linMod),
         replace=.replace,
         iniDf=.iniDf)
  } else if (type == "after") {
    .replace <- paste0("rx.linMod.", .var, ".f", num)
    list(after=paste0(.replace, " <- ", .linMod),
         replace="0",
         iniDf=.iniDf)
  }
}
#' @describeIn linMod linear model without intercept
#' @export
linMod0 <- function(...,intercept=FALSE) {
  linMod(..., intercept=intercept)
}

#' @describeIn linMod linear model before where it occurs
#' @export
linModB <- function(..., type="before") {
  linMod(..., type=type)
}

#' @describeIn linMod linear model before where the user function occurs
#' @export
linModB0 <- function(..., intercept=FALSE, type="before") {
  linMod(..., intercept=intercept, type=type)
}
#' @describeIn linMod linear model after where the user function occurs
linModA <- function(..., type="after") {
  linMod(..., type=type)
}

#' @describeIn linMod liner model without an intercept placed after where the user function occurs
#' @export
linModA0 <- function(..., intercept=FALSE, type="after") {
  linMod(..., intercept=intercept, type=type)
}

#' @describeIn linMod linear model where initial estimates are generated from the data
#' @export
linModD <- function(..., intercept=TRUE, data=TRUE) {
  linMod(..., intercept=intercept, data=data)
}

#' @describeIn linMod linear model where initial estimates are generated from the data (no intercept)
#' @export
linModD0 <- function(..., intercept=FALSE, data=TRUE) {
  linMod(..., intercept=intercept, data=data)
}
