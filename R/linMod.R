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
attr(rxUdfUi.linModD, "nargs") <- 3L

#' @export
rxUdfUi.linModD0 <- function(fun) {
  eval(fun)
}
attr(rxUdfUi.linModD0, "nargs") <- 3L

#' @export
rxUdfUi.linModM <- function(fun) {
  eval(fun)
}
attr(rxUdfUi.linModM, "nargs") <- 2L

#' @export
rxUdfUi.linModM0 <- function(fun) {
  eval(fun)
}
attr(rxUdfUi.linModM0, "nargs") <- 2L

#' @export
rxUdfUi.default <- function(fun) {
  stop("rxode2 user defined function '", fun, "' not supported", call.=FALSE) # nocov
}

#' Linear model to replace in rxode2 ui model
#'
#' @param variable The variable that the rxode2 will be made on.
#'
#' @param power The power of the polynomial that will be generated.
#'
#' @param intercept Boolean that tells if the intercept be generated.
#'
#' @param type the type of linear model replacement to be used.
#'
#' @param num the number the particular model is being generated. If
#'   unspecified, query using `rxUdfUiNum()`.
#'
#' @param iniDf the initialization `data.frame`, if `NULL` query using
#'   `rxUdfUiIniDf()`
#'
#' @param dv the dependent variable to use to generate the initial
#'   estimates from the data. If `NULL` query using `rxUdfUiData()`.
#'
#' @param data logical that tells if the initial estimates of the
#'   linear model should be estimated from the data.
#'
#' @param mv logical that tell if the model variables need to be used
#'   to generate model variables.
#'
#' @param ... arguments that are passed to `linMod()` for the other
#'   abbreviations of `linMod()`
#'
#' @return a list for use in when generating the `rxode2` ui model see
#'   `rxUdfUi()` for details.
#'
#' @export
#' @family User functions
#' @author Matthew L. Fidler
#' @examples
#'
#' linMod(x, 3)
linMod <- function(variable, power, dv="dv",
                   intercept=TRUE,type=c("replace", "before", "after"),
                   num=NULL, iniDf=NULL, data=FALSE, mv=FALSE) {
  .dv <- as.character(substitute(dv))
  .tmp <- suppressWarnings(try(force(dv), silent=TRUE))
  if (!inherits(.tmp, "try-error")) {
    if (is.character(.tmp)) {
      .dv <- dv
    }
  }
  .var <- as.character(substitute(variable))
  .tmp <- try(force(variable), silent=TRUE)
  .doExp3 <- FALSE
  if (!inherits(.tmp, "try-error")) {
    if (is.character(.tmp)) {
      .var <- variable
    } else if (!inherits(.tmp, "formula")) {
      .dv <- as.character(substitute(dv))
      .tmp <- suppressWarnings(try(force(dv), silent=TRUE))
      if (!inherits(.tmp, "try-error")) {
        if (is.character(.tmp)) {
          .dv <- dv
        }
      }
    } else if (length(variable) == 2) {
      if (!identical(variable[[1]], quote(`~`))) {
        stop("unexpected formula, needs to be the form ~x^3",
             call.=FALSE)
      }
      .doExp3 <- TRUE
      .exp3 <- variable[[2]]
    } else {
      if (length(variable) != 3) {
        stop("unexpected formula, needs to be the form dv~x^3",
             call.=FALSE)
      }
      if (!identical(variable[[1]], quote(`~`))) {
        stop("unexpected formula, needs to be the form dv~x^3",
             call.=FALSE)
      }
      .dv <- as.character(variable[[2]])
      data <- TRUE
      .exp3 <- variable[[3]]
      .doExp3 <- TRUE
    }
    if (.doExp3) {
      if (length(.exp3) == 1) {
        .var <- variable <- as.character(.exp3)
        power <- 1
      } else if (length(.exp3) == 3) {
        if (!identical(.exp3[[1]], quote(`^`))) {
          stop("unexpected formula, needs to be the form dv~x^3",
               call.=FALSE)
        }
        if (!is.numeric(.exp3[[3]])) {
          stop("unexpected formula, needs to be the form dv~x^3",
               call.=FALSE)
        }
        .var <- variable <- as.character(.exp3[[2]])
        power <- .exp3[[3]]
      } else {
        stop("unexpected formula, needs to be the form dv~x^3",
             call.=FALSE)
      }
    }
  }
  checkmate::assertCharacter(.var, len=1L, any.missing=FALSE, pattern = "^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$", min.chars=1L,
                             .var.name="variable")
  checkmate::assertCharacter(.dv, len=1L, any.missing=FALSE, pattern = "^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$", min.chars=1L,
                             .var.name="dv")
  checkmate::assertLogical(intercept, len=1L, any.missing=FALSE)
  checkmate::assertIntegerish(power, lower=ifelse(intercept, 0L, 1L), len=1L)
  if (is.null(num)) {
    num <- rxUdfUiNum()
  }
  checkmate::assertIntegerish(num, lower=1, any.missing=FALSE, len=1)
  if (mv && is.null(rxUdfUiMv())) {
    if (intercept) {
      return(list(replace=paste0("linModM(", .var, ", ", power, ")"),
                  uiUseMv=TRUE))
    } else {
      return(list(replace=paste0("linModM0(", .var, ", ", power, ")"),
                  uiUseMv=TRUE))
    }
  }
  if (data && is.null(rxUdfUiData())) {
    if (intercept) {
      return(list(replace=paste0("linModD(", .var, ", ", power, ", ", .dv, ")"),
                  uiUseData=TRUE))
    } else {
      return(list(replace=paste0("linModD0(", .var, ", ", power, ",", .dv, ")"),
                  uiUseData=TRUE))
    }
  }
  if (is.null(iniDf)) {
    iniDf <- rxUdfUiIniDf()
  }
  assertIniDf(iniDf, null.ok=TRUE)
  type <- match.arg(type)
  .mv <- rxUdfUiMv()
  if (!is.null(.mv)) {
    .varsMv <- c(.mv$lhs, .mv$params, .mv$state)
    .pre <- paste0(.var, num, rxIntToLetter(seq_len(power+ifelse(intercept, 1L, 0L))-1L))
    .pre <- vapply(.pre, function(v) {
      if (v %in% .varsMv) {
        paste0("rx.linMod.", v)
      } else {
        v
      }
    }, character(1), USE.NAMES=FALSE)
  } else {
    .pre <- paste0("rx.linMod.", .var, num, rxIntToLetter(seq_len(power+ifelse(intercept, 1L, 0L))-1L))
  }

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
      .wdv <- which(tolower(names(.dat)) == tolower(.dv))
      if (length(.wdv) == 0L) {
        warning(.dv, "not found in data, so no initial estimates will be set to zero")
      } else {
        names(.dat)[.wdv] <- .dv
        .model <-
          stats::lm(
            stats::as.formula(
              paste0(.dv, " ~ stats::poly(", .var, ",", power, ")",
                     ifelse(intercept, "", "+0"))),
            data=rxUdfUiData()
          )
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

#' @describeIn linMod linear model where the model variables are used to generate the model variables
#' @export
linModM <- function(..., intercept=TRUE, mv=TRUE) {
  linMod(..., intercept=intercept, mv=mv)
}
#' @describeIn linMod linear model where the model variables are used to generate the model variables (no intercept)
#' @export
linModM0 <- function(..., intercept=FALSE, mv=TRUE) {
  linMod(..., intercept=intercept, mv=mv)
}
