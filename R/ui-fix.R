#' Hard fix population variables
#'
#' @param item expression to consider
#' @param var variable list to fix
#' @param isLhs is the expression a lhs expression?
#' @return expression with variables replaced with constants
#' @noRd
#' @author Matthew L. Fidler
.rxFixPopVar <- function(item, var, isLhs=FALSE) {
  if (is.atomic(item)) {
    return(item)
  }
  if (is.name(item)) {
    .n <- as.character(item)
    .n <- var[.n]
    if (!is.na(.n)) {
      return(setNames(.n, NULL))
    }
    return(item)
  } else if (is.call(item)) {
    if (isLhs && identical(item[[1]], quote(`/`))) {
      # handle d/dt() differently so that d doesn't get renamed
      .num <- item[[2]]
      .denom <- item[[3]]
      if (is.call(.num)) .num <- as.call(lapply(.num, .rxFixPopVar, var=var, isLhs=TRUE))
      if (is.call(.denom)) .denom <- as.call(lapply(.denom, .rxFixPopVar, var=var, isLhs=TRUE))
      return(as.call(c(list(item[[1]]), .num, .denom)))
    } else if (isLhs && length(item) == 2L &&
                 is.numeric(item[[2]])) {
      .env <- new.env(parent=emptyenv())
      .env$new <- NULL
      lapply(seq_along(var),
             function(i) {
               if (!is.null(.env$new)) return(NULL)
               .curVal <- setNames(var[i], NULL)
               .old <- str2lang(names(var[i]))
               if (identical(item[[1]], .old)) {
                 .env$new <- .curVal
               }
               return(NULL)
             })
      if (!is.null(.env$new)) {
        # handle x(0) = items
        return(as.call(c(.env$new, lapply(item[-1], .rxFixPopVar, var=var, isLhs=isLhs))))
      }
    }
    if (identical(item[[1]], quote(`=`)) ||
          identical(item[[1]], quote(`<-`)) ||
          identical(item[[1]], quote(`~`))) {
      .elhs <- lapply(item[c(-1, -3)], .rxFixPopVar, var=var, isLhs=TRUE)
      .erhs <- lapply(item[c(-1, -2)], .rxFixPopVar, var=var, isLhs=FALSE)
      return(as.call(c(item[[1]], .elhs, .erhs)))
    } else {
      return(as.call(c(list(item[[1]]), lapply(item[-1], .rxFixPopVar, var=var, isLhs=isLhs))))
    }
  } else {
    stop("unknown expression", call.=FALSE)
  }
}

#' Apply the fixed population estimated parameters
#'
#' @param ui rxode2 ui function
#' @param returnNull boolean for if unchanged values should return the
#'   original ui (`FALSE`) or null (`TRUE`)
#' @return when `returnNull` is TRUE, NULL if nothing was changed, or
#'   the changed model ui.  When `returnNull` is FALSE, return a ui no
#'   matter if it is changed or not.
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' One.comp.transit.allo <- function() {
#'  ini({
#'    # Where initial conditions/variables are specified
#'    lktr <- log(1.15)  #log k transit (/h)
#'    lcl  <- log(0.15)  #log Cl (L/hr)
#'    lv   <- log(7)     #log V (L)
#'    ALLC <- fix(0.75)  #allometric exponent cl
#'    ALLV <- fix(1.00)  #allometric exponent v
#'    prop.err <- 0.15   #proportional error (SD/mean)
#'    add.err <- 0.6     #additive error (mg/L)
#'    eta.ktr ~ 0.5
#'    eta.cl ~ 0.1
#'    eta.v ~ 0.1
#'  })
#'  model({
#'    #Allometric scaling on weight
#'    cl <- exp(lcl + eta.cl + ALLC * logWT70)
#'    v  <- exp(lv + eta.v + ALLV * logWT70)
#'    ktr <- exp(lktr + eta.ktr)
#'    # RxODE-style differential equations are supported
#'    d/dt(depot)   = -ktr * depot
#'    d/dt(central) =  ktr * trans - (cl/v) * central
#'    d/dt(trans)   =  ktr * depot - ktr * trans
#'    ## Concentration is calculated
#'    cp = central/v
#'    # And is assumed to follow proportional and additive error
#'    cp ~ prop(prop.err) + add(add.err)
#'  })
#' }
#'
#' m <- rxFixPop(One.comp.transit.allo)
#' m
#'
#' # now everything is already fixed, so calling again will do nothing
#'
#' rxFixPop(m)
#'
#' # if you call it with returnNull=TRUE when no changes have been
#' # performed, the function will return NULL
#'
#' rxFixPop(m, returnNull=TRUE)
#'
rxFixPop <- function(ui, returnNull=FALSE) {
  checkmate::assertLogical(returnNull, any.missing = FALSE, len=1, null.ok=FALSE)
  .model <- rxUiDecompress(assertRxUi(ui))
  .model <- .copyUi(.model)
  .iniDf <- .model$iniDf
  .w <- which(!is.na(.iniDf$ntheta) & is.na(.iniDf$err) & .iniDf$fix)
  if (length(.w) == 0L) {
    if (returnNull) return(NULL)
    return(.model)
  }
  .v <- setNames(.iniDf$est[.w], .iniDf$name[.w])
  .lst <- lapply(.model$lstExpr,
                 function(e) {
                   .rxFixPopVar(e, .v)
                 })
  .iniDf <- .iniDf[-.w, ]
  .iniDf$ntheta <- ifelse(is.na(.iniDf$ntheta), NA_integer_, seq_along(.iniDf$ntheta))
  assign("iniDf", .iniDf, envir=.model)
  suppressMessages({
    model(.model) <- .lst
    .model
  })
}
.lineHasFixedResEnv <- new.env(parent=emptyenv())
.lineHasFixedResEnv$err <- NULL
#' Does this line have a fixed residual expression?
#'
#' @param line parsed line to check
#' @param errs errors to check against
#' @return FALSE
#' @export
#' @author Matthew L. Fidler
#' @examples
.lineHasFixedRes <- function(line, errs) {
  if (is.call(line)) {
    return(any(sapply(line, .lineHasFixedRes, errs=errs)))
  } else if (is.name(line)) {
    .cline <- as.character(line)
    if (.cline %in% errs) {
      .lineHasFixedResEnv$err <- .cline
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else if (is.atomic(line)) {
    return(FALSE)
  } else {
    stop("unknown expression", call.=FALSE)
  }
}

#' Literally fix residual parameters
#'
#' @inheritParams rxFixPop
#' @return model with residual parameters literally fixed in the model
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' One.comp.transit.allo <- function() {
#'  ini({
#'    # Where initial conditions/variables are specified
#'    lktr <- log(1.15)  #log k transit (/h)
#'    lcl  <- log(0.15)  #log Cl (L/hr)
#'    lv   <- log(7)     #log V (L)
#'    ALLC <- 0.75  #allometric exponent cl
#'    ALLV <- 1.00  #allometric exponent v
#'    prop.err <- fix(0.15)   #proportional error (SD/mean)
#'    add.err <- fix(0.6)     #additive error (mg/L)
#'    eta.ktr ~ 0.5
#'    eta.cl ~ 0.1
#'    eta.v ~ 0.1
#'  })
#'  model({
#'    #Allometric scaling on weight
#'    cl <- exp(lcl + eta.cl + ALLC * logWT70)
#'    v  <- exp(lv + eta.v + ALLV * logWT70)
#'    ktr <- exp(lktr + eta.ktr)
#'    # RxODE-style differential equations are supported
#'    d/dt(depot)   = -ktr * depot
#'    d/dt(central) =  ktr * trans - (cl/v) * central
#'    d/dt(trans)   =  ktr * depot - ktr * trans
#'    ## Concentration is calculated
#'    cp = central/v
#'    # And is assumed to follow proportional and additive error
#'    cp ~ prop(prop.err) + add(add.err)
#'  })
#' }
#'
#' m <- rxFixRes(One.comp.transit.allo)
rxFixRes <- function(ui, returnNull=FALSE) {
  checkmate::assertLogical(returnNull, any.missing = FALSE, len=1, null.ok=FALSE)
  .model <- rxUiDecompress(assertRxUi(ui))
  .model <- .copyUi(.model)
  .iniDf <- .model$iniDf
  .w <- which(!is.na(.iniDf$ntheta) & !is.na(.iniDf$err) & .iniDf$fix)
  if (length(.w) == 0L) {
    if (returnNull) return(NULL)
    return(.model)
  }
  .v <- setNames(.iniDf$est[.w], .iniDf$name[.w])

  .lstExpr0 <- .model$lstExpr
  .env <- new.env(parent=emptyenv())
  .env$i <- 1
  .env$fix <- .iniDf$name[.w]
  .lst <- lapply(seq_len(length(.lstExpr0)+length(.w)),
                 function(i) {
                   .item <- .lstExpr0[[.env$i]]
                   if (is.call(.item) &&
                         identical(.item[[1]], quote(`~`)) &&
                         .lineHasFixedRes(.item, .env$fix)) {
                     .cerr <- .lineHasFixedResEnv$err
                     .env$fix <- .env$fix[.env$fix != .cerr]
                     str2lang(paste0(.cerr, " <- ", .v[.cerr]))
                   } else {
                     .env$i <- .env$i + 1L
                     .item
                   }
                 })

  .iniDf <- .iniDf[-.w, ]
  .iniDf$ntheta <- ifelse(is.na(.iniDf$ntheta), NA_integer_, seq_along(.iniDf$ntheta))
  assign("iniDf", .iniDf, envir=.model)
  suppressMessages({
    model(.model) <- .lst
    .model
  })
}
