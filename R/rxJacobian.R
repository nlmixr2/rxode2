## Refactor Jacobian calculation
#' Faster expand.grid
#'
#' Only support x and y as characters right now
#'
#' @param x first element (must be character)
#' @param y second element (must be character)
#' @param type Internal type=0L is traditional expand grid and
#'     type=1L is jacobian expand grid (adds symbols)
#' @return Expand grid
#' @author Matthew Fidler
#' @keywords internal
#' @examples
#'
#' ##
#' rxExpandGrid(letters, letters)
#'
#' ## Another fast method; See
#' ## https://stackoverflow.com/questions/10405637/use-outer-instead-of-expand-grid
#'
#' expand.grid.jc <- function(seq1, seq2) {
#'   cbind(
#'     Var1 = rep.int(seq1, length(seq2)),
#'     Var2 = rep.int(seq2, rep.int(length(seq1), length(seq2)))
#'   )
#' }
#' \donttest{
#' microbenchmark::microbenchmark(rxExpandGrid(letters, letters), expand.grid.jc(letters, letters))
#' }
#' @export
rxExpandGrid <- function(x, y, type = 0L) {
  .Call(`_rxode2_rxExpandGrid_`, x, y, type)
}

## Assumes model is loaded.
#'  Internal function for calculating the jacobian
#'
#'
#' @param model symengine environment
#' @param vars Variables
#' @return Jacobian informat
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxJacobian <- function(model, vars = TRUE) {
  if (rxIs(vars, "logical")) {
    if (vars) {
      .pars <- .rxParams(model, TRUE)
      if (any(.pars == "ETA[1]")) {
        .pars <- .pars[regexpr(rex::rex(start, "ETA[", any_numbers, "]"), .pars) != -1]
      }
      .jac <- rxExpandGrid(
        rxState(model),
        c(rxState(model), .pars),
        1L
      )
    } else {
      .pars <- NULL
      .jac <- rxExpandGrid(
        rxState(model),
        rxState(model),
        1L
      )
    }
  } else if (rxIs(vars, "character")) {
    .pars <- vars
    .jac <- rxExpandGrid(
      rxState(model),
      c(rxState(model), vars),
      1L
    )
  }
  assign("..vars", .pars, envir = model)
  .malert("calculate jacobian")
  rxProgress(dim(.jac)[1])
  on.exit({
    rxProgressAbort()
  })
  .ret <- apply(.jac, 1, function(x) {
    .l <- x["line"]
    .l <- eval(parse(text = .l))
    rxTick()
    paste0(x["rx"], "=", rxFromSE(.l))
  })
  assign("..jacobian", .ret, envir = model)
  rxProgressStop()
  return(.ret)
}

## Assumes .rxJacobian called on model c(state,vars)
#'  Sensitivity for model
#'
#'
#' @param model symengiE model environme
#' @param vars Variables for single sensitivity
#' @param vars2 if present, 2 parameter sensitivity
#' @return Sensitivity
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxSens <- function(model, vars, vars2) {
  .state <- rxState(model)
  if (length(.state) > 0L) {
    if (missing(vars)) vars <- get("..vars", envir = model)
    if (!missing(vars2)) {
      .grd <- rxode2::serxExpandSens2_(.state, vars, vars2)
    } else {
      .grd <- rxode2::rxExpandSens_(.state, vars)
    }
    .malert("calculate sensitivities")
    rxProgress(dim(.grd)[1])
    on.exit({
      rxProgressAbort()
    })
    lapply(c(.grd$ddtS, .grd$ddS2), function(x) {
      assign(x, symengine::Symbol(x), envir = model)
    })
    .ret <- apply(.grd, 1, function(x) {
      .l <- x["line"]
      .l <- eval(parse(text = .l))
      .ret <- paste0(x["ddt"], "=", rxFromSE(.l))
      if (exists(x["s0"], envir = model)) {
        .l <- x["s0D"]
        .l <- eval(parse(text = .l))
        if (paste(.l) != "0") {
          .ret <- paste0(
            .ret, "\n", x["s0r"], "=", rxFromSE(.l),
            "+0.0"
          )
        }
      }
      rxTick()
      return(.ret)
    })
    if (missing(vars2)) {
      assign("..sens", .ret, envir = model)
    } else {
      assign("..sens2", .ret, envir = model)
    }
    rxProgressStop()
  } else {
    assign("..sens", NULL, envir = model)
    .ret <- NULL
  }
  return(.ret)
}


## Check for good functions for predfn and pkpars and error functions
.goodFns <- c(".GlobalEnv", "package:rxode2", "package:nlmixr")
.checkGood <- function(x) {
  .tmp <- suppressWarnings({
    find(deparse1(substitute(x)))
  })
  if (!identical(.tmp, character())) {
    if (!any(.tmp == .goodFns)) {
      stop(sprintf(gettext("'%s' is from '%s' and cannot be used in this context"), deparse1(substitute(x)), .tmp),
        call. = FALSE
      )
    }
  }
}

#' Get the state information for the current model
#'
#' @param obj rxode2 model that can take rxModelVars
#' @return character vector of initial preserved states (state),
#'     extra states (statef) and dvid translation information
#'     (dvid). This is used in generating the final rxode2 model.
#' @author Matthew Fidler
#' @noRd
.rxGenFunState <- function(obj) {
  .mv0 <- rxModelVars(obj)
  .curDvid <- .mv0$dvid
  .state0 <- .mv0$state
  if (length(.state0) > 0) {
    .state0 <- paste(paste0("cmt(", .mv0$state, ");\n"), collapse = "")
  } else {
    .state0 <- ""
  }
  .statef <- .mv0$stateExtra
  if (length(.statef) > 0) {
    .statef <- paste0(paste(paste0("\ncmt(", .mv0$stateExtra, ");"), collapse = ""), "\n")
  } else {
    .statef <- ""
  }
  if (length(.curDvid) > 1) {
    .dvidF <- paste0(
      "\ndvid(",
      paste(.curDvid, collapse = ","), ");\n"
    )
  } else {
    .dvidF <- ""
  }
  return(c(state = .state0, statef = .statef, dvid = .dvidF))
}

#' Generate a rxode2 block augmented by a pkpars function
#'
#' @inheritParams rxSEinner
#' @return New model augmented by pred function
#' @author Matthew L. Fidler
#' @noRd
.rxGenPkpars <- function(rx, pkpars, init) {
  if (!is.null(pkpars)) {
    .txt <- as.vector(unlist(strsplit(rxParsePk(pkpars, init = init), "\n")))
    .re <- rex::rex(
      start, "init",
      or("_", ".", ""),
      or("C", "c"), "ond",
      or("ition", ""),
      or("", "s"), any_spaces,
      or("=", "<-", "~"), any_spaces,
      "c(", capture(anything), ")", any_spaces, ";",
      any_spaces, end
    )
    .w <- which(regexpr(.re, .txt) != -1)
    if (length(.w) == 1L) {
      .inis <- .txt[.w]
      .inis <- strsplit(gsub(.re, "\\1", .inis), " *, *")[[1]]
      if (length(rxState(rx)) == length(.inis)) {
        .inis <- paste(paste0(rxState(rx), "(0)=", .inis, "+0.0;"), collapse = "\n")
        .txt[.w] <- .inis
      } else {
        stop(gettext("specified %s initial conditions when there are only %s states"),
          length(.inis), length(rxState(rx)),
          call. = FALSE
        )
      }
    } else if (length(.w) > 1) {
      stop("only one 'initCondition=' supported",
        call. = FALSE
      )
    }
    .newmod <- rxGetModel(paste0(paste(.txt, collapse = "\n"), "\n", rxNorm(rx)))
    return(.newmod)
  } else {
    return(rx)
  }
}
#' Generate Augmented pred/err rxode2 model
#'
#' @inheritParams rxSEinner
#' @return A list of (1) rxode2 model variables augmented with
#'     pred/error information and (2) extra error variables created.
#' @author Matthew L Fidler
#' @noRd
.rxGenPred <- function(obj, predfn, errfn, init,
                       addProp = c("combined2", "combined1")) {
  .extraPars <- NULL
  add <- function(...) {} # maker rcheck happy
  if (is.null(errfn)) {
    errfn <- function() {
      add(0.1)
    }
  }
  ## Get maximum theta.
  .txt <- rxParsePred(predfn, init = init)
  .predMod <- rxGetModel(.txt)
  .pars <- .rxParams(rxGetModel(paste0(
    rxNorm(obj), "\n",
    rxNorm(.predMod)
  )), FALSE)
  .w <- which(sapply(.pars, function(x) {
    return(substr(x, 0, 2) == "TH")
  }))
  .pars <- .pars[.w]
  if (length(.pars) > 0) {
    .mtheta <- max(as.numeric(gsub(rex::rex("THETA[", capture(numbers), "]"), "\\1", .pars)))
  } else {
    .mtheta <- 0
  }
  .err <- rxParseErr(errfn, baseTheta = .mtheta + 1, init = init, addProp = addProp)
  .predMod <- rxParsePred(predfn, init = init, .err)
  .extraPars <- attr(.predMod, "ini")
  .predMod <- rxGetModel(.predMod)
  .full <- paste0(rxNorm(obj), "\n", rxNorm(.predMod))
  .full <- rxGetModel(.full)
  return(list(.full, .extraPars))
}

.rxFixR <- function(.newmod, addProp) {
  if (!exists("..fixR", envir = .newmod)) {
    if (exists("rx_r_", envir = .newmod)) {
      ## Breaks focei for non-trivial examples
      ## if (!rxErrEnv.hasAdd) {
      ##   ## Convert abs() to abs1()
      ##   .r <- get("rx_r_", envir = .newmod)
      ##   .r <- paste0("abs1(", rxFromSE(.r), ")")
      ##   .r <- symengine::S(rxToSE(.r))
      ##   assign("rx_r_", .r, envir=.newmod)
      ## }
      if (addProp == "combined1") {
        assign("rx_r_", get("rx_r_", envir = .newmod)^2, envir = .newmod)
      }
      assign("..fixR", TRUE, envir = .newmod)
    }
  }
}

.rxLoadPrune <- function(mod, doConst = TRUE, promoteLinSens = TRUE, fullModel = FALSE,
                         addProp = c("combined2", "combined1")) {
  addProp <- match.arg(addProp)
  if (fullModel) {
    .malert("pruning branches ({.code if}/{.code else}) of full model...")
  } else {
    .malert("pruning branches ({.code if}/{.code else})...")
  }
  .newmod <- rxGetModel(rxPrune(mod))
  .msuccess("done")
  ## message("Loading into symengine environment...", appendLF=FALSE)
  if (fullModel) {
    .malert("loading full model into {.pkg symengine} environment...")
  } else {
    .malert("loading into {.pkg symengine} environment...")
  }
  .newmod <- rxS(.newmod, doConst, promoteLinSens = promoteLinSens)
  .rxFixR(.newmod, addProp)
  .msuccess("done")
  return(.newmod)
}

#' Generate and load rxode2 function into sympy environment
#'
#' @inheritParams rxSEinner
#' @param full A boolean indicating if rxode2 is creating a full model
#'   or a SAEM model
#' @return Sympy environment
#' @author Matthew Fidler
#' @noRd
.rxGenFun <- function(obj, predfn, pkpars = NULL, errfn = NULL,
                      init = NULL, promoteLinSens = TRUE, full = TRUE,
                      addProp = c("combined2", "combined1")) {
  rxSolveFree()
  rxTempDir()
  .checkGood(predfn)
  .checkGood(pkpars)
  .checkGood(errfn)
  ## Probably need assignInMyNamespace...
  if (full) {
    .malert("creating full model...")
  } else {
    .malert("creating SAEM model...")
  }
  .stateInfo <- .rxGenFunState(obj)
  .newmod <- .rxGenPkpars(obj, pkpars, init)
  .newmod <- .rxGenPred(.newmod, predfn, errfn, init, addProp = addProp)
  .extraPars <- .newmod[[2]]
  .newmod <- .newmod[[1]]
  .newmod <- .rxLoadPrune(.newmod, promoteLinSens = promoteLinSens, addProp = addProp)
  .newmod$..stateInfo <- .stateInfo
  .newmod$..extraPars <- .extraPars
  return(.newmod)
}

.rxIsOp <- function(x) {
  (identical(x, quote(`*`)) ||
    identical(x, quote(`**`)) ||
    identical(x, quote(`^`)) ||
    identical(x, quote(`+`)) ||
    identical(x, quote(`-`)) ||
    identical(x, quote(`/`)) ||
    identical(x, quote(`==`)) ||
    identical(x, quote(`!=`)) ||
    identical(x, quote(`>=`)) ||
    identical(x, quote(`<=`)) ||
    identical(x, quote(`<`)) ||
    identical(x, quote(`>`)) ||
    identical(x, quote(`&&`)) ||
    identical(x, quote(`&`)) ||
    identical(x, quote(`||`)) ||
    identical(x, quote(`|`)))
}

.rxIsEqOrTilde <- function(x) {
  identical(x, quote(`=`)) ||
    identical(x, quote(`<-`)) ||
    identical(x, quote(`~`))
}

.rxGenSaemHide0 <- function(x) {
  if (is.name(x) || is.atomic(x)) {
    return(as.character(x))
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(`(`))) {
      return(paste0("(", .rxGenSaemHide0(x[[2]]), ")"))
    } else if (identical(x[[1]], quote(`{`))) {
      return(paste0("{\n", paste(lapply(x[-1], .rxGenSaemHide0),
        collapse = "\n"
      ), "\n}"))
    } else if (as.character(x[[1]]) == "if") {
      if (length(x) == 3) {
        return(paste0(
          "if (", .rxGenSaemHide0(x[[2]]), ") ",
          .rxGenSaemHide0(x[[3]])
        ))
      } else if (length(x) == 4) {
        return(paste0(
          "if (", .rxGenSaemHide0(x[[2]]), ") ",
          .rxGenSaemHide0(x[[3]]), " else ",
          .rxGenSaemHide0(x[[4]])
        ))
      }
    } else if (.rxIsOp(x[[1]])) {
      if (length(x) == 3) {
        return(paste0(
          .rxGenSaemHide0(x[[2]]),
          as.character(x[[1]]),
          .rxGenSaemHide0(x[[3]])
        ))
      } else {
        ## Unary Operators
        return(paste(
          as.character(x[[1]]),
          .rxGenSaemHide0(x[[2]])
        ))
      }
    } else if (.rxIsEqOrTilde(x[[1]])) {
      .x2 <- .rxGenSaemHide0(x[[2]])
      if (.x2 == "nlmixr_pred" || regexpr("[(]", .x2) != -1) {
        return(paste0(
          .x2, "=",
          .rxGenSaemHide0(x[[3]])
        ))
      } else {
        return(paste0(
          .x2, "~",
          .rxGenSaemHide0(x[[3]])
        ))
      }
    } else {
      return(paste0(as.character(x[[1]]), "(", paste(unlist(lapply(
        x[-1],
        .rxGenSaemHide0
      )),
      collapse = ", "
      ), ")"))
    }
  }
}
.rxGenSaemHide <- function(x) {
  .ret <- strsplit(.rxGenSaemHide0(eval(parse(text = paste0("quote({", x, "})")))), "\n")[[1]]
  .ret <- .ret[-c(1, length(.ret))]
  paste(.ret, collapse = "\n")
}
#' Generate pred-only SAEM rxode2 model
#'
#' @param obj rxode2 model (text or actual model)
#' @param predfn prediction function
#' @param pkpars PKpars function
#' @param sum.prod Change addition and deletion to sum() and prod()
#'   to stabilize round off errors
#' @param optExpression Boolean for optimizing expression to minimize
#'   more expensive calls
#' @param loadSymengine Boolean indicating if the model should be
#'   loaded into symengine.  This cause all the ODEs to be collapsed
#'   into one expression that is eventually optimized if
#'   `optExpression` is `TRUE`.
#' @return rxode2 text
#' @author Matthew Fidler
#' @keywords internal
#' @export
rxGenSaem <- function(obj, predfn, pkpars = NULL, sum.prod = FALSE, optExpression = TRUE,
                      loadSymengine = TRUE) {
  rxReq("symengine")
  .digest <- digest::digest(list(
    rxModelVars(obj)$md5["parsed_md5"],
    ifelse(is.function(pkpars), paste(deparse1(body(pkpars)), collapse = ""), ""),
    sum.prod, optExpression, loadSymengine
  ))
  .path <- file.path(rxTempDir(), paste0("saem-", .digest, ".rds"))
  if (file.exists(.path)) {
    .ret <- try(readRDS(.path), silent = TRUE)
    if (!inherits(.ret, "try-error")) {
      return(.ret)
    }
  }
  nlmixrAdd <- NULL
  add <- function(...) {}
  .errfn <- function() {
    return(add(nlmixrAdd))
  }
  ## FIXME need to check if predfn is nlmixr_pred
  ## Add prop not supplied here, so it doesn't matter what you choose
  if (loadSymengine) {
    .s <- .rxGenFun(obj, predfn, pkpars,
      errfn = .errfn,
      init = NULL, promoteLinSens = FALSE, full = FALSE
    )
    .prd <- get("rx_pred_", envir = .s)
    .prd <- paste0("rx_pred_=", rxFromSE(.prd))
    .lhs0 <- .s$..lhs0
    if (is.null(.lhs0)) .lhs0 <- ""
    .ddt <- .s$..ddt
    if (is.null(.ddt)) .ddt <- ""
    .saem <- paste(c(
      .s$..stateInfo["state"],
      .lhs0,
      .ddt,
      .prd,
      .s$..stateInfo["statef"],
      .s$..stateInfo["dvid"],
      ""
    ), collapse = "\n")
    if (is.null(pkpars)) {
      .mv <- rxModelVars(obj)$params
    } else {
      .mv <- deparse1(body(pkpars))
      .len <- length(.mv)
      .mv <- if (.mv[1] == "{") .mv[2:(.len - 1)] else .mv
      .mv <- rxModelVars(paste(.mv, collapse = "\n"))$params
    }
  } else {
    if (is.null(pkpars)) {
      .mv <- rxModelVars(obj)
      .state <- .mv$state
      .mv <- .mv$params
      .saem <- rxNorm(obj)
    } else {
      .mv <- deparse(body(pkpars))
      .len <- length(.mv)
      .mv <- if (.mv[1] == "{") .mv[2:(.len - 1)] else .mv
      .pre <- paste(.mv, collapse = "\n")
      .saem <- paste0(.pre, "\n", rxNorm(obj))
      .mv <- rxModelVars(.saem)
      .state <- .mv$state
      .mv <- .mv$params
    }
    if (!any(rxModelVars(.saem)$lhs == "nlmixr_pred")) {
      warning("this was not generated from nlmixr, loading into symengine")
      return(rxGenSaem(
        obj = obj, predfn = predfn, pkpars = pkpars, sum.prod = sum.prod, optExpression = optExpression,
        loadSymengine = TRUE
      ))
    }
    .malert("generate SAEM model")
    .saem <- .rxGenSaemHide(.saem)
    .saem <- paste0(
      ifelse(length(.state) > 0,
        paste(paste0("cmt(", .state, ")"), collapse = "\n"), ""
      ), "\n",
      .saem
    )
    .msuccess("done")
  }
  if (sum.prod) {
    .malert("stabilizing round off errors in SAEM model...")
    .saem <- rxSumProdModel(.saem)
    .msuccess("done")
  }
  if (optExpression) {
    .saem <- rxOptExpr(.saem, "SAEM model")
  }
  .saem <- paste0(
    "params(", paste(.mv, collapse = ","), ")\n",
    .saem
  )
  saveRDS(.saem, .path)

  return(.saem)
}



#' Generate the ETA sensitivities for FO related methods
#'
#' @inheritParams rxSEinner
#' @return rxode2/symengine environment
#' @author Matthew L. Fidler
#' @noRd
.rxGenEtaS <- function(obj, predfn, pkpars = NULL, errfn = NULL,
                       init = NULL, promoteLinSens = TRUE,
                       theta = FALSE, addProp = c("combined2", "combined1")) {
  .s <- .rxGenFun(obj, predfn, pkpars, errfn, init,
    promoteLinSens = promoteLinSens, addProp = addProp
  )
  .etaVars <- NULL
  if (theta && exists("..maxTheta", .s)) {
    .etaVars <- paste0("THETA_", seq(1, .s$..maxTheta), "_")
  } else if (exists("..maxEta", .s)) {
    .etaVars <- paste0("ETA_", seq(1, .s$..maxEta), "_")
  }
  if (length(.etaVars) == 0L) {
    stop("cannot identify parameters for sensitivity analysis\n   with nlmixr an 'eta' initial estimate must use '~'", call. = FALSE)
  }
  .stateVars <- rxState(.s)
  .s <- .rxGenFun(obj, predfn, pkpars, errfn, init,
    promoteLinSens = promoteLinSens,
    addProp = addProp
  )
  .rxJacobian(.s, c(.stateVars, .etaVars))
  .rxSens(.s, .etaVars)
  return(.s)
}

#' Generate the d(err)/d(eta) values for FO related methods
#'
#' @inheritParams rxSEinner
#' @return rxode2/symengine environment
#' @author Matthew L. Fidler
#' @noRd
.rxGenHdEta <- function(obj, predfn, pkpars = NULL, errfn = NULL,
                        init = NULL, pred.minus.dv = TRUE,
                        promoteLinSens = TRUE,
                        theta = FALSE, addProp = c("combined2", "combined1")) {
  ## Equation 19 in Almquist
  .s <- .rxGenEtaS(obj, predfn, pkpars, errfn, init,
    promoteLinSens = promoteLinSens,
    theta = theta, addProp = addProp
  )
  .stateVars <- rxState(.s)
  .grd <- rxExpandFEta_(
    .stateVars, .s$..maxEta,
    ifelse(pred.minus.dv, 1L, 2L)
  )
  if (.useUtf()) {
    .malert("calculate \u2202(f)/\u2202(\u03B7)")
  } else {
    .malert("calculate d(f)/d(eta)")
  }
  rxProgress(dim(.grd)[1])
  on.exit({
    rxProgressAbort()
  })
  .any.zero <- FALSE
  .all.zero <- TRUE
  .ret <- apply(.grd, 1, function(x) {
    .l <- x["calc"]
    .l <- eval(parse(text = .l))
    .ret <- paste0(x["dfe"], "=", rxFromSE(.l))
    .zErr <- suppressWarnings(try(as.numeric(get(x["dfe"], .s)), silent = TRUE))
    if (identical(.zErr, 0)) {
      .any.zero <<- TRUE
    } else if (.all.zero) {
      .all.zero <<- FALSE
    }
    rxTick()
    return(.ret)
  })
  if (.all.zero) {
    stop("none of the predictions depend on 'ETA'", call. = FALSE)
  }
  if (.any.zero) {
    warning("some of the predictions do not depend on 'ETA'", call. = FALSE)
  }
  .s$..HdEta <- .ret
  .s$..pred.minus.dv <- pred.minus.dv
  rxProgressStop()
  return(.s)
}
#' Finalize rxode2 pred based on symengine saved info
#'
#' @param .s Symengine/rxode2 object
#' @inheritParams rxSEinner
#' @return Nothing
#' @author Matthew L Fidler
#' @noRd
.rxFinalizePred <- function(.s, sum.prod = FALSE,
                            optExpression = TRUE) {
  .prd <- get("rx_pred_", envir = .s)
  .prd <- paste0("rx_pred_=", rxFromSE(.prd))
  .r <- get("rx_r_", envir = .s)
  .r <- paste0("rx_r_=", rxFromSE(.r))
  .yj <- paste(get("rx_yj_", envir = .s))
  .yj <- paste0("rx_yj_~", rxFromSE(.yj))
  .lambda <- paste(get("rx_lambda_", envir = .s))
  .lambda <- paste0("rx_lambda_~", rxFromSE(.lambda))
  .hi <- paste(get("rx_hi_", envir = .s))
  .hi <- paste0("rx_hi_~", rxFromSE(.hi))
  .low <- paste(get("rx_low_", envir = .s))
  .low <- paste0("rx_low_~", rxFromSE(.low))
  .lhs0 <- .s$..lhs0
  if (is.null(.lhs0)) .lhs0 <- ""
  .lhs <- .s$..lhs
  if (is.null(.lhs)) .lhs <- ""
  .ddt <- .s$..ddt
  if (is.null(.ddt)) .ddt <- ""
  .s$..pred <- paste(c(
    .s$..stateInfo["state"],
    .lhs0,
    .ddt,
    .yj,
    .lambda,
    .hi,
    .low,
    .prd,
    .r,
    .lhs,
    .s$..stateInfo["statef"],
    .s$..stateInfo["dvid"],
    "tad=tad()",
    "dosenum=dosenum()",
    ""
  ), collapse = "\n")
  .s$..pred.nolhs <- paste(c(
    .s$..stateInfo["state"],
    .lhs0,
    .ddt,
    .yj,
    .lambda,
    .hi,
    .low,
    .prd,
    .r,
    .s$..stateInfo["statef"],
    .s$..stateInfo["dvid"],
    ""
  ), collapse = "\n")
  if (sum.prod) {
    .malert("stabilizing round off errors in predictions or EBE model...")
    .s$..pred <- rxSumProdModel(.s$..pred)
    .msuccess("done")
  }
  if (optExpression) {
    .s$..pred <- rxOptExpr(.s$..pred, "EBE model")
  }
}

#' Generate FOCE without interaction
#'
#' @inheritParams rxSEinner
#' @return rxode2/symengine environment
#' @author Matthew Fidler
.rxGenFoce <- function(obj, predfn, pkpars = NULL, errfn = NULL,
                       init = NULL, pred.minus.dv = TRUE,
                       sum.prod = FALSE,
                       optExpression = TRUE,
                       promoteLinSens = TRUE,
                       theta = FALSE,
                       addProp = c("combined2", "combined1")) {
  .s <- .rxGenHdEta(obj, predfn, pkpars, errfn, init, pred.minus.dv,
    promoteLinSens = promoteLinSens, theta = theta,
    addProp = addProp
  )
  .s$..REta <- NULL
  ## Take etas from rx_r
  eval(parse(text = rxRepR0_(.s$..maxEta)))
  .rxFinalizeInner(.s, sum.prod, optExpression)
  .rxFinalizePred(.s, sum.prod, optExpression)
  .s$..outer <- NULL
  return(.s)
}

#' Generate pieces for FOCEi inner problem
#'
#' @inheritParams rxSEinner
#' @return rxode2/symengine environment
#' @author Matthew L. Fidler
#' @noRd
.rxGenFocei <- function(obj, predfn, pkpars = NULL, errfn = NULL,
                        init = NULL, pred.minus.dv = TRUE,
                        sum.prod = FALSE,
                        optExpression = TRUE,
                        promoteLinSens = TRUE, theta = FALSE,
                        addProp = c("combined2", "combined1")) {
  .s <- .rxGenHdEta(obj, predfn, pkpars, errfn, init, pred.minus.dv,
    promoteLinSens = promoteLinSens, theta = theta,
    addProp = addProp
  )
  .stateVars <- rxState(.s)
  .grd <- rxExpandFEta_(.stateVars, .s$..maxEta, FALSE)
  if (.useUtf()) {
    .malert("calculate \u2202(R\u00B2)/\u2202(\u03B7)")
  } else {
    .malert("calculate d(R^2)/d(eta)")
  }
  rxProgress(dim(.grd)[1])
  on.exit({
    rxProgressAbort()
  })
  .ret <- apply(.grd, 1, function(x) {
    .l <- x["calc"]
    .l <- eval(parse(text = .l))
    .ret <- paste0(x["dfe"], "=", rxFromSE(.l))
    rxTick()
    return(.ret)
  })
  .s$..REta <- .ret
  rxProgressStop()
  .rxFinalizeInner(.s, sum.prod, optExpression)
  .rxFinalizePred(.s, sum.prod, optExpression)
  .s$..outer <- NULL
  return(.s)
}

#' Generate pieces for EBE only problem
#'
#' @inheritParams rxSEinner
#' @return rxode2/symengine environment
#' @author Matthew L. Fidler
#' @noRd
.rxGenEBE <- function(obj, predfn, pkpars = NULL, errfn = NULL,
                      init = NULL, pred.minus.dv = TRUE,
                      sum.prod = FALSE, optExpression = TRUE,
                      theta = FALSE, addProp = c("combined2", "combined1")) {
  .s <- .rxGenFun(obj, predfn, pkpars, errfn, init, addProp = addProp)
  .s$..inner <- NULL
  .s$..outer <- NULL
  .rxFinalizePred(.s, sum.prod, optExpression)
  return(.s)
}

rxSymPyAbsLog <- FALSE
rxSymPyLogSign <- NULL

rxSymPyExpThetas <- NULL
rxSymPyExpEtas <- NULL


#' Setup Pred function based on rxode2 object.
#'
#' This is for the so-called inner problem.
#'
#' @param obj rxode2 object
#' @param predfn Prediction function
#' @param pkpars Pk Pars function
#' @param errfn Error function
#' @param init Initialization parameters for scaling.
#' @param grad Boolaen indicated if the the equations for the
#'     gradient be calculated
#' @param sum.prod A boolean determining if rxode2 should use more
#'     numerically stable sums/products.
#' @param pred.minus.dv Boolean stating if the FOCEi objective
#'     function is based on PRED-DV (like NONMEM).  Default TRUE.
#' @param only.numeric Instead of setting up the sensitivities for
#'     the inner problem, modify the rxode2 to use numeric
#'     differentiation for the numeric inner problem only.
#' @param optExpression Optimize the model text for computer
#'     evaluation.
#' @param interaction Boolean to determine if dR^2/deta is calculated
#'     for FOCEi (not needed for FOCE)
#' @param theta Calculate THETA derivatives instead of ETA
#'     derivatives.  By default FALSE
#' @param addProp one of "combined1" and "combined2"; These are the
#'   two forms of additive+proportional errors supported by
#'   monolix/nonmem:
#'
#' combined1: transform(y)=transform(f)+(a+b*f^c)*eps
#'
#' combined2: transform(y)=transform(f)+(a^2+b^2*f^(2c))*eps
#'
#' @return rxode2 object expanded with predfn and with calculated
#'     sensitivities.
#' @inheritParams rxS
#' @author Matthew L. Fidler
#' @keywords internal
#' @export
#' @importFrom utils find
rxSEinner <- function(obj, predfn, pkpars = NULL, errfn = NULL, init = NULL,
                      grad = FALSE, sum.prod = FALSE, pred.minus.dv = TRUE,
                      only.numeric = FALSE, optExpression = TRUE, interaction = TRUE, ...,
                      promoteLinSens = TRUE, theta = FALSE, addProp = c("combined2", "combined1")) {
  rxReq("symengine")
  addProp <- match.arg(addProp)
  .digest <- digest::digest(list(
    rxModelVars(obj)$md5["parsed_md5"],
    ifelse(is.function(predfn), paste(deparse1(body(predfn)), collapse = ""), ""),
    ifelse(is.function(pkpars), paste(deparse1(body(pkpars)), collapse = ""), ""),
    ifelse(is.function(errfn), paste(deparse1(body(errfn)), collapse = ""), ""),
    init, grad, sum.prod, pred.minus.dv, only.numeric, optExpression,
    interaction, promoteLinSens, theta,
    addProp
  ))
  .path <- file.path(rxTempDir(), paste0("inner-", .digest, ".rds"))
  if (file.exists(.path)) {
    return(readRDS(.path))
  }
  assignInMyNamespace(".rxSupportedFunsExtra", TRUE)
  on.exit({
    assignInMyNamespace(".rxSupportedFunsExtra", FALSE)
  })
  assignInMyNamespace("rxErrEnv.lambda", NULL)
  assignInMyNamespace("rxErrEnv.yj", NULL)
  assignInMyNamespace("rxErrEnv.hi", "1")
  assignInMyNamespace("rxErrEnv.low", "0")
  assignInMyNamespace("rxSymPyExpThetas", NULL)
  assignInMyNamespace("rxSymPyExpEtas", NULL)
  if (only.numeric) {
    .s <- .rxGenEBE(obj, predfn, pkpars, errfn, init, pred.minus.dv,
      sum.prod, optExpression,
      theta = theta, addProp = addProp
    )
  } else if (interaction) {
    .s <- .rxGenFocei(obj, predfn, pkpars, errfn, init, pred.minus.dv,
      sum.prod, optExpression,
      promoteLinSens = promoteLinSens, theta = theta,
      addProp = addProp
    )
  } else {
    .s <- .rxGenFoce(obj, predfn, pkpars, errfn, init, pred.minus.dv,
      sum.prod, optExpression,
      promoteLinSens = promoteLinSens, theta = theta,
      addProp = addProp
    )
  }
  .toRx <- function(x, msg) {
    if (is.null(x)) {
      return(NULL)
    }
    .malert(msg)
    .ret <- rxode2(x)
    .msuccess("done")
    return(.ret)
  }
  if (exists("..maxTheta", .s)) {
    .eventTheta <- rep(0L, .s$..maxTheta)
  } else {
    .eventTheta <- integer()
  }
  if (exists("..maxEta", .s)) {
    .eventEta <- rep(0L, .s$..maxEta)
  } else {
    .eventEta <- integer()
  }
  for (.v in .s$..eventVars) {
    .vars <- as.character(get(.v, envir = .s))
    .vars <- rxGetModel(paste0("rx_lhs=", .vars))$params
    for (.v2 in .vars) {
      .reg <- rex::rex(start, "ETA_", capture(any_numbers), "_", end)
      if (regexpr(.reg, .v2) != -1) {
        .num <- as.numeric(sub(.reg, "\\1", .v2))
        .eventEta[.num] <- 1L
      }
      .reg <- rex::rex(start, "THETA_", capture(any_numbers), "_", end)
      if (regexpr(.reg, .v2) != -1) {
        .num <- as.numeric(sub(.reg, "\\1", .v2))
        .eventTheta[.num] <- 1L
      }
    }
  }
  pred.opt <- NULL
  inner <- .toRx(.s$..inner, "compiling inner model...")
  if (!is.null(inner)) {
    if (sum.prod) {
      .malert("stabilizing round off errors in FD model...")
      .s$..pred.nolhs <- rxSumProdModel(.s$..pred.nolhs)
      .msuccess("done")
    }
    if (optExpression) {
      .s$..pred.nolhs <- rxOptExpr(.s$..pred.nolhs, "FD model")
    }
    .s$..pred.nolhs <- paste(c(
      paste0("params(", paste(inner$params, collapse = ","), ")"),
      .s$..pred.nolhs
    ), collapse = "\n")
    pred.opt <- .s$..pred.nolhs
  }
  .nullInt <- function(x) {
    if (rxode2::rxIs(x, "integer") || rxode2::rxIs(x, "numeric")) {
      return(as.integer(x))
    } else {
      return(integer(0))
    }
  }
  .ret <- list(
    obj = obj,
    inner = inner,
    pred.only = .toRx(.s$..pred, "compiling EBE model..."),
    extra.pars = .s$..extraPars,
    outer = .toRx(.s$..outer),
    pred.nolhs = .toRx(pred.opt, "compiling events FD model..."),
    theta = NULL,
    ## warn=.zeroSens,
    pred.minus.dv = pred.minus.dv,
    log.thetas = .nullInt(.s$..extraTheta[["exp"]]),
    log.etas = .nullInt(.s$..extraEta[["exp"]]),
    extraProps = .s$..extraTheta,
    eventTheta = .eventTheta,
    eventEta = .eventEta
    ## ,
    ## cache.file=cache.file
  )
  class(.ret) <- "rxFocei"
  saveRDS(.ret, .path)
  return(.ret)
}
#' @rdname rxSEinner
#' @export
rxSymPySetupPred <- rxSEinner

## norm <- rxode2("
## d/dt(y)  = dy
## d/dt(dy) = mu*(1-y^2)*dy - y
## ## Initial conditions
## y(0) = 2
## dy(0) = 0
## ## mu
## mu = 1 ## nonstiff; 10 moderately stiff; 1000 stiff
## ")
