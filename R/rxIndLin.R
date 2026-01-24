.rxIndLinStrategy <- "curState"
#' This sets the inductive linearization strategy for matrix building
#'
#' When there is more than one state in a ODE that cannot be
#' separated this specifies how it is incorporated into the matrix
#' exponential.
#'
#' @param strategy The strategy for inductive linearization matrix building
#'
#' * `curState` Prefer parameterizing in terms of the current
#'    state, followed by the first state observed in the term.
#' * `split` Split the parameterization between all states in the
#'   term by dividing each by the number of states in the term and then
#'   adding a matrix term for each state.
#'
#' @return Nothing
#' @author Matthew L. Fidler
#' @export
rxIndLinStrategy <- function(strategy = c("curState", "split")) {
  assignInMyNamespace(".rxIndLinStrategy", match.arg(strategy))
}

.rxIndLinState <- NULL
#' Set the preferred factoring by state
#'
#' @param preferred A list of each state's preferred factorization
#' @return Nothing
#' @author Matthew Fidler
#' @export
rxIndLinState <- function(preferred = NULL) {
  if (is.null(preferred)) {
    return(assignInMyNamespace(".rxIndLinState", preferred))
  }
  checkmate::assertList(preferred, names = "unique")
  lapply(seq_along(preferred), function(x) {
    if (!checkmate::checkCharacter(preferred[[x]],
      names = "unnamed"
    )) {
      stop(sprintf(gettext("'rxIndLinState' list element '%s' must be a unnamed character vector"), names(preferred)[x]),
        call. = FALSE
      )
    }
  })
  assignInMyNamespace(".rxIndLinState", preferred)
}

.rxIndLinLine <- function(line, states, state0) {
  .tmp <- symengine::expand(line) ## Expand line
  .tmp <- rxFromSE(.tmp) ## Convert SE->rxode2; Changes things like X^1 -> X

  .ret <- eval(parse(text = paste0("rxSplitPlusQ(quote(", .tmp, "))")))
  .lst <- list()
  .fullIndLin <- FALSE
  ## This adds the x to the name environment in the .lst list above.
  .addIt <- function(x, name) {
    .cur <- .lst
    if (any(names(.lst) == name)) {
      .cur[[name]] <- c(.cur[[name]], x)
    } else {
      .cur[[name]] <- x
    }
    .lst <<- .cur
  }
  ## This fixes the multiplication so that -1*x becomes -x and *1* becomes *
  ## Also changes y^2*z*1/y to y^1 to y
  ## This collapses a character vector with "*" between it
  ## If we have *1/x this becomes simply /x
  .multCollapse <- function(x) {
    as.character(symengine::S(paste(x, collapse = "*")))
  }
  sapply(.ret, function(x) {
    .mult <- eval(parse(text = paste0("rxSplitPlusQ(quote(", x, "),mult=TRUE)")))
    .mult <- unlist(lapply(.mult, function(x) {
      if (x == "-1") {
        return(x)
      }
      .min <- substr(x, 0, 1)
      if (.min == "-") {
        c("-1", substr(x, 2, nchar(x)))
      } else {
        return(x)
      }
    }))

    .curStates <- unlist(lapply(.mult, function(x) {
      .pars <- rxModelVars(paste0("rx_expr=", x))$params
      return(.pars[.pars %fin% states])
    }))
    .addState <- function(.state, .mult) {
      .num <- which(.mult == .state)
      if (length(.num) == 0) {
        .fullIndLin <<- TRUE
        .rest <- c(.mult, paste0("1/", .state))
        .expr <- .multCollapse(.rest)
        .addIt(.expr, .state)
      } else {
        if (length(.num) > 1) .fullIndLin <<- TRUE
        .num <- .num[1]
        .rest <- .mult[-.num]
        if (length(.rest) == 0) {
          .addIt("1", .state)
        } else if (length(.rest) == 1 && .rest[1] == "-1") {
          .addIt("-1", .state)
        } else {
          if (!.fullIndLin) {
            ## Check to see if this is inductive or
            ## depends on other states.
            .otherStates <- unlist(lapply(.rest, function(x) {
              .pars <- rxModelVars(paste0("rx_expr=", x))$params
              return(.pars[.pars %fin% states])
            }))
            if (length(.otherStates) > 0) .fullIndLin <<- TRUE
          }
          .expr <- .multCollapse(.rest)
          .addIt(.expr, .state)
        }
      }
    }
    if (length(.curStates) == 1) {
      .addState(.curStates, .mult)
    } else if (length(.curStates) > 1) {
      if (.rxIndLinStrategy == "split") {
        ## Use strategy #3, split between all the compartments
        .extra <- paste0("1/", length(.curStates))
        for (.s in .curStates) {
          .addState(.s, c(.mult, .extra))
        }
      } else {
        .pref <- .rxIndLinState[[state0]]
        .addPref <- FALSE
        if (!is.null(.pref)) {
          .pref <- intersect(.pref, .curStates)
          if (length(.pref) > 0) {
            .addState(.pref[1], .mult)
            .addPref <- TRUE
          }
        }
        if (!.addPref) {
          if (any(.curStates == state0)) {
            ## If there is d/dt(state1) = ... (state1*state2*state3) ...
            ## or some other complex expression prefer expressing
            .addState(state0, .mult)
          } else {
            ## Otherwise just use the first state identified.
            ## Strategy #1 just add the first
            .addState(.curStates[1], .mult)
          }
        }
      }
    } else {
      ## This is some "constant" or time-base experssion that
      ## does not depend on state.  Hence it is the extra constant
      .expr <- .multCollapse(.mult)
      .addIt(.expr, "_rxF")
    }
  })
  .ret <- sapply(c(states, "_rxF"), function(x) {
    if (any(names(.lst) == x)) {
      return(gsub("[+][-]", "-", paste(.lst[[x]], collapse = "+")))
    }
    return("0")
  })
  rxTick()
  return(c(.ret, paste0(.fullIndLin)))
}

.indLinInfo <- list()

#' This creates the inductive linearization pieces to integrate into
#' a rxode2 model.
#'
#' @param model rxode2 model type of object
#'
#' @param doConst Replace constants with values; By default this is
#'     `FALSE`.
#'
#' @return List:
#'
#' *  Matrix Exponential initial matrix A for exp(t*A)
#'
#' * Inductive Linerization vector for F
#'
#' * Extra rxode2 code for model generation
#'
#' * Generated C code for model variables; With ME only this will
#' be a list of size 1, otherwise it is a list of size 2.
#'
#' @author Matthew Fidler
#' @noRd
.rxIndLin <- function(model, doConst = FALSE) {
  rxReq("symengine")
  .env <- .rxLoadPrune(model, doConst = doConst)
  .states <- rxState(.env)
  rxProgress(length(.states))
  .minfo("create inductive linearization matrices")
  on.exit({
    rxProgressAbort()
  })
  .ret <- eval(parse(text = rxIndLin_(.states)))
  .w <- setNames(which(.ret[, "indLin"] == "TRUE") - 1, NULL)
  .fullIndLin <- length(.w) > 0
  .ret0 <- .ret[.states, .states, drop = FALSE]
  .ret1 <- .ret[, "_rxF", drop = FALSE]
  .code <- c(paste0("_rxM=", as.vector(.ret0), ";"))
  if (all(.ret1 == "0")) {
    .ret <- list(
      A = .ret0,
      f = NULL,
      fullIndLin = .fullIndLin,
      wIndLin = .w
    )
  } else {
    .ret <- list(
      A = .ret0,
      f = .ret1,
      fullIndLin = .fullIndLin,
      wIndLin = .w
    )
    .code <- c(.code, paste("_rxF=", as.vector(.ret1)))
  }
  assignInMyNamespace(".indLinInfo", .ret)
  rxProgressStop()
  .malert("indLin is in development and results subject to change")
  return(paste(.code, collapse = "\n"))
}
