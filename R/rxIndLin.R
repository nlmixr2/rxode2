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
#' @details
#'
#' This no longer affects the conversion.  The `matExp()` rate matrix has to be
#' constant in the states, and dividing any one state out of a multi-state
#' product always leaves a coefficient that still reads a state, so such a term
#' goes to the `indLin()` forcing whichever state is preferred.  The setting is
#' kept so existing code keeps working.
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
#'
#' @details
#'
#' Like [rxIndLinStrategy()], this no longer affects the conversion: a
#' multi-state product cannot yield a state-free rate constant whichever state
#' is factored out, so it becomes an `indLin()` forcing instead.  The setting is
#' kept so existing code keeps working.
#'
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
  .tmp <- tryCatch(
    symengine::expand(line), ## Expand line; simplifies X^1->X etc
    error = function(e) {
      ## Re-parsing is the usual fallback for complex exprs, but symengine
      ## cannot parse a dotted identifier (`eta.Cl`) and that is an ordinary
      ## rxode2 name.  Expansion is a simplification, not a requirement, so
      ## give up on it rather than on the conversion.
      tryCatch(symengine::S(as.character(line)),
               error = function(e2) line)
    }
  )
  ## rxFromSE() needs a Basic; if the fallback above handed back the original
  ## object it is already one, but a plain string must pass through untouched.
  .tmp <- if (is.character(.tmp)) .tmp else rxFromSE(.tmp)

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
    .txt <- paste(x, collapse = "*")
    # symengine cannot parse a dotted identifier (`eta.Cl`), which is an
    # ordinary rxode2 name, and this call is only tidying the result
    # cosmetically.  Fall back to the plain product rather than failing the
    # whole conversion: `-1*x` instead of `-x` is uglier but identical.
    tryCatch(as.character(symengine::S(.txt)),
             error = function(e) .txt)
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
      return(.pars[.pars %in% states])
    }))
    ## The A matrix of a matExp() model has to be constant in the states --
    ## that is the premise of the matrix exponential, and it is what
    ## rxode2parseHandleEvid.h's jump code already assumes.  So a term only
    ## belongs in A when dividing out one state leaves a coefficient that
    ## reads no state at all; anything else is the nonlinear residual and
    ## goes to the forcing, where the solver's Picard iteration can chase it
    ## (rxode2#1186).
    .addForcing <- function(.mult) {
      .fullIndLin <<- TRUE
      .addIt(.multCollapse(.mult), "_rxF")
    }
    .addState <- function(.state, .mult) {
      .num <- which(.mult == .state)
      if (length(.num) == 0) {
        ## The state is in there but not as a bare factor (vmax/(km+central)),
        ## so no division by it produces a linear coefficient.  This used to
        ## multiply in `1/state`, which also made A singular at state == 0.
        .addForcing(.mult)
      } else {
        .num <- .num[1]
        .rest <- .mult[-.num]
        if (length(.rest) == 0) {
          .addIt("1", .state)
        } else if (length(.rest) == 1 && .rest[1] == "-1") {
          .addIt("-1", .state)
        } else {
          ## Whatever is left after dividing out the state still has to be
          ## state free to be a legal rate constant.
          .otherStates <- unlist(lapply(.rest, function(x) {
            .pars <- rxModelVars(paste0("rx_expr=", x))$params
            return(.pars[.pars %in% states])
          }))
          if (length(.otherStates) > 0) {
            .addForcing(.mult)
          } else {
            .addIt(.multCollapse(.rest), .state)
          }
        }
      }
    }
    if (any(grepl("(^|[^A-Za-z0-9._])linCmt[AB]?[(]", .mult))) {
      ## A linCmt() call reads the solved compartments, which are states of the
      ## system even though they carry no d/dt().  A coefficient built from one
      ## is therefore not constant over the matrix-exponential step, so the term
      ## is the nonlinear residual and belongs in the forcing, where the solver
      ## re-evaluates it as it iterates (rxode2#1215).
      .addForcing(.mult)
    } else if (length(.curStates) == 1) {
      .addState(.curStates, .mult)
    } else if (length(.curStates) > 1) {
      ## A product of two states (or a state with itself) can never leave a
      ## state-free coefficient whichever one is divided out, so it goes to
      ## the forcing whole.  Routing here rather than inside .addState() also
      ## stops the "split" strategy emitting the same forcing term n times.
      .addForcing(.mult)
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

#' Matrix-exponential cache statistics for inductive linearization
#'
#' Reports whether the per-thread content-addressed exponential cache used by
#' `matExp()`/`indLin()` solving actually served anything.  The same numbers ride
#' out of an [rxSolve()] as `$counts$dadt` (computed) and `$counts$jac` (reused),
#' but a fit drives the solver directly and never builds that data frame, so this
#' is the only way to observe the cache from inside one.
#'
#' @param reset logical; when `TRUE` zero the counters after reading, so a
#'   measurement is one call before the work and one after
#'
#' @return named numeric vector: `computed` (exponentials actually
#'   exponentiated), `reused` (exponentials served from a cache slot), `noSlot`
#'   (the subset of `computed` that ran on a thread with no cache slot of its
#'   own, so it could not have hit -- nonzero means the pool was never sized or
#'   is smaller than the thread team), and `slots` (the current pool size)
#'
#' @author Matthew L. Fidler
#' @keywords internal
#' @export
#' @examples
#' rxIndLinExpStats()
rxIndLinExpStats <- function(reset = FALSE) {
  checkmate::assertLogical(reset, len = 1, any.missing = FALSE)
  .Call(`_rxode2_rxIndLinExpStats`, reset)
}
