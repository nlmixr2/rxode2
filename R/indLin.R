#' Transition ODEs written in d/dt() format to matrix exponential / inductive linearization format
#'
#' @param model rxode2 model, text, or function
#' @param doConst Replace constants with values; By default this is `FALSE`.
#' @param calcSens A character vector of parameter names for which sensitivities should be calculated.
#' @return A character string representing the matrix exponential model code
#' @author Matthew L. Fidler
#' @export
indLin <- function(model, doConst = FALSE, calcSens = NULL) {
  rxReq("symengine")
  if (!is.null(calcSens)) {
    return(rxSensMatExp(model = model, calcSens = calcSens, doConst = doConst))
  }
  
  # 1. Parse model to get model variables and load symengine environment
  .mv <- rxModelVars(model)
  # promoteLinSens=FALSE: a linCmt() the conversion inlines into a rate constant
  # must stay `linCmtA()`.  Promoting it to `linCmtB()` would make the CONVERTED
  # model request Stan sensitivities the source model never had, adding
  # rx__sens_* pseudo-compartments to it (rxode2#1215).
  .env <- .rxLoadPrune(model, doConst = doConst, promoteLinSens = FALSE)
  # `$state` counts linCmt() pseudo-compartments (depot/central/peripheral*),
  # which have no d/dt() behind them: nothing to convert, and the analytic
  # solver keeps handling them.  Converting them emitted cmt()/indLin() lines
  # for a derivative that does not exist (rxode2#1215).
  .states <- setdiff(rxState(.env), .rxLinCmt(.mv))

  if (length(.states) == 0L) {
    stop("No state variables (compartments) found in the model.", call. = FALSE)
  }

  # 2. Call the C/C++ registered function to get inductive linearization matrices
  .ret <- eval(parse(text = rxIndLin_((.states))))
  
  # 3. Extract the coefficient matrix (rows/cols = states) and forcing function vector
  .ret0 <- .ret[.states, .states, drop = FALSE]
  .ret1 <- .ret[, "_rxF", drop = FALSE]
  
  # 4. Construct the new model code
  .code <- c("matExp()")
  
  # Add compartment declarations to ensure ordering
  for (.s in .states) {
    .code <- c(.code, paste0("cmt(", .s, ")"))
  }
  
  # Extract off-diagonal transfer rates and diagonal output/elimination rates
  for (j in seq_along(.states)) {
    .cmt1 <- .states[j]
    .offTerms <- list()
    
    # Off-diagonals: rate of transfer from cmt1 to cmt2
    for (i in seq_along(.states)) {
      .cmt2 <- .states[i]
      if (i != j) {
        .val <- .ret0[i, j]
        if (.val != "0") {
          .kname <- paste0("k_", .cmt1, "_", .cmt2)
          # Check if the expression is already the micro-constant name (either snake_case or dot notation)
          if (.val == .kname || .val == paste0("k.", .cmt1, ".", .cmt2)) {
            .code <- c(.code, paste0("param(", .val, ")"))
          } else {
            .code <- c(.code, paste0(.kname, " = ", .val))
          }
          # eval-in-env, not symengine::S(): S() re-parses, and symengine's
          # parser rejects an ordinary rxode2 name like `eta.Cl`.  The SE
          # layer already binds every model symbol in `.env` (created with
          # symengine::Symbol(), which tolerates dots), so evaluating there is
          # how the rest of rxode2 turns an expression into a Basic.
          .offTerms <- c(.offTerms, eval(parse(text = .val), envir = .env))
        }
      }
    }
    
    # Diagonal column sum: elimination/output rate from cmt1
    .diag <- .ret0[j, j]
    if (.diag != "0" || length(.offTerms) > 0) {
      .sumExpr <- eval(parse(text = .diag), envir = .env)
      for (.t in .offTerms) {
        .sumExpr <- .sumExpr + .t
      }
      # Negate the Basic directly rather than round-tripping through a string:
      # same simplification, and no re-parse to fail on a dotted name.  Expanded
      # for the same reason the sensitivity path expands it (rxode2#1298): a
      # column sum symengine leaves as a product of sums prints as a non-zero
      # elimination that is really zero.
      .elimStr <- as.character(.rxIndLinExpand(-.sumExpr))
      if (!.rxIndLinIsZeroTxt(.elimStr)) {
        .knameOut <- paste0("k_", .cmt1, "_output")
        if (.elimStr == .knameOut || .elimStr == paste0("k.", .cmt1, ".output")) {
          .code <- c(.code, paste0("param(", .elimStr, ")"))
        } else {
          .code <- c(.code, paste0(.knameOut, " = ", .elimStr))
        }
      }
    }
  }
  
  # Forcing functions: indLin property
  for (i in seq_along(.states)) {
    .cmt2 <- .states[i]
    .fVal <- .ret1[i, 1]
    if (.fVal != "0") {
      .code <- c(.code, paste0("indLin(", .cmt2, ") <- ", .fVal))
    }
  }
  
  # 4b. Explicit Jacobian (df/dy) lines.
  #
  # `calc_jac` is already declared and compiled for a matExp() model; it is
  # empty only because nothing emits df()/dy(), which is what sets `found_jac`
  # in the parser.  Emitting it is what lets a Newton iteration or an
  # exponential-Rosenbrock step have a Jacobian at all -- and note the failure
  # mode if it is skipped is a SILENT zero Jacobian, not an error.
  #
  # Differentiate the full right-hand side, `rx__d_dt_<state>__`, which the
  # symengine load already built and which `rxIndLin_()` above has just read.
  # Deriving it instead from the split (A from `.ret0` plus the forcing) would
  # be fewer symengine calls, but the forcing is only available here as text
  # that has been through rxFromSE() -- so it can carry C-level helpers such as
  # `Rx_pow_di()`, which are legal rxode2 syntax but not functions in the
  # symengine environment.  The per-state Basic `rx__indLinForce_<state>__`
  # that would avoid that (R/symengine.R:1152-1161) is only captured for a
  # model already written in `indLin()` form, not on this conversion path.
  # This is the same derivation `rxSensMatExp()` performs at :345-352.
  #
  # `.jacMax` bounds the symbolic work: symengine is the slowest thing in this
  # pipeline and the solver must not depend on it, so above this many states
  # the emission is skipped and the runtime falls back to differencing IndF().
  #
  # Compartments the conversion invents -- an `output` sink created by a
  # `k_<cmt>_output` rate -- are not in `.states` and get no row.  They have no
  # dynamics of their own in the source model, so their row was zero before
  # this change too.
  .jacMax <- getOption("rxode2.indLinJacMaxStates", 24L)
  if (length(.states) <= .jacMax) {
    # Direct symengine::D on Basics held in locals, as rxSensMatExp does:
    # `with(.env, ...)` would not see them, since it ignores the calling frame.
    #
    # Route the name through rxToSE() first.  A compartment may legitimately be
    # called `I`, `E` or `Catalan`, and symengine parses those as constants
    # rather than symbols -- `symengine::S("I")` is the imaginary unit, so
    # differentiating by it fails with "Input is not a SYMBOL".  rxToSE() maps
    # each to the `rx_SymPy_Res_*` name the environment actually binds it under
    # (`.rxSEreserved`, R/symengine.R:524).
    .stateSym <- lapply(.states, function(.s) symengine::S(rxToSE(.s)))
    names(.stateSym) <- .states
    for (.ii in seq_along(.states)) {
      .ddtName <- paste0("rx__d_dt_", .states[.ii], "__")
      if (!exists(.ddtName, envir = .env, inherits = FALSE)) next
      .rhsI <- base::get(.ddtName, envir = .env, inherits = FALSE)
      # A constant derivative -- `d/dt(depot) <- 0`, a common way to declare a
      # dosing-only compartment -- is stored as a plain numeric rather than a
      # symengine Basic, and symengine::D() rejects it outright.  Its row is
      # zero anyway, so there is nothing to emit.
      if (!inherits(.rhsI, "Basic")) next
      for (.jj in seq_along(.states)) {
        .d <- symengine::D(.rhsI, .stateSym[[.jj]])
        .dTxt <- rxFromSE(.d)
        if (!.rxIndLinIsZeroTxt(.dTxt)) {
          .code <- c(.code, paste0("df(", .states[.ii], ")/dy(", .states[.jj],
                                   ") = ", .dTxt))
        }
      }
    }
  }

  # 5. Extract and preserve the non-ODE lines from the original normalized model
  .normModel <- .mv$model["normModel"]
  .lines <- unlist(strsplit(.normModel, "[\n;]"))
  .lines <- trimws(.lines)
  for (.l in .lines) {
    if (.l != "") {
      # Keep lines that do not define ODE derivatives or CMTs (since we output cmt declarations at the top)
      if (!grepl("^d/dt\\(", .l) && !grepl("^cmt\\(", .l)) {
        .code <- c(.code, .l)
      }
    }
  }
  
  # If there are no assignments in the code, append a dummy assignment to avoid "nothing in output queue to write" compiler error
  if (!any(grepl("=", .code) | grepl("<-", .code))) {
    .code <- c(.code, "dummy = 1")
  }
  
  return(paste(.code, collapse = "\n"))
}

#' Is a symengine expression algebraically zero?
#'
#' The one place the indLin/matExp generators decide a term does not exist.
#' Tested on the expanded form: symengine keeps a product of sums as a product,
#' so `rhs - A.X` leaves an expression that prints as non-zero even when every
#' term cancels -- and emitting it as an `indLin()` forcing demotes the whole
#' model from one cached exponential per interval to the fixed-point iteration
#' (rxode2#1298).
#'
#' @param e symengine expression (or anything `rxFromSE()` accepts).
#' @return `TRUE` when the expression is zero.
#' @noRd
#' @author Matthew L. Fidler
.rxIndLinIsZero <- function(e) {
  # rxFromSE() resolves its argument by name (substitute()), so the expansion
  # has to be bound to a plain local before it is handed over.
  .se <- .rxIndLinExpand(e)
  .z <- rxFromSE(.se)
  .rxIndLinIsZeroTxt(.z)
}

#' Is a translated expression string zero?
#'
#' `as.character()` of a symengine zero is `"0"`, but a float zero prints as
#' `"0.0"` (`0.0*a`), so the text test has to accept both spellings.
#'
#' @param t Expression text.
#' @return `TRUE` when the text is a zero.
#' @noRd
#' @author Matthew L. Fidler
.rxIndLinIsZeroTxt <- function(t) {
  t == "0" || t == "0.0" || t == "-0"
}

#' Does an expression reference a compartment?
#'
#' A forcing that reads any compartment -- a physical state or one of the
#' `rx__sens_*` sensitivity compartments -- is what makes the solver classify
#' the model as state dependent and take the iterating driver.
#'
#' @param e symengine expression.
#' @param statesSe The symengine name of each physical state.
#' @return `TRUE` when the expression references a compartment.
#' @noRd
#' @author Matthew L. Fidler
.rxIndLinReadsState <- function(e, statesSe) {
  # An expression whose symbols cannot be read is assumed to read one, which
  # leaves the caller on the length rule rather than on a claim it cannot back.
  .v <- tryCatch(vapply(symengine::free_symbols(e), as.character, character(1)),
                 error = function(.e) NULL)
  if (is.null(.v)) return(TRUE)
  any(.v %in% statesSe) || any(startsWith(.v, "rx__sens_"))
}

#' Expand a symengine expression where the expansion is worth taking
#'
#' `symengine::expand()` is what cancels the algebraically-zero residual the
#' term-wise `rhs - A.X` split leaves behind, and it also collapses the
#' un-simplified `k_*_output` constants the same split produces.  Two rules
#' decide whether to keep it:
#'
#' - Given `statesSe`, an expansion that takes the expression from reading a
#'   compartment to reading none is taken however long it gets, because that
#'   is the difference between the iterating driver and one cached exponential
#'   per interval.  Without it a residual whose state terms cancel but whose
#'   state-free part expands wide -- `(p1+..+p5)*(p6+..+p10) - (a+b)*x - a*x -
#'   b*x` -- keeps `x` in the text and iterates for nothing.
#' - Otherwise the expansion buys no reclassification -- which is every rate
#'   constant, elimination and cross-term coefficient, none of which the
#'   classification reads -- and the result is re-evaluated on every `ME()`
#'   call, so it is kept only when it is no longer than what it replaced.  A
#'   cancellation to `0` always is.
#'
#' @param e symengine expression; anything that is not a `Basic` (a constant
#'   `d/dt(x) <- 0` is stored as a plain numeric) is returned unchanged.
#' @param statesSe The symengine name of each physical state, for an expression
#'   whose state dependence is what will be classified (a forcing).  `NULL`
#'   elsewhere, which leaves only the length rule.
#' @return `e`, or its expansion.
#' @noRd
#' @author Matthew L. Fidler
.rxIndLinExpand <- function(e, statesSe = NULL) {
  if (!inherits(e, "Basic")) return(e)
  .x <- tryCatch(symengine::expand(e), error = function(.e) NULL)
  if (is.null(.x)) return(e)
  if (!is.null(statesSe) && .rxIndLinReadsState(e, statesSe) &&
      !.rxIndLinReadsState(.x, statesSe)) {
    return(.x)
  }
  if (nchar(as.character(.x)) > nchar(as.character(e))) return(e)
  .x
}

#' Total derivative of an indLin/matExp Jacobian-entry expression
#'
#' `expr` is a scalar Jacobian-entry expression, differentiated wrt every
#' symbol it references: the explicit partial wrt `byVar`, plus a chain term
#' for every physical state and every pre-existing sensitivity compartment
#' symbol (the same symbol with `_BY_byVar` appended).  Generalizes
#' `.rxEventSensD2Expr()`'s state+sens coupling to arbitrary sens symbols.
#'
#' @param expr symengine expression (a Jacobian entry, or a total derivative
#'   of one built by a previous call to this function).
#' @param byVar Parameter name to differentiate wrt.
#' @param states Physical state names.
#' @param statesSe The symengine name of each of `states`, in the same order.
#' @param byVarSe The symengine name of `byVar`.
#'
#'   Both are required rather than defaulted from `rxToSE()`: `rxToSE()` parses,
#'   and parsing stops working once symengine `Basic` arithmetic has been done in
#'   the same session (`user function '[[' requires 0 arguments`).  The caller
#'   has to map every name before it touches a Basic.
#' @return symengine expression for the total derivative.
#' @noRd
.rxIndLinTotalD <- function(expr, byVar, states, statesSe, byVarSe) {
  .vars <- tryCatch(
    vapply(symengine::free_symbols(expr), as.character, character(1)),
    error = function(e) character(0)
  )
  .tot <- NULL
  .add <- function(.term) {
    if (is.null(.tot)) .tot <<- .term else .tot <<- .tot + .term
  }
  # Symbol(), not S(): S() re-parses, so it rejects a dotted rxode2 name
  # (`eta.cl`) and reads a compartment called `I`, `E` or `Catalan` as the
  # matching mathematical constant, which D() then refuses to differentiate by.
  # The names differentiated by are symengine-side (rxToSE()); the names built
  # INTO a sensitivity compartment are model-side, since that is what cmt()
  # declared.
  if (byVarSe %in% .vars) {
    .add(symengine::D(expr, symengine::Symbol(byVarSe)))
  }
  for (.i in seq_along(states)) {
    if (!(statesSe[[.i]] %in% .vars)) next
    .dl <- symengine::D(expr, symengine::Symbol(statesSe[[.i]]))
    if (!.rxIndLinIsZero(.dl)) {
      .add(.dl * symengine::Symbol(paste0("rx__sens_", states[[.i]], "_BY_", byVar, "__")))
    }
  }
  for (.s in .vars) {
    if (!startsWith(.s, "rx__sens_") || !endsWith(.s, "__")) next
    .ds <- symengine::D(expr, symengine::Symbol(.s))
    if (.rxIndLinIsZero(.ds)) next
    .target <- paste0(substring(.s, 1L, nchar(.s) - 2L), "_BY_", byVar, "__")
    .add(.ds * symengine::Symbol(.target))
  }
  if (is.null(.tot)) symengine::S("0") else .tot
}

#' Apply `.rxIndLinTotalD()` repeatedly, one variable at a time
#'
#' @param base Starting symengine expression (a Jacobian entry).
#' @param byVars Character vector of variables to differentiate by, in order.
#' @param states Physical state names.
#' @param statesSe The symengine name of each of `states`, in the same order.
#' @param byVarsSe The symengine name of each of `byVars`, in the same order.
#' @return symengine expression for the repeated total derivative.
#' @noRd
.rxIndLinChainD <- function(base, byVars, states, statesSe, byVarsSe) {
  .e <- base
  for (.i in seq_along(byVars)) {
    .e <- .rxIndLinTotalD(.e, byVars[[.i]], states, statesSe, byVarsSe[[.i]])
  }
  .e
}

#' Accumulator for non-depleting (`_nd`) cross-term contributions
#'
#' At second/third order, distinct terms can target the same `from -> to` pair
#' when differentiated parameters coincide; their coefficients must be summed,
#' not emitted as conflicting duplicate-LHS lines.  Accumulates by `(from, to)`
#' key in first-seen order, so emission has one line per pair.
#'
#' @return list with `add(from, to, val)` and `emit()` (character vector of
#'   `k_<from>_<to>_nd = <expr>` lines, skipping pairs that summed to zero).
#' @noRd
.rxIndLinNdAccumulator <- function() {
  .env <- new.env(parent = emptyenv())
  .order <- character(0)
  .add <- function(from, to, val) {
    if (.rxIndLinIsZero(val)) return(invisible())
    .key <- paste0(from, "\r", to)
    if (!exists(.key, envir = .env, inherits = FALSE)) {
      .order <<- c(.order, .key)
    } else {
      # Two families that landed on the same pair can cancel, so the sum is
      # expanded before it is stored -- `emit()`'s zero test reads it back.
      val <- .rxIndLinExpand(base::get(.key, envir = .env, inherits = FALSE) + val)
    }
    assign(.key, val, envir = .env)
    invisible()
  }
  .emit <- function() {
    .lines <- character(0)
    for (.key in .order) {
      .val <- base::get(.key, envir = .env, inherits = FALSE)
      if (.rxIndLinIsZero(.val)) next
      .parts <- strsplit(.key, "\r", fixed = TRUE)[[1L]]
      .lines <- c(.lines, paste0("k_", .parts[1L], "_", .parts[2L], "_nd = ", rxFromSE(.val)))
    }
    .lines
  }
  list(add = .add, emit = .emit)
}

#' Differentiate and expand a matrix exponential model with forward sensitivities
#'
#' The system is split the way [indLin()] splits it: a rate matrix that is
#' constant in the states, expressed as `k_from_to` micro-constants, plus an
#' `indLin()` forcing carrying everything else.  Each sensitivity compartment
#' gets the same rate matrix, the `(dA/dp).X` cross terms as non-depleting
#' transfers, and its own forcing `d(f)/dp + (df/dy).S^p`.
#'
#' @param model rxode2 model, text, or function
#' @param calcSens A character vector of parameter names for which sensitivities should be calculated.
#' @param calcSens2 character vector (or `NULL`) requesting second-order
#'   sensitivities `rx__sens_<x>_BY_<p>_BY_<q>__` (`p` over `calcSens`, `q` over
#'   `calcSens2`; every `calcSens2` element must also be in `calcSens`).
#'   Ignored for `linCmt()` states (those use Stan forward-AD).
#' @param calcSens3 character vector (or `NULL`) requesting third-order
#'   sensitivities `rx__sens_<x>_BY_<p>_BY_<q>_BY_<r>__` (`r` over `calcSens3`).
#'   Requires `calcSens2`; every `calcSens3` element must also be in `calcSens2`.
#' @param doConst Replace constants with values; By default this is `FALSE`.
#' @param env A pre-loaded symengine environment (from `.rxLoadPrune()`) to
#'   reuse instead of reloading `model`; when `NULL` it is built internally.
#' @return A character string representing the matrix exponential sensitivity-expanded model code
#' @author Matthew L. Fidler
#' @export
rxSensMatExp <- function(model, calcSens, calcSens2 = NULL, calcSens3 = NULL, doConst = FALSE, env = NULL) {
  rxReq("symengine")
  if (!is.character(calcSens)) {
    stop("'calcSens' must be a character vector of parameter names.", call. = FALSE)
  }
  if (!is.null(calcSens2)) {
    if (!is.character(calcSens2)) {
      stop("'calcSens2' must be a character vector of parameter names.", call. = FALSE)
    }
    if (!all(calcSens2 %in% calcSens)) {
      stop("'calcSens2' must be a subset of 'calcSens' (every second-order parameter needs its own first-order sensitivity).", call. = FALSE)
    }
  }
  if (!is.null(calcSens3)) {
    if (is.null(calcSens2)) {
      stop("'calcSens3' requires 'calcSens2' to be supplied.", call. = FALSE)
    }
    if (!is.character(calcSens3)) {
      stop("'calcSens3' must be a character vector of parameter names.", call. = FALSE)
    }
    if (!all(calcSens3 %in% calcSens2)) {
      stop("'calcSens3' must be a subset of 'calcSens2' (every third-order parameter needs its own second-order sensitivity).", call. = FALSE)
    }
  }

  # 1. Load model into symengine environment (or reuse a pre-loaded one)
  .mv <- rxModelVars(model)
  if (is.null(env)) {
    .env <- .rxLoadPrune(model, doConst = doConst)
  } else {
    .env <- env
  }
  # Materialize d/dt(<state>) from matExp k_from_to constants (and any indLin()
  # forcing); a no-op for d/dt() input, which already carries rx__d_dt_<state>__.
  .rxInjectMatExpOdes(.env)
  # rxState returns all compartments; for sensitivity we only use non-output,
  # non-linCmt() states -- linCmt sensitivities come from Stan forward-AD, not
  # this Jacobian-based (matExp/indLin) expansion, at any order.
  .states <- setdiff(rxState(.env), c("output", .rxLinCmt(.mv)))

  if (length(.states) == 0L) {
    stop("No state variables (compartments) found in the model.", call. = FALSE)
  }

  # 1b. Map every model name to the symengine name it is bound under, BEFORE
  # touching a single Basic.  rxToSE() parses, and parsing stops working once
  # symengine arithmetic has been done ("user function '[[' requires 0
  # arguments"), so this cannot be deferred into the loops below.  The mapping
  # matters because a compartment may legitimately be called `I`, `E` or
  # `Catalan`, which symengine reads as the matching mathematical constant
  # rather than a symbol (`.rxSEreserved`, R/symengine.R:524).
  # The anonymous wrapper is required: rxToSE() resolves its argument with
  # substitute(), so handing it to vapply() as a bare FUN passes it the literal
  # `X[[i]]` expression and it reports `user function '[[' requires 0 arguments`.
  .toSe <- function(.n) rxToSE(.n)
  .statesSe <- vapply(.states, .toSe, character(1), USE.NAMES = FALSE)
  # Every compartment the generated model can read, which is what the solver
  # classifies a forcing on -- `.states` drops the linCmt() pseudo-compartments
  # (they have no matExp dynamics of their own), but a forcing that reads one
  # still moves within the step and still has to take the iterating driver.
  .cmtSe <- c(.statesSe,
              vapply(.rxLinCmt(.mv), .toSe, character(1), USE.NAMES = FALSE))
  .parSe <- vapply(unique(c(calcSens, calcSens2, calcSens3)), .toSe, character(1))

  # 2. Split the system the way indLin() does: dX/dt = A.X + F(X), with A
  #    CONSTANT IN THE STATES.  That is the premise of the matrix exponential
  #    (rxode2#1186); anything that cannot leave a state-free coefficient is the
  #    nonlinear residual and belongs in the indLin() forcing, where the solver
  #    iterates it.  The Jacobian is still needed -- for the df()/dy() block and
  #    for the forcing -- but it is no longer what the rate constants come from.
  .zero <- symengine::S("0")
  # `.zero +` coerces a plain numeric (how a constant `d/dt(depot) <- 0` is
  # stored) to a Basic, which symengine::D() and the arithmetic below require.
  .rhs <- lapply(.states, function(.s) {
    .v <- paste0("rx__d_dt_", .s, "__")
    if (exists(.v, envir = .env, inherits = FALSE)) {
      .zero + base::get(.v, envir = .env, inherits = FALSE)
    } else {
      .zero
    }
  })
  names(.rhs) <- .states
  # Symbol(), not S(): S() re-parses, so it turns the mapped-away reserved
  # names back into constants and cannot read a dotted rxode2 name at all.
  .stateSym <- lapply(.statesSe, symengine::Symbol)
  names(.stateSym) <- .states
  # Full Jacobian, for the df()/dy() block only.
  .jac <- lapply(.states, function(.i) {
    .row <- lapply(.states, function(.j) .rxIndLinExpand(symengine::D(.rhs[[.i]], .stateSym[[.j]])))
    names(.row) <- .states
    .row
  })
  names(.jac) <- .states
  # The rate matrix, from the same term-wise routing indLin() uses.  rxIndLin_()
  # generates R code that reads the locals `.env` and `.states`, so both have to
  # be in scope here.  `.states` drops `output` and any linCmt() compartment, so
  # the routing treats those as parameters -- correct, since neither has matExp
  # dynamics of its own (linCmt sensitivities come from Stan forward-AD).
  .aTxt <- eval(parse(text = rxIndLin_(.states)))[.states, .states, drop = FALSE]
  # eval-in-env rather than symengine::S(): S() re-parses, and symengine's
  # parser rejects an ordinary rxode2 name such as `eta.Cl` (same reason as
  # indLin():57-62).  A coefficient is state free, so it cannot carry the
  # C-level helpers (Rx_pow_di() and friends) the forcing text can.
  .A <- lapply(.states, function(.i) {
    .row <- lapply(.states, function(.j) {
      .t <- .aTxt[.i, .j]
      if (.t == "0") .zero else .rxIndLinExpand(.zero + eval(parse(text = .t), envir = .env))
    })
    names(.row) <- .states
    .row
  })
  names(.A) <- .states
  # The forcing, as the residual the rate matrix does not reproduce.  Taken
  # symbolically rather than from rxIndLin_()'s `_rxF` column, which is text
  # that has been through rxFromSE() and so can carry C-level helpers such as
  # Rx_pow_di() that do not exist in the symengine environment -- the hazard
  # indLin():105-113 documents.  The split is term wise, so the residual is the
  # same expression; symengine cancels the A.X part outright.  This is also
  # what carries a state-free input term (`d/dt(x) = k0 - ke*x`), which the
  # Jacobian never saw.  Expanded, because symengine leaves `A_ij * X_j` as a
  # product of a sum and a symbol and so cancels only part of it: without the
  # expansion every model with two or more compartments keeps a residual that
  # is algebraically zero, and a structurally non-zero forcing is what puts the
  # solve on the fixed-point iteration instead of one cached exponential per
  # interval (rxode2#1298).
  .force <- lapply(.states, function(.i) {
    .f <- .rhs[[.i]]
    for (.j in .states) {
      .aij <- .A[[.i]][[.j]]
      if (!.rxIndLinIsZero(.aij)) .f <- .f - .aij * .stateSym[[.j]]
    }
    .rxIndLinExpand(.f, .cmtSe)
  })
  names(.force) <- .states
  # elimination from compartment j: -(A[j,j] + sum_{i != j} A[i,j]).  Computed
  # once per compartment: it is read in every sensitivity block, and the same
  # expansion that cancels the forcing is what turns `-q/v-(-q/v-cl/v)` into
  # the `cl/v` the emitted `k_<j>_output` should have said all along.
  .elimAll <- lapply(.states, function(.j) {
    .e <- -.A[[.j]][[.j]]
    for (.i in .states) {
      if (.i != .j) .e <- .e - .A[[.i]][[.j]]
    }
    .rxIndLinExpand(.e)
  })
  names(.elimAll) <- .states
  .elimOf <- function(.j) .elimAll[[.j]]

  # 3. Build model code
  .code <- c("matExp()")
  for (.s in .states) {
    .code <- c(.code, paste0("cmt(", .s, ")"))
  }
  for (.p in calcSens) {
    for (.s in .states) {
      .code <- c(.code, paste0("cmt(rx__sens_", .s, "_BY_", .p, "__)"))
    }
  }
  if (!is.null(calcSens2)) {
    for (.p in calcSens) {
      for (.q in calcSens2) {
        for (.s in .states) {
          .code <- c(.code, paste0("cmt(rx__sens_", .s, "_BY_", .p, "_BY_", .q, "__)"))
        }
      }
    }
  }
  if (!is.null(calcSens3)) {
    for (.p in calcSens) {
      for (.q in calcSens2) {
        for (.r in calcSens3) {
          for (.s in .states) {
            .code <- c(.code, paste0("cmt(rx__sens_", .s, "_BY_", .p, "_BY_", .q, "_BY_", .r, "__)"))
          }
        }
      }
    }
  }

  # 4. Original block: decompose A into k_from_to / k_from_output micro-constants.
  #    NB: rxFromSE() resolves its argument by name (substitute()), so the
  #    symengine entry must be bound to a plain local first.
  for (.j in .states) {
    for (.i in .states) {
      if (.i == .j) next
      .aij <- .A[[.i]][[.j]]
      if (.rxIndLinIsZero(.aij)) next
      .kname <- paste0("k_", .j, "_", .i)
      .val <- rxFromSE(.aij)
      # A matExp()-form input can carry the rate as a model parameter, in which
      # case the coefficient IS the micro-constant name; declare it rather than
      # assigning it to itself (same case indLin():51-56 handles).
      if (.val == .kname || .val == paste0("k.", .j, ".", .i)) {
        .code <- c(.code, paste0("param(", .val, ")"))
      } else {
        .code <- c(.code, paste0(.kname, " = ", .val))
      }
    }
    .elim <- .elimOf(.j)
    if (!.rxIndLinIsZero(.elim)) {
      .knameOut <- paste0("k_", .j, "_output")
      .elimVal <- rxFromSE(.elim)
      if (.elimVal == .knameOut || .elimVal == paste0("k.", .j, ".output")) {
        .code <- c(.code, paste0("param(", .elimVal, ")"))
      } else {
        .code <- c(.code, paste0(.knameOut, " = ", .elimVal))
      }
    }
  }

  # 4a. The primal forcing.  Everything the rate matrix cannot represent goes
  # here, so the states themselves are right for a nonlinear model -- A.X is
  # not f(X).  An indLin() line in the input model was dropped by the preserve
  # loop below and is re-derived here.
  for (.i in .states) {
    .fi <- .force[[.i]]
    if (!.rxIndLinIsZero(.fi)) {
      .code <- c(.code, paste0("indLin(", .i, ") <- ", rxFromSE(.fi)))
    }
  }

  # 4b. Explicit Jacobian (df/dy) lines from the FULL Jacobian (A + dF/dX).
  # matExp()/indLin() models have a no-op dydt(), so the event-sensitivity
  # Jacobian column would be zero; these df()/dy() lines populate calc_jac with
  # the known Jacobian, which handle_evid reads instead for these models.
  # Emitted unconditionally (any eventSens="jump" solve needs it), and only for
  # the physical block -- _esJacColF() (rxode2parseHandleEvid.h) sizes its
  # calc_jac buffer by the physical state count, so rows for the sensitivity
  # compartments would write past it.
  for (.i in .states) {
    for (.j in .states) {
      .jij <- .jac[[.i]][[.j]]
      if (!.rxIndLinIsZero(.jij)) {
        .code <- c(.code, paste0("df(", .i, ")/dy(", .j, ") = ", rxFromSE(.jij)))
      }
    }
  }

  # 5. Sensitivity blocks for each parameter.
  for (.p in calcSens) {
    .pSe <- .parSe[[.p]]
    .pSym <- symengine::Symbol(.pSe)
    .S <- function(.s) paste0("rx__sens_", .s, "_BY_", .p, "__")
    # 5a. Diagonal block: sensitivity states obey the same dynamics as the
    #     originals (reuse the original micro-constants).
    for (.j in .states) {
      for (.i in .states) {
        if (.i == .j) next
        if (!.rxIndLinIsZero(.A[[.i]][[.j]])) {
          .code <- c(.code, paste0("k_", .S(.j), "_", .S(.i), " = k_", .j, "_", .i))
        }
      }
      if (!.rxIndLinIsZero(.elimOf(.j))) {
        .code <- c(.code, paste0("k_", .S(.j), "_output = k_", .j, "_output"))
      }
    }
    # 5b. Cross terms: (dA/dp) * X enter the sensitivity states as non-depleting
    #     transfers X_j -> S^p_i with rate dA[i,j]/dp (matrix entry set directly,
    #     X_j is not depleted).
    for (.j in .states) {
      for (.i in .states) {
        .dAdp <- .rxIndLinExpand(symengine::D(.A[[.i]][[.j]], .pSym))
        if (!.rxIndLinIsZero(.dAdp)) {
          .code <- c(.code, paste0("k_", .j, "_", .S(.i), "_nd = ", rxFromSE(.dAdp)))
        }
      }
    }
    # 5bf. Forcing: the part of the system that is not in A contributes
    #     d(F_i)/dp + sum_j (d(F_i)/dX_j) S^p_j to the sensitivity compartment,
    #     which is exactly .rxIndLinTotalD() (rxode2#1187).
    for (.i in .states) {
      .fi <- .force[[.i]]
      if (.rxIndLinIsZero(.fi)) next
      .g <- .rxIndLinExpand(.rxIndLinTotalD(.fi, .p, .states, .statesSe, .pSe),
                            .cmtSe)
      if (!.rxIndLinIsZero(.g)) {
        .code <- c(.code, paste0("indLin(", .S(.i), ") <- ", rxFromSE(.g)))
      }
    }
  }

  # 5c. Second-order sensitivity blocks (Hessian path, if calcSens2 given).
  # For (p, q), rx__sens_<x>_BY_<p>_BY_<q>__ obeys the total-derivative-wrt-q
  # of the first-order ODE (A = Jacobian, dAdp_ij = d(A[i,j])/dp):
  #   d(S^{pq}_i)/dt = sum_k A_ik * S^{pq}_k                 [homogeneous: reuse]
  #                  + sum_k totalD_q(A_ik)  * S^p_k          [from S^p_k]
  #                  + sum_j dAdp_ij         * S^q_j          [from S^q_j]
  #                  + sum_j totalD_q(dAdp_ij) * X_j          [from X_j]
  # totalD_q() (.rxIndLinTotalD()) is the total derivative wrt q.  Coefficients
  # are computed index-by-index (not by scanning a pre-built sum's free
  # symbols, which fails when a coefficient depends on the state it multiplies).
  if (!is.null(calcSens2)) {
    for (.p in calcSens) {
      .pSe <- .parSe[[.p]]
      .pSym <- symengine::Symbol(.pSe)
      .S1p <- function(.s) paste0("rx__sens_", .s, "_BY_", .p, "__")
      for (.q in calcSens2) {
        .qSe <- .parSe[[.q]]
        .S1q <- function(.s) paste0("rx__sens_", .s, "_BY_", .q, "__")
        .S2 <- function(.s) paste0("rx__sens_", .s, "_BY_", .p, "_BY_", .q, "__")
        # homogeneous block: S^{pq} obeys the same dynamics as X / S^p (reuse).
        for (.j in .states) {
          for (.i in .states) {
            if (.i == .j) next
            if (!.rxIndLinIsZero(.A[[.i]][[.j]])) {
              .code <- c(.code, paste0("k_", .S2(.j), "_", .S2(.i), " = k_", .j, "_", .i))
            }
          }
          if (!.rxIndLinIsZero(.elimOf(.j))) {
            .code <- c(.code, paste0("k_", .S2(.j), "_output = k_", .j, "_output"))
          }
        }
        # cross terms.  Accumulate by (from,to): when p == q (a diagonal
        # Hessian entry), the "from S^p_k" and "from S^q_j" families collapse
        # onto the same source compartment for k == j and MUST be summed, not
        # emitted as two conflicting k_..._nd lines for the same pair.
        .acc <- .rxIndLinNdAccumulator()
        for (.i in .states) {
          for (.k in .states) {
            .c2a <- .rxIndLinTotalD(.A[[.i]][[.k]], .q, .states, .statesSe, .qSe) # from S^p_k
            .acc$add(.S1p(.k), .S2(.i), .c2a)
          }
          for (.j in .states) {
            .dAdp <- .rxIndLinExpand(symengine::D(.A[[.i]][[.j]], .pSym))
            .acc$add(.S1q(.j), .S2(.i), .dAdp) # from S^q_j
            .c2c <- .rxIndLinTotalD(.dAdp, .q, .states, .statesSe, .qSe) # from X_j
            .acc$add(.j, .S2(.i), .c2c)
          }
        }
        .code <- c(.code, .acc$emit())
        # forcing: the first-order forcing differentiated once more.  The second
        # pass chains through the states (-> S^q) and through the
        # rx__sens_*_BY_<p>__ symbols the first pass introduced (-> _BY_p_BY_q),
        # which is the naming .S2() emits.  No accumulator is needed: a forcing
        # is keyed by one target compartment, so p == q still gives one line.
        for (.i in .states) {
          .fi <- .force[[.i]]
          if (.rxIndLinIsZero(.fi)) next
          .g2 <- .rxIndLinExpand(.rxIndLinChainD(.fi, c(.p, .q), .states, .statesSe,
                                                 c(.pSe, .qSe)), .cmtSe)
          if (!.rxIndLinIsZero(.g2)) {
            .code <- c(.code, paste0("indLin(", .S2(.i), ") <- ", rxFromSE(.g2)))
          }
        }
      }
    }
  }

  # 5d. Third-order sensitivity blocks (if calcSens3 given).  For (p, q, r),
  # rx__sens_<x>_BY_<p>_BY_<q>_BY_<r>__ obeys the total-derivative-wrt-r of the
  # second-order ODE (pieces chained one variable further with
  # .rxIndLinChainD()), for every state pair (i,k)/(i,j):
  #   homogeneous:      A_ik                     -> S^{pqr}_k   (reuse)
  #   from S^{pq}_k:    totalD_r(A_ik)
  #   from S^{pr}_k:    totalD_q(A_ik)
  #   from S^p_k:       totalD_r(totalD_q(A_ik))
  #   from S^{qr}_j:    dAdp_ij
  #   from S^q_j:       totalD_r(dAdp_ij)
  #   from S^r_j:       totalD_q(dAdp_ij)
  #   from X_j:         totalD_r(totalD_q(dAdp_ij))
  # The forcing is chained one variable further as well (rxode2#1188).
  if (!is.null(calcSens3)) {
    for (.p in calcSens) {
      .pSe <- .parSe[[.p]]
      .pSym <- symengine::Symbol(.pSe)
      .S1p <- function(.s) paste0("rx__sens_", .s, "_BY_", .p, "__")
      for (.q in calcSens2) {
        .qSe <- .parSe[[.q]]
        .S1q <- function(.s) paste0("rx__sens_", .s, "_BY_", .q, "__")
        .S2pq <- function(.s) paste0("rx__sens_", .s, "_BY_", .p, "_BY_", .q, "__")
        for (.r in calcSens3) {
          .rSe <- .parSe[[.r]]
          .S1r <- function(.s) paste0("rx__sens_", .s, "_BY_", .r, "__")
          .S2pr <- function(.s) paste0("rx__sens_", .s, "_BY_", .p, "_BY_", .r, "__")
          .S2qr <- function(.s) paste0("rx__sens_", .s, "_BY_", .q, "_BY_", .r, "__")
          .S3 <- function(.s) paste0("rx__sens_", .s, "_BY_", .p, "_BY_", .q, "_BY_", .r, "__")
          # homogeneous block: S^{pqr} obeys the same dynamics (reuse).
          for (.j in .states) {
            for (.i in .states) {
              if (.i == .j) next
              if (!.rxIndLinIsZero(.A[[.i]][[.j]])) {
                .code <- c(.code, paste0("k_", .S3(.j), "_", .S3(.i), " = k_", .j, "_", .i))
              }
            }
            if (!.rxIndLinIsZero(.elimOf(.j))) {
              .code <- c(.code, paste0("k_", .S3(.j), "_output = k_", .j, "_output"))
            }
          }
          # cross terms.  Accumulate by (from,to): whenever two of p/q/r
          # coincide, several of the eight families below collapse onto the
          # same source compartment (e.g. p==q==r makes S2pq(k) and S2qr(k)
          # the same name) and their coefficients must be summed.
          .acc <- .rxIndLinNdAccumulator()
          for (.i in .states) {
            for (.k in .states) {
              .Aik <- .A[[.i]][[.k]]
              .acc$add(.S2pq(.k), .S3(.i), .rxIndLinTotalD(.Aik, .r, .states, .statesSe, .rSe)) # from S^{pq}_k
              .acc$add(.S2pr(.k), .S3(.i), .rxIndLinTotalD(.Aik, .q, .states, .statesSe, .qSe)) # from S^{pr}_k
              .acc$add(.S1p(.k), .S3(.i), .rxIndLinChainD(.Aik, c(.q, .r), .states, .statesSe, c(.qSe, .rSe))) # from S^p_k
            }
            for (.j in .states) {
              .dAdp <- .rxIndLinExpand(symengine::D(.A[[.i]][[.j]], .pSym))
              .acc$add(.S2qr(.j), .S3(.i), .dAdp) # from S^{qr}_j
              .acc$add(.S1q(.j), .S3(.i), .rxIndLinTotalD(.dAdp, .r, .states, .statesSe, .rSe)) # from S^q_j
              .acc$add(.S1r(.j), .S3(.i), .rxIndLinTotalD(.dAdp, .q, .states, .statesSe, .qSe)) # from S^r_j
              .acc$add(.j, .S3(.i), .rxIndLinChainD(.dAdp, c(.q, .r), .states, .statesSe, c(.qSe, .rSe))) # from X_j
            }
          }
          .code <- c(.code, .acc$emit())
          # forcing: the second-order forcing differentiated once more.  The
          # third pass chains through the states (-> S^r) and through the
          # rx__sens_*_BY_<p>__ / _BY_<p>_BY_<q>__ symbols the earlier passes
          # introduced (-> _BY_p_BY_q_BY_r), which is what .S3() emits.  Every
          # name it can reach is declared: calcSens3 is a subset of calcSens2,
          # which is a subset of calcSens.  No accumulator, for the same reason
          # as second order -- a forcing is keyed by one target compartment.
          for (.i in .states) {
            .fi <- .force[[.i]]
            if (.rxIndLinIsZero(.fi)) next
            .g3 <- .rxIndLinExpand(.rxIndLinChainD(.fi, c(.p, .q, .r), .states, .statesSe,
                                                   c(.pSe, .qSe, .rSe)), .cmtSe)
            if (!.rxIndLinIsZero(.g3)) {
              .code <- c(.code, paste0("indLin(", .S3(.i), ") <- ", rxFromSE(.g3)))
            }
          }
        }
      }
    }
  }

  # 6. Preserve non-ODE output / lhs lines from the original normalized model
  #    (e.g. cp = central/v); drop structural lines we re-emit above.
  .normModel <- .mv$model["normModel"]
  .lines <- trimws(unlist(strsplit(.normModel, "[\n;]")))
  for (.l in .lines) {
    if (nzchar(.l) && !grepl("^d/dt\\(", .l) && !grepl("^cmt\\(", .l) &&
        !grepl("^matExp\\(", .l) && !grepl("^indLin\\(", .l) &&
        !grepl("^k[_.][^=]*<?=", .l)) {
      .code <- c(.code, .l)
    }
  }

  return(paste(.code, collapse = "\n"))
}

#' @rdname indLin
#' @export
rxOdeToIndLin <- indLin

#' @rdname indLin
#' @export
rxToIndLin <- indLin
