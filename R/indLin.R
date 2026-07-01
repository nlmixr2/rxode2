#' Transition ODEs written in d/dt() format to matrix exponential / inductive linearization format
#'
#' @param model rxode2 model, text, or function
#' @param doConst Replace constants with values; By default this is `FALSE`.
#' @param calcSens A character vector of parameter names for which sensitivities should be calculated.
#' @return A character string representing the matrix exponential model code
#' @author Matthew L. Fidler & Antigravity
#' @export
indLin <- function(model, doConst = FALSE, calcSens = NULL) {
  rxReq("symengine")
  if (!is.null(calcSens)) {
    return(rxSensMatExp(model = model, calcSens = calcSens, doConst = doConst))
  }
  
  # 1. Parse model to get model variables and load symengine environment
  .mv <- rxModelVars(model)
  .env <- .rxLoadPrune(model, doConst = doConst)
  .states <- rxState(.env)
  
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
          .offTerms <- c(.offTerms, symengine::S(.val))
        }
      }
    }
    
    # Diagonal column sum: elimination/output rate from cmt1
    .diag <- .ret0[j, j]
    if (.diag != "0" || length(.offTerms) > 0) {
      .sumExpr <- symengine::S(.diag)
      for (.t in .offTerms) {
        .sumExpr <- .sumExpr + .t
      }
      .elimStr <- as.character(symengine::S(paste0("-(", rxFromSE(.sumExpr), ")")))
      if (.elimStr != "0") {
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

#' Total derivative of an indLin/matExp Jacobian-entry expression
#'
#' `expr` is a scalar expression built from the original-system Jacobian `A`
#' (never a sum of `coefficient * compartment` pieces -- see the "index-driven,
#' not free-symbol-scan" note in the second/third order sections of
#' `rxSensMatExp`), so it is safe to differentiate wrt every symbol it
#' references: the explicit partial wrt `byVar`, plus a chain term for every
#' physical state (`d/dx_l * rx__sens_x_l_BY_byVar__`) and every already-built
#' sensitivity compartment symbol already present in `expr`
#' (`d/d(rx__sens_x_BY_v1..__) * rx__sens_x_BY_v1_.._BY_byVar__`, i.e. the same
#' symbol with `_BY_byVar` appended before the trailing `__`). This mirrors
#' `.rxEventSensD2Expr()`'s state+sens coupling pattern one level more
#' generally (arbitrary pre-existing sens symbols, not just a single `S^p`).
#'
#' @param expr symengine expression (a Jacobian entry, or a total derivative
#'   of one built by a previous call to this function).
#' @param byVar Parameter name to differentiate wrt.
#' @param states Physical state names.
#' @return symengine expression for the total derivative.
#' @noRd
.rxIndLinTotalD <- function(expr, byVar, states) {
  .isZero <- function(.e) {
    .z <- rxFromSE(.e)
    .z == "0" || .z == "0.0" || .z == "-0"
  }
  .vars <- tryCatch(
    vapply(symengine::free_symbols(expr), as.character, character(1)),
    error = function(e) character(0)
  )
  .tot <- NULL
  .add <- function(.term) {
    if (is.null(.tot)) .tot <<- .term else .tot <<- .tot + .term
  }
  if (byVar %in% .vars) {
    .add(symengine::D(expr, symengine::S(byVar)))
  }
  for (.l in states) {
    if (!(.l %in% .vars)) next
    .dl <- symengine::D(expr, symengine::S(.l))
    if (!.isZero(.dl)) {
      .add(.dl * symengine::S(paste0("rx__sens_", .l, "_BY_", byVar, "__")))
    }
  }
  for (.s in .vars) {
    if (!startsWith(.s, "rx__sens_") || !endsWith(.s, "__")) next
    .ds <- symengine::D(expr, symengine::S(.s))
    if (.isZero(.ds)) next
    .target <- paste0(substring(.s, 1L, nchar(.s) - 2L), "_BY_", byVar, "__")
    .add(.ds * symengine::S(.target))
  }
  if (is.null(.tot)) symengine::S("0") else .tot
}

#' Apply `.rxIndLinTotalD()` repeatedly, one variable at a time
#'
#' @param base Starting symengine expression (a Jacobian entry).
#' @param byVars Character vector of variables to differentiate by, in order.
#' @param states Physical state names.
#' @return symengine expression for the repeated total derivative.
#' @noRd
.rxIndLinChainD <- function(base, byVars, states) {
  .e <- base
  for (.v in byVars) .e <- .rxIndLinTotalD(.e, .v, states)
  .e
}

#' Accumulator for non-depleting (`_nd`) cross-term contributions
#'
#' At second and third order, several distinct mathematical terms can target
#' the *same* `from -> to` compartment pair whenever two of the differentiated
#' parameters coincide (e.g. `calcSens2` reusing a `calcSens` parameter, or a
#' `calcSens3` parameter equal to a `calcSens2` one) -- their coefficients must
#' be *summed*, not emitted as separate (conflicting, possibly duplicate-LHS)
#' `k_from_to_nd = ...` lines. This accumulates by `(from, to)` key, in
#' first-seen order, so the final emission has exactly one line per pair.
#'
#' @return list with `add(from, to, val)` and `emit()` (character vector of
#'   `k_<from>_<to>_nd = <expr>` lines, skipping pairs that summed to zero).
#' @noRd
.rxIndLinNdAccumulator <- function() {
  .isZero <- function(.e) {
    .z <- rxFromSE(.e)
    .z == "0" || .z == "0.0" || .z == "-0"
  }
  .env <- new.env(parent = emptyenv())
  .order <- character(0)
  .add <- function(from, to, val) {
    if (.isZero(val)) return(invisible())
    .key <- paste0(from, "\r", to)
    if (!exists(.key, envir = .env, inherits = FALSE)) {
      .order <<- c(.order, .key)
    } else {
      val <- get(.key, envir = .env, inherits = FALSE) + val
    }
    assign(.key, val, envir = .env)
    invisible()
  }
  .emit <- function() {
    .lines <- character(0)
    for (.key in .order) {
      .val <- get(.key, envir = .env, inherits = FALSE)
      if (.isZero(.val)) next
      .parts <- strsplit(.key, "\r", fixed = TRUE)[[1L]]
      .lines <- c(.lines, paste0("k_", .parts[1L], "_", .parts[2L], "_nd = ", rxFromSE(.val)))
    }
    .lines
  }
  list(add = .add, emit = .emit)
}

#' Differentiate and expand a matrix exponential model with forward sensitivities
#'
#' @param model rxode2 model, text, or function
#' @param calcSens A character vector of parameter names for which sensitivities should be calculated.
#' @param calcSens2 character vector (or `NULL`) requesting second-order
#'   (Hessian-path) matrix-exponential sensitivities `rx__sens_<x>_BY_<p>_BY_<q>__`,
#'   where `p` ranges over `calcSens` and `q` over `calcSens2`. Every element of
#'   `calcSens2` must also appear in `calcSens` (its own first-order sensitivity
#'   compartment must already exist for the cross terms to reference). Mirrors
#'   `rxode2(calcSens2=)` for ordinary ODE models (`?rxode2`); unlike that path
#'   (which reuses the generic `.rxSens()`/`rxExpandSens2_()` machinery),
#'   `matExp()`/`indLin()` models cannot contain `d/dt()` lines, so the
#'   second-order sensitivity ODEs are expressed the same way as the first-order
#'   ones: as `k_from_to` micro-constant transfers into the
#'   `rx__sens_<x>_BY_<p>_BY_<q>__` compartments. Ignored (with no compartments
#'   generated) for any state that is part of a `linCmt()` compartment -- linCmt
#'   sensitivities come from Stan forward-AD, not this Jacobian-based expansion.
#' @param calcSens3 character vector (or `NULL`) requesting third-order
#'   sensitivities `rx__sens_<x>_BY_<p>_BY_<q>_BY_<r>__`, where `p` ranges over
#'   `calcSens`, `q` over `calcSens2`, and `r` over `calcSens3`. Requires
#'   `calcSens2` to be supplied; every element of `calcSens3` must also appear
#'   in `calcSens2`. Same linCmt exclusion as `calcSens2`.
#' @param doConst Replace constants with values; By default this is `FALSE`.
#' @param env A pre-loaded symengine environment (as returned by
#'   `.rxLoadPrune()`) to reuse instead of reloading `model` from scratch.
#'   When `NULL` (the default), the environment is built internally from
#'   `model`/`doConst`. Passing an existing `env` lets callers that have
#'   already loaded/pruned the model (e.g. to inject matrix-exponential
#'   `d/dt()` terms or other model modifications) feed that same
#'   environment in, so the sensitivity expansion sees those modifications
#'   instead of re-deriving them from the original `model` text.
#' @return A character string representing the matrix exponential sensitivity-expanded model code
#' @author Matthew L. Fidler & Antigravity
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

  # 2. Build the system Jacobian A[i, j] = d(d/dt X_i)/d X_j directly from the
  #    materialized derivatives.  For linear models A is constant; for nonlinear
  #    models (e.g. Michaelis-Menten) the entries are state-dependent expressions.
  .zero <- symengine::S("0")
  .isZero <- function(.e) {
    .z <- rxFromSE(.e)
    .z == "0" || .z == "0.0" || .z == "-0"
  }
  .rhs <- lapply(.states, function(.s) {
    .v <- paste0("rx__d_dt_", .s, "__")
    if (exists(.v, envir = .env, inherits = FALSE)) {
      base::get(.v, envir = .env, inherits = FALSE)
    } else {
      .zero
    }
  })
  names(.rhs) <- .states
  .stateSym <- lapply(.states, function(.s) symengine::S(.s))
  names(.stateSym) <- .states
  .A <- lapply(.states, function(.i) {
    .row <- lapply(.states, function(.j) symengine::D(.rhs[[.i]], .stateSym[[.j]]))
    names(.row) <- .states
    .row
  })
  names(.A) <- .states
  # elimination from compartment j: -(A[j,j] + sum_{i != j} A[i,j])
  .elimOf <- function(.j) {
    .e <- -.A[[.j]][[.j]]
    for (.i in .states) {
      if (.i != .j) .e <- .e - .A[[.i]][[.j]]
    }
    .e
  }

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
      if (!.isZero(.aij)) {
        .code <- c(.code, paste0("k_", .j, "_", .i, " = ", rxFromSE(.aij)))
      }
    }
    .elim <- .elimOf(.j)
    if (!.isZero(.elim)) {
      .code <- c(.code, paste0("k_", .j, "_output = ", rxFromSE(.elim)))
    }
  }

  # 4b. Explicit Jacobian (df/dy) lines from the already-computed `.A` matrix.
  #
  # matExp()/indLin() models solve the primal system by matrix-exponential
  # propagation, not by evaluating a symbolic RHS -- so the compiled `dydt()`
  # never writes its output array for these models (it is a no-op stub). The
  # event-sensitivity ("jump") dtau/lag row (`handle_evid`) normally sources
  # its Jacobian column from a central difference of `dydt`, which is
  # therefore silently always zero for matExp models. `df(<i>)/dy(<j>) <-
  # <A[i][j]>` lines are legal in a matExp() model (only `d/dt()` lines are
  # forbidden) and populate `calc_jac` with the real, already-known Jacobian
  # -- `handle_evid` uses `calc_jac` instead of `dydt` for matExp models
  # specifically (see `_rxEsUseCalcJac` / `.rxSetEventSensDims()`). Emitted
  # unconditionally (not just when `calcSens2`/`calcSens3` are given) since
  # any `eventSens="jump"` solve of this model -- even 1st order -- needs it.
  for (.i in .states) {
    for (.j in .states) {
      .aij <- .A[[.i]][[.j]]
      if (!.isZero(.aij)) {
        .code <- c(.code, paste0("df(", .i, ")/dy(", .j, ") = ", rxFromSE(.aij)))
      }
    }
  }

  # 5. Sensitivity blocks for each parameter.
  for (.p in calcSens) {
    .pSym <- symengine::S(.p)
    .S <- function(.s) paste0("rx__sens_", .s, "_BY_", .p, "__")
    # 5a. Diagonal block: sensitivity states obey the same dynamics as the
    #     originals (reuse the original micro-constants).
    for (.j in .states) {
      for (.i in .states) {
        if (.i == .j) next
        if (!.isZero(.A[[.i]][[.j]])) {
          .code <- c(.code, paste0("k_", .S(.j), "_", .S(.i), " = k_", .j, "_", .i))
        }
      }
      if (!.isZero(.elimOf(.j))) {
        .code <- c(.code, paste0("k_", .S(.j), "_output = k_", .j, "_output"))
      }
    }
    # 5b. Cross terms: (dA/dp) * X enter the sensitivity states as non-depleting
    #     transfers X_j -> S^p_i with rate dA[i,j]/dp (matrix entry set directly,
    #     X_j is not depleted).
    for (.j in .states) {
      for (.i in .states) {
        .dAdp <- symengine::D(.A[[.i]][[.j]], .pSym)
        if (!.isZero(.dAdp)) {
          .code <- c(.code, paste0("k_", .j, "_", .S(.i), "_nd = ", rxFromSE(.dAdp)))
        }
      }
    }
  }

  # 5c. Second-order sensitivity blocks (Hessian path, if calcSens2 given).
  #
  # For a fixed pair (p in calcSens, q in calcSens2), the compartments
  # rx__sens_<x>_BY_<p>_BY_<q>__ obey the total derivative wrt q of the
  # first-order sensitivity ODE (see the plan doc / project memory for the
  # full derivation): writing A for the Jacobian and dAdp_ij = d(A[i,j])/dp,
  #   d(S^{pq}_i)/dt = sum_k A_ik * S^{pq}_k                 [homogeneous: reuse]
  #                  + sum_k totalD_q(A_ik)  * S^p_k          [from S^p_k]
  #                  + sum_j dAdp_ij         * S^q_j          [from S^q_j]
  #                  + sum_j totalD_q(dAdp_ij) * X_j          [from X_j]
  # where totalD_q() is the *total* derivative wrt q (explicit + chained
  # through every physical state's own S^q, via .rxIndLinTotalD()).  Each
  # coefficient is computed directly from its (i,j) or (i,k) indices -- NOT by
  # symbolically expanding a pre-built sum and differentiating wrt each free
  # symbol, which would be wrong whenever a coefficient depends on the same
  # state it multiplies (the classic reason the first-order block above builds
  # dAdp_ij index-by-index rather than assembling one expression and scanning
  # its free symbols).
  if (!is.null(calcSens2)) {
    for (.p in calcSens) {
      .pSym <- symengine::S(.p)
      .S1p <- function(.s) paste0("rx__sens_", .s, "_BY_", .p, "__")
      for (.q in calcSens2) {
        .S1q <- function(.s) paste0("rx__sens_", .s, "_BY_", .q, "__")
        .S2 <- function(.s) paste0("rx__sens_", .s, "_BY_", .p, "_BY_", .q, "__")
        # homogeneous block: S^{pq} obeys the same dynamics as X / S^p (reuse).
        for (.j in .states) {
          for (.i in .states) {
            if (.i == .j) next
            if (!.isZero(.A[[.i]][[.j]])) {
              .code <- c(.code, paste0("k_", .S2(.j), "_", .S2(.i), " = k_", .j, "_", .i))
            }
          }
          if (!.isZero(.elimOf(.j))) {
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
            .c2a <- .rxIndLinTotalD(.A[[.i]][[.k]], .q, .states) # from S^p_k
            .acc$add(.S1p(.k), .S2(.i), .c2a)
          }
          for (.j in .states) {
            .dAdp <- symengine::D(.A[[.i]][[.j]], .pSym)
            .acc$add(.S1q(.j), .S2(.i), .dAdp) # from S^q_j
            .c2c <- .rxIndLinTotalD(.dAdp, .q, .states) # from X_j
            .acc$add(.j, .S2(.i), .c2c)
          }
        }
        .code <- c(.code, .acc$emit())
      }
    }
  }

  # 5d. Third-order sensitivity blocks (if calcSens3 given).
  #
  # For a fixed triple (p in calcSens, q in calcSens2, r in calcSens3),
  # rx__sens_<x>_BY_<p>_BY_<q>_BY_<r>__ obeys the total derivative wrt r of the
  # second-order ODE above.  Applying the product rule to each of its four
  # additive pieces gives one "coefficient changes" and one "compartment
  # changes" contribution per piece; collecting by source compartment (using
  # the same totalD_q(A_ik)/dAdp_ij pieces already computed above, now chained
  # one variable further with `.rxIndLinChainD()`) gives, for every pair of
  # states (i,k)/(i,j):
  #   homogeneous:      A_ik                     -> S^{pqr}_k   (reuse)
  #   from S^{pq}_k:    totalD_r(A_ik)
  #   from S^{pr}_k:    totalD_q(A_ik)
  #   from S^p_k:       totalD_r(totalD_q(A_ik))
  #   from S^{qr}_j:    dAdp_ij
  #   from S^q_j:       totalD_r(dAdp_ij)
  #   from S^r_j:       totalD_q(dAdp_ij)
  #   from X_j:         totalD_r(totalD_q(dAdp_ij))
  # (See the project plan / memory for the full derivation and the
  # p/q/r-permutation symmetry check that validates it.)
  if (!is.null(calcSens3)) {
    for (.p in calcSens) {
      .pSym <- symengine::S(.p)
      .S1p <- function(.s) paste0("rx__sens_", .s, "_BY_", .p, "__")
      for (.q in calcSens2) {
        .S1q <- function(.s) paste0("rx__sens_", .s, "_BY_", .q, "__")
        .S2pq <- function(.s) paste0("rx__sens_", .s, "_BY_", .p, "_BY_", .q, "__")
        for (.r in calcSens3) {
          .S1r <- function(.s) paste0("rx__sens_", .s, "_BY_", .r, "__")
          .S2pr <- function(.s) paste0("rx__sens_", .s, "_BY_", .p, "_BY_", .r, "__")
          .S2qr <- function(.s) paste0("rx__sens_", .s, "_BY_", .q, "_BY_", .r, "__")
          .S3 <- function(.s) paste0("rx__sens_", .s, "_BY_", .p, "_BY_", .q, "_BY_", .r, "__")
          # homogeneous block: S^{pqr} obeys the same dynamics (reuse).
          for (.j in .states) {
            for (.i in .states) {
              if (.i == .j) next
              if (!.isZero(.A[[.i]][[.j]])) {
                .code <- c(.code, paste0("k_", .S3(.j), "_", .S3(.i), " = k_", .j, "_", .i))
              }
            }
            if (!.isZero(.elimOf(.j))) {
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
              .acc$add(.S2pq(.k), .S3(.i), .rxIndLinTotalD(.Aik, .r, .states)) # from S^{pq}_k
              .acc$add(.S2pr(.k), .S3(.i), .rxIndLinTotalD(.Aik, .q, .states)) # from S^{pr}_k
              .acc$add(.S1p(.k), .S3(.i), .rxIndLinChainD(.Aik, c(.q, .r), .states)) # from S^p_k
            }
            for (.j in .states) {
              .dAdp <- symengine::D(.A[[.i]][[.j]], .pSym)
              .acc$add(.S2qr(.j), .S3(.i), .dAdp) # from S^{qr}_j
              .acc$add(.S1q(.j), .S3(.i), .rxIndLinTotalD(.dAdp, .r, .states)) # from S^q_j
              .acc$add(.S1r(.j), .S3(.i), .rxIndLinTotalD(.dAdp, .q, .states)) # from S^r_j
              .acc$add(.j, .S3(.i), .rxIndLinChainD(.dAdp, c(.q, .r), .states)) # from X_j
            }
          }
          .code <- c(.code, .acc$emit())
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
