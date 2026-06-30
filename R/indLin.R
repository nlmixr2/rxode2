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

#' Differentiate and expand a matrix exponential model with forward sensitivities
#'
#' @param model rxode2 model, text, or function
#' @param calcSens A character vector of parameter names for which sensitivities should be calculated.
#' @param doConst Replace constants with values; By default this is `FALSE`.
#' @return A character string representing the matrix exponential sensitivity-expanded model code
#' @author Matthew L. Fidler & Antigravity
#' @export
rxSensMatExp <- function(model, calcSens, doConst = FALSE, env = NULL) {
  rxReq("symengine")
  if (!is.character(calcSens)) {
    stop("'calcSens' must be a character vector of parameter names.", call. = FALSE)
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
  # rxState returns all compartments; for sensitivity we only use non-output states
  .states <- setdiff(rxState(.env), "output")

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
