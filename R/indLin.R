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
  # rxState returns all compartments; for sensitivity we only use non-output states
  .allStates <- rxState(.env)
  .states <- setdiff(.allStates, "output")

  if (length(.states) == 0L) {
    stop("No state variables (compartments) found in the model.", call. = FALSE)
  }

  # 2. Read the rate matrix directly from k_from_to variables in the symengine env.
  #    A[to, from] = k_from_to (off-diagonal rate of transfer from -> to)
  #    Diagonal A[i,i] = -(k_i_output + sum of k_i_j for j != i)
  .transfers <- list()   # list of list(from, to, name, expr)
  .outputs   <- list()   # list of list(from, name, expr)

  for (.p in ls(envir = .env, all.names = TRUE)) {
    .m <- regexec("^k[_.]([^_.]+)[_.]([^_.]+)$", .p)[[1L]]
    if (length(.m) == 1L) next
    .from <- substring(.p, .m[2L], .m[2L] + attr(.m, "match.length")[2L] - 1L)
    .to   <- substring(.p, .m[3L], .m[3L] + attr(.m, "match.length")[3L] - 1L)
    if (!(.from %in% .states)) next
    .expr <- base::get(.p, envir = .env, inherits = FALSE)
    if (.to == "output") {
      .outputs[[.from]] <- list(name = .p, expr = .expr)
    } else if (.to %in% .states) {
      .transfers[[paste0(.from, "->", .to)]] <- list(from = .from, to = .to, name = .p, expr = .expr)
    }
  }

  # 3. Build model code
  .code <- c("matExp()")

  # compartment declarations for original states
  for (.s in .states) {
    .code <- c(.code, paste0("cmt(", .s, ")"))
  }
  # compartment declarations for sensitivity states
  for (.p in calcSens) {
    for (.s in .states) {
      .code <- c(.code, paste0("cmt(rx__sens_", .s, "_BY_", .p, "__)"))
    }
  }

  # original rate assignments (these define the k_ variables for the matExp solver)
  for (.key in names(.transfers)) {
    .t <- .transfers[[.key]]
    .rateExpr <- .t$expr  # force evaluation before rxFromSE (avoid NSE $ issue)
    .code <- c(.code, paste0(.t$name, " = ", rxFromSE(.rateExpr)))
  }
  for (.s in names(.outputs)) {
    .o <- .outputs[[.s]]
    .outExpr <- .o$expr  # force evaluation
    .code <- c(.code, paste0(.o$name, " = ", rxFromSE(.outExpr)))
  }

  # forcing functions for original states (e.g. f(), alag(), non-linear forcings)
  .fvars <- ls(.env)[grepl("^\\.\\.(lhs|forcing)", ls(.env))]
  # (these are carried in by the existing non-ODE lines below)

  # 4. Sensitivity blocks for each parameter
  for (.p in calcSens) {
    .pSym <- symengine::S(.p)

    # Diagonal block: sensitivity states share same transfers and outputs as originals
    for (.key in names(.transfers)) {
      .t <- .transfers[[.key]]
      .c1 <- .t$from; .c2 <- .t$to
      .code <- c(.code, paste0(
        "k_rx__sens_", .c1, "_BY_", .p, "___rx__sens_", .c2, "_BY_", .p, "__ = ",
        .t$name
      ))
    }
    for (.s in names(.outputs)) {
      .code <- c(.code, paste0(
        "k_rx__sens_", .s, "_BY_", .p, "___output = ",
        .outputs[[.s]]$name
      ))
    }

    # Cross-terms: (dA/dθ) * X  ─ non-depleting forcing into sens states
    .forcings <- stats::setNames(as.list(rep("0", length(.states))), .states)

    for (.key in names(.transfers)) {
      .t <- .transfers[[.key]]
      .tExpr <- .t$expr  # force evaluation
      .deriv <- symengine::D(.tExpr, .pSym)
      .dStr <- rxFromSE(.deriv)
      if (.dStr == "0") next
      .fFrom <- .forcings[[.t$from]]
      .fTo   <- .forcings[[.t$to]]
      .forcings[[.t$from]] <- paste0(.fFrom, "-(", .dStr, ")*", .t$from)
      .forcings[[.t$to]]   <- paste0(.fTo,   "+(", .dStr, ")*", .t$from)
    }
    for (.s in names(.outputs)) {
      .outExpr <- .outputs[[.s]]$expr  # force evaluation
      .deriv <- symengine::D(.outExpr, .pSym)
      .dStr <- rxFromSE(.deriv)
      if (.dStr == "0") next
      .fFrom <- .forcings[[.s]]
      .forcings[[.s]] <- paste0(.fFrom, "-(", .dStr, ")*", .s)
    }

    for (.s in .states) {
      .f <- .forcings[[.s]]
      if (!is.null(.f) && .f != "0") {
        .code <- c(.code, paste0("indLin(rx__sens_", .s, "_BY_", .p, "__) <- ", .f))
      }
    }
  }

  # 5. Preserve non-ODE, non-cmt lines from the original normalized model
  .normModel <- .mv$model["normModel"]
  .lines <- trimws(unlist(strsplit(.normModel, "[\n;]")))
  for (.l in .lines) {
    if (nzchar(.l) && !grepl("^d/dt\\(", .l) && !grepl("^cmt\\(", .l) &&
        !grepl("^matExp\\(", .l)) {
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
