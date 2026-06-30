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
      .elimExpr <- symengine::expand(-.sumExpr)
      .elimStr <- rxFromSE(.elimExpr)
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
rxSensMatExp <- function(model, calcSens, doConst = FALSE) {
  rxReq("symengine")
  if (!is.character(calcSens)) {
    stop("'calcSens' must be a character vector of parameter names.", call. = FALSE)
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
  for (.p in calcSens) {
    for (.s in .states) {
      .code <- c(.code, paste0("cmt(rx__sens_", .s, "_BY_", .p, "__)"))
    }
  }
  
  # Keep track of original transfers and outputs to replicate in sensitivity blocks
  .origTransfers <- list()
  .origOutputs <- list()
  .origForcings <- list()
  
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
          if (.val == .kname || .val == paste0("k.", .cmt1, ".", .cmt2)) {
            .code <- c(.code, paste0("param(", .val, ")"))
            .origTransfers[[paste0(.cmt1, "->", .cmt2)]] <- .val
          } else {
            .code <- c(.code, paste0(.kname, " = ", .val))
            .origTransfers[[paste0(.cmt1, "->", .cmt2)]] <- .kname
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
      .elimExpr <- symengine::expand(-.sumExpr)
      .elimStr <- rxFromSE(.elimExpr)
      if (.elimStr != "0") {
        .knameOut <- paste0("k_", .cmt1, "_output")
        if (.elimStr == .knameOut || .elimStr == paste0("k.", .cmt1, ".output")) {
          .code <- c(.code, paste0("param(", .elimStr, ")"))
          .origOutputs[[.cmt1]] <- .elimStr
        } else {
          .code <- c(.code, paste0(.knameOut, " = ", .elimStr))
          .origOutputs[[.cmt1]] <- .knameOut
        }
      }
    }
    
    # Forcing functions
    .fVal <- .ret1[j, 1]
    if (.fVal != "0") {
      .code <- c(.code, paste0("indLin(", .cmt1, ") <- ", .fVal))
      .origForcings[[.cmt1]] <- .fVal
    }
  }
  
  # 5. Generate sensitivity equations for each parameter
  for (.p in calcSens) {
    # Diagonal block: copy the transfers and outputs of the original model
    for (.transKey in names(.origTransfers)) {
      .parts <- strsplit(.transKey, "->")[[1]]
      .c1 <- .parts[1]
      .c2 <- .parts[2]
      .val <- .origTransfers[[.transKey]]
      .code <- c(.code, paste0("k_rx__sens_", .c1, "_BY_", .p, "___rx__sens_", .c2, "_BY_", .p, "__ = ", .val))
    }
    for (.c1 in names(.origOutputs)) {
      .val <- .origOutputs[[.c1]]
      .code <- c(.code, paste0("k_rx__sens_", .c1, "_BY_", .p, "___output = ", .val))
    }
    
    # Cross-terms: diff(A) * y using non-depleting transfers
    for (j in seq_along(.states)) {
      .cmt1 <- .states[j]
      for (i in seq_along(.states)) {
        .cmt2 <- .states[i]
        .exprVal <- symengine::S(.ret0[i, j])
        .deriv <- symengine::D(.exprVal, symengine::S(.p))
        .derivStr <- rxFromSE(.deriv)
        if (.derivStr != "0") {
          .knameNd <- paste0("k_", .cmt1, "_rx__sens_", .cmt2, "_BY_", .p, "___nd")
          .code <- c(.code, paste0(.knameNd, " = ", .derivStr))
        }
      }
    }
    
    # Differentiated forcing terms: diff(f) * 1
    for (i in seq_along(.states)) {
      .cmt2 <- .states[i]
      .exprF <- symengine::S(.ret1[i, 1])
      .derivF <- symengine::D(.exprF, symengine::S(.p))
      .derivFStr <- rxFromSE(.derivF)
      if (.derivFStr != "0") {
        .code <- c(.code, paste0("indLin(rx__sens_", .cmt2, "_BY_", .p, "__) <- ", .derivFStr))
      }
    }
  }
  
  # 6. Extract and preserve the non-ODE lines from the original normalized model
  .normModel <- .mv$model["normModel"]
  .lines <- unlist(strsplit(.normModel, "[\n;]"))
  .lines <- trimws(.lines)
  for (.l in .lines) {
    if (.l != "") {
      if (!grepl("^d/dt\\(", .l) && !grepl("^cmt\\(", .l)) {
        .code <- c(.code, .l)
      }
    }
  }
  
  if (!any(grepl("=", .code) | grepl("<-", .code))) {
    .code <- c(.code, "dummy = 1")
  }
  
  return(paste(.code, collapse = "\n"))
}

#' @rdname indLin
#' @export
rxOdeToIndLin <- indLin

#' @rdname indLin
#' @export
rxToIndLin <- indLin
