#' Transition ODEs written in d/dt() format to matrix exponential / inductive linearization format
#'
#' @param model rxode2 model, text, or function
#' @param doConst Replace constants with values; By default this is `FALSE`.
#' @return A character string representing the matrix exponential model code
#' @author Matthew L. Fidler & Antigravity
#' @export
indLin <- function(model, doConst = FALSE) {
  rxReq("symengine")
  
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

#' @rdname indLin
#' @export
rxOdeToIndLin <- indLin

#' @rdname indLin
#' @export
rxToIndLin <- indLin
