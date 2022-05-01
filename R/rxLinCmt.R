#' Find the assignments in R expression
#'
#' @param x R expression
#' @return list of assigned parameters
#' @author Hadley Wickham and Matthew L. Fidler
#' @keywords internal
#' @export
findLhs <- function(x) {
  ## Modified from http://adv-r.had.co.nz/Expressions.html find_assign4
  if (is.atomic(x) || is.name(x)) {
    character()
  } else if (is.call(x)) {
    if ((identical(x[[1]], quote(`<-`)) ||
      identical(x[[1]], quote(`=`)) ||
      identical(x[[1]], quote(`~`))) &&
      is.name(x[[2]])) {
      .lhs <- as.character(x[[2]])
    } else {
      .lhs <- character()
    }
    unique(c(.lhs, unlist(lapply(x, rxode2::findLhs))))
  } else if (is.pairlist(x)) {
    unique(unlist(lapply(x, rxode2::findLhs)))
  } else {
    stop(sprintf("do not know how to handle type '%s'", typeof(x)),
      call. = FALSE
    )
  }
}



.rxDerivedReg <- rex::rex(
  start,
  or(
    group(or("V", "Q", "VP", "VT", "CLD"), number),
    "KA", "VP", "VT", "CLD", "V", "VC", "CL", "VSS", "K", "KE", "KEL",
    "Q", "VT", group("K", number, number), "AOB", "ALPHA", "BETA", "GAMMA",
    "A", "B", "C"
  ),
  end
)

#' Calculate derived parameters for the 1-, 2-, and 3- compartment
#' linear models.
#'
#' This calculates the derived parameters based on what is provided
#' in a data frame or arguments
#'
#' @param ... The input can be:
#'
#'
#'  * A data frame with PK parameters in it; This should ideally
#'  be a data frame with one pk parameter per row since it will
#'  output a data frame with one PK parameter per row.
#'
#'  * PK parameters as either a vector or a scalar
#'
#'
#' @param verbose boolean that when TRUE provides a message about the detected pk parameters
#'   and the detected compartmental model.  By default this is `FALSE`.
#'
#' @param digits represents the number of significant digits for the
#'   output; If the number is zero or below (default), do not round.
#'
#' @return Return a data.frame of derived PK parameters for a 1-, 2-,
#'   or 3-compartment linear model given provided clearances and
#'   volumes based on the inferred model type.
#'
#' The model parameters that will be provided in the data frame are:
#'
#' * `vc`: Central Volume (for 1-, 2- and 3-
#'   compartment models)
#'
#' * `kel`: First-order elimination rate (for 1-, 2-, and
#'   3-compartment models)
#'
#' * `k12`: First-order rate of transfer from central to
#'   first peripheral compartment; (for 2- and 3-compartment models)
#'
#' * `k21`: First-order rate of transfer from first
#'   peripheral to central compartment, (for 2- and 3-compartment
#'   models)
#'
#' * `k13`: First-order rate of transfer from central to
#'   second peripheral compartment; (3-compartment model)
#'
#' * `k31`: First-order rate of transfer from second
#'   peripheral to central compartment (3-compartment model)
#'
#' * `vp`: Peripheral Volume (for 2- and 3- compartment models)
#'
#' * `vp2`: Peripheral Volume for 3rd compartment (3- compartment model)
#'
#' * `vss`: Volume of distribution at steady state; (1-, 2-, and 3-compartment models)
#'
#' * `t12alpha`: \eqn{t_{1/2,\alpha}}; (1-, 2-, and 3-compartment models)
#'
#' * `t12beta`: \eqn{t_{1/2,\beta}}; (2- and 3-compartment models)
#'
#' * `t12gamma`: \eqn{t_{1/2,\gamma}}; (3-compartment model)
#'
#' * `alpha`: \eqn{\alpha}; (1-, 2-, and 3-compartment models)
#'
#' * `beta`: \eqn{\beta}; (2- and 3-compartment models)
#'
#' * `gamma`: \eqn{\beta}; (3-compartment model)
#'
#' * `A`: true `A`; (1-, 2-, and 3-compartment models)
#'
#' * `B`: true `B`; (2- and 3-compartment models)
#'
#' * `C`: true `C`; (3-compartment model)
#'
#' * `fracA`: fractional A; (1-, 2-, and 3-compartment models)
#'
#' * `fracB`: fractional B; (2- and 3-compartment models)
#'
#' * `fracC`: fractional C; (3-compartment model)
#'
#' @author Matthew Fidler and documentation from Justin Wilkins, \email{justin.wilkins@@occams.com}
#'
#' @references Shafer S. L. `CONVERT.XLS`
#'
#' @references Rowland M, Tozer TN. Clinical Pharmacokinetics and Pharmacodynamics: Concepts and Applications (4th). Clipping Williams & Wilkins, Philadelphia, 2010.
#'
#' @examples
#'
#' ## Note that rxode2 parses the names to figure out the best PK parameter
#'
#' params <- rxDerived(cl = 29.4, v = 23.4, Vp = 114, vp2 = 4614, q = 270, q2 = 73)
#'
#' ## That is why this gives the same results as the value before
#'
#' params <- rxDerived(CL = 29.4, V1 = 23.4, V2 = 114, V3 = 4614, Q2 = 270, Q3 = 73)
#'
#' ## You may also use micro-constants alpha/beta etc.
#'
#' params <- rxDerived(k12 = 0.1, k21 = 0.2, k13 = 0.3, k31 = 0.4, kel = 10, v = 10)
#'
#' ## or you can mix vectors and scalars
#'
#' params <- rxDerived(CL = 29.4, V = 1:3)
#'
#' ## If you want, you can round to a number of significant digits
#' ## with the `digits` argument:
#'
#' params <- rxDerived(CL = 29.4, V = 1:3, digits = 2)
#' @export
rxDerived <- function(..., verbose = FALSE, digits = 0) {
  .lst <- list(...)
  if (inherits(.lst[[1]], "data.frame")) {
    .lst <- .lst[[1]]
  }
  .namesU <- toupper(names(.lst))
  .w <- which(regexpr(.rxDerivedReg, .namesU) != -1)
  if (length(.w) > 1L) {
    if (verbose) {
      message("parameters: ", paste(names(.lst)[.w], collapse = ","))
    }
    .linCmt <- .Call(
      `_linCmtParse`, names(.lst)[.w],
      c(
        "with(.lst,.Call(`_calcDerived`, ", "list(", "0, 0, 0, 0, ",
        ", 0, 0, 0, 0),digits))"
      ),
      verbose
    )$str
    .env <- environment()
    return(eval(parse(text = .linCmt), envir = .env))
  } else {
    stop("cannot figure out PK parameters to convert", call. = FALSE)
  }
}

#' Get the linear compartment model true function
#'
#' @inheritParams rxode2
#' @return model with linCmt() replaced with linCmtA()
#' @author Matthew Fidler
#' @export
rxGetLin <- function(model, linCmtSens = c("linCmtA", "linCmtB", "linCmtC"), verbose = FALSE) {
  .mv <- rxGetModel(model)
  if (.Call(`_rxode2_isLinCmt`) == 1L) {
    .vars <- c(.mv$params, .mv$lhs, .mv$slhs)
    return(.Call(
      `_rxode2_linCmtGen`,
      length(.mv$state),
      .vars,
      setNames(
        c(
          "linCmtA" = 1L, "linCmtB" = 2L,
          "linCmtC" = 3L
        )[match.arg(linCmtSens)],
        NULL
      ), verbose
    ))
  } else {
    return(model)
  }
}