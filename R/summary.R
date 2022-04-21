#' Print expanded information about the rxode2 object.
#'
#' This prints the expanded information about the rxode2 object.
#'
#' @param object rxode2 object
#' @param ... Ignored parameters
#' @return object is returned
#' @author Matthew L.Fidler
#' @export
summary.rxode2 <- function(object, ...) {
  print.rxode2(object, rxSuppress = TRUE)
  summary.rxDll(object$cmpMgr$rxDll(), noprint = TRUE)
  invisible(object)
}

#' @export
summary.rxC <- function(object, ...) {
  cat(sprintf("//C file: %s\n", object))
  cat("//\n")
  suppressWarnings(cat(paste0(paste(readLines(object), collapse = "\n"), "\n")))
}

#' Summary of rxDll object
#'
#' This gives expanded information about the rxDll object
#'
#' @param object RxDll object
#'
#' @param ... Other arguments.  Includes `noprint`, which is a
#'     logical telling if the object should print the rxDll object
#'     first. By default this is FALSE
#'
#' @return object is returned
#'
#' @keywords internal
#' @author Matthew L.Fidler
#' @export
summary.rxDll <- function(object, ...) {
  .args <- as.list(match.call(expand.dots = TRUE))
  if (any(names(.args) == "noprint")) {
    .noprint <- .args$noprint
  } else {
    .noprint <- FALSE
  }
  if (!.noprint) {
    print(object)
  }
  cat(sprintf("DLL: %s\n", getOption("rxode2.dll.print", rxode2::rxDll(object))))
  cat(sprintf(
    "Jacobian: %s\n",
    ifelse(rxode2::rxModelVars(object)$jac == "fulluser", "Full User Specified",
      "Full Internally Calculated"
    )
  ))
  print(coef(object))
  if (length(rxode2::rxLhs(object)) > 0) {
    cat("\nCalculated Variables:\n")
    print(rxode2::rxLhs(object))
  }
  .mv <- rxModelVars(object)
  if (length(.mv$indLin) > 0) {
    cat(cli::cli_format_method({
      .h2("Inductive Linearization Matrix/Matrices:")
    }), "\n")
    print(.mv$indLin)
  }
  if (!is.na(object$linCmtM)) {
    .tmp <- object$linCmtM
  } else {
    .tmp <- setNames(rxode2::rxModelVars(object)$model["normModel"], NULL)
  }
  class(.tmp) <- "rxModelText"
  print(.tmp)
  return(invisible(object))
}


#' @export
summary.rxSolve <- function(object, ...) {
  if (rxIs(object, "rxSolve")) {
    cat(cli::cli_format_method({
      .h2(crayon::bold("Summary of Solved rxode2 object"))
    }), sep = "\n")
    .model <- object$model
    print(.model, .summary = TRUE)
    print(object, .summary = TRUE, ...)
  } else {
    class(object) <- "data.frame"
    NextMethod("summary", object)
  }
}
