#' @rdname rxEvid
#' @export
format.rxEvid <- function(x, ...) {
  .x <- unclass(x)
  format(as.character.rxEvid(.x), align = "left", width = 12)
}

#' @rdname rxEvid
#' @export
format.rxRateDur <- function(x, ...) {
  .x <- unclass(x)
  format(as.character.rxRateDur(.x), align = "left")
}

.fmt3 <- function(name, bound, access) {
  paste0(
    crayon::bold(name), " (", crayon::yellow(bound),
    crayon::bold$blue(paste0("$", access)), "):"
  )
}
#' @export
format.boundParams <- function(x, ...) {
  cli::cli_format_method({
    .h2(.fmt3("Parameters", x, "params"))
  })
}

#' @export
format.boundInits <- function(x, ...) {
  cli::cli_format_method({
    .h2(.fmt3("Initial Conditions", x, "inits"))
  })
}


#' @export
format.rxSolveSimType <- function(x, ...) {
  .args <- as.list(match.call(expand.dots = TRUE))
  if (any(names(.args) == "bound")) {
    .bound <- .args$bound
  } else {
    .bound <- .getBound(x, parent.frame(2))
  }
  .uncert <- character(0)
  if (!isNullZero(x$thetaMat)) {
    .uncert <- c(.uncert, paste0("parameters (", crayon::yellow(.bound), crayon::bold$blue("$thetaMat"), " for changes)"))
  }
  if (!isNullZero(x$omegaList)) {
    .uncert <- c(.uncert, paste0("omega matrix (", crayon::yellow(.bound), crayon::bold$blue("$omegaList"), ")"))
  }
  if (!isNullZero(x$sigmaList)) {
    .uncert <- c(.uncert, paste0("sigma matrix (", crayon::yellow(.bound), crayon::bold$blue("$sigmaList"), ")"))
  }
  if (length(.uncert) == 0L) {
    .first <- paste0("\nSimulation ", crayon::bold("without uncertainty"), " in parameters, omega, or sigma matricies\n\n")
  } else if (length(.uncert) == 1L) {
    .first <- paste0("\nSimulation ", crayon::bold("with uncertainty"), " in ", paste(.uncert, collapse = ", "), "\n")
  } else {
    .first <- paste0("\nSimulation ", crayon::bold("with uncertainty"), " in:")
    return(cli::cli_format_method({
      cli::cli_text("")
      cli::cli_text(.first)
      cli::cli_ul(.uncert)
      cli::cli_text("")
    }))
  }
  return(cli::cli_format_method({
    cli::cli_text("")
    cli::cli_text(.first)
    cli::cli_text("")
  }))
}
