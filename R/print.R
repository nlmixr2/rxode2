.h2 <- function(x) {
  cli::cli_text(crayon::bold(paste0(cli::symbol$line, cli::symbol$line, " ", x, " ", cli::symbol$line, cli::symbol$line)))
}

#' Print information about the rxode2 object.
#'
#' This prints the model name and its status for being able to be solved
#'
#' @param x An rxode2 object
#' @param ... Ignored parameters
#' @author Matthew L.Fidler
#' @return original object
#' @export
#' @keywords internal
print.rxode2 <- function(x, ...) {
  rxModelVars(x)
  x <- .getReal(x)
  .bound <- .getBound(x, parent.frame(2))
  .valid <- x$isValid()
  .msg2 <- ""
  if (!.valid) {
    .msg <- crayon::red$bold("invalid")
    .msg2 <- paste0(" re-create with ", crayon::blue("rxode2::"), crayon::yellow("rxode2"))
    .ico <- crayon::red(cli::symbol$cross)
  } else {
    .loaded <- x$isLoaded()
    if (.loaded) {
      .msg <- crayon::green$bold("ready")
      .ico <- crayon::green(cli::symbol$tick)
    } else {
      .msg <- crayon::yellow$bold("unloaded")
      .ico <- crayon::yellow(cli::symbol$warning)
      .msg2 <- paste0(" reload with ", crayon::blue("rxode2::"), crayon::yellow("rxLoad"))
    }
  }
  if (.useUtf()) {
    .ico <- paste0(.ico, " ")
  } else {
    .ico <- ""
  }
  .dll <- getOption("rxode2.basename.print", basename(rxode2::rxDll(x)))
  .env <- attr(x, ".env")
  .pkg <- ""
  .new <- ""
  if (!is.null(x$package)) {
    .dll <- .bound
    .pkg <- crayon::blue(paste0(x$package, "::"))
    if (regexpr("_new", .dll) != -1) {
      .dll <- gsub("_new", "", .dll)
      .new <- " (updated)"
    }
  } else {
    .dll <- substr(.dll, 1, nchar(.dll) - nchar(.Platform$dynlib.ext) - 1)
  }
  cat(paste0(
    crayon::bold("rxode2 "), as.vector(rxode2::rxVersion()["version"]), " model named ", .pkg,
    crayon::yellow$bold(getOption("rxode2.dll.print", .dll)), .new, " model (", .ico, .msg,
    .msg2, ")."
  ), "\n")
  .indLin <- rxModelVars(x)$indLin
  if (length(.indLin) > 0) {
    cat(crayon::bold("  indLin: "))
    if (.indLin[["fullIndLin"]]) {
      if (is.null(.indLin[["f"]])) {
        cat(paste0(crayon::yellow("homogenous matrix exponential + Inductive Linearization")))
      } else {
        cat(paste0(crayon::red("in"), crayon::yellow("homogenous matrix exponential + Inductive Linearization")))
      }
    } else {
      if (is.null(.indLin[["f"]])) {
        cat(crayon::yellow("homogenous matrix exponential"))
      } else {
        cat(paste0(crayon::red("in"), crayon::yellow("homogenous matrix exponential")))
      }
    }
    cat("\n")
  }
  if (!any(names(list(...)) == "rxSuppress") && .valid) {
    .cur <- rxode2::rxState(x)
    if (length(.cur) > 0) {
      cat(paste0(crayon::yellow(.bound), crayon::blue$bold("$state"), ": ", paste(.cur, collapse = ", "), "\n"))
    }
    .cur <- x$stateExtra
    if (length(.cur) > 0) {
      cat(paste0(crayon::yellow(.bound), crayon::blue$bold("$stateExtra"), ": ", paste(.cur, collapse = ", "), "\n"))
    }
    .cur <- rxode2::rxParams(x)
    if (length(.cur) > 0) {
      cat(paste0(crayon::yellow(.bound), crayon::blue$bold("$params"), ": ", paste(.cur, collapse = ", "), "\n"))
    }
    .cur <- rxode2::rxLhs(x)
    if (length(.cur) > 0) {
      cat(paste0(crayon::yellow(.bound), crayon::blue$bold("$lhs"), ": ", paste(.cur, collapse = ", "), "\n"))
    }
  }
  invisible(x)
}

#' Print the rxCoef object
#'
#' This prints out the user supplied arguments for rxCoef object
#'
#' @param x rxCoef object
#'
#' @keywords internal
#' @author Matthew L.Fidler
#' @return original object
#' @export
print.rxCoef <- function(x, ...) {
  .rxDllObj <- x$rxode2
  if (length(rxParams(.rxDllObj)) > 0) {
    cat(cli::cli_format_method({
      .h2("User supplied parameters:")
    }), "\n")
    print(rxode2::rxInits(.rxDllObj, NULL, rxode2::rxParams(.rxDllObj), NA, TRUE))
    cat(cli::cli_format_method({
      .h2("User initial conditions:")
    }), "\n")
    .tmp <- rxode2::rxInits(.rxDllObj, NULL, rxode2::rxState(.rxDllObj), 0, TRUE)
    if (length(x$sens) > 0) {
      .tmp <- .tmp[regexpr(getFromNamespace("regSens", "rxode2"), names(.tmp)) == -1]
    }
    print(.tmp)
  }
  cat(cli::cli_format_method({
    .h2("Compartments:")
  }), "\n")
  .tmp <- rxode2::rxState(.rxDllObj)
  if (length(.tmp) > 0) {
    names(.tmp) <- paste0("cmt=", seq_along(.tmp))
    if (length(x$sens) > 0) {
      .tmp1 <- .tmp[regexpr(getFromNamespace("regSens", "rxode2"), .tmp) == -1]
      print(.tmp1)
      cat(cli::cli_format_method({
        .h2("Sensitivities:")
      }), "\n")
      .tmp2 <- gsub(
        getFromNamespace("regSens", "rxode2"), "d/dt(d(\\1)/d(\\2))",
        .tmp[regexpr(regSens, .tmp) != -1]
      )
      print(.tmp2)
    } else {
      print(.tmp)
    }
  } else {
    cat("No ODEs in this DLL.\n")
  }
  return(invisible())
}

#' @export
print.rxC <- function(x, ...) {
  cat(sprintf("C file: %s  ('summary' for code)\n", getOption("rxode2.c.print", x)))
}


#' Print rxDll object
#'
#' This tells if the rxDll is loaded, ready and/or deleted.
#'
#' @keywords internal
#' @author Matthew L.Fidler
#' @return original object
#' @export
print.rxDll <- function(x, ...) {
  if (file.exists(x$dll)) {
    cat(sprintf("rxode2 DLL named \"%s\"", getOption("rxode2.basename.print", basename(x$dll))))
    if (rxDllLoaded(x)) {
      cat(" is loaded and ready to use.\n")
    } else {
      cat(" is not loaded now.\n")
    }
  } else {
    cat(sprintf("rxode2 DLL named \"%s\" has been deleted.\n", getOption("rxode2.basename.print", basename(x$dll))))
  }
  invisible(x)
}



#' @export
print.rxSolveCovs <- function(x, ...) {
  .args <- as.list(match.call(expand.dots = TRUE))
  if (any(names(.args) == "bound")) {
    .bound <- .args$bound
  } else {
    .bound <- .getBound(x, parent.frame(2))
  }
  NextMethod()
}

#' @export
print.boundInits <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

#' @export
print.rxSolveInits <- function(x, ...) {
  .args <- as.list(match.call(expand.dots = TRUE))
  if (any(names(.args) == "bound")) {
    .bound <- .args$bound
  } else {
    .bound <- .getBound(x, parent.frame(2))
  }
  print(structure(.bound, class = "boundInits"), ...)
  .df <- x$inits
  print(.df)
  NextMethod()
}

#' @export
print.rxSolveSimType <- function(x, ...) {
  if (any(names(x) == "sim.id")) {
    cat(format(x, ...), sep = "\n")
  } else if (!is.null(x$omegaList)) {
    cat(format(x, ...), sep = "\n")
  } else if (!is.null(x$sigmaList)) {
    cat(format(x, ...), sep = "\n")
  } else if (!is.null(x$thetaMat)) {
    cat(format(x, ...), sep = "\n")
  }
}


#' @export
print.rxSolve <- function(x, ...) {
  if (rxIs(x, "rxSolve")) {
    .nb <- TRUE
    .args <- as.list(match.call(expand.dots = TRUE))
    if (any(names(.args) == "n")) {
      .n <- .args$n
    } else {
      .n <- 6L
    }
    if (any(names(.args) == "width")) {
      .width <- .args$width
    } else {
      .width <- NULL
    }
    if (any(names(.args) == "bound")) {
      .bound <- .args$bound
    } else {
      .bound <- .getBound(x, parent.frame(2))
      assignInMyNamespace(".getBoundRemember", .bound)
      on.exit({
        assignInMyNamespace(".getBoundRemember", NULL)
      }, add=TRUE)
    }
    if (.nb) {
      .df <- x$pars
      if (rxIs(.df, "data.frame")) {
        .cls <- c(
          paste0("Parameters ", .bound, "$params"),
          "paged_df", "data.frame"
        )
        class(.df) <- .cls
        .out <- utils::capture.output({
          print(.df)
        })
        if (length(.out) > 0) .nb <- FALSE
      }
    }
    if (.nb) {
      .df <- x$inits
      .df <- as.data.frame(t(x$inits))
      .cls <- c(
        paste0("Initial\u00A0State ", .bound, "$inits"),
        "paged_df", "data.frame"
      )
      class(.df) <- .cls
      .out <- utils::capture.output({
        print(.df)
      })
      if (length(.out) > 0) .nb <- FALSE
    }
    if (.nb) {
      print.rxSolveSimType(x)
      .df <- x
      .cls <- c(
        paste0("Solved\u00A0Data: ", .bound),
        "paged_df", "data.frame"
      )
      class(.df) <- .cls
      print(.df)
      return(invisible(x))
    } else {
      .summary <- any(names(.args) == ".summary")
      if (!.summary) {
        cat(cli::cli_format_method({
          .h2(crayon::bold("Solved rxode2 object"))
        }), sep = "\n")
      }
      NextMethod()
      if (.summary) {
        cat(cli::cli_format_method({
          .h2(crayon::bold("Summary of data (object):"))
        }), sep = "\n")
        print(summary.data.frame(x))
      } else {
        cat(cli::cli_format_method({
          .h2(crayon::bold("First part of data (object):"))
        }), sep = "\n")
        .isDplyr <- requireNamespace("tibble", quietly = TRUE) &&
          getOption("rxode2.display.tbl", TRUE)
        if (!.isDplyr) {
          print(head(as.data.frame(x), n = .n))
        } else {
          print(tibble::as_tibble(x), n = .n, width = .width)
        }
      }
    }
  } else {
    print.data.frame(x)
  }
}

#' @export
print.rxModelText <- function(x, ...) {
  .args <- as.list(match.call(expand.dots = TRUE))
  .summary <- any(names(.args) == ".summary")
  if (any(names(.args) == "bound")) {
    .bound <- .args$bound
  } else {
    .bound <- .getBound(x, parent.frame(2))
  }
  .code <- deparse(body(eval(parse(text = paste("function() {", as.vector(x), "}")))))
  .code[1] <- "rxode2({"
  .code[length(.code)] <- "})"
  if (.summary) {
    cat(cli::cli_format_method({
      .h2(.fmt3("Model", .bound, "model"))
    }), sep = "\n")
  } else {
    cat(cli::cli_format_method({
      .h2(crayon::bold("rxode2 Model Syntax"))
    }), sep = "\n")
  }
  cat(paste(.code, collapse = "\n"), "\n")
}

#' @export
print.boundParams <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

#' @export
print.rxSolveParams <- function(x, ..., n = 0L) {
  .args <- as.list(match.call(expand.dots = TRUE))
  if (any(names(.args) == "bound")) {
    .bound <- .args$bound
  } else {
    .bound <- .getBound(x, parent.frame(2))
  }
  class(.bound) <- "boundParams"
  .df <- x$.params.single
  if (length(.df) > 0) {
    print(.bound, ...)
    print(.df, ...)
  } else {
    .df <- x$pars
    if (rxIs(.df, "data.frame")) {
      print(.bound, ...)
      print(tibble::as_tibble(.df), ...)
    }
  }
  NextMethod()
}

#' @export
print.rxSymInvCholEnv <- function(x, ...) {
  if (is.null(x$theta)) {
    cat(sprintf("Uninitialized $theta, please assign (requires %s arguments)!\n", x$ntheta))
  } else {
    cat(sprintf("$theta=c(%s) for:\n\n", paste(x$theta, collapse = ", ")))
    print(x$invobj$fmat)
    cat("\nThis allows accessing $omegaInv, $omega, etc. For a full list see str(.)\n")
  }
}
