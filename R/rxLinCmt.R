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
    if (
      (identical(x[[1]], quote(`<-`)) ||
        identical(x[[1]], quote(`=`)) ||
        identical(x[[1]], quote(`~`))) &&
        is.name(x[[2]])
    ) {
      .lhs <- as.character(x[[2]])
    } else {
      .lhs <- character()
    }
    unique(c(.lhs, unlist(lapply(x, rxode2::findLhs))))
  } else if (is.pairlist(x)) {
    unique(unlist(lapply(x, rxode2::findLhs)))
  } else {
    stop(sprintf("do not know how to handle type '%s'", typeof(x)), call. = FALSE)
  }
}

#' Does this model still carry an unexpanded `linCmt()`?
#'
#' The parser records `linCmt()` in a parser-global flag
#' (`_rxode2_isLinCmt`) that only an actual parse refreshes, so a model
#' handed around as model variables has to be asked directly; otherwise
#' the flag describes whatever was parsed last (nlmixr2/rxode2#1227).
#'
#' The model's own `flags` answer instead: `linCmtFlg` is nonzero for
#' every linear compartment model (the parser always adds a central
#' compartment), while `ncmt` and `linCmt` (`tb.linCmtN`) are written
#' only by parsing an already-expanded `linCmtA()`/`linCmtB()` call --
#' so a nonzero `linCmtFlg` with both still at the values the parse
#' reset them to is `tb.linCmt == 1`.  Both are checked because either
#' one alone can be the argument an expanded call passes.
#'
#' @param model anything `rxModelVars()` accepts; pass model variables
#'   where they are already at hand, since anything else is parsed
#'
#' @return `TRUE` when the model contains a `linCmt()` that has not been
#'   expanded to `linCmtA()`/`linCmtB()`
#'
#' @noRd
#' @author Matthew L. Fidler
.rxHasUnexpandedLinCmt <- function(model) {
  .mv <- try(rxModelVars(model), silent = TRUE)
  if (inherits(.mv, "try-error")) {
    return(FALSE)
  }
  .need <- c("ncmt", "linCmt", "linCmtFlg")
  .flags <- .mv$flags
  if (!is.numeric(.flags) || !all(.need %in% names(.flags))) {
    # model variables too old (or too damaged) to carry the flags: a parse
    # always produces them, and the normalized text round-trips exactly
    .mv <- try(rxModelVars(setNames(rxNorm(.mv), NULL)), silent = TRUE)
    if (inherits(.mv, "try-error")) {
      return(FALSE)
    }
    .flags <- .mv$flags
    if (!is.numeric(.flags) || !all(.need %in% names(.flags))) {
      return(FALSE)
    }
  }
  .flags[["linCmtFlg"]] != 0L && .flags[["ncmt"]] == 0L && .flags[["linCmt"]] == -100L
}

#' Get the linear compartment model true function
#'
#' @inheritParams rxode2
#' @return model with linCmt() replaced with linCmtA()
#' @author Matthew Fidler
#' @export
rxGetLin <- function(model, linCmtSens = c("linCmtA", "linCmtB"), verbose = FALSE) {
  .mv <- rxGetModel(model) # nolint
  if (.rxHasUnexpandedLinCmt(.mv)) {
    .vars <- c(.mv$params, .mv$lhs, .mv$slhs)
    .Call(
      `_rxode2_linCmtGen`, # nolint
      length(.mv$state),
      .vars,
      setNames(
        c(
          "linCmtA" = 1L,
          "linCmtB" = 2L
        )[match.arg(linCmtSens)],
        NULL
      ),
      verbose
    )
  } else {
    model
  }
}
#' Check if an expression is a linCmt call
#'
#' @param expr expression to check (a line in the model)
#'
#' @return `TRUE` if the expression is a linCmt call, `FALSE`
#'   otherwise
#'
#' @noRd
#' @author Matthew L. Fidler
.isLinCmtCall <- function(expr) {
  if (!is.call(expr)) {
    return(FALSE)
  }
  if (
    !identical(expr[[1]], quote(`=`)) &&
      !identical(expr[[1]], quote(`<-`)) &&
      !identical(expr[[1]], quote(`~`))
  ) {
    return(FALSE)
  }
  !is.null(.linToOdeFindLinCmt(expr[[3]]))
}
#' Find the linCmtA/linCmtB call in an expression
#'
#' @param expr expression (like `1e6 * linCmtA(...)`)
#' @return the first `linCmtA()`/`linCmtB()` call, or `NULL`
#' @noRd
#' @author Matthew L. Fidler
.linToOdeFindLinCmt <- function(expr) {
  if (!is.call(expr)) {
    return(NULL)
  }
  if (is.name(expr[[1]]) && as.character(expr[[1]]) %in% c("linCmtA", "linCmtB")) {
    return(expr)
  }
  for (.e in as.list(expr)[-1]) {
    .ret <- .linToOdeFindLinCmt(.e)
    if (!is.null(.ret)) {
      return(.ret)
    }
  }
  NULL
}
#' Linear compartment model to ODE model expression conversion
#'
#' @param ui rxUi model object
#'
#' @return list of R expressions representing the ODE model equivalent to the linCmt model
#' @noRd
#' @author Matthew L. Fidler
.linToOdeLinExpr <- function(ui) {
  if (is.null(ui$mvL)) {
    return(list())
  }
  # the mvL gives the linear compartment translation
  .mvLExpr <- as.list(str2lang(paste0(
    "{",
    rxNorm(ui$mvL), # nolint
    "}"
  )))[-1]
  .mvLExpr <- Filter(
    function(expr) {
      is.call(expr)
    },
    .mvLExpr
  )
  .mvLExpr <- Filter(.isLinCmtCall, .mvLExpr)
  # linCmt() can be part of an expression (like `cp <- 1e6 * linCmt()`);
  # keep only the linCmtA()/linCmtB() call as the right hand side
  lapply(.mvLExpr, function(e) {
    e[[3]] <- .linToOdeFindLinCmt(e[[3]])
    e
  })
}
#' This converts the linCmtA/linCmtB
#'
#' @param expr the actual linCmtA or linCmtB expression, which is used
#'   to extract the parameters for the ODE translation
#' @return list of parameters for the ODE translation
#' @noRd
#' @author Matthew L. Fidler
.linToOdeArgs <- function(expr) {
  .rhs <- expr[[3]]
  .args <- as.list(.rhs)[-1]
  list(
    ncmt = as.integer(eval(.args[[4]], envir = baseenv())),
    oral0 = as.integer(eval(.args[[5]], envir = baseenv())),
    trans = as.integer(eval(.args[[7]], envir = baseenv())),
    p1 = .args[[8]],
    v1 = .args[[9]],
    p2 = .args[[10]],
    p3 = .args[[11]],
    p4 = .args[[12]],
    p5 = .args[[13]],
    ka = .args[[14]]
  )
}
#' This builds the micro parameters for the one compartment model
#'
#' @param args arguments extracted from the linCmtA/linCmtB
#'   expression, which are used
#' @return list of parameters for the ODE translation
#' @noRd
#' @author Matthew L. Fidler
.linToOdeBuildMicro1 <- function(args) {
  .ret <- list(ncmt = 1L, oral0 = args$oral0, ka = args$ka)
  if (args$trans == 1L) {
    .ret$v <- args$v1
    .ret$k <- bquote(.(args$p1) / .(args$v1))
  } else if (args$trans %in% c(2L, 11L)) {
    .ret$v <- args$v1
    .ret$k <- args$p1
  } else if (args$trans == 10L) {
    .ret$v <- bquote(1 / .(args$v1))
    .ret$k <- args$p1
  } else {
    stop("unsupported 1-cmt linCmt translation", call. = FALSE)
  }
  .ret
}
#' This builds the micro parameters for the 2 compartment model
#'
#' @param the actual linCmtA or linCmtB expression, which is used
#'   to extract the parameters for the ODE translation
#'
#' @return list of parameters for the ODE translation
#'
#' @noRd
#'
#' @author Matthew L. Fidler
#'
.linToOdeBuildMicro2 <- function(args) {
  .ret <- list(ncmt = 2L, oral0 = args$oral0, ka = args$ka)
  if (args$trans == 1L) {
    .ret$v <- args$v1
    .ret$k <- bquote(.(args$p1) / .(args$v1))
    .ret$k12 <- bquote(.(args$p2) / .(args$v1))
    .ret$k21 <- bquote(.(args$p2) / .(args$p3))
  } else if (args$trans == 2L) {
    .ret$v <- args$v1
    .ret$k <- args$p1
    .ret$k12 <- args$p2
    .ret$k21 <- args$p3
  } else if (args$trans == 3L) {
    .ret$v <- args$v1
    .ret$k <- bquote(.(args$p1) / .(args$v1))
    .ret$k12 <- bquote(.(args$p2) / .(args$v1))
    .ret$k21 <- bquote(.(args$p2) / (.(args$p3) - .(args$v1)))
  } else if (args$trans == 4L) {
    .ret$v <- args$v1
    .ret$k21 <- args$p3
    .ret$k <- bquote((.(args$p1) * .(args$p2)) / .(.ret$k21))
    .ret$k12 <- bquote(.(args$p1) + .(args$p2) - .(.ret$k21) - .(.ret$k))
  } else if (args$trans == 5L) {
    .ret$v <- args$v1
    .ret$k21 <- bquote((.(args$p3) * .(args$p2) + .(args$p1)) / (.(args$p3) + 1))
    .ret$k <- bquote((.(args$p1) * .(args$p2)) / .(.ret$k21))
    .ret$k12 <- bquote(.(args$p1) + .(args$p2) - .(.ret$k21) - .(.ret$k))
  } else if (args$trans == 11L) {
    .A <- bquote(1 / .(args$v1)) # nolint
    .B <- args$p3 # nolint
    .alpha <- args$p1
    .beta <- args$p2
    .ret$v <- bquote(1 / (.(.A) + .(.B)))
    .ret$k21 <- bquote((.(.A) * .(.beta) + .(.B) * .(.alpha)) * .(.ret$v))
    .ret$k <- bquote((.(.alpha) * .(.beta)) / .(.ret$k21))
    .ret$k12 <- bquote(.(.alpha) + .(.beta) - .(.ret$k21) - .(.ret$k))
  } else if (args$trans == 10L) {
    .A <- args$v1 # nolint
    .B <- args$p3 # nolint
    .alpha <- args$p1
    .beta <- args$p2
    .ret$v <- bquote(1 / (.(.A) + .(.B)))
    .ret$k21 <- bquote((.(.A) * .(.beta) + .(.B) * .(.alpha)) * .(.ret$v))
    .ret$k <- bquote((.(.alpha) * .(.beta)) / .(.ret$k21))
    .ret$k12 <- bquote(.(.alpha) + .(.beta) - .(.ret$k21) - .(.ret$k))
  } else {
    stop("unsupported 2-cmt linCmt translation", call. = FALSE)
  }
  .ret
}
#' Build the micro parameters for the 3 compartment model
#'
#' @param args arguments extracted from the linCmtA/linCmtB
#'   expression, which are used
#'
#' @return list of parameters for the ODE translation
#'
#' @noRd
#'
#' @author Matthew L. Fidler
.linToOdeBuildMicro3 <- function(args) {
  .ret <- list(ncmt = 3L, oral0 = args$oral0, ka = args$ka)
  if (args$trans == 1L) {
    .ret$v <- args$v1
    .ret$k <- bquote(.(args$p1) / .(args$v1))
    .ret$k12 <- bquote(.(args$p2) / .(args$v1))
    .ret$k21 <- bquote(.(args$p2) / .(args$p3))
    .ret$k13 <- bquote(.(args$p4) / .(args$v1))
    .ret$k31 <- bquote(.(args$p4) / .(args$p5))
  } else if (args$trans == 2L) {
    .ret$v <- args$v1
    .ret$k <- args$p1
    .ret$k12 <- args$p2
    .ret$k21 <- args$p3
    .ret$k13 <- args$p4
    .ret$k31 <- args$p5
  } else if (args$trans %in% c(10L, 11L)) {
    if (args$trans == 11) {
      .A <- bquote(1 / .(args$v1)) # nolint
    } else {
      .A <- args$v1 # nolint
    }
    .B <- args$p3 # nolint
    .C <- args$p5 # nolint
    .alpha <- args$p1
    .beta <- args$p2
    .gamma <- args$p4
    .ret$v <- bquote(1 / (.(.A) + .(.B) + .(.C)))
    .btemp <- bquote(
      -(.(.alpha) *
        .(.C) +
        .(.alpha) * .(.B) +
        .(.gamma) * .(.A) +
        .(.gamma) * .(.B) +
        .(.beta) * .(.A) +
        .(.beta) * .(.C)) *
        .(.ret$v)
    )
    .ctemp <- bquote(
      (.(.alpha) * .(.beta) * .(.C) + .(.alpha) * .(.gamma) * .(.B) + .(.beta) * .(.gamma) * .(.A)) * .(.ret$v)
    )
    .dtemp <- bquote(sqrt((.(.btemp)) * (.(.btemp)) - 4 * (.(.ctemp))))
    .ret$k21 <- bquote(0.5 * (-(.(.btemp)) + .(.dtemp)))
    .ret$k31 <- bquote(0.5 * (-(.(.btemp)) - .(.dtemp)))
    .ret$k <- bquote((.(.alpha) * .(.beta) * .(.gamma)) / .(.ret$k21) / .(.ret$k31))
    .ret$k12 <- bquote(
      ((.(.beta) * .(.gamma) + .(.alpha) * .(.beta) + .(.alpha) * .(.gamma)) -
        .(.ret$k21) * (.(.alpha) + .(.beta) + .(.gamma)) -
        .(.ret$k) * .(.ret$k31) +
        .(.ret$k21) * .(.ret$k21)) /
        (.(.ret$k31) - .(.ret$k21))
    )
    .ret$k13 <- bquote(.(.alpha) + .(.beta) + .(.gamma) - (.(.ret$k) + .(.ret$k12) + .(.ret$k21) + .(.ret$k31)))
  } else {
    stop("unsupported 3-cmt linCmt translation", call. = FALSE)
  }
  .ret
}
#' Return the micro-constants for the ODE translation of a linCmt model
#'
#' @param expr the actual linCmtA or linCmtB expression, which is used
#'
#' @return list of micro-constants for the ODE translation
#' @noRd
#' @author Matthew L. Fidler
.linToOdeBuildMicro <- function(expr) {
  .args <- .linToOdeArgs(expr)
  if (.args$ncmt == 1L) {
    .linToOdeBuildMicro1(.args)
  } else if (.args$ncmt == 2L) {
    .linToOdeBuildMicro2(.args)
  } else if (.args$ncmt == 3L) {
    .linToOdeBuildMicro3(.args)
  } else {
    stop("unsupported linCmt compartment count", call. = FALSE)
  }
}
#' convert the predDf linCmt() line to the ODE equivalent
#'
#' @param ui rxode2 ui model
#' @param i line number to check for linCmt() call
#' @return data frame row with the ODE equivalent of the linCmt() call, or `NULL`
#' @noRd
#' @author Matthew L. Fidler
.linToOdePredLine <- function(ui, i) {
  if (is.null(ui$predDf)) {
    return(NULL)
  }
  .w <- which(ui$predDf$line == i)
  if (length(.w) == 1L) {
    ui$predDf[.w, , drop = FALSE]
  } else {
    NULL
  }
}
#' Convert the micro-constants to ODE lines for the ODE translation of a linCmt model
#'
#' @param micro list of the micro constant
#' @return a list of R expressions for the ode model
#' @noRd
#' @author Matthew L. Fidler
.linToOdeOdeLines <- function(micro) {
  .ret <- list()
  .input <- "0"
  .kTxt <- paste0("(", deparse1(micro$k), ")")
  if (micro$oral0 == 1) {
    .kaTxt <- paste0("(", deparse1(micro$ka), ")")
    .ret[[length(.ret) + 1L]] <- str2lang(paste0("d/dt(depot) <- -", .kaTxt, " * depot"))
    .input <- paste0(.kaTxt, " * depot")
  }
  if (micro$ncmt == 1) {
    .ret[[length(.ret) + 1L]] <- str2lang(paste0("d/dt(central) <- ", .input, " - ", .kTxt, " * central"))
  } else if (micro$ncmt == 2L) {
    .k12Txt <- paste0("(", deparse1(micro$k12), ")")
    .k21Txt <- paste0("(", deparse1(micro$k21), ")")
    .ret[[length(.ret) + 1L]] <- str2lang(paste0(
      "d/dt(central) <- ",
      .input,
      " - ",
      .kTxt,
      " * central - ",
      .k12Txt,
      " * central + ",
      .k21Txt,
      " * peripheral1"
    ))
    .ret[[length(.ret) + 1L]] <- str2lang(paste0(
      "d/dt(peripheral1) <- ",
      .k12Txt,
      " * central - ",
      .k21Txt,
      " * peripheral1"
    ))
  } else if (micro$ncmt == 3L) {
    .k12Txt <- paste0("(", deparse1(micro$k12), ")")
    .k21Txt <- paste0("(", deparse1(micro$k21), ")")
    .k13Txt <- paste0("(", deparse1(micro$k13), ")")
    .k31Txt <- paste0("(", deparse1(micro$k31), ")")
    .ret[[length(.ret) + 1L]] <- str2lang(paste0(
      "d/dt(central) <- ",
      .input,
      " - ",
      .kTxt,
      " * central - ",
      .k12Txt,
      " * central + ",
      .k21Txt,
      " * peripheral1 - ",
      .k13Txt,
      " * central + ",
      .k31Txt,
      " * peripheral2"
    ))
    .ret[[length(.ret) + 1L]] <- str2lang(paste0(
      "d/dt(peripheral1) <- ",
      .k12Txt,
      " * central - ",
      .k21Txt,
      " * peripheral1"
    ))
    .ret[[length(.ret) + 1L]] <- str2lang(paste0(
      "d/dt(peripheral2) <- ",
      .k13Txt,
      " * central - ",
      .k31Txt,
      " * peripheral2"
    ))
  }
  .ret
}
#' Convert the micro expressions to lhs values for ode solving
#'
#' @param expr linCmt expression
#' @param predLine the pred line for the linCmt() model
#' @param micro list of micro constants
#' @param name name for the prediction of a `linCmt() ~ ...` endpoint
#' @return a list of lhs expressions for ode conversion
#' @noRd
#' @author Matthew L. Fidler
.linToOdeLhsLines <- function(expr, predLine, micro, name = "rxLinCmtOde") {
  .vExpr <- str2lang(paste0("(", deparse1(micro$v), ")"))
  .conc <- call("/", str2lang("central"), .vExpr)
  if (!is.null(predLine) && isTRUE(predLine$linCmt)) {
    # a `linCmt() ~ ...` endpoint: define the prediction and use it as
    # the endpoint (with the same residual error and condition)
    .lhs <- predLine$var
    if (.lhs == "rxLinCmt") {
      # rxLinCmt is read back as a linCmt() model, so it gets a name the
      # model does not use
      .lhs <- name
    }
    .lhsExpr <- str2lang(.lhs)
    return(list(call("<-", .lhsExpr, .conc), .linToOdeReplaceLinCmt(expr, .lhsExpr)))
  }
  .rhs <- expr[[3]]
  if (is.name(expr[[2]]) && is.call(.rhs) && identical(.rhs[[1]], quote(linCmt))) {
    return(list(call("<-", expr[[2]], .conc)))
  }
  # linCmt() is part of an expression (like 1e6 * linCmt()) or an ODE
  list(.linToOdeReplaceLinCmt(expr, call("(", .conc)))
}
#' Replace linCmt() calls in an expression
#'
#' @param expr expression
#' @param by replacement expression
#' @return expression with every `linCmt(...)` call replaced by `by`
#' @noRd
#' @author Matthew L. Fidler
.linToOdeReplaceLinCmt <- function(expr, by) {
  if (!is.call(expr)) {
    return(expr)
  }
  if (identical(expr[[1]], quote(linCmt))) {
    return(by)
  }
  as.call(lapply(as.list(expr), .linToOdeReplaceLinCmt, by = by))
}
#' Render the linCmt() system as an ode system
#'
#' @param expr  linCmt expression
#' @param predLine predicion line data frame
#' @param mvExpr linCmtA/linCmtB expression
#' @param odes write the ODEs of the linear compartment system (only
#'   the first line that uses the system writes them)
#' @param name name for the prediction of a `linCmt() ~ ...` endpoint
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.linToOdeRender <- function(expr, predLine, mvExpr, odes = TRUE, name = "rxLinCmtOde") {
  .micro <- .linToOdeBuildMicro(mvExpr)
  c(
    if (odes) .linToOdeOdeLines(.micro),
    .linToOdeLhsLines(expr, predLine, .micro, name = name)
  )
}
#' A variable name the model does not use
#'
#' @param ui rxode2 ui model
#' @param name preferred name
#' @return `name`, or `name` with a number added when the model already
#'   uses it
#' @noRd
#' @author Matthew L. Fidler
.linToOdeFreeName <- function(ui, name = "rxLinCmtOde") {
  .used <- unique(c(
    unlist(lapply(ui$lstExpr, all.vars)),
    ui$iniDf$name
  ))
  .ret <- name
  .i <- 0L
  while (.ret %in% .used) {
    .i <- .i + 1L
    .ret <- paste0(name, .i)
  }
  .ret
}
#' Convert the linear compartment models to ode expressions.
#'
#' @param ui rxUi model object
#' @return list of R expressions representing the ODE model equivalent to the linCmt model
#' @noRd
#' @author Matthew L. Fidler
.linToOdeExpr <- function(ui) {
  .linExpr <- .linToOdeLinExpr(ui)
  if (length(.linExpr) == 0L) {
    return(ui$lstExpr)
  }
  # a model has one linear compartment system; several lines (or
  # endpoints) can use it, but its ODEs are written only once
  .linCur <- 1L
  .odes <- TRUE
  .name <- .linToOdeFreeName(ui)
  .ret <- list()
  for (i in seq_along(ui$lstExpr)) {
    .expr <- ui$lstExpr[[i]]
    .predLine <- .linToOdePredLine(ui, i)
    if (regexpr("linCmt\\s*\\(", deparse1(.expr), perl = TRUE) != -1) {
      .ret <- c(.ret, .linToOdeRender(.expr, .predLine, .linExpr[[.linCur]], odes = .odes, name = .name))
      .odes <- FALSE
      .linCur <- min(.linCur + 1L, length(.linExpr))
    } else {
      .ret[[length(.ret) + 1L]] <- .expr
    }
  }
  c(.linToOdeCmtOrder(ui, .ret), .ret)
}
#' State names in the order a set of model expressions defines them
#'
#' @param exprs list of model expressions
#' @return character vector of compartment names, first definition first
#' @noRd
#' @author Matthew L. Fidler
.linToOdeStateOrder <- function(exprs) {
  .ret <- character(0)
  for (.e in exprs) {
    .txt <- deparse1(.e)
    .m <- regmatches(
      .txt,
      regexpr("^\\s*(?:d\\s*/\\s*dt|cmt)\\s*\\(\\s*[A-Za-z_.][A-Za-z0-9_.]*\\s*\\)", .txt, perl = TRUE)
    )
    if (length(.m) == 0L) {
      next
    }
    .ret <- c(.ret, gsub("^.*\\(\\s*|\\s*\\)$", "", .m))
  }
  unique(.ret)
}
#' Keep the compartment numbers of a linCmt() model with other ODEs
#'
#' A `linCmt()` model numbers its depot and central compartments first
#' and the other ODE states after them, whatever order the model lines
#' are in.  The ODE translation numbers the states in the order they are
#' defined, so `cmt()` statements are added to keep the numbers of the
#' original model (the peripheral compartments, which have no number in
#' the `linCmt()` model, come last).
#'
#' They are added ONLY when the translation would otherwise number the
#' states differently, i.e. when the model defines an ODE before the line
#' that uses `linCmt()`.  When the `linCmt()` line comes first the
#' translated ODEs are already emitted in the right place and the
#' declarations are redundant -- and not harmless: an estimation method
#' that appends sensitivity states (`nlm`/`trust` with
#' `solveType="hessian"`) mis-numbered the compartments of a model
#' carrying them, so a mixed `linCmt()`/ODE fit gave an objective
#' function several thousand times the all-ODE model's.
#'
#' @param ui rxode2 ui model
#' @param exprs translated model expressions
#' @return list of `cmt()` expressions (empty when the translation
#'   already numbers the states the same way)
#' @noRd
#' @author Matthew L. Fidler
.linToOdeCmtOrder <- function(ui, exprs) {
  .state <- ui$stateDf
  if (is.null(.state) || nrow(.state) == 0L) {
    return(list())
  }
  .names <- .state[["Compartment Name"]][order(.state[["Compartment Number"]])]
  if (all(.names %in% c("depot", "central"))) {
    return(list())
  }
  .natural <- .linToOdeStateOrder(exprs)
  if (
    length(.natural) >= length(.names) &&
      identical(.natural[seq_along(.names)], .names)
  ) {
    return(list())
  }
  lapply(.names, function(n) {
    str2lang(paste0("cmt(", n, ")"))
  })
}

#' Convert linCmt rxUi models to ODE rxUi models
#'
#' @param ui rxUi-like model object
#'
#' @return rxUi model with `linCmt()` translated to explicit ODEs
#' @examples
#' oneCmt <- function() {
#'   ini({
#'     tka <- 0.45
#'     tcl <- log(2.7)
#'     tv <- 3.45
#'     add.sd <- 0.7
#'   })
#'   model({
#'     ka <- exp(tka)
#'     cl <- exp(tcl)
#'     v <- exp(tv)
#'     cp <- linCmt()
#'     cp ~ add(add.sd)
#'   })
#' }
#'
#' oneCmtOde <- linToOde(oneCmt)
#'
#'
#'
#' pkpd <- function() {
#'   ini({
#'     ka <- 1
#'     cl <- 2
#'     v <- 20
#'     kin <- 1
#'     kout <- 1
#'     ec50 <- 2
#'   })
#'   model({
#'     cp <- linCmt(ka, cl, v)
#'     eff(0) <- 1
#'     d/dt(eff) <- kin - kout * (1 - cp/(ec50 + cp)) * eff
#'   })
#' }
#'
#' pkpdOde <- linToOde(pkpd)
#'
#' @author Matthew Fidler
#' @export
linToOde <- function(ui) {
  .ui <- as.rxUi(ui) # nolint
  .ui <- rxUiDecompress(.ui) # nolint
  .expr <- .linToOdeExpr(.ui)
  if (identical(.expr, .ui$lstExpr)) {
    return(rxUiCompress(.ui)) # nolint
  }
  .ls <- ls(.ui$meta, all.names = TRUE)
  .ret <- vector("list", length(.ls) + ifelse(length(.ui$iniDf$cond) > 0, 3, 2))
  .ret[[1]] <- quote(`{`)
  for (.i in seq_along(.ls)) {
    .var <- .ls[.i]
    .ret[[.i + 1]] <- rxUiDeparse(.ui$meta[[.var]], .var) # nolint
  }
  .len <- length(.ls)
  if (length(.ui$iniDf$cond) > 0) {
    .ret[[.len + 2]] <- .ui$iniFun
    .ret[[.len + 3]] <- bquote(model(.(as.call(c(quote(`{`), .expr)))))
  } else {
    .ret[[.len + 2]] <- bquote(model(.(as.call(c(quote(`{`), .expr)))))
  }
  .fun <- function() {}
  body(.fun) <- as.call(.ret)
  if (is.function(.ui$model)) {
    environment(.fun) <- environment(.ui$model)
  }
  suppressMessages(as.rxUi(.fun)) # nolint
}

#' Get the micro-constant parameterization of a linCmt() model
#'
#' This extracts the solved linear compartment model(s) from a model
#' and returns them as micro-constants (`k`, `k12`, `k21`, `k13`,
#' `k31`), the central volume `v` and the absorption rate `ka`, each
#' as an R expression in terms of the model variables. This is useful
#' for translating a `linCmt()` model to software that has its own
#' closed-form linear compartment solutions (like NONMEM's `ADVAN1-4`,
#' `ADVAN11-12` or Monolix's `pkmodel()`).
#'
#' @param ui rxUi-like model object
#'
#' @return A list with one element per linear compartment system
#'   (endpoints that share the same `linCmt()`, like conditional
#'   `linCmt() ~ ... | cond` endpoints, share one element).
#'   Each element is a list with `ncmt` (number of compartments),
#'   `oral0` (1 when there is a depot compartment, 0 otherwise), and
#'   the expressions `ka`, `v`, `k`, `k12`, `k21`, `k13` and `k31`.
#'   `ka` is 0 for a model without a depot, and the transfer rates a
#'   model does not have (like `k13` and `k31` for 2 compartments) are
#'   `NULL`. A model without `linCmt()` returns an empty list.
#' @examples
#'
#' oneCmt <- function() {
#'   ini({
#'     tka <- 0.45
#'     tcl <- log(2.7)
#'     tv <- 3.45
#'     add.sd <- 0.7
#'   })
#'   model({
#'     ka <- exp(tka)
#'     cl <- exp(tcl)
#'     v <- exp(tv)
#'     cp <- linCmt()
#'     cp ~ add(add.sd)
#'   })
#' }
#'
#' linCmtMicro(oneCmt)
#'
#' @author Matthew L. Fidler
#' @export
linCmtMicro <- function(ui) {
  .ui <- rxUiDecompress(as.rxUi(ui)) # nolint
  lapply(.linToOdeLinExpr(.ui), .linToOdeBuildMicro)
}
