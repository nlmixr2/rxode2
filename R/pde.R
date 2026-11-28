## Method-of-lines expansion of 1-D partial differential equations.
##
## A model that declares `grid(x, from, to, n)` is rewritten, before it is
## parsed, into plain ODE states `C_1 ... C_n` (cell-centered finite volumes of
## width h = (to - from)/n); see `?rxPde` for the syntax.

#' Partial differential equations (method of lines) in rxode2 models
#'
#' A model that declares a one-dimensional grid with `grid()` may write
#' partial differential equations on it; before parsing, they are expanded into
#' ordinary ODE states by the method of lines, so solving, sensitivities and
#' estimation see a normal ODE model.
#'
#' The grid `grid(x, from = 0, to = 1, n = 50)` splits `[from, to]` into `n`
#' cells of width `h = (to - from)/n` (finite volumes) with centers
#' `from + (i - 0.5)*h`.  `from` and `to` may be numbers or expressions of
#' model parameters; `n` must be a positive integer constant or, in a model
#' with `ini()`, the name of a fixed `ini()` value (`nx <- fix(50)`) that
#' `ini(nx = 100)` can change.  The grid name cannot be `t` or `time`.  Supported statements and operators are:
#'
#' - `d/dt(C[x]) = rhs` declares the gridded state `C`, expanded into states
#'   `C_1, ..., C_n`.  Inside `rhs`, `C[x]` (or the bare `C`) is the value in
#'   the current cell and `x` is the cell center.
#' - `C[x](0) = expr` sets the initial condition of every cell (`expr` may use
#'   `x`).
#' - `r[x] <- expr` defines a gridded intermediate variable `r_1, ..., r_n`.
#' - `d2x(C)` is the second derivative (central difference); `dx(C)` is the
#'   first derivative as an upwind finite-volume difference for flow towards
#'   increasing `x`; `dx(C, "forward")` is upwind for flow towards decreasing
#'   `x` and `dx(C, "central")` is a central difference.  For a grid named `a`
#'   these are `d2a()` and `da()`.
#' - `value(C, x = 0) = g` is a fixed-value (Dirichlet) boundary condition at
#'   the boundary `x = 0` (which must be `from` or `to`), used by `d2x()`,
#'   central `dx()` and point values at that edge; an upwind `dx()` uses it
#'   only at its inflow edge.  `g` cannot use gridded values.
#' - `flux(C, x = 0) = q` adds an inward flux `q` (amount per unit length per
#'   time) through the boundary; the difference operators then use a zero
#'   gradient there.  A boundary without a condition has zero gradient
#'   (no flux).
#' - `integrate(C, x)` is the total over the grid (`h` times the cell sum); its
#'   first argument may be an expression of gridded values.
#' - `C[x = 0.5]` is the value at a point, interpolated linearly between cell
#'   centers and the boundary values.
#'
#' Doses go to individual cells by their state names (for example
#' `cmt = "C_1"`); a cell holds a concentration, so a dose `amt` adds
#' `amt * h` to `integrate()`.
#'
#' @param model model text
#' @return the model text with every grid expanded into ODE states; text
#'   without a `grid()` declaration is returned unchanged
#' @author Matthew L. Fidler
#' @family Model definition
#' @export
#' @examples
#' cat(rxPde("
#'   grid(x, from = 0, to = 1, n = 4)
#'   d/dt(C[x]) = D * d2x(C) - k * C[x]
#'   flux(C, x = 0) = kin
#'   tot = integrate(C, x)
#' "))
rxPde <- function(model) {
  checkmate::assertCharacter(model, any.missing = FALSE)
  .model <- paste(model, collapse = "\n")
  if (!.rxPdeMaybe(.model)) {
    return(.model)
  }
  .rxPdeExpand(.model)
}

## Fast pre-check: only text with a statement starting with grid( is expanded.
.rxPdeMaybe <- function(text) {
  length(text) == 1L &&
    !is.na(text) &&
    grepl("(^|[\n;{])[ \t]*grid[ \t]*[(]", text, perl = TRUE)
}

## Expand model text, or a file name holding one, when it declares a grid.
## Returns NULL when there is nothing to expand.
.rxPdeModelText <- function(model) {
  if (!is.character(model) || length(model) != 1L || is.na(model)) {
    return(NULL)
  }
  .text <- model
  if (!grepl("\n", model, fixed = TRUE) && file.exists(model)) {
    .text <- paste(readLines(model, warn = FALSE), collapse = "\n")
  }
  if (!.rxPdeMaybe(.text)) {
    return(NULL)
  }
  .rxPdeExpand(.text)
}

## Format a number for generated code.
.rxPdeNum <- function(x) {
  format(x, digits = 15, scientific = FALSE, trim = TRUE)
}

.rxPdeDeparse <- function(e) {
  paste(deparse(e, width.cutoff = 500L), collapse = " ")
}

## A numeric constant when `e` has no variables, otherwise the expression.
.rxPdeConst <- function(e) {
  if (is.numeric(e)) {
    return(as.numeric(e))
  }
  if (is.call(e) && length(all.vars(e)) == 0L) {
    .v <- try(eval(e, baseenv()), silent = TRUE)
    if (is.numeric(.v) && length(.v) == 1L) {
      return(as.numeric(.v))
    }
  }
  e
}

.rxPdeGridDef <- function(e) {
  .f <- function(name, from, to, n) NULL
  .mc <- try(match.call(.f, e), silent = TRUE)
  if (inherits(.mc, "try-error")) {
    stop("'grid()' takes (name, from, to, n): ", .rxPdeDeparse(e), call. = FALSE)
  }
  .mc <- as.list(.mc)[-1]
  if (!all(c("name", "from", "to", "n") %in% names(.mc))) {
    stop("'grid()' needs name, from, to and n: ", .rxPdeDeparse(e), call. = FALSE)
  }
  if (!is.name(.mc$name)) {
    stop("the grid name must be a variable name: ", .rxPdeDeparse(e), call. = FALSE)
  }
  .name <- as.character(.mc$name)
  if (.name %in% c("t", "time")) {
    stop("a grid cannot be named '", .name, "'", call. = FALSE)
  }
  .n <- .rxPdeConst(.mc$n)
  if (!is.numeric(.n) || length(.n) != 1L || is.na(.n) || .n < 1 || .n != round(.n)) {
    stop("'n' in grid(", .name, ") must be a positive integer constant or a fixed ini() value", call. = FALSE)
  }
  .from <- .rxPdeConst(.mc$from)
  .to <- .rxPdeConst(.mc$to)
  .num <- is.numeric(.from) && is.numeric(.to)
  if (.num && .to <= .from) {
    stop("grid(", .name, ") needs 'to' > 'from'", call. = FALSE)
  }
  .h <- if (.num) {
    .rxPdeNum((.to - .from) / .n)
  } else {
    paste0("(((", .rxPdeDeparse(.mc$to), ") - (", .rxPdeDeparse(.mc$from), "))/", .n, ")")
  }
  .h2 <- if (.num) {
    .rxPdeNum(((.to - .from) / .n)^2)
  } else {
    paste0("(", .h, ")^2")
  }
  list(name = .name, n = as.integer(.n), from = .from, to = .to, num = .num, h = .h, h2 = .h2)
}

## Text of the center of cell i.
.rxPdeCenter <- function(g, i) {
  if (g$num) {
    return(.rxPdeNum(g$from + (i - 0.5) * (g$to - g$from) / g$n))
  }
  paste0("(", .rxPdeDeparse(g$from), " + ", .rxPdeNum(i - 0.5), "*", g$h, ")")
}

## Which boundary ("left"/"right") a value denotes, for grid g.
.rxPdeSide <- function(g, b, what) {
  .b <- .rxPdeConst(b)
  .same <- function(bound) {
    if (is.numeric(.b) && is.numeric(bound)) isTRUE(all.equal(.b, bound)) else identical(.b, bound)
  }
  if (.same(g$from)) {
    return("left")
  }
  if (.same(g$to)) {
    return("right")
  }
  stop(what, ": the boundary must be ", g$name, " = from or ", g$name, " = to of its grid", call. = FALSE)
}

## Statement kind: list(kind, op, lhs, rhs) or NULL for a non-assignment.
.rxPdeStatement <- function(e) {
  if (!is.call(e) || !(as.character(e[[1]]) %in% c("=", "<-", "~")) || length(e) != 3L) {
    return(NULL)
  }
  list(op = as.character(e[[1]]), lhs = e[[2]], rhs = e[[3]])
}

## Name of a gridded variable written as C[x], or NULL.
.rxPdeIndexed <- function(e, grids) {
  if (
    is.call(e) &&
      identical(e[[1]], as.name("[")) &&
      length(e) == 3L &&
      is.name(e[[2]]) &&
      is.name(e[[3]]) &&
      is.null(names(e)[3]) &&
      as.character(e[[3]]) %in% names(grids)
  ) {
    return(c(var = as.character(e[[2]]), grid = as.character(e[[3]])))
  }
  NULL
}

## Gridded state of d/dt(C[x]), or NULL.
.rxPdeDdtLhs <- function(lhs, grids) {
  if (
    is.call(lhs) &&
      identical(lhs[[1]], as.name("/")) &&
      length(lhs) == 3L &&
      identical(lhs[[2]], as.name("d")) &&
      is.call(lhs[[3]]) &&
      identical(lhs[[3]][[1]], as.name("dt")) &&
      length(lhs[[3]]) == 2L
  ) {
    return(.rxPdeIndexed(lhs[[3]][[2]], grids))
  }
  NULL
}

## Gridded state of C[x](0) or C(0), or NULL.
.rxPdeIniLhs <- function(lhs, grids, vars) {
  if (is.call(lhs) && length(lhs) == 2L && is.numeric(lhs[[2]]) && isTRUE(lhs[[2]] == 0)) {
    .f <- lhs[[1]]
    .ix <- .rxPdeIndexed(.f, grids)
    if (!is.null(.ix)) {
      return(.ix)
    }
    if (is.name(.f) && as.character(.f) %in% names(vars)) {
      return(c(var = as.character(.f), grid = vars[[as.character(.f)]]))
    }
  }
  NULL
}

## flux(C, x = b) / value(C, x = b) boundary condition, or NULL.
.rxPdeBcLhs <- function(lhs) {
  if (
    is.call(lhs) &&
      is.name(lhs[[1]]) &&
      as.character(lhs[[1]]) %in% c("flux", "value") &&
      length(lhs) == 3L &&
      is.name(lhs[[2]])
  ) {
    .nm <- names(lhs)
    if (is.null(.nm) || .nm[3] == "") {
      stop(
        "'",
        as.character(lhs[[1]]),
        "()' needs a named boundary, e.g. ",
        as.character(lhs[[1]]),
        "(",
        as.character(lhs[[2]]),
        ", x = 0)",
        call. = FALSE
      )
    }
    return(list(
      type = as.character(lhs[[1]]),
      var = as.character(lhs[[2]]),
      grid = .nm[3],
      at = lhs[[3]]
    ))
  }
  NULL
}

.rxPdeSym <- function(var, i) paste0(var, "_", i)

## Difference stencils for gridded variable `v` on grid g at cell i.
.rxPdeD2 <- function(v, g, i, bc) {
  .n <- g$n
  .c <- .rxPdeSym(v, i)
  if (i > 1L && i < .n) {
    return(paste0(
      "(",
      .rxPdeSym(v, i - 1L),
      " - 2*",
      .c,
      " + ",
      .rxPdeSym(v, i + 1L),
      ")/",
      g$h2
    ))
  }
  ## h * (face gradient) on the right and left faces
  .r <- if (i < .n) {
    paste0(.rxPdeSym(v, i + 1L), " - ", .c)
  } else if (!is.null(bc$right) && bc$right$type == "value") {
    paste0("2*((", bc$right$expr, ") - ", .c, ")")
  } else {
    NULL
  }
  .l <- if (i > 1L) {
    paste0(.c, " - ", .rxPdeSym(v, i - 1L))
  } else if (!is.null(bc$left) && bc$left$type == "value") {
    paste0("2*(", .c, " - (", bc$left$expr, "))")
  } else {
    NULL
  }
  if (is.null(.r) && is.null(.l)) {
    return("0")
  }
  .num <- if (is.null(.l)) {
    paste0("(", .r, ")")
  } else if (is.null(.r)) {
    paste0("(-(", .l, "))")
  } else {
    paste0("((", .r, ") - (", .l, "))")
  }
  paste0(.num, "/", g$h2)
}

.rxPdeD1 <- function(v, g, i, bc, method) {
  .n <- g$n
  .bcv <- function(side, edge) {
    .b <- bc[[side]]
    if (!is.null(.b) && .b$type == "value") paste0("(", .b$expr, ")") else edge
  }
  ## value on face j + 1/2 (j = 0..n)
  .face <- function(j) {
    if (j == 0L) {
      if (method == "forward") .rxPdeSym(v, 1L) else .bcv("left", .rxPdeSym(v, 1L))
    } else if (j == .n) {
      if (method == "backward") .rxPdeSym(v, .n) else .bcv("right", .rxPdeSym(v, .n))
    } else {
      switch(
        method,
        backward = .rxPdeSym(v, j),
        forward = .rxPdeSym(v, j + 1L),
        central = paste0("(", .rxPdeSym(v, j), " + ", .rxPdeSym(v, j + 1L), ")/2")
      )
    }
  }
  .r <- .face(i)
  .l <- .face(i - 1L)
  if (identical(.r, .l)) {
    return("0")
  }
  paste0("((", .r, ") - (", .l, "))/", g$h)
}

## Linear interpolation of gridded variable v at the constant point `at`.
.rxPdeInterp <- function(v, g, at, bc, what) {
  .at <- .rxPdeConst(at)
  if (!is.numeric(.at)) {
    stop(what, ": the point must be a numeric constant", call. = FALSE)
  }
  if (!g$num) {
    stop(what, ": point values need a grid with numeric 'from' and 'to'", call. = FALSE)
  }
  if (.at < g$from || .at > g$to) {
    stop(what, ": the point is outside the grid", call. = FALSE)
  }
  .h <- (g$to - g$from) / g$n
  .pos <- c(g$from, g$from + (seq_len(g$n) - 0.5) * .h, g$to)
  .edge <- function(side, sym) {
    .b <- bc[[side]]
    if (!is.null(.b) && .b$type == "value") paste0("(", .b$expr, ")") else sym
  }
  .val <- c(
    .edge("left", .rxPdeSym(v, 1L)),
    .rxPdeSym(v, seq_len(g$n)),
    .edge("right", .rxPdeSym(v, g$n))
  )
  .j <- findInterval(.at, .pos, rightmost.closed = TRUE)
  .w <- (.at - .pos[.j]) / (.pos[.j + 1L] - .pos[.j])
  if (isTRUE(all.equal(.w, 0))) {
    return(.val[.j])
  }
  if (isTRUE(all.equal(.w, 1))) {
    return(.val[.j + 1L])
  }
  paste0("(", .rxPdeNum(1 - .w), "*", .val[.j], " + ", .rxPdeNum(.w), "*", .val[.j + 1L], ")")
}

.rxPdeStop <- function(...) {
  stop(..., call. = FALSE)
}

## A gridded variable or grid coordinate written as a bare name.
.rxPdeExprName <- function(e, env, ctx) {
  .s <- as.character(e)
  if (.s %in% names(env$vars)) {
    .g <- env$vars[[.s]]
    if (is.null(ctx) || ctx$grid != .g) {
      .rxPdeStop(
        "'",
        .s,
        "' is defined on grid '",
        .g,
        "'; use ",
        .s,
        "[",
        .g,
        " = value] or integrate(",
        .s,
        ", ",
        .g,
        ") here"
      )
    }
    return(as.name(.rxPdeSym(.s, ctx$i)))
  }
  if (.s %in% names(env$grids)) {
    if (is.null(ctx) || ctx$grid != .s) {
      .rxPdeStop("the grid coordinate '", .s, "' is only defined in an equation on that grid")
    }
    return(str2lang(.rxPdeCenter(env$grids[[.s]], ctx$i)))
  }
  e
}

## C[x] or C[x = value]
.rxPdeExprIndex <- function(e, env, ctx) {
  .v <- as.character(e[[2]])
  .g <- env$vars[[.v]]
  .nm <- names(e)[3]
  if (!is.null(.nm) && .nm != "") {
    if (.nm != .g) {
      .rxPdeStop("'", .v, "' is on grid '", .g, "', not '", .nm, "'")
    }
    return(str2lang(.rxPdeInterp(.v, env$grids[[.g]], e[[3]], env$bc[[.v]], .rxPdeDeparse(e))))
  }
  if (!identical(e[[3]], as.name(.g))) {
    .rxPdeStop("'", .v, "' is indexed as ", .v, "[", .g, "] or ", .v, "[", .g, " = value]")
  }
  .rxPdeExprName(e[[2]], env, ctx)
}

## integrate(expr, x)
.rxPdeExprIntegrate <- function(e, env) {
  if (length(e) != 3L || !is.name(e[[3]]) || !(as.character(e[[3]]) %in% names(env$grids))) {
    .rxPdeStop("use integrate(expr, <grid>): ", .rxPdeDeparse(e))
  }
  .g <- env$grids[[as.character(e[[3]])]]
  .terms <- vapply(
    seq_len(.g$n),
    function(i) {
      .rxPdeDeparse(.rxPdeExpr(e[[2]], env, list(grid = .g$name, i = i)))
    },
    character(1)
  )
  str2lang(paste0(.g$h, "*(", paste(.terms, collapse = " + "), ")"))
}

## Method of a first-derivative operator call.
.rxPdeD1Method <- function(e, fn) {
  if (length(e) == 2L) {
    return("backward")
  }
  .method <- e[[3]]
  if (length(e) > 3L || !is.character(.method) || !(.method %in% c("backward", "forward", "central"))) {
    .rxPdeStop("use ", fn, "(<variable>) or ", fn, "(<variable>, \"backward\"|\"forward\"|\"central\")")
  }
  .method
}

## dx(C) / d2x(C)
.rxPdeExprOp <- function(e, env, ctx, fn, op) {
  if (length(e) < 2L || !is.name(e[[2]]) || !(as.character(e[[2]]) %in% names(env$vars))) {
    .rxPdeStop("'", fn, "()' needs a variable on grid '", op$grid, "': ", .rxPdeDeparse(e))
  }
  .v <- as.character(e[[2]])
  if (env$vars[[.v]] != op$grid) {
    .rxPdeStop("'", .v, "' is not on grid '", op$grid, "'")
  }
  if (is.null(ctx) || ctx$grid != op$grid) {
    .rxPdeStop("'", fn, "()' is only defined in an equation on grid '", op$grid, "'")
  }
  .g <- env$grids[[op$grid]]
  if (op$order == 2L) {
    if (length(e) != 2L) {
      .rxPdeStop("'", fn, "()' takes one argument")
    }
    return(str2lang(.rxPdeD2(.v, .g, ctx$i, env$bc[[.v]])))
  }
  str2lang(.rxPdeD1(.v, .g, ctx$i, env$bc[[.v]], .rxPdeD1Method(e, fn)))
}

## Expand an expression.  `ctx` is NULL outside a gridded equation, or
## list(grid = <grid name>, i = <cell>) inside one.
.rxPdeExpr <- function(e, env, ctx) {
  if (is.name(e)) {
    return(.rxPdeExprName(e, env, ctx))
  }
  if (!is.call(e)) {
    return(e)
  }
  .fn <- if (is.name(e[[1]])) as.character(e[[1]]) else ""
  if (.fn == "[" && length(e) == 3L && is.name(e[[2]]) && as.character(e[[2]]) %in% names(env$vars)) {
    return(.rxPdeExprIndex(e, env, ctx))
  }
  if (.fn == "integrate") {
    return(.rxPdeExprIntegrate(e, env))
  }
  .op <- env$ops[[.fn]]
  if (!is.null(.op)) {
    return(.rxPdeExprOp(e, env, ctx, .fn, .op))
  }
  .args <- as.list(e)
  for (.j in seq_along(.args)[-1]) {
    if (!identical(.args[[.j]], quote(expr = )) && !is.null(.args[[.j]])) {
      .args[.j] <- list(.rxPdeExpr(.args[[.j]], env, ctx))
    }
  }
  as.call(.args)
}

## TRUE when expression `e` uses anything that needs expanding.
.rxPdeUses <- function(e, env) {
  .n <- all.names(e)
  any(.n %in% c(names(env$vars), names(env$grids), names(env$ops), "integrate"))
}

## Grids and their difference operators.
.rxPdeSetupGrids <- function(env, gridExprs) {
  env$grids <- list()
  env$ops <- list()
  for (.e in gridExprs) {
    .g <- .rxPdeGridDef(.e)
    if (!is.null(env$grids[[.g$name]])) {
      .rxPdeStop("grid '", .g$name, "' is declared twice")
    }
    env$grids[[.g$name]] <- .g
    env$ops[[paste0("d", .g$name)]] <- list(grid = .g$name, order = 1L)
    env$ops[[paste0("d2", .g$name)]] <- list(grid = .g$name, order = 2L)
  }
  invisible(env)
}

## Record a gridded state (d/dt(C[x])) or gridded lhs (r[x] <- ...).
.rxPdeAddVar <- function(env, ix, state) {
  .v <- ix[["var"]]
  .g <- env$vars[[.v]]
  if (state && !is.null(.g)) {
    .rxPdeStop("'", .v, "' is defined more than once as a gridded state or variable")
  }
  if (!state && .v %in% env$states) {
    .rxPdeStop("'", .v, "' is a gridded state and cannot also be assigned as ", .v, "[", ix[["grid"]], "]")
  }
  if (!is.null(.g) && .g != ix[["grid"]]) {
    .rxPdeStop("'", .v, "' is assigned on grids '", .g, "' and '", ix[["grid"]], "'")
  }
  if (state) {
    env$states <- c(env$states, .v)
  }
  env$vars[[.v]] <- ix[["grid"]]
  invisible(env)
}

## Kind of one statement (pass 1); records gridded variables and boundary
## conditions in `env`.
.rxPdeClassify <- function(e, env) {
  .st <- .rxPdeStatement(e)
  if (is.null(.st)) {
    return("")
  }
  .ix <- .rxPdeDdtLhs(.st$lhs, env$grids)
  if (!is.null(.ix)) {
    .rxPdeAddVar(env, .ix, TRUE)
    return("ddt")
  }
  .ix <- .rxPdeIndexed(.st$lhs, env$grids)
  if (!is.null(.ix)) {
    .rxPdeAddVar(env, .ix, FALSE)
    return("lhs")
  }
  .bc <- .rxPdeBcLhs(.st$lhs)
  if (!is.null(.bc)) {
    .bc$rhs <- .st$rhs
    env$bcs[[length(env$bcs) + 1L]] <- .bc
    return("bc")
  }
  ""
}

## Check one boundary condition and store it in env$bc.
.rxPdeAddBc <- function(bc, env) {
  .what <- paste0(bc$type, "(", bc$var, ", ", bc$grid, " = ", .rxPdeDeparse(bc$at), ")")
  if (!(bc$var %in% env$states)) {
    .rxPdeStop(.what, ": '", bc$var, "' is not a gridded state (d/dt(", bc$var, "[x]))")
  }
  if (env$vars[[bc$var]] != bc$grid) {
    .rxPdeStop(.what, ": '", bc$var, "' is on grid '", env$vars[[bc$var]], "'")
  }
  .side <- .rxPdeSide(env$grids[[bc$grid]], bc$at, .what)
  .cur <- env$bc[[bc$var]]
  if (is.null(.cur)) {
    .cur <- list()
  }
  if (!is.null(.cur[[.side]])) {
    .rxPdeStop(.what, ": a boundary has only one condition")
  }
  if (bc$type == "value" && .rxPdeUses(bc$rhs, env)) {
    .rxPdeStop(.what, ": a fixed boundary value cannot use gridded values")
  }
  .cur[[.side]] <- list(type = bc$type, rhs = bc$rhs, expr = .rxPdeDeparse(bc$rhs))
  env$bc[[bc$var]] <- .cur
  invisible(env)
}

## Expand gridded values in flux() conditions, once every value() is known.
.rxPdeExpandFlux <- function(env) {
  for (.v in names(env$bc)) {
    for (.side in names(env$bc[[.v]])) {
      .b <- env$bc[[.v]][[.side]]
      if (.b$type == "flux" && .rxPdeUses(.b$rhs, env)) {
        env$bc[[.v]][[.side]]$expr <- .rxPdeDeparse(.rxPdeExpr(.b$rhs, env, NULL))
      }
    }
  }
  invisible(env)
}

## Names a statement assigns (x <- ..., d/dt(x) = ...), including inside
## if/else blocks.
.rxPdeAssigned <- function(e) {
  if (!is.call(e)) {
    return(character(0))
  }
  .st <- .rxPdeStatement(e)
  if (!is.null(.st)) {
    .l <- .st$lhs
    if (is.name(.l)) {
      return(as.character(.l))
    }
    if (
      is.call(.l) &&
        identical(.l[[1]], as.name("/")) &&
        length(.l) == 3L &&
        is.call(.l[[3]]) &&
        length(.l[[3]]) == 2L &&
        is.name(.l[[3]][[2]])
    ) {
      return(as.character(.l[[3]][[2]]))
    }
    return(character(0))
  }
  unlist(lapply(as.list(e)[-1], .rxPdeAssigned))
}

## Expanded names must not clash with names the model assigns or with other
## gridded variables; other references (f(C_1), C_1(0), ...) are allowed.
.rxPdeCheckClash <- function(exprs, env) {
  .new <- unlist(lapply(names(env$vars), function(v) {
    .rxPdeSym(v, seq_len(env$grids[[env$vars[[v]]]]$n))
  }))
  .clash <- c(
    intersect(.new, unique(unlist(lapply(exprs, .rxPdeAssigned)))),
    intersect(.new, names(env$vars)),
    .new[duplicated(.new)]
  )
  if (length(.clash) > 0L) {
    .rxPdeStop(
      "the expanded grid variable(s) ",
      paste(.clash, collapse = ", "),
      " clash with names already in the model"
    )
  }
  invisible(env)
}

## A statement outside the grid equations: kept verbatim unless it uses
## gridded values.
.rxPdeEmitOther <- function(e, src, env) {
  if (!.rxPdeUses(e, env)) {
    return(src)
  }
  .st <- .rxPdeStatement(e)
  if (is.null(.st)) {
    return(.rxPdeDeparse(.rxPdeExpr(e, env, NULL)))
  }
  paste(.rxPdeDeparse(.rxPdeExpr(.st$lhs, env, NULL)), .st$op, .rxPdeDeparse(.rxPdeExpr(.st$rhs, env, NULL)))
}

.rxPdeExpand <- function(text) {
  .exprs <- try(parse(text = text, keep.source = TRUE), silent = TRUE)
  if (inherits(.exprs, "try-error")) {
    .rxPdeStop(
      "a model with grid() must be R syntax to expand its partial differential equations:\n",
      attr(.exprs, "condition")$message
    )
  }
  .src <- vapply(attr(.exprs, "srcref"), function(s) paste(as.character(s), collapse = "\n"), character(1))
  .exprs <- as.list(.exprs)
  .isGrid <- vapply(.exprs, function(e) is.call(e) && identical(e[[1]], as.name("grid")), logical(1))
  if (!any(.isGrid)) {
    return(text)
  }
  .env <- new.env(parent = emptyenv())
  .rxPdeSetupGrids(.env, .exprs[.isGrid])
  .env$vars <- list()
  .env$states <- character(0)
  .env$bcs <- list()
  .env$bc <- list()
  .kind <- rep("grid", length(.exprs))
  .kind[!.isGrid] <- vapply(.exprs[!.isGrid], .rxPdeClassify, character(1), env = .env)
  ## initial conditions can only be recognized once the gridded states are known
  .isIni <- vapply(
    .exprs,
    function(e) {
      .st <- .rxPdeStatement(e)
      !is.null(.st) && !is.null(.rxPdeIniLhs(.st$lhs, .env$grids, .env$vars))
    },
    logical(1)
  )
  .kind[.kind == "" & .isIni] <- "ini"
  for (.bc in .env$bcs) {
    .rxPdeAddBc(.bc, .env)
  }
  .rxPdeExpandFlux(.env)
  .rxPdeCheckClash(.exprs, .env)
  .out <- lapply(seq_along(.exprs), function(k) {
    switch(
      .kind[k],
      grid = character(0),
      bc = character(0),
      ddt = ,
      lhs = ,
      ini = .rxPdeEmitCells(.exprs[[k]], .env, .kind[k]),
      .rxPdeEmitOther(.exprs[[k]], .src[k], .env)
    )
  })
  paste(unlist(.out), collapse = "\n")
}

## Emit the n cell statements of a gridded d/dt(), lhs or initial condition.
.rxPdeEmitCells <- function(e, env, kind) {
  .st <- .rxPdeStatement(e)
  .ix <- switch(
    kind,
    ddt = .rxPdeDdtLhs(.st$lhs, env$grids),
    lhs = .rxPdeIndexed(.st$lhs, env$grids),
    ini = .rxPdeIniLhs(.st$lhs, env$grids, env$vars)
  )
  .v <- .ix[["var"]]
  .g <- env$grids[[.ix[["grid"]]]]
  .bc <- env$bc[[.v]]
  vapply(
    seq_len(.g$n),
    function(i) {
      .rhs <- .rxPdeDeparse(.rxPdeExpr(.st$rhs, env, list(grid = .g$name, i = i)))
      .s <- .rxPdeSym(.v, i)
      if (kind == "ddt") {
        .src <- character(0)
        if (i == 1L && !is.null(.bc$left) && .bc$left$type == "flux") {
          .src <- c(.src, paste0(" + (", .bc$left$expr, ")/", .g$h))
        }
        if (i == .g$n && !is.null(.bc$right) && .bc$right$type == "flux") {
          .src <- c(.src, paste0(" + (", .bc$right$expr, ")/", .g$h))
        }
        if (length(.src) > 0L) {
          .rhs <- paste0("(", .rhs, ")", paste(.src, collapse = ""))
        }
        return(paste0("d/dt(", .s, ") ", .st$op, " ", .rhs))
      }
      if (kind == "ini") {
        return(paste0(.s, "(0) ", .st$op, " ", .rhs))
      }
      paste0(.s, " ", .st$op, " ", .rhs)
    },
    character(1)
  )
}

## Grid name of a model line `grid(name, ...)` (arguments in any order), or
## NULL for any other line.
.rxPdeGridName <- function(line) {
  if (!is.call(line) || !identical(line[[1]], as.name("grid"))) {
    return(NULL)
  }
  .mc <- try(match.call(function(name, from, to, n) NULL, line), silent = TRUE)
  if (inherits(.mc, "try-error") || !is.name(.mc$name)) {
    return(NULL)
  }
  as.character(.mc$name)
}

## TRUE for a model line `grid(name, ...)`.
.rxPdeIsGridLine <- function(line) {
  !is.null(.rxPdeGridName(line))
}

## Line of the model's `grid()` with the same name as the piped one, or -1L to
## append a new grid.
.rxPdeGridModelLine <- function(line, rxui) {
  .name <- .rxPdeGridName(line)
  .w <- which(vapply(
    rxui$lstExpr,
    function(e) identical(.rxPdeGridName(e), .name),
    logical(1)
  ))
  if (length(.w) == 0L) {
    return(-1L)
  }
  .w[1]
}

## Expand the grids of a `{` block of model lines (before code that re-prints
## calls positionally, like .rxPrune()); other blocks are returned unchanged.
.rxPdeExpandBlock <- function(x) {
  .lines <- as.list(x)[-1]
  if (!any(vapply(.lines, .rxPdeIsGridLine, logical(1)))) {
    return(x)
  }
  .txt <- .rxPdeExpand(paste(vapply(.lines, deparse1, character(1)), collapse = "\n"))
  str2lang(paste0("{\n", .txt, "\n}"))
}

## Grid size `n` set by a fixed ini() constant: the model line keeps the
## constant's name in the "rxPdeN" attribute while its `n` holds the value, so
## every model built from the ui sees a number and `ini()` piping can update it.

## The ini() value of the grid size `nm`.
.rxPdeIniN <- function(nm, iniDf) {
  .w <- which(iniDf$name == nm & !is.na(iniDf$ntheta))
  if (length(.w) != 1L) {
    return(NULL)
  }
  if (!isTRUE(iniDf$fix[.w])) {
    stop("the grid size '", nm, "' must be fixed in ini(): ", nm, " <- fix(", iniDf$est[.w], ")", call. = FALSE)
  }
  iniDf$est[.w]
}

## A grid line with its `n` resolved from ini(), or the line unchanged.
.rxPdeIniGridLine <- function(line, iniDf) {
  if (!.rxPdeIsGridLine(line)) {
    return(line)
  }
  .mc <- as.list(match.call(function(name, from, to, n) NULL, line))[-1]
  .nm <- attr(line, "rxPdeN")
  if (is.null(.nm) && is.name(.mc$n)) {
    .nm <- as.character(.mc$n)
  }
  if (is.null(.nm)) {
    return(line)
  }
  .n <- .rxPdeIniN(.nm, iniDf)
  if (is.null(.n)) {
    return(line)
  }
  .mc$n <- .n
  .ret <- .rxPdeGridCall(.mc)
  attr(.ret, "rxPdeN") <- .nm
  .ret
}

## `grid(name, from = , to = , n = )` from matched arguments.
.rxPdeGridCall <- function(mc) {
  as.call(c(list(quote(grid), mc$name), mc[c("from", "to", "n")]))
}

## Resolve the ini() grid sizes of model lines (a list or a `{` call).
.rxPdeIniGrid <- function(lines, iniDf) {
  .block <- is.call(lines) && identical(lines[[1]], quote(`{`))
  .lines <- if (.block) as.list(lines)[-1] else lines
  if (!any(vapply(.lines, .rxPdeIsGridLine, logical(1)))) {
    return(lines)
  }
  if (!is.data.frame(iniDf)) {
    iniDf <- as.data.frame(iniDf)
  }
  .lines <- lapply(.lines, .rxPdeIniGridLine, iniDf = iniDf)
  if (.block) {
    return(as.call(c(list(quote(`{`)), .lines)))
  }
  .lines
}

## Mark the parsed grid lines of `env$lstExpr` whose size came from ini(), by
## grid name, as in the resolved model block `x`.
.rxPdeIniGridTag <- function(env, x) {
  .lines <- as.list(x)[-1]
  .grid <- vapply(
    .lines,
    function(l) {
      .nm <- attr(l, "rxPdeN")
      if (is.null(.nm)) NA_character_ else .rxPdeGridName(l)
    },
    character(1)
  )
  .w <- which(!is.na(.grid))
  if (length(.w) == 0L) {
    return(invisible())
  }
  .size <- setNames(vapply(.lines[.w], attr, character(1), which = "rxPdeN"), .grid[.w])
  .lst <- env$lstExpr
  for (.i in seq_along(.lst)) {
    .g <- .rxPdeGridName(.lst[[.i]])
    if (!is.null(.g) && .g %in% names(.size)) {
      attr(.lst[[.i]], "rxPdeN") <- .size[[.g]]
    }
  }
  env$lstExpr <- .lst
  invisible()
}

## ini() names used as grid sizes in model lines.
.rxPdeIniGridNames <- function(lines) {
  unlist(lapply(lines, function(l) attr(l, "rxPdeN")), use.names = FALSE)
}

## Model lines with ini() grid sizes written by name again (for printing and
## re-parsing the model function).
.rxPdeIniGridRestore <- function(lines) {
  lapply(lines, function(l) {
    .nm <- attr(l, "rxPdeN")
    if (is.null(.nm)) {
      return(l)
    }
    .mc <- as.list(match.call(function(name, from, to, n) NULL, l))[-1]
    .mc$n <- as.name(.nm)
    .rxPdeGridCall(.mc)
  })
}

## TRUE when an ini() grid size of the ui no longer matches its ini() value.
.rxPdeIniGridChanged <- function(rxui) {
  .lines <- if (is.environment(rxui)) get0("lstExpr", envir = rxui, inherits = FALSE)
  if (!is.list(.lines)) {
    return(FALSE)
  }
  for (.l in .lines) {
    if (!is.null(attr(.l, "rxPdeN")) && !identical(.rxPdeIniGridLine(.l, rxui$iniDf), .l)) {
      return(TRUE)
    }
  }
  FALSE
}

## Parse the ui again, in place, when ini() changed a grid size (the states
## change with it).
.rxPdeIniGridRebuild <- function(rxui) {
  if (!.rxPdeIniGridChanged(rxui)) {
    return(invisible())
  }
  .new <- rxUiDecompress(rxui$fun())
  for (.i in ls(envir = .new, all.names = TRUE)) {
    assign(.i, get(.i, envir = .new), envir = rxui)
  }
  invisible()
}
