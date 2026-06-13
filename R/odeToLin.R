## Collect additive terms from an expression tree, tracking sign.
## Returns list of {sign=±1, expr}.
.collectAddTerms <- function(expr, sign = 1L) {
  if (!is.call(expr)) return(list(list(sign = sign, expr = expr)))
  .fn <- expr[[1]]
  if (identical(.fn, quote(`(`)) && length(expr) == 2L) {
    return(.collectAddTerms(expr[[2]], sign))
  }
  if (identical(.fn, quote(`+`)) && length(expr) == 3L) {
    return(c(.collectAddTerms(expr[[2]], sign), .collectAddTerms(expr[[3]], sign)))
  }
  if (identical(.fn, quote(`-`)) && length(expr) == 3L) {
    return(c(.collectAddTerms(expr[[2]], sign), .collectAddTerms(expr[[3]], -sign)))
  }
  if (identical(.fn, quote(`-`)) && length(expr) == 2L) {
    return(.collectAddTerms(expr[[2]], -sign))
  }
  list(list(sign = sign, expr = expr))
}

## Return all state variable names referenced anywhere in an expression.
.statesInExpr <- function(expr, states) {
  if (is.name(expr)) {
    .nm <- as.character(expr)
    return(if (.nm %in% states) .nm else character(0))
  }
  if (!is.call(expr)) return(character(0))
  if (identical(expr[[1]], quote(`(`)) && length(expr) == 2L) {
    return(.statesInExpr(expr[[2]], states))
  }
  unique(unlist(lapply(as.list(expr)[-1], .statesInExpr, states = states)))
}

## Extract the coefficient from `coef * state_nm` or `state_nm * coef`,
## where coef does not reference any state variable.
## Returns the coefficient expression, or NULL if the term is not that form.
.extractMultCoef <- function(expr, state_nm, states) {
  if (is.name(expr)) {
    if (as.character(expr) == state_nm) return(quote(1))
    return(NULL)
  }
  if (!is.call(expr)) return(NULL)
  .fn <- expr[[1]]
  if (identical(.fn, quote(`(`)) && length(expr) == 2L) {
    return(.extractMultCoef(expr[[2]], state_nm, states))
  }
  if (!identical(.fn, quote(`*`))) return(NULL)
  .lhs <- expr[[2]]; .rhs <- expr[[3]]
  .lu <- length(.statesInExpr(.lhs, states)) > 0L
  .ru <- length(.statesInExpr(.rhs, states)) > 0L
  if (.lu && !.ru && is.name(.lhs) && as.character(.lhs) == state_nm) return(.rhs)
  if (!.lu && .ru && is.name(.rhs) && as.character(.rhs) == state_nm) return(.lhs)
  NULL
}

## Parse one additive term: returns {sign, coef, state} where state=NA means
## a non-state (input/forcing) term. Returns NULL if the term is nonlinear
## (references multiple states or uses a state nonlinearly).
.parseOneLinTerm <- function(sign, term_expr, states) {
  .refs <- .statesInExpr(term_expr, states)
  if (length(.refs) == 0L) {
    return(list(sign = sign, coef = term_expr, state = NA_character_))
  }
  if (length(.refs) > 1L) return(NULL)
  .state <- .refs[1L]
  .coef  <- .extractMultCoef(term_expr, .state, states)
  if (is.null(.coef)) return(NULL)
  list(sign = sign, coef = .coef, state = .state)
}

## Parse an ODE RHS into a flat list of {sign, coef, state} terms.
## Returns NULL if the RHS is not linear in all state variables.
.parseLinearRhs <- function(rhs, states) {
  .raw    <- .collectAddTerms(rhs)
  .parsed <- lapply(.raw, function(.t) .parseOneLinTerm(.t$sign, .t$expr, states))
  if (any(vapply(.parsed, is.null, logical(1)))) return(NULL)
  .parsed
}

## Detect the topology of a linear ODE system and classify each compartment.
##
## odes: list of {cmt=name, terms=parsed_terms} for each ODE
## output_cmt: the state variable identified as the central compartment
##   (from the output line var <- cmt / v).
##
## Returns list(ncmt, oral0, central, depot, peripheral1, peripheral2) or NULL.
.odeToLinDetectTopology <- function(odes, output_cmt, cmt_names) {
  .n <- length(odes)
  if (.n == 0L || .n > 4L) return(NULL)

  ## Build inflow map: flows_in[[cmt]] = list of {from, coef} for positive
  ## cross-compartment terms in cmt's ODE.
  .flows_in  <- setNames(vector("list", .n), cmt_names)
  .flows_out <- setNames(vector("list", .n), cmt_names)

  for (.ode in odes) {
    .cmt <- .ode$cmt
    for (.t in .ode$terms) {
      if (is.na(.t$state) || .t$state == .cmt) next
      if (.t$sign > 0L) {
        .flows_in[[.cmt]] <- c(.flows_in[[.cmt]],
                               list(list(from = .t$state, coef = .t$coef)))
      }
    }
  }
  ## Derive outflow map from inflow map.
  for (.cmt in cmt_names) {
    for (.f in .flows_in[[.cmt]]) {
      .flows_out[[.f$from]] <- c(.flows_out[[.f$from]], list(list(to = .cmt)))
    }
  }

  .central <- output_cmt
  if (!.central %in% cmt_names) return(NULL)

  .depot       <- NULL
  .peripherals <- character(0)

  for (.cmt in cmt_names[cmt_names != .central]) {
    .n_in  <- length(.flows_in[[.cmt]])
    .out   <- .flows_out[[.cmt]]
    .n_out <- length(.out)

    .all_out_to_central  <- .n_out > 0L &&
      all(vapply(.out, function(.f) .f$to == .central, logical(1)))
    .all_in_from_central <- .n_in > 0L &&
      all(vapply(.flows_in[[.cmt]], function(.f) .f$from == .central, logical(1)))

    if (.n_in == 0L && .n_out == 1L && .all_out_to_central) {
      ## Depot: no inflows, exactly one outflow to central
      if (!is.null(.depot)) return(NULL)
      .depot <- .cmt
    } else if (.n_in > 0L && .all_in_from_central && .all_out_to_central) {
      ## Peripheral: inflow from central, outflow to central
      .peripherals <- c(.peripherals, .cmt)
    } else {
      return(NULL)
    }
  }

  .ncmt <- 1L + length(.peripherals)
  if (.ncmt > 3L) return(NULL)

  list(
    ncmt        = .ncmt,
    oral0       = if (is.null(.depot)) 0L else 1L,
    central     = .central,
    depot       = .depot,
    peripheral1 = if (length(.peripherals) >= 1L) .peripherals[1L] else NULL,
    peripheral2 = if (length(.peripherals) >= 2L) .peripherals[2L] else NULL
  )
}

## Find a line of the form  var <- cmt / v_expr  where cmt is a state variable.
## Returns {var, cmt, v_expr, line_idx} or NULL.
.odeToLinFindOutput <- function(lstExpr, states) {
  for (.i in seq_along(lstExpr)) {
    .e <- lstExpr[[.i]]
    if (!is.call(.e)) next
    if (!identical(.e[[1]], quote(`<-`)) && !identical(.e[[1]], quote(`=`))) next
    if (length(.e) < 3L || !is.name(.e[[2]])) next
    .rhs <- .e[[3]]
    if (!is.call(.rhs) || length(.rhs) != 3L) next
    if (!identical(.rhs[[1]], quote(`/`))) next
    .num <- .rhs[[2]]
    if (!is.name(.num)) next
    .cmt_nm <- as.character(.num)
    if (!.cmt_nm %in% states) next
    return(list(
      var      = as.character(.e[[2]]),
      cmt      = .cmt_nm,
      v_expr   = .rhs[[3]],
      line_idx = .i
    ))
  }
  NULL
}

## Check if an expression is d/dt(name).
## In R's AST, `d/dt(x)` parses as call("/", d, call(dt, x)), NOT call("d/dt", x).
.isDtExpr <- function(expr) {
  is.call(expr) && length(expr) == 3L &&
    identical(expr[[1]], quote(`/`)) &&
    is.name(expr[[2]]) && identical(expr[[2]], quote(d)) &&
    is.call(expr[[3]]) && length(expr[[3]]) == 2L &&
    is.name(expr[[3]][[1]]) && as.character(expr[[3]][[1]]) == "dt" &&
    is.name(expr[[3]][[2]])
}

## Extract compartment name from d/dt(name) expression.
.getDtCmt <- function(expr) as.character(expr[[3]][[2]])

## Attempt to detect whether ui is a linear compartment ODE model.
## Returns a list with topology + output info, or NULL if not convertible.
.odeToLinDetect <- function(ui) {
  .lstExpr <- ui$lstExpr
  .states   <- rxModelVars(ui)$state # nolint

  if (length(.states) == 0L) return(NULL)

  ## Gather ODE lines and their compartment names.
  .ode_idx   <- integer(0)
  .cmt_names <- character(0)
  for (.i in seq_along(.lstExpr)) {
    .e <- .lstExpr[[.i]]
    if (!is.call(.e)) next
    if (!identical(.e[[1]], quote(`<-`)) && !identical(.e[[1]], quote(`=`))) next
    if (length(.e) < 3L || !is.call(.e[[2]])) next
    if (!.isDtExpr(.e[[2]])) next
    .ode_idx   <- c(.ode_idx, .i)
    .cmt_names <- c(.cmt_names, .getDtCmt(.e[[2]]))
  }

  if (length(.ode_idx) == 0L || length(.ode_idx) > 4L) return(NULL)
  if (!all(.cmt_names %in% .states)) return(NULL)

  ## Parse each ODE RHS; bail if any is nonlinear in state variables.
  .odes <- lapply(seq_along(.ode_idx), function(.j) {
    .e <- .lstExpr[[.ode_idx[.j]]]
    .terms <- .parseLinearRhs(.e[[3]], .states)
    if (is.null(.terms)) return(NULL)
    list(cmt = .cmt_names[.j], terms = .terms)
  })
  if (any(vapply(.odes, is.null, logical(1)))) return(NULL)

  ## Find output line: var <- central_cmt / v_expr
  .out <- .odeToLinFindOutput(.lstExpr, .states)
  if (is.null(.out)) return(NULL)

  ## Classify topology.
  .topo <- .odeToLinDetectTopology(.odes, .out$cmt, .cmt_names)
  if (is.null(.topo)) return(NULL)

  c(.topo, list(
    output_var = .out$var,
    output_cmt = .out$cmt,
    v_expr     = .out$v_expr,
    output_idx = .out$line_idx,
    ode_idx    = .ode_idx
  ))
}

## Build a new lstExpr with ODE lines removed and the output line replaced by
## `output_var <- linCmt()`.
## Map ODE compartment names to the standard linCmt names ("depot", "central").
## linCmt() models always use these two names regardless of what the ODE called them.
.odeToLinCmtMap <- function(info) {
  .map <- character(0)
  if (isTRUE(info$oral0 == 1L) && !is.null(info$depot)) {
    .map[info$depot] <- "depot"
  }
  if (!is.null(info$central)) {
    .map[info$central] <- "central"
  }
  .map
}

## Apply compartment renaming to a single f/rate/dur/alag assignment line.
## Returns the line unchanged if it is not a modifier or its compartment is
## not in the mapping.
.odeToLinRenameCmt <- function(e, cmtMap) {
  if (!is.call(e)) return(e)
  if (!identical(e[[1]], quote(`<-`)) && !identical(e[[1]], quote(`=`))) return(e)
  if (length(e) < 3L || !is.call(e[[2]])) return(e)
  .fn <- as.character(e[[2]][[1]])
  if (!(.fn %in% c("f", "rate", "dur", "alag"))) return(e)
  if (length(e[[2]]) < 2L) return(e)
  .cmt <- as.character(e[[2]][[2]])
  .newCmt <- cmtMap[.cmt]
  if (is.na(.newCmt)) return(e)
  e[[2]][[2]] <- as.name(.newCmt)
  e
}

.odeToLinBuildExpr <- function(lstExpr, info) {
  .linCmtLine <- call("<-", as.name(info$output_var), str2lang("linCmt()"))
  .cmtMap <- .odeToLinCmtMap(info)
  .ret <- list()
  for (.i in seq_along(lstExpr)) {
    if (.i %in% info$ode_idx) {
      next  # remove ODE lines
    } else if (.i == info$output_idx) {
      .ret[[length(.ret) + 1L]] <- .linCmtLine  # replace output with linCmt()
    } else {
      .e <- .odeToLinRenameCmt(lstExpr[[.i]], .cmtMap)
      .ret[[length(.ret) + 1L]] <- .e
    }
  }
  .ret
}

## Package-scope cache: maps model-text key -> compiled converted rxUi.
## Prevents recompilation when rxSolve(..., useLinCmt=TRUE) is called repeatedly
## with the same model.
.odeToLinCache <- new.env(parent = emptyenv(), hash = TRUE)

## Cheap cache key: normalized text of all model lines.
.odeToLinCacheKey <- function(ui) {
  paste0(vapply(ui$lstExpr, deparse1, character(1)), collapse = "\n")
}

## Rebuild an rxUi from a modified lstExpr (following the linToOde pattern).
.rebuildRxUiFromExpr <- function(ui, expr) {
  .ls  <- ls(ui$meta, all.names = TRUE)
  .has_ini <- length(ui$iniDf$cond) > 0L
  .ret <- vector("list", length(.ls) + if (.has_ini) 3L else 2L)
  .ret[[1L]] <- quote(`{`)
  for (.i in seq_along(.ls)) {
    .ret[[.i + 1L]] <- rxUiDeparse(ui$meta[[.ls[.i]]], .ls[.i]) # nolint
  }
  .len <- length(.ls)
  if (.has_ini) {
    .ret[[.len + 2L]] <- ui$iniFun
    .ret[[.len + 3L]] <- bquote(model(.(as.call(c(quote(`{`), expr)))))
  } else {
    .ret[[.len + 2L]] <- bquote(model(.(as.call(c(quote(`{`), expr)))))
  }
  .fun <- function() {}
  body(.fun) <- as.call(.ret)
  if (is.function(ui$model)) environment(.fun) <- environment(ui$model)
  suppressMessages(as.rxUi(.fun)) # nolint
}

#' Convert ODE-based linear compartment models to analytical linCmt() form
#'
#' Detects whether a model's ODE equations form a standard 1-3 compartment
#' pharmacokinetic system and, if so, replaces them with a \code{linCmt()}
#' analytical solution. The conversion removes all \code{d/dt()} equations
#' and the central-compartment output assignment (e.g. \code{cp <- central/v}),
#' adding \code{cp <- linCmt()} in their place. All other model lines
#' (parameter assignments, error model, etc.) are preserved unchanged.
#'
#' Detection succeeds when:
#' \enumerate{
#'   \item Every ODE right-hand side is linear in the state variables.
#'   \item There are 1-4 compartments total (depot + 1-3 pharmacokinetic
#'         compartments).
#'   \item The compartment topology matches a standard PK model
#'         (optional depot feeding a central compartment, with 0-2 peripheral
#'         compartments exchanging bidirectionally with central).
#'   \item There is exactly one output line of the form
#'         \code{var <- central_cmt / v_expr}.
#' }
#'
#' The converted model retains all named parameter assignments
#' (\code{cl}, \code{v}, \code{ka}, \code{q}, \code{vp}, etc.) so that
#' \code{linCmt()} can infer the parameterization automatically. For models
#' that already use canonical PK parameter names, no additional assignments
#' are added.
#'
#' @param ui rxUi-like model object (function, rxUi, or anything accepted by
#'   \code{as.rxUi}).
#' @return An rxUi model with ODE equations replaced by \code{linCmt()}, or
#'   (with a message) the original model unchanged if conversion is not possible.
#' @seealso \code{\link{linToOde}} for the inverse transformation.
#' @examples
#' oneCmtOde <- function() {
#'   ini({
#'     tka <- 0.45
#'     tcl <- log(2.7)
#'     tv <- 3.45
#'     add.sd <- 0.7
#'   })
#'   model({
#'     ka  <- exp(tka)
#'     cl  <- exp(tcl)
#'     v   <- exp(tv)
#'     d/dt(depot)   <- -ka * depot
#'     d/dt(central) <- ka * depot - cl/v * central
#'     cp <- central / v
#'     cp ~ add(add.sd)
#'   })
#' }
#' linCmtModel <- odeToLin(oneCmtOde)
#'
#' twoCmtOde <- function() {
#'   ini({
#'     tcl <- 1; tv <- 3; tq <- 0.5; tvp <- 6; add.sd <- 0.7
#'   })
#'   model({
#'     cl <- exp(tcl)
#'     v <- exp(tv)
#'     q <- exp(tq)
#'     vp <- exp(tvp)
#'     d/dt(central)    <- -(cl+q)/v * central + q/vp * peripheral
#'     d/dt(peripheral) <-  q/v * central - q/vp * peripheral
#'     cp <- central / v
#'     cp ~ add(add.sd)
#'   })
#' }
#' linCmtModel2 <- odeToLin(twoCmtOde)
#'
#' @author Matthew Fidler
#' @export
odeToLin <- function(ui) {
  .ui <- as.rxUi(ui) # nolint
  .ui <- rxUiDecompress(.ui) # nolint

  .info <- .odeToLinDetect(.ui)
  if (is.null(.info)) {
    message("model does not appear to be a linear compartment ODE; returning unchanged")
    return(rxUiCompress(.ui)) # nolint
  }

  .expr <- .odeToLinBuildExpr(.ui$lstExpr, .info)
  .rebuildRxUiFromExpr(.ui, .expr)
}
