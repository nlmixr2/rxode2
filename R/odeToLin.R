## Collect additive terms from an expression tree, tracking sign.
## Returns list of {sign=+-1, expr}.
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

## Return all free symbol names referenced in an expression, excluding the
## heads of calls (i.e. function names).  Used to gather the PK parameter
## names appearing in ODE rate coefficients so they can be passed explicitly
## to linCmt().
.freeSymbolsInExpr <- function(expr) {
  if (is.name(expr)) return(as.character(expr))
  if (!is.call(expr)) return(character(0))
  unique(unlist(lapply(as.list(expr)[-1L], .freeSymbolsInExpr)))
}

## Extract the coefficient from `coef * stateNm` or `stateNm * coef`,
## where coef does not reference any state variable.
## Returns the coefficient expression, or NULL if the term is not that form.
.extractMultCoef <- function(expr, stateNm, states) {
  if (is.name(expr)) {
    if (as.character(expr) == stateNm) return(quote(1))
    return(NULL)
  }
  if (!is.call(expr)) return(NULL)
  .fn <- expr[[1]]
  if (identical(.fn, quote(`(`)) && length(expr) == 2L) {
    return(.extractMultCoef(expr[[2]], stateNm, states))
  }
  if (!identical(.fn, quote(`*`))) return(NULL)
  .lhs <- expr[[2]]; .rhs <- expr[[3]]
  .lu <- length(.statesInExpr(.lhs, states)) > 0L
  .ru <- length(.statesInExpr(.rhs, states)) > 0L
  if (.lu && !.ru && is.name(.lhs) && as.character(.lhs) == stateNm) return(.rhs)
  if (!.lu && .ru && is.name(.rhs) && as.character(.rhs) == stateNm) return(.lhs)
  NULL
}

## Parse one additive term: returns {sign, coef, state} where state=NA means
## a non-state (input/forcing) term. Returns NULL if the term is nonlinear
## (references multiple states or uses a state nonlinearly).
.parseOneLinTerm <- function(sign, termExpr, states) {
  .refs <- .statesInExpr(termExpr, states)
  if (length(.refs) == 0L) {
    return(list(sign = sign, coef = termExpr, state = NA_character_))
  }
  if (length(.refs) > 1L) return(NULL)
  .state <- .refs[1L]
  .coef  <- .extractMultCoef(termExpr, .state, states)
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

## Net signed coefficient (sum of sign*coef) of state `s` across a parsed term
## list, returned as a single expression; NULL when `s` does not appear.  Note
## the parsed sign is not enough on its own: a standalone `-ka*depot` RHS parses
## (by R precedence) as `(-ka)*depot`, i.e. a positive-sign term whose
## coefficient is `-ka` -- so the sign must be folded into the coefficient here.
.odeToLinNetCoef <- function(terms, s) {
  .rel <- Filter(function(.t) !is.na(.t$state) && .t$state == s, terms)
  if (length(.rel) == 0L) return(NULL)
  .signed <- lapply(.rel, function(.t) if (.t$sign < 0L) bquote(-(.(.t$coef))) else .t$coef)
  Reduce(function(.a, .b) bquote(.(.a) + .(.b)), .signed)
}

## Verify mass balance for every non-central compartment.  A genuine depot or
## peripheral exchanges with central with no independent loss: the flux it loses
## (its net coefficient of itself in its own ODE) exactly equals the flux central
## gains from it (central's net coefficient of that state), so the two cancel.  A
## metabolite (e.g. `d/dt(met) <- kpm*central - kelm*met - kbt*met`) carries an
## extra independent-elimination term, so its loss (kelm + kbt) exceeds the kbt
## returned to central; linCmt() would silently model it as a lossless peripheral
## and give wrong results, so such systems must NOT convert.  The cancellation is
## checked numerically (robust to how the coefficients are written) at two
## distinct positive parameter assignments.
.odeToLinMassBalanced <- function(odes, central, others) {
  .byCmt <- setNames(odes, vapply(odes, function(.o) .o$cmt, character(1)))
  .centralOde <- .byCmt[[central]]
  if (is.null(.centralOde)) return(FALSE)
  for (.c in others) {
    .ode <- .byCmt[[.c]]
    if (is.null(.ode)) return(FALSE)
    .selfNet <- .odeToLinNetCoef(.ode$terms, .c)        # what C loses (a net outflow)
    .centNet <- .odeToLinNetCoef(.centralOde$terms, .c) # what central gains from C
    if (is.null(.selfNet) || is.null(.centNet)) return(FALSE)
    .syms <- unique(c(.freeSymbolsInExpr(.selfNet), .freeSymbolsInExpr(.centNet)))
    .balanced <- function(.offset) {
      .vals <- as.list(setNames(seq_along(.syms) + .offset, .syms))
      .v <- tryCatch(eval(bquote(.(.selfNet) + .(.centNet)), .vals, baseenv()),
                     error = function(e) NA_real_)
      length(.v) == 1L && is.finite(.v) && abs(.v) < 1e-8
    }
    if (!.balanced(1.5) || !.balanced(3.7)) return(FALSE)
  }
  TRUE
}

## Detect the topology of a linear ODE system and classify each compartment.
##
## odes: list of {cmt=name, terms=parsed_terms} for each ODE
## outputCmt: the state variable identified as the central compartment
##   (from the output line var <- cmt / v).
##
## Returns list(ncmt, oral0, central, depot, peripheral1, peripheral2) or NULL.
.odeToLinDetectTopology <- function(odes, outputCmt, cmtNames) {
  .n <- length(odes)
  if (.n == 0L || .n > 4L) return(NULL)

  ## Build inflow map: flowsIn[[cmt]] = list of {from, coef} for positive
  ## cross-compartment terms in cmt's ODE.
  .flowsIn  <- setNames(vector("list", .n), cmtNames)
  .flowsOut <- setNames(vector("list", .n), cmtNames)

  for (.ode in odes) {
    .cmt <- .ode$cmt
    for (.t in .ode$terms) {
      if (is.na(.t$state) || .t$state == .cmt) next
      if (.t$sign > 0L) {
        .flowsIn[[.cmt]] <- c(.flowsIn[[.cmt]],
                              list(list(from = .t$state, coef = .t$coef)))
      }
    }
  }
  ## Derive outflow map from inflow map.
  for (.cmt in cmtNames) {
    for (.f in .flowsIn[[.cmt]]) {
      .flowsOut[[.f$from]] <- c(.flowsOut[[.f$from]], list(list(to = .cmt)))
    }
  }

  .central <- outputCmt
  if (!.central %in% cmtNames) return(NULL)

  .depot       <- NULL
  .peripherals <- character(0)

  for (.cmt in cmtNames[cmtNames != .central]) {
    .nIn  <- length(.flowsIn[[.cmt]])
    .out  <- .flowsOut[[.cmt]]
    .nOut <- length(.out)

    .allOutToCentral  <- .nOut > 0L &&
      all(vapply(.out, function(.f) .f$to == .central, logical(1)))
    .allInFromCentral <- .nIn > 0L &&
      all(vapply(.flowsIn[[.cmt]], function(.f) .f$from == .central, logical(1)))

    if (.nIn == 0L && .nOut == 1L && .allOutToCentral) {
      ## Depot: no inflows, exactly one outflow to central
      if (!is.null(.depot)) return(NULL)
      .depot <- .cmt
    } else if (.nIn > 0L && .allInFromCentral && .allOutToCentral) {
      ## Peripheral: inflow from central, outflow to central
      .peripherals <- c(.peripherals, .cmt)
    } else {
      return(NULL)
    }
  }

  .ncmt <- 1L + length(.peripherals)
  if (.ncmt > 3L) return(NULL)

  ## Reject systems where a depot/peripheral has independent loss (e.g. a
  ## metabolite), which linCmt() cannot represent.
  .others <- c(.peripherals, if (is.null(.depot)) character(0) else .depot)
  if (!.odeToLinMassBalanced(odes, .central, .others)) return(NULL)

  list(
    ncmt        = .ncmt,
    oral0       = if (is.null(.depot)) 0L else 1L,
    central     = .central,
    depot       = .depot,
    peripheral1 = if (length(.peripherals) >= 1L) .peripherals[1L] else NULL,
    peripheral2 = if (length(.peripherals) >= 2L) .peripherals[2L] else NULL
  )
}

## Find a line of the form  var <- cmt / vExpr  where cmt is a state variable.
## Returns {var, cmt, vExpr, lineIdx} or NULL.
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
    .cmtNm <- as.character(.num)
    if (!.cmtNm %in% states) next
    return(list(
      var     = as.character(.e[[2]]),
      cmt     = .cmtNm,
      vExpr   = .rhs[[3]],
      lineIdx = .i
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

## Collect the names of state variables referenced as *values* in an
## expression.  The compartment-position argument of an `f`/`rate`/`dur`/`alag`
## modifier or an adaptive-dosing call (`bolus`, `infuse`, ...) is NOT a value
## reference: those positions are rewritten to the standard linCmt compartment
## names by .odeToLinRenameExpr, so they never block conversion.  Anything else
## that names a state -- e.g. a peripheral observable `periph` in
## `Cp <- periph / vp` -- is a value reference.
.odeToLinValueStateRefs <- function(expr, states) {
  if (is.name(expr)) {
    .nm <- as.character(expr)
    return(if (.nm %in% states) .nm else character(0))
  }
  if (!is.call(expr)) return(character(0))
  .fn <- if (is.name(expr[[1L]])) as.character(expr[[1L]]) else ""
  ## f/rate/dur/alag(<cmt>, ...): the first argument is a compartment position.
  if (.fn %in% c("f", "rate", "dur", "alag")) {
    .rest <- as.list(expr)[-1L]
    if (length(.rest) >= 1L) .rest <- .rest[-1L]
    return(unique(unlist(lapply(.rest, .odeToLinValueStateRefs, states = states))))
  }
  ## Adaptive dosing calls: the compartment-position argument is rewritten too.
  ## Indices mirror .odeToLinRenameAdaptiveCall.
  .cmtIdx <- switch(.fn,
    bolus = 3L, replace = 3L, multiply = 3L, phantom = 3L,
    infuse = 4L, infuseDur = 4L,
    `evid_` = 5L,
    NULL)
  .idx <- seq_along(expr)[-1L]
  if (!is.null(.cmtIdx)) .idx <- setdiff(.idx, .cmtIdx)
  unique(unlist(lapply(.idx, function(.k) .odeToLinValueStateRefs(expr[[.k]], states))))
}

## TRUE when a compartment state is referenced as a value by some model line
## other than the ODE equations and the single central output line (e.g. a
## peripheral observable `Cp <- periph / vp`, or a state alias).  Such a
## reference cannot be served by a bare central-only linCmt() output, so the
## caller takes the coupled-conversion path (rename the compartments to their
## canonical linCmt names) when it is safe, and otherwise keeps the explicit
## ODEs.
.odeToLinStateReferencedElsewhere <- function(lstExpr, cmtNames, odeIdx, outputIdx) {
  for (.i in seq_along(lstExpr)) {
    if (.i %in% odeIdx || .i == outputIdx) next
    if (length(.odeToLinValueStateRefs(lstExpr[[.i]], cmtNames)) > 0L) {
      return(TRUE)
    }
  }
  FALSE
}

## TRUE when every residual/endpoint (`~`) line predicts the central output
## variable.  A linCmt() model can only observe the central concentration as an
## estimated endpoint: mapping a peripheral observable to its own endpoint
## (`Cp ~ ... | peripheral1`) collides with linCmt()'s internal peripheral
## compartment ("required for linCmt() but defined in ODE too").  So a coupled
## system converts only when the peripheral references are *output-only*.
.odeToLinAllEndpointsCentral <- function(lstExpr, outputVar) {
  for (.e in lstExpr) {
    if (!is.call(.e) || !identical(.e[[1]], quote(`~`)) || length(.e) < 3L) next
    .lhs <- .e[[2]]
    if (!is.name(.lhs) || as.character(.lhs) != outputVar) return(FALSE)
  }
  TRUE
}

## Recursively rename every reference to an ODE compartment name to its
## canonical linCmt name throughout an expression (value references included,
## e.g. `Cp <- periph / vp` -> `Cp <- peripheral1 / vp`).  Used only on the
## coupled-conversion path; compartment names live in a separate namespace from
## parameters, so a name match is unambiguous.
.odeToLinRenameValueRefs <- function(e, cmtMap) {
  if (is.name(e)) {
    .new <- cmtMap[as.character(e)]
    if (!is.na(.new)) return(as.name(.new))
    return(e)
  }
  if (!is.call(e)) return(e)
  for (.i in seq_along(e)) e[[.i]] <- .odeToLinRenameValueRefs(e[[.i]], cmtMap)
  e
}

## Map the central endpoint to the `central` compartment (`Cc ~ err` ->
## `Cc ~ err | central`).  Anchoring the endpoint to the existing central
## compartment stops rxode2 injecting a fresh `cmt(<pred>)` observation slot,
## which would otherwise flip the model into explicit-compartment mode and make
## in-model peripheral references resolve to 0.  Left unchanged if already
## conditioned with `|`.
.odeToLinAddCentralCond <- function(e) {
  .rhs <- e[[3]]
  if (is.call(.rhs) && identical(.rhs[[1]], quote(`|`))) return(e)
  e[[3]] <- call("|", .rhs, as.name("central"))
  e
}

## Attempt to detect whether ui is a linear compartment ODE model.
## Returns a list with topology + output info, or NULL if not convertible.
.odeToLinDetect <- function(ui) {
  .lstExpr <- ui$lstExpr
  .states  <- rxModelVars(ui)$state # nolint

  if (length(.states) == 0L) return(NULL)

  ## Gather ODE lines and their compartment names.
  .odeIdx  <- integer(0)
  .cmtNames <- character(0)
  for (.i in seq_along(.lstExpr)) {
    .e <- .lstExpr[[.i]]
    if (!is.call(.e)) next
    if (!identical(.e[[1]], quote(`<-`)) && !identical(.e[[1]], quote(`=`))) next
    if (length(.e) < 3L || !is.call(.e[[2]])) next
    if (!.isDtExpr(.e[[2]])) next
    .odeIdx   <- c(.odeIdx, .i)
    .cmtNames <- c(.cmtNames, .getDtCmt(.e[[2]]))
  }

  if (length(.odeIdx) == 0L || length(.odeIdx) > 4L) return(NULL)
  if (!all(.cmtNames %in% .states)) return(NULL)

  ## Parse each ODE RHS; bail if any is nonlinear in state variables.
  .odes <- lapply(seq_along(.odeIdx), function(.j) {
    .e <- .lstExpr[[.odeIdx[.j]]]
    .terms <- .parseLinearRhs(.e[[3]], .states)
    if (is.null(.terms)) return(NULL)
    list(cmt = .cmtNames[.j], terms = .terms)
  })
  if (any(vapply(.odes, is.null, logical(1)))) return(NULL)

  ## Find output line: var <- centralCmt / vExpr
  .out <- .odeToLinFindOutput(.lstExpr, .states)
  if (is.null(.out)) return(NULL)

  ## Classify topology.
  .topo <- .odeToLinDetectTopology(.odes, .out$cmt, .cmtNames)
  if (is.null(.topo)) return(NULL)

  ## Handle compartment states referenced as a value outside the ODE equations
  ## and the central output line (e.g. a peripheral observable `Cp <- periph /
  ## vp`).  linCmt() materializes central/peripheral1/peripheral2/depot as
  ## accessible solved compartments, so rather than dropping the coupled state
  ## we keep the model analytic by renaming the ODE compartments to their
  ## canonical linCmt names (the coupled path; see .odeToLinBuildExpr).  This is
  ## only valid when every estimated endpoint predicts the central output --
  ## a peripheral with its own `~` endpoint collides with linCmt()'s internal
  ## compartment -- so such models keep their explicit ODE states instead.
  .coupled <- FALSE
  if (.odeToLinStateReferencedElsewhere(.lstExpr, .cmtNames, .odeIdx, .out$lineIdx)) {
    if (!.odeToLinAllEndpointsCentral(.lstExpr, .out$var)) return(NULL)
    .coupled <- TRUE
  }

  ## Collect the PK parameter names referenced in the rate coefficients and
  ## the volume expression so they can be passed explicitly to linCmt().
  ## Passing them explicitly lets linCmt() infer the parameterization even
  ## when the parameters are defined only in ini() (no model-body assignment).
  .params <- character(0)
  for (.ode in .odes) {
    for (.t in .ode$terms) {
      if (!is.na(.t$state)) {
        .params <- c(.params, .freeSymbolsInExpr(.t$coef))
      }
    }
  }
  .params <- c(.params, .freeSymbolsInExpr(.out$vExpr))
  .params <- setdiff(unique(.params),
                     c(.states, .out$var, "t", "time", "pi"))

  c(.topo, list(
    outputVar = .out$var,
    outputCmt = .out$cmt,
    vExpr     = .out$vExpr,
    outputIdx = .out$lineIdx,
    odeIdx    = .odeIdx,
    params    = .params,
    coupled   = .coupled
  ))
}

## Build a new lstExpr with ODE lines removed and the output line replaced by
## `outputVar <- linCmt()`.
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
  ## On the coupled path the peripheral compartments are referenced by retained
  ## model lines (observables), so they too must map to their canonical linCmt
  ## names to resolve against the analytic solution's solved compartments.
  if (isTRUE(info$coupled)) {
    if (!is.null(info$peripheral1)) .map[info$peripheral1] <- "peripheral1"
    if (!is.null(info$peripheral2)) .map[info$peripheral2] <- "peripheral2"
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

## Rename the compartment argument in an adaptive dosing call.
## Argument positions (AST index, where e[[1]] is the function name):
##   bolus(amt, cmt, ...)       -> e[[3]]
##   replace(amt, cmt)          -> e[[3]]
##   multiply(amt, cmt)         -> e[[3]]
##   phantom(amt, cmt, ...)     -> e[[3]]
##   infuse(amt, rate, cmt, ...) -> e[[4]]
##   infuseDur(amt, dur, cmt, ...) -> e[[4]]
##   evid_(time, evid, amt, cmt, ...) -> e[[5]]
## obs() and reset() have no compartment argument and are left unchanged.
.odeToLinRenameAdaptiveCall <- function(e, cmtMap) {
  if (!is.call(e)) return(e)
  .fn <- as.character(e[[1]])
  .cmtIdx <- switch(.fn,
    bolus = 3L, replace = 3L, multiply = 3L, phantom = 3L,
    infuse = 4L, infuseDur = 4L,
    `evid_` = 5L,
    NULL
  )
  if (is.null(.cmtIdx)) return(e)
  if (length(e) < .cmtIdx) return(e)
  .cmtArg <- e[[.cmtIdx]]
  if (!is.name(.cmtArg)) return(e)
  .cmtNm <- as.character(.cmtArg)
  .newNm <- cmtMap[.cmtNm]
  if (is.na(.newNm)) return(e)
  e[[.cmtIdx]] <- as.name(.newNm)
  e
}

## Recursively rename compartments throughout an expression tree.
## Handles f/rate/dur/alag assignments and adaptive dosing calls.
## Recurses into if/block/other constructs to find nested calls.
.odeToLinRenameExpr <- function(e, cmtMap) {
  if (!is.call(e)) return(e)
  .fn <- as.character(e[[1]])
  if (.fn %in% c("<-", "=") && length(e) >= 3L && is.call(e[[2]])) {
    .innerFn <- as.character(e[[2]][[1]])
    if (.innerFn %in% c("f", "rate", "dur", "alag")) {
      return(.odeToLinRenameCmt(e, cmtMap))
    }
  }
  if (.fn %in% c("bolus", "infuse", "infuseDur", "replace", "multiply", "phantom", "evid_")) {
    return(.odeToLinRenameAdaptiveCall(e, cmtMap))
  }
  for (.i in seq_along(e)) {
    .child <- e[[.i]]
    if (is.call(.child)) {
      e[[.i]] <- .odeToLinRenameExpr(.child, cmtMap)
    }
  }
  e
}

.odeToLinBuildExpr <- function(lstExpr, info) {
  ## Build `linCmt(<params>)` with the detected PK parameter names so the
  ## parameterization is inferred even when parameters live only in ini().
  ## linCmt() resolves its parameters by name, so the argument order does not
  ## matter.  Fall back to a bare linCmt() if no parameters were detected.
  .linCmtCall <- if (length(info$params) > 0L) {
    as.call(c(list(as.name("linCmt")), lapply(info$params, as.name)))
  } else {
    str2lang("linCmt()")
  }
  .linCmtLine <- call("<-", as.name(info$outputVar), .linCmtCall)
  .cmtMap <- .odeToLinCmtMap(info)
  .ret <- list()
  for (.i in seq_along(lstExpr)) {
    if (.i %in% info$odeIdx) {
      next  # remove ODE lines
    } else if (.i == info$outputIdx) {
      .ret[[length(.ret) + 1L]] <- .linCmtLine  # replace output with linCmt()
    } else if (isTRUE(info$coupled)) {
      ## Coupled path: rename every compartment reference (values included) to
      ## its canonical linCmt name, and anchor the central endpoint to the
      ## central compartment so no observation compartment is injected.
      .e <- lstExpr[[.i]]
      if (is.call(.e) && identical(.e[[1]], quote(`~`)) && length(.e) >= 3L &&
          is.name(.e[[2]]) && as.character(.e[[2]]) == info$outputVar) {
        .e <- .odeToLinAddCentralCond(.e)
      }
      .ret[[length(.ret) + 1L]] <- .odeToLinRenameValueRefs(.e, .cmtMap)
    } else {
      .e <- .odeToLinRenameExpr(lstExpr[[.i]], .cmtMap)
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
  .ls     <- ls(ui$meta, all.names = TRUE)
  .hasIni <- length(ui$iniDf$cond) > 0L
  .ret <- vector("list", length(.ls) + if (.hasIni) 3L else 2L)
  .ret[[1L]] <- quote(`{`)
  for (.i in seq_along(.ls)) {
    .ret[[.i + 1L]] <- rxUiDeparse(ui$meta[[.ls[.i]]], .ls[.i]) # nolint
  }
  .len <- length(.ls)
  if (.hasIni) {
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
#'     tcl <- 1
#'     tv <- 3
#'     tq <- 0.5
#'     tvp <- 6
#'     add.sd <- 0.7
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
