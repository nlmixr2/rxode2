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

## TRUE when an expression is a constant that evaluates to exactly zero.  A
## zero term adds nothing to a rate, so dropping it is lossless.
.odeToLinIsZeroExpr <- function(expr) {
  if (length(all.vars(expr)) > 0L) return(FALSE)
  .v <- tryCatch(eval(expr, baseenv()), error = function(e) NA_real_)
  is.numeric(.v) && length(.v) == 1L && !is.na(.v) && .v == 0
}

## Parse one additive term: returns {sign, coef, state}, `NA` for a constant
## zero term (the caller drops it), or NULL when the term cannot be carried
## into linCmt().
##
## A term referencing no state is an exogenous input -- transit() absorption, a
## zero-order or endogenous production rate, a dose supplied through a
## covariate column.  linCmt() is driven entirely by the event table dosing
## records and has no parameter able to carry such a term, so a non-zero one is
## rejected exactly like a nonlinear term.  (It used to be parsed as
## `state = NA` and then dropped by both the topology detector and the
## reconstruction, silently solving a different model than the one written.)
.parseOneLinTerm <- function(sign, termExpr, states) {
  .refs <- .statesInExpr(termExpr, states)
  if (length(.refs) == 0L) {
    if (.odeToLinIsZeroExpr(termExpr)) return(NA)
    return(NULL)
  }
  if (length(.refs) > 1L) return(NULL)
  .state <- .refs[1L]
  .coef  <- .extractMultCoef(termExpr, .state, states)
  if (is.null(.coef)) return(NULL)
  list(sign = sign, coef = .coef, state = .state)
}

## Parse an ODE RHS into a flat list of {sign, coef, state} terms, every one
## proportional to exactly one state.  Returns NULL if the RHS is not linear in
## all state variables or carries a non-zero exogenous input term.
.parseLinearRhs <- function(rhs, states) {
  .raw    <- .collectAddTerms(rhs)
  .parsed <- lapply(.raw, function(.t) .parseOneLinTerm(.t$sign, .t$expr, states))
  if (any(vapply(.parsed, is.null, logical(1)))) return(NULL)
  Filter(is.list, .parsed) # drop the constant-zero terms
}

## Net signed coefficient (sum of sign*coef) of state `s`, as a single
## expression; NULL when `s` is absent.  The parsed sign is folded into the
## coefficient (`-ka*depot` parses as a positive-sign term with coef `-ka`).
.odeToLinNetCoef <- function(terms, s) {
  .rel <- Filter(function(.t) !is.na(.t$state) && .t$state == s, terms)
  if (length(.rel) == 0L) return(NULL)
  .signed <- lapply(.rel, function(.t) if (.t$sign < 0L) bquote(-(.(.t$coef))) else .t$coef)
  Reduce(function(.a, .b) bquote(.(.a) + .(.b)), .signed)
}

## Verify mass balance for every non-central compartment: a genuine
## depot/peripheral loses exactly the flux central gains from it (the two net
## coefficients cancel).  A metabolite has an extra independent-elimination
## term that breaks the cancellation, so it must NOT convert.  Checked
## numerically at two distinct positive parameter assignments.
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
    if (!.balanced(.odeToLinProbe[1L]) || !.balanced(.odeToLinProbe[2L])) return(FALSE)
  }
  TRUE
}

## Offsets for the two parameter assignments the numeric guards probe at.
## Irrational, so no expression with rational coefficients can cancel at both
## by coincidence -- a guard that only samples round numbers can be satisfied
## by a quadratic whose roots are exactly those numbers.
.odeToLinProbe <- c(sqrt(2), exp(1))

## Evaluate an expression to one finite number under `vals`; NA on failure.
.odeToLinNum <- function(expr, vals) {
  .v <- tryCatch(eval(expr, vals, baseenv()), error = function(e) NA_real_)
  if (!is.numeric(.v) || length(.v) != 1L || !is.finite(.v)) return(NA_real_)
  as.numeric(.v)
}

## Relative-tolerance equality for the rate comparisons.
.odeToLinNear <- function(a, b) {
  !is.na(a) && !is.na(b) && abs(a - b) <= 1e-8 * max(1, abs(a), abs(b))
}

## The rate constants and central volume linCmt() will use for `params` taking
## `vals`.  rxDerived() runs the same `_linCmtParse` parameterization inference
## that linCmt() does, so this cannot drift from it.  NULL when the names are
## not a parameterization linCmt() recognizes.
.odeToLinDerivedRates <- function(params, vals) {
  .d <- tryCatch(do.call(rxDerived, vals[params]), error = function(e) NULL) # nolint
  if (!is.data.frame(.d) || nrow(.d) != 1L) return(NULL)
  .get <- function(.n) {
    if (is.null(.d[[.n]])) return(0)
    .v <- as.numeric(.d[[.n]][1L])
    if (!is.finite(.v)) return(NA_real_)
    .v
  }
  list(kel = .get("kel"), k12 = .get("k12"), k21 = .get("k21"),
       k13 = .get("k13"), k31 = .get("k31"), vc = .get("vc"))
}

## TRUE when linCmt(<params>) reproduces this system's own rate constants and
## reported volume.  The emitted call passes parameter NAMES only, so the
## structure of a rate coefficient is otherwise discarded: `- 2 * kel * central`
## would solve as if it eliminated at `kel`, and `cp <- central / (2 * v)` would
## report `central / vc`.  Compared at two parameter assignments.
.odeToLinRatesMatch <- function(odes, topo, params, vExpr) {
  if (length(params) == 0L) return(FALSE)
  .byCmt <- setNames(odes, vapply(odes, function(.o) .o$cmt, character(1)))
  .matches <- function(.offset) {
    .vals <- as.list(setNames(as.numeric(seq_along(params)) + .offset, params))
    .r <- .odeToLinDerivedRates(params, .vals)
    if (is.null(.r)) return(FALSE)
    .net <- function(.cmt, .state) {
      .e <- .odeToLinNetCoef(.byCmt[[.cmt]]$terms, .state)
      if (is.null(.e)) return(NA_real_)
      .odeToLinNum(.e, .vals)
    }
    ## linCmt() reports central / vc.
    if (!.odeToLinNear(.odeToLinNum(vExpr, .vals), .r$vc)) return(FALSE)
    ## linCmt() absorbs at the value of the parameter named ka, so the depot
    ## rate must be that parameter itself, unscaled.
    if (!is.null(topo$depot)) {
      .e <- .odeToLinNetCoef(.byCmt[[topo$depot]]$terms, topo$depot)
      if (is.null(.e)) return(FALSE)
      .sym <- .freeSymbolsInExpr(.e)
      if (length(.sym) != 1L || is.null(.vals[[.sym]])) return(FALSE)
      if (!.odeToLinNear(-.odeToLinNum(.e, .vals), .vals[[.sym]])) return(FALSE)
    }
    ## Central loses kel plus every peripheral transfer.
    .peri <- list(list(topo$peripheral1, .r$k12, .r$k21),
                  list(topo$peripheral2, .r$k13, .r$k31))
    .out <- .r$kel +
      (if (is.null(topo$peripheral1)) 0 else .r$k12) +
      (if (is.null(topo$peripheral2)) 0 else .r$k13)
    if (!.odeToLinNear(-.net(topo$central, topo$central), .out)) return(FALSE)
    for (.p in .peri) {
      if (is.null(.p[[1L]])) next
      if (!.odeToLinNear(.net(.p[[1L]], topo$central), .p[[2L]])) return(FALSE)
      if (!.odeToLinNear(-.net(.p[[1L]], .p[[1L]]), .p[[3L]])) return(FALSE)
      if (!.odeToLinNear(.net(topo$central, .p[[1L]]), .p[[3L]])) return(FALSE)
    }
    TRUE
  }
  .matches(.odeToLinProbe[1L]) && .matches(.odeToLinProbe[2L])
}

## Detect the topology of a linear ODE system and classify each compartment.
## odes: list of {cmt, terms}; outputCmt: the central compartment (from the
## output line).  Returns list(ncmt, oral0, central, depot, peripheral1,
## peripheral2) or NULL.
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

## Names of model-defined variables whose value transitively depends on a
## compartment state -- e.g. the central concentration `Cc <- central / vc`,
## or anything built from it.  A term in an ODE RHS that references one of
## these is state-dependent even when it names no state directly, so it must
## block linCmt() conversion (see `.odeToLinDetect`).  Lines defining the ODEs
## themselves are excluded; only assignments to a plain name are tracked.
.odeToLinStateDerivedVars <- function(lstExpr, states, odeIdx) {
  .tainted <- character(0)
  repeat {
    .added <- FALSE
    for (.i in seq_along(lstExpr)) {
      if (.i %in% odeIdx) next
      .e <- lstExpr[[.i]]
      if (!is.call(.e)) next
      if (!identical(.e[[1]], quote(`<-`)) && !identical(.e[[1]], quote(`=`))) next
      if (length(.e) < 3L || !is.name(.e[[2]])) next
      .lhs <- as.character(.e[[2]])
      if (.lhs %in% .tainted) next
      if (any(all.vars(.e[[3]]) %in% c(states, .tainted))) {
        .tainted <- c(.tainted, .lhs)
        .added <- TRUE
      }
    }
    if (!.added) break
  }
  unique(.tainted)
}

## Index and compartment name of every `d/dt(<cmt>) <- ...` line, in order.
.odeToLinOdeLines <- function(lstExpr) {
  .odeIdx   <- integer(0)
  .cmtNames <- character(0)
  for (.i in seq_along(lstExpr)) {
    .e <- lstExpr[[.i]]
    if (!is.call(.e)) next
    if (!identical(.e[[1]], quote(`<-`)) && !identical(.e[[1]], quote(`=`))) next
    if (length(.e) < 3L || !is.call(.e[[2]])) next
    if (!.isDtExpr(.e[[2]])) next
    .odeIdx   <- c(.odeIdx, .i)
    .cmtNames <- c(.cmtNames, .getDtCmt(.e[[2]]))
  }
  list(odeIdx = .odeIdx, cmtNames = .cmtNames)
}

## Compartments whose ODE carries a non-zero exogenous input term, with the
## offending term deparsed.  Diagnostic only: it explains a declined conversion
## in `odeToLin()` and is never called on the solve path.
.odeToLinExogenousInputs <- function(lstExpr, states) {
  .lines <- .odeToLinOdeLines(lstExpr)
  .ret <- character(0)
  for (.j in seq_along(.lines$odeIdx)) {
    .rhs <- lstExpr[[.lines$odeIdx[.j]]][[3]]
    for (.t in .collectAddTerms(.rhs)) {
      if (length(.statesInExpr(.t$expr, states)) > 0L) next
      if (.odeToLinIsZeroExpr(.t$expr)) next
      .ret[.lines$cmtNames[.j]] <- deparse1(.t$expr)
      break
    }
  }
  .ret
}

## Attempt to detect whether ui is a linear compartment ODE model.
## Returns a list with topology + output info, or NULL if not convertible.
.odeToLinDetect <- function(ui) {
  .lstExpr <- ui$lstExpr
  .states  <- rxModelVars(ui)$state # nolint

  if (length(.states) == 0L) return(NULL)

  ## Gather ODE lines and their compartment names.
  .lines    <- .odeToLinOdeLines(.lstExpr)
  .odeIdx   <- .lines$odeIdx
  .cmtNames <- .lines$cmtNames

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

  ## Bail when an ODE RHS depends on a state indirectly through a state-derived
  ## value (e.g. Michaelis-Menten via `Cc <- central/vc`), which the direct
  ## linearity check misses; keep the explicit ODE states.
  .stateDerived <- .odeToLinStateDerivedVars(.lstExpr, .states, .odeIdx)
  if (length(.stateDerived) > 0L &&
        any(vapply(.odes, function(.o) {
          any(vapply(.o$terms,
                     function(.t) any(all.vars(.t$coef) %in% .stateDerived),
                     logical(1)))
        }, logical(1)))) {
    return(NULL)
  }

  ## Find output line: var <- centralCmt / vExpr
  .out <- .odeToLinFindOutput(.lstExpr, .states)
  if (is.null(.out)) return(NULL)

  ## Classify topology.
  .topo <- .odeToLinDetectTopology(.odes, .out$cmt, .cmtNames)
  if (is.null(.topo)) return(NULL)

  ## Compartment states referenced as a value outside the ODEs and the central
  ## output line (e.g. `Cp <- periph/vp`): keep the model analytic by renaming
  ## the ODE compartments to their canonical linCmt names (the coupled path),
  ## valid only when every endpoint predicts the central output.
  .coupled <- FALSE
  if (.odeToLinStateReferencedElsewhere(.lstExpr, .cmtNames, .odeIdx, .out$lineIdx)) {
    if (!.odeToLinAllEndpointsCentral(.lstExpr, .out$var)) return(NULL)
    .coupled <- TRUE
  }

  ## PK parameter names from the rate coefficients and volume expression,
  ## passed explicitly to linCmt() so it can infer the parameterization even
  ## for ini()-only parameters.
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

  ## linCmt() rebuilds the rate constants from those names alone, so refuse
  ## unless they reproduce the system that was written.
  if (!.odeToLinRatesMatch(.odes, .topo, .params, .out$vExpr)) return(NULL)

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

## Rename the compartment argument in an adaptive dosing call.  cmt AST index:
## bolus/replace/multiply/phantom -> e[[3]], infuse/infuseDur -> e[[4]],
## evid_ -> e[[5]]; obs()/reset() have no cmt and are left unchanged.
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

## Cheap cache key: normalized text of all model lines plus the initial
## estimates.  The converted model rebuilt by `.rebuildRxUiFromExpr()` bakes in
## `ui$iniFun`, so two models that share the same `model({})` equations but
## differ only in their `ini({})` block must map to distinct cache entries.
## Keying on the equations alone caused the second model to reuse the first
## model's parameters (see the rxSolve(useLinCmt=TRUE) regression).
.odeToLinCacheKey <- function(ui) {
  .model <- paste0(vapply(ui$lstExpr, deparse1, character(1)), collapse = "\n")
  .ini <- if (length(ui$iniDf$cond) > 0L) deparse1(ui$iniFun) else ""
  paste0(.model, "\n#### ini ####\n", .ini)
}

## Package-scope cache: maps the same model-text key used by `.odeToLinCache`
## to how the converted model's compartments line up with the original's.
## Deriving that requires `rxModelVars()` on both models, which is constant per
## converted model, so it is computed once here rather than on every solve.
.odeToLinCmtInfoCache <- new.env(parent = emptyenv(), hash = TRUE)

## How many leading compartment INDICES address the same compartment in both
## models.  A numeric `cmt`, and an event table with no `cmt` column at all
## (which doses compartment 1), are only safe up to this many: the conversion
## renumbers whenever the ODE declares its compartments in a different order
## from linCmt()'s canonical `depot`, `central`, and it drops any peripheral
## the ODE declared as a state.
.odeToLinSafeCmtN <- function(original, converted, cmtMap) {
  .o <- rxModelVars(original)$state
  .c <- rxModelVars(converted)$state
  .n <- min(length(.o), length(.c))
  if (.n == 0L) return(0L)
  .head <- .o[seq_len(.n)]
  .m <- cmtMap[.head]
  .m[is.na(.m)] <- .head[is.na(.m)]
  ## A position is safe when the converted model holds that compartment under
  ## either its new name or its original one -- the latter covers the case
  ## where the rebuild failed and the caller is comparing a model with itself.
  .k <- which(.m != .c[seq_len(.n)] & .head != .c[seq_len(.n)])
  if (length(.k) == 0L) return(as.integer(.n))
  as.integer(.k[1L] - 1L)
}

## How the converted model's compartments line up with the original's: the
## names the conversion renamed away, the original's own state names, and how
## far numeric compartment indices stay valid.  Cached by model key.
.odeToLinCmtInfo <- function(cacheKey, original, converted, cmtMap) {
  if (exists(cacheKey, envir = .odeToLinCmtInfoCache, inherits = FALSE)) {
    return(get(cacheKey, envir = .odeToLinCmtInfoCache, inherits = FALSE))
  }
  .o <- rxModelVars(original)$state
  .c <- rxModelVars(converted)$state
  .ret <- list(
    lost   = setdiff(.o, .c),
    states = .o,
    nSafe  = .odeToLinSafeCmtN(original, converted, cmtMap),
    nMax   = max(length(.o), length(.c))
  )
  assign(cacheKey, .ret, envir = .odeToLinCmtInfoCache)
  .ret
}

## TRUE when no event data can tell the two models apart: every compartment
## index lines up and no name was renamed away.  Materializing an event table
## is not free, so the caller checks this first and skips it -- which is the
## common case, every model whose compartments convert one for one.
.odeToLinCmtAlwaysOk <- function(info) {
  info$nSafe >= info$nMax && length(info$lost) == 0L
}

## Is a converted linCmt() model safe to use for the given solve data?
##
## The conversion renames compartments (an ODE `centre` becomes linCmt's
## `central`) and renumbers them: linCmt() always orders its compartments
## `depot`, `central`, and it keeps no state for a peripheral the ODE declared.
## A record addressing a compartment by a *name* the converted model no longer
## has would be routed nowhere (all-zero predictions); one addressing it by an
## *index* past `info$nSafe` would be routed to a different compartment (a model
## declaring `d/dt(central)` before `d/dt(depot)` numbers them the other way
## round from linCmt(), so `cmt = 1` doses central before conversion and depot
## after).  Either way, fall back to the original ODE model.  An event table
## with no `cmt` column, and a name that is not a compartment of the original
## model such as the "(default)" placeholder, both address compartment 1.
.odeToLinCmtCompatible <- function(info, data) {
  .defaultOk <- info$nSafe >= 1L
  if (is.null(data) || !is.data.frame(data)) {
    return(.defaultOk)
  }
  .nm <- names(data)
  .col <- .nm[tolower(.nm) == "cmt"]
  if (length(.col) == 0L) {
    return(.defaultOk)
  }
  .cmt <- data[[.col[1L]]]
  if (is.factor(.cmt)) {
    .cmt <- as.character(.cmt)
  }
  if (!is.character(.cmt)) {
    .cmt <- suppressWarnings(as.numeric(.cmt))
    .cmt <- .cmt[!is.na(.cmt)]
    if (length(.cmt) == 0L) {
      return(.defaultOk)
    }
    ## A negative index turns a compartment off; it still names one.  An index
    ## past every compartment of both models is an observation slot the two
    ## handle alike -- NONMEM-style data observing a one compartment model in
    ## `cmt = 2` must keep converting.
    .k <- abs(.cmt)
    return(all(.k >= 1 & (.k <= info$nSafe | .k > info$nMax)))
  }
  .cmt <- unique(.cmt[!is.na(.cmt)])
  if (length(.cmt) == 0L) {
    return(.defaultOk)
  }
  if (any(.cmt %in% info$lost)) {
    return(FALSE)
  }
  if (!all(.cmt %in% info$states)) {
    return(.defaultOk)
  }
  TRUE
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
#' Detects whether a model's ODE equations form a standard 1-3 compartment PK
#' system and, if so, replaces the \code{d/dt()} equations and the central
#' output assignment (e.g. \code{cp <- central/v}) with \code{cp <- linCmt()},
#' preserving all other model lines.  Detection requires linear ODE
#' right-hand sides, 1-4 compartments in a standard depot/central/peripheral
#' topology, and one output line of the form \code{var <- central_cmt / v_expr}.
#' Named parameter assignments are retained so \code{linCmt()} can infer the
#' parameterization.  Every right-hand side term must be proportional to a
#' compartment; an exogenous input (\code{transit()} absorption, a zero-order or
#' endogenous production rate, a dose carried in a covariate column) has no
#' \code{linCmt()} representation and declines the conversion.  So does a rate
#' coefficient, or a concentration line, that is not what its named parameters
#' imply -- a scale factor, an inverted ratio or a covariate factor written into
#' the ODE -- since \code{linCmt()} rebuilds the rates from those names alone.
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
#' @author Matthew Fidler
#' @export
odeToLin <- function(ui) {
  .ui <- as.rxUi(ui) # nolint
  .ui <- rxUiDecompress(.ui) # nolint

  .info <- .odeToLinDetect(.ui)
  if (is.null(.info)) {
    .exo <- .odeToLinExogenousInputs(.ui$lstExpr, rxModelVars(.ui)$state) # nolint
    if (length(.exo) > 0L) {
      message("linCmt() cannot carry the input term",
              if (length(.exo) > 1L) "s" else "", " ",
              paste0("`", .exo, "` in d/dt(", names(.exo), ")", collapse = ", "),
              "; returning unchanged")
    } else {
      message("model does not appear to be a linear compartment ODE; returning unchanged")
    }
    return(rxUiCompress(.ui)) # nolint
  }

  .expr <- .odeToLinBuildExpr(.ui$lstExpr, .info)
  .rebuildRxUiFromExpr(.ui, .expr)
}
