rxTest({
  # past(state, tau) <- expr : user-specified non-constant delay() pre-history.
  # delay(state, tau) returns expr evaluated at t-tau for t<t0 instead of the
  # constant initial condition.

  test_that("past() parses and round-trips through rxNorm", {
    .m <- rxode2("y(0) <- 1\nd/dt(y) <- -delay(y, 1)\npast(y, 1) <- exp(0.5*t)\n")
    .n <- rxNorm(.m)
    expect_true(grepl("past(y,1)=exp(0.5 * t)", .n, fixed = TRUE) ||
                grepl("past(y,1)=exp(0.5*t)", .n, fixed = TRUE))
  })

  test_that("constant past() value drives the pre-history (not the IC)", {
    .ev <- et(seq(0, 1, by = 0.25))
    ## for t<1, delay(y,1) = past value; y' = -past
    .p2 <- rxSolve(rxode2("y(0) <- 1\nd/dt(y) <- -delay(y, 1)\npast(y, 1) <- 2\n"), .ev)
    .p0 <- rxSolve(rxode2("y(0) <- 1\nd/dt(y) <- -delay(y, 1)\npast(y, 1) <- 0.5\n"), .ev)
    ## past=2 -> y decreases by 2 per unit time; past=0.5 -> by 0.5
    expect_equal(.p2$y, 1 - 2 * .p2$time, tolerance = 1e-6)
    expect_equal(.p0$y, 1 - 0.5 * .p0$time, tolerance = 1e-6)
  })

  test_that("non-constant past() matches the analytic solution", {
    ## y'(t) = -delay(y,1), history h(t)=exp(0.5 t); for t<=1 the delayed value
    ## is exp(0.5(t-1)), so y(t) = 1 - 2*(exp(0.5(t-1)) - exp(-0.5)).
    .ev <- et(seq(0, 1, by = 0.05))
    .s <- rxSolve(rxode2("y(0) <- 1\nd/dt(y) <- -delay(y, 1)\npast(y, 1) <- exp(0.5*t)\n"),
                  .ev, atol = 1e-10, rtol = 1e-10)
    .ana <- 1 - 2 * (exp(0.5 * (.ev$get.EventTable()$time - 1)) - exp(-0.5))
    expect_equal(.s$y, .ana, tolerance = 1e-6)
  })

  test_that("past() first-order sensitivities match finite differences", {
    .ev <- et(seq(0, 3, by = 0.25))
    .mstr <- function(k, a, b) sprintf(
      "k=%.12g\na=%.12g\nb=%.12g\ny(0) <- a\nd/dt(y) <- -k*delay(y, 1)\npast(y, 1) <- a*exp(b*t)\n",
      k, a, b)
    .s <- rxSolve(rxode2(.mstr(0.3, 1, 0.5), calcSens = c("k", "a", "b")),
                  .ev, atol = 1e-11, rtol = 1e-11)
    .fwd <- function(k, a, b) rxSolve(rxode2(.mstr(k, a, b)), .ev,
                                      atol = 1e-11, rtol = 1e-11)$y
    .eps <- 1e-4
    .base <- c(k = 0.3, a = 1, b = 0.5)
    for (.p in c("k", "a", "b")) {
      .hi <- .base; .hi[.p] <- .hi[.p] + .eps
      .lo <- .base; .lo[.p] <- .lo[.p] - .eps
      .fd <- (.fwd(.hi[1], .hi[2], .hi[3]) - .fwd(.lo[1], .lo[2], .lo[3])) / (2 * .eps)
      expect_equal(.s[[paste0("rx__sens_y_BY_", .p, "__")]], .fd, tolerance = 1e-4)
    }
  })

  test_that("past() history is validated", {
    .ev <- et(seq(0, 2, by = 0.5))
    ## history may not reference an ODE state
    expect_error(rxSolve(rxode2("y(0)<-1\nd/dt(y)<- -delay(y,1)\npast(y,1)<-0.5*y\n"), .ev),
                 "may not reference ODE state")
    ## duration must match the delay
    expect_error(rxSolve(rxode2("y(0)<-1\nd/dt(y)<- -delay(y,2)\npast(y,1)<-exp(0.5*t)\n"), .ev),
                 "does not match any delay")
    ## past on a non-delayed state (while another state is delayed) is rejected
    expect_error(rxSolve(rxode2("y(0)<-1\nz(0)<-1\nd/dt(y)<- -y\nd/dt(z)<- -delay(z,1)\npast(y,1)<-exp(t)\n"), .ev),
                 "has no delay")
    ## a valid history solves without error
    .s <- suppressMessages(rxSolve(rxode2("y(0)<-1\nd/dt(y)<- -delay(y,1)\npast(y,1)<-exp(0.5*t)\n"), .ev))
    expect_true(all(is.finite(.s$y)))
  })

  test_that("past() second-order sensitivities match finite differences", {
    .ev <- et(seq(0, 3, by = 0.5))
    .mstr <- function(k, a, b) sprintf(
      "k=%.12g\na=%.12g\nb=%.12g\ny(0) <- a\nd/dt(y) <- -k*delay(y, 1)\npast(y, 1) <- a*exp(b*t)\n",
      k, a, b)
    .pars <- c("k", "a", "b")
    .s <- rxSolve(rxode2(.mstr(0.3, 1, 0.5), calcSens = .pars, calcSens2 = .pars),
                  .ev, atol = 1e-12, rtol = 1e-12)
    .fwd <- function(m) rxSolve(rxode2(.mstr(m[1], m[2], m[3])), .ev,
                                atol = 1e-12, rtol = 1e-12)$y
    .base <- c(k = 0.3, a = 1, b = 0.5); names(.base) <- .pars
    .h <- 1e-3
    .same <- function(p) {
      .hi <- .base; .hi[p] <- .hi[p] + .h; .lo <- .base; .lo[p] <- .lo[p] - .h
      (.fwd(.hi) - 2 * .fwd(.base) + .fwd(.lo)) / (.h * .h)
    }
    .cross <- function(p, q) {
      .pp <- .base; .pp[p] <- .pp[p] + .h; .pp[q] <- .pp[q] + .h
      .pm <- .base; .pm[p] <- .pm[p] + .h; .pm[q] <- .pm[q] - .h
      .mp <- .base; .mp[p] <- .mp[p] - .h; .mp[q] <- .mp[q] + .h
      .mm <- .base; .mm[p] <- .mm[p] - .h; .mm[q] <- .mm[q] - .h
      (.fwd(.pp) - .fwd(.pm) - .fwd(.mp) + .fwd(.mm)) / (4 * .h * .h)
    }
    for (.i in seq_along(.pars)) for (.j in .i:length(.pars)) {
      .p <- .pars[.i]; .q <- .pars[.j]
      .col <- paste0("rx__sens_y_BY_", .p, "_BY_", .q, "__")
      if (is.null(.s[[.col]])) .col <- paste0("rx__sens_y_BY_", .q, "_BY_", .p, "__")
      .fd <- if (.p == .q) .same(.p) else .cross(.p, .q)
      expect_equal(.s[[.col]], .fd, tolerance = 1e-3)
    }
  })

  # past(state, tau) without a d/dt(state) is a user error.  Reporting it used to
  # append nothing to the error buffer and then trim a trailing "', " that was
  # never written, moving the write offset before the start of the buffer; the
  # heap corruption surfaced as a double free on a *later*, unrelated parse.
  test_that("past() without d/dt() errors cleanly and leaves the parser usable", {
    .bad <- "a=1\nb=0.5\nT=12.8\npast(G, T) = a*exp(b*t)\nR2 = a\n"
    expect_error(suppressMessages(rxModelVars(.bad)), "syntax error")
    # the message names the offending property rather than a mangled fragment
    expect_output(try(suppressMessages(rxModelVars(.bad)), silent = TRUE),
                  "'past(G)' present, but d/dt(G) not defined", fixed = TRUE)
    # parsing again, and parsing a good model afterwards, must still work
    expect_error(suppressMessages(rxModelVars(.bad)), "syntax error")
    expect_true(inherits(rxModelVars("d/dt(G)=-G\n"), "rxModelVars"))
  })

  # A past() line is compartment-scoped: it only parses in a chunk that also has
  # the matching d/dt().  .rxDelaySensAugment() appends past() after every d/dt(),
  # so on a model long enough to chunk it lands in the last chunk, away from its
  # d/dt() -- .rxDisguiseCmt() must hide it for the chunk to parse standalone.
  test_that("past() survives chunked expression optimization", {
    .filler <- paste(sprintf("v%d = %d * exp(kg * t) + sin(v0 * %d)", 1:45, 1:45, 1:45),
                     collapse = "\n")
    .mod <- paste0(
      "a = 1\nb = 0.5\nT = 12.8\nk3 = 5\nkg = 0.4\nk4 = 0.3\nk5 = 0.1\nv0 = 2\n",
      "G(0) = a\nd/dt(G) = k3 - kg * G\n",
      "I(0) = 0\nd/dt(I) = k4 * G - k4 * delay(G, T)\n",
      "D(0) = 0\nd/dt(D) = k4 * delay(G, T) - k5 * D\n",
      .filler, "\n", "R2 = D + v1 + v45\n",
      "past(G, T) = a * exp(b * t)\n")

    .dg <- rxode2:::.rxDisguiseCmt(.mod)
    # the past line is hidden as an ordinary assignment, so a chunk parses alone
    expect_false(any(grepl("^past", strsplit(.dg, "\n")[[1]])))
    expect_true(any(grepl("^rx__disg_lhs__", strsplit(.dg, "\n")[[1]])))
    # ... and is restored byte-exactly, spacing included
    expect_equal(strsplit(rxode2:::.rxRestoreCmt(.dg), "\n")[[1]],
                 strsplit(.mod, "\n")[[1]])

    .o <- suppressMessages(rxOptExpr(.mod, "m", chunkLines = 40L))
    expect_true(any(grepl("^past\\(G, *T\\)", strsplit(.o, "\n")[[1]])))
    expect_false(grepl("rx__disg_", .o, fixed = TRUE))
    # optimizing must not change what the model means
    .ev <- et(seq(0, 30, by = 1))
    .s1 <- rxSolve(rxode2(.mod), .ev, method = "dop853", atol = 1e-10, rtol = 1e-10,
                   dense = TRUE)
    .s2 <- rxSolve(rxode2(.o), .ev, method = "dop853", atol = 1e-10, rtol = 1e-10,
                   dense = TRUE)
    expect_equal(.s1$R2, .s2$R2, tolerance = 1e-8)
  })

  # An expression duration is the case the estimation models hit: symengine inlines
  # `T <- exp(lT)` into every delay(), and rxOptExpr() then factors that expression
  # into a temporary.  Both must keep the past() line matched to its delay() (#1192).
  test_that("an expression past() duration optimizes and keeps its delay() match", {
    .filler <- paste(sprintf("v%d = %d * exp(kg * t) + sin(v0 * %d)", 1:45, 1:45, 1:45),
                     collapse = "\n")
    .head <- paste0(
      "lT = 2.5\na = 1\nb = 0.5\nk3 = 5\nkg = 0.4\nk4 = 0.3\nk5 = 0.1\nv0 = 2\n",
      "G(0) = a\nd/dt(G) = k3 - kg * G\n",
      "I(0) = 0\nd/dt(I) = k4 * G - k4 * delay(G, exp(lT))\n",
      "D(0) = 0\nd/dt(D) = k4 * delay(G, exp(lT)) - k5 * D\n")
    .tail <- "R2 = D\npast(G, exp(lT)) = a * exp(b * t)\n"
    .ev <- et(seq(0, 30, by = 1))
    .sol <- function(.x) {
      rxSolve(rxode2(.x), .ev, method = "dop853", atol = 1e-10, rtol = 1e-10,
              dense = TRUE)
    }
    for (.chunkLines in c(0L, 40L)) {
      # pad past chunkLines only for the chunked arm, so each arm exercises its path
      .mod <- if (.chunkLines == 0L) paste0(.head, .tail)
              else paste0(.head, .filler, "\n", .tail)
      .o <- suppressMessages(rxOptExpr(.mod, "m", chunkLines = .chunkLines))
      expect_error(rxModelVars(.o), NA)
      # the duration was factored out of the delay() calls ...
      expect_true(grepl("delay\\(G, *rx_expr_", .o))
      # ... and the past() line followed it, so the history still matches its delay()
      expect_true(grepl("past\\(G,rx_expr_", .o))
      expect_error(.rxValidatePast(rxModelVars(.o)), NA)
      # optimizing must not change what the model means
      expect_equal(.sol(.mod)$R2, .sol(.o)$R2, tolerance = 1e-8)
    }
  })

  # The duration was stored as verbatim source text and emitted unresolved, while every
  # delay() had its duration inlined by symengine -- so a generated model carried a
  # past() naming an expression no delay() used any more (#1193).
  .genModel <- "T=exp(lT)\nk=0.3\ny(0)=1\nd/dt(y)=-k*delay(y,T)\npast(y,T)=exp(b*t)\n"

  test_that("generated models keep the past() duration matched to delay() (#1193)", {
    .ev <- et(seq(0, 6, by = 0.25))
    .p <- c(lT = 0.2, b = 0.5)
    .ref <- rxSolve(rxode2(.genModel), .p, .ev, method = "dop853", dense = TRUE,
                    atol = 1e-10, rtol = 1e-10)
    for (.g in list(rxode2(.genModel, calcJac = TRUE),
                    suppressMessages(rxode2(.genModel, calcSens = "k")))) {
      .n <- gsub(" ", "", rxNorm(.g), fixed = TRUE)
      expect_false(grepl("past(y,T)", .n, fixed = TRUE))
      expect_true(grepl("past(y,exp(lT))", .n, fixed = TRUE))
      expect_error(.rxValidatePast(.g), NA)
      # ... and the generated model therefore solves, and means the same thing
      .s <- rxSolve(.g, .p, .ev, method = "dop853", dense = TRUE,
                    atol = 1e-10, rtol = 1e-10)
      expect_equal(.s$y, .ref$y, tolerance = 1e-8)
    }
  })

  test_that("sensitivity-compartment histories carry the resolved duration too", {
    # differentiating the history w.r.t. b is non-zero, so .rxDelaySensAugment() also
    # emits a past(rx__sens_y_BY_b__, tau) line -- it must resolve the duration the
    # same way the base history does, and .rxValidatePast() skips rx__sens_ lines so
    # only a test can say so
    .g <- suppressMessages(rxode2(.genModel, calcSens = "b"))
    .past <- .rxPastTerms(.g)
    expect_equal(length(.past), 2L)                        # base + rx__sens_ history
    .tau <- vapply(.past, `[[`, character(1), "tau")
    expect_false(any(.tau == "T"))
    expect_equal(length(unique(.tau)), 1L)
    # the duration is the one the model's delay() terms actually use
    expect_true(all(.tau %in% .rxDelayTerms(.g)$tau))
  })

  test_that("a THETA[]/ETA[] duration resolves like the rest of the model", {
    # the stored duration is rxode2 source text, where THETA[1] is a subscript -- in the
    # symengine env it is the symbol THETA_1_, so it has to be translated before it is
    # evaluated.  Every mu-referenced (nlmixr2) model reaches this.
    .m <- paste0("T=exp(lT)\nk=0.3\ny(0)=1\n",
                 "d/dt(y)=-k*delay(y,T+THETA[1])\npast(y,T+THETA[1])=exp(b*t)\n")
    .g <- rxode2(.m, calcJac = TRUE)
    .n <- gsub(" ", "", rxNorm(.g), fixed = TRUE)
    expect_false(grepl("past(y,T+THETA[1])", .n, fixed = TRUE))
    expect_true(grepl("past(y,THETA[1]+exp(lT))", .n, fixed = TRUE))
    expect_error(.rxValidatePast(.g), NA)
  })

  test_that("a THETA[]/ETA[] history resolves, so it can be differentiated", {
    # an unresolved history is not a Basic, and .rxDelaySensAugment() then silently
    # emits no per-parameter sensitivity pre-history at all
    .e <- rxS(paste0("k=0.3\nG(0)=1\nd/dt(G)=-k*delay(G,exp(THETA[1]))\n",
                     "past(G,exp(THETA[1]))=exp(ETA[1]*t)\n"), doConst = FALSE)
    .p <- .rxPastFromEnv(.e, "G")
    expect_equal(.p$tau, "exp(THETA[1])")
    expect_equal(.p$rhs, "exp(t*ETA[1])")
    expect_true(inherits(.p$rhsB, "Basic"))
  })
})
