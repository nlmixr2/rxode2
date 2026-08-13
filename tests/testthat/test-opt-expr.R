rxTest({
  test_that("simple expression optimization", {
    exp1 <- "rx_yj_~2;\nrx_lambda_~1;\nrx_pred_=10*exp(-THETA[1]*t*exp(ETA[1]));\nrx__sens_rx_pred__BY_ETA_1___=-10*THETA[1]*t*exp(ETA[1])*exp(-THETA[1]*t*exp(ETA[1]));\nrx_r_=100*Rx_pow_di(THETA[2],2)*exp(-2*THETA[1]*t);\ndvid(3,4)\n"
    suppressMessages(expect_equal(
      rxOptExpr(exp1),
      "rx_yj_~2\nrx_lambda_~1\nrx_expr_0~exp(ETA[1])\nrx_expr_1~exp(-THETA[1]*t*rx_expr_0)\nrx_pred_=10*rx_expr_1\nrx__sens_rx_pred__BY_ETA_1___=-10*THETA[1]*t*rx_expr_0*rx_expr_1\nrx_r_=100*Rx_pow_di(THETA[2], 2)*exp(-2*THETA[1]*t)\ndvid(3, 4)"
    ))
  })

  test_that("expression optimization errors", {
    suppressMessages(expect_error(
      rxOptExpr("A1=exp(-k10*(tau - tinf))*r1*(1.0 - exp(-k10*tinf))/(k10*(1.0 - exp(-tau*k10)))"),
      NA
    ))
    suppressMessages(expect_error(
      rxOptExpr("A1=r1/ka\nA1ka=-r1/ka^2\nA1k20=0\nA1b1=0\nA1r1=ka^(-1)\nA2=r1/k20\nA2ka=0\nA2k20=-r1/k20^2\nA2b1=0\nA2r1=k20^(-1)"),
      NA
    ))
  })

  suppressMessages(rxOptExpr("a=1+(-1/2)*b"))

  suppressMessages(rxOptExpr("a=-1*exp(b)"))

  suppressMessages(rxOptExpr("a=1+(((-1/2)))*b"))

  suppressMessages(rxOptExpr("a=1+(1/2)*b; c=d^(1/2); e=(1/2)*f^(1/2)"))

  test_that("simple expression optimization", {
    expect_equal(length(..rxOpt(quote(exp(ETA[1] + THETA[4]) + 0))), 1L)
  })

  ## chunked optimization -------------------------------------------------

  # a model big enough to chunk, carrying both kinds of compartment-scoped construct:
  # a parameter-dependent initial condition and dosing modifiers.  Optimizing a chunk
  # that held one of these *without* its d/dt() used to be a syntax error, which is
  # what the disguise/restore round trip below exists to prevent.
  .chunkModel <- function(n = 60L) {
    .s <- vapply(seq_len(n), function(i) {
      sprintf("v%d=exp(THETA[1]+ETA[1])*exp(THETA[2]*%d)+sin(THETA[3]*t)", i, i)
    }, character(1))
    paste(c("d/dt(depot)=-exp(THETA[1]+ETA[1])*depot",
            "d/dt(center)=exp(THETA[1]+ETA[1])*depot-exp(THETA[2])*center",
            "depot(0)=exp(THETA[4])",          # parameter-dependent initial condition
            "f(depot)=exp(THETA[5])",          # dosing modifiers
            "alag(depot)=exp(THETA[6])",
            .s,
            "rx_pred_=center*exp(THETA[3])"), collapse = "\n")
  }

  test_that("the default chunks a large model; chunkLines=0 optimizes it whole", {
    .m <- .chunkModel()
    .def <- suppressMessages(rxOptExpr(.m, "model"))
    # per-chunk rx_expr_c<i>_ names prove the default took the chunked path
    expect_true(grepl("rx_expr_c[0-9]+_", .def))
    expect_identical(.def, suppressMessages(rxOptExpr(.m, "model", chunkLines = 40L)))
    # chunkLines = 0 still forces the single whole-model pass
    .whole <- suppressMessages(rxOptExpr(.m, "model", chunkLines = 0L))
    expect_false(grepl("rx_expr_c[0-9]+_", .whole))
  })

  test_that("chunked optimization yields an equivalent model", {
    .m <- .chunkModel()
    .whole <- suppressMessages(rxOptExpr(.m, "model", chunkLines = 0L))
    .chunk <- suppressMessages(rxOptExpr(.m, "model", chunkLines = 40L))
    # the compartment-scoped lines survive, in place, and the chunked model still builds
    expect_true(grepl("depot(0)=", .chunk, fixed = TRUE))
    expect_true(grepl("f(depot)=", .chunk, fixed = TRUE))
    expect_true(grepl("alag(depot)=", .chunk, fixed = TRUE))
    expect_error(rxModelVars(.chunk), NA)
    # same states, and same parameters (chunking must not leave an undefined variable
    # behind as a spurious input parameter)
    .rv <- function(.x) {
      .mv <- rxModelVars(.x)
      list(state = .mv$state, params = sort(.mv$params[!grepl("^rx_expr_", .mv$params)]))
    }
    expect_identical(.rv(.whole), .rv(.chunk))
  })

  test_that("a model at or below chunkLines is untouched by the default", {
    .m <- "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-cl/v*center"
    suppressMessages(expect_identical(rxOptExpr(.m, "model"),
                                      rxOptExpr(.m, "model", chunkLines = 0L)))
  })

  test_that("compartment-scoped disguise/restore round-trips exactly", {
    # byte-exact for every spacing variant, so a chunk boundary can never alter the
    # surrounding whitespace, and idempotent in both directions
    .lines <- c("depot(0)=W0", "  depot(0)=W0", "\tdepot(0)=W0", "depot(0) = W0",
                "depot (0)=W0", "f(depot)=ff", "f( depot )=ff", "f (depot)=ff",
                "F(depot)=ff", "alag( central )=tlag",
                "rate(depot)=r0", "dur(depot)=d0", "lag(depot)=tl",
                "rx__sens_W_BY_ETA_1___(0)=exp(x)",
                "d/dt(depot)=-ka*depot", "ka=exp(tka)")
    for (.l in .lines) {
      expect_identical(.rxRestoreCmt(.rxDisguiseCmt(.l)), .l)
    }
    .txt <- paste(.lines, collapse = "\n")
    expect_identical(.rxRestoreCmt(.rxDisguiseCmt(.txt)), .txt)
    expect_identical(.rxDisguiseCmt(.rxDisguiseCmt(.txt)), .rxDisguiseCmt(.txt))
    expect_identical(.rxRestoreCmt(.rxRestoreCmt(.txt)), .txt)
    # every disguised left-hand side is a syntactically valid identifier, even for the
    # spacing variants (`depot (0)=`, `f( depot )=`), so the chunk it lands in parses
    .lhs <- trimws(sub("=.*$", "", strsplit(.rxDisguiseCmt(.txt), "\n", fixed = TRUE)[[1]]))
    .disg <- grepl("^rx__disg_", .lhs)
    expect_true(all(grepl("^[a-zA-Z][a-zA-Z0-9_.]*$", .lhs[.disg])))
    # nothing compartment-scoped is left for a chunk to orphan
    expect_false(any(endsWith(.lhs, "(0)")))
    expect_false(any(grepl("^([fF]|alag|lag|rate|dur)[ \t]*\\(", .lhs) & endsWith(.lhs, ")")))
  })

  test_that("grammar-accepted spacing variants chunk without falling back", {
    # `depot (0)=` and `f( depot )=` used to be disguised into an identifier containing
    # spaces, a syntax error that silently sent every such model down the whole-model
    # fallback; they are now hex-encoded, so the chunked path handles them.
    .pad <- vapply(1:60, function(i) {
      sprintf("v%d=exp(THETA[1]+ETA[1])*exp(THETA[2]*%d)", i, i)
    }, character(1))
    .m <- paste(c("d/dt(depot)=-exp(THETA[1]+ETA[1])*depot",
                  "d/dt(center)=exp(THETA[1]+ETA[1])*depot-exp(THETA[2])*center",
                  "depot (0)=exp(THETA[4])",
                  "f( depot )=exp(THETA[5])",
                  "alag (depot)=exp(THETA[6])",
                  .pad, "rx_pred_=center"), collapse = "\n")
    .out <- suppressMessages(rxOptExpr(.m, "model", chunkLines = 40L))
    # per-chunk rx_expr_c<i>_ names prove the chunked path ran (a fallback has none)
    expect_true(grepl("rx_expr_c[0-9]+_", .out))
    # the spacing-variant lines are restored byte-exact and the model still parses
    expect_true(grepl("depot (0)=", .out, fixed = TRUE))
    expect_true(grepl("f( depot )=", .out, fixed = TRUE))
    expect_true(grepl("alag (depot)=", .out, fixed = TRUE))
    expect_error(rxModelVars(.out), NA)
  })

  test_that("a chunk boundary separating delay() from its d/dt() no longer falls back", {
    # delay(state, T) parses only where d/dt(state) is defined; a boundary between the
    # two used to fail the chunk, print the parser's :ERR: block, and lose the chunking
    # speedup to the whole-model fallback.  The call is now disguised across the
    # boundary and restored byte-exact.
    .pad <- vapply(1:60, function(i) {
      sprintf("v%d=exp(THETA[1]+ETA[1])*exp(THETA[2]*%d)", i, i)
    }, character(1))
    .m <- paste(c("d/dt(x)=-exp(THETA[1])*x",
                  .pad,
                  "a=delay(x, exp(THETA[2]))+exp(THETA[1]+ETA[1])",
                  "b=rxDelayD(x, exp(THETA[2]))*2+exp(THETA[1]+ETA[1])",
                  "rx_pred_=a+b"), collapse = "\n")
    .chunk <- suppressMessages(rxOptExpr(.m, "model", chunkLines = 40L))
    # per-chunk rx_expr_c<i>_ names prove the chunked path ran (a fallback has none)
    expect_true(grepl("rx_expr_c[0-9]+_", .chunk))
    # the calls are restored byte-exact, nothing disguised is left, and it still parses
    expect_true(grepl("delay(x, exp(THETA[2]))", .chunk, fixed = TRUE))
    expect_true(grepl("rxDelayD(x, exp(THETA[2]))", .chunk, fixed = TRUE))
    expect_false(grepl("rx__disg_delay_", .chunk, fixed = TRUE))
    expect_error(rxModelVars(.chunk), NA)
    # same states and parameters as the whole-model pass
    .whole <- suppressMessages(rxOptExpr(.m, "model", chunkLines = 0L))
    .rv <- function(.x) {
      .mv <- rxModelVars(.x)
      list(state = .mv$state, params = sort(.mv$params[!grepl("^rx_expr_", .mv$params)]))
    }
    expect_identical(.rv(.whole), .rv(.chunk))
  })

  test_that("a chunk holding delay() and its d/dt() keeps optimizing the call", {
    # co-located calls are NOT disguised, so their arguments still join the chunk's
    # common-subexpression pool
    .pad <- vapply(1:60, function(i) {
      sprintf("v%d=exp(THETA[1]+ETA[1])*exp(THETA[2]*%d)", i, i)
    }, character(1))
    .m <- paste(c("d/dt(x)=-exp(THETA[1])*x",
                  "a=delay(x, exp(THETA[2]))+1",
                  "b=delay(x, exp(THETA[2]))*2",
                  .pad,
                  "rx_pred_=a+b"), collapse = "\n")
    .chunk <- suppressMessages(rxOptExpr(.m, "model", chunkLines = 40L))
    expect_true(grepl("rx_expr_c[0-9]+_", .chunk))
    # the delay argument was factored in place of the literal exp(THETA[2])
    expect_true(grepl("delay\\(x, rx_expr_c[0-9]+_[0-9]+\\)", .chunk))
  })

  test_that("a delay() state with no d/dt() anywhere raises the whole-model error", {
    # the disguise only hides a call whose d/dt() exists in another chunk; a malformed
    # model must still reach the whole-model fallback and raise the unchunked error
    .pad <- vapply(1:60, function(i) {
      sprintf("v%d=exp(THETA[1]+ETA[1])*exp(THETA[2]*%d)", i, i)
    }, character(1))
    .m <- paste(c(.pad, "a=delay(x, exp(THETA[2]))+1", "rx_pred_=a"), collapse = "\n")
    .err <- function(.chunkLines) {
      tryCatch({
        .z <- capture.output(suppressMessages(
          rxOptExpr(.m, "model", chunkLines = .chunkLines)), type = "output")
        NA_character_
      }, error = function(e) conditionMessage(e))
    }
    .chunk <- .err(40L)
    expect_false(is.na(.chunk))
    expect_identical(.chunk, .err(0L))
  })

  test_that("delay disguise/restore round-trips and is chunk-local", {
    .chunks <- list(
      c("d/dt(x)=-k*x", "d/dt(y)=k*x-ke*y",
        "a=delay(x, tau)+1"),                      # d/dt(x) here: kept as-is
      c("b=delay(x, exp(tau))*2",                  # split from d/dt(x): disguised
        "c=rxDelayD2(y, tau+delay(x, exp(tau)))",  # nested call inside a disguised one
        "d=delay(z, tau)"))                        # no d/dt(z) anywhere: left alone
    .d <- .rxDisguiseDelayChunks(.chunks)
    expect_identical(.d$chunks[[1]], .chunks[[1]])
    # every delay-family call split from its d/dt() is hidden in chunk 2 ...
    expect_false(any(grepl("delay\\(x|rxDelayD2\\(y", .d$chunks[[2]])))
    # ... while the call whose state has no d/dt() anywhere survives untouched
    expect_true(grepl("delay(z, tau)", .d$chunks[[2]][3], fixed = TRUE))
    # identical calls share one name; restore is byte-exact
    expect_identical(.d$chunks[[2]][1],
                     sub("delay(x, exp(tau))", names(.d$map)[match("delay(x, exp(tau))", .d$map)],
                         .chunks[[2]][1], fixed = TRUE))
    .joined <- vapply(.d$chunks, paste, character(1), collapse = "\n")
    expect_identical(.rxRestoreDelay(paste(.joined, collapse = "\n"), .d$map),
                     paste(vapply(.chunks, paste, character(1), collapse = "\n"),
                           collapse = "\n"))
  })

  test_that("a filename is read as a file when chunking, like the unchunked call", {
    .f <- tempfile(fileext = ".rx")
    on.exit(unlink(.f), add = TRUE)
    writeLines(.chunkModel(), .f)
    suppressMessages(expect_identical(
      rxOptExpr(.f, "model", chunkLines = 40L),
      rxOptExpr(.chunkModel(), "model", chunkLines = 40L)))
  })

  test_that("chunks are cost balanced and lossless", {
    .ln <- c("aaaaaaaaaaaaaaaaaaaa", "b", "c", "dddddddddddddddddddd", "e", "f")
    .ch <- .rxBalancedChunks(.ln, 20)
    expect_identical(unlist(.ch, use.names = FALSE), .ln)  # order and content preserved
    expect_true(length(.ch) > 1L)
  })

  test_that("a model already using the chunker's own generated names is not chunked", {
    # Chunking introduces names into the model's namespace -- rx_expr_c<i>_ for the
    # temporaries a chunk contributes, and rx__disg_ while a compartment-scoped line is
    # disguised.  rxode2 does not reserve these, so a model may legitimately use one; it
    # would then be captured (renaming or restoring would rewrite the model's own variable)
    # and the model would silently change.  Such a model is left to the whole-model call.
    .pad <- vapply(1:45, function(i) sprintf("a%d=exp(THETA[1])*%d", i, i), character(1))
    .ode <- c("d/dt(depot)=-exp(THETA[1])*depot",
              "d/dt(center)=exp(THETA[1])*depot-exp(THETA[2])*center")
    for (.nm in c("rx_expr_c1_0", "rx__disg_ic__depot__", "rx__disg_mod__f__depot__")) {
      .m <- paste(c(.ode, paste0(.nm, "=exp(THETA[3])"), .pad,
                    paste0("cp=center*", .nm)), collapse = "\n")
      .chunked <- suppressMessages(rxOptExpr(.m, "model", chunkLines = 40L))
      # falls back to the whole-model call, so the model's own variable is untouched
      expect_identical(.chunked, suppressMessages(rxOptExpr(.m, "model", chunkLines = 0L)))
      expect_true(grepl(.nm, .chunked, fixed = TRUE))
    }
  })

  test_that("only the rx_expr_ names a chunk introduces are renamed", {
    # The chunks are optimized independently, so each restarts its rx_expr_ counter and the
    # names must be prefixed per chunk.  But renaming *every* rx_expr_ occurrence would
    # break two cases: a name already in the model (it was optimized before) whose
    # definition and uses land in different chunks would be renamed differently and pulled
    # apart; and a variable that merely contains "rx_expr_" would be rewritten.
    .pad <- vapply(1:45, function(i) sprintf("a%d=exp(THETA[1]+ETA[1])*%d", i, i), character(1))
    .base <- c("d/dt(depot)=-exp(THETA[1]+ETA[1])*depot",
               "d/dt(center)=exp(THETA[1]+ETA[1])*depot-exp(THETA[2])*center")

    # (a) re-optimizing an already-optimized model
    .once <- suppressMessages(rxOptExpr(paste(c(.base, .pad, "cp=center"), collapse = "\n")))
    expect_true(grepl("rx_expr_", .once))
    .twice <- suppressMessages(rxOptExpr(.once, "model", chunkLines = 40L))
    expect_error(rxModelVars(.twice), NA)
    # no rx_expr_ left dangling as an input parameter
    expect_false(any(grepl("^rx_expr_", rxModelVars(.twice)$params)))

    # (b) a user variable whose name merely contains "rx_expr_"
    .m <- paste(c(.base, "my_rx_expr_var=exp(THETA[3])", .pad,
                  "cp=center*my_rx_expr_var"), collapse = "\n")
    .o <- suppressMessages(rxOptExpr(.m, "model", chunkLines = 40L))
    expect_true(grepl("my_rx_expr_var", .o, fixed = TRUE))   # left exactly as it was
    expect_error(rxModelVars(.o), NA)
    expect_false(any(grepl("^rx_expr_", rxModelVars(.o)$params)))
  })

  test_that("a failing chunk falls back to the whole model, so chunking changes nothing", {
    # A chunk is a fragment, so it can fail where the whole model would not.  But it can
    # equally fail because the model is genuinely malformed, and the chunk alone cannot tell
    # the two apart -- so any chunk failure falls back to optimizing the whole model, which
    # can.  Chunking must therefore return the same model, and raise the same error, as the
    # unchunked call in BOTH cases.
    .pad <- vapply(1:60, function(i) sprintf("v%d=exp(THETA[1])*%d", i, i), character(1))

    # (a) valid model whose chunk orphans a compartment-scoped line.  `f(depot) <- ...`
    # (assignment with `<-`) is accepted by rxode2, but the disguise only splits a line at
    # its first "=", so it is not disguised and its chunk cannot be optimized on its own.
    .frag <- paste(c("d/dt(depot)=-exp(THETA[1])*depot",
                     "d/dt(center)=exp(THETA[1])*depot-exp(THETA[2])*center",
                     .pad, "f(depot) <- exp(THETA[3])", "rx_pred_=center"), collapse = "\n")
    .out <- suppressMessages(rxOptExpr(.frag, "model", chunkLines = 40L))
    expect_identical(.out, suppressMessages(rxOptExpr(.frag, "model", chunkLines = 0L)))
    expect_true(grepl("rx_expr_", .out))   # fell back, so it is still fully optimized
    expect_error(rxModelVars(.out), NA)

    # (b) genuinely malformed model: chunking must raise, exactly as the unchunked call does
    .bad <- paste(c("d/dt(depot)=-ka*depot", "y = = 3 +", .pad), collapse = "\n")
    suppressMessages(expect_error(rxOptExpr(.bad, "model")))
    suppressMessages(expect_error(rxOptExpr(.bad, "model", chunkLines = 40L)))
  })

  test_that("parallel chunking gives the identical model and leaves no daemons behind", {
    # daemons only start when the model splits into at least 4 chunks, so use a model
    # large enough to cross that threshold
    .m <- .chunkModel(150L)
    .seq <- suppressMessages(rxOptExpr(.m, "model", chunkLines = 40L, parallel = 1L))
    .par <- suppressMessages(rxOptExpr(.m, "model", chunkLines = 40L, parallel = 2L))
    # optimizing the chunks in daemons must not change the model at all
    expect_identical(.seq, .par)
    # the pool is started for the call and shut down when it returns
    expect_equal(sum(mirai::status()$connections), 0L)
    # the default parallel (0 = the rxode2 thread setting) is the same model too,
    # and equally leaves no pool behind
    .def <- suppressMessages(rxOptExpr(.m, "model"))
    expect_identical(.def, .seq)
    expect_equal(sum(mirai::status()$connections), 0L)
  })

  test_that("too few chunks do not pay for a daemon pool", {
    # .chunkModel() splits into only 2 chunks: even with parallel forced on, no pool is
    # started (its startup would cost more than it saves) and none is left behind
    .m <- .chunkModel()
    .par <- suppressMessages(rxOptExpr(.m, "model", chunkLines = 40L, parallel = 2L))
    .seq <- suppressMessages(rxOptExpr(.m, "model", chunkLines = 40L, parallel = 1L))
    expect_identical(.par, .seq)
    expect_equal(sum(mirai::status()$connections), 0L)
  })

  # A past() duration is an ordinary expression, not a left-hand side; rendering it with
  # ..rxOptLhs() only ever worked for a name, a number, `(x)` and `x/y` (the last two by
  # accident -- they are there for d/dt(x)) and stopped on anything else (#1192).
  .pastModel <- function(tau) {
    paste(c("lT=1.2", "a=1", "b=0.5", "k3=5", "kg=0.4",
            "G(0)=a", "d/dt(G)=k3-kg*G",
            sprintf("past(G,%s)=a*exp(b*t)", tau),
            sprintf("z1=delay(G,%s)", tau),
            sprintf("z2=2*delay(G,%s)", tau)), collapse = "\n")
  }

  test_that("a past() duration that is an expression optimizes (#1192)", {
    for (.tau in c("exp(lT)", "lT*2", "2^lT", "exp(THETA[1]+ETA[1])")) {
      .o <- suppressMessages(rxOptExpr(.pastModel(.tau), "model", chunkLines = 0L))
      expect_error(rxModelVars(.o), NA)
      # the duration picked up the same temporary the delay() calls did, so the
      # past() history still matches its delay() -- see .rxValidatePast()
      expect_error(.rxValidatePast(rxModelVars(.o)), NA)
      expect_true(grepl("past\\(G,rx_expr_[0-9]+\\)", .o))
      expect_true(grepl("delay\\(G, *rx_expr_[0-9]+\\)", .o))
    }
  })

  test_that("a degenerate past() duration is rendered exactly as before", {
    for (.tau in c("lT", "12.8", "(lT)")) {
      .o <- suppressMessages(rxOptExpr(.pastModel(.tau), "model", chunkLines = 0L))
      expect_true(grepl(sprintf("past(G,%s)=", .tau), .o, fixed = TRUE))
      expect_error(.rxValidatePast(rxModelVars(.o)), NA)
    }
  })

  test_that(".rxRealignPastTau() only ever rewrites the duration it is sure of", {
    .r <- .rxRealignPastTau
    .pastOf <- function(.x) grep("past\\(", strsplit(.x, "\n")[[1]], value = TRUE)
    # nothing to do: no past() at all, and a past() already naming its delay's duration
    .none <- "d/dt(G)=-delay(G,rx_expr_0)\nrx_expr_0~exp(lT)\n"
    expect_identical(.r(.none, "d/dt(G)=-delay(G,exp(lT))\n"), .none)
    .ok <- "past(G,exp(lT))=2\nd/dt(G)=-delay(G,exp(lT))\n"
    expect_identical(.r(.ok, .ok), .ok)
    # the left-hand side is delimited by the parenthesis matching past(, not by the first
    # "=", so a duration holding one is still re-pointed
    .eq <- "past(G, h(x)==1)=2\nd/dt(G)=-delay(G,rx_expr_0)\nrx_expr_0~h(x)==1\n"
    expect_identical(.pastOf(.r(.eq, "past(G, h(x)==1)=2\nd/dt(G)=-delay(G,h(x)==1)\n")),
                     "past(G,rx_expr_0)=2")
    # the delay() scan matches parentheses rather than parsing, so a model whose
    # if/else the R parser would reject is still realigned
    .ie <- paste(c("if (t>0) {", "v1=1", "}", "else {", "v1=2", "}",
                   "rx_expr_0~exp(lT)", "d/dt(G)=-delay(G, rx_expr_0)",
                   "past(G,exp(lT))=2"), collapse = "\n")
    expect_identical(.pastOf(.r(.ie, "d/dt(G)=-delay(G, exp(lT))\npast(G,exp(lT))=2")),
                     "past(G,rx_expr_0)=2")
    # two distinct durations on one state: with the pre-optimization text each past()
    # line follows the delay() it came from (order of first appearance is preserved) ...
    .two0 <- paste(c("d/dt(G)=-delay(G, exp(lT))-delay(G, 2*lT)",
                     "past(G,exp(lT))=2", "past(G,2*lT)=3"), collapse = "\n")
    .two1 <- paste(c("rx_expr_0~exp(lT)", "rx_expr_1~2*lT",
                     "d/dt(G)=-delay(G, rx_expr_0)-delay(G, rx_expr_1)",
                     "past(G,exp(lT))=2", "past(G,2*lT)=3"), collapse = "\n")
    expect_identical(.pastOf(.r(.two1, .two0)),
                     c("past(G,rx_expr_0)=2", "past(G,rx_expr_1)=3"))
    # ... and when it does not line up with them, neither line is guessed at
    expect_identical(.r(.two1, "d/dt(G)=-delay(G, exp(lT))\npast(G,exp(lT))=2\npast(G,2*lT)=3"),
                     .two1)
    # when it does rewrite, only the duration changes -- spacing is kept
    .sp <- "   past(G,exp(lT))  =  2\nd/dt(G)=-delay(G,rx_expr_0)\nrx_expr_0~exp(lT)"
    expect_identical(strsplit(.r(.sp, "   past(G,exp(lT))  =  2\nd/dt(G)=-delay(G,exp(lT))"),
                              "\n")[[1]][1L],
                     "   past(G,rx_expr_0)  =  2")
    # a duration that matched no delay() BEFORE optimizing is the model's own error and
    # is left for the validator: re-pointing it would make a rejected duration accepted
    .bad1 <- "rx_expr_0~exp(lT)\nd/dt(G)=-delay(G,rx_expr_0)\npast(G,typo)=2\n"
    .bad0 <- "d/dt(G)=-delay(G,exp(lT))\npast(G,typo)=2\n"
    expect_identical(.r(.bad1, .bad0), .bad1)
    # optimizing can DROP a duration (0*delay(G,10) folds to 0), and the one left is not
    # the one the history meant: re-pointing there would hand the surviving delay() term
    # another one's history and the model would solve, wrongly
    .dc0 <- "d/dt(G)=0*delay(G,10)+delay(G,20)\npast(G,10)=5\n"
    .dc1 <- "d/dt(G)=0+delay(G, 20)\npast(G,10)=5\n"
    expect_identical(.r(.dc1, .dc0), .dc1)
    # two histories on one state whose delay() durations optimized down to one: rewriting
    # both would leave one silently shadowing the other, so neither is guessed at
    .mrg0 <- paste(c("d/dt(G)=-delay(G,1+1)-delay(G,2)",
                     "past(G,1+1)=2", "past(G,2)=3"), collapse = "\n")
    .mrg1 <- paste(c("rx_expr_0~2", "d/dt(G)=-delay(G,rx_expr_0)-delay(G,rx_expr_0)",
                     "past(G,1+1)=2", "past(G,2)=3"), collapse = "\n")
    expect_identical(.r(.mrg1, .mrg0), .mrg1)
    # a state may lead with a "." -- d/dt(.y.1) parses, so the scan must take it
    .dot <- "past(.y.1,exp(lT))=2\nd/dt(.y.1)=-delay(.y.1,rx_expr_0)\nrx_expr_0~exp(lT)\n"
    expect_identical(.pastOf(.r(.dot, "past(.y.1,exp(lT))=2\nd/dt(.y.1)=-delay(.y.1,exp(lT))\n")),
                     "past(.y.1,rx_expr_0)=2")
    # the same duration twice is one duration, so the past() line still follows it
    .dup0 <- "d/dt(G)=-delay(G, exp(lT))-delay(G, exp(lT))\npast(G,exp(lT))=2"
    .dup1 <- paste(c("rx_expr_0~exp(lT)", "d/dt(G)=-delay(G, rx_expr_0)-delay(G, rx_expr_0)",
                     "past(G,exp(lT))=2"), collapse = "\n")
    expect_identical(.pastOf(.r(.dup1, .dup0)), "past(G,rx_expr_0)=2")
    # the scan reads a line at a time, so a delay() split over lines is not read at all.
    # Either text holding one is only a subset of the model's delay() calls and nothing can
    # be concluded from it -- not that a duration was already wrong (it may be the call
    # that was dropped), and not which optimized duration a past() line meant.  Neither is
    # realigned, which is what happened before this pass existed
    .ml0 <- "lT=2.5\nd/dt(G)=-delay(G,\n  exp(lT))\npast(G, exp(lT))=10\n"
    .ml1 <- "rx_expr_0~exp(lT)\nd/dt(G)=-delay(G, rx_expr_0)\npast(G, exp(lT))=10\n"
    expect_identical(.r(.ml1, .ml0), .ml1)
    expect_identical(.r("d/dt(G)=-delay(G,\n rx_expr_0)\npast(G,exp(lT))=2\n", .ml0),
                     "d/dt(G)=-delay(G,\n rx_expr_0)\npast(G,exp(lT))=2\n")
    # in particular a duration is never matched by position against a scan that dropped a
    # call: here the two readable durations of `orig` line up with the two of the optimized
    # text only by coincidence, and past(G,exp(lT)*1) would take tau2's temporary
    .co0 <- paste(c("d/dt(G)=-delay(G, exp(lT))-delay(G,", "tau2)-delay(G, exp(lT)*1)",
                    "past(G, exp(lT))=1", "past(G, tau2)=2",
                    "past(G, exp(lT)*1)=3"), collapse = "\n")
    .co1 <- paste(c("rx_expr_1~exp(lT)", "rx_expr_2~tau2",
                    "d/dt(G)=-delay(G, rx_expr_1)-delay(G, rx_expr_2)-delay(G, rx_expr_1)",
                    "past(G, exp(lT))=1", "past(G, tau2)=2",
                    "past(G, exp(lT)*1)=3"), collapse = "\n")
    expect_identical(.r(.co1, .co0), .co1)
    # a delay() in a comment is not a delay(): counting one would leave the state with a
    # duration no delay() has, and the past() line refused a rewrite it needed
    .cm0 <- paste(c("d/dt(G)=-delay(G,exp(lT)) # delay(G,junk)", "past(G,exp(lT))=2"),
                  collapse = "\n")
    .cm1 <- paste(c("rx_expr_0~exp(lT)", "d/dt(G)=-delay(G,rx_expr_0) # delay(G,junk)",
                    "past(G,exp(lT))=2"), collapse = "\n")
    expect_identical(.pastOf(.r(.cm1, .cm0)), "past(G,rx_expr_0)=2")
    # ... and an unbalanced parenthesis in one does not stop the scan either
    .cm2 <- "rx_expr_0~exp(lT)\nd/dt(G)=-delay(G,rx_expr_0) # (see above\npast(G,exp(lT))=2"
    expect_identical(.pastOf(.r(.cm2, "d/dt(G)=-delay(G,exp(lT)) # (see above\npast(G,exp(lT))=2")),
                     "past(G,rx_expr_0)=2")
    # a delay() inside a string is not one either -- it must neither be read nor stop the
    # scan, which would take the whole model with it
    .st0 <- "s=\"delay(G,\"\nd/dt(G)=-delay(G,exp(lT))\npast(G,exp(lT))=2"
    .st1 <- "s=\"delay(G,\"\nrx_expr_0~exp(lT)\nd/dt(G)=-delay(G,rx_expr_0)\npast(G,exp(lT))=2"
    expect_identical(.pastOf(.r(.st1, .st0)), "past(G,rx_expr_0)=2")
    # ... but a duration may legitimately hold one, and it is read back whole: masking
    # says what is code, it does not change what the duration is
    .sd0 <- paste(c("OCC=\"first\"", "d/dt(I)=delay(G, 1+(OCC==\"first\"))",
                    "past(G, 1+(OCC==\"first\"))=2"), collapse = "\n")
    .sd1 <- paste(c("OCC=\"first\"", "rx_expr_0~1+(OCC==\"first\")",
                    "d/dt(I)=delay(G, rx_expr_0)", "past(G, 1+(OCC==\"first\"))=2"),
                  collapse = "\n")
    expect_identical(.pastOf(.r(.sd1, .sd0)), "past(G,rx_expr_0)=2")
    # a comma or a parenthesis inside a string does not split the call either
    expect_identical(.rxDelayDurs("d/dt(I)=delay(G, f(\"a,b)\")+1)\n")[["G"]],
                     "f(\"a,b)\")+1")
    # code only: the comment goes, a string keeps its quotes and its width but not its
    # contents, and a "#" inside a string is not a comment
    expect_identical(.rxCodeOnly("printf(\"a#b\") # c"), "printf(\"   \") ")
    expect_identical(.rxCodeOnly("s=\"delay(G,\""), "s=\"        \"")
    expect_identical(.rxCodeOnly("d/dt(G)=-delay(G,rx_expr_0)"),
                     "d/dt(G)=-delay(G,rx_expr_0)")
  })

  test_that("the chunked path rejects a wrong duration the way the whole model does", {
    .pad <- paste(sprintf("v%d=%d", 1:60, 1:60), collapse = "\n")
    .mod <- function(.tau, .dt = "delay(G,exp(lT))") {
      paste0("lT=0.2\nb=0.5\nG(0)=1\n", .pad,
             "\nd/dt(G)=-exp(lT)*", .dt, "\npast(G,", .tau, ")=exp(b*t)\n")
    }
    # ... end to end: a dropped delay() leaves its history dangling, and it is reported
    .dc <- suppressMessages(rxOptExpr("G(0)=1\nd/dt(G)=0*delay(G,10)+delay(G,20)\npast(G,10)=5\n",
                                      "m", chunkLines = 1L))
    expect_error(.rxValidatePast(rxModelVars(.dc)), "'10' does not match")
    # a comment naming a delay() the model does not have must not stop the realign
    .cm <- suppressMessages(rxOptExpr(.mod("exp(lT)", "delay(G,exp(lT)) # delay(G,junk)"),
                                      "m", chunkLines = 15L))
    expect_error(.rxValidatePast(rxModelVars(.cm)), NA)
    expect_true(grepl("past\\(G,rx_expr_", .cm))
    # ... including when the model's own delay() is split over lines, which the realign
    # scan cannot read: an unreadable model must not be guessed into a valid one
    expect_error(.rxValidatePast(rxModelVars(suppressMessages(
      rxOptExpr(.mod("typo", "delay(G,\n exp(lT))"), "m", chunkLines = 15L)))),
      "'typo' does not match")
    for (.chunk in c(0L, 10L)) {
      .o <- suppressMessages(rxOptExpr(.mod("exp(lT)"), "m", chunkLines = .chunk))
      expect_error(.rxValidatePast(rxModelVars(.o)), NA)
      expect_true(grepl("past\\(G,rx_expr_", .o))
      # a genuinely wrong duration still names itself in the error, on either path
      .b <- suppressMessages(rxOptExpr(.mod("typo"), "m", chunkLines = .chunk))
      expect_error(.rxValidatePast(rxModelVars(.b)), "'typo' does not match")
    }
  })

  test_that("an unsupported lhs names itself and does not print (#1192)", {
    # the branch is only reachable directly -- an unsupported lhs is already a parse
    # error in rxModelVars() -- but the stray print() it used to do landed in the
    # middle of the progress bar
    expect_error(..rxOptLhs(quote(foo(bar, baz))),
                 "foo(bar, baz)", fixed = TRUE)
    expect_output(try(..rxOptLhs(quote(foo(bar, baz))), silent = TRUE), NA)
  })
})
