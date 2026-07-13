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

  test_that("chunkLines=0 (the default) is exactly the unchunked behaviour", {
    .m <- .chunkModel()
    suppressMessages(expect_identical(rxOptExpr(.m, "model"),
                                      rxOptExpr(.m, "model", chunkLines = 0L)))
  })

  test_that("chunked optimization yields an equivalent model", {
    .m <- .chunkModel()
    .whole <- suppressMessages(rxOptExpr(.m, "model"))
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

  test_that("a model at or below chunkLines is not chunked", {
    .m <- "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-cl/v*center"
    suppressMessages(expect_identical(rxOptExpr(.m, "model", chunkLines = 40L),
                                      rxOptExpr(.m, "model")))
  })

  test_that("compartment-scoped disguise/restore round-trips exactly", {
    # byte-exact for every spacing variant, so a chunk boundary can never alter the
    # surrounding whitespace, and idempotent in both directions
    .lines <- c("depot(0)=W0", "  depot(0)=W0", "\tdepot(0)=W0", "depot(0) = W0",
                "depot (0)=W0", "f(depot)=ff", "f( depot )=ff", "alag( central )=tlag",
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
    # nothing compartment-scoped is left for a chunk to orphan
    .lhs <- trimws(sub("=.*$", "", strsplit(.rxDisguiseCmt(.txt), "\n", fixed = TRUE)[[1]]))
    expect_false(any(endsWith(.lhs, "(0)")))
    expect_false(any(grepl("^(f|alag|lag|rate|dur)\\(", .lhs) & endsWith(.lhs, ")")))
  })

  test_that("chunks are cost balanced and lossless", {
    .ln <- c("aaaaaaaaaaaaaaaaaaaa", "b", "c", "dddddddddddddddddddd", "e", "f")
    .ch <- .rxBalancedChunks(.ln, 20)
    expect_identical(unlist(.ch, use.names = FALSE), .ln)  # order and content preserved
    expect_true(length(.ch) > 1L)
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

    # (a) valid model whose chunk orphans a compartment-scoped line.  `f (depot)` (a space
    # before the paren) is accepted by rxode2 but is not matched as a dosing modifier, so it
    # is not disguised and its chunk cannot be optimized on its own.
    .frag <- paste(c("d/dt(depot)=-exp(THETA[1])*depot",
                     "d/dt(center)=exp(THETA[1])*depot-exp(THETA[2])*center",
                     .pad, "f (depot)=exp(THETA[3])", "rx_pred_=center"), collapse = "\n")
    .out <- suppressMessages(rxOptExpr(.frag, "model", chunkLines = 40L))
    expect_identical(.out, suppressMessages(rxOptExpr(.frag, "model")))
    expect_true(grepl("rx_expr_", .out))   # fell back, so it is still fully optimized
    expect_error(rxModelVars(.out), NA)

    # (b) genuinely malformed model: chunking must raise, exactly as the unchunked call does
    .bad <- paste(c("d/dt(depot)=-ka*depot", "y = = 3 +", .pad), collapse = "\n")
    suppressMessages(expect_error(rxOptExpr(.bad, "model")))
    suppressMessages(expect_error(rxOptExpr(.bad, "model", chunkLines = 40L)))
  })

  test_that("parallel chunking gives the identical model and leaves no daemons behind", {
    skip_if_not_installed("mirai")
    .m <- .chunkModel()
    .seq <- suppressMessages(rxOptExpr(.m, "model", chunkLines = 40L))
    .par <- suppressMessages(rxOptExpr(.m, "model", chunkLines = 40L, parallel = 2L))
    # optimizing the chunks in daemons must not change the model at all
    expect_identical(.seq, .par)
    # the pool is started for the call and shut down when it returns
    expect_equal(sum(mirai::status()$connections), 0L)
  })
})
