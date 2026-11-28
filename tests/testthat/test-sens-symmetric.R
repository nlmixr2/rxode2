rxTest({
  .mod <- "
  ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
  d/dt(depot) <- -ka*depot
  d/dt(center) <- ka*depot - cl/v*center
  cp <- center/v"
  .et <- c("eta.ka", "eta.cl", "eta.v")
  .p <- c(tka = 0.4, tcl = -2, tv = 2, eta.ka = 0.05, eta.cl = 0.05, eta.v = 0.05)
  .ev <- et(amt = 100, cmt = "depot") |> et(seq(0.25, 48, length.out = 30))

  .sens2 <- function(symmetric) {
    .s <- .rxLoadPrune(rxModelVars(rxode2(.mod)), FALSE, pkTime = TRUE)
    .rxJacobian(.s, .et)
    .rxSens(.s, .et)
    .rxSens(.s, .et, .et, symmetric = symmetric)
    .s$..sens2
  }

  test_that("symmetric builds each unordered pair once", {
    .full <- .sens2(FALSE)
    .sym <- .sens2(TRUE)
    expect_length(.full, 2L * 3L * 3L)
    expect_length(.sym, 2L * 3L * 4L / 2L)
    ## the kept states keep their name and equation
    expect_true(all(.sym %in% .full))
    expect_identical(.sens2(TRUE), .sym)
  })

  test_that("default is the full grid", {
    expect_identical(.sens2(FALSE), {
      .s <- .rxLoadPrune(rxModelVars(rxode2(.mod)), FALSE, pkTime = TRUE)
      .rxJacobian(.s, .et)
      .rxSens(.s, .et)
      .rxSens(.s, .et, .et)
      .s$..sens2
    })
  })

  test_that("symmetric second order equals the full grid", {
    .full <- rxode2(.mod, calcSens = .et, calcSens2 = .et)
    .a <- rxSolve(.full, .p, .ev, atol = 1e-12, rtol = 1e-12, returnType = "data.frame")
    ## model text with the full grid's lines for the unordered pairs only
    .keep <- vapply(strsplit(.sens2(TRUE), "=", fixed = TRUE), `[`, "", 1L)
    .txt <- strsplit(rxNorm(.full), "\n")[[1]]
    .isS2 <- grepl("^d/dt\\(rx__sens_.*_BY_.*_BY_.*__\\)", .txt)
    .txt <- .txt[!.isS2 | vapply(.txt, function(x) any(startsWith(x, paste0(.keep, "="))), TRUE)]
    .sym <- rxode2(paste(.txt, collapse = "\n"))
    expect_length(grep("_BY_.*_BY_", rxModelVars(.sym)$state), 2L * 6L)
    .b <- rxSolve(.sym, .p, .ev, atol = 1e-12, rtol = 1e-12, returnType = "data.frame")
    .s2 <- grep("_BY_.*_BY_", names(.b), value = TRUE)
    for (.c in c("cp", "rx__sens_center_BY_eta.cl__", .s2)) {
      expect_equal(.b[[.c]], .a[[.c]], tolerance = 1e-10, info = .c)
    }
    ## S_ab == S_ba in the full grid, so the dropped states are duplicates
    .sn <- function(st, a, b) paste0("rx__sens_", st, "_BY_", a, "_BY_", b, "__")
    expect_equal(.a[[.sn("center", "eta.ka", "eta.v")]], .a[[.sn("center", "eta.v", "eta.ka")]], tolerance = 1e-8)
    expect_equal(.a[[.sn("depot", "eta.cl", "eta.v")]], .a[[.sn("depot", "eta.v", "eta.cl")]], tolerance = 1e-8)
  })
})
