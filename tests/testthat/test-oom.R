test_that("rxSolveChunked is reproducible with the same seed", {
  rxTest({
    mod <- rxode2({
      k <- exp(lk + eta.k)
      d/dt(A) <- -k * A
    })
    et_pop <- et(seq(0, 24, by = 1)) |> et(amt = 100) |> et(id = 1:20)

    chnk1 <- rxSolveChunked(mod, c(lk = log(0.1)), et_pop, seed = 42,
                             omega = lotri::lotri(eta.k ~ 0.09),
                             chunkSize = 5)
    chnk2 <- rxSolveChunked(mod, c(lk = log(0.1)), et_pop, seed = 42,
                             omega = lotri::lotri(eta.k ~ 0.09),
                             chunkSize = 5)
    expect_s3_class(chnk1, "rxSolveOom")
    expect_equal(nrow(chnk1), 20L * 25L)

    got1 <- as.data.frame(chnk1)
    got2 <- as.data.frame(chnk2)
    expect_equal(got1$A, got2$A, tolerance = 1e-6)
  })
})

test_that("rxSolveChunked matches rxSolve with same rxSetSeed", {
  rxTest({
    mod <- rxode2({
      k <- exp(lk + eta.k)
      d/dt(A) <- -k * A
    })
    et_pop <- et(seq(0, 24, by = 1)) |> et(amt = 100) |> et(id = 1:20)

    rxSetSeed(42)
    full <- rxSolve(mod, c(lk = log(0.1)), et_pop,
                    omega = lotri::lotri(eta.k ~ 0.09))
    chnk <- rxSolveChunked(mod, c(lk = log(0.1)), et_pop, seed = 42,
                            omega = lotri::lotri(eta.k ~ 0.09),
                            chunkSize = 5)

    full_df <- as.data.frame(full)
    chnk_df <- as.data.frame(chnk)
    full_df <- full_df[order(full_df$id, full_df$time), ]
    chnk_df <- chnk_df[order(chnk_df$id, chnk_df$time), ]
    expect_equal(full_df$A, chnk_df$A, tolerance = 1e-6)
  })
})

test_that("rxSolve with file returns rxSolveOom and manifest is written", {
  rxTest({
    mod <- rxode2({
      d/dt(A) <- -k * A
    })
    et_pop <- et(seq(0, 24, by = 1)) |> et(amt = 100) |> et(id = 1:10)

    .prefix <- tempfile("oomsim")
    out <- rxSolve(mod, c(k = 0.1), et_pop,
                   file = .prefix, chunkSize = 5L)
    expect_s3_class(out, "rxSolveOom")
    expect_true(file.exists(paste0(.prefix, "_manifest.rds")))

    print_out <- capture.output(print(out))
    expect_true(any(grepl("rxSolveOom", print_out)))
  })
})

test_that("$.rxSolveOom extracts a column across all chunks", {
  rxTest({
    mod <- rxode2({
      d/dt(A) <- -k * A
    })
    et_pop <- et(seq(0, 24, by = 1)) |> et(amt = 100) |> et(id = 1:10)

    chnk <- rxSolveChunked(mod, c(k = 0.1), et_pop, seed = 99, chunkSize = 3)

    .A_chnk <- chnk$A
    expect_length(.A_chnk, nrow(as.data.frame(chnk)))
  })
})

test_that("as_arrow_table.rxSolveOom returns an Arrow Table", {
  rxTest({
    skip_if_not_installed("arrow")
    mod <- rxode2({
      d/dt(A) <- -k * A
    })
    et_pop <- et(seq(0, 24, by = 1)) |> et(amt = 100) |> et(id = 1:10)
    chnk <- rxSolveChunked(mod, c(k = 0.1), et_pop, chunkSize = 3)

    tbl <- arrow::as_arrow_table(chnk)
    expect_true(inherits(tbl, "Table"))
    expect_equal(as.integer(tbl$num_rows), nrow(chnk))
    expect_true("A" %in% names(tbl))
  })
})

test_that("as_arrow_dataset.rxSolveOom returns a lazy Arrow Dataset", {
  rxTest({
    skip_if_not_installed("arrow")
    skip_if_not_installed("dplyr")
    mod <- rxode2({
      d/dt(A) <- -k * A
    })
    et_pop <- et(seq(0, 24, by = 1)) |> et(amt = 100) |> et(id = 1:10)
    chnk <- rxSolveChunked(mod, c(k = 0.1), et_pop, chunkSize = 3)

    ds <- as_arrow_dataset(chnk)
    expect_true(inherits(ds, "Dataset"))

    collected <- dplyr::collect(ds)
    full_df   <- as.data.frame(chnk)
    collected <- collected[order(collected$id, collected$time), ]
    full_df   <- full_df[order(full_df$id, full_df$time), ]
    expect_equal(collected$A, full_df$A, tolerance = 1e-6)
  })
})

test_that("as.data.frame.rxSolveOom is consistent with and without arrow", {
  rxTest({
    mod <- rxode2({
      d/dt(A) <- -k * A
    })
    et_pop <- et(seq(0, 24, by = 1)) |> et(amt = 100) |> et(id = 1:10)
    chnk <- rxSolveChunked(mod, c(k = 0.1), et_pop, chunkSize = 3)

    df <- as.data.frame(chnk)
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 10L * 25L)
    expect_true("A" %in% names(df))
  })
})

test_that("as_tibble.rxSolveOom returns a tibble", {
  rxTest({
    skip_if_not_installed("tibble")
    mod <- rxode2({
      d/dt(A) <- -k * A
    })
    et_pop <- et(seq(0, 24, by = 1)) |> et(amt = 100) |> et(id = 1:10)
    chnk <- rxSolveChunked(mod, c(k = 0.1), et_pop, chunkSize = 3)

    tib <- tibble::as_tibble(chnk)
    expect_true(inherits(tib, "tbl_df"))
    expect_equal(nrow(tib), 10L * 25L)
  })
})

test_that("as.data.table.rxSolveOom returns a data.table", {
  rxTest({
    skip_if_not_installed("data.table")
    mod <- rxode2({
      d/dt(A) <- -k * A
    })
    et_pop <- et(seq(0, 24, by = 1)) |> et(amt = 100) |> et(id = 1:10)
    chnk <- rxSolveChunked(mod, c(k = 0.1), et_pop, chunkSize = 3)

    dt <- data.table::as.data.table(chnk)
    expect_true(data.table::is.data.table(dt))
    expect_equal(nrow(dt), 10L * 25L)
  })
})

test_that("print.rxSolveOom shows [Arrow-backed] when arrow is installed", {
  rxTest({
    skip_if_not_installed("arrow")
    mod <- rxode2({
      d/dt(A) <- -k * A
    })
    et_pop <- et(seq(0, 24, by = 1)) |> et(amt = 100) |> et(id = 1:10)
    chnk <- rxSolveChunked(mod, c(k = 0.1), et_pop, chunkSize = 3)

    out <- capture.output(print(chnk))
    expect_true(any(grepl("Arrow-backed", out)))
  })
})

test_that("rxEventTableFile works with rds format", {
  rxTest({
    mod <- rxode2({
      d/dt(A) <- -k * A
    })
    et <- et(seq(0, 12, by = 1)) |> et(amt = 100)
    .etDf <- as.data.frame(et)

    .tf <- tempfile(fileext = ".rds")
    saveRDS(.etDf, .tf)

    .etf <- rxEventTableFile(.tf, format = "rds")
    expect_s3_class(.etf, "rxEtFile")
  })
})

test_that("rxSolve(parallel=) mirai path matches the serial chunked solve", {
  rxTest({
    skip_if_not_installed("mirai")
    skip_if_not_installed("arrow")
    mod <- rxode2({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v  <- exp(tv)
      d/dt(depot)  <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp           <- center / v
    })
    et_pop <- et(amt = 100) |> et(seq(0, 24, by = 4)) |> et(id = 1:6)
    pars   <- c(tka = log(0.5), tcl = log(4), tv = log(70))
    omega  <- lotri::lotri(eta.ka ~ 0.09, eta.cl ~ 0.04)

    rxSetSeed(42)
    ser <- rxSolve(mod, pars, et_pop, omega = omega,
                   file = tempfile("rxSer"), chunkSize = 2)
    rxSetSeed(42)
    par <- rxSolve(mod, pars, et_pop, omega = omega,
                   file = tempfile("rxPar"), chunkSize = 2, parallel = 2)

    expect_s3_class(par, "rxSolveOom")
    ser_df <- as.data.frame(ser)
    par_df <- as.data.frame(par)
    ser_df <- ser_df[order(ser_df$id, ser_df$time), ]
    par_df <- par_df[order(par_df$id, par_df$time), ]
    expect_equal(nrow(par_df), nrow(ser_df))
    expect_equal(sort(unique(par_df$id)), 1:6)
    # parallel chunking must not change the eta draws -> identical solve
    expect_equal(par_df$cp, ser_df$cp, tolerance = 1e-8)
  })
})
