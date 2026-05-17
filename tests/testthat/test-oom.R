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

test_that("rxSolve with oomFile returns rxSolveOom and manifest is written", {
  rxTest({
    mod <- rxode2({
      d/dt(A) <- -k * A
    })
    et_pop <- et(seq(0, 24, by = 1)) |> et(amt = 100) |> et(id = 1:10)

    .prefix <- tempfile("oomsim")
    out <- rxSolve(mod, c(k = 0.1), et_pop,
                   oomFile = .prefix, oomChunkSize = 5L)
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
