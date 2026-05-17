test_that("rxSolveChunked reproduces rxSolve output (seed reproducibility)", {
  rxTest({
    mod <- rxode2({
      d/dt(A) <- -k * A
    })
    # Use nStud for multi-study simulation that creates multiple rows
    et <- et(seq(0, 24, by = 1)) |> et(amt = 100)

    rxSetSeed(42)
    ref <- as.data.frame(rxSolve(mod, c(k = 0.1), et, nSub = 20,
                                  omega = lotri::lotri(eta.k ~ 0.09)))

    chnk <- rxSolveChunked(mod, c(k = 0.1), et, seed = 42,
                            omega = lotri::lotri(eta.k ~ 0.09),
                            nSub = 20, chunkSize = 5)
    expect_s3_class(chnk, "rxSolveOom")
    expect_equal(nrow(chnk), nrow(ref))

    got <- as.data.frame(chnk)
    expect_equal(got$A, ref$A, tolerance = 1e-6)
  })
})

test_that("rxSolve with oomFile returns rxSolveOom and manifest is written", {
  rxTest({
    mod <- rxode2({
      d/dt(A) <- -k * A
    })
    et <- et(seq(0, 24, by = 1)) |> et(amt = 100)

    .prefix <- tempfile("oomsim")
    out <- rxSolve(mod, c(k = 0.1), et,
                   omega = lotri::lotri(eta.k ~ 0.09), nSub = 10,
                   rxControl(oomFile = .prefix, oomChunkSize = 5L))
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
    et <- et(seq(0, 24, by = 1)) |> et(amt = 100)

    chnk <- rxSolveChunked(mod, c(k = 0.1), et, seed = 99,
                            omega = lotri::lotri(eta.k ~ 0.09),
                            nSub = 10, chunkSize = 3)

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
