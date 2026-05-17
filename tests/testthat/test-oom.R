test_that("rxSolveChunked reproduces rxSolve output (seed reproducibility)", {
  rxTest({
    mod <- rxode2({
      d/dt(A) <- -k * A
    })
    et <- et(seq(0, 24, by = 1)) |> et(amt = 100, id = 1:20)

    rxSetSeed(42)
    ref <- as.data.frame(rxSolve(mod, list(k = 0.1), et))

    chnk <- rxSolveChunked(mod, list(k = 0.1), et, chunkSize = 5, seed = 42)
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
    et <- et(seq(0, 24, by = 1)) |> et(amt = 100, id = 1:10)

    .prefix <- tempfile("oomsim")
    out <- rxSolve(mod, list(k = 0.1), et, rxControl(oomFile = .prefix, oomChunkSize = 5L))
    expect_s3_class(out, "rxSolveOom")
    expect_true(file.exists(paste0(.prefix, "_manifest.rds")))
    expect_equal(nrow(out), 10L * 25L)

    print_out <- capture.output(print(out))
    expect_true(any(grepl("rxSolveOom", print_out)))
  })
})

test_that("$.rxSolveOom extracts a column across all chunks", {
  rxTest({
    mod <- rxode2({
      d/dt(A) <- -k * A
    })
    et <- et(seq(0, 24, by = 1)) |> et(amt = 100, id = 1:10)

    chnk <- rxSolveChunked(mod, list(k = 0.1), et, chunkSize = 3, seed = 99)
    .ref <- as.data.frame(rxSolve(mod, list(k = 0.1), et))

    .A_chnk <- chnk$A
    expect_length(.A_chnk, nrow(as.data.frame(chnk)))
  })
})

test_that("rxEventTableFile works with rds format", {
  rxTest({
    mod <- rxode2({
      d/dt(A) <- -k * A
    })
    et <- et(seq(0, 12, by = 1)) |> et(amt = 100, id = 1:6)
    .etDf <- as.data.frame(et)

    .tf <- tempfile(fileext = ".rds")
    saveRDS(.etDf, .tf)

    .etf <- rxEventTableFile(.tf, format = "rds")
    expect_s3_class(.etf, "rxEtFile")

    out <- rxSolveChunked(mod, list(k = 0.1), .etf, chunkSize = 2, seed = 7)
    expect_s3_class(out, "rxSolveOom")
    expect_equal(nrow(out), 6L * 13L)
  })
})
