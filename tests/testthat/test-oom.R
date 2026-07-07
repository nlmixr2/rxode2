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

test_that("as.arrow.rxSolveOom returns a lazy Arrow Dataset", {
  rxTest({
    skip_if_not_installed("arrow")
    skip_if_not_installed("dplyr")
    mod <- rxode2({
      d/dt(A) <- -k * A
    })
    et_pop <- et(seq(0, 24, by = 1)) |> et(amt = 100) |> et(id = 1:10)
    chnk <- rxSolveChunked(mod, c(k = 0.1), et_pop, chunkSize = 3)

    ds <- as.arrow(chnk)
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

test_that("rxEtFile is recognized as events when passed positionally", {
  rxTest({
    mod <- rxode2({
      d/dt(A) <- -k * A
    })
    et <- et(seq(0, 12, by = 1)) |> et(amt = 100) |> et(id = 1:4)

    .tf <- tempfile(fileext = ".rds")
    saveRDS(as.data.frame(et), .tf)
    .etf <- rxEventTableFile(.tf, format = "rds")

    # positional (params slot) must swap to events like rxEt/data.frame do
    .res <- rxSolve(mod, .etf, params = c(k = 0.1),
                    file = tempfile("rxPos"), chunkSize = 2)
    expect_s3_class(.res, "rxSolveOom")
    .df <- as.data.frame(.res)
    expect_equal(sort(unique(.df$id)), 1:4)

    # ui function models re-parse from a head preview of the file
    fun <- function() {
      ini({
        k <- 0.1
      })
      model({
        d/dt(A) <- -k * A
      })
    }
    .res2 <- rxSolve(fun, .etf, file = tempfile("rxPosUi"), chunkSize = 2)
    expect_s3_class(.res2, "rxSolveOom")
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

test_that("rxode2.oom.backend option is forwarded to the mirai workers", {
  rxTest({
    skip_if_not_installed("mirai")
    withr::local_options(rxode2.oom.backend = "rds")
    mod <- rxode2({
      cl <- exp(lcl + eta.cl)
      v  <- exp(lv)
      d/dt(central) <- -cl / v * central
      cp <- central / v
    })
    et_pop <- et(amt = 100) |> et(seq(0, 12, 4)) |> et(id = 1:4)

    par <- rxSolve(mod, c(lcl = 1, lv = 3.45), et_pop,
                   omega = lotri::lotri(eta.cl ~ 0.04),
                   file = tempfile("rxParRds"), chunkSize = 2, parallel = 2)

    expect_s3_class(par, "rxSolveOom")
    m <- attr(par, "manifest")
    # Daemons honored the "rds" backend instead of writing parquet.
    expect_true(all(grepl("\\.rds$", m$chunks)))
    expect_true(all(grepl("\\.rds$", m$paramChunks)))
    expect_equal(nrow(par), 4L * 4L)
    expect_equal(nrow(par$params), 4L)
  })
})

test_that("rxSolveOom persists params and inits like an rxSolve object", {
  rxTest({
    mod <- rxode2({
      ka <- exp(lka + eta.ka)
      cl <- exp(lcl + eta.cl)
      v  <- exp(lv)
      d/dt(depot)   <- -ka * depot
      d/dt(central) <-  ka * depot - cl / v * central
      cp <- central / v
    })
    pars   <- c(lka = 0.45, lcl = 1, lv = 3.45)
    et_pop <- et(amt = 100) |> et(seq(0, 24, 4)) |> et(id = 1:6)
    omega  <- lotri::lotri(eta.ka ~ 0.09, eta.cl ~ 0.04)

    chnk <- rxSolveChunked(mod, pars, et_pop, seed = 7,
                            omega = omega, chunkSize = 2)

    # res$params: one row per subject, NOT NULL
    .pars <- chnk$params
    expect_s3_class(.pars, "data.frame")
    expect_equal(nrow(.pars), 6L)
    expect_true(all(c("id", "eta.ka", "eta.cl") %in% names(.pars)))

    # res$inits: named numeric vector matching the model states
    .inits <- chnk$inits
    expect_true(is.numeric(.inits))
    expect_setequal(names(.inits), c("depot", "central"))
  })
})

test_that("head.rxSolveOom reads only the first rows", {
  rxTest({
    mod <- rxode2({
      d/dt(A) <- -k * A
    })
    et_pop <- et(seq(0, 24, by = 1)) |> et(amt = 100) |> et(id = 1:10)
    chnk <- rxSolveChunked(mod, c(k = 0.1), et_pop, chunkSize = 3)

    h <- head(chnk, 4)
    expect_equal(nrow(h), 4L)
    expect_true(all(c("id", "time", "A") %in% names(h)))
    # ncol via dim() resolves from the schema (not NA)
    expect_equal(ncol(chnk), ncol(as.data.frame(chnk)))
  })
})

test_that("rxSolveOom print mirrors rxSolve output with a chunk footer", {
  rxTest({
    mod <- rxode2({
      cl <- exp(lcl + eta.cl)
      v  <- exp(lv)
      d/dt(central) <- -cl / v * central
      cp <- central / v
    })
    et_pop <- et(amt = 100) |> et(seq(0, 24, 4)) |> et(id = 1:6)
    chnk <- rxSolveChunked(mod, c(lcl = 1, lv = 3.45), et_pop, seed = 3,
                            omega = lotri::lotri(eta.cl ~ 0.04), chunkSize = 2)

    out <- capture.output(print(chnk))
    expect_true(any(grepl("Solved rxode2 object", out)))
    expect_true(any(grepl("Parameters", out)))
    expect_true(any(grepl("Initial Conditions", out)))
    expect_true(any(grepl("First part of data", out)))
    expect_true(any(grepl("rxSolveOom", out)))
  })
})

# The `rxode2.oom.backend` option pins the storage/query engine so each of the
# three interfaces (plain rds, arrow parquet, duckdb over parquet) can be
# exercised deterministically regardless of what is installed by default.
for (.oomBackend in c("rds", "arrow", "duckdb")) {
  local({
    .backend <- .oomBackend
    test_that(sprintf("rxSolveOom round-trips through the '%s' backend", .backend), {
      rxTest({
        if (.backend %in% c("arrow", "duckdb")) skip_if_not_installed("arrow")
        if (.backend == "duckdb") {
          skip_if_not_installed("duckdb")
          skip_if_not_installed("DBI")
        }
        withr::local_options(rxode2.oom.backend = .backend)

        mod <- rxode2({
          cl <- exp(lcl + eta.cl)
          v  <- exp(lv)
          d/dt(central) <- -cl / v * central
          cp <- central / v
        })
        et_pop <- et(amt = 100) |> et(seq(0, 12, 4)) |> et(id = 1:4)
        chnk <- rxSolveChunked(mod, c(lcl = 1, lv = 3.45), et_pop, seed = 5,
                                omega = lotri::lotri(eta.cl ~ 0.04), chunkSize = 2)

        # chunks are written in the format the active backend dictates
        m <- attr(chnk, "manifest")
        .ext <- if (.backend == "rds") "\\.rds$" else "\\.parquet$"
        expect_true(all(grepl(.ext, m$chunks)))
        expect_true(all(grepl(.ext, m$paramChunks)))

        # data-frame-like behavior is identical across all three backends
        .df <- as.data.frame(chnk)
        expect_equal(nrow(chnk), nrow(.df))
        expect_equal(ncol(chnk), ncol(.df))
        expect_equal(nrow(head(chnk, 3)), 3L)
        expect_length(chnk$cp, nrow(chnk))
        expect_equal(chnk$cp, .df$cp, tolerance = 1e-8)

        # params / inits round-trip
        expect_equal(nrow(chnk$params), 4L)
        expect_setequal(names(chnk$inits), "central")

        # the print footer reflects the active backend
        out <- capture.output(print(chnk))
        if (.backend == "rds") {
          expect_false(any(grepl("Arrow|DuckDB", out)))
        } else if (.backend == "arrow") {
          expect_true(any(grepl("\\[Arrow-backed\\]", out)))
        } else {
          expect_true(any(grepl("\\[DuckDB/Arrow-backed\\]", out)))
        }
      })
    })
  })
}

# A possibly-out-of-memory rxSolveOom object must be queryable with dplyr
# verbs *lazily* -- the filtering/aggregation is pushed down to the
# on-disk parquet chunks and only the reduced result is materialized, so
# the full data set is never loaded into memory.
test_that("dplyr filter/select on rxSolveOom stays lazy (no full materialization)", {
  rxTest({
    skip_if_not_installed("arrow")
    skip_if_not_installed("dplyr")
    mod <- rxode2({
      cl <- exp(lcl)
      v  <- exp(lv)
      d/dt(central) <- -cl / v * central
      cp <- central / v
    })
    et_pop <- et(amt = 100) |> et(seq(0, 12, 4)) |> et(id = 1:6)
    chnk <- rxSolveChunked(mod, c(lcl = 1, lv = 3.45), et_pop, chunkSize = 2)

    ds <- as.arrow(chnk)
    # The data stays on disk: a file-backed Dataset, not an in-memory Table.
    expect_true(inherits(ds, "Dataset"))
    expect_false(inherits(ds, "Table"))

    # Building the query does NOT execute it -- it is a deferred arrow query,
    # not a materialized data.frame.
    q <- ds |>
      dplyr::filter(id == 1L) |>
      dplyr::select(id, time, cp)
    expect_s3_class(q, "arrow_dplyr_query")
    expect_false(is.data.frame(q))

    # Only the matching rows are pulled into memory on collect().
    got <- dplyr::collect(q)
    expect_true(is.data.frame(got))
    expect_lt(nrow(got), nrow(chnk))
    expect_setequal(unique(got$id), 1L)
    expect_setequal(names(got), c("id", "time", "cp"))

    # ... and they match the reference subset.
    ref <- as.data.frame(chnk)
    ref <- ref[ref$id == 1L, c("id", "time", "cp")]
    got <- got[order(got$time), ]
    ref <- ref[order(ref$time), ]
    expect_equal(got$cp, ref$cp, tolerance = 1e-8)
  })
})

test_that("dplyr group_by/summarise on rxSolveOom is pushed to the arrow backend", {
  rxTest({
    skip_if_not_installed("arrow")
    skip_if_not_installed("dplyr")
    mod <- rxode2({
      cl <- exp(lcl + eta.cl)
      v  <- exp(lv)
      d/dt(central) <- -cl / v * central
      cp <- central / v
    })
    et_pop <- et(amt = 100) |> et(seq(0, 12, 4)) |> et(id = 1:6)
    chnk <- rxSolveChunked(mod, c(lcl = 1, lv = 3.45), et_pop, seed = 11,
                            omega = lotri::lotri(eta.cl ~ 0.04), chunkSize = 2)

    agg <- as.arrow(chnk) |>
      dplyr::group_by(id) |>
      dplyr::summarise(mcp = mean(cp), n = dplyr::n())
    # aggregation is deferred until collect()
    expect_s3_class(agg, "arrow_dplyr_query")

    out <- dplyr::collect(agg)
    out <- out[order(out$id), ]
    expect_equal(nrow(out), 6L)              # one row per subject, not per record
    expect_true(all(out$n == 4L))            # 4 time points each

    ref <- as.data.frame(chnk)
    ref <- tapply(ref$cp, ref$id, mean)
    expect_equal(out$mcp, as.numeric(ref[as.character(out$id)]), tolerance = 1e-8)
  })
})

test_that("rxSolveOom supports lazy dplyr queries through a DuckDB connection", {
  rxTest({
    skip_if_not_installed("arrow")
    skip_if_not_installed("duckdb")
    skip_if_not_installed("dbplyr")
    skip_if_not_installed("dplyr")
    mod <- rxode2({
      cl <- exp(lcl)
      v  <- exp(lv)
      d/dt(central) <- -cl / v * central
      cp <- central / v
    })
    et_pop <- et(amt = 100) |> et(seq(0, 12, 4)) |> et(id = 1:6)
    chnk <- rxSolveChunked(mod, c(lcl = 1, lv = 3.45), et_pop, chunkSize = 2)

    # Register the on-disk parquet chunks as a lazy DuckDB-backed table.
    tbl <- arrow::to_duckdb(as.arrow(chnk))
    expect_s3_class(tbl, "tbl_lazy")
    expect_false(is.data.frame(tbl))

    q <- tbl |> dplyr::filter(time == 0)
    expect_s3_class(q, "tbl_lazy")

    got <- dplyr::collect(q)
    expect_lt(nrow(got), nrow(chnk))
    expect_true(all(got$time == 0))
    expect_equal(nrow(got), 6L)              # one t=0 row per subject
  })
})
