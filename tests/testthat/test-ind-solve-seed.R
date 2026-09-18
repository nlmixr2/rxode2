rxTest({
  # Every par_*() loop seeds a subject's random stream the same way (seed0 + id)
  # and closes the seed block it claimed, for every solver including the
  # adjoint ones.  Subject indexing (rx->ordId) is in
  # test-ind-solve-subject-id.R.

  test_that("every par_*() loop seeds per subject and closes its block", {
    # The same copy-paste exposure as above, for the seeding: a loop must seed
    # `seed0 + _id` (not `seed0 + solveid - 1`, which four solvers used, so a
    # subject's draws depended on which solver ran) and must close the block it
    # claimed with setRxSeedFinal(seed0 + nsolve) (98 never did).  indLin and
    # cvodesadj reject a model with an in-model random draw or need an
    # adjoint-expanded one, so this is the only check that reaches them.
    .src <- file.path("..", "..", "src")
    skip_if(!dir.exists(.src), "source tree not available (installed package)")
    .files <- list.files(.src, "\\.(cpp|h)$", full.names = TRUE)
    skip_if(length(.files) == 0L)
    .badSeed <- character(0)
    .unclosed <- character(0)
    for (.f in .files) {
      .l <- readLines(.f, warn = FALSE)
      .hit <- grep("setSeedEng1(seed0", .l, fixed = TRUE)
      for (.i in .hit) {
        if (grepl("^\\s*(//|\\*|/\\*)", .l[.i])) {
          next
        }
        if (!grepl("setSeedEng1(seed0 + _id)", .l[.i], fixed = TRUE)) {
          .badSeed <- c(.badSeed, sprintf("%s:%d: %s", basename(.f), .i, trimws(.l[.i])))
        }
      }
      # every claimed block must be closed in the same function
      .claim <- grep("seed0 = getRxSeed1(", .l, fixed = TRUE)
      for (.i in .claim) {
        .close <- which(.l == "}")
        .close <- .close[.close > .i]
        .stop <- if (length(.close)) .close[1] else length(.l)
        if (!any(grepl("setRxSeedFinal", .l[.i:.stop], fixed = TRUE))) {
          .unclosed <- c(.unclosed, sprintf("%s:%d", basename(.f), .i))
        }
      }
    }
    expect_equal(.badSeed, character(0))
    expect_equal(.unclosed, character(0))
  })

  test_that("every solver seeds a subject's random stream the same way", {
    # Each par_*() loop seeds the per-subject stream right before solving that
    # subject.  lsoda/lsode/bdf/indLin used to seed `seed0 + solveid - 1` where
    # the rest seed `seed0 + id`, so the same seed gave a different simulation
    # depending on which solver ran, and subject 0 was seeded outside the block
    # setRxSeedFinal() reserves.
    .m <- rxode2({
      d/dt(a) <- -k * a
      z <- rxnorm()
    })
    .ev <- et(amt = 1) |> et(0:3) |> et(id = 1:3)
    .ref <- rxSolve(.m, .ev, c(k = 1), method = "liblsoda", seed = 42, returnType = "data.frame", addDosing = FALSE)
    for (.meth in c("dop853", "rk4", "lsoda", "lsode", "bdf")) {
      .r <- rxSolve(.m, .ev, c(k = 1), method = .meth, seed = 42, returnType = "data.frame", addDosing = FALSE)
      expect_equal(.r$z, .ref$z, info = .meth)
    }
  })

  test_that("every solver closes the seed block it claimed", {
    # A par_*() loop claims a block with getRxSeed1(cores) but consumes one
    # seed per subject, so it has to close the block with
    # setRxSeedFinal(seed0 + nsolve).  98 solvers never did, leaving the global
    # seed short of what they used -- a second solve in the same session then
    # re-consumed seeds the first had already spent.  indLin is absent because
    # it rejects a model with an in-model random draw; it is covered by the
    # source check above instead.
    .m <- rxode2({
      d/dt(a) <- -k * a
      z <- rxnorm()
    })
    .ev <- et(amt = 1) |> et(0:1) |> et(id = 1:8)
    .advance <- function(meth) {
      rxSetSeed(42)
      invisible(suppressWarnings(rxSolve(.m, .ev, c(k = 1), method = meth, returnType = "data.frame")))
      rxGetSeed() - 42
    }
    .ref <- .advance("liblsoda")
    for (.meth in c(
      "lsoda",
      "lsode",
      "bdf",
      "dop853",
      "rk4",
      "f78",
      "dop5",
      "ck54",
      "ros4",
      "vern65",
      "vern98",
      "cvode",
      "abm",
      "em",
      "backwardEuler",
      "gauss6",
      "radauiia5",
      "sdirk43",
      "trapz",
      "ssp3",
      "euler",
      "heun",
      "midpoint",
      "rk3",
      "mm"
    )) {
      expect_equal(.advance(.meth), .ref, info = .meth)
    }
  })

  test_that("the adjoint solvers seed per subject too", {
    # par_cvodesadj() never called setSeedEng1() at all, so its subjects
    # inherited whatever stream was current; it also never claimed or closed a
    # seed block.  These solvers need an adjoint-expanded model.
    skip_on_cran()
    .txt <- paste("ka <- 1.2", "d/dt(depot) <- -ka * depot", "d/dt(center) <- ka * depot - cl / v * center", sep = "\n")
    .adj <- rxode2(.rxAdjointExpand(.txt, c("cl", "v"))$text)
    .ev <- et(amt = 100, cmt = "depot") |> et(c(1, 2, 6, 8, 12)) |> et(id = 1:6)
    .p <- c(cl = 3.5, v = 25)
    .advance <- function(meth) {
      rxSetSeed(42)
      invisible(suppressWarnings(rxSolve(.adj, .ev, params = .p, method = meth, cores = 1)))
      rxGetSeed() - 42
    }
    .ref <- .advance("liblsoda")
    for (.meth in c("cvodesadj", "liblsodaadj", "abs", "rk4s", "dop853s")) {
      expect_equal(.advance(.meth), .ref, info = .meth)
    }
  })
})
