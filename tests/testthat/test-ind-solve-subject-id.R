rxTest({
  # `ind_solve()` takes a SUBJECT ID (nlmixr2/nlmixr2est#1020).  The ~100
  # per-individual drivers it dispatches to must read their `solveid` argument
  # the same way; the position -> id mapping through `rx->ordId` belongs in the
  # `par_*()` loops, which are the only callers that walk positions.
  #
  # rxode2 itself always calls `sortIds(rx, 1)`, which leaves `rx->ordId` the
  # identity, so a mismatched driver cannot be observed from R here -- only a
  # downstream caller that calls `sortIds(rx, 0)` to reorder between iterations
  # (nlmixr2est's FOCEi) sees it.  The convention is therefore asserted against
  # the sources, which is also where it regresses: the drivers are near-copies
  # of one another and the mapping spreads by copy-paste.

  test_that("no ind_*() driver maps its solveid through rx->ordId", {
    .src <- file.path("..", "..", "src")
    skip_if(!dir.exists(.src), "source tree not available (installed package)")
    .files <- list.files(.src, "\\.(cpp|h)$", full.names = TRUE)
    skip_if(length(.files) == 0L)
    .bad <- character(0)
    for (.f in .files) {
      .l <- readLines(.f, warn = FALSE)
      .hit <- grep("rx->ordId[solveid]", .l, fixed = TRUE)
      if (length(.hit) == 0L) next
      .txt <- .l[.hit]
      # the one sanctioned form, in a par_*() loop that walks positions
      .ok <- grepl("int _id = rx->ordId[solveid] - 1;", .txt, fixed = TRUE) |
        grepl("^\\s*(//|\\*|/\\*)", .txt)
      if (any(!.ok)) {
        .bad <- c(.bad, sprintf("%s:%d: %s", basename(.f), .hit[!.ok],
                                trimws(.txt[!.ok])))
      }
    }
    expect_equal(.bad, character(0))
  })

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
        if (grepl("^\\s*(//|\\*|/\\*)", .l[.i])) next
        if (!grepl("setSeedEng1(seed0 + _id)", .l[.i], fixed = TRUE)) {
          .badSeed <- c(.badSeed, sprintf("%s:%d: %s", basename(.f), .i,
                                          trimws(.l[.i])))
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

  test_that("the par_*() loops still index rx->subjects by position", {
    # The other half of the same change: the loops walk positions and must map
    # to an id before calling a driver.  Solving the same problem with solvers
    # drawn from each family exercises a different par_*() loop each time.
    .m <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
    })
    # a different dose per subject, so a solve landing on the wrong individual
    # shifts cp by a factor of two or more -- far outside solver-order noise
    .ev <- do.call(rbind, lapply(1:4, function(i) {
      as.data.frame(et(amt = 100 * i, ii = 12, until = 48) |>
                      et(seq(0, 48, by = 2)) |>
                      et(id = i))
    }))
    .p <- c(ka = 1.5, cl = 2.7, v = 31)
    .ref <- rxSolve(.m, .ev, .p, method = "liblsoda",
                    returnType = "data.frame", addDosing = FALSE)
    # the tolerance only has to exclude a subject permutation, not to pin down
    # each method's truncation error -- backwardEuler is first order
    for (.meth in c("lsoda", "dop853", "rk4", "f78", "dop5", "ck54", "ros4",
                    "vern65", "vern76", "vern98", "dop87", "cvode", "abm",
                    "backwardEuler", "gauss6", "radauiia5", "sdirk43")) {
      .r <- rxSolve(.m, .ev, .p, method = .meth,
                    returnType = "data.frame", addDosing = FALSE)
      expect_equal(.r$id, .ref$id, info = .meth)
      expect_equal(.r$cp, .ref$cp, tolerance = 1e-2, info = .meth)
    }
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
    .ref <- rxSolve(.m, .ev, c(k = 1), method = "liblsoda", seed = 42,
                    returnType = "data.frame", addDosing = FALSE)
    for (.meth in c("dop853", "rk4", "lsoda", "lsode", "bdf")) {
      .r <- rxSolve(.m, .ev, c(k = 1), method = .meth, seed = 42,
                    returnType = "data.frame", addDosing = FALSE)
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
      invisible(suppressWarnings(rxSolve(.m, .ev, c(k = 1), method = meth,
                                         returnType = "data.frame")))
      rxGetSeed() - 42
    }
    .ref <- .advance("liblsoda")
    for (.meth in c("lsoda", "lsode", "bdf", "dop853", "rk4", "f78", "dop5",
                    "ck54", "ros4", "vern65", "vern98", "cvode", "abm", "em",
                    "backwardEuler", "gauss6", "radauiia5", "sdirk43", "trapz",
                    "ssp3", "euler", "heun", "midpoint", "rk3", "mm")) {
      expect_equal(.advance(.meth), .ref, info = .meth)
    }
  })

  test_that("the adjoint solvers seed per subject too", {
    # par_cvodesadj() never called setSeedEng1() at all, so its subjects
    # inherited whatever stream was current; it also never claimed or closed a
    # seed block.  These solvers need an adjoint-expanded model.
    skip_on_cran()
    .txt <- paste("ka <- 1.2",
                  "d/dt(depot) <- -ka * depot",
                  "d/dt(center) <- ka * depot - cl / v * center",
                  sep = "\n")
    .adj <- rxode2(.rxAdjointExpand(.txt, c("cl", "v"))$text)
    .ev <- et(amt = 100, cmt = "depot") |> et(c(1, 2, 6, 8, 12)) |> et(id = 1:6)
    .p <- c(cl = 3.5, v = 25)
    .advance <- function(meth) {
      rxSetSeed(42)
      invisible(suppressWarnings(rxSolve(.adj, .ev, params = .p, method = meth,
                                         cores = 1)))
      rxGetSeed() - 42
    }
    .ref <- .advance("liblsoda")
    for (.meth in c("cvodesadj", "liblsodaadj", "abs", "rk4s", "dop853s")) {
      expect_equal(.advance(.meth), .ref, info = .meth)
    }
  })
})
