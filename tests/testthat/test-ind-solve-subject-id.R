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
      as.data.frame(et(amt = 100 * i, ii = 12, until = 48) %>%
                      et(seq(0, 48, by = 2)) %>%
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
})
