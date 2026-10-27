rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # A subject whose adjoint solve fails returns NA states and sensitivities, as a
  # failed forward solve does, and leaves the other subjects' results alone.
  # With maxsteps = 100, the fast-absorption subject 2 cannot be solved.
  test_that("a failed cvodesadj subject returns NA and the others are unchanged", {
    .txt <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
    .one <- function(id) {
      data.frame(id = id, time = c(0, 1, 4, 8), evid = c(1L, 0L, 0L, 0L), amt = c(100, 0, 0, 0), cmt = "depot")
    }
    .ev <- rbind(.one(1L), .one(2L))
    .p <- data.frame(id = 1:2, ka = c(1.2, 3000), k = c(0.2, 0.2))
    expect_warning(
      .a <- rxSolveAdjointRk4(.txt, .ev, params = .p, calcSens = c("ka", "k"), method = "cvodesadj", maxsteps = 100),
      "could not solve the ODEs correctly; These values are replaced with .NA."
    )
    .cols <- c("depot", "center", grep("^rx__sens_", names(.a), value = TRUE))
    expect_length(.cols, 6L)
    expect_equal(nrow(.a), 6L)
    expect_true(all(is.na(as.matrix(.a[.a$id == 2, .cols]))))
    .s1 <- rxSolveAdjointRk4(.txt, .one(1L), params = c(ka = 1.2, k = 0.2), calcSens = c("ka", "k"),
      method = "cvodesadj", maxsteps = 100)
    for (.cn in .cols) expect_equal(.a[.a$id == 1, .cn], .s1[[.cn]], label = .cn)
  })
})
