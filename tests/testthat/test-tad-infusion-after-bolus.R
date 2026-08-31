rxTest({
  # rxode2 issue #1316: an infusion was dropped from the dose history whenever
  # the subject's dose record started with a bolus.  handleTlastInline() asked
  # for the infusion duration with ind->ixds, the solver's running dose counter,
  # which the output/lhs pass never advances, so the duration lookup failed
  # (tad()/dosenum()/tlast() then skipped the dose) or silently returned the
  # duration of a different infusion (making dose() wrong).

  .mod <- rxode2({
    ka <- 1
    cl <- 5
    v <- 50
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - (cl / v) * center
    tadx <- tad()
    dn <- dosenum()
    dz <- dose()
    tl <- tlast()
  })

  .obs <- function(t) {
    data.frame(time = t, evid = 0, amt = 0, cmt = "center", rate = 0, dur = 0)
  }
  .dose <- function(time, amt, cmt, rate = 0, dur = 0) {
    data.frame(time = time, evid = 1, amt = amt, cmt = cmt, rate = rate, dur = dur)
  }
  .solve <- function(e) {
    e$id <- 1
    rxSolve(.mod, e, returnType = "data.frame")[, c("time", "tadx", "dn", "dz", "tl")]
  }

  test_that("an infusion after a bolus still updates tad()/dosenum()/dose()", {
    .r <- .solve(rbind(.dose(0, 100, "depot"),
                       .dose(240, 50, "center", rate = 40),
                       .obs(c(1, 241, 250))))
    expect_equal(.r$dn, c(1, 2, 2))
    expect_equal(.r$tadx, c(1, 1, 10))
    expect_equal(.r$tl, c(0, 240, 240))
    expect_equal(.r$dz, c(100, 50, 50))
  })

  test_that("a modeled-duration infusion after a bolus is also counted", {
    .r <- .solve(rbind(.dose(0, 100, "depot"),
                       .dose(240, 50, "center", dur = 1.25),
                       .obs(c(1, 241, 250))))
    expect_equal(.r$dn, c(1, 2, 2))
    expect_equal(.r$tadx, c(1, 1, 10))
    expect_equal(.r$dz, c(100, 50, 50))
  })

  test_that("two infusions after a bolus are both counted", {
    .r <- .solve(rbind(.dose(0, 100, "depot"),
                       .dose(120, 50, "center", rate = 40),
                       .dose(240, 50, "center", rate = 40),
                       .obs(c(1, 121, 241, 250))))
    expect_equal(.r$dn, c(1, 2, 3, 3))
    expect_equal(.r$tadx, c(1, 1, 1, 10))
    expect_equal(.r$dz, c(100, 50, 50, 50))
  })

  test_that("a bolus into the infused compartment does not hide the infusion", {
    .r <- .solve(rbind(.dose(0, 100, "center"),
                       .dose(240, 50, "center", rate = 40),
                       .obs(c(1, 241, 250))))
    expect_equal(.r$dn, c(1, 2, 2))
    expect_equal(.r$tadx, c(1, 1, 10))
    expect_equal(.r$dz, c(100, 50, 50))
  })

  test_that("dose() reports each infusion's own amount, not the first one's", {
    # both infusions share rate 40 but have different durations; before the fix
    # the second one reported the first infusion's duration (hence amount)
    .r <- .solve(rbind(.dose(0, 100, "center", rate = 40),
                       .dose(240, 50, "center", rate = 40),
                       .obs(c(1, 241, 250))))
    expect_equal(.r$dn, c(1, 2, 2))
    expect_equal(.r$dz, c(100, 50, 50))
  })
})
