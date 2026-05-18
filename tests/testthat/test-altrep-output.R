rxTest({
  .isAltrep <- function(x) {
    rxIs(x, "altrep")
  }

  test_that("repeated simulation event columns use ALTREP repetition", {
    mod <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(centr) <- ka * depot - cl / v * centr
      cp <- centr / v
    })
    ev <- et(amt = 100, cmt = 1, ii = 12, addl = 1) |> et(seq(0, 24, by = 1))

    p <- data.frame(id = 1:2, ka = c(1, 1), cl = c(1, 1), v = c(10, 10))
    out <- rxSolve(mod, p, ev,
                   addDosing = TRUE,
                   returnType = "data.frame")

    expect_true(.isAltrep(out$time))
    expect_true(.isAltrep(out$evid))
    expect_true(.isAltrep(out$amt))
  })

  test_that("runtime event mutation fallback keeps event columns materialized", {
    mod <- rxode2({
      d/dt(x) <- -x
      if (t < 1) evid_(t + 0.1, 1, 10, 1, 0, 0, 0, 0)
    })
    ev <- et(amt = 1, time = 0) |> et(seq(0, 1, by = 0.1))

    p <- data.frame(id = 1:2, x = c(1, 1))
    out <- rxSolve(mod, p, ev,
                   addDosing = TRUE, returnType = "data.frame")

    expect_false(.isAltrep(out$time))
  })

  # ── covariate (addCov = TRUE) columns ──────────────────────────────────────

  test_that("covariate column is ALTREP via homogenous event-table path", {
    # Identical subjects → homogenous ET optimization → rx->nsim > 1
    mod <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(centr) <- ka * depot - (cl * (wt / 70)^0.75) / v * centr
      cp <- centr / v
    })
    ev <- et(amt = 100, cmt = 1, ii = 12, addl = 1) |> et(seq(0, 24, by = 1))
    ev_wt <- cbind(ev, wt = 70.0)   # same covariate for every subject
    p <- data.frame(id = 1:3, ka = 0.5, cl = 1, v = 10)

    out <- rxSolve(mod, p, ev_wt, addCov = TRUE, returnType = "data.frame")

    expect_true(.isAltrep(out$wt))
    expect_true(all(out$wt == 70))
  })

  test_that("covariate columns are ALTREP via explicit nsim (stochastic) path", {
    # Multiple virtual studies from omega draws; covariate values are fixed
    # per-subject across sims → each sim block is identical → ALTREP.
    pk_cov <- function() {
      ini({
        tka <- 0.5; tcl <- 0; tv <- log(10)
        eta.ka ~ 0.3; eta.cl ~ 0.2; eta.v ~ 0.1
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl) * (wt / 70)^0.75 * (age / 40)^0.3
        v  <- exp(tv + eta.v)
        d/dt(depot) <- -ka * depot
        d/dt(centr) <- ka * depot - cl / v * centr
        cp <- centr / v
      })
    }
    ev_df <- as.data.frame(
      et(amt = 100, cmt = 1) |> et(seq(0, 24, by = 1)) |> et(id = 1:3)
    )
    n <- sum(ev_df$id == 1L)
    ev_df$wt  <- rep(c(70, 80, 90), each = n)
    ev_df$age <- rep(c(30, 40, 50), each = n)

    set.seed(42)
    out <- rxSolve(pk_cov, ev_df, nsim = 3, addCov = TRUE,
                   returnType = "data.frame")

    expect_true(.isAltrep(out$wt))
    expect_true(.isAltrep(out$age))
    for (.id in 1:3) {
      expect_equal(unique(out$wt[out$id  == .id]), c(70, 80, 90)[.id])
      expect_equal(unique(out$age[out$id == .id]), c(30, 40, 50)[.id])
    }
  })

  # ── keep columns ───────────────────────────────────────────────────────────

  test_that("keep columns (double, integer, logical, factor) are ALTREP via homogenous event-table path", {
    mod <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(centr) <- ka * depot - cl / v * centr
      cp <- centr / v
    })
    ev <- et(amt = 100, cmt = 1, ii = 12, addl = 1) |> et(seq(0, 24, by = 1))
    # cbind onto a single-subject event table; identical for every subject
    ev_df <- cbind(ev, score = 1.5, code = 2L, flag = TRUE, label = "low")
    ev_df$grp <- factor(rep("A", nrow(ev_df)), levels = c("A", "B"))
    p <- data.frame(id = 1:3, ka = 0.5, cl = 1, v = 10)

    out <- rxSolve(mod, p, ev_df, returnType = "data.frame",
                   keep = c("score", "code", "flag", "grp", "label"))

    expect_true(.isAltrep(out$score))
    expect_true(.isAltrep(out$code))
    expect_true(.isAltrep(out$flag))
    expect_true(.isAltrep(out$grp))
    # Factor levels must survive the ALTREP wrapping
    expect_equal(levels(out$grp), c("A", "B"))
    # Character keep columns cannot be rep-ALTREP
    expect_false(.isAltrep(out$label))
    # All values must be correct
    expect_true(all(out$score == 1.5))
    expect_true(all(out$label == "low"))
  })

  test_that("keep columns (double, integer, logical, factor) are ALTREP via explicit nsim (stochastic) path", {
    # Per-subject varying keep values; same values in every sim block → ALTREP.
    pk_keep <- function() {
      ini({
        tka <- 0.5; tcl <- 0; tv <- log(10)
        eta.ka ~ 0.3; eta.cl ~ 0.2; eta.v ~ 0.1
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v  <- exp(tv + eta.v)
        d/dt(depot) <- -ka * depot
        d/dt(centr) <- ka * depot - cl / v * centr
        cp <- centr / v
      })
    }
    ev_df <- as.data.frame(
      et(amt = 100, cmt = 1) |> et(seq(0, 24, by = 1)) |> et(id = 1:3)
    )
    n <- sum(ev_df$id == 1L)
    ev_df$score <- rep(c(1.5, 2.5, 3.5),          each = n)
    ev_df$code  <- rep(c(1L,  2L,  3L),            each = n)
    ev_df$flag  <- rep(c(TRUE, FALSE, TRUE),        each = n)
    ev_df$grp   <- factor(rep(c("A", "B", "A"),    each = n), levels = c("A", "B"))
    ev_df$label <- rep(c("low", "high", "low"),     each = n)

    set.seed(42)
    out <- rxSolve(pk_keep, ev_df, nsim = 3,
                   keep = c("score", "code", "flag", "grp", "label"),
                   returnType = "data.frame")

    expect_true(.isAltrep(out$score))
    expect_true(.isAltrep(out$code))
    expect_true(.isAltrep(out$flag))
    expect_true(.isAltrep(out$grp))
    # Factor levels must survive the ALTREP wrapping
    expect_equal(levels(out$grp), c("A", "B"))
    # Character keep columns cannot be rep-ALTREP
    expect_false(.isAltrep(out$label))
    # Values correct for each subject across all sims
    for (.id in 1:3) {
      .rows <- out$id == .id
      expect_equal(unique(out$score[.rows]), c(1.5, 2.5, 3.5)[.id])
      expect_equal(unique(out$code[.rows]),  c(1L,  2L,  3L)[.id])
      expect_equal(unique(out$label[.rows]), c("low", "high", "low")[.id])
    }
  })
})
