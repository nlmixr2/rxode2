rxTest({
  .isAltrep <- function(x) {
    rxIs(x, "altrep")
  }

  for (rt in c("data.frame", "tibble", "data.table", "rxSolve")) {
    for (ad in c(TRUE, FALSE, NA))  {

      test_that(sprintf("repeated simulation event columns use ALTREP repetition %s, %s",
                        rt, ad), {
        mod <- rxode2({
          d/dt(depot) <- -ka * depot
          d/dt(centr) <- ka * depot - cl / v * centr
          cp <- centr / v
        })
        ev <- et(amt = 100, cmt = 1, ii = 12, addl = 1) |> et(seq(0, 24, by = 1))

        p <- data.frame(id = 1:2, ka = c(1, 1), cl = c(1, 1), v = c(10, 10))
        out <- rxSolve(mod, p, ev,
                       addDosing = ad,
                       returnType = rt)

        if (!identical(rt, "data.table")) {
          expect_true(.isAltrep(out$sim.id))
          expect_true(.isAltrep(out$time))
          if (!identical(ad, FALSE)) {
            expect_true(.isAltrep(out$evid))
            expect_true(.isAltrep(out$amt))
          }
        }
      })

      test_that(sprintf("runtime event mutation fallback keeps event columns materialized %s %s", rt, ad), {

        mod <- rxode2({
          d/dt(x) <- -x
          if (t < 1) evid_(t + 0.1, 1, 10, 1, 0, 0, 0, 0)
        })
        ev <- et(amt = 1, time = 0) |> et(seq(0, 1, by = 0.1))

        p <- data.frame(id = 1:2, x = c(1, 1))
        out <- rxSolve(mod, p, ev,
                       addDosing = ad, returnType = rt)

        expect_false(.isAltrep(out$time))
      })

      # ── covariate (addCov = TRUE) columns ──────────────────────────────────────

      test_that(sprintf("covariate column is ALTREP via homogenous event-table path (%s %s)",
              rt, ad), {
        # Identical subjects → homogenous ET optimization → rx->nsim > 1
        mod <- rxode2({
          d/dt(depot) <- -ka * depot
          d/dt(centr) <- ka * depot - (cl * (wt / 70)^0.75) / v * centr
          cp <- centr / v
        })
        ev <- et(amt = 100, cmt = 1, ii = 12, addl = 1) |> et(seq(0, 24, by = 1))
        evWt <- cbind(ev, wt = 70.0)   # same covariate for every subject
        p <- data.frame(id = 1:3, ka = 0.5, cl = 1, v = 10)

        out <- rxSolve(mod, p, evWt, addCov = TRUE, returnType = rt)

        if (!identical(rt, "data.table")) {
          expect_true(.isAltrep(out$wt))
        }
        expect_true(all(out$wt == 70))
      })

      test_that(sprintf("covariate columns are ALTREP via explicit nsim (stochastic) path (%s %s)",
                      rt, ad), {
        # Multiple virtual studies from omega draws; covariate values are fixed
        # per-subject across sims → each sim block is identical → ALTREP.
        pkCov <- function() {
          ini({
            tka <- 0.5
            tcl <- 0
            tv <- log(10)
            eta.ka ~ 0.3
            eta.cl ~ 0.2
            eta.v ~ 0.1
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
        evDf <- as.data.frame(
          et(amt = 100, cmt = 1) |> et(seq(0, 24, by = 1)) |> et(id = 1:3)
        )
        n <- sum(evDf$id == 1L)
        evDf$wt  <- rep(c(70, 80, 90), each = n)
        evDf$age <- rep(c(30, 40, 50), each = n)

        set.seed(42)
        out <- rxSolve(pkCov, evDf, nsim = 3, addCov = TRUE,
                       returnType = rt)

        if (!identical(rt, "data.table")) {
          expect_true(.isAltrep(out$wt))
          expect_true(.isAltrep(out$age))
        }
        for (.id in 1:3) {
          expect_equal(unique(out$wt[out$id  == .id]), c(70, 80, 90)[.id])
          expect_equal(unique(out$age[out$id == .id]), c(30, 40, 50)[.id])
        }
      })

      # ── keep columns ───────────────────────────────────────────────────────────

      test_that(paste0("keep columns (double, integer, logical, factor) are ALTREP via homogenous event-table path, %s %s", rt, ad), {
        mod <- rxode2({
          d/dt(depot) <- -ka * depot
          d/dt(centr) <- ka * depot - cl / v * centr
          cp <- centr / v
        })
        ev <- et(amt = 100, cmt = 1, ii = 12, addl = 1) |> et(seq(0, 24, by = 1))
        # cbind onto a single-subject event table; identical for every subject
        evDf <- cbind(ev, score = 1.5, code = 2L, flag = TRUE, label = "low")
        evDf$grp <- factor(rep("A", nrow(evDf)), levels = c("A", "B"))
        p <- data.frame(id = 1:3, ka = 0.5, cl = 1, v = 10)

        out <- rxSolve(mod, p, evDf, returnType = rt,
                       keep = c("score", "code", "flag", "grp", "label"))

        if (!identical(rt, "data.table")) {
          expect_true(.isAltrep(out$score))
          expect_true(.isAltrep(out$code))
          expect_true(.isAltrep(out$flag))
          expect_true(.isAltrep(out$grp))
          # Factor levels must survive the ALTREP wrapping
          expect_equal(levels(out$grp), c("A", "B"))
          expect_true(.isAltrep(out$label))
        }
        # All values must be correct
        expect_true(all(out$score == 1.5))
        expect_true(all(out$label == "low"))
      })

      test_that(sprintf("nStud > 1 makes event-table and keep columns ALTREP (theo_sd / WT) %s %s", rt, ad), {

        one.cmt <- function() {
          ini({
            tka <- 0.45
            tcl <- log(c(0, 2.7, 100))
            tv <- 3.45
            eta.ka ~ 0.6
            eta.cl ~ 0.3
            eta.v ~ 0.1
            add.sd <- 0.7
          })
          model({
            ka <- exp(tka + eta.ka)
            cl <- exp(tcl + eta.cl)
            v  <- exp(tv + eta.v)
            linCmt() ~ add(add.sd)
          })
        }
        d <- nlmixr2data::theo_sd

        set.seed(42)
        out <- rxSolve(one.cmt, d, nStud = 4, keep = "WT",
                       addDosing = ad, returnType = rt)

        if (!identical(rt, "data.table")) {
          # Event-table columns that repeat identically across studies must be ALTREP.
          expect_true(.isAltrep(out$time))
          if (!identical(ad, FALSE)) {
            expect_true(.isAltrep(out$evid))
            expect_true(.isAltrep(out$amt))
          }
          # The kept WT covariate must also be ALTREP.
          expect_true(.isAltrep(out$WT))
        }

        # WT must be constant within each output id and match the source data.
        wtRef <- setNames(vapply(unique(d$ID), function(i) d$WT[d$ID == i][1], numeric(1)),
                          unique(d$ID))
        nOrig <- length(unique(d$ID))
        for (.id in unique(out$id)) {
          # id cycles 1..nOrig across each study block
          origId <- ((.id - 1L) %% nOrig) + 1L
          wtVals <- unique(out$WT[out$id == .id])
          expect_length(wtVals, 1)
          expect_equal(wtVals, wtRef[[as.character(origId)]])
        }
      })

      test_that(paste0("keep columns (double, integer, logical, factor) are ALTREP via explicit nsim (stochastic) path %s %s", rt, ad), {
        # Per-subject varying keep values; same values in every sim block → ALTREP.
        pkKeep <- function() {
          ini({
            tka <- 0.5
            tcl <- 0
            tv <- log(10)
            eta.ka ~ 0.3
            eta.cl ~ 0.2
            eta.v ~ 0.1
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
        evDf <- as.data.frame(
          et(amt = 100, cmt = 1) |> et(seq(0, 24, by = 1)) |> et(id = 1:3)
        )
        n <- sum(evDf$id == 1L)
        evDf$score <- rep(c(1.5, 2.5, 3.5),          each = n)
        evDf$code  <- rep(c(1L,  2L,  3L),            each = n)
        evDf$flag  <- rep(c(TRUE, FALSE, TRUE),        each = n)
        evDf$grp   <- factor(rep(c("A", "B", "A"),    each = n), levels = c("A", "B"))
        evDf$label <- rep(c("low", "high", "low"),     each = n)

        set.seed(42)
        out <- rxSolve(pkKeep, evDf, nsim = 3,
                       keep = c("score", "code", "flag", "grp", "label"),
                       returnType = rt)

        if (!identical(rt, "data.table")) {
          expect_true(.isAltrep(out$score))
          expect_true(.isAltrep(out$code))
          expect_true(.isAltrep(out$flag))
          expect_true(.isAltrep(out$grp))
          # Factor levels must survive the ALTREP wrapping
          expect_equal(levels(out$grp), c("A", "B"))
          expect_true(.isAltrep(out$label))
        }
        # Values correct for each subject across all sims
        for (.id in 1:3) {
          .rows <- out$id == .id
          expect_equal(unique(out$score[.rows]), c(1.5, 2.5, 3.5)[.id])
          expect_equal(unique(out$code[.rows]),  c(1L,  2L,  3L)[.id])
          expect_equal(unique(out$label[.rows]), c("low", "high", "low")[.id])
        }
      })
    }
  }
})
