rxTest({

  .isAltrep <- function(x) {
    rxIs(x, "altrep")
  }

  .isRepint <- function(x) {
    rxIs(x, "repint")
  }

  .isSeqrep <- function(x) {
    rxIs(x, "seqrep")
  }

  .isRepstr <- function(x) {
    rxIs(x, "repstr")
  }

  for (rt in c("data.frame", "tibble", "rxSolve")) {
    for (ad in c(TRUE, FALSE, NA))  {

      test_that(sprintf("repeated simulation event columns use ALTREP repetition %s, %s",
                        rt, ad), {

        mod <- rxode2({
          d/dt(depot) <- -ka * depot
          d/dt(centr) <- ka * depot - cl / v * centr
          cp <- centr / v
        })

        ev <- et(amt = 100, cmt = 1, ii = 12, addl = 1) |>
          et(seq(0, 24, by = 1))

        p <- data.frame(id = 1:2, ka = c(1, 1), cl = c(1, 1), v = c(10, 10))
        out <- rxSolve(mod, p, ev,
                       addDosing = ad,
                       returnType = rt)

          expect_true(.isAltrep(out$sim.id))
          # simSubjectPath: id column is absent (subjects identified by sim.id)
          expect_false("id" %in% names(out))
          expect_true(.isAltrep(out$time))
          if (!identical(ad, FALSE)) {
            expect_true(.isAltrep(out$evid))
            expect_true(.isAltrep(out$amt))
          }
      })

      test_that(sprintf("runtime event mutation fallback keeps event columns materialized %s %s", rt, ad), {

        mod <- rxode2({
          d/dt(x) <- -x
          if (t < 1) evid_(t + 0.1, 1, 10, 1, 0, 0, 0, 0)
        })
        ev <- et(amt = 1, time = 0) |>
          et(seq(0, 1, by = 0.1))

        p <- data.frame(id = 1:2, x = c(1, 1))
        out <- rxSolve(mod, p, ev,
                       addDosing = ad, returnType = rt)

        expect_false(.isAltrep(out$time))
      })

      # -- covariate (addCov = TRUE) columns --------------------------------------

      test_that(sprintf("covariate column is ALTREP via homogenous event-table path (%s %s)",
                        rt, ad), {

        # Identical subjects -> homogenous ET optimization -> rx->nsim > 1
        mod <- rxode2({
          d/dt(depot) <- -ka * depot
          d/dt(centr) <- ka * depot - (cl * (wt / 70)^0.75) / v * centr
          cp <- centr / v
        })
        ev <- et(amt = 100, cmt = 1, ii = 12, addl = 1) |>
          et(seq(0, 24, by = 1))
        evWt <- cbind(ev, wt = 70.0)   # same covariate for every subject
        p <- data.frame(id = 1:3, ka = 0.5, cl = 1, v = 10)

        out <- rxSolve(mod, p, evWt, addCov = TRUE, returnType = rt)

        expect_true(.isAltrep(out$wt))
        # simSubjectPath: id column is absent (subjects identified by sim.id)
        expect_false("id" %in% names(out))

        expect_true(all(out$wt == 70))
      })

      test_that(sprintf("covariate columns are ALTREP via explicit nsim (stochastic) path (%s %s)",
                        rt, ad), {

        # Multiple virtual studies from omega draws; covariate values are fixed
        # per-subject across sims -> each sim block is identical -> ALTREP.
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

      # -- keep columns -----------------------------------------------------------

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

        skip_if_not_installed("nlmixr2data")
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


        # Event-table columns that repeat identically across studies must be ALTREP.
        expect_true(.isAltrep(out$sim.id))
        expect_true(max(out$sim.id) == 4)
        expect_true(.isSeqrep(out$id))
        expect_true(.isAltrep(out$time))

        if (!identical(ad, FALSE)) {
          expect_true(.isAltrep(out$evid))
          expect_true(.isAltrep(out$amt))
        }
        # The kept WT covariate must also be ALTREP.
        expect_true(.isAltrep(out$WT))


        # WT must be constant within each output id and match the source data.
        wtRef <- setNames(vapply(unique(d$ID),
                                 function(i) {
                                   d$WT[d$ID == i][1]
                                 }, numeric(1)),
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
        # Per-subject varying keep values; same values in every sim block -> ALTREP.
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

        expect_true(.isAltrep(out$score))
        expect_true(.isAltrep(out$code))
        expect_true(.isAltrep(out$flag))
        expect_true(.isAltrep(out$grp))
        # Factor levels must survive the ALTREP wrapping
        expect_equal(levels(out$grp), c("A", "B"))
        expect_true(.isAltrep(out$label))
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

  # -- TBS columns (returnType = "data.frame.TBS") ---------------------------

  for (ad in c(TRUE, FALSE, NA)) {

    test_that(sprintf("TBS columns are ALTREP via homogeneous event-table path (addDosing=%s)", ad), {
      mod <- rxode2({
        d/dt(depot) <- -ka * depot
        d/dt(centr) <- ka * depot - cl / v * centr
        cp <- centr / v
      })
      ev <- et(amt = 100, cmt = 1, ii = 12, addl = 1) |> et(seq(0, 24, by = 1))
      p <- data.frame(id = 1:3, ka = 0.5, cl = 1, v = 10)

      out <- rxSolve(mod, p, ev, addDosing = ad, returnType = "data.frame.TBS")

      expect_true(.isAltrep(out$rxLambda))
      expect_true(.isAltrep(out$rxYj))
      expect_true(.isAltrep(out$rxLow))
      expect_true(.isAltrep(out$rxHi))
      # Values must be replicated correctly
      expect_true(all(out$rxLambda == out$rxLambda[1]))
      expect_true(all(out$rxYj    == out$rxYj[1]))
    })

    test_that(sprintf("TBS columns are ALTREP via nStud path (addDosing=%s)", ad), {
      skip_if_not_installed("nlmixr2data")
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
      out <- rxSolve(one.cmt, d, nStud = 3, addDosing = ad,
                     returnType = "data.frame.TBS")

      expect_true(.isAltrep(out$rxLambda))
      expect_true(.isAltrep(out$rxYj))
      expect_true(.isAltrep(out$rxLow))
      expect_true(.isAltrep(out$rxHi))
      expect_true(all(out$rxLambda == out$rxLambda[1]))
      expect_true(all(out$rxYj    == out$rxYj[1]))
    })
  }

  # -- Sequential integer IDs should never be a factor ----------------------

  test_that("sequential integer IDs are never a factor, regardless of event-table uniformity", {
    mod <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(centr) <- ka * depot - cl / v * centr
      cp <- centr / v
    })

    # Non-uniform event tables (arm1 vs arm2 style) -- non-ALTREP path.
    # Previously this produced a factor id due to a stale guard; verify it
    # now returns a plain integer.
    ev1 <- as.data.frame(et(amt = 1000, cmt = 1, time = c(0, 24, 48)) |> et(seq(0, 72, by = 5)))
    ev2 <- as.data.frame(et(amt = 2000, cmt = 1, time = c(5, 29, 53)) |> et(seq(0, 77, by = 6)))
    ev1$id <- 1L; ev2$id <- 2L
    ev3 <- ev1; ev3$id <- 3L; ev4 <- ev2; ev4$id <- 4L
    evDf <- rbind(ev1, ev2, ev3, ev4)
    p <- data.frame(id = 1:4, ka = 0.5, cl = 1, v = 10)

    out <- rxSolve(mod, p, evDf, returnType = "data.frame")
    expect_false(is.factor(out$id))
    expect_equal(sort(unique(out$id)), 1:4)

    # Uniform event tables -- ALTREP seqrep path.  Should also not be a factor.
    ev <- et(amt = 100, cmt = 1, ii = 12, addl = 1) |> et(seq(0, 24, by = 1))
    evDf2 <- rbind(
      cbind(as.data.frame(ev), id = 1L),
      cbind(as.data.frame(ev), id = 2L),
      cbind(as.data.frame(ev), id = 3L),
      cbind(as.data.frame(ev), id = 4L)
    )
    out2 <- rxSolve(mod, p, evDf2, returnType = "data.frame")
    expect_false(is.factor(out2$id))
    expect_equal(sort(unique(out2$id)), 1:4)
  })

  # -- Non-sequential integer IDs --------------------------------------------

  test_that("non-sequential integer IDs produce ALTREP repint id column (nStud > 1)", {
    pkMod <- function() {
      ini({
        tka <- 0.5
        tcl <- 0
        tv  <- log(10)
        eta.ka ~ 0.3
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl)
        v  <- exp(tv)
        d/dt(depot) <- -ka * depot
        d/dt(centr) <- ka * depot - cl / v * centr
        cp <- centr / v
      })
    }
    # IDs must come from the event table; the parameter data frame is matched
    # by position, not by its id column.
    evDf <- as.data.frame(
      et(amt = 100, cmt = 1, ii = 12, addl = 1) |>
        et(seq(0, 24, by = 1)) |>
        et(id = c(5L, 10L, 15L))
    )
    set.seed(42)
    out <- rxSolve(pkMod, evDf, nStud = 2)

    # nStud=2: fill loop writes lvlI values directly per subject; re-wrap block
    # creates rep_int(base, times=2) ALTREP since the pattern is uniform across sims.
    expect_true(.isAltrep(out$id))
    expect_true(.isRepint(out$id))
    expect_false(is.factor(out$id))
    expect_equal(sort(unique(out$id)), c(5L, 10L, 15L))
    for (.id in c(5L, 10L, 15L)) {
      expect_true(all(out$id[out$id == .id] == .id))
    }
  })

  # -- String keep columns -- dedicated rx_rep_str ALTREP tests --------------

  test_that("string keep column is rx_rep_str ALTREP via homogeneous path", {
    mod  <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(centr) <- ka * depot - cl / v * centr
      cp <- centr / v
    })
    ev   <- et(amt = 100, cmt = 1, ii = 12, addl = 1) |> et(seq(0, 24, by = 1))
    evDf <- cbind(ev, label = "low")
    p    <- data.frame(id = 1:3, ka = 0.5, cl = 1, v = 10)

    out <- rxSolve(mod, p, evDf, keep = "label")

    expect_true(.isAltrep(out$label))
    expect_true(.isRepstr(out$label))
    expect_true(all(out$label == "low"))
    expect_null(attr(out$label, "class"))
  })

  test_that("string keep column is rx_rep_str ALTREP via stochastic nsim path", {
    pkKeep <- function() {
      ini({ tka <- 0.5; tcl <- 0; tv <- log(10); eta.ka ~ 0.3 })
      model({
        ka <- exp(tka + eta.ka); cl <- exp(tcl); v <- exp(tv)
        d/dt(depot) <- -ka * depot
        d/dt(centr) <- ka * depot - cl / v * centr
        cp <- centr / v
      })
    }
    evDf <- as.data.frame(
      et(amt = 100, cmt = 1) |> et(seq(0, 24, by = 1)) |> et(id = 1:3)
    )
    n <- sum(evDf$id == 1L)
    evDf$label <- rep(c("low", "high", "low"), each = n)

    set.seed(42)
    out <- rxSolve(pkKeep, evDf, nsim = 3, keep = "label")

    expect_true(.isAltrep(out$label))
    expect_true(.isRepstr(out$label))
    for (.id in 1:3) {
      expect_equal(unique(out$label[out$id == .id]),
                   c("low", "high", "low")[.id])
    }
  })

  test_that("single-sim string keep column is NOT ALTREP", {
    mod  <- rxode2({ d/dt(A) <- -k * A })
    ev   <- et(seq(0, 12, by = 1)) |> et(amt = 100)
    evDf <- cbind(ev, label = "ctrl")

    out <- rxSolve(mod, c(k = 0.1), evDf, keep = "label")

    expect_false(.isAltrep(out$label))
    expect_equal(unique(out$label), "ctrl")
  })

  test_that("string ALTREP column materialises correctly", {
    mod  <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(centr) <- ka * depot - cl / v * centr
    })
    ev   <- et(amt = 100, cmt = 1) |> et(seq(0, 24, by = 1))
    evDf <- cbind(ev, grp = "A")
    p    <- data.frame(id = 1:4, ka = 0.5, cl = 1, v = 10)

    out <- rxSolve(mod, p, evDf, keep = "grp")
    expect_true(.isAltrep(out$grp))

    # Force full materialisation via DATAPTR then verify values are correct
    mat <- as.character(out$grp)
    expect_true(all(mat == "A"))
    expect_equal(length(mat), nrow(out))
  })

  test_that("element access on string ALTREP column exercises Elt without materialising", {
    mod  <- rxode2({ d/dt(A) <- -k * A })
    ev   <- et(seq(0, 10, by = 1)) |> et(amt = 1)
    evDf <- cbind(ev, trt = "drug")
    p    <- data.frame(id = 1:5, k = 0.2)

    out <- rxSolve(mod, p, evDf, keep = "trt")
    expect_true(.isAltrep(out$trt))

    expect_equal(out$trt[[1L]], "drug")
    expect_equal(out$trt[[nrow(out)]], "drug")
    expect_true(.isAltrep(out$trt))
  })
})
