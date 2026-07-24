rxTest({
  test_that("batched multi-subject linCmt() + modeled alag() does not leak amounts between subjects (#1153)", {
    mod <- function() {
      ini({
        tcl <- log(3)
        tv <- log(30)
        tka <- log(1)
        tlag <- log(0.7)
      })
      model({
        cl <- exp(tcl)
        v <- exp(tv)
        ka <- exp(tka)
        alag(depot) <- exp(tlag)
        cp <- linCmt(cl, v, ka)
      })
    }

    tms <- c(0.9, 1.7, 3, 6, 10)
    ev <- et(amt = 100, cmt = "depot") %>% et(tms)
    pars <- data.frame(tcl = log(3) + c(-0.5, 0, 0.5), tv = log(30),
                       tka = log(1), tlag = log(0.7))

    # reference: each subject solved alone
    ref <- unlist(lapply(seq_len(nrow(pars)), function(i) {
      rxSolve(mod, params = pars[i, ], events = ev,
              returnType = "data.frame", addDosing = FALSE)$cp
    }))

    # batched: subjects share one thread when cores < nSub; before the fix the
    # per-thread linCmt() save buffer leaked subject k-1's amounts into k
    for (cr in 1:2) {
      o <- rxSolve(mod, params = pars, events = ev, cores = cr,
                   returnType = "data.frame", addDosing = FALSE)
      expect_equal(o$cp, ref, tolerance = 1e-8)
    }
  })

  test_that("nonzero linCmt() initial amounts via inits= are still honored", {
    mod <- function() {
      ini({
        tcl <- log(3)
        tv <- log(30)
        tka <- log(1)
      })
      model({
        cl <- exp(tcl)
        v <- exp(tv)
        ka <- exp(tka)
        cp <- linCmt(cl, v, ka)
      })
    }
    tms <- c(0.5, 1, 2, 4)
    ev <- et(tms) # no dose; only the initial amount can make cp nonzero
    s <- rxSolve(mod, events = ev, inits = c(central = 50),
                 returnType = "data.frame")
    expect_equal(s$cp, 50 / 30 * exp(-0.1 * tms), tolerance = 1e-6)
  })
})
