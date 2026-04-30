rxTest({
  test_that("model-defined IOV simulates with occasion data; RxODE#982", {
    two.compartment.tacro.test.Polimorf <- function() {
      ini({
        tka <- log(2.49)
        tcl <- log(19.84)
        tvc <- log(330.4)
        tvp <- log(118.2)
        tvq <- log(35.44)
        eta.ka ~ 0.82
        eta.cl ~ 0.358
        eta.vc ~ 1.186
        eta.q ~ 0.58
        iov.cl ~ 0.25^2 | occ
        prop.sd <- 0.204
      })
      model({
        cl <- exp(tcl + eta.cl + iov.cl)
        ka <- exp(tka + eta.ka)
        vc <- exp(tvc + eta.vc)
        vp <- exp(tvp)
        q <- exp(tvq + eta.q)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / vc * center
        d / dt(periph) <- q / vc * depot - q / vp * periph
        cp <- center / vc
        cp ~ prop(prop.sd)
      })
    }

    ev <- et(amt = 1000, ii = 24, addl = 6) |>
      et(seq(0, 24, by = 0.5)) |>
      et(seq(0, 24, by = 0.5) + 24 * 3) |>
      et(seq(0, 24, by = 0.5) + 24 * 6) |>
      et(id = 1:8)

    ev <- as.data.frame(ev)
    ev$occ <- 1
    ev$occ[ev$time > 25] <- 2
    ev$occ[ev$time > 24 * 5 + 1] <- 3

    set.seed(982)
    rxSetSeed(982)

    sol <- suppressWarnings(rxSolve(two.compartment.tacro.test.Polimorf, ev))

    expect_s3_class(sol, "rxSolve")
    expect_true(any(names(sol) == "iov.cl"))
    expect_true(any(names(sol) == "sim"))
    expect_equal(as.character(sort(unique(sol$occ))), c("1", "2", "3"))
    expect_true(all(is.finite(sol$iov.cl)))
    expect_true(any(!is.na(sol$sim)))
  })
})
