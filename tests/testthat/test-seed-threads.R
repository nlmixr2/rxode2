rxTest({
  # nlmixr2/rxode2#1376: a seeded simulation must not depend on the thread count
  .ev <- et(amt = 320) |> et(c(0.5, 1, 2, 4, 8, 12, 24)) |> et(id = 1:12)
  .omega <- lotri(eta.ka ~ 0.6, eta.cl ~ 0.3, eta.v ~ 0.1)

  .mLin <- function() {
    ini({
      tka <- 0.45; tcl <- 1; tv <- 3.45
      eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd)
    })
  }

  .mOde <- function() {
    ini({
      tka <- 0.45; tcl <- 1; tv <- 3.45
      eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      r <- rnorm()
      cp ~ add(add.sd)
    })
  }

  for (.rxseed in c(-1, 1009)) {
    test_that(paste0("rxSolve() simulations do not depend on cores (rxseed=", .rxseed, ")"), {
      .sim <- function(cores) {
        rxWithSeed(1009, rxseed = .rxseed, {
          list(
            lin = rxSolve(.mLin, .ev, nStud = 5, cores = cores, returnType = "data.frame")$sim,
            ode = rxSolve(.mOde, .ev, cores = cores, returnType = "data.frame")[, c("sim", "r", "ka")],
            odeStud = rxSolve(.mOde, .ev, nStud = 2, cores = cores, returnType = "data.frame")[, c("sim", "r", "ka")],
            next1 = rxRmvn(3, c(0, 0, 0), .omega)
          )
        })
      }
      .s1 <- .sim(1L)
      expect_identical(.sim(2L), .s1)
      expect_identical(.sim(4L), .s1)
    })

    test_that(paste0("rxRmvn() draws do not depend on ncores (rxseed=", .rxseed, ")"), {
      .draw <- function(ncores) {
        rxWithSeed(1009, rxseed = .rxseed, {
          list(
            rxRmvn(50, c(0, 0, 0), .omega, ncores = ncores),
            rxRmvn(20, c(0, 0, 0), .omega, lower = -0.5, upper = 0.5, ncores = ncores),
            rxRmvn(20, 0, diag(1), lower = -0.5, upper = 0.5, ncores = ncores),
            rxRmvn(3, c(0, 0, 0), .omega)
          )
        })
      }
      .d1 <- .draw(1L)
      expect_identical(.draw(2L), .d1)
      expect_identical(.draw(4L), .d1)
    })
  }

  test_that("thread engines seeded by rxSeedEng() do not repeat across calls", {
    rxWithSeed(1009, rxseed = 1009, {
      .a <- rxnorm(n = 40, ncores = 4)
      .b <- rxnorm(n = 40, ncores = 4)
    })
    expect_length(intersect(.a, .b), 0)
  })
})
