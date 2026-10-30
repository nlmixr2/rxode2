rxTest({
  # adjoint sensitivities against finite differences for a model whose f() (and
  # modeled infusion duration) read newind: 1 at the first dose, 2 after it
  .newindAdj <- function(.m, .cs, .p, .ev, .methods) {
    .mf <- rxode2(.m)
    .center <- function(p) {
      as.data.frame(rxSolve(.mf, .ev, params = p, atol = 1e-12, rtol = 1e-12))$center
    }
    .base <- .center(.p)
    .fd <- vapply(
      .cs,
      function(.pn) {
        .p2 <- .p
        .p2[.pn] <- .p2[.pn] + 1e-6
        (.center(.p2) - .base) / 1e-6
      },
      numeric(length(.base))
    )
    .madj <- rxode2(.rxAdjointExpand(.m, .cs)$text)
    for (.meth in .methods) {
      .s <- as.data.frame(rxSolve(.madj, .ev, params = .p, method = .meth, cores = 1, atol = 1e-10, rtol = 1e-10))
      for (.pn in .cs) {
        expect_equal(
          .s[[sprintf("rx__sens_center_BY_%s__", .pn)]],
          .fd[, .pn],
          tolerance = 1e-4,
          label = paste(.meth, .pn)
        )
      }
    }
  }

  test_that("adjoint F sensitivity uses newind = 1 at the first dose (#1526)", {
    skip_on_cran()
    .newindAdj(
      "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\nf(depot)=F1*(newind <= 1) + F2*(newind > 1)",
      c("ka", "cl", "v", "F1", "F2"),
      c(ka = 1.2, cl = 3.5, v = 25, F1 = 0.7, F2 = 0.4),
      et(et(amt = 100, cmt = "depot", ii = 6, addl = 2), c(1, 7, 13)),
      c("rk4s", "dop853s", "liblsodaadj", "radauiia5s")
    )
  })

  test_that("adjoint modeled-infusion sensitivity uses newind = 1 at the first dose (#1526)", {
    skip_on_cran()
    # liblsodaadj and cvodesadj do not carry the f()/dur() infusion duals
    .newindAdj(
      "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\nf(depot)=F1*(newind <= 1) + F2*(newind > 1)\ndur(depot)=D1*(newind <= 1) + D2*(newind > 1)",
      c("ka", "cl", "v", "F1", "F2", "D1", "D2"),
      c(ka = 1.2, cl = 3.5, v = 25, F1 = 0.7, F2 = 0.4, D1 = 1.5, D2 = 2.5),
      et(et(amt = 100, cmt = "depot", ii = 6, addl = 2, rate = -2), c(1, 3, 7, 13)),
      c("rk4s", "dop853s", "radauiia5s")
    )
  })

  test_that("adjoint newind at a first-record steady state and a late data start (#1526)", {
    skip_on_cran()
    .newindAdj(
      "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\nf(depot)=F1*(newind <= 1) + F2*(newind > 1)\ndur(depot)=D1*(newind <= 1) + D2*(newind > 1)",
      c("ka", "cl", "v", "F1", "F2", "D1", "D2"),
      c(ka = 1.2, cl = 3.5, v = 25, F1 = 0.7, F2 = 0.4, D1 = 1.5, D2 = 2.5),
      et(et(amt = 100, cmt = "depot", ii = 6, ss = 1, rate = -2), c(1, 3, 5)),
      c("rk4s", "dop853s")
    )
    # the hidden time-0 start record is not the first record
    .newindAdj(
      "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\nf(depot)=F1*(newind <= 1) + F2*(newind > 1)",
      c("ka", "cl", "v", "F1", "F2"),
      c(ka = 1.2, cl = 3.5, v = 25, F1 = 0.7, F2 = 0.4),
      et(et(amt = 100, cmt = "depot", time = 1, ii = 6, addl = 2), c(2, 8, 14)),
      c("rk4s", "dop853s", "liblsodaadj", "radauiia5s")
    )
  })
})
