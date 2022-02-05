test_that("ordinal simulation", {

  f <- function() {
    ini({
      tkel <- 0.1
      tp0 <- -3
      eta.p ~ 0.02
      add.sd <- 0.2
    })
    model({
      kel <- tkel
      d/dt(kpd) <- -kel * kpd
      p1 <- expit(tp0 + eta.p)

      kpd ~ add(add.sd)

      cac ~ c(p1)
    })
  }


  tmp <- rxode2(f)

  expect_error(tmp$simulationModel, NA)

  ev <- et(amt=0.7, ii=24, until=7 * 24, cmt=1) %>%
    et(seq(0.1, 24 * 8, by=12), cmt=1) %>%
    et(seq(0.1, 24 * 8, by=12), cmt=2) %>%
    et(id=1:20) %>%
    dplyr::as_tibble()

  rxWithPreserveSeed({

    s <- rxSolve(tmp, ev,
                 returnType="tibble", addCov=TRUE)

    s <- s %>% dplyr::filter(CMT == 2)
    expect_equal(length(as.numeric(table(s$sim))), 2)
  })

})
