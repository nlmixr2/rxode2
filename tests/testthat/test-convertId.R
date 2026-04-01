rxTest({
  test_that("in certain solves, the convertId_ did not always work, test here", {

    f <- function() {
      description <- "One compartment PK model with linear clearance"
      ini({
        lcl <- 1
        label("Clearance (CL)")
        lvc <- 3.45
        label("Central volume of distribution (V)")
        propSd <- c(0, 0.5)
        label("Proportional residual error (fraction)")
      })
      model({
        cl <- exp(lcl)
        vc <- exp(lvc)
        cp <- linCmt()
        cp ~ prop(propSd)
      })
    }

    dMod <-
      data.frame(
        Dose = c(5, 5, 5, 5, 5, 5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 5, 5, 5, 5, 5, 5),
        ID = c("001-0001", "001-0002", "001-0003", "001-0004", "001-0005", "001-0006", "001-0007", "001-0008", "001-0009", "001-0010", "001-0011", "001-0012", "001-0013", "001-0014", "001-0015", "001-0016", "001-0017", "001-0018"),
        TIME = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
        EVID = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
        CMT = c("central", "central", "central", "central", "central", "central", "central", "central", "central", "central", "central", "central", "central", "central", "central", "central", "central", "central"),
        AMT = c(5, 5, 5, 5, 5, 5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 5, 5, 5, 5, 5, 5),
        WT = c(70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70)
      )

    f <- suppressWarnings(rxSolve(f, dMod))

    expect_error(print(f), NA)
    ## expect_snapshot(print(f))

  })
})
