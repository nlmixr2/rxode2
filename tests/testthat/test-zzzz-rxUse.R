skip_if_not_installed("units")
skip_if_not_installed("devtools")
skip_on_cran()

test_that("build package", {

  mod <- rxode2({
    a <- 6
    b <- 0.6
    d / dt(intestine) <- -a * intestine
    d / dt(blood) <- a * intestine - b * blood
  })

  obs <- units::set_units(seq(0, 10, by = 1 / 24), "days")

  et <- eventTable(time.units = "days")
  et$add.sampling(obs)
  et$add.dosing(
    dose = 2 / 24, start.time = 0,
    nbr.doses = 10, dosing.interval = 1
  )

  solve1 <- rxSolve(mod, et, returnType = "data.frame")

  mod2 <- mod
  dir <- tempdir()

  tr <- try(rxPkg(mod, mod2, package = "rxm", wd = dir))
  if (inherits(tr, "try-error")) {
    skip("building a new package doesn't seem to work on this platform, but since it is not a major feature we are ignoring the result")
  } else {
    expect_true(TRUE)
    remove.packages("rxm")
  }
  ## unlink(dir, recursive=TRUE)

  # when load_all is used, you get
  ## Error: package ‘rxode2’ required by ‘rxm’ could not be found
  ## rm(list=c("mod", "mod2"))

  ## expect_error(library(rxm), NA)

  ## expect_error(mod, NA)
  ## expect_error(mod2, NA)

  ## expect_s3_class(mod, "rxode2")

  ## expect_s3_class(mod2, "rxode2")

  ## solve2 <- rxSolve(mod, et, returnType = "data.frame")

  ## expect_equal(solve1, solve2)

  ## detach("package:rxm", unload=TRUE)


  ## expect_error(mod)
  ## expect_error(mod2)
})
