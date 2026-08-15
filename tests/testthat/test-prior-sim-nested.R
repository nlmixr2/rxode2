rxTest({

  ## A conditioned model (`eta ~ 0.1 | occ`) carries its omega as a 'lotri'
  ## of nesting levels and solves through `expandPars_()`.  That path reads
  ## the degrees of freedom off the omega itself, so a prior has to reach it
  ## there -- putting it on `priorOmega` is silent: the solve succeeds and
  ## simply never draws.  See issue #1253.

  .ev <- function() {
    .e <- et(et(amt=100), seq(0, 24, by=8))
    .e <- et(.e, id=1:4)
    .e$occ <- rep(1:2, length.out=nrow(.e))
    .e
  }

  .mod <- function(prior=NULL) {
    .f <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.cl ~ 0.3
        eta.v ~ 0.1 | occ
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        cp <- linCmt()
        cp ~ add(add.sd)
      })
    }
    .u <- rxode2(.f)
    if (is.null(prior)) return(.u)
    eval(parse(text=paste0(".u %>% ini(", prior, ")")))
  }

  .diag <- function(r) vapply(r$omegaList, function(m) diag(as.matrix(m)), double(3))

  test_that("a prior on the occasion level is drawn, the id level is not", {
    skip_on_cran()
    set.seed(3)
    .r <- rxSolve(.mod("prior(eta.v) ~ invWishart(5)"), .ev(), nStud=3, nSub=4)

    expect_equal(length(.r$omegaList), 3L)
    .d <- .diag(.r)
    ## the id level has no prior, so it stays at its estimate
    expect_true(all(.d[1, ] == 0.3))
    ## the occasion level was drawn, and differently each study
    expect_equal(length(unique(.d[2, ])), 3L)
    ## one draw is shared across the occasions of a study
    expect_equal(.d[2, ], .d[3, ])
  })

  test_that("a prior on the id level is drawn, the occasion level is not", {
    skip_on_cran()
    set.seed(5)
    .r <- rxSolve(.mod("prior(eta.cl) ~ invWishart(6)"), .ev(), nStud=3, nSub=4)

    .d <- .diag(.r)
    expect_equal(length(unique(.d[1, ])), 3L)
    expect_true(all(.d[2, ] == 0.1))
  })

  test_that("both levels can carry their own degrees of freedom", {
    skip_on_cran()
    set.seed(5)
    .r <- rxSolve(.mod("prior(eta.cl) ~ invWishart(6)") %>%
                    ini(prior(eta.v) ~ invWishart(5)),
                  .ev(), nStud=3, nSub=4)

    .d <- .diag(.r)
    expect_equal(length(unique(.d[1, ])), 3L)
    expect_equal(length(unique(.d[2, ])), 3L)
  })

  test_that("a nested model with no prior is unchanged", {
    skip_on_cran()
    set.seed(3)
    .r <- rxSolve(.mod(), .ev(), nStud=3, nSub=4)
    ## nothing to draw from, so nothing is drawn
    expect_equal(length(.r$omegaList), 0L)
  })

})
