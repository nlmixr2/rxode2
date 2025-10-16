rxTest({

  test_that("mix() in normal rxode2 model", {
    expect_error(rxModelVars("a = mix(a)"))
    expect_error(rxModelVars("a = mix(a, b)"))
    expect_error(rxModelVars("a = mix(a, b, c)"), NA)
    expect_error(rxModelVars("a = mix(a, b, c, d)"))
    expect_error(rxModelVars("a = mix(a, p1, c); c = mix(a, p1, c, p2, e)"))
    expect_error(rxModelVars("a = mix(a, p1, c); c = mix(a, p1, d)"), NA)
    expect_error(rxModelVars("mixest <- 1"))
    expect_error(rxModelVars("mixnum <- 1"))
    expect_error(rxModelVars("mixunif <- 1"))
  })

  test_that("good mix() in ui model", {
    f <- function() {
      ini({
        p1 <- 0.1
        p2 <- 0.2
        eta.a ~ 0.1
      })
      model({
        a <- mix(a1, p1, a2, p2, a3) + eta.a
        b <- mix(b1, b2, b3)
      })
    }

    expect_error(rxModelVars(f()), NA)

    f <- f()
    expect_equal(f$mixProbs, c("p1", "p2"))
  })

  test_that("mix() requires between subject variability", {
    f <- function() {
      ini({
        p1 <- 0.1
        p2 <- 0.2
      })
      model({
        a <- mix(a1, p1, a2, p2, a3)
        b <- mix(b1, b2, b3)
      })
    }
    expect_error(f())
  })

  test_that("mix() requires require probabilities in the ini block", {
    f <- function() {
      ini({
        a1 <- 0.1
        a2 <- 0.2
        eta.a1 ~ 0.1
      })
      model({
        a <- mix(a1, p1, a2, p2, a3) + eta.a1
        b <- mix(b1, b2, b3)
      })
    }

    expect_error(f())
  })

  test_that("mix() requires require probabilities to sum to a number between 0 and 1", {

    f <- function() {
      ini({
        p1 <- -10
        p2 <- 0.2
        eta.a1 ~ 0.1
      })
      model({
        a <- mix(a1, p1, a2, p2, a3) + eta.a1
        b <- mix(b1, b2, b3)
      })
    }

    expect_error(f())
  })

  test_that("mix() requires the same probabilities in each proportion", {

    f <- function() {
      ini({
        p1 <- 0.1
        p2 <- 0.2
        eta.a1 ~ 0.1
      })
      model({
        a <- mix(a1, p1, a2, p2, a3) + eta.a1
        b <- mix(b1, p2, b2, p1, b3)
      })
    }

    expect_error(f())

    f <- function() {
      ini({
        p1 <- 0.1
        p2 <- 0.2
        eta.a1 ~ 0.1
      })
      model({
        a <- mix(a1, p1, a2, p2, a3) + eta.a1
        b <- mix(b1, p1, b2, p2, b3)
      })
    }

    expect_error(f(), NA)

    f <- f()

    # Gets the probabilities
    expect_equal(f$mixProbs, c("p1", "p2"))

    # Throws error that this is a mixed model
    expect_error(assertRxUiNoMix(f))

    one.cmt <- function() {
      ini({
        ## You may label each parameter with a comment
        tka <- 0.45 # Log Ka
        tcl <- log(c(0, 2.7, 100)) # Log Cl
        ## This works with interactive models
        ## You may also label the preceding line with label("label text")
        tv <- 3.45; label("log V")
        ## the label("Label name") works with all models
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }

    expect_error(assertRxUiNoMix(one.cmt), NA)

  })

  test_that("test dsl to change mix()", {
    expect_equal(rxToSE("mix(a1, p1, b)"),
                 "(rxEq(mixest, 1)*(a1)+rxEq(mixest, 2)*(b))")

    expect_equal(rxToSE("mix(a1, p1, b, p2, c)"),
                 "(rxEq(mixest, 1)*(a1)+rxEq(mixest, 2)*(b)+rxEq(mixest, 3)*(c))")
  })

  test_that("mix() simulation", {
    rxWithSeed(42, {

      one.cmt <- function() {
        ini({
          ## You may label each parameter with a comment
          tka <- 0.45 # Log Ka
          tcl1 <- log(c(0, 2.7, 100)) # Log Cl
          tcl2 <- log(c(0, 0.1, 120)) # Log Cl
          ## This works with interactive models
          ## You may also label the preceding line with label("label text")
          tv <- 3.45; label("log V")
          p1 <- 0.3
          ## the label("Label name") works with all models
          eta.ka ~ 0.6
          eta.cl ~ 0.3
          eta.v ~ 0.1
          add.sd <- 0.7
        })
        model({
          ka <- exp(tka + eta.ka)
          cl <- mix(exp(tcl1 + eta.cl), p1, exp(tcl2 + eta.cl))
          v <- exp(tv + eta.v)
          me <- mixest
          mn <- mixnum
          mu <- mixunif
          linCmt() ~ add(add.sd)
        })
      }

      s <- rxSolve(one.cmt, et(amt=320, ii=12, addl=2, cmt=1) %>%
                              et(seq(0, 72)) %>%
                              et(id=1:20))

      expect_false(all(s$me == 1))
      expect_false(all(s$me == 2))

      expect_true(any(s$me == 1))
      expect_true(any(s$me == 2))
    })
  })

  test_that("mix() simulation, and the re-estimate with same mixest", {

    one.cmt <- function() {
      ini({
        ## You may label each parameter with a comment
        tka <- 0.45 # Log Ka
        tcl1 <- log(c(0, 2.7, 100)) # Log Cl
        tcl2 <- log(c(0, 0.1, 120)) # Log Cl
        ## This works with interactive models
        ## You may also label the preceding line with label("label text")
        tv <- 3.45; label("log V")
        p1 <- 0.3
        ## the label("Label name") works with all models
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- mix(exp(tcl1 + eta.cl), p1, exp(tcl2 + eta.cl))
        v <- exp(tv + eta.v)
        me <- mixest
        mu <- mixunif
        mn <- mixnum
        linCmt() ~ add(add.sd)
      })
    }

    s0 <- s <- rxSolve(one.cmt, et(amt=320, ii=12, addl=2, cmt=1) %>%
                                  et(seq(0, 72)) %>%
                                  et(id=1:20), addDosing=TRUE) %>%
      dplyr::rename(mixest=me, dv=sim) %>%
      dplyr::select(id, mixest, evid, cmt, amt, time, dv, mu)

    trn <- etTrans(s, one.cmt)
    lst <- attr(class(trn), ".rxode2.lst")
    class(lst) <- NULL
    expect_true(all(lst$mixUnif >= 1))
    expect_true(length(lst$mixUnif) == length(lst$idLvl))
    expect_length(lst$covParPos, 0)
    expect_length(lst$covParPos0, 0)

    s2 <- s
    s2$mixest <- NULL

    trn <- etTrans(s2, one.cmt)
    lst2 <- attr(class(trn), ".rxode2.lst")
    class(lst2) <- NULL

    for (n in names(lst)) {
      if (n %in% c("mixUnif", "lib_name")) next
      expect_equal(lst[[n]], lst2[[n]], info=n)
    }

    # Now error with mixtures above nmix in the model
    s$mixest[s$id == 1] <- 3
    expect_error(etTrans(s, one.cmt))

    ## mixest below 1
    s$mixest[s$id == 1] <- 0
    expect_error(etTrans(s, one.cmt))

    # non integer mixest
    s$mixest[s$id == 1] <- 0.5
    expect_error(etTrans(s, one.cmt))

    # Time varying mixest
    s$mixest <- seq_along(s$mixest) %% 2 + 1

    expect_error(etTrans(s, one.cmt))

    expect_error(rxSolve(one.cmt, s0, addDosing=TRUE, nStud=100))

    s2 <- rxSolve(one.cmt, s0, addDosing=TRUE) %>%
      dplyr::rename(mixest=me, dv=sim) %>%
      dplyr::select(id, mixest, evid, cmt, amt, time, dv, mu)

    # keeps all the mixest
    expect_true(all(s2$mixest == s0$mixest))

    # mixunif is not simulated, so the values are not the same
    expect_false(all(s2$mu == s0$mu))


    s0 <- rxSolve(one.cmt, et(amt=320, ii=12, addl=2, cmt=1) %>%
                             et(seq(0, 72)) %>%
                             et(id=1:20), addDosing=TRUE) %>%
      dplyr::rename(mixunif=mu, dv=sim) %>%
      dplyr::select(id, mixunif, evid, cmt, amt, time, dv, me)

    s2 <- rxSolve(one.cmt, s0, addDosing=TRUE) %>%
      dplyr::rename(mixunif=mu, dv=sim) %>%
      dplyr::select(id, mixunif, evid, cmt, amt, time, dv, me)

    # keeps all the mixest
    expect_true(all(s2$me == s0$me))

    # mixunif is not simulated, so the values are not the same
    expect_true(all(s2$mixunif == s0$mixunif))


    s0 <- rxSolve(one.cmt, et(amt=320, ii=12, addl=2, cmt=1) %>%
                             et(seq(0, 72)) %>%
                             et(id=1:20), addDosing=TRUE) %>%
      dplyr::rename(mixunif=mu, mixest=me, dv=sim) %>%
      dplyr::select(id, mixunif, evid, cmt, amt, time, dv, mixest)

    expect_error(rxSolve(one.cmt, s0, addDosing=TRUE))

  })

  test_that("test mixture models load with rxS()", {

 expect_error(rxS("tka=THETA[1];\ntcl1=THETA[2];\ntcl2=THETA[3];\ntv=THETA[4];\np1=THETA[5];\nadd.sd=THETA[6];\neta.ka=ETA[1];\neta.cl=ETA[2];\neta.v=ETA[3];\nka=exp(tka+eta.ka);\ncl=mix(exp(tcl1+eta.cl),p1,exp(tcl2+eta.cl));\nv=exp(tv+eta.v);\nme=mixest;\nmn=mixnum;\nmu=mixunif;\nrx_yj_~2;\nrx_lambda_~1;\nrx_low_~0;\nrx_hi_~1;\nrx_pred_f_~linCmtA(rx__PTR__,t,2,1,1,-1,1,cl,v,0.0,0.0,0.0,0.0,ka);\nrx_pred_~rx_pred_f_;\nrx_r_~(add.sd)^2;\n"),
                 NA)


  })
})
