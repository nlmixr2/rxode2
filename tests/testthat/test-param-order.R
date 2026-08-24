rxTest({
  test_that("param order", {
    mod <- rxode2({
      a <- 6
      b <- 0.6
      cmt(blood) # cmt = 1 now
      d / dt(intestine) <- -a * intestine
      d / dt(blood) <- a * intestine - b * blood
    })

    expect_equal(rxModelVars(mod)$param, c("a", "b"))
  })

  test_that("param order rev", {
    mod2 <- rxode2({
      param(b, a)
      a <- 6
      b <- 0.6
      cmt(blood) # cmt = 1 now
      d / dt(intestine) <- -a * intestine
      d / dt(blood) <- a * intestine - b * blood
    })

    expect_equal(rxModelVars(mod2)$param, c("b", "a"))
  })

  test_that("large params()", {
    expect_error(tmp <- rxode2("param(tktr,tka,tcl,tv,poplogit,tec50,tkout,te0)
cmt(depot)
cmt(gut)
cmt(center)
cmt(effect)
effect(0)=exp(te0)
rx_expr_3~exp(tktr)
d/dt(depot)=-rx_expr_3*depot
rx_expr_1~exp(tka)
d/dt(gut)=-rx_expr_1*gut+rx_expr_3*depot
d/dt(center)=rx_expr_1*gut-exp(tcl-tv)*center
rx_expr_2~exp(-tv)
rx_expr_4~rx_expr_2*center
d/dt(effect)=-exp(tkout)*effect+exp(te0+tkout)*(1-exp(poplogit-tv)*center/((1+exp(poplogit))*(rx_expr_4+exp(tec50))))
rx_expr_0~CMT==6
rx_pred_=effect*(rx_expr_0)+rx_expr_4*(CMT==5)*(1-(rx_expr_0))
cmt(cp)
cmt(pca)
dvid(5, 6)"), NA)

    expect_equal(rxModelVars(tmp)$param, c(
      "tktr", "tka", "tcl", "tv", "poplogit", "tec50", "tkout", "te0",
      "CMT"
    ))
  })
  # nlmixr2/rxode2#1279
  test_that("param()/interp statements do not splice in the prior line", {
    .m <- rxModelVars("d/dt(x)=-a*x;\nparam(a,b);\ny=x*b;\n")
    expect_equal(rxNorm(.m), "d/dt(x)=-a*x;\nparam(a,b);\ny=x*b;\n")
    expect_equal(.m$params, c("a", "b"))
    # the normalized text has to parse back to the same model
    expect_equal(rxModelVars(rxNorm(.m))$params, .m$params)

    .i <- rxModelVars("y=z*a;\nlocf(z);\n")
    expect_equal(rxNorm(.i), "y=z*a;\nlocf(z);\n")
    expect_equal(rxModelVars(rxNorm(.i))$params, .i$params)
  })

  test_that("repeated param() statements merge into one", {
    .two <- rxModelVars(paste0("param(THETA[1],THETA[2],ETA[1]);\ncmt(centr);\n",
                               "param(THETA[1],THETA[2],ETA[1],DV);\n",
                               "d/dt(centr)=-exp(THETA[1]+ETA[1])*centr;\n",
                               "rx_pred_=llikNorm(DV,centr,exp(THETA[2]));\nrx_r_=0;\n"))
    .one <- rxModelVars(paste0("param(THETA[1],THETA[2],ETA[1],DV);\ncmt(centr);\n",
                               "d/dt(centr)=-exp(THETA[1]+ETA[1])*centr;\n",
                               "rx_pred_=llikNorm(DV,centr,exp(THETA[2]));\nrx_r_=0;\n"))
    expect_equal(.two$params, c("THETA[1]", "THETA[2]", "ETA[1]", "DV"))
    expect_equal(.two$params, .one$params)
    # a single param() statement in the normalized model, matching $params
    expect_equal(sum(grepl("^param\\(", strsplit(rxNorm(.two), "\n")[[1]])), 1L)
    expect_equal(rxNorm(.two), rxNorm(.one))

    # a parameter introduced by use between the two statements keeps its place
    .mid <- rxModelVars("param(a);\ny=z*a;\nparam(a,c);\nw=c;\n")
    expect_equal(.mid$params, c("a", "z", "c"))
    expect_equal(rxNorm(.mid), "param(a,z,c);\ny=z*a;\nw=c;\n")
    expect_equal(rxModelVars(rxNorm(.mid))$params, .mid$params)

    # every declared name became a state, so nothing is left to declare
    expect_equal(rxNorm(rxModelVars("param(a);\nparam(a);\nd/dt(a)=-a;\n")),
                 "d/dt(a)=-a;\n")

    # a single param() statement is left alone
    expect_equal(rxNorm(rxModelVars("param(a,b);\nd/dt(x)=-a*x*b;\n")),
                 "param(a,b);\nd/dt(x)=-a*x*b;\n")
  })

  test_that("a parameter declared by a later param() is filled at solve time", {
    .m <- rxode2("param(a,b);\ncmt(centr);\nparam(a,b,DV);\nd/dt(centr)=-a*centr;\ny=centr*b+DV;\n")
    .d <- as.data.frame(et(et(amt = 100, cmt = "centr"), 1:3))
    .d$DV <- 7
    .s <- rxSolve(.m, .d, params = c(a = 0.1, b = 1))
    expect_equal(.s$y, .s$centr + 7)
  })
})
