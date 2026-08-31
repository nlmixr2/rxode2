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

    for (.f in c("locf", "linear", "nocb", "midpoint")) {
      .txt <- paste0("y=z*a;\n", .f, "(z);\n")
      .i <- rxModelVars(.txt)
      expect_equal(rxNorm(.i), .txt)
      expect_equal(rxModelVars(rxNorm(.i))$params, .i$params)
    }
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

    # a parameter introduced by use before the first statement keeps its place
    .pre <- rxModelVars("y=z*a;\nparam(a,b);\nparam(a,b,c);\nw=b*c;\n")
    expect_equal(.pre$params, c("z", "a", "b", "c"))
    expect_equal(rxNorm(.pre), "y=z*a;\nparam(z,a,b,c);\nw=b*c;\n")
    expect_equal(rxModelVars(rxNorm(.pre))$params, .pre$params)

    # a parameter introduced by use between the two statements keeps its place
    .mid <- rxModelVars("param(a);\ny=z*a;\nparam(a,c);\nw=c;\n")
    expect_equal(.mid$params, c("a", "z", "c"))
    expect_equal(rxNorm(.mid), "param(a,z,c);\ny=z*a;\nw=c;\n")
    expect_equal(rxModelVars(rxNorm(.mid))$params, .mid$params)

    # the first statement declares only names that became states, so the merged
    # statement must still keep the parameters introduced ahead of it in place
    .st <- rxModelVars("param(b);\ny=c;\nparam(d);\nd/dt(b)=-b;\n")
    expect_equal(.st$params, c("c", "d"))
    expect_equal(rxNorm(.st), "param(c,d);\ny=c;\nd/dt(b)=-b;\n")
    expect_equal(rxModelVars(rxNorm(.st))$params, .st$params)

    # every declared name became a state, but the model still has an implicit
    # parameter: the statements go, the parameter stays
    .dropAll <- rxModelVars("param(a);\nd/dt(a)=-a;\ny=c;\nparam(b);\nd/dt(b)=-b;\n")
    expect_equal(.dropAll$params, "c")
    expect_equal(rxNorm(.dropAll), "d/dt(a)=-a;\ny=c;\nd/dt(b)=-b;\n")
    expect_equal(rxModelVars(rxNorm(.dropAll))$params, .dropAll$params)

    # every declared name became a state, so nothing is left to declare
    expect_equal(rxNorm(rxModelVars("param(a);\nparam(a);\nd/dt(a)=-a;\n")),
                 "d/dt(a)=-a;\n")

    # an interpolation set on a parameter pulled into the merged statement stays
    .int <- rxModelVars("param(a);\nlocf(z);\ny=z*a;\nparam(a,c);\nw=c;\n")
    expect_equal(.int$params, c("a", "z", "c"))
    expect_equal(rxNorm(.int), "param(a,z,c);\nlocf(z);\ny=z*a;\nw=c;\n")
    expect_equal(rxModelVars(rxNorm(.int))$interp, .int$interp)
    expect_equal(as.character(.int$interp[["z"]]), "locf")

    # a string covariate pulled into the merged statement keeps its levels
    .lvl <- rxModelVars(paste0("param(a);\nif (SEX == \"male\") {\n b <- 1\n} else {\n b <- 2\n}\n",
                               "param(a,c);\ny=a*b*c;\n"))
    expect_true("SEX" %in% .lvl$params)
    expect_equal(rxModelVars(rxNorm(.lvl))$params, .lvl$params)
    expect_equal(rxModelVars(rxNorm(.lvl))$strCmpParams, .lvl$strCmpParams)
    expect_equal(rxModelVars(rxNorm(.lvl))$interp, .lvl$interp)

    # a parameter introduced only after the last statement keeps its place
    .post <- rxModelVars("param(a);\nparam(b);\ny=c;\n")
    expect_equal(.post$params, c("a", "b", "c"))
    expect_equal(rxNorm(.post), "param(a,b);\ny=c;\n")
    expect_equal(rxModelVars(rxNorm(.post))$params, .post$params)

    # a dual lhs/parameter assigned before the first statement is merged in
    .dual <- rxModelVars("param(q);\ny=1;\nparam(y,a);\n")
    expect_equal(.dual$params, c("q", "y", "a"))
    expect_equal(rxNorm(.dual), "param(q,y,a);\ny=1;\n")
    expect_equal(rxModelVars(rxNorm(.dual))$params, .dual$params)
    expect_equal(unclass(rxModelVars(rxNorm(.dual))$interp), unclass(.dual$interp))

    # the `params()` spelling merges the same way
    .alias <- rxModelVars("params(a, b);\nparams(c, d);\ny=a*b*c*d;\n")
    expect_equal(.alias$params, c("a", "b", "c", "d"))
    expect_equal(rxNorm(.alias), "param(a,b,c,d);\ny=a*b*c*d;\n")
    expect_equal(rxModelVars(rxNorm(.alias))$params, .alias$params)

    # the first statement declares nothing but states, the second survives
    .st2 <- rxModelVars("param(a,b);\nd/dt(a)=-a;\nd/dt(b)=-b;\nparam(c);\ny=c;\n")
    expect_equal(.st2$params, "c")
    expect_equal(rxNorm(.st2), "param(c);\nd/dt(a)=-a;\nd/dt(b)=-b;\ny=c;\n")
    expect_equal(rxModelVars(rxNorm(.st2))$params, .st2$params)

    # an interpolation statement sitting between the two is kept in place
    .int2 <- rxModelVars("param(a);\nlocf(b);\nparam(b);\ny=a*b;\n")
    expect_equal(.int2$params, c("a", "b"))
    expect_equal(rxNorm(.int2), "param(a,b);\nlocf(b);\ny=a*b;\n")
    expect_equal(unclass(rxModelVars(rxNorm(.int2))$interp), unclass(.int2$interp))

    # a single param() statement is left alone
    expect_equal(rxNorm(rxModelVars("param(a,b);\nd/dt(x)=-a*x*b;\n")),
                 "param(a,b);\nd/dt(x)=-a*x*b;\n")
  })

  test_that("a dual lhs/parameter gets its interpolation set", {
    # `a` is declared by param() and assigned, so it is both an lhs and a
    # parameter; the interp vector is allocated uninitialized, so the slot has
    # to be written on that path too
    .m <- rxModelVars("param(a,n);\nlocf(n);\na=a+1;\ny=a*n;\n")
    expect_equal(.m$params, c("a", "n"))
    expect_equal(as.character(.m$interp), c("default", "locf"))
    expect_equal(unclass(rxModelVars(rxNorm(.m))$interp), unclass(.m$interp))
  })

  test_that("a parameter declared by a later param() is filled at solve time", {
    .m <- rxode2("param(a,b);\ncmt(centr);\nparam(a,b,DV);\nd/dt(centr)=-a*centr;\ny=centr*b+DV;\n")
    .d <- as.data.frame(et(et(amt = 100, cmt = "centr"), 1:3))
    .d$DV <- 7
    .s <- rxSolve(.m, .d, params = c(a = 0.1, b = 1))
    expect_equal(.s$y, .s$centr + 7)
  })
})
