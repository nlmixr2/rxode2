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
