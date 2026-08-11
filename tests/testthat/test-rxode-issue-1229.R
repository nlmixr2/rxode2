rxTest({
  # rxode2#1229: `%%` was missing from the infix tables of .rxPrune() and
  # rxOptExpr(), so it was deparsed as the prefix call `%%(a, b)`, which is not
  # parsable rxode2.  This blocked every nlmixr2 estimation method, since they
  # all run the prune/optimize stages.

  test_that(".rxPrune() keeps `%%` infix", {
    m <- rxGetModel(quote({
      fl <- 0
      if ((time %% 24) > 12) {
        fl <- 1
      }
      d/dt(A) <- -ka*A*(1 + fl)
    }))
    .p <- rxPrune(m)
    expect_false(any(grepl("%%(", .p, fixed = TRUE)))
    expect_equal(.p,
                 paste("fl=0",
                       "fl=((t%%24)>12)*(1)+(1-(((t%%24)>12)))*(fl)",
                       "d/dt(A)=-ka*A*(1+fl)", sep = "\n"))
    # the whole point: the pruned model is still rxode2
    expect_error(rxGetModel(paste(.p, collapse = "\n")), NA)
  })

  test_that("rxOptExpr() keeps `%%` infix", {
    expect_equal(suppressMessages(rxOptExpr("fl=1*((t%%24)>12)\nkFI=exp(a*(1-fl)+b*fl)\nd/dt(A)=kFI\n")),
                 "fl=((t%%24)>12)\nkFI=exp(a*(1-fl)+b*fl)\nd/dt(A)=kFI")
  })

  test_that("non-primary `%%` operands are parenthesized", {
    # the grammar wants a primary expression on either side of `%%`, so
    # anything else has to be wrapped
    .chk <- function(txt) {
      .p <- rxPrune(rxGetModel(eval(parse(text = paste0("quote({", txt, "})")))))
      expect_error(rxGetModel(paste(.p, collapse = "\n")), NA)
      .o <- suppressMessages(rxOptExpr(txt))
      expect_error(rxGetModel(.o), NA)
    }
    .chk("fl<-(time+3)%%(24*2)\nd/dt(A)<--ka*A*fl\n")
    .chk("fl<-exp(a)%%exp(b)\nd/dt(A)<--ka*A*fl\n")
    .chk("fl<--time%%24\nd/dt(A)<--ka*A*fl\n")
    .chk("fl<-2*time%%24\nd/dt(A)<--ka*A*fl\n")
    .chk("fl<-time%%24*2\nd/dt(A)<--ka*A*fl\n")
  })

  test_that("a `%%` model builds its symengine estimation model", {
    one <- function() {
      ini({
        tka <- 0.45
        tcl <- -3.2
        tv <- -1
        eta.ka ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl)
        v <- exp(tv)
        fl <- 1.0*((time %% 24) > 12)
        d/dt(depot) <- -ka*depot
        d/dt(center) <- ka*depot - cl/v*center*(1 + fl)
        cp <- center/v
        cp ~ add(add.sd)
      })
    }
    ui <- one()
    expect_error(rxUiGet.symengineModelPrune(list(ui, TRUE)), NA)
    expect_error(rxS("fl=1*((t%%24)>12)\nd/dt(A)=-ka*A*(1+fl)\n", TRUE), NA)
  })
})
