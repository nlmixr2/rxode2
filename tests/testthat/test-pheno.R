test_that("label replacement", {
  .pheno <- c("function() {", "  # Pheno with covariance", "  ini({", "    tcl <- log(0.008) # typical value of clearance", "    tv <-  log(0.6)   # typical value of volume", "    ## var(eta.cl)", "    eta.cl + eta.v ~ c(1,", "                       0.01, 1) ## cov(eta.cl, eta.v), var(eta.v)", "    # interindividual variability on clearance and volume", "    add.err <- 0.1    # residual variability", "  })", "  model({", "    cl <- exp(tcl + eta.cl) # individual value of clearance", "    v <- exp(tv + eta.v)    # individual value of volume",  "    ke <- cl / v            # elimination rate constant", "    d/dt(A1) = - ke * A1    # model differential equation", "    cp = A1 / v             # concentration in plasma", "    cp ~ add(add.err)       # define error model", "  })", "}")
  suppressMessages(
    expect_error(.rxReplaceCommentWithLabel(.pheno), NA)
  )
})
