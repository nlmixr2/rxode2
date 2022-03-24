.rx <- loadNamespace("rxode2")

test_that("comments are parsed correctly", {
  
  cmt <- c("function() {", "      ini({", "        ## You may label each parameter with a comment",
           "        tka <- 0.45 # Log Ka", "        tcl <- log(c(0, 2.7, 100)) # Log Cl",
           "        ## This works with interactive models", "        ## You may also label the preceding line with label(\"label text\")",
           "        tv <- 3.45; label(\"log V\")", "        ## the label(\"Label name\") works with all models",
           "        eta.ka ~ 0.6", "        eta.cl ~ 0.3", "        eta.v ~ 0.1",
           "        add.sd <- 0.7", "      })", "      model({", "        ka <- exp(tka + eta.ka)",
           "        cl <- exp(tcl + eta.cl)", "        v <- exp(tv + eta.v)",
           "        linCmt() ~ add(add.sd)", "      })", "    }")
  
  eq <- c("function () ", "{", "    ini({", "        tka <- 0.45", "        label(\"Log Ka\")",
          "        tcl <- log(c(0, 2.7, 100))", "        label(\"Log Cl\")",
          "        tv <- 3.45", "        label(\"log V\")", "        eta.ka ~ 0.6",
          "        eta.cl ~ 0.3", "        eta.v ~ 0.1", "        add.sd <- 0.7",
          "    })", "    model({", "        ka <- exp(tka + eta.ka)", "        cl <- exp(tcl + eta.cl)",
          "        v <- exp(tv + eta.v)", "        linCmt() ~ add(add.sd)",
          "    })", "}")
  
  expect_message(
    expect_equal(.rx$.rxReplaceCommentWithLabel(cmt), eq),
    regexp="parameter labels from comments will be replaced by 'label()'", fixed=TRUE
  )
  
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
  
  expect_message(
    str <- .rx$.rxFunction2string(one.cmt),
    regexp="parameter labels from comments will be replaced by 'label()'", fixed=TRUE
  )
  
  if (!is.null(attr(one.cmt, "srcref"))) {
    expect_equal(str, eq)
    attr(one.cmt, "srcref") <- NULL
  }
  
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
      linCmt() ~ add(add.sd) | tmp
    })
  }
  
  expect_message(
    mkstr <- .rx$.rxFunction2string(one.cmt),
    regexp="parameter labels from comments will be replaced by 'label()'", fixed=TRUE
  )
  
  expect_equal(mkstr,
               c("function () ", "{", "    ini({", "        tka <- 0.45", "        label(\"Log Ka\")",
                 "        tcl <- log(c(0, 2.7, 100))", "        label(\"Log Cl\")",
                 "        tv <- 3.45", "        label(\"log V\")", "        eta.ka ~ 0.6",
                 "        eta.cl ~ 0.3", "        eta.v ~ 0.1", "        add.sd <- 0.7",
                 "    })", "    model({", "        ka <- exp(tka + eta.ka)", "        cl <- exp(tcl + eta.cl)",
                 "        v <- exp(tv + eta.v)", "        linCmt() ~ add(add.sd) | tmp",
                 "    })", "}"))
  
})

test_that("meta information parsing", {
  one.cmt <- function() {
    meta1 <- "meta"
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
    meta2 <- "meta2"
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd)
    })
  }
  
  tmp1 <- one.cmt()
  
  expect_equal(tmp1$meta$meta1, "meta")
  expect_equal(tmp1$meta$meta2, "meta2")
  
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
      lambda <- c(-2, 1, 2)
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd) + boxCox(lambda) | tmp
    })
  }
  
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
      lambda <- 3 + v
      linCmt() ~ add(add.sd) + boxCox(lambda) | tmp
    })
  }
  
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
      linCmt() ~ lnorm(add.sd) | tmp
    })
  }
  
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
      bLambda <- c(0, 3)
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ lnorm(add.sd) | tmp
      tmp2 ~ dpois(bLambda)
    })
  }
  
  cov <- function() {
    ini({
      ## You may label each parameter with a comment
      tka <- 0.45 # Log Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      tvp <- 3.45; label("log V")
      cl.wt <- 0.1
      v.wt <- 0.1
      cl.sex <- 0.1
      v.sex <- 0.1
      cl.age <- 0.1
      v.age <- 0.1
      vp.wt <- 1
      vp.sex <- 1
      vp.age <- 1
      ## the label("Label name") works with all models
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
      v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
      vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl/v * center
      cp = center/v
      cp ~ add(add.sd)
    })
  }
  
  pk.turnover.emax <- function() {
    ini({
      tktr <- log(1)
      tka <- log(1)
      tcl <- log(0.1)
      tv <- log(10)
      ##
      eta.ktr ~ 1
      eta.ka ~ 1
      eta.cl ~ 2
      eta.v ~ 1
      prop.err <- 0.1
      pkadd.err <- 0.1
      ##
      temax <- logit(0.8)
      #temax <- 7.5
      tec50 <- log(0.5)
      tkout <- log(0.05)
      te0 <- log(100)
      ##
      eta.emax ~ .5
      eta.ec50  ~ .5
      eta.kout ~ .5
      eta.e0 ~ .5
      ##
      pdadd.err <- 10
    })
    model({
      ktr <- exp(tktr + eta.ktr)
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      ##
      #poplogit = log(temax/(1-temax))
      emax=expit(temax+eta.emax)
      #logit=temax+eta.emax
      ec50 =  exp(tec50 + eta.ec50)
      kout = exp(tkout + eta.kout)
      e0 = exp(te0 + eta.e0)
      ##
      DCP = center/v
      PD=1-emax*DCP/(ec50+DCP)
      ##
      effect(0) = e0
      kin = e0*kout
      ##
      d/dt(depot) = -ktr * depot
      d/dt(gut) =  ktr * depot -ka * gut
      d/dt(center) =  ka * gut - cl / v * center
      d/dt(effect) = kin*PD -kout*effect
      ##
      cp = center / v
      cp ~ prop(prop.err) + add(pkadd.err)
      effect ~ add(pdadd.err)
    })
  }
  
  turnover.emax.noeta <- function() {
    ini({
      tktr <- log(1)
      tka <- log(1)
      tcl <- log(0.1)
      tv <- log(10)
      ##
      prop.err <- 0.1
      pkadd.err <- 0.1
      ##
      temax <- logit(0.8)
      #temax <- 7.5
      tec50 <- log(0.5)
      tkout <- log(0.05)
      te0 <- log(100)
      ##
      pdadd.err <- 10
    })
    model({
      ktr <- exp(tktr)
      ka <- exp(tka)
      cl <- exp(tcl)
      v <- exp(tv)
      ##
      #poplogit = log(temax/(1-temax))
      emax=expit(temax)
      ec50 =  exp(tec50)
      kout = exp(tkout)
      e0 = exp(te0)
      ##
      DCP = center/v
      PD=1-emax*DCP/(ec50+DCP)
      ##
      effect(0) = e0
      kin = e0*kout
      ##
      d/dt(depot) = -ktr * depot
      d/dt(gut) =  ktr * depot -ka * gut
      d/dt(center) =  ka * gut - cl / v * center
      d/dt(effect) = kin*PD -kout*effect
      ##
      cp = center / v
      cp ~ prop(prop.err) + add(pkadd.err)
      effect ~ add(pdadd.err)
    })
  }
  
  f <- function() {
    ini({
      lKA <- log(0.294)
      CL <- 18.6
      V2 <- 40.2
      Q <- 10.5
      V3 <- 297
      Kin <- 1
      Kout <- 1
      EC50 <- 200
      eta.ka ~ 0.12
      prop.sd ~ 0.2
    })
    model({
      KA <- exp(lKA + eta.ka)
      C2 <- centr/V2
      C3 <- peri/V3
      d/dt(depot) <- -KA*depot
      d/dt(centr) <- KA*depot - CL*C2 - Q*C2 + Q*C3
      d/dt(peri) <- Q*C2 - Q*C3
      d/dt(eff) <- Kin - Kout*(1-C2/(EC50+C2))*eff
      eff(0) <- 1
      C2 ~ prop(prop.sd)
    })
  }
  
  expect_error(f(), "prop.sd")
  
  one.cmt <- function() {
    ini({
      ## You may label each parameter with a comment
      tka <- 0.45 # Log Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      ## the label("Label name") works with all models
      eta.ka + eta.cl ~ c(0.6,
                          0.001, 0.3)
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd) | tmp
      vv ~ add(add.sd)
    })
  }
  
  expect_error(one.cmt())
})
