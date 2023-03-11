rxTest({ # mostly tested in 'rxode2et'
  test_that("warfarin model", {
    warfarin <- nlmixr2data::warfarin

    mod <- rxode2({
      lka <- log(0.1) # log Ka
      lv <- log(10) # Log Vc
      lcl <- log(4) # Log Cl
      lq <- log(10) # log Q
      lvp <- log(20) # Log Vp
      eta.ka <- 0
      eta.v <- 0.1
      eta.cl <- 0.1
      ka <- exp(lka + eta.ka + sex + age + dvid)
      cl <- exp(lcl + eta.cl)
      v <- exp(lv + eta.v)
      q <- exp(lq)
      vp <- exp(lvp)
      sf <- (sex == "female")
      sm <- (sex == "male")
      d.cp <- (dvid == "cp")
      d.pca <- (dvid == "pca")
      cp <- linCmt()
    })

    t <- rxSolve(mod, warfarin, keep = c("sex", "age", "dvid"))

    expect_equal(sort(unique(t$sf)), c(0, 1))
    expect_equal(sort(unique(t$sm)), c(0, 1))
    expect_equal(sort(unique(t$d.cp)), c(0, 1))
    expect_equal(sort(unique(t$d.pca)), c(0, 1))

    expect_s3_class(t$sex, "factor")
    expect_s3_class(t$dvid, "factor")

    expect_equal(as.double((t$sex == "male") * 1), t$sm)
    expect_equal(as.double((t$sex == "female") * 1), t$sf)

    t <- rxSolve(mod, warfarin, addCov = TRUE)

    expect_equal(as.double((t$sex == "male") * 1), t$sm)
    expect_equal(as.double((t$sex == "female") * 1), t$sf)
    ## expect_equal(as.double((t$dvid == "cp") * 1), t$d.cp)
    ## expect_equal(as.double((t$pca == "pca") * 1), t$d.pca)

    warfarin$sex <- paste(warfarin$sex)

    t <- rxSolve(mod, warfarin, keep = c("sex", "age", "dvid"))

    expect_equal(sort(unique(t$sf)), c(0, 1))
    expect_equal(sort(unique(t$sm)), c(0, 1))
    expect_equal(sort(unique(t$d.cp)), c(0, 1))
    expect_equal(sort(unique(t$d.pca)), c(0, 1))

    expect_s3_class(t$sex, "factor")
    expect_s3_class(t$dvid, "factor")

    expect_equal(as.double((t$sex == "male") * 1), t$sm)
    expect_equal(as.double((t$sex == "female") * 1), t$sf)

    t <- rxSolve(mod, warfarin, addCov = TRUE)

    expect_equal(as.double((t$sex == "male") * 1), t$sm)
    expect_equal(as.double((t$sex == "female") * 1), t$sf)
  })
})
