rxTest({
  test_that("proper errors from event tables with single id", {

    m <- function() {
      ini({
        tCL20 <-  c(10)
        tV1 <- 20
        tCL12 <- 30
        tV2 <- 40
        tCL23 <- 50
        tCL32 <- 60
        betaWTCL20 <- 0.1
        betaSexV1 <- 0.2
        prop.err <- c(0, 0.268724820805969)
        eta.V1 + eta.CL12 + eta.V2 + eta.CL20 + eta.CL23 + eta.CL32 ~
          cor(sd(1,
                 0.25, 1.10,
                 -0.25, 0.25, 1.2,
                 -0.35, 0.35, -0.45, 1.3,
                 -0.2,  0.2,  0.2,   -0.2, 1.4,
                 -0.1,  0.1, -0.1, 0.1, -0.1, 1.4))
      })
      model({
        CL20 <- exp(tCL20 + eta.CL20) * ((WTBL/85)^betaWTCL20)
        V1 <- exp(tV1 + eta.V1) * (1 + betaSexV1 * SEXN)
        CL12 <- exp(tCL12 + eta.CL12)
        V2 <- exp(tV2 + eta.V2)
        CL23 <- exp(tCL23 + eta.CL23)
        CL32 <- exp(tCL32 + eta.CL32)
        k12 <- CL12/V1
        k20 <- CL20/V2
        k23 <- CL23/V2
        k32 <- CL32/V1
        d/dt(central) = -k12 * central
        d/dt(tissue) = k12 * central - (k20 + k23) * tissue +    k32 * central_ext
        d/dt(central_ext) = k23 * tissue - k32 * central_ext
        d/dt(central_tot) = (central + central_ext)/V1
        conc = (central + central_ext)/V1
        IPRED = conc
        IPRED ~ prop(prop.err)
      })
    }


    ev <- eventTable() |>
      add.dosing(dose=900630, nbr.doses=1, rate=900630/2) |>
      et(0, 600, length.out=10)

    expect_error(rxSolve(m, ev,

                 # Create individual covariate data-frame
                 iCov=data.frame(ID=1:10,
                                 WTBL=runif(n = 1000, min = 50, max = 150),
                                 SEXN=rbinom(n = 1000, size = 1, prob = 0.5)),
                 # in this case it would be useful to keep the WT in the output dataset
                 keep=c("WTBL","SEXN")),
                 "to use 'iCov' you must have an id in your event table")
  })


  test_that("iCov", {

    nsub <- 10 # 100 sub-problems

    mod1 <- rxode2({
      C2 = centr/V2
      C3 = peri/V3
      d/dt(depot) = -KA*depot
      d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3
      d/dt(peri) = Q*C2 - Q*C3
      d/dt(eff) = Kin - Kout*(1-C2/(EC50+C2))*eff
      eff(0) = 1
    })


    ev <- et() |>
      et(amt=10000, addl=9,ii=12) |>
      et(time=120, amt=20000, addl=4, ii=24) |>
      et(0:240) |> ## Add Sampling
      et(id=1:nsub) ## Add id

    sigma <- matrix(c(0.09,0.08,0.08,0.25),2,2) # IIV covariance matrix
    mv <- rxRmvn(n=nsub, rep(0,2), sigma) # Sample from covariance matrix
    CL <- 7*exp(mv[,1])
    V2 <- 40*exp(mv[,2])
    params.all <- cbind(KA=0.3, CL=CL, V2=V2, Q=10, V3=300,
                        Kin=0.2, Kout=0.2, EC50=8)
    paramsDf <- as.data.frame(params.all)
    paramsDf$ptS=paste(1:nsub, "pt")
    paramsDf$ptF <- factor(paramsDf$ptS)
    paramsDf$ptI <- 1:nsub
    paramsDf$ptL <- paramsDf$ptI <=50

    expect_error(etTrans(ev, mod1, iCov=paramsDf, keep=c("CL", "V2")),
                 "'iCov' must have an 'id' column")

    paramsDf$id <- 1:nsub+1
    expect_error(etTrans(ev, mod1, iCov=paramsDf, keep=c("CL", "V2")),
                 "some of the 'id' values do not match the input event table")


    paramsDf$id <- 1:nsub

    paramsDf2 <- paramsDf

    paramsDf2$id <- paste("pt ",paramsDf2$id)

    expect_error(etTrans(ev, mod1, iCov=paramsDf2),
                 "data 'id' column is an integer; 'iCov' 'id' also needs to be an integer")

    paramsDf2 <- paramsDf[-1,]

    expect_error(etTrans(ev, mod1, iCov=paramsDf2),
                 "the 'id' in the iCov must have 1 unique match to the event table")

    paramsDf2 <- rbind(paramsDf[1,], paramsDf)

    expect_error(etTrans(ev, mod1, iCov=paramsDf2),
                 "the 'id' in the iCov must have 1 unique match to the event table")

    # Now check for proper behavior

    tmp <- etTrans(ev, mod1, iCov=paramsDf)
    tmp2 <- attr(class(tmp), ".rxode2.lst")
    class(tmp2) <- NULL

    for (v in c("EC50", "Kout", "Kin", "V3", "Q", "V2", "CL", "KA")) {
      expect_equal(tmp2$cov1[[v]], paramsDf[[v]])
      expect_equal(tmp2$pars[v,], paramsDf[[v]])
    }

    # Check keep of various sorts in iCov
    tmp <- rxSolve(mod1, ev, iCov=paramsDf, keep=c("CL", "V2", "ptS", "ptF", "ptI", "ptL"))
    expect_true(is.character(tmp$ptS))
    expect_true(all(paste(1:nsub, "pt") %fin% unique(tmp$ptS)))
    expect_true(is.factor(tmp$ptF))
    expect_true(all(paste(1:nsub, "pt") %fin% unique(tmp$ptF)))
    expect_true(is.integer(tmp$ptI))
    expect_true(is.logical(tmp$ptL))
    expect_true(all((tmp$ptI <= 50) == tmp$ptL))

    for (v in
         c("evid",  "time", "amt", "value", "cmt", "ytype", "state", "var", "dv",
           "rate","dur", "addl", "ii", "mdv", "cens", "limit", "method")) {
      names(paramsDf)[9] <- v
      expect_error(etTrans(ev, mod1, iCov=paramsDf),
                   paste0("cannot specify '", v,"' in 'iCov'"))
    }

  })
})
