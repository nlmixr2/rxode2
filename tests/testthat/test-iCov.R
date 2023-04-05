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


    ev <- eventTable() %>%
      add.dosing(dose=900630, nbr.doses=1, rate=900630/2) %>%
      et(0, 600, length.out=10)

    expect_error(rxSolve(m, ev,

                 # Create individual covariate data-frame
                 iCov=data.frame(ID=1:10,
                                 WTBL=runif(n = 1000, min = 50, max = 150),
                                 SEXN=rbinom(n = 1000, size = 1, prob = 0.5)),
                 # in this case it would be useful to keep the WT in the output dataset
                 keep=c("WTBL","SEXN")), "must have an id")



  })
})
