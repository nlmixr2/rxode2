rxTest({
  test_that("nodup-keep", {

    #rxode2 issue #169
    mod3 <- rxode2({
      KA=2.94E-01
      ## Clearance with individuals
      CL=1.86E+01 * (WT / 70) ^ 0.75
      V2=4.02E+01
      Q=1.05E+01
      V3=2.97E+02
      Kin=1
      Kout=1
      EC50=200
      ## The linCmt() picks up the variables from above
      C2   = linCmt()
      Tz= 8
      amp=0.1
      eff(0) = 1  ## This specifies that the effect compartment starts at 1.
      d/dt(eff) =  Kin - Kout*(1-C2/(EC50+C2))*eff
    })

    ev <- readRDS(test_path("nodup-keep-ev.rds"))

    rxWithSeed(10, {

      r1 <- solve(mod3, ev,
                  # Create individual covariate data-frame
                  keep="WT", returnType="data.frame")

      expect_length(which(names(r1)=="WT"), 1)
    })


  })

})
