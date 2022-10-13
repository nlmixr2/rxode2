rxTest({
  test_that("nocb infusion rxode2 vs NONMEM; Issue RxODE#468", {

    # rxode2 model
    mod_run30x <- rxode2({
      THETA_Cl = 4.0
      THETA_Vc = 70.0
      THETA_Ka = 1.0

      TVCl  = THETA_Cl*((BW/75)^1.5)*(0.75^SEX)
      TVVc  = THETA_Vc
      TVKa  = THETA_Ka

      Cl    = TVCl*exp(ETA_Cl)
      Vc    = TVVc*exp(ETA_Vc)
      Ka    = TVKa*exp(ETA_Ka)

      K20   = Cl/Vc
      IPRED = centr/Vc

      d/dt(depot)  = - Ka*depot
      d/dt(centr)  =   Ka*depot - K20*centr
    })

    # IV to centr
    pat_302_1 <- data.frame(ID=1,
                            TIME=c(0.00000,1.50000,4.40000,7.10000,24.60000,72.00000,
                                   96.00000,120.00000,144.00000,168.00000,192.00000,
                                   216.00000),
                            EVID=c(1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1),
                            AMT=c(10000,0,0,0,0,10000,10000,10000,10000,10000,10000,10000),
                            CMT=2,
                            II=0,
                            ADDL=0,
                            RATE=c(10000,0,0,0,0,10000,10000,10000,10000,10000,10000,10000),
                            MDV=c(1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1),
                            DV=c(NA,107.26700,100.99500,54.05460,5.97081,NA,NA,NA,NA,NA,NA,NA),
                            BW=c(77,113,92,132,126,128,41,56,68,45,93,126),
                            SEX=c(0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1))

    eta_302_1 <- c(ETA_Cl=0.398845969, ETA_Vc=0.0762201167, ETA_Ka=0)  # NONMEM

    rx_302_1   <- rxode2::rxSolve(mod_run30x,eta_302_1,pat_302_1,
                                  covsInterpolation="nocb",
                                  addDosing=TRUE)

    # Oral to depot
    pat_301_1 <- data.frame(ID=1,
                            TIME=c(0.00000,1.50000,4.40000,7.10000,24.60000,72.00000,
                                   96.00000,120.00000,144.00000,168.00000,192.00000,
                                   216.00000),
                            EVID=c(1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1),
                            AMT=c(10000,0,0,0,0,10000,10000,10000,10000,10000,10000,10000),
                            CMT=c(1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1),
                            II=0,
                            ADDL=0,
                            MDV=c(1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1),
                            DV=c(NA,85.93470,105.34400,57.21840,6.32941,NA,NA,NA,NA,NA,NA,NA),
                            BW=c(77,113,92,132,126,128,41,56,68,45,93,126),
                            SEX=c(0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1))



    eta_301_1 <- c(ETA_Cl=0.397632632, ETA_Vc=0.0632252546, ETA_Ka=-0.0815987631)  # NONMEM

    rx_301_1   <- rxode2::rxSolve(mod_run30x,eta_301_1,pat_301_1,
                                  covsInterpolation="nocb",
                                  addDosing=TRUE)

    # import NONMEM output
    lst <- qs::qread("test-issue-468.qs")

    run301_tab <- as.data.frame(lst[[1]])
    run302_tab <- as.data.frame(lst[[2]])

    # plotting
    run301_tab$time <- run301_tab$TIME
    run302_tab$time <- run302_tab$TIME


    simu_30x <- rbind(cbind(rx_301_1[,c("time","IPRED")],run="oral",solver="rxode2"),
                      cbind(rx_302_1[,c("time","IPRED")],run="IV",solver="rxode2"),
                      cbind(run301_tab[run301_tab$ID == 1,c("time","IPRED")],run="oral",
                            solver="NONMEM"),
                      cbind(run302_tab[run302_tab$ID == 1,c("time","IPRED")],run="IV",
                            solver="NONMEM"))


    expect_equal(as.data.frame(rx_301_1[,c("time","IPRED")]),
                 as.data.frame(run301_tab[run301_tab$ID == 1,c("time","IPRED")]),
                 tolerance = 1e-5)

    expect_equal(as.data.frame(rx_302_1[,c("time","IPRED")]),
                 as.data.frame(run302_tab[run302_tab$ID == 1,c("time","IPRED")]),
                 tolerance = 1e-5)

  })
})
