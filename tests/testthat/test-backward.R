rxTest({

  test_that("error with wd specified but modName not specified", {

expect_error(rxode2("cp<-cent/vc;d/dt(gutcp)<--ka*gutcp;d/dt(cent)<-(ka*gutcp)-q/vc*cent+q/vp*pericp-((vmax*cp)/vc)/(km+cp);d/dt(pericp)<-cent*q/vc-q/vp*pericp;f(gutcp)=bio;alag(gutcp)<-lag;gutcp(0)<-0;cent(0)<-0;pericp(0)<-0;", wd=getwd()))

  })

  ## Dynmodel routines
  ode <- "
   dose=200;
   pi = 3.1415926535897931;
   if (t<=0) {
      fI = 0;
   } else {
      fI = F*dose*sqrt(MIT/(2.0*pi*CVI2*t^3))*exp(-(t-MIT)^2/(2.0*CVI2*MIT*t));
   }
   C2 = centr/V2;
   C3 = peri/V3;
   d/dt(centr) = fI - CL*C2 - Q*C2 + Q*C3;
   d/dt(peri)  =              Q*C2 - Q*C3;
"

  sys1 <- rxode2(model = ode)

  ev <- eventTable()
  ev$add.sampling(c(0, c(15, 30, 60, 90, 120, 150, 210, 270, 330, 360, 390, 420, 450, 480)))
  theta <- structure(c(190, 0.65, 0.92, 0.0793, 0.64, 0.292, 9.63), .Names = c("MIT", "CVI2", "F", "CL", "V2", "Q", "V3"))

  val1 <- sys1$solve(theta, ev, atol = 1e-6, rtol = 1e-6)

  ## Prior rxode2 solving...
  val2 <- structure(c(0, 15, 30, 60, 90, 120, 150, 210, 270, 330, 360, 390, 420, 450, 480, 0, 0.00468853094648131, 0.373067597127217, 2.06008521781962, 3.11868121404469, 3.61478640740494, 3.77791523493719, 3.57583622912201, 3.07240077291895, 2.50781365737478, 2.23759663817273, 1.98421582221308, 1.75065831378882, 1.53813326790626, 1.34666851432174, 0, 0.00275724533602532, 0.779135520764223, 12.5294880193912, 29.5561651547891, 42.805783957353, 50.756288454544, 54.6956530842046, 50.0020491493438, 42.249424648696, 38.1457972477322, 34.1486661506102, 30.3627294500035, 26.8473328340894, 23.6307416646032, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 3.14159265358979, 3.14159265358979, 3.14159265358979, 3.14159265358979, 3.14159265358979, 3.14159265358979, 3.14159265358979, 3.14159265358979, 3.14159265358979, 3.14159265358979, 3.14159265358979, 3.14159265358979, 3.14159265358979, 3.14159265358979, 3.14159265358979, 0, 0.00555514066938253, 0.241306787665833, 0.863322464865012, 0.937389549382366, 0.809244619745276, 0.654268731228509, 0.409232361102111, 0.256995004576163, 0.164606161831775, 0.132752793640541, 0.107575895938534, 0.0875618379140851, 0.0715643817688998, 0.0587112500960777, 0, 0.00732582960387705, 0.582918120511276, 3.21888315284315, 4.87293939694483, 5.64810376157022, 5.90299255458936, 5.58724410800314, 4.80062620768586, 3.9184588396481, 3.49624474714489, 3.10033722220793, 2.73540361529503, 2.40333323110352, 2.10416955362771, 0, 0.00028631831111374, 0.0809071153441561, 1.30108909858683, 3.06917602853469, 4.44504506306884, 5.27064262248639, 5.6797147543307, 5.19232078394016, 4.38727151076802, 3.96114197795765, 3.54607125136139, 3.15293140706163, 2.78788502950045, 2.45386725489129), .Dim = c(15L, 8L), .Dimnames = list(NULL, c("time", "centr", "peri", "dose", "pi", "fI", "C2", "C3")))

  val2 <- val2[, dimnames(val1)[[2]]]

  test_that("pi values will not corrupt solver", {
    expect_equal(round(val1, 3), round(val2, 3))
  })

  ## Now test without pi

  ode <- "
   dose=200;
   if (t<=0) {
      fI = 0;
   } else {
      fI = F*dose*sqrt(MIT/(2.0*pi*CVI2*t^3))*exp(-(t-MIT)^2/(2.0*CVI2*MIT*t));
   }
   C2 = centr/V2;
   C3 = peri/V3;
   d/dt(centr) = fI - CL*C2 - Q*C2 + Q*C3;
   d/dt(peri)  =              Q*C2 - Q*C3;
"
  sys1 <- rxode2(model = ode)

  val3 <- sys1$solve(theta, ev, atol = 1e-6, rtol = 1e-6)

  test_that("pi is not needed...", {
    expect_equal(round(val1, 3), round(val3, 3))
  })

  ## Now try environmental solve
  object <- rxode2({
    d / dt(depot) <- -depot * exp(ETA[1] + THETA[1])
    d / dt(center) <- -center * exp(ETA[2] + THETA[2]) * exp(-ETA[3] - THETA[3]) + depot * exp(ETA[1] + THETA[1])
    d / dt(rx__sens_depot_BY_ETA_1___) <- -depot * exp(ETA[1] + THETA[1]) - rx__sens_depot_BY_ETA_1___ * exp(ETA[1] + THETA[1])
    d / dt(rx__sens_depot_BY_ETA_2___) <- -rx__sens_depot_BY_ETA_2___ * exp(ETA[1] + THETA[1])
    d / dt(rx__sens_depot_BY_ETA_3___) <- -rx__sens_depot_BY_ETA_3___ * exp(ETA[1] + THETA[1])
    d / dt(rx__sens_center_BY_ETA_1___) <- depot * exp(ETA[1] + THETA[1]) - rx__sens_center_BY_ETA_1___ * exp(ETA[2] + THETA[2]) * exp(-ETA[3] - THETA[3]) + rx__sens_depot_BY_ETA_1___ * exp(ETA[1] + THETA[1])
    d / dt(rx__sens_center_BY_ETA_2___) <- -center * exp(ETA[2] + THETA[2]) * exp(-ETA[3] - THETA[3]) - rx__sens_center_BY_ETA_2___ * exp(ETA[2] + THETA[2]) * exp(-ETA[3] - THETA[3]) + rx__sens_depot_BY_ETA_2___ * exp(ETA[1] + THETA[1])
    d / dt(rx__sens_center_BY_ETA_3___) <- center * exp(ETA[2] + THETA[2]) * exp(-ETA[3] - THETA[3]) - rx__sens_center_BY_ETA_3___ * exp(ETA[2] + THETA[2]) * exp(-ETA[3] - THETA[3]) + rx__sens_depot_BY_ETA_3___ * exp(ETA[1] + THETA[1])
    rx_pred_ <- center * exp(-ETA[3] - THETA[3])
    rx__sens_rx_pred__BY_ETA_1___ <- rx__sens_center_BY_ETA_1___ * exp(-ETA[3] - THETA[3])
    rx__sens_rx_pred__BY_ETA_2___ <- rx__sens_center_BY_ETA_2___ * exp(-ETA[3] - THETA[3])
    rx__sens_rx_pred__BY_ETA_3___ <- -center * exp(-ETA[3] - THETA[3]) + rx__sens_center_BY_ETA_3___ * exp(-ETA[3] - THETA[3])
    rx_r_ <- Rx_pow_di(THETA[4], 2)
    rx__sens_rx_r__BY_ETA_1___ <- 0
    rx__sens_rx_r__BY_ETA_2___ <- 0
    rx__sens_rx_r__BY_ETA_3___ <- 0
  })


  et <- eventTable()
  et$import.EventTable(structure(list(time = c(0, 0, 0.25, 0.57, 1.12, 2.02, 3.82, 5.1, 7.03, 9.05, 12.12, 24, 24.37, 48, 72, 96, 120, 144, 144, 144.25, 144.57, 145.12, 146.02, 147.82, 149.1, 151.03, 153.05, 156.12, 168.37), evid = c(101L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 101L, 0L, 101L, 101L, 101L, 101L, 101L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), amt = c(4.02, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4.02, 0, 4.02, 4.02, 4.02, 4.02, 4.02, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), .Names = c("time", "evid", "amt"), row.names = c(NA, 29L), class = "data.frame"))

  args <- list(
    object = object, et, invisible = 1, epsilon = 1e-04, cov = NULL, atol = 1e-06,
    rtol = 1e-06, maxsteps = 99999, numDeriv.method = "simple",
    c.hess = NULL, estimate = FALSE, inner.opt = "n1qn1", add.grad = FALSE,
    eta = structure(c(0.23787542222305, -0.528088850787306, -0.219490126341574), .Dim = c(1L, 3L)), theta = c(
      0.261713493062619, -3.18457293837742,
      -0.824924506160168, 1.01900805433423
    ), do.solve = FALSE
  )


  object <- rxode2({
    d / dt(depot) <- prod(-depot, exp(ETA[1] + THETA[1]))
    d / dt(center) <- prod(-center, exp(-ETA[3] - THETA[3]), exp(ETA[2] + THETA[2] + prod(THETA[4], WT))) + prod(depot, exp(ETA[1] + THETA[1]))
    d / dt(rx__sens_depot_BY_ETA_1___) <- prod(-depot, exp(ETA[1] + THETA[1])) - prod(rx__sens_depot_BY_ETA_1___, exp(ETA[1] + THETA[1]))
    d / dt(rx__sens_depot_BY_ETA_2___) <- prod(-rx__sens_depot_BY_ETA_2___, exp(ETA[1] + THETA[1]))
    d / dt(rx__sens_depot_BY_ETA_3___) <- prod(-rx__sens_depot_BY_ETA_3___, exp(ETA[1] + THETA[1]))
    d / dt(rx__sens_center_BY_ETA_1___) <- prod(depot, exp(ETA[1] + THETA[1])) - prod(rx__sens_center_BY_ETA_1___, exp(-ETA[3] - THETA[3]), exp(ETA[2] + THETA[2] + prod(THETA[4], WT))) + prod(rx__sens_depot_BY_ETA_1___, exp(ETA[1] + THETA[1]))
    d / dt(rx__sens_center_BY_ETA_2___) <- prod(-center, exp(-ETA[3] - THETA[3]), exp(ETA[2] + THETA[2] + prod(THETA[4], WT))) - prod(rx__sens_center_BY_ETA_2___, exp(-ETA[3] - THETA[3]), exp(ETA[2] + THETA[2] + prod(THETA[4], WT))) + prod(rx__sens_depot_BY_ETA_2___, exp(ETA[1] + THETA[1]))
    d / dt(rx__sens_center_BY_ETA_3___) <- prod(center, exp(-ETA[3] - THETA[3]), exp(ETA[2] + THETA[2] + prod(THETA[4], WT))) - prod(rx__sens_center_BY_ETA_3___, exp(-ETA[3] - THETA[3]), exp(ETA[2] + THETA[2] + prod(THETA[4], WT))) + prod(rx__sens_depot_BY_ETA_3___, exp(ETA[1] + THETA[1]))
    rx_pred_ <- prod(center, exp(-ETA[3] - THETA[3]))
    rx__sens_rx_pred__BY_ETA_1___ <- prod(rx__sens_center_BY_ETA_1___, exp(-ETA[3] - THETA[3]))
    rx__sens_rx_pred__BY_ETA_2___ <- prod(rx__sens_center_BY_ETA_2___, exp(-ETA[3] - THETA[3]))
    rx__sens_rx_pred__BY_ETA_3___ <- prod(-center, exp(-ETA[3] - THETA[3])) + prod(rx__sens_center_BY_ETA_3___, exp(-ETA[3] - THETA[3]))
    rx_r_ <- Rx_pow_di(THETA[5], 2)
    rx__sens_rx_r__BY_ETA_1___ <- 0
    rx__sens_rx_r__BY_ETA_2___ <- 0
    rx__sens_rx_r__BY_ETA_3___ <- 0
  })

  ## Also for backward compatible it needs to take covariate size = nObs+nDose

  mod1 <- rxode2({
    C2 <- centr / V2
    C3 <- peri / V3
    d / dt(depot) <- -KA * depot
    d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
    d / dt(peri) <- Q * C2 - Q * C3
    d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
  })

  ev <- eventTable(amount.units = "mg", time.units = "hours") %>%
    add.dosing(dose = 10000, nbr.doses = 10, dosing.interval = 12) %>%
    add.dosing(dose = 20000, nbr.doses = 5, start.time = 120, dosing.interval = 24) %>%
    add.sampling(0:240)

  theta <-
    c(
      KA = 2.94E-01, CL = 1.86E+01, V2 = 4.02E+01, # central
      Q = 1.05E+01, V3 = 2.97E+02, # peripheral
      Kin = 1, Kout = 1, EC50 = 200
    ) # effects

    inits <- c(eff = 1)

    x <- solve(mod1, theta, ev, inits)

    test_that("Can retrieve initial conditions.", {
      expect_equal(x$eff0, 1)
      expect_equal(x$eff.0, 1)
      expect_equal(x$eff_0, 1)
      expect_equal(x$centr0, 0)
      expect_equal(x$centr.0, 0)
      expect_equal(x$centr_0, 0)
      expect_equal(x$depot0, 0)
      expect_equal(x$depot.0, 0)
      expect_equal(x$depot_0, 0)
      expect_equal(x$peri0, 0)
      expect_equal(x$peri.0, 0)
      expect_equal(x$peri_0, 0)
    })

    test_that("Can Update initial conditions", {
      x$eff0 <- 2
      expect_equal(x$eff[1], 2)
      x$eff.0 <- 1
      expect_equal(x$eff[1], 1)
      x$eff_0 <- 0.5
      expect_equal(x$eff[1], 0.5)
    })

    x <- solve(mod1, theta, ev, inits)

    test_that("Add sampling makes sense", {
      ## Piping does not update object, like dplyr.
      tmp <- x %>% add.sampling(0.5)
      expect_equal(as.numeric(tmp$time[2]), 0.5)
      expect_equal(as.numeric(x$time[2]), 1)
      ## $ access updates object.
      expect_warning(x$add.sampling(0.5), NA) # from issue #750
      expect_equal(as.numeric(x$time[2]), 0.5)
    })
    x <- solve(mod1, theta, ev, inits)

    test_that("Add dosing makes sense", {
      tmp <- x %>% add.dosing(dose = 500, start.time = 0.5)
      expect_equal(tmp$get.dosing()$time[2], 0.5)
      expect_equal(x$get.dosing()$time[2], 120)
      x$add.dosing(0.5)
      expect_equal(x$get.dosing()$time[2], 0)
    })

    x <- solve(mod1, theta, ev, inits)

    x$t <- seq(0, 5, length.out = 20)

    test_that("Changing sampling makes sense.", {
      expect_equal(length(x$t), 20)
      expect_equal(as.numeric(min(x$t)), 0)
      expect_equal(as.numeric(max(x$t)), 5)
    })

    x <- solve(mod1, theta, ev, inits)
    t1 <- x$centr

    x$Q <- 5

    t2 <- x$centr

    test_that("Changing parameters change values.", {
      expect_true(!(all(t1 == t2)))
    })

    x <- rxModelVars(c("tka=THETA[1];", "tcl=THETA[2];", "tv=THETA[3];", "thwt=THETA[4];", "add.err=THETA[5];", "eta.ka=ETA[1];", "eta.cl=ETA[2];", "eta.v=ETA[3];", "ka=exp(tka+eta.ka);", "cl=exp(tcl+eta.cl+thwt*WT);", "v=exp(tv+eta.v);", "d/dt(depot)=-ka*depot;", "d/dt(center)=ka*depot-cl/v*center;", "cp=center/v;", "nlmixr_pred=cp;"))

    test_that("rxModelVars takes character vector.", {
      expect_s3_class(x, "rxModelVars")
    })
})
