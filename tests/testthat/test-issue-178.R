rxodeTest(
  {
    context("Issue #178: deactivate active compiled model")
    td <- tempdir()

    .rxWithWd(td, {
      test_that("178", {
        ## Define model
        skip_on_os("solaris")
        skip_on_os("mac")
        ode <- "
   C2 = centr/V2;
   C3 = peri/V3;
   d/dt(depot) = -KA*depot;
   d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
   d/dt(peri) = Q*C2 - Q*C3;
   d/dt(eff) = Kin*(1-C2/(EC50+C2)) - Kout*eff;
"

        mod1 <- rxode2(model = ode, modName = "mod1")

        ode <- "
   C2 = centr/V2;
   d/dt(depot) = -KA*depot;
   d/dt(centr) = KA*depot - CL*C2;
   d/dt(eff) = Kin*(1-C2/(EC50+C2)) - Kout*eff;
"

        ## Compile model
        mod1 <- rxode2(model = ode, modName = "mod1")

        expect_equal(rxModelVars(ode)$params, rxModelVars(mod1)$params)
      })
    })
  },
  test = "lvl2"
)
