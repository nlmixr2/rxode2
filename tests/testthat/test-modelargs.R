rxodeTest(
  {
    context("Test that model specification can take string, file name or rxode expressions")
    ode <- rxode2(model = "d/dt(y) = r * y * (1 - y/K);")

    ode2 <- rxode2({
      d / dt(y) <- r * y * (1 - y / K)
    })

    test_that("string and expression returns the same result", {
      expect_equal(rxModelVars(ode)$md5["parsed_md5"], rxModelVars(ode2)$md5["parsed_md5"])
    })

    tmp <- tempfile(fileext = ".rx")

    sink(tmp)
    cat("d/dt(y) = r * y * (1 - y/K);\n")
    sink()
    while (!file.exists(tmp)) {
      Sys.sleep(1)
    }
    ode3 <- rxode2(tmp)
    unlink(tmp)

    test_that("file and string returns the same result", {
      expect_equal(rxModelVars(ode)$md5["parsed_md5"], rxModelVars(ode3)$md5["parsed_md5"])
      expect_equal(rxNorm(ode), rxNorm(ode3))
    })


    ode <- rxode2("
         C2 = centr/V2;
         C3 = peri/V3;
         d/dt(depot) =-KA*depot;
         d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
         d/dt(peri)  =                    Q*C2 - Q*C3;
         d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;")

    ode2 <- rxode2({
      C2 <- centr / V2
      C3 <- peri / V3
      d / dt(depot) <- -KA * depot
      d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
      d / dt(peri) <- Q * C2 - Q * C3
      d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
    })

    tmp <- tempfile(fileext = ".rx")
    sink(tmp)
    cat("
         C2 = centr/V2;
         C3 = peri/V3;
         d/dt(depot) =-KA*depot;
         d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
         d/dt(peri)  =                    Q*C2 - Q*C3;
         d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;")
    sink()
    while (!file.exists(tmp)) {
      Sys.sleep(1)
    }
    ode3 <- rxode2(tmp)
    unlink(tmp)

    test_that("string and expression returns the same result", {
      expect_equal(rxModelVars(ode)$md5["parsed_md5"], rxModelVars(ode2)$md5["parsed_md5"])
    })
    test_that("file and string returns the same result", {
      expect_equal(rxModelVars(ode)$md5["parsed_md5"], rxModelVars(ode3)$md5["parsed_md5"])
    })
  },
  silent = TRUE,
  test = "cran"
)
