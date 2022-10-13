rxTest({
  test_that("Issue RxODE#50", {
    .file <- test_path("rxodedemo_ode.txt")
    
    sink(.file)
    cat("C2 = centr/V2;
C3 = peri/V3;
d/dt(depot) =-KAdepot;
d/dt(centr) = KAdepot - CLC2 - QC2 + QC3;
d/dt(peri) = QC2 - QC3;
d/dt(eff) = Kin - Kout(1-C2/(EC50+C2))*eff;")
    sink()
    
    expect_true(file.exists(.file))
    suppressMessages(
      m1 <-
        try(
          rxode2(filename = .file, modName = test_path("m1")),
          silent=TRUE
        )
    )
    expect_false(is(class(m1), "rxode2"))
    expect_true(file.exists(.file))
    
    sink(.file)
    cat("C2 = centr/V2;
C3 = peri/V3;
d/dt(depot) =-KA*depot;
d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
d/dt(peri)  =                    Q*C2 - Q*C3;
d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;")
    sink()
    
    expect_true(file.exists(.file))
    m1 <- rxode2(filename = .file, modName = test_path("m1"))
    expect_true(is(m1, "rxode2"))
    expect_true(file.exists(test_path("rxodedemo_ode.txt")))
    
    rxDelete(m1)
    unlink(test_path("rxodedemo_ode.txt"))
    if (dir.exists(test_path("m1.d"))) unlink(test_path("m1.d"), recursive = TRUE)
  })
})
