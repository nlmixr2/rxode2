rxode2Test(
  {
    # Working Directory + Model Name (Issue RxODE#4)

    model_text <- "
  C2 = centr/V2;
  C3 = peri/V3;
  d/dt(depot) =-KA*depot;
  d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
  d/dt(peri)  =                    Q*C2 - Q*C3;
  d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;
  "
    modName <- paste(sample(LETTERS, 5), collapse = "")
    model <- rxode2::rxode2(model = model_text, modName = modName, wd = "mytempdir")

    test_that("Model has the correct states", {
      expect_equal(
        rxState(model),
        c("depot", "centr", "peri", "eff")
      )
      expect_equal(
        rxParams(model),
        c("V2", "V3", "KA", "CL", "Q", "Kin", "Kout", "EC50")
      )
    })

    rxDelete(model)
    unlink("mytempdir", recursive = TRUE)
  },
  silent = TRUE,
  test = "lvl2"
)
