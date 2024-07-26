rxTest({
  skip_if_not_installed("units")
  # context("tad family of functions with odes")

  test_that("tad family works with ode", {

    mod1 <- rxode2({
      KA <- 2.94E-01
      CL <- 1.86E+01
      V2 <- 4.02E+01
      Q <- 1.05E+01
      V3 <- 2.97E+02
      Kin <- 1
      Kout <- 1
      EC50 <- 200
      fDepot <- 1
      fPeri <- 1
      durDepot <- 1
      rateDepot <- 1
      durPeri <- 1
      ratePeri <- 1
      C2 <- centr / V2
      C3 <- peri / V3
      d/dt(depot) <- -KA * depot
      d/dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
      d/dt(peri) <- Q * C2 - Q * C3
      d/dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      f(depot) <- fDepot
      f(peri) <- fPeri
      dur(depot) = durDepot
      rate(depot) = rateDepot
      dur(peri) = durPeri
      rate(peri) = ratePeri
      ## TAD tests
      tad <- tad()
      tad0 <- tad0()
      dosen <- dosenum()
      tl <- tlast()
      tl0 <- tlast0()
      tafd <- tafd()
      tafd0 <- tafd0()
      tfirst <- tfirst()
      tfirst0 <- tfirst0()
      tadd <- tad(depot)
      tadd0 <- tad0(depot)
      tfirstd <- tfirst(depot)
      tfirstd0 <- tfirst0(depot)
      tlastd <- tlast(depot)
      tlastd0 <- tlast0(depot)
      tafdd <- tafd(depot)
      tafdd0 <- tafd0(depot)
      tadp <- tad(peri)
      tadp0 <- tad0(peri)
      tafdp <- tafd(peri)
      tafdp0 <- tafd0(peri)
      tfirstp <- tfirst(peri)
      tfirstp0 <- tfirst0(peri)
      tlastp <- tlast(peri)
      tlastp0 <-  tlast0(peri)
      dose <- dose()
      dosed <- dose(depot)
      dosep <- dose(peri)
    })

    ev <- et(amountUnits = "mg", timeUnits = "hours") %>%
      et(time = 1, amt = 10000, addl = 9, ii = 12, cmt = "depot") %>%
      et(time = 120, amt = 2000, addl = 4, ii = 14, cmt = "depot") %>%
      et(time = 122, amt = 2200, addl = 4, ii = 14, cmt = "peri") %>%
      et(0, 240, by = 3)

    r1 <- rxSolve(mod1, ev, addDosing = TRUE)

    r <- c(NA, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000,
            10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000,
            10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000,
            10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000,
            10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000,
            10000, 10000, 10000, 10000, 10000, 2000, 2000, 2200, 2200, 2200,
            2200, 2200, 2000, 2000, 2200, 2200, 2200, 2200, 2200, 2000, 2200,
            2200, 2200, 2200, 2200, 2000, 2000, 2200, 2200, 2200, 2200, 2200,
            2000, 2000, 2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200,
            2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200,
            2200, 2200)

    expect_equal(r1$dosed, c(NA, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000,
                             10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000,
                             10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000,
                             10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000,
                             10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000,
                             10000, 10000, 10000, 10000, 10000, 2000, 2000, 2000, 2000, 2000,
                             2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000,
                             2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000,
                             2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000,
                             2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000,
                             2000, 2000))

    expect_equal(r1$dosep, c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                             NA, NA, NA, NA, NA, 2200, 2200, 2200, 2200, 2200, 2200, 2200,
                             2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200,
                             2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200,
                             2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200,
                             2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200, 2200))

    expect_equal(r1$dosen, c(
      0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4,
      4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8,
      9, 9, 9, 9, 9, 10, 10, 10, 10, 11, 11, 12, 12, 12, 12, 12, 13,
      13, 14, 14, 14, 14, 14, 15, 16, 16, 16, 16, 16, 17, 17, 18, 18,
      18, 18, 18, 19, 19, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
      20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20
    ))
    r <- c(
      NA, 0, 2, 5, 8, 11, 0, 2, 5, 8, 11, 0, 2, 5, 8, 11, 0, 2, 5,
      8, 11, 0, 2, 5, 8, 11, 0, 2, 5, 8, 11, 0, 2, 5, 8, 11, 0, 2,
      5, 8, 11, 0, 2, 5, 8, 11, 0, 2, 5, 8, 0, 0, 0, 1, 4, 7, 10, 0,
      1, 0, 2, 5, 8, 11, 0, 0, 0, 3, 6, 9, 0, 0, 0, 1, 4, 7, 10, 0,
      1, 0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44,
      47, 50, 53, 56, 59, 62)

    expect_equal(r1$tad, r)

    r <- ifelse(is.na(r), 0, r)

    expect_equal(r1$tad0, r)

    r <- c(
      NA, 0, 2, 5, 8, 11, 12, 14, 17, 20, 23, 24, 26, 29, 32, 35,
      36, 38, 41, 44, 47, 48, 50, 53, 56, 59, 60, 62, 65, 68, 71, 72,
      74, 77, 80, 83, 84, 86, 89, 92, 95, 96, 98, 101, 104, 107, 108,
      110, 113, 116, 119, 119, 121, 122, 125, 128, 131, 133, 134, 135,
      137, 140, 143, 146, 147, 149, 149, 152, 155, 158, 161, 161, 163,
      164, 167, 170, 173, 175, 176, 177, 179, 182, 185, 188, 191, 194,
      197, 200, 203, 206, 209, 212, 215, 218, 221, 224, 227, 230, 233,
      236, 239)

    expect_equal(r1$tafd, r)

    r <- ifelse(is.na(r), 0, r)
    expect_equal(r1$tafd0, r)

    r <- c(
      NA, 0, 2, 5, 8, 11, 0, 2, 5, 8, 11, 0, 2, 5, 8, 11, 0, 2, 5,
      8, 11, 0, 2, 5, 8, 11, 0, 2, 5, 8, 11, 0, 2, 5, 8, 11, 0, 2,
      5, 8, 11, 0, 2, 5, 8, 11, 0, 2, 5, 8, 0, 0, 2, 3, 6, 9, 12, 0,
      1, 2, 4, 7, 10, 13, 0, 2, 2, 5, 8, 11, 0, 0, 2, 3, 6, 9, 12,
      0, 1, 2, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43,
      46, 49, 52, 55, 58, 61, 64)

    expect_equal(r1$tadd, r)

    r <- ifelse(is.na(r), 0, r)
    expect_equal(r1$tadd0, r)


    r <- c(
      NA, 0, 2, 5, 8, 11, 12, 14, 17, 20, 23, 24, 26, 29, 32, 35,
      36, 38, 41, 44, 47, 48, 50, 53, 56, 59, 60, 62, 65, 68, 71, 72,
      74, 77, 80, 83, 84, 86, 89, 92, 95, 96, 98, 101, 104, 107, 108,
      110, 113, 116, 119, 119, 121, 122, 125, 128, 131, 133, 134, 135,
      137, 140, 143, 146, 147, 149, 149, 152, 155, 158, 161, 161, 163,
      164, 167, 170, 173, 175, 176, 177, 179, 182, 185, 188, 191, 194,
      197, 200, 203, 206, 209, 212, 215, 218, 221, 224, 227, 230, 233,
      236, 239)

    expect_equal(r1$tafdd, r)

    r <- ifelse(is.na(r), 0, r)
    expect_equal(r1$tafdd0, r)

    r <- c(
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, 0, 1, 4, 7, 10, 12, 13, 0, 2, 5, 8, 11, 12,
      0, 0, 3, 6, 9, 12, 12, 0, 1, 4, 7, 10, 12, 13, 0, 2, 5, 8, 11,
      14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47, 50, 53, 56, 59,
      62)

    expect_equal(r1$tadp, r)

    r <- ifelse(is.na(r), 0, r)
    expect_equal(r1$tadp0, r)

    expect_equal(r1$tfirst[1], NA_real_)
    expect_true(all(r1$tfirst[-1] == 1))

    r <- c(
      NA, 1, 1, 1, 1, 1, 13, 13, 13, 13, 13, 25, 25, 25, 25, 25,
      37, 37, 37, 37, 37, 49, 49, 49, 49, 49, 61, 61, 61, 61, 61, 73,
      73, 73, 73, 73, 85, 85, 85, 85, 85, 97, 97, 97, 97, 97, 109,
      109, 109, 109, 120, 120, 122, 122, 122, 122, 122, 134, 134, 136,
      136, 136, 136, 136, 148, 150, 150, 150, 150, 150, 162, 162, 164,
      164, 164, 164, 164, 176, 176, 178, 178, 178, 178, 178, 178, 178,
      178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178,
      178, 178)

    expect_equal(
      r1$tl,r)

    r <- ifelse(is.na(r), 0, r)
    expect_equal(r1$tl0,r)

    r <- c(
      NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    expect_equal(r1$tfirstd, r)

    r <- ifelse(is.na(r), 0, r)
    expect_equal(r1$tfirstd0, r)

    r <- c(
      NA, 1, 1, 1, 1, 1, 13, 13, 13, 13, 13, 25, 25, 25, 25, 25,
      37, 37, 37, 37, 37, 49, 49, 49, 49, 49, 61, 61, 61, 61, 61, 73,
      73, 73, 73, 73, 85, 85, 85, 85, 85, 97, 97, 97, 97, 97, 109,
      109, 109, 109, 120, 120, 120, 120, 120, 120, 120, 134, 134, 134,
      134, 134, 134, 134, 148, 148, 148, 148, 148, 148, 162, 162, 162,
      162, 162, 162, 162, 176, 176, 176, 176, 176, 176, 176, 176, 176,
      176, 176, 176, 176, 176, 176, 176, 176, 176, 176, 176, 176, 176,
      176, 176)

    expect_equal(r1$tlastd, r)
    r <- ifelse(is.na(r), 0, r)

    expect_equal(r1$tlastd0, r)

    r <- c(
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, 122, 122, 122, 122, 122, 122, 122, 136, 136,
      136, 136, 136, 136, 150, 150, 150, 150, 150, 150, 150, 164, 164,
      164, 164, 164, 164, 164, 178, 178, 178, 178, 178, 178, 178, 178,
      178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178,
      178)
    expect_equal(
      r1$tlastp, r)

    r <- ifelse(is.na(r), 0, r)
    expect_equal(r1$tlastp0, r)
    r <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, 122, 122, 122, 122, 122, 122, 122, 122, 122,
      122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122,
      122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122,
      122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122,
      122)

    expect_equal(r1$tfirstp, r)

    r <-  ifelse(is.na(r), 0, r)
    expect_equal(r1$tfirstp0, r)

    f <- function(r1, r2) {
      expect_equal(r1$dose, r2$dose)
      expect_equal(r1$dosed, r2$dosed)
      expect_equal(r1$dosep, r2$dosep)
      expect_equal(r1$dosen, r2$dosen)
      expect_equal(r1$tad, r2$tad)
      expect_equal(r1$tafd, r2$tafd)
      expect_equal(r1$tadd, r2$tadd)
      expect_equal(r1$tafdd, r2$tafdd)
      expect_equal(r1$tadp, r2$tadp)
      expect_equal(r1$tl, r2$tl)
      expect_equal(r1$tfirst, r2$tfirst)
      expect_equal(r1$tfirstd, r2$tfirstd)
      expect_equal(r1$tlastd, r2$tlastd)
      expect_equal(r1$tlastp, r2$tlastp)
      expect_equal(r1$tfirstp, r2$tfirstp)
      expect_equal(r1$tad0, r2$tad0)
      expect_equal(r1$tafd0, r2$tafd0)
      expect_equal(r1$tadd0, r2$tadd0)
      expect_equal(r1$tafdd0, r2$tafdd0)
      expect_equal(r1$tadp0, r2$tadp0)
      expect_equal(r1$tl0, r2$tl0)
      expect_equal(r1$tfirst0, r2$tfirst0)
      expect_equal(r1$tfirstd0, r2$tfirstd0)
      expect_equal(r1$tlastd0, r2$tlastd0)
      expect_equal(r1$tlastp0, r2$tlastp0)
      expect_equal(r1$tfirstp0, r2$tfirstp0)
    }


    r2 <- rxSolve(mod1, c(fDepot=0.5, fPeri=0.25), ev, addDosing = TRUE)

    f(r1, r2)

    ev <- et(amountUnits = "mg", timeUnits = "hours") %>%
      et(time = 1, amt = 10000, rate=10000 / 6, addl = 9, ii = 12, cmt = "depot") %>%
      et(time = 120, amt = 2000, rate=2000 / 6, addl = 4, ii = 14, cmt = "depot") %>%
      et(time = 122, amt = 2200, rate=2200 / 6, addl = 4, ii = 14, cmt = "peri") %>%
      et(0, 240, by = 3)

    r2 <- rxSolve(mod1, ev, addDosing = TRUE)

    f(r1, r2)

    r2 <- rxSolve(mod1, c(fDepot=0.5, fPeri=0.25), ev, addDosing = TRUE)

    f(r1, r2)

    ev <- et(amountUnits = "mg", timeUnits = "hours") %>%
      et(time = 1, amt = 10000, dur=6, addl = 9, ii = 12, cmt = "depot") %>%
      et(time = 120, amt = 2000, dur=6, addl = 4, ii = 14, cmt = "depot") %>%
      et(time = 122, amt = 2200, dur=6, addl = 4, ii = 14, cmt = "peri") %>%
      et(0, 240, by = 3)

    r3 <- rxSolve(mod1, ev, addDosing=TRUE)

    f(r1, r3)

    r2 <- rxSolve(mod1, c(fDepot=0.5, fPeri=0.25), ev, addDosing = TRUE)

    f(r1, r2)

    ev <- et(amountUnits = "mg", timeUnits = "hours") %>%
      et(time = 1, amt = 10000, rate = -1, addl = 9, ii = 12, cmt = "depot") %>%
      et(time = 120, amt = 2000, rate = -1, addl = 4, ii = 14, cmt = "depot") %>%
      et(time = 122, amt = 2200, rate= -1, addl = 4, ii = 14, cmt = "peri") %>%
      et(0, 240, by = 3)

    r1 <- rxSolve(mod1, ev, addDosing = TRUE)

    r2 <- rxSolve(mod1, c(fDepot=0.5, fPeri=0.25), ev, addDosing = TRUE)

    f(r1, r2)

    ev <- et(amountUnits = "mg", timeUnits = "hours") %>%
      et(time = 1, amt = 10000, rate = -2, addl = 9, ii = 12, cmt = "depot") %>%
      et(time = 120, amt = 2000, rate = -2, addl = 4, ii = 14, cmt = "depot") %>%
      et(time = 122, amt = 2200, rate= -2, addl = 4, ii = 14, cmt = "peri") %>%
      et(0, 240, by = 3)

    r1 <- rxSolve(mod1, ev, addDosing = TRUE)

    r2 <- rxSolve(mod1, c(fDepot=0.5, fPeri=0.25), ev, addDosing = TRUE)

    f(r1, r2)

  })

  test_that("test parsing of ode", {
    suppressMessages(withr::with_options(
      # Default option set
      list(rxode2.syntax.require.ode.first=NULL),
      expect_error(rxode2({
        KA <- 2.94E-01
        CL <- 1.86E+01
        V2 <- 4.02E+01
        Q <- 1.05E+01
        V3 <- 2.97E+02
        Kin <- 1
        Kout <- 1
        EC50 <- 200
        C2 <- centr / V2
        C3 <- peri / V3
        ## error in capturing early
        f <- tad(eff)
        d / dt(depot) <- -KA * depot
        d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
        d / dt(peri) <- Q * C2 - Q * C3
        d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      }))
    ))

    ## now change the option
    withr::with_options(
      list(rxode2.syntax.require.ode.first = FALSE),
      expect_error(rxode2({
        KA <- 2.94E-01
        CL <- 1.86E+01
        V2 <- 4.02E+01
        Q <- 1.05E+01
        V3 <- 2.97E+02
        Kin <- 1
        Kout <- 1
        EC50 <- 200
        C2 <- centr / V2
        C3 <- peri / V3
        ## error in capturing early
        f <- tad(eff)
        d / dt(depot) <- -KA * depot
        d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
        d / dt(peri) <- Q * C2 - Q * C3
        d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      }), NA)
    )
  })

  # context("tad family of functions with linCmt()")

  test_that("lincmt solution tad family", {

    sol.1c.ka <- rxode2({
      KA <- 2
      V <- 20
      CL <- 25
      C2 <- linCmt(V, CL, KA)
      tad <- tad()
      tafd <- tafd()
      tadd <- tad(depot)
      tafdd <- tafd(depot)
      tadc <- tad(central)
      tafdc <- tafd(central)
      dose <- dose()
      dosed <- dose(depot)
      dosec <- dose(central)
    })

    et <- eventTable() %>%
      add.dosing(dose = 3, nbr.doses = 6, dosing.interval = 8) %>%
      add.dosing(
        dose = 6, nbr.doses = 6, dosing.interval = 8,
        start.time = 2, dosing.to = "central"
      ) %>%
      add.sampling(seq(0, 48, length.out = 200))

    s1 <- rxSolve(sol.1c.ka, et)

    sol.1c.ka <- rxode2({
      KA <- 2
      V <- 20
      CL <- 25
      C2 <- linCmt(V, CL, KA)
      tad <- tad()
      tafd <- tafd()
      tadc <- tad(central)
      tafdc <- tafd(central)
      dose <- dose()
      dosed <- dose(depot)
      dosec <- dose(central)
    })

    s2 <- rxSolve(sol.1c.ka, et)

    expect_equal(s2$tad, s1$tad)
    expect_equal(s2$tafd, s1$tafd)
    expect_equal(s2$tadc, s1$tadc)
    expect_equal(s2$tafdc, s1$tafdc)
    expect_equal(s2$dose, s1$dose)
    expect_equal(s2$dosed, s1$dosed)
    expect_equal(s2$dosec, s1$dosec)

    expect_error(rxode2({
      V <- 20
      CL <- 25
      C2 <- linCmt(V, CL)
      tad <- tad()
      tafd <- tafd()
      tadd <- tad(depot)
      tafdd <- tafd(depot)
      tadc <- tad(central)
      tafdc <- tafd(central)
    }))

    one.cmt <- rxode2({
      V <- 20
      CL <- 25
      C2 <- linCmt(V, CL)
      tad <- tad()
      tafd <- tafd()
      tadc <- tad(central)
      tafdc <- tafd(central)
      dose <- dose()
      dosec <- dose(central)
    })

    et <- eventTable() %>%
      add.dosing(dose = 3, nbr.doses = 6, dosing.interval = 8) %>%
      add.sampling(seq(0, 48, length.out = 200))

    s <- rxSolve(one.cmt, et)

    expect_equal(s$tad, s$tadc)
    expect_equal(s$tafd, s$tafdc)
    expect_equal(s$dose, s$dosec)
  })

  # context("tad family of functions with linCmt()/ode mix")

  test_that("ode mixed", {

    mod3 <- rxode2({
      KA <- 2.94E-01
      CL <- 1.86E+01
      V2 <- 4.02E+01
      Q <- 1.05E+01
      V3 <- 2.97E+02
      Kin0 <- 1
      Kout <- 1
      EC50 <- 200
      ## The linCmt() picks up the variables from above
      C2 <- linCmt()
      Tz <- 8
      amp <- 0.1
      eff(0) <- 1 ## This specifies that the effect compartment starts at 1.
      ## Kin changes based on time of day (like cortosol)
      Kin <- Kin0 + amp * cos(2 * pi * (ctime - Tz) / 24)
      d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      tadd <- tad(depot)
      tad <- tad()
      tade <- tad(eff)
      dose <- dose()
      dosed <- dose(depot)
      dosee <- dose(eff)
    })

    ev <- eventTable(amount.units = "mg", time.units = "hours") %>%
      add.dosing(dose = 10000, nbr.doses = 1, dosing.to = 1) %>%
      add.sampling(seq(0, 48, length.out = 100))


    ## Create data frame of 8 am dosing for the first dose This is done
    ## with base R but it can be done with dplyr or data.table
    ev$ctime <- (ev$time + units::set_units(8, hr)) %% 24

    x <- rxSolve(mod3, ev)

    expect_equal(x$tad, x$tadd)
    expect_true(all(is.na(x$tade)))

    expect_equal(x$dose, x$dosed)
    expect_true(all(is.na(x$dosee)))

    ev <- eventTable(amount.units = "mg", time.units = "hours") %>%
      add.dosing(dose = 10000, nbr.doses = 1, dosing.to = 1) %>%
      add.dosing(dose = -1, start.time = 6, nbr.doses = 1, dosing.to = 3) %>%
      add.sampling(seq(0, 48, length.out = 20))


    ## Create data frame of 8 am dosing for the first dose This is done
    ## with base R but it can be done with dplyr or data.table
    ev$ctime <- (ev$time + units::set_units(8, hr)) %% 24

    x <- rxSolve(mod3, ev, addDosing = TRUE)

    expect_false(isTRUE(all.equal(x$tad, x$tadd)))

    expect_false(isTRUE(all.equal(x$tad, x$tade)))

  })
})
