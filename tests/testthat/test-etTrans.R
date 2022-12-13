rxTest({ # mostly tested in 'rxode2et'

  for (radi in 1:2) {
    rxode2et::forderForceBase(switch(radi,
                                     TRUE,
                                     FALSE
    ))
    radix <- switch(radi,
                    "base::order",
                    "data.table::forder"
    )
    # context(sprintf("etTrans checks (radix: %s)", radix))
    rxSetIni0(FALSE)

    mod <- rxode2("
a = 6
b = 0.6
d/dt(intestine) = -a*intestine
d/dt(blood)     = a*intestine - b*blood
")

    et <- eventTable()
    et$add.dosing(
      dose = 2 / 24, rate = 2, start.time = 0,
      nbr.doses = 10, dosing.interval = 1
    )
    et <- et %>%
      et(0.05, evid = 2) %>%
      et(amt = 3, time = 0.5, cmt = out) %>%
      et(amt = 3, time = 0.1, cmt = intestine, ss = 1, ii = 3) %>%
      et(amt = 3, time = 0.3, cmt = intestine, ss = 2, ii = 3) %>%
      et(time = 0.2, cmt = "-intestine") %>%
      as.data.frame()

    test_that("error for empty data", {
      expect_error(suppressWarnings({
        etTrans(et, mod)
      }))
    })

    ett1 <- etTrans(et, mod, keepDosingOnly = TRUE)
    tmp1 <- sort(unique(ett1$EVID))

    et$cmt <- factor(et$cmt)
    ett2 <- etTrans(et, mod, keepDosingOnly = TRUE)

    test_that("factor and character give same compartment information", {
      expect_equal(attr(class(ett2), ".rxode2.lst")$cmtInfo, attr(class(ett1), ".rxode2.lst")$cmtInfo)
      expect_equal(attr(class(ett2), ".rxode2.lst")$cmtInfo, c("intestine", "blood", "out"))
    })

    test_that("factor and character give same evids", {
      expect_equal(ett1$EVID, ett2$EVID)
    })

    et0 <- et

    et$cmt <- paste(et$cmt)
    et$cmt[1:2] <- NA_character_

    ett1 <- etTrans(et, mod, keepDosingOnly = TRUE, addCmt = TRUE)

    test_that("string NA gives 1 for default compartment", {
      expect_equal(ett1$EVID, ett2$EVID)
    })

    et <- et0
    et$cmt[1:2] <- NA_integer_
    ett2 <- etTrans(et, mod, keepDosingOnly = TRUE, addCmt = TRUE)

    test_that("factor NA gives 1 for default compartment", {
      expect_equal(ett2$EVID, ett1$EVID)
    })

    et$cmt <- as.integer(et$cmt)

    et$cmt[1:2] <- NA_integer_

    ett2 <- etTrans(et, mod, keepDosingOnly = TRUE, addCmt = TRUE)

    test_that("factor NA gives 1 for default compartment", {
      expect_equal(ett2$EVID[1:2], ett1$EVID[1:2])
    })

    et <- eventTable()
    et$add.dosing(
      dose = 2 / 24, rate = 2, start.time = 0,
      nbr.doses = 10, dosing.interval = 1
    )
    et <- et %>%
      et(0.05, evid = 2) %>%
      et(amt = 3, time = 0.5, cmt = "-out") %>%
      as.data.frame()

    test_that("error for negative non ODE compartments", {
      expect_error(etTrans(et, mod, keepDosingOnly = TRUE))
      et$cmt <- factor(et$cmt)
      expect_error(etTrans(et, mod, keepDosingOnly = TRUE))
    })

    et <- eventTable()
    et$add.dosing(
      dose = 2 / 24, rate = 2, start.time = 0,
      nbr.doses = 10, dosing.interval = 1
    )
    et <- et %>%
      et(0.05, evid = 2) %>%
      et(amt = 3, time = 0.25, cmt = "out") %>%
      et(amt = 3, time = 0.5, cmt = "-out") %>%
      as.data.frame()

    test_that("error for negative non ODE compartments after defined compartment", {
      expect_error(etTrans(et, mod, keepDosingOnly = TRUE))
      et$cmt <- factor(et$cmt)
      expect_error(etTrans(et, mod, keepDosingOnly = TRUE))
    })

    et <- et() %>% et(amt = 3, time = 0.24, evid = 4)

    test_that("EVID=4 makes sense", {
      expect_warning(
        expect_equal(
          etTrans(et, mod, keepDosingOnly = TRUE)$EVID,
          c(3L, 101L)
        )
      )
    })


    mod <- rxode2("    CO = (187 * WT^0.81) * 60/1000
    QHT = 4 * CO/100
    QBR = 12 * CO/100
    QMU = 17 * CO/100
    QAD = 5 * CO/100
    QSK = 5 * CO/100
    QSP = 3 * CO/100
    QPA = 1 * CO/100
    QLI = 25.5 * CO/100
    QST = 1 * CO/100
    QGU = 14 * CO/100
    QHA = QLI - (QSP + QPA + QST + QGU)
    QBO = 5 * CO/100
    QKI = 19 * CO/100
    QRB = CO - (QHT + QBR + QMU + QAD + QSK + QLI + QBO + QKI)
    QLU = QHT + QBR + QMU + QAD + QSK + QLI + QBO + QKI + QRB
    VLU = (0.76 * WT/100)/1.051
    VHT = (0.47 * WT/100)/1.03
    VBR = (2 * WT/100)/1.036
    VMU = (40 * WT/100)/1.041
    VAD = (21.42 * WT/100)/0.916
    VSK = (3.71 * WT/100)/1.116
    VSP = (0.26 * WT/100)/1.054
    VPA = (0.14 * WT/100)/1.045
    VLI = (2.57 * WT/100)/1.04
    VST = (0.21 * WT/100)/1.05
    VGU = (1.44 * WT/100)/1.043
    VBO = (14.29 * WT/100)/1.99
    VKI = (0.44 * WT/100)/1.05
    VAB = (2.81 * WT/100)/1.04
    VVB = (5.62 * WT/100)/1.04
    VRB = (3.86 * WT/100)/1.04
    BP = 0.61
    fup = 0.028
    fub = fup/BP
    KbLU = exp(0.8334)
    KbHT = exp(1.1205)
    KbSK = exp(-0.5238)
    KbSP = exp(0.3224)
    KbPA = exp(0.3224)
    KbLI = exp(1.7604)
    KbST = exp(0.3224)
    KbGU = exp(1.2026)
    KbKI = exp(1.3171)
    S15 = VVB * BP/1000
    C15 = Venous_Blood/S15
    lnC15 = log(C15)
    d/dt(Lungs) = QLU * (Venous_Blood/VVB - Lungs/KbLU/VLU)
    d/dt(Heart) = QHT * (Arterial_Blood/VAB - Heart/KbHT/VHT)
    d/dt(Brain) = QBR * (Arterial_Blood/VAB - Brain/KbBR/VBR)
    d/dt(Muscles) = QMU * (Arterial_Blood/VAB - Muscles/KbMU/VMU)
    d/dt(Adipose) = QAD * (Arterial_Blood/VAB - Adipose/KbAD/VAD)
    d/dt(Skin) = QSK * (Arterial_Blood/VAB - Skin/KbSK/VSK)
    d/dt(Spleen) = QSP * (Arterial_Blood/VAB - Spleen/KbSP/VSP)
    d/dt(Pancreas) = QPA * (Arterial_Blood/VAB - Pancreas/KbPA/VPA)
    d/dt(Liver) = QHA * Arterial_Blood/VAB + QSP * Spleen/KbSP/VSP + QPA * Pancreas/KbPA/VPA + QST * Stomach/KbST/VST + QGU * Gut/KbGU/VGU - CLint * fub * Liver/KbLI/VLI - QLI * Liver/KbLI/VLI
    d/dt(Stomach) = QST * (Arterial_Blood/VAB - Stomach/KbST/VST)
    d/dt(Gut) = QGU * (Arterial_Blood/VAB - Gut/KbGU/VGU)
    d/dt(Bones) = QBO * (Arterial_Blood/VAB - Bones/KbBO/VBO)
    d/dt(Kidneys) = QKI * (Arterial_Blood/VAB - Kidneys/KbKI/VKI)
    d/dt(Arterial_Blood) = QLU * (Lungs/KbLU/VLU - Arterial_Blood/VAB)
    d/dt(Venous_Blood) = QHT * Heart/KbHT/VHT + QBR * Brain/KbBR/VBR + QMU * Muscles/KbMU/VMU + QAD * Adipose/KbAD/VAD + QSK * Skin/KbSK/VSK + QLI * Liver/KbLI/VLI + QBO * Bones/KbBO/VBO + QKI * Kidneys/KbKI/VKI + QRB * Rest_of_Body/KbRB/VRB - QLU * Venous_Blood/VVB
    d/dt(Rest_of_Body) = QRB * (Arterial_Blood/VAB - Rest_of_Body/KbRB/VRB)")


    test_that("strange rate doesn't affect model", {
      et1 <- test_path("etTrans1.qs")
      skip_if_not(file.exists(et1))
      dat <- qs::qread(et1)
      expect_false(any(etTrans(dat, mod)$AMT < 0, na.rm = TRUE))
    })

    test_that("Missing evid gives the same results", {
      theoSd <- nlmixr2data::theo_sd
      d <- theoSd[, names(theoSd) != "EVID"]

      mod <- rxode2({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        cp <- linCmt()
      })

      t1 <- etTrans(theoSd, mod)
      t2 <- etTrans(d, mod)

      expect_equal(t1$ID, t2$ID)
      expect_equal(t1$TIME, t2$TIME)
      expect_equal(t1$EVID, t2$EVID)
      expect_equal(t1$AMT, t2$AMT)
      expect_equal(t1$II, t2$II)
      expect_equal(t1$DV, t2$DV)
    })

    ## Test non-standard inputs
    tmp <- as.data.frame(et() %>% et(amt = 3, time = 0.24, evid = 4))
    for (col in c("ss", "evid", "dur", "amt", "addl", "dv", "mdv", "rate", "ii")) {
      et <- data.frame(col = "a", tmp[, names(tmp) != col], stringsAsFactors = FALSE)
      names(et)[1] <- col
      test_that(sprintf("Non-numeric inputs raise errors (%s)", col), {
        expect_error(etTrans(et, mod), col)
      })
    }

    ## Test dates
    d1 <- data.frame(DATE = c("10-1-86", "10-1-86", "10-2-86"), TIME = c("9:15", "14:40", "8:30"), stringsAsFactors = F)
    d1$DV <- 0

    d2 <- rbind(data.frame(ID = 1, d1, stringsAsFactors = F), data.frame(ID = 2, d1, stringsAsFactors = F))
    d2[d2$ID == 2, "DATE"] <- gsub("^10", "11", d2[d2$ID == 2, "DATE"])

    d3 <- d1
    d3$DATE <- c("10-1-1986", "10-1-1986", "10-2-1986")

    d4 <- d1
    d4$DATE <- c("10 1 1986", "10/1/86", "10-2-1986")

    test_that("DATE conversion works correctly", {
      tmp <- etTrans(d1, mod)
      expect_equal(c(0, 5.41666666666667, 23.25), tmp$TIME)
      tmp <- etTrans(d2, mod)
      expect_equal(c(0, 5.41666666666667, 23.25, 0, 5.41666666666667, 23.25), tmp$TIME)
      tmp <- etTrans(d3, mod)
      expect_equal(c(0, 5.41666666666667, 23.25), tmp$TIME)
      tmp <- etTrans(d4, mod)
      expect_equal(c(0, 5.41666666666667, 23.25), tmp$TIME)
    })

    ## Dat1= day month year
    d1 <- data.frame(DV = 0, DAT1 = c("1-10-86", "1-10-86", "2-10-86"), TIME = c("9:15", "14:40", "8:30"), stringsAsFactors = F)

    d2 <- rbind(data.frame(ID = 1, d1, stringsAsFactors = F), data.frame(ID = 2, d1, stringsAsFactors = F))
    d2[d2$ID == 2, "DAT1"] <- gsub("-10-", "-11-", d2[d2$ID == 2, "DAT1"])

    d3 <- d1
    d3$DAT1 <- c("1-10-1986", "1-10-1986", "2-10-1986")

    d4 <- d1
    d4$DAT1 <- c("1-10-1986", "1-10-86", "2-10-1986")

    test_that("DAT1 conversion works correctly", {
      tmp <- etTrans(d1, mod)
      expect_equal(c(0, 5.41666666666667, 23.25), tmp$TIME)
      tmp <- etTrans(d2, mod)
      expect_equal(c(0, 5.41666666666667, 23.25, 0, 5.41666666666667, 23.25), tmp$TIME)
      tmp <- etTrans(d3, mod)
      expect_equal(c(0, 5.41666666666667, 23.25), tmp$TIME)
      tmp <- etTrans(d4, mod)
      expect_equal(c(0, 5.41666666666667, 23.25), tmp$TIME)
    })

    ## Dat2 = year month day

    d1 <- data.frame(DAT2 = c("86-10-1", "86-10-1", "86-10-2"), TIME = c("9:15", "14:40", "8:30"), stringsAsFactors = F)
    d1$DV <- 0

    d2 <- rbind(data.frame(ID = 1, d1, stringsAsFactors = F), data.frame(ID = 2, d1, stringsAsFactors = F))
    d2[d2$ID == 2, "DAT2"] <- gsub("-10-", "-11-", d2[d2$ID == 2, "DAT2"])

    d3 <- d1
    d3$DAT2 <- c("1986-10-1", "1986-10-1", "1986-10-2")

    d4 <- d1
    d4$DAT2 <- c("1986-10-1", "86-10-1", "1986-10-2")

    test_that("DAT2 conversion works correctly", {
      tmp <- etTrans(d1, mod)
      expect_equal(c(0, 5.41666666666667, 23.25), tmp$TIME)
      tmp <- etTrans(d2, mod)
      expect_equal(c(0, 5.41666666666667, 23.25, 0, 5.41666666666667, 23.25), tmp$TIME)
      tmp <- etTrans(d3, mod)
      expect_equal(c(0, 5.41666666666667, 23.25), tmp$TIME)
      tmp <- etTrans(d4, mod)
      expect_equal(c(0, 5.41666666666667, 23.25), tmp$TIME)
    })

    ## DAT3 conversion
    d1 <- data.frame(DAT3 = c("86-1-10", "86-1-10", "86-2-10"), TIME = c("9:15", "14:40", "8:30"), stringsAsFactors = F)
    d1$DV <- 0

    d2 <- rbind(data.frame(ID = 1, d1, stringsAsFactors = F), data.frame(ID = 2, d1, stringsAsFactors = F))
    d2[d2$ID == 2, "DAT3"] <- gsub("-10$", "-11", d2[d2$ID == 2, "DAT3"])

    d3 <- d1
    d3$DAT3 <- c("1986-1-10", "1986-1-10", "1986-2-10")

    d4 <- d1
    d4$DAT3 <- c("1986-1-10", "86-1-10", "1986-2-10")

    test_that("DAT3 conversion works correctly", {
      tmp <- etTrans(d1, mod)
      expect_equal(c(0, 5.41666666666667, 23.25), tmp$TIME)
      tmp <- etTrans(d2, mod)
      expect_equal(c(0, 5.41666666666667, 23.25, 0, 5.41666666666667, 23.25), tmp$TIME)
      tmp <- etTrans(d3, mod)
      expect_equal(c(0, 5.41666666666667, 23.25), tmp$TIME)
      tmp <- etTrans(d4, mod)
      expect_equal(c(0, 5.41666666666667, 23.25), tmp$TIME)
    })

    d1 <- data.frame(DV = 0, DATE = c("10-1-86", "10-1-86", "10-2-86"), TIME = c("9:15", "14:40", "8:30"), stringsAsFactors = F)

    d2 <- d1
    d2$DAT1 <- d2$DATE

    d3 <- d1
    d3$DAT2 <- d3$DATE

    d4 <- d1
    d4$DAT3 <- d4$DATE

    test_that("Multiple DATE errors", {
      expect_error(etTrans(d2, mod))
      expect_error(etTrans(d3, mod))
      expect_error(etTrans(d4, mod))
    })


    d1 <- data.frame(DV = 0, DATE = c("10-1-86", "10-1-86", "10-2-86"), TIME = c("9.15", "14:40", "8:30"), stringsAsFactors = F)

    test_that("Bad Date/Time combination", {
      expect_error(etTrans(d1, mod))
    })

    ## Test mixed classic rxode2 and NONMEM inputs
    test_that("mixed rxode2/NONMEM EVID/data gives a warning", {
      mod <- rxode2({
        d1 <- exp(td1 + eta.d1)
        cl <- exp(tcl + eta.cl)
        d / dt(center) <- -cl / v * center
        dur(center) <- d1
        cp <- center / v
      })


      d <- structure(list(
        ID = c(1L, 1L, 1L), TIME = c(0, 0, 0.25),
        DV = c(0, 0.74, 2.84),
        AMT = c(319.992, 0, 0), EVID = c(101L, 0L, 0L),
        WT = c(79.6, 79.6, 79.6), RATE = c(-2, 0, 0)
      ),
      row.names = c(NA, 3L), class = "data.frame"
      )

      expect_warning(etTrans(d, mod), "'rate'")

      d <- structure(list(
        ID = c(1L, 1L, 1L), TIME = c(0, 0, 0.25),
        DV = c(0, 0.74, 2.84),
        AMT = c(319.992, 0, 0), EVID = c(101L, 0L, 0L),
        WT = c(79.6, 79.6, 79.6), DUR = c(-2, 0, 0)
      ),
      row.names = c(NA, 3L), class = "data.frame"
      )

      expect_warning(etTrans(d, mod), "'dur'")

      d <- structure(list(
        ID = c(1L, 1L, 1L), TIME = c(0, 0, 0.25),
        DV = c(0, 0.74, 2.84),
        AMT = c(319.992, 0, 0), EVID = c(101L, 0L, 0L),
        WT = c(79.6, 79.6, 79.6), SS = c(1, 0, 0),
        II = c(24, 0, 0)
      ),
      row.names = c(NA, 3L), class = "data.frame"
      )

      expect_warning(etTrans(d, mod), "'ss'")
    })

    mod <- rxode2("    x1(0) = x10\n    d/dt(x1) = a * x1\n    Volume = x1;\ncmt(Volume);\n\n    nlmixr_pred <- Volume")

    test_that("DV=NA; issue #106", {
      RawData2 <- data.frame(
        ID = c(1, 1, 1, 1, 2, 2, 2, 2),
        TIME = c(0, 3, 4, 5, 0, 3, 4, 5),
        DV = c(NA, 30, 80, 250, NA, 40, 150, 400)
      )

      dat1 <- etTrans(RawData2, mod)

      RawData2a <- data.frame(
        ID = c(1, 1, 1, 1, 2, 2, 2, 2),
        TIME = c(0, 3, 4, 5, 0, 3, 4, 5),
        DV = c(NA, 30, 80, 250, NA, 40, 150, 400),
        AMT = c(NA, NA, NA, NA, NA, NA, NA, NA)
      )

      dat1a <- etTrans(RawData2a, mod)

      RawData2b <- data.frame(
        ID = c(1, 1, 1, 1, 2, 2, 2, 2),
        TIME = c(0, 3, 4, 5, 0, 3, 4, 5),
        DV = c(NA, 30, 80, 250, NA, 40, 150, 400),
        AMT = c(0, 0, 0, 0, 0, 0, 0, 0)
      )

      dat1b <- etTrans(RawData2b, mod)


      RawData2c <- data.frame(
        ID = c(1, 1, 1, 1, 2, 2, 2, 2),
        TIME = c(0, 3, 4, 5, 0, 3, 4, 5),
        DV = c(NA, 30, 80, 250, NA, 40, 150, 400),
        AMT = c(1, 0, 0, 0, 1, 0, 0, 0)
      )
      dat1c <- etTrans(RawData2c, mod)

      expect_equal(dat1a$EVID, c(2L, 0L, 0L, 0L, 2L, 0L, 0L, 0L))
      expect_equal(dat1a$EVID, dat1b$EVID)
      expect_equal(dat1c$EVID, c(101L, 0L, 0L, 0L, 101L, 0L, 0L, 0L))
    })

    RawData3 <- data.frame(
      ID = c(1, 1, 1, 1, 2, 2, 2, 2),
      TIME = c(0, 3, 4, 5, 0, 3, 4, 5),
      DV = c(0, 30, 80, 250, 0, 40, 150, 400),
      EVID = c(2, 0, 0, 0, 2, 0, 0, 0)
    )

    dat2 <- etTrans(RawData3, mod)

    RawData4 <- data.frame(
      ID = c(1, 1, 1, 1, 2, 2, 2, 2),
      TIME = c(0, 3, 4, 5, 0, 3, 4, 5),
      DV = c(0, 30, 80, 250, 0, 40, 150, 400),
      EVID = c(2, 0, 0, 0, 2, 0, 0, 0),
      CMT = c(1, 0, 0, 0, 1, 0, 0, 0)
    )

    dat3 <- etTrans(RawData4, mod)

    RawData5 <- data.frame(
      ID = c(1, 1, 1, 1, 2, 2, 2, 2),
      TIME = c(0, 3, 4, 5, 0, 3, 4, 5),
      DV = c(0, 30, 80, 250, 0, 40, 150, 400),
      EVID = c(2, 0, 0, 0, 2, 0, 0, 0),
      CMT = c(2, 0, 0, 0, 2, 0, 0, 0)
    )

    dat4 <- etTrans(RawData5, mod)

    test_that("dat2=dat4", {
      expect_equal(as.data.frame(dat2), as.data.frame(dat4))
    })

    test_that("dat3 has evid w/amt 0", {
      expect_equal(dat3$EVID, c(101L, 2L, 0L, 0L, 0L, 101L, 2L, 0L, 0L, 0L))
      expect_equal(dat3$AMT, c(0, NA, NA, NA, NA, 0, NA, NA, NA, NA))
    })

    test_that("X(0) should be at time zero; see issue #105", {
      mod <- rxode2("    x1(0) = x10\n    d/dt(x1) = a * x1\n    Volume = x1;\ncmt(Volume);\n\n    nlmixr_pred <- Volume")

      rxSetIni0(FALSE)
      RawData2 <- data.frame(
        ID = c(1, 1, 1, 2, 2, 2),
        TIME = c(3, 4, 5, 3, 4, 5),
        DV = c(30, 80, 250, 40, 150, 400)
      )
      expect_warning(dat1 <- etTrans(RawData2, mod))

      expect_equal(dat1$TIME, RawData2$TIME)

      rxSetIni0(TRUE)
      dat1 <- etTrans(RawData2, mod)

      expect_equal(dat1$TIME, c(0, 3, 4, 5, 0, 3, 4, 5))
      expect_equal(dat1$EVID, c(9L, 0L, 0L, 0L, 9L, 0L, 0L, 0L))
    })

    rxSetIni0(TRUE)
    test_that("censoring checks", {
      mod <- rxode2("
a = 6
b = 0.6
d/dt(intestine) = -a*intestine
d/dt(blood)     = a*intestine - b*blood
")
      et <- eventTable()
      et$add.dosing(
        dose = 2 / 24, rate = 2, start.time = 0,
        nbr.doses = 10, dosing.interval = 1
      )
      et <- et %>% et(0, 24, by = 0.1)

      tmp <- et
      tmp$cens <- 0
      tmp$cens[1] <- 2

      expect_error(etTrans(tmp, mod))

      tmp <- et
      tmp$cens <- 0
      tmp$dv <- 3
      tmp$cens[2] <- 1

      ret <- suppressWarnings(etTrans(tmp, mod))
      expect_false(any(names(ret) == "CENS"))
      expect_equal(attr(class(ret), ".rxode2.lst")$censAdd, 0L)
      expect_equal(attr(class(ret), ".rxode2.lst")$limitAdd, 0L)

      tmp <- et
      tmp$cens <- 0
      tmp$dv[1] <- 2
      tmp$cens <- 0
      tmp$cens[1] <- 1

      ret <- etTrans(tmp, mod)
      expect_true(any(names(ret) == "CENS"))
      expect_equal(attr(class(ret), ".rxode2.lst")$censAdd, 1L)
      expect_equal(attr(class(ret), ".rxode2.lst")$limitAdd, 0L)

      tmp$limit <- 0

      ret <- etTrans(tmp, mod)
      expect_true(any(names(ret) == "CENS"))
      expect_true(any(names(ret) == "LIMIT"))
      expect_equal(attr(class(ret), ".rxode2.lst")$censAdd, 1L)
      expect_equal(attr(class(ret), ".rxode2.lst")$limitAdd, 1L)
    })

    test_that("rxode2 constant infusion taken to steady state", {
      trn1 <- etTrans(et(amt = 0, rate = 10, ss = 1), mod, keepDosingOnly = TRUE) %>% as.data.frame()
      expect_equal(structure(list(
        ID = structure(1L, class = "factor", .Label = "1"),
        TIME = 0, EVID = 10140L, AMT = 10, II = 0, DV = NA_real_
      ),
      class = "data.frame", row.names = c(NA, -1L)
      ), trn1)

      trn1 <- etTrans(et(amt = 0, rate = -1, ss = 1), mod, keepDosingOnly = TRUE) %>% as.data.frame()

      expect_equal(structure(list(
        ID = structure(1L, class = "factor", .Label = "1"),
        TIME = 0, EVID = 90140L, AMT = 0, II = 0, DV = NA_real_
      ),
      class = "data.frame", row.names = c(NA, -1L)
      ), trn1)
    })

    ## etTrans example from xgxr + nlmixr + ggpmx
    test_that("etTrans", {
      lst <- readRDS(test_path("test-etTrans-1.rds"))
      events2 <- lst$events
      events2 <- events2[, names(events2) != "CENS"]

      # suppressWarnings() is used on the outside because the rxSetIni0(FALSE)
      # warning only occurs once per session
      t0 <- suppressWarnings(etTrans(events2, rxode2(lst$object), FALSE, FALSE, FALSE, FALSE, NULL, character(0)))
      expect_s3_class(t0, "rxEtTran")

      t1 <- etTrans(events2, rxode2(lst$object), FALSE, FALSE, FALSE, TRUE, NULL, character(0))
      expect_s3_class(t1, "rxEtTran")
    })

    test_that("etTrans drop levels are correct", {

      dat <- readRDS(test_path("etTrans-drop.rds"))

      mod <- rxode2({
        lka <- log(0.1) # log Ka
        lv <- log(10) # Log Vc
        lcl <- log(4) # Log Cl
        lq <- log(10) # log Q
        lvp <- log(20) # Log Vp
        eta.ka <- 0
        eta.v <- 0.1
        eta.cl <- 0.1
        ka <- exp(lka + eta.ka)
        cl <- exp(lcl + eta.cl)
        v <- exp(lv + eta.v)
        q <- exp(lq)
        vp <- exp(lvp)
        cp <- linCmt()
      })

      # suppressWarnings() is used on the outside because the rxSetIni0(FALSE)
      # warning only occurs once per session
      suppressWarnings(expect_warning(expect_warning(
        tmp <- etTrans(dat, mod),
        regexp="while censoring is included"), regexp="IDs without observations"
        ))
      lvls <- c(
        "32", "33", "35", "36", "37", "40", "41", "42", "43", "47",
        "48", "49", "50", "51", "54", "55", "57", "59", "61", "62", "63",
        "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "74",
        "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", "85",
        "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96",
        "97", "98", "99", "100", "101", "102", "103", "104", "105", "106",
        "107", "108", "109", "110", "111", "112", "113", "114", "115",
        "116", "117", "118", "119", "120", "121", "122", "123", "124",
        "125", "126", "127", "128", "129", "130", "131", "132", "133",
        "134", "135", "136", "137", "138", "139", "140", "141", "142",
        "143", "144", "145", "146", "147", "148", "149", "150", "151",
        "152", "153", "154", "155", "156", "157", "158", "159", "160",
        "161", "162", "163", "164", "165", "166", "167", "168", "169",
        "170", "171", "172", "173", "174", "175", "176", "177", "178",
        "179", "180"
      )
      expect_equal(attr(class(tmp), ".rxode2.lst")$idLvl, lvls)
      expect_equal(levels(tmp$ID), lvls)
    })

    test_that("warfarin model", {
      warfarin <- nlmixr2data::warfarin

      mod <- rxode2({
        lka <- log(0.1) # log Ka
        lv <- log(10) # Log Vc
        lcl <- log(4) # Log Cl
        lq <- log(10) # log Q
        lvp <- log(20) # Log Vp
        eta.ka <- 0
        eta.v <- 0.1
        eta.cl <- 0.1
        ka <- exp(lka + eta.ka + sex + age + dvid)
        cl <- exp(lcl + eta.cl)
        v <- exp(lv + eta.v)
        q <- exp(lq)
        vp <- exp(lvp)
        sf <- (sex == "female")
        sm <- (sex == "male")
        d.cp <- (dvid == "cp")
        d.pca <- (dvid == "pca")
        cp <- linCmt()
      })

      t <- rxSolve(mod, warfarin, keep = c("sex", "age", "dvid"))

      expect_equal(sort(unique(t$sf)), c(0, 1))
      expect_equal(sort(unique(t$sm)), c(0, 1))
      expect_equal(sort(unique(t$d.cp)), c(0, 1))
      expect_equal(sort(unique(t$d.pca)), c(0, 1))

      expect_s3_class(t$sex, "factor")
      expect_s3_class(t$dvid, "factor")

      expect_equal(as.double((t$sex == "male") * 1), t$sm)
      expect_equal(as.double((t$sex == "female") * 1), t$sf)

      t <- rxSolve(mod, warfarin, addCov = TRUE)

      expect_equal(as.double((t$sex == "male") * 1), t$sm)
      expect_equal(as.double((t$sex == "female") * 1), t$sf)
      ## expect_equal(as.double((t$dvid == "cp") * 1), t$d.cp)
      ## expect_equal(as.double((t$pca == "pca") * 1), t$d.pca)

      warfarin$sex <- paste(warfarin$sex)

      t <- rxSolve(mod, warfarin, keep = c("sex", "age", "dvid"))

      expect_equal(sort(unique(t$sf)), c(0, 1))
      expect_equal(sort(unique(t$sm)), c(0, 1))
      expect_equal(sort(unique(t$d.cp)), c(0, 1))
      expect_equal(sort(unique(t$d.pca)), c(0, 1))

      expect_s3_class(t$sex, "factor")
      expect_s3_class(t$dvid, "factor")

      expect_equal(as.double((t$sex == "male") * 1), t$sm)
      expect_equal(as.double((t$sex == "female") * 1), t$sf)

      t <- rxSolve(mod, warfarin, addCov = TRUE)

      expect_equal(as.double((t$sex == "male") * 1), t$sm)
      expect_equal(as.double((t$sex == "female") * 1), t$sf)
    })
    test_that("phantom doses", {

      mod <- rxode2("
a = 6
b = 0.6
d/dt(intestine) = -a*intestine
d/dt(blood)     = a*intestine - b*blood
")

      d <- et(amt=3, evid=7, cmt=2) %>%
        et(1:10)

      tran <- etTrans(d, mod)

      expect_equal(tran$EVID[1], 250L)

    })

    test_that("non time varying covariates with evid=9", {

      rx <- rxode2({
        param(lkng, ltau, lec50, kmax, propErr, addErr, cp, tumor0)
        kng <- exp(lkng)
        tau <- exp(ltau)
        taulast <- tau
        ec50 <- exp(lec50)
        edrug <- kmax * cp/(ec50 + cp)
        tumor(0) <- tumor0
        d/dt(transit1) <- (edrug - transit1)/tau
        d/dt(transit2) <- (transit1 - transit2)/tau
        d/dt(transit3) <- (transit2 - transit3)/tau
        d/dt(transitlast) <- transit3/tau - transitlast/taulast
        d/dt(tumor) <- kng * tumor - transitlast * tumor
        rx_yj_ ~ 2
        rx_lambda_ ~ 1
        rx_low_ ~ 0
        rx_hi_ ~ 1
        rx_pred_f_ ~ tumor
        rx_pred_ ~ rx_pred_f_
        rx_r_ ~ (addErr)^2 + (rx_pred_f_)^2 * (propErr)^2
        ipredSim <- rxTBSi(rx_pred_, rx_lambda_, rx_yj_, rx_low_,
                           rx_hi_)
        sim <- rxTBSi(rx_pred_ + sqrt(rx_r_) * err.tumor, rx_lambda_,
                      rx_yj_, rx_low_, rx_hi_)
        dvid(5)
      })

      prepfit <- qs::qdeserialize(qs::base91_decode("un]\"BAAA@QRtHACAAAAAAAuWeBAABdk1kus^^d8Ah9}?=Z:alBMc4Iv(F\":C?hVBAMZRxwFfBB7IB.y6FTL)yFQA@v;KlgSH3Vn~rL/,{CP/ez~`.3>$Wj$rcy==//#}Pu?\"V(RgKtf1J3qQg/yI7*1]/WV=iegVmPs3?a\":kEMu~/*zmX#;E4`i@It`]Ouu[N]T8G3!4A.^j0<Zd;mY7D`P6Z|KTj_p?r7u[IPuyFRoUcL\"6u|(G_on6g9c{ZLJ[_gE^&47rbL(#6W{EA%hQUW!][2;k<}\"(4SB{!~>M%xmxwMU%ET6#~xvX9rH!;S53gbLTWY]Fcri\"]7\"|Z^W{xobiiTc~DLN_;.Itj(INGKCupDYxEA^!GzfHO=aDW&(I)z}0*mZD\"^b.O!QdY2rVRD;~Z*HB(]G_Dpj]*0A`]+7VoGDF@,vk>jx}tFI>MVOnZojuABN9Bt\"O~V[n6U[kn|W74&xR7CL(Skn:CA)NP`||hQ%w/i+&c8$#KxsFdb4,qI\"Fl&lLg,?$eh&s{`QxtwPWi$GX<[*<0{to@[:NAy}a=O`wedEA*Abqhz2bL2sfII3ZRJR#5q~:FeBW%/F<]`(?Q:c(qc,DZ_d.&|J(NW~Q4kz;Us(7e+Z0YGMdvf.%XRgD]FA2D10sl^KxuPXvXSm+p}ndVY!3`o}Iq+M;i~mLmr1In0~ymm]K2x9g9Ij.UkBOTriq+93<po9tNj@%%W#FzA+/MMb]k3YmXS*RdjE{?pjnr%q6}&81.2&#ni.Au{pL>#eKT9uaMmvF7L^aDwL?sj>s|}[XMQdEx(yS|vIDfwqH:YXc(EfrzuplGn3`|X=ObNnD%;3(ST3tWr^D+vDG=cjKk!^:5ZfpXK5/dqF@dW6+*lb~@\"*H_t@3rRG9w|kG1!SRghm{sUOcDQ?.gYd?c;:IC~RF6lEfd4X;I3+4Y&8.x0P[h8]Ffo)$Y=7)|08Vid5@2btxd4I[0>E7~+CCX~|{Ve0qf^aQ1M4vsQN<B6]~R{LE*vH*8[Q|b[`QO%/.,mr9d<+O/PqesyX).kR)a[.s^OsSJLRZgrwt?NA#EQf)p,+wjGm!8aRz9^SKrW5`PTsjH_EVK.eZ)T]k0Md!mRLU%+}K!{BB>[uCto@(_b&&NA0eV*R}~Tp<Ey:7|>#Kz+AL~%_Wu@J&>C__{[#o3Lmxh$%R;264SAA)O;,:*:p.s:z+=pot[NkrFE@[}VoBF=P<AI)v_tBGUZy4^}6MUL|glMtnz]K/D|qUE``3x])hpz{k$OXX}C4B%m,AwVR)[N\"d$i}eLLJ;bFzpavPm|k^Ef;$C|S+GBP;CdOQSE<>v:;Cj#YR]M@a{68?;s&4Y4Zd[D`znL|kp1n)}0+Q`(/LcQM42JIA}ZFH+9>#dQLs>JX?#GT<W+#YOo[zpESq=N.@3/bpfI)5wKpkIRY>_M&#wui|N/HWs/WyJ/CSn*BaX!:]**?VL`zPu?xA|.+kn)M[xCjLHQ@8A6HX^T4hXT7F~JQw])fR/^Y>YBD([_QsK;[iYPcbW2Z\"BA9q~@t^rOgPg95OnA01xfA00E8k)eQtyH#MzpQ$4|et<eI&o|Uhx|t19ZFY$DAw5Z<,6p7MQM*N07@b$w):77pmoP{iZ4KvtvMyKYw|?{al}MZNtX<:RTKFB?PAdg=76:~4(HH*s62mDm2K:Qw^(9,*ICI.`?HiTsl8KIYV0C!0%0S:JM6(#2A)w(y(<9\"?n`k{0V!yrmQChNo8WWL=/*eAA_+&bKol/+*eiJi*e2dIW!hi.)+RHLR9O}Z69<<eE;aD%#B0ql|30pbBs3Z^0(q$nWfGs|LD$erJR2r@>6W(eGPRmx0B?bGrM@DA31ha^UZWMe52Egm6nn3=lcs3[gq!vBvrKiTSf],3$wjk7^`BPE#{kPh.CcbM*h:9+XPRfGx1P^)X!utw{#;**|LZtNdAwx\"IC?m[]Wil9!m3Tx}{Xx@9;`=+ODLPp)w]zc\"D.rl7/d:ismtgiesu]/CF[9v7ICZ:U?wky`@wQn])q@|Dd#IfQbwAQF`7]{f;hJCK=Vv)N6M4x4>G.u%o7I5fCay=\"^,?M!600bO\"MBTL9~4}eDkY:YkGc6G9oMzo3<9[Ye`GFvPyXiw5$Z]*PByzRza9ik5LJ;}@p;JH}+.,1om5gxAkHdn%`#kbKMJuiDOa#*M!h.2dUje501>,MIW$207SwS$M~FCMaG:`&l?j<c+ZMyFt+u:CpBWOZL)$@WywmY|LCGPdVv=>K]e\"!>HG]Tw0Xbm7SOIjt.B%cH5Ewy_YgfI6(#$I]1Q4Jte1:^W%XdqR4C#OV#._I`G$k|#>NfxcZp!Y]YnX~yZ0nx),%jfE\"u7yS,:y&oJV`;4nMg0g[a]Tidjt|y[*~_8H;*X61T/1kv9x$f/3N?~>xL$mO^0Nk6W|U[wD"))

      trans <- etTrans(prepfit,rx)

      expect_true(all(names(trans) != "tumor0"))
      expect_true(any(names(attr(class(trans), ".rxode2.lst")$cov1) =="tumor0"))

    })
  }
})
