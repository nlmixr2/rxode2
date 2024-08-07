# Individual keep AGE==AGE2
if (!.Call(`_rxode2_isIntel`)) {

  test_that("Make sure the keep gives the right values", {
    TVQ <- 4
    TVV3 <- 7
    dat <- data.frame(AGE = c(20, 30), ID = c(10, 20)) %>%
      dplyr::mutate(id = seq(from = 1, to = dplyr::n())) %>%
      dplyr::rename(NMID = ID)


    par.tab <- data.frame(
      ThetaKa = c(0.7, 0.9),
      ThetaCl = c(4.6, 5.4),
      ThetaV2 = c(7, 8),
      ID = c(10, 20)
    ) %>%
      dplyr::mutate(id = seq(from = 1, to = dplyr::n())) %>%
      dplyr::mutate(ThetaQ = TVQ, ThetaV3 = TVV3) %>%
      dplyr::rename(NMID = ID)

    tabtot <- dat %>%
      dplyr::left_join(., par.tab, by = c("NMID", "id"))

    mod1 <- rxode2({
      ## PK parameters
      CL <- exp(ThetaCl)
      KA <- exp(ThetaKa)
      V2 <- exp(ThetaV2)
      Q <- exp(ThetaQ)
      V3 <- exp(ThetaV3)

      K20 <- CL / V2
      K23 <- Q / V2
      K32 <- Q / V3

      CP <- A2 / V2

      ##
      d / dt(A1) <- -KA * A1
      d / dt(A2) <- KA * transit3 - K23 * A2 + K32 * A3 - K20 * A2
      d / dt(A3) <- K23 * A2 - K32 * A3

      d / dt(transit1) <- KA * A1 - KA * transit1
      d / dt(transit2) <- KA * transit1 - KA * transit2
      d / dt(transit3) <- KA * transit2 - KA * transit3

      f(A1) <- 1

      d / dt(AUC) <- CP
      A1(0) <- 0
      A2(0) <- 0
      A3(0) <- 0

      AGE2 <- AGE
    })

    NSubj <- length(tabtot$id)

    dose_ref <- 8000 ##

    ev_ref <- eventTable() %>%
      et(dose = dose_ref / 1000, time = seq(0, 24, 24)) %>%
      et(id = 1:NSubj) %>%
      add.sampling(seq(0, 24, 1)) %>%
      dplyr::mutate(DOSE = dose_ref) %>%
      dplyr::group_by(id) %>%
      tidyr::fill(DOSE, .direction = "downup") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Cycle = dplyr::case_when(
        time <= 12 ~ 1, #
        time >= 12 ~ 2, #
        TRUE ~ 0
      )) %>%
      dplyr::as_tibble()

    ev_ref <- ev_ref %>%
      dplyr::left_join(., tabtot, by = "id") %>%
      dplyr::as_tibble()

    PK.ev_ref2 <- rxSolve(mod1,
                          events = ev_ref, cores = 2,
                          seed = 123, addCov = TRUE, keep = c("Cycle", "AGE")
                          )

    expect_equal(PK.ev_ref2$AGE, PK.ev_ref2$AGE2)
  })

  test_that("rxSolve 'keep' maintains character output (#190/#622)", {

    one.cmt <- function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }

    d <- nlmixr2data::theo_sd
    d$SEX <- ifelse(d$ID < 7, "M", "F")
    d$fSEX <- factor(d$SEX)
    d$oSEX <- d$fSEX
    class(d$oSEX) <- c("ordered", "factor")
    d$iSEX <- as.integer(d$fSEX)
    d$lSEX <- as.logical(d$iSEX == 1)
    d$dSEX <- d$iSEX + 0.5
    library(units)
    d$uSEX <- set_units(d$dSEX, kg)
    d$eSEX <- lapply(d$SEX, function(e) {
      str2lang(e)
    })

    sim <- rxSolve(one.cmt, events = d, keep = c("SEX", "fSEX", "iSEX", "dSEX", "oSEX", "uSEX", "lSEX"))

    expect_type(sim$SEX, "character")
    expect_s3_class(sim$fSEX, "factor")
    expect_equal(levels(sim$fSEX), c("F", "M"))
    expect_type(sim$iSEX, "integer")
    expect_type(sim$dSEX, "double")
    expect_true(inherits(sim$oSEX, "ordered"))
    expect_true(inherits(sim$uSEX, "units"))
    expect_true(is.logical(sim$lSEX))

    d <- nlmixr2data::theo_sd
    d$SEX <- ifelse(d$ID < 7, "M", "F")
    d$SEX[4] <- NA_character_
    d$fSEX <- factor(d$SEX)
    d$fSEX[4] <- NA_integer_
    d$iSEX <- as.integer(d$fSEX)
    d$iSEX[4] <- NA_integer_
    d$lSEX <- as.logical(d$iSEX == 1)
    d$lSEX[4] <- NA
    d$dSEX <- d$iSEX + 0.5
    d$eSEX <- lapply(d$SEX, function(e) {
      str2lang(e)
    })

    sim <- rxSolve(one.cmt, events = d, keep = c("SEX", "fSEX", "iSEX", "dSEX", "lSEX"))

    expect_true(is.na(d$SEX[4]))
    expect_true(is.na(d$fSEX[4]))
    expect_true(is.na(d$iSEX[4]))
    expect_true(is.na(d$lSEX[4]))

    expect_error(rxSolve(one.cmt, events = d, keep = c("eSEX")))

  })

  test_that("rxSolve 'keep' does not crash and keeps correct values #756", {

    qs <- test_path("keep-756.qs")
    skip_if_not(file.exists(qs), "Test file not found")

    d <- qs::qread(qs)

    mod <- function() {
      ini({
        ka_anon1 <- 1
        f_anon1 <- 0.8
        vc_anon1 <- 3
        hl_anon1 <- 5
      })
      model({
        mw_anon1 <- 55000
        mw_convert_anon1 <- 1 / mw_anon1 * 1e3
        kel_anon1 <- log(2)/(hl_anon1/60/24)
        kel_target <- log(2)/1.2
        kform_target <- 1.4*kel_target
        kd_anon1_target_umolL <- 1.5/1000
        d/dt(depot_anon1) <- -ka_anon1*depot_anon1
        d/dt(central_anon1) <- ka_anon1*depot_anon1 - kel_anon1*central_anon1
        central_anon1_umolL <- central_anon1/vc_anon1*mw_convert_anon1
        totalconc <- central_anon1_umolL + central_target + kd_anon1_target_umolL
        bound_umolL <- (totalconc - sqrt(totalconc^2 - 4*central_anon1_umolL*central_target))/2
        free_central_target <- central_target - bound_umolL
        d/dt(central_target) <- kform_target - kel_target*free_central_target - kel_anon1*bound_umolL - 1.1 * (1.3*central_target - peripheral_target)
        d/dt(peripheral_target) <- 1.1 * (1.3*central_target - peripheral_target)
        d/dt(cleared_amount_bound_anon1_umol) <- kel_anon1*bound_umolL
        f(depot_anon1) <- f_anon1
        central_target(0) <- 1.4
        peripheral_target(0) <- 1.4*1.3
      })
    }

    expect_error(rxSolve(mod, d, keep="target_name"), NA)

    tmp <- rxSolve(mod, d, keep="target_name", addDosing=FALSE)

    expect_false(any(is.na(tmp$target_name)))

    tmp <- rxSolve(mod, d, keep="target_name", addDosing=TRUE, keepInterpolation="na")

    expect_true(any(is.na(tmp$target_name)))

    tmp <- rxSolve(mod, d, keep="target_name", addDosing=TRUE, keepInterpolation="locf")

    expect_false(any(is.na(tmp$target_name)))

    tmp <- rxSolve(mod, d, keep="target_name", addDosing=TRUE, keepInterpolation="nocb")

    expect_false(any(is.na(tmp$target_name)))


    ## print(head(tmp[,c("id", "amt", "target_name")]))

    et <- etTrans(d, mod, keep="target_name")
    et2 <- attr(class(et), ".rxode2.lst")
    class(et2) <- NULL

    expect_equal(length(et2$keepL$keepL[[1]]),
                 length(et$ID))

    class(et) <- "data.frame"
    et <- cbind(et, k=et2$keepL$keepL[[1]])

    d2 <- d %>% dplyr::filter(EVID==1) %>% dplyr::select(ID, TIME) %>%
      dplyr::mutate(i=1)

    # only variables in the dataset are considered NA
    expect_true(merge(et, d2, all.x=TRUE) %>%
                  dplyr::filter(i!= 1) %>% dplyr::pull(k) %>% all(is.na(.)))

    ## s <- rxSolve(mod, d, keep="target_name")

  })
}
