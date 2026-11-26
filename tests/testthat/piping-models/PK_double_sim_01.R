PK_double_sim_01 <- function() {
  description <- "PK double absorption model with simultaneous zero order and first order absorptions"
  reference <- "nlmixr2lib template"
  units <- list(time = "time_unit", dosing = "dose_unit", concentration = "conc_unit/vol_unit")
  # Issue #482: what each ODE state holds, in what amount units, in what
  # biological matrix. analyte/specimen proposed by a local model from the
  # model description; units derived from the units block. verified = FALSE
  # means NOT checked against the source paper.
  compartmentData <- list(
    depot1 = list(analyte = "drug", units = NA_character_, specimen = "administration site", verified = FALSE),
    depot2 = list(analyte = "drug", units = NA_character_, specimen = "administration site", verified = FALSE),
    central = list(analyte = "drug", units = NA_character_, specimen = "plasma", verified = FALSE)
  )

  ini({
    tk01 <- 0.4 ; label("Zero order absorption rate from first site (K01)")
    lka2 <- 0.45 ; label("First order Absorption rate (Ka)")
    lcl <- 1 ; label("Clearance (CL)")
    lvc  <- 3 ; label("Central volume of distribution (V)")
    propSd <- 0.5 ; label("Proportional residual error (fraction)")
    lgfdepot1 <- logit(0.8); label("Logit-scale fraction of dose entering first depot (depot1)")
    ltlag <- log(9);          label("Log lag time before second depot (depot2) begins releasing (time units)")
  })
  model({
    k01 <- exp(tk01)
    ka2 <- exp(lka2)
    cl <- exp(lcl)
    vc <- exp(lvc)
    fdepot1 <- expit(lgfdepot1)
    alag <- exp(ltlag)

    kel <- cl/vc

    d/dt(depot1) <- -k01
    f(depot1) <- fdepot1
    d/dt(depot2) <- -ka2*depot2
    lag(depot2) <- alag
    f(depot2) <- 1-fdepot1
    d/dt(central) <-  k01+ka2*depot2-kel*central

    Cc <- central / vc

    Cc ~ prop(propSd)
  })
}
