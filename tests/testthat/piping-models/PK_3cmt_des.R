PK_3cmt_des <- function() {
  description <- "Three compartment PK model with linear clearance using differential equations"
  reference <- "nlmixr2lib template"
  units <- list(time = "time_unit", dosing = "dose_unit", concentration = "conc_unit/vol_unit")
  # Issue #482: what each ODE state holds, in what amount units, in what
  # biological matrix. analyte/specimen proposed by a local model from the
  # model description; units derived from the units block. verified = FALSE
  # means NOT checked against the source paper.
  compartmentData <- list(
    depot = list(analyte = "drug", units = NA_character_, specimen = "administration site", verified = FALSE),
    central = list(analyte = "drug", units = NA_character_, specimen = "plasma", verified = FALSE),
    peripheral1 = list(analyte = "drug", units = NA_character_, specimen = "plasma", verified = FALSE),
    peripheral2 = list(analyte = "drug", units = NA_character_, specimen = "plasma", verified = FALSE)
  )

  ini({
    lka <- 0.45 ; label("Absorption rate (Ka)")
    lcl <- 1 ; label("Clearance (CL)")
    lvc  <- 3 ; label("Central volume of distribution (V)")
    lvp  <- 5 ; label("Peripheral volume of distribution (Vp)")
    lvp2  <- 8 ; label("Second peripheral volume of distribution (Vp2)")
    lq  <- 0.1 ; label("Intercompartmental clearance (Q)")
    lq2  <- 0.5 ; label("Second intercompartmental clearance (Q2)")
    propSd <- 0.5 ; label("Proportional residual error (fraction)")
  })
  model({
    ka <- exp(lka)
    cl <- exp(lcl)
    vc <- exp(lvc)
    vp <- exp(lvp)
    vp2 <- exp(lvp2)
    q  <- exp(lq)
    q2  <- exp(lq2)

    kel <- cl/vc
    k12 <- q/vc
    k21 <- q/vp
    k13 <- q2/vc
    k31 <- q2/vp2

    d/dt(depot) <- -ka*depot
    d/dt(central) <-  ka*depot - kel*central - k12*central + k21*peripheral1 - k13*central + k31*peripheral2
    d/dt(peripheral1) <- k12*central - k21*peripheral1
    d/dt(peripheral2) <- k13*central - k31*peripheral2
    Cc <- central / vc

    Cc ~ prop(propSd)
  })
}
