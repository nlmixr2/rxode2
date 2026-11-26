PK_2cmt_no_depot <- function() {
  description <- "Two compartment PK model with linear clearance using differential equations"
  reference <- "nlmixr2lib template"
  units <- list(time = "time_unit", dosing = "dose_unit", concentration = "conc_unit/vol_unit")
  # Issue #482: what each ODE state holds, in what amount units, in what
  # biological matrix. analyte/specimen proposed by a local model from the
  # model description; units derived from the units block. verified = FALSE
  # means NOT checked against the source paper.
  compartmentData <- list(
    central = list(analyte = "drug", units = NA_character_, specimen = "plasma", verified = FALSE),
    peripheral1 = list(analyte = "drug", units = NA_character_, specimen = "plasma", verified = FALSE)
  )

  ini({
    lcl <- 1 ; label("Clearance (CL)")
    lvc  <- 3 ; label("Central volume of distribution (V)")
    lvp  <- 5 ; label("Peripheral volume of distribution (Vp)")
    lq  <- 0.1 ; label("Intercompartmental clearance (Q)")
    propSd <- 0.5 ; label("Proportional residual error (fraction)")
  })
  model({
    cl <- exp(lcl)
    vc <- exp(lvc)
    vp <- exp(lvp)
    q  <- exp(lq)

    kel <- cl/vc
    k12 <- q/vc
    k21 <- q/vp


    d/dt(central) <-  - kel*central - k12*central + k21*peripheral1
    d/dt(peripheral1) <- k12*central - k21*peripheral1
    Cc <- central / vc

    Cc ~ prop(propSd)
  })
}
