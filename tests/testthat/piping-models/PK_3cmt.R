PK_3cmt <- function() {
  description <- "Three compartment PK model with linear clearance"
  reference <- "nlmixr2lib template"
  units <- list(time = "time_unit", dosing = "dose_unit", concentration = "conc_unit/vol_unit")
  # Issue #482: what each ODE state holds, in what amount units, in what
  # biological matrix. analyte/specimen proposed by a local model from the
  # model description; units derived from the units block. verified = FALSE
  # means NOT checked against the source paper.
  compartmentData <- list(
    depot = list(analyte = "drug", units = NA_character_, specimen = "administration site", verified = FALSE),
    central = list(analyte = "drug", units = NA_character_, specimen = "plasma", verified = FALSE)
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
    q   <- exp(lq)
    q2  <- exp(lq2)

    Cc <- linCmt()
    Cc ~ prop(propSd)
  })
}
