rxTest({
  if (!.Call(`_rxode2_isIntel`)) {
    test_that("ui props", {

      fun_odes  <- function() {
        description <- "Dupilumab PK model (Kovalenko 2020)"
        reference <- "Kovalenko P, Davis JD, Li M, et al. Base and Covariate Population Pharmacokinetic Analyses of Dupilumab Using Phase 3 Data. Clinical Pharmacology in Drug Development. 2020;9(6):756-767. doi:10.1002/cpdd.780"
        # Model 1 from table 1 and supplementary Table 2 in the publication and its
        # supplement.
        covariateData <-
          list(
            WT = "Body weight in kg"
          )
        ini({
          lvc <- log(2.48); label("central volume (L)")
          lke <- log(0.0534); label("elimination rate (1/d)")
          lkcp <- log(0.213); label("central-to-peripheral rate (1/d)")
          Mpc <- 0.686; label("ratio of kcp and kpc (kpc is peripheral to central rate with units of 1/d)")
          lka <- log(0.256); label("absorption rate (1/d)")
          lMTT <- log(0.105); label("mean transit time (d)")
          lVm <- log(1.07); label("maximum target-mediated rate of elimination (mg/L/d)")
          Km <- fixed(0.01); label("Michaelis-Menten constant (mg/L)")
          lfdepot <- log(0.643); label("Bioavailability (fraction)")
          e_wt_vc <- 0.711; label("Exponent of weight on central volume (unitless)")

          etalvc ~ 0.192
          etalke ~ 0.285
          etalka ~ 0.474
          etalvm ~ 0.236
          etamtt ~ 0.525 # etamtt is assumed to be on log-scale MTT to prevent negative values; this is a difference relative to Supplementary Table 2

          cppropSd <- 0.15; label("Proportional residual error (fraction)")
          cpaddSd <- fixed(0.03); label("Additive residual error (mg/L)")
        })
        model({
          # Weight normalization to 75 kg is assumed based on prior publication.  It
          # is not specified in the current publication:
          # Kovalenko P, DiCioccio AT, Davis JD, et al. Exploratory Population PK
          # Analysis of Dupilumab, a Fully Human Monoclonal Antibody Against
          # IL-4Ralpha, in Atopic Dermatitis Patients and Normal Volunteers. CPT
          # Pharmacometrics Syst Pharmacol. 2016;5(11):617-624. doi:10.1002/psp4.12136
          vc <- exp(lvc + etalvc)*(WT/75)^e_wt_vc
          ke <- exp(lke + etalke)
          kcp <- exp(lkcp)
          ka <- exp(lka + etalka)
          MTT <- exp(lMTT + etamtt)
          Vm <- exp(lVm + etalvm)

          # Derived parameters
          kpc <- kcp/Mpc
          ktr <- (3 + 1)/MTT

          d/dt(depot) <- -ktr*depot
          d/dt(transit1) <- ktr*(depot - transit1)
          d/dt(transit2) <- ktr*(transit1 - transit2)
          d/dt(transit3) <- ktr*transit2 - ka*transit3
          # Linear and Michaelis-Menten clearance
          d/dt(central) <-                 ka*transit3 - ke*central - kcp*central + kpc*periph - central*(Vm/(Km + central/vc))
          d/dt(periph) <-                                             kcp*central - kpc*periph

          f(depot) <- exp(lfdepot)
          # No unit conversion is required to change mg/L (dosing amount/central
          # volume unit) to mg/L (measurement unit)
          Cc <- central/vc
          Cc ~ add(cpaddSd) + prop(cppropSd)
        })
      }

      fun_ana <- function() {
        description <- "Two compartment PK model with linear clearance for average monoclonal antibodies (Davda 2014)"
        reference <- "Davda JP, Dodds MG, Gibbs MA, Wisdom W, Gibbs JP. A model-based meta-analysis of monoclonal antibody pharmacokinetics to guide optimal first-in-human study design. MAbs. 2014;6(4):1094-1102. doi:10.4161/mabs.29095"
        ini({
          lfdepot <- log(0.744) ; label("Subcutaneous bioavailability (fraction)")
          lka <- log(0.282) ; label("Absorption rate (Ka, 1/day)")
          lcl <- log(0.200) ; label("Clearance (CL, L/day)")
          lv  <- log(3.61) ; label("Central volume of distribution (V, L)")
          lvp  <- log(2.75) ; label("Peripheral volume of distribution (Vp, L)")
          lq  <- log(0.747) ; label("Intercompartmental clearance (Q, L/day)")

          allocl <- 0.865 ; label("Allometric exponent on clearance and intercompartmental clearance (unitless)")
          allov <- 0.957 ; label("Allometric exponent on volumes of distribution (unitless)")

          etafdepot ~ 0
          etaka ~ 0.416
          etacl + etav + etavp ~ c(0.0987,
                                   0.0786, 0.116,
                                   0.0377, 0.0619, 0.0789)
          etaq ~ 0.699

          prop.err <- 0.144 ; label("Proportional residual error (fraction)")
        })
        model({
          # WT is body weight in kg
          fdepot <- exp(lfdepot + etafdepot)
          ka <- exp(lka + etaka)
          wtnorm <- log(WT/70)
          cl <- exp(lcl + allocl*wtnorm + etacl)
          q  <- exp(lq + allocl*wtnorm + etaq)
          v <- exp(lv + allov*wtnorm + etav)
          vp <- exp(lvp + allov*wtnorm + etavp)
          Cc <- linCmt()

          f(depot) <- fdepot  # Units are dosing units/L (typically mg/L = ug/mL)
          Cc ~ prop(prop.err)

        })
      }

      obj_ana = rxode2(fun_ana)
      obj_odes = rxode2(fun_odes)

      expect_equal(obj_ana$props,
                   list(pop = c("lfdepot", "lka", "lcl", "lv", "lvp", "lq", "allocl", "allov"),
                        resid = "prop.err",
                        group = list(id = c("etafdepot", "etaka", "etacl", "etav", "etavp", "etaq")),
                        linCmt = TRUE,
                        cmt = c("depot", "central"),
                        output = list(primary = c("fdepot", "ka", "wtnorm", "cl", "q", "v", "vp"),
                                      secondary = character(0),
                                      endpoint = "Cc",
                                      state = c("depot", "central")),
                        cmtProp=data.frame(Compartment = "depot", Property = "f")))

      expect_equal(obj_odes$props,
                   list(pop = c("lvc", "lke", "lkcp", "Mpc", "lka", "lMTT", "lVm", "Km", "lfdepot", "e_wt_vc"),
                        resid = c("cppropSd", "cpaddSd"),
                        group = list(id = c("etalvc", "etalke", "etalka", "etalvm", "etamtt")),
                        linCmt = FALSE,
                        cmt = c("depot", "transit1", "transit2", "transit3", "central", "periph"),
                        output = list(primary = c("vc", "ke", "kcp", "ka", "MTT", "Vm", "kpc"),
                                      secondary = "ktr",
                                      endpoint = "Cc",
                                      state = c("depot", "transit1", "transit2", "transit3", "central", "periph")),
                        cmtProp=data.frame(Compartment="depot", Property="f")))


      fun_ana2 <- function() {
        description <- "Two compartment PK model with linear clearance for average monoclonal antibodies (Davda 2014)"
        reference <- "Davda JP, Dodds MG, Gibbs MA, Wisdom W, Gibbs JP. A model-based meta-analysis of monoclonal antibody pharmacokinetics to guide optimal first-in-human study design. MAbs. 2014;6(4):1094-1102. doi:10.4161/mabs.29095"
        ini({
          lfdepot <- log(0.744) ; label("Subcutaneous bioavailability (fraction)")
          lka <- log(0.282) ; label("Absorption rate (Ka, 1/day)")
          lcl <- log(0.200) ; label("Clearance (CL, L/day)")
          lv  <- log(3.61) ; label("Central volume of distribution (V, L)")
          lvp  <- log(2.75) ; label("Peripheral volume of distribution (Vp, L)")
          lq  <- log(0.747) ; label("Intercompartmental clearance (Q, L/day)")

          allocl <- 0.865 ; label("Allometric exponent on clearance and intercompartmental clearance (unitless)")
          allov <- 0.957 ; label("Allometric exponent on volumes of distribution (unitless)")

          etafdepot ~ 0
          etaka ~ 0.416
          etacl + etav + etavp ~ c(0.0987,
                                   0.0786, 0.116,
                                   0.0377, 0.0619, 0.0789)
          etaq ~ 0.699

          prop.err <- 0.144 ; label("Proportional residual error (fraction)")
        })
        model({
          # WT is body weight in kg
          fdepot <- exp(lfdepot + etafdepot)
          ka <- exp(lka + etaka)
          wtnorm <- log(WT/70)
          cl <- exp(lcl + allocl*wtnorm + etacl)
          q  <- exp(lq + allocl*wtnorm + etaq)
          v <- exp(lv + allov*wtnorm + etav)
          vp <- exp(lvp + allov*wtnorm + etavp)
          linCmt() ~ prop(prop.err)
          f(depot) <- fdepot  # Units are dosing units/L (typically mg/L = ug/mL)

        })
      }

      tmp <- fun_ana2()

      expect_equal(tmp$props,
                   list(pop = c("lfdepot", "lka", "lcl", "lv", "lvp", "lq", "allocl", "allov"),
                        resid = "prop.err",
                        group = list(id = c("etafdepot", "etaka", "etacl", "etav", "etavp", "etaq")),
                        linCmt = TRUE,
                        cmt = c("depot", "central"),
                        output = list(primary = c("fdepot", "ka", "wtnorm", "cl", "q", "v", "vp"),
                                      secondary = character(0),
                                      endpoint = character(0),
                                      state = c("depot", "central")),
                        cmtProp=data.frame(Compartment = "depot", Property = "f")))

    })

    test_that("linCmt single compartment parses correctly", {

      mod <- function() {
        ini({
          cl <- 0.1
          vc <- 4
          err.sd <- 0.1
        })
        model({
          Cc <- linCmt(cl, vc)
          Cc ~ add(err.sd)
        })
      }

      mod <- mod()

      expect_error(mod$props, NA)

      expect_equal(mod$props,
                   list(pop = c("cl", "vc"),
                        resid = "err.sd",
                        group = structure(list(), names = character(0)),
                        linCmt = TRUE,
                        cmt = "central",
                        output = list(primary = character(0),
                                      secondary = character(0),
                                      endpoint = "Cc",
                                      state = "central"),
                        cmtProp=NULL))

    })

    test_that("state based endpoint", {

      oncology_sdm_lobo_2002 <- function() {
        description <- "Signal transduction model for delayed concentration effects on cancer cell growth"
        reference <- "Lobo ED, Balthasar JP. Pharmacodynamic modeling of chemotherapeutic effects: Application of a transit compartment model to characterize methotrexate effects in vitro. AAPS J. 2002;4(4):212-222. doi:10.1208/ps040442"
        depends<-"Cc"
        units<-list(time="hr")
        # Values for lkng, ltau, lec50, and kmax are for methotrexate from Lobo 2002,
        # Table 2.  propErr and addErr are added as reasonable values though not from
        # Lobo 2002 where no value is apparent in the paper.
        ini({
          lkng <- log(0.02) ; label("Cell net growth rate (growth minus death) (1/hr)")
          ltau <- log(34.1) ; label("Mean transit time of each transit compartment (hr)")
          lec50 <- log(0.1) ; label("Drug concentration reducing the cell growth by 50% (ug/mL)")
          kmax <- 0.29 ; label("Maximum drug-related reduction in cell growth (1/hr)")
          tumorVolpropSd <- c(0, 0.3) ; label("Proportional residual error (fraction)")
          tumorVoladdSd <- c(0, 50, 1000) ; label("Additive residual error (tumor volume units)")
        })
        model({
          # Cc is the drug concentration
          kng <- exp(lkng)
          tau <- exp(ltau)
          ec50 <- exp(lec50)
          drugEffectTumorVol <- kmax*Cc/(ec50 + Cc)
          tumorVol(0) <- tumorVol0
          d/dt(tumorVol) <- kng*tumorVol - transit4*tumorVol
          d/dt(transit1) <- (drugEffectTumorVol - transit1)/tau
          d/dt(transit2) <- (transit1 - transit2)/tau
          d/dt(transit3) <- (transit2 - transit3)/tau
          d/dt(transit4) <- (transit3 - transit4)/tau
          tumorVol ~ prop(tumorVolpropSd) + add(tumorVoladdSd)
        })
      }

      rx_obj = rxode2::rxode2(oncology_sdm_lobo_2002)

      expect_equal(rx_obj$props$output$endpoint, "tumorVol")
    })
  }
})
