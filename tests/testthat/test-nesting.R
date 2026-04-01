rxTest({
  test_that("nesting works for single case #842", {

    library(dplyr)
    rxWithSeed(1234, {
      #Set model code
      model <-
        RxODE({
          #Parameters
          KA <- KA * exp(eta.KA)
          Fa <- Fa * exp(eta.Fa + iov.Fa) # IIV and IOV on bioavailability
          #Initial conditions
          A2(0) = 0
          #Model
          d/dt(DEPOT) = -KA * DEPOT
          f(DEPOT) = Fa
          d/dt(A2) = KA * DEPOT - (CL/V)*A2
          Cp = A2/V * 1000
        })

      #Define thetas
      theta <- c(KA = 0.925,   # Absorption rate constant (hr-1)
                 V  = 0.431,   # Central volume (mL)
                 CL = 0.137,  # Central clearance (mL/hr)
                 Fa = 0.134)

      #Define omegas
      omega <- lotri(lotri(eta.KA ~ 0.293^2,
                           eta.Fa ~ 0.0836^2) | id(nu=20),
                     lotri(iov.Fa ~ 0.767^2) | occ(nu=10))

      #Define event table
      ev <- et(amount.units = "mg", time.units = "hours") |>
        et(amt = 30*0.25 , cmt = "DEPOT", addl = 9, ii = 12) |> #Dose is 30mg/kg, multiply by average rat weight of 250 grams
        et(0,120) |>
        et(id = 1:20) |>
        mutate(occ = ceiling(as.numeric(time) / 12)) |> # 10 occasions in total
        mutate(occ = ifelse(occ == 0, 1, occ))

      expect_error(rxSolve(object=model, params=theta, events=ev, omega=omega, sigma=NULL), NA)

      results <- rxSolve(object=model, params=theta, events=ev, omega=omega, sigma=NULL)

      expect_equal(names(results$params),
                   c("id", "iov.Fa(occ==1)", "iov.Fa(occ==2)", "iov.Fa(occ==3)",
                     "iov.Fa(occ==4)", "iov.Fa(occ==5)", "iov.Fa(occ==6)", "iov.Fa(occ==7)",
                     "iov.Fa(occ==8)", "iov.Fa(occ==9)", "iov.Fa(occ==10)", "KA",
                     "eta.KA", "Fa", "eta.Fa", "CL", "V"))

    })


  })
})
