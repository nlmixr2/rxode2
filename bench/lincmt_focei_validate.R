## Phase 4: FOCEi-level validation of forward-mode AD in linCmtB
## Fits the same model with linCmtSensType='AD' (forward-mode fvar)
## and 'ADr' (reverse-mode, escape hatch) and compares obj/estimates.
##
## Requires: nlmixr2, nlmixr2est, nlmixr2data
library(nlmixr2)
library(nlmixr2data)

theo <- theo_sd

## ---------- 1-cmt oral linCmt model ----------------------------------------
m1cmt <- function() {
  ini({
    tka  <- log(1.57)
    tcl  <- log(2.72)
    tv   <- log(31.07)
    eta.ka ~ 0.6
    eta.cl ~ 0.09
    eta.v  ~ 0.1
    add.err <- 0.7
  })
  model({
    ka  <- exp(tka + eta.ka)
    cl  <- exp(tcl + eta.cl)
    v   <- exp(tv  + eta.v)
    linCmt() ~ add(add.err)
  })
}

fitFOCEi <- function(model, data, sensType) {
  nlmixr2(model, data, "focei",
          control = foceiControl(print = 0,
                                 maxOuterIterations = 100,
                                 maxInnerIterations = 300,
                                 rxControl = rxode2::rxControl(linCmtSensType = sensType)))
}

cat("\n=== 1-cmt oral: forward AD ('AD') ===\n")
fit_fwd <- fitFOCEi(m1cmt, theo, "AD")

cat("\n=== 1-cmt oral: reverse AD ('ADr') ===\n")
fit_rev <- fitFOCEi(m1cmt, theo, "ADr")

cat("\n--- Objective function comparison ---\n")
cat("Forward (AD): ", fit_fwd$objective, "\n")
cat("Reverse (ADr):", fit_rev$objective, "\n")
cat("Delta:        ", abs(fit_fwd$objective - fit_rev$objective), "\n")

cat("\n--- Parameter estimate comparison ---\n")
params_fwd <- fit_fwd$parFixedDf[, c("Estimate", "SE")]
params_rev <- fit_rev$parFixedDf[, c("Estimate", "SE")]
rownames(params_fwd) <- rownames(fit_fwd$parFixedDf)
rownames(params_rev) <- rownames(fit_rev$parFixedDf)
cat("Forward:\n"); print(params_fwd)
cat("Reverse:\n"); print(params_rev)

max_est_diff <- max(abs(params_fwd$Estimate - params_rev$Estimate) /
                      (abs(params_rev$Estimate) + 1e-10))
cat(sprintf("Max relative difference in estimates: %.2e\n", max_est_diff))

## ---------- 2-cmt oral linCmt model ----------------------------------------
m2cmt <- function() {
  ini({
    tka  <- log(1.57)
    tcl  <- log(2.72)
    tv   <- log(31.07)
    tq   <- log(1.0)
    tvp  <- log(20.0)
    eta.cl ~ 0.09
    eta.v  ~ 0.1
    add.err <- 0.7
  })
  model({
    ka  <- exp(tka)
    cl  <- exp(tcl + eta.cl)
    v   <- exp(tv  + eta.v)
    q   <- exp(tq)
    vp  <- exp(tvp)
    linCmt() ~ add(add.err)
  })
}

cat("\n=== 2-cmt oral: forward AD ('AD') ===\n")
fit2_fwd <- fitFOCEi(m2cmt, theo, "AD")

cat("\n=== 2-cmt oral: reverse AD ('ADr') ===\n")
fit2_rev <- fitFOCEi(m2cmt, theo, "ADr")

cat("\n--- 2-cmt objective function comparison ---\n")
cat("Forward (AD): ", fit2_fwd$objective, "\n")
cat("Reverse (ADr):", fit2_rev$objective, "\n")
cat("Delta:        ", abs(fit2_fwd$objective - fit2_rev$objective), "\n")

max_est_diff2 <- max(abs(fit2_fwd$parFixedDf$Estimate - fit2_rev$parFixedDf$Estimate) /
                       (abs(fit2_rev$parFixedDf$Estimate) + 1e-10))
cat(sprintf("Max relative difference in estimates: %.2e\n", max_est_diff2))

## ---------- Summary ---------------------------------------------------------
cat("\n========== PHASE 4 VALIDATION SUMMARY ==========\n")
ok1 <- abs(fit_fwd$objective - fit_rev$objective) < 1e-3
ok2 <- abs(fit2_fwd$objective - fit2_rev$objective) < 1e-3
cat(sprintf("1-cmt oral  OFV match (< 1e-3): %s  (delta = %.2e)\n",
            if (ok1) "PASS" else "FAIL",
            abs(fit_fwd$objective - fit_rev$objective)))
cat(sprintf("2-cmt IV    OFV match (< 1e-3): %s  (delta = %.2e)\n",
            if (ok2) "PASS" else "FAIL",
            abs(fit2_fwd$objective - fit2_rev$objective)))
cat("=================================================\n")
