# FOCEi-level validation for the closed-form transition matrix
# (linCmtSensPhi = 2, now the default) and the multi-direction forward pass
# (linCmtSensType = "ADm").
#
# A fit is the thing the routes exist for, so the objective is the check
# that matters: a route that is a summation-order change must not move the
# objective function value beyond round-off.
#
# nlmixr2est is used AS INSTALLED, against this worktree's rxode2 loaded
# with devtools::load_all(compile = FALSE).  That works because nothing here
# changes the rxode2 ABI: the new sensitivity type and the new transition
# matrix are an added integer in an existing rxControl slot and an added
# branch behind it.
#
# Usage: Rscript bench/lincmt_focei_phi_validate.R
suppressMessages({
  devtools::load_all(".", compile = FALSE, quiet = TRUE)
  library(nlmixr2)
  library(nlmixr2data)
})

m1cmt <- function() {
  ini({
    tka <- log(1.57); tcl <- log(2.72); tv <- log(31.07)
    eta.ka ~ 0.6; eta.cl ~ 0.09; eta.v ~ 0.1
    add.err <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
    linCmt() ~ add(add.err)
  })
}
m2cmt <- function() {
  ini({
    tka <- log(1.57); tcl <- log(2.72); tv <- log(31.07)
    tq <- log(1.2); tvp <- log(40)
    eta.ka ~ 0.6; eta.cl ~ 0.09; eta.v ~ 0.1
    prop.err <- 0.15
  })
  model({
    ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
    q <- exp(tq); vp <- exp(tvp)
    linCmt() ~ prop(prop.err)
  })
}

fit1 <- function(model, sens, phi) {
  suppressWarnings(suppressMessages(
    nlmixr2(model, theo_sd, "focei",
            control = foceiControl(print = 0L, calcTables = FALSE,
                                   maxOuterIterations = 30L,
                                   rxControl = rxode2::rxControl(
                                     linCmtSensType = sens,
                                     linCmtSensPhi = phi)))))
}

arms <- list(c("AD", "0"), c("AD", "1"), c("AD", "2"), c("ADm", "2"))
for (nm in c("1cmt", "2cmt")) {
  mod <- if (nm == "1cmt") m1cmt else m2cmt
  ref <- NULL
  for (a in arms) {
    f <- fit1(mod, a[1], as.integer(a[2]))
    o <- f$objDf$OBJF
    if (is.null(ref)) ref <- o
    cat(sprintf("%-5s sens=%-3s phi=%s  OBJF = %.6f   delta = %.3e\n",
                nm, a[1], a[2], o, abs(o - ref)))
  }
}
