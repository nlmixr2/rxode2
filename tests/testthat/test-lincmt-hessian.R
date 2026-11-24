# Second-order linCmt() sensitivities (rxode2lincmt#17): the derivative table
# turns second derivatives of linCmtB() first-order reads into the Hessian
# reads which1 = -20 (amounts) / -21 (concentration), the parser asks the
# solve to keep them (ndiff bit 128), and the solved values match central
# differences of the model's own first-order sensitivities.
rxTest({
  skip_on_cran()

  .hessModel <- function(pk, lin, etas) {
    base <- rxode2(paste(c(pk, lin), collapse = "\n"))
    s <- rxS(base, promoteLinSens = TRUE)
    .toRx <- function(e) {
      .chr <- as.character(e)
      rxFromSE(.chr)
    }
    cpSE <- get("cp", envir = s)
    cpTxt <- .toRx(cpSE)
    lines <- c(pk, paste0("cp <- ", cpTxt))
    for (a in seq_along(etas)) {
      gaTxt <- .toRx(symengine::D(cpSE, symengine::Symbol(etas[a])))
      lines <- c(lines, sprintf("g%d <- %s", a, gaTxt))
      # back into the model's symengine environment, then once more
      .rt <- paste0("rxGa = ", gaTxt)
      rxToSE(.rt, envir = s)
      ga <- get("rxGa", envir = s)
      for (b in a:length(etas)) {
        lines <- c(lines, sprintf("h%d%d <- %s", a, b, .toRx(symengine::D(ga, symengine::Symbol(etas[b])))))
      }
    }
    paste(lines, collapse = "\n")
  }

  .hessCheck <- function(txt, theta, etas, ev, eta0) {
    m <- rxode2(txt)
    expect_true(bitwAnd(rxModelVars(m)$flags[["ndiff"]], 128L) != 0L)
    .solve <- function(eta) {
      as.data.frame(rxSolve(m, c(theta, setNames(eta, etas)), ev, returnType = "data.frame"))
    }
    r0 <- .solve(eta0)
    for (a in seq_along(etas)) {
      for (b in a:length(etas)) {
        d <- 1e-5
        ep <- eta0
        ep[b] <- ep[b] + d
        em <- eta0
        em[b] <- em[b] - d
        g <- sprintf("g%d", a)
        fd <- (.solve(ep)[[g]] - .solve(em)[[g]]) / (2 * d)
        an <- r0[[sprintf("h%d%d", a, b)]]
        expect_false(anyNA(an))
        expect_lt(max(abs(an - fd)) / max(abs(fd)), 1e-6)
      }
    }
  }

  obs <- seq(0.5, 48, by = 1.5)

  test_that("second-order linCmt() sensitivities: 1-cmt IV infusion, multiple doses", {
    pk <- c("cl <- exp(tcl + eta.cl)", "v <- exp(tv + eta.v)")
    txt <- .hessModel(pk, "cp <- linCmt(cl, v)", c("eta.cl", "eta.v"))
    expect_true(grepl(",-20,", txt, fixed = TRUE))
    ev <- et(et(amt = 100, rate = 50, cmt = "central", ii = 12, addl = 2), obs)
    .hessCheck(txt, c(tcl = log(2), tv = log(20)), c("eta.cl", "eta.v"), ev, c(0.1, -0.2))
  })

  test_that("second-order linCmt() sensitivities: 2-cmt oral, steady state", {
    pk <- c(
      "cl <- exp(tcl + eta.cl)",
      "v <- exp(tv + eta.v)",
      "q <- exp(tq)",
      "v2 <- exp(tv2)",
      "ka <- exp(tka + eta.ka)"
    )
    etas <- c("eta.cl", "eta.v", "eta.ka")
    txt <- .hessModel(pk, "cp <- linCmt(cl, v, q, v2, ka)", etas)
    ev <- et(et(amt = 100, cmt = "depot", ii = 12, addl = 2, ss = 1), obs)
    theta <- c(tcl = log(2), tv = log(20), tq = log(4), tv2 = log(40), tka = log(0.8))
    .hessCheck(txt, theta, etas, ev, c(0.1, -0.2, 0.15))
  })

  test_that("the chained rxFromSE() derivative keeps linCmt() curvature", {
    # the path nlmixr2est's exact inner Hessian takes: before, the cross term
    # silently lost its curvature and the diagonal was refused
    base <- rxode2("cl <- exp(THETA[1] + ETA[1])\nv <- exp(THETA[2] + ETA[2])\ncp <- linCmt(cl, v)")
    s <- rxS(base, promoteLinSens = TRUE)
    cp <- get("cp", envir = s)
    for (pr in list(c("ETA_1_", "ETA_1_"), c("ETA_1_", "ETA_2_"), c("ETA_2_", "ETA_2_"))) {
      .chr <- as.character(symengine::D(symengine::D(cp, symengine::S(pr[1])), symengine::S(pr[2])))
      out <- rxFromSE(.chr)
      expect_true(grepl(",-20,", out, fixed = TRUE))
      # no raw sensitivity-column read survives: they come back as states
      expect_false(grepl("linCmtB\\([^)]*,[1-9][0-9]*,-2,", out))
    }
  })

  test_that("second-order linCmt() sensitivities with a modeled bioavailability", {
    # F scales the dose amount, which carries no dependence on the PK
    # parameters, so their curvature is exact with f() in the model (the
    # derivative in F's own random effect is the eta-space carry's jump)
    pk <- c("cl <- exp(tcl + eta.cl)", "v <- exp(tv + eta.v)", "ka <- exp(tka)", "fd <- 0.8")
    txt <- .hessModel(pk, "cp <- linCmt(cl, v, ka)", c("eta.cl", "eta.v"))
    txt <- paste0(txt, "\nf(depot) <- fd")
    ev <- et(et(amt = 100, cmt = "depot", ii = 12, addl = 1), obs)
    .hessCheck(txt, c(tcl = log(2), tv = log(20), tka = log(0.8)), c("eta.cl", "eta.v"), ev, c(0.1, -0.2))
  })

  test_that("Hessian reads are NA once the parameters change between records", {
    # a time-varying covariate moves cl mid-profile: the quadratic carry is an
    # expansion around one parameter vector, so the change is not followed
    pk <- c("cl <- exp(tcl + eta.cl) * (1 + 0.5 * wt)", "v <- exp(tv + eta.v)")
    txt <- .hessModel(pk, "cp <- linCmt(cl, v)", c("eta.cl", "eta.v"))
    m <- rxode2(txt)
    d <- data.frame(
      id = 1,
      time = c(0, 1, 2, 6, 8, 10),
      amt = c(100, 0, 0, 0, 0, 0),
      evid = c(1, 0, 0, 0, 0, 0),
      cmt = "central",
      wt = c(0, 0, 0, 1, 1, 1)
    )
    p <- c(tcl = log(2), tv = log(20), eta.cl = 0, eta.v = 0)
    r <- as.data.frame(rxSolve(m, p, d, covsInterpolation = "locf"))
    expect_false(anyNA(r$h11[r$time < 6]))
    expect_true(all(is.na(r$h11[r$time > 6])))
  })

  # The eta-space carry (rxode2lincmt which1 = -30 .. -38) follows what the
  # parameter-space Hessian cannot: parameters that change between records,
  # and a modeled bioavailability on boluses.  The sentinels are written by
  # hand here (an estimator's code generation emits them).
  .etaCarryModel <- function(withF) {
    L <- function(w1, w2, p2 = "0") {
      sprintf("linCmtB(rx__PTR__,t,1,1,1,%d,%d,1,cl,v,%s,0,0,0,ka)", w1, w2, p2)
    }
    E <- 16
    lines <- c(
      "cl <- exp(tcl + eta.cl) * (1 + 0.5 * wt)",
      "v <- exp(tv)",
      "ka <- exp(tka)",
      "fd <- 0.8 * exp(eta.f)",
      paste0("cp <- ", L(-1, -1)),
      paste0("z1 ~ ", L(-30, 0 * E + 0, "cl")), # dcl/deta.cl
      paste0("z2 ~ ", L(-31, (0 * E + 0) * E + 0, "cl")), # d2cl/deta.cl2
      if (withF) paste0("z3 ~ ", L(-37, 0 * E + 1, "fd")), # dF(depot)/deta.f
      if (withF) paste0("z4 ~ ", L(-38, (0 * E + 1) * E + 1, "fd")),
      paste0("z5 ~ ", L(-32, 0)),
      paste0("e1 <- ", L(-33, 0)),
      paste0("e2 <- ", L(-33, 1)),
      paste0("e11 <- ", L(-34, 0 * E + 0)),
      paste0("e12 <- ", L(-34, 0 * E + 1)),
      paste0("e22 <- ", L(-34, 1 * E + 1)),
      if (withF) "f(depot) <- fd"
    )
    rxode2(paste(lines, collapse = "\n"))
  }
  .etaCarryData <- data.frame(
    id = 1,
    time = c(0, 1, 2, 4, 6, 8, 10, 14, 16, 20, 24),
    amt = c(100, 0, 0, 0, 0, 0, 0, 0, 100, 0, 0),
    evid = c(1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
    cmt = "depot",
    wt = c(0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0)
  )
  .etaCarryCheck <- function(m, interp, pairs) {
    .solve <- function(e) {
      p <- c(tcl = log(2), tv = log(20), tka = log(0.8), eta.cl = e[1], eta.f = e[2])
      as.data.frame(rxSolve(m, p, .etaCarryData, covsInterpolation = interp, addDosing = FALSE))
    }
    e0 <- c(0.1, -0.2)
    h <- 1e-5
    r0 <- .solve(e0)
    .fd <- function(col, j) {
      ep <- e0
      em <- e0
      ep[j] <- ep[j] + h
      em[j] <- em[j] - h
      (.solve(ep)[[col]] - .solve(em)[[col]]) / (2 * h)
    }
    for (pr in pairs) {
      an <- r0[[pr[1]]]
      fd <- .fd(pr[2], as.integer(pr[3]))
      expect_false(anyNA(an))
      expect_lt(max(abs(an - fd)) / max(abs(fd)), 1e-7)
    }
  }

  test_that("eta-space carry: exact with a time-varying covariate, locf and nocb", {
    m <- .etaCarryModel(FALSE)
    for (interp in c("locf", "nocb")) {
      .etaCarryCheck(m, interp, list(c("e1", "cp", "1"), c("e11", "e1", "1")))
    }
  })

  test_that("eta-space carry: exact through a modeled bioavailability (the jump)", {
    m <- .etaCarryModel(TRUE)
    pairs <- list(
      c("e1", "cp", "1"),
      c("e2", "cp", "2"),
      c("e11", "e1", "1"),
      c("e12", "e1", "2"),
      c("e22", "e2", "2")
    )
    for (interp in c("locf", "nocb")) {
      .etaCarryCheck(m, interp, pairs)
    }
  })
})
