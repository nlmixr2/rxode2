rxTest({
  # A negative depot amount (negative dose, carried sensitivity state) used to
  # skip the 3-cmt oral depot branch, leaving the depot output unassigned:
  # garbage under forward mode, a segfault under reverse mode (#1275).

  nAlast <- function(ncmt, oral0) {
    npars <- 2L * ncmt + oral0
    ncmt + oral0 + ncmt * npars + oral0
  }
  call3 <- function(alast, sensType, ka = 1.1) {
    rxode2:::linCmtModelDouble(0.7, 1.0, 20, 2.0, 40, 0.5, 60, ka,
                               alast, rep(0, 4), 3L, 1L, 1L, TRUE,
                               0L, 0, 0, 0, 0L, 0L, as.integer(sensType), 0.001)
  }
  depot <- function(v) {
    a <- numeric(nAlast(3L, 1L))
    a[1] <- v
    a
  }

  for (st in c(3L, 30L)) {
    test_that(sprintf("3-cmt oral kernel is linear in a negative depot (sensType %d)", st), {
      pos <- call3(depot(1e-6), st)
      neg <- call3(depot(-1e-6), st)
      expect_equal(neg$val, -pos$val)
      expect_equal(neg$J, -pos$J)
      expect_true(all(is.finite(neg$J)))
      # d(val)/d(ka) at the negative depot vs a central difference
      h <- 1e-6
      fd <- (call3(depot(-1e-6), st, ka = 1.1 + h)$val -
               call3(depot(-1e-6), st, ka = 1.1 - h)$val) / (2 * h)
      expect_equal(neg$Jg[7], fd, tolerance = 1e-6)
    })
  }

  test_that("negative depot dose through rxSolve() matches the ODE and forward mode", {
    m <- function() {
      ini({})
      model({
        cl <- 1; v <- 20; q <- 2; v2 <- 40; q2 <- 0.5; v3 <- 60; ka <- 1.1
        cp <- linCmt()
      })
    }
    ev <- et(amt = 100, cmt = "depot") %>%
      et(amt = -100, time = 0.5, cmt = "depot") %>%
      et(seq(0, 6, by = 0.5))
    ode <- rxSolve(linToOde(rxode2(m)), ev, returnType = "data.frame",
                   useLinCmt = FALSE, atol = 1e-10, rtol = 1e-10)$cp
    expect_true(min(ode) < 0) # the depot really goes negative
    for (st in c("AD", "ADr")) {
      lin <- rxSolve(m, ev, linCmtSensType = st, returnType = "data.frame")$cp
      expect_equal(lin, ode, tolerance = 1e-6)
    }

    args <- "rx__PTR__, t, 1, 3, 1, %d, %d, 1, cl, v, q, vp, q2, vp2, ka"
    lines <- c(sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
               vapply(0:6, function(k) {
                 sprintf("d%d=linCmtB(%s)", k, sprintf(args, -2L, k))
               }, ""))
    mg <- suppressWarnings(rxode2(paste(lines, collapse = "\n")))
    pars <- c(cl = 1, v = 20, q = 2, vp = 40, q2 = 0.5, vp2 = 60, ka = 1.1)
    evg <- et(amt = 100, cmt = 1) %>% et(amt = -100, time = 0.5, cmt = 1) %>%
      et(seq(0.25, 6, by = 0.5))
    solve <- function(p, st) {
      rxSolve(mg, p, evg, linCmtSensType = st, returnType = "data.frame")
    }
    fwd <- solve(pars, "AD")
    rev <- solve(pars, "ADr")
    cols <- c("cp", paste0("d", 0:6))
    expect_equal(as.matrix(rev[cols]), as.matrix(fwd[cols]), tolerance = 1e-12)
    h <- 1e-4
    for (k in 0:6) {
      nm <- names(pars)[k + 1]
      p1 <- pars; p1[nm] <- p1[nm] * (1 + h)
      p2 <- pars; p2[nm] <- p2[nm] * (1 - h)
      fd <- (solve(p1, "ADr")$cp - solve(p2, "ADr")$cp) / (2 * h * pars[[nm]])
      expect_equal(rev[[paste0("d", k)]], fd, tolerance = 1e-5)
    }
  })
})
