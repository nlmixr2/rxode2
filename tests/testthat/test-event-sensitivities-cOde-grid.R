rxTest({
  # Port of cOde's test-sensitivitiesSymb.R (dkaschek/cOde, 2026), "works for
  # all possible combinations of states and events": A -> B driven by INPUT, with events on INPUT (linear
  # target, as in cOde) and on A (nonlinear target).  Each combination of event
  # time (constant or the parameter ton), value (constant or the parameter
  # xon), method (replace/add/multiply) and sensitivity parameters (including
  # initial values) must agree with finite differences, not only parse.
  .combModel <- function(target, timePar, valuePar, mode, calcSens = NULL, calcSens2 = NULL) {
    .lines <- c(
      "A(0) <- A0",
      "INPUT(0) <- I0",
      if (timePar) sprintf("alag(%s) <- ton", target),
      if (valuePar) sprintf("f(%s) <- xon", target),
      "d/dt(A) <- -kcat*INPUT*A/(Km+A)",
      "d/dt(B) <- kcat*INPUT*A/(Km+A) - k*B",
      "d/dt(INPUT) <- 0"
    )
    rxode2(paste(.lines, collapse = "\n"), calcSens = calcSens,
           calcSens2 = calcSens2, eventSens = mode)
  }

  test_that("cOde event combinations: jump sensitivities match finite differences", {
    pBase <- c(A0 = 2, I0 = 0.5, kcat = 1.1, Km = 0.7, k = 0.4)
    obs <- seq(0.05, 6, by = 0.2)
    for (.target in c("INPUT", "A")) {
      for (.timePar in c(FALSE, TRUE)) {
        for (.valuePar in c(FALSE, TRUE)) {
          p <- c(pBase, if (.timePar) c(ton = 1.3), if (.valuePar) c(xon = 0.8))
          .sp <- names(p)
          mj <- .combModel(.target, .timePar, .valuePar, "jump", calcSens = .sp)
          mf <- .combModel(.target, .timePar, .valuePar, "fd")
          for (.evid in c(1L, 5L, 6L)) {
            e <- et(amt = 1.5, cmt = .target, evid = .evid, time = if (.timePar) 0 else 1.3) |> et(obs)
            sj <- rxSolve(mj, e, p, atol = 1e-12, rtol = 1e-12)
            for (.a in .sp) {
              pp <- p
              pp[.a] <- pp[.a] + 1e-6
              pm <- p
              pm[.a] <- pm[.a] - 1e-6
              sp <- rxSolve(mf, e, pp, atol = 1e-12, rtol = 1e-12)
              sm <- rxSolve(mf, e, pm, atol = 1e-12, rtol = 1e-12)
              for (.st in c("A", "B", "INPUT")) {
                expect_equal(
                  sj[[paste0("rx__sens_", .st, "_BY_", .a, "__")]],
                  (sp[[.st]] - sm[[.st]]) / 2e-6,
                  tolerance = 1e-5,
                  label = sprintf(
                    "target=%s timePar=%s valuePar=%s evid=%d d%s/d%s",
                    .target,
                    .timePar,
                    .valuePar,
                    .evid,
                    .st,
                    .a
                  )
                )
              }
            }
          }
        }
      }
    }
  })

  test_that("cOde event combinations: 2nd-order jump sensitivities match finite differences of the 1st order", {
    p <- c(A0 = 2, I0 = 0.5, kcat = 1.1, Km = 0.7, k = 0.4, ton = 1.3, xon = 0.8)
    .sp <- c("kcat", "ton", "xon")
    obs <- seq(0.05, 6, by = 0.2)
    for (.target in c("INPUT", "A")) {
      m1 <- .combModel(.target, TRUE, TRUE, "jump", calcSens = .sp)
      m2 <- .combModel(.target, TRUE, TRUE, "jump", calcSens = .sp, calcSens2 = .sp)
      for (.evid in c(1L, 5L, 6L)) {
        e <- et(amt = 1.5, cmt = .target, evid = .evid) |> et(obs)
        s2 <- rxSolve(m2, e, p, atol = 1e-12, rtol = 1e-12)
        for (.b in .sp) {
          pp <- p
          pp[.b] <- pp[.b] + 1e-5
          pm <- p
          pm[.b] <- pm[.b] - 1e-5
          sp <- rxSolve(m1, e, pp, atol = 1e-12, rtol = 1e-12)
          sm <- rxSolve(m1, e, pm, atol = 1e-12, rtol = 1e-12)
          for (.a in .sp) {
            for (.st in c("A", "B")) {
              .n1 <- paste0("rx__sens_", .st, "_BY_", .a, "__")
              expect_equal(
                s2[[paste0("rx__sens_", .st, "_BY_", .a, "_BY_", .b, "__")]],
                (sp[[.n1]] - sm[[.n1]]) / 2e-5,
                tolerance = 1e-5,
                label = sprintf("target=%s evid=%d d2%s/d%s d%s", .target, .evid, .st, .a, .b)
              )
            }
          }
        }
      }
    }
  })
})
