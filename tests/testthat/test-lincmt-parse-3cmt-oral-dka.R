rxTest({
  # The parser registered the 3-cmt oral d/d(ka) read (which1=-2, which2=6)
  # under the wrong case label, so the model failed to build.  The read must
  # build and match a finite difference on ka.
  a <- "rx__PTR__, t, 1, 3, 1, %d, %d, 1, cl, v, q, vp, q2, vp2, ka"
  m <- suppressWarnings(rxode2(paste0("cp=linCmtB(", sprintf(a, -1L, -1L), ")\n",
                                      "dka=linCmtB(", sprintf(a, -2L, 6L), ")")))
  pars <- c(cl = 2.1, v = 21, q = 3.3, vp = 43, q2 = 1.1, vp2 = 80, ka = 1.3)
  ev <- et(amt = 100, cmt = 1) |> et(c(0.5, 2, 6, 12))
  test_that("3-cmt oral d/d(ka) read builds and matches finite differences", {
    s <- rxSolve(m, pars, ev, returnType = "data.frame")
    h <- 1e-5
    pUp <- pars
    pUp["ka"] <- pars[["ka"]] + h
    pDn <- pars
    pDn["ka"] <- pars[["ka"]] - h
    up <- rxSolve(m, pUp, ev, returnType = "data.frame")
    dn <- rxSolve(m, pDn, ev, returnType = "data.frame")
    fd <- (up$cp - dn$cp) / (2 * h)
    expect_true(max(abs(s$dka - fd) / pmax(abs(fd), 1e-8)) < 1e-6)
  })
})
