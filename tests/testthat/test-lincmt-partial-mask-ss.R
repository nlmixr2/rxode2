rxTest({
  # A model that reads only some linCmtB() directions (a FOCEi inner model
  # with fewer etas than parameters) must solve a steady-state regimen to the
  # same values as one that reads every direction.  The unrequested Jacobian
  # columns are carried into the next row's Alast reconstruction, so they
  # have to be a finite zero, not the NA the slot starts with.
  .gradModel <- function(ncmt, oral0, dirs) {
    args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, 1, cl, v, q, vp, q2, vp2, ka",
                    ncmt, oral0)
    lines <- c(sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
               vapply(dirs, function(k) {
                 sprintf("d%d=linCmtB(%s)", k, sprintf(args, -2L, k))
               }, ""))
    suppressWarnings(rxode2(paste(lines, collapse = "\n")))
  }
  .parsFor <- function(ncmt, oral0) {
    p <- c(cl = 2.1, v = 21, q = 3.3, vp = 43, q2 = 0.9, vp2 = 61, ka = 1.3)
    if (ncmt < 2) p[c("q", "vp")] <- 0
    if (ncmt < 3) p[c("q2", "vp2")] <- 0
    if (oral0 == 0) p["ka"] <- 0
    p
  }
  .evSs <- function() {
    do.call(rbind, lapply(1:2, function(i) {
      dose <- data.frame(id = i, time = c(0, 48, 60),
                         amt = c(100, 100, 50), evid = 1, cmt = 1,
                         rate = c(if (i == 1) 25 else 0, 0, 0),
                         ii = c(12, 12, 0), ss = c(1, 2, 0))
      obs <- data.frame(id = i, time = c(0.7, 2.3, 5.9, 11.1, 13.4, 20.2, 30.5,
                                          47.5, 49.1, 53.3, 59.4, 61.2, 66.6, 80.1) + 0.15 * i,
                        amt = 0, evid = 0, cmt = 1, rate = 0, ii = 0, ss = 0)
      rbind(dose, obs)
    }))
  }
  test_that("partial direction masks survive steady-state rows", {
    ev <- .evSs()
    for (cfg in list(c(1L, 0L), c(1L, 1L), c(2L, 0L), c(2L, 1L), c(3L, 0L), c(3L, 1L))) {
      ncmt <- cfg[1]; oral0 <- cfg[2]
      npars <- 2L * ncmt + oral0
      full <- rxSolve(.gradModel(ncmt, oral0, seq_len(npars) - 1L), .parsFor(ncmt, oral0),
                      ev, linCmtSensType = "AD", returnType = "data.frame")
      expect_false(anyNA(full$cp))
      for (dirs in list(0L, c(0L, 1L), c(1L, npars - 1L))) {
        for (st in c("AD", "ADr")) {
          part <- rxSolve(.gradModel(ncmt, oral0, dirs), .parsFor(ncmt, oral0), ev,
                          linCmtSensType = st, returnType = "data.frame")
          expect_false(anyNA(part$cp))
          expect_equal(part$cp, full$cp, tolerance = 1e-10)
          for (d in dirs) {
            nm <- sprintf("d%d", d)
            expect_equal(part[[nm]], full[[nm]], tolerance = 1e-8)
          }
        }
      }
    }
  })
})
