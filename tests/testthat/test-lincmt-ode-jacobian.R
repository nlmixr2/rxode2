rxTest({
  # A linCmt() model with an ODE state: the steppers that take a Jacobian
  # (ros4, iem) hold only the ODE states, so their Jacobian is that size, not
  # the size of every state the model has.
  .ljModel <- rxode2({
    cp <- linCmt(cl, v, ka)
    d/dt(eff) <- 0.4 * cp - 0.3 * eff
  })
  .ljPar <- c(cl = 1, v = 10, ka = 1.2)
  .ljEv <- rbind(
    data.frame(id = 1L, time = c(0, 10.7, 16.3), evid = 1L, amt = 100, cmt = "depot", rate = c(0, 25, 0)),
    data.frame(id = 1L, time = seq(0.25, 24, by = 0.25), evid = 0L, amt = 0, cmt = "eff", rate = 0)
  )
  .ljEv <- .ljEv[order(.ljEv$time, -.ljEv$evid), ]

  test_that("ros4 and iem solve a linCmt() model with an ODE state", {
    .ref <- rxSolve(.ljModel, .ljEv, params = .ljPar, method = "liblsoda",
                    atol = 1e-12, rtol = 1e-12, returnType = "data.frame")
    .tol <- c(ros4 = 1e-5, iem = 5e-4)
    for (.method in names(.tol)) {
      .s <- rxSolve(.ljModel, .ljEv, params = .ljPar, method = .method,
                    atol = 1e-8, rtol = 1e-8, returnType = "data.frame")
      expect_equal(.s$eff, .ref$eff, tolerance = .tol[[.method]], label = .method)
    }
  })
})
