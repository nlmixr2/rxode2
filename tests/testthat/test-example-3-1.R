## Example 3.1 from
## "Solving Differential Equations in R" by Soetaert et al (2012)
## https://cran.r-project.org/web/packages/diffEq/vignettes/ODEinR.pdf Example #1
skip_if_not(file.exists(test_path("test-example-3-1.qs")))
df.test <- qs::qread(test_path("test-example-3-1.qs"))

ms <- c("liblsoda", "lsoda", "dop853")
ode <- rxode2(model = "d/dt(y) = r * y * (1.0 - y/K);")
.rxWithOptions(list(digits = 6), {
  for (meth in ms) {
    # context(sprintf("Example 3-1 (%s)", meth))

    ## create event table with times at which we observe the system
    et <- eventTable(time.units = NA)
    et$add.sampling(seq(from = 0, to = 20, by = 0.2))

    ## same model, different initial values (2 and 12)
    out1 <- ode$solve(params = c(r = 1, K = 10), events = et, inits = c(y = 2))
    out2 <- ode$solve(params = c(r = 1, K = 10), events = et, inits = c(y = 12))

    ## matplot(x = out1[,1], y = cbind(out1[,2], out2[,2]), type = "l",
    ##    main = "logistic growth", xlab="time", ylab="")

    ## Now use a non-stiff solver
    out2.ns <-
      ode$solve(params = c(r = 1, K = 10), events = et, inits = c(y = 12), method = "dop853")

    df <- round(cbind(out1, out2, out2.ns), 3)

    test_that("Runs example 3.1 correctly", {
      expect_equal(df, round(df.test, 3))
    })

    head(cbind(out1, out2, out2.ns), n = 15)
  }
})
