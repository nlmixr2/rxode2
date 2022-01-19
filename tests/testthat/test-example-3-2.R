## Example 3.2 from
## "Solving Differential Equations in R" by Soetaert et al (2012)
## https://cran.r-project.org/web/packages/diffEq/vignettes/ODEinR.pdf Example #2
## Lorenz model in Vingette.

rxode2Test(
  {
    ode <- rxode2("
         d/dt(X) = a*X + Y*Z;
         d/dt(Y) = b*(Y - Z);
         d/dt(Z) = -X*Y + c*Y - Z;")

    et <- eventTable() # default time units
    et$add.sampling(seq(from = 0, to = 100, by = 0.01))

    out <-
      ode$solve(
        params = c(a = -8 / 3, b = -10, c = 28),
        events = et,
        inits = c(X = 1, Y = 1, Z = 1)
      )

    out <- round(out, 6)


    test_that("Runs example 3.2 correctly", {
      expect_equal(
        round(out[1:15, ], 3),
        structure(c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.12, 0.13, 0.14, 1, 0.985, 0.973, 0.965, 0.962, 0.964, 0.973, 0.99, 1.017, 1.058, 1.114, 1.19, 1.292, 1.425, 1.597, 1, 1.013, 1.049, 1.107, 1.187, 1.288, 1.41, 1.554, 1.721, 1.914, 2.133, 2.382, 2.664, 2.981, 3.337, 1, 1.26, 1.524, 1.798, 2.089, 2.4, 2.739, 3.109, 3.518, 3.97, 4.471, 5.029, 5.65, 6.342, 7.11), .Dim = c(15L, 4L), .Dimnames = list(NULL, c("time", "X", "Y", "Z")))
      )
    })
  },
  silent = TRUE,
  test = "lvl2"
)
