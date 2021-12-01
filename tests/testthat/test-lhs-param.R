rxode2Test(
  {
    context("Dual lhs/param values (Issue #135)")

    test_that("Two defined variables", {
      mod1 <- rxode2({
        k <- k + 3
        km <- km + 4
      })

      expect_equal(mod1$lhs, c("k", "km"))
      expect_equal(mod1$params, c("k", "km"))

      expect_equal(as.vector(rxSolve(mod1, et(0), params = c(k = 3, km = 4), returnType = "matrix")), as.double(c(0, 6, 8)))
    })


    test_that("Two defined variables with ini", {
      mod1 <- rxode2({
        k <- 3
        k <- k + 3
        km <- 4
        km <- km + 4
      })
      expect_equal(mod1$lhs, c("k", "km"))
      expect_equal(mod1$params, c("k", "km"))
      expect_equal(rxInits(mod1), c(k = 3, km = 4))
      expect_equal(as.vector(rxSolve(mod1, et(0), returnType = "matrix")), as.double(c(0, 6, 8)))
    })

    test_that("lhs/params changes", {
      mod4 <- rxode2({
        j <- k + m
        k <- j + 3
      })
      expect_equal(mod4$lhs, c("j", "k"))
      expect_equal(mod4$params, c("k", "m"))
    })

    test_that("one variable/param", {
      mod1 <- rxode2({
        k <- k + 3
      })
      expect_equal(mod1$lhs, "k")
      expect_equal(mod1$params, "k")
    })

    test_that("Sc is not lhs", {
      tmp <- rxode2({
        Sc <- L^2 * ((g * e) / (g + e)) * (1 + (km * L / v))
        d / dt(L) <- ((1 / (3 * L^2))) * (((v / g) * Sc) - km * L^3)
      })
      expect_false(any(tmp$params == "Sc"))
    })


    test_that("Last item of last line still counts", {
      mod4 <- rxode2({
        j <- k
        k <- j + 3
      })
      expect_equal(mod4$lhs, c("j", "k"))
      expect_equal(mod4$params, "k")

      mod4 <- rxode2({
        j <- k + 4
        k <- j + 3
      })
      expect_equal(mod4$lhs, c("j", "k"))
      expect_equal(mod4$params, "k")
    })

    test_that("Make sure CL is correctly identified as hidden lhs not a dual", {
      mod2 <- rxode2({
        C2 <- centr / V2
        C3 ~ peri / V3
        CL ~ TCL * exp(eta.Cl)
        d / dt(depot) ~ -KA * depot
        d / dt(centr) ~ KA * depot - CL * C2 - Q * C2 + Q * C3
        d / dt(peri) ~ Q * C2 - Q * C3
        d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
        eff(0) <- 1000
        e1 <- err1
        e2 <- err2
        resp <- eff + e1
        pk <- C2 * exp(e2)
      })
      expect_false(any("CL" == mod2$params))
      expect_false(any("CL" == mod2$lhs))
    })

    test_that("newind variables not identified as dual parameters", {
      ode.1c <- rxode2({
        V <- 20
        Cl <- 1
        fc <- 1
        C2 <- center / V
        ni <- newind
        ni2 <- NEWIND
        d / dt(center) ~ -Cl * C2
        f(center) <- fc
      })

      expect_equal(ode.1c$params, c("V", "Cl", "fc"))
      expect_equal(ode.1c$lhs, c("C2", "ni", "ni2"))
    })

    test_that("suppressed assignments give correct variables", {
      mod1 <- rxode2({
        k ~ k + 3
        km ~ km + 4
        ret <- k + km
      })

      expect_equal(mod1$lhs, "ret")
      expect_equal(mod1$params, c("k", "km"))
      expect_equal(rxSolve(mod1, c(k = 1, km = 2), et(0))$ret, as.double(10))
    })

    test_that("a=NA gives correct variables", {
      mod1 <- rxode2("a=NA;\nb=2;\nc=a+b")
      expect_equal(mod1$params, "b")
      expect_equal(mod1$lhs, c("a", "c"))

      mod1 <- rxode2("a=2;\nb=NA;\nc=a+b")
      expect_equal(mod1$params, "a")
      expect_equal(mod1$lhs, c("b", "c"))

      mod1 <- rxode2("a~NA;\nb~2;\nc=a+b")
      expect_equal(mod1$params, character(0))
      expect_equal(mod1$lhs, "c")

      mod1 <- rxode2("a~2;\nb~NA;\nc=a+b")
      expect_equal(mod1$params, character(0))
      expect_equal(mod1$lhs, "c")
    })
  },
  test = "parsing"
)
