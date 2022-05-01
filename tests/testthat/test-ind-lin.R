rxTest({
  test_that("Matrix exponential alone works", {
    # Test inductive linearization

    ## Case 1 ME alone from wikipedia
    mod <- suppressMessages(rxode2(
    {
      d / dt(x) <- 2 * x - y + z
      d / dt(y) <- 3 * y - 1 * z
      d / dt(z) <- 2 * x + y + 3 * z
      x(0) <- 0.1
      y(0) <- 0.1
      z(0) <- 0.1
    },
    indLin = TRUE
    ))

    m <- rxSolve(mod, et(seq(0, 24, length.out = 50)), method = "indLin")
    m2 <- rxSolve(mod, et(seq(0, 24, length.out = 50)), method = "lsoda")

    expect_equal(as.data.frame(m), as.data.frame(m2), tolerance = 1e-5)

    ## Now do without indLin in the rxode2

    mod <- rxode2({
      d / dt(x) <- 2 * x - y + z
      d / dt(y) <- 3 * y - 1 * z
      d / dt(z) <- 2 * x + y + 3 * z
      x(0) <- 0.1
      y(0) <- 0.1
      z(0) <- 0.1
    })

    m <- suppressMessages(rxSolve(mod, et(seq(0, 24, length.out = 50)), method = "indLin"))
    m2 <- rxSolve(mod, et(seq(0, 24, length.out = 50)), method = "lsoda")

    ## FIXME
    ## expect_equal(as.data.frame(m), as.data.frame(m2), tolerance = 1e-5)

    ## Case 2 ME alone with inhomogenous systems

    mod <- suppressMessages(rxode2(
    {
      d / dt(x) <- 2 * x - y + z + exp(-2 * t)
      d / dt(y) <- 3 * y - 1 * z
      d / dt(z) <- 2 * x + y + 3 * z + exp(-2 * t)
      x(0) <- 0.1
      y(0) <- 0.1
      z(0) <- 0.1
    },
    indLin = TRUE
    ))

    m <- rxSolve(mod, et(seq(0, 24, length.out = 50)), method = "indLin")
    m2 <- rxSolve(mod, et(seq(0, 24, length.out = 50)), method = "lsoda")

    ## gridExtra::grid.arrange(plot(m), plot(m2))

    ## FIXME?
    ## expect_equal(as.data.frame(m), as.data.frame(m2), tolerance =1e-5)

    mod <- suppressMessages(rxode2("
a = 6
b = 0.6
d/dt(intestine) = -a*intestine
d/dt(blood)     = a*intestine - b*blood
", indLin = TRUE))


    et <- eventTable(time.units = "days")
    et$add.sampling(seq(0, 10, by = 1 / 24))
    et$add.dosing(
      dose = 2 / 24, rate = 2, start.time = 0,
      nbr.doses = 10, dosing.interval = 1
    )

    pk <- rxSolve(mod, et, method = "indLin")
    pk2 <- rxSolve(mod, et, method = "liblsoda")
    expect_equal(as.data.frame(pk), as.data.frame(pk2), tolerance = 1e-5)

    ## plot(microbenchmark::microbenchmark(rxSolve(mod,et, method="indLin",indLinMatExpType=1L),rxSolve(mod,et, method="indLin",indLinMatExpType=2L), rxSolve(mod,et, method="indLin",indLinMatExpType=3L), rxSolve(mod,et, method="lsoda")), log="y")

    et2 <- eventTable(time.units = "days")
    et2$add.sampling(seq(0, 10, by = 1 / 24))
    et2$add.dosing(
      dose = 2, start.time = 0,
      nbr.doses = 10, dosing.interval = 1
    )

    pk <- rxSolve(mod, et2, method = "indLin")

    pk2 <- rxSolve(mod, et2, method = "liblsoda")

    expect_equal(as.data.frame(pk), as.data.frame(pk2), tolerance = 1e-5)

    ## Inductive linearization
    mmModel <- suppressMessages(rxode2(
    {
      ka <- 1
      Vc <- 1
      Vmax <- 0.00734
      Km <- 0.3672
      Cp <- center / Vc
      d / dt(center) <- -Vmax / (Km + Cp) * Cp
    },
    indLin = TRUE
    ))

    mmModel <- suppressMessages(rxode2(
    {
      ka <- 1
      Vc <- 1
      Vmax <- 0.00734
      Km <- 0.3672
      Cp <- center / Vc
      d / dt(center) <- -Vmax / (Km + Cp) * Cp + exp(-10 * t)
    },
    indLin = TRUE
    ))

    ## Inductive + 1x1 matrix
    ## FIXME this should be inductive too...
    mmModel <- suppressMessages(rxode2(
    {
      ka <- 1
      Vc <- 1
      Vmax <- 0.00734
      Km <- 0.3672
      d / dt(depot) <- -ka * depot
      d / dt(center) <- ka * depot - Vmax / (Km + Cp) * Cp
      Cp <- center / Vc
    },
    indLin = TRUE
    ))

    ## This is inductive
    mmModel <- suppressMessages(rxode2(
    {
      ka <- 1
      Vc <- 1
      Vmax <- 0.00734
      Km <- 0.3672
      d / dt(depot) <- -ka * depot
      Cp <- center / Vc
      d / dt(center) <- ka * depot - Vmax / (Km + Cp) * Cp
    },
    indLin = TRUE
    ))

    mmModel <- suppressMessages(rxode2(
    {
      ka <- 1
      Vc <- 1
      Vmax <- 0.00734
      Km <- 0.3672
      V4 <- 4.3
      Q <- 1.5
      K12 <- Q / Vc
      K21 <- Q / Vp
      Cp <- center / Vc
      d / dt(depot) <- -ka * depot
      d / dt(center) <- ka * depot - Vmax / (Km + Cp) * Cp + K21 * periph - K12 * center
      d / dt(periph) <- -K21 * periph + K12 * center
    },
    indLin = TRUE
    ))

    ## Inductive linearization
    mmModel <- suppressMessages(rxode2(
    {
      ka <- 1
      Vc <- 1
      Vmax <- 0.00734
      Km <- 0.3672
      d / dt(depot) <- -ka * depot
      Cp <- center / Vc
      d / dt(center) <- ka * depot - Vmax / (Km + Cp) * Cp
    },
    indLin = TRUE
    ))

    et <- eventTable(time.units = "days")
    et$add.sampling(seq(0, 10, by = 1 / 24))
    et$add.dosing(
      dose = 2, start.time = 0,
      nbr.doses = 10, dosing.interval = 6
    )

    pk <- rxSolve(mmModel, et, method = "indLin")
    pk2 <- rxSolve(mmModel, et, method = "liblsoda")

    ## gridExtra::grid.arrange(plot(pk), plot(pk2))

    expect_equal(as.data.frame(pk), as.data.frame(pk2), tolerance = 7e-5)

    mmModel <- suppressMessages(rxode2(
    {
      ka <- 1
      Vc <- 1
      Vmax <- 0.00734
      Km <- 0.3672
      d / dt(depot) <- -ka * depot
      Cp <- center / Vc
      d / dt(center) <- ka * depot - Vmax / (Km + Cp) * Cp + 5 * exp(-0.5 * t)
    },
    indLin = TRUE
    ))

    pk <- rxSolve(mmModel, et, method = "indLin")
    pk2 <- rxSolve(mmModel, et, method = "lsoda")

    ## gridExtra::grid.arrange(plot(pk), plot(pk2))
    ## These are not equal...
    ## expect_equal(as.data.frame(pk), as.data.frame(pk2), tolerance =7e-5)

    ## plot(microbenchmark::microbenchmark(rxSolve(mmModel,et, method="indLin",indLinMatExpType=1L),rxSolve(mmModel,et, method="indLin",indLinMatExpType=2L), rxSolve(mmModel,et, method="indLin",indLinMatExpType=3L), rxSolve(mmModel,et, method="lsoda")), log="y")

    ## Van der Pol Equation
    ## mu = 1000 stiff
    ## me = 1 non-stiff
    ## rxIndLinState(list(y="dy", dy="y"))
    rxIndLinState(NULL)
    rxIndLinStrategy()
    van1 <- suppressMessages(rxode2(
    {
      y(0) <- 2
      d / dt(y) <- dy
      d / dt(dy) <- mu * (1 - y^2) * dy - y
    },
    indLin = TRUE
    ))

    van <- van1

    rxIndLinState(list(y = "dy", dy = "y"))
    ## rxIndLinState(NULL)
    rxIndLinStrategy()
    van2 <- suppressMessages(rxode2(
    {
      y(0) <- 2
      d / dt(y) <- dy
      d / dt(dy) <- mu * (1 - y^2) * dy - y
    },
    indLin = TRUE
    ))

    ## rxIndLinState(list(y="dy", dy="y"))
    rxIndLinState(NULL)
    rxIndLinStrategy("split")
    van3 <- suppressMessages(rxode2(
    {
      y(0) <- 2
      d / dt(y) <- dy
      d / dt(dy) <- mu * (1 - y^2) * dy - y
    },
    indLin = TRUE
    ))

    et <- eventTable()
    ## 3000 causes weird behavior of indLin / lsoda
    et$add.sampling(seq(0, 20, length.out = 200))

    s1 <- rxSolve(van1, et, c(mu = 1000), method = "lsoda")
    s2 <- rxSolve(van1, et, c(mu = 1000), method = "indLin")
    ## s3 <- rxSolve(van, et, c(mu=1000), method="dop853")

    ## f <- function(mu = 1, ...) {
    ##   s1 <- rxSolve(van1, et, c(mu = mu), method = "lsoda") %>% plot() +
    ##     ggtitle(sprintf("Lsoda mu=%s", mu))
    ##   s2 <- rxSolve(van1, et, c(mu = mu), method = "indLin", ...) %>% plot() +
    ##     ggtitle(sprintf("indLin1 mu=%s", mu))
    ##   s3 <- rxSolve(van3, et, c(mu = mu), method = "indLin", ...) %>% plot() +
    ##     ggtitle(sprintf("indLin3 mu=%s", mu))
    ##   ## s4 <- rxSolve(van3, et, c(mu=mu), method="indLin", ...) %>% plot() +
    ##   ##     ggtitle(sprintf("indLin3 mu=%s", mu))
    ##   s4 <- rxSolve(van3, et, c(mu = mu), method = "dop853", ...) %>% plot() +
    ##     ggtitle(sprintf("dop853 mu=%s", mu))
    ##   gridExtra::grid.arrange(s1, s2, s3, s4)
    ## }

    ## uses library animation
    ## saveGIF({
    ##     for (i in seq(0.1, 15, by=0.1)){
    ##         print(f(mu=i))
    ##     }
    ## }, movie.name="indLin-dop.gif", interval=0.1, nmax=30, ani.width=600, ani.hegith=300)

    expect_equal(as.data.frame(s1), as.data.frame(s2), tolerance = 1e-5)

    s1 <- rxSolve(van, et, c(mu = 1), method = "lsoda")
    s2 <- rxSolve(van, et, c(mu = 1), method = "indLin")
    ## expect_equal(as.data.frame(s1), as.data.frame(s2), tolerance =1e-4)
    ## s3 <- rxSolve(van, et, c(mu=1), method="dop853")

    ## s1 %>% rename(y.lsoda=y, dy.lsoda=dy) %>%
    ##     merge(s2) %>% mutate(y.diff=y.lsoda - y) %>%
    ##     ggplot(aes(time, y.diff)) + geom_line()

    ## gridExtra::grid.arrange(plot(s1), plot(s2))


    ## f <- function(mu=5){
    ##     s1 <- rxSolve(van, et, c(mu=mu), method="lsoda")
    ##     s2 <- rxSolve(van, et, c(mu=mu), method="indLin")
    ##     s1 %>% rename(y.lsoda=y, dy.lsoda=dy) %>%
    ##         merge(s2) %>% mutate(y.diff=y.lsoda - y) %>%
    ##         ggplot(aes(time, y.diff)) + geom_line() + ylim(-5, 5) +
    ##         ggtitle(paste0("mu=", mu)) ->
    ##         ret
    ##     return(ret)
    ## }

    ## expect_equal(as.data.frame(s1), as.data.frame(s2), tolerance =1e-4)

    ## gridExtra::grid.arrange(plot(s1), plot(s2), plot(s3))

    ## expect_equal(as.data.frame(s1), as.data.frame(s2))

    ## microbenchmark::microbenchmark(rxSolve(mmModel,et, method="indLin"),
    ##                                rxSolve(mmModel,et, method="liblsoda"))

    iSec <- suppressMessages(rxode2(
    {
      d / dt(Ga) <- -ka * Ga
      d / dt(Gt) <- ka * Ga - ka * Gt
      Gprod <- Gss * (Clg + Clgi * Iss)
      d / dt(Gc) <- ka * Gt - Gprod + Q / Vp * Gp - (Clg + Clgi * Ie + Q) / Vg * Gc
      Gc(0) <- Gss * Vg
      d / dt(Gp) <- -Q / Vp * Gp + Q / Vg * Gc
      d / dt(Ge) <- Gc * Kge - Ge * Kge
      d / dt(I) <- (Iss * Cli) * (1 + Sincr * Gt) * (Ge / Gss)^IPRG - Cli / Vi * I
      I(0) <- Iss * Vi
      d / dt(Ie) <- kie * I - kie * Ie
    },
    indLin = TRUE
    ))
  })
})
