if (file.exists(test_path("test-nmtest.qs"))) {

  d <- qs::qread(test_path("test-nmtest.qs"))
  # internally rxode2 treats lag time evids differently than
  # non-lagged events

  # 2 different models to test then
  f <- rxode2({
    cl <- 1.1
    v <- 20
    ka <- 1.5
    d/dt(depot) <- -ka*depot
    d/dt(central) <- ka*depot - (cl/v)*central
    f(central) <- bioav
    if (mode == 1) rate(central) <- rat2
    if (mode == 2) dur(central) <- dur2
    cp <- central/(v/1000)
  })


  fl <- rxode2({
    cl <- 1.1
    v <- 20
    ka <- 1.5
    d/dt(depot) <- -ka*depot
    d/dt(central) <- ka*depot - (cl/v)*central
    lag(central) <- lagt
    f(central) <- bioav
    if (mode == 1) rate(central) <- rat2
    if (mode == 2) dur(central) <- dur2
    cp <- central/(v/1000)
  })

  library(ggplot2)

  p <- TRUE

  solveEqual <- function(id, plot = p) {
    noLag <-  d[d$id == id & d$evid != 0,]$lagt == 0
    if (plot) {
      d <- d[d$id == id,]
      ## print(ggplot(d, aes(time, cp)) +
      ##         geom_point(col="red") +
      ##         rxode2::rxTheme() +
      ##         ggtitle(paste0("id=", id)))
      ## print(etTrans(d, fl))
      s1 <- rxSolve(fl, d[d$id == id,])
      print(plot(s1, cp) +
              geom_point(data=d[d$id == id, ], aes(x=time, y=cp), col="red") +
              ggtitle(paste0("id=", id)))
    } else {
      if (noLag) {
        test_that(paste0("nmtest id:", id, " no alag"), {
          s1 <- rxSolve(f, d[d$id == id,])
          expect_equal(s1$cp, d[d$id == id & d$evid == 0,]$cp, tolerance = 0.01)
        })
       }
      test_that(paste0("nmtest id:", id, " alag"), {
        s1 <- rxSolve(fl, d[d$id == id,])
        expect_equal(s1$cp, d[d$id == id & d$evid == 0,]$cp, tolerance = 0.01)
      })
    }
  }

  invisible(lapply(seq_len(range(d$id)[2]), solveEqual))

}
